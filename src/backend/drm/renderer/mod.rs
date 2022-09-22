//! TODO: Docs

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    os::unix::prelude::AsRawFd,
    rc::Rc,
    sync::{Arc, Mutex},
};

use drm::control::{framebuffer, plane};
use drm_fourcc::{DrmFormat, DrmFourcc, DrmModifier};
use indexmap::IndexMap;
use wayland_server::{protocol::wl_buffer::WlBuffer, Resource};

#[cfg(all(feature = "backend_egl", feature = "use_system_lib"))]
use crate::backend::egl::display::BUFFER_READER;
use crate::{
    backend::{
        allocator::{
            dmabuf::{AsDmabuf, Dmabuf},
            Allocator, Buffer, Slot, Swapchain,
        },
        drm::DrmError,
        renderer::{
            damage::DamageTrackedRenderer,
            element::{Id, RenderElement, UnderlyingStorage},
            Bind, ImportAll, Renderer, Texture,
        },
    },
    output::Output,
    utils::{Buffer as BufferCoords, Physical, Point, Rectangle, Scale, Size, Transform},
};

use super::{plane_zpos, surface::PlaneConfig, DrmSurface};

mod elements;
pub mod gbm;

use elements::*;

#[derive(Debug)]
enum ScanoutBuffer<B: Buffer> {
    #[cfg(feature = "wayland_frontend")]
    Wayland(crate::backend::renderer::utils::Buffer),
    Swapchain(Slot<B>),
}

struct DrmScanoutBuffer<D: AsRawFd + 'static, B: Buffer, F: Framebuffer> {
    buffer: ScanoutBuffer<B>,
    fb: OwnedFramebuffer<D, F>,
}

impl<D, B, F> std::fmt::Debug for DrmScanoutBuffer<D, B, F>
where
    D: AsRawFd + std::fmt::Debug + 'static,
    B: Buffer + std::fmt::Debug,
    F: Framebuffer + std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GbmScanoutBuffer")
            .field("buffer", &self.buffer)
            .field("fb", &self.fb)
            .finish()
    }
}

impl<D: AsRawFd + 'static, B: Buffer, F: Framebuffer> AsRef<framebuffer::Handle>
    for DrmScanoutBuffer<D, B, F>
{
    fn as_ref(&self) -> &drm::control::framebuffer::Handle {
        self.fb.as_ref()
    }
}

#[derive(Debug)]
struct ElementState<D, B>
where
    D: AsRawFd + 'static,
    B: Framebuffer,
{
    fb_cache: HashMap<wayland_server::Weak<WlBuffer>, Option<OwnedFramebuffer<D, B>>>,
}

impl<D, B> ElementState<D, B>
where
    D: AsRawFd + 'static,
    B: Framebuffer,
{
    fn get(&self, buffer: &WlBuffer) -> Option<Option<OwnedFramebuffer<D, B>>> {
        self.fb_cache.get(&buffer.downgrade()).cloned()
    }

    fn insert(&mut self, buffer: &WlBuffer, fb: Option<OwnedFramebuffer<D, B>>) {
        self.fb_cache.insert(buffer.downgrade(), fb);
    }

    fn cleanup(&mut self) {
        // TODO: key.upgrade().is_ok() should be enough, but there is a bug in wayland-rs
        self.fb_cache
            .retain(|key, _| key.upgrade().map(|b| b.is_alive()).unwrap_or(false));
    }
}

impl<D, B> Clone for ElementState<D, B>
where
    D: AsRawFd + 'static,
    B: Framebuffer,
{
    fn clone(&self) -> Self {
        Self {
            fb_cache: self.fb_cache.clone(),
        }
    }
}

impl<D, B> Default for ElementState<D, B>
where
    D: AsRawFd + 'static,
    B: Framebuffer,
{
    fn default() -> Self {
        Self {
            fb_cache: Default::default(),
        }
    }
}

#[derive(Debug)]
struct PlaneInfo {
    pub handle: plane::Handle,
    pub zpos: Option<i32>,
}

impl PlaneInfo {
    fn from_plane<A: AsRawFd>(surface: &DrmSurface<A>, handle: plane::Handle) -> Self {
        let zpos = plane_zpos(surface, handle).expect("failed to query plane zpos");
        PlaneInfo { handle, zpos }
    }
}

#[derive(Debug)]
struct Planes {
    primary: PlaneInfo,
    cursor: Option<PlaneInfo>,
    overlay: Vec<PlaneInfo>,
}

impl Planes {
    fn from_surface<A: AsRawFd>(surface: &DrmSurface<A>) -> Self {
        let planes = surface.planes().unwrap();
        let primary = PlaneInfo::from_plane(surface, planes.primary);
        let cursor = planes.cursor.map(|plane| PlaneInfo::from_plane(surface, plane));
        let mut overlay = planes
            .overlay
            .into_iter()
            .map(|plane| PlaneInfo::from_plane(surface, plane))
            .collect::<Vec<_>>();
        overlay.sort_by_key(|info| std::cmp::Reverse(info.zpos));

        Planes {
            primary,
            cursor,
            overlay,
        }
    }
}

#[derive(Debug)]
struct State<B: AsRef<framebuffer::Handle>> {
    planes: HashMap<plane::Handle, Option<PlaneConfig<Wrapped<B>>>>,
}

impl<B: AsRef<framebuffer::Handle>> State<B> {
    fn is_assigned(&self, handle: plane::Handle) -> bool {
        self.planes
            .get(&handle)
            .map(|config| config.is_some())
            .unwrap_or(false)
    }

    fn overlaps(&self, handle: plane::Handle, element_geometry: Rectangle<i32, Physical>) -> bool {
        self.planes
            .get(&handle)
            .and_then(|config| {
                config
                    .as_ref()
                    .map(|config| config.dst.overlaps(element_geometry))
            })
            .unwrap_or(false)
    }
}

#[derive(Debug)]
struct Wrapped<B>(Rc<B>);

impl<B> std::ops::Deref for Wrapped<B> {
    type Target = B;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<B> From<B> for Wrapped<B> {
    fn from(outer: B) -> Self {
        Self(Rc::new(outer))
    }
}

impl<B> AsRef<framebuffer::Handle> for Wrapped<B>
where
    B: AsRef<framebuffer::Handle>,
{
    fn as_ref(&self) -> &framebuffer::Handle {
        (*self.0).as_ref()
    }
}

impl<B> Clone for Wrapped<B> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<B: AsRef<framebuffer::Handle>> State<B> {
    fn from_planes(planes: &Planes) -> Self {
        let cursor_plane_count = if planes.cursor.is_some() { 1 } else { 0 };
        let mut tmp = HashMap::with_capacity(planes.overlay.len() + cursor_plane_count + 1);
        tmp.insert(planes.primary.handle, None);
        if let Some(info) = planes.cursor.as_ref() {
            tmp.insert(info.handle, None);
        }
        tmp.extend(planes.overlay.iter().map(|info| (info.handle, None)));

        State { planes: tmp }
    }
}

impl<B: AsRef<framebuffer::Handle>> State<B> {
    fn test_state<A: AsRawFd + 'static>(
        &mut self,
        surface: &DrmSurface<A>,
        plane: plane::Handle,
        config: PlaneConfig<Wrapped<B>>,
        allow_modeset: bool,
    ) -> Result<bool, DrmError> {
        let current_config = self.planes.get_mut(&plane).expect("unknown plane");
        let backup = current_config.clone();
        *current_config = Some(config);

        let res = surface.test_state(
            &*self
                .planes
                .iter()
                .map(|(key, value)| (*key, value.clone()))
                .collect::<Vec<_>>(),
            allow_modeset,
        );

        match res {
            Ok(true) => Ok(true),
            Ok(false) => {
                // TODO: log
                self.planes.insert(plane, backup);
                Ok(false)
            }
            Err(err) => {
                // TODO: log
                self.planes.insert(plane, backup);
                Err(err)
            }
        }
    }

    fn commit<A: AsRawFd>(
        &self,
        surface: &DrmSurface<A>,
        event: bool,
    ) -> Result<(), crate::backend::drm::error::Error> {
        surface.commit(
            &*self
                .planes
                .iter()
                .map(|(key, value)| (*key, value.clone()))
                .collect::<Vec<_>>(),
            event,
        )
    }

    fn page_flip<A: AsRawFd>(
        &self,
        surface: &DrmSurface<A>,
        event: bool,
    ) -> Result<(), crate::backend::drm::error::Error> {
        surface.page_flip(
            &*self
                .planes
                .iter()
                .map(|(key, value)| (*key, value.clone()))
                .collect::<Vec<_>>(),
            event,
        )
    }
}

/// TODO: docs
#[derive(Debug)]
pub enum ExportBuffer<'a, B: Buffer> {
    /// A wayland buffer
    #[cfg(feature = "wayland_frontend")]
    Wayland(&'a WlBuffer),
    /// A [`Allocator`] buffer
    Allocator(&'a B),
}

/// TODO: docs
pub trait Framebuffer: AsRef<framebuffer::Handle> {
    /// Destroy the framebuffer
    fn destroy<D>(&mut self, device: &D)
    where
        D: drm::control::Device,
    {
        let _ = device.destroy_framebuffer(*AsRef::<framebuffer::Handle>::as_ref(&self));
    }
}

/// TODO: Docs
pub trait ImportBuffer<B: Buffer>
where
    B: Buffer,
{
    /// Type of the framebuffer
    type Framebuffer: Framebuffer;

    /// Type of the error
    type Error: std::error::Error;

    /// Add a framebuffer for the specified buffer
    fn add_framebuffer<D: drm::control::Device>(
        &self,
        drm: &D,
        buffer: &ExportBuffer<'_, B>,
    ) -> Result<Option<Self::Framebuffer>, Self::Error>;
}

impl<I, B> ImportBuffer<B> for Arc<Mutex<I>>
where
    I: ImportBuffer<B>,
    B: Buffer,
{
    type Framebuffer = <I as ImportBuffer<B>>::Framebuffer;
    type Error = <I as ImportBuffer<B>>::Error;

    fn add_framebuffer<D: drm::control::Device>(
        &self,
        drm: &D,
        buffer: &ExportBuffer<'_, B>,
    ) -> Result<Option<Self::Framebuffer>, Self::Error> {
        let guard = self.lock().unwrap();
        guard.add_framebuffer(drm, buffer)
    }
}

impl<I, B> ImportBuffer<B> for Rc<RefCell<I>>
where
    I: ImportBuffer<B>,
    B: Buffer,
{
    type Framebuffer = <I as ImportBuffer<B>>::Framebuffer;
    type Error = <I as ImportBuffer<B>>::Error;

    fn add_framebuffer<D: drm::control::Device>(
        &self,
        drm: &D,
        buffer: &ExportBuffer<'_, B>,
    ) -> Result<Option<Self::Framebuffer>, Self::Error> {
        self.borrow().add_framebuffer(drm, buffer)
    }
}

/// Specialized render for drm output which uses planes
#[derive(Debug)]
pub struct DrmRenderer<A, B, D, I>
where
    A: Allocator<B>,
    B: Buffer,
    D: AsRawFd + 'static,
    I: ImportBuffer<B>,
    <I as ImportBuffer<B>>::Framebuffer: std::fmt::Debug + 'static,
{
    output: Output,
    surface: Arc<DrmSurface<D>>,
    planes: Planes,
    damage_tracked_renderer: DamageTrackedRenderer,

    framebuffer_exporter: I,

    current_fb: State<DrmScanoutBuffer<D, B, <I as ImportBuffer<B>>::Framebuffer>>,
    pending_fb: Option<State<DrmScanoutBuffer<D, B, <I as ImportBuffer<B>>::Framebuffer>>>,
    queued_fb: Option<State<DrmScanoutBuffer<D, B, <I as ImportBuffer<B>>::Framebuffer>>>,
    swapchain: Swapchain<A, B>,

    cursor_size: Size<i32, Physical>,
    cursor_swapchain: Swapchain<A, B>,
    cursor_damage_tracked_renderer: DamageTrackedRenderer,

    last_size: Size<i32, Physical>,
    last_scale: Scale<f64>,

    element_states: IndexMap<Id, ElementState<D, <I as ImportBuffer<B>>::Framebuffer>>,
}

/// Errors thrown by a [`GbmBufferedSurface`]
#[derive(Debug, thiserror::Error)]
pub enum Error<
    A: std::error::Error + Send + Sync + 'static,
    B: std::error::Error + Send + Sync + 'static,
    I: std::error::Error + Send + Sync + 'static,
> {
    /// No supported pixel format for the given plane could be determined
    #[error("No supported plane buffer format found")]
    NoSupportedPlaneFormat,
    /// No supported pixel format for the given renderer could be determined
    #[error("No supported renderer buffer format found")]
    NoSupportedRendererFormat,
    /// The supported pixel formats of the renderer and plane are incompatible
    #[error("Supported plane and renderer buffer formats are incompatible")]
    FormatsNotCompatible,
    /// The swapchain is exhausted, you need to call `frame_submitted`
    #[error("Failed to allocate a new buffer")]
    NoFreeSlotsError,
    /// Failed to renderer using the given renderer
    #[error("Failed to render test frame")]
    InitialRenderingError,
    /// Error accessing the drm device
    #[error("The underlying drm surface encounted an error: {0}")]
    DrmError(#[from] DrmError),
    /// Error during buffer allocation
    #[error("The underlying allocator encounted an error: {0}")]
    Allocator(#[source] A),
    /// Error during exporting the buffer as dmabuf
    #[error("Failed to export the allocated buffer as dmabuf: {0}")]
    AsDmabufError(#[source] B),
    /// Error during importing the buffer
    #[error("The buffer importer encountered an error: {0}")]
    Import(#[source] I),
}

const SUPPORTED_FORMATS: &[DrmFourcc] = &[DrmFourcc::Argb8888, DrmFourcc::Xrgb8888];

impl<A, B, D, I> DrmRenderer<A, B, D, I>
where
    A: Allocator<B> + Clone,
    <A as Allocator<B>>::Error: std::error::Error + Send + Sync,
    B: Buffer + AsDmabuf,
    <B as AsDmabuf>::Error: std::error::Error + Send + Sync + std::fmt::Debug,
    D: AsRawFd + std::fmt::Debug + 'static,
    I: ImportBuffer<B>,
    <I as ImportBuffer<B>>::Framebuffer: std::fmt::Debug + 'static,
    <I as ImportBuffer<B>>::Error: std::error::Error + Send + Sync,
{
    /// Initialize a new [`DrmRenderer`]
    pub fn new<L>(
        output: &Output,
        surface: DrmSurface<D>,
        mut allocator: A,
        framebuffer_exporter: I,
        renderer_formats: HashSet<DrmFormat>,
        cursor_size: Size<u32, BufferCoords>,
        log: L,
    ) -> Result<Self, Error<<A as Allocator<B>>::Error, <B as AsDmabuf>::Error, <I as ImportBuffer<B>>::Error>>
    where
        L: Into<Option<::slog::Logger>>,
    {
        let log = crate::slog_or_fallback(log).new(slog::o!("backend" => "drm_render"));

        let mut error = None;
        let surface = Arc::new(surface);

        let cursor_size = Size::from((cursor_size.w as i32, cursor_size.h as i32));
        let output_mode = output.current_mode().unwrap();
        let output_scale = output.current_scale().fractional_scale().into();
        let damage_tracked_renderer = DamageTrackedRenderer::from_output(output);

        for format in SUPPORTED_FORMATS {
            slog::debug!(log, "Testing color format: {}", format);
            match Self::find_supported_format(
                surface.clone(),
                allocator.clone(),
                &framebuffer_exporter,
                renderer_formats.clone(),
                *format,
                log.clone(),
            ) {
                Ok((swapchain, planes, current_state)) => {
                    let cursor_swapchain = Swapchain::new(
                        allocator,
                        cursor_size.w as u32,
                        cursor_size.h as u32,
                        crate::backend::allocator::Fourcc::Argb8888,
                        vec![crate::backend::allocator::Modifier::Linear],
                    );

                    let cursor_damage_tracked_renderer =
                        DamageTrackedRenderer::new(cursor_size, output_scale, Transform::Normal);

                    let drm_renderer = DrmRenderer {
                        current_fb: current_state,
                        pending_fb: None,
                        queued_fb: None,
                        swapchain,
                        framebuffer_exporter,
                        cursor_size,
                        cursor_swapchain,
                        cursor_damage_tracked_renderer,
                        surface,
                        damage_tracked_renderer,
                        output: output.clone(),
                        last_size: output_mode.size,
                        last_scale: output_scale,
                        planes,
                        element_states: IndexMap::new(),
                    };

                    return Ok(drm_renderer);
                }
                Err((alloc, err)) => {
                    slog::warn!(log, "Preferred format {} not available: {:?}", format, err);
                    allocator = alloc;
                    error = Some(err);
                }
            }
        }
        Err(error.unwrap())

        // // Ok...I don't know what I am doing...at least on i915 scaling a plane for
        // // the first time requires a modeset. And it requires a modset for both directions
        // // down and up-scaling. Forcing that early seems to be enough to initialize the
        // // internal scalers for later use....
        // current_state.commit(&surface, false).unwrap();
        // // let primary_plane = plane_states.get_mut(0).unwrap();
        // // primary_plane.state.as_mut().unwrap().dst.size = mode_size.to_f64().upscale(0.5).to_i32_round();
        // // surface.commit(&*plane_states, false).unwrap();
        // // let primary_plane = plane_states.get_mut(0).unwrap();
        // // primary_plane.state.as_mut().unwrap().dst.size = mode_size.to_f64().upscale(1.5).to_i32_round();
        // // surface.commit(&*plane_states, false).unwrap();
        // // let primary_plane = plane_states.get_mut(0).unwrap();
        // // primary_plane.state.as_mut().unwrap().dst.size = mode_size;
        // // surface.commit(&*plane_states, false).unwrap();

        // let cursor_swapchain = Swapchain::new(
        //     allocator,
        //     cursor_size.w as u32,
        //     cursor_size.h as u32,
        //     crate::backend::allocator::Fourcc::Argb8888,
        //     vec![crate::backend::allocator::Modifier::Linear],
        // );

        // let cursor_damage_tracked_renderer =
        //     DamageTrackedRenderer::new(cursor_size, output_scale, Transform::Normal);

        // DrmRenderer {
        //     current_fb: current_state,
        //     pending_fb: None,
        //     queued_fb: None,
        //     swapchain,
        //     framebuffer_exporter,
        //     cursor_size,
        //     cursor_swapchain,
        //     cursor_damage_tracked_renderer,
        //     surface,
        //     damage_tracked_renderer,
        //     output: output.clone(),
        //     last_size: output_mode.size,
        //     last_scale: output_scale,
        //     planes,
        //     element_states: IndexMap::new(),
        // }
    }

    #[allow(clippy::type_complexity)]
    fn find_supported_format(
        drm: Arc<DrmSurface<D>>,
        allocator: A,
        framebuffer_exporter: &I,
        mut renderer_formats: HashSet<DrmFormat>,
        code: DrmFourcc,
        logger: slog::Logger,
    ) -> Result<
        (
            Swapchain<A, B>,
            Planes,
            State<DrmScanoutBuffer<D, B, <I as ImportBuffer<B>>::Framebuffer>>,
        ),
        (
            A,
            Error<<A as Allocator<B>>::Error, <B as AsDmabuf>::Error, <I as ImportBuffer<B>>::Error>,
        ),
    > {
        // select a format
        let mut plane_formats = match drm.supported_formats(drm.plane()) {
            Ok(formats) => formats.iter().cloned().collect::<HashSet<_>>(),
            Err(err) => return Err((allocator, err.into())),
        };

        if !plane_formats.iter().any(|fmt| fmt.code == code) {
            return Err((allocator, Error::NoSupportedPlaneFormat));
        }
        plane_formats.retain(|fmt| fmt.code == code);
        renderer_formats.retain(|fmt| fmt.code == code);

        slog::trace!(logger, "Plane formats: {:?}", plane_formats);
        slog::trace!(logger, "Renderer formats: {:?}", renderer_formats);
        slog::debug!(
            logger,
            "Remaining intersected formats: {:?}",
            plane_formats
                .intersection(&renderer_formats)
                .collect::<HashSet<_>>()
        );

        if plane_formats.is_empty() {
            return Err((allocator, Error::NoSupportedPlaneFormat));
        } else if renderer_formats.is_empty() {
            return Err((allocator, Error::NoSupportedRendererFormat));
        }

        let formats = {
            // Special case: if a format supports explicit LINEAR (but no implicit Modifiers)
            // and the other doesn't support any modifier, force Implicit.
            // This should at least result in a working pipeline possibly with a linear buffer,
            // but we cannot be sure.
            if (plane_formats.len() == 1
                && plane_formats.iter().next().unwrap().modifier == DrmModifier::Invalid
                && renderer_formats
                    .iter()
                    .all(|x| x.modifier != DrmModifier::Invalid)
                && renderer_formats.iter().any(|x| x.modifier == DrmModifier::Linear))
                || (renderer_formats.len() == 1
                    && renderer_formats.iter().next().unwrap().modifier == DrmModifier::Invalid
                    && plane_formats.iter().all(|x| x.modifier != DrmModifier::Invalid)
                    && plane_formats.iter().any(|x| x.modifier == DrmModifier::Linear))
            {
                vec![DrmFormat {
                    code,
                    modifier: DrmModifier::Invalid,
                }]
            } else {
                plane_formats
                    .intersection(&renderer_formats)
                    .cloned()
                    .collect::<Vec<_>>()
            }
        };
        slog::debug!(logger, "Testing Formats: {:?}", formats);

        let modifiers = formats.iter().map(|x| x.modifier).collect::<Vec<_>>();
        let mode = drm.pending_mode();

        let mut swapchain: Swapchain<A, B> = Swapchain::new(
            allocator,
            mode.size().0 as u32,
            mode.size().1 as u32,
            code,
            modifiers,
        );

        // Test format
        let buffer = match swapchain.acquire() {
            Ok(buffer) => buffer.unwrap(),
            Err(err) => return Err((swapchain.allocator, Error::Allocator(err))),
        };

        let dmabuf = match buffer.export() {
            Ok(dmabuf) => dmabuf,
            Err(err) => return Err((swapchain.allocator, Error::AsDmabufError(err))),
        };
        let fb_buffer = match framebuffer_exporter.add_framebuffer(&*drm, &ExportBuffer::Allocator(&buffer)) {
            Ok(fb_buffer) => fb_buffer.unwrap(),
            Err(err) => return Err((swapchain.allocator, Error::Import(err))),
        };
        let fb = OwnedFramebuffer::new(drm.clone(), fb_buffer);
        let userdata = buffer.userdata();
        userdata.insert_if_missing(|| dmabuf.clone());
        userdata.insert_if_missing(|| fb);

        let mode = drm.pending_mode();
        let handle = buffer
            .userdata()
            .get::<OwnedFramebuffer<D, <I as ImportBuffer<B>>::Framebuffer>>()
            .unwrap()
            .clone();

        let mode_size = Size::from((mode.size().0 as i32, mode.size().1 as i32));
        let planes = Planes::from_surface(&drm);

        let mut current_state = State::from_planes(&planes);
        match current_state.test_state(
            &drm,
            planes.primary.handle,
            PlaneConfig {
                src: Rectangle::from_loc_and_size(Point::default(), dmabuf.size()).to_f64(),
                dst: Rectangle::from_loc_and_size(Point::default(), mode_size),
                transform: Transform::Normal,
                buffer: Wrapped::from(DrmScanoutBuffer {
                    buffer: ScanoutBuffer::Swapchain(buffer),
                    fb: handle,
                }),
            },
            true,
        ) {
            Ok(_) => {
                slog::debug!(logger, "Choosen format: {:?}", dmabuf.format());
                Ok((swapchain, planes, current_state))
            }
            Err(err) => {
                slog::warn!(
                    logger,
                    "Mode-setting failed with automatically selected buffer format {:?}: {}",
                    dmabuf.format(),
                    err
                );
                Err((swapchain.allocator, err.into()))
            }
        }
    }

    /// Render the output
    pub fn render_output<R, E, L>(&mut self, renderer: &mut R, elements: &[E], clear_color: [f32; 4], log: L)
    where
        E: RenderElement<R>,
        R: Renderer + ImportAll + Bind<Dmabuf>,
        <R as Renderer>::TextureId: Texture + 'static,
        L: Into<Option<slog::Logger>>,
    {
        let log = crate::slog_or_fallback(log);

        let current_size = self.output.current_mode().unwrap().size;
        let output_scale = self.output.current_scale().fractional_scale().into();
        let output_geo = Rectangle::from_loc_and_size((0, 0), current_size);

        if self.last_size != current_size {
            // Output size changed, reset everything
            slog::debug!(log, "output size changed, resizing swapchain");
            self.swapchain
                .resize(current_size.w as u32, current_size.h as u32);
            self.last_size = current_size;
        }

        if self.last_scale != output_scale {
            self.cursor_damage_tracked_renderer = DamageTrackedRenderer::new(
                (self.cursor_size.w as i32, self.cursor_size.h as i32),
                output_scale,
                Transform::Normal,
            );
            self.last_scale = output_scale;
        }

        let primary_plane_buffer = self
            .swapchain
            .acquire()
            .expect("failed to acquire buffer")
            .expect("no buffer");

        let age = primary_plane_buffer.age().into();
        let dmabuf = primary_plane_buffer
            .userdata()
            .get::<Dmabuf>()
            .cloned()
            .unwrap_or_else(|| {
                let dmabuf = primary_plane_buffer
                    .export()
                    .expect("failed to export buffer as dmabuffer");
                let fb_buffer = self
                    .framebuffer_exporter
                    .add_framebuffer(&*self.surface, &ExportBuffer::Allocator(&primary_plane_buffer))
                    .unwrap()
                    .unwrap();
                let fb = OwnedFramebuffer::new(self.surface.clone(), fb_buffer);

                let userdata = primary_plane_buffer.userdata();
                userdata.insert_if_missing(|| dmabuf.clone());
                userdata.insert_if_missing(|| fb);
                dmabuf
            });

        let fb = primary_plane_buffer
            .userdata()
            .get::<OwnedFramebuffer<D, <I as ImportBuffer<B>>::Framebuffer>>()
            .unwrap()
            .clone();

        let mut opaque_regions: Vec<Rectangle<i32, Physical>> = Vec::new();
        let mut element_states = IndexMap::new();
        let mut pending_state = State::from_planes(&self.planes);

        // TODO: If we only have a single element we could try to do direct scan-out
        // on the primary plane here

        match pending_state.test_state(
            &self.surface,
            self.planes.primary.handle,
            PlaneConfig {
                src: Rectangle::from_loc_and_size(Point::default(), dmabuf.size()).to_f64(),
                dst: Rectangle::from_loc_and_size(Point::default(), current_size),
                transform: Transform::Normal,
                buffer: Wrapped::from(DrmScanoutBuffer {
                    buffer: ScanoutBuffer::Swapchain(primary_plane_buffer),
                    fb,
                }),
            },
            self.surface.commit_pending(),
        ) {
            Ok(true) => {}
            Ok(false) => {
                panic!("failed to test primary plane");
            }
            Err(err) => {
                panic!("failed to test primary plane: {:?}", err);
            }
        }

        let mut primary_plane_elements: Vec<DrmRenderElements<'_, R, E>> = Vec::with_capacity(elements.len());

        for element in elements.iter() {
            let element_geometry = element.geometry(output_scale);

            // First test if the element overlaps with the output
            // if not we can skip it
            if !element_geometry.overlaps(output_geo) {
                continue;
            }

            // Then test if the element is completely hidden behind opaque regions
            let is_hidden = opaque_regions
                .iter()
                .fold([element_geometry].to_vec(), |geometry, opaque_region| {
                    geometry
                        .into_iter()
                        .flat_map(|g| g.subtract_rect(*opaque_region))
                        .collect::<Vec<_>>()
                })
                .is_empty();

            if is_hidden {
                // No need to draw a completely hidden element
                slog::trace!(log, "skipping completely obscured element {:?}", element.id());
                continue;
            }

            if !self.try_assign_element(
                renderer,
                element,
                &mut element_states,
                &mut primary_plane_elements,
                output_scale,
                &mut pending_state,
                &log,
            ) {
                primary_plane_elements.push(element.into());
            }

            opaque_regions.extend(
                element
                    .opaque_regions(output_scale)
                    .into_iter()
                    .map(|mut region| {
                        region.loc += element_geometry.loc;
                        region
                    }),
            );

            // TODO: If the remaining element count after a success is 1 we could
            // directly try direct scan-out on the primary plane for the remaining
            // element
            // (does not apply if we have assigned a element to an underlay plane,
            // but could be used if there is not overlap)
        }

        // TODO: If we only have a single element on the primary
        // plane we could try to do direct scan out for that element
        // on the primary if its geometry matches the output geometry
        // or when the clear color will result in black color anyway
        // (does not apply if we have assigned a element to an underlay plane,
        // but could be used if there is not overlap)

        // Cleanup old state (e.g. old dmabuffers)
        for element_state in element_states.values_mut() {
            element_state.cleanup();
        }

        renderer.bind(dmabuf).expect("failed to bind render buffer");

        // Draw the remaining elements on the primary plane
        let render_res = self.damage_tracked_renderer.render_output(
            renderer,
            age,
            &*primary_plane_elements,
            clear_color,
            log,
        );

        if render_res.is_err() {
            // Rendering failed at some point, reset the buffers
            // as we probably now have some half drawn buffer
            self.swapchain.reset_buffers();
        } else {
            self.queued_fb = Some(pending_state);

            if self.pending_fb.is_none() && self.queued_fb.is_some() {
                self.submit();
            }
        }

        self.element_states = element_states;
    }

    fn submit(&mut self) {
        let state = self.queued_fb.take().unwrap();

        let flip = if self.surface.commit_pending() {
            state.commit(&self.surface, true)
        } else {
            state.page_flip(&self.surface, true)
        };
        if flip.is_ok() {
            let primary_slot = state.planes.get(&self.planes.primary.handle).and_then(|config| {
                config.as_ref().and_then(|config| match &config.buffer.buffer {
                    ScanoutBuffer::Swapchain(slot) => Some(slot),
                    _ => None,
                })
            });

            if let Some(slot) = primary_slot {
                self.swapchain.submitted(slot);
            }

            if let Some(cursor_plane) = self.planes.cursor.as_ref() {
                let cursor_slot = state.planes.get(&cursor_plane.handle).and_then(|config| {
                    config.as_ref().and_then(|config| match &config.buffer.buffer {
                        ScanoutBuffer::Swapchain(slot) => Some(slot),
                        _ => None,
                    })
                });

                if let Some(slot) = cursor_slot {
                    self.cursor_swapchain.submitted(slot);
                }
            }

            self.pending_fb = Some(state);
        }

        flip.expect("failed to flip");
    }

    /// Marks the current frame as submitted.
    ///
    /// *Note*: Needs to be called, after the vblank event of the matching [`DrmDevice`](super::super::DrmDevice)
    /// was received after calling [`GbmBufferedSurface::queue_buffer`] on this surface.
    /// Otherwise the underlying swapchain will run out of buffers eventually.
    pub fn frame_submitted(&mut self) {
        if let Some(mut pending) = self.pending_fb.take() {
            std::mem::swap(&mut pending, &mut self.current_fb);
            if self.queued_fb.is_some() {
                self.submit();
            }
        }
    }

    /// Reset the underlying buffers
    pub fn reset_buffers(&mut self) {
        self.swapchain.reset_buffers();
        self.cursor_swapchain.reset_buffers();
        self.element_states.clear();
    }

    #[allow(clippy::too_many_arguments)]
    fn try_assign_element<'a, R, E>(
        &mut self,
        renderer: &mut R,
        element: &E,
        element_states: &mut IndexMap<Id, ElementState<D, <I as ImportBuffer<B>>::Framebuffer>>,
        primary_plane_elements: &mut Vec<DrmRenderElements<'a, R, E>>,
        scale: Scale<f64>,
        state: &mut State<DrmScanoutBuffer<D, B, <I as ImportBuffer<B>>::Framebuffer>>,
        log: &slog::Logger,
    ) -> bool
    where
        R: Renderer + ImportAll + Bind<Dmabuf>,
        E: RenderElement<R>,
    {
        if self.try_assign_cursor_plane(renderer, element, scale, state, log) {
            slog::trace!(log, "assigned element to cursor plane");
            return true;
        }

        #[cfg(feature = "wayland_frontend")]
        if self.try_assign_overlay_plane(
            renderer,
            element,
            element_states,
            primary_plane_elements,
            scale,
            state,
            log,
        ) {
            slog::trace!(log, "assigned element to overlay plane");
            return true;
        }

        false
    }

    fn try_assign_cursor_plane<R, E>(
        &mut self,
        renderer: &mut R,
        element: &E,
        scale: Scale<f64>,
        state: &mut State<DrmScanoutBuffer<D, B, <I as ImportBuffer<B>>::Framebuffer>>,
        log: &slog::Logger,
    ) -> bool
    where
        R: Renderer + Bind<Dmabuf>,
        E: RenderElement<R>,
    {
        // if we have no cursor plane we can exit early
        let plane_info = match self.planes.cursor.as_ref() {
            Some(plane_info) => plane_info,
            None => return false,
        };

        // something is already assigned to our cursor plane
        if state.is_assigned(plane_info.handle) {
            return false;
        }

        // if the element is greater than the cursor size we can not
        // use the cursor plane to scan out the element
        if element.geometry(scale).size > self.cursor_size {
            return false;
        }

        let cursor_slot = self.cursor_swapchain.acquire().unwrap().unwrap();

        let dmabuf = cursor_slot
            .userdata()
            .get::<Dmabuf>()
            .cloned()
            .unwrap_or_else(|| {
                let dmabuf = cursor_slot.export().expect("failed to export dmabuf");
                let framebuffer = self
                    .framebuffer_exporter
                    .add_framebuffer(&*self.surface, &ExportBuffer::Allocator(&cursor_slot))
                    .expect("failed to add framebuffer")
                    .unwrap();
                let fb = OwnedFramebuffer::new(self.surface.clone(), framebuffer);

                let userdata = cursor_slot.userdata();
                userdata.insert_if_missing(|| dmabuf.clone());
                userdata.insert_if_missing(|| fb);
                dmabuf
            });

        let fb = cursor_slot
            .userdata()
            .get::<OwnedFramebuffer<D, <I as ImportBuffer<B>>::Framebuffer>>()
            .unwrap()
            .clone();

        let buffer_size = dmabuf.size();

        renderer.bind(dmabuf).unwrap();

        self.cursor_damage_tracked_renderer
            .render_output(
                renderer,
                cursor_slot.age().into(),
                &[RelocatingRenderElement::from_element(Point::default(), element)],
                COLOR_TRANSPARENT,
                log.clone(),
            )
            .expect("failed to render cursor plane");

        let location = element.location(scale);

        state
            .test_state(
                &self.surface,
                plane_info.handle,
                PlaneConfig {
                    src: Rectangle::from_loc_and_size(Point::default(), buffer_size).to_f64(),
                    dst: Rectangle::from_loc_and_size(location, self.cursor_size),
                    transform: Transform::Normal,
                    buffer: Wrapped::from(DrmScanoutBuffer {
                        buffer: ScanoutBuffer::Swapchain(cursor_slot),
                        fb,
                    }),
                },
                false,
            )
            .unwrap_or_default()
    }

    #[cfg(feature = "wayland_frontend")]
    #[allow(clippy::too_many_arguments)]
    fn try_assign_overlay_plane<'a, R, E>(
        &self,
        renderer: &mut R,
        element: &E,
        element_states: &mut IndexMap<Id, ElementState<D, <I as ImportBuffer<B>>::Framebuffer>>,
        primary_plane_elements: &mut Vec<DrmRenderElements<'a, R, E>>,
        scale: Scale<f64>,
        state: &mut State<DrmScanoutBuffer<D, B, <I as ImportBuffer<B>>::Framebuffer>>,
        log: &slog::Logger,
    ) -> bool
    where
        R: Renderer + ImportAll,
        E: RenderElement<R>,
    {
        let element_id = element.id();

        let buffer = if let Some(UnderlyingStorage::Wayland(buffer)) = element.underlying_storage(renderer) {
            buffer
        } else {
            return false;
        };

        let element_geometry = element.geometry(scale);

        let overlaps_with_primary_plane_element = primary_plane_elements.iter().any(|e| {
            let other_geometry = e.geometry(scale);
            other_geometry.overlaps(element_geometry)
        });

        let is_opaque = element_is_opaque(element, scale);

        for plane in self.planes.overlay.iter() {
            // something is already assigned to our overlay plane
            if state.is_assigned(plane.handle) {
                slog::trace!(
                    log,
                    "skipping plane {:?} with zpos {:?} for element {:?}, already has element assigned, skipping",
                    plane.handle,
                    plane.zpos,
                    element_id,
                );
                continue;
            }

            // test if the plane represents an underlay
            let is_underlay = self.planes.primary.zpos.unwrap_or_default() > plane.zpos.unwrap_or_default();

            if is_underlay && !is_opaque {
                slog::trace!(
                    log,
                    "skipping direct scan-out on plane plane {:?} with zpos {:?}, element {:?} is not opaque",
                    plane.handle,
                    plane.zpos,
                    element_id
                );
                continue;
            }

            // if the element overlaps with an element on
            // the primary plane and is not an underlay
            // we can not assign it to any overlay plane
            if overlaps_with_primary_plane_element && !is_underlay {
                slog::trace!(
                    log,
                    "skipping direct scan-out on plane plane {:?} with zpos {:?}, element {:?} overlaps with element on primary plane", plane.handle, plane.zpos, element_id,
                );
                return false;
            }

            let overlaps_with_plane_underneath = self
                .planes
                .overlay
                .iter()
                .filter(|info| {
                    info.handle != plane.handle
                        && info.zpos.unwrap_or_default() <= plane.zpos.unwrap_or_default()
                })
                .any(|overlapping_plane| state.overlaps(overlapping_plane.handle, element_geometry));

            // if we overlap we a plane below which already
            // has an element assigned we can not use the
            // plane for direct scan-out
            if overlaps_with_plane_underneath {
                slog::trace!(
                    log,
                    "skipping direct scan-out on plane {:?} with zpos {:?}, element {:?} geometry {:?} overlaps with plane underneath", plane.handle, plane.zpos, element_id, element_geometry,
                );
                continue;
            }

            // First we try to find a state in our new states, this is important if
            // we got the same id multiple times. If we can't find it we use the previous
            // state if available
            let mut element_state = element_states
                .get(element_id)
                .or_else(|| self.element_states.get(element_id))
                .cloned()
                .unwrap_or_default();

            element_state.cleanup();

            let cached_fb = element_state.get(&buffer);

            if cached_fb.is_none() {
                slog::trace!(
                    log,
                    "no cached fb, exporting new fb for element {:?} buffer {:?}",
                    element_id,
                    &buffer
                );

                let fb = self
                    .framebuffer_exporter
                    .add_framebuffer(&*self.surface, &ExportBuffer::Wayland(&buffer))
                    .map(|fb| fb.map(|fb| OwnedFramebuffer::new(self.surface.clone(), fb)))
                    .unwrap_or_else(|err| {
                        slog::debug!(log, "failed to add framebuffer: {:?}", err);
                        None
                    });

                if fb.is_none() {
                    slog::debug!(
                        log,
                        "could not import framebuffer for element {:?} buffer {:?}",
                        element_id,
                        &buffer
                    );
                }

                element_state.insert(&buffer, fb);
            } else {
                slog::trace!(
                    log,
                    "using cached fb for element {:?} buffer {:?}",
                    element_id,
                    &buffer
                );
            }

            element_states.insert(element_id.clone(), element_state);

            let fb = match element_states
                .get(element_id)
                .and_then(|state| state.get(&buffer).unwrap())
            {
                Some(fb) => fb,
                None => {
                    return false;
                }
            };

            // Try to assign the element to a plane
            slog::trace!(log, "testing direct scan-out for element {:?} on plane {:?} with zpos {:?}: fb: {:?}, buffer: {:?}", element_id, plane.handle, plane.zpos, &fb, &buffer);

            // TODO: Also use element.transform()
            let transform = buffer_transform(&buffer);

            if state
                .test_state(
                    &self.surface,
                    plane.handle,
                    PlaneConfig {
                        src: element.src(),
                        dst: element_geometry,
                        transform,
                        buffer: Wrapped::from(DrmScanoutBuffer {
                            fb,
                            buffer: ScanoutBuffer::Wayland(buffer.clone()),
                        }),
                    },
                    false,
                )
                .unwrap_or_default()
            {
                if is_underlay {
                    // if we assigned the element to an underlay we have to
                    // punch a hole into the primary plane
                    primary_plane_elements
                        .push(HolepunchRenderElement::from_render_element(element, scale).into());
                }

                slog::trace!(
                    log,
                    "successfully assigned element {:?} to plane {:?} with zpos {:?} for direct scan-out",
                    element_id,
                    plane.handle,
                    plane.zpos,
                );
                // we found our plane
                return true;
            } else {
                // TODO: we should probably remember that the test failed
                // on this plane and not retry it again for the same buffer
                slog::debug!(
                    log,
                    "skipping direct scan-out on plane {:?} with zpos {:?} for element {:?}, test failed",
                    plane.handle,
                    plane.zpos,
                    element_id
                );
            }
        }

        false
    }
}

#[cfg(feature = "wayland_frontend")]
fn buffer_transform(buffer: &WlBuffer) -> Transform {
    if let Ok(dmabuf) = crate::wayland::dmabuf::get_dmabuf(buffer) {
        if dmabuf.y_inverted() {
            return Transform::Flipped;
        } else {
            return Transform::Normal;
        }
    }

    #[cfg(all(feature = "backend_egl", feature = "use_system_lib"))]
    if let Some(Ok(egl_buffer)) = BUFFER_READER
        .lock()
        .unwrap()
        .as_ref()
        .and_then(|x| x.upgrade())
        .map(|x| x.egl_buffer_contents(buffer))
    {
        if egl_buffer.y_inverted {
            return Transform::Flipped;
        } else {
            return Transform::Normal;
        }
    }

    Transform::Normal
}

fn element_is_opaque<R, E>(element: &E, scale: Scale<f64>) -> bool
where
    R: Renderer,
    E: RenderElement<R>,
{
    let opaque_regions = element.opaque_regions(scale);
    let element_geometry = Rectangle::from_loc_and_size(Point::default(), element.geometry(scale).size);

    opaque_regions
        .iter()
        .fold([element_geometry].to_vec(), |geometry, opaque_region| {
            geometry
                .into_iter()
                .flat_map(|g| g.subtract_rect(*opaque_region))
                .collect::<Vec<_>>()
        })
        .is_empty()
}

struct OwnedFrambufferInner<D: AsRawFd + 'static, B: Framebuffer> {
    drm: Arc<DrmSurface<D>>,
    buffer: B,
}

impl<A: AsRawFd + 'static, B: Framebuffer> Drop for OwnedFrambufferInner<A, B> {
    fn drop(&mut self) {
        self.buffer.destroy(&*self.drm);
    }
}

impl<A: AsRawFd + 'static, B: Framebuffer> AsRef<framebuffer::Handle> for OwnedFrambufferInner<A, B> {
    fn as_ref(&self) -> &framebuffer::Handle {
        AsRef::<framebuffer::Handle>::as_ref(&self.buffer)
    }
}

struct OwnedFramebuffer<D: AsRawFd + 'static, B: Framebuffer> {
    inner: Arc<OwnedFrambufferInner<D, B>>,
}

impl<D: AsRawFd + 'static, B: Framebuffer + std::fmt::Debug> std::fmt::Debug for OwnedFramebuffer<D, B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OwnedFramebuffer")
            .field("buffer", &self.inner.buffer)
            .finish()
    }
}

impl<D: AsRawFd + 'static, B: Framebuffer> OwnedFramebuffer<D, B> {
    fn new(surface: Arc<DrmSurface<D>>, buffer: B) -> Self {
        OwnedFramebuffer {
            inner: Arc::new(OwnedFrambufferInner { drm: surface, buffer }),
        }
    }
}

impl<D: AsRawFd + 'static, B: Framebuffer> Clone for OwnedFramebuffer<D, B> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<D: AsRawFd + 'static, B: Framebuffer> AsRef<framebuffer::Handle> for OwnedFramebuffer<D, B> {
    fn as_ref(&self) -> &framebuffer::Handle {
        (*self.inner).as_ref()
    }
}
