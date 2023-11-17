//! Implementation of the rendering traits using pixman
use std::{
    collections::HashSet,
    ops::Deref,
    rc::Rc,
    sync::atomic::{AtomicBool, Ordering},
};

use drm_fourcc::{DrmFormat, DrmFourcc, DrmModifier};
use pixman::{Filter, FormatCode, Image, Operation, Repeat};
use tracing::warn;
use wayland_server::{protocol::wl_buffer, Resource, Weak};

use crate::{
    backend::allocator::{
        dmabuf::{DmaBufSyncFlags, Dmabuf, DmabufMapping},
        format::has_alpha,
        Buffer,
    },
    utils::{Buffer as BufferCoords, Physical, Rectangle, Scale, Size, Transform},
    wayland::{compositor::SurfaceData, shm},
};

#[cfg(all(
    feature = "wayland_frontend",
    feature = "backend_egl",
    feature = "use_system_lib"
))]
use super::ImportEgl;
use super::{
    sync::SyncPoint, Bind, DebugFlags, ExportMem, Frame, ImportDma, ImportDmaWl, ImportMem, ImportMemWl,
    Offscreen, Renderer, Texture, TextureFilter, TextureMapping, Unbind,
};

mod error;

pub use error::*;

lazy_static::lazy_static! {
    static ref SUPPORTED_FORMATS: Vec<drm_fourcc::DrmFourcc> = {
        vec![
            #[cfg(target_endian = "little")]
            DrmFourcc::Rgb565,
            DrmFourcc::Xrgb8888,
            DrmFourcc::Argb8888,
            DrmFourcc::Xbgr8888,
            DrmFourcc::Abgr8888,
            DrmFourcc::Rgbx8888,
            DrmFourcc::Rgba8888,
            DrmFourcc::Bgrx8888,
            DrmFourcc::Bgra8888,
            #[cfg(target_endian = "little")]
            DrmFourcc::Xrgb2101010,
            #[cfg(target_endian = "little")]
            DrmFourcc::Argb2101010,
            #[cfg(target_endian = "little")]
            DrmFourcc::Xbgr2101010,
            #[cfg(target_endian = "little")]
            DrmFourcc::Abgr2101010,
        ]
    };
}

#[derive(Debug)]
enum PixmanTarget {
    Image(PixmanImage),
    RenderBuffer(PixmanRenderBuffer),
}

impl PixmanTarget {
    fn image(&self) -> &pixman::Image<'static, 'static> {
        match self {
            PixmanTarget::Image(buffer) => &buffer.image,
            PixmanTarget::RenderBuffer(render_buffer) => &render_buffer.0,
        }
    }
}

/// Offscreen render buffer
#[derive(Debug)]
pub struct PixmanRenderBuffer(pixman::Image<'static, 'static>);

impl From<pixman::Image<'static, 'static>> for PixmanRenderBuffer {
    fn from(value: pixman::Image<'static, 'static>) -> Self {
        Self(value)
    }
}

impl Offscreen<PixmanRenderBuffer> for PixmanRenderer {
    #[profiling::function]
    fn create_buffer(
        &mut self,
        format: DrmFourcc,
        size: Size<i32, BufferCoords>,
    ) -> Result<PixmanRenderBuffer, <Self as Renderer>::Error> {
        let format_code =
            FormatCode::try_from(format).map_err(|_| PixmanError::UnsupportedPixelFormat(format))?;
        let image = pixman::Image::new(format_code, size.w as usize, size.h as usize, true)
            .map_err(|_| PixmanError::Unsupported)?;
        Ok(PixmanRenderBuffer::from(image))
    }
}

impl Bind<PixmanRenderBuffer> for PixmanRenderer {
    #[profiling::function]
    fn bind(&mut self, target: PixmanRenderBuffer) -> Result<(), <Self as Renderer>::Error> {
        self.target = Some(PixmanTarget::RenderBuffer(target));
        Ok(())
    }

    fn supported_formats(&self) -> Option<HashSet<DrmFormat>> {
        lazy_static::lazy_static! {
            static ref RENDER_BUFFER_FORMATS: HashSet<DrmFormat> = {
                SUPPORTED_FORMATS.iter().copied().map(|code| DrmFormat {
                    code,
                    modifier: DrmModifier::Linear,
                }).collect()
            };
        }
        Some(RENDER_BUFFER_FORMATS.clone())
    }
}

#[derive(Debug)]
struct PixmanDmabufMapping {
    dmabuf: Dmabuf,
    _mapping: DmabufMapping,
}

#[derive(Debug)]
struct PixmanImage {
    buffer: Option<Weak<wl_buffer::WlBuffer>>,
    dmabuf: Option<PixmanDmabufMapping>,
    image: Image<'static, 'static>,
    _flipped: bool, /* TODO: What about flipped textures? */
}

impl PixmanImage {
    #[inline]
    fn image(&self) -> &Image<'static, 'static> {
        &self.image
    }

    #[profiling::function]
    fn accessor<'l>(&'l self) -> Result<TextureAccessor<'l>, PixmanError> {
        if let Some(buffer) = self.buffer.as_ref() {
            buffer.upgrade().map_err(|_| PixmanError::WlBufferDestroyed)?;
        }

        let guard = if let Some(mapping) = self.dmabuf.as_ref() {
            Some(DmabufReadGuard::new(&mapping.dmabuf)?)
        } else {
            None
        };

        Ok(TextureAccessor {
            image: self.image(),
            _guard: guard,
        })
    }
}

/// A handle to a pixman texture
#[derive(Debug, Clone)]
pub struct PixmanTexture(Rc<PixmanImage>);

impl From<pixman::Image<'static, 'static>> for PixmanTexture {
    fn from(image: pixman::Image<'static, 'static>) -> Self {
        Self(Rc::new(PixmanImage {
            buffer: None,
            dmabuf: None,
            _flipped: false,
            image,
        }))
    }
}

struct DmabufReadGuard<'fd> {
    dmabuf: &'fd Dmabuf,
}

impl<'fd> DmabufReadGuard<'fd> {
    #[profiling::function]
    pub fn new(dmabuf: &'fd Dmabuf) -> std::io::Result<Self> {
        dmabuf.sync(DmaBufSyncFlags::START | DmaBufSyncFlags::READ)?;
        Ok(Self { dmabuf })
    }
}

impl<'fd> Drop for DmabufReadGuard<'fd> {
    #[profiling::function]
    fn drop(&mut self) {
        if let Err(err) = self.dmabuf.sync(DmaBufSyncFlags::END | DmaBufSyncFlags::READ) {
            tracing::warn!(?err, "failed to end sync read");
        }
    }
}

struct TextureAccessor<'l> {
    image: &'l Image<'static, 'static>,
    _guard: Option<DmabufReadGuard<'l>>,
}

impl<'l> Deref for TextureAccessor<'l> {
    type Target = Image<'static, 'static>;

    fn deref(&self) -> &Self::Target {
        self.image
    }
}

impl PixmanTexture {
    #[inline]
    fn image(&self) -> &Image<'static, 'static> {
        self.0.image()
    }

    #[profiling::function]
    fn accessor<'l>(&'l self) -> Result<TextureAccessor<'l>, PixmanError> {
        self.0.accessor()
    }
}

impl Texture for PixmanTexture {
    fn width(&self) -> u32 {
        self.image().width() as u32
    }

    fn height(&self) -> u32 {
        self.image().height() as u32
    }

    fn format(&self) -> Option<DrmFourcc> {
        DrmFourcc::try_from(self.image().format()).ok()
    }
}

/// Handle to the currently rendered frame during [`PixmanRenderer::render`](Renderer::render).
#[derive(Debug)]
pub struct PixmanFrame<'frame> {
    renderer: &'frame mut PixmanRenderer,

    transform: Transform,
    output_size: Size<i32, Physical>,
    size: Size<i32, Physical>,

    finished: AtomicBool,
}

impl<'frame> Frame for PixmanFrame<'frame> {
    type Error = PixmanError;

    type TextureId = PixmanTexture;

    fn id(&self) -> usize {
        0
    }

    #[profiling::function]
    fn clear(&mut self, color: [f32; 4], at: &[Rectangle<i32, Physical>]) -> Result<(), Self::Error> {
        let target_image = self
            .renderer
            .target
            .as_ref()
            .map(|target| target.image())
            .expect("frame without target");

        let solid = pixman::Solid::new(color).map_err(|_| PixmanError::Unsupported)?;

        let mut clip_region =
            pixman::Region32::init_rect(0, 0, self.output_size.w as u32, self.output_size.h as u32);

        let at = at
            .iter()
            .map(|rect| {
                let rect = self.transform.transform_rect_in(*rect, &self.size);

                let p1 = rect.loc;
                let p2 = p1 + rect.size.to_point();
                pixman::Box32 {
                    x1: p1.x,
                    y1: p1.y,
                    x2: p2.x,
                    y2: p2.y,
                }
            })
            .collect::<Vec<_>>();
        let at = pixman::Region32::init_rects(&at);
        clip_region = clip_region.intersect(&at);

        target_image.set_clip_region32(Some(&clip_region)).unwrap();

        target_image.composite32(
            Operation::Src,
            &solid,
            None,
            0,
            0,
            0,
            0,
            0,
            0,
            target_image.width() as i32,
            target_image.height() as i32,
        );

        target_image.set_clip_region32(None).unwrap();

        Ok(())
    }

    #[profiling::function]
    fn draw_solid(
        &mut self,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        color: [f32; 4],
    ) -> Result<(), Self::Error> {
        let target_image = self
            .renderer
            .target
            .as_ref()
            .map(|target| target.image())
            .expect("frame without target");

        let solid = pixman::Solid::new(color).map_err(|_| PixmanError::Unsupported)?;

        let mut clip_region =
            pixman::Region32::init_rect(0, 0, self.output_size.w as u32, self.output_size.h as u32);

        let damage_boxes = damage
            .iter()
            .copied()
            .map(|mut rect| {
                rect.loc += dst.loc;

                let rect = self.transform.transform_rect_in(rect, &self.size);

                let p1 = rect.loc;
                let p2 = p1 + rect.size.to_point();
                pixman::Box32 {
                    x1: p1.x,
                    y1: p1.y,
                    x2: p2.x,
                    y2: p2.y,
                }
            })
            .collect::<Vec<_>>();
        let damage_region = pixman::Region32::init_rects(&damage_boxes);
        clip_region = clip_region.intersect(&damage_region);

        target_image.set_clip_region32(Some(&clip_region)).unwrap();

        let operation = if color[3] == 1f32 {
            Operation::Src
        } else {
            Operation::Over
        };

        target_image.composite32(
            operation,
            &solid,
            None,
            0,
            0,
            0,
            0,
            0,
            0,
            target_image.width() as i32,
            target_image.height() as i32,
        );

        if self.renderer.debug_flags.contains(DebugFlags::TINT) {
            target_image.composite32(
                Operation::Over,
                &self.renderer.tint,
                None,
                0,
                0,
                0,
                0,
                0,
                0,
                target_image.width() as i32,
                target_image.height() as i32,
            );
        }

        target_image.set_clip_region32(None).unwrap();

        Ok(())
    }

    #[profiling::function]
    fn render_texture_from_to(
        &mut self,
        texture: &Self::TextureId,
        src: Rectangle<f64, BufferCoords>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        src_transform: Transform,
        _alpha: f32, /* TODO: Use for alpha mask */
    ) -> Result<(), Self::Error> {
        let src_image = texture.accessor()?;

        let target_image = self
            .renderer
            .target
            .as_ref()
            .map(|target| target.image())
            .expect("frame without target");

        let dst_loc = dst.loc;
        let dst = self.transform.transform_rect_in(dst, &self.size);

        // Our renderer works with clock-wise rotation, but the scr_transform in contrast to
        // the output transform is specified counter-clock-wise.
        let src_transform = src_transform.invert();

        let src: Rectangle<i32, BufferCoords> = src.to_i32_up::<i32>();

        let image_transform = match (src_transform, self.transform) {
            (Transform::Normal, output_transform) => output_transform,

            (Transform::_90, Transform::Normal) => Transform::_270,
            (Transform::_90, Transform::_90) => Transform::Normal,
            (Transform::_90, Transform::_180) => Transform::_90,
            (Transform::_90, Transform::_270) => Transform::_180,
            (Transform::_90, Transform::Flipped) => Transform::Flipped90,
            (Transform::_90, Transform::Flipped90) => Transform::Flipped180,
            (Transform::_90, Transform::Flipped180) => Transform::Flipped270,
            (Transform::_90, Transform::Flipped270) => Transform::Flipped,

            (Transform::_180, Transform::Normal) => Transform::_180,
            (Transform::_180, Transform::_90) => Transform::_270,
            (Transform::_180, Transform::_180) => Transform::Normal,
            (Transform::_180, Transform::_270) => Transform::_90,
            (Transform::_180, Transform::Flipped) => Transform::Flipped180,
            (Transform::_180, Transform::Flipped90) => Transform::Flipped270,
            (Transform::_180, Transform::Flipped180) => Transform::Flipped,
            (Transform::_180, Transform::Flipped270) => Transform::Flipped90,

            (Transform::_270, Transform::Normal) => Transform::_90,
            (Transform::_270, Transform::_90) => Transform::_180,
            (Transform::_270, Transform::_180) => Transform::_270,
            (Transform::_270, Transform::_270) => Transform::Normal,
            (Transform::_270, Transform::Flipped) => Transform::Flipped270,
            (Transform::_270, Transform::Flipped90) => Transform::Flipped,
            (Transform::_270, Transform::Flipped180) => Transform::Flipped90,
            (Transform::_270, Transform::Flipped270) => Transform::Flipped180,

            (Transform::Flipped, Transform::Normal) => Transform::Flipped,
            (Transform::Flipped, Transform::_90) => Transform::Flipped90,
            (Transform::Flipped, Transform::_180) => Transform::Flipped180,
            (Transform::Flipped, Transform::_270) => Transform::Flipped270,
            (Transform::Flipped, Transform::Flipped) => Transform::Normal,
            (Transform::Flipped, Transform::Flipped90) => Transform::_90,
            (Transform::Flipped, Transform::Flipped180) => Transform::_180,
            (Transform::Flipped, Transform::Flipped270) => Transform::_270,

            (Transform::Flipped90, Transform::Normal) => Transform::Flipped90,
            (Transform::Flipped90, Transform::_90) => Transform::Flipped180,
            (Transform::Flipped90, Transform::_180) => Transform::Flipped270,
            (Transform::Flipped90, Transform::_270) => Transform::Flipped,
            (Transform::Flipped90, Transform::Flipped) => Transform::_270,
            (Transform::Flipped90, Transform::Flipped90) => Transform::Normal,
            (Transform::Flipped90, Transform::Flipped180) => Transform::_90,
            (Transform::Flipped90, Transform::Flipped270) => Transform::_180,

            (Transform::Flipped180, Transform::Normal) => Transform::Flipped180,
            (Transform::Flipped180, Transform::_90) => Transform::Flipped270,
            (Transform::Flipped180, Transform::_180) => Transform::Flipped,
            (Transform::Flipped180, Transform::_270) => Transform::Flipped90,
            (Transform::Flipped180, Transform::Flipped) => Transform::_180,
            (Transform::Flipped180, Transform::Flipped90) => Transform::_270,
            (Transform::Flipped180, Transform::Flipped180) => Transform::Normal,
            (Transform::Flipped180, Transform::Flipped270) => Transform::_90,

            (Transform::Flipped270, Transform::Normal) => Transform::Flipped270,
            (Transform::Flipped270, Transform::_90) => Transform::Flipped,
            (Transform::Flipped270, Transform::_180) => Transform::Flipped90,
            (Transform::Flipped270, Transform::_270) => Transform::Flipped180,
            (Transform::Flipped270, Transform::Flipped) => Transform::_90,
            (Transform::Flipped270, Transform::Flipped90) => Transform::_180,
            (Transform::Flipped270, Transform::Flipped180) => Transform::_270,
            (Transform::Flipped270, Transform::Flipped270) => Transform::Normal,
        };

        let dst_src_size = image_transform.transform_size(src.size);
        let scale = dst_src_size.to_f64() / dst.size.to_f64();

        let (src_x, src_y, dest_x, dest_y, width, height, transform) =
            if image_transform != Transform::Normal || scale != Scale::from(1f64) {
                let mut transform = pixman::Transform::identity();

                // compensate for offset
                transform = transform.translate(-dst.loc.x, -dst.loc.y, false).unwrap();

                // scale to src image size

                transform = transform.scale(scale.x, scale.y, false).unwrap();

                let (cos, sin, x, y) = match image_transform {
                    Transform::Normal => (1, 0, 0, 0),
                    Transform::_90 => (0, -1, 0, src.size.h),
                    Transform::_180 => (-1, 0, src.size.w, src.size.h),
                    Transform::_270 => (0, 1, src.size.w, 0),
                    Transform::Flipped => (1, 0, src.size.w, 0),
                    Transform::Flipped90 => (0, -1, src.size.w, src.size.h),
                    Transform::Flipped180 => (-1, 0, 0, src.size.h),
                    Transform::Flipped270 => (0, 1, 0, 0),
                };

                // rotation
                transform = transform.rotate(cos, sin, false).unwrap();

                // flipped
                if image_transform.flipped() {
                    transform = transform.scale(-1, 1, false).unwrap();
                }

                // Compensate rotation and flipped
                transform = transform.translate(x, y, false).unwrap();

                // crop src
                transform = transform.translate(src.loc.x, src.loc.y, false).unwrap();

                (
                    0,
                    0,
                    0,
                    0,
                    target_image.width() as i32,
                    target_image.height() as i32,
                    Some(transform),
                )
            } else {
                (
                    src.loc.x, src.loc.y, dst.loc.x, dst.loc.y, src.size.w, src.size.h, None,
                )
            };

        let filter = match self.renderer.upscale_filter {
            TextureFilter::Linear => Filter::Bilinear,
            TextureFilter::Nearest => Filter::Nearest,
        };

        if let Some(transform) = transform {
            src_image.set_transform(transform).unwrap();
        } else {
            src_image.clear_transform().unwrap();
        }
        src_image.set_filter(filter, &[]).unwrap();
        src_image.set_repeat(Repeat::None);

        let mut clip_region =
            pixman::Region32::init_rect(0, 0, self.output_size.w as u32, self.output_size.h as u32)
                .intersect(&pixman::Region32::init_rect(
                    dst.loc.x,
                    dst.loc.y,
                    dst.size.w as u32,
                    dst.size.h as u32,
                ));

        let damage_boxes = damage
            .iter()
            .copied()
            .map(|mut rect| {
                rect.loc += dst_loc;

                let rect = self.transform.transform_rect_in(rect, &self.size);

                let p1 = rect.loc;
                let p2 = p1 + rect.size.to_point();
                pixman::Box32 {
                    x1: p1.x,
                    y1: p1.y,
                    x2: p2.x,
                    y2: p2.y,
                }
            })
            .collect::<Vec<_>>();
        let damage_region = pixman::Region32::init_rects(&damage_boxes);
        clip_region = clip_region.intersect(&damage_region);

        target_image.set_clip_region32(Some(&clip_region)).unwrap();

        let has_alpha = DrmFourcc::try_from(src_image.format())
            .ok()
            .map(has_alpha)
            .unwrap_or(true);

        let op = if has_alpha {
            Operation::Over
        } else {
            Operation::Src
        };

        target_image.composite32(
            op, &src_image, None, src_x, src_y, 0, 0, dest_x, dest_y, width, height,
        );

        if self.renderer.debug_flags.contains(DebugFlags::TINT) {
            profiling::scope!("tint");
            target_image.composite32(
                Operation::Over,
                &self.renderer.tint,
                None,
                0,
                0,
                0,
                0,
                0,
                0,
                target_image.width() as i32,
                target_image.height() as i32,
            );
        }

        target_image.set_clip_region32(None).unwrap();

        src_image.clear_transform().unwrap();

        Ok(())
    }

    fn transformation(&self) -> Transform {
        self.transform
    }

    #[profiling::function]
    fn finish(mut self) -> Result<super::sync::SyncPoint, Self::Error> {
        self.finish_internal()
    }
}

impl<'frame> PixmanFrame<'frame> {
    #[profiling::function]
    fn finish_internal(&mut self) -> Result<SyncPoint, PixmanError> {
        if self.finished.swap(true, Ordering::SeqCst) {
            return Ok(SyncPoint::signaled());
        }

        if let Some(PixmanTarget::Image(image)) = self.renderer.target.as_ref() {
            if let Some(mapping) = image.dmabuf.as_ref() {
                mapping
                    .dmabuf
                    .sync(DmaBufSyncFlags::END | DmaBufSyncFlags::READ | DmaBufSyncFlags::WRITE)
                    .map_err(PixmanError::Access)?;
            }
        }

        Ok(SyncPoint::signaled())
    }
}

impl<'frame> Drop for PixmanFrame<'frame> {
    fn drop(&mut self) {
        match self.finish_internal() {
            Ok(sync) => {
                sync.wait();
            }
            Err(err) => {
                warn!("Ignored error finishing PixmanFrame on drop: {}", err);
            }
        }
    }
}

/// A renderer utilizing pixman
#[derive(Debug)]
pub struct PixmanRenderer {
    target: Option<PixmanTarget>,
    downscale_filter: TextureFilter,
    upscale_filter: TextureFilter,
    debug_flags: DebugFlags,
    tint: pixman::Solid<'static>,
}

impl PixmanRenderer {
    /// Creates a new pixman renderer
    pub fn new() -> Result<Self, PixmanError> {
        let tint = pixman::Solid::new([0.0, 0.3, 0.0, 0.2]).map_err(|_| PixmanError::Unsupported)?;
        Ok(Self {
            target: None,
            downscale_filter: TextureFilter::Linear,
            upscale_filter: TextureFilter::Linear,
            debug_flags: DebugFlags::empty(),
            tint,
        })
    }
}

impl PixmanRenderer {
    fn import_dmabuf(&mut self, dmabuf: Dmabuf, writeable: bool) -> Result<PixmanImage, PixmanError> {
        if dmabuf.num_planes() != 1 {
            return Err(PixmanError::UnsupportedNumberOfPlanes);
        }

        let size = dmabuf.size();
        let format = dmabuf.format();

        if format.modifier != DrmModifier::Linear {
            return Err(PixmanError::UnsupportedModifier(format.modifier));
        }
        let format = pixman::FormatCode::try_from(format.code)
            .map_err(|_| PixmanError::UnsupportedPixelFormat(format.code))?;

        let dmabuf_mapping = if writeable {
            dmabuf.map_writable()
        } else {
            dmabuf.map_readable()
        }?;
        let stride = dmabuf.strides().next().expect("already checked") as usize;
        let expected_len = stride * size.h as usize;

        if dmabuf_mapping.length() < expected_len {
            return Err(PixmanError::IncompleteBuffer {
                expected: expected_len,
                actual: dmabuf_mapping.length(),
            });
        }

        let image: Image<'_, '_> = unsafe {
            pixman::Image::from_raw_mut(
                format,
                size.w as usize,
                size.h as usize,
                dmabuf_mapping.ptr() as *mut u32,
                stride,
                false,
            )
        }
        .map_err(|_| PixmanError::ImportFailed)?;

        Ok(PixmanImage {
            buffer: None,
            dmabuf: Some(PixmanDmabufMapping {
                dmabuf,
                _mapping: dmabuf_mapping,
            }),
            image,
            _flipped: false,
        })
    }
}

impl Renderer for PixmanRenderer {
    type Error = PixmanError;

    type TextureId = PixmanTexture;

    type Frame<'frame> = PixmanFrame<'frame>;

    fn id(&self) -> usize {
        0
    }

    fn downscale_filter(&mut self, filter: TextureFilter) -> Result<(), Self::Error> {
        self.downscale_filter = filter;
        Ok(())
    }

    fn upscale_filter(&mut self, filter: TextureFilter) -> Result<(), Self::Error> {
        self.upscale_filter = filter;
        Ok(())
    }

    fn set_debug_flags(&mut self, flags: DebugFlags) {
        self.debug_flags = flags;
    }

    fn debug_flags(&self) -> DebugFlags {
        self.debug_flags
    }

    #[profiling::function]
    fn render(
        &mut self,
        output_size: Size<i32, Physical>,
        dst_transform: Transform,
    ) -> Result<PixmanFrame<'_>, Self::Error> {
        if let Some(PixmanTarget::Image(image)) = self.target.as_ref() {
            if let Some(mapping) = image.dmabuf.as_ref() {
                mapping
                    .dmabuf
                    .sync(DmaBufSyncFlags::START | DmaBufSyncFlags::READ | DmaBufSyncFlags::WRITE)
                    .map_err(PixmanError::Access)?;
            }
        }
        Ok(PixmanFrame {
            renderer: self,

            transform: dst_transform,
            output_size,
            size: dst_transform.transform_size(output_size),

            finished: AtomicBool::new(false),
        })
    }
}

impl ImportMem for PixmanRenderer {
    #[profiling::function]
    fn import_memory(
        &mut self,
        data: &[u8],
        format: drm_fourcc::DrmFourcc,
        size: Size<i32, BufferCoords>,
        flipped: bool,
    ) -> Result<<Self as Renderer>::TextureId, <Self as Renderer>::Error> {
        let format =
            pixman::FormatCode::try_from(format).map_err(|_| PixmanError::UnsupportedPixelFormat(format))?;
        let image = pixman::Image::new(format, size.w as usize, size.h as usize, false)
            .map_err(|_| PixmanError::Unsupported)?;
        image.data().copy_from_slice(unsafe {
            std::slice::from_raw_parts(
                data.as_ptr() as *const u32,
                data.len() / std::mem::size_of::<u32>(),
            )
        });
        Ok(PixmanTexture(Rc::new(PixmanImage {
            buffer: None,
            dmabuf: None,
            image,
            _flipped: flipped,
        })))
    }

    #[profiling::function]
    fn update_memory(
        &mut self,
        texture: &<Self as Renderer>::TextureId,
        data: &[u8],
        region: Rectangle<i32, BufferCoords>,
    ) -> Result<(), <Self as Renderer>::Error> {
        if texture.0.buffer.is_some() || texture.0.dmabuf.is_some() {
            return Err(PixmanError::ImportFailed);
        }

        let stride = texture.0.image.stride();
        let expected_len = stride * texture.0.image.height();

        if data.len() < expected_len {
            return Err(PixmanError::IncompleteBuffer {
                expected: expected_len,
                actual: data.len(),
            });
        }

        let src_image = unsafe {
            // SAFETY: As we are never going to write to this image
            // it is safe to cast the passed slice to a mut pointer
            pixman::Image::from_raw_mut(
                texture.0.image.format(),
                texture.0.image.width(),
                texture.0.image.height(),
                data.as_ptr() as *mut _,
                stride,
                false,
            )
        }
        .map_err(|_| PixmanError::ImportFailed)?;

        texture.0.image.composite32(
            Operation::Src,
            &src_image,
            None,
            region.loc.x,
            region.loc.y,
            0,
            0,
            region.loc.x,
            region.loc.x,
            region.size.w,
            region.size.h,
        );

        Ok(())
    }

    fn mem_formats(&self) -> Box<dyn Iterator<Item = drm_fourcc::DrmFourcc>> {
        Box::new(SUPPORTED_FORMATS.iter().copied())
    }
}

/// Texture mapping of a pixman texture
#[derive(Debug)]
pub struct PixmanMapping(pixman::Image<'static, 'static>);

impl Texture for PixmanMapping {
    fn width(&self) -> u32 {
        self.0.width() as u32
    }

    fn height(&self) -> u32 {
        self.0.height() as u32
    }

    fn format(&self) -> Option<DrmFourcc> {
        DrmFourcc::try_from(self.0.format()).ok()
    }
}

impl TextureMapping for PixmanMapping {
    fn flipped(&self) -> bool {
        false
    }
}

impl ExportMem for PixmanRenderer {
    type TextureMapping = PixmanMapping;

    #[profiling::function]
    fn copy_framebuffer(
        &mut self,
        region: Rectangle<i32, BufferCoords>,
        format: DrmFourcc,
    ) -> Result<Self::TextureMapping, <Self as Renderer>::Error> {
        let format_code =
            pixman::FormatCode::try_from(format).map_err(|_| PixmanError::UnsupportedPixelFormat(format))?;
        let copy_image =
            pixman::Image::new(format_code, region.size.w as usize, region.size.h as usize, false)
                .map_err(|_| PixmanError::Unsupported)?;

        if let Some(target) = self.target.as_ref() {
            let _accessor = if let PixmanTarget::Image(image) = target {
                Some(image.accessor()?)
            } else {
                None
            };
            copy_image.composite32(
                Operation::Src,
                target.image(),
                None,
                region.loc.x,
                region.loc.y,
                0,
                0,
                0,
                0,
                region.size.w,
                region.size.h,
            );
        }

        Ok(PixmanMapping(copy_image))
    }

    #[profiling::function]
    fn copy_texture(
        &mut self,
        texture: &Self::TextureId,
        region: Rectangle<i32, BufferCoords>,
        format: DrmFourcc,
    ) -> Result<Self::TextureMapping, Self::Error> {
        let accessor = texture.accessor()?;
        let format_code =
            pixman::FormatCode::try_from(format).map_err(|_| PixmanError::UnsupportedPixelFormat(format))?;
        let copy_image =
            pixman::Image::new(format_code, region.size.w as usize, region.size.h as usize, false)
                .map_err(|_| PixmanError::Unsupported)?;
        copy_image.composite32(
            Operation::Src,
            accessor.image,
            None,
            region.loc.x,
            region.loc.y,
            0,
            0,
            0,
            0,
            region.size.w,
            region.size.h,
        );
        Ok(PixmanMapping(copy_image))
    }

    #[profiling::function]
    fn map_texture<'a>(
        &mut self,
        texture_mapping: &'a Self::TextureMapping,
    ) -> Result<&'a [u8], <Self as Renderer>::Error> {
        let data = texture_mapping.0.data();
        Ok(unsafe {
            std::slice::from_raw_parts(
                data.as_ptr() as *const u8,
                std::mem::size_of_val(data),
            )
        })
    }
}

#[cfg(all(
    feature = "wayland_frontend",
    feature = "backend_egl",
    feature = "use_system_lib"
))]
impl ImportEgl for PixmanRenderer {
    fn bind_wl_display(
        &mut self,
        _display: &wayland_server::DisplayHandle,
    ) -> Result<(), crate::backend::egl::Error> {
        Err(crate::backend::egl::Error::NoEGLDisplayBound)
    }

    fn unbind_wl_display(&mut self) {}

    fn egl_reader(&self) -> Option<&crate::backend::egl::display::EGLBufferReader> {
        None
    }

    fn import_egl_buffer(
        &mut self,
        _buffer: &wl_buffer::WlBuffer,
        _surface: Option<&SurfaceData>,
        _damage: &[Rectangle<i32, BufferCoords>],
    ) -> Result<<Self as Renderer>::TextureId, <Self as Renderer>::Error> {
        Err(PixmanError::Unsupported)
    }
}

#[cfg(feature = "wayland_frontend")]
impl ImportMemWl for PixmanRenderer {
    #[profiling::function]
    fn import_shm_buffer(
        &mut self,
        buffer: &wl_buffer::WlBuffer,
        _surface: Option<&SurfaceData>,
        _damage: &[Rectangle<i32, BufferCoords>],
    ) -> Result<PixmanTexture, PixmanError> {
        let image = shm::with_buffer_contents(buffer, |ptr, _, data| {
            let format = FormatCode::try_from(
                shm::shm_format_to_fourcc(data.format)
                    .ok_or(PixmanError::UnsupportedWlPixelFormat(data.format))?,
            )
            .map_err(|_| PixmanError::UnsupportedWlPixelFormat(data.format))?;
            let image = unsafe {
                // SAFETY: We guarantee that this image is only used for reading,
                // so it is safe to cast the ptr to *mut
                Image::from_raw_mut(
                    format,
                    data.width as usize,
                    data.height as usize,
                    ptr.offset(data.offset as isize) as *mut _,
                    data.stride as usize,
                    false,
                )
            }
            .map_err(|_| PixmanError::ImportFailed)?;
            std::result::Result::<_, PixmanError>::Ok(image)
        })??;
        Ok(PixmanTexture(Rc::new(PixmanImage {
            buffer: Some(buffer.downgrade()),
            dmabuf: None,
            image,
            _flipped: false,
        })))
    }
}

impl ImportDma for PixmanRenderer {
    #[profiling::function]
    fn import_dmabuf(
        &mut self,
        dmabuf: &Dmabuf,
        _damage: Option<&[Rectangle<i32, BufferCoords>]>,
    ) -> Result<<Self as Renderer>::TextureId, <Self as Renderer>::Error> {
        Ok(PixmanTexture(Rc::new(self.import_dmabuf(dmabuf.clone(), false)?)))
    }

    fn dmabuf_formats(&self) -> Box<dyn Iterator<Item = drm_fourcc::DrmFormat>> {
        lazy_static::lazy_static! {
            static ref DMABUF_FORMATS: Vec<DrmFormat> = {
                SUPPORTED_FORMATS.iter().copied().map(|code| DrmFormat {
                    code,
                    modifier: drm_fourcc::DrmModifier::Linear,
                }).collect()
            };
        }
        Box::new(DMABUF_FORMATS.iter().copied())
    }
}

impl ImportDmaWl for PixmanRenderer {}

impl Unbind for PixmanRenderer {
    #[profiling::function]
    fn unbind(&mut self) -> Result<(), <Self as Renderer>::Error> {
        self.target = None;
        Ok(())
    }
}

impl Bind<Dmabuf> for PixmanRenderer {
    #[profiling::function]
    fn bind(&mut self, target: Dmabuf) -> Result<(), <Self as Renderer>::Error> {
        self.target = Some(PixmanTarget::Image(self.import_dmabuf(target, true)?));
        Ok(())
    }

    fn supported_formats(&self) -> Option<HashSet<DrmFormat>> {
        lazy_static::lazy_static! {
            static ref DMABUF_FORMATS: HashSet<DrmFormat> = {
                SUPPORTED_FORMATS.iter().copied().map(|code| DrmFormat {
                    code,
                    modifier: DrmModifier::Linear,
                }).collect()
            };
        }
        Some(DMABUF_FORMATS.clone())
    }
}
