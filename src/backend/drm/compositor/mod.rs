//! Specialized renderer utilizing drm planes with support for direct scan-out

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::{Arc, Mutex},
};

use drm::control::{connector, crtc, framebuffer, plane, Mode};
use drm_fourcc::{DrmFormat, DrmFourcc, DrmModifier};
use indexmap::IndexMap;
use wayland_server::{protocol::wl_buffer::WlBuffer, Resource};

use crate::{
    backend::{
        allocator::{
            dmabuf::{AsDmabuf, Dmabuf},
            Allocator, Buffer, Slot, Swapchain,
        },
        drm::{DrmError, PlaneDamageClips},
        renderer::{
            buffer_y_inverted,
            damage::{DamageTrackedRenderer, DamageTrackedRendererError, OutputNoMode},
            element::{
                utils::{Relocate, RelocateRenderElement},
                Element, Id, RenderElement, RenderElementPresentationState, RenderElementState,
                RenderElementStates, UnderlyingStorage,
            },
            utils::{CommitCounter, DamageTracker, DamageTrackerSnapshot},
            Bind, Blit, DebugFlags, Frame as RendererFrame, Renderer, Texture,
        },
        SwapBuffersError,
    },
    output::Output,
    utils::{Buffer as BufferCoords, Physical, Point, Rectangle, Scale, Size, Transform},
};

use super::{DrmDeviceFd, DrmSurface, PlaneClaim, PlaneInfo, Planes};

mod elements;
#[cfg(feature = "backend_gbm")]
pub mod gbm;

use elements::*;

impl RenderElementState {
    pub(crate) fn zero_copy(visible_area: usize) -> Self {
        RenderElementState {
            visible_area,
            presentation_state: RenderElementPresentationState::ZeroCopy,
        }
    }
}

#[derive(Debug)]
enum ScanoutBuffer<B: Buffer> {
    Wayland(crate::backend::renderer::utils::Buffer),
    Swapchain(Slot<B>),
}

impl<B: Buffer> From<UnderlyingStorage> for ScanoutBuffer<B> {
    fn from(storage: UnderlyingStorage) -> Self {
        match storage {
            UnderlyingStorage::Wayland(buffer) => Self::Wayland(buffer),
        }
    }
}

struct DrmScanoutBuffer<B: Buffer, F: AsRef<framebuffer::Handle>> {
    buffer: ScanoutBuffer<B>,
    fb: OwnedFramebuffer<F>,
}

impl<B, F> std::fmt::Debug for DrmScanoutBuffer<B, F>
where
    B: Buffer + std::fmt::Debug,
    F: AsRef<framebuffer::Handle> + std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DrmScanoutBuffer")
            .field("buffer", &self.buffer)
            .field("fb", &self.fb)
            .finish()
    }
}

impl<B: Buffer, F: AsRef<framebuffer::Handle>> AsRef<framebuffer::Handle> for DrmScanoutBuffer<B, F> {
    fn as_ref(&self) -> &drm::control::framebuffer::Handle {
        self.fb.as_ref()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum ElementFramebufferCacheKey {
    Wayland(wayland_server::Weak<WlBuffer>),
}

impl ElementFramebufferCacheKey {
    fn is_alive(&self) -> bool {
        match self {
            ElementFramebufferCacheKey::Wayland(buffer) => buffer.upgrade().is_ok(),
        }
    }
}

impl From<&UnderlyingStorage> for ElementFramebufferCacheKey {
    fn from(storage: &UnderlyingStorage) -> Self {
        match storage {
            UnderlyingStorage::Wayland(buffer) => Self::Wayland(buffer.downgrade()),
        }
    }
}

#[derive(Debug)]
struct ElementFramebufferCache<B>
where
    B: AsRef<framebuffer::Handle>,
{
    /// Cache for framebuffer handles per cache key (e.g. wayland buffer)
    fb_cache: HashMap<ElementFramebufferCacheKey, Option<OwnedFramebuffer<B>>>,
}

impl<B> ElementFramebufferCache<B>
where
    B: AsRef<framebuffer::Handle>,
{
    fn get(&self, buffer: &UnderlyingStorage) -> Option<Option<OwnedFramebuffer<B>>> {
        self.fb_cache
            .get(&ElementFramebufferCacheKey::from(buffer))
            .cloned()
    }

    fn insert(&mut self, buffer: &UnderlyingStorage, fb: Option<OwnedFramebuffer<B>>) {
        self.fb_cache.insert(ElementFramebufferCacheKey::from(buffer), fb);
    }

    fn cleanup(&mut self) {
        self.fb_cache.retain(|key, _| key.is_alive());
    }
}

impl<B> Clone for ElementFramebufferCache<B>
where
    B: AsRef<framebuffer::Handle>,
{
    fn clone(&self) -> Self {
        Self {
            fb_cache: self.fb_cache.clone(),
        }
    }
}

impl<B> Default for ElementFramebufferCache<B>
where
    B: AsRef<framebuffer::Handle>,
{
    fn default() -> Self {
        Self {
            fb_cache: Default::default(),
        }
    }
}

#[derive(Debug)]
struct PlaneConfig<B> {
    pub src: Rectangle<f64, BufferCoords>,
    pub dst: Rectangle<i32, Physical>,
    pub transform: Transform,
    pub damage_clips: Option<PlaneDamageClips>,
    pub buffer: Owned<B>,
    pub plane_claim: PlaneClaim,
}

impl<B> Clone for PlaneConfig<B> {
    fn clone(&self) -> Self {
        Self {
            src: self.src,
            dst: self.dst,
            transform: self.transform,
            damage_clips: self.damage_clips.clone(),
            buffer: self.buffer.clone(),
            plane_claim: self.plane_claim.clone(),
        }
    }
}

#[derive(Debug)]
struct PlaneState<B> {
    skip: bool,
    element_state: Option<(Id, CommitCounter)>,
    config: Option<PlaneConfig<B>>,
}

impl<B> Default for PlaneState<B> {
    fn default() -> Self {
        Self {
            skip: true,
            element_state: Default::default(),
            config: Default::default(),
        }
    }
}

impl<B> PlaneState<B> {
    fn buffer(&self) -> Option<&B> {
        self.config.as_ref().map(|config| &*config.buffer)
    }
}

impl<B> Clone for PlaneState<B> {
    fn clone(&self) -> Self {
        Self {
            skip: self.skip,
            element_state: self.element_state.clone(),
            config: self.config.clone(),
        }
    }
}

#[derive(Debug)]
struct FrameState<B: AsRef<framebuffer::Handle>> {
    planes: HashMap<plane::Handle, PlaneState<B>>,
}

impl<B: AsRef<framebuffer::Handle>> FrameState<B> {
    fn is_assigned(&self, handle: plane::Handle) -> bool {
        self.planes
            .get(&handle)
            .map(|config| config.config.is_some())
            .unwrap_or(false)
    }

    fn overlaps(&self, handle: plane::Handle, element_geometry: Rectangle<i32, Physical>) -> bool {
        self.planes
            .get(&handle)
            .and_then(|state| {
                state
                    .config
                    .as_ref()
                    .map(|config| config.dst.overlaps(element_geometry))
            })
            .unwrap_or(false)
    }

    fn plane_state(&self, handle: plane::Handle) -> Option<&PlaneState<B>> {
        self.planes.get(&handle)
    }

    fn plane_state_mut(&mut self, handle: plane::Handle) -> Option<&mut PlaneState<B>> {
        self.planes.get_mut(&handle)
    }

    fn plane_buffer(&self, handle: plane::Handle) -> Option<&B> {
        self.plane_state(handle)
            .and_then(|state| state.config.as_ref().map(|config| &*config.buffer))
    }
}

#[derive(Debug)]
struct Owned<B>(Rc<B>);

impl<B> std::ops::Deref for Owned<B> {
    type Target = B;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<B> From<B> for Owned<B> {
    fn from(outer: B) -> Self {
        Self(Rc::new(outer))
    }
}

impl<B> AsRef<framebuffer::Handle> for Owned<B>
where
    B: AsRef<framebuffer::Handle>,
{
    fn as_ref(&self) -> &framebuffer::Handle {
        (*self.0).as_ref()
    }
}

impl<B> Clone for Owned<B> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<B: AsRef<framebuffer::Handle>> FrameState<B> {
    fn from_planes(planes: &Planes) -> Self {
        let cursor_plane_count = usize::from(planes.cursor.is_some());
        let mut tmp = HashMap::with_capacity(planes.overlay.len() + cursor_plane_count + 1);
        tmp.insert(planes.primary.handle, PlaneState::default());
        if let Some(info) = planes.cursor.as_ref() {
            tmp.insert(info.handle, PlaneState::default());
        }
        tmp.extend(
            planes
                .overlay
                .iter()
                .map(|info| (info.handle, PlaneState::default())),
        );

        FrameState { planes: tmp }
    }
}

impl<B: AsRef<framebuffer::Handle>> FrameState<B> {
    fn test_state(
        &mut self,
        surface: &DrmSurface,
        plane: plane::Handle,
        state: PlaneState<B>,
        allow_modeset: bool,
    ) -> Result<bool, DrmError> {
        let current_config = match self.planes.get_mut(&plane) {
            Some(config) => config,
            None => return Ok(false),
        };
        let backup = current_config.clone();
        *current_config = state;

        let res = surface.test_state(
            self.planes
                .iter()
                // Filter out any skipped planes
                .filter(|(_, state)| !state.skip)
                .map(|(handle, state)| super::surface::PlaneState {
                    handle: *handle,
                    config: state.config.as_ref().map(|config| super::PlaneConfig {
                        src: config.src,
                        dst: config.dst,
                        transform: config.transform,
                        damage_clips: config.damage_clips.as_ref().map(|d| d.blob()),
                        fb: *config.buffer.as_ref(),
                    }),
                }),
            allow_modeset,
        );

        match res {
            Ok(true) => Ok(true),
            Ok(false) => {
                self.planes.insert(plane, backup);
                Ok(false)
            }
            Err(err) => {
                self.planes.insert(plane, backup);
                Err(err)
            }
        }
    }

    fn commit(&self, surface: &DrmSurface, event: bool) -> Result<(), crate::backend::drm::error::Error> {
        surface.commit(
            self.planes
                .iter()
                // Filter out any skipped planes
                .filter(|(_, state)| !state.skip)
                .map(|(handle, state)| super::surface::PlaneState {
                    handle: *handle,
                    config: state.config.as_ref().map(|config| super::PlaneConfig {
                        src: config.src,
                        dst: config.dst,
                        transform: config.transform,
                        damage_clips: config.damage_clips.as_ref().map(|d| d.blob()),
                        fb: *config.buffer.as_ref(),
                    }),
                }),
            event,
        )
    }

    fn page_flip(&self, surface: &DrmSurface, event: bool) -> Result<(), crate::backend::drm::error::Error> {
        surface.page_flip(
            self.planes
                .iter()
                // Filter out any skipped planes
                .filter(|(_, state)| !state.skip)
                .map(|(handle, state)| super::surface::PlaneState {
                    handle: *handle,
                    config: state.config.as_ref().map(|config| super::PlaneConfig {
                        src: config.src,
                        dst: config.dst,
                        transform: config.transform,
                        damage_clips: config.damage_clips.as_ref().map(|d| d.blob()),
                        fb: *config.buffer.as_ref(),
                    }),
                }),
            event,
        )
    }
}

/// Possible buffers to export as a framebuffer using [`ExportFramebuffer`]
#[derive(Debug)]
pub enum ExportBuffer<'a, B: Buffer> {
    /// A wayland buffer
    Wayland(&'a WlBuffer),
    /// A [`Allocator`] buffer
    Allocator(&'a B),
}

impl<'a, B: Buffer> From<&'a UnderlyingStorage> for ExportBuffer<'a, B> {
    fn from(storage: &'a UnderlyingStorage) -> Self {
        match storage {
            UnderlyingStorage::Wayland(buffer) => Self::Wayland(buffer),
        }
    }
}

/// Export a [`ExportBuffer`] as a framebuffer
pub trait ExportFramebuffer<B: Buffer>
where
    B: Buffer,
{
    /// Type of the framebuffer
    type Framebuffer: AsRef<framebuffer::Handle>;

    /// Type of the error
    type Error: std::error::Error;

    /// Add a framebuffer for the specified buffer
    fn add_framebuffer(
        &self,
        drm: &DrmDeviceFd,
        buffer: ExportBuffer<'_, B>,
    ) -> Result<Option<Self::Framebuffer>, Self::Error>;
}

impl<F, B> ExportFramebuffer<B> for Arc<Mutex<F>>
where
    F: ExportFramebuffer<B>,
    B: Buffer,
{
    type Framebuffer = <F as ExportFramebuffer<B>>::Framebuffer;
    type Error = <F as ExportFramebuffer<B>>::Error;

    fn add_framebuffer(
        &self,
        drm: &DrmDeviceFd,
        buffer: ExportBuffer<'_, B>,
    ) -> Result<Option<Self::Framebuffer>, Self::Error> {
        let guard = self.lock().unwrap();
        guard.add_framebuffer(drm, buffer)
    }
}

impl<F, B> ExportFramebuffer<B> for Rc<RefCell<F>>
where
    F: ExportFramebuffer<B>,
    B: Buffer,
{
    type Framebuffer = <F as ExportFramebuffer<B>>::Framebuffer;
    type Error = <F as ExportFramebuffer<B>>::Error;

    fn add_framebuffer(
        &self,
        drm: &DrmDeviceFd,
        buffer: ExportBuffer<'_, B>,
    ) -> Result<Option<Self::Framebuffer>, Self::Error> {
        self.borrow().add_framebuffer(drm, buffer)
    }
}

type Frame<A, F> = FrameState<
    DrmScanoutBuffer<
        <A as Allocator>::Buffer,
        <F as ExportFramebuffer<<A as Allocator>::Buffer>>::Framebuffer,
    >,
>;

type FrameErrorType<A, F> = FrameError<
    <A as Allocator>::Error,
    <<A as Allocator>::Buffer as AsDmabuf>::Error,
    <F as ExportFramebuffer<<A as Allocator>::Buffer>>::Error,
>;

type FrameResult<T, A, F> = Result<T, FrameErrorType<A, F>>;

type RenderFrameErrorType<A, F, R> = RenderFrameError<
    <A as Allocator>::Error,
    <<A as Allocator>::Buffer as AsDmabuf>::Error,
    <F as ExportFramebuffer<<A as Allocator>::Buffer>>::Error,
    R,
>;

/// Defines the element for the primary plane
pub enum PrimaryPlaneElement<'a, B: Buffer, E> {
    /// A slot from the swapchain was used for rendering
    /// the primary plane
    Swapchain {
        /// The slot from the swapchain
        slot: &'a Slot<B>,
        /// The transform applied during rendering
        transform: Transform,
        /// The damage on the primary plane
        damage: DamageTrackerSnapshot<i32, BufferCoords>,
    },
    /// An element has been assigned for direct scan-out
    Element(&'a E),
}

impl<'a, B: Buffer + std::fmt::Debug, E: std::fmt::Debug> std::fmt::Debug for PrimaryPlaneElement<'a, B, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Swapchain {
                slot,
                transform,
                damage,
            } => f
                .debug_struct("Swapchain")
                .field("slot", slot)
                .field("transform", transform)
                .field("damage", damage)
                .finish(),
            Self::Element(arg0) => f.debug_tuple("Element").field(arg0).finish(),
        }
    }
}

/// Result for [`DrmCompositor::render_frame`]
pub struct RenderFrameResult<'a, B: Buffer, E> {
    /// Damage of this frame
    pub damage: Option<Vec<Rectangle<i32, Physical>>>,
    /// The render element states of this frame
    pub states: RenderElementStates,
    /// Element for the primary plane
    pub primary_element: PrimaryPlaneElement<'a, B, E>,
    /// Overlay elements in front to back order
    pub overlay_elements: Vec<&'a E>,
    /// Optional cursor plane element
    ///
    /// If set always above all other elements
    pub cursor_element: Option<&'a E>,

    primary_plane_element_id: Id,
}

struct SwapchainElement<'a, B: Buffer> {
    id: Id,
    slot: &'a Slot<B>,
    transform: Transform,
    damage: &'a DamageTrackerSnapshot<i32, BufferCoords>,
}

impl<'a, B: Buffer> Element for SwapchainElement<'a, B> {
    fn id(&self) -> &Id {
        &self.id
    }

    fn current_commit(&self) -> CommitCounter {
        self.damage.current_commit()
    }

    fn src(&self) -> Rectangle<f64, BufferCoords> {
        Rectangle::from_loc_and_size((0, 0), self.slot.size()).to_f64()
    }

    fn geometry(&self, _scale: Scale<f64>) -> Rectangle<i32, Physical> {
        Rectangle::from_loc_and_size(
            (0, 0),
            self.slot.size().to_logical(1, self.transform).to_physical(1),
        )
    }

    fn transform(&self) -> Transform {
        self.transform
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> Vec<Rectangle<i32, Physical>> {
        self.damage
            .damage_since(commit)
            .map(|d| {
                d.into_iter()
                    .map(|d| d.to_logical(1, self.transform, &self.slot.size()).to_physical(1))
                    .collect()
            })
            .unwrap_or_else(|| vec![self.geometry(scale)])
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> Vec<Rectangle<i32, Physical>> {
        vec![self.geometry(scale)]
    }
}

enum FrameResultDamageElement<'a, E, B: Buffer> {
    Element(&'a E),
    Swapchain(SwapchainElement<'a, B>),
}

impl<'a, E, B> Element for FrameResultDamageElement<'a, E, B>
where
    E: Element,
    B: Buffer,
{
    fn id(&self) -> &Id {
        match self {
            FrameResultDamageElement::Element(e) => e.id(),
            FrameResultDamageElement::Swapchain(e) => e.id(),
        }
    }

    fn current_commit(&self) -> CommitCounter {
        match self {
            FrameResultDamageElement::Element(e) => e.current_commit(),
            FrameResultDamageElement::Swapchain(e) => e.current_commit(),
        }
    }

    fn src(&self) -> Rectangle<f64, BufferCoords> {
        match self {
            FrameResultDamageElement::Element(e) => e.src(),
            FrameResultDamageElement::Swapchain(e) => e.src(),
        }
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        match self {
            FrameResultDamageElement::Element(e) => e.geometry(scale),
            FrameResultDamageElement::Swapchain(e) => e.geometry(scale),
        }
    }

    fn location(&self, scale: Scale<f64>) -> Point<i32, Physical> {
        match self {
            FrameResultDamageElement::Element(e) => e.location(scale),
            FrameResultDamageElement::Swapchain(e) => e.location(scale),
        }
    }

    fn transform(&self) -> Transform {
        match self {
            FrameResultDamageElement::Element(e) => e.transform(),
            FrameResultDamageElement::Swapchain(e) => e.transform(),
        }
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> Vec<Rectangle<i32, Physical>> {
        match self {
            FrameResultDamageElement::Element(e) => e.damage_since(scale, commit),
            FrameResultDamageElement::Swapchain(e) => e.damage_since(scale, commit),
        }
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> Vec<Rectangle<i32, Physical>> {
        match self {
            FrameResultDamageElement::Element(e) => e.opaque_regions(scale),
            FrameResultDamageElement::Swapchain(e) => e.opaque_regions(scale),
        }
    }
}

/// Error for [`RenderFrameResult::blit_frame_result`]
#[derive(Debug, thiserror::Error)]
pub enum BlitFrameResultError<R, E> {
    /// A render error occurred
    #[error(transparent)]
    Rendering(R),
    /// A error occurred during exporting the buffer
    #[error(transparent)]
    Export(E),
}

impl<'a, B, E> RenderFrameResult<'a, B, E>
where
    B: Buffer,
{
    /// Get the damage of this frame for the specified dtr and age
    pub fn damage_from_age(
        &self,
        dtr: &mut DamageTrackedRenderer,
        age: usize,
        filter: impl IntoIterator<Item = Id>,
        log: slog::Logger,
    ) -> Result<(Option<Vec<Rectangle<i32, Physical>>>, RenderElementStates), OutputNoMode>
    where
        E: Element,
    {
        let filter_ids: HashSet<Id> = filter.into_iter().collect();

        let mut elements: Vec<FrameResultDamageElement<'_, E, B>> =
            Vec::with_capacity(usize::from(self.cursor_element.is_some()) + self.overlay_elements.len() + 1);
        if let Some(cursor) = self.cursor_element {
            if !filter_ids.contains(cursor.id()) {
                elements.push(FrameResultDamageElement::Element(cursor));
            }
        }

        elements.extend(
            self.overlay_elements
                .iter()
                .filter(|e| !filter_ids.contains(e.id()))
                .map(|e| FrameResultDamageElement::Element(*e)),
        );

        let primary_render_element = match &self.primary_element {
            PrimaryPlaneElement::Swapchain {
                slot,
                transform,
                damage,
            } => FrameResultDamageElement::Swapchain(SwapchainElement {
                id: self.primary_plane_element_id.clone(),
                transform: *transform,
                slot: *slot,
                damage,
            }),
            PrimaryPlaneElement::Element(e) => FrameResultDamageElement::Element(*e),
        };

        elements.push(primary_render_element);

        dtr.damage_output(age, &elements, log)
    }
}

impl<'a, B, E> RenderFrameResult<'a, B, E>
where
    B: Buffer + AsDmabuf,
    <B as AsDmabuf>::Error: std::fmt::Debug,
{
    /// Blit the frame result into a currently bound buffer
    #[allow(clippy::too_many_arguments)]
    pub fn blit_frame_result<R>(
        &self,
        size: impl Into<Size<i32, Physical>>,
        transform: Transform,
        scale: impl Into<Scale<f64>>,
        renderer: &mut R,
        damage: impl IntoIterator<Item = Rectangle<i32, Physical>>,
        filter: impl IntoIterator<Item = Id>,
        log: &slog::Logger,
    ) -> Result<(), BlitFrameResultError<<R as Renderer>::Error, <B as AsDmabuf>::Error>>
    where
        R: Renderer + Blit<Dmabuf>,
        <R as Renderer>::TextureId: 'static,
        E: Element + RenderElement<R>,
    {
        let size = size.into();
        let scale = scale.into();
        let filter_ids: HashSet<Id> = filter.into_iter().collect();
        let damage = damage.into_iter().collect::<Vec<_>>();

        // If we have no damage we can exit early
        if damage.is_empty() {
            return Ok(());
        }

        let mut opaque_regions: Vec<Rectangle<i32, Physical>> = Vec::new();

        let mut elements_to_render: Vec<&'a E> =
            Vec::with_capacity(usize::from(self.cursor_element.is_some()) + self.overlay_elements.len() + 1);

        if let Some(cursor_element) = self.cursor_element.as_ref() {
            if !filter_ids.contains(cursor_element.id()) {
                elements_to_render.push(*cursor_element);
                opaque_regions.extend(cursor_element.opaque_regions(scale));
            }
        }

        for element in self
            .overlay_elements
            .iter()
            .filter(|e| !filter_ids.contains(e.id()))
        {
            elements_to_render.push(element);
            opaque_regions.extend(element.opaque_regions(scale));
        }

        let primary_dmabuf = match &self.primary_element {
            PrimaryPlaneElement::Swapchain { slot, .. } => {
                let dmabuf = slot.export().map_err(BlitFrameResultError::Export)?;
                let size = dmabuf.size();
                let geometry = Rectangle::from_loc_and_size(
                    (0, 0),
                    size.to_logical(1, Transform::Normal).to_physical(1),
                );
                opaque_regions.push(geometry);
                Some((dmabuf, geometry))
            }
            PrimaryPlaneElement::Element(e) => {
                elements_to_render.push(*e);
                opaque_regions.extend(e.opaque_regions(scale));
                None
            }
        };

        let clear_damage = opaque_regions.iter().fold(damage.clone(), |damage, region| {
            damage
                .into_iter()
                .flat_map(|geo| geo.subtract_rect(*region))
                .collect::<Vec<_>>()
        });

        if !clear_damage.is_empty() {
            slog::trace!(log, "clearing frame damage {:#?}", clear_damage);

            let mut frame = renderer
                .render(size, transform)
                .map_err(BlitFrameResultError::Rendering)?;

            frame
                .clear([0f32, 0f32, 0f32, 1f32], &clear_damage)
                .map_err(BlitFrameResultError::Rendering)?;

            frame.finish().map_err(BlitFrameResultError::Rendering)?;
        }

        // first do the potential blit
        if let Some((dmabuf, geometry)) = primary_dmabuf {
            let blit_damage = damage
                .iter()
                .filter_map(|d| d.intersection(geometry))
                .collect::<Vec<_>>();

            slog::trace!(log, "blitting frame with damage: {:#?}", blit_damage);

            for rect in blit_damage {
                renderer
                    .blit_from(
                        dmabuf.clone(),
                        rect,
                        rect,
                        crate::backend::renderer::TextureFilter::Linear,
                    )
                    .map_err(BlitFrameResultError::Rendering)?;
            }
        }

        // then render the remaining elements if any
        if !elements_to_render.is_empty() {
            slog::trace!(log, "drawing {} frame element(s)", elements_to_render.len());

            let mut frame = renderer
                .render(size, transform)
                .map_err(BlitFrameResultError::Rendering)?;

            for element in elements_to_render.iter().rev() {
                let src = element.src();
                let dst = element.geometry(scale);
                let element_damage = damage
                    .iter()
                    .filter_map(|d| {
                        d.intersection(dst).map(|mut d| {
                            d.loc -= dst.loc;
                            d
                        })
                    })
                    .collect::<Vec<_>>();

                // no need to render without damage
                if element_damage.is_empty() {
                    continue;
                }

                slog::trace!(log, "drawing frame element with damage: {:#?}", element_damage);

                element
                    .draw(&mut frame, src, dst, &element_damage, log)
                    .map_err(BlitFrameResultError::Rendering)?;
            }

            frame.finish().map_err(BlitFrameResultError::Rendering)
        } else {
            Ok(())
        }
    }
}

impl<'a, B: Buffer + std::fmt::Debug, E: std::fmt::Debug> std::fmt::Debug for RenderFrameResult<'a, B, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RenderFrameResult")
            .field("damage", &self.damage)
            .field("states", &self.states)
            .field("primary_element", &self.primary_element)
            .field("overlay_elements", &self.overlay_elements)
            .field("cursor_element", &self.cursor_element)
            .finish()
    }
}

/// Specialized renderer utilizing drm planes with support for direct scan-out
#[derive(Debug)]
pub struct DrmCompositor<A, F, U>
where
    A: Allocator,
    F: ExportFramebuffer<A::Buffer>,
    <F as ExportFramebuffer<A::Buffer>>::Framebuffer: std::fmt::Debug + 'static,
{
    output: Output,
    surface: Arc<DrmSurface>,
    planes: Planes,
    damage_tracked_renderer: DamageTrackedRenderer,
    primary_plane_element_id: Id,
    primary_plane_damage_tracker: DamageTracker<i32, BufferCoords>,

    framebuffer_exporter: F,

    current_frame: Frame<A, F>,
    pending_frame: Option<(Frame<A, F>, U)>,
    queued_frame: Option<(Frame<A, F>, U)>,
    next_frame: Option<Frame<A, F>>,

    swapchain: Swapchain<A>,

    cursor_size: Size<i32, Physical>,
    cursor_swapchain: Swapchain<A>,
    cursor_damage_tracked_renderer: DamageTrackedRenderer,

    last_scale: Scale<f64>,
    last_transform: Transform,

    element_states: IndexMap<Id, ElementFramebufferCache<<F as ExportFramebuffer<A::Buffer>>::Framebuffer>>,

    debug_flags: DebugFlags,
}

const SUPPORTED_FORMATS: &[DrmFourcc] = &[DrmFourcc::Argb8888, DrmFourcc::Xrgb8888];

impl<A, F, U> DrmCompositor<A, F, U>
where
    A: Allocator + Clone,
    <A as Allocator>::Error: std::error::Error + Send + Sync,
    <A as Allocator>::Buffer: AsDmabuf,
    <A::Buffer as AsDmabuf>::Error: std::error::Error + Send + Sync + std::fmt::Debug,
    F: ExportFramebuffer<A::Buffer>,
    <F as ExportFramebuffer<A::Buffer>>::Framebuffer: std::fmt::Debug + 'static,
    <F as ExportFramebuffer<A::Buffer>>::Error: std::error::Error + Send + Sync,
{
    /// Initialize a new [`DrmCompositor`]
    #[allow(clippy::too_many_arguments)]
    pub fn new<L>(
        output: &Output,
        surface: DrmSurface,
        planes: Option<Planes>,
        mut allocator: A,
        framebuffer_exporter: F,
        renderer_formats: HashSet<DrmFormat>,
        cursor_size: Size<u32, BufferCoords>,
        log: L,
    ) -> FrameResult<Self, A, F>
    where
        L: Into<Option<::slog::Logger>>,
    {
        let log = crate::slog_or_fallback(log).new(slog::o!("backend" => "drm_render"));

        let mut error = None;
        let surface = Arc::new(surface);
        let planes = match planes {
            Some(planes) => planes,
            None => surface.planes()?,
        };

        let cursor_size = Size::from((cursor_size.w as i32, cursor_size.h as i32));
        let output_scale = output.current_scale().fractional_scale().into();
        let output_transform = output.current_transform();
        let damage_tracked_renderer = DamageTrackedRenderer::from_output(output);

        for format in SUPPORTED_FORMATS {
            slog::debug!(log, "Testing color format: {}", format);
            match Self::find_supported_format(
                surface.clone(),
                &planes,
                allocator.clone(),
                &framebuffer_exporter,
                renderer_formats.clone(),
                *format,
                log.clone(),
            ) {
                Ok((swapchain, current_frame)) => {
                    let cursor_swapchain = Swapchain::new(
                        allocator,
                        cursor_size.w as u32,
                        cursor_size.h as u32,
                        crate::backend::allocator::Fourcc::Argb8888,
                        vec![crate::backend::allocator::Modifier::Linear],
                    );

                    let cursor_damage_tracked_renderer =
                        DamageTrackedRenderer::new(cursor_size, output_scale, output_transform);

                    let drm_renderer = DrmCompositor {
                        primary_plane_element_id: Id::new(),
                        primary_plane_damage_tracker: DamageTracker::new(4),
                        current_frame,
                        pending_frame: None,
                        queued_frame: None,
                        next_frame: None,
                        swapchain,
                        framebuffer_exporter,
                        cursor_size,
                        cursor_swapchain,
                        cursor_damage_tracked_renderer,
                        surface,
                        damage_tracked_renderer,
                        output: output.clone(),
                        last_scale: output_scale,
                        last_transform: output_transform,
                        planes,
                        element_states: IndexMap::new(),
                        debug_flags: DebugFlags::empty(),
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
    }

    fn find_supported_format(
        drm: Arc<DrmSurface>,
        planes: &Planes,
        allocator: A,
        framebuffer_exporter: &F,
        mut renderer_formats: HashSet<DrmFormat>,
        code: DrmFourcc,
        logger: slog::Logger,
    ) -> Result<(Swapchain<A>, Frame<A, F>), (A, FrameErrorType<A, F>)> {
        // select a format
        let mut plane_formats = match drm.supported_formats(drm.plane()) {
            Ok(formats) => formats.iter().cloned().collect::<HashSet<_>>(),
            Err(err) => return Err((allocator, err.into())),
        };

        if !plane_formats.iter().any(|fmt| fmt.code == code) {
            return Err((allocator, FrameError::NoSupportedPlaneFormat));
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
            return Err((allocator, FrameError::NoSupportedPlaneFormat));
        } else if renderer_formats.is_empty() {
            return Err((allocator, FrameError::NoSupportedRendererFormat));
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

        let mut swapchain: Swapchain<A> = Swapchain::new(
            allocator,
            mode.size().0 as u32,
            mode.size().1 as u32,
            code,
            modifiers,
        );

        // Test format
        let buffer = match swapchain.acquire() {
            Ok(buffer) => buffer.unwrap(),
            Err(err) => return Err((swapchain.allocator, FrameError::Allocator(err))),
        };

        let dmabuf = match buffer.export() {
            Ok(dmabuf) => dmabuf,
            Err(err) => {
                return Err((swapchain.allocator, FrameError::AsDmabufError(err)));
            }
        };

        let fb_buffer =
            match framebuffer_exporter.add_framebuffer(drm.device_fd(), ExportBuffer::Allocator(&buffer)) {
                Ok(Some(fb_buffer)) => fb_buffer,
                Ok(None) => return Err((swapchain.allocator, FrameError::NoFramebuffer)),
                Err(err) => return Err((swapchain.allocator, FrameError::FramebufferExport(err))),
            };
        buffer
            .userdata()
            .insert_if_missing(|| OwnedFramebuffer::new(fb_buffer));

        let mode = drm.pending_mode();
        let handle = buffer
            .userdata()
            .get::<OwnedFramebuffer<<F as ExportFramebuffer<A::Buffer>>::Framebuffer>>()
            .unwrap()
            .clone();

        let mode_size = Size::from((mode.size().0 as i32, mode.size().1 as i32));

        let mut current_frame_state = FrameState::from_planes(planes);
        let plane_claim = match drm.claim_plane(planes.primary.handle) {
            Some(claim) => claim,
            None => {
                slog::warn!(logger, "failed to claim primary plane",);
                return Err((swapchain.allocator, FrameError::PrimaryPlaneTestFailed));
            }
        };

        let plane_state = PlaneState {
            skip: false,
            element_state: None,
            config: Some(PlaneConfig {
                src: Rectangle::from_loc_and_size(Point::default(), dmabuf.size()).to_f64(),
                dst: Rectangle::from_loc_and_size(Point::default(), mode_size),
                transform: Transform::Normal,
                damage_clips: None,
                buffer: Owned::from(DrmScanoutBuffer {
                    buffer: ScanoutBuffer::Swapchain(buffer),
                    fb: handle,
                }),
                plane_claim,
            }),
        };

        match current_frame_state.test_state(&drm, planes.primary.handle, plane_state, true) {
            Ok(true) => {
                slog::debug!(logger, "Chosen format: {:?}", dmabuf.format());
                Ok((swapchain, current_frame_state))
            }
            Ok(false) => {
                slog::warn!(
                    logger,
                    "Mode-setting failed with automatically selected buffer format {:?}: test state failed",
                    dmabuf.format()
                );
                Err((swapchain.allocator, FrameError::PrimaryPlaneTestFailed))
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

    /// Render the next frame
    pub fn render_frame<'a, R, E, L>(
        &'a mut self,
        renderer: &mut R,
        elements: &'a [E],
        clear_color: [f32; 4],
        log: L,
    ) -> Result<RenderFrameResult<'a, A::Buffer, E>, RenderFrameErrorType<A, F, R>>
    where
        E: RenderElement<R>,
        R: Renderer + Bind<Dmabuf>,
        <R as Renderer>::TextureId: Texture + 'static,
        L: Into<Option<slog::Logger>>,
    {
        let log = crate::slog_or_fallback(log);

        // Just reset any next state, this will put
        // any already acquired slot back to the swapchain
        self.next_frame.take();

        let current_size = self.output.current_mode().unwrap().size;
        let output_scale = self.output.current_scale().fractional_scale().into();
        let output_transform = self.output.current_transform();
        let output_geometry =
            Rectangle::from_loc_and_size((0, 0), output_transform.transform_size(current_size));

        if self.last_scale != output_scale || self.last_transform != output_transform {
            self.cursor_damage_tracked_renderer = DamageTrackedRenderer::new(
                (self.cursor_size.w as i32, self.cursor_size.h as i32),
                output_scale,
                output_transform,
            );
            self.last_scale = output_scale;
            self.last_transform = output_transform;
        }

        // We always acquire a buffer from the swapchain even
        // if we could end up doing direct scan-out on the primary plane.
        // The reason is that we can't know upfront and we need a framebuffer
        // on the primary plane to test overlay/cursor planes
        let primary_plane_buffer = self
            .swapchain
            .acquire()
            .map_err(FrameError::Allocator)?
            .ok_or(FrameError::NoFreeSlotsError)?;

        // It is safe to call export multiple times as the Slot will cache the dmabuf for us
        let dmabuf = primary_plane_buffer.export().map_err(FrameError::AsDmabufError)?;

        // Let's check if we already have a cached framebuffer for this Slot, if not try to export
        // it and use the Slot userdata to cache it
        let maybe_buffer = primary_plane_buffer
            .userdata()
            .get::<OwnedFramebuffer<<F as ExportFramebuffer<A::Buffer>>::Framebuffer>>();
        if maybe_buffer.is_none() {
            let fb_buffer = self
                .framebuffer_exporter
                .add_framebuffer(
                    self.surface.device_fd(),
                    ExportBuffer::Allocator(&primary_plane_buffer),
                )
                .map_err(FrameError::FramebufferExport)?
                .ok_or(FrameError::NoFramebuffer)?;
            primary_plane_buffer
                .userdata()
                .insert_if_missing(|| OwnedFramebuffer::new(fb_buffer));
        }

        // This unwrap is safe as we error out above if we were unable to export a framebuffer
        let fb = primary_plane_buffer
            .userdata()
            .get::<OwnedFramebuffer<<F as ExportFramebuffer<A::Buffer>>::Framebuffer>>()
            .unwrap()
            .clone();

        let mut output_damage: Vec<Rectangle<i32, Physical>> = Vec::new();
        let mut opaque_regions: Vec<Rectangle<i32, Physical>> = Vec::new();
        let mut element_states = IndexMap::new();
        let mut render_element_states = RenderElementStates {
            states: Default::default(),
        };

        // So first we want to create a clean state, for that we have to reset all overlay and cursor planes
        // to nothing. We only want to test if the primary plane alone can be used for scan-out.
        let mut next_frame_state = {
            let previous_state = self
                .pending_frame
                .as_ref()
                .map(|(state, _)| state)
                .unwrap_or(&self.current_frame);

            // This will create an empty frame state, all planes are skipped by default
            let mut next_frame_state = FrameState::from_planes(&self.planes);

            // We want to set skip to false on all planes that previously had something assigned so that
            // they get cleared when they are not longer used
            for (handle, plane_state) in next_frame_state.planes.iter_mut() {
                let reset_state = previous_state
                    .plane_state(*handle)
                    .map(|state| state.config.is_some())
                    .unwrap_or(false);

                if reset_state {
                    plane_state.skip = false;
                }
            }

            next_frame_state
        };

        // We want to make sure we can actually scan-out the primary plane, so
        // explicitly set skip to false
        let plane_claim = self
            .surface
            .claim_plane(self.planes.primary.handle)
            .ok_or(FrameError::PrimaryPlaneTestFailed)?;
        let primary_plane_state = PlaneState {
            skip: false,
            element_state: None,
            config: Some(PlaneConfig {
                src: Rectangle::from_loc_and_size(Point::default(), dmabuf.size()).to_f64(),
                dst: Rectangle::from_loc_and_size(Point::default(), current_size),
                // NOTE: We do not apply the transform to the primary plane as this is handled by the dtr/renderer
                transform: Transform::Normal,
                damage_clips: None,
                buffer: Owned::from(DrmScanoutBuffer {
                    buffer: ScanoutBuffer::Swapchain(primary_plane_buffer),
                    fb,
                }),
                plane_claim,
            }),
        };

        if !next_frame_state
            .test_state(
                &self.surface,
                self.planes.primary.handle,
                primary_plane_state,
                self.surface.commit_pending(),
            )
            .map_err(FrameError::DrmError)?
        {
            return Err(RenderFrameError::PrepareFrame(FrameError::PrimaryPlaneTestFailed));
        }

        // This holds all elements that are visible on the output
        // A element is considered visible if it intersects with the output geometry
        // AND is not completely hidden behind opaque regions
        let mut output_elements: Vec<(&'a E, usize)> = Vec::with_capacity(elements.len());

        for element in elements.iter() {
            let element_id = element.id();
            let element_geometry = element.geometry(output_scale);
            let element_loc = element_geometry.loc;

            // First test if the element overlaps with the output
            // if not we can skip it
            let element_output_geometry = match element_geometry.intersection(output_geometry) {
                Some(geo) => geo,
                None => continue,
            };

            // Then test if the element is completely hidden behind opaque regions
            let element_visible_area = opaque_regions
                .iter()
                .fold([element_output_geometry].to_vec(), |geometry, opaque_region| {
                    geometry
                        .into_iter()
                        .flat_map(|g| g.subtract_rect(*opaque_region))
                        .collect::<Vec<_>>()
                })
                .into_iter()
                .fold(0usize, |acc, item| acc + (item.size.w * item.size.h) as usize);

            if element_visible_area == 0 {
                // No need to draw a completely hidden element
                slog::trace!(log, "skipping completely obscured element {:?}", element.id());

                // We allow multiple instance of a single element, so do not
                // override the state if we already have one
                if !render_element_states.states.contains_key(element_id) {
                    render_element_states
                        .states
                        .insert(element_id.clone(), RenderElementState::skipped());
                }
                continue;
            }

            output_elements.push((element, element_visible_area));

            let element_opaque_regions = element
                .opaque_regions(output_scale)
                .into_iter()
                .map(|mut region| {
                    region.loc += element_loc;
                    region
                })
                .filter_map(|geo| geo.intersection(output_geometry))
                .collect::<Vec<_>>();

            opaque_regions.extend(element_opaque_regions);
        }

        let overlay_plane_lookup: HashMap<plane::Handle, PlaneInfo> =
            self.planes.overlay.iter().map(|p| (p.handle, *p)).collect();

        // This will hold the element that has been selected for direct scan-out on
        // the primary plane if any
        let mut primary_plane_scanout_element: Option<&'a E> = None;
        // This will hold all elements that have been assigned to the primary plane
        // for rendering
        let mut primary_plane_elements: Vec<&'a E> = Vec::with_capacity(elements.len());
        // This will hold the element per plane that has been assigned to a overlay/underlay
        // plane for direct scan-out
        let mut overlay_plane_elements: IndexMap<plane::Handle, &'a E> =
            IndexMap::with_capacity(self.planes.overlay.len());
        // This will hold the element assigned on the cursor plane if any
        let mut cursor_plane_element: Option<&'a E> = None;

        let output_elements_len = output_elements.len();
        for (index, (element, element_visible_area)) in output_elements.into_iter().enumerate() {
            let element_id = element.id();
            let element_geometry = element.geometry(output_scale);
            let remaining_elements = output_elements_len - index;

            // Check if we found our last item, we can try to do
            // direct scan-out on the primary plane
            // If we already assigned an element to
            // an underlay plane we will have a hole punch element
            // on the primary plane, this will disable direct scan-out
            // on the primary plane.
            let try_assign_primary_plane = if remaining_elements == 1 && primary_plane_elements.is_empty() {
                let element_is_opaque = element_is_opaque(element, output_scale);
                let crtc_background_matches_clear_color =
                    (clear_color[0] == 0f32 && clear_color[1] == 0f32 && clear_color[2] == 0f32)
                        || clear_color[3] == 0f32;
                let element_spans_complete_output = element_geometry == output_geometry;
                let overlaps_with_underlay = self
                    .planes
                    .overlay
                    .iter()
                    .filter(|p| p.zpos.unwrap_or_default() <= self.planes.primary.zpos.unwrap_or_default())
                    .any(|p| next_frame_state.overlaps(p.handle, element_geometry));
                !overlaps_with_underlay
                    && (crtc_background_matches_clear_color
                        || (element_spans_complete_output && element_is_opaque))
            } else {
                false
            };

            let direct_scan_out_plane = self.try_assign_element(
                renderer,
                element,
                &mut element_states,
                &primary_plane_elements,
                output_scale,
                &mut next_frame_state,
                &mut output_damage,
                output_transform,
                output_geometry,
                try_assign_primary_plane,
                &log,
            );

            if let Some(direct_scan_out_plane) = direct_scan_out_plane {
                match direct_scan_out_plane.type_ {
                    drm::control::PlaneType::Overlay => {
                        overlay_plane_elements.insert(direct_scan_out_plane.handle, element);
                    }
                    drm::control::PlaneType::Primary => primary_plane_scanout_element = Some(element),
                    drm::control::PlaneType::Cursor => cursor_plane_element = Some(element),
                }

                if let Some(state) = render_element_states.states.get_mut(element_id) {
                    state.presentation_state = RenderElementPresentationState::ZeroCopy;
                    state.visible_area += element_visible_area;
                } else {
                    render_element_states.states.insert(
                        element_id.clone(),
                        RenderElementState::zero_copy(element_visible_area),
                    );
                }
            } else {
                // No need to insert a state, this will be done by the dtr
                primary_plane_elements.push(element);
            }
        }

        // Cleanup old state (e.g. old dmabuffers)
        for element_state in element_states.values_mut() {
            element_state.cleanup();
        }
        self.element_states = element_states;

        let previous_state = self
            .pending_frame
            .as_ref()
            .map(|(state, _)| state)
            .unwrap_or(&self.current_frame);

        // If a plane has been moved or no longer has a buffer we need to report that as damage
        for (handle, previous_plane_state) in previous_state.planes.iter() {
            if let Some(previous_config) = previous_plane_state.config.as_ref() {
                let next_state = next_frame_state
                    .plane_state(*handle)
                    .as_ref()
                    .and_then(|state| state.config.as_ref());

                if next_state
                    .map(|next_config| next_config.dst != previous_config.dst)
                    .unwrap_or(true)
                {
                    output_damage.push(previous_config.dst);

                    if let Some(next_config) = next_state {
                        slog::trace!(
                            log,
                            "damaging move plane {:?}: {:?} -> {:?}",
                            handle,
                            previous_config.dst,
                            next_config.dst
                        );
                        output_damage.push(next_config.dst);
                    } else {
                        slog::trace!(
                            log,
                            "damaging removed plane {:?}: {:?}",
                            handle,
                            previous_config.dst
                        );
                    }
                }
            }
        }

        let render = next_frame_state
            .plane_buffer(self.planes.primary.handle)
            .map(|config| matches!(&config.buffer, ScanoutBuffer::Swapchain(_)))
            .unwrap_or(false);

        if render {
            slog::trace!(
                log,
                "rendering {} elements on the primary plane {:?}",
                primary_plane_elements.len(),
                self.planes.primary.handle,
            );
            let (dmabuf, age) = {
                let primary_plane_state = next_frame_state.plane_state(self.planes.primary.handle).unwrap();
                let config = primary_plane_state.config.as_ref().unwrap();
                let slot = match &config.buffer.buffer {
                    ScanoutBuffer::Swapchain(slot) => slot,
                    _ => unreachable!(),
                };

                // It is safe to call export multiple times as the Slot will cache the dmabuf for us
                let dmabuf = slot.export().map_err(FrameError::AsDmabufError)?;
                let age = slot.age().into();
                (dmabuf, age)
            };

            renderer
                .bind(dmabuf)
                .map_err(DamageTrackedRendererError::Rendering)?;

            // store the current renderer debug flags and replace them
            // with our own
            let renderer_debug_flags = renderer.debug_flags();
            renderer.set_debug_flags(self.debug_flags);

            // First we collect all our fake elements for overlay and underlays
            // This is used to transport the opaque regions for elements that
            // have been assigned to planes and to realize hole punching for
            // underlays
            let mut elements = overlay_plane_elements
                .iter()
                .map(|(p, element)| {
                    let is_underlay = overlay_plane_lookup.get(p).unwrap().zpos.unwrap_or_default()
                        <= self.planes.primary.zpos.unwrap_or_default();
                    if is_underlay {
                        HolepunchRenderElement::from_render_element(element, output_scale).into()
                    } else {
                        OverlayPlaneElement::from_render_element(*element).into()
                    }
                })
                .collect::<Vec<_>>();

            // Then render all remaining elements assigned to the primary plane
            elements.extend(
                primary_plane_elements
                    .iter()
                    .map(|e| DrmRenderElements::Other(*e)),
            );

            let render_res = self.damage_tracked_renderer.render_output(
                renderer,
                age,
                &elements,
                clear_color,
                log.clone(),
            );

            // restore the renderer debug flags
            renderer.set_debug_flags(renderer_debug_flags);

            match render_res {
                Ok((render_damage, states)) => {
                    for (id, mut state) in states.states.into_iter() {
                        if let Some(existing_state) = render_element_states.states.get_mut(&id) {
                            // Our overlay plane element and the holepunch element would result in double accounting of
                            // the visible area, so we just reduce the renderer visible area by the visible are of the
                            // overlay plane/holepunch element
                            state.visible_area =
                                state.visible_area.saturating_sub(existing_state.visible_area);

                            if matches!(
                                existing_state.presentation_state,
                                RenderElementPresentationState::Skipped
                            ) {
                                *existing_state = state;
                            } else {
                                existing_state.visible_area += state.visible_area;
                            }
                        } else {
                            render_element_states.states.insert(id.clone(), state);
                        }
                    }

                    // Fixup damage on plane, if we used the plane for direct scan-out before
                    // but now use it for rendering we do not replace the damage which is
                    // the whole plane initially.
                    let had_direct_scan_out = previous_state
                        .plane_state(self.planes.primary.handle)
                        .map(|state| state.element_state.is_some())
                        .unwrap_or(true);

                    let primary_plane_state = next_frame_state
                        .plane_state_mut(self.planes.primary.handle)
                        .unwrap();
                    let config = primary_plane_state.config.as_mut().unwrap();

                    if !had_direct_scan_out {
                        if let Some(render_damage) = render_damage {
                            slog::trace!(log, "rendering damage: {:?}", render_damage);

                            self.primary_plane_damage_tracker
                                .add(render_damage.iter().map(|d| {
                                    d.to_logical(1).to_buffer(
                                        1,
                                        Transform::Normal,
                                        &output_geometry.size.to_logical(1),
                                    )
                                }));
                            output_damage.extend(render_damage.clone());
                            config.damage_clips = PlaneDamageClips::from_damage(
                                self.surface.device_fd(),
                                config.src,
                                config.dst,
                                render_damage,
                            )
                            .ok()
                            .flatten();
                        } else {
                            slog::trace!(log, "skipping primary plane, no damage");

                            primary_plane_state.skip = true;
                            *config = previous_state
                                .plane_state(self.planes.primary.handle)
                                .and_then(|state| state.config.as_ref().cloned())
                                .unwrap_or_else(|| config.clone());
                        }
                    } else {
                        slog::trace!(
                            log,
                            "clearing previous direct scan-out on primary plane, damaging complete output"
                        );
                        output_damage.push(output_geometry);
                        self.primary_plane_damage_tracker
                            .add([output_geometry.to_logical(1).to_buffer(
                                1,
                                Transform::Normal,
                                &output_geometry.size.to_logical(1),
                            )]);
                    }
                }
                Err(err) => {
                    // Rendering failed at some point, reset the buffers
                    // as we probably now have some half drawn buffer
                    self.swapchain.reset_buffers();
                    return Err(RenderFrameError::from(err));
                }
            }
        }

        self.next_frame = Some(next_frame_state);

        let primary_plane_element = if render {
            let slot = {
                let primary_plane_state = self
                    .next_frame
                    .as_ref()
                    .unwrap()
                    .plane_state(self.planes.primary.handle)
                    .unwrap();
                let config = primary_plane_state.config.as_ref().unwrap();
                match &config.buffer.buffer {
                    ScanoutBuffer::Swapchain(slot) => slot,
                    _ => unreachable!(),
                }
            };
            PrimaryPlaneElement::Swapchain {
                slot,
                transform: output_transform,
                damage: self.primary_plane_damage_tracker.snapshot(),
            }
        } else {
            PrimaryPlaneElement::Element(primary_plane_scanout_element.unwrap())
        };

        let damage = if output_damage.is_empty() {
            None
        } else {
            Some(output_damage)
        };
        let frame_reference: RenderFrameResult<'a, A::Buffer, E> = RenderFrameResult {
            primary_element: primary_plane_element,
            damage,
            overlay_elements: overlay_plane_elements.into_values().collect(),
            cursor_element: cursor_plane_element,
            states: render_element_states,
            primary_plane_element_id: self.primary_plane_element_id.clone(),
        };

        Ok(frame_reference)
    }

    /// Queues the current frame for rendering.
    ///
    /// *Note*: This function needs to be followed up with [`DrmCompositor::frame_submitted`]
    /// when a vblank event is received, that denotes successful scan-out of the frame.
    /// Otherwise the underlying swapchain will eventually run out of buffers.
    ///
    /// `user_data` can be used to attach some data to a specific buffer and later retrieved with [`DrmCompositor::frame_submitted`]
    pub fn queue_frame(&mut self, user_data: U) -> FrameResult<(), A, F> {
        self.queued_frame = self.next_frame.take().map(|state| {
            if let Some(plane_state) = state.plane_state(self.planes.primary.handle) {
                if !plane_state.skip {
                    let slot = plane_state.buffer().and_then(|config| match &config.buffer {
                        ScanoutBuffer::Swapchain(slot) => Some(slot),
                        _ => None,
                    });

                    if let Some(slot) = slot {
                        self.swapchain.submitted(slot);
                    }
                }
            }

            if let Some(cursor_plane) = self.planes.cursor.as_ref() {
                if let Some(plane_state) = state.plane_state(cursor_plane.handle) {
                    if !plane_state.skip {
                        let slot = plane_state.buffer().and_then(|config| match &config.buffer {
                            ScanoutBuffer::Swapchain(slot) => Some(slot),
                            _ => None,
                        });

                        if let Some(slot) = slot {
                            self.cursor_swapchain.submitted(slot);
                        }
                    }
                }
            }

            (state, user_data)
        });
        if self.pending_frame.is_none() && self.queued_frame.is_some() {
            self.submit()?;
        }
        Ok(())
    }

    fn submit(&mut self) -> FrameResult<(), A, F> {
        let (state, user_data) = self.queued_frame.take().unwrap();

        let flip = if self.surface.commit_pending() {
            state.commit(&self.surface, true)
        } else {
            state.page_flip(&self.surface, true)
        };
        if flip.is_ok() {
            self.pending_frame = Some((state, user_data));
        }

        flip.map_err(FrameError::DrmError)
    }

    /// Marks the current frame as submitted.
    ///
    /// *Note*: Needs to be called, after the vblank event of the matching [`DrmDevice`](super::super::DrmDevice)
    /// was received after calling [`DrmCompositor::queue_frame`] on this surface.
    /// Otherwise the underlying swapchain will run out of buffers eventually.
    pub fn frame_submitted(&mut self) -> FrameResult<Option<U>, A, F> {
        if let Some((mut pending, user_data)) = self.pending_frame.take() {
            std::mem::swap(&mut pending, &mut self.current_frame);
            if self.queued_frame.is_some() {
                self.submit()?;
            }
            Ok(Some(user_data))
        } else {
            Ok(None)
        }
    }

    /// Reset the underlying buffers
    pub fn reset_buffers(&mut self) {
        self.swapchain.reset_buffers();
        self.cursor_swapchain.reset_buffers();
    }

    /// Returns the underlying [`crtc`](drm::control::crtc) of this surface
    pub fn crtc(&self) -> crtc::Handle {
        self.surface.crtc()
    }

    /// Returns the underlying [`plane`](drm::control::plane) of this surface
    pub fn plane(&self) -> plane::Handle {
        self.surface.plane()
    }

    /// Currently used [`connector`](drm::control::connector)s of this `Surface`
    pub fn current_connectors(&self) -> impl IntoIterator<Item = connector::Handle> {
        self.surface.current_connectors()
    }

    /// Returns the pending [`connector`](drm::control::connector)s
    /// used for the next frame queued via [`queue_buffer`](GbmBufferedSurface::queue_buffer).
    pub fn pending_connectors(&self) -> impl IntoIterator<Item = connector::Handle> {
        self.surface.pending_connectors()
    }

    /// Tries to add a new [`connector`](drm::control::connector)
    /// to be used after the next commit.
    ///
    /// **Warning**: You need to make sure, that the connector is not used with another surface
    /// or was properly removed via `remove_connector` + `commit` before adding it to another surface.
    /// Behavior if failing to do so is undefined, but might result in rendering errors or the connector
    /// getting removed from the other surface without updating it's internal state.
    ///
    /// Fails if the `connector` is not compatible with the underlying [`crtc`](drm::control::crtc)
    /// (e.g. no suitable [`encoder`](drm::control::encoder) may be found)
    /// or is not compatible with the currently pending
    /// [`Mode`](drm::control::Mode).
    pub fn add_connector(&self, connector: connector::Handle) -> FrameResult<(), A, F> {
        self.surface
            .add_connector(connector)
            .map_err(FrameError::DrmError)
    }

    /// Tries to mark a [`connector`](drm::control::connector)
    /// for removal on the next commit.    
    pub fn remove_connector(&self, connector: connector::Handle) -> FrameResult<(), A, F> {
        self.surface
            .remove_connector(connector)
            .map_err(FrameError::DrmError)
    }

    /// Tries to replace the current connector set with the newly provided one on the next commit.
    ///
    /// Fails if one new `connector` is not compatible with the underlying [`crtc`](drm::control::crtc)
    /// (e.g. no suitable [`encoder`](drm::control::encoder) may be found)
    /// or is not compatible with the currently pending
    /// [`Mode`](drm::control::Mode).    
    pub fn set_connectors(&self, connectors: &[connector::Handle]) -> FrameResult<(), A, F> {
        self.surface
            .set_connectors(connectors)
            .map_err(FrameError::DrmError)
    }

    /// Returns the currently active [`Mode`](drm::control::Mode)
    /// of the underlying [`crtc`](drm::control::crtc)    
    pub fn current_mode(&self) -> Mode {
        self.surface.current_mode()
    }

    /// Returns the currently pending [`Mode`](drm::control::Mode)
    /// to be used after the next commit.    
    pub fn pending_mode(&self) -> Mode {
        self.surface.pending_mode()
    }

    /// Tries to set a new [`Mode`](drm::control::Mode)
    /// to be used after the next commit.
    ///
    /// Fails if the mode is not compatible with the underlying
    /// [`crtc`](drm::control::crtc) or any of the
    /// pending [`connector`](drm::control::connector)s.
    pub fn use_mode(&mut self, mode: Mode) -> FrameResult<(), A, F> {
        self.surface.use_mode(mode).map_err(FrameError::DrmError)?;
        let (w, h) = mode.size();
        self.swapchain.resize(w as _, h as _);
        Ok(())
    }

    /// Set the [`DebugFlags`] to use
    ///
    /// Note: This will reset the primary plane swapchain if
    /// the flags differ from the current flags
    pub fn set_debug_flags(&mut self, flags: DebugFlags) {
        if self.debug_flags != flags {
            self.debug_flags = flags;
            self.swapchain.reset_buffers();
        }
    }

    /// Returns the current enabled [`DebugFlags`]
    pub fn debug_flags(&self) -> DebugFlags {
        self.debug_flags
    }

    /// Returns a reference to the underlying drm surface
    pub fn surface(&self) -> &DrmSurface {
        &self.surface
    }

    #[allow(clippy::too_many_arguments)]
    fn try_assign_element<'a, R, E>(
        &mut self,
        renderer: &mut R,
        element: &'a E,
        element_states: &mut IndexMap<
            Id,
            ElementFramebufferCache<<F as ExportFramebuffer<A::Buffer>>::Framebuffer>,
        >,
        primary_plane_elements: &[&'a E],
        scale: Scale<f64>,
        frame_state: &mut Frame<A, F>,
        output_damage: &mut Vec<Rectangle<i32, Physical>>,
        output_transform: Transform,
        output_geometry: Rectangle<i32, Physical>,
        try_assign_primary_plane: bool,
        log: &slog::Logger,
    ) -> Option<PlaneInfo>
    where
        R: Renderer + Bind<Dmabuf>,
        E: RenderElement<R>,
    {
        if try_assign_primary_plane {
            if let Some(plane) = self.try_assign_primary_plane(
                renderer,
                element,
                element_states,
                scale,
                frame_state,
                output_damage,
                output_geometry,
                log,
            ) {
                slog::trace!(
                    log,
                    "assigned element {:?} to primary plane {:?}",
                    element.id(),
                    self.planes.primary.handle
                );
                return Some(plane);
            }
        }

        if let Some(plane) = self.try_assign_cursor_plane(
            renderer,
            element,
            element_states,
            scale,
            frame_state,
            output_damage,
            output_transform,
            output_geometry,
            log,
        ) {
            slog::trace!(
                log,
                "assigned element {:?} to cursor plane {:?}",
                element.id(),
                self.planes.cursor.as_ref().map(|p| p.handle)
            );
            return Some(plane);
        }

        if let Some(plane) = self.try_assign_overlay_plane(
            renderer,
            element,
            element_states,
            primary_plane_elements,
            scale,
            frame_state,
            output_damage,
            output_transform,
            output_geometry,
            log,
        ) {
            slog::trace!(log, "assigned element {:?} to overlay plane", element.id());
            return Some(plane);
        }

        None
    }

    #[allow(clippy::too_many_arguments)]
    fn try_assign_cursor_plane<R, E>(
        &mut self,
        renderer: &mut R,
        element: &E,
        element_states: &mut IndexMap<
            Id,
            ElementFramebufferCache<<F as ExportFramebuffer<A::Buffer>>::Framebuffer>,
        >,
        scale: Scale<f64>,
        frame_state: &mut Frame<A, F>,
        output_damage: &mut Vec<Rectangle<i32, Physical>>,
        output_transform: Transform,
        output_geometry: Rectangle<i32, Physical>,
        log: &slog::Logger,
    ) -> Option<PlaneInfo>
    where
        R: Renderer + Bind<Dmabuf>,
        E: RenderElement<R>,
    {
        // if we have no cursor plane we can exit early
        let plane_info = match self.planes.cursor.as_ref() {
            Some(plane_info) => plane_info,
            None => return None,
        };

        // something is already assigned to our cursor plane
        if frame_state.is_assigned(plane_info.handle) {
            slog::trace!(
                log,
                "skipping element {:?} on cursor plane {:?}, plane already has element assigned",
                element.id(),
                plane_info.handle
            );
            return None;
        }

        let element_geometry = element.geometry(scale);
        let element_size = output_transform.transform_size(element_geometry.size);

        // if the element is greater than the cursor size we can not
        // use the cursor plane to scan out the element
        if element_size.w > self.cursor_size.w || element_size.h > self.cursor_size.h {
            slog::trace!(
                log,
                "element {:?} too big for cursor plane {:?}, skipping",
                element.id(),
                plane_info.handle,
            );
            return None;
        }

        // if the element exposes the underlying storage we can try to do
        // direct scan-out
        if let Some(underlying_storage) = element.underlying_storage(renderer) {
            slog::trace!(
                log,
                "trying to assign element {:?} for direct scan-out on cursor plane {:?}",
                element.id(),
                plane_info.handle,
            );
            if let Some(plane) = self.try_assign_plane(
                element,
                element_geometry,
                &underlying_storage,
                plane_info,
                element_states,
                scale,
                frame_state,
                output_damage,
                output_transform,
                output_geometry,
                log,
            ) {
                slog::trace!(
                    log,
                    "assigned element {:?} for direct scan-out on cursor plane {:?}",
                    element.id(),
                    plane_info.handle,
                );
                return Some(plane);
            }
        }

        slog::trace!(
            log,
            "trying to render element {:?} on cursor plane {:?}",
            element.id(),
            plane_info.handle
        );

        // if we fail to acquire a slot we can just return false and
        // force the cursor to be rendered on the primary plane
        let cursor_slot = match self.cursor_swapchain.acquire() {
            Ok(Some(slot)) => slot,
            Ok(None) => {
                slog::debug!(
                    log,
                    "failed to acquire slot for cursor plane {:?}: no free slot",
                    plane_info.handle
                );
                return None;
            }
            Err(err) => {
                slog::debug!(
                    log,
                    "failed to acquire slot for cursor plane {:?}: {}",
                    plane_info.handle,
                    err
                );
                return None;
            }
        };

        let dmabuf = match cursor_slot.export() {
            Ok(dmabuf) => dmabuf,
            Err(err) => {
                slog::debug!(
                    log,
                    "failed to export cursor slot for cursor plane {:?}: {}",
                    plane_info.handle,
                    err
                );
                return None;
            }
        };

        let maybe_buffer = cursor_slot
            .userdata()
            .get::<OwnedFramebuffer<<F as ExportFramebuffer<A::Buffer>>::Framebuffer>>();

        if maybe_buffer.is_none() {
            let framebuffer = match self
                .framebuffer_exporter
                .add_framebuffer(self.surface.device_fd(), ExportBuffer::Allocator(&cursor_slot))
            {
                Ok(Some(fb)) => fb,
                Ok(None) => {
                    slog::debug!(
                        log,
                        "failed to export framebuffer for cursor plane {:?}: no framebuffer available",
                        plane_info.handle
                    );
                    return None;
                }
                Err(err) => {
                    slog::debug!(
                        log,
                        "failed to export framebuffer for cursor plane {:?}: {}",
                        plane_info.handle,
                        err
                    );
                    return None;
                }
            };
            cursor_slot
                .userdata()
                .insert_if_missing(|| OwnedFramebuffer::new(framebuffer));
        }

        // This is safe as we error out above if we failed to export the framebuffer
        let fb = cursor_slot
            .userdata()
            .get::<OwnedFramebuffer<<F as ExportFramebuffer<A::Buffer>>::Framebuffer>>()
            .unwrap()
            .clone();

        let dmabuf_size = dmabuf.size();
        if let Err(err) = renderer.bind(dmabuf) {
            slog::debug!(
                log,
                "failed to bind cursor buffer for cursor plane {:?}: {}",
                plane_info.handle,
                err
            );
            return None;
        };

        // Try to claim the plane, if this fails we can not use it
        let plane_claim = match self.surface.claim_plane(plane_info.handle) {
            Some(claim) => claim,
            None => {
                slog::trace!(log, "failed to claim plane {:?}", plane_info.handle);
                return None;
            }
        };

        // save the renderer debug flags and disable all for the cursor plane
        let renderer_debug_flags = renderer.debug_flags();
        renderer.set_debug_flags(DebugFlags::empty());

        let render_res = self.cursor_damage_tracked_renderer.render_output(
            renderer,
            cursor_slot.age().into(),
            &[RelocateRenderElement::from_element(
                element,
                Point::default(),
                Relocate::Absolute,
            )],
            COLOR_TRANSPARENT,
            log.clone(),
        );

        // restore the renderer debug flags
        renderer.set_debug_flags(renderer_debug_flags);

        let (render_damage, _) = match render_res {
            Ok(res) => res,
            Err(err) => {
                slog::debug!(
                    log,
                    "rendering failed for cursor plane {:?}: {}",
                    plane_info.handle,
                    err
                );
                // Rendering failed, but we do not know if some parts have been rendered, reset the buffers in this case
                self.cursor_swapchain.reset_buffers();
                return None;
            }
        };

        let previous_state = self
            .pending_frame
            .as_ref()
            .map(|(state, _)| state)
            .unwrap_or(&self.current_frame);

        // if we had nothing assigned previously we force a full damage on the plane
        // this is important in case the cursor left the output and re-entered it but
        // the render element stayed the same where the dtr will not report any damage.
        // also report full damage (and prevent skip) if we did direct scan-out on the
        // cursor plane before
        let render_damage = if previous_state
            .plane_state(plane_info.handle)
            .map(|state| state.config.is_none() || state.element_state.is_some())
            .unwrap_or(true)
        {
            Some(vec![Rectangle::from_loc_and_size(
                Point::default(),
                self.cursor_size,
            )])
        } else {
            render_damage
        };

        let location = output_transform.transform_point_in(element.location(scale), &output_geometry.size)
            - output_transform.transform_point_in(Point::default(), &self.cursor_size);

        let src = Rectangle::from_loc_and_size(Point::default(), dmabuf_size).to_f64();
        let dst = Rectangle::from_loc_and_size(location, self.cursor_size);

        // If we have no damage and the src/dst properties are unchanged we can skip
        // the update on the plane
        let skip = render_damage.is_none()
            && previous_state
                .plane_state(self.planes.primary.handle)
                .map(|state| {
                    state
                        .config
                        .as_ref()
                        .map(|config| {
                            config.src == src && config.dst == dst && config.transform == Transform::Normal
                        })
                        .unwrap_or(false)
                })
                .unwrap_or(false);

        let render_output_damage = render_damage.as_ref().map(|damage| {
            damage
                .iter()
                .cloned()
                .map(|mut rect| {
                    rect.loc += dst.loc;
                    rect
                })
                .collect::<Vec<_>>()
        });
        let damage_clips = render_damage.and_then(|damage| {
            PlaneDamageClips::from_damage(self.surface.device_fd(), src, dst, damage)
                .ok()
                .flatten()
        });

        let mut config = Some(PlaneConfig {
            src,
            dst,
            transform: Transform::Normal,
            damage_clips,
            buffer: Owned::from(DrmScanoutBuffer {
                buffer: ScanoutBuffer::Swapchain(cursor_slot),
                fb,
            }),
            plane_claim,
        });

        if skip {
            config = previous_state
                .plane_state(plane_info.handle)
                .and_then(|state| state.config.clone());
        }

        let plane_state = PlaneState {
            skip,
            element_state: None,
            config,
        };

        let res = frame_state
            .test_state(&self.surface, plane_info.handle, plane_state, false)
            .unwrap_or_default();

        if res {
            if let Some(damage) = render_output_damage {
                output_damage.extend(damage);
            }

            Some(*plane_info)
        } else {
            slog::trace!(log, "failed to test cursor plane {:?} state", plane_info.handle);
            None
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn try_assign_overlay_plane<'a, R, E>(
        &self,
        renderer: &mut R,
        element: &'a E,
        element_states: &mut IndexMap<
            Id,
            ElementFramebufferCache<<F as ExportFramebuffer<A::Buffer>>::Framebuffer>,
        >,
        primary_plane_elements: &[&'a E],
        scale: Scale<f64>,
        frame_state: &mut Frame<A, F>,
        output_damage: &mut Vec<Rectangle<i32, Physical>>,
        output_transform: Transform,
        output_geometry: Rectangle<i32, Physical>,
        log: &slog::Logger,
    ) -> Option<PlaneInfo>
    where
        R: Renderer,
        E: RenderElement<R>,
    {
        let element_id = element.id();

        // Check if we have a free plane, otherwise we can exit early
        if self.planes.overlay.is_empty()
            || self
                .planes
                .overlay
                .iter()
                .all(|plane| frame_state.is_assigned(plane.handle))
        {
            slog::trace!(
                log,
                "skipping overlay planes for element {:?}, no free planes",
                element_id
            );
        }

        // We can only try to do direct scan-out for element that provide a underlying storage
        let underlying_storage = match element.underlying_storage(renderer) {
            Some(underlying_storage) => underlying_storage,
            None => {
                return None;
            }
        };

        let element_geometry = element.geometry(scale);

        let overlaps_with_primary_plane_element = primary_plane_elements.iter().any(|e| {
            let other_geometry = e.geometry(scale);
            other_geometry.overlaps(element_geometry)
        });

        let is_opaque = element_is_opaque(element, scale);

        for plane in self.planes.overlay.iter() {
            // something is already assigned to our overlay plane
            if frame_state.is_assigned(plane.handle) {
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
                return None;
            }

            let overlaps_with_plane_underneath = self
                .planes
                .overlay
                .iter()
                .filter(|info| {
                    info.handle != plane.handle
                        && info.zpos.unwrap_or_default() <= plane.zpos.unwrap_or_default()
                })
                .any(|overlapping_plane| frame_state.overlaps(overlapping_plane.handle, element_geometry));

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

            if let Some(plane) = self.try_assign_plane(
                element,
                element_geometry,
                &underlying_storage,
                plane,
                element_states,
                scale,
                frame_state,
                output_damage,
                output_transform,
                output_geometry,
                log,
            ) {
                return Some(plane);
            }
        }

        None
    }

    #[allow(clippy::too_many_arguments)]
    fn try_assign_primary_plane<R, E>(
        &self,
        renderer: &mut R,
        element: &E,
        element_states: &mut IndexMap<
            Id,
            ElementFramebufferCache<<F as ExportFramebuffer<A::Buffer>>::Framebuffer>,
        >,
        scale: Scale<f64>,
        frame_state: &mut Frame<A, F>,
        output_damage: &mut Vec<Rectangle<i32, Physical>>,
        output_geometry: Rectangle<i32, Physical>,
        log: &slog::Logger,
    ) -> Option<PlaneInfo>
    where
        R: Renderer,
        E: RenderElement<R>,
    {
        // We can only try to do direct scan-out for element that provide a underlying storage
        let underlying_storage = match element.underlying_storage(renderer) {
            Some(underlying_storage) => underlying_storage,
            None => {
                return None;
            }
        };

        let element_geometry = element.geometry(scale);

        self.try_assign_plane(
            element,
            element_geometry,
            &underlying_storage,
            &self.planes.primary,
            element_states,
            scale,
            frame_state,
            output_damage,
            Transform::Normal,
            output_geometry,
            log,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn try_assign_plane<R, E>(
        &self,
        element: &E,
        element_geometry: Rectangle<i32, Physical>,
        underlying_storage: &UnderlyingStorage,
        plane: &PlaneInfo,
        element_states: &mut IndexMap<
            Id,
            ElementFramebufferCache<<F as ExportFramebuffer<A::Buffer>>::Framebuffer>,
        >,
        scale: Scale<f64>,
        frame_state: &mut Frame<A, F>,
        output_damage: &mut Vec<Rectangle<i32, Physical>>,
        output_transform: Transform,
        output_geometry: Rectangle<i32, Physical>,
        log: &slog::Logger,
    ) -> Option<PlaneInfo>
    where
        R: Renderer,
        E: RenderElement<R>,
    {
        let element_id = element.id();

        // First we try to find a state in our new states, this is important if
        // we got the same id multiple times. If we can't find it we use the previous
        // state if available
        let mut element_state = element_states
            .get(element_id)
            .or_else(|| self.element_states.get(element_id))
            .cloned()
            .unwrap_or_default();

        element_state.cleanup();

        let cached_fb = element_state.get(underlying_storage);

        if cached_fb.is_none() {
            slog::trace!(
                log,
                "no cached fb, exporting new fb for element {:?} underlying storage {:?}",
                element_id,
                &underlying_storage
            );

            let fb = self
                .framebuffer_exporter
                .add_framebuffer(self.surface.device_fd(), ExportBuffer::from(underlying_storage))
                .map(|fb| fb.map(OwnedFramebuffer::new))
                .unwrap_or_else(|err| {
                    slog::trace!(log, "failed to add framebuffer: {:?}", err);
                    None
                });

            if fb.is_none() {
                slog::trace!(
                    log,
                    "could not import framebuffer for element {:?} underlying storage {:?}",
                    element_id,
                    &underlying_storage
                );
            }

            element_state.insert(underlying_storage, fb);
        } else {
            slog::trace!(
                log,
                "using cached fb for element {:?} underlying storage {:?}",
                element_id,
                &underlying_storage
            );
        }

        element_states.insert(element_id.clone(), element_state);

        let fb = match element_states
            .get(element_id)
            .and_then(|state| state.get(underlying_storage).unwrap())
        {
            Some(fb) => fb,
            None => {
                return None;
            }
        };

        let plane_claim = match self.surface.claim_plane(plane.handle) {
            Some(claim) => claim,
            None => {
                slog::trace!(
                    log,
                    "failed to claim plane {:?} for element {:?}",
                    plane.handle,
                    element_id
                );
                return None;
            }
        };

        // Try to assign the element to a plane
        slog::trace!(log, "testing direct scan-out for element {:?} on plane {:?} with zpos {:?}: fb: {:?}, underlying storage: {:?}, element_geometry: {:?}", element_id, plane.handle, plane.zpos, &fb, &underlying_storage, element_geometry);

        let transform = apply_output_transform(
            apply_underlying_storage_transform(element.transform(), underlying_storage),
            output_transform,
        );

        let previous_state = self
            .pending_frame
            .as_ref()
            .map(|(state, _)| state)
            .unwrap_or(&self.current_frame);

        let previous_commit = previous_state.planes.get(&plane.handle).and_then(|state| {
            state.element_state.as_ref().and_then(
                |(id, counter)| {
                    if id == element_id {
                        Some(*counter)
                    } else {
                        None
                    }
                },
            )
        });

        let element_damage = element.damage_since(scale, previous_commit);

        let src = element.src();
        let dst = output_transform.transform_rect_in(element_geometry, &output_geometry.size);

        // We can only skip the plane update if we have no damage and if
        // the src/dst properties are unchanged. Also we can not skip if
        // the fb did change (this includes the case where we previously
        // had not assigned anything to the plane)
        let skip = element_damage.is_empty()
            && previous_state
                .plane_state(plane.handle)
                .map(|state| {
                    state
                        .config
                        .as_ref()
                        .map(|config| {
                            config.src == src
                                && config.dst == dst
                                && config.transform == transform
                                && config.buffer.fb == fb
                        })
                        .unwrap_or(false)
                })
                .unwrap_or(false);

        let element_output_damage = element_damage
            .iter()
            .cloned()
            .map(|mut rect| {
                rect.loc += element_geometry.loc;
                rect
            })
            .collect::<Vec<_>>();

        let damage_clips = if element_damage.is_empty() {
            None
        } else {
            PlaneDamageClips::from_damage(self.surface.device_fd(), src, element_geometry, element_damage)
                .ok()
                .flatten()
        };

        let plane_state = PlaneState {
            skip,
            element_state: Some((element_id.clone(), element.current_commit())),
            config: Some(PlaneConfig {
                src,
                dst,
                transform,
                damage_clips,
                buffer: Owned::from(DrmScanoutBuffer {
                    fb,
                    buffer: ScanoutBuffer::from(underlying_storage.clone()),
                }),
                plane_claim,
            }),
        };

        let res = frame_state
            .test_state(&self.surface, plane.handle, plane_state, false)
            .unwrap_or_default();

        if res {
            output_damage.extend(element_output_damage);

            slog::trace!(
                log,
                "successfully assigned element {:?} to plane {:?} with zpos {:?} for direct scan-out",
                element_id,
                plane.handle,
                plane.zpos,
            );

            Some(*plane)
        } else {
            slog::trace!(
                log,
                "skipping direct scan-out on plane {:?} with zpos {:?} for element {:?}, test failed",
                plane.handle,
                plane.zpos,
                element_id
            );

            None
        }
    }
}

fn apply_underlying_storage_transform(
    element_transform: Transform,
    storage: &UnderlyingStorage,
) -> Transform {
    match storage {
        UnderlyingStorage::Wayland(buffer) => {
            if buffer_y_inverted(buffer).unwrap_or(false) {
                match element_transform {
                    Transform::Normal => Transform::Flipped,
                    Transform::_90 => Transform::Flipped90,
                    Transform::_180 => Transform::Flipped180,
                    Transform::_270 => Transform::Flipped270,
                    Transform::Flipped => Transform::Normal,
                    Transform::Flipped90 => Transform::_90,
                    Transform::Flipped180 => Transform::_180,
                    Transform::Flipped270 => Transform::_270,
                }
            } else {
                element_transform
            }
        }
    }
}

fn apply_output_transform(transform: Transform, output_transform: Transform) -> Transform {
    match (transform, output_transform) {
        (Transform::Normal, output_transform) => output_transform,

        (Transform::_90, Transform::Normal) => Transform::_270,
        (Transform::_90, Transform::_90) => Transform::Normal,
        (Transform::_90, Transform::_180) => Transform::_90,
        (Transform::_90, Transform::_270) => Transform::_180,
        (Transform::_90, Transform::Flipped) => Transform::Flipped270,
        (Transform::_90, Transform::Flipped90) => Transform::Flipped,
        (Transform::_90, Transform::Flipped180) => Transform::Flipped90,
        (Transform::_90, Transform::Flipped270) => Transform::Flipped180,

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
        (Transform::_270, Transform::Flipped) => Transform::Flipped90,
        (Transform::_270, Transform::Flipped90) => Transform::Flipped180,
        (Transform::_270, Transform::Flipped180) => Transform::Flipped270,
        (Transform::_270, Transform::Flipped270) => Transform::Flipped,

        (Transform::Flipped, Transform::Normal) => Transform::Flipped,
        (Transform::Flipped, Transform::_90) => Transform::Flipped90,
        (Transform::Flipped, Transform::_180) => Transform::Flipped180,
        (Transform::Flipped, Transform::_270) => Transform::Flipped270,
        (Transform::Flipped, Transform::Flipped) => Transform::Normal,
        (Transform::Flipped, Transform::Flipped90) => Transform::_90,
        (Transform::Flipped, Transform::Flipped180) => Transform::_180,
        (Transform::Flipped, Transform::Flipped270) => Transform::_270,

        (Transform::Flipped90, Transform::Normal) => Transform::Flipped270,
        (Transform::Flipped90, Transform::_90) => Transform::Flipped,
        (Transform::Flipped90, Transform::_180) => Transform::Flipped90,
        (Transform::Flipped90, Transform::_270) => Transform::Flipped180,
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

        (Transform::Flipped270, Transform::Normal) => Transform::Flipped90,
        (Transform::Flipped270, Transform::_90) => Transform::Flipped180,
        (Transform::Flipped270, Transform::_180) => Transform::Flipped270,
        (Transform::Flipped270, Transform::_270) => Transform::Flipped,
        (Transform::Flipped270, Transform::Flipped) => Transform::_90,
        (Transform::Flipped270, Transform::Flipped90) => Transform::_180,
        (Transform::Flipped270, Transform::Flipped180) => Transform::_270,
        (Transform::Flipped270, Transform::Flipped270) => Transform::Normal,
    }
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

struct OwnedFramebuffer<B: AsRef<framebuffer::Handle>>(Arc<B>);

impl<B: AsRef<framebuffer::Handle>> PartialEq for OwnedFramebuffer<B> {
    fn eq(&self, other: &Self) -> bool {
        AsRef::<framebuffer::Handle>::as_ref(&self) == AsRef::<framebuffer::Handle>::as_ref(&other)
    }
}

impl<B: AsRef<framebuffer::Handle> + std::fmt::Debug> std::fmt::Debug for OwnedFramebuffer<B> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("OwnedFramebuffer").field(&self.0).finish()
    }
}

impl<B: AsRef<framebuffer::Handle>> OwnedFramebuffer<B> {
    fn new(buffer: B) -> Self {
        OwnedFramebuffer(Arc::new(buffer))
    }
}

impl<B: AsRef<framebuffer::Handle>> Clone for OwnedFramebuffer<B> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<B: AsRef<framebuffer::Handle>> AsRef<framebuffer::Handle> for OwnedFramebuffer<B> {
    fn as_ref(&self) -> &framebuffer::Handle {
        (*self.0).as_ref()
    }
}

impl<A, F, U> Drop for DrmCompositor<A, F, U>
where
    A: Allocator,
    F: ExportFramebuffer<A::Buffer>,
    <F as ExportFramebuffer<A::Buffer>>::Framebuffer: std::fmt::Debug + 'static,
{
    fn drop(&mut self) {
        let previous_state = self
            .pending_frame
            .as_ref()
            .map(|(state, _)| state)
            .unwrap_or(&self.current_frame);

        // other ttys that use no cursor or overlay planes, might not clear it themselves.
        // This makes sure our cursor and overlays won't stay visible.
        for plane in &self.planes.overlay {
            if previous_state.is_assigned(plane.handle) {
                let _ = self.surface.clear_plane(plane.handle);
            }
        }

        if let Some(plane) = self.planes.cursor.as_ref() {
            if previous_state.is_assigned(plane.handle) {
                let _ = self.surface.clear_plane(plane.handle);
            }
        }
    }
}

/// Errors thrown by a [`DrmCompositor`]
#[derive(Debug, thiserror::Error)]
pub enum FrameError<
    A: std::error::Error + Send + Sync + 'static,
    B: std::error::Error + Send + Sync + 'static,
    F: std::error::Error + Send + Sync + 'static,
> {
    /// Testing the primary plane failed
    #[error("Testing the primary plane failed")]
    PrimaryPlaneTestFailed,
    /// No supported pixel format for the given plane could be determined
    #[error("No supported plane buffer format found")]
    NoSupportedPlaneFormat,
    /// No supported pixel format for the given renderer could be determined
    #[error("No supported renderer buffer format found")]
    NoSupportedRendererFormat,
    /// The swapchain is exhausted, you need to call `frame_submitted`
    #[error("Failed to allocate a new buffer")]
    NoFreeSlotsError,
    /// Error accessing the drm device
    #[error("The underlying drm surface encountered an error: {0}")]
    DrmError(#[from] DrmError),
    /// Error during buffer allocation
    #[error("The underlying allocator encountered an error: {0}")]
    Allocator(#[source] A),
    /// Error during exporting the buffer as dmabuf
    #[error("Failed to export the allocated buffer as dmabuf: {0}")]
    AsDmabufError(#[source] B),
    /// Error during exporting a framebuffer
    #[error("The framebuffer export encountered an error: {0}")]
    FramebufferExport(#[source] F),
    /// No framebuffer available
    #[error("No framebuffer available")]
    NoFramebuffer,
}

/// Error returned from [`DrmCompositor::render_frame`]
#[derive(thiserror::Error)]
pub enum RenderFrameError<
    A: std::error::Error + Send + Sync + 'static,
    B: std::error::Error + Send + Sync + 'static,
    F: std::error::Error + Send + Sync + 'static,
    R: Renderer,
> {
    /// Preparing the frame encountered an error
    #[error(transparent)]
    PrepareFrame(#[from] FrameError<A, B, F>),
    /// Rendering the frame encountered en error
    #[error(transparent)]
    RenderFrame(#[from] DamageTrackedRendererError<R>),
}

impl<A, B, F, R> std::fmt::Debug for RenderFrameError<A, B, F, R>
where
    A: std::error::Error + Send + Sync + 'static,
    B: std::error::Error + Send + Sync + 'static,
    F: std::error::Error + Send + Sync + 'static,
    R: Renderer,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PrepareFrame(arg0) => f.debug_tuple("PrepareFrame").field(arg0).finish(),
            Self::RenderFrame(arg0) => f.debug_tuple("RenderFrame").field(arg0).finish(),
        }
    }
}

impl<
        A: std::error::Error + Send + Sync + 'static,
        B: std::error::Error + Send + Sync + 'static,
        F: std::error::Error + Send + Sync + 'static,
    > From<FrameError<A, B, F>> for SwapBuffersError
{
    fn from(err: FrameError<A, B, F>) -> SwapBuffersError {
        match err {
            x @ FrameError::NoSupportedPlaneFormat
            | x @ FrameError::NoSupportedRendererFormat
            | x @ FrameError::PrimaryPlaneTestFailed
            | x @ FrameError::NoFramebuffer => SwapBuffersError::ContextLost(Box::new(x)),
            x @ FrameError::NoFreeSlotsError => SwapBuffersError::TemporaryFailure(Box::new(x)),
            FrameError::DrmError(err) => err.into(),
            FrameError::Allocator(err) => SwapBuffersError::ContextLost(Box::new(err)),
            FrameError::AsDmabufError(err) => SwapBuffersError::ContextLost(Box::new(err)),
            FrameError::FramebufferExport(err) => SwapBuffersError::ContextLost(Box::new(err)),
        }
    }
}
