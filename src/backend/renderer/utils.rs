//! Utility module for helpers around drawing [`WlSurface`]s with [`Renderer`]s.

use crate::{
    backend::renderer::{buffer_dimensions, Frame, ImportAll, Renderer},
    utils::{Buffer, Coordinate, DeadResource, Logical, Point, Rectangle, Size, Transform},
    wayland::{
        compositor::{
            get_parent, is_sync_subsurface, with_states, with_surface_tree_downward,
            with_surface_tree_upward, BufferAssignment, Damage, SubsurfaceCachedState, SurfaceAttributes,
            SurfaceData, TraversalAction,
        },
        seat::InputTransform,
    },
};
use std::collections::VecDeque;
use std::{
    any::TypeId,
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
};
use wayland_server::protocol::{wl_buffer::WlBuffer, wl_surface::WlSurface};

#[derive(Default)]
pub(crate) struct SurfaceState {
    pub(crate) commit_count: usize,
    pub(crate) buffer_dimensions: Option<Size<i32, Buffer>>,
    pub(crate) buffer_scale: i32,
    pub(crate) buffer_transform: Transform,
    pub(crate) buffer: Option<WlBuffer>,
    pub(crate) damage: VecDeque<Vec<Rectangle<i32, Buffer>>>,
    pub(crate) renderer_seen: HashMap<(TypeId, usize), usize>,
    pub(crate) textures: HashMap<(TypeId, usize), Box<dyn std::any::Any>>,
    #[cfg(feature = "desktop")]
    pub(crate) space_seen: HashMap<crate::desktop::space::SpaceOutputHash, usize>,
}

const MAX_DAMAGE: usize = 4;

impl SurfaceState {
    pub fn update_buffer(&mut self, attrs: &mut SurfaceAttributes) {
        match attrs.buffer.take() {
            Some(BufferAssignment::NewBuffer { buffer, .. }) => {
                // new contents
                self.buffer_dimensions = buffer_dimensions(&buffer);

                #[cfg(feature = "desktop")]
                if self.buffer_scale != attrs.buffer_scale
                    || self.buffer_transform != attrs.buffer_transform.into()
                {
                    self.reset_space_damage();
                }
                self.buffer_scale = attrs.buffer_scale;
                self.buffer_transform = attrs.buffer_transform.into();

                if let Some(old_buffer) = std::mem::replace(&mut self.buffer, Some(buffer)) {
                    if &old_buffer != self.buffer.as_ref().unwrap() {
                        old_buffer.release();
                    }
                }
                self.textures.clear();
                self.commit_count = self.commit_count.wrapping_add(1);
                let mut buffer_damage = attrs
                    .damage
                    .drain(..)
                    .flat_map(|dmg| {
                        match dmg {
                            Damage::Buffer(rect) => rect,
                            Damage::Surface(rect) => rect.to_buffer(
                                self.buffer_scale,
                                self.buffer_transform,
                                &self.surface_size().unwrap(),
                            ),
                        }
                        .intersection(Rectangle::from_loc_and_size(
                            (0, 0),
                            self.buffer_dimensions.unwrap(),
                        ))
                    })
                    .collect::<Vec<Rectangle<i32, Buffer>>>();
                buffer_damage.dedup();
                self.damage.push_front(buffer_damage);
                self.damage.truncate(MAX_DAMAGE);
            }
            Some(BufferAssignment::Removed) => {
                // remove the contents
                self.buffer_dimensions = None;
                if let Some(buffer) = self.buffer.take() {
                    buffer.release();
                };
                self.textures.clear();
                self.commit_count = self.commit_count.wrapping_add(1);
                self.damage.clear();
            }
            None => {}
        }
    }

    pub(crate) fn damage_since(&self, commit: Option<usize>) -> Vec<Rectangle<i32, Buffer>> {
        // on overflow the wrapping_sub should end up
        let recent_enough = commit
            // if commit > commit_count we have overflown, in that case the following map might result
            // in a false-positive, if commit is still very large. So we force false in those cases.
            // That will result in a potentially sub-optimal full damage every usize::MAX frames,
            // which is acceptable.
            .filter(|commit| *commit <= self.commit_count)
            .map(|commit| self.commit_count.wrapping_sub(self.damage.len()) <= commit)
            .unwrap_or(false);
        if recent_enough {
            self.damage
                .iter()
                .take(self.commit_count.wrapping_sub(commit.unwrap()))
                .fold(Vec::new(), |mut acc, elem| {
                    acc.extend(elem);
                    acc
                })
        } else {
            self.buffer_dimensions
                .as_ref()
                .map(|size| vec![Rectangle::from_loc_and_size((0, 0), *size)])
                .unwrap_or_else(Vec::new)
        }
    }

    #[cfg(feature = "desktop")]
    pub fn reset_space_damage(&mut self) {
        self.space_seen.clear();
    }

    /// Returns the size of the surface.
    pub fn surface_size(&self) -> Option<Size<i32, Logical>> {
        self.buffer_dimensions
            .as_ref()
            .map(|dim| dim.to_logical(self.buffer_scale, self.buffer_transform))
    }
}

/// Handler to let smithay take over buffer management.
///
/// Needs to be called first on the commit-callback of
/// [`crate::wayland::compositor::compositor_init`].
///
/// Consumes the buffer of [`SurfaceAttributes`], the buffer will
/// not be accessible anymore, but [`draw_surface_tree`] and other
/// `draw_*` helpers of the [desktop module](`crate::desktop`) will
/// become usable for surfaces handled this way.
pub fn on_commit_buffer_handler(surface: &WlSurface) {
    if !is_sync_subsurface(surface) {
        with_surface_tree_upward(
            surface,
            (),
            |_, _, _| TraversalAction::DoChildren(()),
            |_surf, states, _| {
                states
                    .data_map
                    .insert_if_missing(|| RefCell::new(SurfaceState::default()));
                let mut data = states
                    .data_map
                    .get::<RefCell<SurfaceState>>()
                    .unwrap()
                    .borrow_mut();
                data.update_buffer(&mut *states.cached_state.current::<SurfaceAttributes>());
            },
            |_, _, _| true,
        );

        update_surface_view(surface);
    }
}

/// A transform that can be attached to a surface
/// to change how it will be presented on screen
/// Can be used to crop and scale on compositor side.
///
/// This applies to the whole surface tree and will
/// override all transforms on it's children.
#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct SurfaceTransform {
    /// Defines an optional source [`Rectangle`] that can
    /// be used for cropping a surface tree
    pub src: Option<Rectangle<f64, Logical>>,

    /// Defines an optional scale which should be used for
    /// rendering the surface tree
    pub scale: Option<Size<f64, Logical>>,
}

impl SurfaceTransform {
    fn merge(mut self, other: SurfaceTransform, offset: Point<f64, Logical>) -> SurfaceTransform {
        self.src = self.src.map(|mut src| {
            src.loc -= offset;
            src
        });

        self.src = self.src.or(other.src);
        self.scale = self.scale.or(other.scale);

        self
    }
}

/// Access the current [`SurfaceTransform`] for the specified surface
pub fn with_surface_transform<F, R>(surface: &WlSurface, mut f: F) -> Result<R, DeadResource>
where
    F: FnMut(&mut SurfaceTransform) -> R,
{
    let res = with_states(surface, |states| {
        states
            .data_map
            .insert_if_missing(|| RefCell::new(SurfaceTransform::default()));
        let mut transform = states
            .data_map
            .get::<RefCell<SurfaceTransform>>()
            .unwrap()
            .borrow_mut();
        f(&mut *transform)
    });

    update_surface_view(surface);

    res
}

fn surface_transform(states: &SurfaceData) -> SurfaceTransform {
    states
        .data_map
        .get::<RefCell<SurfaceTransform>>()
        .map(|t| *t.borrow())
        .unwrap_or_default()
}

#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub(crate) struct SurfaceView {
    pub src: Rectangle<f64, Logical>,
    pub dst: Size<f64, Logical>,
    pub offset: Point<f64, Logical>,
}

impl SurfaceView {
    pub(crate) fn rect_to_global<N>(&self, rect: Rectangle<N, Logical>) -> Rectangle<f64, Logical>
    where
        N: Coordinate,
    {
        let (scale_x, scale_y) = self.scale();
        let mut rect = rect.to_f64();
        rect.loc -= self.src.loc;
        rect.loc.x = rect.loc.x.upscale(scale_x);
        rect.loc.y = rect.loc.y.upscale(scale_y);
        rect.size.w = rect.size.w.upscale(scale_x);
        rect.size.h = rect.size.h.upscale(scale_y);
        rect
    }

    // pub(crate) fn rect_to_local<N>(&self, rect: Rectangle<N, Logical>) -> Rectangle<f64, Logical>
    // where
    //     N: Coordinate,
    // {
    //     let (scale_x, scale_y) = self.scale();
    //     let mut rect = rect.to_f64();
    //     rect.loc.x = rect.loc.x.downscale(scale_x);
    //     rect.loc.y = rect.loc.y.downscale(scale_y);
    //     rect.size.w = rect.size.w.downscale(scale_x);
    //     rect.size.h = rect.size.h.downscale(scale_y);
    //     rect.loc += self.src.loc;
    //     rect
    // }

    // pub(crate) fn point_to_global<N>(&self, point: Point<N, Logical>) -> Point<f64, Logical>
    // where
    //     N: Coordinate,
    // {
    //     let (scale_x, scale_y) = self.scale();
    //     let mut point = point.to_f64();
    //     point -= self.src.loc;
    //     point.x = point.x.upscale(scale_x);
    //     point.y = point.y.upscale(scale_y);
    //     point
    // }

    pub(crate) fn point_to_local<N>(&self, point: Point<N, Logical>) -> Point<f64, Logical>
    where
        N: Coordinate,
    {
        let (scale_x, scale_y) = self.scale();
        let mut point = point.to_f64();
        point.x = point.x.downscale(scale_x);
        point.y = point.y.downscale(scale_y);
        point += self.src.loc;
        point
    }

    fn scale(&self) -> (f64, f64) {
        (self.dst.w / self.src.size.w, self.dst.h / self.src.size.h)
    }
}

pub(crate) fn surface_view(states: &SurfaceData) -> SurfaceView {
    states
        .data_map
        .get::<RefCell<SurfaceView>>()
        .map(|view| *view.borrow())
        .unwrap_or_default()
}

fn with_surface_tree_transform_downward<P, F>(
    surface: &WlSurface,
    initial: (SurfaceTransform, Point<f64, Logical>),
    processor: P,
    post_filter: F,
) where
    P: FnMut(&WlSurface, &SurfaceData, &(SurfaceTransform, Point<f64, Logical>)),
    F: FnMut(&WlSurface, &SurfaceData, &(SurfaceTransform, Point<f64, Logical>)) -> bool,
{
    with_surface_tree_downward(
        surface,
        initial,
        |_, states, (parent_transform, _)| {
            let offset = if states.role == Some("subsurface") {
                states
                    .cached_state
                    .current::<SubsurfaceCachedState>()
                    .location
                    .to_f64()
            } else {
                Default::default()
            };

            let transform = parent_transform.merge(surface_transform(states), offset);

            if let Some(data) = states.data_map.get::<RefCell<SurfaceState>>() {
                let data = data.borrow_mut();

                if let Some(size) = data.surface_size() {
                    // TODO: Take wp_viewporter into account
                    let surface_rect = Rectangle::from_loc_and_size((0., 0.), size.to_f64());
                    let crop_rect = transform.src.unwrap_or(surface_rect);

                    if let Some(src) = surface_rect.intersection(crop_rect) {
                        debug_assert!(src.loc.x >= 0.0);
                        debug_assert!(src.loc.y >= 0.0);
                        // TODO: The parent crop should only be provided in case of
                        // compositor driven crop, wp_viewporter does not handle
                        // shifting the children (the client is responsible) to update
                        // the sub surface offsets.
                        TraversalAction::DoChildren((transform, src.loc))
                    } else {
                        // If there is no intersection we can not show the surface
                        TraversalAction::SkipChildren
                    }
                } else {
                    TraversalAction::SkipChildren
                }
            } else {
                TraversalAction::SkipChildren
            }
        },
        processor,
        post_filter,
    );
}

/// Returns the parent transform and in case the
/// parent transform results in a crop also the
/// size of the crop so that the children offset
/// can be updated to reflect the crop
fn resolve_parent_surface_transform(surface: &WlSurface) -> (SurfaceTransform, Point<f64, Logical>) {
    // TODO: In case the desktop feature is enabled, we have to check if the
    // surface is a popup and walk the complete tree from the toplevel to our
    // popup

    // First we search for the root of this surface so that
    // we can check if we have a inherited transform
    let mut root = get_parent(surface).unwrap_or_else(|| surface.clone());
    while let Some(parent) = get_parent(&root) {
        root = parent;
    }

    // If the surface is already the root there
    // can be no parent transform
    if root == *surface {
        return (SurfaceTransform::default(), Point::default());
    }

    // Resolve surface transform until we hit our surface
    let found = RefCell::new((SurfaceTransform::default(), Point::default()));
    with_surface_tree_transform_downward(
        &root,
        (SurfaceTransform::default(), Point::default()),
        |s, _, data| {
            if s == surface {
                // We found our surface, we can use the parent transform now
                *found.borrow_mut() = *data;
            }
        },
        |s, _, _| s != surface,
    );

    found.into_inner()
}

fn update_surface_view(surface: &WlSurface) {
    let initial = resolve_parent_surface_transform(surface);
    with_surface_tree_transform_downward(
        surface,
        initial,
        |_, states, (parent_transform, parent_crop)| {
            let offset = if states.role == Some("subsurface") {
                let offset = states
                    .cached_state
                    .current::<SubsurfaceCachedState>()
                    .location
                    .to_f64();
                Some(offset)
            } else {
                None
            };

            let transform = parent_transform.merge(surface_transform(states), offset.unwrap_or_default());

            if let Some(data) = states.data_map.get::<RefCell<SurfaceState>>() {
                let mut data = data.borrow_mut();

                let surface_view = if let Some(size) = data.surface_size() {
                    // TODO: Take wp_viewporter into account
                    let surface_rect = Rectangle::from_loc_and_size((0., 0.), size.to_f64());
                    let crop_rect = transform.src.unwrap_or(surface_rect);

                    if let Some(src) = surface_rect.intersection(crop_rect) {
                        debug_assert!(src.loc.x >= 0.0);
                        debug_assert!(src.loc.y >= 0.0);

                        // Correct the offset by the (parent)crop
                        let mut offset = offset
                            .map(|offset| (offset + src.loc) - *parent_crop)
                            .unwrap_or_default();

                        if let Some(scale) = parent_transform.scale {
                            offset.x = offset.x.upscale(scale.w);
                            offset.y = offset.y.upscale(scale.h);
                        }

                        let mut dst = src.size;

                        if let Some(scale) = transform.scale {
                            dst.w = dst.w.upscale(scale.w);
                            dst.h = dst.h.upscale(scale.h);
                        }

                        SurfaceView { src, dst, offset }
                    } else {
                        // If there is no intersection we can not show the surface
                        SurfaceView::default()
                    }
                } else {
                    SurfaceView::default()
                };

                states
                    .data_map
                    .insert_if_missing(|| RefCell::new(SurfaceView::default()));
                let mut current_surface_view = states
                    .data_map
                    .get::<RefCell<SurfaceView>>()
                    .unwrap()
                    .borrow_mut();

                #[cfg(feature = "desktop")]
                if surface_view != *current_surface_view {
                    // In case the surface view has changed reset
                    // the space damage seen to update the whole surface
                    // TODO: This is probably not enough in case the offset has
                    // changed as we would need to damage the old location too.
                    // A simple workaround could be to also reset the damage
                    // of the parent.
                    data.reset_space_damage()
                }

                if surface_view.dst.w > 0.0 && surface_view.dst.h > 0.0 {
                    // In case we have a transform we need to also update the input transform
                    // TODO: Using the surface_view for that is not correct as it will also include
                    // wp_viewporter, in which case the coordinates should neither be offset nor scaled
                    states
                        .data_map
                        .insert_if_missing(|| RefCell::new(InputTransform::default()));
                    let mut input_transform = states
                        .data_map
                        .get::<RefCell<InputTransform>>()
                        .unwrap()
                        .borrow_mut();
                    input_transform.offset = surface_view.src.loc;
                    let scale = surface_view.scale();
                    input_transform.scale = Size::from((1.0 / scale.0, 1.0 / scale.1));
                }

                *current_surface_view = surface_view;
            }
        },
        |_, _, _| true,
    );
}

/// Imports buffers of a surface and its subsurfaces using a given [`Renderer`].
///
/// This can be called early as an optimization, if `draw_surface_tree` is used later.
/// `draw_surface_tree` will also import buffers as necessary, but calling `import_surface_tree`
/// already may allow buffer imports to happen before compositing takes place, depending
/// on your event loop.
///
/// Note: This will do nothing, if you are not using
/// [`crate::backend::renderer::utils::on_commit_buffer_handler`]
/// to let smithay handle buffer management.
pub fn import_surface_tree<R>(
    renderer: &mut R,
    surface: &WlSurface,
    log: &slog::Logger,
) -> Result<(), <R as Renderer>::Error>
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
{
    import_surface_tree_and(renderer, surface, log, (0, 0).into(), |_, _, _| {})
}

fn import_surface_tree_and<F, R>(
    renderer: &mut R,
    surface: &WlSurface,
    log: &slog::Logger,
    location: Point<i32, Logical>,
    processor: F,
) -> Result<(), <R as Renderer>::Error>
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
    F: FnMut(&WlSurface, &SurfaceData, &(Point<f64, Logical>, Point<f64, Logical>)),
{
    let texture_id = (TypeId::of::<<R as Renderer>::TextureId>(), renderer.id());
    let mut result = Ok(());
    with_surface_tree_upward(
        surface,
        (location.to_f64(), (0., 0.).into()),
        |_surface, states, (location, surface_offset)| {
            let mut location = *location;
            let mut surface_offset = *surface_offset;
            if let Some(data) = states.data_map.get::<RefCell<SurfaceState>>() {
                let mut data_ref = data.borrow_mut();
                let data = &mut *data_ref;
                // Import a new buffer if necessary
                let last_commit = data.renderer_seen.get(&texture_id);
                let buffer_damage = data.damage_since(last_commit.copied());
                if let Entry::Vacant(e) = data.textures.entry(texture_id) {
                    if let Some(buffer) = data.buffer.as_ref() {
                        match renderer.import_buffer(buffer, Some(states), &buffer_damage) {
                            Some(Ok(m)) => {
                                e.insert(Box::new(m));
                                data.renderer_seen.insert(texture_id, data.commit_count);
                            }
                            Some(Err(err)) => {
                                slog::warn!(log, "Error loading buffer: {}", err);
                                result = Err(err);
                            }
                            None => {
                                slog::error!(log, "Unknown buffer format for: {:?}", buffer);
                            }
                        }
                    }
                }
                // Now, should we be drawn ?
                if data.textures.contains_key(&texture_id) {
                    // if yes, also process the children
                    let surface_view = surface_view(states);
                    location += surface_view.offset;
                    surface_offset += surface_view.offset;
                    TraversalAction::DoChildren((location, surface_offset))
                } else {
                    // we are not displayed, so our children are neither
                    TraversalAction::SkipChildren
                }
            } else {
                // we are not displayed, so our children are neither
                TraversalAction::SkipChildren
            }
        },
        processor,
        |_, _, _| true,
    );
    result
}

/// Draws a surface and its subsurfaces using a given [`Renderer`] and [`Frame`].
///
/// - `scale` needs to be equivalent to the fractional scale the rendered result should have.
/// - `location` is the position the surface should be drawn at.
/// - `damage` is the set of regions of the surface that should be drawn.
///
/// Note: This element will render nothing, if you are not using
/// [`crate::backend::renderer::utils::on_commit_buffer_handler`]
/// to let smithay handle buffer management.
pub fn draw_surface_tree<R>(
    renderer: &mut R,
    frame: &mut <R as Renderer>::Frame,
    surface: &WlSurface,
    scale: f64,
    location: Point<i32, Logical>,
    damage: &[Rectangle<i32, Logical>],
    log: &slog::Logger,
) -> Result<(), <R as Renderer>::Error>
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
{
    let texture_id = (TypeId::of::<<R as Renderer>::TextureId>(), renderer.id());
    let mut result = Ok(());
    let _ = import_surface_tree_and(
        renderer,
        surface,
        log,
        location,
        |_surface, states, (location, surface_offset)| {
            let mut location = *location;
            let mut surface_offset = *surface_offset;
            if let Some(data) = states.data_map.get::<RefCell<SurfaceState>>() {
                let mut data = data.borrow_mut();
                let dimensions = data.surface_size();
                let buffer_scale = data.buffer_scale;
                let attributes = states.cached_state.current::<SurfaceAttributes>();
                if let Some(texture) = data
                    .textures
                    .get_mut(&texture_id)
                    .and_then(|x| x.downcast_mut::<<R as Renderer>::TextureId>())
                {
                    let surface_view = surface_view(states);

                    let dimensions = dimensions.unwrap();
                    // we need to re-extract the subsurface offset, as the previous closure
                    // only passes it to our children
                    location += surface_view.offset;
                    surface_offset += surface_view.offset;

                    let damage = damage
                        .iter()
                        .cloned()
                        // first move the damage by the surface offset in logical space
                        .map(|geo| {
                            let mut geo = geo.to_f64();
                            // make the damage relative to the surface
                            geo.loc -= surface_offset;
                            geo
                        })
                        // then clamp to surface size again in logical space
                        .flat_map(|geo| {
                            geo.intersection(Rectangle::from_loc_and_size((0., 0.), surface_view.dst))
                        })
                        // lastly transform it into physical space
                        .map(|geo| geo.to_physical(scale))
                        .collect::<Vec<_>>();

                    if damage.is_empty() {
                        return;
                    }

                    let src = surface_view.src.to_buffer(
                        buffer_scale as f64,
                        attributes.buffer_transform.into(),
                        &dimensions.to_f64(),
                    );
                    let dst =
                        Rectangle::from_loc_and_size(location.to_f64(), surface_view.dst).to_physical(scale);

                    if src.size.w <= 0.0 || src.size.h <= 0.0 || dst.size.w <= 0.0 || dst.size.h <= 0.0 {
                        return;
                    }

                    if let Err(err) = frame.render_texture_from_to(
                        texture,
                        src,
                        dst,
                        &damage,
                        attributes.buffer_transform.into(),
                        1.0,
                    ) {
                        result = Err(err);
                    }
                }
            }
        },
    );
    result
}
