//! Utility module for helpers around drawing [`WlSurface`]s with [`Renderer`]s.

use crate::{
    backend::renderer::{buffer_dimensions, Frame, ImportAll, Renderer},
    desktop::utils::{surface_transform, SurfaceTransform},
    utils::{Buffer, Coordinate, Logical, Point, Rectangle, Size, Transform},
    wayland::compositor::{
        is_sync_subsurface, with_surface_tree_upward, BufferAssignment, Damage, SubsurfaceCachedState,
        SurfaceAttributes, SurfaceData, TraversalAction,
    },
};
use std::collections::VecDeque;
use std::{
    any::TypeId,
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
};
use wayland_server::protocol::{wl_buffer::WlBuffer, wl_surface::WlSurface};

use super::Texture;

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
    }
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
    F: FnMut(&WlSurface, &SurfaceData, &(Point<i32, Logical>, Point<i32, Logical>, SurfaceTransform)),
{
    let texture_id = (TypeId::of::<<R as Renderer>::TextureId>(), renderer.id());
    let mut result = Ok(());
    let parent_surface_transform = SurfaceTransform::default();
    with_surface_tree_upward(
        surface,
        (location, (0, 0).into(), parent_surface_transform),
        |_surface, states, (location, surface_offset, parent_surface_transform)| {
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
                    let mut surface_transform = surface_transform(states);
                    // if yes, also process the children
                    if states.role == Some("subsurface") {
                        let current = states.cached_state.current::<SubsurfaceCachedState>();
                        let current_location = current.location.to_f64();
                        // When we subtract the crop location we will receive either something positive,
                        // which is our new corrected location or something negative in which case we need to
                        // also crop ourself and set the corrected location to zero.
                        let parent_crop_loc = parent_surface_transform.crop.loc;
                        let temp = current_location - parent_crop_loc;

                        let (subsurface_x_crop, subsurface_x_location) = if temp.x.is_sign_negative() {
                            (temp.x.abs(), 0.0)
                        } else {
                            (0.0, temp.x)
                        };

                        let (subsurface_y_crop, subsurface_y_location) = if temp.y.is_sign_negative() {
                            (temp.y.abs(), 0.0)
                        } else {
                            (0.0, temp.y)
                        };
                        // Now apply the scaling to the location
                        let corrected_subsurface_location = Point::<f64, Logical>::from((
                            subsurface_x_location.upscale(parent_surface_transform.scale.w),
                            subsurface_y_location.upscale(parent_surface_transform.scale.h),
                        ));

                        location += corrected_subsurface_location.to_i32_floor();
                        surface_offset += corrected_subsurface_location.to_i32_floor();

                        // Now we need to calculate the crop for our subsurfaces
                        let mut parent_crop_size = parent_surface_transform.crop.size;
                        // Correct the size so that the subsurface crop can not extend over
                        // the parent crop. Note: Do not use the scaled subsurface location here!
                        parent_crop_size.w -= subsurface_x_location;
                        parent_crop_size.h -= subsurface_y_location;
                        let subsurface_crop = Rectangle::<f64, Logical>::from_loc_and_size(
                            (subsurface_x_crop, subsurface_y_crop),
                            parent_crop_size,
                        );

                        // Now check if we also define a surface local crop, if yes restrict
                        // the  subsurface_crop by the surface local crop. In case the crops do not
                        // intersect the surface will not be visible.
                        let crop_intersection = surface_transform
                            .crop
                            .intersection(subsurface_crop)
                            .unwrap_or_default();

                        surface_transform.crop = crop_intersection;
                    }
                    TraversalAction::DoChildren((location, surface_offset, surface_transform))
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
        |_surface, states, (location, surface_offset, parent_surface_transform)| {
            let mut location = *location;
            let mut surface_offset = *surface_offset;
            if let Some(data) = states.data_map.get::<RefCell<SurfaceState>>() {
                let mut data = data.borrow_mut();
                let dimensions = data.surface_size();
                let buffer_scale = data.buffer_scale;
                let buffer_transform = data.buffer_transform;
                let attributes = states.cached_state.current::<SurfaceAttributes>();
                if let Some(texture) = data
                    .textures
                    .get_mut(&texture_id)
                    .and_then(|x| x.downcast_mut::<<R as Renderer>::TextureId>())
                {
                    let mut surface_transform = surface_transform(states);
                    let dimensions = dimensions.unwrap();
                    // we need to re-extract the subsurface offset, as the previous closure
                    // only passes it to our children
                    if states.role == Some("subsurface") {
                        let current = states.cached_state.current::<SubsurfaceCachedState>();
                        let current_location = current.location.to_f64();
                        // When we subtract the crop location we will receive either something positive,
                        // which is our new corrected location or something negative in which case we need to
                        // also crop ourself and set the corrected location to zero.
                        let parent_crop_loc = parent_surface_transform.crop.loc;
                        let temp = current_location - parent_crop_loc;

                        let (subsurface_x_crop, subsurface_x_location) = if temp.x.is_sign_negative() {
                            (temp.x.abs(), 0.0)
                        } else {
                            (0.0, temp.x)
                        };

                        let (subsurface_y_crop, subsurface_y_location) = if temp.y.is_sign_negative() {
                            (temp.y.abs(), 0.0)
                        } else {
                            (0.0, temp.y)
                        };
                        // Now apply the scaling to the location
                        let corrected_subsurface_location = Point::<f64, Logical>::from((
                            subsurface_x_location.upscale(parent_surface_transform.scale.w),
                            subsurface_y_location.upscale(parent_surface_transform.scale.h),
                        ));

                        surface_offset += corrected_subsurface_location.to_i32_floor();
                        location += corrected_subsurface_location.to_i32_floor();

                        // Now we need to calculate the crop for our subsurfaces
                        let mut parent_crop_size = parent_surface_transform.crop.size;
                        // Correct the size so that the subsurface crop can not extend over
                        // the parent crop. Note: Do not use the scaled subsurface location here!
                        parent_crop_size.w -= subsurface_x_location;
                        parent_crop_size.h -= subsurface_y_location;
                        let subsurface_crop = Rectangle::<f64, Logical>::from_loc_and_size(
                            (subsurface_x_crop, subsurface_y_crop),
                            parent_crop_size,
                        );

                        // Now check if we also define a surface local crop, if yes restrict
                        // the  subsurface_crop by the surface local crop. In case the crops do not
                        // intersect the surface will not be visible.
                        let crop_intersection = surface_transform
                            .crop
                            .intersection(subsurface_crop)
                            .unwrap_or_default();

                        surface_transform.crop = crop_intersection;
                    }

                    let surface_geo =
                        Rectangle::from_loc_and_size((0.0f64, 0.0f64), dimensions.to_f64()).intersection(
                            Rectangle::from_loc_and_size((0.0f64, 0.0f64), surface_transform.crop.size),
                        );

                    let mut surface_geo = match surface_geo {
                        Some(geo) => geo,
                        None => {
                            // TODO: no intersection between damage and crop, should this happen here?
                            // Probably should be already handled in `damage_from_surface_tree`
                            return;
                        }
                    };

                    // Upsacle the geo
                    surface_geo.size.w = surface_geo.size.w.upscale(surface_transform.scale.w);
                    surface_geo.size.h = surface_geo.size.h.upscale(surface_transform.scale.h);

                    let damage = damage
                        .iter()
                        .cloned()
                        // first move the damage by the surface offset in logical space
                        .map(|mut geo| {
                            // make the damage relative to the surface
                            geo.loc -= surface_offset;
                            geo
                        })
                        // then clamp to surface size again in logical space
                        .flat_map(|geo| geo.intersection(Rectangle::from_loc_and_size((0, 0), dimensions)))
                        // lastly transform it into physical space
                        .map(|geo| geo.to_f64().to_physical(scale))
                        .collect::<Vec<_>>();

                    if damage.is_empty() {
                        return;
                    }

                    let src = Rectangle::from_loc_and_size(
                        Point::<f64, Logical>::from((0., 0.)),
                        dimensions.to_f64(),
                    )
                    .intersection(surface_transform.crop)
                    .map(|rect| rect.to_buffer(buffer_scale as f64, buffer_transform, &dimensions.to_f64()));

                    let pos = location.to_f64().to_physical(scale);
                    let dst = Rectangle::from_loc_and_size(pos, surface_geo.size.to_physical(scale));

                    let src = match src {
                        Some(src) => src,
                        None => {
                            return;
                        }
                    };

                    // TODO: Take wp_viewporter into account
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
