//! Utility module for helpers around drawing [`WlSurface`]s with [`Renderer`]s.

use crate::{
    backend::renderer::{buffer_dimensions, Frame, ImportAll, Renderer},
    desktop::utils::{surface_transform, SurfaceTransform},
    utils::{Buffer, Coordinate, Logical, Point, Rectangle, Size, Transform},
    wayland::compositor::{
        is_sync_subsurface, with_surface_tree_upward, BufferAssignment, Damage, SubsurfaceCachedState,
        SurfaceAttributes, TraversalAction,
    },
};
#[cfg(feature = "desktop")]
use std::collections::HashSet;
use std::{
    any::TypeId,
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
};
use wayland_server::protocol::{wl_buffer::WlBuffer, wl_surface::WlSurface};

use super::Texture;

#[derive(Default)]
pub(crate) struct SurfaceState {
    pub(crate) buffer_dimensions: Option<Size<i32, Buffer>>,
    pub(crate) buffer_scale: i32,
    pub(crate) buffer_transform: Transform,
    pub(crate) buffer: Option<WlBuffer>,
    pub(crate) textures: HashMap<(TypeId, usize), Box<dyn std::any::Any>>,
    #[cfg(feature = "desktop")]
    pub(crate) damage_seen: HashSet<crate::desktop::space::SpaceOutputHash>,
}

impl SurfaceState {
    pub fn update_buffer(&mut self, attrs: &mut SurfaceAttributes) {
        match attrs.buffer.take() {
            Some(BufferAssignment::NewBuffer { buffer, .. }) => {
                // new contents
                self.buffer_dimensions = buffer_dimensions(&buffer);
                self.buffer_scale = attrs.buffer_scale;
                self.buffer_transform = attrs.buffer_transform.into();
                if let Some(old_buffer) = std::mem::replace(&mut self.buffer, Some(buffer)) {
                    if &old_buffer != self.buffer.as_ref().unwrap() {
                        old_buffer.release();
                    }
                }
                self.textures.clear();
                #[cfg(feature = "desktop")]
                self.damage_seen.clear();
            }
            Some(BufferAssignment::Removed) => {
                // remove the contents
                self.buffer_dimensions = None;
                if let Some(buffer) = self.buffer.take() {
                    buffer.release();
                };
                self.textures.clear();
                #[cfg(feature = "desktop")]
                self.damage_seen.clear();
            }
            None => {}
        }
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
    let mut result = Ok(());
    let texture_id = (TypeId::of::<<R as Renderer>::TextureId>(), renderer.id());
    with_surface_tree_upward(
        surface,
        (),
        |_surface, states, _| {
            if let Some(data) = states.data_map.get::<RefCell<SurfaceState>>() {
                let mut data_ref = data.borrow_mut();
                let data = &mut *data_ref;
                let attributes = states.cached_state.current::<SurfaceAttributes>();
                // Import a new buffer if available
                let surface_size = data.surface_size();
                if let Entry::Vacant(e) = data.textures.entry(texture_id) {
                    if let Some(buffer) = data.buffer.as_ref() {
                        let surface_size = surface_size.unwrap();
                        let buffer_damage = attributes
                            .damage
                            .iter()
                            .map(|dmg| match dmg {
                                Damage::Buffer(rect) => *rect,
                                Damage::Surface(rect) => rect.to_buffer(
                                    attributes.buffer_scale,
                                    attributes.buffer_transform.into(),
                                    &surface_size,
                                ),
                            })
                            .collect::<Vec<_>>();

                        match renderer.import_buffer(buffer, Some(states), &buffer_damage) {
                            Some(Ok(m)) => {
                                e.insert(Box::new(m));
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
                // Now, was the import successful?
                if data.textures.contains_key(&texture_id) {
                    TraversalAction::DoChildren(())
                } else {
                    // we are not displayed, so our children are neither
                    TraversalAction::SkipChildren
                }
            } else {
                // we are not displayed, so our children are neither
                TraversalAction::SkipChildren
            }
        },
        |_, _, _| {},
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
    let mut result = Ok(());
    let texture_id = (TypeId::of::<<R as Renderer>::TextureId>(), renderer.id());
    let parent_surface_transform = SurfaceTransform::default();

    with_surface_tree_upward(
        surface,
        (location, parent_surface_transform),
        |_surface, states, (location, parent_surface_transform)| {
            let mut location = *location;
            if let Some(data) = states.data_map.get::<RefCell<SurfaceState>>() {
                let mut data_ref = data.borrow_mut();
                let data = &mut *data_ref;
                let attributes = states.cached_state.current::<SurfaceAttributes>();
                // Import a new buffer if necessary
                let surface_size = data.surface_size();
                if let Entry::Vacant(e) = data.textures.entry(texture_id) {
                    if let Some(buffer) = data.buffer.as_ref() {
                        let surface_size = surface_size.unwrap();
                        let buffer_damage = attributes
                            .damage
                            .iter()
                            .map(|dmg| match dmg {
                                Damage::Buffer(rect) => *rect,
                                Damage::Surface(rect) => rect.to_buffer(
                                    attributes.buffer_scale,
                                    attributes.buffer_transform.into(),
                                    &surface_size,
                                ),
                            })
                            .collect::<Vec<_>>();

                        match renderer.import_buffer(buffer, Some(states), &buffer_damage) {
                            Some(Ok(m)) => {
                                e.insert(Box::new(m));
                            }
                            Some(Err(err)) => {
                                slog::warn!(log, "Error loading buffer: {}", err);
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
                    TraversalAction::DoChildren((location, surface_transform))
                } else {
                    // we are not displayed, so our children are neither
                    TraversalAction::SkipChildren
                }
            } else {
                // we are not displayed, so our children are neither
                TraversalAction::SkipChildren
            }
        },
        |_surface, states, (location, parent_surface_transform)| {
            let mut location = *location;
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
                    let mut surface_offset = (0, 0).into();
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

                        surface_offset = corrected_subsurface_location.to_i32_floor();
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
                            // TODO: Log?
                            eprintln!("no overlap");
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
                            // make the damage relative to the surfaec
                            geo.loc -= surface_offset;
                            geo
                        })
                        // then clamp to surface size again in logical space
                        .flat_map(|geo| {
                            geo.to_f64().intersection(surface_geo).map(|mut geo| {
                                // Scale the damage back to surface local coorinates
                                geo.loc.x = geo.loc.x.downscale(surface_transform.scale.w);
                                geo.loc.y = geo.loc.y.downscale(surface_transform.scale.h);
                                geo.size.w = geo.size.w.downscale(surface_transform.scale.w);
                                geo.size.h = geo.size.h.downscale(surface_transform.scale.h);
                                geo
                            })
                        })
                        // lastly transform it into buffer space
                        .map(|geo| geo.to_buffer(buffer_scale as f64, buffer_transform, &dimensions.to_f64()))
                        .collect::<Vec<_>>();

                    let pos = location.to_f64().to_physical(scale);
                    let dst = Rectangle::from_loc_and_size(pos, surface_geo.size.to_physical(scale));

                    let src = Rectangle::from_loc_and_size(
                        Point::<f64, Buffer>::from((0., 0.)),
                        texture.size().to_f64(),
                    );
                    let surface_crop = surface_transform.crop.to_buffer(
                        buffer_scale as f64,
                        buffer_transform,
                        &dimensions.to_f64(),
                    );
                    let src = match src.intersection(surface_crop) {
                        Some(src) => src,
                        None => {
                            // TODO: Log?
                            eprintln!("no intersect");
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
        |_, _, _| true,
    );

    result
}
