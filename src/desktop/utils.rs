//! Helper functions to ease dealing with surface trees

use crate::{
    backend::renderer::utils::SurfaceState,
    desktop::Space,
    utils::{Coordinate, Logical, Point, Rectangle, Size},
    wayland::{
        compositor::{
            with_surface_tree_downward, with_surface_tree_upward, Damage, SubsurfaceCachedState,
            SurfaceAttributes, SurfaceData, TraversalAction,
        },
        output::Output,
    },
};
use wayland_server::protocol::wl_surface;

use std::cell::RefCell;

use super::WindowSurfaceType;

pub(crate) fn surface_transform(states: &SurfaceData) -> SurfaceTransform {
    states
        .data_map
        .get::<RefCell<SurfaceTransform>>()
        .map(|t| *t.borrow())
        .unwrap_or_default()
}

/// A transform that can be attached
/// to a surface.
#[derive(Copy, Clone, Debug)]
pub struct SurfaceTransform {
    pub(crate) scale: Size<f64, Logical>,
    pub(crate) crop: Rectangle<f64, Logical>,
}

impl SurfaceTransform {
    /// Set the scale of the transform
    pub fn set_scale<S: Into<Size<f64, Logical>>>(&mut self, scale: S) {
        self.scale = scale.into();
    }

    /// Set the crop of the transform
    pub fn set_crop(&mut self, crop: Rectangle<f64, Logical>) {
        self.crop = crop;
    }

    /// Transform a point
    pub fn apply_point<N: Coordinate>(&self, point: Point<N, Logical>) -> Point<f64, Logical> {
        let mut point = point.to_f64();
        // First move the point by the crop
        let crop_loc = self.crop.loc;
        point.x = (point.x - crop_loc.x).max(0.0);
        point.y = (point.y - crop_loc.y).max(0.0);
        // Then scale the point
        point.x = point.x.upscale(self.scale.w);
        point.y = point.y.upscale(self.scale.h);
        point
    }

    /// Transform a point
    pub fn revert_point<N: Coordinate>(&self, point: Point<N, Logical>) -> Point<f64, Logical> {
        let mut point = point.to_f64();
        let crop = self.crop.loc;
        point.x = point.x.downscale(self.scale.w) + crop.x;
        point.y = point.y.downscale(self.scale.h) + crop.y;
        point
    }

    /// Transform a rect
    pub fn apply_rect<N: Coordinate>(&self, rect: Rectangle<N, Logical>) -> Rectangle<f64, Logical> {
        let rect = rect.to_f64();

        let crop = self.crop.loc;
        let loc = Point::<f64, Logical>::from((
            (rect.loc.x - crop.x).upscale(self.scale.w),
            (rect.loc.y - crop.y).upscale(self.scale.h),
        ));
        let size = Size::<f64, Logical>::from((
            rect.size.w.upscale(self.scale.w),
            rect.size.h.upscale(self.scale.h),
        ));

        Rectangle::from_loc_and_size(loc, size)
    }

    /// Transform a rect
    pub fn revert_rect<N: Coordinate>(&self, rect: Rectangle<N, Logical>) -> Rectangle<f64, Logical> {
        let rect = rect.to_f64();

        let crop = self.crop.loc;
        let loc = Point::<f64, Logical>::from((
            rect.loc.x.downscale(self.scale.w) + crop.x,
            rect.loc.y.downscale(self.scale.h) + crop.y,
        ));
        let size = Size::<f64, Logical>::from((
            rect.size.w.downscale(self.scale.w),
            rect.size.h.downscale(self.scale.h),
        ));

        Rectangle::from_loc_and_size(loc, size)
    }

    /// Transform a size
    pub fn apply_size<N: Coordinate>(&self, size: Size<N, Logical>) -> Size<f64, Logical> {
        let size = size.to_f64();
        Size::from((size.w.upscale(self.scale.w), size.h.upscale(self.scale.h)))
    }

    /// Transform a size
    pub fn revert_size<N: Coordinate>(&self, size: Size<N, Logical>) -> Size<f64, Logical> {
        let size = size.to_f64();
        Size::from((size.w.downscale(self.scale.w), size.h.downscale(self.scale.h)))
    }
}

impl Default for SurfaceTransform {
    fn default() -> Self {
        Self {
            scale: Size::from((1.0, 1.0)),
            crop: Rectangle::from_extemities((0., 0.), (f64::MAX, f64::MAX)),
        }
    }
}

impl SurfaceState {
    fn contains_point<P: Into<Point<f64, Logical>>>(&self, attrs: &SurfaceAttributes, point: P) -> bool {
        let point = point.into();
        let size = match self.surface_size() {
            None => return false, // If the surface has no size, it can't have an input region.
            Some(size) => size,
        };

        let rect = Rectangle {
            loc: (0, 0).into(),
            size,
        }
        .to_f64();

        // The input region is always within the surface itself, so if the surface itself doesn't contain the
        // point we can return false.
        if !rect.contains(point) {
            return false;
        }

        // If there's no input region, we're done.
        if attrs.input_region.is_none() {
            return true;
        }

        attrs
            .input_region
            .as_ref()
            .unwrap()
            .contains(point.to_i32_round())
    }
}

/// Returns the bounding box of a given surface and all its subsurfaces.
///
/// - `location` can be set to offset the returned bounding box.
pub fn bbox_from_surface_tree<P>(surface: &wl_surface::WlSurface, location: P) -> Rectangle<i32, Logical>
where
    P: Into<Point<i32, Logical>>,
{
    let location: Point<i32, Logical> = location.into();
    let mut bounding_box = Rectangle::from_loc_and_size(location, (0, 0));
    let parent_surface_transform = SurfaceTransform::default();

    with_surface_tree_downward(
        surface,
        (location, parent_surface_transform),
        |_, states, (loc, parent_surface_transform)| {
            let mut loc = *loc;
            let data = states.data_map.get::<RefCell<SurfaceState>>();

            if let Some(size) = data.and_then(|d| d.borrow().surface_size()) {
                let mut surface_transform = surface_transform(states);

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

                    loc += corrected_subsurface_location.to_i32_floor();

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

                    // TODO: Now check if we also define a surface local crop, if yes restrict
                    // the  subsurface_crop by the surface local crop. In case the crops do not
                    // intersect the surface will not be visible.
                    let crop_intersection = surface_transform
                        .crop
                        .intersection(subsurface_crop)
                        .unwrap_or_default();

                    surface_transform.crop = crop_intersection;
                }

                let surface_bbox = Rectangle::from_loc_and_size((0.0, 0.0), size.to_f64());
                if let Some(mut bbox) = surface_bbox.intersection(surface_transform.crop) {
                    bbox.loc -= surface_transform.crop.loc;
                    bbox.loc.x = bbox.loc.x.upscale(surface_transform.scale.w);
                    bbox.loc.y = bbox.loc.y.upscale(surface_transform.scale.h);
                    bbox.size.w = bbox.size.w.upscale(surface_transform.scale.w);
                    bbox.size.h = bbox.size.h.upscale(surface_transform.scale.h);
                    bbox.loc += loc.to_f64();
                    // Update the bounding box.
                    bounding_box = bounding_box.merge(bbox.to_i32_up());
                }
                TraversalAction::DoChildren((loc, surface_transform))
            } else {
                // If the parent surface is unmapped, then the child surfaces are hidden as
                // well, no need to consider them here.
                TraversalAction::SkipChildren
            }
        },
        |_, _, _| {},
        |_, _, _| true,
    );
    bounding_box
}

/// Returns the damage rectangles of the current buffer for a given surface and its subsurfaces.
///
/// - `location` can be set to offset the returned bounding box.
/// - if a `key` is set the damage is only returned on the first call with the given key values.
///   Subsequent calls will return an empty vector until the buffer is updated again and new
///   damage values may be retrieved.
pub fn damage_from_surface_tree<P>(
    surface: &wl_surface::WlSurface,
    location: P,
    key: Option<(&Space, &Output)>,
) -> Vec<Rectangle<i32, Logical>>
where
    P: Into<Point<i32, Logical>>,
{
    use super::space::SpaceOutputTuple;

    let mut damage = Vec::new();
    let key = key.map(|x| SpaceOutputTuple::from(x).owned_hash());
    let parent_surface_transform = SurfaceTransform::default();
    let location: Point<i32, Logical> = location.into();

    with_surface_tree_upward(
        surface,
        (location, parent_surface_transform),
        |_surface, states, (location, parent_surface_transform)| {
            let mut location = *location;
            let mut surface_transform = surface_transform(states);
            if let Some(data) = states.data_map.get::<RefCell<SurfaceState>>() {
                let data = data.borrow();
                if key
                    .as_ref()
                    .map(|key| !data.damage_seen.contains(key))
                    .unwrap_or(true)
                    && states.role == Some("subsurface")
                {
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

                    // TODO: Now check if we also define a surface local crop, if yes restrict
                    // the  subsurface_crop by the surface local crop. In case the crops do not
                    // intersect the surface will not be visible.
                    let crop_intersection = surface_transform
                        .crop
                        .intersection(subsurface_crop)
                        .unwrap_or_default();

                    surface_transform.crop = crop_intersection;
                }
            }
            // TODO: Inherit/Correct scale from parent and local transform
            TraversalAction::DoChildren((location, surface_transform))
        },
        |_surface, states, (location, parent_surface_transform)| {
            let mut location = *location;
            if let Some(data) = states.data_map.get::<RefCell<SurfaceState>>() {
                let mut data = data.borrow_mut();
                let attributes = states.cached_state.current::<SurfaceAttributes>();

                if key
                    .as_ref()
                    .map(|key| !data.damage_seen.contains(key))
                    .unwrap_or(true)
                {
                    let mut surface_transform = surface_transform(states);

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

                        // TODO: Now check if we also define a surface local crop, if yes restrict
                        // the  subsurface_crop by the surface local crop. In case the crops do not
                        // intersect the surface will not be visible.
                        let crop_intersection = surface_transform
                            .crop
                            .intersection(subsurface_crop)
                            .unwrap_or_default();

                        surface_transform.crop = crop_intersection;
                    }

                    damage.extend(attributes.damage.iter().flat_map(|dmg| {
                        let damage = match dmg {
                            Damage::Buffer(rect) => rect.to_logical(
                                attributes.buffer_scale,
                                attributes.buffer_transform.into(),
                                &data.buffer_dimensions.unwrap(),
                            ),
                            Damage::Surface(rect) => *rect,
                        };
                        damage
                            .to_f64()
                            .intersection(surface_transform.crop)
                            .map(|mut damage| {
                                damage.loc -= surface_transform.crop.loc;
                                damage.loc.x = damage.loc.x.upscale(surface_transform.scale.w);
                                damage.loc.y = damage.loc.y.upscale(surface_transform.scale.h);
                                damage.size.w = damage.size.w.upscale(surface_transform.scale.w);
                                damage.size.h = damage.size.h.upscale(surface_transform.scale.h);
                                damage.loc += location.to_f64();
                                damage.to_i32_up()
                            })
                    }));

                    if let Some(key) = key {
                        data.damage_seen.insert(key);
                    }
                }
            }
        },
        |_, _, _| true,
    );
    damage
}

/// Returns the (sub-)surface under a given position given a surface, if any.
///
/// - `point` has to be the position to query, relative to (0, 0) of the given surface + `location`.
/// - `location` can be used to offset the returned point.
pub fn under_from_surface_tree<P>(
    surface: &wl_surface::WlSurface,
    point: Point<f64, Logical>,
    location: P,
    surface_type: WindowSurfaceType,
) -> Option<(wl_surface::WlSurface, Point<i32, Logical>)>
where
    P: Into<Point<i32, Logical>>,
{
    let found = RefCell::new(None);
    let parent_surface_transform = SurfaceTransform::default();
    let location: Point<i32, Logical> = location.into();

    with_surface_tree_downward(
        surface,
        (location, parent_surface_transform),
        |wl_surface, states, (location, parent_surface_transform)| {
            let mut location = *location;
            let data = states.data_map.get::<RefCell<SurfaceState>>();

            let surface_transform = surface_transform(states);

            if states.role == Some("subsurface") {
                let current = states.cached_state.current::<SubsurfaceCachedState>();
                location += parent_surface_transform
                    .apply_point(current.location)
                    .to_i32_ceil();
            }

            if states.role == Some("subsurface") || surface_type.contains(WindowSurfaceType::TOPLEVEL) {
                let contains_the_point = data
                    .map(|data| {
                        let surface_point = surface_transform.revert_point(point - location.to_f64());
                        data.borrow()
                            .contains_point(&*states.cached_state.current(), surface_point)
                    })
                    .unwrap_or(false);
                if contains_the_point {
                    *found.borrow_mut() = Some((wl_surface.clone(), location));
                }
            }

            if surface_type.contains(WindowSurfaceType::SUBSURFACE) {
                TraversalAction::DoChildren((location, surface_transform))
            } else {
                TraversalAction::SkipChildren
            }
        },
        |_, _, _| {},
        |_, _, _| {
            // only continue if the point is not found
            found.borrow().is_none()
        },
    );
    found.into_inner()
}

/// Sends frame callbacks for a surface and its subsurfaces with the given `time`.
pub fn send_frames_surface_tree(surface: &wl_surface::WlSurface, time: u32) {
    with_surface_tree_downward(
        surface,
        (),
        |_, _, &()| TraversalAction::DoChildren(()),
        |_surf, states, &()| {
            // the surface may not have any user_data if it is a subsurface and has not
            // yet been commited
            for callback in states
                .cached_state
                .current::<SurfaceAttributes>()
                .frame_callbacks
                .drain(..)
            {
                callback.done(time);
            }
        },
        |_, _, &()| true,
    );
}

pub(crate) fn output_update(
    output: &Output,
    output_geometry: Rectangle<i32, Logical>,
    surface_list: &mut Vec<wl_surface::WlSurface>,
    surface: &wl_surface::WlSurface,
    location: Point<i32, Logical>,
    logger: &slog::Logger,
) {
    let parent_surface_transform = SurfaceTransform::default();

    with_surface_tree_downward(
        surface,
        (location, parent_surface_transform),
        |_, states, (location, parent_surface_transform)| {
            let mut location = *location;
            let data = states.data_map.get::<RefCell<SurfaceState>>();

            if data.is_some() {
                let surface_transform = surface_transform(states);
                if states.role == Some("subsurface") {
                    let current = states.cached_state.current::<SubsurfaceCachedState>();
                    location += parent_surface_transform
                        .apply_point(current.location)
                        .to_i32_floor();
                }

                TraversalAction::DoChildren((location, surface_transform))
            } else {
                // If the parent surface is unmapped, then the child surfaces are hidden as
                // well, no need to consider them here.
                TraversalAction::SkipChildren
            }
        },
        |wl_surface, states, (loc, parent_surface_transform)| {
            let mut loc = *loc;
            let data = states.data_map.get::<RefCell<SurfaceState>>();

            if let Some(size) = data.and_then(|d| d.borrow().surface_size()) {
                let surface_transform = surface_transform(states);

                if states.role == Some("subsurface") {
                    let current = states.cached_state.current::<SubsurfaceCachedState>();
                    loc += parent_surface_transform
                        .apply_point(current.location)
                        .to_i32_floor();
                }

                let size = surface_transform.apply_size(size).to_i32_ceil();

                let surface_rectangle = Rectangle { loc, size };
                if output_geometry.overlaps(surface_rectangle) {
                    // We found a matching output, check if we already sent enter
                    output_enter(output, surface_list, wl_surface, logger);
                } else {
                    // Surface does not match output, if we sent enter earlier
                    // we should now send leave
                    output_leave(output, surface_list, wl_surface, logger);
                }
            } else {
                // Maybe the the surface got unmapped, send leave on output
                output_leave(output, surface_list, wl_surface, logger);
            }
        },
        |_, _, _| true,
    );
}

pub(crate) fn output_enter(
    output: &Output,
    surface_list: &mut Vec<wl_surface::WlSurface>,
    surface: &wl_surface::WlSurface,
    logger: &slog::Logger,
) {
    if !surface_list.contains(surface) {
        slog::debug!(
            logger,
            "surface ({:?}) entering output {:?}",
            surface,
            output.name()
        );
        output.enter(surface);
        surface_list.push(surface.clone());
    }
}

pub(crate) fn output_leave(
    output: &Output,
    surface_list: &mut Vec<wl_surface::WlSurface>,
    surface: &wl_surface::WlSurface,
    logger: &slog::Logger,
) {
    if surface_list.contains(surface) {
        slog::debug!(
            logger,
            "surface ({:?}) leaving output {:?}",
            surface,
            output.name()
        );
        output.leave(surface);
        surface_list.retain(|s| s != surface);
    }
}
