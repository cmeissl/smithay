//! Helper functions to ease dealing with surface trees

use crate::{
    backend::renderer::utils::SurfaceState,
    desktop::Space,
    utils::{Logical, Point, Rectangle},
    wayland::{
        compositor::{
            with_surface_tree_downward, with_surface_tree_upward, SurfaceAttributes, TraversalAction,
        },
        output::Output,
    },
};
use wayland_server::protocol::wl_surface;

use std::cell::RefCell;

use super::WindowSurfaceType;

impl SurfaceState {
    fn contains_point<P: Into<Point<f64, Logical>>>(&self, attrs: &SurfaceAttributes, point: P) -> bool {
        let point = point.into();
        let size = match self.surface_view.map(|view| view.dst) {
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
    let location = location.into();
    let mut bounding_box = Rectangle::from_loc_and_size(location, (0, 0));
    with_surface_tree_downward(
        surface,
        location,
        |_, states, loc: &Point<i32, Logical>| {
            let mut loc = *loc;
            let data = states.data_map.get::<RefCell<SurfaceState>>();

            if let Some(surface_view) = data.and_then(|d| d.borrow().surface_view) {
                loc += surface_view.offset;
                // Update the bounding box.
                bounding_box = bounding_box.merge(Rectangle::from_loc_and_size(loc, surface_view.dst));

                TraversalAction::DoChildren(loc)
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
///   damage values may be retrieved. Additionally damage may be internally accumulated, if
///   multiple commits occurred between different calls.
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
    with_surface_tree_upward(
        surface,
        location.into(),
        |_surface, states, location| {
            let mut location = *location;

            if let Some(surface_view) = states
                .data_map
                .get::<RefCell<SurfaceState>>()
                .and_then(|d| d.borrow().surface_view)
            {
                location += surface_view.offset;
                TraversalAction::DoChildren(location)
            } else {
                TraversalAction::SkipChildren
            }
        },
        |_surface, states, location| {
            let mut location = *location;
            if let Some(data) = states.data_map.get::<RefCell<SurfaceState>>() {
                let mut data = data.borrow_mut();
                if key
                    .as_ref()
                    .map(|key| data.space_seen.get(key).copied().unwrap_or(0) < data.commit_count)
                    .unwrap_or(true)
                {
                    if let Some(surface_view) = data.surface_view {
                        location += surface_view.offset;

                        let new_damage = key
                            .as_ref()
                            .map(|key| data.damage_since(data.space_seen.get(key).copied()))
                            .unwrap_or_else(|| {
                                data.damage.front().cloned().unwrap_or_else(|| {
                                    data.buffer_dimensions
                                        .as_ref()
                                        .map(|size| vec![Rectangle::from_loc_and_size((0, 0), *size)])
                                        .unwrap_or_else(Vec::new)
                                })
                            });

                        damage.extend(new_damage.into_iter().flat_map(|rect| {
                            rect.to_f64()
                                // first bring the damage into logical space
                                // Note: We use f64 for this as the damage could
                                // be not dividable by the buffer scale without
                                // a rest
                                .to_logical(
                                    data.buffer_scale as f64,
                                    data.buffer_transform,
                                    &data.buffer_dimensions.unwrap().to_f64(),
                                )
                                // then crop by the surface view (viewporter for example could define a src rect)
                                .intersection(surface_view.src)
                                // move and scale the cropped rect (viewporter could define a dst size)
                                .map(|rect| surface_view.rect_to_global(rect).to_i32_up())
                                // at last move the damage relative to the surface
                                .map(|mut rect| {
                                    rect.loc += location;
                                    rect
                                })
                        }));

                        if let Some(key) = key {
                            let current_commit = data.commit_count;
                            data.space_seen.insert(key, current_commit);
                        }
                    }
                }
            }
        },
        |_, _, _| true,
    );
    damage
}

/// Returns the topmost (sub-)surface under a given position matching the input regions of the surface.
///
/// In case no surface input region matches the point [`None`] is returned.
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
    with_surface_tree_downward(
        surface,
        location.into(),
        |wl_surface, states, location: &Point<i32, Logical>| {
            let mut location = *location;
            let data = states.data_map.get::<RefCell<SurfaceState>>();

            if let Some(surface_view) = data.and_then(|d| d.borrow().surface_view) {
                location += surface_view.offset;

                if states.role == Some("subsurface") || surface_type.contains(WindowSurfaceType::TOPLEVEL) {
                    let contains_the_point = data
                        .map(|data| {
                            data.borrow()
                                .contains_point(&*states.cached_state.current(), point - location.to_f64())
                        })
                        .unwrap_or(false);
                    if contains_the_point {
                        *found.borrow_mut() = Some((wl_surface.clone(), location));
                    }
                }

                if surface_type.contains(WindowSurfaceType::SUBSURFACE) {
                    TraversalAction::DoChildren(location)
                } else {
                    TraversalAction::SkipChildren
                }
            } else {
                // We are completely hidden
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
    with_surface_tree_downward(
        surface,
        (location, false),
        |_, states, (location, parent_unmapped)| {
            let mut location = *location;
            let data = states.data_map.get::<RefCell<SurfaceState>>();

            if *parent_unmapped {
                TraversalAction::DoChildren((location, true))
            } else if let Some(surface_view) = data.and_then(|d| d.borrow().surface_view) {
                location += surface_view.offset;
                TraversalAction::DoChildren((location, false))
            } else {
                TraversalAction::DoChildren((location, true))
            }
        },
        |wl_surface, states, (location, parent_unmapped)| {
            let mut location = *location;

            if *parent_unmapped {
                // The parent is unmapped so we are
                output_leave(output, surface_list, wl_surface, logger);
                return;
            }

            let data = states.data_map.get::<RefCell<SurfaceState>>();

            if let Some(surface_view) = data.and_then(|d| d.borrow().surface_view) {
                location += surface_view.offset;
                let surface_rectangle = Rectangle::from_loc_and_size(location, surface_view.dst);
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
