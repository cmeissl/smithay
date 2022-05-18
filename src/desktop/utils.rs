//! Helper functions to ease dealing with surface trees

use crate::{
    backend::renderer::utils::SurfaceState,
    desktop::Space,
    utils::{Logical, Point, Rectangle, Scale},
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
pub fn bbox_from_surface_tree<P>(
    surface: &wl_surface::WlSurface,
    location: P,
    src: Option<Rectangle<i32, Logical>>,
    scale: impl Into<Scale<f64>>,
) -> Rectangle<i32, Logical>
where
    P: Into<Point<i32, Logical>>,
{
    let location = location.into();
    let scale = scale.into();
    let mut bounding_box = Rectangle::from_loc_and_size(location, (0, 0));
    with_surface_tree_downward(
        surface,
        (Point::from((0, 0)), None),
        |_, states, (surface_offset, parent_crop)| {
            let mut surface_offset = *surface_offset;
            let data = states.data_map.get::<RefCell<SurfaceState>>();

            if let Some(surface_view) = data.and_then(|d| d.borrow().surface_view) {
                // We use the surface view dst size here as this is the size
                // the surface would be rendered on screen without applying the
                // crop but it already includes wp_viewporter
                let surface_rect = Rectangle::from_loc_and_size((0, 0), surface_view.dst);

                let src = src
                    .map(|mut src| {
                        // Move the src rect relative to the surface
                        src.loc -= surface_offset + surface_view.offset;
                        src
                    })
                    .unwrap_or(surface_rect);

                if let Some(intersection) = surface_rect.intersection(src) {
                    let mut offset = surface_view.offset;

                    // Correct the offset by the (parent)crop
                    if let Some(parent_crop) = *parent_crop {
                        offset = (offset + intersection.loc) - parent_crop;
                    }

                    surface_offset += offset;

                    let pos = location + surface_offset.to_f64().upscale(scale).to_i32_round();
                    let size = intersection.size.to_f64().upscale(scale).to_i32_round();
                    let rect = Rectangle::from_loc_and_size(pos, size);

                    bounding_box = bounding_box.merge(rect);

                    TraversalAction::DoChildren((surface_offset, Some(intersection.loc)))
                } else {
                    // We are completely cropped, so are our children
                    TraversalAction::SkipChildren
                }
            } else {
                // We are not mapped, so are our children
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
    src: Option<Rectangle<i32, Logical>>,
    scale: impl Into<Scale<f64>>,
) -> Vec<Rectangle<i32, Logical>>
where
    P: Into<Point<i32, Logical>>,
{
    use super::space::SpaceOutputTuple;

    let location = location.into();
    let scale = scale.into();

    let mut damage = Vec::new();
    let key = key.map(|x| SpaceOutputTuple::from(x).owned_hash());
    with_surface_tree_upward(
        surface,
        (Point::from((0, 0)), None),
        |_surface, states, (surface_offset, parent_crop)| {
            let mut surface_offset = *surface_offset;
            let data = states.data_map.get::<RefCell<SurfaceState>>();

            if let Some(surface_view) = data.and_then(|d| d.borrow().surface_view) {
                // We use the surface view dst size here as this is the size
                // the surface would be rendered on screen without applying the
                // crop but it already includes wp_viewporter
                let surface_rect = Rectangle::from_loc_and_size((0, 0), surface_view.dst);

                let src = src
                    .map(|mut src| {
                        // Move the src rect relative to the surface
                        src.loc -= surface_offset + surface_view.offset;
                        src
                    })
                    .unwrap_or(surface_rect);

                if let Some(intersection) = surface_rect.intersection(src) {
                    let mut offset = surface_view.offset;

                    // Correct the offset by the (parent)crop
                    if let Some(parent_crop) = *parent_crop {
                        offset = (offset + intersection.loc) - parent_crop;
                    }

                    surface_offset += offset;

                    TraversalAction::DoChildren((surface_offset, Some(intersection.loc)))
                } else {
                    // We are completely cropped, so are our children
                    TraversalAction::SkipChildren
                }
            } else {
                // We are not mapped, so are our children
                TraversalAction::SkipChildren
            }
        },
        |_surface, states, (surface_offset, parent_crop)| {
            let mut surface_offset = *surface_offset;
            if let Some(data) = states.data_map.get::<RefCell<SurfaceState>>() {
                let mut data = data.borrow_mut();
                if key
                    .as_ref()
                    .map(|key| data.space_seen.get(key).copied().unwrap_or(0) < data.commit_count)
                    .unwrap_or(true)
                {
                    if let Some(surface_view) = data.surface_view {
                        // We use the surface view dst size here as this is the size
                        // the surface would be rendered on screen without applying the
                        // crop but it already includes wp_viewporter
                        let surface_rect = Rectangle::from_loc_and_size((0, 0), surface_view.dst);

                        let src = src
                            .map(|mut src| {
                                // Move the src rect relative to the surface
                                src.loc -= surface_offset + surface_view.offset;
                                src
                            })
                            .unwrap_or(surface_rect);

                        let intersection = match surface_rect.intersection(src) {
                            Some(rect) => rect,
                            None => {
                                return;
                            }
                        };

                        let mut offset = surface_view.offset;

                        // Correct the offset by the (parent)crop
                        if let Some(parent_crop) = *parent_crop {
                            offset = (offset + intersection.loc) - parent_crop;
                        }

                        surface_offset += offset;

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
                            // First bring the buffer damage to logical space
                            rect.to_f64()
                                .to_logical(
                                    data.buffer_scale as f64,
                                    data.buffer_transform,
                                    &data.buffer_dimensions.unwrap().to_f64(),
                                )
                                // Then crop by the src ignoring any damage outside of it
                                .intersection(surface_view.src)
                                .map(|rect| {
                                    // Now move the rect into global space (for example viewporter could apply a scale)
                                    surface_view.rect_to_global(rect).to_i32_up()
                                })
                                .and_then(|rect| {
                                    // Then apply the compositor driven crop and scale
                                    rect.intersection(src).map(|rect| {
                                        // let pos = (rect.loc - intersection.loc)
                                        //     .to_f64()
                                        //     .upscale(scale)
                                        //     .to_i32_floor()
                                        //     + location
                                        //     + surface_offset.to_f64().upscale(scale).to_i32_floor();
                                        // let tmp = (rect.size.to_point() + rect.loc)
                                        //     .to_f64()
                                        //     .upscale(scale)
                                        //     .to_i32_ceil::<i32>()
                                        //     - rect.loc.to_f64().upscale(scale).to_i32_floor();
                                        // let size = rect.size.to_f64().upscale(scale).to_i32_ceil::<i32>();
                                        // dbg!(tmp);
                                        // dbg!(size);
                                        // dbg!(Rectangle::from_loc_and_size(pos, tmp.to_size()))
                                        // rect.loc -= intersection.loc;
                                        // rect.loc += surface_offset;
                                        // rect.to_f64().upscale(scale).to_i32_up()
                                        let mut rect = rect;
                                        rect.loc += location + surface_offset;
                                        rect
                                    })
                                })
                            // .map(|mut rect| {
                            //     rect.loc += location;
                            //     rect
                            // })
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
    src: Option<Rectangle<i32, Logical>>,
    scale: impl Into<Scale<f64>>,
) -> Option<(wl_surface::WlSurface, Point<i32, Logical>)>
where
    P: Into<Point<i32, Logical>>,
{
    let scale = scale.into();
    let location = location.into();
    let surface_tree_local_point = (point - location.to_f64()).downscale(scale);

    let found = RefCell::new(None);
    with_surface_tree_downward(
        surface,
        (Point::from((0, 0)), None),
        |surface, states, (surface_offset, parent_crop)| {
            let mut surface_offset = *surface_offset;
            let data = states.data_map.get::<RefCell<SurfaceState>>();

            if let Some(surface_view) = data.and_then(|d| d.borrow().surface_view) {
                // We use the surface view dst size here as this is the size
                // the surface would be rendered on screen without applying the
                // crop but it already includes wp_viewporter
                let surface_rect = Rectangle::from_loc_and_size((0, 0), surface_view.dst);

                let src = src
                    .map(|mut src| {
                        // Move the src rect relative to the surface
                        src.loc -= surface_offset + surface_view.offset;
                        src
                    })
                    .unwrap_or(surface_rect);

                if let Some(intersection) = surface_rect.intersection(src) {
                    let mut offset = surface_view.offset;

                    // Correct the offset by the (parent)crop
                    if let Some(parent_crop) = *parent_crop {
                        offset = (offset + intersection.loc) - parent_crop;
                    }

                    surface_offset += offset;

                    if states.role == Some("subsurface") || surface_type.contains(WindowSurfaceType::TOPLEVEL)
                    {
                        let rect = Rectangle::from_loc_and_size(surface_offset, intersection.size).to_f64();
                        // Test if the point is within our cropped surface rectangle
                        if rect.contains(surface_tree_local_point) {
                            // Move the point local to the surface and
                            // add the surface crop so that the point is
                            // correctly offset for the input region test
                            let surface_local_point = (surface_tree_local_point - surface_offset.to_f64())
                                + intersection.loc.to_f64();
                            let data = states.data_map.get::<RefCell<SurfaceState>>();
                            let contains_the_point = data
                                .map(|data| {
                                    data.borrow()
                                        .contains_point(&*states.cached_state.current(), surface_local_point)
                                })
                                .unwrap_or(false);
                            if contains_the_point {
                                *found.borrow_mut() = Some((
                                    surface.clone(),
                                    location + surface_offset.to_f64().upscale(scale).to_i32_round(),
                                ));
                            }
                        }
                    }

                    if surface_type.contains(WindowSurfaceType::SUBSURFACE) {
                        TraversalAction::DoChildren((surface_offset, Some(intersection.loc)))
                    } else {
                        // We are not interested in subsurfaces
                        TraversalAction::SkipChildren
                    }
                } else {
                    // We are completely cropped, so are our children
                    TraversalAction::SkipChildren
                }
            } else {
                // We are not mapped, so are our children
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

#[allow(clippy::too_many_arguments)]
pub(crate) fn output_update(
    output: &Output,
    output_geometry: Rectangle<i32, Logical>,
    surface_list: &mut Vec<wl_surface::WlSurface>,
    surface: &wl_surface::WlSurface,
    location: Point<i32, Logical>,
    src: Option<Rectangle<i32, Logical>>,
    scale: impl Into<Scale<f64>>,
    logger: &slog::Logger,
) {
    let scale = scale.into();

    with_surface_tree_upward(
        surface,
        (Point::from((0, 0)), None, false),
        |_surface, states, (surface_offset, parent_crop, parent_unmapped)| {
            let mut surface_offset = *surface_offset;
            let data = states.data_map.get::<RefCell<SurfaceState>>();

            if *parent_unmapped {
                TraversalAction::DoChildren((surface_offset, None, true))
            } else if let Some(surface_view) = data.and_then(|d| d.borrow().surface_view) {
                // We use the surface view dst size here as this is the size
                // the surface would be rendered on screen without applying the
                // crop but it already includes wp_viewporter
                let surface_rect = Rectangle::from_loc_and_size((0, 0), surface_view.dst);

                let src = src
                    .map(|mut src| {
                        // Move the src rect relative to the surface
                        src.loc -= surface_offset + surface_view.offset;
                        src
                    })
                    .unwrap_or(surface_rect);

                if let Some(intersection) = surface_rect.intersection(src) {
                    let mut offset = surface_view.offset;

                    // Correct the offset by the (parent)crop
                    if let Some(parent_crop) = *parent_crop {
                        offset = (offset + intersection.loc) - parent_crop;
                    }

                    surface_offset += offset;

                    TraversalAction::DoChildren((surface_offset, Some(intersection.loc), false))
                } else {
                    TraversalAction::DoChildren((surface_offset, None, true))
                }
            } else {
                TraversalAction::DoChildren((surface_offset, None, true))
            }
        },
        |wl_surface, states, (surface_offset, parent_crop, parent_unmapped)| {
            if *parent_unmapped {
                // Surface does not match output, if we sent enter earlier
                // we should now send leave
                output_leave(output, surface_list, wl_surface, logger);
                return;
            }

            let mut surface_offset = *surface_offset;
            let data = states.data_map.get::<RefCell<SurfaceState>>();

            if let Some(surface_view) = data.and_then(|d| d.borrow().surface_view) {
                // We use the surface view dst size here as this is the size
                // the surface would be rendered on screen without applying the
                // crop but it already includes wp_viewporter
                let surface_rect = Rectangle::from_loc_and_size((0, 0), surface_view.dst);

                let src = src
                    .map(|mut src| {
                        // Move the src rect relative to the surface
                        src.loc -= surface_offset + surface_view.offset;
                        src
                    })
                    .unwrap_or(surface_rect);

                let intersection = surface_rect.intersection(src).unwrap_or_default();
                let mut offset = surface_view.offset;

                // Correct the offset by the (parent)crop
                if let Some(parent_crop) = *parent_crop {
                    offset = (offset + intersection.loc) - parent_crop;
                }

                surface_offset += offset;

                let pos = (location + surface_offset).to_f64().upscale(scale).to_i32_round();
                let size = intersection.size.to_f64().upscale(scale).to_i32_round();

                let rect = Rectangle::from_loc_and_size(pos, size);

                if output_geometry.overlaps(rect) {
                    // We found a matching output, check if we already sent enter
                    output_enter(output, surface_list, wl_surface, logger);
                } else {
                    // Surface does not match output, if we sent enter earlier
                    // we should now send leave
                    output_leave(output, surface_list, wl_surface, logger);
                }
            } else {
                // Surface does not match output, if we sent enter earlier
                // we should now send leave
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
