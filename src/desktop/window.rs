use crate::{
    backend::renderer::{
        utils::{draw_surface_tree, with_surface_transform, SurfaceTransform},
        ImportAll, Renderer,
    },
    desktop::{utils::*, PopupManager, Space},
    utils::{Coordinate, Logical, Point, Rectangle, Size},
    wayland::{
        compositor::with_states,
        output::Output,
        shell::xdg::{SurfaceCachedState, ToplevelSurface},
    },
};
use std::{
    cell::{Cell, RefCell},
    hash::{Hash, Hasher},
    rc::Rc,
};
use wayland_commons::user_data::UserDataMap;
use wayland_protocols::xdg_shell::server::xdg_toplevel;
use wayland_server::protocol::wl_surface;

crate::utils::ids::id_gen!(next_window_id, WINDOW_ID, WINDOW_IDS);

/// Abstraction around different toplevel kinds
#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    /// xdg-shell [`ToplevelSurface`]
    Xdg(ToplevelSurface),
    /// XWayland surface (TODO)
    #[cfg(feature = "xwayland")]
    X11(X11Surface),
}

/// Xwayland surface
#[derive(Debug, Clone)]
#[cfg(feature = "xwayland")]
pub struct X11Surface {
    /// underlying wl_surface
    pub surface: wl_surface::WlSurface,
}

#[cfg(feature = "xwayland")]
impl std::cmp::PartialEq for X11Surface {
    fn eq(&self, other: &Self) -> bool {
        self.alive() && other.alive() && self.surface == other.surface
    }
}

#[cfg(feature = "xwayland")]
impl X11Surface {
    /// Checks if the surface is still alive.
    pub fn alive(&self) -> bool {
        self.surface.as_ref().is_alive()
    }

    /// Returns the underlying [`WlSurface`](wl_surface::WlSurface), if still any.
    pub fn get_surface(&self) -> Option<&wl_surface::WlSurface> {
        if self.alive() {
            Some(&self.surface)
        } else {
            None
        }
    }
}

impl Kind {
    /// Checks if the surface is still alive.
    pub fn alive(&self) -> bool {
        match *self {
            Kind::Xdg(ref t) => t.alive(),
            #[cfg(feature = "xwayland")]
            Kind::X11(ref t) => t.alive(),
        }
    }

    /// Returns the underlying [`WlSurface`](wl_surface::WlSurface), if still any.
    pub fn get_surface(&self) -> Option<&wl_surface::WlSurface> {
        match *self {
            Kind::Xdg(ref t) => t.get_surface(),
            #[cfg(feature = "xwayland")]
            Kind::X11(ref t) => t.get_surface(),
        }
    }
}

pub(super) struct WindowInner {
    pub(super) id: usize,
    toplevel: Kind,
    bbox: Cell<Rectangle<i32, Logical>>,
    pub(super) z_index: Cell<Option<u8>>,
    user_data: UserDataMap,
    constrain: RefCell<Option<(Size<i32, Logical>, Box<dyn ConstrainBehavior>)>>,
}

impl std::fmt::Debug for WindowInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WindowInner")
            .field("id", &self.id)
            .field("toplevel", &self.toplevel)
            .field("bbox", &self.bbox)
            .field("z_index", &self.z_index)
            .field("user_data", &self.user_data)
            //.field("constrain", &self.constrain)
            .finish()
    }
}

impl Drop for WindowInner {
    fn drop(&mut self) {
        WINDOW_IDS.lock().unwrap().remove(&self.id);
    }
}

/// Represents a single application window
#[derive(Debug, Clone)]
pub struct Window(pub(super) Rc<WindowInner>);

impl PartialEq for Window {
    fn eq(&self, other: &Self) -> bool {
        self.0.id == other.0.id
    }
}

impl Eq for Window {}

impl Hash for Window {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.id.hash(state);
    }
}

bitflags::bitflags! {
    /// Defines the surface types that can be
    /// queried with [`Window::surface_under`]
    pub struct WindowSurfaceType: u32 {
        /// Include the toplevel surface
        const TOPLEVEL = 1;
        /// Include all subsurfaces
        const SUBSURFACE = 2;
        /// Include all popup surfaces
        const POPUP = 4;
        /// Query all surfaces
        const ALL = Self::TOPLEVEL.bits | Self::SUBSURFACE.bits | Self::POPUP.bits;
    }
}

impl Window {
    /// Construct a new [`Window`] from a given compatible toplevel surface
    pub fn new(toplevel: Kind) -> Window {
        let id = next_window_id();

        Window(Rc::new(WindowInner {
            id,
            toplevel,
            bbox: Cell::new(Rectangle::from_loc_and_size((0, 0), (0, 0))),
            user_data: UserDataMap::new(),
            z_index: Cell::new(None),
            //size: Cell::new(None),
            constrain: RefCell::new(None),
        }))
    }

    /// Returns the geometry of this window.
    pub fn geometry(&self) -> Rectangle<i32, Logical> {
        let surface = match self.0.toplevel.get_surface() {
            Some(surface) => surface,
            None => return Rectangle::from_loc_and_size((0, 0), (0, 0)),
        };

        let geometry = self.real_geometry();
        with_surface_transform(surface, |transform| {
            let mut geo = geometry.to_f64();

            if let Some(src) = transform.src {
                geo = geo.intersection(src).unwrap_or_default();
                let pos_rect = Rectangle::from_extemities(
                    (0.0, 0.0),
                    (f64::max(geo.loc.x, 0.0), f64::max(geo.loc.y, 0.0)),
                );
                geo.loc -= src.loc.constrain(pos_rect);
            }

            if let Some(scale) = transform.scale {
                geo.loc.x = geo.loc.x.upscale(scale.w);
                geo.loc.y = geo.loc.y.upscale(scale.h);
                geo.size.w = geo.size.w.upscale(scale.w);
                geo.size.h = geo.size.h.upscale(scale.h);
            }

            geo.loc += transform.offset.unwrap_or_default();

            geo.to_i32_up()
        })
        .unwrap()
    }

    /// Returns a bounding box over this window and its children.
    pub fn bbox(&self) -> Rectangle<i32, Logical> {
        let surface = match self.0.toplevel.get_surface() {
            Some(surface) => surface,
            None => return Rectangle::default(),
        };

        let bbox = self.0.bbox.get();
        with_surface_transform(surface, |transform| {
            let mut bbox = bbox.to_f64();

            if let Some(src) = transform.src {
                bbox = bbox.intersection(src).unwrap_or_default();
                bbox.loc = Point::from((f64::min(bbox.loc.x, 0.0), f64::min(bbox.loc.y, 0.0)));
            }

            if let Some(scale) = transform.scale {
                bbox.loc.x = bbox.loc.x.upscale(scale.w);
                bbox.loc.y = bbox.loc.y.upscale(scale.h);
                bbox.size.w = bbox.size.w.upscale(scale.w);
                bbox.size.h = bbox.size.h.upscale(scale.h);
            }

            bbox.loc += transform.offset.unwrap_or_default();

            bbox.to_i32_up()
        })
        .unwrap()
    }

    /// Returns a bounding box over this window and children including popups.
    ///
    /// Note: You need to use a [`PopupManager`] to track popups, otherwise the bounding box
    /// will not include the popups.
    pub fn bbox_with_popups(&self) -> Rectangle<i32, Logical> {
        let mut bounding_box = self.bbox();
        if let Some(surface) = self.0.toplevel.get_surface() {
            for (popup, location) in PopupManager::popups_for_surface(surface)
                .ok()
                .into_iter()
                .flatten()
            {
                if let Some(surface) = popup.get_surface() {
                    let offset = self.geometry().loc + location - popup.geometry().loc;
                    bounding_box = bounding_box.merge(bbox_from_surface_tree(surface, offset));
                }
            }
        }
        bounding_box
    }

    /// Activate/Deactivate this window
    pub fn set_activated(&self, active: bool) -> bool {
        match self.0.toplevel {
            Kind::Xdg(ref t) => t
                .with_pending_state(|state| {
                    if active {
                        state.states.set(xdg_toplevel::State::Activated)
                    } else {
                        state.states.unset(xdg_toplevel::State::Activated)
                    }
                })
                .unwrap_or(false),
            #[cfg(feature = "xwayland")]
            Kind::X11(ref _t) => false,
        }
    }

    /// Commit any changes to this window
    pub fn configure(&self) {
        match self.0.toplevel {
            Kind::Xdg(ref t) => t.send_configure(),
            #[cfg(feature = "xwayland")]
            Kind::X11(ref _t) => unimplemented!(),
        }
    }

    /// Sends the frame callback to all the subsurfaces in this
    /// window that requested it
    pub fn send_frame(&self, time: u32) {
        let surface = match self.0.toplevel.get_surface() {
            Some(surface) => surface,
            None => return,
        };

        send_frames_surface_tree(surface, time);
        for (popup, _) in PopupManager::popups_for_surface(surface)
            .ok()
            .into_iter()
            .flatten()
        {
            if let Some(surface) = popup.get_surface() {
                send_frames_surface_tree(surface, time);
            }
        }
    }

    /// Updates internal values
    ///
    /// Needs to be called whenever the toplevel surface or any unsynchronized subsurfaces of this window are updated
    /// to correctly update the bounding box of this window.
    pub fn refresh(&self) {
        let surface = match self.0.toplevel.get_surface() {
            Some(surface) => surface,
            None => return,
        };

        let bbox = bbox_from_surface_tree(surface, (0, 0));
        self.0.bbox.set(bbox);

        if bbox.size.w <= 0 || bbox.size.h <= 0 {
            return;
        }

        let transform = if let Some((size, behavior)) = self.0.constrain.borrow().as_ref() {
            let rect = (*behavior).constrain(self, bbox.size, *size);
            let constrain_rect = Rectangle::from_loc_and_size((0.0, 0.0), size.to_f64());
            if let Some(intersection) = rect.intersection(constrain_rect) {
                // Calculate the scale between the real window size
                // and the size the constraint wants the window to have
                let bbox_scale: Size<f64, Logical> =
                    Size::from((bbox.size.w as f64 / rect.size.w, bbox.size.h as f64 / rect.size.h));

                // Calculate how much the constraint wants to crop from
                // the size it selected
                let mut left_top_crop: Point<f64, Logical> =
                    Point::from((-f64::min(rect.loc.x, 0.0), -f64::min(rect.loc.y, 0.0)));

                // Scale the crop to bbox size
                left_top_crop.x = left_top_crop.x.upscale(bbox_scale.w);
                left_top_crop.y = left_top_crop.y.upscale(bbox_scale.h);

                let mut size = intersection.size;
                size.w = size.w.upscale(bbox_scale.w);
                size.h = size.h.upscale(bbox_scale.h);

                let src: Rectangle<f64, Logical> =
                    Rectangle::from_loc_and_size(bbox.loc.to_f64() + left_top_crop, size);

                let scale = Size::from((intersection.size.w / src.size.w, intersection.size.h / src.size.h));

                // Calculate the offset
                let offset = intersection.loc;

                SurfaceTransform {
                    src: Some(src),
                    scale: Some(scale),
                    offset: Some(offset),
                }
            } else {
                SurfaceTransform::default()
            }
        } else {
            SurfaceTransform::default()
        };

        with_surface_transform(surface, |t| {
            *t = transform;
        })
        .unwrap();
    }

    /// Finds the topmost surface under this point matching the input regions of the surface and returns
    /// it together with the location of this surface.
    ///
    /// In case no surface input region matches the point [`None`] is returned.
    ///
    /// - `point` should be relative to (0,0) of the window.
    pub fn surface_under<P: Into<Point<f64, Logical>>>(
        &self,
        point: P,
        surface_type: WindowSurfaceType,
    ) -> Option<(wl_surface::WlSurface, Point<i32, Logical>)> {
        let point = point.into();
        if let Some(surface) = self.0.toplevel.get_surface() {
            if surface_type.contains(WindowSurfaceType::POPUP) {
                for (popup, location) in PopupManager::popups_for_surface(surface)
                    .ok()
                    .into_iter()
                    .flatten()
                {
                    let offset = self.geometry().loc + location - popup.geometry().loc;
                    if let Some(result) = popup
                        .get_surface()
                        .and_then(|surface| under_from_surface_tree(surface, point, offset, surface_type))
                    {
                        return Some(result);
                    }
                }
            }

            under_from_surface_tree(surface, point, (0, 0), surface_type)
        } else {
            None
        }
    }

    /// Damage of all the surfaces of this window.
    ///
    /// If `for_values` is `Some(_)` it will only return the damage on the
    /// first call for a given [`Space`] and [`Output`], if the buffer hasn't changed.
    /// Subsequent calls will return an empty vector until the buffer is updated again.
    pub fn accumulated_damage(&self, for_values: Option<(&Space, &Output)>) -> Vec<Rectangle<i32, Logical>> {
        let mut damage = Vec::new();
        if let Some(surface) = self.0.toplevel.get_surface() {
            damage.extend(
                damage_from_surface_tree(surface, (0, 0), for_values)
                    .into_iter()
                    .flat_map(|rect| rect.intersection(self.bbox())),
            );
        }
        damage
    }

    /// Returns the underlying toplevel
    pub fn toplevel(&self) -> &Kind {
        &self.0.toplevel
    }

    /// Returns a [`UserDataMap`] to allow associating arbitrary data with this window.
    pub fn user_data(&self) -> &UserDataMap {
        &self.0.user_data
    }

    /// Overrides the default z-index of this window.
    /// (Default is [`RenderZindex::Shell`](crate::desktop::space::RenderZindex))
    pub fn override_z_index(&self, index: u8) {
        self.0.z_index.set(Some(index));
    }

    /// Resets a previously overriden z-index to the default of
    /// [`RenderZindex::Shell`](crate::desktop::space::RenderZindex).
    pub fn clear_z_index(&self) {
        self.0.z_index.take();
    }

    /// Allows to define a constrain and [`ConstrainBehavior`] that
    /// will be applied to the window.
    pub fn set_constrain<S, B>(&self, size: S, behavior: B)
    where
        S: Into<Size<i32, Logical>>,
        B: ConstrainBehavior + 'static,
    {
        self.0
            .constrain
            .borrow_mut()
            .replace((size.into(), Box::new(behavior)));
        self.refresh();
    }

    /// Clears the current constrain
    pub fn clear_constrain(&self) {
        self.0.constrain.take();
        self.refresh();
    }

    /// Returns the real geometry of this window without
    /// applying the crop and scale
    fn real_geometry(&self) -> Rectangle<i32, Logical> {
        let surface = match self.0.toplevel.get_surface() {
            Some(surface) => surface,
            None => return Rectangle::default(),
        };

        let bbox = self.0.bbox.get();
        let geometry = with_states(surface, |states| {
            states.cached_state.current::<SurfaceCachedState>().geometry
        })
        .unwrap_or_default();

        if let Some(geometry) = geometry {
            // When applied, the effective window geometry will be the set window geometry clamped to the
            // bounding rectangle of the combined geometry of the surface of the xdg_surface and the associated subsurfaces.
            geometry.intersection(bbox).unwrap_or_default()
        } else {
            // If never set, the value is the full bounds of the surface, including any subsurfaces.
            // This updates dynamically on every commit. This unset is meant for extremely simple clients.
            bbox
        }
    }
}

/// Defines a behavior for constraining a window
/// within a defined size
pub trait ConstrainBehavior {
    /// TODO: Docs
    fn constrain(
        &self,
        window: &Window,
        window_size: Size<i32, Logical>,
        constrain_size: Size<i32, Logical>,
    ) -> Rectangle<f64, Logical>;
}

/// TODO: Docs
pub fn default_constrain(
    _window: &Window,
    window_size: Size<i32, Logical>,
    constrain_size: Size<i32, Logical>,
) -> Rectangle<f64, Logical> {
    let constrain_size = constrain_size.to_f64();
    let window_size = window_size.to_f64();

    // First we calculate the size we want the window to have
    let size = if window_size <= constrain_size {
        // No need to scale, we will just center the window
        constrain_size
    } else {
        // Scale the window into the size while keeping the
        // aspect ratio and center it
        let width_scale = constrain_size.w / window_size.w;
        let height_scale = constrain_size.h / window_size.h;

        let scale_for_fit = f64::min(width_scale, height_scale);

        Size::from((window_size.w * scale_for_fit, window_size.h * scale_for_fit))
    };

    // Then we center the window within the defined size
    let left = (constrain_size.w - size.w) / 2.0;
    let top = (constrain_size.h - size.h) / 2.0;

    Rectangle::from_loc_and_size((left, top), size)
}

/// TODO: Docs
pub fn fit_constrain(
    _window: &Window,
    window_size: Size<i32, Logical>,
    constrain_size: Size<i32, Logical>,
) -> Rectangle<f64, Logical> {
    let constrain_size = constrain_size.to_f64();
    let window_size = window_size.to_f64();

    // Scale the window into the size while keeping the
    // aspect ratio and center it
    let width_scale = constrain_size.w / window_size.w;
    let height_scale = constrain_size.h / window_size.h;

    let scale_for_fit = f64::min(width_scale, height_scale);

    let size = Size::from((window_size.w * scale_for_fit, window_size.h * scale_for_fit));

    // Then we center the window within the defined size
    let left = (constrain_size.w - size.w) / 2.0;
    let top = (constrain_size.h - size.h) / 2.0;

    Rectangle::from_loc_and_size((left, top), size)
}

/// TODO: Docs
pub fn stretch_constrain(
    _window: &Window,
    _window_size: Size<i32, Logical>,
    constrain_size: Size<i32, Logical>,
) -> Rectangle<f64, Logical> {
    Rectangle::from_loc_and_size((0.0, 0.0), constrain_size.to_f64())
}

/// TODO: Docs
pub fn zoom_constrain(
    _window: &Window,
    window_size: Size<i32, Logical>,
    constrain_size: Size<i32, Logical>,
) -> Rectangle<f64, Logical> {
    let constrain_size = constrain_size.to_f64();
    let window_size = window_size.to_f64();

    // Scale the window into the size while keeping the
    // aspect ratio and center it
    let width_scale = constrain_size.w / window_size.w;
    let height_scale = constrain_size.h / window_size.h;

    let scale_for_fit = f64::max(width_scale, height_scale);

    let size = Size::from((window_size.w * scale_for_fit, window_size.h * scale_for_fit));

    // Then we center the window within the defined size
    let left = (constrain_size.w - size.w) / 2.0;
    let top = (constrain_size.h - size.h) / 2.0;

    Rectangle::from_loc_and_size((left, top), size)
}

impl<T> ConstrainBehavior for T
where
    T: Fn(&Window, Size<i32, Logical>, Size<i32, Logical>) -> Rectangle<f64, Logical>,
{
    fn constrain(
        &self,
        window: &Window,
        window_size: Size<i32, Logical>,
        constrain_size: Size<i32, Logical>,
    ) -> Rectangle<f64, Logical> {
        self(window, window_size, constrain_size)
    }
}

/// Renders a given [`Window`] using a provided renderer and frame.
///
/// - `scale` needs to be equivalent to the fractional scale the rendered result should have.
/// - `location` is the position the window should be drawn at.
/// - `damage` is the set of regions of the window that should be drawn.
///
/// Note: This function will render nothing, if you are not using
/// [`crate::backend::renderer::utils::on_commit_buffer_handler`]
/// to let smithay handle buffer management.
pub fn draw_window<R, P>(
    renderer: &mut R,
    frame: &mut <R as Renderer>::Frame,
    window: &Window,
    scale: f64,
    location: P,
    damage: &[Rectangle<i32, Logical>],
    log: &slog::Logger,
) -> Result<(), <R as Renderer>::Error>
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
    P: Into<Point<i32, Logical>>,
{
    let location = location.into();
    if let Some(surface) = window.toplevel().get_surface() {
        draw_surface_tree(renderer, frame, surface, scale, location, damage, log)?;
        for (popup, p_location) in PopupManager::popups_for_surface(surface)
            .ok()
            .into_iter()
            .flatten()
        {
            if let Some(surface) = popup.get_surface() {
                let offset = window.geometry().loc + p_location - popup.geometry().loc;
                let damage = damage
                    .iter()
                    .cloned()
                    .map(|mut geo| {
                        geo.loc -= offset;
                        geo
                    })
                    .collect::<Vec<_>>();
                draw_surface_tree(renderer, frame, surface, scale, location + offset, &damage, log)?;
            }
        }
    }
    Ok(())
}
