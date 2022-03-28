mod grab;
mod manager;

use std::sync::Mutex;

pub use grab::*;
pub use manager::*;
use wayland_server::protocol::wl_surface::WlSurface;

use crate::{
    utils::{Logical, Point, Rectangle},
    wayland::{
        compositor::with_states,
        shell::xdg::{PopupSurface, SurfaceCachedState, XdgPopupSurfaceRoleAttributes},
    },
};

use super::utils::surface_transform;

/// Represents a popup surface
#[derive(Debug, Clone)]
pub enum PopupKind {
    /// xdg-shell [`PopupSurface`]
    Xdg(PopupSurface),
}

impl PopupKind {
    fn alive(&self) -> bool {
        match *self {
            PopupKind::Xdg(ref t) => t.alive(),
        }
    }

    /// Retrieves the underlying [`WlSurface`]
    pub fn get_surface(&self) -> Option<&WlSurface> {
        match *self {
            PopupKind::Xdg(ref t) => t.get_surface(),
        }
    }

    fn parent(&self) -> Option<WlSurface> {
        match *self {
            PopupKind::Xdg(ref t) => t.get_parent_surface(),
        }
    }

    /// Returns the surface geometry as set by the client using `xdg_surface::set_window_geometry`
    pub fn geometry(&self) -> Rectangle<i32, Logical> {
        let wl_surface = match self.get_surface() {
            Some(s) => s,
            None => return Rectangle::from_loc_and_size((0, 0), (0, 0)),
        };

        let parent_surface_transform = self
            .parent()
            .map(|p| with_states(&p, surface_transform).unwrap_or_default())
            .unwrap_or_default();

        let (mut geometry, surface_transform) = with_states(wl_surface, |states| {
            let geometry = states
                .cached_state
                .current::<SurfaceCachedState>()
                .geometry
                .unwrap_or_default();

            let surface_transform = surface_transform(states);

            (geometry, surface_transform)
        })
        .unwrap();

        geometry.loc = parent_surface_transform.apply_point(geometry.loc).to_i32_floor();
        geometry.size = surface_transform.apply_size(geometry.size).to_i32_ceil();

        geometry
    }

    fn send_done(&self) {
        if !self.alive() {
            return;
        }

        match *self {
            PopupKind::Xdg(ref t) => t.send_popup_done(),
        }
    }

    fn location(&self) -> Point<i32, Logical> {
        let wl_surface = match self.get_surface() {
            Some(s) => s,
            None => return (0, 0).into(),
        };
        let loc = with_states(wl_surface, |states| {
            states
                .data_map
                .get::<Mutex<XdgPopupSurfaceRoleAttributes>>()
                .unwrap()
                .lock()
                .unwrap()
                .current
                .geometry
        })
        .unwrap_or_default()
        .loc;

        let parent_surface_transform = self
            .parent()
            .map(|p| with_states(&p, surface_transform).unwrap_or_default())
            .unwrap_or_default();

        parent_surface_transform.apply_point(loc).to_i32_floor()
    }
}

impl From<PopupSurface> for PopupKind {
    fn from(p: PopupSurface) -> PopupKind {
        PopupKind::Xdg(p)
    }
}
