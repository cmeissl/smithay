use crate::{
    backend::renderer::Renderer,
    desktop::{
        layer::{layer_state, LayerSurface},
        popup::{PopupKind, PopupManager},
        space::Space,
        utils::{damage_from_surface_tree, output_geometry_from_surface_tree},
        window::Window,
    },
    utils::{Logical, Physical, Point, Rectangle, Scale},
    wayland::{output::Output, shell::wlr_layer::Layer},
};
use std::any::TypeId;

use super::{window::window_loc, RenderZindex};

#[derive(Debug)]
pub struct RenderPopup {
    location: Point<i32, Logical>,
    popup: PopupKind,
    z_index: u8,
}

impl Window {
    pub(super) fn popup_elements(&self, space_id: usize) -> impl Iterator<Item = RenderPopup> {
        let loc = window_loc(self, &space_id) - self.geometry().loc + self.geometry().loc;
        self.toplevel()
            .get_surface()
            .map(move |surface| {
                PopupManager::popups_for_surface(surface)
                    .ok()
                    .into_iter()
                    .flatten()
                    .map(move |(popup, location)| {
                        let offset = loc + location - popup.geometry().loc;
                        RenderPopup {
                            location: offset,
                            popup,
                            z_index: RenderZindex::Popups as u8,
                        }
                    })
            })
            .into_iter()
            .flatten()
    }
}

impl LayerSurface {
    pub(super) fn popup_elements(&self, _space_id: usize) -> impl Iterator<Item = RenderPopup> + '_ {
        let loc = layer_state(self).location;
        self.get_surface()
            .map(move |surface| {
                PopupManager::popups_for_surface(surface)
                    .ok()
                    .into_iter()
                    .flatten()
                    .map(move |(popup, location)| {
                        let offset = loc + location - popup.geometry().loc;
                        let z_index = if let Some(layer) = self.layer() {
                            if layer == Layer::Overlay {
                                RenderZindex::PopupsOverlay as u8
                            } else {
                                RenderZindex::Popups as u8
                            }
                        } else {
                            0
                        };

                        RenderPopup {
                            location: offset,
                            popup,
                            z_index,
                        }
                    })
            })
            .into_iter()
            .flatten()
    }
}

impl RenderPopup {
    pub(super) fn elem_id(&self) -> usize {
        self.popup
            .get_surface()
            .map(|s| s.as_ref().id() as usize)
            .unwrap_or(0)
    }

    pub(super) fn elem_type_of(&self) -> TypeId {
        TypeId::of::<RenderPopup>()
    }

    pub(super) fn elem_geometry(
        &self,
        _space_id: usize,
        scale: impl Into<Scale<f64>>,
    ) -> Rectangle<i32, Physical> {
        let scale = scale.into();
        let surface = match self.popup.get_surface() {
            Some(surface) => surface,
            None => return Rectangle::default(),
        };
        output_geometry_from_surface_tree(
            surface,
            self.location.to_f64().to_physical(scale).to_i32_round(),
            scale,
        )
    }

    pub(super) fn elem_accumulated_damage(
        &self,
        scale: impl Into<Scale<f64>>,
        for_values: Option<(&Space, &Output)>,
    ) -> Vec<Rectangle<i32, Physical>> {
        if let Some(surface) = self.popup.get_surface() {
            damage_from_surface_tree(surface, (0, 0), scale, for_values)
        } else {
            Vec::new()
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn elem_draw<R, S>(
        &self,
        _space_id: usize,
        _renderer: &mut R,
        _frame: &mut <R as Renderer>::Frame,
        _scale: S,
        _location: Point<i32, Physical>,
        _damage: &[Rectangle<i32, Physical>],
        _log: &slog::Logger,
    ) -> Result<(), <R as Renderer>::Error>
    where
        R: Renderer,
        S: Into<Scale<f64>>,
    {
        // popups are special, we track them, but they render with their parents
        Ok(())
    }

    pub(super) fn elem_z_index(&self) -> u8 {
        self.z_index
    }
}
