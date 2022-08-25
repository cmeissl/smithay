use super::SpaceElement;
use crate::{
    backend::renderer::{
        element::{
            surface::{render_elements_from_surface_tree, WaylandSurfaceRenderElement},
            AsRenderElements,
        },
        ImportAll, Renderer,
    },
    desktop::{window::Window, PopupManager, WindowSurfaceType},
    utils::{Logical, Physical, Point, Rectangle, Scale},
    wayland::{
        compositor::{with_surface_tree_downward, TraversalAction},
        output::Output,
    },
};

impl SpaceElement for Window {
    fn geometry(&self) -> Rectangle<i32, Logical> {
        self.geometry()
    }

    fn bbox(&self) -> Rectangle<i32, Logical> {
        self.bbox_with_popups()
    }

    fn input_region(&self, point: &Point<f64, Logical>) -> Option<Point<i32, Logical>> {
        self.surface_under(*point, WindowSurfaceType::ALL)
            .map(|(_s, point)| point)
    }

    fn z_index(&self) -> u8 {
        self.0.z_index.load(std::sync::atomic::Ordering::SeqCst)
    }

    fn set_activate(&self, activated: bool) {
        self.set_activated(activated);
    }
    fn output_enter(&self, output: &Output) {
        self.0.outputs.lock().unwrap().push(output.clone());
        self.update_outputs(None)
    }
    fn output_leave(&self, output: &Output) {
        self.0.outputs.lock().unwrap().retain(|o| o != output);
        self.update_outputs(Some(output))
    }
    fn refresh(&self) {
        self.update_outputs(None)
    }
}

impl<R> AsRenderElements<R> for Window
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
{
    type RenderElement = WaylandSurfaceRenderElement;

    fn render_elements<C: From<WaylandSurfaceRenderElement>>(
        &self,
        location: Point<i32, Physical>,
        scale: Scale<f64>,
    ) -> Vec<C> {
        let surface = self.toplevel().wl_surface();

        let mut render_elements: Vec<C> = Vec::new();
        let popup_render_elements =
            PopupManager::popups_for_surface(surface).flat_map(|(popup, popup_offset)| {
                let offset = (self.geometry().loc + popup_offset - popup.geometry().loc)
                    .to_f64()
                    .to_physical(scale)
                    .to_i32_round();

                render_elements_from_surface_tree(popup.wl_surface(), location + offset, scale)
            });

        render_elements.extend(popup_render_elements);

        render_elements.extend(render_elements_from_surface_tree(surface, location, scale));

        render_elements
    }
}
