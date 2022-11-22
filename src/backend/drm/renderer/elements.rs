use crate::{
    backend::renderer::{
        element::{Element, Id, RenderElement},
        utils::CommitCounter,
        Frame, Renderer,
    },
    render_elements,
    utils::{Buffer, Physical, Point, Rectangle, Scale, Transform},
};

pub const COLOR_TRANSPARENT: [f32; 4] = [0f32, 0f32, 0f32, 0f32];

render_elements! {
    pub DrmRenderElements<'a, R, E>;
    Holepunch=HolepunchRenderElement,
    Other=&'a E,
}

pub struct HolepunchRenderElement {
    id: Id,
    geometry: Rectangle<i32, Physical>,
}

impl HolepunchRenderElement {
    pub fn from_render_element<R, E>(element: &E, scale: impl Into<Scale<f64>>) -> Self
    where
        R: Renderer,
        E: RenderElement<R>,
    {
        HolepunchRenderElement {
            id: element.id().clone(),
            geometry: element.geometry(scale.into()),
        }
    }
}

impl<R> RenderElement<R> for HolepunchRenderElement
where
    R: Renderer,
{
    fn draw<'frame>(
        &self,
        frame: &mut <R as Renderer>::Frame<'frame>,
        _src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        _log: &slog::Logger,
    ) -> Result<(), <R as Renderer>::Error> {
        frame.clear(
            COLOR_TRANSPARENT,
            &damage
                .iter()
                .cloned()
                .map(|mut d| {
                    d.loc += dst.loc;
                    d
                })
                .collect::<Vec<_>>(),
        )
    }
}

impl Element for HolepunchRenderElement {
    fn id(&self) -> &Id {
        &self.id
    }

    fn current_commit(&self) -> CommitCounter {
        CommitCounter::default()
    }

    fn src(&self) -> Rectangle<f64, Buffer> {
        Rectangle::default()
    }

    fn geometry(&self, _scale: Scale<f64>) -> Rectangle<i32, Physical> {
        self.geometry
    }

    fn transform(&self) -> Transform {
        Transform::Normal
    }

    fn opaque_regions(&self, _scale: Scale<f64>) -> Vec<Rectangle<i32, Physical>> {
        vec![Rectangle::from_loc_and_size(Point::default(), self.geometry.size)]
    }
}
