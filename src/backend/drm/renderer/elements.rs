use crate::{
    backend::renderer::{
        element::{Element, Id, RenderElement, UnderlyingStorage},
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

pub struct RelocatingRenderElement<'a, E> {
    location: Point<i32, Physical>,
    element: &'a E,
}

impl<'a, E> RelocatingRenderElement<'a, E> {
    pub fn from_element(location: impl Into<Point<i32, Physical>>, element: &'a E) -> Self {
        RelocatingRenderElement {
            location: location.into(),
            element,
        }
    }
}

impl<'a, R, E> RenderElement<R> for RelocatingRenderElement<'a, E>
where
    R: Renderer,
    E: Element + RenderElement<R>,
{
    fn draw<'frame>(
        &self,
        frame: &mut <R as Renderer>::Frame<'frame>,
        src: Rectangle<f64, Buffer>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        log: &slog::Logger,
    ) -> Result<(), <R as Renderer>::Error> {
        self.element.draw(frame, src, dst, damage, log)
    }

    fn underlying_storage(&self, renderer: &mut R) -> Option<UnderlyingStorage> {
        self.element.underlying_storage(renderer)
    }
}

impl<'a, E> Element for RelocatingRenderElement<'a, E>
where
    E: Element,
{
    fn id(&self) -> &Id {
        self.element.id()
    }

    fn current_commit(&self) -> CommitCounter {
        self.element.current_commit()
    }

    fn src(&self) -> Rectangle<f64, Buffer> {
        self.element.src()
    }

    fn location(&self, _scale: Scale<f64>) -> Point<i32, Physical> {
        self.location
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        Rectangle::from_loc_and_size(self.location, self.element.geometry(scale).size)
    }

    fn transform(&self) -> Transform {
        self.element.transform()
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> Vec<Rectangle<i32, Physical>> {
        self.element.damage_since(scale, commit)
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> Vec<Rectangle<i32, Physical>> {
        self.element.opaque_regions(scale)
    }
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
