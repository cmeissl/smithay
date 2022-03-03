use crate::{
    backend::renderer::{ImportAll, Renderer},
    desktop::space::{RenderElement, SpaceElement},
    utils::{Logical, Point, Rectangle},
    wayland::output::Output,
};
use indexmap::IndexMap;
use wayland_server::protocol::wl_surface::WlSurface;

use std::{
    any::TypeId,
    cell::{RefCell, RefMut},
    collections::{HashMap, VecDeque},
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ToplevelId {
    t_id: TypeId,
    id: usize,
}

impl<'a, R, E> From<&SpaceElement<'a, R, E>> for ToplevelId
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
    E: RenderElement<R>,
{
    fn from(elem: &SpaceElement<'a, R, E>) -> ToplevelId {
        ToplevelId {
            t_id: elem.type_of(),
            id: elem.id(),
        }
    }
}

#[derive(Clone, Default)]
pub struct OutputState {
    pub location: Point<i32, Logical>,
    pub render_scale: f64,

    // damage and last_state are in space coordinate space
    pub old_damage: VecDeque<Vec<Rectangle<i32, Logical>>>,
    pub last_state: IndexMap<ToplevelId, Rectangle<i32, Logical>>,

    // surfaces for tracking enter and leave events
    pub surfaces: Vec<WlSurface>,
}

pub type OutputUserdata = RefCell<HashMap<usize, OutputState>>;
pub fn output_state(space: usize, o: &Output) -> RefMut<'_, OutputState> {
    let userdata = o.user_data();
    userdata.insert_if_missing(OutputUserdata::default);
    RefMut::map(userdata.get::<OutputUserdata>().unwrap().borrow_mut(), |m| {
        m.entry(space).or_default()
    })
}
