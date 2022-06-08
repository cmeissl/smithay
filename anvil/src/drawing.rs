#![allow(clippy::too_many_arguments)]

use std::sync::Mutex;

use slog::Logger;
#[cfg(feature = "debug")]
use smithay::utils::Buffer;
use smithay::{
    backend::renderer::{Frame, ImportAll, Renderer, Texture},
    desktop::space::{RenderElement, SpaceOutputTuple, SurfaceTree},
    reexports::wayland_server::protocol::wl_surface,
    utils::{Logical, Physical, Point, Rectangle, Scale, Size, Transform},
    wayland::{
        compositor::{get_role, with_states},
        seat::CursorImageAttributes,
    },
};

pub static CLEAR_COLOR: [f32; 4] = [0.8, 0.8, 0.9, 1.0];

smithay::custom_elements! {
    pub CustomElem<R>;
    SurfaceTree=SurfaceTree,
    PointerElement=PointerElement::<<R as Renderer>::TextureId>,
    #[cfg(feature = "debug")]
    FpsElement=FpsElement::<<R as Renderer>::TextureId>,
}

pub fn draw_cursor(
    surface: wl_surface::WlSurface,
    location: impl Into<Point<i32, Logical>>,
    log: &Logger,
) -> SurfaceTree {
    let mut position = location.into();
    let ret = with_states(&surface, |states| {
        Some(
            states
                .data_map
                .get::<Mutex<CursorImageAttributes>>()
                .unwrap()
                .lock()
                .unwrap()
                .hotspot,
        )
    })
    .unwrap_or(None);
    position -= match ret {
        Some(h) => h,
        None => {
            warn!(
                log,
                "Trying to display as a cursor a surface that does not have the CursorImage role."
            );
            (0, 0).into()
        }
    };
    SurfaceTree {
        surface,
        position,
        z_index: 100, /* Cursor should always be on-top */
    }
}

pub fn draw_dnd_icon(
    surface: wl_surface::WlSurface,
    location: impl Into<Point<i32, Logical>>,
    log: &Logger,
) -> SurfaceTree {
    if get_role(&surface) != Some("dnd_icon") {
        warn!(
            log,
            "Trying to display as a dnd icon a surface that does not have the DndIcon role."
        );
    }
    SurfaceTree {
        surface,
        position: location.into(),
        z_index: 100, /* Cursor should always be on-top */
    }
}

pub struct PointerElement<T: Texture> {
    texture: T,
    position: Point<i32, Logical>,
    size: Size<i32, Logical>,
}

impl<T: Texture> PointerElement<T> {
    pub fn new(texture: T, pointer_pos: Point<i32, Logical>) -> PointerElement<T> {
        let size = texture.size().to_logical(1, Transform::Normal);
        PointerElement {
            texture,
            position: pointer_pos,
            size,
        }
    }
}

impl<R> RenderElement<R> for PointerElement<<R as Renderer>::TextureId>
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
{
    fn id(&self) -> usize {
        0
    }

    fn geometry(&self, scale: impl Into<Scale<f64>>) -> Rectangle<i32, Physical> {
        let scale = scale.into();
        Rectangle::from_loc_and_size(
            self.position.to_f64().to_physical(scale).to_i32_round(),
            self.size.to_f64().to_physical(scale).to_i32_round(),
        )
    }

    fn accumulated_damage(
        &self,
        scale: impl Into<Scale<f64>>,
        _: Option<SpaceOutputTuple<'_, '_>>,
    ) -> Vec<Rectangle<i32, Physical>> {
        vec![Rectangle::from_loc_and_size((0, 0), self.size)
            .to_f64()
            .to_physical(scale)
            .to_i32_up()]
    }

    fn draw(
        &self,
        _renderer: &mut R,
        frame: &mut <R as Renderer>::Frame,
        scale: impl Into<Scale<f64>>,
        location: Point<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        _log: &Logger,
    ) -> Result<(), <R as Renderer>::Error> {
        let scale = scale.into();
        frame.render_texture_at(
            &self.texture,
            location.to_f64(),
            1,
            scale,
            Transform::Normal,
            &*damage.iter().map(|rect| rect.to_f64()).collect::<Vec<_>>(),
            1.0,
        )?;
        Ok(())
    }
}

#[cfg(feature = "debug")]
pub static FPS_NUMBERS_PNG: &[u8] = include_bytes!("../resources/numbers.png");

#[cfg(feature = "debug")]
pub struct FpsElement<T: Texture> {
    value: u32,
    texture: T,
}

#[cfg(feature = "debug")]
impl<R> RenderElement<R> for FpsElement<<R as Renderer>::TextureId>
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: 'static,
{
    fn id(&self) -> usize {
        0
    }

    fn geometry(&self, scale: impl Into<Scale<f64>>) -> Rectangle<i32, Physical> {
        let digits = if self.value < 10 {
            1
        } else if self.value < 100 {
            2
        } else {
            3
        };
        Rectangle::from_loc_and_size((0, 0), (24 * digits, 35))
            .to_f64()
            .to_physical(scale)
            .to_i32_round()
    }

    fn accumulated_damage(
        &self,
        scale: impl Into<Scale<f64>>,
        _: Option<SpaceOutputTuple<'_, '_>>,
    ) -> Vec<Rectangle<i32, Physical>> {
        vec![Rectangle::from_loc_and_size((0, 0), (24 * 3, 35))
            .to_f64()
            .to_physical(scale)
            .to_i32_round()]
    }

    fn draw(
        &self,
        _renderer: &mut R,
        frame: &mut <R as Renderer>::Frame,
        scale: impl Into<Scale<f64>>,
        location: Point<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
        _log: &Logger,
    ) -> Result<(), <R as Renderer>::Error> {
        let value_str = std::cmp::min(self.value, 999).to_string();
        let scale = scale.into();
        let mut offset: Point<i32, Physical> = Point::from((0, 0));
        for digit in value_str.chars().map(|d| d.to_digit(10).unwrap()) {
            let damage = damage
                .iter()
                .cloned()
                .map(|mut rect| {
                    rect.loc -= offset;
                    rect
                })
                .flat_map(|x| {
                    x.intersection(Rectangle::from_loc_and_size(
                        (0, 0),
                        Size::from((22, 35)).to_f64().to_physical(scale).to_i32_round(),
                    ))
                })
                .map(|x| x.to_f64())
                .collect::<Vec<_>>();
            let src: Rectangle<i32, Buffer> = match digit {
                9 => Rectangle::from_loc_and_size((0, 0), (22, 35)),
                6 => Rectangle::from_loc_and_size((22, 0), (22, 35)),
                3 => Rectangle::from_loc_and_size((44, 0), (22, 35)),
                1 => Rectangle::from_loc_and_size((66, 0), (22, 35)),
                8 => Rectangle::from_loc_and_size((0, 35), (22, 35)),
                0 => Rectangle::from_loc_and_size((22, 35), (22, 35)),
                2 => Rectangle::from_loc_and_size((44, 35), (22, 35)),
                7 => Rectangle::from_loc_and_size((0, 70), (22, 35)),
                4 => Rectangle::from_loc_and_size((22, 70), (22, 35)),
                5 => Rectangle::from_loc_and_size((44, 70), (22, 35)),
                _ => unreachable!(),
            };
            let dst = Rectangle::from_loc_and_size(
                location + offset,
                Size::from((22, 35)).to_f64().to_physical(scale).to_i32_round(),
            );
            frame.render_texture_from_to(
                &self.texture,
                src.to_f64(),
                dst.to_f64(),
                &damage,
                Transform::Normal,
                1.0,
            )?;
            offset += Point::from((24.0, 0.0)).to_physical(scale).to_i32_round();
        }

        Ok(())
    }
}

#[cfg(feature = "debug")]
pub fn draw_fps<R>(texture: &<R as Renderer>::TextureId, value: u32) -> FpsElement<<R as Renderer>::TextureId>
where
    R: Renderer + ImportAll,
    <R as Renderer>::TextureId: Clone,
{
    FpsElement {
        value,
        texture: texture.clone(),
    }
}
