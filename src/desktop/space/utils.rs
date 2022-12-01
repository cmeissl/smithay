use crate::{
    backend::renderer::{
        element::{
            utils::{
                constrain_as_render_elements, ConstrainAlign, ConstrainScaleBehavior, CropRenderElement,
                RelocateRenderElement, RescaleRenderElement,
            },
            AsRenderElements,
        },
        Renderer,
    },
    utils::{Logical, Point, Scale, Size},
};

use super::SpaceElement;

/// Defines the reference size for the constrain behavior.
#[derive(Debug, Copy, Clone)]
pub enum ConstrainReference {
    /// Use the bounding box as the reference
    BoundingBox,
    /// Use the geometry as the reference
    Geometry,
}

/// Defines the behavior for [`constrain_space_element`]
#[derive(Debug, Copy, Clone)]
pub struct ConstrainBehavior {
    /// Defines what should be used as the reference for calculating the scale
    pub reference: ConstrainReference,
    /// Defines how the element should be scaled
    pub behavior: ConstrainScaleBehavior,
    /// Defines the alignment of the element inside of the constrain
    pub align: ConstrainAlign,
}

/// Constrain the render element of a  [`SpaceElement`]
pub fn constrain_space_element<R, E, C>(
    renderer: &mut R,
    element: &E,
    location: impl Into<Point<i32, Logical>>,
    scale: impl Into<Scale<f64>>,
    size: impl Into<Size<i32, Logical>>,
    behavior: ConstrainBehavior,
) -> impl Iterator<Item = C>
where
    R: Renderer,
    E: SpaceElement + AsRenderElements<R>,
    C: From<
        CropRenderElement<
            RelocateRenderElement<RescaleRenderElement<<E as AsRenderElements<R>>::RenderElement>>,
        >,
    >,
{
    let location = location.into();
    let scale = scale.into();
    let constrain_size = size.into();

    let scale_reference = match behavior.reference {
        ConstrainReference::BoundingBox => element.bbox(),
        ConstrainReference::Geometry => element.geometry(),
    };

    constrain_as_render_elements(
        element,
        renderer,
        location.to_physical_precise_round(scale),
        constrain_size.to_physical_precise_round(scale),
        scale_reference.to_physical_precise_round(scale),
        behavior.behavior,
        behavior.align,
        scale,
    )
}
