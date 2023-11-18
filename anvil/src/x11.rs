#[cfg(feature = "xwayland")]
use std::ffi::OsString;
use std::{
    sync::{atomic::Ordering, Mutex},
    time::Duration,
};

use crate::{
    drawing::*,
    render::*,
    state::{post_repaint, take_presentation_feedback, AnvilState, Backend, CalloopData},
};
#[cfg(feature = "egl")]
use smithay::backend::renderer::ImportEgl;
#[cfg(feature = "debug")]
use smithay::backend::{allocator::Fourcc, renderer::ImportMem};

use smithay::{
    backend::{
        allocator::{
            dmabuf::{Dmabuf, DmabufAllocator},
            gbm::{GbmAllocator, GbmBufferFlags},
            vulkan::{ImageUsageFlags, VulkanAllocator},
        },
        egl::{EGLContext, EGLDisplay},
        renderer::{
            damage::OutputDamageTracker,
            element::AsRenderElements,
            gles::{GlesError, GlesFrame, GlesRenderer, GlesTexture},
            pixman::{PixmanError, PixmanFrame, PixmanRenderer, PixmanTexture},
            Bind, Frame, ImportDma, ImportDmaWl, ImportMem, ImportMemWl, Renderer, Texture, Unbind,
        },
        vulkan::{version::Version, Instance, PhysicalDevice},
        x11::{WindowBuilder, X11Backend, X11Event, X11Surface},
    },
    delegate_dmabuf,
    input::pointer::{CursorImageAttributes, CursorImageStatus},
    output::{Mode, Output, PhysicalProperties, Subpixel},
    reexports::{
        ash::vk::ExtPhysicalDeviceDrmFn,
        calloop::EventLoop,
        gbm,
        wayland_protocols::wp::presentation_time::server::wp_presentation_feedback,
        wayland_server::{protocol::wl_surface, Display},
    },
    utils::{DeviceFd, IsAlive, Scale, Transform},
    wayland::{
        compositor,
        dmabuf::{
            DmabufFeedback, DmabufFeedbackBuilder, DmabufGlobal, DmabufHandler, DmabufState, ImportNotifier,
        },
    },
};
use tracing::{error, info, trace, warn};

pub const OUTPUT_NAME: &str = "x11";

#[derive(Debug)]
pub struct X11Data {
    render: bool,
    mode: Mode,
    // FIXME: If GlesRenderer is dropped before X11Surface, then the MakeCurrent call inside Gles2Renderer will
    // fail because the X11Surface is keeping gbm alive.
    renderer: AutoRenderer,
    damage_tracker: OutputDamageTracker,
    surface: X11Surface,
    dmabuf_state: DmabufState,
    _dmabuf_global: DmabufGlobal,
    _dmabuf_default_feedback: DmabufFeedback,
    #[cfg(feature = "debug")]
    fps: fps_ticker::Fps,
}

#[macro_export]
#[doc(hidden)]
macro_rules! impl_renderer_internal {
    (@enum $(#[$attr:meta])* $vis:vis $name:ident; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$attr])*
        $vis enum $name {
            $(
                $(
                    #[$meta]
                )*
                $body($field)
            ),*,
            #[doc(hidden)]
            _GenericCatcher(std::convert::Infallible),
        }
    };
    (@error $(#[$error_attr:meta])* $vis:vis $error:ident; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$error_attr])*
        $vis enum $error {
            $(
                $(
                    #[$meta]
                )*
                $body($other_error)
            ),*,
            #[doc(hidden)]
            _GenericCatcher(std::convert::Infallible),
        }

        impl std::fmt::Display for $error {
            fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> { todo!() }
        }

        impl std::error::Error for $error {

        }
    };
    (@call_texture $name:ident; $($x:ident),*) => {
        smithay::backend::renderer::Texture::$name($($x),*)
    };
    (@call_frame $name:ident; $($x:ident),*) => {
        smithay::backend::renderer::Frame::$name($($x),*)
    };
    (@call_renderer $name:ident; $($x:ident),*) => {
        smithay::backend::renderer::Renderer::$name($($x),*)
    };
    (@texture_id $(#[$texture_id_attr:meta])* $vis:vis $texture_id:ident $error:ident; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$texture_id_attr])*
        $vis enum $texture_id {
            $(
                $(
                    #[$meta]
                )*
                $body($other_texture_id)
            ),*,
            #[doc(hidden)]
            _GenericCatcher(std::convert::Infallible),
        }

        impl smithay::backend::renderer::Texture for $texture_id {
            fn width(&self) -> u32 {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_texture width; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn height(&self) -> u32 {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_texture height; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn format(&self) -> Option<smithay::backend::allocator::Fourcc> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_texture format; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
        }
    };
    (@frame $(#[$frame_attr:meta])* $vis:vis $frame:ident<$frame_lifetime:lifetime> $error:ident $texture_id:ident; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$frame_attr])*
        $vis enum $frame<$frame_lifetime> {
            $(
                $(
                    #[$meta]
                )*
                $body($other_frame)
            ),*,
            #[doc(hidden)]
            _GenericCatcher(std::convert::Infallible),
        }

        impl<$frame_lifetime> smithay::backend::renderer::Frame for $frame<$frame_lifetime> {
            type Error = $error;
            type TextureId = $texture_id;

            fn id(&self) -> usize {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_frame id; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn clear(
                &mut self,
                color: [f32; 4],
                at: &[smithay::utils::Rectangle<i32, smithay::utils::Physical>],
            ) -> Result<(), Self::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_frame clear; x, color, at)
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn draw_solid(
                &mut self,
                dst: smithay::utils::Rectangle<i32, smithay::utils::Physical>,
                damage: &[smithay::utils::Rectangle<i32, smithay::utils::Physical>],
                color: [f32; 4],
            ) -> Result<(), Self::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_frame draw_solid; x, dst, damage, color)
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn render_texture_from_to(
                &mut self,
                texture: &Self::TextureId,
                src: smithay::utils::Rectangle<f64, smithay::utils::Buffer>,
                dst: smithay::utils::Rectangle<i32, smithay::utils::Physical>,
                damage: &[smithay::utils::Rectangle<i32, smithay::utils::Physical>],
                src_transform: Transform,
                alpha: f32,
            ) -> Result<(), Self::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => {
                            let texture = match texture {
                                $texture_id::$body(texture) => texture,
                                _ => unreachable!()
                            };
                            $crate::impl_renderer_internal!(@call_frame render_texture_from_to; x, texture, src, dst, damage, src_transform, alpha)
                                .map_err(|err| $error::$body(err))
                        }
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn transformation(&self) -> smithay::utils::Transform {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_frame transformation; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn finish(self) -> Result<smithay::backend::renderer::sync::SyncPoint, Self::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_frame finish; x)
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
        }
    };
    (@renderer $vis:vis $name:ident $error:ident $texture_id:ident $frame:ident<$frame_lifetime:lifetime>; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl smithay::backend::renderer::Renderer for $name {
            type Error = $error;
            type TextureId = $texture_id;
            type Frame<$frame_lifetime> = $frame<$frame_lifetime>;

            fn id(&self) -> usize {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_renderer id; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn downscale_filter(
                &mut self,
                filter: smithay::backend::renderer::TextureFilter,
            ) -> Result<(), Self::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_renderer downscale_filter; x, filter)
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn upscale_filter(
                &mut self,
                filter: smithay::backend::renderer::TextureFilter,
            ) -> Result<(), Self::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_renderer upscale_filter; x, filter)
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn set_debug_flags(&mut self, flags: smithay::backend::renderer::DebugFlags) {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_renderer set_debug_flags; x, flags)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn debug_flags(&self) -> smithay::backend::renderer::DebugFlags {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_renderer debug_flags; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn render(
                &mut self,
                output_size: smithay::utils::Size<i32, smithay::utils::Physical>,
                dst_transform: Transform,
            ) -> Result<Self::Frame<'_>, Self::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_renderer render; x, output_size, dst_transform)
                            .map(|frame| $frame::$body(frame))
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
        }
    };
    (@call_import_egl $name:ident; $($x:ident),*) => {
        smithay::backend::renderer::ImportEgl::$name($($x),*)
    };
    (@call_import_dma $name:ident; $($x:ident),*) => {
        smithay::backend::renderer::ImportDma::$name($($x),*)
    };
    (@call_import_dma_wl $name:ident; $($x:ident),*) => {
        smithay::backend::renderer::ImportDmaWl::$name($($x),*)
    };
    (@call_import_mem $name:ident; $($x:ident),*) => {
        smithay::backend::renderer::ImportMem::$name($($x),*)
    };
    (@call_import_mem_wl $name:ident; $($x:ident),*) => {
        smithay::backend::renderer::ImportMemWl::$name($($x),*)
    };
    (@import $name:ident $error:ident $texture_id:ident ImportEgl $($import:ident)*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl smithay::backend::renderer::ImportEgl for $name {
            fn bind_wl_display(
                &mut self,
                display: &smithay::reexports::wayland_server::DisplayHandle,
            ) -> Result<(), smithay::backend::egl::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_import_egl bind_wl_display; x, display)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn unbind_wl_display(&mut self) {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_import_egl unbind_wl_display; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn egl_reader(&self) -> Option<&smithay::backend::egl::display::EGLBufferReader> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_import_egl egl_reader; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn import_egl_buffer(
                &mut self,
                buffer: &smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer,
                surface: Option<&smithay::wayland::compositor::SurfaceData>,
                damage: &[smithay::utils::Rectangle<i32, smithay::utils::Buffer>],
            ) -> Result<<Self as Renderer>::TextureId, <Self as Renderer>::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_import_egl import_egl_buffer; x, buffer, surface, damage)
                            .map(|texture| $texture_id::$body(texture))
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
        }

        $crate::impl_renderer_internal!(@import $name $error $texture_id $($import)*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident $error:ident $texture_id:ident ImportDma $($import:ident)*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl smithay::backend::renderer::ImportDma for $name {
            fn import_dmabuf(
                &mut self,
                dmabuf: &Dmabuf,
                damage: Option<&[smithay::utils::Rectangle<i32, smithay::utils::Buffer>]>,
            ) -> Result<<Self as Renderer>::TextureId, <Self as Renderer>::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_import_dma import_dmabuf; x, dmabuf, damage)
                            .map(|texture| $texture_id::$body(texture))
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn dmabuf_formats(&self) -> Box<dyn Iterator<Item = smithay::backend::allocator::Format>> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_import_dma dmabuf_formats; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn has_dmabuf_format(&self, format: smithay::backend::allocator::Format) -> bool {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_import_dma has_dmabuf_format; x, format)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
        }

        $crate::impl_renderer_internal!(@import $name $error $texture_id $($import)*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident $error:ident $texture_id:ident ImportDmaWl $($import:ident)*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl smithay::backend::renderer::ImportDmaWl for $name {
            fn import_dma_buffer(
                &mut self,
                buffer: &smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer,
                surface: Option<&smithay::wayland::compositor::SurfaceData>,
                damage: &[smithay::utils::Rectangle<i32, smithay::utils::Buffer>],
            ) -> Result<<Self as Renderer>::TextureId, <Self as Renderer>::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_import_dma_wl import_dma_buffer; x, buffer, surface, damage)
                            .map(|texture| $texture_id::$body(texture))
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
        }

        $crate::impl_renderer_internal!(@import $name $error $texture_id $($import)*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident $error:ident $texture_id:ident ImportMem $($import:ident)*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl smithay::backend::renderer::ImportMem for $name {
            fn import_memory(
                &mut self,
                data: &[u8],
                format: gbm::Format,
                size: smithay::utils::Size<i32, smithay::utils::Buffer>,
                flipped: bool,
            ) -> Result<<Self as Renderer>::TextureId, <Self as Renderer>::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_import_mem import_memory; x, data, format, size, flipped)
                            .map(|texture| $texture_id::$body(texture))
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn update_memory(
                &mut self,
                texture: &<Self as Renderer>::TextureId,
                data: &[u8],
                region: smithay::utils::Rectangle<i32, smithay::utils::Buffer>,
            ) -> Result<(), <Self as Renderer>::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => {
                            let texture = match texture {
                                $texture_id::$body(texture) => texture,
                                _ => unreachable!()
                            };
                            $crate::impl_renderer_internal!(@call_import_mem update_memory; x, texture, data, region)
                                .map_err(|err| $error::$body(err))
                        }
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn mem_formats(&self) -> Box<dyn Iterator<Item = gbm::Format>> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_import_mem mem_formats; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
        }

        $crate::impl_renderer_internal!(@import $name $error $texture_id $($import)*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident $error:ident $texture_id:ident ImportMemWl $($import:ident)*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl smithay::backend::renderer::ImportMemWl for $name {
            fn import_shm_buffer(
                &mut self,
                buffer: &smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer,
                surface: Option<&smithay::wayland::compositor::SurfaceData>,
                damage: &[smithay::utils::Rectangle<i32, smithay::utils::Buffer>],
            ) -> Result<<Self as Renderer>::TextureId, <Self as Renderer>::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_import_mem_wl import_shm_buffer; x, buffer, surface, damage)
                            .map(|texture| $texture_id::$body(texture))
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn shm_formats(
                &self,
            ) -> Box<dyn Iterator<Item = smithay::reexports::wayland_server::protocol::wl_shm::Format>> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::impl_renderer_internal!(@call_import_mem_wl shm_formats; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
        }

        $crate::impl_renderer_internal!(@import $name $error $texture_id $($import)*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident $error:ident $texture_id:ident; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {};
}

macro_rules! impl_renderer {
    ($(#[$attr:meta])* $vis:vis $name:ident {
            $(#[$error_attr:meta])* type Error = $error:ident;
            $(#[$texture_id_attr:meta])* type TextureId = $texture_id:ident;
            $(#[$frame_attr:meta])* type Frame = $frame:ident<$frame_lifetime:lifetime>;
        };
        Imports=[$($import:ident,)*];
        $($tail:tt)*) => {
        $crate::impl_renderer_internal!(@enum $(#[$attr])* $vis $name; $($tail)*);
        $crate::impl_renderer_internal!(@error $(#[$error_attr])* $vis $error; $($tail)*);
        $crate::impl_renderer_internal!(@texture_id $(#[$texture_id_attr])* $vis $texture_id $error; $($tail)*);
        $crate::impl_renderer_internal!(@frame $(#[$frame_attr])* $vis $frame<$frame_lifetime> $error $texture_id; $($tail)*);
        $crate::impl_renderer_internal!(@renderer $vis $name $error $texture_id $frame<$frame_lifetime>; $($tail)*);
        $crate::impl_renderer_internal!(@import $name $error $texture_id $($import)*; $($tail)*);
    };
}

impl_renderer! {
    #[derive(Debug)]
    pub AutoRenderer {
        #[derive(Debug)]
        type Error = AutoRendererError;
        #[derive(Debug, Clone)]
        type TextureId = AutoRendererTexture;
        #[derive(Debug)]
        type Frame = AutoRendererFrame<'frame>;
    };
    Imports=[ImportDma, ImportDmaWl, ImportEgl, ImportMem, ImportMemWl,];
    Gles(GlesRenderer {
        type Error = GlesError;
        type TextureId = GlesTexture;
        type Frame = GlesFrame<'frame>;
    }),
    Software(PixmanRenderer {
        type Error = PixmanError;
        type TextureId = PixmanTexture;
        type Frame = PixmanFrame<'frame>;
    }),
}

impl Bind<Dmabuf> for AutoRenderer {
    fn bind(&mut self, target: Dmabuf) -> Result<(), <Self as Renderer>::Error> {
        match self {
            AutoRenderer::Gles(renderer) => renderer.bind(target).map_err(AutoRendererError::Gles),
            AutoRenderer::Software(renderer) => renderer.bind(target).map_err(AutoRendererError::Software),
            _ => unreachable!(),
        }
    }

    fn supported_formats(&self) -> Option<std::collections::HashSet<smithay::backend::allocator::Format>> {
        match self {
            AutoRenderer::Gles(renderer) => Bind::<Dmabuf>::supported_formats(renderer),
            AutoRenderer::Software(renderer) => Bind::<Dmabuf>::supported_formats(renderer),
            _ => unreachable!(),
        }
    }
}

impl Unbind for AutoRenderer {
    fn unbind(&mut self) -> Result<(), <Self as Renderer>::Error> {
        match self {
            AutoRenderer::Gles(renderer) => renderer.unbind().map_err(AutoRendererError::Gles),
            AutoRenderer::Software(renderer) => renderer.unbind().map_err(AutoRendererError::Software),
            _ => unreachable!(),
        }
    }
}

impl DmabufHandler for AnvilState<X11Data> {
    fn dmabuf_state(&mut self) -> &mut DmabufState {
        &mut self.backend_data.dmabuf_state
    }

    fn dmabuf_imported(&mut self, _global: &DmabufGlobal, dmabuf: Dmabuf, notifier: ImportNotifier) {
        if self.backend_data.renderer.import_dmabuf(&dmabuf, None).is_err() {
            notifier.failed();
        }
    }
}
delegate_dmabuf!(AnvilState<X11Data>);

impl Backend for X11Data {
    fn seat_name(&self) -> String {
        "x11".to_owned()
    }
    fn reset_buffers(&mut self, _output: &Output) {
        self.surface.reset_buffers();
    }
    fn early_import(&mut self, _surface: &wl_surface::WlSurface) {}
}

pub fn run_x11() {
    let mut event_loop = EventLoop::try_new().unwrap();
    let display = Display::new().unwrap();
    let mut display_handle = display.handle();

    let backend = X11Backend::new().expect("Failed to initilize X11 backend");
    let handle = backend.handle();

    // Obtain the DRM node the X server uses for direct rendering.
    let (node, fd) = handle
        .drm_node()
        .expect("Could not get DRM node used by X server");

    // Create the gbm device for buffer allocation.
    let device = gbm::Device::new(DeviceFd::from(fd)).expect("Failed to create gbm device");
    // Initialize EGL using the GBM device.
    let egl = EGLDisplay::new(device.clone()).expect("Failed to create EGLDisplay");
    // Create the OpenGL context
    let context = EGLContext::new(&egl).expect("Failed to create EGLContext");

    let window = WindowBuilder::new()
        .title("Anvil")
        .build(&handle)
        .expect("Failed to create first window");

    let skip_vulkan = std::env::var("ANVIL_NO_VULKAN")
        .map(|x| {
            x == "1" || x.to_lowercase() == "true" || x.to_lowercase() == "yes" || x.to_lowercase() == "y"
        })
        .unwrap_or(false);

    let vulkan_allocator = if !skip_vulkan {
        Instance::new(Version::VERSION_1_2, None)
            .ok()
            .and_then(|instance| {
                PhysicalDevice::enumerate(&instance).ok().and_then(|devices| {
                    devices
                        .filter(|phd| phd.has_device_extension(ExtPhysicalDeviceDrmFn::name()))
                        .find(|phd| {
                            phd.primary_node().unwrap() == Some(node)
                                || phd.render_node().unwrap() == Some(node)
                        })
                })
            })
            .and_then(|physical_device| {
                VulkanAllocator::new(
                    &physical_device,
                    ImageUsageFlags::COLOR_ATTACHMENT | ImageUsageFlags::SAMPLED,
                )
                .ok()
            })
    } else {
        None
    };

    // #[cfg_attr(not(feature = "egl"), allow(unused_mut))]
    // let mut renderer = AutoRenderer::from(unsafe { GlesRenderer::new(context) }.expect("Failed to initialize renderer"));

    #[cfg_attr(not(feature = "egl"), allow(unused_mut))]
    let mut renderer = AutoRenderer::Software(PixmanRenderer::new().unwrap());

    let modifier = Bind::<Dmabuf>::supported_formats(&renderer)
        .map(|formats| {
            formats
                .iter()
                .filter_map(|format| {
                    if format.code == window.format() {
                        Some(format.modifier)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    let surface = match vulkan_allocator {
        // Create the surface for the window.
        Some(vulkan_allocator) => handle
            .create_surface(
                &window,
                DmabufAllocator(vulkan_allocator),
                modifier.iter().copied(),
            )
            .expect("Failed to create X11 surface"),
        None => handle
            .create_surface(
                &window,
                DmabufAllocator(GbmAllocator::new(device, GbmBufferFlags::RENDERING)),
                modifier.iter().copied(),
            )
            .expect("Failed to create X11 surface"),
    };

    #[cfg(feature = "egl")]
    if renderer.bind_wl_display(&display.handle()).is_ok() {
        info!("EGL hardware-acceleration enabled");
    }

    let dmabuf_formats = renderer.dmabuf_formats().collect::<Vec<_>>();
    let dmabuf_default_feedback = DmabufFeedbackBuilder::new(node.dev_id(), dmabuf_formats)
        .build()
        .unwrap();
    let mut dmabuf_state = DmabufState::new();
    let dmabuf_global = dmabuf_state.create_global_with_default_feedback::<AnvilState<X11Data>>(
        &display.handle(),
        &dmabuf_default_feedback,
    );

    let size = {
        let s = window.size();

        (s.w as i32, s.h as i32).into()
    };

    let mode = Mode {
        size,
        refresh: 60_000,
    };

    #[cfg(feature = "debug")]
    let fps_image =
        image::io::Reader::with_format(std::io::Cursor::new(FPS_NUMBERS_PNG), image::ImageFormat::Png)
            .decode()
            .unwrap();
    #[cfg(feature = "debug")]
    let fps_texture = renderer
        .import_memory(
            &fps_image.to_rgba8(),
            Fourcc::Abgr8888,
            (fps_image.width() as i32, fps_image.height() as i32).into(),
            false,
        )
        .expect("Unable to upload FPS texture");
    #[cfg(feature = "debug")]
    let mut fps_element = FpsElement::new(fps_texture);
    let output = Output::new(
        OUTPUT_NAME.to_string(),
        PhysicalProperties {
            size: (0, 0).into(),
            subpixel: Subpixel::Unknown,
            make: "Smithay".into(),
            model: "X11".into(),
        },
    );
    let _global = output.create_global::<AnvilState<X11Data>>(&display.handle());
    output.change_current_state(Some(mode), None, None, Some((0, 0).into()));
    output.set_preferred(mode);

    let damage_tracker = OutputDamageTracker::from_output(&output);

    let data = X11Data {
        render: true,
        mode,
        surface,
        renderer,
        damage_tracker,
        dmabuf_state,
        _dmabuf_global: dmabuf_global,
        _dmabuf_default_feedback: dmabuf_default_feedback,
        #[cfg(feature = "debug")]
        fps: fps_ticker::Fps::default(),
    };

    let mut state = AnvilState::init(display, event_loop.handle(), data, true);
    state
        .shm_state
        .update_formats(state.backend_data.renderer.shm_formats());
    state.space.map_output(&output, (0, 0));

    let output_clone = output.clone();
    event_loop
        .handle()
        .insert_source(backend, move |event, _, data| match event {
            X11Event::CloseRequested { .. } => {
                data.state.running.store(false, Ordering::SeqCst);
            }
            X11Event::Resized { new_size, .. } => {
                let output = &output_clone;
                let size = { (new_size.w as i32, new_size.h as i32).into() };

                data.state.backend_data.mode = Mode {
                    size,
                    refresh: 60_000,
                };
                output.delete_mode(output.current_mode().unwrap());
                output.change_current_state(Some(data.state.backend_data.mode), None, None, None);
                output.set_preferred(data.state.backend_data.mode);
                crate::shell::fixup_positions(&mut data.state.space, data.state.pointer.current_location());

                data.state.backend_data.render = true;
            }
            X11Event::PresentCompleted { .. } | X11Event::Refresh { .. } => {
                data.state.backend_data.render = true;
            }
            X11Event::Input(event) => {
                data.state
                    .process_input_event_windowed(&data.display_handle, event, OUTPUT_NAME)
            }
        })
        .expect("Failed to insert X11 Backend into event loop");

    #[cfg(feature = "xwayland")]
    if let Err(e) = state.xwayland.start(
        state.handle.clone(),
        None,
        std::iter::empty::<(OsString, OsString)>(),
        true,
        |_| {},
    ) {
        error!("Failed to start XWayland: {}", e);
    }
    info!("Initialization completed, starting the main loop.");

    let mut pointer_element = PointerElement::default();

    while state.running.load(Ordering::SeqCst) {
        if state.backend_data.render {
            profiling::scope!("render_frame");

            let backend_data = &mut state.backend_data;
            // We need to borrow everything we want to refer to inside the renderer callback otherwise rustc is unhappy.
            let cursor_status = &state.cursor_status;
            #[cfg(feature = "debug")]
            let fps = backend_data.fps.avg().round() as u32;
            #[cfg(feature = "debug")]
            fps_element.update_fps(fps);

            let (buffer, age) = backend_data.surface.buffer().expect("gbm device was destroyed");
            if let Err(err) = backend_data.renderer.bind(buffer) {
                error!("Error while binding buffer: {}", err);
                profiling::finish_frame!();
                continue;
            }

            #[cfg(feature = "debug")]
            if let Some(renderdoc) = state.renderdoc.as_mut() {
                renderdoc.start_frame_capture(
                    backend_data.renderer.egl_context().get_context_handle(),
                    std::ptr::null(),
                );
            }

            let mut cursor_guard = cursor_status.lock().unwrap();
            let mut elements: Vec<CustomRenderElements<AutoRenderer>> = Vec::new();

            // draw the cursor as relevant
            // reset the cursor if the surface is no longer alive
            let mut reset = false;
            if let CursorImageStatus::Surface(ref surface) = *cursor_guard {
                reset = !surface.alive();
            }
            if reset {
                *cursor_guard = CursorImageStatus::default_named();
            }
            let cursor_visible = !matches!(*cursor_guard, CursorImageStatus::Surface(_));

            let scale = Scale::from(output.current_scale().fractional_scale());
            let cursor_hotspot = if let CursorImageStatus::Surface(ref surface) = *cursor_guard {
                compositor::with_states(surface, |states| {
                    states
                        .data_map
                        .get::<Mutex<CursorImageAttributes>>()
                        .unwrap()
                        .lock()
                        .unwrap()
                        .hotspot
                })
            } else {
                (0, 0).into()
            };
            let cursor_pos = state.pointer.current_location() - cursor_hotspot.to_f64();
            let cursor_pos_scaled = cursor_pos.to_physical(scale).to_i32_round();

            pointer_element.set_status(cursor_guard.clone());
            elements.extend(pointer_element.render_elements(
                &mut backend_data.renderer,
                cursor_pos_scaled,
                scale,
                1.0,
            ));

            // draw the dnd icon if any
            if let Some(surface) = state.dnd_icon.as_ref() {
                if surface.alive() {
                    elements.extend(AsRenderElements::<AutoRenderer>::render_elements(
                        &smithay::desktop::space::SurfaceTree::from_surface(surface),
                        &mut backend_data.renderer,
                        cursor_pos_scaled,
                        scale,
                        1.0,
                    ));
                }
            }

            #[cfg(feature = "debug")]
            elements.push(CustomRenderElements::Fps(fps_element.clone()));

            let render_res = render_output(
                &output,
                &state.space,
                elements,
                &mut backend_data.renderer,
                &mut backend_data.damage_tracker,
                age.into(),
                state.show_window_preview,
            );

            match render_res {
                Ok(render_output_result) => {
                    trace!("Finished rendering");
                    if let Err(err) = backend_data.surface.submit() {
                        backend_data.surface.reset_buffers();
                        warn!("Failed to submit buffer: {}. Retrying", err);
                    } else {
                        state.backend_data.render = false;
                    };

                    #[cfg(feature = "debug")]
                    if render_output_result.damage.is_some() {
                        if let Some(renderdoc) = state.renderdoc.as_mut() {
                            renderdoc.end_frame_capture(
                                state.backend_data.renderer.egl_context().get_context_handle(),
                                std::ptr::null(),
                            );
                        }
                    } else if let Some(renderdoc) = state.renderdoc.as_mut() {
                        renderdoc.discard_frame_capture(
                            state.backend_data.renderer.egl_context().get_context_handle(),
                            std::ptr::null(),
                        );
                    }

                    // Send frame events so that client start drawing their next frame
                    let time = state.clock.now();
                    post_repaint(&output, &render_output_result.states, &state.space, None, time);

                    if render_output_result.damage.is_some() {
                        let mut output_presentation_feedback =
                            take_presentation_feedback(&output, &state.space, &render_output_result.states);
                        output_presentation_feedback.presented(
                            time,
                            output
                                .current_mode()
                                .map(|mode| Duration::from_secs_f64(1_000f64 / mode.refresh as f64))
                                .unwrap_or_default(),
                            0,
                            wp_presentation_feedback::Kind::Vsync,
                        )
                    }
                }
                Err(err) => {
                    #[cfg(feature = "debug")]
                    if let Some(renderdoc) = state.renderdoc.as_mut() {
                        renderdoc.discard_frame_capture(
                            backend_data.renderer.egl_context().get_context_handle(),
                            std::ptr::null(),
                        );
                    }

                    backend_data.surface.reset_buffers();
                    error!("Rendering error: {}", err);
                    // TODO: convert RenderError into SwapBuffersError and skip temporary (will retry) and panic on ContextLost or recreate
                }
            }

            #[cfg(feature = "debug")]
            state.backend_data.fps.tick();
            window.set_cursor_visible(cursor_visible);
            profiling::finish_frame!();
        }

        let mut calloop_data = CalloopData {
            state,
            display_handle,
        };
        let result = event_loop.dispatch(Some(Duration::from_millis(16)), &mut calloop_data);
        CalloopData {
            state,
            display_handle,
        } = calloop_data;

        if result.is_err() {
            state.running.store(false, Ordering::SeqCst);
        } else {
            state.space.refresh();
            state.popups.cleanup();
            display_handle.flush_clients().unwrap();
        }
    }
}
