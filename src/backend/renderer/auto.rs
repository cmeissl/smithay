//! TODO: Docs
#[macro_export]
#[doc(hidden)]
macro_rules! auto_renderer_internal {
    /* Enum */
    (@enum $(#[$attr:meta])* $vis:vis $name:ident<$($lt:lifetime),+, $($custom:ident),+> $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$attr])*
        $vis enum $name<$($lt),+, $($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
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
    (@enum $(#[$attr:meta])* $vis:vis $name:ident<$($lt:lifetime),+>; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$attr])*
        $vis enum $name<$($lt),*> {
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
    (@enum $(#[$attr:meta])* $vis:vis $name:ident<$($custom:ident),+> $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$attr])*
        $vis enum $name<$($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
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

    /* Error */
    (@error $(#[$error_attr:meta])* $vis:vis $error:ident<$($error_lifetime:lifetime),+, $($error_custom:ident),+> $(where $($error_target:ty: $error_bound:tt $(+ $error_additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$error_attr])*
        $vis enum $error<$($error_lifetime),+, $($error_custom),+>
        $(where $($error_target: $error_bound $(+ $error_additional_bound)*),+)? {
            $(
                $(
                    #[$meta]
                )*
                $body($other_error)
            ),*,
            #[doc(hidden)]
            _GenericCatcher(std::convert::Infallible),
        }

        impl<$($error_lifetime),+, $($error_custom),+> std::fmt::Display for $error<$($error_lifetime),+, $($error_custom),+>
        $(where $($error_target: $error_bound $(+ $error_additional_bound)*),+)? {
            fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> { todo!() }
        }

        impl<$($error_lifetime),+, $($error_custom),+> std::error::Error for $error<$($error_lifetime),+, $($error_custom),+>
        $(where $($error_target: $error_bound $(+ $error_additional_bound)*),+)? {

        }
    };
    (@error $(#[$error_attr:meta])* $vis:vis $error:ident<$($error_lifetime:lifetime),+>; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$error_attr])*
        $vis enum $error<$($error_lifetime),+> {
            $(
                $(
                    #[$meta]
                )*
                $body($other_error)
            ),*,
            #[doc(hidden)]
            _GenericCatcher(std::convert::Infallible),
        }

        impl<$($error_lifetime),+> std::fmt::Display for $error<$($error_lifetime),+> {
            fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> { todo!() }
        }

        impl<$($error_lifetime),+> std::error::Error for $error<$($error_lifetime),+> {

        }
    };
    (@error $(#[$error_attr:meta])* $vis:vis $error:ident<$($error_custom:ident),+> $(where $($error_target:ty: $error_bound:tt $(+ $error_additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$error_attr])*
        $vis enum $error<$($error_custom),+>
        $(where $($error_target: $error_bound $(+ $error_additional_bound)*),+)? {
            $(
                $(
                    #[$meta]
                )*
                $body($other_error)
            ),*,
            #[doc(hidden)]
            _GenericCatcher(std::convert::Infallible),
        }

        impl<$($error_custom),+> std::fmt::Display for $error<$($error_custom),+>
        $(where $($error_target: $error_bound $(+ $error_additional_bound)*),+)? {
            fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> { todo!() }
        }

        impl<$($error_custom),+> std::error::Error for $error<$($error_custom),+>
        $(where $($error_target: $error_bound $(+ $error_additional_bound)*),+)? {

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

    /* Texture */
    (@call_texture $name:ident; $($x:ident),*) => {
        $crate::backend::renderer::Texture::$name($($x),*)
    };
    (@texture_id_impl $error:ident $texture_id:ident; $($(#[$meta:meta])* $body:ident),*) => {
            fn width(&self) -> u32 {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_texture width; x)
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
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_texture height; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn format(&self) -> Option<$crate::backend::allocator::Fourcc> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_texture format; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
    };
    (@texture_id $(#[$texture_id_attr:meta])* $vis:vis $error:ident $texture_id:ident<$($texture_id_lifetime:lifetime),+, $($texture_id_custom:ident),+> $(where $($texture_id_target:ty: $texture_id_bound:tt $(+ $texture_id_additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$texture_id_attr])*
        $vis enum $texture_id<$($texture_id_lifetime),+, $($texture_id_custom),+>
        $(where $($texture_id_target: $texture_id_bound $(+ $texture_id_additional_bound)*),+)? {
            $(
                $(
                    #[$meta]
                )*
                $body($other_texture_id)
            ),*,
            #[doc(hidden)]
            _GenericCatcher(std::convert::Infallible),
        }

        impl<$($texture_id_lifetime),+, $($texture_id_custom),+> $crate::backend::renderer::Texture for $texture_id<$($texture_id_lifetime),+, $($texture_id_custom),+>
        $(where $($texture_id_target: $texture_id_bound $(+ $texture_id_additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@texture_id_impl $error $texture_id; $($(#[$meta])* $body),*);
        }
    };
    (@texture_id $(#[$texture_id_attr:meta])* $vis:vis $error:ident $texture_id:ident<$($texture_id_lifetime:lifetime),+>; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$texture_id_attr])*
        $vis enum $texture_id<$($texture_id_lifetime),+,> {
            $(
                $(
                    #[$meta]
                )*
                $body($other_texture_id)
            ),*,
            #[doc(hidden)]
            _GenericCatcher(std::convert::Infallible),
        }

        impl<$($texture_id_lifetime),+> $crate::backend::renderer::Texture for $texture_id<$($texture_id_lifetime),+> {
            $crate::auto_renderer_internal!(@texture_id_impl $error $texture_id; $($(#[$meta])* $body),*);
        }
    };
    (@texture_id $(#[$texture_id_attr:meta])* $vis:vis $error:ident $texture_id:ident<$($texture_id_custom:ident),+> $(where $($texture_id_target:ty: $texture_id_bound:tt $(+ $texture_id_additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$texture_id_attr])*
        $vis enum $texture_id<$($texture_id_custom),+>
        $(where $($texture_id_target: $texture_id_bound $(+ $texture_id_additional_bound)*),+)? {
            $(
                $(
                    #[$meta]
                )*
                $body($other_texture_id)
            ),*,
            #[doc(hidden)]
            _GenericCatcher(std::convert::Infallible),
        }

        impl<$($texture_id_custom),+> $crate::backend::renderer::Texture for $texture_id<$($texture_id_custom),+>
        $(where $($texture_id_target: $texture_id_bound $(+ $texture_id_additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@texture_id_impl $error $texture_id; $($(#[$meta])* $body),*);
        }
    };
    (@texture_id $(#[$texture_id_attr:meta])* $vis:vis $error:ident $texture_id:ident; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
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

        impl $crate::backend::renderer::Texture for $texture_id {
            $crate::auto_renderer_internal!(@texture_id_impl $error $texture_id; $($(#[$meta])* $body),*);
        }
    };

    /* Frame */
    (@call_frame $name:ident; $($x:ident),*) => {
        $crate::backend::renderer::Frame::$name($($x),*)
    };
    (@frame_impl $error:ident $texture_id:ident; $($(#[$meta:meta])* $body:ident),*) => {
            fn id(&self) -> usize {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_frame id; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn clear(
                &mut self,
                color: [f32; 4],
                at: &[$crate::utils::Rectangle<i32, $crate::utils::Physical>],
            ) -> Result<(), Self::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_frame clear; x, color, at)
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn draw_solid(
                &mut self,
                dst: $crate::utils::Rectangle<i32, $crate::utils::Physical>,
                damage: &[$crate::utils::Rectangle<i32, $crate::utils::Physical>],
                color: [f32; 4],
            ) -> Result<(), Self::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_frame draw_solid; x, dst, damage, color)
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn render_texture_from_to(
                &mut self,
                texture: &Self::TextureId,
                src: $crate::utils::Rectangle<f64, $crate::utils::Buffer>,
                dst: $crate::utils::Rectangle<i32, $crate::utils::Physical>,
                damage: &[$crate::utils::Rectangle<i32, $crate::utils::Physical>],
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
                            $crate::auto_renderer_internal!(@call_frame render_texture_from_to; x, texture, src, dst, damage, src_transform, alpha)
                                .map_err(|err| $error::$body(err))
                        }
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn transformation(&self) -> $crate::utils::Transform {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_frame transformation; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn finish(self) -> Result<$crate::backend::renderer::sync::SyncPoint, Self::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_frame finish; x)
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
    };
    (@frame $(#[$frame_attr:meta])* $vis:vis $frame:ident<$frame_lifetime:lifetime $(,$additional_frame_lifetime:lifetime)* $(,$frame_custom:ident)*> $error:ident$(<$($error_lifetime:lifetime),*, $($error_custom:ident),*>)? $texture_id:ident$(<$($texture_id_lifetime:lifetime),*, $($texture_id_custom:ident),*>)? $(where $($frame_target:ty: $frame_bound:tt $(+ $frame_additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$frame_attr])*
        $vis enum $frame<$frame_lifetime $(,$additional_frame_lifetime)* $(,$frame_custom)*>
        $(where $($frame_target: $frame_bound $(+ $frame_additional_bound)*),+)? {
            $(
                $(
                    #[$meta]
                )*
                $body($other_frame)
            ),*,
            #[doc(hidden)]
            _GenericCatcher(std::convert::Infallible),
        }

        impl<$frame_lifetime $(,$additional_frame_lifetime)* $(,$frame_custom)*> $crate::backend::renderer::Frame for $frame<$frame_lifetime $(,$additional_frame_lifetime)* $(,$frame_custom)*>
        $(where $($frame_target: $frame_bound $(+ $frame_additional_bound)*),+)? {
            type Error = $error$(<$($error_lifetime),*, $($error_custom),*>)?;
            type TextureId = $texture_id$(<$($texture_id_lifetime),*, $($texture_id_custom),*>)?;

            $crate::auto_renderer_internal!(@frame_impl $error $texture_id; $($(#[$meta])* $body),*);
        }
    };
    (@frame $(#[$frame_attr:meta])* $vis:vis $frame:ident<$frame_lifetime:lifetime $(,$additional_frame_lifetime:lifetime)*> $error:ident$(<$($error_lifetime:lifetime),*>)? $texture_id:ident$(<$($texture_id_lifetime:lifetime),*>)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$frame_attr])*
        $vis enum $frame<$frame_lifetime $(,$additional_frame_lifetime)*> {
            $(
                $(
                    #[$meta]
                )*
                $body($other_frame)
            ),*,
            #[doc(hidden)]
            _GenericCatcher(std::convert::Infallible),
        }

        impl<$frame_lifetime $(,$additional_frame_lifetime)*> $crate::backend::renderer::Frame for $frame<$frame_lifetime $(,$additional_frame_lifetime)*> {
            type Error = $error$(<$($error_lifetime),*>)?;
            type TextureId = $texture_id$(<$($texture_id_lifetime),*>)?;

            $crate::auto_renderer_internal!(@frame_impl $error $texture_id; $($(#[$meta])* $body),*);
        }
    };
    (@frame $(#[$frame_attr:meta])* $vis:vis $frame:ident<$frame_lifetime:lifetime $(,$frame_custom:ident)*> $error:ident$(<$($error_custom:ident),*>)? $texture_id:ident$(<$($texture_id_custom:ident),*>)? $(where $($frame_target:ty: $frame_bound:tt $(+ $frame_additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(#[$frame_attr])*
        $vis enum $frame<$frame_lifetime $(,$frame_custom)*>
        $(where $($frame_target: $frame_bound $(+ $frame_additional_bound)*),+)? {
            $(
                $(
                    #[$meta]
                )*
                $body($other_frame)
            ),*,
            #[doc(hidden)]
            _GenericCatcher(std::convert::Infallible),
        }

        impl<$frame_lifetime $(,$frame_custom)*> $crate::backend::renderer::Frame for $frame<$frame_lifetime $(,$frame_custom)*>
        $(where $($frame_target: $frame_bound $(+ $frame_additional_bound)*),+)? {
            type Error = $error$(<$($error_custom),*>)?;
            type TextureId = $texture_id$(<$($texture_id_custom),*>)?;

            $crate::auto_renderer_internal!(@frame_impl $error $texture_id; $($(#[$meta])* $body),*);
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

        impl<$frame_lifetime> $crate::backend::renderer::Frame for $frame<$frame_lifetime> {
            type Error = $error;
            type TextureId = $texture_id;

            $crate::auto_renderer_internal!(@frame_impl $error $texture_id; $($(#[$meta])* $body),*);
        }
    };

    /* Renderer */
    (@call_renderer $name:ident; $($x:ident),*) => {
        $crate::backend::renderer::Renderer::$name($($x),*)
    };
    (@renderer_impl $frame:ident $error:ident $texture_id:ident; $($(#[$meta:meta])* $body:ident),*) => {
            fn id(&self) -> usize {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_renderer id; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn downscale_filter(
                &mut self,
                filter: $crate::backend::renderer::TextureFilter,
            ) -> Result<(), Self::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_renderer downscale_filter; x, filter)
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn upscale_filter(
                &mut self,
                filter: $crate::backend::renderer::TextureFilter,
            ) -> Result<(), Self::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_renderer upscale_filter; x, filter)
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn set_debug_flags(&mut self, flags: $crate::backend::renderer::DebugFlags) {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_renderer set_debug_flags; x, flags)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn debug_flags(&self) -> $crate::backend::renderer::DebugFlags {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_renderer debug_flags; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn render(
                &mut self,
                output_size: $crate::utils::Size<i32, $crate::utils::Physical>,
                dst_transform: Transform,
            ) -> Result<Self::Frame<'_>, Self::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_renderer render; x, output_size, dst_transform)
                            .map(|frame| $frame::$body(frame))
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
    };
    (@renderer $vis:vis $name:ident<$($lt:lifetime),+, $($custom:ident),+> $error:ident$(<$($error_lifetime:lifetime),*, $($error_custom:ident),*>)? $texture_id:ident$(<$($texture_id_lifetime:lifetime),*, $($texture_id_custom:ident),*>)? $frame:ident<$frame_lifetime:lifetime $(,$additional_frame_lifetime:lifetime)* $(,$frame_custom:ident)*> $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($lt),+, $($custom),+> $crate::backend::renderer::Renderer for $name<$($lt),+, $($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            type Error = $error$(<$($error_lifetime),*, $($error_custom),*>)?;
            type TextureId = $texture_id$(<$($texture_id_lifetime),*, $($texture_id_custom),*>)?;
            type Frame<$frame_lifetime> = $frame<$frame_lifetime $(,$additional_frame_lifetime)* $(,$frame_custom)*> where Self: $frame_lifetime;

            $crate::auto_renderer_internal!(@renderer_impl $frame $error $texture_id; $($(#[$meta])* $body),*);
        }
    };
    (@renderer $vis:vis $name:ident<$($lt:lifetime),+> $error:ident$(<$($error_lifetime:lifetime),*>)? $texture_id:ident$(<$($texture_id_lifetime:lifetime),*>)? $frame:ident<$frame_lifetime:lifetime $(,$additional_frame_lifetime:lifetime)*>; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($lt),*> $crate::backend::renderer::Renderer for $name<$($lt),+> {
            type Error = $error$(<$($error_lifetime),*>)?;
            type TextureId = $texture_id$(<$($texture_id_lifetime),*>)?;
            type Frame<$frame_lifetime> = $frame<$frame_lifetime $(,$additional_frame_lifetime)*> where Self: $frame_lifetime;

            $crate::auto_renderer_internal!(@renderer_impl $frame $error $texture_id; $($(#[$meta])* $body),*);
        }
    };
    (@renderer $vis:vis $name:ident<$($custom:ident),+> $error:ident$(<$($error_custom:ident),*>)? $texture_id:ident$(<$($texture_id_custom:ident),*>)? $frame:ident<$frame_lifetime:lifetime $(,$frame_custom:ident)*> $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($custom),+> $crate::backend::renderer::Renderer for $name<$($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            type Error = $error$(<$($error_custom),*>)?;
            type TextureId = $texture_id$(<$($texture_id_custom),*>)?;
            type Frame<$frame_lifetime> = $frame<$frame_lifetime $(,$frame_custom)*> where Self: $frame_lifetime;

            $crate::auto_renderer_internal!(@renderer_impl $frame $error $texture_id; $($(#[$meta])* $body),*);
        }
    };
    (@renderer $vis:vis $name:ident $error:ident $texture_id:ident $frame:ident<$frame_lifetime:lifetime>; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl $crate::backend::renderer::Renderer for $name {
            type Error = $error;
            type TextureId = $texture_id;
            type Frame<$frame_lifetime> = $frame<$frame_lifetime> where Self: $frame_lifetime;

            $crate::auto_renderer_internal!(@renderer_impl $frame $error $texture_id; $($(#[$meta])* $body),*);
        }
    };

    /* Imports */
    /* Import - Egl */
    (@call_import_egl $name:ident; $($x:ident),*) => {
        $crate::backend::renderer::ImportEgl::$name($($x),*)
    };
    (@import_egl_impl $error:ident $texture_id:ident; $($(#[$meta:meta])* $body:ident),*) => {
            fn bind_wl_display(
                &mut self,
                display: &$crate::reexports::wayland_server::DisplayHandle,
            ) -> Result<(), $crate::backend::egl::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_import_egl bind_wl_display; x, display)
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
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_import_egl unbind_wl_display; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn egl_reader(&self) -> Option<&$crate::backend::egl::display::EGLBufferReader> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_import_egl egl_reader; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn import_egl_buffer(
                &mut self,
                buffer: &$crate::reexports::wayland_server::protocol::wl_buffer::WlBuffer,
                surface: Option<&$crate::wayland::compositor::SurfaceData>,
                damage: &[$crate::utils::Rectangle<i32, $crate::utils::Buffer>],
            ) -> Result<<Self as Renderer>::TextureId, <Self as Renderer>::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_import_egl import_egl_buffer; x, buffer, surface, damage)
                            .map(|texture| $texture_id::$body(texture))
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
    };
    (@import $name:ident<$($lt:lifetime),+, $($custom:ident),+> $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportEgl $($(#[$other_import_attr:meta])* $other_import:ident)*; $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($lt),+, $($custom),+> $crate::backend::renderer::ImportEgl for $name<$($lt),+, $($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@import_egl_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name<$($lt),+, $($custom),+> $error $texture_id $($(#[$other_import_attr])* $other_import)*; $(where $($target: $bound $(+ $additional_bound)*),+)?; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident<$($lt:lifetime),+> $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportEgl $($(#[$other_import_attr:meta])* $other_import:ident)*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($lt),+> $crate::backend::renderer::ImportEgl for $name<$($lt),+> {
            $crate::auto_renderer_internal!(@import_egl_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name<$($lt),+> $error $texture_id $($(#[$other_import_attr])* $other_import)*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident<$($custom:ident),+> $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportEgl $($(#[$other_import_attr:meta])* $other_import:ident)*; $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($custom),+> $crate::backend::renderer::ImportEgl for $name<$($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@import_egl_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name<$($custom),+> $error $texture_id $($(#[$other_import_attr])* $other_import)*; $(where $($target: $bound $(+ $additional_bound)*),+)?; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportEgl $($(#[$other_import_attr:meta])* $other_import:ident)*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl $crate::backend::renderer::ImportEgl for $name {
            $crate::auto_renderer_internal!(@import_egl_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name $error $texture_id $($(#[$other_import_attr])* $other_import)*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };

    /* Import - Dma */
    (@call_import_dma $name:ident; $($x:ident),*) => {
        $crate::backend::renderer::ImportDma::$name($($x),*)
    };
    (@import_dma_impl $error:ident $texture_id:ident; $($(#[$meta:meta])* $body:ident),*) => {
            fn import_dmabuf(
                &mut self,
                dmabuf: &Dmabuf,
                damage: Option<&[$crate::utils::Rectangle<i32, $crate::utils::Buffer>]>,
            ) -> Result<<Self as Renderer>::TextureId, <Self as Renderer>::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_import_dma import_dmabuf; x, dmabuf, damage)
                            .map(|texture| $texture_id::$body(texture))
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn dmabuf_formats(&self) -> Box<dyn Iterator<Item = $crate::backend::allocator::Format>> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_import_dma dmabuf_formats; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn has_dmabuf_format(&self, format: $crate::backend::allocator::Format) -> bool {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_import_dma has_dmabuf_format; x, format)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
    };
    (@import $name:ident<$($lt:lifetime),+, $($custom:ident),+> $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportDma $($(#[$other_import_attr:meta])* $other_import:ident)*; $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($lt),+, $($custom),+> $crate::backend::renderer::ImportDma for $name<$($lt),+, $($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@import_dma_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name<$($lt),+, $($custom),+> $error $texture_id $($(#[$other_import_attr])* $other_import)*; $(where $($target: $bound $(+ $additional_bound)*),+)?; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident<$($lt:lifetime),+> $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportDma $($(#[$other_import_attr:meta])* $other_import:ident)*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($lt),+> $crate::backend::renderer::ImportDma for $name<$($lt),+> {
            $crate::auto_renderer_internal!(@import_dma_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name<$($lt),+> $error $texture_id $($(#[$other_import_attr])* $other_import)*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident<$($custom:ident),+> $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportDma $($(#[$other_import_attr:meta])* $other_import:ident)*; $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($custom),+> $crate::backend::renderer::ImportDma for $name<$($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@import_dma_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name<$($custom),+> $error $texture_id $($(#[$other_import_attr])* $other_import)*; $(where $($target: $bound $(+ $additional_bound)*),+)?; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportDma $($(#[$other_import_attr:meta])* $other_import:ident)*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl $crate::backend::renderer::ImportDma for $name {
            $crate::auto_renderer_internal!(@import_dma_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name $error $texture_id $($(#[$other_import_attr])* $other_import)*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };

    /* Import - DmaWl */
    (@call_import_dma_wl $name:ident; $($x:ident),*) => {
        $crate::backend::renderer::ImportDmaWl::$name($($x),*)
    };
    (@import_dma_wl_impl $error:ident $texture_id:ident; $($(#[$meta:meta])* $body:ident),*) => {
            fn import_dma_buffer(
                &mut self,
                buffer: &$crate::reexports::wayland_server::protocol::wl_buffer::WlBuffer,
                surface: Option<&$crate::wayland::compositor::SurfaceData>,
                damage: &[$crate::utils::Rectangle<i32, $crate::utils::Buffer>],
            ) -> Result<<Self as Renderer>::TextureId, <Self as Renderer>::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_import_dma_wl import_dma_buffer; x, buffer, surface, damage)
                            .map(|texture| $texture_id::$body(texture))
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
    };
    (@import $name:ident<$($lt:lifetime),+, $($custom:ident),+> $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportDmaWl $($(#[$other_import_attr:meta])* $other_import:ident)*; $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($lt),+, $($custom),+> $crate::backend::renderer::ImportDmaWl for $name<$($lt),+, $($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@import_dma_wl_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name<$($lt),+, $($custom),+> $error $texture_id $($(#[$other_import_attr])* $other_import)*; $(where $($target: $bound $(+ $additional_bound)*),+)?; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident<$($lt:lifetime),+> $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportDmaWl $($(#[$other_import_attr:meta])* $other_import:ident)*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($lt),+> $crate::backend::renderer::ImportDmaWl for $name<$($lt),+> {
            $crate::auto_renderer_internal!(@import_dma_wl_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name<$($lt),+> $error $texture_id $($(#[$other_import_attr])* $other_import)*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident<$($custom:ident),+> $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportDmaWl $($(#[$other_import_attr:meta])* $other_import:ident)*; $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($custom),+> $crate::backend::renderer::ImportDmaWl for $name<$($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@import_dma_wl_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name<$($custom),+> $error $texture_id $($(#[$other_import_attr])* $other_import)*; $(where $($target: $bound $(+ $additional_bound)*),+)?; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportDmaWl $($(#[$other_import_attr:meta])* $other_import:ident)*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl $crate::backend::renderer::ImportDmaWl for $name {
            $crate::auto_renderer_internal!(@import_dma_wl_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name $error $texture_id $($(#[$other_import_attr])* $other_import)*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };

    /* Import - Mem */
    (@call_import_mem $name:ident; $($x:ident),*) => {
        $crate::backend::renderer::ImportMem::$name($($x),*)
    };
    (@import_mem_impl $error:ident $texture_id:ident; $($(#[$meta:meta])* $body:ident),*) => {
            fn import_memory(
                &mut self,
                data: &[u8],
                format: gbm::Format,
                size: $crate::utils::Size<i32, $crate::utils::Buffer>,
                flipped: bool,
            ) -> Result<<Self as Renderer>::TextureId, <Self as Renderer>::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_import_mem import_memory; x, data, format, size, flipped)
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
                region: $crate::utils::Rectangle<i32, $crate::utils::Buffer>,
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
                            $crate::auto_renderer_internal!(@call_import_mem update_memory; x, texture, data, region)
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
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_import_mem mem_formats; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
    };
    (@import $name:ident<$($lt:lifetime),+, $($custom:ident),+> $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportMem $($(#[$other_import_attr:meta])* $other_import:ident)*; $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($lt),*, $($custom),*> $crate::backend::renderer::ImportMem for $name<$($lt),*, $($custom),*>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@import_mem_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name<$($lt),+, $($custom),+> $error $texture_id $($(#[$other_import_attr])* $other_import)*; $(where $($target: $bound $(+ $additional_bound)*),+)?; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident<$($lt:lifetime),+> $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportMem $($(#[$other_import_attr:meta])* $other_import:ident)*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($lt),*> $crate::backend::renderer::ImportMem for $name<$($lt),*> {
            $crate::auto_renderer_internal!(@import_mem_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name<$($lt),+> $error $texture_id $($(#[$other_import_attr])* $other_import)*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident<$($custom:ident),+> $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportMem $($(#[$other_import_attr:meta])* $other_import:ident)*; $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($custom),*> $crate::backend::renderer::ImportMem for $name<$($custom),*>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@import_mem_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name<$($custom),+> $error $texture_id $($(#[$other_import_attr])* $other_import)*; $(where $($target: $bound $(+ $additional_bound)*),+)?; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportMem $($(#[$other_import_attr:meta])* $other_import:ident)*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl $crate::backend::renderer::ImportMem for $name {
            $crate::auto_renderer_internal!(@import_mem_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name $error $texture_id $($(#[$other_import_attr])* $other_import)*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };

    /* Import - MemWl */
    (@call_import_mem_wl $name:ident; $($x:ident),*) => {
        $crate::backend::renderer::ImportMemWl::$name($($x),*)
    };
    (@import_mem_wl_impl $error:ident $texture_id:ident; $($(#[$meta:meta])* $body:ident),*) => {
            fn import_shm_buffer(
                &mut self,
                buffer: &$crate::reexports::wayland_server::protocol::wl_buffer::WlBuffer,
                surface: Option<&$crate::wayland::compositor::SurfaceData>,
                damage: &[$crate::utils::Rectangle<i32, $crate::utils::Buffer>],
            ) -> Result<<Self as Renderer>::TextureId, <Self as Renderer>::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_import_mem_wl import_shm_buffer; x, buffer, surface, damage)
                            .map(|texture| $texture_id::$body(texture))
                            .map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn shm_formats(
                &self,
            ) -> Box<dyn Iterator<Item = $crate::reexports::wayland_server::protocol::wl_shm::Format>> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::auto_renderer_internal!(@call_import_mem_wl shm_formats; x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
    };
    (@import $name:ident<$($lt:lifetime),+, $($custom:ident),+> $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportMemWl $($(#[$other_import_attr:meta])* $other_import:ident)*; $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($lt),+, $($custom),+> $crate::backend::renderer::ImportMemWl for $name<$($lt),+, $($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@import_mem_wl_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name<$($lt),+, $($custom),+> $error $texture_id $($(#[$other_import_attr])* $other_import)*; $(where $($target: $bound $(+ $additional_bound)*),+)?; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident<$($lt:lifetime),+> $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportMemWl $($(#[$other_import_attr:meta])* $other_import:ident)*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($lt),+> $crate::backend::renderer::ImportMemWl for $name<$($lt),+> {
            $crate::auto_renderer_internal!(@import_mem_wl_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name<$($lt),+> $error $texture_id $($(#[$other_import_attr])* $other_import)*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident<$($custom:ident),+> $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportMemWl $($(#[$other_import_attr:meta])* $other_import:ident)*; $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($custom),+> $crate::backend::renderer::ImportMemWl for $name<$($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@import_mem_wl_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name<$($custom),+> $error $texture_id $($(#[$other_import_attr])* $other_import)*; $(where $($target: $bound $(+ $additional_bound)*),+)?; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@import $name:ident $error:ident $texture_id:ident $(#[$import_attr:meta])* ImportMemWl $($(#[$other_import_attr:meta])* $other_import:ident)*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl $crate::backend::renderer::ImportMemWl for $name {
            $crate::auto_renderer_internal!(@import_mem_wl_impl $error $texture_id; $($(#[$meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@import $name $error $texture_id $($(#[$other_import_attr])* $other_import)*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };

    /* Import - Tail */
    (@import $name:ident<$($lt:lifetime),+, $($custom:ident),+> $error:ident $texture_id:ident; $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {};
    (@import $name:ident<$($lt:lifetime),+> $error:ident $texture_id:ident; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {};
    (@import $name:ident<$($custom:ident),+> $error:ident $texture_id:ident; $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {};
    (@import $name:ident $error:ident $texture_id:ident; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {};
    
    /* Bind/Unbind */
    (@bind_impl $error:ident $bind:ty; $($(#[$meta:meta])* $body:ident),*) => {
            fn bind(&mut self, target: $bind) -> Result<(), <Self as Renderer>::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::backend::renderer::Bind::<$bind>::bind(x, target).map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }

            fn supported_formats(&self) -> Option<std::collections::HashSet<$crate::backend::allocator::Format>> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::backend::renderer::Bind::<$bind>::supported_formats(x)
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
    };
    (@bind $name:ident<$($lt:lifetime),+, $($custom:ident),+> $error:ident $texture_id:ident $(#[$bind_attr:meta])* $bind:ty, $($(#[$other_bind_attr:meta])* $other_bind:ty,),* $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(
            #[$bind_attr]
        )*
        impl<$($lt),+, $($custom),+> $crate::backend::renderer::Bind<$bind> for $name<$($lt),+, $($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@bind_impl $error $bind; $($(#[$meta:meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@bind $name<$($lt),+, $($custom),+> $error $texture_id $($(#[$other_bind_attr])* $other_bind,),* $(where $($target: $bound $(+ $additional_bound)*),+)?; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@bind $name:ident<$($lt:lifetime),+> $error:ident $texture_id:ident $(#[$bind_attr:meta])* $bind:ty, $($(#[$other_bind_attr:meta])* $other_bind:ty,),*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(
            #[$bind_attr]
        )*
        impl<$($lt),+> $crate::backend::renderer::Bind<$bind> for $name<$($lt),+> {
            $crate::auto_renderer_internal!(@bind_impl $error $bind; $($(#[$meta:meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@bind $name<$($lt),+> $error $texture_id $($(#[$other_bind_attr])* $other_bind,),*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@bind $name:ident<$($custom:ident),+> $error:ident $texture_id:ident $(#[$bind_attr:meta])* $bind:ty, $($(#[$other_bind_attr:meta])* $other_bind:ty,),* $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(
            #[$bind_attr]
        )*
        impl<$($custom),+> $crate::backend::renderer::Bind<$bind> for $name<$($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@bind_impl $error $bind; $($(#[$meta:meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@bind $name<$($custom),+> $error $texture_id $($(#[$other_bind_attr])* $other_bind,),* $(where $($target: $bound $(+ $additional_bound)*),+)?; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@bind $name:ident $error:ident $texture_id:ident $(#[$bind_attr:meta])* $bind:ty, $($(#[$other_bind_attr:meta])* $other_bind:ty,),*; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        $(
            #[$bind_attr]
        )*
        impl $crate::backend::renderer::Bind<$bind> for $name {
            $crate::auto_renderer_internal!(@bind_impl $error $bind; $($(#[$meta:meta])* $body),*);
        }

        $crate::auto_renderer_internal!(@bind $name $error $texture_id $($(#[$other_bind_attr])* $other_bind,),*; $($(#[$meta])* $body ($field { type Error = $other_error; type TextureId = $other_texture_id; type Frame = $other_frame; })),*);
    };
    (@unbind_impl $error:ident; $($(#[$meta:meta])* $body:ident),*) => {
            fn unbind(&mut self) -> Result<(), <Self as Renderer>::Error> {
                match self {
                    $(
                        #[allow(unused_doc_comments)]
                        $(
                            #[$meta]
                        )*
                        Self::$body(x) => $crate::backend::renderer::Unbind::unbind(x).map_err(|err| $error::$body(err))
                    ),*,
                    Self::_GenericCatcher(_) => unreachable!(),
                }
            }
    };
    (@bind $name:ident<$($lt:lifetime),+, $($custom:ident),+> $error:ident $texture_id:ident $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($lt),+, $($custom),+> $crate::backend::renderer::Unbind for $name<$($lt),+, $($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@unbind_impl $error; $($(#[$meta])* $body),*);
        }
    };
    (@bind $name:ident<$($lt:lifetime),+> $error:ident $texture_id:ident; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($lt),+> $crate::backend::renderer::Unbind for $name<$($lt),+> {
            $crate::auto_renderer_internal!(@unbind_impl $error; $($(#[$meta])* $body),*);
        }
    };
    (@bind $name:ident<$($custom:ident),+> $error:ident $texture_id:ident $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)?; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl<$($custom),+> $crate::backend::renderer::Unbind for $name<$($custom),+>
        $(where $($target: $bound $(+ $additional_bound)*),+)? {
            $crate::auto_renderer_internal!(@unbind_impl $error; $($(#[$meta])* $body),*);
        }
    };
    (@bind $name:ident $error:ident $texture_id:ident; $($(#[$meta:meta])* $body:ident ($field:ty { type Error = $other_error:ty; type TextureId = $other_texture_id:ty; type Frame = $other_frame:ty; })),* $(,)?) => {
        impl $crate::backend::renderer::Unbind for $name {
            $crate::auto_renderer_internal!(@unbind_impl $error; $($(#[$meta])* $body),*);
        }
    };
}

/// TODO: Docs
#[macro_export]
macro_rules! auto_renderer {
    ($(#[$attr:meta])* $vis:vis $name:ident<$($lt:lifetime),+, $($custom:ident),+> $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)? {
            $(#[$error_attr:meta])* type Error = $error:ident$(<$($error_lifetime:lifetime),*, $($error_custom:ident),*>)? $(where $($error_target:ty: $error_bound:tt $(+ $error_additional_bound:tt)*),+)?;
            $(#[$texture_id_attr:meta])* type TextureId = $texture_id:ident$(<$($texture_id_lifetime:lifetime),*, $($texture_id_custom:ident),*>)? $(where $($texture_id_target:ty: $texture_id_bound:tt $(+ $texture_id_additional_bound:tt)*),+)?;
            $(#[$frame_attr:meta])* type Frame = $frame:ident<$frame_lifetime:lifetime $(,$additional_frame_lifetime:lifetime)* $(,$frame_custom:ident)*> $(where $($frame_target:ty: $frame_bound:tt $(+ $frame_additional_bound:tt)*),+)?;
        };
        Imports=[$($(#[$import_attr:meta])* $import:ident,)*];
        Binds=[$($(#[$bind_attr:meta])* $bind:ty,)*];
        $($tail:tt)*) => {
        $crate::auto_renderer_internal!(@enum $(#[$attr])* $vis $name<$($lt),+, $($custom),+> $(where $($target: $bound $(+ $additional_bound)*),+)?; $($tail)*);
        $crate::auto_renderer_internal!(@error $(#[$error_attr])* $vis $error$(<$($error_lifetime),*, $($error_custom),*>)? $(where $($error_target: $error_bound $(+ $error_additional_bound)*),+)?; $($tail)*);
        $crate::auto_renderer_internal!(@texture_id $(#[$texture_id_attr])* $vis $error $texture_id$(<$($texture_id_lifetime),*, $($texture_id_custom),*>)? $(where $($texture_id_target: $texture_id_bound $(+ $texture_id_additional_bound)*),+)?; $($tail)*);
        $crate::auto_renderer_internal!(@frame $(#[$frame_attr])* $vis $frame<$frame_lifetime $(,$additional_frame_lifetime)* $(,$frame_custom)*> $error$(<$($error_lifetime),*, $($error_custom),*>)? $texture_id$(<$($texture_id_lifetime),*, $($texture_id_custom),*>)? $(where $($frame_target: $frame_bound $(+ $frame_additional_bound)*),+)?; $($tail)*);
        $crate::auto_renderer_internal!(@renderer $vis $name<$($lt),+, $($custom),+> $error$(<$($error_lifetime),*, $($error_custom),*>)? $texture_id$(<$($texture_id_lifetime),*, $($texture_id_custom),*>)? $frame<$frame_lifetime $(,$additional_frame_lifetime)* $(,$frame_custom)*> $(where $($target: $bound $(+ $additional_bound)*),+)?; $($tail)*);
        $crate::auto_renderer_internal!(@import $name<$($lt),+, $($custom),+> $error $texture_id $($(#[$import_attr])* $import)*; $(where $($target: $bound $(+ $additional_bound)*),+)?; $($tail)*);
        $crate::auto_renderer_internal!(@bind $name<$($lt),+, $($custom),+> $error $texture_id $($(#[$bind_attr])* $bind,)* $(where $($target: $bound $(+ $additional_bound)*),+)?; $($tail)*);
    };
    ($(#[$attr:meta])* $vis:vis $name:ident<$($lt:lifetime),+> {
            $(#[$error_attr:meta])* type Error = $error:ident$(<$($error_lifetime:lifetime),*>)?;
            $(#[$texture_id_attr:meta])* type TextureId = $texture_id:ident$(<$($texture_id_lifetime:lifetime),*>)?;
            $(#[$frame_attr:meta])* type Frame = $frame:ident<$frame_lifetime:lifetime $(,$additional_frame_lifetime:lifetime)*>;
        };
        Imports=[$($(#[$import_attr:meta])* $import:ident,)*];
        Binds=[$($(#[$bind_attr:meta])* $bind:ty,)*];
        $($tail:tt)*) => {
        $crate::auto_renderer_internal!(@enum $(#[$attr])* $vis $name<$($lt),+>; $($tail)*);
        $crate::auto_renderer_internal!(@error $(#[$error_attr])* $vis $error$(<$($error_lifetime),*>)?; $($tail)*);
        $crate::auto_renderer_internal!(@texture_id $(#[$texture_id_attr])* $vis $error $texture_id$(<$($texture_id_lifetime),*>)?; $($tail)*);
        $crate::auto_renderer_internal!(@frame $(#[$frame_attr])* $vis $frame<$frame_lifetime$ (,$additional_frame_lifetime)*> $error$(<$($error_lifetime),*>)? $texture_id$(<$($texture_id_lifetime),*>)?; $($tail)*);
        $crate::auto_renderer_internal!(@renderer $vis $name<$($lt),+> $error$(<$($error_lifetime),*>)? $texture_id$(<$($texture_id_lifetime),*>)? $frame<$frame_lifetime $(,$additional_frame_lifetime)*>; $($tail)*);
        $crate::auto_renderer_internal!(@import $name<$($lt),+> $error $texture_id $($(#[$import_attr])* $import)*; $($tail)*);
        $crate::auto_renderer_internal!(@bind $name<$($lt),+> $error $texture_id $($(#[$bind_attr])* $bind,)*; $($tail)*);
    };
    ($(#[$attr:meta])* $vis:vis $name:ident<$($custom:ident),+> $(where $($target:ty: $bound:tt $(+ $additional_bound:tt)*),+)? {
            $(#[$error_attr:meta])* type Error = $error:ident$(<$($error_custom:ident),*>)? $(where $($error_target:ty: $error_bound:tt $(+ $error_additional_bound:tt)*),+)?;
            $(#[$texture_id_attr:meta])* type TextureId = $texture_id:ident$(<$($texture_id_custom:ident),*>)? $(where $($texture_id_target:ty: $texture_id_bound:tt $(+ $texture_id_additional_bound:tt)*),+)?;
            $(#[$frame_attr:meta])* type Frame = $frame:ident<$frame_lifetime:lifetime $(,$frame_custom:ident)*> $(where $($frame_target:ty: $frame_bound:tt $(+ $frame_additional_bound:tt)*),+)?;
        };
        Imports=[$($(#[$import_attr:meta])* $import:ident,)*];
        Binds=[$($(#[$bind_attr:meta])* $bind:ty,)*];
        $($tail:tt)*) => {
        $crate::auto_renderer_internal!(@enum $(#[$attr])* $vis $name<$($custom),+> $(where $($target: $bound $(+ $additional_bound)*),+)?; $($tail)*);
        $crate::auto_renderer_internal!(@error $(#[$error_attr])* $vis $error$(<$($error_custom),*>)? $(where $($error_target: $error_bound $(+ $error_additional_bound)*),+)?; $($tail)*);
        $crate::auto_renderer_internal!(@texture_id $(#[$texture_id_attr])* $vis $error $texture_id$(<$($texture_id_custom),*>)? $(where $($texture_id_target: $texture_id_bound $(+ $texture_id_additional_bound)*),+)?; $($tail)*);
        $crate::auto_renderer_internal!(@frame $(#[$frame_attr])* $vis $frame<$frame_lifetime $(,$frame_custom)*> $error$(<$($error_custom),*>)? $texture_id$(<$($texture_id_custom),*>)? $(where $($frame_target: $frame_bound $(+ $frame_additional_bound)*),+)?; $($tail)*);
        $crate::auto_renderer_internal!(@renderer $vis $name<$($custom),+> $error$(<$($error_custom),*>)? $texture_id$(<$($texture_id_custom),*>)? $frame<$frame_lifetime $(,$frame_custom)*> $(where $($target: $bound $(+ $additional_bound)*),+)?; $($tail)*);
        $crate::auto_renderer_internal!(@import $name<$($custom),+> $error $texture_id $($(#[$import_attr])* $import)*; $(where $($target: $bound $(+ $additional_bound)*),+)?; $($tail)*);
        $crate::auto_renderer_internal!(@bind $name<$($custom),+> $error $texture_id $($(#[$bind_attr])* $bind,)* $(where $($target: $bound $(+ $additional_bound)*),+)?; $($tail)*);
    };
    ($(#[$attr:meta])* $vis:vis $name:ident {
            $(#[$error_attr:meta])* type Error = $error:ident;
            $(#[$texture_id_attr:meta])* type TextureId = $texture_id:ident;
            $(#[$frame_attr:meta])* type Frame = $frame:ident<$frame_lifetime:lifetime $(,$additional_frame_lifetime:lifetime)*>;
        };
        Imports=[$($(#[$import_attr:meta])* $import:ident,)*];
        Binds=[$($(#[$bind_attr:meta])* $bind:ty,)*];
        $($tail:tt)*) => {
        $crate::auto_renderer_internal!(@enum $(#[$attr])* $vis $name; $($tail)*);
        $crate::auto_renderer_internal!(@error $(#[$error_attr])* $vis $error; $($tail)*);
        $crate::auto_renderer_internal!(@texture_id $(#[$texture_id_attr])* $vis $error $texture_id; $($tail)*);
        $crate::auto_renderer_internal!(@frame $(#[$frame_attr])* $vis $frame<$frame_lifetime $(,$additional_frame_lifetime)*> $error $texture_id; $($tail)*);
        $crate::auto_renderer_internal!(@renderer $vis $name $error $texture_id $frame<$frame_lifetime $(,$additional_frame_lifetime)*>; $($tail)*);
        $crate::auto_renderer_internal!(@import $name $error $texture_id $($(#[$import_attr])* $import)*; $($tail)*);
        $crate::auto_renderer_internal!(@bind $name $error $texture_id $($(#[$bind_attr])* $bind,)*; $($tail)*);
    };
}

pub use auto_renderer;