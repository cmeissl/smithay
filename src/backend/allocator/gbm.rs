//! Module for Buffers created using [libgbm](gbm).
//!
//! The re-exported [`GbmDevice`](gbm::Device) implements the [`Allocator`] trait
//! and [`GbmBuffer`](gbm::BufferObject) satisfies the [`Buffer`] trait while also allowing
//! conversions to and from [dmabufs](super::dmabuf).

use super::{
    dmabuf::{AsDmabuf, Dmabuf, DmabufFlags, MAX_PLANES},
    Allocator, Buffer, Format, Fourcc, Modifier,
};
#[cfg(feature = "backend_drm")]
use crate::backend::drm::DrmNode;
use crate::utils::{Buffer as BufferCoords, Size};
#[cfg(feature = "backend_drm")]
use drm::buffer::PlanarBuffer;
use gbm::BufferObject;
pub use gbm::{BufferObjectFlags as GbmBufferFlags, Device as GbmDevice};
use std::os::unix::io::{AsFd, BorrowedFd};
use tracing::instrument;

/// A GBM buffer object
#[derive(Debug)]
pub struct GbmBuffer {
    bo: BufferObject<()>,
    size: Size<i32, BufferCoords>,
    format: Format,
}

#[cfg(feature = "backend_drm")]
impl PlanarBuffer for GbmBuffer {
    #[inline]
    fn size(&self) -> (u32, u32) {
        (self.size.w as u32, self.size.h as u32)
    }

    #[inline]
    fn format(&self) -> Fourcc {
        self.format.code
    }

    #[inline]
    fn modifier(&self) -> Option<Modifier> {
        match self.format.modifier {
            Modifier::Invalid => None,
            x => Some(x),
        }
    }

    #[inline]
    fn pitches(&self) -> [u32; 4] {
        self.bo.pitches()
    }

    #[inline]
    fn handles(&self) -> [Option<drm::buffer::Handle>; 4] {
        self.bo.handles()
    }

    #[inline]
    fn offsets(&self) -> [u32; 4] {
        self.bo.offsets()
    }
}

#[cfg(feature = "backend_drm")]
impl drm::buffer::Buffer for GbmBuffer {
    #[inline]
    fn size(&self) -> (u32, u32) {
        (self.size.w as u32, self.size.h as u32)
    }

    #[inline]
    fn format(&self) -> Fourcc {
        self.format.code
    }

    #[inline]
    fn pitch(&self) -> u32 {
        self.bo.pitch()
    }

    #[inline]
    fn handle(&self) -> drm::buffer::Handle {
        drm::buffer::Buffer::handle(&self.bo)
    }
}

impl GbmBuffer {
    /// Create a [`GbmBuffer`] from an existing [`BufferObject`]
    ///
    /// `implicit` forces the object to assume the modifier is `Invalid` for cases,
    /// where the buffer was allocated with an older api, that doesn't support modifiers.
    ///
    /// Gbm might otherwise give us the underlying or a non-sensical modifier,
    /// which can fail in various other apis.
    pub fn from_bo(bo: BufferObject<()>, implicit: bool) -> Self {
        let size = (bo.width().unwrap_or(0) as i32, bo.height().unwrap_or(0) as i32).into();
        let format = Format {
            code: bo.format().unwrap_or(Fourcc::Argb8888), // we got to return something, but this should never happen anyway
            modifier: if implicit {
                Modifier::Invalid
            } else {
                bo.modifier().unwrap_or(Modifier::Invalid)
            },
        };
        Self { bo, size, format }
    }
}

impl std::ops::Deref for GbmBuffer {
    type Target = BufferObject<()>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.bo
    }
}

impl std::ops::DerefMut for GbmBuffer {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.bo
    }
}

/// Light wrapper around an [`GbmDevice`] to implement the [`Allocator`]-trait
#[derive(Clone, Debug)]
pub struct GbmAllocator<A: AsFd + 'static> {
    device: GbmDevice<A>,
    default_flags: GbmBufferFlags,
}

impl<A: AsFd + 'static> AsRef<GbmDevice<A>> for GbmAllocator<A> {
    #[inline]
    fn as_ref(&self) -> &GbmDevice<A> {
        &self.device
    }
}

impl<A: AsFd + 'static> AsMut<GbmDevice<A>> for GbmAllocator<A> {
    #[inline]
    fn as_mut(&mut self) -> &mut GbmDevice<A> {
        &mut self.device
    }
}

impl<A: AsFd + 'static> AsFd for GbmAllocator<A> {
    #[inline]
    fn as_fd(&self) -> BorrowedFd<'_> {
        self.device.as_fd()
    }
}

impl<A: AsFd + 'static> GbmAllocator<A> {
    /// Create a new [`GbmAllocator`] from a [`GbmDevice`] with some default usage flags,
    /// to be used when [`Allocator::create_buffer`] is invoked.
    pub fn new(device: GbmDevice<A>, default_flags: GbmBufferFlags) -> GbmAllocator<A> {
        GbmAllocator {
            device,
            default_flags,
        }
    }

    /// Alternative to [`Allocator::create_buffer`], if you need a one-off buffer with
    /// a different set of usage flags.
    #[instrument(level = "trace", skip(self), fields(self.device = ?self.device, err))]
    #[profiling::function]
    pub fn create_buffer_with_flags(
        &mut self,
        width: u32,
        height: u32,
        fourcc: Fourcc,
        modifiers: &[Modifier],
        flags: GbmBufferFlags,
    ) -> Result<GbmBuffer, std::io::Error> {
        // #[cfg(feature = "backend_gbm_has_create_with_modifiers2")]
        // let result = self
        //     .device
        //     .create_buffer_object_with_modifiers2(width, height, fourcc, modifiers.iter().copied(), flags)
        //     .map(|bo| GbmBuffer::from_bo(bo, false));

        // #[cfg(not(feature = "backend_gbm_has_create_with_modifiers2"))]
        // let result = if (flags & !(GbmBufferFlags::SCANOUT | GbmBufferFlags::RENDERING)).is_empty() {
        //     self.device
        //         .create_buffer_object_with_modifiers(width, height, fourcc, modifiers.iter().copied())
        //         .map(|bo| GbmBuffer::from_bo(bo, false))
        // } else if modifiers.contains(&Modifier::Invalid) || modifiers.contains(&Modifier::Linear) {
            return self
                .device
                .create_buffer_object(width, height, fourcc, flags)
                .map(|bo| GbmBuffer::from_bo(bo, false));
        // } else {
        //     return Err(std::io::Error::new(
        //         std::io::ErrorKind::Other,
        //         "unsupported combination of flags and modifiers",
        //     ));
        // };

        // match result {
        //     Ok(bo) => Ok(bo),
        //     Err(err) => {
        //         if modifiers.contains(&Modifier::Invalid) || modifiers.contains(&Modifier::Linear) {
        //             self.device
        //                 .create_buffer_object(width, height, fourcc, flags)
        //                 .map(|bo| GbmBuffer::from_bo(bo, true))
        //         } else {
        //             Err(err)
        //         }
        //     }
        // }
    }
}

impl<A: AsFd + 'static> Allocator for GbmAllocator<A> {
    type Buffer = GbmBuffer;
    type Error = std::io::Error;

    #[profiling::function]
    fn create_buffer(
        &mut self,
        width: u32,
        height: u32,
        fourcc: Fourcc,
        modifiers: &[Modifier],
    ) -> Result<GbmBuffer, Self::Error> {
        self.create_buffer_with_flags(width, height, fourcc, modifiers, self.default_flags)
    }
}

impl Buffer for GbmBuffer {
    #[inline]
    fn size(&self) -> Size<i32, BufferCoords> {
        self.size
    }

    #[inline]
    fn format(&self) -> Format {
        self.format
    }
}

/// Errors during conversion to a dmabuf handle from a gbm buffer object
#[derive(thiserror::Error, Debug)]
pub enum GbmConvertError {
    /// The gbm device was destroyed
    #[error("The gbm device was destroyed")]
    DeviceDestroyed(#[from] gbm::DeviceDestroyedError),
    /// The buffer consists out of multiple file descriptions, which is currently unsupported
    #[error("Buffer consists out of multiple file descriptors, which is currently unsupported")]
    UnsupportedBuffer,
    /// The conversion returned an invalid file descriptor
    #[error("Buffer returned invalid file descriptor")]
    InvalidFD(#[from] gbm::InvalidFdError),
}

impl From<gbm::FdError> for GbmConvertError {
    #[inline]
    fn from(err: gbm::FdError) -> Self {
        match err {
            gbm::FdError::DeviceDestroyed(err) => err.into(),
            gbm::FdError::InvalidFd(err) => err.into(),
        }
    }
}

impl AsDmabuf for GbmBuffer {
    type Error = GbmConvertError;

    #[cfg(feature = "backend_gbm_has_fd_for_plane")]
    #[profiling::function]
    fn export(&self) -> Result<Dmabuf, GbmConvertError> {
        let planes = self.plane_count()? as i32;

        let mut builder = Dmabuf::builder_from_buffer(self, DmabufFlags::empty());
        for idx in 0..planes {
            let fd = self.fd_for_plane(idx)?;

            builder.add_plane(
                // SAFETY: `gbm_bo_get_fd_for_plane` returns a new fd owned by the caller.
                fd,
                idx as u32,
                self.offset(idx)?,
                self.stride_for_plane(idx)?,
            );
        }

        #[cfg(feature = "backend_drm")]
        if let Some(node) = self.device_fd().ok().and_then(|fd| DrmNode::from_file(fd).ok()) {
            builder.set_node(node);
        }

        Ok(builder.build().unwrap())
    }

    #[cfg(not(feature = "backend_gbm_has_fd_for_plane"))]
    #[profiling::function]
    fn export(&self) -> Result<Dmabuf, GbmConvertError> {
        let planes = self.plane_count()? as i32;

        let mut iter = (0i32..planes).map(|i| self.handle_for_plane(i));
        let first = iter.next().expect("Encountered a buffer with zero planes");
        // check that all handles are the same
        let handle = iter.try_fold(first, |first, next| {
            if let (Ok(next), Ok(first)) = (next, first) {
                if unsafe { next.u64_ == first.u64_ } {
                    return Some(Ok(first));
                }
            }
            None
        });
        if handle.is_none() {
            // GBM is lacking a function to get a FD for a given plane. Instead,
            // check all planes have the same handle. We can't use
            // drmPrimeHandleToFD because that messes up handle ref'counting in
            // the user-space driver.
            return Err(GbmConvertError::UnsupportedBuffer);
        }

        let mut builder = Dmabuf::builder_from_buffer(self, DmabufFlags::empty());
        for idx in 0..planes {
            let fd = self.fd()?;

            builder.add_plane(
                // SAFETY: `gbm_bo_get_fd` returns a new fd owned by the caller.
                fd,
                idx as u32,
                self.offset(idx)?,
                self.stride_for_plane(idx)?,
            );
        }

        #[cfg(feature = "backend_drm")]
        if let Some(node) = self.device_fd().ok().and_then(|fd| DrmNode::from_file(fd).ok()) {
            builder.set_node(node);
        }

        Ok(builder.build().unwrap())
    }
}

impl Dmabuf {
    /// Import a Dmabuf using libgbm, creating a gbm Buffer Object to the same underlying data.
    #[profiling::function]
    pub fn import_to<A: AsFd + 'static>(
        &self,
        gbm: &GbmDevice<A>,
        usage: GbmBufferFlags,
    ) -> std::io::Result<GbmBuffer> {
        let mut handles = [None; MAX_PLANES];
        for (i, handle) in self.handles().take(MAX_PLANES).enumerate() {
            handles[i] = Some(handle);
        }
        let mut strides = [0i32; MAX_PLANES];
        for (i, stride) in self.strides().take(MAX_PLANES).enumerate() {
            strides[i] = stride as i32;
        }
        let mut offsets = [0i32; MAX_PLANES];
        for (i, offset) in self.offsets().take(MAX_PLANES).enumerate() {
            offsets[i] = offset as i32;
        }

        if self.has_modifier() || self.num_planes() > 1 || self.offsets().next().unwrap() != 0 {
            gbm.import_buffer_object_from_dma_buf_with_modifiers(
                self.num_planes() as u32,
                handles,
                self.width(),
                self.height(),
                self.format().code,
                usage,
                strides,
                offsets,
                self.format().modifier,
            )
            .map(|bo| GbmBuffer::from_bo(bo, false))
        } else {
            gbm.import_buffer_object_from_dma_buf(
                handles[0].unwrap(),
                self.width(),
                self.height(),
                strides[0] as u32,
                self.format().code,
                if self.format().modifier == Modifier::Linear {
                    usage | GbmBufferFlags::LINEAR
                } else {
                    usage
                },
            )
            .map(|bo| GbmBuffer::from_bo(bo, true))
        }
    }
}
