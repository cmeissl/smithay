use std::os::unix::prelude::{AsRawFd, FromRawFd};

use drm_fourcc::DrmModifier;
use io_lifetimes::OwnedFd;

use super::{
    dmabuf::{Dmabuf, DmabufFlags},
    Allocator,
};
use thiserror::Error;

use nix::ioctl_readwrite;

const DMA_HEAP_IOC_MAGIC: u8 = b'H';
const DMA_HEAP_IOC_ALLOC: u8 = 0;

#[derive(Default)]
#[repr(C)]
pub struct dma_heap_allocation_data {
    pub len: u64,
    pub fd: u32,
    pub fd_flags: u32,
    pub heap_flags: u64,
}

ioctl_readwrite!(
    dma_heap_alloc,
    DMA_HEAP_IOC_MAGIC,
    DMA_HEAP_IOC_ALLOC,
    dma_heap_allocation_data
);

pub struct DmabufHeap {
    fd: OwnedFd,
}

impl DmabufHeap {
    pub fn system() -> Self {
        let fd = nix::fcntl::open(
            "/dev/dma_heap/system",
            nix::fcntl::OFlag::O_CLOEXEC | nix::fcntl::OFlag::O_RDONLY,
            nix::sys::stat::Mode::empty(),
        )
        .expect("failed to open system heap");
        let fd = unsafe { OwnedFd::from_raw_fd(fd) };
        DmabufHeap { fd }
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("unsupported format")]
    UnsupportedFormat(drm_fourcc::DrmFourcc),
    #[error("unknown bpp")]
    FormatUnknownBpp(drm_fourcc::DrmFourcc),
    #[error("unsupported modifier")]
    UnsupportedModifiers,
    #[error("failed to allocate")]
    AllocationFailed(nix::errno::Errno),
}

impl Allocator<Dmabuf> for DmabufHeap {
    type Error = Error;

    fn create_buffer(
        &mut self,
        width: u32,
        height: u32,
        fourcc: drm_fourcc::DrmFourcc,
        modifiers: &[drm_fourcc::DrmModifier],
    ) -> Result<Dmabuf, Self::Error> {
        if !modifiers
            .iter()
            .any(|modifier| *modifier == drm_fourcc::DrmModifier::Linear)
        {
            return Err(Error::UnsupportedModifiers);
        }

        let bpp =
            crate::backend::allocator::format::get_bpp(fourcc).ok_or(Error::FormatUnknownBpp(fourcc))?;
        let size = width as usize * bpp * height as usize;
        let mut allocation_data = dma_heap_allocation_data {
            len: size as u64,
            fd_flags: (nix::fcntl::OFlag::O_CLOEXEC | nix::fcntl::OFlag::O_RDWR).bits() as u32,
            ..Default::default()
        };

        let _ = unsafe {
            dma_heap_alloc(self.fd.as_raw_fd(), &mut allocation_data).map_err(Error::AllocationFailed)?
        };

        let mut builder = Dmabuf::builder((width as i32, height as i32), fourcc, DmabufFlags::empty());
        builder.add_plane(
            allocation_data.fd as i32,
            0,
            0,
            (width as usize * bpp) as u32,
            DrmModifier::Linear,
        );
        let dmabuf = builder.build().unwrap();
        Ok(dmabuf)
    }
}

#[cfg(test)]
mod test {
    use drm_fourcc::DrmModifier;

    use crate::backend::allocator::Allocator;

    use super::DmabufHeap;

    #[test]
    fn test() {
        let mut allocator = DmabufHeap::system();
        let buffer = allocator
            .create_buffer(
                1920,
                1080,
                drm_fourcc::DrmFourcc::Argb8888,
                &[DrmModifier::Linear],
            )
            .expect("failed to create buffer");
    }
}
