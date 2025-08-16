use std::os::unix::io::AsFd;

use super::{ExportBuffer, ExportFramebuffer};
use crate::backend::allocator::gbm::GbmBuffer;
use crate::reexports::drm::control::{Device, FbCmd2Flags};
use crate::utils::DevPath;
use crate::{
    backend::{
        allocator::{dmabuf::Dmabuf, Buffer},
        drm::DrmDeviceFd,
    },
    reexports::{
        drm::{self, buffer::PlanarBuffer},
        gbm::{BufferObject, Device as GbmDevice},
    },
};
use wayland_server::protocol::wl_buffer;

#[derive(Debug)]
struct GemBuffer {
    size: (u32, u32),
    format: crate::reexports::gbm::Format,
    modifier: Option<crate::reexports::gbm::Modifier>,
    pitches: [u32; 4],
    handles: [Option<drm::buffer::Handle>; 4],
    offsets: [u32; 4],
}

impl PlanarBuffer for GemBuffer {
    fn size(&self) -> (u32, u32) {
        self.size
    }

    fn format(&self) -> crate::reexports::gbm::Format {
        self.format
    }

    fn modifier(&self) -> Option<crate::reexports::gbm::Modifier> {
        self.modifier
    }

    fn pitches(&self) -> [u32; 4] {
        self.pitches
    }

    fn handles(&self) -> [Option<drm::buffer::Handle>; 4] {
        self.handles
    }

    fn offsets(&self) -> [u32; 4] {
        self.offsets
    }
}

#[derive(Debug, Clone)]
pub struct GemFramebufferExporter<D: AsFd> {
    gbm: GbmDevice<D>,
}

impl<D: AsFd> GemFramebufferExporter<D> {
    pub fn new(gbm: GbmDevice<D>) -> Self {
        GemFramebufferExporter { gbm }
    }
}

impl<D: AsFd> DevPath for GemFramebufferExporter<D> {
    fn dev_path(&self) -> Option<std::path::PathBuf> {
        self.gbm.dev_path()
    }
}

#[derive(Debug)]
pub struct GemFramebuffer {
    drm: DrmDeviceFd,
    buffer: GemBuffer,
    fb: drm::control::framebuffer::Handle,
}

impl AsRef<drm::control::framebuffer::Handle> for GemFramebuffer {
    fn as_ref(&self) -> &drm::control::framebuffer::Handle {
        &self.fb
    }
}

impl crate::backend::drm::Framebuffer for GemFramebuffer {
    fn format(&self) -> crate::backend::allocator::Format {
        crate::backend::allocator::Format {
            code: self.buffer.format,
            modifier: self.buffer.modifier.unwrap(),
        }
    }
}

impl Drop for GemFramebuffer {
    fn drop(&mut self) {
        let _ = self.drm.destroy_framebuffer(self.fb);

        let mut gem_handles = self
            .buffer
            .handles
            .iter()
            .filter_map(|handle| {
                if handle.is_some() {
                    Some(handle.unwrap())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        gem_handles.dedup();

        for gem_handle in gem_handles {
            let _ = self.drm.close_buffer(gem_handle);
        }
    }
}

fn fb_from_wl_buffer<D: AsFd>(
    drm: &DrmDeviceFd,
    gbm: &GbmDevice<D>,
    buffer: &wl_buffer::WlBuffer,
) -> Result<Option<GemFramebuffer>, Error> {
    if let Ok(dmabuf) = crate::wayland::dmabuf::get_dmabuf(buffer) {
        return Ok(Some(fb_from_dmabuf(drm, &dmabuf)?));
    }

    Ok(None)
}

fn align_to(a: u32, alignment: u32) -> u32 {
    (a + (alignment - 1)) & (!(alignment - 1))
}

fn fb_from_dmabuf(drm: &DrmDeviceFd, dmabuf: &Dmabuf) -> Result<GemFramebuffer, Error> {
    let mut handles: [Option<drm::buffer::Handle>; 4] = [None; 4];

    for (index, fd) in dmabuf.handles().enumerate() {
        let handle = drm.prime_fd_to_buffer(fd).unwrap();
        handles[index] = Some(handle);
    }

    let num_planes = dmabuf.num_planes();
    let pitches = dmabuf.strides().collect::<Vec<_>>();
    let offsets = dmabuf.offsets().collect::<Vec<_>>();

    let format = dmabuf.format();

    let size = if let Some(alignment) = alignment(format) {
        (
            align_to(dmabuf.width(), alignment.0),
            align_to(dmabuf.height(), alignment.1),
        )
    } else {
        (dmabuf.width(), dmabuf.height())
    };

    let modifier = if format.modifier != crate::backend::allocator::Modifier::Invalid {
        Some(format.modifier)
    } else {
        None
    };
    let flags = if modifier.is_some() {
        FbCmd2Flags::MODIFIERS
    } else {
        FbCmd2Flags::empty()
    };
    let gem_buffer = GemBuffer {
        size,
        format: format.code,
        modifier,
        pitches: [
            pitches[0],
            if num_planes > 1 { pitches[1] } else { 0 },
            if num_planes > 2 { pitches[2] } else { 0 },
            if num_planes > 3 { pitches[3] } else { 0 },
        ],
        offsets: [
            offsets[0],
            if num_planes > 1 { offsets[1] } else { 0 },
            if num_planes > 2 { offsets[2] } else { 0 },
            if num_planes > 3 { offsets[3] } else { 0 },
        ],
        handles,
    };

    let fb = drm
        .add_planar_framebuffer(&gem_buffer, flags)
        .map_err(|err| Error::Drm(err))?;

    Ok(GemFramebuffer {
        drm: drm.clone(),
        buffer: gem_buffer,
        fb,
    })
}

fn alignment(format: crate::backend::allocator::Format) -> Option<(u32, u32)> {
    match format.modifier {
        crate::backend::allocator::Modifier::Vivante_super_tiled => Some((64, 64)),
        _ => None,
    }
}

fn fb_from_bo<T>(drm: &DrmDeviceFd, bo: &BufferObject<T>) -> Result<GemFramebuffer, Error> {
    let format = bo.format();
    let modifier = bo.modifier();

    let size = if let Some(alignment) = alignment(crate::backend::allocator::Format {
        code: format,
        modifier,
    }) {
        (
            align_to(bo.width(), alignment.0),
            align_to(bo.height(), alignment.1),
        )
    } else {
        (bo.width(), bo.height())
    };

    let modifier = if modifier != crate::backend::allocator::Modifier::Invalid {
        Some(modifier)
    } else {
        None
    };
    let flags = if modifier.is_some() {
        FbCmd2Flags::MODIFIERS
    } else {
        FbCmd2Flags::empty()
    };

    let gem_buffer = GemBuffer {
        size,
        format,
        modifier,
        pitches: bo.pitches(),
        offsets: bo.offsets(),
        handles: bo.handles(),
    };

    let fb = drm
        .add_planar_framebuffer(&gem_buffer, flags)
        .map_err(|err| Error::Drm(err))?;

    Ok(GemFramebuffer {
        drm: drm.clone(),
        buffer: gem_buffer,
        fb,
    })
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("DRM access error")]
    Drm(std::io::Error),
}

impl<D: AsFd> ExportFramebuffer<GbmBuffer> for GemFramebufferExporter<D> {
    type Framebuffer = GemFramebuffer;
    type Error = Error;

    fn add_framebuffer(
        &self,
        drm: &DrmDeviceFd,
        buffer: ExportBuffer<'_, GbmBuffer>,
        _allow_opaque_fallback: bool,
    ) -> Result<Option<Self::Framebuffer>, Self::Error> {
        match buffer {
            ExportBuffer::Wayland(wl_buffer) => fb_from_wl_buffer(drm, &self.gbm, wl_buffer),
            ExportBuffer::Allocator(buffer) => fb_from_bo(drm, buffer).map(Some),
        }
    }

    fn can_add_framebuffer(&self, buffer: &ExportBuffer<'_, GbmBuffer>) -> bool {
        true
    }
}
