//! Helper for synchronizing rendering operations
use std::{os::unix::io::OwnedFd, sync::Arc};

use downcast_rs::{impl_downcast, Downcast};

#[cfg(feature = "backend_egl")]
mod egl;

/// A fence that will be signaled in finite time
pub trait Fence: std::fmt::Debug + Send + Sync + Downcast {
    /// Queries the state of the fence
    fn is_signaled(&self) -> bool;

    /// Blocks the current thread until the fence is signaled.
    fn wait(&self);

    /// Returns whether this fence has a native handle
    fn is_native(&self) -> bool;

    /// Get the native fence fd if available
    fn native(&self) -> Option<OwnedFd>;
}
impl_downcast!(Fence);

/// A sync point the will be signaled in finite time
#[derive(Debug, Clone)]
#[must_use]
pub struct SyncPoint {
    fence: Option<Arc<dyn Fence>>,
}

impl Default for SyncPoint {
    fn default() -> Self {
        Self::signaled()
    }
}

impl SyncPoint {
    /// Create an already signaled sync point
    pub fn signaled() -> Self {
        Self {
            fence: Default::default(),
        }
    }

    /// Get a reference to the underlying [`Fence`] if any
    ///
    /// Returns `None` if the sync point does not contain a fence
    /// or contains a different type of fence.
    pub fn get<F: Fence + 'static>(&self) -> Option<&F> {
        self.fence.as_ref().and_then(|f| f.downcast_ref())
    }

    /// Queries the state of the sync point
    ///
    /// Will always return `true` in case the sync point does not contain a fence
    pub fn is_reached(&self) -> bool {
        self.fence.as_ref().map(|f| f.is_signaled()).unwrap_or(true)
    }

    /// Blocks the current thread until the sync point is signaled
    ///
    /// If the sync point does not contain a fence this will never block.
    pub fn wait(&self) {
        if let Some(fence) = self.fence.as_ref() {
            fence.wait();
        }
    }

    /// Returns whether this sync point holds a native handle
    ///
    /// Will always return `false` in case the sync point does not contain a fence
    pub fn is_native(&self) -> bool {
        self.fence.as_ref().map(|f| f.is_native()).unwrap_or(false)
    }

    /// Get the native fence if available
    ///
    /// Will always return `None` in case the sync point does not contain a fence
    pub fn native(&self) -> Option<OwnedFd> {
        self.fence.as_ref().and_then(|f| f.native())
    }
}

impl<T: Fence + 'static> From<T> for SyncPoint {
    fn from(value: T) -> Self {
        SyncPoint {
            fence: Some(Arc::new(value)),
        }
    }
}
