use std::{os::unix::io::OwnedFd, time::Duration};

use crate::backend::{egl::fence::EGLFence, renderer::sync::Fence};

impl Fence for EGLFence {
    fn wait(&self) {
        self.client_wait(None, false);
    }

    fn is_native(&self) -> bool {
        self.is_native()
    }

    fn native(&self) -> Option<OwnedFd> {
        self.fd().ok()
    }

    fn is_signaled(&self) -> bool {
        self.client_wait(Some(Duration::ZERO), false)
    }
}
