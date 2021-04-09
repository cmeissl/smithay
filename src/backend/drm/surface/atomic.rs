use drm::control::atomic::AtomicModeReq;
use drm::control::Device as ControlDevice;
use drm::control::{
    connector, crtc, dumbbuffer::DumbBuffer, framebuffer, plane, property, AtomicCommitFlags, Mode,
};

use std::collections::HashSet;
use std::os::unix::io::AsRawFd;
use std::sync::{atomic::{AtomicBool, Ordering}, Arc, Mutex, RwLock};

use crate::backend::drm::{
    device::{DevPath, DrmDeviceInternal},
    device::atomic::Mapping,
    error::Error,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct State {
    pub mode: Mode,
    pub blob: property::Value<'static>,
    pub connectors: HashSet<connector::Handle>,
}

pub struct AtomicDrmSurface<A: AsRawFd + 'static> {
    pub(super) fd: Arc<DrmDeviceInternal<A>>,
    active: Arc<AtomicBool>,
    crtc: crtc::Handle,
    plane: plane::Handle,
    prop_mapping: Mapping,
    state: RwLock<State>,
    pending: RwLock<State>,
    test_buffer: Mutex<Option<(DumbBuffer, framebuffer::Handle)>>,
    logger: ::slog::Logger,
}

impl<A: AsRawFd + 'static> AtomicDrmSurface<A> {
    pub fn new(
        fd: Arc<DrmDeviceInternal<A>>,
        active: Arc<AtomicBool>,
        crtc: crtc::Handle,
        plane: plane::Handle,
        prop_mapping: Mapping,
        mode: Mode,
        connectors: &[connector::Handle],
        logger: ::slog::Logger,
    ) -> Result<Self, Error> {
        let logger = logger.new(o!("smithay_module" => "backend_drm_atomic", "drm_module" => "surface"));
        info!(
            logger,
            "Initializing drm surface ({:?}:{:?}) with mode {:?} and connectors {:?}", crtc, plane, mode, connectors
        );

        let crtc_info = fd.get_crtc(crtc).map_err(|source| Error::Access {
            errmsg: "Error loading crtc info",
            dev: fd.dev_path(),
            source,
        })?;

        // If we have no current mode, we create a fake one, which will not match (and thus gets overriden on the commit below).
        // A better fix would probably be making mode an `Option`, but that would mean
        // we need to be sure, we require a mode to always be set without relying on the compiler.
        // So we cheat, because it works and is easier to handle later.
        let current_mode = crtc_info.mode().unwrap_or_else(|| unsafe { std::mem::zeroed() });
        let current_blob = match crtc_info.mode() {
            Some(mode) => fd
                .create_property_blob(&mode)
                .map_err(|source| Error::Access {
                    errmsg: "Failed to create Property Blob for mode",
                    dev: fd.dev_path(),
                    source,
                })?,
            None => property::Value::Unknown(0),
        };

        let blob = fd
            .create_property_blob(&mode)
            .map_err(|source| Error::Access {
                errmsg: "Failed to create Property Blob for mode",
                dev: fd.dev_path(),
                source,
            })?;

        let res_handles = fd.resource_handles()
            .map_err(|source| Error::Access {
                errmsg: "Error loading drm resources",
                dev: fd.dev_path(),
                source,
            })?;

        // the current set of connectors are those, that already have the correct `CRTC_ID` set.
        // so we collect them for `current_state` and set the user-given once in `pending_state`.
        //
        // If they don't match, `commit_pending` will return true and they will be changed on the next `commit`.
        let mut current_connectors = HashSet::new();
        for conn in res_handles.connectors() {
            let crtc_prop = 
                prop_mapping
                .0
                .get(&conn)
                .expect("Unknown handle")
                .get("CRTC_ID")
                .ok_or_else(|| Error::UnknownProperty {
                    handle: (*conn).into(),
                    name: "CRTC_ID",
                })
                .map(|x| *x)?;
            if let (Ok(crtc_prop_info), Ok(props)) = (fd.get_property(crtc_prop), fd.get_properties(*conn))
            {
                let (ids, vals) = props.as_props_and_values();
                for (&id, &val) in ids.iter().zip(vals.iter()) {
                    if id == crtc_prop {
                        if let property::Value::CRTC(Some(conn_crtc)) =
                            crtc_prop_info.value_type().convert_value(val)
                        {
                            if conn_crtc == crtc {
                                current_connectors.insert(*conn);
                            }
                        }
                        break;
                    }
                }
            }
        }
        let state = State {
            mode: current_mode,
            blob: current_blob,
            connectors: current_connectors,
        };
        let pending = State {
            mode,
            blob,
            connectors: connectors.iter().copied().collect(),
        };

        let surface = AtomicDrmSurface {
            fd,
            active,
            crtc,
            plane,
            prop_mapping,
            state: RwLock::new(state),
            pending: RwLock::new(pending),
            test_buffer: Mutex::new(None),
            logger,
        };

        Ok(surface)
    }

    // we need a framebuffer to do test commits, which we use to verify our pending state.
    // here we create a dumbbuffer for that purpose.
    fn create_test_buffer(&self, mode: &Mode) -> Result<framebuffer::Handle, Error> {
        let (w, h) = mode.size();
        let db = self.fd
            .create_dumb_buffer((w as u32, h as u32), crate::backend::allocator::Fourcc::Argb8888, 32)
            .map_err(|source| Error::Access {
                errmsg: "Failed to create dumb buffer",
                dev: self.fd.dev_path(),
                source,
            })?;
        let fb = self.fd
            .add_framebuffer(&db, 32, 32)
            .map_err(|source| Error::Access {
                errmsg: "Failed to create framebuffer",
                dev: self.fd.dev_path(),
                source,
            })?;

        let mut test_buffer = self.test_buffer.lock().unwrap();
        if let Some((old_db, old_fb)) = test_buffer.take() {
            let _ = self.fd.destroy_framebuffer(old_fb);
            let _ = self.fd.destroy_dumb_buffer(old_db);
        };
        *test_buffer = Some((db, fb));

        Ok(fb)
    }

    pub fn current_connectors(&self) -> HashSet<connector::Handle> {
        self.state.read().unwrap().connectors.clone()
    }

    pub fn pending_connectors(&self) -> HashSet<connector::Handle> {
        self.pending.read().unwrap().connectors.clone()
    }

    pub fn current_mode(&self) -> Mode {
        self.state.read().unwrap().mode
    }

    pub fn pending_mode(&self) -> Mode {
        self.pending.read().unwrap().mode
    }

    pub fn add_connector(&self, conn: connector::Handle) -> Result<(), Error> {
        if !self.active.load(Ordering::SeqCst) {
            return Err(Error::DeviceInactive);
        }

        let info = self.fd
            .get_connector(conn)
            .map_err(|source| Error::Access {
                errmsg: "Error loading connector info",
                dev: self.fd.dev_path(),
                source,
            })?;

        let mut pending = self.pending.write().unwrap();

        // check if the connector can handle the current mode
        if info.modes().contains(&pending.mode) {
            // check if config is supported
            let req = self.build_request(
                &mut [conn].iter(),
                &mut [].iter(),
                self.plane,
                Some(self.create_test_buffer(&pending.mode)?),
                Some(pending.mode),
                Some(pending.blob),
            )?;
            self.fd.atomic_commit(
                &[AtomicCommitFlags::AllowModeset, AtomicCommitFlags::TestOnly],
                req,
            )
            .map_err(|_| Error::TestFailed(self.crtc))?;

            // seems to be, lets add the connector
            pending.connectors.insert(conn);

            Ok(())
        } else {
            Err(Error::ModeNotSuitable(pending.mode))
        }
    }

    pub fn remove_connector(&self, conn: connector::Handle) -> Result<(), Error> {
        if !self.active.load(Ordering::SeqCst) {
            return Err(Error::DeviceInactive);
        }

        let mut pending = self.pending.write().unwrap();

        // the test would also prevent this, but the error message is far less helpful
        if pending.connectors.contains(&conn) && pending.connectors.len() == 1 {
            return Err(Error::SurfaceWithoutConnectors(self.crtc));
        }

        // check if new config is supported (should be)
        let req = self.build_request(
            &mut [].iter(),
            &mut [conn].iter(),
            self.plane,
            Some(self.create_test_buffer(&pending.mode)?),
            Some(pending.mode),
            Some(pending.blob),
        )?;
        self.fd.atomic_commit(
            &[AtomicCommitFlags::AllowModeset, AtomicCommitFlags::TestOnly],
            req,
        )
        .map_err(|_| Error::TestFailed(self.crtc))?;

        // seems to be, lets remove the connector
        pending.connectors.remove(&conn);

        Ok(())
    }

    pub fn set_connectors(&self, connectors: &[connector::Handle]) -> Result<(), Error> {
        // the test would also prevent this, but the error message is far less helpful
        if connectors.is_empty() {
            return Err(Error::SurfaceWithoutConnectors(self.crtc));
        }

        if !self.active.load(Ordering::SeqCst) {
            return Err(Error::DeviceInactive);
        }

        let current = self.state.write().unwrap();
        let mut pending = self.pending.write().unwrap();

        let conns = connectors.iter().cloned().collect::<HashSet<_>>();
        let mut added = conns.difference(&current.connectors);
        let mut removed = current.connectors.difference(&conns);

        let req = self.build_request(
            &mut added,
            &mut removed,
            self.plane,
            Some(self.create_test_buffer(&pending.mode)?),
            Some(pending.mode),
            Some(pending.blob),
        )?;

        self.fd.atomic_commit(
            &[AtomicCommitFlags::AllowModeset, AtomicCommitFlags::TestOnly],
            req,
        )
        .map_err(|_| Error::TestFailed(self.crtc))?;

        pending.connectors = conns;

        Ok(())
    }

    pub fn use_mode(&self, mode: Mode) -> Result<(), Error> {
        if !self.active.load(Ordering::SeqCst) {
            return Err(Error::DeviceInactive);
        }

        let mut pending = self.pending.write().unwrap();

        // check if new config is supported
        let new_blob = self.fd
            .create_property_blob(&mode)
            .map_err(|source| Error::Access {
                errmsg: "Failed to create Property Blob for mode",
                dev: self.fd.dev_path(),
                source,
            })?;

        let test_fb = Some(self.create_test_buffer(&pending.mode)?);
        let req = self.build_request(
            &mut pending.connectors.iter(),
            &mut [].iter(),
            self.plane,
            test_fb,
            Some(mode),
            Some(new_blob),
        )?;
        if let Err(err) = self.fd
            .atomic_commit(
                &[AtomicCommitFlags::AllowModeset, AtomicCommitFlags::TestOnly],
                req,
            )
            .map_err(|_| Error::TestFailed(self.crtc))
        {
            let _ = self.fd.destroy_property_blob(new_blob.into());
            return Err(err);
        }

        // seems to be, lets change the mode
        pending.mode = mode;
        pending.blob = new_blob;

        Ok(())
    }
    
    pub fn commit_pending(&self) -> bool {
        *self.pending.read().unwrap() != *self.state.read().unwrap()
    }

    pub fn commit(&self, framebuffer: framebuffer::Handle) -> Result<(), Error> {
        if !self.active.load(Ordering::SeqCst) {
            return Err(Error::DeviceInactive);
        }

        let mut current = self.state.write().unwrap();
        let pending = self.pending.write().unwrap();

        debug!(
            self.logger,
            "Preparing Commit.\n\tCurrent: {:?}\n\tPending: {:?}\n", *current, *pending
        );

        // we need the differences to know, which connectors need to change properties
        let current_conns = current.connectors.clone();
        let pending_conns = pending.connectors.clone();
        let mut removed = current_conns.difference(&pending_conns);
        let mut added = pending_conns.difference(&current_conns);

        for conn in removed.clone() {
            if let Ok(info) = self.fd.get_connector(*conn) {
                info!(self.logger, "Removing connector: {:?}", info.interface());
            } else {
                info!(self.logger, "Removing unknown connector");
            }
        }

        for conn in added.clone() {
            if let Ok(info) = self.fd.get_connector(*conn) {
                info!(self.logger, "Adding connector: {:?}", info.interface());
            } else {
                info!(self.logger, "Adding unknown connector");
            }
        }

        if current.mode != pending.mode {
            info!(self.logger, "Setting new mode: {:?}", pending.mode.name());
        }

        trace!(self.logger, "Testing screen config");

        // test the new config and return the request if it would be accepted by the driver.
        let req = {
            let req = self.build_request(
                &mut added,
                &mut removed,
                self.plane,
                Some(framebuffer),
                Some(pending.mode),
                Some(pending.blob),
            )?;

            if let Err(err) = self.fd
                .atomic_commit(
                    &[AtomicCommitFlags::AllowModeset, AtomicCommitFlags::TestOnly],
                    req.clone(),
                )
                .map_err(|_| Error::TestFailed(self.crtc))
            {
                warn!(
                    self.logger,
                    "New screen configuration invalid!:\n\t{:#?}\n\t{}\n", req, err
                );

                return Err(err);
            } else {
                if current.mode != pending.mode {
                    if let Err(err) = self.fd.destroy_property_blob(current.blob.into()) {
                        warn!(self.logger, "Failed to destory old mode property blob: {}", err);
                    }
                }

                // new config
                req
            }
        };

        debug!(self.logger, "Setting screen: {:?}", req);
        let result = self.fd
            .atomic_commit(
                &[
                    // on the atomic api we can modeset and trigger a page_flip event on the same call!
                    AtomicCommitFlags::PageFlipEvent,
                    AtomicCommitFlags::AllowModeset,
                    // we also do not need to wait for completion, like with `set_crtc`.
                    // and have tested this already, so we do not expect any errors later down the line.
                    AtomicCommitFlags::Nonblock,
                ],
                req,
            )
            .map_err(|source| Error::Access {
                errmsg: "Error setting crtc",
                dev: self.fd.dev_path(),
                source,
            });

        if result.is_ok() {
            *current = pending.clone();
        }

        result
    }

    pub fn page_flip(&self, framebuffer: framebuffer::Handle) -> Result<(), Error> {
        if !self.active.load(Ordering::SeqCst) {
            return Err(Error::DeviceInactive);
        }

        // page flips work just like commits with fewer parameters..
        let req = self.build_request(
            &mut [].iter(),
            &mut [].iter(),
            self.plane,
            Some(framebuffer),
            None,
            None,
        )?;

        // .. and without `AtomicCommitFlags::AllowModeset`.
        // If we would set anything here, that would require a modeset, this would fail,
        // indicating a problem in our assumptions.
        trace!(self.logger, "Queueing page flip: {:?}", req);
        self.fd.atomic_commit(
            &[AtomicCommitFlags::PageFlipEvent, AtomicCommitFlags::Nonblock],
            req,
        )
        .map_err(|source| Error::Access {
            errmsg: "Page flip commit failed",
            dev: self.fd.dev_path(),
            source,
        })?;

        Ok(())
    }

    pub(crate) fn conn_prop_handle(
        &self,
        handle: connector::Handle,
        name: &'static str,
    ) -> Result<property::Handle, Error> {
        self
            .prop_mapping
            .0
            .get(&handle)
            .expect("Unknown handle")
            .get(name)
            .ok_or_else(|| Error::UnknownProperty {
                handle: handle.into(),
                name,
            })
            .map(|x| *x)
    }

    pub(crate) fn crtc_prop_handle(&self, handle: crtc::Handle, name: &'static str) -> Result<property::Handle, Error> {
        self
            .prop_mapping
            .1
            .get(&handle)
            .expect("Unknown handle")
            .get(name)
            .ok_or_else(|| Error::UnknownProperty {
                handle: handle.into(),
                name,
            })
            .map(|x| *x)
    }

    #[allow(dead_code)]
    pub(crate) fn fb_prop_handle(
        &self,
        handle: framebuffer::Handle,
        name: &'static str,
    ) -> Result<property::Handle, Error> {
        self
            .prop_mapping
            .2
            .get(&handle)
            .expect("Unknown handle")
            .get(name)
            .ok_or_else(|| Error::UnknownProperty {
                handle: handle.into(),
                name,
            })
            .map(|x| *x)
    }

    pub(crate) fn plane_prop_handle(
        &self,
        handle: plane::Handle,
        name: &'static str,
    ) -> Result<property::Handle, Error> {
        self
            .prop_mapping
            .3
            .get(&handle)
            .expect("Unknown handle")
            .get(name)
            .ok_or_else(|| Error::UnknownProperty {
                handle: handle.into(),
                name,
            })
            .map(|x| *x)
    }

    // If a mode is set a matching blob needs to be set (the inverse is not true)
    pub fn build_request(
        &self,
        new_connectors: &mut dyn Iterator<Item = &connector::Handle>,
        removed_connectors: &mut dyn Iterator<Item = &connector::Handle>,
        plane: plane::Handle,
        framebuffer: Option<framebuffer::Handle>,
        mode: Option<Mode>,
        blob: Option<property::Value<'static>>,
    ) -> Result<AtomicModeReq, Error> {
        // okay, here we build the actual requests used by the surface.
        let mut req = AtomicModeReq::new();

        // requests consist out of a set of properties and their new values
        // for different drm objects (crtc, plane, connector, ...).

        // for every connector that is new, we need to set our crtc_id
        for conn in new_connectors {
            req.add_property(
                *conn,
                self.conn_prop_handle(*conn, "CRTC_ID")?,
                property::Value::CRTC(Some(self.crtc)),
            );
        }

        // for every connector that got removed, we need to set no crtc_id.
        // (this is a bit problematic, because this means we need to remove, commit, add, commit
        // in the right order to move a connector to another surface. otherwise we disable the
        // the connector here again...)
        for conn in removed_connectors {
            req.add_property(
                *conn,
                self.conn_prop_handle(*conn, "CRTC_ID")?,
                property::Value::CRTC(None),
            );
        }

        // we need to set the new mode, if there is one
        if let Some(blob) = blob {
            req.add_property(self.crtc, self.crtc_prop_handle(self.crtc, "MODE_ID")?, blob);
        }

        // we also need to set this crtc active
        req.add_property(
            self.crtc,
            self.crtc_prop_handle(self.crtc, "ACTIVE")?,
            property::Value::Boolean(true),
        );

        // and we need to set the framebuffer for our primary plane
        if let Some(fb) = framebuffer {
            req.add_property(
                plane,
                self.plane_prop_handle(plane, "FB_ID")?,
                property::Value::Framebuffer(Some(fb)),
            );
        }
        
        // we also need to connect the plane
        req.add_property(
            plane,
            self.plane_prop_handle(plane, "CRTC_ID")?,
            property::Value::CRTC(Some(self.crtc)),
        );

        // if there is a new mode, we should also make sure the plane is sized correctly
        if let Some(mode) = mode {
            req.add_property(
                plane,
                self.plane_prop_handle(plane, "SRC_X")?,
                property::Value::UnsignedRange(0),
            );
            req.add_property(
                plane,
                self.plane_prop_handle(plane, "SRC_Y")?,
                property::Value::UnsignedRange(0),
            );
            req.add_property(
                plane,
                self.plane_prop_handle(plane, "SRC_W")?,
                // these are 16.16. fixed point
                property::Value::UnsignedRange((mode.size().0 as u64) << 16),
            );
            req.add_property(
                plane,
                self.plane_prop_handle(plane, "SRC_H")?,
                property::Value::UnsignedRange((mode.size().1 as u64) << 16),
            );
            // we can map parts of the plane onto different coordinated on the crtc, but we just use a 1:1 mapping.
            req.add_property(
                plane,
                self.plane_prop_handle(plane, "CRTC_X")?,
                property::Value::SignedRange(0),
            );
            req.add_property(
                plane,
                self.plane_prop_handle(plane, "CRTC_Y")?,
                property::Value::SignedRange(0),
            );
            req.add_property(
                plane,
                self.plane_prop_handle(plane, "CRTC_W")?,
                property::Value::UnsignedRange(mode.size().0 as u64),
            );
            req.add_property(
                plane,
                self.plane_prop_handle(plane, "CRTC_H")?,
                property::Value::UnsignedRange(mode.size().1 as u64),
            );
        }

        Ok(req)
    }

    // this helper function disconnects the plane.
    // this is mostly used to remove the contents quickly, e.g. on tty switch,
    // as other compositors might not make use of other planes,
    // leaving our e.g. cursor or overlays as a relict of a better time on the screen.
    pub fn clear_plane(&self) -> Result<(), Error> {
        let mut req = AtomicModeReq::new();

        req.add_property(
            self.plane,
            self.plane_prop_handle(self.plane, "CRTC_ID")?,
            property::Value::CRTC(None),
        );

        req.add_property(
            self.plane,
            self.plane_prop_handle(self.plane, "FB_ID")?,
            property::Value::Framebuffer(None),
        );

        self.fd.atomic_commit(&[AtomicCommitFlags::TestOnly], req.clone())
            .map_err(|_| Error::TestFailed(self.crtc))?;

        self.fd.atomic_commit(&[AtomicCommitFlags::Nonblock], req)
            .map_err(|source| Error::Access {
                errmsg: "Failed to commit on clear_plane",
                dev: self.fd.dev_path(),
                source,
            })
    }
}

impl<A: AsRawFd + 'static> Drop for AtomicDrmSurface<A> {
    fn drop(&mut self) {
        if let Some((db, fb)) = self.test_buffer.lock().unwrap().take() {
            let _ = self.fd.destroy_framebuffer(fb);
            let _ = self.fd.destroy_dumb_buffer(db);
        }

        if !self.active.load(Ordering::SeqCst) {
            // the device is gone or we are on another tty
            // old state has been restored, we shouldn't touch it.
            // if we are on another tty the connectors will get disabled
            // by the device, when switching back
            return;
        }

        // other ttys that use no cursor, might not clear it themselves.
        // This makes sure our cursor won't stay visible.
        if let Err(err) = self.clear_plane() {
            warn!(
                self.logger,
                "Failed to clear plane on {:?}: {}", self.crtc, err
            );
        }

        // disable connectors again
        let current = self.state.read().unwrap();
        let mut req = AtomicModeReq::new();
        for conn in current.connectors.iter() {
            let prop = self
                .prop_mapping
                .0
                .get(&conn)
                .expect("Unknown Handle")
                .get("CRTC_ID")
                .expect("Unknown property CRTC_ID");
            req.add_property(*conn, *prop, property::Value::CRTC(None));
        }
        let active_prop = self
            .prop_mapping
            .1
            .get(&self.crtc)
            .expect("Unknown Handle")
            .get("ACTIVE")
            .expect("Unknown property ACTIVE");
        let mode_prop = self
            .prop_mapping
            .1
            .get(&self.crtc)
            .expect("Unknown Handle")
            .get("MODE_ID")
            .expect("Unknown property MODE_ID");

        req.add_property(self.crtc, *active_prop, property::Value::Boolean(false));
        req.add_property(self.crtc, *mode_prop, property::Value::Unknown(0));
        if let Err(err) = self.fd.atomic_commit(&[AtomicCommitFlags::AllowModeset], req) {
            warn!(self.logger, "Unable to disable connectors: {}", err);
        }
    }
}

#[cfg(test)]
mod test {
    use super::AtomicDrmSurface;
    use std::fs::File;

    fn is_send<S: Send>() {}

    #[test]
    fn surface_is_send() {
        is_send::<AtomicDrmSurface<File>>();
    }
}
