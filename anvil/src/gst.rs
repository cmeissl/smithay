use std::{cell::RefCell, rc::Rc, sync::atomic::Ordering};

use crate::{drawing::*, state::Backend, AnvilState};
use gstreamer::prelude::{Cast, ElementExt, GstBinExt};
#[cfg(feature = "debug")]
use image::GenericImageView;
use slog::Logger;
#[cfg(feature = "debug")]
use smithay::backend::renderer::{gles2::Gles2Texture, ImportMem};
#[cfg(feature = "egl")]
use smithay::{backend::renderer::ImportDma, wayland::dmabuf::init_dmabuf_global};
use smithay::{
    backend::{
        allocator::{
            dmabuf::{AsDmabuf, Dmabuf},
            Allocator, Slot, Swapchain,
        },
        egl::{EGLContext, EGLDisplay},
        renderer::{gles2::Gles2Renderer, Bind, ImportEgl},
    },
    reexports::{
        calloop::EventLoop,
        gbm::{self, BufferObject},
        nix::{fcntl, fcntl::OFlag, sys::stat},
        wayland_server::{
            protocol::{wl_output, wl_surface},
            Display,
        },
    },
    utils::Size,
    wayland::{
        output::{Mode, Output, PhysicalProperties},
        seat::CursorImageStatus,
    },
};

pub struct GstData {
    pub(crate) swapchain:
        Swapchain<Box<dyn Allocator<BufferObject<()>, Error = std::io::Error> + 'static>, BufferObject<()>>,
    pub(crate) buffer: Option<Slot<BufferObject<()>>>,
    #[cfg(feature = "debug")]
    fps_texture: Gles2Texture,
    #[cfg(feature = "debug")]
    fps: fps_ticker::Fps,
}

impl Backend for GstData {
    fn seat_name(&self) -> String {
        "gst".to_owned()
    }

    fn reset_buffers(&mut self, _output: &Output) {
        self.swapchain.reset_buffers();
        self.buffer = None;
    }

    fn early_import(&mut self, _surface: &wl_surface::WlSurface) {}
}

pub fn run_gst(log: Logger) {
    let mut event_loop = EventLoop::try_new().unwrap();
    let display = Rc::new(RefCell::new(Display::new()));

    // Open the device
    let open_flags = OFlag::O_RDWR | OFlag::O_CLOEXEC;
    let device_fd =
        fcntl::open("/dev/dri/card0", open_flags, stat::Mode::empty()).expect("Failed to open device");
    // Create the gbm device for buffer allocation.
    let device = gbm::Device::new(device_fd).expect("Failed to create gbm device");
    // Initialize EGL using the GBM device.
    let egl = EGLDisplay::new(&device, log.clone()).expect("Failed to create EGLDisplay");
    // Create the OpenGL context
    let context = EGLContext::new(&egl, log.clone()).expect("Failed to create EGLContext");

    let renderer =
        unsafe { Gles2Renderer::new(context, log.clone()) }.expect("Failed to initialize renderer");
    let renderer = Rc::new(RefCell::new(renderer));

    #[cfg(feature = "egl")]
    {
        if renderer.borrow_mut().bind_wl_display(&*display.borrow()).is_ok() {
            info!(log, "EGL hardware-acceleration enabled");
            let dmabuf_formats = renderer
                .borrow_mut()
                .dmabuf_formats()
                .cloned()
                .collect::<Vec<_>>();
            let renderer = renderer.clone();
            init_dmabuf_global(
                &mut *display.borrow_mut(),
                dmabuf_formats,
                move |buffer, _| renderer.borrow_mut().import_dmabuf(buffer, None).is_ok(),
                log.clone(),
            );
        }
    }

    let size = Size::from((1920, 1080));

    let mode = Mode {
        size,
        refresh: 60_000,
    };

    let swapchain = Swapchain::new(
        Box::new(device.clone()) as Box<dyn Allocator<BufferObject<()>, Error = std::io::Error> + 'static>,
        size.w as u32,
        size.h as u32,
        gbm::Format::Xbgr8888,
        vec![gbm::Modifier::Linear],
    );

    gstreamer::init().unwrap();

    let pipeline = gstreamer::parse_launch(
        "appsrc name=appsrc ! videoconvert ! x264enc tune=zerolatency speed-preset=superfast ! h264parse config-interval=-1 ! mpegtsmux ! queue ! udpsink host=127.0.0.1 port=5000",
    )
    .expect("failed to parse pipeline")
    .downcast::<gstreamer::Pipeline>()
    .unwrap();

    let appsrc = pipeline
        .by_name("appsrc")
        .unwrap()
        .downcast::<gstreamer_app::AppSrc>()
        .unwrap();

    let video_info =
        gstreamer_video::VideoInfo::builder(gstreamer_video::VideoFormat::Rgbx, size.w as u32, size.h as u32)
            .fps(gstreamer::Fraction::new(60, 1))
            .build()
            .expect("Failed to create video info");

    appsrc.set_caps(Some(&video_info.to_caps().unwrap()));
    appsrc.set_do_timestamp(true);
    appsrc.set_is_live(true);

    let bus = pipeline.bus().expect("pipeline without bus");

    pipeline
        .set_state(gstreamer::State::Playing)
        .expect("failed to set pipeline to playing");

    #[cfg(feature = "debug")]
    let fps_image =
        image::io::Reader::with_format(std::io::Cursor::new(FPS_NUMBERS_PNG), image::ImageFormat::Png)
            .decode()
            .unwrap();
    let data = GstData {
        swapchain,
        buffer: None,
        #[cfg(feature = "debug")]
        fps_texture: {
            renderer
                .borrow_mut()
                .import_memory(
                    &fps_image.to_rgba8(),
                    (fps_image.width() as i32, fps_image.height() as i32).into(),
                    false,
                )
                .expect("Unable to upload FPS texture")
        },
        #[cfg(feature = "debug")]
        fps: fps_ticker::Fps::default(),
    };

    let mut state = AnvilState::init(display.clone(), event_loop.handle(), data, log.clone(), true);
    let output = Output::new(
        "gst".to_string(),
        PhysicalProperties {
            size: (0, 0).into(),
            subpixel: wl_output::Subpixel::Unknown,
            make: "Smithay".into(),
            model: "gst".into(),
        },
        log.clone(),
    );
    let _global = output.create_global(&mut *display.borrow_mut());
    output.change_current_state(Some(mode), None, None, Some((0, 0).into()));
    output.set_preferred(mode);
    state.space.borrow_mut().map_output(&output, (0, 0));

    let start_time = std::time::Instant::now();

    #[cfg(feature = "xwayland")]
    state.start_xwayland();

    info!(log, "Initialization completed, starting the main loop.");

    while state.running.load(Ordering::SeqCst) {
        let mut space = state.space.borrow_mut();

        {
            let backend_data = &mut state.backend_data;
            let mut renderer = renderer.borrow_mut();

            // We need to borrow everything we want to refer to inside the renderer callback otherwise rustc is unhappy.
            let (x, y) = state.pointer_location.into();
            let dnd_icon = &state.dnd_icon;
            let cursor_status = &state.cursor_status;
            #[cfg(feature = "debug")]
            let fps = backend_data.fps.avg().round() as u32;
            #[cfg(feature = "debug")]
            let fps_texture = &backend_data.fps_texture;

            if backend_data.buffer.is_none() {
                let buffer = backend_data
                    .swapchain
                    .acquire()
                    .expect("Failed to acquire buffer")
                    .expect("no free buffers");
                backend_data.buffer = Some(buffer);
            }

            let slot = backend_data.buffer.as_ref().unwrap();
            let age = slot.age();
            let dmabuf = match slot.userdata().get::<Dmabuf>() {
                Some(dmabuf) => dmabuf.clone(),
                None => {
                    let dmabuf = slot.export().expect("failed to export buffer");
                    slot.userdata().insert_if_missing(|| dmabuf.clone());
                    dmabuf
                }
            };

            if let Err(err) = renderer.bind(dmabuf) {
                error!(log, "Error while binding buffer: {}", err);
                continue;
            }

            let mut elements = Vec::<CustomElem<Gles2Renderer>>::new();
            let dnd_guard = dnd_icon.lock().unwrap();
            let mut cursor_guard = cursor_status.lock().unwrap();

            // draw the dnd icon if any
            if let Some(ref surface) = *dnd_guard {
                if surface.as_ref().is_alive() {
                    elements.push(draw_dnd_icon(surface.clone(), (x as i32, y as i32), &log).into());
                }
            }

            // draw the cursor as relevant
            // reset the cursor if the surface is no longer alive
            let mut reset = false;
            if let CursorImageStatus::Image(ref surface) = *cursor_guard {
                reset = !surface.as_ref().is_alive();
            }
            if reset {
                *cursor_guard = CursorImageStatus::Default;
            }
            if let CursorImageStatus::Image(ref surface) = *cursor_guard {
                elements.push(draw_cursor(surface.clone(), (x as i32, y as i32), &log).into());
            } else {
                // TODO: Draw cursor
            }

            // draw FPS
            #[cfg(feature = "debug")]
            {
                elements.push(draw_fps::<Gles2Renderer>(fps_texture, fps).into());
            }

            let render_res = crate::render::render_output(
                &output,
                &mut *space,
                &mut *renderer,
                age.into(),
                &*elements,
                &log,
            );
            match render_res {
                Ok(_) => {
                    trace!(log, "Finished rendering");
                    // Get a new buffer
                    let mut next = backend_data
                        .swapchain
                        .acquire()
                        .expect("Failed to acquire buffer")
                        .expect("no free buffers");

                    // Swap the buffers
                    if let Some(current) = backend_data.buffer.as_mut() {
                        std::mem::swap(&mut next, current);
                    }

                    let buffer = next
                        .map(&device, 0, 0, size.w as u32, size.h as u32, |map| {
                            let mut buffer = gstreamer::Buffer::with_size(map.buffer().len())
                                .expect("failed to create buffer");

                            {
                                let buffer = buffer.get_mut().unwrap();

                                let mut vframe = gstreamer_video::VideoFrameRef::from_buffer_ref_writable(
                                    buffer,
                                    &video_info,
                                )
                                .unwrap();

                                let plane_data = vframe.plane_data_mut(0).unwrap();
                                plane_data.clone_from_slice(map.buffer());
                            }

                            buffer
                        })
                        .expect("failed to map buffer")
                        .expect("failed to map buffer");

                    appsrc.push_buffer(buffer).expect("failed to push buffer");
                }
                Err(err) => {
                    backend_data.swapchain.reset_buffers();
                    backend_data.buffer = None;
                    error!(log, "Rendering error: {}", err);
                }
            }

            #[cfg(feature = "debug")]
            state.backend_data.fps.tick();
        }

        // Send frame events so that client start drawing their next frame
        space.send_frames(start_time.elapsed().as_millis() as u32);
        std::mem::drop(space);

        if event_loop
            .dispatch(Some(std::time::Duration::ZERO), &mut state)
            .is_err()
        {
            state.running.store(false, Ordering::SeqCst);
        } else {
            state.space.borrow_mut().refresh();
            state.popups.borrow_mut().cleanup();
            display.borrow_mut().flush_clients(&mut state);

            for msg in bus.iter() {
                use gstreamer::MessageView;

                match msg.view() {
                    MessageView::Eos(..) => break,
                    MessageView::Error(err) => {
                        error!(log, "Gstreamer: {:?}", err);
                        state.running.store(false, Ordering::SeqCst);
                    }
                    _ => (),
                }
            }
        }
    }

    let _ = pipeline.set_state(gstreamer::State::Null);
}
