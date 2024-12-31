use smithay::backend::input::{AbsolutePositionEvent, Axis, AxisSource, ButtonState, Event, InputBackend, InputEvent, KeyboardKeyEvent, PointerAxisEvent, PointerButtonEvent};
use smithay::backend::renderer::element::utils::select_dmabuf_feedback;
use smithay::backend::renderer::element::{default_primary_scanout_output_compare, RenderElementStates};
use smithay::desktop::utils::{surface_primary_scanout_output, update_surface_primary_scanout_output};
use smithay::output::Output;
use smithay::reexports::wayland_protocols::xdg::shell::server::xdg_toplevel;
use smithay::wayland::buffer::BufferHandler;
use smithay::wayland::commit_timing::CommitTimerBarrierStateUserData;
use smithay::wayland::dmabuf::DmabufFeedback;
use smithay::wayland::fifo::FifoBarrierCachedState;
use smithay::wayland::fractional_scale::with_fractional_scale;
use std::cell::RefCell;
use std::collections::HashMap;
use tracing::{error, info};
use std::time::Instant;
use std::ffi::OsString;
use std::sync::Arc;

use smithay::utils::{Monotonic, Point, Rectangle, Size, Time, SERIAL_COUNTER};
use smithay::input::pointer::{
    AxisFrame, ButtonEvent, CursorImageStatus, GesturePinchBeginEvent, GesturePinchEndEvent, GesturePinchUpdateEvent, GestureSwipeBeginEvent, GestureSwipeEndEvent, GestureSwipeUpdateEvent, GrabStartData, MotionEvent, PointerGrab, PointerInnerHandle
};
use smithay::backend::renderer::utils::on_commit_buffer_handler;
use smithay::reexports::wayland_server::protocol::wl_seat;
use smithay::utils::{Logical, Serial};
use smithay::{delegate_compositor, delegate_data_device, delegate_output, delegate_seat, delegate_shm, delegate_xdg_shell};
use smithay::desktop::{find_popup_root_surface, get_popup_toplevel_coords, PopupKind, PopupManager, Space, Window, WindowSurfaceType};
use smithay::input::{Seat, SeatHandler, SeatState};
use smithay::reexports::calloop::generic::Generic;
use smithay::reexports::calloop::{EventLoop, Interest, LoopSignal, PostAction};
use smithay::reexports::wayland_server::backend::{ClientData, ClientId};
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::{Client, Display, DisplayHandle, Resource};
use smithay::wayland::compositor::{self, get_parent, is_sync_subsurface, CompositorClientState, CompositorHandler, CompositorState};
use smithay::wayland::output::{OutputHandler, OutputManagerState};
use smithay::wayland::selection::data_device::{set_data_device_focus, ClientDndGrabHandler, DataDeviceHandler, DataDeviceState, ServerDndGrabHandler};
use smithay::wayland::selection::SelectionHandler;
use smithay::wayland::shell::xdg::{PopupSurface, PositionerState, SurfaceCachedState, ToplevelSurface, XdgShellHandler, XdgShellState};
use smithay::wayland::shm::{ShmHandler, ShmState};
use smithay::wayland::socket::ListeningSocketSource;
use smithay::reexports::calloop::Mode;

use crate::CalloopData;

// use this instead of unwrap to keep the program running.
macro_rules! some {
    ($option: expr, $retval: expr) => {
        match $option {
            Some(x) => x,
            None => {
                error!("failed matching Some variant of option: {}", stringify!($option));
                return $retval;
            },
        }
    };
    ($option: expr) => {
        some!($option, ())
    };
}

pub struct Wind {
    pub start_time: std::time::Instant,
    pub socket_name: OsString,
    pub display_handle: DisplayHandle,

    pub space: Space<Window>,
    pub loop_signal: LoopSignal,

    pub compositor_state: CompositorState,
    pub xdg_shell_state: XdgShellState,
    pub shm_state: ShmState,
    pub output_manager_state: OutputManagerState,
    pub seat_state: SeatState<Wind>,
    pub data_device_state: DataDeviceState,
    pub popups: PopupManager,

    pub seat: Seat<Self>,
}

impl Wind {
    pub fn new(event_loop: &mut EventLoop<CalloopData>, display: Display<Self>) -> Self {
        let start_time = Instant::now();
        let dh = display.handle();

        let compositor_state = CompositorState::new::<Self>(&dh);
        let xdg_shell_state = XdgShellState::new::<Self>(&dh);
        let shm_state = ShmState::new::<Self>(&dh, vec!{});
        let output_manager_state = OutputManagerState::new_with_xdg_output::<Self>(&dh);
        let mut seat_state = SeatState::new();
        let data_device_state = DataDeviceState::new::<Self>(&dh);
        let popups = PopupManager::default();

        let mut seat = seat_state.new_wl_seat(&dh, "winit");

        seat.add_keyboard(Default::default(), 200, 25).expect("to add default kbd");
        seat.add_pointer();

        let space = Space::default();
        let socket_name = Self::init_wayland_listener(display, event_loop);
        let loop_signal = event_loop.get_signal();

        Self{
            start_time,
            display_handle: dh,
            space,
            loop_signal,
            socket_name,
            compositor_state,
            xdg_shell_state,
            shm_state,
            output_manager_state,
            seat_state,
            data_device_state,
            popups,
            seat,
        }
    }

    fn init_wayland_listener(display: Display<Self>, event_loop: &mut EventLoop<CalloopData>) -> OsString {
        let listening_sock = ListeningSocketSource::new_auto().expect("to create a socket");
        let socket_name = listening_sock.socket_name().to_os_string();
        event_loop.handle()
            .insert_source(listening_sock, move |client_stream, _, state| {
                state
                    .display_handle
                    .insert_client(client_stream, Arc::new(ClientState::default()))
                    .unwrap();
            })
            .expect("to init wayland event source");

        event_loop.handle()
            .insert_source(Generic::new(display, Interest::READ, Mode::Level), |_, display, state| {
                unsafe {
                    display.get_mut().dispatch_clients(&mut state.state).unwrap();
                }
                Ok(PostAction::Continue)
            })
            .expect("to initialize client processing");

        return socket_name;
    }
    
    pub fn pre_repaint(&mut self, output: &Output, frame_target: impl Into<Time<Monotonic>>) {
        let frame_target = frame_target.into();
        let mut clients = HashMap::<ClientId, Client>::new();
        for wind in self.space.elements() {
            wind.with_surfaces(|surface, states| {
                let clear_commit_timer = surface_primary_scanout_output(surface, states)
                    .map(|primary| &primary == output)
                    .unwrap_or(true);
                if clear_commit_timer {
                    if let Some(mut commit_timer_state) = states
                        .data_map
                        .get::<CommitTimerBarrierStateUserData>()
                        .map(|commit_timer| commit_timer.lock().unwrap()) {
                            commit_timer_state.signal_until(frame_target);
                            let client = surface.client().unwrap();
                            clients.insert(client.id(), client);
                    }
                }
            });
        }

        let map = smithay::desktop::layer_map_for_output(output);
        for layer_surface in map.layers() {
            layer_surface.with_surfaces(|surface, states| {
                let clear_commit_timer = surface_primary_scanout_output(surface, states)
                    .map(|primary| &primary == output)
                    .unwrap_or(true);
                if clear_commit_timer {
                    if let Some(mut commit_timer_state) = states
                        .data_map
                        .get::<CommitTimerBarrierStateUserData>()
                        .map(|t| t.lock().unwrap()) {
                            commit_timer_state.signal_until(frame_target);
                            let client = surface.client().unwrap();
                            clients.insert(client.id(), client);
                    }
                }
            });
        }
        std::mem::drop(map);

        let dh = self.display_handle.clone();
        for client in clients.values() {
            self.client_compositor_state(&client).blocker_cleared(self, &dh);
        }
    }

    pub fn post_repaint(
        &mut self,
        output: &Output,
        time: impl Into<std::time::Duration>,
        dmabuf_feedback: Option<(DmabufFeedback, DmabufFeedback)>,
        render_element_states: &RenderElementStates,
    ) {
        let time = time.into();
        let throttle = Some(std::time::Duration::from_secs(1));

        let mut clients = HashMap::<ClientId, Client>::new();
        for (idx, wind) in self.space.elements().enumerate() {
            wind.with_surfaces(|surface, states| {
                let primary_scanout_output = update_surface_primary_scanout_output(
                    surface, 
                    output, 
                    states, 
                    render_element_states, 
                    default_primary_scanout_output_compare,
                );
                if let Some(output) = primary_scanout_output.as_ref() {
                    with_fractional_scale(states, |fractional| {
                        fractional.set_preferred_scale(output.current_scale().fractional_scale());
                    })
                }

                if primary_scanout_output
                    .as_ref()
                    .map(|o| o == output)
                    .unwrap_or(true) {
                        let fifo_barrier = states
                            .cached_state
                            .get::<FifoBarrierCachedState>()
                            .current()
                            .barrier
                            .take();

                        if let Some(fifo_barrier) = fifo_barrier {
                            println!("pre fifo");
                            fifo_barrier.signal();
                            println!("post fifo");
                            let client = surface.client().unwrap();
                            clients.insert(client.id(), client);
                        }
                }
            });

            if self.space.outputs_for_element(wind).contains(output) {
                println!("sending frame {time:?} {throttle:?}");
                wind.send_frame(output, time, throttle, surface_primary_scanout_output);
                println!("sent frame");
                if let Some(dmabuf_feedback) = dmabuf_feedback.as_ref() {
                    println!("sending dmabuf_feedback");
                    wind.send_dmabuf_feedback(output, surface_primary_scanout_output, |surface, _| {
                        select_dmabuf_feedback(
                            surface, 
                            render_element_states, 
                            &dmabuf_feedback.0, 
                            &dmabuf_feedback.1,
                        )
                    });
                }
            }
        }

        let map = smithay::desktop::layer_map_for_output(output);
        for layer_surface in map.layers() {
            layer_surface.with_surfaces(|surface, states| {
                let primary_scanout_output = update_surface_primary_scanout_output(
                    surface, 
                    output, 
                    states, 
                    render_element_states, 
                    default_primary_scanout_output_compare
                );

                if let Some(output) = primary_scanout_output.as_ref() {
                    with_fractional_scale(states, |fractional| {
                        fractional.set_preferred_scale(output.current_scale().fractional_scale());
                    });
                }

                if primary_scanout_output
                    .as_ref()
                    .map(|o| o == output)
                    .unwrap_or(true) {
                        let fifo_barrier = states
                            .cached_state
                            .get::<FifoBarrierCachedState>()
                            .current()
                            .barrier
                            .take();
                        if let Some(fifo_barrier) = fifo_barrier {
                            fifo_barrier.signal();
                            let client = surface.client().unwrap();
                            clients.insert(client.id(), client);
                        }
                }
            });

            layer_surface.send_frame(output, time, throttle, surface_primary_scanout_output);
            if let Some(dmabuf_feedback) = dmabuf_feedback.as_ref() {
                layer_surface.send_dmabuf_feedback(output, surface_primary_scanout_output, |surface, _| {
                    select_dmabuf_feedback(
                        surface, 
                        render_element_states, 
                        &dmabuf_feedback.0, 
                        &dmabuf_feedback.1
                    )
                });
            }
        }
        std::mem::drop(map);

        let dh = self.display_handle.clone();
        for client in clients.into_values() {
            self.client_compositor_state(&client).blocker_cleared(self, &dh);
        }
    }
}

impl OutputHandler for Wind {}


#[derive(Default)]
pub struct ClientState {
    pub compositor_state: CompositorClientState,
}

impl ClientData for ClientState {
    fn initialized(&self, _client_id: smithay::reexports::wayland_server::backend::ClientId) {
    }

    fn disconnected(&self, _client_id: smithay::reexports::wayland_server::backend::ClientId, _reason: smithay::reexports::wayland_server::backend::DisconnectReason) {
    }
}

impl SeatHandler for Wind {
    type KeyboardFocus = WlSurface;
    type PointerFocus = WlSurface;
    type TouchFocus = WlSurface;

    fn seat_state(&mut self) -> &mut SeatState<Wind> {
        return &mut self.seat_state;
    }

    fn cursor_image(&mut self, _seat: &Seat<Self>, _image: smithay::input::pointer::CursorImageStatus) {}

    fn focus_changed(&mut self, seat: &Seat<Self>, focused: Option<&WlSurface>) {
        let dh = &self.display_handle;
        let client = focused.and_then(|s| dh.get_client(s.id()).ok());
        set_data_device_focus(dh, seat, client);
    }
}


impl DataDeviceHandler for Wind {
    fn data_device_state(&self) -> &DataDeviceState {
        &self.data_device_state
    }
}

impl ServerDndGrabHandler for Wind {}
impl ClientDndGrabHandler for Wind {}
impl ShmHandler for Wind {
    fn shm_state(&self) -> &ShmState {
        &self.shm_state
    }
}

delegate_seat!(Wind);
delegate_output!(Wind);
delegate_data_device!(Wind);
delegate_xdg_shell!(Wind);
delegate_compositor!(Wind);
delegate_shm!(Wind);

impl SelectionHandler for Wind {
    type SelectionUserData = ();
}

impl CompositorHandler for Wind {
    fn compositor_state(&mut self) -> &mut CompositorState {
        return &mut self.compositor_state;
    }

    fn client_compositor_state<'a>(&self, client: &'a Client) -> &'a CompositorClientState {
        &client.get_data::<ClientState>().unwrap().compositor_state
    }

    fn commit(&mut self, surface: &WlSurface) {
        on_commit_buffer_handler::<Self>(surface);
        if !is_sync_subsurface(surface) {
            let mut root = surface.clone();
            while let Some(parent) = get_parent(&root) {
                root = parent;
            }
            if let Some(window) = self.space.elements().find(|w| {
                w.toplevel().map(|w| w.wl_surface()) == Some(&root)
            }) {
                window.on_commit();
            }
        }


    }
}

impl XdgShellHandler for Wind {
    fn xdg_shell_state(&mut self) -> &mut XdgShellState {
        &mut self.xdg_shell_state
    }

    fn new_toplevel(&mut self, surface: ToplevelSurface) {
        let window = Window::new_wayland_window(surface);
        self.space.map_element(window, (0, 0), false);
    }

    fn new_popup(&mut self, surface: PopupSurface, _positioner: PositionerState) {
        self.unconstrain_popup(&surface);
        self.popups.track_popup(PopupKind::Xdg(surface));
    }

    fn reposition_request(&mut self, surface: PopupSurface, positioner: PositionerState, token: u32) {
        surface.with_pending_state(|state| {
            let geometry = positioner.get_geometry();
            state.geometry = geometry;
            state.positioner = positioner;
        });
        self.unconstrain_popup(&surface);
        surface.send_repositioned(token);
    }

    fn move_request(&mut self, surface: ToplevelSurface, seat: wl_seat::WlSeat, serial: Serial) {
        let seat = some!(Seat::from_resource(&seat));
        let wl_surface = surface.wl_surface();
        let Some(start_data) = check_grab(&seat, wl_surface, serial) else {
            return;
        };

        let pointer = some!(seat.get_pointer());
        let window = some!(self.space.elements().find(|w| {
            w.toplevel().map(|w| w.wl_surface()) == Some(wl_surface)
        })).clone();
        let initial_window_location = some!(self.space.element_location(&window));
        pointer.set_grab(self, MoveSurfaceGrab{
            start_data, window, initial_window_location,
        }, serial, smithay::input::pointer::Focus::Clear);
    }

    fn resize_request(
        &mut self,
        surface: ToplevelSurface,
        seat: wl_seat::WlSeat,
        serial: Serial,
        edges: smithay::reexports::wayland_protocols::xdg::shell::server::xdg_toplevel::ResizeEdge,
    ) {
        let seat = some!(Seat::from_resource(&seat));
        let wl_surface = surface.wl_surface();
        let Some(start_data) = check_grab(&seat, wl_surface, serial) else {
            return;
        };
        let pointer = some!(seat.get_pointer());
        let window = some!(self.space.elements().find(|w| {
            w.toplevel().map(|w| w.wl_surface()) == Some(wl_surface)
        })).clone();
        let initial_window_location = some!(self.space.element_location(&window));
        let initial_window_size = window.geometry().size;
        surface.with_pending_state(|state| {
            state.states.set(xdg_toplevel::State::Resizing);
        });
        surface.send_pending_configure();
        pointer.set_grab(self, ResizeSurfaceGrab::start(
                start_data, 
                window, 
                edges.into(), 
                Rectangle::from_loc_and_size(initial_window_location, initial_window_size),
        ), serial, smithay::input::pointer::Focus::Clear);
    }

    fn grab(&mut self, _surface: PopupSurface, _seat: wl_seat::WlSeat, _serial: Serial) {}
}

impl BufferHandler for Wind {
    fn buffer_destroyed(&mut self, buffer: &smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer) {
    }
}


fn check_grab(
    seat: &Seat<Wind>,
    surface: &WlSurface,
    serial: Serial,
) -> Option<GrabStartData<Wind>> {
    let pointer = seat.get_pointer()?;
    if !pointer.has_grab(serial) {
        return None;
    }
    
    let start_data = pointer.grab_start_data()?;
    let (focus, _) = start_data.focus.as_ref()?;
    if !focus.id().same_client_as(&surface.id()) {
        return None;
    }

    return Some(start_data);
}

impl Wind {
    fn unconstrain_popup(&self, popup: &PopupSurface) {
        let Ok(root) = find_popup_root_surface(&PopupKind::Xdg(popup.clone())) else {
            return;
        };

        let Some(window) = self.space.elements().find(|w| {
            w.toplevel().unwrap().wl_surface() == &root
        }) else {
            return;
        };

        let output = self.space.outputs().next().unwrap();
        let output_geo = self.space.output_geometry(output).unwrap();
        let window_geo = self.space.element_geometry(window).unwrap();

        let mut target = output_geo;
        target.loc -= get_popup_toplevel_coords(&PopupKind::Xdg(popup.clone()));
        target.loc -= window_geo.loc;
        popup.with_pending_state(|state| {
            state.geometry = state.positioner.get_unconstrained_geometry(target);
        });
    }
}

pub struct MoveSurfaceGrab {
    pub start_data: GrabStartData<Wind>,
    pub window: Window,
    pub initial_window_location: Point<i32, Logical>,
}

impl PointerGrab<Wind> for MoveSurfaceGrab {
    fn motion(
        &mut self,
        data: &mut Wind,
        handle: &mut smithay::input::pointer::PointerInnerHandle<'_, Wind>,
        _focus: Option<(<Wind as SeatHandler>::PointerFocus, Point<f64, Logical>)>,
        event: &smithay::input::pointer::MotionEvent,
    ) {
        handle.motion(data, None, event);
        let delta = event.location - self.start_data.location;
        let new_location = self.initial_window_location.to_f64() + delta;
        data.space.map_element(self.window.clone(), new_location.to_i32_round(), true);
    }

    fn relative_motion(
        &mut self,
        data: &mut Wind,
        handle: &mut smithay::input::pointer::PointerInnerHandle<'_, Wind>,
        focus: Option<(<Wind as SeatHandler>::PointerFocus, Point<f64, Logical>)>,
        event: &smithay::input::pointer::RelativeMotionEvent,
    ) {
        handle.relative_motion(data, focus, event);
    }

    fn button(&mut self, data: &mut Wind, handle: &mut PointerInnerHandle<'_, Wind>, event: &smithay::input::pointer::ButtonEvent) {
        handle.button(data, event);
        const BTN_LEFT: u32 = 0x110;
        if !handle.current_pressed().contains(&BTN_LEFT) {
            handle.unset_grab(self, data, event.serial, event.time, true);
        }
    }

    fn axis(&mut self, data: &mut Wind, handle: &mut PointerInnerHandle<'_, Wind>, details: smithay::input::pointer::AxisFrame) {
        handle.axis(data, details);
    }

    fn frame(&mut self, data: &mut Wind, handle: &mut PointerInnerHandle<'_, Wind>) {
        handle.frame(data);
    }

    fn gesture_swipe_begin(
        &mut self,
        data: &mut Wind,
        handle: &mut smithay::input::pointer::PointerInnerHandle<'_, Wind>,
        event: &GestureSwipeBeginEvent,
    ) {
        handle.gesture_swipe_begin(data, event);
    }

    fn gesture_swipe_update(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        event: &GestureSwipeUpdateEvent,
    ) {
        handle.gesture_swipe_update(data, event);
    }

    fn gesture_swipe_end(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        event: &GestureSwipeEndEvent,
    ) {
        handle.gesture_swipe_end(data, event);
    }

    fn gesture_pinch_begin(
        &mut self,
        data: &mut Wind,
        handle: &mut smithay::input::pointer::PointerInnerHandle<'_, Wind>,
        event: &GesturePinchBeginEvent,
    ) {
        handle.gesture_pinch_begin(data, event);
    }

    fn gesture_pinch_update(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        event: &GesturePinchUpdateEvent,
    ) {
        handle.gesture_pinch_update(data, event);
    }

    fn gesture_pinch_end(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        event: &GesturePinchEndEvent,
    ) {
        handle.gesture_pinch_end(data, event);
    }

    fn gesture_hold_end(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        event: &smithay::input::pointer::GestureHoldEndEvent,
    ) {
        handle.gesture_hold_end(data, event);
    }

    fn gesture_hold_begin(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        event: &smithay::input::pointer::GestureHoldBeginEvent,
    ) {
        handle.gesture_hold_begin(data, event);
    }

    fn start_data(&self) -> &GrabStartData<Wind> {
        return &self.start_data;
    }

    fn unset(&mut self, data: &mut Wind) {
    }
}

pub struct ResizeSurfaceGrab {
    start_data: GrabStartData<Wind>,
    window: Window,
    edges: ResizeEdge,
    initial_rect: Rectangle<i32, Logical>,
    last_window_size: Size<i32, Logical>,
}

impl ResizeSurfaceGrab {
    pub fn start(
        start_data: GrabStartData<Wind>,
        window: Window,
        edges: ResizeEdge,
        initial_window_rect: Rectangle<i32, Logical>,
    ) -> Self {
        let initial_rect = initial_window_rect;
        ResizeSurfaceState::Resizing {edges, initial_rect}
            .into_userdata(window.toplevel().unwrap().wl_surface());
        Self {
            start_data,
            window,
            edges,
            initial_rect,
            last_window_size: initial_rect.size,
        }
    }
}

impl PointerGrab<Wind> for ResizeSurfaceGrab {
    fn motion(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        focus: Option<(<Wind as SeatHandler>::PointerFocus, Point<f64, Logical>)>,
        event: &smithay::input::pointer::MotionEvent,
    ) {
        handle.motion(data, None, event);
        let mut delta = event.location - self.start_data.location;
        let mut new_size = (self.initial_rect.size.w, self.initial_rect.size.h);

        if self.edges.intersects(ResizeEdge::LEFT | ResizeEdge::RIGHT) {
            if self.edges.intersects(ResizeEdge::LEFT) {
                delta.x = -delta.x;
            }
            new_size.0 = (new_size.0 as f64 + delta.x) as i32;
        }

        if self.edges.intersects(ResizeEdge::TOP | ResizeEdge::BOTTOM) {
            if self.edges.intersects(ResizeEdge::TOP) {
                delta.y = -delta.y;
            }

            new_size.1 = (new_size.1 as f64 + delta.y) as i32;
        }

        let (min_size, max_size) = compositor::with_states(self.window.toplevel().unwrap().wl_surface(), |states| {
            let mut guard = states.cached_state.get::<SurfaceCachedState>();
            let data = guard.current();
            (data.min_size, data.max_size)
        });

        let min_width = min_size.w.max(1);
        let min_height = min_size.h.max(1);
        let max_width = if max_size.w == 0 { i32::max_value() } else { max_size.w };
        let max_height = if max_size.h == 0 { i32::max_value() } else { max_size.h };
        
        self.last_window_size = Size::from((
            new_size.0.max(min_width).min(max_width),
            new_size.1.max(min_height).min(max_height),
        ));

        let xdg = self.window.toplevel().unwrap();
        xdg.with_pending_state(|state| {
            state.states.set(xdg_toplevel::State::Resizing);
            state.size = Some(self.last_window_size);
        });
        xdg.send_pending_configure();
    }

    fn relative_motion(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        focus: Option<(<Wind as SeatHandler>::PointerFocus, Point<f64, Logical>)>,
        event: &smithay::input::pointer::RelativeMotionEvent,
    ) {
        handle.relative_motion(data, focus, event);
    }

    fn button(&mut self, data: &mut Wind, handle: &mut PointerInnerHandle<'_, Wind>, event: &smithay::input::pointer::ButtonEvent) {
        handle.button(data, event);
        const BTN_LEFT: u32 = 0x110;
        if !handle.current_pressed().contains(&BTN_LEFT) {
            handle.unset_grab(self, data, event.serial, event.time, true);
        }

        let xdg = self.window.toplevel().unwrap();
        xdg.with_pending_state(|state| {
            state.states.unset(xdg_toplevel::State::Resizing);
            state.size = Some(self.last_window_size);
        });

        xdg.send_pending_configure();

        ResizeSurfaceState::WaitingForLastCommit { 
            edges: self.edges, initial_rect: self.initial_rect,
        }.into_userdata(xdg.wl_surface());
    }

    fn axis(&mut self, data: &mut Wind, handle: &mut PointerInnerHandle<'_, Wind>, details: smithay::input::pointer::AxisFrame) {
        handle.axis(data, details);
    }

    fn frame(&mut self, data: &mut Wind, handle: &mut PointerInnerHandle<'_, Wind>) {
        handle.frame(data);
    }

    fn gesture_swipe_begin(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        event: &GestureSwipeBeginEvent,
    ) {
        handle.gesture_swipe_begin(data, event);
    }

    fn gesture_swipe_update(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        event: &GestureSwipeUpdateEvent,
    ) {
        handle.gesture_swipe_update(data, event);
    }

    fn gesture_swipe_end(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        event: &GestureSwipeEndEvent,
    ) {
        handle.gesture_swipe_end(data, event);
    }

    fn gesture_pinch_begin(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        event: &GesturePinchBeginEvent,
    ) {
        handle.gesture_pinch_begin(data, event);
    }

    fn gesture_pinch_update(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        event: &GesturePinchUpdateEvent,
    ) {
        handle.gesture_pinch_update(data, event);
    }

    fn gesture_pinch_end(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        event: &GesturePinchEndEvent,
    ) {
        handle.gesture_pinch_end(data, event);
    }

    fn gesture_hold_begin(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        event: &smithay::input::pointer::GestureHoldBeginEvent,
    ) {
        handle.gesture_hold_begin(data, event);
    }

    fn gesture_hold_end(
        &mut self,
        data: &mut Wind,
        handle: &mut PointerInnerHandle<'_, Wind>,
        event: &smithay::input::pointer::GestureHoldEndEvent,
    ) {
        handle.gesture_hold_end(data, event);
    }

    fn start_data(&self) -> &GrabStartData<Wind> {
        &self.start_data
    }

    fn unset(&mut self, _data: &mut Wind) {}
}

#[derive(Default)]
enum ResizeSurfaceState {
    #[default]
    Idle,
    Resizing {
        edges: ResizeEdge,
        initial_rect: Rectangle<i32, Logical>,
    },
    WaitingForLastCommit {
        edges: ResizeEdge,
        initial_rect: Rectangle<i32, Logical>,
    },
}

impl ResizeSurfaceState {
    fn into_userdata(self, surface: &WlSurface)
    {
        compositor::with_states(surface, |states| {
            states.data_map.insert_if_missing(RefCell::<Self>::default);
        })
    }

    fn commit(&mut self) -> Option<(ResizeEdge, Rectangle<i32, Logical>)> {
        match *self {
            Self::Resizing{edges, initial_rect} => Some((edges, initial_rect)),
            Self::WaitingForLastCommit { edges, initial_rect } => {
                *self = Self::Idle;
                Some((edges, initial_rect))
            },
            Self::Idle => None,
        }
    }
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct ResizeEdge: u32 {
        const TOP    = 0b0001;
        const BOTTOM = 0b0010;
        const LEFT   = 0b0100;
        const RIGHT  = 0b1000;

        const TOP_LEFT    = Self::TOP.bits() | Self::LEFT.bits();
        const BOTTOM_LEFT = Self::BOTTOM.bits() | Self::LEFT.bits();

        const TOP_RIGHT    = Self::TOP.bits() | Self::RIGHT.bits();
        const BOTTOM_RIGHT = Self::BOTTOM.bits() | Self::RIGHT.bits();
    }
}

impl From<xdg_toplevel::ResizeEdge> for ResizeEdge {
    fn from(x: xdg_toplevel::ResizeEdge) -> Self {
        Self::from_bits(x as u32).unwrap()
    }
}

impl Wind {
    pub fn process_input_event<I: InputBackend>(&mut self, event: InputEvent<I>) {
        match event {
            InputEvent::Keyboard { event, .. } => {
                let serial = SERIAL_COUNTER.next_serial();
                let time = Event::time_msec(&event);
                self.seat.get_keyboard().unwrap().input::<(), _>(
                    self,
                    event.key_code(),
                    event.state(),
                    serial,
                    time,
                    |_, _, _| smithay::input::keyboard::FilterResult::Forward,
                );
            },
            InputEvent::PointerMotion { .. } => {},
            InputEvent::PointerMotionAbsolute { event, .. } => {
                let output = self.space.outputs().next().unwrap();
                let output_geo = self.space.output_geometry(output).unwrap();
                let pos = event.position_transformed(output_geo.size) + output_geo.loc.to_f64();
                let serial = SERIAL_COUNTER.next_serial();
                let pointer = self.seat.get_pointer().unwrap();
                let under = self.surface_under(pos);
                pointer.motion(
                    self,
                    under,
                    &MotionEvent {
                        location: pos,
                        serial,
                        time: event.time_msec(),
                    },
                );
                pointer.frame(self);
            },
            InputEvent::PointerButton { event, .. } => {
                let pointer = self.seat.get_pointer().unwrap();
                let keyboard = self.seat.get_keyboard().unwrap();
                let serial = SERIAL_COUNTER.next_serial();
                let button = event.button_code();
                let button_state = event.state();
                if ButtonState::Pressed == button_state && !pointer.is_grabbed() {
                    if let Some((window, _)) = self.space
                        .element_under(pointer.current_location())
                        .map(|(w, l)| (w.clone(), l)) 
                    {
                        self.space.raise_element(&window, true);
                        keyboard.set_focus(
                            self,
                            Some(window.toplevel().unwrap().wl_surface().clone()),
                            serial,
                        );
                        for win in self.space.elements() {
                            win.toplevel().unwrap().send_pending_configure();
                        }
                    } else {
                        for win in self.space.elements() {
                            win.set_activated(false);
                            win.toplevel().unwrap().send_pending_configure();
                        }
                        keyboard.set_focus(self, None, serial);
                    }
                }

                pointer.button(
                    self,
                    &ButtonEvent {
                        button,
                        state: button_state,
                        serial,
                        time: event.time_msec(),
                    }
                );
                pointer.frame(self);
            },
            InputEvent::PointerAxis { event, .. } => {
                let source = event.source();
                let horizontal_amount = event.amount(Axis::Horizontal)
                    .unwrap_or(event.amount_v120(Axis::Horizontal).unwrap_or(0.) * 15.0 / 120.);
                let vertical_amount = event.amount(Axis::Vertical)
                    .unwrap_or(event.amount_v120(Axis::Vertical).unwrap_or(0.) * 15.0 / 120.);
                let horizontal_amount_discrete = event.amount_v120(Axis::Horizontal);
                let vertical_amount_discrete = event.amount_v120(Axis::Vertical);
                let mut frame = AxisFrame::new(event.time_msec()).source(source);
                
                if horizontal_amount != 0.0 {
                    frame = frame.value(Axis::Horizontal, horizontal_amount);
                    if let Some(discrete) = horizontal_amount_discrete {
                        frame = frame.v120(Axis::Horizontal, discrete as i32);
                    }
                }

                if vertical_amount != 0.0 {
                    frame = frame.value(Axis::Vertical, vertical_amount);
                    if let Some(discrete) = vertical_amount_discrete {
                        frame = frame.v120(Axis::Vertical, discrete as i32);
                    }
                }

                if source == AxisSource::Finger {
                    if event.amount(Axis::Horizontal) == Some(0.0) {
                        frame = frame.stop(Axis::Horizontal);
                    }
                    if event.amount(Axis::Vertical) == Some(0.0) {
                        frame = frame.stop(Axis::Vertical);
                    }
                }

                let pointer = self.seat.get_pointer().unwrap();
                pointer.axis(self, frame);
                pointer.frame(self);
            },
            _ => {},
        }
    }

    pub fn surface_under(&self, pos: Point<f64, Logical>) -> Option<(WlSurface, Point<f64, Logical>)> {
        self.space.element_under(pos).and_then(|(window, location)| {
            window
                .surface_under(pos - location.to_f64(), WindowSurfaceType::ALL)
                .map(|(s, p)| (s, (p + location).to_f64()))
        })
    }
}
