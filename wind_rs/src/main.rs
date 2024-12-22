use std::collections::{HashSet, BinaryHeap};
use std::cmp::Reverse;
use std::thread::sleep;
use simplelog::*;
use log::{info, warn, error, debug, trace};
use x11rb::{
    CURRENT_TIME,
    connection::Connection, 
    protocol::{Event, xproto::*}, 
    COPY_DEPTH_FROM_PARENT
};
use anyhow::Result;

const DRAG_BUTTON: Button = 1;
const TITLEBAR_HEIGHT: u16 = 20;

fn main() -> Result<()> {
    TermLogger::init(LevelFilter::Trace, Config::default(), TerminalMode::Stderr, ColorChoice::Auto).unwrap();
    info!("starting up");
    let (conn, screen_num) = x11rb::connect(None)?;
    let screen = &conn.setup().roots[screen_num];
    become_wm(&conn, screen).expect("failed elevating to window manager");
    info!("elevated to window manager");
    let mut wm_state = WmState::new(&conn, screen_num)?;
    wm_state.scan_windows()?;

    loop {
        wm_state.refresh();
        debug!("refreshed wm_state");
        conn.flush()?;
        trace!("flushed conn");

        let event = conn.wait_for_event()?;
        debug!("next event");
        let mut event_option = Some(event);
        while let Some(event) = event_option {
            debug!("handling event");
            if let Err(err) = wm_state.handle_event(event) {
                error!("Failed handling event: {err}");
            }
            debug!("polling for event");
            event_option = conn.poll_for_event()?;
        }
        trace!("finished polling");
    }
}

fn become_wm(conn: &impl Connection, screen: &Screen) -> Result<()> {
    let change = ChangeWindowAttributesAux::default()
        .event_mask(EventMask::SUBSTRUCTURE_REDIRECT | EventMask::SUBSTRUCTURE_NOTIFY);
    conn.change_window_attributes(screen.root, &change)?.check()?;
    Ok(())
}

struct WmState<'a, C: Connection> {
    conn: &'a C,
    screen_num: usize,
    black_gc: Gcontext,
    windows: Vec<WindowState>,
    pending_expose: HashSet<Window>,
    wm_protocols: Atom,
    wm_delete_window: Atom,
    sequences_to_ignore: BinaryHeap<Reverse<u16>>,
    drag_window: Option<(Window, (i16, i16))>,
}

impl<'a, C: Connection> WmState<'a, C> {
    fn new(conn: &'a C, screen_num: usize) -> Result<WmState<'a, C>> {
        let screen = &conn.setup().roots[screen_num];
        let black_gc = conn.generate_id()?;
        let font = conn.generate_id()?;
        conn.open_font(font, b"9x15")?;
        let gc_aux = CreateGCAux::new()
            .graphics_exposures(0)
            .background(screen.white_pixel)
            .background(screen.black_pixel)
            .font(font);
        conn.create_gc(black_gc, screen.root, &gc_aux)?;
        conn.close_font(font)?;
        
        let wm_protocols = conn.intern_atom(false, b"WM_PROTOCOLS")?;
        let wm_delete_window = conn.intern_atom(false, b"WM_DELETE_WINDOW")?;
        Ok(Self{
            conn,
            screen_num,
            black_gc,
            windows: vec![],
            pending_expose: HashSet::new(),
            wm_protocols: wm_protocols.reply()?.atom,
            wm_delete_window: wm_delete_window.reply()?.atom,
            sequences_to_ignore: BinaryHeap::new(),
            drag_window: None,
        })
    }

    fn scan_windows(&mut self) -> Result<()> {
        let screen = &self.conn.setup().roots[self.screen_num];
        let tree_reply = self.conn.query_tree(screen.root)?.reply()?;
        for win in tree_reply.children {
            let attr = self.conn.get_window_attributes(win)?;
            let geom = self.conn.get_geometry(win)?;
            if let (Ok(_), Ok(geom)) = (attr.reply(), geom.reply()) {
                self.manage_window(win, &geom)?;
            } else {
                warn!("Skipping window {win}, failed getting attributes and geometry");
            }
        }

        Ok(())
    }

    fn manage_window(&mut self, win: Window, geom: &GetGeometryReply) -> Result<()> {
        debug!("Managing window {win}");
        let screen = &self.conn.setup().roots[self.screen_num];
        if self.window_by_id(win).is_some() {
            warn!("Requested to manage window {win} which is already being managed");
            return Ok(());
        }

        let frame_win = self.conn.generate_id()?;
        debug!("created frame {frame_win}");
        let win_aux = CreateWindowAux::new()
            .event_mask(
                EventMask::EXPOSURE |
                EventMask::SUBSTRUCTURE_NOTIFY |
                EventMask::BUTTON_PRESS |
                EventMask::BUTTON_RELEASE |
                EventMask::POINTER_MOTION |
                EventMask::ENTER_WINDOW,
            )
            .background_pixel(screen.white_pixel);
        self.conn.create_window(
            COPY_DEPTH_FROM_PARENT,
            frame_win,
            screen.root,
            geom.x,
            geom.y,
            geom.width,
            screen.height_in_pixels,
            1,
            WindowClass::INPUT_OUTPUT,
            0,
            &win_aux,
        )?;
        self.conn.configure_window(win, &ConfigureWindowAux::new()
            .height((screen.height_in_pixels - TITLEBAR_HEIGHT) as u32)
        )?;
        trace!("waiting to grab server");
        self.conn.grab_server()?;
        trace!("grabbed server");
        self.conn.change_save_set(SetMode::INSERT, win)?;
        let cookie = self.conn.reparent_window(win, frame_win, 0, TITLEBAR_HEIGHT.try_into()?)?;
        self.conn.map_window(win)?;
        self.conn.map_window(frame_win)?;
        self.conn.ungrab_server()?;
        trace!("freed server");
        self.windows.push(WindowState::new(win, frame_win, geom));
        self.sequences_to_ignore.push(Reverse(cookie.sequence_number().try_into()?));
        Ok(())
    }

    fn draw_titlebar(&self, state: &WindowState) -> Result<()> {
        let close_x = state.close_x_position();
        self.conn.poly_line(
            CoordMode::ORIGIN,
            state.frame_window,
            self.black_gc,
            &[
                Point{x: close_x, y: 0},
                Point{x: state.width.try_into()?, y: TITLEBAR_HEIGHT.try_into()?},
            ],
        )?;
        self.conn.poly_line(
            CoordMode::ORIGIN,
            state.frame_window,
            self.black_gc,
            &[
                Point{x: close_x, y: TITLEBAR_HEIGHT.try_into()?},
                Point{x: state.width.try_into()?, y: 0},
            ]
        )?;
        let reply = self.conn.get_property(
            false,
            state.window,
            AtomEnum::WM_NAME,
            AtomEnum::STRING,
            0,
            u32::MAX,
        )?.reply()?;
        self.conn.image_text8(state.frame_window, self.black_gc, 1, 10, &reply.value)?;
        Ok(())
    }

    fn refresh(&mut self) {
        trace!("refresh");
        for &win in self.pending_expose.iter() {
            if let Some(state) = self.window_by_id(win) {
                if let Err(err) = self.draw_titlebar(state) {
                    warn!("Failed redrawing window {:x?} {:?}", state.window, err);
                }
            }
        }
        self.pending_expose.clear();
    }

    fn window_by_id(&self, win: Window) -> Option<&WindowState> {
        self.windows
            .iter()
            .find(|state| state.window == win || state.frame_window == win)
    }

    fn handle_event(&mut self, event: Event) -> Result<()> {
        if let Some(seqno) = event.wire_sequence_number() {
            while let Some(&Reverse(to_ignore)) = self.sequences_to_ignore.peek() {
                if to_ignore.wrapping_sub(seqno) <= u16::MAX / 2 {
                    if to_ignore == seqno {
                        debug!("ignoring event: {:?}", event);
                        return Ok(());
                    } else {
                        break;
                    }
                }
                self.sequences_to_ignore.pop();
            }
        }
        debug!("processing event: {:?}", event);

        match event {
            Event::UnmapNotify(ev) => self.handle_unmap_notify(ev),
            Event::ConfigureRequest(ev) => self.handle_configure_request(ev),
            Event::MapRequest(ev) => self.handle_map_request(ev),
            Event::Expose(ev) => self.handle_expose(ev),
            Event::EnterNotify(ev) => self.handle_enter_notify(ev),
            Event::ButtonPress(ev) => self.handle_button_press(ev),
            Event::ButtonRelease(ev) => self.handle_button_release(ev),
            Event::MotionNotify(ev) => self.handle_motion_notify(ev),
            _ => Ok(())
        }
    }

    fn handle_unmap_notify(&mut self, ev: UnmapNotifyEvent) -> Result<()> {
        debug!("handle unmap notifiy");
        let root = self.conn.setup().roots[self.screen_num].root;
        for state in self.windows.iter() {
            if state.window != ev.window {
                continue;
            }

            self.conn.change_save_set(SetMode::DELETE, state.window)?;
            self.conn.reparent_window(state.window, root, state.x, state.y)?;
            self.conn.destroy_window(state.frame_window)?;
        }
        self.windows.retain(|state| state.window != ev.window);
        Ok(())
    }

    fn handle_configure_request(&mut self, ev: ConfigureRequestEvent) -> Result<()> {
        debug!("handle configure request");
        if self.window_by_id(ev.window).is_some() {
            warn!("configure request for windows owned by wm not implemented");
            return Ok(());
        }

        // build screens in horizontal stack.
        let screen = &self.conn.setup().roots[self.screen_num];
        debug!("requested height: {}", ev.height);
        debug!("target height: {}", screen.height_in_pixels - TITLEBAR_HEIGHT);
        let n_windows = (self.windows.len() + 1) as u32;
        let float_width = f64::from(screen.width_in_pixels) / f64::from(n_windows);
        for (idx, win) in self.windows.iter().enumerate() {
            let win_x = float_width * f64::from(idx as u32);
            let next_win_x = float_width * f64::from((idx+1) as u32);
            let aux = ConfigureWindowAux::new()
                .x(win_x as i32)
                .width((next_win_x as u32) - (win_x as u32));
            self.conn.configure_window(win.frame_window, &aux)?;
        }
        let win_x = float_width * f64::from(n_windows-1);
        let final_width = screen.width_in_pixels - 1;
        let aux = ConfigureWindowAux::from_configure_request(&ev)
            .sibling(None)
            .stack_mode(None)
            .x(win_x as i32)
            .width((final_width as u32) - (win_x as u32));
        debug!("{:?}", aux.height);
        self.conn.configure_window(ev.window, &aux)?;
        debug!("configuring window {}, {:?}", ev.window, aux);
        return Ok(());
    }

    fn handle_map_request(&mut self, ev: MapRequestEvent) -> Result<()> {
        debug!("handle map request");
        self.manage_window(
            ev.window,
            &self.conn.get_geometry(ev.window)?.reply()?,
        )
    }

    fn handle_expose(&mut self, ev: ExposeEvent) -> Result<()> {
        debug!("handle expose");
        self.pending_expose.insert(ev.window);
        return Ok(());
    }

    fn handle_enter_notify(&mut self, ev: EnterNotifyEvent) -> Result<()> {
        debug!("handle enter notify");
        if let Some(state) = self.window_by_id(ev.event) {
            self.conn.set_input_focus(InputFocus::PARENT, state.window, CURRENT_TIME)?;
            self.conn.configure_window(
                state.frame_window,
                &ConfigureWindowAux::new().stack_mode(StackMode::ABOVE),
            )?;
        }
        return Ok(());
    }

    fn handle_button_press(&mut self, ev: ButtonPressEvent) -> Result<()> {
        debug!("handle button press");
        if ev.detail != DRAG_BUTTON || u16::from(ev.state) != 0 {
            return Ok(());
        }

        if let Some(state) = self.window_by_id(ev.event) {
            if self.drag_window.is_none() && ev.event_x < state.close_x_position() {
                let (x, y) = (-ev.event_x, -ev.event_y);
                self.drag_window = Some((state.frame_window, (x, y)));
            }
        }

        return Ok(());
    }

    fn handle_button_release(&mut self, ev: ButtonReleaseEvent) -> Result<()> {
        debug!("handle button release");
        if ev.detail == DRAG_BUTTON {
            self.drag_window = None;
        }

        if let Some(state) = self.window_by_id(ev.event) {
            if ev.event_x >= state.close_x_position() {
                let event = ClientMessageEvent::new(
                    32,
                    state.window,
                    self.wm_protocols,
                    [self.wm_delete_window, 0, 0, 0, 0],
                );
                self.conn.send_event(false, state.window, EventMask::NO_EVENT, event)?;
            }
        }

        Ok(())
    }

    fn handle_motion_notify(&mut self, ev: MotionNotifyEvent) -> Result<()> {
        debug!("handle motion notify");
        if let Some((wind, (x, y))) = self.drag_window {
            let (x, y) = (x + ev.root_x, y + ev.root_y);
            self.conn.configure_window(wind, &ConfigureWindowAux::new().x(x as i32).y(y as i32))?;
        }

        Ok(())
    }
}

struct WindowState {
    window: Window,
    frame_window: Window,
    width: u16,
    x: i16,
    y: i16,
}

impl WindowState {
    fn new(window: Window, frame_window: Window, geom: &GetGeometryReply) -> Self {
        Self {
            window,
            frame_window,
            x: geom.x,
            y: geom.y,
            width: geom.width,
        }
    }

    fn close_x_position(&self) -> i16 {
        0.max(self.width - TITLEBAR_HEIGHT) as _
    }
}
