use std::time::Duration;

use smithay::{backend::renderer::{damage::OutputDamageTracker, element::surface::WaylandSurfaceRenderElement, gles::GlesRenderer}, desktop::utils::{surface_presentation_feedback_flags_from_states, surface_primary_scanout_output, OutputPresentationFeedback}, input::pointer::CursorImageStatus, output::PhysicalProperties, reexports::{
        calloop::EventLoop, wayland_protocols::wp::presentation_time::server::wp_presentation_feedback, wayland_server::{Display, DisplayHandle}, winit::platform::pump_events::PumpStatus
    }, utils::{Rectangle, Scale}, wayland::presentation::Refresh};
use state::Wind;
use smithay::{
    backend::winit::{self, WinitEvent},
    output::{Output, Mode}
};

mod state;

fn main() -> Result<(), ()> {
    tracing_subscriber::fmt().with_max_level(tracing::Level::DEBUG).init();

    let mut event_loop = EventLoop::<CalloopData>::try_new().expect("to create an event loop");
    let display = Display::<Wind>::new().expect("to create a display");
    let display_handle = display.handle();
    let state = Wind::new(&mut event_loop, display);
    let mut data = CalloopData {
        state,
        display_handle,
    };

    run_winit(event_loop, data);

    Ok(())
}

pub struct CalloopData {
    pub display_handle: DisplayHandle,
    pub state: Wind,
}

fn run_winit(
    mut event_loop: EventLoop<CalloopData>,
    mut data: CalloopData,
) {
    let (mut backend, mut winit) = winit::init::<GlesRenderer>().expect("failed creating window");
    let mode = Mode{
        size: backend.window_size(),
        refresh: 120_000,
    };

    let output = Output::new(
        "winit".to_string(),
        PhysicalProperties {
            size: (0, 0).into(),
            subpixel: smithay::output::Subpixel::Unknown,
            make: "Smithay".into(),
            model: "Winit".into(),
        },
    );
    let dh = data.display_handle.clone();
    output.create_global::<Wind>(&dh);
    output.change_current_state(Some(mode), Some(smithay::utils::Transform::Flipped180), None, Some((0, 0).into()));
    output.set_preferred(mode);
    data.state.space.map_output(&output, (0, 0));
    let mut damage_tracker = OutputDamageTracker::from_output(&output);
    std::env::set_var("WAYLAND_DISPLAY", &data.state.socket_name);
    println!("{:?}", &data.state.socket_name);

    let mut running = true;

    let clock = smithay::utils::Clock::<smithay::utils::Monotonic>::new();
    while running {
        let status = winit.dispatch_new_events(|event| match event {
            WinitEvent::Resized { size, .. } => {
                output.change_current_state(
                    Some(Mode{
                        size,
                        refresh: 120_000,
                    }), 
                    None, 
                    None, 
                    None,
                );
            },
            WinitEvent::Input(event) => data.state.process_input_event(event),
            WinitEvent::CloseRequested => {
                data.state.loop_signal.stop();
                running = false;
            },
            _ => {},
        });

        if let PumpStatus::Exit(_) = status {
            break;
        }

        let now = clock.now();
        let frame_target = now + output
            .current_mode()
            .map(|mode| Duration::from_secs_f64(1_000f64 / mode.refresh as f64))
            .unwrap_or_default();
        data.state.pre_repaint(&output, frame_target);

        let render_res = backend.bind().and_then(|_| {
            let age = backend.buffer_age().unwrap_or(0);
            let renderer = backend.renderer();
            Ok(smithay::desktop::space::render_output::<_, WaylandSurfaceRenderElement<GlesRenderer>, _, _>(
                &output, 
                renderer, 
                1.0, 
                age, 
                [&data.state.space], 
                &[], 
                &mut damage_tracker, 
                [0.1, 0.1, 0.1, 1.0]
            ).unwrap())
        });

        match render_res {
            Ok(render_output_result) => 'damage: {
                let Some(damage) = render_output_result.damage else {
                    break 'damage;
                };
                println!("ddddddddamage");
                backend.submit(Some(damage)).unwrap();
                let mut output_presentation_feedback = OutputPresentationFeedback::new(&output);
                for wind in data.state.space.elements() {
                    if data.state.space.outputs_for_element(wind).contains(&output) {
                        wind.take_presentation_feedback(
                            &mut output_presentation_feedback, 
                            surface_primary_scanout_output, 
                            |surface, _| surface_presentation_feedback_flags_from_states(surface, &render_output_result.states),
                        );
                    }
                }
                let map = smithay::desktop::layer_map_for_output(&output);
                for layer_surface in map.layers() {
                    layer_surface.take_presentation_feedback(
                        &mut output_presentation_feedback, 
                        surface_primary_scanout_output, 
                        |surface, _| surface_presentation_feedback_flags_from_states(surface, &render_output_result.states),
                    );
                }
                std::mem::drop(map);
                output_presentation_feedback.presented(
                    frame_target, 
                    output.current_mode().map(|mode| {
                        Refresh::fixed(Duration::from_secs_f64(1_000f64 / mode.refresh as f64))
                    }).unwrap_or(Refresh::Unknown), 
                    0, 
                    wp_presentation_feedback::Kind::Vsync,
                );

                data.state.post_repaint(&output, frame_target, None, &render_output_result.states);
            },
            Err(smithay::backend::SwapBuffersError::ContextLost(err)) => {
                println!("swap buffers: {err}");
            },
            Err(err) => {
                println!("generic: {err}");
            },
        }

        match event_loop.dispatch(Some(std::time::Duration::from_millis(1)), &mut data) {
            Ok(_) => {
                println!("dispatch");
                let mut dh = data.display_handle.clone();
                data.state.space.refresh();
                data.state.popups.cleanup();
                dh.flush_clients().unwrap();
            },
            Err(err) => {
                println!("failed dispatch: {err}");
                return;
            },
        }
        println!("loop end");
    }
}
