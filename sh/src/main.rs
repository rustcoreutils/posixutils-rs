//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::trap::TrapAction;
use crate::cli::{parse_args, ExecutionMode};
use crate::shell::Shell;
use crate::signals::{setup_signal_handling, Signal};
use atty::Stream;
use nix::libc;
use std::io;
use std::os::fd::RawFd;
use std::time::Duration;

mod builtin;
mod cli;
mod nonempty;
mod parse;
mod program;
mod shell;
mod signals;
mod utils;
mod wordexp;

static mut GLOBAL_SHELL: Option<Shell> = None;

fn get_global_shell() -> &'static mut Shell {
    unsafe { GLOBAL_SHELL.as_mut().unwrap() }
}

extern "C" fn on_exit() {
    let action = get_global_shell().exit_action.clone();
    get_global_shell().execute_action(action);
}

fn execute_string(string: &str, shell: &mut Shell) {
    match shell.execute_program(string) {
        Ok(_) => {}
        Err(syntax_err) => {
            eprintln!(
                "sh({}): syntax error: {}",
                syntax_err.lineno, syntax_err.message
            );
            // both bash and sh use 2 as the exit code for a syntax error
            std::process::exit(2);
        }
    }
}

fn main() {
    let is_attached_to_terminal = atty::is(Stream::Stdin) && atty::is(Stream::Stdout);
    let args = parse_args(std::env::args().collect(), is_attached_to_terminal).unwrap();
    unsafe {
        GLOBAL_SHELL = Some(Shell::initialize_from_system(
            args.program_name,
            args.arguments,
            args.set_options,
            args.execution_mode == ExecutionMode::Interactive,
        ))
    };
    unsafe { libc::atexit(on_exit) };
    unsafe { setup_signal_handling() };
    match args.execution_mode {
        ExecutionMode::Interactive => {
            nix::fcntl::fcntl(
                libc::STDIN_FILENO,
                nix::fcntl::FcntlArg::F_SETFL(nix::fcntl::OFlag::O_NONBLOCK),
            )
            .unwrap();
            let mut buffer = String::new();
            eprint!("{}", get_global_shell().get_ps1());
            loop {
                while io::stdin().read_line(&mut buffer).is_ok_and(|n| n > 0) {
                    if buffer.ends_with("\\\n") {
                        continue;
                    }
                    match get_global_shell().execute_program(&buffer) {
                        Ok(_) => {
                            buffer.clear();
                            eprint!("{}", get_global_shell().get_ps1());
                        }
                        Err(syntax_err) => {
                            if !syntax_err.could_be_resolved_with_more_input {
                                eprintln!("sh: syntax error: {}", syntax_err.message);
                                buffer.clear();
                                eprint!("{}", get_global_shell().get_ps1());
                            } else {
                                eprint!("{}", get_global_shell().get_ps2());
                            }
                        }
                    }
                }
                std::thread::sleep(Duration::from_millis(16));
                get_global_shell().process_signals();
            }
        }
        ExecutionMode::ReadCommandsFromStdin => {
            let mut buffer = String::new();
            while io::stdin().read_line(&mut buffer).is_ok_and(|n| n > 0) {
                if buffer.ends_with("\\\n") {
                    continue;
                }
                match get_global_shell().execute_program(&buffer) {
                    Ok(_) => {
                        buffer.clear();
                    }
                    Err(syntax_err) => {
                        if !syntax_err.could_be_resolved_with_more_input {
                            eprintln!(
                                "sh({}): syntax error: {}",
                                syntax_err.lineno, syntax_err.message
                            );
                            std::process::exit(2);
                        }
                    }
                }
            }
        }
        other => match other {
            ExecutionMode::ReadCommandsFromString(command_string) => {
                execute_string(&command_string, get_global_shell());
            }
            ExecutionMode::ReadFromFile(file) => {
                let file_contents = std::fs::read_to_string(file).expect("could not read file");
                execute_string(&file_contents, get_global_shell());
            }
            _ => unreachable!(),
        },
    }
    std::process::exit(get_global_shell().last_pipeline_exit_status);
}
