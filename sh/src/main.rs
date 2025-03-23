//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::cli::{parse_args, ExecutionMode};
use crate::shell::Shell;
use crate::signals::setup_signal_handling;
use crate::utils::is_process_in_foreground;
use atty::Stream;
use nix::libc;
use nix::sys::signal::{sigaction, SaFlags, SigAction, SigSet};
use nix::sys::signal::{SigHandler, Signal as NixSignal};
use std::io;
use std::os::fd::AsFd;
use std::time::Duration;

mod builtin;
mod cli;
mod jobs;
mod nonempty;
mod option_parser;
mod parse;
mod shell;
mod signals;
mod utils;
mod wordexp;

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

fn interactive_shell(shell: &mut Shell) {
    if is_process_in_foreground() {
        let pgid = nix::unistd::getpgrp();
        nix::unistd::tcsetpgrp(io::stdin().as_fd(), pgid).unwrap();
    }
    nix::fcntl::fcntl(
        libc::STDIN_FILENO,
        nix::fcntl::FcntlArg::F_SETFL(nix::fcntl::OFlag::O_NONBLOCK),
    )
    .unwrap();
    let ignore_action = SigAction::new(SigHandler::SigIgn, SaFlags::empty(), SigSet::empty());
    unsafe {
        sigaction(NixSignal::SIGQUIT, &ignore_action).unwrap();
    }
    unsafe {
        sigaction(NixSignal::SIGTERM, &ignore_action).unwrap();
    }
    if shell.set_options.monitor {
        // job control signals
        unsafe {
            sigaction(NixSignal::SIGTTIN, &ignore_action).unwrap();
        }
        unsafe {
            sigaction(NixSignal::SIGTTOU, &ignore_action).unwrap();
        }
        unsafe {
            sigaction(NixSignal::SIGTSTP, &ignore_action).unwrap();
        }
    }
    let mut buffer = String::new();
    eprint!("{}", shell.get_ps1());
    loop {
        while io::stdin().read_line(&mut buffer).is_ok_and(|n| n > 0) {
            if buffer.ends_with("\\\n") {
                continue;
            }
            match shell.execute_program(&buffer) {
                Ok(_) => {
                    buffer.clear();
                    eprint!("{}", shell.get_ps1());
                }
                Err(syntax_err) => {
                    if !syntax_err.could_be_resolved_with_more_input {
                        eprintln!("sh: syntax error: {}", syntax_err.message);
                        buffer.clear();
                        eprint!("{}", shell.get_ps1());
                    } else {
                        eprint!("{}", shell.get_ps2());
                    }
                }
            }
        }
        std::thread::sleep(Duration::from_millis(16));
        shell.update_global_state();
    }
}

fn main() {
    let is_attached_to_terminal = atty::is(Stream::Stdin) && atty::is(Stream::Stdout);
    let args = parse_args(std::env::args().collect(), is_attached_to_terminal).unwrap();
    let mut shell = Shell::initialize_from_system(
        args.program_name,
        args.arguments,
        args.set_options,
        args.execution_mode == ExecutionMode::Interactive,
    );
    unsafe { setup_signal_handling() };
    match args.execution_mode {
        ExecutionMode::Interactive => interactive_shell(&mut shell),
        ExecutionMode::ReadCommandsFromStdin => {
            let mut buffer = String::new();
            while io::stdin().read_line(&mut buffer).is_ok_and(|n| n > 0) {
                if buffer.ends_with("\\\n") {
                    continue;
                }
                match shell.execute_program(&buffer) {
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
                execute_string(&command_string, &mut shell);
            }
            ExecutionMode::ReadFromFile(file) => {
                let file_contents = std::fs::read_to_string(file).expect("could not read file");
                execute_string(&file_contents, &mut shell);
            }
            _ => unreachable!(),
        },
    }
    shell.exit(shell.last_pipeline_exit_status);
}
