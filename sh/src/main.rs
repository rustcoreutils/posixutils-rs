//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::cli::args::{parse_args, ExecutionMode};
use crate::cli::terminal::is_attached_to_terminal;
use crate::cli::{clear_line, set_cursor_pos};
use crate::shell::Shell;
use crate::signals::setup_signal_handling;
use crate::utils::is_process_in_foreground;
use cli::terminal::read_nonblocking_char;
use cli::vi::{Action, ViEditor};
use nix::sys::signal::{sigaction, SaFlags, SigAction, SigSet};
use nix::sys::signal::{SigHandler, Signal as NixSignal};
use std::io;
use std::io::Write;
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

fn print_line(line: &[u8], mut cursor_position: usize, shell: &mut Shell, print_ps2: bool) {
    clear_line();
    if print_ps2 {
        let ps2 = shell.get_ps2();
        print!("{}", ps2);
        cursor_position += ps2.len();
    } else {
        let ps1 = shell.get_ps1();
        print!("{}", ps1);
        cursor_position += ps1.len();
    }
    std::io::stdout().write(&line).unwrap();
    set_cursor_pos(cursor_position);
    io::stdout().flush().unwrap();
}

fn standard_repl(shell: &mut Shell) {
    let mut buffer = Vec::new();
    let mut print_ps2 = false;
    clear_line();
    io::stdout().flush().unwrap();
    eprint!("{}", shell.get_ps1());
    loop {
        while let Some(c) = read_nonblocking_char() {
            match c {
                b'\x7F' => {
                    if !buffer.is_empty() {
                        buffer.pop();
                    }
                }
                b'\x04' => {
                    // EOF
                    shell.exit(shell.last_pipeline_exit_status);
                }
                b'\n' => {
                    buffer.push(b'\n');
                    if buffer.ends_with(b"\\\n") {
                        continue;
                    }
                    let program_string = match std::str::from_utf8(&buffer) {
                        Ok(buf) => buf,
                        Err(_) => {
                            eprintln!("sh: invalid utf-8 sequence");
                            buffer.clear();
                            continue;
                        }
                    };
                    print!("\n");
                    shell.terminal.reset();
                    match shell.execute_program(program_string) {
                        Ok(_) => {
                            buffer.clear();
                            print_ps2 = false;
                        }
                        Err(syntax_err) => {
                            if !syntax_err.could_be_resolved_with_more_input {
                                eprintln!("sh: syntax error: {}", syntax_err.message);
                                buffer.clear();
                                print_ps2 = false;
                            } else {
                                print_ps2 = true;
                            }
                        }
                    }
                    shell.terminal.set_nonblocking_no_echo();
                }
                other if !other.is_ascii_control() => {
                    buffer.push(other);
                }
                _ => {}
            }
            print_line(&buffer, buffer.len(), shell, print_ps2);
        }
        std::thread::sleep(Duration::from_millis(16));
        shell.update_global_state();
        if shell.set_options.vi {
            return;
        }
    }
}

fn vi_repl(shell: &mut Shell) {
    let mut editor = ViEditor::default();
    let mut buffer = Vec::new();
    let mut print_ps2 = false;
    clear_line();
    io::stdout().flush().unwrap();
    eprint!("{}", shell.get_ps1());
    loop {
        while let Some(c) = read_nonblocking_char() {
            match editor.process_new_input(c, shell) {
                Ok(Action::Execute(command)) => {
                    buffer.extend(command.iter());
                    if buffer.ends_with(b"\\\n") {
                        continue;
                    }
                    let program_string = match std::str::from_utf8(&buffer) {
                        Ok(buf) => buf,
                        Err(_) => {
                            eprintln!("sh: invalid utf-8 sequence");
                            buffer.clear();
                            continue;
                        }
                    };
                    print!("\n");
                    shell.terminal.reset();
                    match shell.execute_program(program_string) {
                        Ok(_) => {
                            buffer.clear();
                            print_ps2 = false;
                        }
                        Err(syntax_err) => {
                            if !syntax_err.could_be_resolved_with_more_input {
                                eprintln!("sh: syntax error: {}", syntax_err.message);
                                buffer.clear();
                            } else {
                                print_ps2 = true;
                            }
                        }
                    }
                    shell.terminal.set_nonblocking_no_echo();
                }
                Ok(Action::Eof) => shell.exit(shell.last_pipeline_exit_status),
                Ok(Action::Redraw) => {
                    // nothing, we redraw anyway
                }
                Ok(Action::None) => {}
                Err(_) => {
                    print!("\x07");
                }
            }
            print_line(
                &editor.current_line(),
                editor.cursor_position(),
                shell,
                print_ps2,
            );
        }
        std::thread::sleep(Duration::from_millis(16));
        shell.update_global_state();
        if !shell.set_options.vi {
            return;
        }
    }
}

fn interactive_shell(shell: &mut Shell) {
    if is_process_in_foreground() {
        let pgid = nix::unistd::getpgrp();
        nix::unistd::tcsetpgrp(io::stdin().as_fd(), pgid).unwrap();
    }
    shell.terminal.set_nonblocking_no_echo();
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
    loop {
        if shell.set_options.vi {
            vi_repl(shell);
        } else {
            standard_repl(shell);
        }
    }
}

fn main() {
    let args = parse_args(std::env::args().collect(), is_attached_to_terminal()).unwrap();
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
