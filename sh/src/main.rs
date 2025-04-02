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
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use nix::sys::signal::{sigaction, SaFlags, SigAction, SigSet};
use nix::sys::signal::{SigHandler, Signal as NixSignal};
use std::error::Error;
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
pub mod pattern;
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

fn flush_stdout() {
    // this is a basic operation, if this doesn't work,
    // there's nothing else we can do
    io::stdout().flush().expect("could not flush stdout");
}

fn write_stdout(bytes: &[u8]) {
    io::stdout()
        .write_all(bytes)
        .expect("failed to write to stdout");
}

fn print_prompt(shell: &mut Shell, print_ps2: bool) -> usize {
    if print_ps2 {
        let ps2 = shell.get_ps2();
        print!("{}", ps2);
        ps2.len()
    } else {
        let ps1 = shell.get_ps1();
        print!("{}", ps1);
        ps1.len()
    }
}

fn standard_repl(shell: &mut Shell) {
    let mut program_buffer = Vec::new();
    let mut line_buffer = Vec::new();
    let mut print_ps2 = false;
    clear_line();
    flush_stdout();
    eprint!("{}", shell.get_ps1());
    loop {
        while let Some(c) = read_nonblocking_char() {
            match c {
                b'\x7F' => {
                    if !line_buffer.is_empty() {
                        line_buffer.pop();
                    }
                }
                b'\x04' => {
                    // EOF
                    shell.exit(shell.last_pipeline_exit_status);
                }
                b'\n' => {
                    line_buffer.push(b'\n');
                    program_buffer.extend(&line_buffer);
                    line_buffer.clear();
                    if program_buffer.ends_with(b"\\\n") {
                        continue;
                    }
                    let program_string = match std::str::from_utf8(&program_buffer) {
                        Ok(buf) => buf,
                        Err(_) => {
                            eprintln!("sh: invalid utf-8 sequence");
                            program_buffer.clear();
                            continue;
                        }
                    };
                    println!();
                    shell.terminal.reset();
                    match shell.execute_program(program_string) {
                        Ok(_) => {
                            program_buffer.clear();
                            print_ps2 = false;
                        }
                        Err(syntax_err) => {
                            if !syntax_err.could_be_resolved_with_more_input {
                                eprintln!("sh: syntax error: {}", syntax_err.message);
                                program_buffer.clear();
                                print_ps2 = false;
                            } else {
                                print_ps2 = true;
                            }
                        }
                    }
                    shell.terminal.set_nonblocking_no_echo();
                }
                other if !other.is_ascii_control() => {
                    line_buffer.push(other);
                }
                _ => {}
            }
            let mut cursor_position = line_buffer.len();
            clear_line();
            cursor_position += print_prompt(shell, print_ps2);
            write_stdout(&line_buffer);
            set_cursor_pos(cursor_position);
            flush_stdout();
        }
        std::thread::sleep(Duration::from_millis(16));
        shell.handle_async_events();
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
    flush_stdout();
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
                    println!();
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
            let mut cursor_position = editor.cursor_position();
            clear_line();
            cursor_position += print_prompt(shell, print_ps2);
            write_stdout(editor.current_line(shell));
            set_cursor_pos(cursor_position);
            flush_stdout()
        }
        std::thread::sleep(Duration::from_millis(16));
        shell.handle_async_events();
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

fn main() -> Result<(), Box<dyn Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = match parse_args(std::env::args().collect(), is_attached_to_terminal()) {
        Ok(args) => args,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    };
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
