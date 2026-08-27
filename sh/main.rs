//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::cli::args::{parse_args, ExecutionMode};
use crate::cli::terminal::is_attached_to_terminal;
use crate::cli::{clear_line, set_cursor_pos};
use crate::os::{getpgrp, is_process_in_foreground, tcsetpgrp, wait_for_input};
use crate::parse::ParserError;
use crate::shell::Shell;
use crate::shstr::{ShStr, ShString};
use cli::terminal::read_nonblocking_char;
use cli::vi::{Action, ViEditor};
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use os::signals::{
    handle_signal_ignore, handle_signal_write_to_signal_buffer, setup_signal_handling, Signal,
};
use std::error::Error;
use std::fs::File;
use std::io;
use std::io::{BufRead, IsTerminal, Read, Write};
use std::os::fd::AsRawFd;

mod builtin;
mod cli;
mod jobs;
mod nonempty;
mod option_parser;
mod os;
mod parse;
pub mod pattern;
mod shell;
mod shstr;
mod utils;
mod wordexp;

/// Reads a `command_file` operand, pairing each failure with the exit status
/// POSIX prescribes for it: 127 when the file could not be found, 126 when it
/// was found but cannot serve as a script, and 128 only for an unrecoverable
/// read error. The distinction is which step failed, not which errno came
/// back, so the open and the read are done separately.
fn read_command_file(path: &ShStr) -> Result<Vec<u8>, (io::Error, i32)> {
    let mut file = match File::open(path) {
        Ok(file) => file,
        Err(err) => {
            let status = if err.kind() == io::ErrorKind::NotFound {
                127
            } else {
                126
            };
            return Err((err, status));
        }
    };
    // Opening a directory succeeds; it still cannot be executed, so it is a
    // 126 rather than the read error it would otherwise look like.
    if file.metadata().is_ok_and(|metadata| metadata.is_dir()) {
        return Err((io::Error::other(gettext("Is a directory")), 126));
    }
    // Read as bytes: a script is not required to be text, and refusing one
    // that contains a byte which is not part of a character would make the
    // shell unable to run scripts other shells run.
    let mut contents = Vec::new();
    match file.read_to_end(&mut contents) {
        Ok(_) => Ok(contents),
        Err(err) => Err((err, 128)),
    }
}

fn execute_string(string: &[u8], shell: &mut Shell) {
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

/// Writes any pending mail notifications to stderr before a prompt.
fn report_mail(shell: &mut Shell) {
    for message in shell.check_mail() {
        eprintln!("{message}");
    }
}

/// The interactive editor's own output goes to standard error, so that a shell
/// whose stdout is redirected does not have its prompt and echoed input mixed
/// into the redirected stream.
fn flush_terminal() {
    let _ = io::stderr().flush();
}

fn write_terminal(bytes: &[u8]) {
    let _ = io::stderr().write_all(bytes);
}

/// Writes the prompt and returns its width in *characters*: the caller uses it
/// as a cursor column, and a byte count misplaces the cursor for any prompt
/// containing a multi-byte character.
fn print_prompt(shell: &mut Shell, print_ps2: bool) -> usize {
    let prompt = if print_ps2 {
        shell.get_ps2()
    } else {
        shell.get_ps1()
    };
    write_terminal(prompt.as_bytes());
    prompt.chars().count()
}

fn standard_repl(shell: &mut Shell) {
    let mut program_buffer = Vec::new();
    let mut line_buffer = Vec::new();
    let mut print_ps2 = false;
    clear_line();
    flush_terminal();
    report_mail(shell);
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
                    let _ = writeln!(io::stderr());
                    shell.terminal.reset();
                    match shell.execute_program(program_string.as_bytes()) {
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
            write_terminal(&line_buffer);
            set_cursor_pos(cursor_position);
            flush_terminal();
        }
        // Wait for a keystroke or a signal rather than waking 62 times a
        // second to find neither.
        let _ = wait_for_input(io::stdin().as_raw_fd());
        shell.signal_manager.reset_sigint_count();
        shell.handle_async_events();
        if shell.signal_manager.get_sigint_count() > 0 {
            program_buffer.clear();
            line_buffer.clear();
            let _ = writeln!(io::stderr());
            report_mail(shell);
            eprint!("{}", shell.get_ps1());
        }
        if shell.set_options.vi {
            return;
        }
    }
}

fn vi_repl(shell: &mut Shell) {
    let mut editor = ViEditor::default();
    let mut program_buffer = Vec::new();
    let mut print_ps2 = false;
    clear_line();
    flush_terminal();
    report_mail(shell);
    eprint!("{}", shell.get_ps1());
    loop {
        while let Some(c) = read_nonblocking_char() {
            match editor.process_new_input(c, shell) {
                Ok(Action::Execute(command)) => {
                    program_buffer.extend(command.iter());
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
                    let _ = writeln!(io::stderr());
                    shell.terminal.reset();
                    match shell.execute_program(program_string.as_bytes()) {
                        Ok(_) => {
                            program_buffer.clear();
                            print_ps2 = false;
                        }
                        Err(syntax_err) => {
                            if !syntax_err.could_be_resolved_with_more_input {
                                eprintln!("sh: syntax error: {}", syntax_err.message);
                                program_buffer.clear();
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
            write_terminal(editor.current_line(shell));
            set_cursor_pos(cursor_position);
            flush_terminal()
        }
        // Wait for a keystroke or a signal rather than waking 62 times a
        // second to find neither.
        let _ = wait_for_input(io::stdin().as_raw_fd());
        shell.signal_manager.reset_sigint_count();
        shell.handle_async_events();
        if shell.signal_manager.get_sigint_count() > 0 {
            program_buffer.clear();
            editor.reset_current_line();
            let _ = writeln!(io::stderr());
            report_mail(shell);
            eprint!("{}", shell.get_ps1());
        }
        if !shell.set_options.vi {
            return;
        }
    }
}

/// Reads and runs commands without the line editor.
///
/// `sh -i` may be handed a pipe or a file for standard input. The shell is
/// still interactive (it prompts, and job control is on), but the editor would
/// redraw the line into that output, so it is skipped entirely.
fn non_terminal_repl(shell: &mut Shell) -> ! {
    let mut program_buffer = String::new();
    loop {
        report_mail(shell);
        if program_buffer.is_empty() {
            eprint!("{}", shell.get_ps1());
        } else {
            eprint!("{}", shell.get_ps2());
        }
        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Ok(0) => shell.exit(shell.last_pipeline_exit_status),
            Ok(_) => {}
            // POSIX: an unrecoverable read error exits 128.
            Err(_) => shell.exit(128),
        }
        program_buffer.push_str(&line);
        match shell.execute_program(program_buffer.as_bytes()) {
            Ok(_) => program_buffer.clear(),
            Err(syntax_err) => {
                if !syntax_err.could_be_resolved_with_more_input {
                    eprintln!(
                        "sh({}): syntax error: {}",
                        syntax_err.lineno, syntax_err.message
                    );
                    program_buffer.clear();
                }
            }
        }
        shell.handle_async_events();
    }
}

fn interactive_shell(shell: &mut Shell) {
    if is_process_in_foreground() {
        let pgid = getpgrp();
        tcsetpgrp(io::stdin().as_raw_fd(), pgid).unwrap();
    }
    shell.terminal.set_nonblocking_no_echo();
    unsafe { handle_signal_ignore(Signal::SigQuit) }
    unsafe { handle_signal_ignore(Signal::SigTerm) }
    unsafe { handle_signal_write_to_signal_buffer(Signal::SigInt) }
    if shell.set_options.monitor {
        // job control signals
        unsafe { handle_signal_ignore(Signal::SigTtin) }
        unsafe { handle_signal_ignore(Signal::SigTtou) }
        unsafe { handle_signal_ignore(Signal::SigTstp) }
    }
    // POSIX: an interactive shell expands $ENV and, if the result is an absolute
    // pathname, executes that file in the current environment. ENV is ignored if
    // the real and effective user/group IDs differ.
    let env_file = shell.get_var_and_expand("ENV", "");
    if env_file.starts_with('/') {
        let ids_match =
            unsafe { libc::getuid() == libc::geteuid() && libc::getgid() == libc::getegid() };
        if ids_match {
            if let Ok(contents) = std::fs::read_to_string(&env_file) {
                if let Err(err) = shell.execute_program(contents.as_bytes()) {
                    eprintln!("sh: {env_file}: {}", err.message);
                }
            }
        }
    }
    if !io::stdin().is_terminal() {
        non_terminal_repl(shell);
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

    // `args()` panics inside libstd on an argument that is not valid UTF-8;
    // `args_os()` cannot. POSIX argv is a list of byte strings.
    let args = match parse_args(
        std::env::args_os().map(ShString::from).collect(),
        is_attached_to_terminal(),
    ) {
        Ok(args) => args,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    };
    let mut shell = Shell::initialize_from_system(
        args.program_name,
        args.arguments.into_iter().collect(),
        args.set_options,
        args.execution_mode == ExecutionMode::Interactive,
    );
    unsafe { setup_signal_handling() };
    match args.execution_mode {
        ExecutionMode::Interactive => interactive_shell(&mut shell),
        ExecutionMode::ReadCommandsFromStdin => {
            // Read as bytes: `read_line` needs valid UTF-8 and silently ends
            // the loop on a script that is not text.
            let mut buffer: Vec<u8> = Vec::new();
            // A construct that is merely incomplete is not an error while more
            // input may still arrive, but it becomes one at end of input.
            let mut incomplete: Option<ParserError> = None;
            while io::stdin()
                .lock()
                .read_until(b'\n', &mut buffer)
                .is_ok_and(|n| n > 0)
            {
                if buffer.ends_with(b"\\\n") {
                    continue;
                }
                match shell.execute_program(&buffer) {
                    Ok(_) => {
                        buffer.clear();
                        incomplete = None;
                    }
                    Err(syntax_err) => {
                        if !syntax_err.could_be_resolved_with_more_input {
                            eprintln!(
                                "sh({}): syntax error: {}",
                                syntax_err.lineno, syntax_err.message
                            );
                            std::process::exit(2);
                        }
                        incomplete = Some(syntax_err);
                    }
                }
            }
            if let Some(syntax_err) = incomplete {
                eprintln!(
                    "sh({}): syntax error: {}",
                    syntax_err.lineno, syntax_err.message
                );
                std::process::exit(2);
            }
        }
        other => match other {
            ExecutionMode::ReadCommandsFromString(command_string) => {
                execute_string(command_string.as_bytes(), &mut shell);
            }
            ExecutionMode::ReadFromFile(file) => match read_command_file(&file) {
                Ok(file_contents) => execute_string(&file_contents, &mut shell),
                Err((err, status)) => {
                    eprintln!("sh: {}: {err}", file.display());
                    std::process::exit(status);
                }
            },
            _ => unreachable!(),
        },
    }
    shell.exit(shell.last_pipeline_exit_status);
}
