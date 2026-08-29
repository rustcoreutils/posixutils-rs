//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::OsString;
use std::io::{BufWriter, IsTerminal};

use bc_util::{
    interpreter::Interpreter,
    output::OutputWriter,
    parser::{parse_program, ParseError},
};
use clap::Parser;

use gettextrs::gettext;
use plib::diag;
use rustyline::{error::ReadlineError, DefaultEditor};

mod bc_util;

/// bc - arbitrary-precision arithmetic language
#[derive(Parser)]
#[command(version, about = gettext("bc - arbitrary-precision arithmetic language"))]
struct Args {
    #[arg(short = 'l')]
    define_math_functions: bool,

    files: Vec<OsString>,
}

/// The source name bc implementations conventionally use for standard input.
const STDIN_NAME: &str = "(standard_in)";

/// Stack for the interpreter thread.
///
/// Expression and function-call evaluation recurse on the machine stack, and
/// bc programs recurse legitimately -- a recursive factorial is the textbook
/// example. The default thread stack bounds that far below `MAX_EVAL_DEPTH`,
/// so the work runs on a thread large enough that the interpreter's own limit
/// is what reports the problem, with a diagnostic, instead of the process
/// aborting on a guard page.
const INTERPRETER_STACK_SIZE: usize = 512 * 1024 * 1024;

/// Make sure standard input, output and error are open before anything else
/// runs.
///
/// A process can be started with one of them closed. The first file anything
/// opens then lands on that descriptor, and program output goes silently into
/// it -- here the message catalog, opened while installing the locale. Taking
/// the free slots with /dev/null first, as coreutils does, keeps output out of
/// an unrelated file.
fn ensure_std_fds_open() {
    use std::os::fd::{AsRawFd, IntoRawFd};
    while let Ok(file) = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open("/dev/null")
    {
        if file.as_raw_fd() > 2 {
            // 0, 1 and 2 were all taken already; this one closes on drop.
            break;
        }
        // Leak it deliberately: it is holding a standard descriptor open.
        let _ = file.into_raw_fd();
    }
}

fn main() {
    ensure_std_fds_open();
    match std::thread::Builder::new()
        .stack_size(INTERPRETER_STACK_SIZE)
        .spawn(run)
    {
        // `run` ends the process itself; join only returns if it panicked.
        Ok(interpreter) => {
            if interpreter.join().is_err() {
                std::process::exit(1);
            }
        }
        Err(e) => {
            diag::init("bc");
            diag::error(&format!("{}", e));
            std::process::exit(1);
        }
    }
}

/// Report a runtime or write failure. Returns true so callers can record that
/// something went wrong.
fn report(e: impl std::fmt::Display) -> bool {
    diag::error(&format!("{}", e));
    true
}

/// Report each diagnostic of a parse failure at its own position.
fn report_parse_error(e: &ParseError) -> bool {
    for (line, col, message) in e.diagnostics() {
        diag::error_at(diag::Position::new(line, col), message);
    }
    true
}

fn run() {
    diag::init_locale("bc");

    let args = Args::parse();

    let mut interpreter = Interpreter::default();
    let mut had_error = false;
    // Block-buffered so a long-running loop streams rather than accumulating
    // its whole output in memory; flushed after each input item so interactive
    // output appears without delay, as POSIX requires.
    let stdout = std::io::stdout();
    let mut sink = BufWriter::new(stdout.lock());
    let mut out = OutputWriter::new(&mut sink);

    // POSIX describes error recovery for "an interactive invocation of bc". A
    // session at a terminal recovers and exits 0; a script fed on standard
    // input reports failure the way a file operand does.
    let interactive = std::io::stdin().is_terminal();

    if args.define_math_functions {
        diag::set_source("math library");
        let library = include_str!("bc_util/math_functions.bc");
        let load = parse_program(library, None)
            .map_err(|e| e.to_string())
            .and_then(|lib| interpreter.exec(lib, &mut out).map_err(|e| e.to_string()));
        if let Err(e) = load {
            diag::error(&format!(
                "{}: {}",
                gettext("internal error loading the standard math functions"),
                e
            ));
            std::process::exit(1);
        }
    }

    for file in args.files {
        let name = file.to_string_lossy().into_owned();
        diag::set_source(&name);
        // Read bytes, so a file that exists but is not text is reported as
        // that rather than as a failure to access it.
        let bytes = match std::fs::read(&file) {
            Ok(bytes) => bytes,
            Err(e) => {
                // POSIX CONSEQUENCES OF ERRORS: if a file operand cannot be
                // accessed, write a diagnostic and terminate.
                diag::error(&format!("{}: {}", name, diag::io_error_text(&e)));
                let _ = out.flush();
                std::process::exit(1);
            }
        };
        let text = match String::from_utf8(bytes) {
            Ok(text) => text,
            Err(_) => {
                diag::error(&format!("{}: {}", name, gettext("not a text file")));
                had_error = true;
                continue;
            }
        };
        match parse_program(&text, file.to_str()) {
            Ok(program) => {
                if let Err(e) = interpreter.exec(program, &mut out) {
                    had_error |= report(e);
                }
                if let Err(e) = out.flush() {
                    had_error |= report(e);
                }
            }
            Err(e) => had_error |= report_parse_error(&e),
        }
        if interpreter.has_quit() {
            if let Err(e) = out.flush() {
                had_error |= report(e);
            }
            std::process::exit(if had_error { 1 } else { 0 });
        }
    }

    let mut repl = match DefaultEditor::new() {
        Ok(repl) => repl,
        Err(e) => {
            diag::error(&format!("{}", e));
            std::process::exit(1);
        }
    };
    diag::set_source(STDIN_NAME);
    let mut line_buffer = String::new();
    while !interpreter.has_quit() {
        let prompt = if line_buffer.is_empty() { ">> " } else { ".. " };
        match repl.readline(prompt) {
            Ok(line) => {
                line_buffer.push_str(&line);
                line_buffer.push('\n');
                match parse_program(&line_buffer, None) {
                    Ok(program) => {
                        // An interactive session recovers from a runtime error
                        // and its exit status is unaffected; a script does not.
                        let mut failed = false;
                        if let Err(e) = interpreter.exec(program, &mut out) {
                            failed = report(e);
                        }
                        if let Err(e) = out.flush() {
                            failed = report(e);
                        }
                        had_error |= failed && !interactive;
                        line_buffer.clear();
                    }
                    Err(e) if !e.is_incomplete => {
                        report_parse_error(&e);
                        had_error |= !interactive;
                        line_buffer.clear();
                    }
                    _ => {}
                }
                let _ = repl.add_history_entry(line);
            }
            // End of input (Ctrl-D) or interrupt (Ctrl-C): exit silently.
            Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => break,
            Err(e) => {
                had_error |= report(format!("{:?}", e));
                break;
            }
        }
    }

    // Input that ended in the middle of a construct is not simply discarded: a
    // truncated script has to say so rather than exit as if it had run.
    if !line_buffer.is_empty() {
        if let Err(e) = parse_program(&line_buffer, None) {
            report_parse_error(&e);
            had_error = true;
        }
    }

    if let Err(e) = out.flush() {
        had_error |= report(e);
    }
    std::process::exit(if had_error { 1 } else { 0 });
}
