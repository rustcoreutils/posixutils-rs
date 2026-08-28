//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::OsString;
use std::io::BufWriter;

use bc_util::{interpreter::Interpreter, output::OutputWriter, parser::parse_program};
use clap::Parser;

use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
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

// Program output goes to stdout as it is produced; diagnostics go to stderr
// (POSIX: the standard error shall be used only for diagnostic messages).
// Returns true if an error occurred.
fn report_error(result: Result<(), impl std::fmt::Display>) -> bool {
    match result {
        Ok(()) => false,
        Err(e) => {
            eprintln!("{}", e);
            true
        }
    }
}

/// Stack for the interpreter thread.
///
/// Expression and function-call evaluation recurse on the machine stack, and
/// bc programs recurse legitimately -- a recursive factorial is the textbook
/// example. The default thread stack bounds that far below `MAX_EVAL_DEPTH`,
/// so the work runs on a thread large enough that the interpreter's own limit
/// is what reports the problem, with a diagnostic, instead of the process
/// aborting on a guard page.
const INTERPRETER_STACK_SIZE: usize = 512 * 1024 * 1024;

fn main() {
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
            eprintln!("bc: {}", e);
            std::process::exit(1);
        }
    }
}

fn run() {
    setlocale(LocaleCategory::LcAll, "");
    let _ = textdomain("posixutils-rs");
    let _ = bind_textdomain_codeset("posixutils-rs", "UTF-8");

    let args = Args::parse();

    let mut interpreter = Interpreter::default();
    let mut had_error = false;
    // Block-buffered so a long-running loop streams rather than accumulating
    // its whole output in memory; flushed after each input item so interactive
    // output appears without delay, as POSIX requires.
    let stdout = std::io::stdout();
    let mut sink = BufWriter::new(stdout.lock());
    let mut out = OutputWriter::new(&mut sink);

    if args.define_math_functions {
        let load = parse_program(include_str!("bc_util/math_functions.bc"), None)
            .map_err(|e| e.to_string())
            .and_then(|lib| interpreter.exec(lib, &mut out).map_err(|e| e.to_string()));
        if let Err(e) = load {
            eprintln!("bc: internal error loading standard math functions: {}", e);
            std::process::exit(1);
        }
    }

    for file in args.files {
        match std::fs::read_to_string(&file) {
            Ok(s) => match parse_program(&s, file.to_str()) {
                Ok(program) => {
                    had_error |= report_error(interpreter.exec(program, &mut out));
                    had_error |= report_error(out.flush());
                }
                Err(e) => {
                    eprintln!("{}", e);
                    had_error = true;
                }
            },
            Err(_) => {
                // POSIX CONSEQUENCES OF ERRORS: if a file operand cannot be
                // accessed, write a diagnostic and terminate.
                eprintln!("bc: cannot read file: {}", file.to_string_lossy());
                std::process::exit(1);
            }
        };
        if interpreter.has_quit() {
            had_error |= report_error(out.flush());
            std::process::exit(if had_error { 1 } else { 0 });
        }
    }

    let mut repl = match DefaultEditor::new() {
        Ok(repl) => repl,
        Err(e) => {
            eprintln!("bc: {}", e);
            std::process::exit(1);
        }
    };
    let mut line_buffer = String::new();
    while !interpreter.has_quit() {
        let prompt = if line_buffer.is_empty() { ">> " } else { ".. " };
        match repl.readline(prompt) {
            Ok(line) => {
                line_buffer.push_str(&line);
                line_buffer.push('\n');
                match parse_program(&line_buffer, None) {
                    Ok(program) => {
                        // A runtime error in the REPL is recovered from: the
                        // session continues and its exit status is unaffected.
                        report_error(interpreter.exec(program, &mut out));
                        report_error(out.flush());
                        line_buffer.clear();
                    }
                    Err(e) if !e.is_incomplete => {
                        eprintln!("{}", e);
                        line_buffer.clear();
                    }
                    _ => {}
                }
                let _ = repl.add_history_entry(line);
            }
            // End of input (Ctrl-D) or interrupt (Ctrl-C): exit silently.
            Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => break,
            Err(e) => {
                eprintln!("bc: {:?}", e);
                break;
            }
        }
    }

    had_error |= report_error(out.flush());
    std::process::exit(if had_error { 1 } else { 0 });
}
