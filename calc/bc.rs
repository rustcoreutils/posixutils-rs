//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::OsString;

use bc_util::{
    interpreter::{ExecutionResult, Interpreter},
    parser::parse_program,
};
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

// Write program output to stdout and any diagnostic to stderr (POSIX: the
// standard error shall be used only for diagnostic messages). The partial
// output captured before an error is genuine program output, so it still goes
// to stdout. Returns true if an error occurred.
fn print_output_or_error(result: ExecutionResult<String>) -> bool {
    match result {
        Ok(output) => {
            print!("{}", output);
            false
        }
        Err(e) => {
            print!("{}", e.partial_output());
            eprintln!("{}", e);
            true
        }
    }
}

fn main() {
    setlocale(LocaleCategory::LcAll, "");
    let _ = textdomain("posixutils-rs");
    let _ = bind_textdomain_codeset("posixutils-rs", "UTF-8");

    let args = Args::parse();

    let mut interpreter = Interpreter::default();
    let mut had_error = false;

    if args.define_math_functions {
        let load = parse_program(include_str!("bc_util/math_functions.bc"), None)
            .map_err(|e| e.to_string())
            .and_then(|lib| interpreter.exec(lib).map(|_| ()).map_err(|e| e.to_string()));
        if let Err(e) = load {
            eprintln!("bc: internal error loading standard math functions: {}", e);
            std::process::exit(1);
        }
    }

    for file in args.files {
        match std::fs::read_to_string(&file) {
            Ok(s) => match parse_program(&s, file.to_str()) {
                Ok(program) => {
                    had_error |= print_output_or_error(interpreter.exec(program));
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
                        print_output_or_error(interpreter.exec(program));
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

    std::process::exit(if had_error { 1 } else { 0 });
}
