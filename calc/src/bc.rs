//
// Copyright (c) 2024 Hemi Labs, Inc.
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

use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use rustyline::{error::ReadlineError, DefaultEditor, Result};

mod bc_util;

/// bc - arbitrary-precision arithmetic language
#[derive(Debug, Parser)]
#[command(author, version, about, long_about)]
struct Args {
    #[arg(short = 'l')]
    define_math_functions: bool,

    files: Vec<OsString>,
}

fn print_output_or_error(result: ExecutionResult<String>) {
    match result {
        Ok(output) => {
            print!("{}", output);
        }
        Err(e) => {
            print!("{}", e.partial_output());
            println!("{}", e);
        }
    }
}

fn main() -> Result<()> {
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let args = Args::parse();
    let mut interpreter = Interpreter::default();

    if args.define_math_functions {
        let lib = parse_program(include_str!("bc_util/math_functions.bc"), None)
            .expect("error parsing standard math functions");
        interpreter
            .exec(lib)
            .expect("error loading standard math functions");
    }

    for file in args.files {
        match std::fs::read_to_string(&file) {
            Ok(s) => match parse_program(&s, file.to_str()) {
                Ok(program) => print_output_or_error(interpreter.exec(program)),
                Err(e) => println!("{}", e),
            },
            Err(_) => {
                eprintln!("Could not read file: {}", file.to_string_lossy());
                return Ok(());
            }
        };
        if interpreter.has_quit() {
            return Ok(());
        }
    }

    let mut repl = DefaultEditor::new()?;
    let mut line_buffer = String::new();
    while !interpreter.has_quit() {
        let line = if line_buffer.is_empty() {
            repl.readline(">> ")
        } else {
            repl.readline(".. ")
        };
        match line {
            Ok(line) => {
                line_buffer.push_str(&line);
                line_buffer.push('\n');
                match parse_program(&line_buffer, None) {
                    Ok(program) => {
                        print_output_or_error(interpreter.exec(program));
                        line_buffer.clear();
                    }
                    Err(e) if !e.is_incomplete => {
                        println!("{}", e);
                        line_buffer.clear();
                    }
                    _ => {}
                }
                repl.add_history_entry(line)?;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(e) => {
                eprintln!("Error: {:?}", e);
                break;
            }
        }
    }
    Ok(())
}
