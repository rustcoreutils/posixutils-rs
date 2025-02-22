//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::cli::{parse_args, ExecutionMode};
use crate::interpreter::Interpreter;
use crate::parse::parse;
use atty::Stream;
use std::collections::HashMap;
use std::io;

mod cli;
mod interpreter;
mod parse;
mod program;
mod utils;

fn main() {
    let is_attached_to_terminal = atty::is(Stream::Stdin) && atty::is(Stream::Stdout);
    let args = parse_args(std::env::args().collect(), is_attached_to_terminal).unwrap();
    match args.execution_mode {
        ExecutionMode::Interactive | ExecutionMode::ReadCommandsFromStdin => {
            let mut interpreter = Interpreter::initialize_from_system(
                args.program_name,
                args.arguments,
                args.set_options,
            );
            let mut buffer = String::new();
            let stdin = io::stdin();
            while stdin.read_line(&mut buffer).is_ok_and(|n| n > 0) {
                match parse(&buffer, &HashMap::default()) {
                    Ok(program) => {
                        interpreter.interpret(program);
                        buffer.clear();
                    }
                    Err(err) if !err.could_be_resolved_with_more_input => {
                        println!("{}", err.message);
                    }
                    Err(_) => {}
                }
            }
        }
        other => {
            let mut interpreter = Interpreter::initialize_from_system(
                args.program_name,
                args.arguments,
                args.set_options,
            );
            match other {
                ExecutionMode::ReadCommandsFromString(command_string) => {
                    // TODO: impl proper error reporting
                    let program =
                        parse(&command_string, &HashMap::default()).expect("parsing error");
                    interpreter.interpret(program);
                }
                ExecutionMode::ReadFromFile(file) => {
                    // TODO: impl proper error reporting
                    let file_contents = std::fs::read_to_string(file).expect("could not read file");
                    let program =
                        parse(&file_contents, &HashMap::default()).expect("parsing error");
                    interpreter.interpret(program);
                }
                _ => unreachable!(),
            }
        }
    }
}
