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
use crate::parser::parse;
use atty::Stream;
use std::io;

mod cli;
mod interpreter;
mod lexer;
mod parser;
mod program;

fn main() {
    let is_attached_to_terminal = atty::is(Stream::Stdin) && atty::is(Stream::Stdout);
    let args = parse_args(
        std::env::args().collect::<Vec<_>>(),
        is_attached_to_terminal,
    )
    .unwrap();
    let mut interpreter = Interpreter::initialize_from_system();

    match args.execution_mode {
        ExecutionMode::Interactive | ExecutionMode::ReadCommandsFromStdin => {
            let mut buffer = String::new();
            let stdin = io::stdin();
            while stdin.read_line(&mut buffer).is_ok_and(|n| n > 0) {
                match parse(&buffer) {
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
        ExecutionMode::ReadCommandsFromString(command_string) => {
            // TODO: impl proper error reporting
            let program = parse(&command_string).expect("parsing error");
            interpreter.interpret(program);
        }
        ExecutionMode::ReadFromFile(_) => {
            todo!()
        }
    }
}
