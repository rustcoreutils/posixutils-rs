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
use crate::parse::command_parser::CommandParser;
use crate::parse::{AliasTable, ParseResult};
use atty::Stream;
use std::io;

mod cli;
mod interpreter;
mod parse;
mod program;
mod utils;

fn execute_program(program: &str, interpreter: &mut Interpreter) -> ParseResult<()> {
    let mut parser = CommandParser::new(program)?;
    let alias_table = AliasTable::default();
    loop {
        match parser.parse_next_command(&alias_table)? {
            Some(command) => {
                interpreter.interpret(&command);
            }
            None => break,
        }
    }
    Ok(())
}

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
                match execute_program(&buffer, &mut interpreter) {
                    Ok(_) => {
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
                    execute_program(&command_string, &mut interpreter).expect("parsing error");
                }
                ExecutionMode::ReadFromFile(file) => {
                    // TODO: impl proper error reporting
                    let file_contents = std::fs::read_to_string(file).expect("could not read file");
                    execute_program(&file_contents, &mut interpreter).expect("parsing error");
                }
                _ => unreachable!(),
            }
        }
    }
}
