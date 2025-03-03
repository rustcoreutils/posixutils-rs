//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::cli::{parse_args, ExecutionMode};
use crate::shell::{ExecutionError, Shell};
use atty::Stream;
use std::io;

mod builtin;
mod cli;
mod parse;
mod program;
mod shell;
mod utils;
mod wordexp;

fn main() {
    let is_attached_to_terminal = atty::is(Stream::Stdin) && atty::is(Stream::Stdout);
    let args = parse_args(std::env::args().collect(), is_attached_to_terminal).unwrap();
    let mut shell =
        Shell::initialize_from_system(args.program_name, args.arguments, args.set_options);
    match args.execution_mode {
        ExecutionMode::Interactive | ExecutionMode::ReadCommandsFromStdin => {
            let mut buffer = String::new();
            let stdin = io::stdin();
            while stdin.read_line(&mut buffer).is_ok_and(|n| n > 0) {
                if buffer.ends_with("\\\n") {
                    continue;
                }
                match shell.execute_program(&buffer) {
                    Ok(_) => {
                        buffer.clear();
                    }
                    Err(ExecutionError::ParserError(err))
                    if !err.could_be_resolved_with_more_input =>
                        {
                            println!("{}", err.message);
                        }
                    Err(_) => {}
                }
            }
        }
        other => {
            match other {
                ExecutionMode::ReadCommandsFromString(command_string) => {
                    // TODO: impl proper error reporting
                    shell
                        .execute_program(&command_string)
                        .expect("parsing error");
                }
                ExecutionMode::ReadFromFile(file) => {
                    // TODO: impl proper error reporting
                    let file_contents = std::fs::read_to_string(file).expect("could not read file");
                    shell
                        .execute_program(&file_contents)
                        .expect("parsing error");
                }
                _ => unreachable!(),
            }
        }
    }
}
