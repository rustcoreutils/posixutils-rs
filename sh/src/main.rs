//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::interpreter::Interpreter;
use crate::parser::parse;
use std::io;

mod interpreter;
mod lexer;
mod parser;
mod program;

fn main() {
    let mut interpreter = Interpreter::initialize_from_system();
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
