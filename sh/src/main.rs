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

mod interpreter;
mod lexer;
mod parser;
mod program;

fn main() {
    let program = parse("/bin/echo test");
    println!("{:?}", program);
    let mut interpreter = Interpreter::initialize_from_system();
    interpreter.interpret(program);
}
