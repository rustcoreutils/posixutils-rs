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
    interpreter::Interpreter,
    parser::{is_incomplete, parse_program},
};
use clap::Parser;

mod bc_util;

/// bc - arbitrary-precision arithmetic language
#[derive(Debug, Parser)]
#[command(author, version, about, long_about)]
struct Args {
    #[arg(short = 'l')]
    define_math_functions: bool,

    files: Vec<OsString>,
}

fn exec_str(s: &str, interpreter: &mut Interpreter) -> bool {
    match parse_program(s) {
        Ok(program) => match interpreter.exec(program) {
            Ok(output) => {
                print!("{}", output.string);
                if output.has_quit {
                    return true;
                }
            }
            Err(e) => {
                println!("{}", e);
            }
        },
        Err(e) => {
            println!("{}", e);
        }
    }
    false
}

fn main() {
    let args = Args::parse();
    let mut interpreter = Interpreter::default();
    for file in args.files {
        match std::fs::read_to_string(&file) {
            Ok(s) => {
                if exec_str(&s, &mut interpreter) {
                    return;
                }
            }
            Err(_) => {
                eprintln!("Could not read file: {}", file.to_string_lossy());
                return;
            }
        };
    }

    let mut buf = String::new();
    loop {
        std::io::stdin().read_line(&mut buf).unwrap();
        if is_incomplete(&buf) {
            continue;
        } else {
            if exec_str(&buf, &mut interpreter) {
                return;
            }
            buf.clear()
        }
    }
}
