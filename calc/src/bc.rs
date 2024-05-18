//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::OsString;

use bc_util::parser::parse_program;

mod bc_util;

/// bc - arbitrary-precision arithmetic language
#[derive(Debug, clap::Parser)]
#[command(author, version, about, long_about)]
struct Args {
    #[arg(short = 'l')]
    define_math_functions: bool,

    files: Vec<OsString>,
}

fn main() {
    loop {
        let mut buf = String::new();
        std::io::stdin().read_line(&mut buf).unwrap();
        match parse_program(&buf) {
            Ok(program) => {
                println!("{:?}", program);
            }
            Err(e) => {
                println!("{}", e);
            }
        }
    }
}
