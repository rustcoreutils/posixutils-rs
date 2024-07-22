//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use compiler::compile_program;

mod compiler;
mod format;
mod interpreter;
mod program;
mod regex;

fn main() {
    let text = r#"
    BEGIN {
        a[1]
    }
    "#;
    let program = compile_program(text)
        .inspect_err(|e| println!("{}", e))
        .unwrap();
    println!("{:?}", program);
}
