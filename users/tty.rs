//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, IsTerminal};

use plib::curuser::tty;

fn main() {
    let is_tty = io::stdin().is_terminal();
    if !is_tty {
        println!("not a tty");
        std::process::exit(1);
    }

    let ttyname = tty();

    println!("{}", ttyname);
}
