//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

fn main() {
    let is_tty = atty::is(atty::Stream::Stdin);
    if !is_tty {
        println!("not a tty");
        std::process::exit(1);
    }

    let ttyname = plib::curuser::tty();

    println!("{}", ttyname);
}
