//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::curuser::tty;
use std::io::{self, IsTerminal};

/// tty - return user's terminal name
#[derive(Parser)]
#[command(version, about = gettext("tty - return user's terminal name"))]
struct Args {}

fn main() {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let _args = Args::parse();

    let is_tty = io::stdin().is_terminal();
    if !is_tty {
        println!("not a tty");
        std::process::exit(1);
    }

    match tty() {
        Some(ttyname) => println!("{}", ttyname),
        None => {
            println!("not a tty");
            std::process::exit(1);
        }
    }
}
