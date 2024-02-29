//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - echo needs to translate backslash-escaped octal numbers:
// ```
// \0num
//	Write an 8-bit value that is the 0, 1, 2 or 3-digit octal number _num_.
//

extern crate plib;

use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;

fn translate_str(s: &str) -> String {
    let mut output = String::with_capacity(s.len());

    let mut in_bs = false;
    let mut nl = true;

    for ch in s.chars() {
        if ch == '\\' {
            in_bs = true;
        } else if in_bs {
            in_bs = false;
            match ch {
                'a' => {
                    output.push('\x07');
                }
                'b' => {
                    output.push('\x08');
                }
                'c' => {
                    nl = false;
                    break;
                }
                'f' => {
                    output.push('\x12');
                }
                'n' => {
                    output.push('\n');
                }
                'r' => {
                    output.push('\r');
                }
                't' => {
                    output.push('\t');
                }
                'v' => {
                    output.push('\x11');
                }
                '\\' => {
                    output.push_str("\\");
                }
                _ => {}
            }
        } else {
            output.push(ch);
        }
    }

    if nl {
        output.push_str("\n");
    }

    output
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut args: Vec<String> = std::env::args().collect();
    args.remove(0);

    let echo_str = translate_str(&args.join(" "));

    println!("{}", echo_str);

    Ok(())
}
