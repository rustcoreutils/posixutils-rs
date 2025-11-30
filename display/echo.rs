//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use std::io::{self, Write};

fn translate_str(skip_nl: bool, s: &str) -> String {
    let mut output = String::with_capacity(s.len());
    let mut nl = true;

    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let ch = chars[i];
        if ch == '\\' {
            if i + 1 >= chars.len() {
                // Trailing backslash - preserve it
                output.push('\\');
                i += 1;
                continue;
            }

            let next = chars[i + 1];
            match next {
                'a' => {
                    output.push('\x07');
                    i += 2;
                }
                'b' => {
                    output.push('\x08');
                    i += 2;
                }
                'c' => {
                    nl = false;
                    break;
                }
                'f' => {
                    output.push('\x0c');
                    i += 2;
                }
                'n' => {
                    output.push('\n');
                    i += 2;
                }
                'r' => {
                    output.push('\r');
                    i += 2;
                }
                't' => {
                    output.push('\t');
                    i += 2;
                }
                'v' => {
                    output.push('\x0b');
                    i += 2;
                }
                '\\' => {
                    output.push('\\');
                    i += 2;
                }
                '0' => {
                    // Octal escape: \0num where num is 0-3 octal digits
                    i += 2; // Skip \0
                    let mut octal_value: u8 = 0;
                    let mut digits = 0;

                    while digits < 3 && i < chars.len() {
                        let digit = chars[i];
                        if ('0'..='7').contains(&digit) {
                            octal_value = octal_value * 8 + (digit as u8 - b'0');
                            i += 1;
                            digits += 1;
                        } else {
                            break;
                        }
                    }
                    output.push(octal_value as char);
                }
                _ => {
                    // Unknown escape - preserve the character after backslash
                    output.push(next);
                    i += 2;
                }
            }
        } else {
            output.push(ch);
            i += 1;
        }
    }

    if nl && !skip_nl {
        output.push('\n');
    }

    output
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args: Vec<String> = std::env::args().collect();
    args.remove(0);

    let skip_nl = {
        if !args.is_empty() && (args[0] == "-n") {
            args.remove(0);
            true
        } else {
            false
        }
    };

    let echo_str = translate_str(skip_nl, &args.join(" "));

    io::stdout().write_all(echo_str.as_bytes())?;

    Ok(())
}
