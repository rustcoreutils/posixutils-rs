//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use gettextrs::{LocaleCategory, bind_textdomain_codeset, setlocale, textdomain};
use std::io::{self, Write};

fn translate_str(skip_nl: bool, s: &str) -> Vec<u8> {
    let mut output = Vec::with_capacity(s.len());
    let mut nl = true;

    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let ch = chars[i];
        if ch == '\\' {
            if i + 1 >= chars.len() {
                // Trailing backslash - preserve it
                output.push(b'\\');
                i += 1;
                continue;
            }

            let next = chars[i + 1];
            match next {
                'a' => {
                    output.push(0x07);
                    i += 2;
                }
                'b' => {
                    output.push(0x08);
                    i += 2;
                }
                'c' => {
                    nl = false;
                    break;
                }
                'f' => {
                    output.push(0x0c);
                    i += 2;
                }
                'n' => {
                    output.push(b'\n');
                    i += 2;
                }
                'r' => {
                    output.push(b'\r');
                    i += 2;
                }
                't' => {
                    output.push(b'\t');
                    i += 2;
                }
                'v' => {
                    output.push(0x0b);
                    i += 2;
                }
                '\\' => {
                    output.push(b'\\');
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
                    // Push raw byte value directly - octal escapes produce single bytes
                    output.push(octal_value);
                }
                _ => {
                    // Unknown escape - preserve the character after backslash
                    // Encode the char as UTF-8 bytes
                    let mut buf = [0u8; 4];
                    let encoded = next.encode_utf8(&mut buf);
                    output.extend_from_slice(encoded.as_bytes());
                    i += 2;
                }
            }
        } else {
            // Encode the char as UTF-8 bytes
            let mut buf = [0u8; 4];
            let encoded = ch.encode_utf8(&mut buf);
            output.extend_from_slice(encoded.as_bytes());
            i += 1;
        }
    }

    if nl && !skip_nl {
        output.push(b'\n');
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

    let echo_bytes = translate_str(skip_nl, &args.join(" "));

    io::stdout().write_all(&echo_bytes)?;

    Ok(())
}
