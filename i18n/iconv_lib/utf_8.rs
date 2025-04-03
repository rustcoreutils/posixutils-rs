//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{iter, process::exit};

/// Convert UTF-8 to UCS-4
pub fn to_ucs4<I: Iterator<Item = u8> + 'static>(
    mut input: I,
    omit_invalid: bool,
    suppress_error: bool,
) -> Box<dyn Iterator<Item = u32>> {
    let mut buffer = Vec::with_capacity(4);
    let iter = iter::from_fn(move || {
        loop {
            if buffer.is_empty() {
                buffer.extend(input.by_ref().take(4));
                if buffer.is_empty() {
                    return None; // End of input
                }
            }
            let byte = buffer[0];

            // 1 byte
            if byte <= 0x7F {
                let code_point = byte as u32;
                buffer.drain(0..1);
                return Some(code_point);

            // 2 bytes
            } else if (0xC0..=0xDF).contains(&byte) {
                if buffer.len() < 2 {
                    let add_bytes = input.by_ref().take(2 - buffer.len()).collect::<Vec<_>>();
                    buffer.extend(add_bytes);
                    if buffer.len() < 2 {
                        if omit_invalid {
                            buffer.clear();
                            continue;
                        } else {
                            if !suppress_error {
                                eprintln!("Error: Incomplete 2-byte UTF-8 sequence");
                            }
                            exit(1)
                        }
                    }
                }
                if (buffer[1] & 0xC0) != 0x80 {
                    if omit_invalid {
                        buffer.drain(0..1);
                        continue;
                    } else {
                        if !suppress_error {
                            eprintln!("Error: Invalid 2-byte UTF-8 sequence");
                        }
                        exit(1)
                    }
                }
                let code_point = ((byte as u32 & 0x1F) << 6) | (buffer[1] as u32 & 0x3F);
                if code_point < 0x80 {
                    if omit_invalid {
                        buffer.drain(0..2);
                        continue;
                    } else {
                        if !suppress_error {
                            eprintln!("Error: Overlong 2-byte sequence");
                        }
                        exit(1)
                    }
                }
                buffer.drain(0..2);
                return Some(code_point);

            // 3 bytes
            } else if (0xE0..=0xEF).contains(&byte) {
                if buffer.len() < 3 {
                    let add_bytes = input.by_ref().take(3 - buffer.len()).collect::<Vec<_>>();
                    buffer.extend(add_bytes);
                    if buffer.len() < 3 {
                        if omit_invalid {
                            buffer.clear();
                            continue;
                        } else {
                            if !suppress_error {
                                eprintln!("Error: Incomplete 3-byte UTF-8 sequence");
                            }
                            exit(1)
                        }
                    }
                }
                if (buffer[1] & 0xC0) != 0x80 || (buffer[2] & 0xC0) != 0x80 {
                    if omit_invalid {
                        buffer.drain(0..1);
                        continue;
                    } else {
                        if !suppress_error {
                            eprintln!("Error: Invalid 3-byte UTF-8 sequence");
                        }
                        exit(1)
                    }
                }
                let code_point = ((byte as u32 & 0x0F) << 12)
                    | ((buffer[1] as u32 & 0x3F) << 6)
                    | (buffer[2] as u32 & 0x3F);
                if code_point < 0x800 || (0xD800..=0xDFFF).contains(&code_point) {
                    if omit_invalid {
                        buffer.drain(0..3);
                        continue;
                    } else {
                        if !suppress_error {
                            eprintln!("Error: Invalid 3-byte sequence");
                        }
                        exit(1)
                    }
                }
                buffer.drain(0..3);
                return Some(code_point);

            // 4 bytes
            } else if (0xF0..=0xF4).contains(&byte) {
                if buffer.len() < 4 {
                    let add_bytes = input.by_ref().take(4 - buffer.len()).collect::<Vec<_>>();
                    buffer.extend(add_bytes);
                    if buffer.len() < 4 {
                        if omit_invalid {
                            buffer.clear();
                            continue;
                        } else {
                            if !suppress_error {
                                eprintln!("Error: Incomplete 4-byte UTF-8 sequence");
                            }
                            exit(1)
                        }
                    }
                }
                if (buffer[1] & 0xC0) != 0x80
                    || (buffer[2] & 0xC0) != 0x80
                    || (buffer[3] & 0xC0) != 0x80
                {
                    if omit_invalid {
                        buffer.drain(0..1);
                        continue;
                    } else {
                        if !suppress_error {
                            eprintln!("Error: Invalid 4-byte UTF-8 sequence");
                        }
                        exit(1)
                    }
                }
                let code_point = ((byte as u32 & 0x07) << 18)
                    | ((buffer[1] as u32 & 0x3F) << 12)
                    | ((buffer[2] as u32 & 0x3F) << 6)
                    | (buffer[3] as u32 & 0x3F);
                if !(0x10000..=0x10FFFF).contains(&code_point) {
                    if omit_invalid {
                        buffer.drain(0..4);
                        continue;
                    } else {
                        if !suppress_error {
                            eprintln!("Error: Invalid code point in 4-byte sequence");
                        }
                        exit(1)
                    }
                }
                buffer.drain(0..4);
                return Some(code_point);
            } else {
                if omit_invalid {
                    buffer.drain(0..1);
                    continue;
                } else {
                    if !suppress_error {
                        eprintln!("Error: Invalid byte");
                    }
                    exit(1);
                }
            }
        }
    });
    Box::new(iter)
}

/// Convert UCS-4 to UTF-8
pub fn from_ucs4<I: Iterator<Item = u32> + 'static>(
    input: I,
    omit_invalid: bool,
    suppress_error: bool,
) -> Box<dyn Iterator<Item = u8>> {
    let iter = input.flat_map(move |code_point| {
        if code_point <= 0x7F {
            Some(vec![code_point as u8])
        } else if code_point <= 0x7FF {
            Some(vec![
                0xC0 | ((code_point >> 6) as u8),
                0x80 | ((code_point & 0x3F) as u8),
            ])
        } else if code_point <= 0xFFFF {
            if (0xD800..=0xDFFF).contains(&code_point) {
                if !suppress_error {
                    eprintln!(
                        "Error: Surrogate code point U+{:04X} is not allowed in UTF-8",
                        code_point
                    );
                }
                if omit_invalid { None } else { Some(vec![]) }
            } else {
                Some(vec![
                    0xE0 | ((code_point >> 12) as u8),
                    0x80 | (((code_point >> 6) & 0x3F) as u8),
                    0x80 | ((code_point & 0x3F) as u8),
                ])
            }
        } else if code_point <= 0x10FFFF {
            Some(vec![
                0xF0 | ((code_point >> 18) as u8),
                0x80 | (((code_point >> 12) & 0x3F) as u8),
                0x80 | (((code_point >> 6) & 0x3F) as u8),
                0x80 | ((code_point & 0x3F) as u8),
            ])
        } else {
            if omit_invalid {
                None
            } else {
                if !suppress_error {
                    eprintln!(
                        "Error: Code point U+{:X} is out of valid Unicode range",
                        code_point
                    );
                    exit(1)
                }
                Some(vec![])
            }
        }
    });
    Box::new(iter.flatten())
}
