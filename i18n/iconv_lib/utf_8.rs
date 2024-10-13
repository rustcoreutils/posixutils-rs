//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

/// Convert UTF-8 to UCS-4
pub fn to_ucs4(input: &[u8], omit_invalid: bool, supress_error: bool) -> (u32, Vec<u32>) {
    let mut result = Vec::new();
    let mut i = 0;

    while i < input.len() {
        let byte = input[i];
        if byte <= 0x7F {
            result.push(byte as u32);
            i += 1;
        } else if byte >= 0xC0 && byte <= 0xDF {
            if i + 1 >= input.len() || (input[i + 1] & 0xC0) != 0x80 {
                if omit_invalid {
                    i += 1;
                    continue;
                } else {
                    if !supress_error {
                        eprintln!("Error: Invalid 2-byte UTF-8 sequence at position {}", i);
                    }

                    return (1, result);
                }
            }
            let code_point = ((byte as u32 & 0x1F) << 6) | (input[i + 1] as u32 & 0x3F);
            if code_point < 0x80 {
                if omit_invalid {
                    i += 2;
                    continue;
                } else {
                    if !supress_error {
                        eprintln!("Error: Overlong 2-byte sequence at position {}", i);
                    }
                    return (1, result);
                }
            }
            result.push(code_point);
            i += 2;
        } else if byte >= 0xE0 && byte <= 0xEF {
            if i + 2 >= input.len()
                || (input[i + 1] & 0xC0) != 0x80
                || (input[i + 2] & 0xC0) != 0x80
            {
                if omit_invalid {
                    i += 1;
                    continue;
                } else {
                    if !supress_error {
                        eprintln!("Error: Invalid 3-byte UTF-8 sequence at position {}", i);
                    }
                    return (1, result);
                }
            }
            let code_point = ((byte as u32 & 0x0F) << 12)
                | ((input[i + 1] as u32 & 0x3F) << 6)
                | (input[i + 2] as u32 & 0x3F);

            if code_point < 0x800 || (0xD800..=0xDFFF).contains(&code_point) {
                if omit_invalid {
                    i += 3;
                    continue;
                } else {
                    if !supress_error {
                        eprintln!("Error: Invalid 3-byte sequence at position {}", i);
                    }
                    return (1, result);
                }
            }
            result.push(code_point);
            i += 3;
        } else if byte >= 0xF0 && byte <= 0xF4 {
            if i + 3 >= input.len()
                || (input[i + 1] & 0xC0) != 0x80
                || (input[i + 2] & 0xC0) != 0x80
                || (input[i + 3] & 0xC0) != 0x80
            {
                if omit_invalid {
                    i += 1;
                    continue;
                } else {
                    if !supress_error {
                        eprintln!("Error: Invalid 4-byte UTF-8 sequence at position {}", i);
                    }
                    return (1, result);
                }
            }
            let code_point = ((byte as u32 & 0x07) << 18)
                | ((input[i + 1] as u32 & 0x3F) << 12)
                | ((input[i + 2] as u32 & 0x3F) << 6)
                | (input[i + 3] as u32 & 0x3F);
            if code_point > 0x10FFFF || code_point < 0x10000 {
                if omit_invalid {
                    i += 4;
                    continue;
                } else {
                    if !supress_error {
                        eprintln!(
                            "Error: Invalid code point in 4-byte sequence at position {}",
                            i
                        );
                    }
                    return (1, result);
                }
            }
            result.push(code_point);
            i += 4;
        } else if omit_invalid {
            i += 1;
        } else {
            if !supress_error {
                eprintln!("Error: Invalid byte at position {}", i);
            }
            return (1, result);
        }
    }
    (0, result)
}

/// Convert UCS-4 to UTF-8
pub fn from_ucs4(input: &[u32], omit_invalid: bool, supress_error: bool) -> (u32, Vec<u8>) {
    let mut result = Vec::new();

    for &code_point in input {
        if code_point <= 0x7F {
            result.push(code_point as u8);
        } else if code_point <= 0x7FF {
            result.push(0xC0 | ((code_point >> 6) as u8));
            result.push(0x80 | ((code_point & 0x3F) as u8));
        } else if code_point <= 0xFFFF {
            if (0xD800..=0xDFFF).contains(&code_point) {
                if omit_invalid {
                    continue;
                } else {
                    if !supress_error {
                        eprintln!(
                            "Error: Surrogate code point U+{:04X} is not allowed in UTF-8",
                            code_point
                        );
                    }
                    return (1, result);
                }
            }
            result.push(0xE0 | ((code_point >> 12) as u8));
            result.push(0x80 | (((code_point >> 6) & 0x3F) as u8));
            result.push(0x80 | ((code_point & 0x3F) as u8));
        } else if code_point <= 0x10FFFF {
            result.push(0xF0 | ((code_point >> 18) as u8));
            result.push(0x80 | (((code_point >> 12) & 0x3F) as u8));
            result.push(0x80 | (((code_point >> 6) & 0x3F) as u8));
            result.push(0x80 | ((code_point & 0x3F) as u8));
        } else {
            if omit_invalid {
                continue;
            } else {
                if !supress_error {
                    eprintln!(
                        "Error: Code point U+{:X} is out of valid Unicode range",
                        code_point
                    );
                }
                return (1, result);
            }
        }
    }

    (0, result)
}
