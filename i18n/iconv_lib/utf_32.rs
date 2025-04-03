//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::iter;
use std::process::exit;

use byteorder::{BigEndian, ByteOrder, LittleEndian};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UTF32Variant {
    UTF32LE,
    UTF32BE,
    UTF32,
}

const BOM: u32 = 0x0000FEFF;
const BOM_OE: u32 = 0xFFFE0000;

/// Convert UTF-32 to UCS-4
pub fn to_ucs4<I: Iterator<Item = u8> + 'static>(
    mut input: I,
    omit_invalid: bool,
    suppress_error: bool,
    variant: UTF32Variant,
) -> Box<dyn Iterator<Item = u32>> {
    let mut buffer = Vec::with_capacity(4);
    let mut variant = variant;
    let mut bom_checked = false;

    let iter = iter::from_fn(move || {
        loop {
            if buffer.len() < 4 {
                buffer.extend(input.by_ref().take(4 - buffer.len()));
                if buffer.len() < 4 {
                    return None; // End of input
                }
            }

            if !bom_checked {
                let first_word = BigEndian::read_u32(&buffer);
                match variant {
                    UTF32Variant::UTF32 => {
                        if first_word == BOM {
                            variant = UTF32Variant::UTF32BE;
                            buffer.clear();
                        } else if first_word == BOM_OE {
                            variant = UTF32Variant::UTF32LE;
                            buffer.clear();
                        } else {
                            variant = if cfg!(target_endian = "little") {
                                UTF32Variant::UTF32LE
                            } else {
                                UTF32Variant::UTF32BE
                            };
                        }
                    }
                    _ => {}
                }
                bom_checked = true;
                if buffer.is_empty() {
                    continue;
                }
            }

            let code_point = match variant {
                UTF32Variant::UTF32LE => LittleEndian::read_u32(&buffer),
                UTF32Variant::UTF32BE => BigEndian::read_u32(&buffer),
                UTF32Variant::UTF32 => unreachable!(),
            };

            buffer.clear();

            if code_point >= 0x110000 {
                if !suppress_error {
                    eprintln!("Error: Invalid code point U+{:X}", code_point);
                }
                if omit_invalid {
                    continue;
                } else {
                    return None;
                }
            }

            return Some(code_point);
        }
    });
    Box::new(iter)
}

/// Convert UCS-4 to UTF-32
pub fn from_ucs4<I: Iterator<Item = u32> + 'static>(
    input: I,
    omit_invalid: bool,
    suppress_error: bool,
    variant: UTF32Variant,
) -> Box<dyn Iterator<Item = u8>> {
    let variant = match variant {
        UTF32Variant::UTF32LE => UTF32Variant::UTF32LE,
        UTF32Variant::UTF32BE => UTF32Variant::UTF32BE,
        UTF32Variant::UTF32 => {
            if cfg!(target_endian = "little") {
                UTF32Variant::UTF32LE
            } else {
                UTF32Variant::UTF32BE
            }
        }
    };

    let mut code_point = input.peekable();
    let mut buf = [0u8; 4];
    let mut idx = 4;

    let iter = iter::from_fn(move || loop {
        if idx < 4 {
            let byte = buf[idx];
            idx += 1;
            return Some(byte);
        }

        match code_point.next() {
            Some(cp) => {
                if cp > 0x10FFFF {
                    if omit_invalid {
                        continue;
                    } else {
                        if !suppress_error {
                            eprintln!("Error: Invalid Unicode code point U+{:X}", cp);
                        }
                        exit(1);
                    }
                }
                write_u32(&mut buf, cp, variant); // Write code point to buffer
                idx = 0;
            }
            None => return None,
        }
    });

    Box::new(iter)
}

#[inline]
fn write_u32(buffer: &mut [u8; 4], value: u32, variant: UTF32Variant) {
    match variant {
        UTF32Variant::UTF32LE => LittleEndian::write_u32(buffer, value),
        UTF32Variant::UTF32BE => BigEndian::write_u32(buffer, value),
        _ => unreachable!(),
    }
}
