//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use byteorder::{BigEndian, ByteOrder, LittleEndian};
use std::{
    iter::{self},
    process::exit,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UTF16Variant {
    UTF16LE,
    UTF16BE,
    UTF16,
}

const BOM: u16 = 0xFEFF;
const BOM_SWAPPED: u16 = 0xFFFE;

// Convert UTF-16 to UCS-4
pub fn to_ucs4<I: Iterator<Item = u8> + 'static>(
    mut input: I,
    omit_invalid: bool,
    suppress_error: bool,
    variant: UTF16Variant,
) -> Box<dyn Iterator<Item = u32>> {
    let mut buffer = Vec::with_capacity(4);
    let mut determined_variant = variant;
    let mut bom_checked = false;

    let iter = iter::from_fn(move || {
        loop {
            if buffer.len() < 2 {
                buffer.extend(input.by_ref().take(4 - buffer.len()));
                if buffer.len() < 2 {
                    return None; // End of input
                }
            }

            if !bom_checked {
                bom_checked = true;
                if variant == UTF16Variant::UTF16 {
                    let first_word = BigEndian::read_u16(&buffer[0..2]);
                    if first_word == BOM {
                        determined_variant = UTF16Variant::UTF16BE;
                        buffer.drain(0..2);
                        continue;
                    } else if first_word == BOM_SWAPPED {
                        determined_variant = UTF16Variant::UTF16LE;
                        buffer.drain(0..2);
                        continue;
                    } else {
                        determined_variant = if cfg!(target_endian = "little") {
                            UTF16Variant::UTF16LE
                        } else {
                            UTF16Variant::UTF16BE
                        };
                    }
                }
            }

            let code_unit = match determined_variant {
                UTF16Variant::UTF16LE => LittleEndian::read_u16(&buffer[0..2]),
                UTF16Variant::UTF16BE => BigEndian::read_u16(&buffer[0..2]),
                UTF16Variant::UTF16 => unreachable!(),
            };

            // Surrogate pair
            if (0xD800..=0xDBFF).contains(&code_unit) {
                if buffer.len() < 4 {
                    buffer.extend(input.by_ref().take(4 - buffer.len()));
                    if buffer.len() < 4 {
                        if omit_invalid {
                            buffer.clear();
                            continue;
                        } else {
                            if !suppress_error {
                                eprintln!("Error: Unpaired surrogate at end of input");
                            }
                            return None;
                        }
                    }
                }

                let low_surrogate = match determined_variant {
                    UTF16Variant::UTF16LE => LittleEndian::read_u16(&buffer[2..4]),
                    UTF16Variant::UTF16BE => BigEndian::read_u16(&buffer[2..4]),
                    UTF16Variant::UTF16 => unreachable!(),
                };

                if !(0xDC00..=0xDFFF).contains(&low_surrogate) {
                    if omit_invalid {
                        buffer.drain(0..2);
                        continue;
                    } else {
                        if !suppress_error {
                            eprintln!("Error: Invalid low surrogate");
                        }
                        return None;
                    }
                }

                let high = u32::from(code_unit - 0xD800);
                let low = u32::from(low_surrogate - 0xDC00);
                let code_point = (high << 10) + low + 0x10000;
                buffer.drain(0..4);
                return Some(code_point);

            // Unpaired low surrogate
            } else if (0xDC00..=0xDFFF).contains(&code_unit) {
                if omit_invalid {
                    buffer.drain(0..2);
                    continue;
                } else {
                    if !suppress_error {
                        eprintln!("Error: Unpaired low surrogate");
                    }
                    return None;
                }
            } else {
                buffer.drain(0..2);
                return Some(u32::from(code_unit));
            }
        }
    });

    Box::new(iter)
}

/// Convert UTF-32 from UCS-4
pub fn from_ucs4<I: Iterator<Item = u32> + 'static>(
    input: I,
    omit_invalid: bool,
    suppress_error: bool,
    variant: UTF16Variant,
) -> Box<dyn Iterator<Item = u8>> {
    let variant = match variant {
        UTF16Variant::UTF16LE | UTF16Variant::UTF16BE => variant,
        UTF16Variant::UTF16 => {
            if cfg!(target_endian = "little") {
                UTF16Variant::UTF16LE
            } else {
                UTF16Variant::UTF16BE
            }
        }
    };

    let iter = input.flat_map(move |code_point| {
        let mut utf16 = Vec::new();

        if code_point <= 0xD7FF || (0xE000..=0xFFFF).contains(&code_point) {
            utf16.push(code_point as u16);
        } else if (0xD800..=0xDFFF).contains(&code_point) {
            if !omit_invalid {
                return Vec::new();
            } else {
                if !suppress_error {
                    eprintln!("Error: Isolated surrogate code point U+{:04X}", code_point);
                }
                exit(1)
            }
        } else if code_point <= 0x10FFFF {
            let code_point = code_point - 0x10000;
            let high_surrogate = ((code_point >> 10) as u16) + 0xD800;
            let low_surrogate = ((code_point & 0x3FF) as u16) + 0xDC00;
            utf16.extend_from_slice(&[high_surrogate, low_surrogate]);
        } else if !omit_invalid {
            return Vec::new();
        } else {
            if !suppress_error {
                eprintln!("Error: Invalid Unicode code point U+{:X}", code_point);
            }
            exit(1)
        }

        to_bytes(&utf16, variant)
    });

    Box::new(iter)
}

#[inline]
fn to_bytes(utf16: &[u16], variant: UTF16Variant) -> Vec<u8> {
    utf16
        .iter()
        .flat_map(|&code_unit| match variant {
            UTF16Variant::UTF16LE => code_unit.to_le_bytes().to_vec(),
            UTF16Variant::UTF16BE => code_unit.to_be_bytes().to_vec(),
            _ => unreachable!(),
        })
        .collect()
}
