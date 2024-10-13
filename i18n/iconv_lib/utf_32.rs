//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use byteorder::{BigEndian, ByteOrder, LittleEndian};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UTF32Variant {
    UTF32LE,
    UTF32BE,
    UTF32,
}

const BOM: u32 = 0x0000FEFF;
const BOM_OE: u32 = 0xFFFE0000;

pub fn to_ucs4(
    input: &[u8],
    omit_invalid: bool,
    suppress_error: bool,
    variant: UTF32Variant,
) -> (u32, Vec<u32>) {
    if input.len() < 4 {
        if !suppress_error {
            eprintln!("Error: Input is too short");
        }
        return (1, Vec::new());
    }

    let (variant, start_index) = match variant {
        UTF32Variant::UTF32LE => (UTF32Variant::UTF32LE, 0),
        UTF32Variant::UTF32BE => (UTF32Variant::UTF32BE, 0),
        UTF32Variant::UTF32 => {
            let first_word = BigEndian::read_u32(&input[0..4]);
            if first_word == BOM {
                (UTF32Variant::UTF32BE, 4)
            } else if first_word == BOM_OE {
                (UTF32Variant::UTF32LE, 4)
            } else {
                if cfg!(target_endian = "little") {
                    (UTF32Variant::UTF32LE, 0)
                } else {
                    (UTF32Variant::UTF32BE, 0)
                }
            }
        }
    };

    let mut ucs4: Vec<u32> = Vec::new();
    let mut index = start_index;

    while index < input.len() {
        if index + 4 > input.len() {
            break;
        }
        let code_point = match variant {
            UTF32Variant::UTF32LE => LittleEndian::read_u32(&input[index..index + 4]),
            UTF32Variant::UTF32BE => BigEndian::read_u32(&input[index..index + 4]),
            _ => unreachable!(),
        };

        if code_point >= 0x110000 {
            if !suppress_error {
                eprintln!("Error: Invalid code point U+{:X}", code_point);
            }
            if omit_invalid {
                index += 4;
                continue;
            } else {
                return (1, ucs4);
            }
        }
        ucs4.push(code_point);
        index += 4;
    }

    (0, ucs4)
}

pub fn from_ucs4(
    input: &[u32],
    omit_invalid: bool,
    suppress_error: bool,
    variant: UTF32Variant,
) -> (u32, Vec<u8>) {
    let mut utf32: Vec<u8> = Vec::with_capacity(input.len() * 4);
    let (variant, include_bom) = match variant {
        UTF32Variant::UTF32LE => (UTF32Variant::UTF32LE, false),
        UTF32Variant::UTF32BE => (UTF32Variant::UTF32BE, false),
        UTF32Variant::UTF32 => {
            if cfg!(target_endian = "little") {
                (UTF32Variant::UTF32LE, true)
            } else {
                (UTF32Variant::UTF32BE, true)
            }
        }
    };

    if include_bom {
        write_u32(&mut utf32, BOM, variant);
    }

    for &code_point in input {
        if code_point > 0x10FFFF {
            if !suppress_error {
                eprintln!("Error: Invalid Unicode code point U+{:X}", code_point);
            }
            if omit_invalid {
                continue;
            } else {
                return (1, utf32);
            }
        }
        write_u32(&mut utf32, code_point, variant);
    }

    (0, utf32)
}

fn write_u32(buffer: &mut Vec<u8>, value: u32, variant: UTF32Variant) {
    let mut temp = [0u8; 4];
    match variant {
        UTF32Variant::UTF32LE => LittleEndian::write_u32(&mut temp, value),
        UTF32Variant::UTF32BE => BigEndian::write_u32(&mut temp, value),
        _ => unreachable!(),
    }
    buffer.extend_from_slice(&temp);
}
