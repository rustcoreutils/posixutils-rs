use byteorder::{BigEndian, ByteOrder, LittleEndian};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UTF16Variant {
    UTF16LE,
    UTF16BE,
    UTF16,
}

const BOM: u16 = 0xFEFF;
const BOM_SWAPPED: u16 = 0xFFFE;

pub fn to_ucs4(
    input: &[u8],
    omit_invalid: bool,
    suppress_error: bool,
    variant: UTF16Variant,
) -> (u32, Vec<u32>) {
    if input.len() < 2 {
        if !suppress_error {
            eprintln!("Error: Input is too short");
        }
        return (1, Vec::new());
    }

    let (variant, mut index) = match variant {
        UTF16Variant::UTF16LE => (UTF16Variant::UTF16LE, 0),
        UTF16Variant::UTF16BE => (UTF16Variant::UTF16BE, 0),
        UTF16Variant::UTF16 => {
            let first_word = BigEndian::read_u16(&input[0..2]);
            if first_word == BOM {
                (UTF16Variant::UTF16BE, 2)
            } else if first_word == BOM_SWAPPED {
                (UTF16Variant::UTF16LE, 2)
            } else {
                if cfg!(target_endian = "little") {
                    (UTF16Variant::UTF16LE, 0)
                } else {
                    (UTF16Variant::UTF16BE, 0)
                }
            }
        }
    };
    let mut ucs4: Vec<u32> = Vec::new();

    while index < input.len() {
        if index + 2 > input.len() {
            break;
        }

        let code_unit = match variant {
            UTF16Variant::UTF16LE => LittleEndian::read_u16(&input[index..index + 2]),
            UTF16Variant::UTF16BE => BigEndian::read_u16(&input[index..index + 2]),
            _ => {
                if !suppress_error {
                    eprintln!("Error: Unknown UTF-16 variant");
                }
                return (1, ucs4);
            }
        };

        // Surrogate pair
        if (0xD800..=0xDBFF).contains(&code_unit) {
            if index + 4 > input.len() {
                if !suppress_error {
                    eprintln!("Error: Unpaired surrogate at end of input");
                }
                if omit_invalid {
                    break;
                } else {
                    return (1, ucs4);
                }
            }

            let low_surrogate = match variant {
                UTF16Variant::UTF16LE => LittleEndian::read_u16(&input[(index + 2)..(index + 4)]),
                UTF16Variant::UTF16BE => BigEndian::read_u16(&input[(index + 2)..(index + 4)]),
                _ => {
                    if !suppress_error {
                        eprintln!("Error: Unknown UTF-16 variant");
                    }
                    return (1, ucs4);
                }
            };

            if !(0xDC00..=0xDFFF).contains(&low_surrogate) {
                if !suppress_error {
                    eprintln!("Error: Invalid low surrogate at position {}", index + 2);
                }
                if omit_invalid {
                    index += 2;
                    continue;
                } else {
                    return (1, ucs4);
                }
            }

            let high = u32::from(code_unit - 0xD800);
            let low = u32::from(low_surrogate - 0xDC00);
            let code_point = (high << 10) + low + 0x10000;

            ucs4.push(code_point);
            index += 4;

            // Unpaired low surrogate
        } else if (0xDC00..=0xDFFF).contains(&code_unit) {
            if !suppress_error {
                eprintln!("Error: Unpaired low surrogate at position {}", index);
            }
            if omit_invalid {
                index += 2;
                continue;
            } else {
                return (1, ucs4);
            }
        } else {
            ucs4.push(u32::from(code_unit));
            index += 2;
        }
    }

    (0, ucs4)
}

pub fn from_ucs4(
    input: &[u32],
    omit_invalid: bool,
    suppress_error: bool,
    variant: UTF16Variant,
) -> (u32, Vec<u8>) {
    let mut utf16: Vec<u8> = Vec::with_capacity(input.len() * 4); // Pre-allocate assuming worst case
    let variant = match variant {
        UTF16Variant::UTF16LE => UTF16Variant::UTF16LE,
        UTF16Variant::UTF16BE => UTF16Variant::UTF16BE,
        UTF16Variant::UTF16 => {
            if cfg!(target_endian = "little") {
                UTF16Variant::UTF16LE
            } else {
                UTF16Variant::UTF16BE
            }
        }
    };

    for &code_point in input {
        if code_point <= 0xFFFF {
            if (0xD800..=0xDFFF).contains(&code_point) {
                if !suppress_error {
                    eprintln!("Error: Isolated surrogate code point U+{:04X}", code_point);
                }
                if omit_invalid {
                    continue;
                } else {
                    return (1, utf16);
                }
            }

            match variant {
                UTF16Variant::UTF16LE => LittleEndian::write_u16(&mut utf16, code_point as u16),
                UTF16Variant::UTF16BE => BigEndian::write_u16(&mut utf16, code_point as u16),
                _ => unreachable!(),
            }
        } else if code_point <= 0x10FFFF {
            let code_point = code_point - 0x10000;
            let high_surrogate = (code_point >> 10) as u16 + 0xD800;
            let low_surrogate = (code_point & 0x3FF) as u16 + 0xDC00;
            match variant {
                UTF16Variant::UTF16LE => {
                    LittleEndian::write_u16(&mut utf16, high_surrogate);
                    LittleEndian::write_u16(&mut utf16, low_surrogate);
                }
                UTF16Variant::UTF16BE => {
                    BigEndian::write_u16(&mut utf16, high_surrogate);
                    BigEndian::write_u16(&mut utf16, low_surrogate);
                }
                _ => unreachable!(),
            }
        } else {
            if !suppress_error {
                eprintln!("Error: Invalid Unicode code point U+{:X}", code_point);
            }
            if omit_invalid {
                continue;
            } else {
                return (1, utf16);
            }
        }
    }

    (0, utf16)
}
