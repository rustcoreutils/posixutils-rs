//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::str::Chars;

const BASE_8_DIGITS: [char; 8] = ['0', '1', '2', '3', '4', '5', '6', '7'];
const BASE_10_DIGITS: [char; 10] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
const BASE_16_DIGITS_LOWER: [char; 16] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
];
const BASE_16_DIGITS_UPPER: [char; 16] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
];

fn insert_hex_identifier(target: &mut String, integer_format: IntegerFormat, args: &FormatArgs) {
    if args.alternative_form && integer_format == IntegerFormat::HexLower {
        target.push('0');
        target.push('x');
    } else if args.alternative_form && integer_format == IntegerFormat::HexUpper {
        target.push('0');
        target.push('X');
    }
}

fn copy_buffer_to_target(buffer: &[u8], target: &mut String) {
    for c in buffer.iter() {
        target.push(*c as char);
    }
}
fn pad_target(target: &mut String, padding: usize, byte: u8) {
    for _ in 0..padding {
        target.push(byte as char);
    }
}

#[derive(Default)]
pub struct FormatArgs {
    left_justified: bool,
    signed: bool,
    prefix_space: bool,
    alternative_form: bool,
    zero_padded: bool,
    width: usize,
    precision: Option<usize>,
}

/// Parse the conversion specifier arguments from the format string.
/// # Arguments
/// `iter` - An iterator over the characters of the format string. The iterator should be positioned
/// after the '%' character that starts the conversion specifier.
/// # Returns
/// A tuple containing the conversion specifier character and the parsed arguments.
pub fn parse_conversion_specifier_args(iter: &mut Chars) -> Result<(char, FormatArgs), String> {
    let iter_next = |iter: &mut Chars| iter.next().ok_or("invalid format string".to_string());

    let parse_number = |next: &mut char, iter: &mut Chars| -> Result<usize, String> {
        let mut number = 0;
        loop {
            match *next {
                c if c.is_digit(10) => {
                    number = number * 10 + c.to_digit(10).unwrap() as usize;
                }
                _ => break,
            }
            *next = iter_next(iter)?;
        }
        Ok(number)
    };

    let mut result = FormatArgs::default();
    let mut next = iter_next(iter)?;
    loop {
        match next {
            '-' => result.left_justified = true,
            '+' => result.signed = true,
            ' ' => result.prefix_space = true,
            '#' => result.alternative_form = true,
            '0' => result.zero_padded = true,
            _ => break,
        }
        next = iter_next(iter)?;
    }

    result.width = parse_number(&mut next, iter)?;

    result.precision = if next == '.' {
        next = iter_next(iter)?;
        Some(parse_number(&mut next, iter)?)
    } else {
        None
    };

    Ok((next, result))
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IntegerFormat {
    Decimal,
    Octal,
    HexLower,
    HexUpper,
}

pub fn fmt_write_unsigned(
    target: &mut String,
    value: u64,
    integer_format: IntegerFormat,
    args: &FormatArgs,
) {
    let base;
    let digits: &[char];
    match integer_format {
        IntegerFormat::Decimal => {
            base = 10;
            digits = &BASE_10_DIGITS;
        }
        IntegerFormat::Octal => {
            base = 8;
            digits = &BASE_8_DIGITS;
        }
        IntegerFormat::HexLower => {
            base = 16;
            digits = &BASE_16_DIGITS_LOWER;
        }
        IntegerFormat::HexUpper => {
            base = 16;
            digits = &BASE_16_DIGITS_UPPER;
        }
    }

    let mut value = value;
    // 22 is the maximum number of digits needed to represent a u64 in base 8 (the lowest base)
    // 22 = ceil(log8(u64::MAX))
    let mut buffer = [0u8; 22];
    let mut index = buffer.len();
    while value != 0 {
        index -= 1;
        buffer[index] = digits[(value % base) as usize] as u8;
        value /= base;
    }

    let mut precision = args.precision.unwrap_or(1);

    // regarding the alternative form for octal numbers:
    // > For the o conversion specifier, it shall increase
    // > the precision to force the first digit of the result to be a zero
    if args.alternative_form
        && integer_format == IntegerFormat::Octal
        && precision < buffer.len() - index
    {
        precision = buffer.len() - index + 1;
    }

    // left justified:
    //    (Ox | OX)? precision buffer padding
    // right justified zero padded:
    //    (Ox | OX)? padding precision buffer
    // right justified space padded:
    //    padding (Ox | OX)? precision buffer

    let buffer_length = buffer.len() - index;
    let number_length = match integer_format {
        IntegerFormat::HexLower | IntegerFormat::HexUpper if args.alternative_form => {
            buffer_length.max(precision) + 2
        }
        _ => buffer_length.max(precision),
    };

    if args.left_justified {
        insert_hex_identifier(target, integer_format, args);
        if precision > buffer_length {
            pad_target(target, precision - buffer_length, b'0');
        }
        copy_buffer_to_target(&buffer[index..], target);
        pad_target(target, args.width.saturating_sub(number_length), b' ');
    } else if args.zero_padded {
        insert_hex_identifier(target, integer_format, args);
        pad_target(target, args.width.saturating_sub(number_length), b'0');
        if precision > buffer_length {
            pad_target(target, precision - buffer_length, b'0');
        }
        copy_buffer_to_target(&buffer[index..], target);
    } else {
        pad_target(target, args.width.saturating_sub(number_length), b' ');
        insert_hex_identifier(target, integer_format, args);
        if precision > buffer_length {
            pad_target(target, precision - buffer_length, b'0');
        }
        copy_buffer_to_target(&buffer[index..], target);
    }
}

pub fn fmt_write_signed(target: &mut String, value: i64, args: &FormatArgs) {
    let mut unsigned_value = value.unsigned_abs();
    // 20 is the maximum number of digits needed to represent a u64 in base 10
    let mut buffer = [0u8; 20];
    let mut index = buffer.len();
    while unsigned_value != 0 {
        index -= 1;
        buffer[index] = BASE_10_DIGITS[(unsigned_value % 10) as usize] as u8;
        unsigned_value /= 10;
    }

    let precision = args.precision.unwrap_or(1);
    let buffer_length = buffer.len() - index;

    let sign = if value < 0 {
        "-"
    } else if args.signed {
        "+"
    } else if args.prefix_space {
        " "
    } else {
        ""
    };
    let number_length = buffer_length.max(precision) + sign.len();

    // left justified:
    //    sign precision buffer padding
    // right justified zero padded:
    //    sign padding precision buffer
    // right justified space padded:
    //    padding sign precision buffer
    if args.left_justified {
        target.push_str(sign);
        if precision > buffer_length {
            pad_target(target, precision - buffer_length, b'0');
        }
        copy_buffer_to_target(&buffer[index..], target);
        pad_target(target, args.width.saturating_sub(number_length), b' ');
    } else if args.zero_padded {
        target.push_str(sign);
        pad_target(target, args.width.saturating_sub(number_length), b'0');
        if precision > buffer_length {
            pad_target(target, precision - buffer_length, b'0');
        }
        copy_buffer_to_target(&buffer[index..], target);
    } else {
        pad_target(target, args.width.saturating_sub(number_length), b' ');
        target.push_str(sign);
        if precision > buffer_length {
            pad_target(target, precision - buffer_length, b'0');
        }
        copy_buffer_to_target(&buffer[index..], target);
    }
}

pub fn fmt_write_string(target: &mut String, value: &str, args: &FormatArgs) {
    let precision = args.precision.unwrap_or(usize::MAX);
    let str_len = value.len().min(precision);
    let padding = args.width.saturating_sub(str_len);
    if args.left_justified {
        target.push_str(&value[..str_len]);
        pad_target(target, padding, b' ');
    } else {
        pad_target(target, padding, b' ');
        target.push_str(&value[..str_len]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_conversion_specifier_args() {
        let mut iter = "-+ #0123.456d".chars();
        let (specifier, args) = parse_conversion_specifier_args(&mut iter).unwrap();
        assert_eq!(specifier, 'd');
        assert_eq!(args.left_justified, true);
        assert_eq!(args.signed, true);
        assert_eq!(args.prefix_space, true);
        assert_eq!(args.alternative_form, true);
        assert_eq!(args.zero_padded, true);
        assert_eq!(args.width, 123);
        assert_eq!(args.precision, Some(456));
    }

    #[test]
    fn test_write_unsigned_decimal() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::Decimal,
            &FormatArgs::default(),
        );
        assert_eq!(target, "123");
    }

    #[test]
    fn test_write_unsigned_decimal_with_extra_precision() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::Decimal,
            &FormatArgs {
                precision: Some(5),
                ..Default::default()
            },
        );
        assert_eq!(target, "00123");
    }

    #[test]
    fn test_write_unsigned_decimal_with_width() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::Decimal,
            &FormatArgs {
                width: 5,
                ..Default::default()
            },
        );
        assert_eq!(target, "  123");
    }

    #[test]
    fn test_write_unsigned_decimal_with_width_zero_padded() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::Decimal,
            &FormatArgs {
                width: 5,
                zero_padded: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "00123");
    }

    #[test]
    fn test_write_unsigned_decimal_left_justified() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::Decimal,
            &FormatArgs {
                left_justified: true,
                width: 5,
                ..Default::default()
            },
        );
        assert_eq!(target, "123  ");
    }

    #[test]
    fn test_write_unsigned_decimal_left_justified_zero_padded() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::Decimal,
            &FormatArgs {
                left_justified: true,
                zero_padded: true,
                width: 5,
                ..Default::default()
            },
        );
        assert_eq!(target, "123  ");
    }

    #[test]
    fn test_write_unsigned_zero_with_default_precision() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            0,
            IntegerFormat::Decimal,
            &FormatArgs::default(),
        );
        assert_eq!(target, "0");
    }

    #[test]
    fn test_write_unsigned_zero_with_zero_precision() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            0,
            IntegerFormat::Decimal,
            &FormatArgs {
                precision: Some(0),
                ..Default::default()
            },
        );
        assert_eq!(target, "");
    }

    #[test]
    fn test_write_unsigned_octal() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::Octal,
            &FormatArgs::default(),
        );
        assert_eq!(target, "173");
    }

    #[test]
    fn test_write_unsigned_octal_alternative_form() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::Octal,
            &FormatArgs {
                alternative_form: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "0173");
    }

    #[test]
    fn test_write_unsigned_octal_with_precision() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::Octal,
            &FormatArgs {
                precision: Some(5),
                ..Default::default()
            },
        );
        assert_eq!(target, "00173");
    }

    #[test]
    fn test_write_unsigned_octal_alternative_form_with_precision() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::Octal,
            &FormatArgs {
                precision: Some(5),
                alternative_form: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "00173");
    }

    #[test]
    fn test_write_unsigned_octal_with_width() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::Octal,
            &FormatArgs {
                width: 5,
                ..Default::default()
            },
        );
        assert_eq!(target, "  173");
    }

    #[test]
    fn test_write_unsigned_octal_alternative_form_with_width() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::Octal,
            &FormatArgs {
                width: 5,
                alternative_form: true,
                ..Default::default()
            },
        );
        assert_eq!(target, " 0173");
    }

    #[test]
    fn test_write_unsigned_octal_left_justified() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::Octal,
            &FormatArgs {
                left_justified: true,
                width: 5,
                ..Default::default()
            },
        );
        assert_eq!(target, "173  ");
    }

    #[test]
    fn test_write_unsigned_octal_left_justified_zero_padded() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::Octal,
            &FormatArgs {
                left_justified: true,
                zero_padded: true,
                width: 5,
                ..Default::default()
            },
        );
        assert_eq!(target, "173  ");
    }

    #[test]
    fn test_write_unsigned_hex_lower() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::HexLower,
            &FormatArgs::default(),
        );
        assert_eq!(target, "7b");
    }

    #[test]
    fn test_write_unsigned_hex_lower_alternative_form() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::HexLower,
            &FormatArgs {
                alternative_form: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "0x7b");
    }

    #[test]
    fn test_write_unsigned_hex_lower_with_precision() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::HexLower,
            &FormatArgs {
                precision: Some(5),
                ..Default::default()
            },
        );
        assert_eq!(target, "0007b");
    }

    #[test]
    fn test_write_unsigned_hex_lower_alternative_form_with_precision() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::HexLower,
            &FormatArgs {
                precision: Some(5),
                alternative_form: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "0x0007b");
    }

    #[test]
    fn test_write_unsigned_hex_lower_with_width() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::HexLower,
            &FormatArgs {
                width: 5,
                ..Default::default()
            },
        );
        assert_eq!(target, "   7b");
    }

    #[test]
    fn test_write_unsigned_hex_lower_alternative_form_with_width() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::HexLower,
            &FormatArgs {
                width: 5,
                alternative_form: true,
                ..Default::default()
            },
        );
        assert_eq!(target, " 0x7b");
    }

    #[test]
    fn test_write_unsigned_hex_lower_left_justified() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::HexLower,
            &FormatArgs {
                left_justified: true,
                width: 5,
                ..Default::default()
            },
        );
        assert_eq!(target, "7b   ");
    }

    #[test]
    fn test_write_unsigned_hex_lower_alternative_form_left_justified() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::HexLower,
            &FormatArgs {
                left_justified: true,
                alternative_form: true,
                width: 5,
                ..Default::default()
            },
        );
        assert_eq!(target, "0x7b ");
    }

    #[test]
    fn test_write_unsigned_hex_upper() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::HexUpper,
            &FormatArgs::default(),
        );
        assert_eq!(target, "7B");
    }

    #[test]
    fn test_write_unsigned_hex_upper_alternative_form() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::HexUpper,
            &FormatArgs {
                alternative_form: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "0X7B");
    }

    #[test]
    fn test_write_signed_positive() {
        let mut target = String::new();
        fmt_write_signed(&mut target, 123, &FormatArgs::default());
        assert_eq!(target, "123");
    }

    #[test]
    fn test_write_signed_positive_signed() {
        let mut target = String::new();
        fmt_write_signed(
            &mut target,
            123,
            &FormatArgs {
                signed: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "+123");
    }

    #[test]
    fn test_write_signed_positive_prefix_space() {
        let mut target = String::new();
        fmt_write_signed(
            &mut target,
            123,
            &FormatArgs {
                prefix_space: true,
                ..Default::default()
            },
        );
        assert_eq!(target, " 123");
    }

    #[test]
    fn test_write_signed_negative() {
        let mut target = String::new();
        fmt_write_signed(&mut target, -123, &FormatArgs::default());
        assert_eq!(target, "-123");
    }

    #[test]
    fn test_write_signed_negative_signed() {
        let mut target = String::new();
        fmt_write_signed(
            &mut target,
            -123,
            &FormatArgs {
                signed: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "-123");
    }

    #[test]
    fn test_write_signed_negative_prefix_space() {
        let mut target = String::new();
        fmt_write_signed(
            &mut target,
            -123,
            &FormatArgs {
                prefix_space: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "-123");
    }

    #[test]
    fn test_write_positive_signed_with_precision() {
        let mut target = String::new();
        fmt_write_signed(
            &mut target,
            123,
            &FormatArgs {
                precision: Some(5),
                ..Default::default()
            },
        );
        assert_eq!(target, "00123");
    }

    #[test]
    fn test_write_negative_signed_with_precision() {
        let mut target = String::new();
        fmt_write_signed(
            &mut target,
            -123,
            &FormatArgs {
                precision: Some(5),
                ..Default::default()
            },
        );
        assert_eq!(target, "-00123");
    }

    #[test]
    fn test_write_positive_signed_with_width() {
        let mut target = String::new();
        fmt_write_signed(
            &mut target,
            123,
            &FormatArgs {
                width: 5,
                ..Default::default()
            },
        );
        assert_eq!(target, "  123");
    }

    #[test]
    fn test_write_negative_signed_with_width() {
        let mut target = String::new();
        fmt_write_signed(
            &mut target,
            -123,
            &FormatArgs {
                width: 5,
                ..Default::default()
            },
        );
        assert_eq!(target, " -123");
    }

    #[test]
    fn test_write_positive_signed_with_width_zero_padded() {
        let mut target = String::new();
        fmt_write_signed(
            &mut target,
            123,
            &FormatArgs {
                width: 5,
                zero_padded: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "00123");
    }

    #[test]
    fn test_write_positive_signed_with_width_with_sign_zero_padded() {
        let mut target = String::new();
        fmt_write_signed(
            &mut target,
            123,
            &FormatArgs {
                width: 5,
                zero_padded: true,
                signed: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "+0123");
    }

    #[test]
    fn test_write_positive_signed_with_width_with_space_zero_padded() {
        let mut target = String::new();
        fmt_write_signed(
            &mut target,
            123,
            &FormatArgs {
                width: 5,
                zero_padded: true,
                prefix_space: true,
                ..Default::default()
            },
        );
        assert_eq!(target, " 0123");
    }

    #[test]
    fn test_write_negative_signed_with_width_zero_padded() {
        let mut target = String::new();
        fmt_write_signed(
            &mut target,
            -123,
            &FormatArgs {
                width: 5,
                zero_padded: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "-0123");
    }

    #[test]
    fn test_write_string() {
        let mut target = String::new();
        fmt_write_string(&mut target, "hello", &FormatArgs::default());
        assert_eq!(target, "hello");
    }

    #[test]
    fn test_write_string_with_precision_less_than_length() {
        let mut target = String::new();
        fmt_write_string(
            &mut target,
            "hello",
            &FormatArgs {
                precision: Some(3),
                ..Default::default()
            },
        );
        assert_eq!(target, "hel");
    }

    #[test]
    fn test_write_string_with_precision_greater_than_length() {
        let mut target = String::new();
        fmt_write_string(
            &mut target,
            "hello",
            &FormatArgs {
                precision: Some(10),
                ..Default::default()
            },
        );
        assert_eq!(target, "hello");
    }

    #[test]
    fn test_write_string_with_width() {
        let mut target = String::new();
        fmt_write_string(
            &mut target,
            "hello",
            &FormatArgs {
                width: 10,
                ..Default::default()
            },
        );
        assert_eq!(target, "     hello");
    }

    #[test]
    fn test_write_string_left_justified() {
        let mut target = String::new();
        fmt_write_string(
            &mut target,
            "hello",
            &FormatArgs {
                left_justified: true,
                width: 10,
                ..Default::default()
            },
        );
        assert_eq!(target, "hello     ");
    }

    #[test]
    fn test_write_string_left_justified_with_precision() {
        let mut target = String::new();
        fmt_write_string(
            &mut target,
            "hello",
            &FormatArgs {
                left_justified: true,
                width: 10,
                precision: Some(3),
                ..Default::default()
            },
        );
        assert_eq!(target, "hel       ");
    }

    #[test]
    fn test_write_string_with_width_with_precision() {
        let mut target = String::new();
        fmt_write_string(
            &mut target,
            "hello",
            &FormatArgs {
                width: 10,
                precision: Some(3),
                ..Default::default()
            },
        );
        assert_eq!(target, "       hel");
    }
}
