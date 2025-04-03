//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fmt::Write;
use std::str::Chars;

const BASE_8_DIGITS: [char; 8] = ['0', '1', '2', '3', '4', '5', '6', '7'];
const BASE_10_DIGITS: [char; 10] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
const BASE_16_DIGITS_LOWER: [char; 16] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
];
const BASE_16_DIGITS_UPPER: [char; 16] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
];

fn integer_hex_prefix_str(integer_format: IntegerFormat, args: &FormatArgs) -> &'static str {
    if args.alternative_form {
        if integer_format == IntegerFormat::HexLower {
            return "0x";
        } else if integer_format == IntegerFormat::HexUpper {
            return "0X";
        }
    }
    ""
}

fn decimal_point() -> char {
    if cfg!(miri) {
        '.'
    } else {
        unsafe { (*(*libc::localeconv()).decimal_point) as u8 as char }
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

fn number_to_digits(buffer: &mut [u8], mut value: u64, base: u64, digits: &[char]) -> usize {
    let mut index = buffer.len();
    while value != 0 {
        index -= 1;
        buffer[index] = digits[(value % base) as usize] as u8;
        value /= base;
    }
    buffer.len() - index
}

fn sign_str(is_negative: bool, args: &FormatArgs) -> &str {
    if is_negative {
        "-"
    } else if args.signed {
        "+"
    } else if args.prefix_space {
        " "
    } else {
        ""
    }
}

fn write_inf_or_nan(target: &mut String, value: f64, lowercase_version: bool, sign: &str) -> bool {
    if value.is_nan() {
        target.push_str(sign);
        target.push_str(if lowercase_version { "nan" } else { "NAN" });
        true
    } else if value.is_infinite() {
        target.push_str(sign);
        target.push_str(if lowercase_version { "inf" } else { "INF" });
        true
    } else {
        false
    }
}

/// swaps the sign at `target[write_starting_index]` with the last space
/// padding character before the number
/// # Panics
/// Panics if:
/// - `sign` is neither an empty string nor a single ASCII character
/// - `sign` is not the first character in the string
/// - the character before the first digit in the substring starting at `write_starting_index`
///   is not an ASCII character
fn swap_sign_in_front_of_number(target: &mut String, sign: &str, write_starting_index: usize) {
    // sign is ASCII or empty string
    assert!(sign.len() <= 1);
    if !sign.is_empty() {
        // sign is the first character in the string
        assert_eq!(
            target[write_starting_index..write_starting_index + 1],
            *sign
        );

        let sign_index = write_starting_index;
        // we know that at least one digit is written, so we can safely unwrap
        let first_digit_in_substring = target[write_starting_index..]
            .find(|c: char| c.is_ascii_digit())
            .unwrap();
        let final_sign_index = write_starting_index + first_digit_in_substring - 1;

        // the first byte before the start of the number is ASCII
        assert!(target.as_bytes()[final_sign_index].is_ascii());

        // both the value at sign_index and final_sign_index are single byte ASCII character,
        // so it's safe to swap them
        unsafe { target.as_bytes_mut().swap(sign_index, final_sign_index) };
    }
}

/// Removes the exponent part of the number at the end of `target` and writes it to `exponent_buffer`.
/// The exponent character is removed from `target` but is not written to `exponent_buffer`.
fn gather_exponent(target: &mut String) -> ([u8; 5], usize) {
    // there are at most 5 characters after the 'e'. One for the sign and four for the exponent
    // (4 = ceil(log10(2^11)), where 11 is the number of bits in the exponent of an f64)
    let mut exponent_buffer = [0u8; 5];
    let mut exponent_buffer_length = 0;
    while matches!(target.as_bytes().last(), Some(c) if *c != b'e') {
        exponent_buffer[exponent_buffer_length] = target.pop().unwrap() as u8;
        exponent_buffer_length += 1;
    }
    // pop the 'e' character
    target.pop();
    (exponent_buffer, exponent_buffer_length)
}

fn write_exponent(
    target: &mut String,
    exponent_buffer: &[u8; 5],
    mut exponent_buffer_length: usize,
    lowercase_version: bool,
    should_add_dot_after_number: bool,
) {
    if should_add_dot_after_number {
        target.push(decimal_point());
    }

    // push the exponent character
    if lowercase_version {
        target.push('e');
    } else {
        target.push('E');
    }

    // push the sign character
    if exponent_buffer[exponent_buffer_length - 1] != b'-' {
        target.push('+');
    } else {
        target.push('-');
        exponent_buffer_length -= 1;
    }

    // the exponent value should have at least two digits
    if exponent_buffer_length <= 1 {
        target.push('0');
    }
    // copy the rest of the buffer
    for c in exponent_buffer[0..exponent_buffer_length].iter().rev() {
        target.push(*c as char);
    }
}

fn remove_trailing_zeros(
    target: &mut String,
    sign: &str,
    write_starting_index: usize,
    width: usize,
    zero_padded: bool,
    left_justified: bool,
) {
    let trailing_spaces = target.chars().rev().take_while(|c| *c == ' ').count();
    let trailing_zeros = target
        .chars()
        .rev()
        .skip(trailing_spaces)
        .take_while(|c| *c == '0')
        .count();
    let final_number_length = target.len() - write_starting_index - trailing_zeros;

    if final_number_length >= width {
        // the number cannot have trailing spaces and trailing zeros, otherwise it would mean that
        // even if we remove chars we could still stay under width, but this cannot be, as any padding
        // chars mean that the number alone was not wide enough
        assert!(trailing_spaces == 0 || trailing_zeros == 0);

        target.truncate(target.len() - trailing_zeros);
    } else if left_justified {
        let first_trailing_zero = target.len() - trailing_spaces - trailing_zeros;
        // this is safe as we just swap ascii bytes for other ascii bytes. No multibyte chars
        // involved
        unsafe {
            let bytes =
                target[first_trailing_zero..first_trailing_zero + trailing_zeros].as_bytes_mut();
            for c in bytes {
                *c = b' ';
            }
        }
    } else if zero_padded {
        // we need to rotate the zeros after the sign
        let first = write_starting_index + sign.is_empty() as usize;
        // we know that the last `trailing_zeros` bytes of `target` are b'0',
        // so it's safe to rotate them (no multibyte chars are split)
        unsafe { target[first..].as_bytes_mut().rotate_right(trailing_zeros) };
    } else {
        unsafe {
            let bytes = target.as_bytes_mut();
            // rotate is safe, see above
            bytes.rotate_right(trailing_zeros);
            // we just replace b'0' with b' '. No multibyte chars are involved
            for b in bytes[..trailing_zeros].iter_mut() {
                *b = b' ';
            }
        }
    }
    if target.chars().last().is_some_and(|c| c == decimal_point()) {
        target.pop();
    }
}

fn base_scientific_float_format(
    args: &FormatArgs,
    target: &mut String,
    sign: &str,
    value: f64,
    precision: usize,
    width: usize,
) {
    // left justified
    //   sign integer_part <decimal_point> fractional_part e exponent_sign exponent_value padding
    // right justified zero padded
    //   sign padding integer_part <decimal point> fractional_part e exponent_sign exponent_value
    // right justified space padded
    //  padding sign integer_part <decimal point> fractional_part e exponent_sign exponent_value

    let write_starting_index = target.len();
    target.push_str(sign);
    if args.left_justified {
        write!(target, "{:.1$e}", value, precision).expect("error writing to string");
    } else if args.zero_padded {
        write!(target, "{:01$.2$e}", value, width, precision).expect("error writing to string")
    } else {
        write!(target, "{:1$.2$e}", value, width, precision).expect("error writing to string");
        swap_sign_in_front_of_number(target, sign, write_starting_index);
    }
}

#[derive(Default, Clone)]
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
                c if c.is_ascii_digit() => {
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

    result.precision = if next == decimal_point() {
        next = iter_next(iter)?;
        Some(parse_number(&mut next, iter)?)
    } else {
        None
    };

    Ok((next, result))
}

#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq, Eq, Clone, Copy)]
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

    // 22 is the maximum number of digits needed to represent a u64 in base 8 (the lowest base)
    // 22 = ceil(log8(u64::MAX))
    let mut buffer = [0u8; 22];
    let buffer_length = number_to_digits(&mut buffer, value, base, digits);
    let buffer_start = buffer.len() - buffer_length;

    let mut precision = args.precision.unwrap_or(1);

    // regarding the alternative form for octal numbers:
    // > For the o conversion specifier, it shall increase
    // > the precision to force the first digit of the result to be a zero
    if args.alternative_form && integer_format == IntegerFormat::Octal && precision < buffer_length
    {
        precision = buffer_length + 1;
    }

    let hex_prefix = integer_hex_prefix_str(integer_format, args);

    // left justified:
    //    hex_prefix precision buffer padding
    // right justified zero padded:
    //    hex_prefix padding precision buffer
    // right justified space padded:
    //    padding hex_prefix precision buffer

    let number_length = buffer_length.max(precision) + hex_prefix.len();

    if args.left_justified {
        target.push_str(hex_prefix);
        if precision > buffer_length {
            pad_target(target, precision - buffer_length, b'0');
        }
        copy_buffer_to_target(&buffer[buffer_start..], target);
        pad_target(target, args.width.saturating_sub(number_length), b' ');
    } else if args.precision.is_none() && args.zero_padded {
        // > For d, i, o, u, x, and X conversion specifiers, if a precision
        // > is specified, the '0' flag shall be ignored
        target.push_str(hex_prefix);
        pad_target(target, args.width.saturating_sub(number_length), b'0');
        if precision > buffer_length {
            pad_target(target, precision - buffer_length, b'0');
        }
        copy_buffer_to_target(&buffer[buffer_start..], target);
    } else {
        pad_target(target, args.width.saturating_sub(number_length), b' ');
        target.push_str(hex_prefix);
        if precision > buffer_length {
            pad_target(target, precision - buffer_length, b'0');
        }
        copy_buffer_to_target(&buffer[buffer_start..], target);
    }
}

pub fn fmt_write_signed(target: &mut String, value: i64, args: &FormatArgs) {
    let unsigned_value = value.unsigned_abs();
    // 20 is the maximum number of digits needed to represent a u64 in base 10
    let mut buffer = [0u8; 20];
    let buffer_length = number_to_digits(&mut buffer, unsigned_value, 10, &BASE_10_DIGITS);
    let buffer_start = buffer.len() - buffer_length;

    let precision = args.precision.unwrap_or(1);

    let sign = sign_str(value.is_negative(), args);
    let number_length = buffer_length.max(precision) + sign.len();

    // left justified:
    //    sign precision buffer padding
    // right justified zero padded:
    //    sign padding precision buffer
    // right justified space padded:
    //    padding sign precision buffer
    if args.left_justified {
        target.push_str(sign);
        pad_target(target, precision.saturating_sub(buffer_length), b'0');
        copy_buffer_to_target(&buffer[buffer_start..], target);
        pad_target(target, args.width.saturating_sub(number_length), b' ');
    } else if args.zero_padded {
        target.push_str(sign);
        pad_target(target, args.width.saturating_sub(number_length), b'0');
        pad_target(target, precision.saturating_sub(buffer_length), b'0');
        copy_buffer_to_target(&buffer[buffer_start..], target);
    } else {
        pad_target(target, args.width.saturating_sub(number_length), b' ');
        target.push_str(sign);
        pad_target(target, precision.saturating_sub(buffer_length), b'0');
        copy_buffer_to_target(&buffer[buffer_start..], target);
    }
}

pub fn fmt_write_hex_float(
    target: &mut String,
    value: f64,
    lowercase_version: bool,
    args: &FormatArgs,
) {
    let sign = sign_str(value.is_sign_negative(), args);
    if write_inf_or_nan(target, value, lowercase_version, sign) {
        return;
    }

    let bits = value.to_bits();
    let biased_exponent = ((bits >> 52) & 0x7ff) as i64;
    let first_digit;
    let exponent;
    if biased_exponent == 0 {
        // subnormal number
        exponent = -1022;
        first_digit = '0';
    } else {
        exponent = biased_exponent - 1023;
        first_digit = '1';
    };
    let exponent_sign = if exponent < 0 { '-' } else { '+' };
    let mut exponent_buffer = [0u8; 4];
    let exponent_buffer_length = number_to_digits(
        &mut exponent_buffer,
        exponent.unsigned_abs(),
        10,
        &BASE_10_DIGITS,
    );
    let exponent_buffer_start = exponent_buffer.len() - exponent_buffer_length;

    let fraction = bits & 0x000f_ffff_ffff_ffff;
    // leading 0s are significant in this case
    let mut fraction_buffer = [b'0'; 13];
    let x_char;
    let p_char;
    if lowercase_version {
        x_char = 'x';
        p_char = 'p';
        number_to_digits(&mut fraction_buffer, fraction, 16, &BASE_16_DIGITS_LOWER);
    } else {
        x_char = 'X';
        p_char = 'P';
        number_to_digits(&mut fraction_buffer, fraction, 16, &BASE_16_DIGITS_UPPER);
    };
    // > if the precision is missing and FLT_RADIX is a power of 2, then the
    // > precision shall be sufficient for an exact representation of the value
    let fraction_buffer_length;
    let extra_trailing_zeros;
    if let Some(precision) = args.precision {
        fraction_buffer_length = fraction_buffer.len().min(precision);
        extra_trailing_zeros = precision.saturating_sub(fraction_buffer_length);
    } else {
        fraction_buffer_length = fraction_buffer
            .iter()
            .rposition(|&c| c != b'0')
            .map_or(0, |i| i + 1);
        extra_trailing_zeros = 0;
    };

    let number_length =
        sign.len() + 4 + fraction_buffer_length + extra_trailing_zeros + 2 + exponent_buffer_length;

    let copy_fraction = |target: &mut String| {
        if fraction_buffer_length != 0 {
            target.push(decimal_point());
            copy_buffer_to_target(&fraction_buffer[..fraction_buffer_length], target);
            pad_target(target, extra_trailing_zeros, b'0');
        } else if args.alternative_form {
            target.push(decimal_point());
        }
    };

    // left justified
    //   sign 0x first_digit <decimal point> fraction p exponent_sign exponent padding
    // right justified zero padded
    //   sign 0x padding first_digit <decimal point> fraction p exponent_sign exponent
    // right justified space padded
    //   padding sign 0x first_digit <decimal point> fraction p exponent_sign exponent
    if args.left_justified {
        target.push_str(sign);
        target.push('0');
        target.push(x_char);
        target.push(first_digit);
        copy_fraction(target);
        target.push(p_char);
        target.push(exponent_sign);
        copy_buffer_to_target(&exponent_buffer[exponent_buffer_start..], target);
        pad_target(target, args.width.saturating_sub(number_length), b' ');
    } else {
        if args.zero_padded {
            target.push_str(sign);
            target.push('0');
            target.push(x_char);
            pad_target(target, args.width.saturating_sub(number_length), b'0');
        } else {
            pad_target(target, args.width.saturating_sub(number_length), b' ');
            target.push_str(sign);
            target.push('0');
            target.push(x_char);
        }
        target.push(first_digit);
        copy_fraction(target);
        target.push(p_char);
        target.push(exponent_sign);
        copy_buffer_to_target(&exponent_buffer[exponent_buffer_start..], target);
    }
}

pub fn fmt_write_decimal_float(
    target: &mut String,
    value: f64,
    lowercase_version: bool,
    args: &FormatArgs,
) {
    let sign = sign_str(value.is_sign_negative(), args);
    if write_inf_or_nan(target, value, lowercase_version, sign) {
        return;
    }

    let value = value.abs();
    let precision = args.precision.unwrap_or(6);
    let write_starting_index = target.len();
    let should_add_dot_after_number = precision == 0 && args.alternative_form;
    let width = args
        .width
        .saturating_sub(sign.len() + should_add_dot_after_number as usize);

    // left justified
    //   sign integer_part <decimal_point> fractional_part padding
    // right justified zero padded
    //   sign padding integer_part <decimal point> fractional_part
    // right justified space padded
    //  padding sign integer_part <decimal point> fractional_part

    if args.left_justified {
        target.push_str(sign);
        write!(target, "{:.1$}", value, precision).expect("error writing to string");
        if should_add_dot_after_number {
            target.push(decimal_point());
        }
        let number_length = target.len() - write_starting_index;
        pad_target(target, args.width.saturating_sub(number_length), b' ');
    } else {
        if args.zero_padded {
            target.push_str(sign);
            write!(target, "{:01$.2$}", value, width, precision).expect("error writing to string");
        } else {
            target.push_str(sign);
            write!(target, "{:1$.2$}", value, width, precision).expect("error writing to string");
            swap_sign_in_front_of_number(target, sign, write_starting_index);
        }
        if should_add_dot_after_number {
            target.push(decimal_point());
        }
    }
}

pub fn fmt_write_scientific_float(
    target: &mut String,
    value: f64,
    lowercase_version: bool,
    args: &FormatArgs,
) {
    let sign = sign_str(value.is_sign_negative(), args);
    if write_inf_or_nan(target, value, lowercase_version, sign) {
        return;
    }

    let value = value.abs();

    let exponent = value.log10().trunc();
    let mut additional_exponent_length = 0;
    // if the exponent is not negative, we need to add a '+' sign to the exponent part
    if !exponent.is_sign_negative() {
        additional_exponent_length += 1;
    }
    // if the exponent value is only one digit, we need to pad it with a zero
    if exponent.abs() < 10.0 {
        additional_exponent_length += 1;
    }

    let value = value.abs();
    let precision = args.precision.unwrap_or(6);
    let write_starting_index = target.len();
    let should_add_dot_after_number = precision == 0 && args.alternative_form;
    let width = args.width.saturating_sub(
        sign.len() + should_add_dot_after_number as usize + additional_exponent_length,
    );

    base_scientific_float_format(args, target, sign, value, precision, width);

    let (exponent_buffer, exponent_buffer_length) = gather_exponent(target);
    write_exponent(
        target,
        &exponent_buffer,
        exponent_buffer_length,
        lowercase_version,
        should_add_dot_after_number,
    );

    if args.left_justified {
        let number_length = target.len() - write_starting_index;
        pad_target(target, args.width.saturating_sub(number_length), b' ');
    }
}

pub fn fmt_write_float_general(
    target: &mut String,
    value: f64,
    lowercase_version: bool,
    args: &FormatArgs,
) {
    let sign = sign_str(value.is_sign_negative(), args);
    if write_inf_or_nan(target, value, lowercase_version, sign) {
        return;
    }

    let abs_value = value.abs();
    let exponent = abs_value.log10().trunc() as i64;

    // the POSIX standard doesn't specify a default value. Here we follow the C standard
    // which uses 6. This also matches other implementations
    // We also want to always print at least one digit
    let significant_digits = args.precision.unwrap_or(6).max(1);

    if exponent < -4 || significant_digits <= exponent.max(0) as usize {
        // in scientific notation, the number of significant digits is the precision plus one
        let precision = significant_digits - 1;

        let mut additional_exponent_length = 0;
        // if the exponent is not negative, we need to add a '+' sign to the exponent part
        if !exponent.is_negative() {
            additional_exponent_length += 1;
        }
        // if the exponent value is only one digit, we need to pad it with a zero
        if exponent.abs() < 10 {
            additional_exponent_length += 1;
        }

        let value = value.abs();
        let write_starting_index = target.len();
        let should_add_dot_after_number = precision == 0 && args.alternative_form;
        let width = args.width.saturating_sub(
            sign.len() + should_add_dot_after_number as usize + additional_exponent_length,
        );

        base_scientific_float_format(args, target, sign, value, precision, width);

        let (exponent_buffer, exponent_buffer_length) = gather_exponent(target);

        if !args.alternative_form {
            remove_trailing_zeros(
                target,
                sign,
                write_starting_index,
                width,
                args.zero_padded,
                args.left_justified,
            );
        }

        write_exponent(
            target,
            &exponent_buffer,
            exponent_buffer_length,
            lowercase_version,
            should_add_dot_after_number,
        );

        if args.left_justified {
            let number_length = target.len() - write_starting_index;
            pad_target(target, args.width.saturating_sub(number_length), b' ');
        }
    } else {
        let contains_significant_integer_digits = value.trunc() != 0.0;
        // in decimal notation, the number of significant digits is the precision plus the number of
        // digits in the integer part of the number. If the integer part is 0, the leading zero is
        // not considered a significant digit
        let precision = significant_digits.saturating_sub(
            exponent.max(0) as usize + contains_significant_integer_digits as usize,
        );

        let value = value.abs();
        let write_starting_index = target.len();
        let should_add_dot_after_number = precision == 0 && args.alternative_form;
        let width = args
            .width
            .saturating_sub(sign.len() + should_add_dot_after_number as usize);

        // left justified
        //   sign integer_part <decimal_point> fractional_part padding
        // right justified zero padded
        //   sign padding integer_part <decimal point> fractional_part
        // right justified space padded
        //  padding sign integer_part <decimal point> fractional_part

        if args.left_justified {
            target.push_str(sign);
            write!(target, "{:.1$}", value, precision).expect("error writing to string");
        } else if args.zero_padded {
            target.push_str(sign);
            write!(target, "{:01$.2$}", value, width, precision).expect("error writing to string");
        } else {
            target.push_str(sign);
            write!(target, "{:1$.2$}", value, width, precision).expect("error writing to string");
            swap_sign_in_front_of_number(target, sign, write_starting_index);
        }

        // in this case `precision` is an upper bound on the number of decimal digits
        if !args.alternative_form {
            remove_trailing_zeros(
                target,
                sign,
                write_starting_index,
                width,
                args.zero_padded,
                args.left_justified,
            );
        }

        if should_add_dot_after_number {
            target.push(decimal_point());
        }

        if args.left_justified {
            let number_length = target.len() - write_starting_index;
            pad_target(target, args.width.saturating_sub(number_length), b' ');
        }
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
        assert!(args.left_justified);
        assert!(args.signed);
        assert!(args.prefix_space);
        assert!(args.alternative_form);
        assert!(args.zero_padded);
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
    fn test_write_unsigned_with_precision_ignores_zero_padding() {
        let mut target = String::new();
        fmt_write_unsigned(
            &mut target,
            123,
            IntegerFormat::Decimal,
            &FormatArgs {
                precision: Some(5),
                width: 10,
                zero_padded: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "     00123");
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

    #[test]
    fn test_write_float_hex_lower() {
        let mut target = String::new();
        fmt_write_hex_float(&mut target, 123.456, true, &FormatArgs::default());
        assert_eq!(target, "0x1.edd2f1a9fbe77p+6");
    }

    #[test]
    fn test_write_float_hex_lower_integer_number() {
        let mut target = String::new();
        fmt_write_hex_float(&mut target, 2.0, true, &FormatArgs::default());
        assert_eq!(target, "0x1p+1");
    }

    #[test]
    fn test_write_float_hex_lower_alternative_form() {
        let mut target = String::new();
        fmt_write_hex_float(
            &mut target,
            2.0,
            true,
            &FormatArgs {
                alternative_form: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "0x1.p+1");
    }

    #[test]
    fn test_write_float_hex_lower_with_lower_precision() {
        let mut target = String::new();
        fmt_write_hex_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                precision: Some(5),
                ..Default::default()
            },
        );
        assert_eq!(target, "0x1.edd2fp+6");
    }

    #[test]
    fn test_write_float_hex_lower_with_higher_precision() {
        let mut target = String::new();
        fmt_write_hex_float(
            &mut target,
            123.5,
            true,
            &FormatArgs {
                precision: Some(15),
                ..Default::default()
            },
        );
        assert_eq!(target, "0x1.ee0000000000000p+6");
    }

    #[test]
    fn test_write_float_hex_lower_with_width() {
        let mut target = String::new();
        fmt_write_hex_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 22,
                ..Default::default()
            },
        );
        assert_eq!(target, "  0x1.edd2f1a9fbe77p+6");
    }

    #[test]
    fn test_write_float_hex_lower_left_justified() {
        let mut target = String::new();
        fmt_write_hex_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                left_justified: true,
                width: 22,
                ..Default::default()
            },
        );
        assert_eq!(target, "0x1.edd2f1a9fbe77p+6  ");
    }

    #[test]
    fn test_write_float_hex_lower_left_justified_zero_padded() {
        let mut target = String::new();
        fmt_write_hex_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                left_justified: true,
                zero_padded: true,
                width: 22,
                ..Default::default()
            },
        );
        assert_eq!(target, "0x1.edd2f1a9fbe77p+6  ");
    }

    #[test]
    fn test_write_float_hex_lower_left_justified_space_padded() {
        let mut target = String::new();
        fmt_write_hex_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                left_justified: true,
                zero_padded: true,
                width: 22,
                ..Default::default()
            },
        );
        assert_eq!(target, "0x1.edd2f1a9fbe77p+6  ");
    }

    #[test]
    fn test_write_float_hex_lower_zero_padded() {
        let mut target = String::new();
        fmt_write_hex_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                zero_padded: true,
                width: 22,
                ..Default::default()
            },
        );
        assert_eq!(target, "0x001.edd2f1a9fbe77p+6");
    }

    #[test]
    fn test_write_float_hex_lower_inf() {
        let mut target = String::new();
        fmt_write_hex_float(&mut target, f64::INFINITY, true, &FormatArgs::default());
        assert_eq!(target, "inf");

        target.clear();
        fmt_write_hex_float(&mut target, f64::NEG_INFINITY, true, &FormatArgs::default());
        assert_eq!(target, "-inf");

        target.clear();
        fmt_write_hex_float(
            &mut target,
            f64::INFINITY,
            true,
            &FormatArgs {
                signed: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "+inf");

        target.clear();
        fmt_write_hex_float(
            &mut target,
            f64::INFINITY,
            true,
            &FormatArgs {
                prefix_space: true,
                ..Default::default()
            },
        );
        assert_eq!(target, " inf");
    }

    #[test]
    fn test_write_float_hex_lower_nan() {
        let mut target = String::new();
        fmt_write_hex_float(&mut target, f64::NAN, true, &FormatArgs::default());
        assert_eq!(target, "nan");

        target.clear();
        fmt_write_hex_float(&mut target, -f64::NAN, true, &FormatArgs::default());
        assert_eq!(target, "-nan");

        target.clear();
        fmt_write_hex_float(
            &mut target,
            f64::NAN,
            true,
            &FormatArgs {
                signed: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "+nan");

        target.clear();
        fmt_write_hex_float(
            &mut target,
            f64::NAN,
            true,
            &FormatArgs {
                prefix_space: true,
                ..Default::default()
            },
        );
        assert_eq!(target, " nan");
    }

    #[test]
    fn test_write_float_hex_lower_subnormal() {
        let mut target = String::new();
        fmt_write_hex_float(
            &mut target,
            4.9406564584124654e-324,
            true,
            &FormatArgs::default(),
        );
        assert_eq!(target, "0x0.0000000000001p-1022");
    }

    #[test]
    fn test_write_hex_upper_float() {
        let mut target = String::new();
        fmt_write_hex_float(&mut target, 123.456, false, &FormatArgs::default());
        assert_eq!(target, "0X1.EDD2F1A9FBE77P+6");
    }

    #[test]
    fn test_write_float_hex_upper_inf() {
        let mut target = String::new();
        fmt_write_hex_float(&mut target, f64::INFINITY, false, &FormatArgs::default());
        assert_eq!(target, "INF");
    }

    #[test]
    fn test_write_float_hex_upper_nan() {
        let mut target = String::new();
        fmt_write_hex_float(&mut target, f64::NAN, false, &FormatArgs::default());
        assert_eq!(target, "NAN");
    }

    #[test]
    fn test_write_decimal_float() {
        let mut target = String::new();
        fmt_write_decimal_float(&mut target, 123.456, true, &FormatArgs::default());
        assert_eq!(target, "123.456000");
    }

    #[test]
    fn test_write_decimal_float_integer_value() {
        let mut target = String::new();
        fmt_write_decimal_float(&mut target, 123.0, true, &FormatArgs::default());
        assert_eq!(target, "123.000000");
    }

    #[test]
    fn test_write_decimal_float_negative_value() {
        let mut target = String::new();
        fmt_write_decimal_float(&mut target, -123.456, true, &FormatArgs::default());
        assert_eq!(target, "-123.456000");
    }

    #[test]
    fn test_write_decimal_float_with_sign() {
        let mut target = String::new();
        fmt_write_decimal_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                signed: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "+123.456000");
    }

    #[test]
    fn test_write_decimal_float_with_prefix_space() {
        let mut target = String::new();
        fmt_write_decimal_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                prefix_space: true,
                ..Default::default()
            },
        );
        assert_eq!(target, " 123.456000");
    }

    #[test]
    fn test_write_decimal_float_zero_precision() {
        let mut target = String::new();
        fmt_write_decimal_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                precision: Some(0),
                ..Default::default()
            },
        );
        assert_eq!(target, "123");
    }

    #[test]
    fn test_write_decimal_float_with_limited_precision() {
        let mut target = String::new();
        fmt_write_decimal_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                precision: Some(2),
                ..Default::default()
            },
        );
        assert_eq!(target, "123.46");
    }

    #[test]
    fn test_write_decimal_float_with_width() {
        let mut target = String::new();
        fmt_write_decimal_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 15,
                ..Default::default()
            },
        );
        assert_eq!(target, "     123.456000");
    }

    #[test]
    fn test_write_negative_decimal_float_with_width() {
        let mut target = String::new();
        fmt_write_decimal_float(
            &mut target,
            -123.456,
            true,
            &FormatArgs {
                width: 15,
                ..Default::default()
            },
        );
        assert_eq!(target, "    -123.456000");
    }

    #[test]
    fn test_write_negative_decimal_float_with_width_into_non_empty_string() {
        let mut target = "hello".to_string();
        fmt_write_decimal_float(
            &mut target,
            -123.456,
            true,
            &FormatArgs {
                width: 15,
                ..Default::default()
            },
        );
        assert_eq!(target, "hello    -123.456000");
    }

    #[test]
    fn test_write_decimal_float_with_width_zero_padded() {
        let mut target = String::new();
        fmt_write_decimal_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 15,
                zero_padded: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "00000123.456000");
    }

    #[test]
    fn test_write_negative_decimal_float_with_width_zero_padded() {
        let mut target = String::new();
        fmt_write_decimal_float(
            &mut target,
            -123.456,
            true,
            &FormatArgs {
                width: 15,
                zero_padded: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "-0000123.456000");
    }

    #[test]
    fn test_write_decimal_float_with_width_with_precision() {
        let mut target = String::new();
        fmt_write_decimal_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 15,
                precision: Some(3),
                ..Default::default()
            },
        );
        assert_eq!(target, "        123.456");
    }

    #[test]
    fn test_write_decimal_float_with_width_left_justified() {
        let mut target = String::new();
        fmt_write_decimal_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                left_justified: true,
                width: 15,
                ..Default::default()
            },
        );
        assert_eq!(target, "123.456000     ");
    }

    #[test]
    fn test_write_decimal_float_alternate_form() {
        let mut target = String::new();
        fmt_write_decimal_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                alternative_form: true,
                precision: Some(0),
                ..Default::default()
            },
        );
        assert_eq!(target, "123.");
    }

    #[test]
    fn test_write_decimal_float_alternate_form_with_width() {
        let mut target = String::new();
        fmt_write_decimal_float(
            &mut target,
            123.0,
            true,
            &FormatArgs {
                alternative_form: true,
                width: 5,
                precision: Some(0),
                ..Default::default()
            },
        );
        assert_eq!(target, " 123.");
    }

    #[test]
    fn test_write_decimal_float_lower_inf() {
        let mut target = String::new();
        fmt_write_decimal_float(&mut target, f64::INFINITY, true, &FormatArgs::default());
        assert_eq!(target, "inf");
    }

    #[test]
    fn test_write_decimal_float_lower_nan() {
        let mut target = String::new();
        fmt_write_decimal_float(&mut target, f64::NAN, true, &FormatArgs::default());
        assert_eq!(target, "nan");
    }

    #[test]
    fn test_write_decimal_float_upper_inf() {
        let mut target = String::new();
        fmt_write_decimal_float(&mut target, f64::INFINITY, false, &FormatArgs::default());
        assert_eq!(target, "INF");
    }

    #[test]
    fn test_write_decimal_float_upper_nan() {
        let mut target = String::new();
        fmt_write_decimal_float(&mut target, f64::NAN, false, &FormatArgs::default());
        assert_eq!(target, "NAN");
    }

    #[test]
    fn test_write_scientific_float() {
        let mut target = String::new();
        fmt_write_scientific_float(&mut target, 123.456, true, &FormatArgs::default());
        assert_eq!(target, "1.234560e+02");
    }

    #[test]
    fn test_write_scientific_float_less_than_one() {
        let mut target = String::new();
        fmt_write_scientific_float(&mut target, 0.789, true, &FormatArgs::default());
        assert_eq!(target, "7.890000e-01");
    }

    #[test]
    fn test_write_scientific_float_two_digit_exponent() {
        let mut target = String::new();
        fmt_write_scientific_float(&mut target, 123e20, true, &FormatArgs::default());
        assert_eq!(target, "1.230000e+22");
    }

    #[test]
    fn test_write_scientific_float_three_digit_exponent() {
        let mut target = String::new();
        fmt_write_scientific_float(&mut target, 123e125, true, &FormatArgs::default());
        assert_eq!(target, "1.230000e+127");
    }

    #[test]
    fn test_write_scientific_float_integer_value() {
        let mut target = String::new();
        fmt_write_scientific_float(&mut target, 123.0, true, &FormatArgs::default());
        assert_eq!(target, "1.230000e+02");
    }

    #[test]
    fn test_write_scientific_float_zero_value() {
        let mut target = String::new();
        fmt_write_scientific_float(&mut target, 0.0, true, &FormatArgs::default());
        assert_eq!(target, "0.000000e+00");
    }

    #[test]
    fn test_write_scientific_float_negative_value() {
        let mut target = String::new();
        fmt_write_scientific_float(&mut target, -123.456, true, &FormatArgs::default());
        assert_eq!(target, "-1.234560e+02");
    }

    #[test]
    fn test_write_scientific_float_with_sign() {
        let mut target = String::new();
        fmt_write_scientific_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                signed: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "+1.234560e+02");
    }

    #[test]
    fn test_write_scientific_float_with_prefix_space() {
        let mut target = String::new();
        fmt_write_scientific_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                prefix_space: true,
                ..Default::default()
            },
        );
        assert_eq!(target, " 1.234560e+02");
    }

    #[test]
    fn test_write_scientific_float_zero_precision() {
        let mut target = String::new();
        fmt_write_scientific_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                precision: Some(0),
                ..Default::default()
            },
        );
        assert_eq!(target, "1e+02");
    }

    #[test]
    fn test_write_scientific_float_with_limited_precision() {
        let mut target = String::new();
        fmt_write_scientific_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                precision: Some(2),
                ..Default::default()
            },
        );
        assert_eq!(target, "1.23e+02");
    }

    #[test]
    fn test_write_scientific_float_with_width() {
        let mut target = String::new();
        fmt_write_scientific_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 15,
                ..Default::default()
            },
        );
        assert_eq!(target, "   1.234560e+02");
    }

    #[test]
    fn test_write_negative_scientific_float_with_width() {
        let mut target = String::new();
        fmt_write_scientific_float(
            &mut target,
            -123.456,
            true,
            &FormatArgs {
                width: 15,
                ..Default::default()
            },
        );
        assert_eq!(target, "  -1.234560e+02");
    }

    #[test]
    fn test_write_negative_scientific_float_with_width_into_non_empty_string() {
        let mut target = "value: ".to_string();
        fmt_write_scientific_float(
            &mut target,
            -123.456,
            true,
            &FormatArgs {
                width: 15,
                ..Default::default()
            },
        );
        assert_eq!(target, "value:   -1.234560e+02");
    }

    #[test]
    fn test_write_scientific_float_with_width_zero_padded() {
        let mut target = String::new();
        fmt_write_scientific_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 15,
                zero_padded: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "0001.234560e+02");
    }

    #[test]
    fn test_write_negative_scientific_float_with_width_zero_padded() {
        let mut target = String::new();
        fmt_write_scientific_float(
            &mut target,
            -123.456,
            true,
            &FormatArgs {
                width: 15,
                zero_padded: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "-001.234560e+02");
    }

    #[test]
    fn test_write_scientific_float_with_width_with_precision() {
        let mut target = String::new();
        fmt_write_scientific_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 15,
                precision: Some(3),
                ..Default::default()
            },
        );
        assert_eq!(target, "      1.235e+02");
    }

    #[test]
    fn test_write_scientific_float_with_width_left_justified() {
        let mut target = String::new();
        fmt_write_scientific_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                left_justified: true,
                width: 15,
                ..Default::default()
            },
        );
        assert_eq!(target, "1.234560e+02   ");
    }

    #[test]
    fn test_write_scientific_float_alternate_form() {
        let mut target = String::new();
        fmt_write_scientific_float(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                alternative_form: true,
                precision: Some(0),
                ..Default::default()
            },
        );
        assert_eq!(target, "1.e+02");
    }

    #[test]
    fn test_write_scientific_float_alternate_form_with_width() {
        let mut target = String::new();
        fmt_write_scientific_float(
            &mut target,
            123.0,
            true,
            &FormatArgs {
                alternative_form: true,
                width: 5,
                precision: Some(0),
                ..Default::default()
            },
        );
        assert_eq!(target, "1.e+02");
    }

    #[test]
    fn test_write_scientific_float_lower_inf() {
        let mut target = String::new();
        fmt_write_scientific_float(&mut target, f64::INFINITY, true, &FormatArgs::default());
        assert_eq!(target, "inf");
    }

    #[test]
    fn test_write_scientific_float_lower_nan() {
        let mut target = String::new();
        fmt_write_scientific_float(&mut target, f64::NAN, true, &FormatArgs::default());
        assert_eq!(target, "nan");
    }

    #[test]
    fn test_write_scientific_float_upper_inf() {
        let mut target = String::new();
        fmt_write_scientific_float(&mut target, f64::INFINITY, false, &FormatArgs::default());
        assert_eq!(target, "INF");
    }

    #[test]
    fn test_write_scientific_float_upper_nan() {
        let mut target = String::new();
        fmt_write_scientific_float(&mut target, f64::NAN, false, &FormatArgs::default());
        assert_eq!(target, "NAN");
    }

    #[test]
    fn test_write_float_general_as_decimal() {
        let mut target = String::new();
        fmt_write_float_general(&mut target, 123.456, true, &FormatArgs::default());
        assert_eq!(target, "123.456");
    }

    #[test]
    fn test_write_float_general_as_scientific_small() {
        let mut target = String::new();
        fmt_write_float_general(&mut target, 0.00001, true, &FormatArgs::default());
        assert_eq!(target, "1e-05");
    }

    #[test]
    fn test_write_float_general_as_decimal_left_space_padded() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 10,
                ..Default::default()
            },
        );
        assert_eq!(target, "   123.456");
    }

    #[test]
    fn test_write_float_general_as_decimal_left_zero_padded() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 10,
                zero_padded: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "000123.456");
    }

    #[test]
    fn test_write_float_general_as_decimal_left_space_padded_with_precision() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 10,
                precision: Some(3),
                ..Default::default()
            },
        );
        assert_eq!(target, "       123");
    }

    #[test]
    fn test_write_float_general_as_decimal_left_zero_padded_with_precision() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 10,
                precision: Some(3),
                zero_padded: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "0000000123");
    }

    #[test]
    fn test_write_signed_float_general_as_decimal_left_space_padded() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 10,
                signed: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "  +123.456");
    }

    #[test]
    fn test_write_signed_float_general_as_decimal_left_zero_padded() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 10,
                zero_padded: true,
                signed: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "+00123.456");
    }

    #[test]
    fn test_write_signed_float_general_as_decimal_left_space_padded_with_precision() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 10,
                signed: true,
                precision: Some(3),
                ..Default::default()
            },
        );
        assert_eq!(target, "      +123");
    }

    #[test]
    fn test_write_signed_float_general_as_decimal_left_zero_padded_with_precision() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 10,
                precision: Some(3),
                signed: true,
                zero_padded: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "+000000123");
    }

    #[test]
    fn test_write_float_general_as_scientific_left_space_padded() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123456.0,
            true,
            &FormatArgs {
                width: 10,
                precision: Some(3),
                ..Default::default()
            },
        );
        assert_eq!(target, "  1.23e+05");
    }

    #[test]
    fn test_write_signed_float_general_as_scientific_left_space_padded() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123456.0,
            true,
            &FormatArgs {
                width: 10,
                precision: Some(3),
                signed: true,
                ..Default::default()
            },
        );
        assert_eq!(target, " +1.23e+05");
    }

    #[test]
    fn test_write_float_general_as_scientific_left_zero_padded() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123456.0,
            true,
            &FormatArgs {
                width: 10,
                precision: Some(3),
                zero_padded: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "001.23e+05");
    }

    #[test]
    fn test_write_signed_float_general_as_scientific_left_zero_padded() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123456.0,
            true,
            &FormatArgs {
                width: 10,
                precision: Some(3),
                signed: true,
                zero_padded: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "+01.23e+05");
    }

    #[test]
    fn test_write_float_general_as_decimal_right_space_padded() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 10,
                left_justified: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "123.456   ");
    }

    #[test]
    fn test_write_float_general_as_decimal_right_space_padded_with_precision() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 10,
                precision: Some(3),
                left_justified: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "123       ");
    }

    #[test]
    fn test_write_signed_float_general_as_decimal_right_space_padded() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 10,
                signed: true,
                left_justified: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "+123.456  ");
    }

    #[test]
    fn test_write_signed_float_general_as_decimal_right_space_padded_with_precision() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123.456,
            true,
            &FormatArgs {
                width: 10,
                signed: true,
                precision: Some(3),
                left_justified: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "+123      ");
    }

    #[test]
    fn test_write_float_general_as_scientific_right_space_padded() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123456.0,
            true,
            &FormatArgs {
                width: 10,
                precision: Some(3),
                left_justified: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "1.23e+05  ");
    }

    #[test]
    fn test_write_signed_float_general_as_scientific_right_space_padded() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123456.0,
            true,
            &FormatArgs {
                width: 10,
                precision: Some(3),
                signed: true,
                left_justified: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "+1.23e+05 ");
    }

    #[test]
    fn test_write_float_general_as_scientific_with_precision() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123456.0,
            true,
            &FormatArgs {
                precision: Some(3),
                ..Default::default()
            },
        );
        assert_eq!(target, "1.23e+05");
    }

    #[test]
    fn test_write_float_general_as_decimal_with_precision() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            123.0,
            true,
            &FormatArgs {
                precision: Some(3),
                ..Default::default()
            },
        );
        assert_eq!(target, "123");
    }

    #[test]
    fn test_write_float_general_as_scientific_alternative_form() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            1000000.0,
            true,
            &FormatArgs {
                alternative_form: true,
                ..Default::default()
            },
        );
        assert_eq!(target, "1.00000e+06");
    }

    #[test]
    fn test_write_float_general_as_scientific_alternative_form_with_one_precision() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            1000000.0,
            true,
            &FormatArgs {
                alternative_form: true,
                precision: Some(1),
                ..Default::default()
            },
        );
        assert_eq!(target, "1.e+06");
    }

    #[test]
    fn test_write_float_general_as_decimal_alternative_form() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            1.234,
            true,
            &FormatArgs {
                alternative_form: true,
                precision: Some(1),
                ..Default::default()
            },
        );
        assert_eq!(target, "1.");
    }

    #[test]
    fn test_write_float_general_as_decimal_without_integer_digits() {
        let mut target = String::new();
        fmt_write_float_general(
            &mut target,
            0.7853981633974,
            true,
            &FormatArgs {
                precision: Some(6),
                ..Default::default()
            },
        );
        assert_eq!(target, "0.785398");
    }
}
