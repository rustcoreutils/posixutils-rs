//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{fmt::Write, str::Chars};

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

/// The radix character of numbers awk writes: always a period, as Rust's
/// formatting writes it, and as awk reads numbers whatever the locale.
const RADIX: char = '.';

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

/// Pads the number written at `target[start..]`, its sign (`sign_len`
/// bytes) first, to the field width: on the right if left-justified, with
/// zeros after the sign if zero-padded, otherwise with spaces before it.
fn pad_number(target: &mut String, start: usize, sign_len: usize, args: &FormatArgs) {
    let padding = args.width.saturating_sub(target[start..].chars().count());
    if padding == 0 {
        return;
    }
    if args.left_justified {
        pad_target(target, padding, b' ');
    } else if args.zero_padded {
        target.insert_str(start + sign_len, &"0".repeat(padding));
    } else {
        target.insert_str(start, &" ".repeat(padding));
    }
}

/// Writes an infinity or NaN (as `inf`/`nan`, upper case for `E`, `F` and
/// `G`) padded to the field width, and returns true; false, writing
/// nothing, for a finite `value`.  The '0' flag does not apply to them.
fn write_inf_or_nan(
    target: &mut String,
    value: f64,
    lowercase_version: bool,
    sign: &str,
    args: &FormatArgs,
) -> bool {
    let text = match (value.is_nan(), value.is_infinite(), lowercase_version) {
        (true, _, true) => "nan",
        (true, _, false) => "NAN",
        (_, true, true) => "inf",
        (_, true, false) => "INF",
        _ => return false,
    };
    let start = target.len();
    target.push_str(sign);
    target.push_str(text);
    let args = FormatArgs {
        zero_padded: false,
        ..args.clone()
    };
    pad_number(target, start, sign.len(), &args);
    true
}

/// Removes the zeros at the end of the fraction of the number at
/// `target[start..]`, and its radix character if no digit follows it; a
/// number without a fraction is left alone.
fn strip_trailing_zeros(target: &mut String, start: usize) {
    if !target[start..].contains(RADIX) {
        return;
    }
    let trimmed = target.trim_end_matches('0').len();
    target.truncate(trimmed);
    if target.ends_with(RADIX) {
        target.pop();
    }
}

/// 2^63: every integral f64 smaller in magnitude fits an `i64`.
pub const I64_LIMIT: f64 = 9_223_372_036_854_775_808.0;
/// 2^64: every nonnegative integral f64 below it fits a `u64`.
pub const U64_LIMIT: f64 = 18_446_744_073_709_551_616.0;

/// The nonnegative `value` as Rust writes it in exponent form with
/// `precision` digits after the radix character: the mantissa, and the
/// exponent of the rounded value.
fn exponent_form(value: f64, precision: usize) -> (String, i32) {
    let mut text = format!("{value:.precision$e}");
    let e = text.find('e').expect("exponent format has an e");
    let exponent = text[e + 1..].parse().expect("an exponent is a number");
    text.truncate(e);
    (text, exponent)
}

/// Writes a number in the style of `%e` from its `mantissa` and `exponent`
/// (see [`exponent_form`]): the mantissa, then `e` (or `E`), the sign of
/// the exponent and at least two digits of it.  With `strip`, the zeros at
/// the end of the fraction are removed (`%g`); with `alternative_form` the
/// radix character is always written.
fn write_scientific(
    target: &mut String,
    (mantissa, exponent): (String, i32),
    lowercase_version: bool,
    strip: bool,
    alternative_form: bool,
) {
    let mantissa_start = target.len();
    target.push_str(&mantissa);
    if strip {
        strip_trailing_zeros(target, mantissa_start);
    }
    if alternative_form && !target[mantissa_start..].contains(RADIX) {
        target.push(RADIX);
    }
    target.push(if lowercase_version { 'e' } else { 'E' });
    target.push(if exponent < 0 { '-' } else { '+' });
    write!(target, "{:02}", exponent.unsigned_abs()).expect("error writing to string");
}

#[derive(Default, Clone)]
pub struct FormatArgs {
    left_justified: bool,
    signed: bool,
    prefix_space: bool,
    alternative_form: bool,
    zero_padded: bool,
    width: usize,
    /// The width was given as `*`; its value must be fetched from the argument
    /// list (see `set_width`).
    width_star: bool,
    precision: Option<usize>,
    /// The precision was given as `.*`; its value must be fetched from the
    /// argument list (see `set_precision`).
    precision_star: bool,
}

impl FormatArgs {
    /// True if the field width was specified as `*` and must be supplied by the
    /// next argument.
    pub fn needs_width_arg(&self) -> bool {
        self.width_star
    }

    /// True if the precision was specified as `.*` and must be supplied by the
    /// next argument.
    pub fn needs_precision_arg(&self) -> bool {
        self.precision_star
    }

    /// Apply a `*` field width fetched from the argument list. Per C/POSIX a
    /// negative width means left-justified with the absolute value.
    pub fn set_width(&mut self, width: i64) -> Result<(), String> {
        if width < 0 {
            self.left_justified = true;
        }
        self.width = checked_field_size(width.unsigned_abs())?;
        Ok(())
    }

    /// Apply a `.*` precision fetched from the argument list. A negative
    /// precision is treated as if the precision were omitted.
    pub fn set_precision(&mut self, precision: i64) -> Result<(), String> {
        self.precision = if precision < 0 {
            None
        } else {
            Some(checked_field_size(precision as u64)?)
        };
        Ok(())
    }

    /// An error if the room the conversion `specifier` takes at least
    /// cannot be allocated: its width, and its precision except for `%s`
    /// and `%c`, where it is the most characters to write.
    pub fn reserve_room(&self, specifier: char, target: &mut String) -> Result<(), String> {
        let precision = match specifier {
            's' | 'c' => 0,
            _ => self.precision.unwrap_or(0),
        };
        let room = self.width.max(precision);
        target
            .try_reserve(room)
            .map_err(|_| "field width or precision too large".to_string())
    }
}

/// The largest field width or precision, that of C (`INT_MAX`).
const MAX_FIELD_SIZE: u64 = i32::MAX as u64;

/// `size` as a field width or precision; an error if it is too large.
fn checked_field_size(size: u64) -> Result<usize, String> {
    if size > MAX_FIELD_SIZE {
        Err("field width or precision too large".to_string())
    } else {
        Ok(size as usize)
    }
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
        let mut number: u64 = 0;
        while let Some(digit) = next.to_digit(10) {
            // saturating: anything past the limit is an error below
            number = number.saturating_mul(10).saturating_add(u64::from(digit));
            *next = iter_next(iter)?;
        }
        checked_field_size(number)
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

    if next == '*' {
        // Width supplied by the next argument; resolved by the caller.
        result.width_star = true;
        next = iter_next(iter)?;
    } else {
        result.width = parse_number(&mut next, iter)?;
    }

    result.precision = if next == '.' {
        next = iter_next(iter)?;
        if next == '*' {
            // Precision supplied by the next argument; resolved by the caller.
            result.precision_star = true;
            next = iter_next(iter)?;
            None
        } else {
            Some(parse_number(&mut next, iter)?)
        }
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
    // The digits of a number never start with a zero, and those of 0 are
    // none at all, so only zeros of precision put one first.
    if args.alternative_form && integer_format == IntegerFormat::Octal && precision <= buffer_length
    {
        precision = buffer_length + 1;
    }

    // > For x or X conversion specifiers, a non-zero result shall have 0x
    // > (or 0X) prefixed to it.
    let hex_prefix = if value == 0 {
        ""
    } else {
        integer_hex_prefix_str(integer_format, args)
    };

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
    write_signed_digits(target, value.is_negative(), &buffer[buffer_start..], args);
}

/// `%d` of the integral `value`, of any magnitude: beyond the range of
/// `i64` its digits are those of the exact value.  A value that is not
/// finite is written as by `%g`.  This is also how awk converts an
/// integral number to a string.
pub fn fmt_write_signed_f64(target: &mut String, value: f64, args: &FormatArgs) {
    if !value.is_finite() {
        fmt_write_integer_as_float(target, value, args);
    } else if value.abs() < I64_LIMIT {
        fmt_write_signed(target, value as i64, args);
    } else {
        let digits = format!("{:.0}", value.abs());
        write_signed_digits(target, value < 0.0, digits.as_bytes(), args);
    }
}

/// An integer conversion of a value it cannot show (one beyond 64 bits, or
/// one that is not finite), written as by `%g` with the same flags and
/// width, as gawk does (though it writes an infinity with a plus sign;
/// this writes it as C's `%g` does).
pub fn fmt_write_integer_as_float(target: &mut String, value: f64, args: &FormatArgs) {
    let args = FormatArgs {
        precision: None,
        alternative_form: false,
        ..args.clone()
    };
    fmt_write_float_general(target, value, true, &args);
}

/// The decimal `digits` of an integer's magnitude, with its sign, as `%d`
/// writes them.
fn write_signed_digits(target: &mut String, is_negative: bool, digits: &[u8], args: &FormatArgs) {
    let buffer_length = digits.len();
    let precision = args.precision.unwrap_or(1);
    // Per C standard, precision overrides zero-flag for integer conversions
    let zero_padded = args.zero_padded && args.precision.is_none();

    let sign = sign_str(is_negative, args);
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
        copy_buffer_to_target(digits, target);
        pad_target(target, args.width.saturating_sub(number_length), b' ');
    } else if zero_padded {
        target.push_str(sign);
        pad_target(target, args.width.saturating_sub(number_length), b'0');
        pad_target(target, precision.saturating_sub(buffer_length), b'0');
        copy_buffer_to_target(digits, target);
    } else {
        pad_target(target, args.width.saturating_sub(number_length), b' ');
        target.push_str(sign);
        pad_target(target, precision.saturating_sub(buffer_length), b'0');
        copy_buffer_to_target(digits, target);
    }
}

pub fn fmt_write_hex_float(
    target: &mut String,
    value: f64,
    lowercase_version: bool,
    args: &FormatArgs,
) {
    let sign = sign_str(value.is_sign_negative(), args);
    if write_inf_or_nan(target, value, lowercase_version, sign, args) {
        return;
    }

    let bits = value.to_bits();
    let biased_exponent = ((bits >> 52) & 0x7ff) as i64;
    let (exponent, first_digit) = if biased_exponent == 0 {
        // subnormal number
        (-1022, '0')
    } else {
        (biased_exponent - 1023, '1')
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
            target.push(RADIX);
            copy_buffer_to_target(&fraction_buffer[..fraction_buffer_length], target);
            pad_target(target, extra_trailing_zeros, b'0');
        } else if args.alternative_form {
            target.push(RADIX);
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
    if write_inf_or_nan(target, value, lowercase_version, sign, args) {
        return;
    }
    let precision = args.precision.unwrap_or(6);
    let start = target.len();
    target.push_str(sign);
    write!(target, "{:.1$}", value.abs(), precision).expect("error writing to string");
    if precision == 0 && args.alternative_form {
        target.push(RADIX);
    }
    pad_number(target, start, sign.len(), args);
}

pub fn fmt_write_scientific_float(
    target: &mut String,
    value: f64,
    lowercase_version: bool,
    args: &FormatArgs,
) {
    let sign = sign_str(value.is_sign_negative(), args);
    if write_inf_or_nan(target, value, lowercase_version, sign, args) {
        return;
    }
    let start = target.len();
    target.push_str(sign);
    write_scientific(
        target,
        exponent_form(value.abs(), args.precision.unwrap_or(6)),
        lowercase_version,
        false,
        args.alternative_form,
    );
    pad_number(target, start, sign.len(), args);
}

/// `%g`: with P significant digits (the precision, 6 if omitted, 1 if
/// zero) and X the exponent `%e` would write, the style of `%f` with
/// precision P - 1 - X if P > X >= -4, otherwise that of `%e` with
/// precision P - 1; the zeros at the end of the fraction are then removed,
/// unless the '#' flag is given.
pub fn fmt_write_float_general(
    target: &mut String,
    value: f64,
    lowercase_version: bool,
    args: &FormatArgs,
) {
    let sign = sign_str(value.is_sign_negative(), args);
    if write_inf_or_nan(target, value, lowercase_version, sign, args) {
        return;
    }
    let abs_value = value.abs();
    let significant_digits = args.precision.unwrap_or(6).max(1);
    let scientific = exponent_form(abs_value, significant_digits - 1);
    let exponent = scientific.1;
    let start = target.len();
    target.push_str(sign);
    if exponent < -4 || exponent >= significant_digits as i32 {
        write_scientific(
            target,
            scientific,
            lowercase_version,
            !args.alternative_form,
            args.alternative_form,
        );
    } else {
        let precision = (significant_digits as i32 - 1 - exponent) as usize;
        let number_start = target.len();
        write!(target, "{:.1$}", abs_value, precision).expect("error writing to string");
        if !args.alternative_form {
            strip_trailing_zeros(target, number_start);
        } else if precision == 0 {
            target.push(RADIX);
        }
    }
    pad_number(target, start, sign.len(), args);
}

/// Writes `value` for `%s`, its width and precision counting characters,
/// as in gawk (in a single-byte locale a character is a byte).
pub fn fmt_write_string(target: &mut String, value: &str, args: &FormatArgs) {
    let (value, chars) = match args.precision {
        // all of it, so only a width needs the character count
        None if value.is_ascii() || args.width == 0 => (value, value.len()),
        None => (value, value.chars().count()),
        Some(precision) if value.is_ascii() => {
            let end = value.len().min(precision);
            (&value[..end], end)
        }
        Some(precision) => match value.char_indices().nth(precision) {
            Some((end, _)) => (&value[..end], precision),
            None => (value, value.chars().count()),
        },
    };
    let padding = args.width.saturating_sub(chars);
    if args.left_justified {
        target.push_str(value);
        pad_target(target, padding, b' ');
    } else {
        pad_target(target, padding, b' ');
        target.push_str(value);
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
        fmt_write_hex_float(&mut target, 5e-324, true, &FormatArgs::default());
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
            0.123456789,
            true,
            &FormatArgs {
                precision: Some(6),
                ..Default::default()
            },
        );
        assert_eq!(target, "0.123457");
    }
}
