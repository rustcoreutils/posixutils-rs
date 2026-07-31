//
// Copyright (c) 2024-2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::{
    error::Error,
    io::{self, Write},
    iter::Peekable,
    os::unix::ffi::OsStrExt,
    process::ExitCode,
    slice::Iter,
};

// the following structure is a printf format conversion specifier
struct ConvSpec {
    // the conversion specifier character
    spec: char,
    // the 1-based argument operand number of a "%n$" conversion (POSIX
    // 111973-111981); None for an ordinary, unnumbered conversion
    arg_number: Option<usize>,
    // the minimum field width
    width: Option<usize>,
    // the precision
    precision: Option<usize>,
    // the conversion specifier flags
    left_justify: bool,
    sign: bool,
    space: bool,
    alt_form: bool,
    zero_pad: bool,
}

impl Default for ConvSpec {
    fn default() -> Self {
        Self {
            spec: ' ',
            arg_number: Default::default(),
            width: Default::default(),
            precision: Default::default(),
            left_justify: Default::default(),
            sign: Default::default(),
            space: Default::default(),
            alt_form: Default::default(),
            zero_pad: Default::default(),
        }
    }
}

// the following structure fully describes a token from
// a tokenized printf format string
enum Token {
    /// a conversion specifier
    Conversion(ConvSpec),
    /// "\c", which means to end output
    IgnoreRestOfFormat,
    /// a literal string
    Literal(Vec<u8>),
}

enum ParseState {
    Literal,
    Flags,
    Width,
    Precision,
    PrecisionValue,
    Specifier,
}

enum ParsedBackslashSequence {
    IgnoreRestOfFormat,
    Byte(u8),
}

// BusyBox: "\400" is interpreted as the ' ' character (octal 40), followed by the '0' character
// Other implementations ignore the 8th bit, so "\400" is interpreted the same as "\000"
fn parse_octal_sequence(
    first_octal_digit: char,
    peekable: &mut Peekable<Iter<'_, u8>>,
) -> Result<u8, Box<dyn Error>> {
    let mut st = String::with_capacity(3_usize);

    st.push(first_octal_digit);

    let mut added_octal_digit_to_buffer = |pe: &mut Peekable<Iter<'_, u8>>| {
        if let Some(&&octal_digit @ b'0'..=b'7') = pe.peek() {
            st.push(char::from(octal_digit));

            pe.next();

            true
        } else {
            false
        }
    };

    if added_octal_digit_to_buffer(peekable) {
        added_octal_digit_to_buffer(peekable);
    };

    let from_str_radix_result = u16::from_str_radix(&st, 8_u32);

    let parsed = match from_str_radix_result {
        Ok(value) => value,
        Err(parse_int_error) => {
            return Err(Box::from(format!(
                "failed to parse octal sequence '{st}' ({parse_int_error})"
            )));
        }
    };

    // Wrap around if greater than a byte
    let wrapped = if parsed > 255_u16 {
        parsed - 255_u16
    } else {
        parsed
    };

    let wrapped_byte = u8::try_from(wrapped)?;

    Ok(wrapped_byte)
}

fn parse_hexadecimal_sequence(peekable: &mut Peekable<Iter<'_, u8>>) -> Result<u8, Box<dyn Error>> {
    let mut st = String::with_capacity(2_usize);

    let mut added_hexadecimal_digit_to_buffer = |pe: &mut Peekable<Iter<'_, u8>>| match pe.peek() {
        Some(
            &&hexadecimal_digit @ b'0'..=b'9'
            | &&hexadecimal_digit @ b'A'..=b'F'
            | &&hexadecimal_digit @ b'a'..=b'f',
        ) => {
            st.push(char::from(hexadecimal_digit));

            pe.next();

            true
        }
        _ => false,
    };

    if !added_hexadecimal_digit_to_buffer(peekable) {
        return Err(Box::from("missing hexadecimal number in escape"));
    }

    added_hexadecimal_digit_to_buffer(peekable);

    let from_str_radix_result = u16::from_str_radix(&st, 16_u32);

    let hexadecimal_digits_parsed = match from_str_radix_result {
        Ok(value) => value,
        Err(parse_int_error) => {
            return Err(Box::from(format!(
                "failed to parse hexadecimal sequence '{st}' ({parse_int_error})"
            )));
        }
    };

    let hexadecimal_digits_parsed_byte = u8::try_from(hexadecimal_digits_parsed)?;

    Ok(hexadecimal_digits_parsed_byte)
}

// "\x00" for hexadecimal is fairly widely supported, but isn't POSIX
// However, it isn't forbidden by POSIX, because:
//
// "The interpretation of a <backslash> followed by any other sequence of characters is unspecified."
//
// https://pubs.opengroup.org/onlinepubs/9799919799/utilities/printf.html
fn escaped_char(
    peekable: &mut Peekable<Iter<'_, u8>>,
) -> Result<ParsedBackslashSequence, Box<dyn Error>> {
    let Some(byte_after_backslash) = peekable.next() else {
        return Ok(ParsedBackslashSequence::Byte(b'\\'));
    };

    let parsed_byte = match byte_after_backslash {
        &byte @ b'0'..=b'7' => {
            let byte = parse_octal_sequence(char::from(byte), peekable)?;

            return Ok(ParsedBackslashSequence::Byte(byte));
        }
        b'a' => b'\x07',
        b'b' => b'\x08',
        b'c' => return Ok(ParsedBackslashSequence::IgnoreRestOfFormat),
        b'e' => b'\x1B',
        b'f' => b'\x0C',
        b'n' => b'\n',
        b'r' => b'\r',
        b't' => b'\t',
        b'v' => b'\x0B',
        b'x' => {
            // Some implementations fail if the character after "\x" is not a valid hexadecimal digit
            let byte = parse_hexadecimal_sequence(peekable)?;

            return Ok(ParsedBackslashSequence::Byte(byte));
        }
        &ue => ue,
    };

    Ok(ParsedBackslashSequence::Byte(parsed_byte))
}

/// Recognize the `n$` prefix of a numbered conversion specification, POSIX
/// 111973-111975: `"%n$"`, where n is a decimal integer in [1,{NL_ARGMAX}]
/// giving the argument operand number.
///
/// The iterator is positioned just past the introducing `'%'`.  A numbered
/// prefix is recognized only when a run of digits is followed by `'$'`, so
/// the probe runs on a clone and the real iterator is advanced only on a
/// match — leaving `"%05d"` and friends to be parsed as flags and width.
fn parse_arg_number(peekable: &mut Peekable<Iter<'_, u8>>) -> Option<usize> {
    let mut probe = peekable.clone();
    let mut digits = String::new();

    while let Some(&&digit @ b'0'..=b'9') = probe.peek() {
        digits.push(char::from(digit));
        probe.next();
    }

    if digits.is_empty() || probe.peek() != Some(&&b'$') {
        return None;
    }
    probe.next();

    // n is 1-based; "%0$" is not a valid argument number.
    let arg_number = digits.parse::<usize>().ok().filter(|&n| n > 0)?;

    *peekable = probe;
    Some(arg_number)
}

fn tokenize_format_str(format: &[u8]) -> Result<Vec<Token>, Box<dyn Error>> {
    let mut conv_spec = ConvSpec::default();
    let mut literal = Vec::<u8>::with_capacity(format.len());
    let mut precision = String::with_capacity(8_usize);
    let mut state = ParseState::Literal;
    let mut tokens = Vec::<Token>::new();
    let mut width = String::with_capacity(8_usize);

    let mut peekable = format.iter().peekable();

    while let Some(&current_byte) = peekable.next() {
        let mut done_with_char = false;

        while !done_with_char {
            match state {
                ParseState::Literal => {
                    if current_byte == b'%' {
                        if !literal.is_empty() {
                            tokens.push(Token::Literal(literal.clone()));

                            literal.clear();
                        }

                        conv_spec.arg_number = parse_arg_number(&mut peekable);
                        state = ParseState::Flags;
                    } else if current_byte == b'\\' {
                        match escaped_char(&mut peekable)? {
                            ParsedBackslashSequence::Byte(byte) => {
                                literal.push(byte);
                            }
                            ParsedBackslashSequence::IgnoreRestOfFormat => {
                                tokens.push(Token::Literal(literal.clone()));

                                literal.clear();

                                tokens.push(Token::IgnoreRestOfFormat);
                            }
                        }
                    } else {
                        literal.push(current_byte);
                    }

                    done_with_char = true;
                }
                ParseState::Flags => {
                    match current_byte {
                        b'-' => conv_spec.left_justify = true,
                        b'+' => conv_spec.sign = true,
                        b' ' => conv_spec.space = true,
                        b'#' => conv_spec.alt_form = true,
                        b'0' => conv_spec.zero_pad = true,
                        _ => {
                            state = ParseState::Width;

                            continue;
                        }
                    }

                    done_with_char = true;
                }
                ParseState::Width => {
                    if current_byte.is_ascii_digit() {
                        width.push(char::from(current_byte));

                        done_with_char = true;
                    } else {
                        if !width.is_empty() {
                            conv_spec.width = Some(width.parse()?);

                            width.clear();
                        }

                        state = ParseState::Precision;
                    }
                }
                ParseState::Precision => {
                    if current_byte == b'.' {
                        state = ParseState::PrecisionValue;

                        done_with_char = true;
                    } else {
                        state = ParseState::Specifier;
                    }
                }
                ParseState::PrecisionValue => {
                    if current_byte.is_ascii_digit() {
                        precision.push(char::from(current_byte));

                        done_with_char = true;
                    } else {
                        if !precision.is_empty() {
                            conv_spec.precision = Some(precision.parse()?);

                            precision.clear();
                        }

                        state = ParseState::Specifier;
                    }
                }
                ParseState::Specifier => {
                    conv_spec.spec = char::from(current_byte);

                    tokens.push(Token::Conversion(conv_spec));

                    conv_spec = ConvSpec::default();

                    state = ParseState::Literal;

                    done_with_char = true;
                }
            }
        }
    }

    if !literal.is_empty() {
        literal.shrink_to_fit();

        tokens.push(Token::Literal(literal));
    }

    Ok(tokens)
}

/// Format a signed integer (the `d` and `i` conversions) with all POSIX flags
/// applied.  The `+` and <space> flags are meaningful only here: XBD 3569-3570
/// define them in terms of a *signed* conversion.
fn format_integer_signed(conv: &ConvSpec, value: i64) -> Vec<u8> {
    let sign_char = if value < 0 {
        Some('-')
    } else if conv.sign {
        Some('+')
    } else if conv.space {
        Some(' ')
    } else {
        None
    };

    format_integer(conv, value.unsigned_abs(), sign_char, 10, false)
}

/// Format an integer with all POSIX flags applied.
///
/// `magnitude` is the absolute value to render; `sign_char` is the sign (or
/// <space>) to prepend, already decided by the caller, and is always `None`
/// for the unsigned conversions `o`, `u`, `x`, and `X` (XBD 3607-3608).
fn format_integer(
    conv: &ConvSpec,
    magnitude: u64,
    sign_char: Option<char>,
    base: u32,
    upper: bool,
) -> Vec<u8> {
    let abs_value = magnitude;

    // Convert to string in the appropriate base
    let digits: String = match base {
        8 => format!("{abs_value:o}"),
        16 if upper => format!("{abs_value:X}"),
        16 => format!("{abs_value:x}"),
        _ => abs_value.to_string(),
    };

    // Alternate form prefix (#)
    let prefix = if conv.alt_form && abs_value != 0 {
        match base {
            8 => "0",
            16 if upper => "0X",
            16 => "0x",
            _ => "",
        }
    } else {
        ""
    };

    // Apply precision (minimum digits, zero-padded)
    let digits_with_precision = if let Some(prec) = conv.precision {
        if digits.len() < prec {
            format!("{:0>width$}", digits, width = prec)
        } else {
            digits
        }
    } else {
        digits
    };

    // Calculate total content length
    let sign_len = if sign_char.is_some() { 1 } else { 0 };
    let content_len = sign_len + prefix.len() + digits_with_precision.len();

    // Apply width
    let width = conv.width.unwrap_or(0);

    let mut output = Vec::with_capacity(width.max(content_len));

    if width > content_len {
        let padding = width - content_len;

        if conv.left_justify {
            // Left justify: content then spaces
            if let Some(s) = sign_char {
                output.push(s as u8);
            }
            output.extend_from_slice(prefix.as_bytes());
            output.extend_from_slice(digits_with_precision.as_bytes());
            output.resize(output.len() + padding, b' ');
        } else if conv.zero_pad && conv.precision.is_none() {
            // Zero pad: sign/prefix, zeros, digits
            if let Some(s) = sign_char {
                output.push(s as u8);
            }
            output.extend_from_slice(prefix.as_bytes());
            output.resize(output.len() + padding, b'0');
            output.extend_from_slice(digits_with_precision.as_bytes());
        } else {
            // Right justify with spaces
            output.resize(output.len() + padding, b' ');
            if let Some(s) = sign_char {
                output.push(s as u8);
            }
            output.extend_from_slice(prefix.as_bytes());
            output.extend_from_slice(digits_with_precision.as_bytes());
        }
    } else {
        // No padding needed
        if let Some(s) = sign_char {
            output.push(s as u8);
        }
        output.extend_from_slice(prefix.as_bytes());
        output.extend_from_slice(digits_with_precision.as_bytes());
    }

    output
}

/// Parse a floating point argument according to POSIX rules
fn parse_float_arg(arg: &[u8]) -> Result<(f64, bool), String> {
    if arg.is_empty() {
        return Ok((0.0, true));
    }

    let arg_str = match std::str::from_utf8(arg) {
        Ok(s) => s,
        Err(_) => return Err("invalid UTF-8".to_string()),
    };

    let arg_str = arg_str.trim();

    if arg_str.is_empty() {
        return Ok((0.0, true));
    }

    // Check for character constant: 'c or "c
    if (arg_str.starts_with('\'') || arg_str.starts_with('"')) && arg_str.len() >= 2 {
        let ch = arg_str.chars().nth(1).unwrap();
        return Ok((ch as u32 as f64, true));
    }

    // Parse as float
    match arg_str.parse::<f64>() {
        Ok(n) => Ok((n, true)),
        Err(_) => {
            // Try to parse as much as we can
            let mut end = 0;
            let chars: Vec<char> = arg_str.chars().collect();

            // Optional sign
            if !chars.is_empty() && (chars[0] == '+' || chars[0] == '-') {
                end = 1;
            }

            // Digits before decimal
            while end < chars.len() && chars[end].is_ascii_digit() {
                end += 1;
            }

            // Optional decimal point and digits after
            if end < chars.len() && chars[end] == '.' {
                end += 1;
                while end < chars.len() && chars[end].is_ascii_digit() {
                    end += 1;
                }
            }

            // Optional exponent
            if end < chars.len() && (chars[end] == 'e' || chars[end] == 'E') {
                let exp_start = end;
                end += 1;
                if end < chars.len() && (chars[end] == '+' || chars[end] == '-') {
                    end += 1;
                }
                let had_exp_digits = end < chars.len() && chars[end].is_ascii_digit();
                while end < chars.len() && chars[end].is_ascii_digit() {
                    end += 1;
                }
                if !had_exp_digits {
                    end = exp_start; // Backtrack if no exponent digits
                }
            }

            if end == 0 {
                return Err(format!("invalid floating point number: {arg_str}"));
            }

            let parsed: &str = &arg_str[..end];
            match parsed.parse::<f64>() {
                Ok(n) => Ok((n, end == arg_str.len())),
                Err(_) => Err(format!("invalid floating point number: {arg_str}")),
            }
        }
    }
}

/// Format a floating point number in scientific notation with proper exponent formatting
fn format_scientific(value: f64, precision: usize, upper: bool) -> String {
    if value.is_nan() {
        return if upper {
            "NAN".to_string()
        } else {
            "nan".to_string()
        };
    }
    if value.is_infinite() {
        return if upper {
            "INF".to_string()
        } else {
            "inf".to_string()
        };
    }

    // Use Rust's formatting then fix the exponent
    let formatted = if upper {
        format!("{:.prec$E}", value, prec = precision)
    } else {
        format!("{:.prec$e}", value, prec = precision)
    };

    // Fix exponent to have at least 2 digits with sign
    fix_exponent(&formatted, upper)
}

/// Fix exponent format to have sign and at least 2 digits
fn fix_exponent(s: &str, upper: bool) -> String {
    let exp_char = if upper { 'E' } else { 'e' };
    if let Some(pos) = s.find(exp_char) {
        let (mantissa, exp_part) = s.split_at(pos);
        let exp_str = &exp_part[1..]; // Skip 'e' or 'E'

        let (sign, digits) = if let Some(stripped) = exp_str.strip_prefix('-') {
            ('-', stripped)
        } else if let Some(stripped) = exp_str.strip_prefix('+') {
            ('+', stripped)
        } else {
            ('+', exp_str)
        };

        // Pad to at least 2 digits
        let padded = if digits.len() < 2 {
            format!("{:0>2}", digits)
        } else {
            digits.to_string()
        };

        format!("{}{}{}{}", mantissa, exp_char, sign, padded)
    } else {
        s.to_string()
    }
}

/// Format a floating point number in general format (%g/%G)
fn format_general(value: f64, precision: usize, upper: bool, alt_form: bool) -> String {
    if value.is_nan() {
        return if upper {
            "NAN".to_string()
        } else {
            "nan".to_string()
        };
    }
    if value.is_infinite() {
        return if upper {
            "INF".to_string()
        } else {
            "inf".to_string()
        };
    }

    // %g uses precision as "significant figures", minimum 1
    let sig_figs = precision.max(1);

    // Get the exponent
    let exp = if value == 0.0 {
        0
    } else {
        value.abs().log10().floor() as i32
    };

    // Use %e if exponent < -4 or >= precision, else %f style
    let result = if exp < -4 || exp >= sig_figs as i32 {
        // Use scientific notation with sig_figs - 1 decimal places
        let e_prec = sig_figs.saturating_sub(1);
        format_scientific(value, e_prec, upper)
    } else {
        // Use fixed notation
        // Number of decimal places = sig_figs - 1 - exp (for positive exp)
        // or sig_figs - 1 + |exp| (for negative exp)
        let decimal_places = if exp >= 0 {
            sig_figs.saturating_sub(1).saturating_sub(exp as usize)
        } else {
            sig_figs.saturating_sub(1) + (-exp) as usize
        };
        format!("{:.prec$}", value, prec = decimal_places)
    };

    // Remove trailing zeros (unless # flag)
    if !alt_form {
        remove_trailing_zeros(&result)
    } else {
        result
    }
}

/// Format a floating point number in hexadecimal format (%a/%A)
fn format_hex_float(value: f64, precision: usize, upper: bool) -> String {
    if value.is_nan() {
        return if upper {
            "NAN".to_string()
        } else {
            "nan".to_string()
        };
    }
    if value.is_infinite() {
        return if upper {
            "INF".to_string()
        } else {
            "inf".to_string()
        };
    }
    if value == 0.0 {
        let zeros = if precision > 0 {
            format!(".{:0<width$}", "", width = precision)
        } else {
            String::new()
        };
        return if upper {
            format!("0X0{}P+0", zeros)
        } else {
            format!("0x0{}p+0", zeros)
        };
    }

    let bits = value.to_bits();
    let exp_bits = ((bits >> 52) & 0x7FF) as i32;
    let mantissa = bits & 0xFFFFFFFFFFFFF;

    let (exponent, leading_digit) = if exp_bits == 0 {
        // Denormalized
        (-1022, 0)
    } else {
        (exp_bits - 1023, 1)
    };

    // Format mantissa as hex
    let mantissa_hex = format!("{:013x}", mantissa);
    let trimmed = mantissa_hex.trim_end_matches('0');
    let frac_part = if trimmed.is_empty() {
        if precision > 0 {
            format!(".{:0<width$}", "", width = precision)
        } else {
            String::new()
        }
    } else if precision > 0 {
        format!(
            ".{:0<width$}",
            &mantissa_hex[..precision.min(13)],
            width = precision
        )
    } else {
        format!(".{}", trimmed)
    };

    let exp_sign = if exponent >= 0 { '+' } else { '-' };
    let exp_abs = exponent.abs();

    if upper {
        format!("0X{}{frac_part}P{exp_sign}{exp_abs}", leading_digit)
    } else {
        format!("0x{}{frac_part}p{exp_sign}{exp_abs}", leading_digit)
    }
}

/// Format a floating point number with all POSIX flags applied
fn format_float(conv: &ConvSpec, value: f64) -> Vec<u8> {
    let is_negative = value.is_sign_negative() && !value.is_nan();
    let abs_value = value.abs();

    // Default precision is 6
    let precision = conv.precision.unwrap_or(6);

    // Format the number according to specifier
    let formatted = match conv.spec {
        'f' | 'F' => {
            if conv.spec == 'F' && (abs_value.is_infinite() || abs_value.is_nan()) {
                if abs_value.is_infinite() {
                    "INF".to_string()
                } else {
                    "NAN".to_string()
                }
            } else {
                format!("{:.prec$}", abs_value, prec = precision)
            }
        }
        'e' => format_scientific(abs_value, precision, false),
        'E' => format_scientific(abs_value, precision, true),
        'g' => format_general(abs_value, precision, false, conv.alt_form),
        'G' => format_general(abs_value, precision, true, conv.alt_form),
        'a' => format_hex_float(abs_value, precision, false),
        'A' => format_hex_float(abs_value, precision, true),
        _ => format!("{:.prec$}", abs_value, prec = precision),
    };

    // Determine sign
    let sign_char = if is_negative {
        Some('-')
    } else if conv.sign {
        Some('+')
    } else if conv.space {
        Some(' ')
    } else {
        None
    };

    // Calculate total content length
    let sign_len = if sign_char.is_some() { 1 } else { 0 };
    let content_len = sign_len + formatted.len();

    // Apply width
    let width = conv.width.unwrap_or(0);

    let mut output = Vec::with_capacity(width.max(content_len));

    if width > content_len {
        let padding = width - content_len;

        if conv.left_justify {
            if let Some(s) = sign_char {
                output.push(s as u8);
            }
            output.extend_from_slice(formatted.as_bytes());
            output.resize(output.len() + padding, b' ');
        } else if conv.zero_pad {
            if let Some(s) = sign_char {
                output.push(s as u8);
            }
            output.resize(output.len() + padding, b'0');
            output.extend_from_slice(formatted.as_bytes());
        } else {
            output.resize(output.len() + padding, b' ');
            if let Some(s) = sign_char {
                output.push(s as u8);
            }
            output.extend_from_slice(formatted.as_bytes());
        }
    } else {
        if let Some(s) = sign_char {
            output.push(s as u8);
        }
        output.extend_from_slice(formatted.as_bytes());
    }

    output
}

/// Remove trailing zeros after decimal point
fn remove_trailing_zeros(s: &str) -> String {
    if s.contains('.') {
        // Find 'e' or 'E' for exponent
        let exp_pos = s.find(['e', 'E']);
        let (num_part, exp_part) = match exp_pos {
            Some(pos) => (&s[..pos], &s[pos..]),
            None => (s, ""),
        };

        let trimmed = num_part.trim_end_matches('0');
        let trimmed = trimmed.strip_suffix('.').unwrap_or(trimmed);

        format!("{}{}", trimmed, exp_part)
    } else {
        s.to_string()
    }
}

fn format_arg_float(conv: &ConvSpec, arg: &[u8]) -> Result<(Vec<u8>, bool), Box<dyn Error>> {
    let mut had_error = false;
    let value = match parse_float_arg(arg) {
        Ok((n, fully_consumed)) => {
            if !fully_consumed {
                let arg_str = std::str::from_utf8(arg).unwrap_or("<invalid>");
                eprintln!("printf: {arg_str}: not completely converted");
                had_error = true;
            }
            n
        }
        Err(e) => {
            let arg_str = std::str::from_utf8(arg).unwrap_or("<invalid>");
            eprintln!("printf: {arg_str}: {e}");
            had_error = true;
            0.0
        }
    };

    Ok((format_float(conv, value), had_error))
}

/// An integer operand after evaluation as a C integer constant.
struct IntOperand {
    /// The value, saturated into the `i128` range.  A 128-bit accumulator is
    /// wide enough to hold every value either a signed or an unsigned 64-bit
    /// conversion can render, so saturation to the conversion's own range can
    /// be decided later, per conversion specifier.
    value: i128,
    /// False if the operand had trailing bytes that were not converted.
    fully_consumed: bool,
    /// True if the operand's magnitude exceeded what could be represented.
    overflowed: bool,
}

/// Parse an integer argument according to POSIX rules (112013-112018):
/// - Leading + or - allowed
/// - 0x or 0X prefix for hexadecimal
/// - 0 prefix for octal
/// - 'c or "c for character constant (value of character c)
fn parse_integer_arg(arg: &[u8]) -> Result<IntOperand, String> {
    let operand = |value: i128| IntOperand {
        value,
        fully_consumed: true,
        overflowed: false,
    };

    if arg.is_empty() {
        return Ok(operand(0));
    }

    let arg_str = match std::str::from_utf8(arg) {
        Ok(s) => s,
        Err(_) => return Err("invalid UTF-8".to_string()),
    };

    let arg_str = arg_str.trim();

    if arg_str.is_empty() {
        return Ok(operand(0));
    }

    // Check for character constant: 'c or "c
    if (arg_str.starts_with('\'') || arg_str.starts_with('"')) && arg_str.len() >= 2 {
        let ch = arg_str.chars().nth(1).unwrap();
        return Ok(operand(i128::from(u32::from(ch))));
    }

    // Handle sign
    let (is_negative, num_str) = if let Some(rest) = arg_str.strip_prefix('-') {
        (true, rest)
    } else if let Some(rest) = arg_str.strip_prefix('+') {
        (false, rest)
    } else {
        (false, arg_str)
    };

    // Split the magnitude into a radix and the run of digits valid for it.
    // Only digits of the right radix are handed to the parser, so the sole
    // remaining failure mode is a magnitude too large for i128 — which is
    // saturated rather than rejected, per 112020-112024.
    let (radix, digits) = if let Some(hex) = num_str
        .strip_prefix("0x")
        .or_else(|| num_str.strip_prefix("0X"))
    {
        (16, hex)
    } else if num_str.len() > 1 && num_str.starts_with('0') {
        (8, num_str)
    } else {
        (10, num_str)
    };

    let valid_len = digits.chars().take_while(|c| c.is_digit(radix)).count();
    if valid_len == 0 {
        return Err(format!("invalid number: {arg_str}"));
    }

    let (magnitude, overflowed) = match i128::from_str_radix(&digits[..valid_len], radix) {
        Ok(n) => (n, false),
        Err(_) => (i128::MAX, true),
    };

    Ok(IntOperand {
        value: if is_negative { -magnitude } else { magnitude },
        fully_consumed: valid_len == digits.len(),
        overflowed,
    })
}

/// Evaluate an integer operand, emitting the diagnostics POSIX 112020-112024
/// requires.  Returns the value and whether an error was reported; on error
/// the caller still writes the accumulated value, as the spec directs.
fn eval_integer_arg(arg: &[u8]) -> (IntOperand, bool) {
    match parse_integer_arg(arg) {
        Ok(operand) => {
            let mut had_error = false;
            if !operand.fully_consumed {
                let arg_str = String::from_utf8_lossy(arg);
                eprintln!("printf: {arg_str}: not completely converted");
                had_error = true;
            }
            if operand.overflowed {
                let arg_str = String::from_utf8_lossy(arg);
                eprintln!("printf: {arg_str}: arithmetic overflow");
                had_error = true;
            }
            (operand, had_error)
        }
        Err(e) => {
            let arg_str = String::from_utf8_lossy(arg);
            eprintln!("printf: {arg_str}: {e}");
            (
                IntOperand {
                    value: 0,
                    fully_consumed: true,
                    overflowed: false,
                },
                true,
            )
        }
    }
}

/// Format an argument under one of the unsigned conversions (`o`, `u`, `x`,
/// `X`).  A negative operand is reinterpreted as its two's-complement bit
/// pattern, matching `strtoul()` and XBD 3607-3608's "unsigned octal /
/// unsigned decimal / unsigned hexadecimal notation".
fn format_arg_uint_base(conv: &ConvSpec, arg: &[u8], base: u32, upper: bool) -> (Vec<u8>, bool) {
    let (operand, mut had_error) = eval_integer_arg(arg);

    let magnitude = if operand.value < 0 {
        // Saturate into the signed range first, then reinterpret the bits.
        let signed = if operand.value < i128::from(i64::MIN) {
            had_error = true;
            i64::MIN
        } else {
            operand.value as i64
        };
        signed as u64
    } else if operand.value > i128::from(u64::MAX) {
        had_error = true;
        u64::MAX
    } else {
        operand.value as u64
    };

    (
        format_integer(conv, magnitude, None, base, upper),
        had_error,
    )
}

/// Format an argument under the signed conversions (`d`, `i`).  An
/// out-of-range operand saturates to `i64::MIN`/`i64::MAX`, which is what
/// `strtol()` returns and what the spec's EXAMPLES (112117-112120) show.
fn format_arg_int(conv: &ConvSpec, arg: &[u8]) -> (Vec<u8>, bool) {
    let (operand, mut had_error) = eval_integer_arg(arg);

    let value = if operand.value < i128::from(i64::MIN) {
        had_error = true;
        i64::MIN
    } else if operand.value > i128::from(i64::MAX) {
        had_error = true;
        i64::MAX
    } else {
        operand.value as i64
    };

    (format_integer_signed(conv, value), had_error)
}

fn format_arg_char(conv: &ConvSpec, arg: &[u8]) -> Vec<u8> {
    let arg_to_use = arg.get(..1_usize).unwrap_or(&[]);

    format_arg_string(conv, arg_to_use)
}

/// Result of processing %b argument
enum ProcessBResult {
    /// Normal bytes to output
    Bytes(Vec<u8>),
    /// \c was encountered - stop all output
    StopOutput(Vec<u8>),
}

/// Process backslash escapes in a %b argument
/// Returns the processed bytes and whether \c was encountered
fn process_b_escapes(arg: &[u8]) -> ProcessBResult {
    let mut output = Vec::with_capacity(arg.len());
    let mut peekable = arg.iter().peekable();

    while let Some(&byte) = peekable.next() {
        if byte == b'\\' {
            let Some(&next_byte) = peekable.next() else {
                // Trailing backslash - preserve it
                output.push(b'\\');
                continue;
            };

            match next_byte {
                b'\\' => output.push(b'\\'),
                b'a' => output.push(b'\x07'),
                b'b' => output.push(b'\x08'),
                b'c' => return ProcessBResult::StopOutput(output),
                b'f' => output.push(b'\x0c'),
                b'n' => output.push(b'\n'),
                b'r' => output.push(b'\r'),
                b't' => output.push(b'\t'),
                b'v' => output.push(b'\x0b'),
                b'0' => {
                    // Octal escape: \0ddd where ddd is 0-3 octal digits
                    let mut octal_value: u8 = 0;
                    let mut digits = 0;

                    while digits < 3 {
                        if let Some(&&digit) = peekable.peek() {
                            if (b'0'..=b'7').contains(&digit) {
                                octal_value =
                                    octal_value.wrapping_mul(8).wrapping_add(digit - b'0');
                                peekable.next();
                                digits += 1;
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    output.push(octal_value);
                }
                _ => {
                    // Unknown escape - output the character after backslash
                    output.push(next_byte);
                }
            }
        } else {
            output.push(byte);
        }
    }

    ProcessBResult::Bytes(output)
}

fn format_arg_b(conv: &ConvSpec, arg: &[u8]) -> (Vec<u8>, bool) {
    match process_b_escapes(arg) {
        ProcessBResult::Bytes(bytes) => (format_arg_string(conv, &bytes), false),
        ProcessBResult::StopOutput(bytes) => (format_arg_string(conv, &bytes), true),
    }
}

fn format_arg_string(conv: &ConvSpec, arg: &[u8]) -> Vec<u8> {
    // Apply precision to limit string length
    let arg = if let Some(prec) = conv.precision {
        if prec < arg.len() {
            &arg[..prec]
        } else {
            arg
        }
    } else {
        arg
    };

    let arg_len = arg.len();

    let Some(width) = conv.width else {
        return arg.to_vec();
    };

    if width <= arg_len {
        return arg.to_vec();
    }

    // For strings, zero padding is treated as space padding
    let padding_char = if conv.zero_pad && conv.spec != 's' && conv.spec != 'b' {
        b'0'
    } else {
        b' '
    };

    let mut output = Vec::<u8>::with_capacity(width);

    if conv.left_justify {
        output.extend_from_slice(arg);
        for _ in 0..(width - arg_len) {
            output.push(padding_char);
        }
    } else {
        for _ in 0..(width - arg_len) {
            output.push(padding_char);
        }
        output.extend_from_slice(arg);
    }

    output
}

/// Result of formatting an argument
struct FormatResult {
    bytes: Vec<u8>,
    stop_output: bool,
    had_error: bool,
}

fn format_arg(conv: &ConvSpec, arg: &[u8]) -> Result<FormatResult, Box<dyn Error>> {
    let (bytes, stop_output, had_error) = match conv.spec {
        'd' | 'i' => {
            let (bytes, had_error) = format_arg_int(conv, arg);
            (bytes, false, had_error)
        }
        'u' | 'o' | 'x' | 'X' => {
            let (base, upper) = match conv.spec {
                'o' => (8, false),
                'x' => (16, false),
                'X' => (16, true),
                _ => (10, false),
            };
            let (bytes, had_error) = format_arg_uint_base(conv, arg, base, upper);
            (bytes, false, had_error)
        }
        'f' | 'F' | 'e' | 'E' | 'g' | 'G' | 'a' | 'A' => {
            let (bytes, had_error) = format_arg_float(conv, arg)?;
            (bytes, false, had_error)
        }
        'c' => (format_arg_char(conv, arg), false, false),
        's' => (format_arg_string(conv, arg), false, false),
        'b' => {
            let (bytes, stop) = format_arg_b(conv, arg);
            (bytes, stop, false)
        }
        '%' => (vec![b'%'], false, false),
        ch => {
            eprintln!("printf: unknown conversion specifier: {ch}");
            (format_arg_string(conv, arg), false, true)
        }
    };

    Ok(FormatResult {
        bytes,
        stop_output,
        had_error,
    })
}

/// Returns Ok(had_error) where had_error indicates if any conversion errors occurred
fn do_printf(format: &[u8], arguments: &[&[u8]]) -> Result<bool, Box<dyn Error>> {
    let token_vec = tokenize_format_str(format)?;

    let mut output = Vec::<u8>::with_capacity(format.len() * 2_usize);
    let mut stop_output = false;
    let mut had_error = false;

    let had_conversion = token_vec
        .iter()
        .any(|t| matches!(t, Token::Conversion(c) if c.spec != '%'));

    // Operands consumed by previous uses of the format operand.  POSIX
    // 111994-111996: on format reuse, the n of a "%n$" conversion refers to
    // the nth operand *following* the highest-numbered operand the previous
    // use consumed.
    let mut base = 0_usize;

    // Process format, reusing it if there are remaining arguments
    loop {
        // Running index for unnumbered conversions, and the highest 1-based
        // operand number a numbered conversion named during this pass.
        let mut next_unnumbered = base;
        let mut highest_numbered = 0_usize;

        for token in &token_vec {
            if stop_output {
                break;
            }

            match token {
                Token::Conversion(co) => {
                    // %% doesn't consume an argument
                    let arg_str = if co.spec == '%' {
                        &[][..]
                    } else if let Some(n) = co.arg_number {
                        highest_numbered = highest_numbered.max(n);
                        let index = base + n - 1;
                        if index >= arguments.len() {
                            // POSIX 111998-112000: a missing operand for a
                            // numbered conversion should be diagnosed and
                            // should not exit zero.
                            eprintln!("printf: missing argument for conversion %{n}$");
                            had_error = true;
                        }
                        arguments.get(index).copied().unwrap_or(&[])
                    } else {
                        let arg = arguments.get(next_unnumbered).copied().unwrap_or(&[]);
                        next_unnumbered += 1;
                        arg
                    };

                    let result = format_arg(co, arg_str)?;
                    output.extend_from_slice(&result.bytes);

                    if result.had_error {
                        had_error = true;
                    }

                    if result.stop_output {
                        stop_output = true;
                        break;
                    }
                }
                Token::IgnoreRestOfFormat => {
                    stop_output = true;
                    break;
                }
                Token::Literal(vec) => {
                    output.extend_from_slice(vec.as_slice());
                }
            }
        }

        // Every argument-consuming conversion advances this past `base`, so a
        // format containing one always makes progress.
        let consumed = next_unnumbered.max(base + highest_numbered);

        // If we've consumed all arguments or there are no conversion specs, stop
        if stop_output || consumed >= arguments.len() || !had_conversion {
            break;
        }

        base = consumed;
    }

    io::stdout().write_all(output.as_slice())?;

    Ok(had_error)
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    // TODO
    // unwrap
    textdomain("posixutils-rs").unwrap();
    // TODO
    // unwrap
    bind_textdomain_codeset("posixutils-rs", "UTF-8").unwrap();

    let args = std::env::args_os().collect::<Vec<_>>();

    match args.as_slice() {
        &[ref _program_file_name, ref format, ref arguments @ ..] => {
            let arguments_vec: Vec<&[u8]> = arguments.iter().map(|os| os.as_bytes()).collect();

            match do_printf(format.as_bytes(), &arguments_vec) {
                Ok(had_error) => {
                    if had_error {
                        ExitCode::FAILURE
                    } else {
                        ExitCode::SUCCESS
                    }
                }
                Err(er) => {
                    eprint!("printf: {er}");
                    ExitCode::FAILURE
                }
            }
        }
        _ => {
            eprintln!("printf: {}", gettext("not enough arguments"));
            ExitCode::FAILURE
        }
    }
}
