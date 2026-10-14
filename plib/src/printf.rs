//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The format engine of POSIX XCU `printf`.
//!
//! Shared by the `printf` utility and by the shell's built-in version, so the
//! two cannot drift apart.  The engine writes to any [`Write`] sink and hands
//! each diagnostic to the caller, which decides where it goes and how it is
//! prefixed; neither the process's standard output nor its standard error is
//! touched here.

use gettextrs::gettext;
use std::{error::Error, io::Write, iter::Peekable, slice::Iter, sync::OnceLock};

/// Receives each diagnostic, without a utility-name prefix or newline.
pub type DiagSink<'a> = dyn FnMut(&str) + 'a;

/// The locale's radix character, resolved once.  POSIX 111924-111926 makes
/// `LC_NUMERIC` determine the character used by the e, E, f, g, and G
/// conversions; Rust's own formatting is locale-independent and always emits
/// a <period>, so the result is substituted after formatting.
fn radix_char() -> &'static str {
    static RADIX: OnceLock<String> = OnceLock::new();
    RADIX.get_or_init(crate::locale::radix_char)
}

/// Replace the <period> Rust's formatting produced with the locale's radix
/// character.  Every floating-point rendering here contains at most one.
fn apply_radix_char(formatted: String) -> String {
    let radix = radix_char();
    if radix == "." {
        formatted
    } else {
        formatted.replacen('.', radix, 1)
    }
}

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

/// Parse the `\ddd` escape of POSIX 111944-111946: one, two, or three octal
/// digits naming "a byte with the numeric value specified by the octal
/// number".
///
/// Three digits can name a value above 377, which does not fit the byte the
/// spec calls for; the low 8 bits are kept (so `\400` is `\0`, as in every
/// implementation that does not instead stop the sequence early).
fn parse_octal_sequence(first_octal_digit: u8, peekable: &mut Peekable<Iter<'_, u8>>) -> u8 {
    let mut value = u16::from(first_octal_digit - b'0');

    for _ in 0..2 {
        match peekable.peek() {
            Some(&&octal_digit @ b'0'..=b'7') => {
                value = value * 8 + u16::from(octal_digit - b'0');
                peekable.next();
            }
            _ => break,
        }
    }

    (value & 0xff) as u8
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
            return Ok(ParsedBackslashSequence::Byte(parse_octal_sequence(
                byte, peekable,
            )));
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

/// The largest field width or precision: C's `printf()` takes them as an
/// `int`.  A larger one is rejected, as dash rejects it, rather than wrapped
/// or saturated.
const FIELD_NUMBER_MAX: usize = i32::MAX as usize;

fn field_number_too_large(text: &str) -> Box<dyn Error> {
    Box::from(format!(
        "{text}: {}",
        gettext("field width or precision too large")
    ))
}

/// Parse the decimal digits of a field width or precision.
fn parse_field_number(digits: &str) -> Result<usize, Box<dyn Error>> {
    digits
        .parse::<usize>()
        .ok()
        .filter(|&n| n <= FIELD_NUMBER_MAX)
        .ok_or_else(|| field_number_too_large(digits))
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
                            conv_spec.width = Some(parse_field_number(&width)?);

                            width.clear();
                        }

                        state = ParseState::Precision;
                    }
                }
                ParseState::Precision => {
                    if current_byte == b'.' {
                        // XBD 3562-3563: "The precision shall take the form of
                        // a <period> ('.') followed by a decimal digit string;
                        // a null digit string is treated as zero."  Commit to
                        // zero now; any digits that follow overwrite it.
                        conv_spec.precision = Some(0);
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
                            conv_spec.precision = Some(parse_field_number(&precision)?);

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

/// One piece of a conversion's output.  A field width or precision can ask
/// for up to `FIELD_NUMBER_MAX` padding bytes, so padding is described, not
/// built: [`write_pieces`] streams it in bounded chunks.
enum Piece {
    Bytes(Vec<u8>),
    /// `count` copies of a byte.
    Fill(u8, usize),
}

/// The text of a converted value: `head`, then `zeros` '0' bytes, then
/// `tail`.  The zeros are a precision's padding, inside any field padding.
struct Body {
    head: Vec<u8>,
    zeros: usize,
    tail: Vec<u8>,
}

impl Body {
    fn len(&self) -> usize {
        self.head.len() + self.zeros + self.tail.len()
    }

    fn push_into(self, pieces: &mut Vec<Piece>) {
        pieces.push(Piece::Bytes(self.head));
        pieces.push(Piece::Fill(b'0', self.zeros));
        pieces.push(Piece::Bytes(self.tail));
    }
}

/// How a conversion fills its field when the value is narrower.
#[derive(Clone, Copy, PartialEq)]
enum Justify {
    /// Spaces after the value (the '-' flag).
    Left,
    /// Zeros between the sign or prefix and the value (the '0' flag).
    ZeroPad,
    /// Spaces before the value.
    Right,
}

/// Lay out `lead` (a sign or radix prefix) and `body` in a field of
/// `width` bytes.
fn justify(lead: Vec<u8>, body: Body, width: usize, how: Justify) -> Vec<Piece> {
    let padding = width.saturating_sub(lead.len() + body.len());
    let mut pieces = Vec::with_capacity(6);
    match how {
        Justify::Left => {
            pieces.push(Piece::Bytes(lead));
            body.push_into(&mut pieces);
            pieces.push(Piece::Fill(b' ', padding));
        }
        Justify::ZeroPad => {
            pieces.push(Piece::Bytes(lead));
            pieces.push(Piece::Fill(b'0', padding));
            body.push_into(&mut pieces);
        }
        Justify::Right => {
            pieces.push(Piece::Fill(b' ', padding));
            pieces.push(Piece::Bytes(lead));
            body.push_into(&mut pieces);
        }
    }
    pieces
}

/// Write `pieces`, streaming each fill in chunks of at most `FILL_CHUNK`
/// bytes so that no field width or precision is ever held in memory.
fn write_pieces(output: &mut dyn Write, pieces: &[Piece]) -> std::io::Result<()> {
    const FILL_CHUNK: usize = 8 * 1024;
    for piece in pieces {
        match piece {
            Piece::Bytes(bytes) => output.write_all(bytes)?,
            Piece::Fill(byte, count) => {
                let chunk = [*byte; FILL_CHUNK];
                let mut left = *count;
                while left > 0 {
                    let n = left.min(FILL_CHUNK);
                    output.write_all(&chunk[..n])?;
                    left -= n;
                }
            }
        }
    }
    Ok(())
}

/// The field layout the flags ask for.  `zero_allowed` is false where the
/// '0' flag does not apply (a precision on an integer, an infinity or NaN).
fn justify_for(conv: &ConvSpec, zero_allowed: bool) -> Justify {
    if conv.left_justify {
        Justify::Left
    } else if conv.zero_pad && zero_allowed {
        Justify::ZeroPad
    } else {
        Justify::Right
    }
}

/// Format a signed integer (the `d` and `i` conversions) with all POSIX flags
/// applied.  The `+` and <space> flags are meaningful only here: XBD 3569-3570
/// define them in terms of a *signed* conversion.
fn format_integer_signed(conv: &ConvSpec, value: i64) -> Vec<Piece> {
    let sign_char = if value < 0 {
        Some(b'-')
    } else if conv.sign {
        Some(b'+')
    } else if conv.space {
        Some(b' ')
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
    sign_char: Option<u8>,
    base: u32,
    upper: bool,
) -> Vec<Piece> {
    // Convert to string in the appropriate base.  XBD 3615-3616: "The result
    // of converting a zero value with a precision of 0 shall be no
    // characters."
    let mut digits: String = if magnitude == 0 && conv.precision == Some(0) {
        String::new()
    } else {
        match base {
            8 => format!("{magnitude:o}"),
            16 if upper => format!("{magnitude:X}"),
            16 => format!("{magnitude:x}"),
            _ => magnitude.to_string(),
        }
    };

    // The precision is the minimum number of digits, reached with leading
    // zeros.
    let zeros = conv.precision.unwrap_or(0).saturating_sub(digits.len());

    // XBD 3574-3575: for the o conversion, '#' "shall increase the precision
    // to force the first digit of the result to be a zero" — it is not a
    // prefix, so it must not add a zero to a result that already has one.
    if base == 8 && conv.alt_form && zeros == 0 && !digits.starts_with('0') {
        digits.insert(0, '0');
    }

    // Alternate form prefix (# on x and X only; see above for o)
    let prefix: &[u8] = if conv.alt_form && magnitude != 0 {
        match base {
            16 if upper => b"0X",
            16 => b"0x",
            _ => b"",
        }
    } else {
        b""
    };

    let mut lead = Vec::with_capacity(3);
    lead.extend(sign_char);
    lead.extend_from_slice(prefix);

    // With a precision, the '0' flag is ignored (XBD 3580-3582).
    let how = justify_for(conv, conv.precision.is_none());
    let body = Body {
        head: Vec::new(),
        zeros,
        tail: digits.into_bytes(),
    };
    justify(lead, body, conv.width.unwrap_or(0), how)
}

/// Parse a floating point argument according to POSIX rules
fn parse_float_arg(arg: &[u8]) -> Result<(f64, bool), String> {
    if arg.is_empty() {
        return Ok((0.0, true));
    }

    let arg_str = match std::str::from_utf8(arg) {
        Ok(s) => s,
        Err(_) => return Err(gettext("invalid UTF-8")),
    };

    // strtod() skips leading blanks but stops at trailing ones, which are
    // then "not completely converted" (POSIX 112020-112024).
    let arg_str = arg_str.trim_start();

    if arg_str.is_empty() {
        return Err(gettext("expected numeric value"));
    }

    // Check for character constant: 'c or "c.  POSIX 112013-112018 attaches
    // this extension to the integer conversions; accepting it here too is a
    // superset, and matches every other implementation.
    if (arg_str.starts_with('\'') || arg_str.starts_with('"')) && arg_str.len() >= 2 {
        let ch = arg_str.chars().nth(1).unwrap();
        return Ok((ch as u32 as f64, arg_str.chars().count() == 2));
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
                return Err(gettext("expected numeric value"));
            }

            let parsed: &str = &arg_str[..end];
            match parsed.parse::<f64>() {
                Ok(n) => Ok((n, end == arg_str.len())),
                Err(_) => Err(gettext("expected numeric value")),
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

/// Render an infinity or a NaN in the style XBD 3627-3632 prescribes: the
/// lowercase conversions produce "inf"/"nan", the uppercase ones "INF"/"NAN".
fn format_nonfinite(value: f64, upper: bool) -> Option<String> {
    if !value.is_finite() {
        let text = match (value.is_nan(), upper) {
            (true, false) => "nan",
            (true, true) => "NAN",
            (false, false) => "inf",
            (false, true) => "INF",
        };
        return Some(text.to_string());
    }
    None
}

/// Format a floating point number in decimal notation (%f/%F)
fn format_fixed(value: f64, precision: usize, upper: bool) -> String {
    if let Some(text) = format_nonfinite(value, upper) {
        return text;
    }

    format!("{:.prec$}", value, prec = precision)
}

/// Hexadecimal digits in the fraction of an `f64`.
const HEX_MANTISSA_DIGITS: usize = 13;

/// Format a floating point number in hexadecimal format (%a/%A).
///
/// `precision` is `None` when the format omitted it, which per XBD 3594-3598
/// means "sufficient for an exact representation of the value" — for a binary
/// FLT_RADIX that is the mantissa with its trailing zeros removed.
fn format_hex_float(value: f64, precision: Option<usize>, upper: bool) -> String {
    if let Some(text) = format_nonfinite(value, upper) {
        return text;
    }

    let bits = value.to_bits();
    let exp_bits = ((bits >> 52) & 0x7FF) as i32;
    let mut mantissa = bits & 0xF_FFFF_FFFF_FFFF;

    let (exponent, mut leading_digit) = if value == 0.0 {
        (0, 0_u64)
    } else if exp_bits == 0 {
        // Denormalized
        (-1022, 0_u64)
    } else {
        (exp_bits - 1023, 1_u64)
    };

    // An explicit precision shorter than the mantissa rounds (half-to-even),
    // rather than truncating, so the printed value is the nearest one
    // representable in the requested number of hexadecimal digits.
    if let Some(prec) = precision.filter(|&p| p < HEX_MANTISSA_DIGITS) {
        let shift = 4 * (HEX_MANTISSA_DIGITS - prec) as u32;
        let kept = mantissa >> shift;
        let rest = mantissa & ((1_u64 << shift) - 1);
        let half = 1_u64 << (shift - 1);

        let rounded = if rest > half || (rest == half && kept & 1 == 1) {
            kept + 1
        } else {
            kept
        };

        if rounded >> (4 * prec as u32) != 0 {
            // Carry out of the fraction bumps the digit before the point.
            leading_digit += 1;
            mantissa = 0;
        } else {
            mantissa = rounded << shift;
        }
    }

    // XBD 3600-3601: "The letters "abcdef" shall be used for a conversion and
    // the letters "ABCDEF" for A conversion."
    let mantissa_hex = if upper {
        format!("{mantissa:013X}")
    } else {
        format!("{mantissa:013x}")
    };
    let frac_part = match precision {
        // XBD 3594-3598: with the precision missing and a binary FLT_RADIX,
        // it "shall be sufficient for an exact representation of the value",
        // and trailing zeros may be omitted.
        None => {
            let trimmed = mantissa_hex.trim_end_matches('0');
            if trimmed.is_empty() {
                String::new()
            } else {
                format!(".{trimmed}")
            }
        }
        // XBD 3599: with a precision of zero and no '#' flag, no
        // decimal-point character appears.
        Some(0) => String::new(),
        Some(prec) => {
            let mut digits = mantissa_hex
                .chars()
                .take(prec.min(HEX_MANTISSA_DIGITS))
                .collect::<String>();
            while digits.len() < prec {
                digits.push('0');
            }
            format!(".{digits}")
        }
    };

    let exp_sign = if exponent >= 0 { '+' } else { '-' };
    let exp_abs = exponent.abs();

    if upper {
        format!("0X{}{frac_part}P{exp_sign}{exp_abs}", leading_digit)
    } else {
        format!("0x{}{frac_part}p{exp_sign}{exp_abs}", leading_digit)
    }
}

/// The most fraction digits a decimal rendering of an `f64` needs to be
/// exact (the smallest subnormal has 1074) and, with room to spare, the most
/// significant digits one can have (767).  A larger precision only appends
/// zeros, which are streamed rather than formatted: Rust's formatter would
/// otherwise build them in memory, and it panics on a precision above
/// `u16::MAX`.
const EXACT_DIGITS: usize = 1100;

/// Render a magnitude under a floating-point conversion, with the
/// precision's excess zeros split out at the end of the digits (before any
/// exponent).
fn float_body(conv: &ConvSpec, abs_value: f64) -> Body {
    // Default precision is 6
    let precision = conv.precision.unwrap_or(6);
    let capped = precision.min(EXACT_DIGITS);
    let mut excess = precision - capped;

    let upper = conv.spec.is_ascii_uppercase();
    let formatted = match conv.spec {
        'e' | 'E' => format_scientific(abs_value, capped, upper),
        'g' | 'G' => {
            // Without '#', %g removes trailing zeros, the excess included.
            if !conv.alt_form {
                excess = 0;
            }
            format_general(abs_value, capped, upper, conv.alt_form)
        }
        'a' | 'A' => {
            excess = conv
                .precision
                .map_or(0, |p| p.saturating_sub(HEX_MANTISSA_DIGITS));
            format_hex_float(
                abs_value,
                conv.precision.map(|p| p.min(HEX_MANTISSA_DIGITS)),
                upper,
            )
        }
        _ => format_fixed(abs_value, capped, upper),
    };
    if !abs_value.is_finite() {
        excess = 0;
    }

    // POSIX 111924-111926 / XBD 3623-3624: LC_NUMERIC determines the radix
    // character.  Rust's formatting always produces a <period>.
    let formatted = apply_radix_char(formatted).into_bytes();

    // The excess zeros end the digits: they precede an exponent.  (A
    // hexadecimal mantissa contains 'e', so only 'p' marks its exponent.)
    let exponent_at = match conv.spec {
        'a' | 'A' => formatted.iter().rposition(|&b| b == b'p' || b == b'P'),
        _ => formatted.iter().position(|&b| b == b'e' || b == b'E'),
    };
    let mut head = formatted;
    let tail = match exponent_at {
        Some(at) => head.split_off(at),
        None => Vec::new(),
    };
    Body {
        head,
        zeros: excess,
        tail,
    }
}

/// Format a floating point number with all POSIX flags applied
fn format_float(conv: &ConvSpec, value: f64) -> Vec<Piece> {
    let is_negative = value.is_sign_negative() && !value.is_nan();
    let abs_value = value.abs();

    let body = float_body(conv, abs_value);

    // Determine sign
    let sign_char = if is_negative {
        Some(b'-')
    } else if conv.sign {
        Some(b'+')
    } else if conv.space {
        Some(b' ')
    } else {
        None
    };

    // XBD 3580-3582: the '0' flag pads with leading zeros "except when
    // converting an infinity or NaN".
    let how = justify_for(conv, abs_value.is_finite());
    justify(
        sign_char.into_iter().collect(),
        body,
        conv.width.unwrap_or(0),
        how,
    )
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

fn format_arg_float(
    conv: &ConvSpec,
    arg: &[u8],
    diag: &mut DiagSink,
) -> Result<(Vec<Piece>, bool), Box<dyn Error>> {
    let mut had_error = false;
    let value = match parse_float_arg(arg) {
        Ok((n, fully_consumed)) => {
            if !fully_consumed {
                let arg_str = String::from_utf8_lossy(arg);
                diag(&format!(
                    "{arg_str}: {}",
                    gettext("not completely converted")
                ));
                had_error = true;
            }
            n
        }
        Err(e) => {
            let arg_str = String::from_utf8_lossy(arg);
            diag(&format!("{arg_str}: {e}"));
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
    };

    if arg.is_empty() {
        return Ok(operand(0));
    }

    let arg_str = match std::str::from_utf8(arg) {
        Ok(s) => s,
        Err(_) => return Err(gettext("invalid UTF-8")),
    };

    // A C integer constant may be preceded by blanks (strtol() skips them),
    // but trailing bytes are "not completely converted" (112020-112024).
    let arg_str = arg_str.trim_start();

    // Blanks alone are not a constant.  (An operand that is empty to begin
    // with is the "missing operand" substitution of item 11, handled above.)
    if arg_str.is_empty() {
        return Err(gettext("expected numeric value"));
    }

    // Check for character constant: 'c or "c
    if (arg_str.starts_with('\'') || arg_str.starts_with('"')) && arg_str.len() >= 2 {
        let ch = arg_str.chars().nth(1).unwrap();
        return Ok(IntOperand {
            value: i128::from(u32::from(ch)),
            // POSIX EXAMPLES 112128-112141: bytes beyond the quoted character
            // are not used by the conversion, so a diagnostic is generated
            // and the exit status is non-zero.
            fully_consumed: arg_str.chars().count() == 2,
        });
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
        return Err(gettext("expected numeric value"));
    }

    // Only digits of the operand's radix reach the parser, so the sole
    // failure mode is a magnitude too large for i128.  Saturate; the
    // conversion's own range check below reports it.
    let magnitude = i128::from_str_radix(&digits[..valid_len], radix).unwrap_or(i128::MAX);

    Ok(IntOperand {
        value: if is_negative { -magnitude } else { magnitude },
        fully_consumed: valid_len == digits.len(),
    })
}

/// Evaluate an integer operand, emitting the diagnostics POSIX 112020-112024
/// requires.  Returns the value and whether an error was reported; on error
/// the caller still writes the accumulated value, as the spec directs.
fn eval_integer_arg(arg: &[u8], diag: &mut DiagSink) -> (IntOperand, bool) {
    match parse_integer_arg(arg) {
        Ok(operand) => {
            let mut had_error = false;
            if !operand.fully_consumed {
                let arg_str = String::from_utf8_lossy(arg);
                diag(&format!(
                    "{arg_str}: {}",
                    gettext("not completely converted")
                ));
                had_error = true;
            }
            (operand, had_error)
        }
        Err(e) => {
            let arg_str = String::from_utf8_lossy(arg);
            diag(&format!("{arg_str}: {e}"));
            (
                IntOperand {
                    value: 0,
                    fully_consumed: true,
                },
                true,
            )
        }
    }
}

/// Report an operand whose value did not fit the conversion.  POSIX
/// 112020-112024 requires the diagnostic alongside the saturated value; the
/// wording follows the spec's own EXAMPLES table (112117-112120).
fn report_overflow(arg: &[u8], diag: &mut DiagSink) {
    let arg_str = String::from_utf8_lossy(arg);
    diag(&format!("{arg_str}: {}", gettext("arithmetic overflow")));
}

/// Format an argument under one of the unsigned conversions (`o`, `u`, `x`,
/// `X`).  A negative operand is reinterpreted as its two's-complement bit
/// pattern, matching `strtoul()` and XBD 3607-3608's "unsigned octal /
/// unsigned decimal / unsigned hexadecimal notation".
fn format_arg_uint_base(
    conv: &ConvSpec,
    arg: &[u8],
    base: u32,
    upper: bool,
    diag: &mut DiagSink,
) -> (Vec<Piece>, bool) {
    let (operand, mut had_error) = eval_integer_arg(arg, diag);

    let magnitude = if operand.value < 0 {
        // Saturate into the signed range first, then reinterpret the bits.
        let signed = if operand.value < i128::from(i64::MIN) {
            had_error = true;
            report_overflow(arg, diag);
            i64::MIN
        } else {
            operand.value as i64
        };
        signed as u64
    } else if operand.value > i128::from(u64::MAX) {
        had_error = true;
        report_overflow(arg, diag);
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
fn format_arg_int(conv: &ConvSpec, arg: &[u8], diag: &mut DiagSink) -> (Vec<Piece>, bool) {
    let (operand, mut had_error) = eval_integer_arg(arg, diag);

    let value = if operand.value < i128::from(i64::MIN) {
        had_error = true;
        report_overflow(arg, diag);
        i64::MIN
    } else if operand.value > i128::from(i64::MAX) {
        had_error = true;
        report_overflow(arg, diag);
        i64::MAX
    } else {
        operand.value as i64
    };

    (format_integer_signed(conv, value), had_error)
}

fn format_arg_char(conv: &ConvSpec, arg: &[u8]) -> Vec<Piece> {
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

fn format_arg_b(conv: &ConvSpec, arg: &[u8]) -> (Vec<Piece>, bool) {
    match process_b_escapes(arg) {
        ProcessBResult::Bytes(bytes) => (format_arg_string(conv, &bytes), false),
        ProcessBResult::StopOutput(bytes) => (format_arg_string(conv, &bytes), true),
    }
}

fn format_arg_string(conv: &ConvSpec, arg: &[u8]) -> Vec<Piece> {
    // Apply precision to limit string length
    let arg = match conv.precision {
        Some(prec) if prec < arg.len() => &arg[..prec],
        _ => arg,
    };

    let padding = conv.width.unwrap_or(0).saturating_sub(arg.len());

    // For strings, zero padding is treated as space padding
    let padding_char = if conv.zero_pad && conv.spec != 's' && conv.spec != 'b' {
        b'0'
    } else {
        b' '
    };

    let value = Piece::Bytes(arg.to_vec());
    let fill = Piece::Fill(padding_char, padding);
    if conv.left_justify {
        vec![value, fill]
    } else {
        vec![fill, value]
    }
}

/// Result of formatting an argument
struct FormatResult {
    pieces: Vec<Piece>,
    stop_output: bool,
    had_error: bool,
}

fn format_arg(
    conv: &ConvSpec,
    arg: &[u8],
    diag: &mut DiagSink,
) -> Result<FormatResult, Box<dyn Error>> {
    let (bytes, stop_output, had_error) = match conv.spec {
        'd' | 'i' => {
            let (bytes, had_error) = format_arg_int(conv, arg, diag);
            (bytes, false, had_error)
        }
        'u' | 'o' | 'x' | 'X' => {
            let (base, upper) = match conv.spec {
                'o' => (8, false),
                'x' => (16, false),
                'X' => (16, true),
                _ => (10, false),
            };
            let (bytes, had_error) = format_arg_uint_base(conv, arg, base, upper, diag);
            (bytes, false, had_error)
        }
        'f' | 'F' | 'e' | 'E' | 'g' | 'G' | 'a' | 'A' => {
            let (bytes, had_error) = format_arg_float(conv, arg, diag)?;
            (bytes, false, had_error)
        }
        'c' => (format_arg_char(conv, arg), false, false),
        's' => (format_arg_string(conv, arg), false, false),
        'b' => {
            let (bytes, stop) = format_arg_b(conv, arg);
            (bytes, stop, false)
        }
        '%' => (vec![Piece::Bytes(vec![b'%'])], false, false),
        ch => {
            diag(&format!(
                "{}: {ch}",
                gettext("unknown conversion specifier")
            ));
            (format_arg_string(conv, arg), false, true)
        }
    };

    Ok(FormatResult {
        pieces: bytes,
        stop_output,
        had_error,
    })
}

/// Split the utility's arguments into the format and the operands after it,
/// or `None` if there is no format.
///
/// XCU 1.4: a utility that accepts no options but does accept operands
/// recognizes "--" as a first argument to be discarded, so `printf -- '-x'`
/// prints `-x`.  Only an exact first "--" is discarded.
pub fn split_operands<A: AsRef<[u8]>>(args: &[A]) -> Option<(&A, &[A])> {
    let args = match args.split_first() {
        Some((first, rest)) if first.as_ref() == b"--" => rest,
        _ => args,
    };
    args.split_first()
}

/// Write `format` applied to `arguments` (the operands after the format) to
/// `output`, reusing the format while operands remain.
///
/// Each diagnostic goes to `diag` as it arises, so a caller that writes
/// diagnostics and output to the same place keeps them in order.  Returns
/// `Ok(had_error)`, where `had_error` means a diagnostic was issued and the
/// exit status must be non-zero (the output is still written, as POSIX
/// directs).  `Err` is a malformed format or a failed write, which ends the
/// output; the caller reports its text.
pub fn printf(
    output: &mut dyn Write,
    format: &[u8],
    arguments: &[&[u8]],
    diag: &mut DiagSink,
) -> Result<bool, Box<dyn Error>> {
    let token_vec = tokenize_format_str(format)?;

    // Write as we go rather than accumulating the whole result: format reuse
    // over a long operand list would otherwise materialize every byte of
    // output in memory before any of it reached the sink.
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
                            diag(&format!(
                                "{} %{n}$",
                                gettext("missing argument for conversion")
                            ));
                            had_error = true;
                        }
                        arguments.get(index).copied().unwrap_or(&[])
                    } else {
                        let arg = arguments.get(next_unnumbered).copied().unwrap_or(&[]);
                        next_unnumbered += 1;
                        arg
                    };

                    let result = format_arg(co, arg_str, diag)?;
                    write_pieces(output, &result.pieces)?;

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
                    output.write_all(vec.as_slice())?;
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

    // POSIX 112028-112030: a failed write is an error.  A buffered sink
    // (standard output is line-buffered) still holds anything not ending in
    // a <newline>, and its write error would otherwise be lost.
    output.flush()?;

    Ok(had_error)
}

#[cfg(test)]
mod tests {
    use super::{printf, split_operands};

    #[test]
    fn leading_double_dash_is_discarded() {
        assert_eq!(split_operands(&["--", "f", "a"]), Some((&"f", &["a"][..])));
        assert_eq!(split_operands(&["--", "--"]), Some((&"--", &[][..])));
        assert_eq!(split_operands(&["f", "--"]), Some((&"f", &["--"][..])));
        assert_eq!(split_operands(&["--"]), None);
        assert_eq!(split_operands::<&str>(&[]), None);
    }

    /// Run the engine, returning (output, diagnostics, result).
    fn run(format: &str, args: &[&str]) -> (String, Vec<String>, Result<bool, String>) {
        let args: Vec<&[u8]> = args.iter().map(|a| a.as_bytes()).collect();
        let mut out = Vec::new();
        let mut diags = Vec::new();
        let mut sink = |m: &str| diags.push(m.to_string());
        let result =
            printf(&mut out, format.as_bytes(), &args, &mut sink).map_err(|e| e.to_string());
        (String::from_utf8(out).unwrap(), diags, result)
    }

    #[test]
    fn format_is_reused_for_extra_operands() {
        let (out, diags, result) = run("%s-%s\n", &["a", "b", "c"]);
        assert_eq!(out, "a-b\nc-\n");
        assert!(diags.is_empty());
        assert_eq!(result, Ok(false));
    }

    #[test]
    fn invalid_number_is_diagnosed_and_still_written() {
        let (out, diags, result) = run("%d|%d\n", &["abc", "5"]);
        assert_eq!(out, "0|5\n");
        assert_eq!(diags, ["abc: expected numeric value"]);
        assert_eq!(result, Ok(true));
    }

    #[test]
    fn malformed_format_is_an_error() {
        let (out, _, result) = run("\\x", &[]);
        assert_eq!(out, "");
        assert_eq!(
            result,
            Err("missing hexadecimal number in escape".to_string())
        );
    }

    /// A sink that keeps only the byte count, the largest single write and
    /// a short head and tail of the output.
    #[derive(Default)]
    struct CountingSink {
        total: usize,
        max_write: usize,
        head: Vec<u8>,
        tail: Vec<u8>,
    }

    impl std::io::Write for CountingSink {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.total += buf.len();
            self.max_write = self.max_write.max(buf.len());
            let room = 32_usize.saturating_sub(self.head.len());
            self.head.extend_from_slice(&buf[..room.min(buf.len())]);
            self.tail.extend_from_slice(buf);
            let excess = self.tail.len().saturating_sub(32);
            self.tail.drain(..excess);
            Ok(buf.len())
        }

        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    fn run_counting(format: &str, args: &[&str]) -> (CountingSink, Result<bool, String>) {
        let args: Vec<&[u8]> = args.iter().map(|a| a.as_bytes()).collect();
        let mut sink = CountingSink::default();
        let mut diags = |_: &str| {};
        let result =
            printf(&mut sink, format.as_bytes(), &args, &mut diags).map_err(|e| e.to_string());
        (sink, result)
    }

    /// The most a single write may carry: a huge field width or precision
    /// must be streamed, never built in memory (in the shell, an allocation
    /// failure kills the user's shell).
    const MAX_WRITE: usize = 64 * 1024;

    #[test]
    fn huge_width_and_precision_are_streamed() {
        const N: usize = 10_000_000;
        // (format, operand, total bytes, first bytes, last bytes)
        type Case = (String, &'static str, usize, &'static [u8], &'static [u8]);
        let cases: &[Case] = &[
            (format!("%{N}d"), "1", N, b"    ", b"   1"),
            (format!("%-{N}s|"), "ab", N + 1, b"ab  ", b"   |"),
            (format!("%0{N}d"), "-1", N, b"-000", b"0001"),
            (format!("%.{N}d"), "7", N, b"0000", b"0007"),
            (format!("%#.{N}o"), "8", N, b"0000", b"0010"),
            (format!("%{N}c"), "xy", N, b"    ", b"   x"),
            (format!("%{N}b"), "a\\tb", N, b"    ", b"a\tb"),
            (format!("%.{N}f"), "1.5", N + 2, b"1.50", b"0000"),
            (format!("%{N}.2f"), "-1.5", N, b"    ", b"1.50"),
            (format!("%.{N}e"), "1.5", N + 6, b"1.50", b"0e+00"),
            (format!("%#.{N}g"), "1.5", N + 1, b"1.50", b"0000"),
            (format!("%.{N}a"), "1", N + 7, b"0x1.", b"000p+0"),
        ];
        for (format, arg, total, head, tail) in cases {
            let (sink, result) = run_counting(format, &[arg]);
            assert_eq!(result, Ok(false), "{format}");
            assert_eq!(sink.total, *total, "{format}");
            assert!(sink.max_write <= MAX_WRITE, "{format}: {}", sink.max_write);
            assert!(sink.head.starts_with(head), "{format}");
            assert!(sink.tail.ends_with(tail), "{format}");
        }
    }

    /// Past the digits a double can carry exactly, a larger precision only
    /// appends zeros, before any exponent.
    #[test]
    fn large_float_precision_appends_exact_zeros() {
        let (short, _, _) = run("%.1100f", &["0.1"]);
        let (long, _, _) = run("%.1300f", &["0.1"]);
        assert_eq!(long, format!("{short}{}", "0".repeat(200)));

        let (short, _, _) = run("%.1100e", &["0.1"]);
        let (long, _, _) = run("%.1300e", &["0.1"]);
        let (mantissa, exponent) = short.split_at(short.find('e').unwrap());
        assert_eq!(long, format!("{mantissa}{}{exponent}", "0".repeat(200)));

        let (short, _, _) = run("%#.1100g", &["0.1"]);
        let (long, _, _) = run("%#.1300g", &["0.1"]);
        assert_eq!(long, format!("{short}{}", "0".repeat(200)));
        // Without '#', %g removes the trailing zeros anyway.
        assert_eq!(run("%.1300g", &["0.1"]).0, run("%.1100g", &["0.1"]).0);
        assert_eq!(run("%.70000g", &["0.5"]).0, "0.5");
    }

    /// A field width or precision is a C int: one that does not fit is
    /// rejected (dash fails the same way), never wrapped or saturated.
    #[test]
    fn width_or_precision_above_int_max_is_an_error() {
        for format in [
            "%2147483648d",
            "%.2147483648d",
            "%.2147483648f",
            "%99999999999999999999d",
            "%.99999999999999999999s",
        ] {
            let (out, _, result) = run(format, &["1"]);
            assert_eq!(out, "", "{format}");
            let err = result.unwrap_err();
            assert!(
                err.ends_with("field width or precision too large"),
                "{format}: {err}"
            );
        }
    }

    #[test]
    fn format_without_conversions_is_used_once() {
        assert_eq!(run("x", &["a", "b"]).0, "x");
        assert_eq!(run("%%", &["a", "b"]).0, "%");
    }
}
