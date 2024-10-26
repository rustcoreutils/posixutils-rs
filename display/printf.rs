//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - floating point support (a, A, e, E, f, F, g, and G conversion specifiers)
// - fix bug:  zero padding does not work for negative numbers
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

fn format_arg_uint(conv: &ConvSpec, arg: usize) -> Vec<u8> {
    format_arg_string(conv, arg.to_string().as_bytes())
}

fn format_arg_octal(conv: &ConvSpec, arg: usize) -> Vec<u8> {
    format_arg_string(conv, format!("{arg:o}").as_bytes())
}

fn format_arg_hex(conv: &ConvSpec, arg: usize, upper: bool) -> Vec<u8> {
    let st = if upper {
        format!("{arg:X}")
    } else {
        format!("{arg:x}")
    };

    format_arg_string(conv, st.as_bytes())
}

fn format_arg_uint_base(conv: &ConvSpec, arg: &[u8]) -> Result<Vec<u8>, Box<dyn Error>> {
    let arg = {
        if arg.is_empty() {
            0_usize
        } else {
            let arg_str = std::str::from_utf8(arg)?;

            match arg_str.parse::<usize>() {
                Ok(n) => n,
                Err(_) => {
                    eprintln!("printf: invalid unsigned integer: {arg_str}");

                    0_usize
                }
            }
        }
    };

    let formatted = match conv.spec {
        'u' => format_arg_uint(conv, arg),
        'o' => format_arg_octal(conv, arg),
        'x' => format_arg_hex(conv, arg, false),
        'X' => format_arg_hex(conv, arg, true),
        ch => {
            panic!("printf: BUG: invalid conversion specifier: {ch}");
        }
    };

    Ok(formatted)
}

fn format_arg_int(conv: &ConvSpec, arg: &[u8]) -> Result<Vec<u8>, Box<dyn Error>> {
    let arg = {
        if arg.is_empty() {
            0_isize
        } else {
            let arg_str = std::str::from_utf8(arg)?;

            match arg_str.parse::<isize>() {
                Ok(n) => n,
                Err(_) => {
                    eprintln!("printf: invalid integer: {arg_str}");

                    0_isize
                }
            }
        }
    };

    Ok(format_arg_string(conv, arg.to_string().as_bytes()))
}

fn format_arg_char(conv: &ConvSpec, arg: &[u8]) -> Vec<u8> {
    let arg_to_use = arg.get(..1_usize).unwrap_or(&[]);

    format_arg_string(conv, arg_to_use)
}

fn format_arg_string(conv: &ConvSpec, arg: &[u8]) -> Vec<u8> {
    let Some(width) = conv.width else {
        return arg.to_vec();
    };

    let arg_len = arg.len();

    let padding_char = match conv.zero_pad {
        true => '0',
        false => ' ',
    };

    let mut encoding_buffer = [0_u8; 4_usize];

    let padding_char_encoded_slice = padding_char.encode_utf8(&mut encoding_buffer).as_bytes();

    let mut output = Vec::<u8>::with_capacity(width);

    if width > arg_len {
        let conv_left_justify = conv.left_justify;

        if conv_left_justify {
            output.extend_from_slice(arg);
        }

        for _ in 0_usize..(width - arg_len) {
            output.extend_from_slice(padding_char_encoded_slice);
        }

        if !conv_left_justify {
            output.extend_from_slice(arg);
        }
    }

    output
}

fn format_arg(conv: &ConvSpec, arg: &[u8]) -> Result<Vec<u8>, Box<dyn Error>> {
    let vec = match conv.spec {
        'd' | 'i' => format_arg_int(conv, arg)?,
        'u' | 'o' | 'x' | 'X' => format_arg_uint_base(conv, arg)?,
        'c' => format_arg_char(conv, arg),
        's' => format_arg_string(conv, arg),
        ch => {
            eprintln!("printf: unknown conversion specifier: {ch}");

            format_arg_string(conv, arg)
        }
    };

    Ok(vec)
}

fn do_printf<'a>(
    format: &[u8],
    mut arguments: impl Iterator<Item = &'a [u8]>,
) -> Result<(), Box<dyn Error>> {
    let token_vec = tokenize_format_str(format)?;

    let mut output = Vec::<u8>::with_capacity(format.len() * 2_usize);

    for token in token_vec {
        match token {
            Token::Conversion(co) => {
                let arg_str = arguments.next().unwrap_or(&[]);

                let format_arg_result = format_arg(&co, arg_str)?;

                output.extend_from_slice(format_arg_result.as_slice());
            }
            Token::IgnoreRestOfFormat => {
                break;
            }
            Token::Literal(vec) => {
                output.extend_from_slice(vec.as_slice());
            }
        }
    }

    io::stdout().write_all(output.as_slice())?;

    Ok(())
}

fn start() -> Result<(), Box<dyn Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME"))?;
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8")?;

    let args = std::env::args_os().collect::<Vec<_>>();

    match &args.as_slice() {
        &&[ref _program_file_name, ref format, ref arguments @ ..] => {
            let arguments_iterator = arguments.iter().map(|os| os.as_bytes());

            do_printf(format.as_bytes(), arguments_iterator)?;
        }
        _ => {
            return Err(Box::from(gettext("not enough arguments")));
        }
    }

    Ok(())
}

fn main() -> ExitCode {
    match start() {
        Ok(()) => ExitCode::SUCCESS,
        Err(bo) => {
            eprint!("printf: {bo}");

            ExitCode::FAILURE
        }
    }
}
