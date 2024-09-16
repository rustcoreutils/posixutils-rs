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
use plib::PROJECT_NAME;
use std::io::{self, Write};

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

impl ConvSpec {
    fn new() -> ConvSpec {
        ConvSpec {
            spec: ' ',
            width: None,
            precision: None,
            left_justify: false,
            sign: false,
            space: false,
            alt_form: false,
            zero_pad: false,
        }
    }
}

// the following structure fully describes a token from
// a tokenized printf format string
enum Token {
    // a literal string
    Literal(String),
    // a conversion specifier
    Conversion(ConvSpec),
}

enum ParseState {
    Literal,
    Flags,
    Width,
    Precision,
    PrecisionValue,
    Specifier,
}

fn escaped_char(c: char) -> char {
    match c {
        'a' => '\x07',
        'b' => '\x08',
        'e' => '\x1b',
        'f' => '\x0c',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        'v' => '\x0b',
        _ => c,
    }
}

fn tokenize_format_str(format: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut literal = String::with_capacity(format.len());
    let mut conversion = ConvSpec::new();
    let mut width = String::with_capacity(8);
    let mut precision = String::with_capacity(8);
    let mut state = ParseState::Literal;
    let mut escape = false;

    for c in format.chars() {
        let mut done_with_char = false;
        while !done_with_char {
            match state {
                ParseState::Literal => {
                    if c == '%' {
                        if !literal.is_empty() {
                            tokens.push(Token::Literal(literal.clone()));
                            literal.clear();
                        }

                        state = ParseState::Flags;
                    } else if c == '\\' {
                        escape = true;
                    } else if escape {
                        escape = false;
                        literal.push(escaped_char(c));
                    } else {
                        literal.push(c);
                    }
                    done_with_char = true;
                }

                ParseState::Flags => {
                    done_with_char = true;
                    match c {
                        '-' => conversion.left_justify = true,
                        '+' => conversion.sign = true,
                        ' ' => conversion.space = true,
                        '#' => conversion.alt_form = true,
                        '0' => conversion.zero_pad = true,
                        _ => {
                            state = ParseState::Width;
                            done_with_char = false;
                        }
                    }
                }

                ParseState::Width => {
                    if c.is_digit(10) {
                        width.push(c);
                        done_with_char = true;
                    } else {
                        if !width.is_empty() {
                            conversion.width = Some(width.parse().unwrap());
                            width.clear();
                        }
                        state = ParseState::Precision;
                    }
                }

                ParseState::Precision => {
                    if c == '.' {
                        state = ParseState::PrecisionValue;
                        done_with_char = true;
                    } else {
                        state = ParseState::Specifier;
                    }
                }

                ParseState::PrecisionValue => {
                    if c.is_digit(10) {
                        precision.push(c);
                        done_with_char = true;
                    } else {
                        if !precision.is_empty() {
                            conversion.precision = Some(precision.parse().unwrap());
                            precision.clear();
                        }

                        state = ParseState::Specifier;
                    }
                }

                ParseState::Specifier => {
                    conversion.spec = c;
                    tokens.push(Token::Conversion(conversion));
                    conversion = ConvSpec::new();
                    state = ParseState::Literal;
                    done_with_char = true;
                }
            }
        }
    }

    if !literal.is_empty() {
        tokens.push(Token::Literal(literal.clone()));
    }

    tokens
}

fn format_arg_uint(conv: &ConvSpec, arg: usize) -> String {
    format_arg_string(conv, arg.to_string().as_str())
}

fn format_arg_octal(conv: &ConvSpec, arg: usize) -> String {
    format_arg_string(conv, format!("{:o}", arg).as_str())
}

fn format_arg_hex(conv: &ConvSpec, arg: usize, upper: bool) -> String {
    let s = if upper {
        format!("{:X}", arg)
    } else {
        format!("{:x}", arg)
    };
    format_arg_string(conv, s.as_str())
}

fn format_arg_uint_base(conv: &ConvSpec, arg: &str) -> String {
    let arg: usize = {
        if arg.is_empty() {
            0
        } else {
            match arg.parse() {
                Ok(n) => n,
                Err(_) => {
                    eprintln!("invalid unsigned integer: {}", arg);
                    0
                }
            }
        }
    };

    match conv.spec {
        'u' => format_arg_uint(conv, arg),
        'o' => format_arg_octal(conv, arg),
        'x' => format_arg_hex(conv, arg, false),
        'X' => format_arg_hex(conv, arg, true),
        _ => {
            panic!("BUG: invalid conversion specifier: {}", conv.spec);
        }
    }
}

fn format_arg_int(conv: &ConvSpec, arg: &str) -> String {
    let arg: isize = {
        if arg.is_empty() {
            0
        } else {
            match arg.parse() {
                Ok(n) => n,
                Err(_) => {
                    eprintln!("invalid integer: {}", arg);
                    0
                }
            }
        }
    };
    format_arg_string(conv, arg.to_string().as_str())
}

fn format_arg_char(conv: &ConvSpec, arg: &str) -> String {
    let arg = if arg.is_empty() { arg } else { &arg[0..1] };
    format_arg_string(conv, arg)
}

fn format_arg_string(conv: &ConvSpec, arg: &str) -> String {
    let mut output = String::with_capacity(conv.width.unwrap_or(arg.len()));

    if conv.width.is_some() {
        let padchar = match conv.zero_pad {
            true => '0',
            false => ' ',
        };

        let width = conv.width.unwrap();
        if conv.left_justify {
            output.push_str(arg);
            if width > arg.len() {
                for _ in 0..width - arg.len() {
                    output.push(padchar);
                }
            }
        } else {
            if width > arg.len() {
                for _ in 0..width - arg.len() {
                    output.push(padchar);
                }
            }
            output.push_str(arg);
        }
    } else {
        output.push_str(arg);
    }

    output
}

fn format_arg(conv: &ConvSpec, arg: &str) -> String {
    match conv.spec {
        'd' | 'i' => format_arg_int(conv, arg),
        'u' | 'o' | 'x' | 'X' => format_arg_uint_base(conv, arg),
        'c' => format_arg_char(conv, arg),
        's' => format_arg_string(conv, arg),

        _ => {
            eprintln!("unknown conversion specifier: {}", conv.spec);
            format_arg_string(conv, arg)
        }
    }
}

fn do_printf(format: &str, args: &[String]) -> io::Result<()> {
    let mut arg_pos = 0;
    let mut output = String::with_capacity(format.len() * 2);
    let blank = String::new();

    let tokenlist = tokenize_format_str(format);
    for token in tokenlist {
        match token {
            Token::Literal(s) => {
                output.push_str(&s);
            }

            Token::Conversion(c) => {
                let arg_str = {
                    if arg_pos >= args.len() {
                        &blank
                    } else {
                        &args[arg_pos]
                    }
                };
                arg_pos += 1;

                output.push_str(format_arg(&c, arg_str).as_str());
            }
        }
    }

    io::stdout().write_all(output.as_bytes())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        return Err(gettext("printf: not enough arguments").into());
    }

    do_printf(&args[1], &args[2..])?;

    Ok(())
}
