//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::Write;

use gettextrs::gettext;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    combinator::fail,
    error::FromExternalError,
    sequence::{delimited, tuple},
    IResult,
};

use crate::{
    lexer::is_whitespace,
    precedence::{self, binary_op, unary_op, Assoc, Operation},
    state::{StackFrame, State},
    Result,
};

use super::MacroImplementation;

pub struct EvalMacro;

impl MacroImplementation for EvalMacro {
    fn evaluate(
        &self,
        mut state: State,
        stderr: &mut dyn Write,
        frame: StackFrame,
    ) -> Result<State> {
        let mut args = frame.args.into_iter();
        let first_arg = args
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;
        let radix_arg = args.next();
        let width_arg = args.next();

        let output = match nom::combinator::all_consuming(nom::combinator::complete(
            parse_and_evaluate,
        ))(&first_arg)
        {
            Ok((_, output)) => output,
            // GNU m4 treats a malformed expression (syntax error, divide by
            // zero, etc.) as a recoverable error: emit a diagnostic, expand to
            // the empty string, set a non-zero exit status, and keep going.
            Err(_) => {
                let msg = gettext("bad expression in eval: {}").replacen(
                    "{}",
                    &String::from_utf8_lossy(&first_arg),
                    1,
                );
                state.emit_error(stderr, format_args!("{msg}"))?;
                return Ok(state);
            }
        };

        // Second argument: output radix (default 10); third: minimum digit count
        // (default 1). Both are decimal; an omitted/blank value takes the default.
        let radix = match parse_optional_decimal(radix_arg.as_deref()) {
            Ok(Some(r)) if (2..=36).contains(&r) => r as u32,
            Ok(None) => 10,
            _ => {
                state.emit_error(
                    stderr,
                    format_args!(
                        "{}",
                        gettext("radix out of range (2-36) or non-numeric in eval")
                    ),
                )?;
                return Ok(state);
            }
        };
        let min_digits = match parse_optional_decimal(width_arg.as_deref()) {
            Ok(Some(w)) if w >= 0 => w as usize,
            Ok(None) => 1,
            _ => {
                state.emit_error(
                    stderr,
                    format_args!("{}", gettext("negative width or non-numeric in eval")),
                )?;
                return Ok(state);
            }
        };

        state
            .input
            .pushback_string(format_radix(output, radix, min_digits).as_bytes());
        Ok(state)
    }
}

/// Parse an optional decimal argument (the eval radix/width). `None` (omitted)
/// and an all-whitespace value yield `Ok(None)` (use the default); a malformed
/// value yields `Err`.
fn parse_optional_decimal(arg: Option<&[u8]>) -> std::result::Result<Option<i64>, ()> {
    match arg {
        Some(a) if a.iter().any(|c| !c.is_ascii_whitespace()) => {
            match nom::combinator::all_consuming(padded(parse_integer))(a) {
                Ok((_, v)) => Ok(Some(v)),
                Err(_) => Err(()),
            }
        }
        _ => Ok(None),
    }
}

/// Format `value` in `radix` (2-36, lowercase digits, GNU style), zero-padded to
/// at least `min_digits` digits, with a leading `-` for negative values.
fn format_radix(value: i64, radix: u32, min_digits: usize) -> String {
    const DIGITS: &[u8] = b"0123456789abcdefghijklmnopqrstuvwxyz";
    let radix = radix as u128;
    // Magnitude via i128 so that i64::MIN is handled without overflow.
    let mut n = (value as i128).unsigned_abs();
    let mut buf: Vec<u8> = Vec::new();
    if n == 0 {
        buf.push(b'0');
    }
    while n > 0 {
        buf.push(DIGITS[(n % radix) as usize]);
        n /= radix;
    }
    while buf.len() < min_digits {
        buf.push(b'0');
    }
    if value < 0 {
        buf.push(b'-');
    }
    buf.reverse();
    String::from_utf8(buf).expect("ascii digits are valid utf8")
}

/// A complete parser for a negative integer `[`[i64::MIN],[i64::MIN]`]`
pub(crate) fn parse_integer(input: &[u8]) -> IResult<&[u8], i64> {
    let (remaining, negative) = nom::combinator::opt(nom::bytes::complete::tag(b"-"))(input)?;
    let (remaining, mut i) = parse_positive_integer(remaining)?;
    if negative.is_some() {
        i = -i;
    }
    Ok((remaining, i))
}

/// A complete parser for a negative integer `[`0,[i64::MIN]`]`
pub(crate) fn parse_positive_integer(input: &[u8]) -> IResult<&[u8], i64> {
    let (remaining, number_input) =
        nom::bytes::complete::take_while(|c: u8| c.is_ascii_digit())(input)?;

    let s = std::str::from_utf8(number_input).map_err(|e| {
        nom::Err::Error(nom::error::Error::from_external_error(
            number_input,
            nom::error::ErrorKind::Fail,
            e,
        ))
    })?;

    let number = s.parse::<i64>().map_err(|e| {
        nom::Err::Error(nom::error::Error::from_external_error(
            number_input,
            nom::error::ErrorKind::Fail,
            e,
        ))
    })?;
    Ok((remaining, number))
}

/// Parse a non-negative integer literal in an `eval` expression. Unlike the
/// decimal-only [`parse_positive_integer`] used by the other built-ins (POSIX:
/// "Except for the first argument to the eval macro, all numeric arguments to
/// built-in macros shall be interpreted as decimal values"), this accepts the
/// ISO C octal (`0NN`) and hexadecimal (`0xNN`) forms, plus GNU's binary
/// (`0bNN`) extension.
fn parse_eval_number(input: &[u8]) -> IResult<&[u8], i64> {
    if let Some(rest) = input
        .strip_prefix(b"0x")
        .or_else(|| input.strip_prefix(b"0X"))
    {
        return parse_radix_digits(input, rest, 16, |c| c.is_ascii_hexdigit());
    }
    if let Some(rest) = input
        .strip_prefix(b"0b")
        .or_else(|| input.strip_prefix(b"0B"))
    {
        return parse_radix_digits(input, rest, 2, |c| matches!(c, b'0' | b'1'));
    }
    // A leading 0 followed by an octal digit is an octal constant; a bare 0
    // falls through to the decimal parser (yielding 0).
    if input.first() == Some(&b'0') && input.get(1).is_some_and(|c| (b'0'..=b'7').contains(c)) {
        return parse_radix_digits(input, &input[1..], 8, |c| (b'0'..=b'7').contains(&c));
    }
    parse_positive_integer(input)
}

fn parse_radix_digits<'a>(
    orig: &'a [u8],
    digits: &'a [u8],
    radix: u32,
    is_digit: impl Fn(u8) -> bool,
) -> IResult<&'a [u8], i64> {
    let (remaining, found) = nom::bytes::complete::take_while1(is_digit)(digits)?;
    let s = std::str::from_utf8(found).expect("ascii digits are valid utf8");
    let number = i64::from_str_radix(s, radix).map_err(|e| {
        nom::Err::Error(nom::error::Error::from_external_error(
            orig,
            nom::error::ErrorKind::Fail,
            e,
        ))
    })?;
    Ok((remaining, number))
}

pub(crate) fn padded_tag(
    t: &[u8],
) -> impl for<'i> Fn(&'i [u8]) -> IResult<&'i [u8], &'i [u8]> + '_ {
    move |input: &[u8]| {
        let (remaining, (_, p, _)) =
            tuple((take_while(is_whitespace), tag(t), take_while(is_whitespace)))(input)?;
        Ok((remaining, p))
    }
}

pub(crate) fn padded<'f, F, O>(f: F) -> impl for<'i> Fn(&'i [u8]) -> IResult<&'i [u8], O> + 'f
where
    F: for<'i> Fn(&'i [u8]) -> IResult<&'i [u8], O> + 'f,
{
    move |input: &[u8]| {
        let f = &f;
        let (remaining, (_, p, _)) =
            tuple((take_while(is_whitespace), f, take_while(is_whitespace)))(input)?;
        Ok((remaining, p))
    }
}

/// `input` takes the bytes of characters that we want to parse and evaluate.
/// The output is a result containing either Ok( ) or Err ( )
///
/// In the Ok ( ) case, it contains a tuple of (remaining, evaluated_output) of the eval.
///
/// For example:
///
/// ```ignore
/// let (remaining, evaluated) = parse_and_evaluate(b"1+2").unwrap();
/// assert_eq!(evaluated, 3);
/// assert_eq!(remaining, &[]);
/// ```
pub fn parse_and_evaluate(input: &[u8]) -> IResult<&[u8], i64> {
    if input.is_empty() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::NonEmpty,
        )));
    }
    precedence::precedence(
        // Unary prefix operators
        alt((
            unary_op(1, padded_tag(b"-")),
            unary_op(1, padded_tag(b"+")),
            unary_op(1, padded_tag(b"~")),
            unary_op(1, padded_tag(b"!")),
        )),
        // No Unary postfix operators
        fail,
        // Binary operators
        alt((
            binary_op(2, Assoc::Left, padded_tag(b"*")),
            binary_op(2, Assoc::Left, padded_tag(b"/")),
            binary_op(2, Assoc::Left, padded_tag(b"%")),
            binary_op(3, Assoc::Left, padded_tag(b"+")),
            binary_op(3, Assoc::Left, padded_tag(b"-")),
            // Shifts bind tighter than the relational operators (ISO C), so they
            // sit between additive (3) and relational (5).
            binary_op(4, Assoc::Left, padded_tag(b"<<")),
            binary_op(4, Assoc::Left, padded_tag(b">>")),
            binary_op(5, Assoc::Left, padded_tag(b"<=")),
            binary_op(5, Assoc::Left, padded_tag(b"<")),
            binary_op(5, Assoc::Left, padded_tag(b">=")),
            binary_op(5, Assoc::Left, padded_tag(b">")),
            binary_op(6, Assoc::Left, padded_tag(b"==")),
            binary_op(6, Assoc::Left, padded_tag(b"!=")),
            // Moved && and || before & and | for parsing order.
            binary_op(10, Assoc::Left, padded_tag(b"&&")),
            binary_op(11, Assoc::Left, padded_tag(b"||")),
            binary_op(7, Assoc::Left, padded_tag(b"&")),
            binary_op(8, Assoc::Left, padded_tag(b"^")),
            binary_op(9, Assoc::Left, padded_tag(b"|")),
        )),
        alt((
            padded(parse_eval_number),
            delimited(padded_tag(b"("), parse_and_evaluate, padded_tag(b")")),
        )),
        |op: Operation<&[u8], &[u8], &[u8], i64>| match op {
            // Arithmetic uses wrapping operations: POSIX leaves signed overflow
            // and out-of-range shifts undefined, and a panic (the Rust default
            // in debug, and for `/`,`%` even in release) must never abort m4.
            Operation::Prefix(b"-", o) => Ok(o.wrapping_neg()),
            Operation::Prefix(b"+", o) => Ok(o),
            Operation::Prefix(b"~", o) => Ok(!o),
            Operation::Prefix(b"!", o) => Ok(bool_to_int(!int_to_bool(o))),
            Operation::Binary(lhs, b"*", rhs) => Ok(lhs.wrapping_mul(rhs)),
            Operation::Binary(_, b"/", 0) => Err("divide by zero"),
            Operation::Binary(lhs, b"/", rhs) => Ok(lhs.wrapping_div(rhs)),
            Operation::Binary(_, b"%", 0) => Err("modulo by zero"),
            Operation::Binary(lhs, b"%", rhs) => Ok(lhs.wrapping_rem(rhs)),
            Operation::Binary(lhs, b"<<", rhs) => Ok(lhs.wrapping_shl(rhs as u32)),
            Operation::Binary(lhs, b">>", rhs) => Ok(lhs.wrapping_shr(rhs as u32)),
            Operation::Binary(lhs, b"+", rhs) => Ok(lhs.wrapping_add(rhs)),
            Operation::Binary(lhs, b"-", rhs) => Ok(lhs.wrapping_sub(rhs)),
            Operation::Binary(lhs, b"<", rhs) => Ok(bool_to_int(lhs < rhs)),
            Operation::Binary(lhs, b"<=", rhs) => Ok(bool_to_int(lhs <= rhs)),
            Operation::Binary(lhs, b">", rhs) => Ok(bool_to_int(lhs > rhs)),
            Operation::Binary(lhs, b">=", rhs) => Ok(bool_to_int(lhs >= rhs)),
            Operation::Binary(lhs, b"==", rhs) => Ok(bool_to_int(lhs == rhs)),
            Operation::Binary(lhs, b"!=", rhs) => Ok(bool_to_int(lhs != rhs)),
            Operation::Binary(lhs, b"&", rhs) => Ok(lhs & rhs),
            Operation::Binary(lhs, b"^", rhs) => Ok(lhs ^ rhs),
            Operation::Binary(lhs, b"|", rhs) => Ok(lhs | rhs),
            Operation::Binary(lhs, b"&&", rhs) => {
                Ok(bool_to_int(int_to_bool(lhs) && int_to_bool(rhs)))
            }
            Operation::Binary(lhs, b"||", rhs) => {
                Ok(bool_to_int(int_to_bool(lhs) || int_to_bool(rhs)))
            }
            _ => Err("Invalid combination"),
        },
    )(input)
}

fn bool_to_int(b: bool) -> i64 {
    match b {
        true => 1,
        false => 0,
    }
}

fn int_to_bool(i: i64) -> bool {
    i != 0
}
