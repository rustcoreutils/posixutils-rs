use std::io::Write;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::combinator::fail;
use nom::error::FromExternalError;
use nom::sequence::{delimited, tuple};
use nom::IResult;

use super::MacroImplementation;
use crate::lexer::is_whitespace;
use crate::precedence::{self, binary_op, unary_op, Assoc, Operation};
use crate::state::{StackFrame, State};
use crate::Result;

pub struct EvalMacro;

impl MacroImplementation for EvalMacro {
    fn evaluate(&self, state: State, _stderr: &mut dyn Write, frame: StackFrame) -> Result<State> {
        let first_arg = frame
            .args
            .into_iter()
            .next()
            .ok_or_else(|| crate::Error::new(crate::ErrorKind::NotEnoughArguments))?;

        let (_, output) = nom::combinator::all_consuming(nom::combinator::complete(
            parse_and_evaluate,
        ))(&first_arg)?;
        state.input.pushback_string(output.to_string().as_bytes());
        Ok(state)
    }
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
            binary_op(5, Assoc::Left, padded_tag(b"<<")),
            binary_op(5, Assoc::Left, padded_tag(b">>")),
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
            padded(parse_positive_integer),
            delimited(padded_tag(b"("), parse_and_evaluate, padded_tag(b")")),
        )),
        |op: Operation<&[u8], &[u8], &[u8], i64>| match op {
            Operation::Prefix(b"-", o) => Ok(-o),
            Operation::Prefix(b"+", o) => Ok(o),
            Operation::Prefix(b"~", o) => Ok(!o),
            Operation::Prefix(b"!", o) => Ok(bool_to_int(!int_to_bool(o))),
            Operation::Binary(lhs, b"*", rhs) => Ok(lhs * rhs),
            Operation::Binary(lhs, b"/", rhs) => Ok(lhs / rhs),
            Operation::Binary(lhs, b"%", rhs) => Ok(lhs % rhs),
            Operation::Binary(lhs, b"<<", rhs) => Ok(lhs << rhs),
            Operation::Binary(lhs, b">>", rhs) => Ok(lhs >> rhs),
            Operation::Binary(lhs, b"+", rhs) => Ok(lhs + rhs),
            Operation::Binary(lhs, b"-", rhs) => Ok(lhs - rhs),
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
