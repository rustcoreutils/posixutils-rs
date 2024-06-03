use std::borrow::Cow;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    combinator::{fail, map_res},
    error::FromExternalError,
    multi::many0,
    sequence::{delimited, tuple},
    IResult,
};

use crate::{
    lexer::{self, is_whitespace},
    precedence::{self, binary_op, unary_op, Assoc, Operation},
};

fn parse_integer(input: &[u8]) -> IResult<&[u8], i64> {
    let (remaining, number_input) =
        nom::bytes::complete::take_while(|c| c >= b'0' && c <= b'9')(input)?;

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

fn padded_tag(t: &[u8]) -> impl for<'i> Fn(&'i [u8]) -> IResult<&'i [u8], &'i [u8]> + '_ {
    move |input: &[u8]| {
        let (remaining, (_, p, _)) =
            tuple((take_while(is_whitespace), tag(t), take_while(is_whitespace)))(input)?;
        Ok((remaining, p))
    }
}

fn padded<'f, F, O>(f: F) -> impl for<'i> Fn(&'i [u8]) -> IResult<&'i [u8], O> + 'f
where
    F: for<'i> Fn(&'i [u8]) -> IResult<&'i [u8], O> + 'f,
{
    move |input: &[u8]| {
        let (remaining, (_, p, _)) = tuple((
            take_while(is_whitespace),
            |i| f(i),
            take_while(is_whitespace),
        ))(input)?;
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
/// ```
/// let (remaining, evaluated) = parse_and_evaluate(b"1+2").unwrap();
/// assert_eq!(evaluated, 3);
/// assert_eq!(remaining, &[]);
/// ```
pub fn parse_and_evaluate(input: &[u8]) -> IResult<&[u8], i64> {
    if input.is_empty() {
        // TODO: return a nice error
    }
    precedence::precedence(
        // Unary prefix operators
        alt((unary_op(1, padded_tag(b"-")), unary_op(1, padded_tag(b"+")))),
        // No Unary postfix operators
        fail,
        // Binary operators
        alt((
            binary_op(2, Assoc::Left, padded_tag(b"*")),
            binary_op(3, Assoc::Left, padded_tag(b"+")),
            binary_op(3, Assoc::Left, padded_tag(b"-")),
            binary_op(3, Assoc::Left, padded_tag(b">")),
            binary_op(3, Assoc::Left, padded_tag(b"<")),
            binary_op(3, Assoc::Left, padded_tag(b"==")),
            binary_op(3, Assoc::Left, padded_tag(b"!=")),
            binary_op(3, Assoc::Left, padded_tag(b">=")),
            binary_op(3, Assoc::Left, padded_tag(b"<=")),
        )),
        alt((
            padded(parse_integer),
            delimited(padded_tag(b"("), parse_and_evaluate, padded_tag(b")")),
        )),
        |op: Operation<&[u8], &[u8], &[u8], i64>| match op {
            Operation::Prefix(b"-", o) => Ok(-o),
            Operation::Prefix(b"+", o) => Ok(o),
            Operation::Binary(lhs, b"*", rhs) => Ok(lhs * rhs),
            Operation::Binary(lhs, b"/", rhs) => Ok(lhs / rhs),
            Operation::Binary(lhs, b"+", rhs) => Ok(lhs + rhs),
            Operation::Binary(lhs, b"-", rhs) => Ok(lhs - rhs),
            Operation::Binary(lhs, b">", rhs) => Ok(bool_to_int(lhs > rhs)),
            Operation::Binary(lhs, b"<", rhs) => Ok(bool_to_int(lhs < rhs)),
            Operation::Binary(lhs, b"==", rhs) => Ok(bool_to_int(lhs == rhs)),
            Operation::Binary(lhs, b"!=", rhs) => Ok(bool_to_int(lhs != rhs)),
            Operation::Binary(lhs, b">=", rhs) => Ok(bool_to_int(lhs >= rhs)),
            Operation::Binary(lhs, b"<=", rhs) => Ok(bool_to_int(lhs <= rhs)),
            // Implement <, >, ==, !=, >=, <=
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

#[cfg(test)]
mod test {
    use crate::eval_macro::padded_tag;

    use super::parse_and_evaluate;
    use test_log::test;

    #[test]
    fn test_add() {
        let (remaining, i) = parse_and_evaluate(b"1+1").unwrap();
        assert_eq!(remaining, &[]);
        assert_eq!(i, 2);
    }

    #[test]
    fn test_add_padded() {
        let (remaining, i) = parse_and_evaluate(b" 1 + 1 ").unwrap();
        assert_eq!(remaining, &[]);
        assert_eq!(i, 2);
    }

    #[test]
    fn test_multiply_add() {
        let (remaining, i) = parse_and_evaluate(b" 7 * 2 + 1 ").unwrap();
        assert_eq!(remaining, &[]);
        assert_eq!(i, 15);
    }

    #[test]
    fn test_padded_lte() {
        let (remaining, unpadded) = padded_tag(b"<=")(b"  <=  ").unwrap();
        assert_eq!(remaining, b"");
        assert_eq!(unpadded, b"<=");
    }
}
