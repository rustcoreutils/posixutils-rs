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

// #[derive(Debug)]
// enum Expression {
//     BinaryOperator(BinaryOperator),
//     UnaryOperator(UnaryOperator),
//     Integer(i32),
// }
//
// /// This can be a complete parser because expressions are only declared within macro arguments
// /// which must have a matching closing bracket to be parsed as a [`crate::lexer::Symbol`], so we
// /// will always have the complete expression when parsing it here.
// impl Expression {
//     fn parse(input: &[u8]) -> IResult<&[u8], Self> {
//         log::trace!("Expression::parse() input: {:?}", utf8(input));
//         nom::combinator::all_consuming(nom::branch::alt((
//             nom::combinator::map(
//                 BinaryOperator::parsehttps://exercism.org/(BinaryOperatorKind::Add),
//                 Self::BinaryOperator,
//             ),
//             // nom::combinator::map(
//             //     BinaryOperator::parse(BinaryOperatorKind::Subtract),
//             //     Self::BinaryOperator,
//             // ),
//             nom::combinator::map(parse_integer, Self::Integer),
//         )))(input)
//     }
//
//     fn evaluate(&self) -> i32 {
//         match self {
//             Expression::Integer(i) => *i,
//             Expression::BinaryOperator(operator) => operator.evaluate(),
//             Expression::UnaryOperator(_) => todo!(),
//         }
//     }
// }
//
// #[derive(Clone, Copy, Debug)]
// enum BinaryOperatorKind {
//     Add,
//     Subtract,
// }
//
// impl BinaryOperatorKind {
//     fn operator_tag(&self) -> &[u8] {
//         match self {
//             BinaryOperatorKind::Add => b"+",
//             BinaryOperatorKind::Subtract => b"-",
//         }
//     }
//     fn parse_operator_tag(self, input: &[u8]) -> IResult<&[u8], Self> {
//         nom::combinator::map(nom::bytes::complete::tag(self.operator_tag()), |_| self)(input)
//     }
// }
//
// #[derive(Debug)]
// struct BinaryOperator {
//     left: Box<Expression>,
//     right: Box<Expression>,
//     kind: BinaryOperatorKind,
// }
//
// fn utf8(input: &[u8]) -> Cow<str> {
//     String::from_utf8_lossy(input)
// }
//
// impl BinaryOperator {
//     fn parse(kind: BinaryOperatorKind) -> impl Fn(&[u8]) -> IResult<&[u8], Self> {
//         move |input: &[u8]| {
//             log::trace!("BinaryOperator::parse({kind:?}) input {:?}", utf8(input));
//             let (remaining, left) = Expression::parse(input)?;
//             let (remaining, _) =
//                 nom::bytes::complete::take_while(lexer::is_whitespace)(remaining)?;
//             let (remaining, kind) = kind.parse_operator_tag(remaining)?;
//             let (remaining, right) = Expression::parse(remaining)?;
//             Ok((
//                 remaining,
//                 Self {
//                     left: Box::new(left),
//                     right: Box::new(right),
//                     kind,
//                 },
//             ))
//         }
//     }
//
//     fn evaluate(&self) -> i32 {
//         let left = self.left.evaluate();
//         let right = self.right.evaluate();
//         match self.kind {
//             BinaryOperatorKind::Add => left + right,
//             BinaryOperatorKind::Subtract => left - right,
//         }
//     }
// }
//
// #[derive(Debug)]
// enum UnaryOperatorKind {
//     Plus,
//     Minus,
//     Not,
// }
//
// #[derive(Debug)]
// struct UnaryOperator {
//     expression: Box<Expression>,
//     kind: BinaryOperatorKind,
// }

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

pub fn parse(input: &[u8]) -> IResult<&[u8], i64> {
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
        )),
        alt((
            padded(parse_integer),
            delimited(padded_tag(b"("), parse, padded_tag(b")")),
        )),
        |op: Operation<&[u8], &[u8], &[u8], i64>| match op {
            Operation::Prefix(b"-", o) => Ok(-o),
            Operation::Prefix(b"+", o) => Ok(o),
            Operation::Binary(lhs, b"*", rhs) => Ok(lhs * rhs),
            Operation::Binary(lhs, b"/", rhs) => Ok(lhs / rhs),
            Operation::Binary(lhs, b"+", rhs) => Ok(lhs + rhs),
            Operation::Binary(lhs, b"-", rhs) => Ok(lhs - rhs),
            _ => Err("Invalid combination"),
        },
    )(input)
}

#[cfg(test)]
mod test {
    use super::parse;
    use test_log::test;

    #[test]
    fn test_add() {
        let (remaining, i) = parse(b"1+1").unwrap();
        assert_eq!(remaining, &[]);
        assert_eq!(i, 2);
    }

    #[test]
    fn test_add_padded() {
        let (remaining, i) = parse(b" 1 + 1 ").unwrap();
        assert_eq!(remaining, &[]);
        assert_eq!(i, 2);
    }

    #[test]
    fn test_multiply_add() {
        let (remaining, i) = parse(b" 7 * 2 + 1 ").unwrap();
        assert_eq!(remaining, &[]);
        assert_eq!(i, 15);
    }
}
