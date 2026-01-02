//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Plural expression parser and evaluator
//!
//! This module parses and evaluates plural form expressions as used in GNU gettext.
//! These expressions are C-style expressions that determine which plural form to use
//! based on a count value `n`.
//!
//! Example expressions:
//! - English: `(n != 1)` - 2 forms: singular when n=1, plural otherwise
//! - Polish: `(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)` - 3 forms

use std::iter::Peekable;
use std::str::Chars;

/// A parsed plural expression that can be evaluated
#[derive(Debug, Clone, PartialEq)]
pub enum PluralExpr {
    /// The variable `n` (the count)
    N,
    /// A numeric literal
    Literal(u64),
    /// Binary operation
    BinaryOp(Box<PluralExpr>, BinaryOp, Box<PluralExpr>),
    /// Ternary conditional: condition ? if_true : if_false
    Ternary(Box<PluralExpr>, Box<PluralExpr>, Box<PluralExpr>),
}

/// Binary operators supported in plural expressions
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    /// Equality (==)
    Eq,
    /// Inequality (!=)
    Ne,
    /// Less than (<)
    Lt,
    /// Less than or equal (<=)
    Le,
    /// Greater than (>)
    Gt,
    /// Greater than or equal (>=)
    Ge,
    /// Modulo (%)
    Mod,
    /// Logical AND (&&)
    And,
    /// Logical OR (||)
    Or,
}

/// Error type for plural expression parsing
#[derive(Debug, Clone, PartialEq)]
pub enum PluralError {
    /// Unexpected character
    UnexpectedChar(char),
    /// Unexpected end of input
    UnexpectedEof,
    /// Expected a specific token
    Expected(String),
    /// Invalid expression
    InvalidExpression(String),
}

impl std::fmt::Display for PluralError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PluralError::UnexpectedChar(c) => write!(f, "unexpected character: '{}'", c),
            PluralError::UnexpectedEof => write!(f, "unexpected end of input"),
            PluralError::Expected(s) => write!(f, "expected {}", s),
            PluralError::InvalidExpression(s) => write!(f, "invalid expression: {}", s),
        }
    }
}

impl std::error::Error for PluralError {}

/// Token types for the lexer
#[derive(Debug, Clone, PartialEq)]
enum Token {
    /// The variable 'n'
    N,
    /// A number
    Number(u64),
    /// Left parenthesis
    LParen,
    /// Right parenthesis
    RParen,
    /// Question mark (ternary)
    Question,
    /// Colon (ternary)
    Colon,
    /// Equality (==)
    Eq,
    /// Inequality (!=)
    Ne,
    /// Less than (<)
    Lt,
    /// Less than or equal (<=)
    Le,
    /// Greater than (>)
    Gt,
    /// Greater than or equal (>=)
    Ge,
    /// Modulo (%)
    Mod,
    /// Logical AND (&&)
    And,
    /// Logical OR (||)
    Or,
    /// End of input
    Eof,
}

/// Lexer for plural expressions
struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer {
            chars: input.chars().peekable(),
        }
    }

    fn next_token(&mut self) -> Result<Token, PluralError> {
        self.skip_whitespace();

        match self.chars.peek().copied() {
            None => Ok(Token::Eof),
            Some(c) => match c {
                'n' => {
                    self.chars.next();
                    Ok(Token::N)
                }
                '0'..='9' => self.read_number(),
                '(' => {
                    self.chars.next();
                    Ok(Token::LParen)
                }
                ')' => {
                    self.chars.next();
                    Ok(Token::RParen)
                }
                '?' => {
                    self.chars.next();
                    Ok(Token::Question)
                }
                ':' => {
                    self.chars.next();
                    Ok(Token::Colon)
                }
                '=' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        Ok(Token::Eq)
                    } else {
                        Err(PluralError::Expected("'=' after '='".to_string()))
                    }
                }
                '!' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        Ok(Token::Ne)
                    } else {
                        Err(PluralError::Expected("'=' after '!'".to_string()))
                    }
                }
                '<' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        Ok(Token::Le)
                    } else {
                        Ok(Token::Lt)
                    }
                }
                '>' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        Ok(Token::Ge)
                    } else {
                        Ok(Token::Gt)
                    }
                }
                '%' => {
                    self.chars.next();
                    Ok(Token::Mod)
                }
                '&' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'&') {
                        self.chars.next();
                        Ok(Token::And)
                    } else {
                        Err(PluralError::Expected("'&' after '&'".to_string()))
                    }
                }
                '|' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'|') {
                        self.chars.next();
                        Ok(Token::Or)
                    } else {
                        Err(PluralError::Expected("'|' after '|'".to_string()))
                    }
                }
                ';' => {
                    // End of expression (in Plural-Forms header)
                    Ok(Token::Eof)
                }
                _ => Err(PluralError::UnexpectedChar(c)),
            },
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.chars.peek() {
            if c.is_whitespace() {
                self.chars.next();
            } else {
                break;
            }
        }
    }

    fn read_number(&mut self) -> Result<Token, PluralError> {
        let mut value: u64 = 0;
        while let Some(&c) = self.chars.peek() {
            if let Some(digit) = c.to_digit(10) {
                value = value * 10 + digit as u64;
                self.chars.next();
            } else {
                break;
            }
        }
        Ok(Token::Number(value))
    }
}

/// Parser for plural expressions
///
/// Operator precedence (lowest to highest):
/// 1. Ternary: ? :
/// 2. Logical OR: ||
/// 3. Logical AND: &&
/// 4. Equality: == !=
/// 5. Relational: < <= > >=
/// 6. Modulo: %
/// 7. Primary: n, literals, (expr)
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
}

impl<'a> Parser<'a> {
    /// Create a new parser for the given input
    pub fn new(input: &'a str) -> Result<Self, PluralError> {
        let mut lexer = Lexer::new(input);
        let current = lexer.next_token()?;
        Ok(Parser { lexer, current })
    }

    /// Parse the plural expression
    pub fn parse(&mut self) -> Result<PluralExpr, PluralError> {
        self.parse_ternary()
    }

    fn advance(&mut self) -> Result<(), PluralError> {
        self.current = self.lexer.next_token()?;
        Ok(())
    }

    fn expect(&mut self, expected: Token) -> Result<(), PluralError> {
        if self.current == expected {
            self.advance()
        } else {
            Err(PluralError::Expected(format!("{:?}", expected)))
        }
    }

    // Ternary: or_expr ('?' ternary ':' ternary)?
    fn parse_ternary(&mut self) -> Result<PluralExpr, PluralError> {
        let cond = self.parse_or()?;

        if self.current == Token::Question {
            self.advance()?;
            let if_true = self.parse_ternary()?;
            self.expect(Token::Colon)?;
            let if_false = self.parse_ternary()?;
            Ok(PluralExpr::Ternary(
                Box::new(cond),
                Box::new(if_true),
                Box::new(if_false),
            ))
        } else {
            Ok(cond)
        }
    }

    // Or: and_expr ('||' and_expr)*
    fn parse_or(&mut self) -> Result<PluralExpr, PluralError> {
        let mut left = self.parse_and()?;

        while self.current == Token::Or {
            self.advance()?;
            let right = self.parse_and()?;
            left = PluralExpr::BinaryOp(Box::new(left), BinaryOp::Or, Box::new(right));
        }

        Ok(left)
    }

    // And: equality_expr ('&&' equality_expr)*
    fn parse_and(&mut self) -> Result<PluralExpr, PluralError> {
        let mut left = self.parse_equality()?;

        while self.current == Token::And {
            self.advance()?;
            let right = self.parse_equality()?;
            left = PluralExpr::BinaryOp(Box::new(left), BinaryOp::And, Box::new(right));
        }

        Ok(left)
    }

    // Equality: relational_expr (('==' | '!=') relational_expr)*
    fn parse_equality(&mut self) -> Result<PluralExpr, PluralError> {
        let mut left = self.parse_relational()?;

        loop {
            let op = match self.current {
                Token::Eq => BinaryOp::Eq,
                Token::Ne => BinaryOp::Ne,
                _ => break,
            };
            self.advance()?;
            let right = self.parse_relational()?;
            left = PluralExpr::BinaryOp(Box::new(left), op, Box::new(right));
        }

        Ok(left)
    }

    // Relational: modulo_expr (('<' | '<=' | '>' | '>=') modulo_expr)*
    fn parse_relational(&mut self) -> Result<PluralExpr, PluralError> {
        let mut left = self.parse_modulo()?;

        loop {
            let op = match self.current {
                Token::Lt => BinaryOp::Lt,
                Token::Le => BinaryOp::Le,
                Token::Gt => BinaryOp::Gt,
                Token::Ge => BinaryOp::Ge,
                _ => break,
            };
            self.advance()?;
            let right = self.parse_modulo()?;
            left = PluralExpr::BinaryOp(Box::new(left), op, Box::new(right));
        }

        Ok(left)
    }

    // Modulo: primary ('%' primary)*
    fn parse_modulo(&mut self) -> Result<PluralExpr, PluralError> {
        let mut left = self.parse_primary()?;

        while self.current == Token::Mod {
            self.advance()?;
            let right = self.parse_primary()?;
            left = PluralExpr::BinaryOp(Box::new(left), BinaryOp::Mod, Box::new(right));
        }

        Ok(left)
    }

    // Primary: 'n' | number | '(' ternary ')'
    fn parse_primary(&mut self) -> Result<PluralExpr, PluralError> {
        match self.current.clone() {
            Token::N => {
                self.advance()?;
                Ok(PluralExpr::N)
            }
            Token::Number(v) => {
                self.advance()?;
                Ok(PluralExpr::Literal(v))
            }
            Token::LParen => {
                self.advance()?;
                let expr = self.parse_ternary()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            _ => Err(PluralError::Expected("n, number, or '('".to_string())),
        }
    }
}

impl PluralExpr {
    /// Parse a plural expression from a string
    pub fn parse(input: &str) -> Result<Self, PluralError> {
        let mut parser = Parser::new(input)?;
        parser.parse()
    }

    /// Evaluate the expression with the given value of n
    pub fn evaluate(&self, n: u64) -> u64 {
        match self {
            PluralExpr::N => n,
            PluralExpr::Literal(v) => *v,
            PluralExpr::BinaryOp(left, op, right) => {
                let l = left.evaluate(n);
                let r = right.evaluate(n);
                match op {
                    BinaryOp::Eq => u64::from(l == r),
                    BinaryOp::Ne => u64::from(l != r),
                    BinaryOp::Lt => u64::from(l < r),
                    BinaryOp::Le => u64::from(l <= r),
                    BinaryOp::Gt => u64::from(l > r),
                    BinaryOp::Ge => u64::from(l >= r),
                    BinaryOp::Mod => {
                        if r != 0 {
                            l % r
                        } else {
                            0
                        }
                    }
                    BinaryOp::And => u64::from(l != 0 && r != 0),
                    BinaryOp::Or => u64::from(l != 0 || r != 0),
                }
            }
            PluralExpr::Ternary(cond, if_true, if_false) => {
                if cond.evaluate(n) != 0 {
                    if_true.evaluate(n)
                } else {
                    if_false.evaluate(n)
                }
            }
        }
    }
}

/// Parse the plural expression from a "Plural-Forms:" header line
///
/// Expected format: "nplurals=N; plural=EXPR;"
pub fn parse_plural_forms(header: &str) -> Option<(usize, PluralExpr)> {
    let mut nplurals = None;
    let mut plural_expr = None;

    for part in header.split(';') {
        let part = part.trim();
        if let Some(val) = part.strip_prefix("nplurals=") {
            nplurals = val.trim().parse().ok();
        } else if let Some(val) = part.strip_prefix("plural=") {
            plural_expr = PluralExpr::parse(val.trim()).ok();
        }
    }

    match (nplurals, plural_expr) {
        (Some(n), Some(expr)) => Some((n, expr)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_english_plural() {
        // nplurals=2; plural=(n != 1);
        let expr = PluralExpr::parse("(n != 1)").unwrap();

        assert_eq!(expr.evaluate(0), 1); // "0 items"
        assert_eq!(expr.evaluate(1), 0); // "1 item"
        assert_eq!(expr.evaluate(2), 1); // "2 items"
        assert_eq!(expr.evaluate(100), 1); // "100 items"
    }

    #[test]
    fn test_french_plural() {
        // nplurals=2; plural=(n > 1);
        let expr = PluralExpr::parse("(n > 1)").unwrap();

        assert_eq!(expr.evaluate(0), 0); // "0 item"
        assert_eq!(expr.evaluate(1), 0); // "1 item"
        assert_eq!(expr.evaluate(2), 1); // "2 items"
    }

    #[test]
    fn test_polish_plural() {
        // nplurals=3; plural=(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);
        let expr =
            PluralExpr::parse("(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)")
                .unwrap();

        assert_eq!(expr.evaluate(1), 0); // singular
        assert_eq!(expr.evaluate(2), 1); // few (2-4)
        assert_eq!(expr.evaluate(3), 1);
        assert_eq!(expr.evaluate(4), 1);
        assert_eq!(expr.evaluate(5), 2); // many
        assert_eq!(expr.evaluate(12), 2); // exception (11-14)
        assert_eq!(expr.evaluate(22), 1); // few (22-24)
        assert_eq!(expr.evaluate(25), 2); // many
    }

    #[test]
    fn test_russian_plural() {
        // nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);
        let expr = PluralExpr::parse(
            "(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)",
        )
        .unwrap();

        assert_eq!(expr.evaluate(1), 0); // singular
        assert_eq!(expr.evaluate(2), 1); // few
        assert_eq!(expr.evaluate(5), 2); // many
        assert_eq!(expr.evaluate(11), 2); // exception
        assert_eq!(expr.evaluate(21), 0); // singular (21, 31, ...)
        assert_eq!(expr.evaluate(22), 1); // few
    }

    #[test]
    fn test_simple_expressions() {
        // Just n
        let expr = PluralExpr::parse("n").unwrap();
        assert_eq!(expr.evaluate(5), 5);

        // Just a number
        let expr = PluralExpr::parse("0").unwrap();
        assert_eq!(expr.evaluate(5), 0);

        // n % 10
        let expr = PluralExpr::parse("n % 10").unwrap();
        assert_eq!(expr.evaluate(25), 5);
    }

    #[test]
    fn test_parse_plural_forms() {
        let header = "nplurals=2; plural=(n != 1);";
        let (nplurals, expr) = parse_plural_forms(header).unwrap();

        assert_eq!(nplurals, 2);
        assert_eq!(expr.evaluate(1), 0);
        assert_eq!(expr.evaluate(2), 1);
    }

    #[test]
    fn test_parse_errors() {
        assert!(PluralExpr::parse("").is_err());
        assert!(PluralExpr::parse("x").is_err());
        assert!(PluralExpr::parse("n +").is_err());
        assert!(PluralExpr::parse("n = 1").is_err()); // single = not valid
    }
}
