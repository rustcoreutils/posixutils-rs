//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::regex::Regex;

#[derive(Clone, Debug, PartialEq)]
enum Token {
    LParen,
    RParen,
    OpMul,
    OpDiv,
    OpRem,
    OpAdd,
    OpSub,
    OpEq,
    OpGT,
    OpLT,
    OpGE,
    OpLE,
    OpNE,
    OpAnd,
    OpOr,
    OpMatch,
    Integer(i128),
    Str(String),
}

// comparison operators
#[derive(Clone, Debug)]
enum CmpOp {
    EQ,
    NE,
    GT,
    LT,
    GE,
    LE,
}

// integer operations
#[derive(Clone, Debug)]
enum IntOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

// convert an lval to a string
fn token_display(t: &Token) -> String {
    match t {
        Token::Integer(val) => val.to_string(),
        Token::Str(val) => String::from(val),
        _ => {
            panic!("BUG: not an lval");
        }
    }
}

// is token an lval?
fn token_is_lval(t: &Token) -> bool {
    matches!(t, Token::Integer(_) | Token::Str(_))
}

// Does the token count as "null or zero"?
//
// Used both for the POSIX EXIT STATUS rule (exit 1 when the result is null or
// zero) and for the `&`/`|` operators, whose operands are treated as integers
// when they consist of an optional sign followed by digits. The empty string
// is null; a string naming the integer zero (e.g. "0", "00", "-0") is zero;
// a non-integer string such as "0.0" or "abc" is neither.
fn token_is_null_or_zero(t: &Token) -> bool {
    match t {
        Token::Integer(val) => *val == 0,
        Token::Str(s) => s.is_empty() || matches!(s.parse::<i128>(), Ok(0)),
        _ => false,
    }
}

// convert token to string
fn token_to_string(t: &Token) -> Result<String, &'static str> {
    match t {
        Token::Integer(val) => Ok(val.to_string()),
        Token::Str(val) => Ok(String::from(val)),
        _ => Err("syntax error: not a string"),
    }
}

// convert token to integer
fn token_to_int(t: &Token) -> Option<i128> {
    match t {
        Token::Integer(val) => Some(*val),
        _ => None,
    }
}

// convert token to integer, returning an error if not an integer
fn token_to_int_req(t: &Token) -> Result<i128, &'static str> {
    match token_to_int(t) {
        Some(val) => Ok(val),
        None => Err("non-integer argument"),
    }
}

// parse a single token
fn parse_token(s: &str) -> Token {
    match s {
        "(" => Token::LParen,
        ")" => Token::RParen,
        "*" => Token::OpMul,
        "/" => Token::OpDiv,
        "%" => Token::OpRem,
        "+" => Token::OpAdd,
        "-" => Token::OpSub,
        "=" => Token::OpEq,
        ">" => Token::OpGT,
        "<" => Token::OpLT,
        ">=" => Token::OpGE,
        "<=" => Token::OpLE,
        "!=" => Token::OpNE,
        "&" => Token::OpAnd,
        "|" => Token::OpOr,
        ":" => Token::OpMatch,
        _ => match s.parse::<i128>() {
            Ok(n) => Token::Integer(n),
            Err(_) => Token::Str(String::from(s)),
        },
    }
}

// tokenize the command line arguments, all in a single pass
fn tokenize() -> Vec<Token> {
    // collect program's command line args
    let mut args: Vec<String> = std::env::args().collect();
    args.remove(0); // remove 1st value, the unnecessary program name

    // POSIX / XBD 12.2 Guideline 10: a leading "--" delimits the end of
    // options. expr has no options, so a single leading "--" is consumed to
    // protect operands that begin with '-'.
    if args.first().map(String::as_str) == Some("--") {
        args.remove(0);
    }

    // parse each arg into a Token
    let mut tokens = Vec::new();
    for arg in &args {
        tokens.push(parse_token(arg));
    }

    tokens
}

// compare two integers
fn cmpint(lhs: i128, rhs: i128, op: CmpOp) -> Token {
    let result: bool = match op {
        CmpOp::EQ => lhs == rhs,
        CmpOp::NE => lhs != rhs,
        CmpOp::GT => lhs > rhs,
        CmpOp::LT => lhs < rhs,
        CmpOp::GE => lhs >= rhs,
        CmpOp::LE => lhs <= rhs,
    };

    Token::Integer(result as i128)
}

// compare two strings
fn cmpstr(lhs: &Token, rhs: &Token, op: CmpOp) -> Result<Token, &'static str> {
    let lhs = token_to_string(lhs)?;
    let rhs = token_to_string(rhs)?;

    let result: bool = match op {
        CmpOp::EQ => lhs == rhs,
        CmpOp::NE => lhs != rhs,
        CmpOp::GT => lhs > rhs,
        CmpOp::LT => lhs < rhs,
        CmpOp::GE => lhs >= rhs,
        CmpOp::LE => lhs <= rhs,
    };

    Ok(Token::Integer(result as i128))
}

// perform a comparison operation
fn cmpop(lhs: &Token, rhs: &Token, op: CmpOp) -> Result<Token, &'static str> {
    let lhs_int = token_to_int(lhs);
    let rhs_int = token_to_int(rhs);

    match (lhs_int, rhs_int) {
        (Some(lhs), Some(rhs)) => {
            // if both are integers, perform int comparison
            Ok(cmpint(lhs, rhs, op))
        }
        // otherwise, convert int to string, and perform string compare
        (Some(lhs), _) => {
            let tmp = Token::Str(lhs.to_string());
            cmpstr(&tmp, rhs, op)
        }
        (_, Some(rhs)) => {
            let tmp = Token::Str(rhs.to_string());
            cmpstr(lhs, &tmp, op)
        }
        _ => cmpstr(lhs, rhs, op),
    }
}

// perform an integer math operation
fn intop(lhs: &Token, rhs: &Token, op: IntOp) -> Result<Token, &'static str> {
    let i1 = token_to_int_req(lhs)?;
    let i2 = token_to_int_req(rhs)?;

    let result = match op {
        IntOp::Add => i1.checked_add(i2),
        IntOp::Sub => i1.checked_sub(i2),
        IntOp::Mul => i1.checked_mul(i2),
        IntOp::Div => {
            if i2 == 0 {
                return Err("division by zero");
            }
            i1.checked_div(i2)
        }
        IntOp::Rem => {
            if i2 == 0 {
                return Err("division by zero");
            }
            i1.checked_rem(i2)
        }
    };

    result.map(Token::Integer).ok_or("integer overflow")
}

// logical and/or operation
fn logop(lhs: &Token, rhs: &Token, is_and: bool) -> Token {
    let lhs_zero = token_is_null_or_zero(lhs);
    let rhs_zero = token_is_null_or_zero(rhs);

    if is_and {
        // expr1 & expr2: return expr1 if neither is null or zero, else 0.
        if !lhs_zero && !rhs_zero {
            lhs.clone()
        } else {
            Token::Integer(0)
        }
    } else if !lhs_zero {
        // expr1 | expr2: return expr1 if it is neither null nor zero,
        // otherwise return expr2 (regardless of expr2's value).
        lhs.clone()
    } else {
        rhs.clone()
    }
}

// Does the BRE pattern contain at least one subexpression "\(...\)"?
// A backslash that is itself escaped ("\\") does not begin one.
fn has_subexpr(pattern: &str) -> bool {
    let bytes = pattern.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'\\' {
            if i + 1 < bytes.len() && bytes[i + 1] == b'(' {
                return true;
            }
            i += 2; // skip the escaped character
        } else {
            i += 1;
        }
    }
    false
}

// matching operator (':')
//
// Per POSIX: the pattern is a Basic Regular Expression (XBD 9.3) anchored to
// the beginning of the string. If the pattern contains a subexpression
// "\(...\)", the string matched by "\1" is returned (the null string if it
// did not match); otherwise the number of characters matched is returned (0
// on failure).
fn matchop(lhs: &Token, rhs: &Token) -> Result<Token, &'static str> {
    let lhs = token_to_string(lhs)?;
    let rhs = token_to_string(rhs)?;

    let re = Regex::bre(&rhs).map_err(|_| "invalid regex")?;
    let captures = re.captures(&lhs);

    // The match must be anchored at the start of the string.
    let anchored = captures
        .as_ref()
        .map(|caps| caps[0].start == 0)
        .unwrap_or(false);

    if has_subexpr(&rhs) {
        // Return the substring captured by "\1", or the null string.
        if anchored {
            let caps = captures.unwrap();
            let group1 = caps.get(1).copied().unwrap_or_default();
            Ok(Token::Str(String::from(group1.as_str(&lhs))))
        } else {
            Ok(Token::Str(String::new()))
        }
    } else if anchored {
        // Return the number of characters (not bytes) matched.
        let caps = captures.unwrap();
        let matched = &lhs[..caps[0].end];
        Ok(Token::Integer(matched.chars().count() as i128))
    } else {
        Ok(Token::Integer(0))
    }
}

// Recursive-descent, precedence-climbing evaluator. Operator precedence and
// associativity follow the POSIX expr EXTENDED DESCRIPTION table (in order of
// decreasing precedence): grouping, ':', '* / %', '+ -', comparisons, '&',
// '|'. All binary operators are left-associative.
struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<Token> {
        let t = self.tokens.get(self.pos).cloned();
        if t.is_some() {
            self.pos += 1;
        }
        t
    }

    // top level: '|', lowest precedence
    fn parse_or(&mut self) -> Result<Token, &'static str> {
        let mut lhs = self.parse_and()?;
        while matches!(self.peek(), Some(Token::OpOr)) {
            self.advance();
            let rhs = self.parse_and()?;
            lhs = logop(&lhs, &rhs, false);
        }
        Ok(lhs)
    }

    // '&'
    fn parse_and(&mut self) -> Result<Token, &'static str> {
        let mut lhs = self.parse_cmp()?;
        while matches!(self.peek(), Some(Token::OpAnd)) {
            self.advance();
            let rhs = self.parse_cmp()?;
            lhs = logop(&lhs, &rhs, true);
        }
        Ok(lhs)
    }

    // comparison operators: = > >= < <= !=
    fn parse_cmp(&mut self) -> Result<Token, &'static str> {
        let mut lhs = self.parse_add()?;
        loop {
            let op = match self.peek() {
                Some(Token::OpEq) => CmpOp::EQ,
                Some(Token::OpNE) => CmpOp::NE,
                Some(Token::OpGT) => CmpOp::GT,
                Some(Token::OpLT) => CmpOp::LT,
                Some(Token::OpGE) => CmpOp::GE,
                Some(Token::OpLE) => CmpOp::LE,
                _ => break,
            };
            self.advance();
            let rhs = self.parse_add()?;
            lhs = cmpop(&lhs, &rhs, op)?;
        }
        Ok(lhs)
    }

    // additive operators: + -
    fn parse_add(&mut self) -> Result<Token, &'static str> {
        let mut lhs = self.parse_mul()?;
        loop {
            let op = match self.peek() {
                Some(Token::OpAdd) => IntOp::Add,
                Some(Token::OpSub) => IntOp::Sub,
                _ => break,
            };
            self.advance();
            let rhs = self.parse_mul()?;
            lhs = intop(&lhs, &rhs, op)?;
        }
        Ok(lhs)
    }

    // multiplicative operators: * / %
    fn parse_mul(&mut self) -> Result<Token, &'static str> {
        let mut lhs = self.parse_match()?;
        loop {
            let op = match self.peek() {
                Some(Token::OpMul) => IntOp::Mul,
                Some(Token::OpDiv) => IntOp::Div,
                Some(Token::OpRem) => IntOp::Rem,
                _ => break,
            };
            self.advance();
            let rhs = self.parse_match()?;
            lhs = intop(&lhs, &rhs, op)?;
        }
        Ok(lhs)
    }

    // matching operator ':', highest-precedence binary operator
    fn parse_match(&mut self) -> Result<Token, &'static str> {
        let mut lhs = self.parse_primary()?;
        while matches!(self.peek(), Some(Token::OpMatch)) {
            self.advance();
            let rhs = self.parse_primary()?;
            lhs = matchop(&lhs, &rhs)?;
        }
        Ok(lhs)
    }

    // primary: ( expr ) | integer | string
    fn parse_primary(&mut self) -> Result<Token, &'static str> {
        match self.advance() {
            Some(Token::LParen) => {
                let val = self.parse_or()?;
                match self.advance() {
                    Some(Token::RParen) => Ok(val),
                    _ => Err("syntax error: expected ')'"),
                }
            }
            Some(t) if token_is_lval(&t) => Ok(t),
            Some(_) => Err("syntax error: unexpected operator"),
            None => Err("syntax error: missing argument"),
        }
    }
}

// evaluate the whole token stream as a single expression
fn eval_expression(tokens: Vec<Token>) -> Result<Token, &'static str> {
    let mut parser = Parser::new(tokens);
    let result = parser.parse_or()?;
    if parser.pos != parser.tokens.len() {
        return Err("syntax error: unexpected trailing argument");
    }
    Ok(result)
}

fn main() {
    setlocale(LocaleCategory::LcAll, "");
    let _ = textdomain("posixutils-rs");
    let _ = bind_textdomain_codeset("posixutils-rs", "UTF-8");

    // tokenize and evaluate the expression
    let arg_tokens = tokenize();
    match eval_expression(arg_tokens) {
        Ok(final_val) => {
            // display the result, then return exit status per POSIX:
            // 0 if the result is neither null nor zero, otherwise 1.
            println!("{}", token_display(&final_val));
            std::process::exit(if token_is_null_or_zero(&final_val) {
                1
            } else {
                0
            });
        }
        Err(msg) => {
            // invalid expression: diagnostic to stderr, exit status 2.
            eprintln!("expr: {}", msg);
            std::process::exit(2);
        }
    }
}
