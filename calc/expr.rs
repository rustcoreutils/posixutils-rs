//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate plib;

use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use regex::Regex;

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
    Integer(i64),
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
    match t {
        Token::Integer(_) => true,
        Token::Str(_) => true,
        _ => false,
    }
}

// is token zero?
fn token_is_zero(t: &Token) -> bool {
    match t {
        Token::Integer(val) => *val == 0,
        Token::Str(s) => s.is_empty(),
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
fn token_to_int(t: &Token) -> Option<i64> {
    match t {
        Token::Integer(val) => Some(*val),
        _ => None,
    }
}

// convert token to integer, returning an error if not an integer
fn token_to_int_req(t: &Token) -> Result<i64, &'static str> {
    match token_to_int(t) {
        Some(val) => Ok(val),
        None => Err("syntax error: not an integer"),
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
        _ => match s.parse::<i64>() {
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

    // parse each arg into a Token
    let mut tokens = Vec::new();
    for arg in &args {
        tokens.push(parse_token(arg));
    }

    tokens
}

// compare two integers
fn cmpint(lhs: i64, rhs: i64, op: CmpOp) -> Token {
    let result: bool = match op {
        CmpOp::EQ => lhs == rhs,
        CmpOp::NE => lhs != rhs,
        CmpOp::GT => lhs > rhs,
        CmpOp::LT => lhs < rhs,
        CmpOp::GE => lhs >= rhs,
        CmpOp::LE => lhs <= rhs,
    };

    Token::Integer(result as i64)
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

    Ok(Token::Integer(result as i64))
}

// perform a comparison operation
fn cmpop(lhs: &Token, rhs: &Token, op: CmpOp) -> Result<Token, &'static str> {
    let lhs_int = token_to_int(lhs);
    let rhs_int = token_to_int(rhs);

    // if both are integers, perform int comparison
    if lhs_int.is_some() && rhs_int.is_some() {
        Ok(cmpint(lhs_int.unwrap(), rhs_int.unwrap(), op))

    // otherwise, convert int to string, and perform string compare
    } else if let Some(val) = lhs_int {
        let tmp = Token::Str(val.to_string());
        cmpstr(&tmp, rhs, op)
    } else if let Some(val) = rhs_int {
        let tmp = Token::Str(val.to_string());
        cmpstr(lhs, &tmp, op)
    } else {
        cmpstr(lhs, rhs, op)
    }
}

// perform an integer math operation
fn intop(lhs: &Token, rhs: &Token, op: IntOp) -> Result<Token, &'static str> {
    let i1 = token_to_int_req(lhs)?;
    let i2 = token_to_int_req(rhs)?;

    match op {
        IntOp::Add => Ok(Token::Integer(i1 + i2)),
        IntOp::Sub => Ok(Token::Integer(i1 - i2)),
        IntOp::Mul => Ok(Token::Integer(i1 * i2)),
        IntOp::Div => Ok(Token::Integer(i1 / i2)),
        IntOp::Rem => Ok(Token::Integer(i1 % i2)),
    }
}

// logical and/or operation
fn logop(lhs: &Token, rhs: &Token, is_and: bool) -> Token {
    let lhs_zero = token_is_zero(lhs);
    let rhs_zero = token_is_zero(rhs);

    if is_and {
        if !lhs_zero && !rhs_zero {
            lhs.clone()
        } else {
            Token::Integer(0)
        }
    } else {
        if !lhs_zero {
            lhs.clone()
        } else if !rhs_zero {
            rhs.clone()
        } else {
            Token::Integer(0)
        }
    }
}

// regex match operation
fn matchop(lhs: &Token, rhs: &Token) -> Result<Token, &'static str> {
    let lhs = token_to_string(lhs)?;
    let rhs = token_to_string(rhs)?;

    let re = match Regex::new(&rhs) {
        Ok(re_res) => re_res,
        Err(_) => {
            return Err("invalid regex");
        }
    };

    match re.captures(&lhs) {
        // no regex match: zero
        None => Ok(Token::Integer(0)),

        // regex matched
        Some(caps) => {
            let cap1 = caps.get(1);

            // if regex subexpression #1 matched, return as string
            if let Some(mtch) = cap1 {
                Ok(Token::Str(String::from(mtch.as_str())))

            // otherwise, return length of overall match as int
            } else {
                Ok(Token::Integer(caps.get(0).unwrap().len() as i64))
            }
        }
    }
}

// find closing right paren
fn find_matching_paren(tokens: &[Token]) -> Option<usize> {
    let mut depth = 0;
    for (i, token) in tokens.iter().enumerate() {
        match token {
            Token::LParen => depth += 1,
            Token::RParen => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }

    None
}

// evaluate an expression
fn eval_expression(tokens: &[Token]) -> Result<Token, &'static str> {
    let mut tokens = tokens.to_vec();

    // continually consume tokens until only one remains
    while tokens.len() >= 3 {
        // handle nested expressions: left hand side
        if tokens[0] == Token::LParen {
            if let Some(i) = find_matching_paren(&tokens) {
                let subexpr = &tokens[1..i];
                let result = eval_expression(subexpr)?;
                tokens.splice(0..=i, vec![result]);
                continue;
            } else {
                return Err("syntax error EP0: unmatched left paren");
            }
        }
        // handle nested expressions: right hand side
        if tokens[2] == Token::LParen {
            if let Some(i) = find_matching_paren(&tokens[2..]) {
                let subexpr = &tokens[3..i + 2];
                let result = eval_expression(subexpr)?;
                tokens.splice(2..=i + 2, vec![result]);
                continue;
            } else {
                return Err("syntax error EP1: unmatched left paren");
            }
        }

        // extract our left hand side, operator, and right hand side
        let lhs = &tokens[0];
        let operator = &tokens[1];
        let rhs = &tokens[2];

        // dispatch to the appropriate operation
        let result = match operator {
            Token::OpAdd => intop(lhs, rhs, IntOp::Add)?,
            Token::OpSub => intop(lhs, rhs, IntOp::Sub)?,
            Token::OpMul => intop(lhs, rhs, IntOp::Mul)?,
            Token::OpDiv => intop(lhs, rhs, IntOp::Div)?,
            Token::OpRem => intop(lhs, rhs, IntOp::Rem)?,

            Token::OpEq => cmpop(lhs, rhs, CmpOp::EQ)?,
            Token::OpNE => cmpop(lhs, rhs, CmpOp::NE)?,
            Token::OpGT => cmpop(lhs, rhs, CmpOp::GT)?,
            Token::OpLT => cmpop(lhs, rhs, CmpOp::LT)?,
            Token::OpGE => cmpop(lhs, rhs, CmpOp::GE)?,
            Token::OpLE => cmpop(lhs, rhs, CmpOp::LE)?,

            Token::OpAnd => logop(lhs, rhs, true),
            Token::OpOr => logop(lhs, rhs, false),

            Token::OpMatch => matchop(lhs, rhs)?,

            Token::LParen | Token::RParen | Token::Integer(_) | Token::Str(_) => {
                return Err("syntax error: wanted operator");
            }
        };

        // replace the lhs, operator, and rhs with the result
        tokens.splice(0..=2, vec![result]);
    }

    // final result should be a single token
    if tokens.len() == 1 {
        let lhs = &tokens[0];
        if token_is_lval(lhs) {
            Ok(lhs.clone())
        } else {
            Err("syntax error: E1")
        }
    } else {
        Err("syntax error: E2")
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // initialize translations
    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    // tokenize and evaluate the expression
    let arg_tokens = tokenize();
    let final_val = eval_expression(&arg_tokens)?;

    // display the result
    println!("{}", token_display(&final_val));

    Ok(())
}
