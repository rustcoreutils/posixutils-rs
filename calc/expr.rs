//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::diag;
use plib::regex::Regex;
use std::io::Write;

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
    /// An operand, exactly as it appeared in argv.
    ///
    /// POSIX operands are byte strings and need not be text. An operand is
    /// treated as an integer only where the operator requires it, so what is
    /// kept here is the argument's own spelling, not a parsed value: `007`
    /// must still print as `007` and match as three characters.
    Operand(Vec<u8>),
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

// the bytes of an operand
fn operand_bytes(t: &Token) -> Result<&[u8], String> {
    match t {
        Token::Operand(bytes) => Ok(bytes),
        _ => Err("syntax error: not a string".to_string()),
    }
}

// is token an lval?
fn token_is_lval(t: &Token) -> bool {
    matches!(t, Token::Operand(_))
}

fn bool_token(value: bool) -> Token {
    Token::Operand(if value { b"1".to_vec() } else { b"0".to_vec() })
}

// Does this operand satisfy the POSIX integer production?
//
// XCU expr: "integer: An argument consisting only of an (optional) unary minus
// followed by digits." A leading plus does not qualify, so `+5` is a string,
// and neither do surrounding blanks.
fn is_integer_text(bytes: &[u8]) -> bool {
    integer_parts(bytes).is_some()
}

// The sign (true if negative) and the digits of an operand that satisfies
// the integer production, or None for one that does not.
fn integer_parts(bytes: &[u8]) -> Option<(bool, &[u8])> {
    let (negative, digits) = match bytes.split_first() {
        Some((b'-', rest)) => (true, rest),
        _ => (false, bytes),
    };
    (!digits.is_empty() && digits.iter().all(u8::is_ascii_digit)).then_some((negative, digits))
}

// The integer value of an operand.
//
// Distinguishes an argument that is not an integer at all from one that is an
// integer this implementation cannot represent; reporting the second as a
// "non-integer argument" says something untrue about the input.
fn integer_value(bytes: &[u8]) -> Result<i128, String> {
    if !is_integer_text(bytes) {
        return Err("non-integer argument".to_string());
    }
    // ASCII by construction, so the conversion cannot fail.
    std::str::from_utf8(bytes)
        .map_err(|_| "non-integer argument".to_string())?
        .parse::<i128>()
        .map_err(|_| "integer out of range".to_string())
}

// Does the operand count as "null or zero"?
//
// Used both for the POSIX EXIT STATUS rule (exit 1 when the result is null or
// zero) and for the `&`/`|` operators. The empty string is null; an operand
// naming the integer zero (`0`, `00`, `-0`) is zero; anything else, including
// `0.0` and `+0`, is neither.
fn is_null_or_zero(bytes: &[u8]) -> bool {
    bytes.is_empty() || matches!(integer_value(bytes), Ok(0))
}

fn token_is_null_or_zero(t: &Token) -> bool {
    match t {
        Token::Operand(bytes) => is_null_or_zero(bytes),
        _ => false,
    }
}

// The argument a token came from, for a diagnostic.
fn token_text(t: &Token) -> String {
    let text: &[u8] = match t {
        Token::LParen => b"(",
        Token::RParen => b")",
        Token::OpMul => b"*",
        Token::OpDiv => b"/",
        Token::OpRem => b"%",
        Token::OpAdd => b"+",
        Token::OpSub => b"-",
        Token::OpEq => b"=",
        Token::OpGT => b">",
        Token::OpLT => b"<",
        Token::OpGE => b">=",
        Token::OpLE => b"<=",
        Token::OpNE => b"!=",
        Token::OpAnd => b"&",
        Token::OpOr => b"|",
        Token::OpMatch => b":",
        Token::Operand(bytes) => bytes,
    };
    String::from_utf8_lossy(text).into_owned()
}

fn unexpected_argument(t: &Token) -> String {
    format!("syntax error: unexpected argument '{}'", token_text(t))
}

// parse a single argument into a token
//
// POSIX APPLICATION USAGE: expr "is not required to be able to tell the
// difference between an operator and an operand except by the value", citing
// `expr = = =`. A word that spells an operator is therefore that operator;
// this is conformant, not a defect to be fixed with lookahead.
fn parse_token(arg: Vec<u8>) -> Token {
    match arg.as_slice() {
        b"(" => Token::LParen,
        b")" => Token::RParen,
        b"*" => Token::OpMul,
        b"/" => Token::OpDiv,
        b"%" => Token::OpRem,
        b"+" => Token::OpAdd,
        b"-" => Token::OpSub,
        b"=" => Token::OpEq,
        b">" => Token::OpGT,
        b"<" => Token::OpLT,
        b">=" => Token::OpGE,
        b"<=" => Token::OpLE,
        b"!=" => Token::OpNE,
        b"&" => Token::OpAnd,
        b"|" => Token::OpOr,
        b":" => Token::OpMatch,
        _ => Token::Operand(arg),
    }
}

// tokenize the command line arguments, all in a single pass
fn tokenize() -> Vec<Token> {
    // POSIX operands are byte strings: a pathname or a compared string need
    // not be text, and decoding argv as UTF-8 aborts on one that is not. On
    // Unix these are the argument's own bytes; Windows arguments are UTF-16,
    // read as UTF-8.
    let mut args: Vec<Vec<u8>> = std::env::args_os()
        .skip(1)
        .map(|arg| arg.as_encoded_bytes().to_vec())
        .collect();

    // POSIX / XBD 12.2 Guideline 10: a leading "--" delimits the end of
    // options. expr has no options, so a single leading "--" is consumed to
    // protect operands that begin with '-'.
    if args.first().map(Vec::as_slice) == Some(b"--".as_slice()) {
        args.remove(0);
    }

    args.into_iter().map(parse_token).collect()
}

// Compare two integers, given as the sign and digits of `integer_parts`, by
// their decimal value, whatever their size: one too large for the arithmetic
// is still an integer, and comparing it as a string gives the wrong answer.
fn compare_integers(a: (bool, &[u8]), b: (bool, &[u8])) -> std::cmp::Ordering {
    use std::cmp::Ordering;
    // the sign and the digits without leading zeros; -0 is zero
    fn magnitude((negative, mut digits): (bool, &[u8])) -> (bool, &[u8]) {
        while let [b'0', rest @ ..] = digits {
            digits = rest;
        }
        (negative && !digits.is_empty(), digits)
    }
    let (a_negative, a_digits) = magnitude(a);
    let (b_negative, b_digits) = magnitude(b);
    let by_size = a_digits
        .len()
        .cmp(&b_digits.len())
        .then_with(|| a_digits.cmp(b_digits));
    match (a_negative, b_negative) {
        (false, false) => by_size,
        (true, true) => by_size.reverse(),
        (true, false) => Ordering::Less,
        (false, true) => Ordering::Greater,
    }
}

// perform a comparison operation
//
// POSIX: "returns the result of a decimal integer comparison if both arguments
// are integers; otherwise, returns the result of a string comparison using the
// locale-specific collation sequence".
fn cmpop(lhs: &Token, rhs: &Token, op: CmpOp) -> Result<Token, String> {
    let lhs = operand_bytes(lhs)?;
    let rhs = operand_bytes(rhs)?;

    let ordering = match (integer_parts(lhs), integer_parts(rhs)) {
        (Some(a), Some(b)) => compare_integers(a, b),
        // Compare the operands as written; canonicalizing an integer first
        // would compare something the caller never passed.
        _ => plib::locale::strcoll_bytes(lhs, rhs),
    };

    let result = match op {
        CmpOp::EQ => ordering.is_eq(),
        CmpOp::NE => ordering.is_ne(),
        CmpOp::GT => ordering.is_gt(),
        CmpOp::LT => ordering.is_lt(),
        CmpOp::GE => ordering.is_ge(),
        CmpOp::LE => ordering.is_le(),
    };

    Ok(bool_token(result))
}

// perform an integer math operation
fn intop(lhs: &Token, rhs: &Token, op: IntOp) -> Result<Token, String> {
    let i1 = integer_value(operand_bytes(lhs)?)?;
    let i2 = integer_value(operand_bytes(rhs)?)?;

    let result = match op {
        IntOp::Add => i1.checked_add(i2),
        IntOp::Sub => i1.checked_sub(i2),
        IntOp::Mul => i1.checked_mul(i2),
        IntOp::Div => {
            if i2 == 0 {
                return Err("division by zero".to_string());
            }
            i1.checked_div(i2)
        }
        IntOp::Rem => {
            if i2 == 0 {
                return Err("division by zero".to_string());
            }
            // x % -1 is zero for every x; checked_rem refuses it only because
            // the corresponding division overflows.
            if i2 == -1 {
                Some(0)
            } else {
                i1.checked_rem(i2)
            }
        }
    };

    result
        .map(|value| Token::Operand(value.to_string().into_bytes()))
        .ok_or_else(|| "integer overflow".to_string())
}

fn token_is_null(t: &Token) -> bool {
    matches!(t, Token::Operand(bytes) if bytes.is_empty())
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
            bool_token(false)
        }
    } else if !lhs_zero {
        lhs.clone()
    } else if !token_is_null(rhs) {
        // POSIX: "returns the evaluation of expr1 if it is neither null nor
        // zero; otherwise, returns the evaluation of expr2 if it is not null;
        // otherwise, zero." expr2 is returned even when it is zero, but a null
        // expr2 yields zero rather than the null string.
        rhs.clone()
    } else {
        bool_token(false)
    }
}

// Index just past the bracket expression that starts at `open`.
//
// XBD 9.3.5: inside a bracket expression a backslash is an ordinary character,
// and a ']' is literal when it is the first character after the optional '^'.
// A character class, collating symbol or equivalence class may itself contain
// a ']', so those are skipped whole.
fn skip_bracket_expression(pattern: &[u8], open: usize) -> usize {
    let mut i = open + 1;
    if pattern.get(i) == Some(&b'^') {
        i += 1;
    }
    if pattern.get(i) == Some(&b']') {
        i += 1;
    }
    while i < pattern.len() {
        if pattern[i] == b'[' {
            if let Some(&kind) = pattern.get(i + 1) {
                if matches!(kind, b':' | b'.' | b'=') {
                    if let Some(end) = find_class_end(pattern, i + 2, kind) {
                        i = end;
                        continue;
                    }
                }
            }
        }
        if pattern[i] == b']' {
            return i + 1;
        }
        i += 1;
    }
    // Unterminated; regcomp will reject the pattern anyway.
    pattern.len()
}

// Index just past a "[:class:]", "[.symbol.]" or "[=equiv=]" opened at `from`.
fn find_class_end(pattern: &[u8], from: usize, kind: u8) -> Option<usize> {
    let mut i = from;
    while i + 1 < pattern.len() {
        if pattern[i] == kind && pattern[i + 1] == b']' {
            return Some(i + 2);
        }
        i += 1;
    }
    None
}

// Does the BRE pattern contain at least one subexpression "\(...\)"?
//
// A backslash that is itself escaped ("\\") does not begin one, and neither
// does one inside a bracket expression, where a backslash is ordinary.
fn has_subexpr(pattern: &[u8]) -> bool {
    let mut i = 0;
    while i < pattern.len() {
        match pattern[i] {
            b'\\' => {
                if pattern.get(i + 1) == Some(&b'(') {
                    return true;
                }
                i += 2; // skip the escaped character
            }
            b'[' => i = skip_bracket_expression(pattern, i),
            _ => i += 1,
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
fn matchop(lhs: &Token, rhs: &Token) -> Result<Token, String> {
    let subject = operand_bytes(lhs)?;
    let pattern = operand_bytes(rhs)?;

    let re = Regex::bre_bytes(pattern).map_err(|e| e.to_string())?;
    let captures = re.captures_bytes(subject);

    // The match must be anchored at the start of the string.
    let anchored = captures
        .as_ref()
        .map(|caps| caps[0].start == 0)
        .unwrap_or(false);

    if has_subexpr(pattern) {
        // Return the substring captured by "\1", or the null string.
        if anchored {
            let caps = captures.unwrap();
            Ok(Token::Operand(caps[1].as_bytes(subject).to_vec()))
        } else {
            Ok(Token::Operand(Vec::new()))
        }
    } else if anchored {
        // Return the number of characters matched. LC_CTYPE decides what a
        // character is, so in the C locale that is the number of bytes; the
        // match offsets are byte offsets either way.
        let caps = captures.unwrap();
        let matched = &subject[..caps[0].end];
        let count = plib::locale::mb_char_slices(matched).len();
        Ok(Token::Operand(count.to_string().into_bytes()))
    } else {
        Ok(bool_token(false))
    }
}

// Recursive-descent, precedence-climbing evaluator. Operator precedence and
// associativity follow the POSIX expr EXTENDED DESCRIPTION table (in order of
// decreasing precedence): grouping, ':', '* / %', '+ -', comparisons, '&',
// '|'. All binary operators are left-associative.
struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    depth: usize,
}

/// Maximum depth of nested parentheses.
///
/// The evaluator recurses on the machine stack, so without a limit a deeply
/// nested expression aborts the process instead of reporting an error. POSIX
/// requires only `{EXPR_NEST_MAX}`, whose minimum is 32; this is far above any
/// real use and below where the stack would run out in an unoptimized build.
const MAX_NEST_DEPTH: usize = 512;

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            pos: 0,
            depth: 0,
        }
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
    fn parse_or(&mut self) -> Result<Token, String> {
        let mut lhs = self.parse_and()?;
        while matches!(self.peek(), Some(Token::OpOr)) {
            self.advance();
            let rhs = self.parse_and()?;
            lhs = logop(&lhs, &rhs, false);
        }
        Ok(lhs)
    }

    // '&'
    fn parse_and(&mut self) -> Result<Token, String> {
        let mut lhs = self.parse_cmp()?;
        while matches!(self.peek(), Some(Token::OpAnd)) {
            self.advance();
            let rhs = self.parse_cmp()?;
            lhs = logop(&lhs, &rhs, true);
        }
        Ok(lhs)
    }

    // comparison operators: = > >= < <= !=
    fn parse_cmp(&mut self) -> Result<Token, String> {
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
    fn parse_add(&mut self) -> Result<Token, String> {
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
    fn parse_mul(&mut self) -> Result<Token, String> {
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
    fn parse_match(&mut self) -> Result<Token, String> {
        let mut lhs = self.parse_primary()?;
        while matches!(self.peek(), Some(Token::OpMatch)) {
            self.advance();
            let rhs = self.parse_primary()?;
            lhs = matchop(&lhs, &rhs)?;
        }
        Ok(lhs)
    }

    // primary: ( expr ) | operand
    fn parse_primary(&mut self) -> Result<Token, String> {
        match self.advance() {
            Some(Token::LParen) => {
                self.depth += 1;
                if self.depth > MAX_NEST_DEPTH {
                    self.depth -= 1;
                    return Err("expression nested too deeply".to_string());
                }
                let val = self.parse_or();
                self.depth -= 1;
                // Propagate before looking for the ')', so a failure inside the
                // group is not reported as a missing parenthesis.
                let val = val?;
                match self.advance() {
                    Some(Token::RParen) => Ok(val),
                    Some(t) => Err(format!(
                        "syntax error: expected ')' but found '{}'",
                        token_text(&t)
                    )),
                    None => Err("syntax error: missing ')'".to_string()),
                }
            }
            Some(t) if token_is_lval(&t) => Ok(t),
            Some(t) => Err(unexpected_argument(&t)),
            None => Err("syntax error: missing argument".to_string()),
        }
    }
}

// evaluate the whole token stream as a single expression
fn eval_expression(tokens: Vec<Token>) -> Result<Token, String> {
    let mut parser = Parser::new(tokens);
    let result = parser.parse_or()?;
    if let Some(t) = parser.peek() {
        return Err(unexpected_argument(t));
    }
    Ok(result)
}

/// Write the result and flush, so a failed write is seen here rather than
/// discarded at exit.
fn write_result(bytes: &[u8]) -> std::io::Result<()> {
    let mut stdout = std::io::stdout().lock();
    stdout.write_all(bytes)?;
    stdout.write_all(b"\n")?;
    stdout.flush()
}

fn main() {
    // Before anything opens a file, including the message catalog.
    plib::io::ensure_std_fds_open();
    diag::init_locale("expr");

    // tokenize and evaluate the expression
    let arg_tokens = tokenize();
    match eval_expression(arg_tokens).and_then(|value| {
        let bytes = operand_bytes(&value)?.to_vec();
        Ok((bytes, token_is_null_or_zero(&value)))
    }) {
        Ok((bytes, null_or_zero)) => {
            // POSIX EXIT STATUS: 0 means the expression evaluates to neither
            // null nor zero *and* the result was successfully written, so a
            // failed write is an error in its own right (">2 An error
            // occurred"), not a silent success.
            if let Err(e) = write_result(&bytes) {
                diag::error(&format!(
                    "{}: {}",
                    gettextrs::gettext("write error"),
                    diag::io_error_text(&e)
                ));
                std::process::exit(3);
            }
            std::process::exit(if null_or_zero { 1 } else { 0 });
        }
        Err(msg) => {
            // invalid expression: diagnostic to stderr, exit status 2.
            diag::error(&msg);
            std::process::exit(2);
        }
    }
}
