//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Evaluation of `test` / `[` expressions (POSIX XCU `test`).
//!
//! Shared by the `test` utility and by the shell's built-in version. Every
//! shell builds `test` in, because a conditional that forks makes every loop
//! pay for a process; keeping one evaluator here is what stops the two from
//! drifting apart.

use std::ffi::CString;
use std::ffi::OsStr;
use std::io::IsTerminal;
use std::os::fd::BorrowedFd;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::{FileTypeExt, MetadataExt, PermissionsExt};
use std::path::Path;

use gettextrs::gettext;

/// Unary operators
#[allow(clippy::upper_case_acronyms)]
#[derive(PartialEq)]
enum UnaryOp {
    Block,
    Char,
    Directory,
    Exists,
    File,
    SGID,
    Symlink,
    StrNonZero,
    FIFO,
    Readable,
    Socket,
    SizeNonZero,
    Terminal,
    SUID,
    Writable,
    Executable,
    StrZero,
}

/// Binary operators
enum BinOp {
    PathEquals,
    PathNewer,
    PathOlder,
    StrEq,
    StrNE,
    StrLT,
    StrGT,
    IntEq,
    IntNE,
    IntLT,
    IntGT,
    IntGE,
    IntLE,
}

fn parse_unary_op(s: &[u8]) -> Option<UnaryOp> {
    match s {
        b"-b" => Some(UnaryOp::Block),
        b"-c" => Some(UnaryOp::Char),
        b"-d" => Some(UnaryOp::Directory),
        b"-e" => Some(UnaryOp::Exists),
        b"-f" => Some(UnaryOp::File),
        b"-g" => Some(UnaryOp::SGID),
        b"-h" => Some(UnaryOp::Symlink),
        b"-L" => Some(UnaryOp::Symlink),
        b"-n" => Some(UnaryOp::StrNonZero),
        b"-p" => Some(UnaryOp::FIFO),
        b"-r" => Some(UnaryOp::Readable),
        b"-S" => Some(UnaryOp::Socket),
        b"-s" => Some(UnaryOp::SizeNonZero),
        b"-t" => Some(UnaryOp::Terminal),
        b"-u" => Some(UnaryOp::SUID),
        b"-w" => Some(UnaryOp::Writable),
        b"-x" => Some(UnaryOp::Executable),
        b"-z" => Some(UnaryOp::StrZero),
        _ => None,
    }
}

fn want_metadata(op: &UnaryOp) -> bool {
    !matches!(
        op,
        UnaryOp::Terminal | UnaryOp::StrNonZero | UnaryOp::StrZero
    )
}

fn eval_str(s: &[u8]) -> bool {
    !s.is_empty()
}

fn eval_unary_str(op: &UnaryOp, s: &[u8]) -> bool {
    match op {
        UnaryOp::StrNonZero => eval_str(s),
        UnaryOp::StrZero => !eval_str(s),

        _ => {
            unreachable!()
        }
    }
}

/// Check file access using libc::access
fn check_access(path: &[u8], mode: libc::c_int) -> bool {
    let c_path = match CString::new(path.to_vec()) {
        Ok(p) => p,
        Err(_) => return false,
    };
    unsafe { libc::access(c_path.as_ptr(), mode) == 0 }
}

fn eval_unary_path(op: &UnaryOp, s: &[u8]) -> bool {
    // A path is bytes; converting it to text first would probe the wrong file.
    let path = Path::new(OsStr::from_bytes(s));

    // -h and -L must use symlink_metadata to not follow symlinks (POSIX requirement)
    if *op == UnaryOp::Symlink {
        return match path.symlink_metadata() {
            Ok(m) => m.file_type().is_symlink(),
            Err(_) => false,
        };
    }

    // -r, -w, -x use access() to check effective permissions (POSIX requirement)
    if *op == UnaryOp::Readable {
        return check_access(s, libc::R_OK);
    }
    if *op == UnaryOp::Writable {
        return check_access(s, libc::W_OK);
    }
    if *op == UnaryOp::Executable {
        return check_access(s, libc::X_OK);
    }

    // All other file tests use metadata (follows symlinks, per POSIX)
    let metadata = match path.metadata() {
        Ok(m) => m,
        Err(_) => {
            return false;
        }
    };

    match op {
        UnaryOp::Block => metadata.file_type().is_block_device(),
        UnaryOp::Char => metadata.file_type().is_char_device(),
        UnaryOp::Directory => metadata.is_dir(),
        UnaryOp::Exists => true,
        UnaryOp::File => metadata.is_file(),
        UnaryOp::SGID => metadata.permissions().mode() & 0o2000 != 0,
        UnaryOp::FIFO => metadata.file_type().is_fifo(),
        UnaryOp::Socket => metadata.file_type().is_socket(),
        UnaryOp::SizeNonZero => metadata.len() > 0,
        UnaryOp::SUID => metadata.permissions().mode() & 0o4000 != 0,
        _ => {
            unreachable!()
        }
    }
}

fn eval_terminal(s: &[u8]) -> bool {
    // A descriptor number is text by definition.
    let fd = match std::str::from_utf8(s)
        .ok()
        .and_then(|t| t.parse::<i32>().ok())
    {
        Some(f) => f,
        None => return false,
    };

    // Use safe Rust IsTerminal trait with BorrowedFd
    unsafe { BorrowedFd::borrow_raw(fd).is_terminal() }
}

fn eval_unary(op: &UnaryOp, s: &[u8]) -> bool {
    if want_metadata(op) {
        eval_unary_path(op, s)
    } else if *op == UnaryOp::Terminal {
        eval_terminal(s)
    } else {
        eval_unary_str(op, s)
    }
}

fn parse_binary_op(s: &[u8]) -> Option<BinOp> {
    match s {
        b"-ef" => Some(BinOp::PathEquals),
        b"-nt" => Some(BinOp::PathNewer),
        b"-ot" => Some(BinOp::PathOlder),
        b"=" => Some(BinOp::StrEq),
        b"!=" => Some(BinOp::StrNE),
        b"<" => Some(BinOp::StrLT),
        b">" => Some(BinOp::StrGT),
        b"-eq" => Some(BinOp::IntEq),
        b"-ne" => Some(BinOp::IntNE),
        b"-lt" => Some(BinOp::IntLT),
        b"-gt" => Some(BinOp::IntGT),
        b"-ge" => Some(BinOp::IntGE),
        b"-le" => Some(BinOp::IntLE),
        _ => None,
    }
}

/// Result type for expression evaluation that can indicate parse errors
#[derive(Debug)]
pub enum EvalResult {
    True,
    False,
    Error(String),
}

impl EvalResult {
    fn is_true(&self) -> bool {
        matches!(self, EvalResult::True)
    }

    fn negate(self) -> EvalResult {
        match self {
            EvalResult::True => EvalResult::False,
            EvalResult::False => EvalResult::True,
            e => e,
        }
    }
}

/// Parse an integer operand, tolerating surrounding blanks as historical
/// `test` implementations do. The diagnostic preserves the original operand.
/// (audit #3)
fn parse_int(s: &[u8]) -> Result<i64, EvalResult> {
    // An integer operand is text; one that is not cannot be a number.
    std::str::from_utf8(s)
        .ok()
        .and_then(|t| t.trim().parse().ok())
        .ok_or_else(|| {
            EvalResult::Error(format!(
                "{}: {}",
                gettext("integer expression expected"),
                String::from_utf8_lossy(s)
            ))
        })
}

fn eval_binary_int(op: &BinOp, s1: &[u8], s2: &[u8]) -> EvalResult {
    let i1: i64 = match parse_int(s1) {
        Ok(v) => v,
        Err(e) => return e,
    };
    let i2: i64 = match parse_int(s2) {
        Ok(v) => v,
        Err(e) => return e,
    };

    let result = match op {
        BinOp::IntEq => i1 == i2,
        BinOp::IntNE => i1 != i2,
        BinOp::IntLT => i1 < i2,
        BinOp::IntGT => i1 > i2,
        BinOp::IntGE => i1 >= i2,
        BinOp::IntLE => i1 <= i2,
        _ => {
            unreachable!()
        }
    };
    if result {
        EvalResult::True
    } else {
        EvalResult::False
    }
}

/// Compare two strings using the current locale's collating sequence
/// (`LC_COLLATE`), as POSIX requires for the `<` and `>` operators. Falls
/// back to byte comparison only if a string contains an embedded NUL, which
/// cannot occur in an argv operand but is guarded against regardless.
fn strcoll(s1: &[u8], s2: &[u8]) -> std::cmp::Ordering {
    match (CString::new(s1.to_vec()), CString::new(s2.to_vec())) {
        (Ok(c1), Ok(c2)) => {
            let r = unsafe { libc::strcoll(c1.as_ptr(), c2.as_ptr()) };
            r.cmp(&0)
        }
        _ => s1.cmp(s2),
    }
}

fn eval_binary_str(op: &BinOp, s1: &[u8], s2: &[u8]) -> bool {
    use std::cmp::Ordering;
    match op {
        BinOp::StrEq => s1 == s2,
        BinOp::StrNE => s1 != s2,
        BinOp::StrLT => strcoll(s1, s2) == Ordering::Less,
        BinOp::StrGT => strcoll(s1, s2) == Ordering::Greater,
        _ => {
            unreachable!()
        }
    }
}

fn eval_binary_path(op: &BinOp, s1: &[u8], s2: &[u8]) -> bool {
    let path1 = Path::new(OsStr::from_bytes(s1));
    let path2 = Path::new(OsStr::from_bytes(s2));
    let md1_res = path1.metadata();
    let md2_res = path2.metadata();

    match op {
        BinOp::PathEquals => {
            if md1_res.is_err() || md2_res.is_err() {
                return false;
            }
            let md1 = md1_res.unwrap();
            let md2 = md2_res.unwrap();

            (md1.dev() == md2.dev()) && (md1.ino() == md2.ino())
        }

        BinOp::PathNewer => {
            if md1_res.is_ok() && md2_res.is_err() {
                true
            } else if md1_res.is_ok() && md2_res.is_ok() {
                let l1 = md1_res.unwrap().modified().unwrap();
                let l2 = md2_res.unwrap().modified().unwrap();

                l1 > l2
            } else {
                false
            }
        }

        BinOp::PathOlder => {
            if md1_res.is_err() && md2_res.is_ok() {
                true
            } else if md1_res.is_ok() && md2_res.is_ok() {
                let l1 = md1_res.unwrap().modified().unwrap();
                let l2 = md2_res.unwrap().modified().unwrap();

                l1 < l2
            } else {
                false
            }
        }

        _ => {
            unreachable!()
        }
    }
}

fn eval_binary(s1: &[u8], op_str: &[u8], s2: &[u8]) -> EvalResult {
    let op = match parse_binary_op(op_str) {
        Some(p) => p,
        None => {
            return EvalResult::Error(format!(
                "{}: {}",
                gettext("unknown operator"),
                String::from_utf8_lossy(op_str)
            ));
        }
    };

    match op {
        BinOp::PathEquals | BinOp::PathNewer | BinOp::PathOlder => {
            if eval_binary_path(&op, s1, s2) {
                EvalResult::True
            } else {
                EvalResult::False
            }
        }
        BinOp::StrEq | BinOp::StrNE | BinOp::StrLT | BinOp::StrGT => {
            if eval_binary_str(&op, s1, s2) {
                EvalResult::True
            } else {
                EvalResult::False
            }
        }
        BinOp::IntEq | BinOp::IntNE | BinOp::IntLT | BinOp::IntGT | BinOp::IntGE | BinOp::IntLE => {
            eval_binary_int(&op, s1, s2)
        }
    }
}

/// Expression parser for XSI-compliant test expressions
/// Supports -a (AND), -o (OR), ! (NOT), and parentheses
struct ExprParser<'a> {
    args: &'a [Vec<u8>],
    pos: usize,
}

impl<'a> ExprParser<'a> {
    fn new(args: &'a [Vec<u8>]) -> Self {
        ExprParser { args, pos: 0 }
    }

    fn peek(&self) -> Option<&[u8]> {
        self.args.get(self.pos).map(|s| s.as_slice())
    }

    fn advance(&mut self) -> Option<&[u8]> {
        if self.pos < self.args.len() {
            let s = &self.args[self.pos];
            self.pos += 1;
            Some(s.as_slice())
        } else {
            None
        }
    }

    fn remaining(&self) -> usize {
        self.args.len() - self.pos
    }

    /// Parse an OR expression (lowest precedence)
    fn parse_or(&mut self) -> EvalResult {
        let mut left = self.parse_and();
        while self.peek() == Some(b"-o".as_slice()) {
            self.advance();
            let right = self.parse_and();
            match (&left, &right) {
                (EvalResult::Error(_), _) => return left,
                (_, EvalResult::Error(_)) => return right,
                _ => {
                    left = if left.is_true() || right.is_true() {
                        EvalResult::True
                    } else {
                        EvalResult::False
                    };
                }
            }
        }
        left
    }

    /// Parse an AND expression
    fn parse_and(&mut self) -> EvalResult {
        let mut left = self.parse_not();
        while self.peek() == Some(b"-a".as_slice()) {
            self.advance();
            let right = self.parse_not();
            match (&left, &right) {
                (EvalResult::Error(_), _) => return left,
                (_, EvalResult::Error(_)) => return right,
                _ => {
                    left = if left.is_true() && right.is_true() {
                        EvalResult::True
                    } else {
                        EvalResult::False
                    };
                }
            }
        }
        left
    }

    /// Parse a NOT expression
    fn parse_not(&mut self) -> EvalResult {
        if self.peek() == Some(b"!".as_slice()) {
            self.advance();
            self.parse_not().negate()
        } else {
            self.parse_primary()
        }
    }

    /// Parse a primary expression (unary test, binary test, or parenthesized expression)
    fn parse_primary(&mut self) -> EvalResult {
        // Handle parentheses
        if self.peek() == Some(b"(".as_slice()) {
            self.advance();
            let result = self.parse_or();
            if self.peek() == Some(b")".as_slice()) {
                self.advance();
                return result;
            } else {
                return EvalResult::Error(gettext("missing closing parenthesis").to_string());
            }
        }

        // Try to parse a unary or binary primary
        let first = match self.advance() {
            Some(s) => s.to_vec(),
            None => return EvalResult::False,
        };

        // Check for unary operators
        if let Some(unary_op) = parse_unary_op(&first) {
            if let Some(operand) = self.advance() {
                return if eval_unary(&unary_op, operand) {
                    EvalResult::True
                } else {
                    EvalResult::False
                };
            } else {
                return EvalResult::Error(format!(
                    "{}: {}",
                    gettext("argument expected"),
                    String::from_utf8_lossy(&first)
                ));
            }
        }

        // Check if next token is a binary operator
        if let Some(op) = self.peek() {
            if parse_binary_op(op).is_some() {
                let op = self.advance().unwrap().to_vec();
                if let Some(second) = self.advance() {
                    return eval_binary(&first, &op, second);
                } else {
                    return EvalResult::Error(format!(
                        "{}: {}",
                        gettext("argument expected"),
                        String::from_utf8_lossy(&op)
                    ));
                }
            }
        }

        // Treat as string test (non-empty string is true)
        if eval_str(&first) {
            EvalResult::True
        } else {
            EvalResult::False
        }
    }
}

/// Evaluate an expression using the extended grammar (`!`, `-a`, `-o`, and
/// `(` `)` grouping). POSIX.1-2024 removed `-a`, `-o`, `(`, and `)` and leaves
/// such expressions unspecified, so this is a compatibility extension for
/// historical scripts. It is the fallback for the 3- and 4-argument forms the
/// count-based algorithm does not assign a meaning, and handles all
/// expressions with more than four arguments. (audit #2)
fn eval_with_parser(args: &[Vec<u8>]) -> EvalResult {
    let mut parser = ExprParser::new(args);
    let result = parser.parse_or();
    if parser.remaining() > 0 {
        return EvalResult::Error(format!(
            "{}: {}",
            gettext("unexpected argument"),
            String::from_utf8_lossy(parser.peek().unwrap_or(b""))
        ));
    }
    result
}

/// Evaluate with POSIX-mandated rules for 0-4 arguments
pub fn eval_posix_strict(args: &[Vec<u8>]) -> EvalResult {
    match args.len() {
        0 => EvalResult::False,

        1 => {
            if eval_str(&args[0]) {
                EvalResult::True
            } else {
                EvalResult::False
            }
        }

        2 => {
            if args[0] == b"!" {
                if eval_str(&args[1]) {
                    EvalResult::False
                } else {
                    EvalResult::True
                }
            } else if let Some(op) = parse_unary_op(&args[0]) {
                if eval_unary(&op, &args[1]) {
                    EvalResult::True
                } else {
                    EvalResult::False
                }
            } else {
                EvalResult::Error(format!(
                    "{}: {}",
                    gettext("unary operator expected"),
                    String::from_utf8_lossy(&args[0])
                ))
            }
        }

        3 => {
            // If $2 is a binary primary, perform the binary test
            if parse_binary_op(&args[1]).is_some() {
                return eval_binary(&args[0], &args[1], &args[2]);
            }
            // If $1 is '!', negate the two-argument test
            if args[0] == b"!" {
                return eval_posix_strict(&args[1..]).negate();
            }
            // XSI: If $1 is '(' and $3 is ')', do unary test of $2
            if args[0] == b"(" && args[2] == b")" {
                if eval_str(&args[1]) {
                    return EvalResult::True;
                } else {
                    return EvalResult::False;
                }
            }
            // Otherwise unspecified by POSIX: fall back to the extended grammar
            // so legacy forms like `test x -a y` still evaluate. (audit #2)
            eval_with_parser(args)
        }

        4 => {
            // If $1 is '!', negate the three-argument test
            if args[0] == b"!" {
                return eval_posix_strict(&args[1..]).negate();
            }
            // XSI: If $1 is '(' and $4 is ')', do two-argument test of $2 and $3
            if args[0] == b"(" && args[3] == b")" {
                return eval_posix_strict(&args[1..3]);
            }
            // Otherwise unspecified by POSIX: fall back to the extended grammar. (audit #2)
            eval_with_parser(args)
        }

        // >4 arguments: extended grammar (also unspecified by POSIX).
        _ => eval_with_parser(args),
    }
}
