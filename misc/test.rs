//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::CString;
use std::os::unix::fs::{FileTypeExt, MetadataExt, PermissionsExt};
use std::path::Path;

use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

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

fn parse_unary_op(s: &str) -> Option<UnaryOp> {
    match s {
        "-b" => Some(UnaryOp::Block),
        "-c" => Some(UnaryOp::Char),
        "-d" => Some(UnaryOp::Directory),
        "-e" => Some(UnaryOp::Exists),
        "-f" => Some(UnaryOp::File),
        "-g" => Some(UnaryOp::SGID),
        "-h" => Some(UnaryOp::Symlink),
        "-L" => Some(UnaryOp::Symlink),
        "-n" => Some(UnaryOp::StrNonZero),
        "-p" => Some(UnaryOp::FIFO),
        "-r" => Some(UnaryOp::Readable),
        "-S" => Some(UnaryOp::Socket),
        "-s" => Some(UnaryOp::SizeNonZero),
        "-t" => Some(UnaryOp::Terminal),
        "-u" => Some(UnaryOp::SUID),
        "-w" => Some(UnaryOp::Writable),
        "-x" => Some(UnaryOp::Executable),
        "-z" => Some(UnaryOp::StrZero),
        _ => None,
    }
}

fn want_metadata(op: &UnaryOp) -> bool {
    !matches!(op, UnaryOp::Terminal | UnaryOp::StrNonZero | UnaryOp::StrZero)
}

fn eval_str(s: &str) -> bool {
    !s.is_empty()
}

fn eval_unary_str(op: &UnaryOp, s: &str) -> bool {
    match op {
        UnaryOp::StrNonZero => eval_str(s),
        UnaryOp::StrZero => !eval_str(s),

        _ => {
            unreachable!()
        }
    }
}

/// Check file access using libc::access
fn check_access(path: &str, mode: libc::c_int) -> bool {
    let c_path = match CString::new(path) {
        Ok(p) => p,
        Err(_) => return false,
    };
    unsafe { libc::access(c_path.as_ptr(), mode) == 0 }
}

fn eval_unary_path(op: &UnaryOp, s: &str) -> bool {
    let path = Path::new(s);

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

fn eval_terminal(s: &str) -> bool {
    let fd = match s.parse::<u32>() {
        Ok(f) => f,
        Err(_) => {
            return false;
        }
    };

    // Normally, posixutils would use the atty crate.
    // Passing an arbitrary fd requires unsafe isatty in this case.

    unsafe { libc::isatty(fd as i32) == 1 }
}

fn eval_unary(op_str: &str, s: &str) -> bool {
    let op = match parse_unary_op(op_str) {
        Some(p) => p,
        None => {
            eprintln!("{}: {}", gettext("unknown operator"), op_str);
            return false;
        }
    };
    if want_metadata(&op) {
        eval_unary_path(&op, s)
    } else if op == UnaryOp::Terminal {
        eval_terminal(s)
    } else {
        eval_unary_str(&op, s)
    }
}

fn parse_binary_op(s: &str) -> Option<BinOp> {
    match s {
        "-ef" => Some(BinOp::PathEquals),
        "-nt" => Some(BinOp::PathNewer),
        "-ot" => Some(BinOp::PathOlder),
        "=" => Some(BinOp::StrEq),
        "!=" => Some(BinOp::StrNE),
        "<" => Some(BinOp::StrLT),
        ">" => Some(BinOp::StrGT),
        "-eq" => Some(BinOp::IntEq),
        "-ne" => Some(BinOp::IntNE),
        "-lt" => Some(BinOp::IntLT),
        "-gt" => Some(BinOp::IntGT),
        "-ge" => Some(BinOp::IntGE),
        "-le" => Some(BinOp::IntLE),
        _ => None,
    }
}

/// Result type for expression evaluation that can indicate parse errors
#[derive(Debug)]
enum EvalResult {
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

fn eval_binary_int(op: &BinOp, s1: &str, s2: &str) -> EvalResult {
    let i1: i64 = match s1.parse() {
        Ok(v) => v,
        Err(_) => {
            return EvalResult::Error(format!(
                "{}: {}",
                gettext("integer expression expected"),
                s1
            ))
        }
    };
    let i2: i64 = match s2.parse() {
        Ok(v) => v,
        Err(_) => {
            return EvalResult::Error(format!(
                "{}: {}",
                gettext("integer expression expected"),
                s2
            ))
        }
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

fn eval_binary_str(op: &BinOp, s1: &str, s2: &str) -> bool {
    match op {
        BinOp::StrEq => s1 == s2,
        BinOp::StrNE => s1 != s2,
        BinOp::StrLT => s1 < s2,
        BinOp::StrGT => s1 > s2,
        _ => {
            unreachable!()
        }
    }
}

fn eval_binary_path(op: &BinOp, s1: &str, s2: &str) -> bool {
    let path1 = Path::new(s1);
    let path2 = Path::new(s2);
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

fn eval_binary(s1: &str, op_str: &str, s2: &str) -> EvalResult {
    let op = match parse_binary_op(op_str) {
        Some(p) => p,
        None => {
            return EvalResult::Error(format!("{}: {}", gettext("unknown operator"), op_str));
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
    args: &'a [String],
    pos: usize,
}

impl<'a> ExprParser<'a> {
    fn new(args: &'a [String]) -> Self {
        ExprParser { args, pos: 0 }
    }

    fn peek(&self) -> Option<&str> {
        self.args.get(self.pos).map(|s| s.as_str())
    }

    fn advance(&mut self) -> Option<&str> {
        if self.pos < self.args.len() {
            let s = &self.args[self.pos];
            self.pos += 1;
            Some(s.as_str())
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
        while self.peek() == Some("-o") {
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
        while self.peek() == Some("-a") {
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
        if self.peek() == Some("!") {
            self.advance();
            self.parse_not().negate()
        } else {
            self.parse_primary()
        }
    }

    /// Parse a primary expression (unary test, binary test, or parenthesized expression)
    fn parse_primary(&mut self) -> EvalResult {
        // Handle parentheses
        if self.peek() == Some("(") {
            self.advance();
            let result = self.parse_or();
            if self.peek() == Some(")") {
                self.advance();
                return result;
            } else {
                return EvalResult::Error(gettext("missing closing parenthesis").to_string());
            }
        }

        // Try to parse a unary or binary primary
        let first = match self.advance() {
            Some(s) => s.to_string(),
            None => return EvalResult::False,
        };

        // Check for unary operators
        if parse_unary_op(&first).is_some() {
            if let Some(operand) = self.advance() {
                return if eval_unary(&first, operand) {
                    EvalResult::True
                } else {
                    EvalResult::False
                };
            } else {
                return EvalResult::Error(format!("{}: {}", gettext("argument expected"), first));
            }
        }

        // Check if next token is a binary operator
        if let Some(op) = self.peek() {
            if parse_binary_op(op).is_some() {
                let op = self.advance().unwrap().to_string();
                if let Some(second) = self.advance() {
                    return eval_binary(&first, &op, second);
                } else {
                    return EvalResult::Error(format!("{}: {}", gettext("argument expected"), op));
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

/// Evaluate with POSIX-mandated rules for 0-4 arguments
fn eval_posix_strict(args: &[String]) -> EvalResult {
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
            if args[0] == "!" {
                if eval_str(&args[1]) {
                    EvalResult::False
                } else {
                    EvalResult::True
                }
            } else if parse_unary_op(&args[0]).is_some() {
                if eval_unary(&args[0], &args[1]) {
                    EvalResult::True
                } else {
                    EvalResult::False
                }
            } else {
                EvalResult::Error(format!(
                    "{}: {}",
                    gettext("unary operator expected"),
                    args[0]
                ))
            }
        }

        3 => {
            // If $2 is a binary primary, perform the binary test
            if parse_binary_op(&args[1]).is_some() {
                return eval_binary(&args[0], &args[1], &args[2]);
            }
            // If $1 is '!', negate the two-argument test
            if args[0] == "!" {
                return eval_posix_strict(&args[1..]).negate();
            }
            // XSI: If $1 is '(' and $3 is ')', do unary test of $2
            if args[0] == "(" && args[2] == ")" {
                if eval_str(&args[1]) {
                    return EvalResult::True;
                } else {
                    return EvalResult::False;
                }
            }
            EvalResult::Error(gettext("syntax error").to_string())
        }

        4 => {
            // If $1 is '!', negate the three-argument test
            if args[0] == "!" {
                return eval_posix_strict(&args[1..]).negate();
            }
            // XSI: If $1 is '(' and $4 is ')', do two-argument test of $2 and $3
            if args[0] == "(" && args[3] == ")" {
                return eval_posix_strict(&args[1..3]);
            }
            EvalResult::Error(gettext("syntax error").to_string())
        }

        _ => {
            // >4 arguments: use XSI expression parser
            let mut parser = ExprParser::new(args);
            let result = parser.parse_or();
            if parser.remaining() > 0 {
                return EvalResult::Error(format!(
                    "{}: {}",
                    gettext("unexpected argument"),
                    parser.peek().unwrap_or("")
                ));
            }
            result
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args: Vec<String> = std::env::args().collect();

    // Detect if invoked as "[" or "test"
    let prog_name = Path::new(&args[0])
        .file_name()
        .map(|s| s.to_string_lossy().to_string())
        .unwrap_or_default();
    let is_bracket = prog_name == "[";

    // If program name is "[", then final arg must be "]"
    if is_bracket {
        if args.last().map(|s| s.as_str()) != Some("]") {
            eprintln!("{}", gettext("missing closing bracket"));
            std::process::exit(2);
        }
        args.pop();
    }

    // Remove program name
    args.remove(0);

    let result = eval_posix_strict(&args);

    match result {
        EvalResult::True => std::process::exit(0),
        EvalResult::False => std::process::exit(1),
        EvalResult::Error(msg) => {
            eprintln!("test: {}", msg);
            std::process::exit(2);
        }
    }
}
