//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! roff numeric-expression evaluator. roff has **no operator precedence** —
//! expressions evaluate strictly left to right, with parentheses for grouping
//! (so `1+2*3` is `9`, not `7`). Operands are integers with an optional and
//! (here) ignored scaling unit; the terminal device treats one basic unit as one
//! cell. Comparisons and the logical `&`/`:` yield `1`/`0`.

/// Evaluate a roff numeric expression. Returns `None` if it is not a well-formed
/// numeric expression (callers treat that as "no value" / false).
pub fn eval_numeric(s: &str) -> Option<i64> {
    let mut p = Parser {
        chars: s.chars().collect(),
        pos: 0,
        depth: 0,
    };
    let v = p.expr()?;
    p.skip_ws();
    // Anything left over (that is not a closing paren handled by the caller)
    // means the expression was malformed.
    if p.pos != p.chars.len() {
        return None;
    }
    Some(v)
}

/// Maximum nesting of `(`, unary `-`/`+`/`!` before an expression is rejected as
/// malformed. `term()` and `expr()` are mutually recursive with one native frame
/// per nesting level, so without this a line such as `.if ((((((…1` overflows the
/// stack — which aborts the process rather than unwinding, and so cannot be
/// contained by the caller. Real expressions nest a handful of levels at most.
const MAX_DEPTH: u32 = 64;

struct Parser {
    chars: Vec<char>,
    pos: usize,
    depth: u32,
}

impl Parser {
    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.peek();
        if c.is_some() {
            self.pos += 1;
        }
        c
    }

    fn skip_ws(&mut self) {
        while matches!(self.peek(), Some(c) if c.is_whitespace()) {
            self.pos += 1;
        }
    }

    /// Left-to-right sequence of terms joined by binary operators.
    fn expr(&mut self) -> Option<i64> {
        self.skip_ws();
        let mut acc = self.term()?;
        loop {
            self.skip_ws();
            let op = match self.peek() {
                Some(c) => c,
                None => break,
            };
            match op {
                '+' | '-' | '*' | '/' | '%' | '&' | ':' => {
                    self.bump();
                    let rhs = self.term()?;
                    acc = apply(acc, op, rhs);
                }
                '<' | '>' | '=' => {
                    self.bump();
                    let mut kind = op.to_string();
                    if matches!(self.peek(), Some('=') | Some('>') | Some('?')) {
                        kind.push(self.bump().unwrap());
                    }
                    let rhs = self.term()?;
                    acc = apply_cmp(acc, &kind, rhs);
                }
                '!' => {
                    self.bump();
                    if self.peek() == Some('=') {
                        self.bump();
                        let rhs = self.term()?;
                        acc = (acc != rhs) as i64;
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        Some(acc)
    }

    /// Depth-guarded wrapper around [`Parser::term_inner`]. Every recursion into
    /// the grammar passes through here, so one check covers both `(`-grouping and
    /// the unary operators.
    fn term(&mut self) -> Option<i64> {
        if self.depth >= MAX_DEPTH {
            return None;
        }
        self.depth += 1;
        let v = self.term_inner();
        self.depth -= 1;
        v
    }

    fn term_inner(&mut self) -> Option<i64> {
        self.skip_ws();
        match self.peek() {
            Some('(') => {
                self.bump();
                let v = self.expr()?;
                self.skip_ws();
                if self.peek() == Some(')') {
                    self.bump();
                }
                Some(v)
            }
            Some('-') => {
                self.bump();
                Some(self.term()?.saturating_neg())
            }
            Some('+') => {
                self.bump();
                self.term()
            }
            Some('!') => {
                self.bump();
                Some((self.term()? == 0) as i64)
            }
            Some(c) if c.is_ascii_digit() || c == '.' => self.number(),
            _ => None,
        }
    }

    fn number(&mut self) -> Option<i64> {
        let start = self.pos;
        while matches!(self.peek(), Some(c) if c.is_ascii_digit() || c == '.') {
            self.pos += 1;
        }
        let numstr: String = self.chars[start..self.pos].iter().collect();
        let val: f64 = numstr.parse().ok()?;
        // Optional, ignored scaling unit (i c p P m n v u f).
        if matches!(
            self.peek(),
            Some('i' | 'c' | 'p' | 'P' | 'm' | 'n' | 'v' | 'u' | 'f')
        ) {
            self.bump();
        }
        Some(val as i64)
    }
}

fn apply(a: i64, op: char, b: i64) -> i64 {
    match op {
        // Saturating, not wrapping: a page is untrusted input, and a debug build
        // (overflow-checks on) would otherwise panic on `.nr x 9999999999999999999*9`.
        '+' => a.saturating_add(b),
        '-' => a.saturating_sub(b),
        '*' => a.saturating_mul(b),
        // `checked_*` covers both division by zero and `i64::MIN / -1`.
        '/' => a.checked_div(b).unwrap_or(0),
        '%' => a.checked_rem(b).unwrap_or(0),
        '&' => (a != 0 && b != 0) as i64,
        ':' => (a != 0 || b != 0) as i64,
        _ => a,
    }
}

fn apply_cmp(a: i64, kind: &str, b: i64) -> i64 {
    match kind {
        "<" => (a < b) as i64,
        ">" => (a > b) as i64,
        "=" | "==" => (a == b) as i64,
        "<=" => (a <= b) as i64,
        ">=" => (a >= b) as i64,
        "<>" => (a != b) as i64,
        ">?" => a.max(b),
        "<?" => a.min(b),
        _ => 0,
    }
}
