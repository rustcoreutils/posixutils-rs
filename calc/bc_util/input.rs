//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Gathering input lines into pieces that can be parsed and run.
//!
//! POSIX has input "executed as [it is] read", one input item at a time,
//! and an item may span many lines: a function definition, a braced block,
//! a string or a comment. Lines are gathered here until they may form whole
//! items, and only then handed to the parser. Re-parsing the gathered text
//! after every line would make a long construct cost quadratic time, so a
//! cheap scan of each line, done once, decides when a parse is worth trying.

/// What the lines gathered so far leave open.
#[derive(Default)]
struct Scan {
    in_string: bool,
    in_comment: bool,
    /// Open `{`, `(` and `[` less closed ones. A negative count is a syntax
    /// error, which the parser reports.
    depth: i64,
    /// The last line ended in a line continuation, `\` before the newline.
    continued: bool,
}

impl Scan {
    /// Account for one line, as the lexer would read it. Strings have no
    /// escapes, and a comment cannot nest.
    fn feed(&mut self, line: &[u8]) {
        self.continued = false;
        let mut i = 0;
        while i < line.len() {
            let c = line[i];
            let next = line.get(i + 1).copied();
            if self.in_comment {
                if c == b'*' && next == Some(b'/') {
                    self.in_comment = false;
                    i += 1;
                }
            } else if self.in_string {
                if c == b'"' {
                    self.in_string = false;
                }
            } else {
                match c {
                    b'"' => self.in_string = true,
                    b'/' if next == Some(b'*') => {
                        self.in_comment = true;
                        i += 1;
                    }
                    b'{' | b'(' | b'[' => self.depth += 1,
                    b'}' | b')' | b']' => self.depth -= 1,
                    b'\\' if next == Some(b'\n') => self.continued = true,
                    _ => {}
                }
            }
            i += 1;
        }
    }

    /// Whether the gathered lines could end with a complete input item.
    fn at_item_boundary(&self) -> bool {
        !self.in_string && !self.in_comment && !self.continued && self.depth <= 0
    }
}

/// Input lines gathered until they parse as whole input items.
pub struct Pending {
    text: String,
    /// The input line number of the first gathered line.
    first_line: usize,
    /// The input line number the next line will have.
    next_line: usize,
    scan: Scan,
}

impl Pending {
    pub fn new() -> Self {
        Pending {
            text: String::new(),
            first_line: 1,
            next_line: 1,
            scan: Scan::default(),
        }
    }

    /// Add one line of input, with its newline if it has one.
    pub fn push_line(&mut self, line: &str) {
        if self.text.is_empty() {
            self.first_line = self.next_line;
        }
        self.text.push_str(line);
        self.next_line += line.bytes().filter(|&b| b == b'\n').count();
        self.scan.feed(line.as_bytes());
    }

    /// Whether parsing the gathered text is worth trying: nothing in it is
    /// left open. A parse may still find it incomplete, as with an `if`
    /// whose body follows on the next line.
    pub fn ready(&self) -> bool {
        self.scan.at_item_boundary()
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn first_line(&self) -> usize {
        self.first_line
    }

    pub fn is_empty(&self) -> bool {
        self.text.is_empty()
    }

    /// Discard the gathered text, once it has been run or rejected. Line
    /// numbering carries on.
    pub fn clear(&mut self) {
        self.text.clear();
        self.scan = Scan::default();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ready_after(lines: &[&str]) -> bool {
        let mut pending = Pending::new();
        for line in lines {
            pending.push_line(line);
        }
        pending.ready()
    }

    #[test]
    fn a_plain_line_is_ready() {
        assert!(ready_after(&["1 + 2\n"]));
        assert!(ready_after(&["define f(x) { return (x) }\n"]));
    }

    #[test]
    fn open_brackets_wait_for_their_close() {
        assert!(!ready_after(&["define f(x) {\n"]));
        assert!(!ready_after(&["define f(x) {\n", "return (x)\n"]));
        assert!(ready_after(&["define f(x) {\n", "return (x)\n", "}\n"]));
        assert!(!ready_after(&["a[1 +\n"]));
        assert!(!ready_after(&["f(1,\n"]));
    }

    #[test]
    fn strings_and_comments_hide_brackets() {
        assert!(ready_after(&["\"{\"\n"]));
        assert!(ready_after(&["/* { */\n"]));
        assert!(!ready_after(&["\"a\n"]));
        assert!(ready_after(&["\"a\n", "b\"\n"]));
        assert!(!ready_after(&["/* a\n"]));
        assert!(ready_after(&["/* a\n", "b */ 1\n"]));
        // A quote inside a comment, and a comment opener inside a string.
        assert!(ready_after(&["/* \" */\n"]));
        assert!(ready_after(&["\"/*\"\n"]));
    }

    #[test]
    fn a_line_continuation_waits_for_the_next_line() {
        assert!(!ready_after(&["1 + \\\n"]));
        assert!(ready_after(&["1 + \\\n", "2\n"]));
    }

    /// What keeps a long construct linear: no parse is tried until it closes.
    #[test]
    fn a_long_body_is_parsed_once() {
        let mut pending = Pending::new();
        pending.push_line("define f() {\n");
        let mut attempts = 0;
        for _ in 0..20_000 {
            pending.push_line("x = x + 1\n");
            attempts += usize::from(pending.ready());
        }
        pending.push_line("}\n");
        attempts += usize::from(pending.ready());
        assert_eq!(attempts, 1);
    }

    #[test]
    fn a_stray_close_is_left_to_the_parser() {
        assert!(ready_after(&["}\n"]));
    }

    #[test]
    fn line_numbers_carry_on_across_items() {
        let mut pending = Pending::new();
        pending.push_line("1\n");
        assert_eq!(pending.first_line(), 1);
        pending.clear();
        pending.push_line("define f() {\n");
        pending.push_line("}\n");
        assert_eq!(pending.first_line(), 2);
        pending.clear();
        pending.push_line("f()\n");
        assert_eq!(pending.first_line(), 4);
        assert_eq!(pending.text(), "f()\n");
    }
}
