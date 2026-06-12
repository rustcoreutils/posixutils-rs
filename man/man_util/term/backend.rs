//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The single terminal backend shared by every front-end (`mdoc`, `man(7)`,
//! and the preprocessors). Front-ends do not build pre-indented strings; they
//! drive this imperative writer with a stream of words plus directives
//! (indent push/pop, line break, blank line, verbatim line). The backend owns
//! the one fill/wrap engine and all width math is in terminal cells via
//! [`display_width`], so styled (overstrike) spans never miscount.
//!
//! Assembly (`finish`) frames the body with the three-part header/footer,
//! collapses blank runs, and resolves style markers into nroff overstrike.

use super::page::{remove_empty_lines, three_part};
use super::style::{apply_styling, display_width};
use crate::man_util::formatter::replace_escapes;

/// Terminal output builder. See the module docs for the driving model.
pub struct Term {
    /// Total line width in columns.
    width: usize,
    /// Whether to resolve style markers into nroff overstrike on `finish`.
    styling: bool,
    /// Finished output lines.
    out: Vec<String>,
    /// Words pending in the current filled paragraph (already style-marked).
    pending: Vec<String>,
    /// Carry buffer for block ingestion (`append_block`): the partially-filled
    /// line that spans across appended blocks, mirroring the mdoc formatter's
    /// single `current_line` accumulator.
    current_line: String,
    /// Current left margin applied to filled text and `emit`/verbatim lines.
    offset: usize,
    /// Saved offsets for [`Term::push_indent`]/[`Term::pop_indent`].
    indent_stack: Vec<usize>,
    /// Centered three-part header line (empty if unset).
    header: String,
    /// Centered three-part footer line (empty if unset).
    footer: String,
    /// True once any non-blank body content has been produced.
    has_body: bool,
}

impl Term {
    pub fn new(width: usize, styling: bool) -> Self {
        Term {
            width: width.max(1),
            styling,
            out: Vec::new(),
            pending: Vec::new(),
            current_line: String::new(),
            offset: 0,
            indent_stack: Vec::new(),
            header: String::new(),
            footer: String::new(),
            has_body: false,
        }
    }

    /// The current left margin (front-ends read this to compute tag/body indents).
    pub fn offset(&self) -> usize {
        self.offset
    }

    /// Whether any non-blank body content has been emitted (empty-page check).
    pub fn has_body(&self) -> bool {
        self.has_body
    }

    /// Set the centered three-part header line (`left`/`center`/`right`). A
    /// trailing newline is kept so the framing in [`Term::finish`] reproduces the
    /// blank line beneath the header.
    pub fn set_header(&mut self, left: &str, center: &str, right: &str) {
        self.header = three_part(left, center, right, self.width) + "\n";
    }

    /// Set the header to an already-composed line (mdoc supplies its own centered
    /// header string, including its trailing newline).
    pub fn set_header_line(&mut self, line: String) {
        self.header = line;
    }

    /// Set the centered three-part footer line (`left`/`center`/`right`). A
    /// leading newline is kept to reproduce the blank line above the footer.
    pub fn set_footer(&mut self, left: &str, center: &str, right: &str) {
        self.footer = format!("\n{}", three_part(left, center, right, self.width));
    }

    /// Set the footer to an already-composed line (mdoc supplies its own footer
    /// string, including its leading newline).
    pub fn set_footer_line(&mut self, line: String) {
        self.footer = line;
    }

    /// Add one already-styled word to the current filled paragraph.
    pub fn word(&mut self, w: impl Into<String>) {
        let w = w.into();
        if !w.is_empty() {
            self.has_body = true;
        }
        self.pending.push(w);
    }

    /// Flush the pending paragraph, then push a single line at `indent`
    /// columns. Used for headings, hanging tags, and no-fill literal lines.
    pub fn emit(&mut self, indent: usize, text: impl Into<String>) {
        self.flush();
        let text = text.into();
        if !text.trim().is_empty() {
            self.has_body = true;
        }
        self.out.push(format!("{}{}", " ".repeat(indent), text));
    }

    /// End the current line without inserting vertical space (roff `.br`).
    pub fn break_line(&mut self) {
        self.flush();
    }

    /// Insert one blank line (paragraph break).
    pub fn blank_line(&mut self) {
        self.flush();
        self.out.push(String::new());
    }

    /// Enter a relative indent (roff `.RS`): remember the current margin and
    /// add `n` columns.
    pub fn push_indent(&mut self, n: usize) {
        self.flush();
        self.indent_stack.push(self.offset);
        self.offset += n;
    }

    /// Leave the most recent relative indent (roff `.RE`).
    pub fn pop_indent(&mut self) {
        self.flush();
        if let Some(prev) = self.indent_stack.pop() {
            self.offset = prev;
        }
    }

    /// Set the left margin absolutely (e.g. a hanging-tag body indent).
    pub fn set_offset(&mut self, n: usize) {
        self.flush();
        self.offset = n;
    }

    /// Reset to a base margin and drop all relative-indent state (new section).
    pub fn reset_indent(&mut self, base: usize) {
        self.flush();
        self.offset = base;
        self.indent_stack.clear();
    }

    /// Ingest a pre-rendered block string (one top-level node's output) and
    /// re-flow it to width, preserving each line's own leading indentation. This
    /// is the mdoc front-end's path into the backend; it carries a partial line
    /// across calls via `current_line`. Escapes are resolved here. (Ported from
    /// the formatter's former `append_formatted_text`, the canonical wrapper.)
    pub fn append_block(&mut self, formatted: &str) {
        if !formatted.trim().is_empty() {
            self.has_body = true;
        }
        let get_indent = |l: &str| {
            l.chars()
                .take_while(|ch| ch.is_whitespace())
                .collect::<String>()
        };

        let is_one_line = !formatted.contains('\n');
        let max_width = self.width;

        for line in formatted.split('\n') {
            let line = replace_escapes(line);

            if !is_one_line && !self.current_line.is_empty() {
                self.out.push(self.current_line.trim_end().to_string());
                self.current_line.clear();
            }

            let line_len = display_width(&self.current_line) + display_width(&line);
            if line_len > max_width || is_one_line {
                let indent = get_indent(&line);
                let max_width = max_width.saturating_sub(display_width(&indent));

                for word in line.split_whitespace() {
                    let line_len = display_width(&self.current_line) + display_width(word);
                    if line_len > max_width {
                        self.out.push(indent.clone() + self.current_line.trim());
                        self.current_line.clear();
                    }

                    self.current_line.push_str(word);

                    if !word.chars().all(|ch| ch.is_control()) {
                        self.current_line.push(' ');
                    }
                }

                let is_not_empty = !(self.current_line.chars().all(|ch| ch.is_whitespace())
                    || self.current_line.is_empty());

                if is_not_empty {
                    self.current_line = indent + &self.current_line;
                }
            } else {
                self.out.push(line.to_string());
            }
        }
    }

    /// Wrap and emit the pending paragraph words at the current offset.
    pub fn flush(&mut self) {
        if self.pending.is_empty() {
            return;
        }
        let words = std::mem::take(&mut self.pending);
        let indent = " ".repeat(self.offset);
        let max = self.width.saturating_sub(self.offset).max(1);
        let mut line = String::new();
        for word in words {
            let wlen = display_width(&word);
            if !line.is_empty() && display_width(&line) + 1 + wlen > max {
                self.out.push(format!("{indent}{line}"));
                line = String::new();
            }
            if line.is_empty() {
                line = word;
            } else {
                line.push(' ');
                line.push_str(&word);
            }
        }
        if !line.is_empty() {
            self.out.push(format!("{indent}{line}"));
        }
    }

    /// Assemble the framed page (header + body + footer), collapse blank runs,
    /// and resolve style markers into terminal output bytes. Handles both the
    /// word-stream path (`flush` of `pending`) and the block path (`current_line`).
    pub fn finish(mut self) -> Vec<u8> {
        self.flush();
        if !self.current_line.is_empty() {
            self.out.push(self.current_line.trim_end().to_string());
            self.current_line.clear();
        }

        // Drop leading blank lines so the page opens at the header.
        let lead = self
            .out
            .iter()
            .take_while(|l| l.chars().all(|c| c.is_whitespace()))
            .count();
        let mut lines = self.out.split_off(lead);

        // The header carries its own trailing newline and the footer its own
        // leading newline; the inserted blank reproduces the gap beneath the
        // header. `remove_empty_lines` then normalizes the runs.
        if !self.header.is_empty() {
            lines.insert(0, String::new());
            lines.insert(0, self.header.clone());
        }
        if !self.footer.is_empty() {
            lines.push(self.footer.clone());
        }

        let content = remove_empty_lines(&lines.join("\n"), 2);
        apply_styling(&content, self.styling).into_bytes()
    }
}
