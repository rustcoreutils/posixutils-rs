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

use super::page::{collapse_blank_lines, three_part};
use super::style::{apply_styling, display_width};

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

    /// Set the centered three-part header line (`left`/`center`/`right`).
    pub fn set_header(&mut self, left: &str, center: &str, right: &str) {
        self.header = three_part(left, center, right, self.width);
    }

    /// Set the centered three-part footer line (`left`/`center`/`right`).
    pub fn set_footer(&mut self, left: &str, center: &str, right: &str) {
        self.footer = three_part(left, center, right, self.width);
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
    /// and resolve style markers into terminal output bytes.
    pub fn finish(mut self) -> Vec<u8> {
        self.flush();

        let mut lines = Vec::with_capacity(self.out.len() + 4);
        if !self.header.is_empty() {
            lines.push(self.header.clone());
            lines.push(String::new());
        }
        lines.append(&mut self.out);
        if !self.footer.is_empty() {
            lines.push(String::new());
            lines.push(self.footer.clone());
        }

        let joined = collapse_blank_lines(&lines);
        apply_styling(&joined, self.styling).into_bytes()
    }
}
