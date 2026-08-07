//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{
    cell::RefCell,
    io::{Read, Write},
    path::PathBuf,
    rc::Rc,
};

use crate::EOF;

/// Sentinel for [`InputState::output_current_line`] meaning "the input file
/// changed": the next directive emitted must carry the file name.
const SYNCLINE_FILE_CHANGED: i64 = -1;

#[derive(Default)]
pub struct InputState {
    pub line_synchronization: bool,
    pub input: Vec<Input>,
    /// The input line the emitted output is notionally sitting on, for `-s`.
    /// Negative means the input file changed and the next directive must name
    /// it. See [`InputState::write_synced`].
    output_current_line: i64,
    /// Whether the next byte written starts a new output line.
    start_of_output_line: bool,
    /// Nesting depth of comment / quoted-string scanning. See
    /// [`InputState::enter_literal_scan`].
    literal_scan_depth: usize,
    /// Whether the current literal has already had a directive decision made
    /// for it; further output lines inside it are not eligible.
    literal_line_start_checked: bool,
}

impl InputState {
    pub fn new(line_synchronization: bool) -> Self {
        Self {
            line_synchronization,
            input: Vec::new(),
            output_current_line: SYNCLINE_FILE_CHANGED,
            start_of_output_line: true,
            literal_scan_depth: 0,
            literal_line_start_checked: false,
        }
    }

    /// Enter a comment or quoted string. A literal is treated as one unit for
    /// `-s`: at most one `#line` directive is written for it, ahead of its first
    /// output line. Later output lines inside the same literal still advance the
    /// counter but never get a directive of their own.
    ///
    /// POSIX ties `-s` to the c17 preprocessor phase, where a directive landing
    /// in the middle of a C comment or string literal would corrupt the
    /// translation unit — so a literal is never split, however far the output
    /// has drifted from the input. The drift is corrected by the first directive
    /// after the literal ends. GNU m4, which scans a whole comment or string
    /// into a single token before shipping it, arrives at the same rule.
    pub fn enter_literal_scan(&mut self) {
        if self.literal_scan_depth == 0 {
            // A literal that begins part-way through an output line gets no
            // directive at all: the decision for the line it started on was
            // already made, for the text in front of it.
            self.literal_line_start_checked = !self.start_of_output_line;
        }
        self.literal_scan_depth += 1;
    }

    pub fn leave_literal_scan(&mut self) {
        self.literal_scan_depth = self.literal_scan_depth.saturating_sub(1);
    }

    pub fn input_push(&mut self, input: Input) {
        // Entering a new file does not emit anything by itself: the directive is
        // written lazily, before the first byte of the next output line, and
        // this makes it carry the new file name when it is.
        self.output_current_line = SYNCLINE_FILE_CHANGED;
        self.input.push(input);
    }

    pub fn input_pop(&mut self) -> Option<Input> {
        let popped = self.input.pop();
        // Returning to the enclosing file is a file change too.
        self.output_current_line = SYNCLINE_FILE_CHANGED;
        popped
    }

    /// Forget where the output is, so the next `-s` directive is written in the
    /// long `#line N "FILE"` form. Called whenever the output stream changes
    /// underfoot — a `divert` to a different buffer, or an `undivert` splicing
    /// text captured at unrelated input lines — since the position tracked for
    /// one stream says nothing about the next.
    pub fn invalidate_syncline_position(&mut self) {
        self.output_current_line = SYNCLINE_FILE_CHANGED;
    }

    /// Account for bytes written straight to the output, bypassing
    /// [`InputState::write_synced`] (see `Output::write_raw`). `mid_line` says
    /// whether those bytes left the output part-way through a line.
    pub fn resume_after_raw_output(&mut self, mid_line: bool) {
        self.invalidate_syncline_position();
        self.start_of_output_line = !mid_line;
    }

    /// Write `buf` to `output`, preceding each output line with a `#line`
    /// directive when line synchronization (`-s`) is on and the output has
    /// drifted from the input line the text came from.
    ///
    /// Directives are emitted lazily — immediately before the first byte of an
    /// output line, never after a newline — so a directive is only ever written
    /// when a byte actually follows it, and it describes the source of that
    /// byte. The file name is included only when the input file changed;
    /// otherwise the short `#line N` form is used.
    pub fn write_synced(&mut self, output: &mut dyn Write, buf: &[u8]) -> std::io::Result<usize> {
        if !self.line_synchronization {
            return output.write(buf);
        }

        let (name, line) = self.current_location();
        let line = line as i64;
        let mut n = 0;
        for c in buf {
            if self.start_of_output_line {
                self.start_of_output_line = false;
                self.output_current_line += 1;
                let eligible = if self.literal_scan_depth > 0 {
                    !std::mem::replace(&mut self.literal_line_start_checked, true)
                } else {
                    true
                };
                if self.output_current_line != line && eligible {
                    let file_changed = self.output_current_line < 1;
                    write!(output, "#line {line}")?;
                    if file_changed && !name.is_empty() {
                        output.write_all(b" \"")?;
                        output.write_all(&name)?;
                        output.write_all(b"\"")?;
                    }
                    output.write_all(b"\n")?;
                    self.output_current_line = line;
                }
            }
            n += output.write(std::slice::from_ref(c))?;
            if *c == b'\n' {
                self.start_of_output_line = true;
            }
        }
        Ok(n)
    }

    /// Get the next character to be parsed. First it tries to get one from the pushback buffer,
    /// otherwise it gets one from the input file.
    pub fn get_next_character(&mut self) -> crate::error::Result<u8> {
        let input = self.input.last_mut().unwrap();
        if let Some(c) = input.pushback_buffer.pop() {
            return Ok(c);
        }
        Ok(input.get_next_character()?)
    }

    pub fn pushback_character(&mut self, c: u8) {
        self.input.last_mut().unwrap().pushback_buffer.push(c);
    }

    pub fn pushback_string(&mut self, s: &[u8]) {
        let pushback_buffer = &mut self.input.last_mut().unwrap().pushback_buffer;
        for c in s.iter().rev() {
            pushback_buffer.push(*c);
        }
    }

    /// Fetch new characters attempting to match them all to token. If any character doesn't match
    /// the token, then all the characters previously fetched are placed onto the pushback buffer.
    ///
    /// **NOTE:** Will panic if token is not length of at least 1.
    ///
    /// * `c` - First character of input which is already available.
    /// * `token` - Token to match against.
    pub fn look_ahead(&mut self, mut c: u8, token: &[u8]) -> crate::error::Result<bool> {
        if c == EOF || c != token[0] {
            return Ok(false);
        }

        let mut i = 1;
        while i < token.len() {
            c = self.get_next_character()?;
            if c == EOF || c != token[i] {
                loop {
                    self.pushback_character(token[i]);
                    if i == 0 {
                        break;
                    }
                    i -= 1;
                }
                return Ok(false);
            }

            i += 1;
        }

        Ok(true)
    }

    /// The name and current line number of the active input, for diagnostics
    /// (`stdin` when reading standard input). Used to format GNU-style
    /// `m4:<file>:<line>:` messages.
    pub fn current_location(&self) -> (Vec<u8>, usize) {
        match self.input.last() {
            Some(input) => {
                let name = match &input.input {
                    InputRead::File { path, .. } => path.as_os_str().as_encoded_bytes().to_vec(),
                    InputRead::Stdin(_) => b"stdin".to_vec(),
                };
                (name, input.line_number)
            }
            None => (b"stdin".to_vec(), 0),
        }
    }
}

#[derive(Clone, Default)]
pub struct InputStateRef(Rc<RefCell<InputState>>);

impl InputStateRef {
    pub fn new(input_state: InputState) -> Self {
        Self(Rc::new(RefCell::new(input_state)))
    }

    pub fn with<F, T>(&self, f: F) -> T
    where
        F: Fn(&InputState) -> T,
    {
        f(&self.0.borrow())
    }

    pub fn input_pop(&self) -> Option<Input> {
        self.0.borrow_mut().input_pop()
    }

    pub fn input_push(&self, input: Input) {
        self.0.borrow_mut().input_push(input)
    }

    pub fn input_len(&self) -> usize {
        self.0.borrow().input.len()
    }

    pub fn get_next_character(&self) -> crate::error::Result<u8> {
        self.0.borrow_mut().get_next_character()
    }

    pub fn pushback_character(&self, c: u8) {
        self.0.borrow_mut().pushback_character(c)
    }

    pub fn pushback_string(&self, s: &[u8]) {
        self.0.borrow_mut().pushback_string(s)
    }

    pub fn look_ahead(&self, c: u8, token: &[u8]) -> crate::error::Result<bool> {
        self.0.borrow_mut().look_ahead(c, token)
    }

    pub fn write_synced(&self, output: &mut dyn Write, buf: &[u8]) -> std::io::Result<usize> {
        self.0.borrow_mut().write_synced(output, buf)
    }

    pub fn invalidate_syncline_position(&self) {
        self.0.borrow_mut().invalidate_syncline_position()
    }

    pub fn resume_after_raw_output(&self, mid_line: bool) {
        self.0.borrow_mut().resume_after_raw_output(mid_line)
    }

    pub fn enter_literal_scan(&self) {
        self.0.borrow_mut().enter_literal_scan()
    }

    pub fn leave_literal_scan(&self) {
        self.0.borrow_mut().leave_literal_scan()
    }

    pub fn current_location(&self) -> (Vec<u8>, usize) {
        self.0.borrow().current_location()
    }
}

pub struct Input {
    pub input: InputRead,
    pub pushback_buffer: Vec<u8>,
    pub line_number: usize,
    /// A `<newline>` has been consumed but not yet accounted for. See
    /// [`Input::get_next_character`].
    pending_newline: bool,
}

impl Input {
    pub fn new(input: InputRead) -> Self {
        Self {
            input,
            pushback_buffer: Vec::new(),
            line_number: 1,
            pending_newline: false,
        }
    }

    fn get_next_character(&mut self) -> std::io::Result<u8> {
        let mut buf: [u8; 1] = [0; 1];
        let n = match &mut self.input {
            InputRead::File { file, .. } => file.read(&mut buf),
            InputRead::Stdin(s) => s.read(&mut buf),
        }?;

        if n == 0 {
            return Ok(EOF);
        }

        let c = buf[0];

        // `line_number` names the line the character being returned sits on, so
        // a `<newline>` only advances it once the first character of the next
        // line is consumed. Advancing eagerly misattributes a diagnostic raised
        // by a macro call that ends at end-of-line to the following line, since
        // deciding whether the macro name is followed by `(` already consumes
        // the `<newline>`.
        if self.pending_newline {
            self.line_number += 1;
            self.pending_newline = false;
        }
        if c == b'\n' {
            self.pending_newline = true;
        }

        Ok(c)
    }
}

#[derive(Debug)]
pub enum InputRead {
    File { file: std::fs::File, path: PathBuf },
    Stdin(std::io::Stdin),
}
