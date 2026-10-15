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

use crate::{macros::BuiltinMacro, EOF};

/// An input's name and a line number in it, as diagnostics print them.
pub type Location = (Rc<[u8]>, usize);

/// Sentinel for [`InputState::output_current_line`] meaning "the input file
/// changed": the next directive emitted must carry the file name.
const SYNCLINE_FILE_CHANGED: i64 = -1;

/// What a built-in token reads as. See [`InputState::get_next_character`].
pub const BUILTIN_TOKEN: u8 = 0x01;

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
    /// The input line of the character read last: its file line, or the
    /// line of the pushed-back text it came from.
    last_line: usize,
    /// The file of the character read last, when it is not the current
    /// input's own: text `m4wrap` saved in another file.
    last_file: Option<Rc<[u8]>>,
    /// While a macro runs, where its call began, which the text it pushes
    /// back is attributed to.
    expansion_site: Option<Location>,
    /// The built-in token the last character read stood for.
    last_builtin: Option<BuiltinMacro>,
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
            last_line: 0,
            last_file: None,
            expansion_site: None,
            last_builtin: None,
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
        self.last_line = 1;
        self.last_file = None;
    }

    pub fn input_pop(&mut self) -> Option<Input> {
        let popped = self.input.pop();
        self.last_line = self.input.last().map_or(0, |i| i.line_number);
        self.last_file = None;
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

        let line = self.current_line_number() as i64;
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
                    if file_changed {
                        // Only now is the path worth building.
                        let (name, _) = self.current_location();
                        if !name.is_empty() {
                            output.write_all(b" \"")?;
                            output.write_all(&name)?;
                            output.write_all(b"\"")?;
                        }
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

    /// Get the next character to be parsed: from the text pushed back onto
    /// the current input, else from the input itself.
    ///
    /// A built-in token (see [`InputState::pushback_builtin`]) reads as
    /// [`BUILTIN_TOKEN`], and [`InputState::take_builtin`] then says which
    /// built-in it is; a plain byte of that value says none.
    pub fn get_next_character(&mut self) -> crate::error::Result<u8> {
        self.last_builtin = None;
        let input = self.input.last_mut().unwrap();
        // An exhausted segment is dropped only here, when reading past it,
        // so that a character read from it and pushed back returns to it.
        while let Some(segment) = input.pushback.last_mut() {
            let builtin = segment.builtin.take();
            let c = match builtin {
                Some(_) => Some(BUILTIN_TOKEN),
                None => segment.text.get(segment.pos).copied(),
            };
            if let Some(c) = c {
                if builtin.is_none() {
                    segment.pos += 1;
                }
                self.last_line = segment.line;
                // Only text from another file (m4wrap's) names one.
                if segment.file.is_some() || self.last_file.is_some() {
                    self.last_file.clone_from(&segment.file);
                }
                self.last_builtin = builtin;
                return Ok(c);
            }
            input.pushback.pop();
        }
        let c = input.get_next_character()?;
        self.last_line = input.line_number;
        self.last_file = None;
        Ok(c)
    }

    /// Append to `out` the bytes that come next, up to the first for which
    /// `stop` is true, which is left unread; the run also ends where a piece
    /// of pushed-back text or a buffer of the input does, so it can be
    /// short. Returns how many bytes it took.
    ///
    /// For runs of plain text, which the main loop would otherwise read and
    /// write a byte at a time.
    pub fn take_run(
        &mut self,
        stop: impl Fn(u8) -> bool,
        out: &mut Vec<u8>,
    ) -> crate::error::Result<usize> {
        let input = self.input.last_mut().unwrap();
        while let Some(segment) = input.pushback.last_mut() {
            if segment.builtin.is_some() {
                return Ok(0);
            }
            let rest = &segment.text[segment.pos..];
            if rest.is_empty() {
                input.pushback.pop();
                continue;
            }
            let n = rest.iter().position(|&c| stop(c)).unwrap_or(rest.len());
            out.extend_from_slice(&rest[..n]);
            segment.pos += n;
            if n > 0 {
                self.last_line = segment.line;
                if segment.file.is_some() || self.last_file.is_some() {
                    self.last_file.clone_from(&segment.file);
                }
            }
            return Ok(n);
        }
        let n = input.take_run(stop, out)?;
        if n > 0 {
            self.last_line = input.line_number;
            self.last_file = None;
        }
        Ok(n)
    }

    /// The built-in token just read, if the last character read was one.
    pub fn take_builtin(&mut self) -> Option<BuiltinMacro> {
        self.last_builtin.take()
    }

    /// Push back a character just read, to be read again next.
    pub fn pushback_character(&mut self, c: u8) {
        if c == BUILTIN_TOKEN {
            if let Some(builtin) = self.last_builtin.take() {
                self.push_segment(Vec::new(), Some(builtin), None);
                return;
            }
        }
        let input = self.input.last_mut().unwrap();
        match input.pushback.last_mut() {
            // Unread it from the text it came from.
            Some(segment)
                if segment.builtin.is_none()
                    && segment.pos > 0
                    && segment.text[segment.pos - 1] == c =>
            {
                segment.pos -= 1
            }
            _ => {
                let site = Some((self.last_file.clone(), self.last_line));
                self.push_segment(vec![c], None, site);
            }
        }
    }

    /// Push back text to be read next: a macro's expansion. As in GNU m4,
    /// the text keeps the location of the call that produced it (see
    /// [`InputState::set_expansion_site`]), and macros called from it report
    /// that location.
    pub fn pushback_string(&mut self, s: &[u8]) {
        if !s.is_empty() {
            self.push_segment(s.to_vec(), None, None);
        }
    }

    /// [`InputState::pushback_string`], taking the text.
    pub fn pushback_vec(&mut self, s: Vec<u8>) {
        if !s.is_empty() {
            self.push_segment(s, None, None);
        }
    }

    /// Push back a built-in token: what `defn` of a built-in expands to. It
    /// is not text: as an argument of `define` or `pushdef` it makes that
    /// built-in, anywhere else it is nothing.
    pub fn pushback_builtin(&mut self, builtin: BuiltinMacro) {
        self.push_segment(Vec::new(), Some(builtin), None);
    }

    /// Push back text attributed to `location`, as `m4wrap` text keeps the
    /// location of its `m4wrap` call.
    pub fn pushback_string_at(&mut self, s: &[u8], location: &Location) {
        if !s.is_empty() {
            let site = Some((Some(location.0.clone()), location.1));
            self.push_segment(s.to_vec(), None, site);
        }
    }

    /// Push a segment of `text`, or a built-in token, attributed to `site`
    /// (a file other than the input's own, and a line), else to the call
    /// being expanded, else to where reading is.
    fn push_segment(
        &mut self,
        text: Vec<u8>,
        builtin: Option<BuiltinMacro>,
        site: Option<(Option<Rc<[u8]>>, usize)>,
    ) {
        let (file, line) = site
            .or_else(|| {
                self.expansion_site
                    .as_ref()
                    .map(|(file, line)| (Some(file.clone()), *line))
            })
            .unwrap_or_else(|| (self.last_file.clone(), self.last_line));
        let input = self.input.last_mut().unwrap();
        // The input's own name is the default; record only another one.
        let file = file.filter(|f| !Rc::ptr_eq(f, &input.name) && **f != *input.name);
        input.pushback.push(Segment {
            text,
            pos: 0,
            line,
            file,
            builtin,
        });
    }

    /// Fetch new characters attempting to match them all to token. If any character doesn't match
    /// the token, then all the characters previously fetched are placed onto the pushback buffer.
    ///
    /// **NOTE:** Will panic if token is not length of at least 1.
    ///
    /// * `c` - First character of input which is already available.
    /// * `token` - Token to match against.
    pub fn look_ahead(&mut self, mut c: u8, token: &[u8]) -> crate::error::Result<bool> {
        // An empty token (quoting or comments disabled) matches nothing.
        if c == EOF || token.first() != Some(&c) {
            return Ok(false);
        }

        let mut i = 1;
        while i < token.len() {
            c = self.get_next_character()?;
            if c == EOF || c != token[i] {
                // Unread what matched so far, after the first character,
                // which the caller still holds, and the one that did not.
                if c != EOF {
                    self.pushback_character(c);
                }
                for &matched in token[1..i].iter().rev() {
                    self.pushback_character(matched);
                }
                return Ok(false);
            }

            i += 1;
        }

        Ok(true)
    }

    /// Where the next pushed-back text will go: the depth of the input
    /// stack and the number of pushed-back segments on its top input.
    pub fn pushback_position(&self) -> (usize, usize) {
        let len = self.input.last().map_or(0, |i| i.pushback.len());
        (self.input.len(), len)
    }

    /// The text pushed back since [`InputState::pushback_position`] gave
    /// `(depth, len)`, in reading order; empty if a new input was entered.
    pub fn pushed_since(&self, depth: usize, len: usize) -> Vec<u8> {
        match self.input.last() {
            Some(input) if self.input.len() == depth && input.pushback.len() >= len => input
                .pushback[len..]
                .iter()
                .rev()
                .flat_map(|segment| segment.text[segment.pos..].iter().copied())
                .collect(),
            _ => Vec::new(),
        }
    }

    /// The current line number of the active input, without the allocation
    /// [`InputState::current_location`] makes for the name.
    ///
    /// `write_synced` consults this for every byte it writes, and the main loop
    /// writes plain text a byte at a time, so building a `Vec<u8>` of the path
    /// there cost an allocation per output byte under `-s`. The name is only
    /// needed on the rare path that actually emits a directive.
    pub fn current_line_number(&self) -> usize {
        self.input.last().map(|i| i.line_number).unwrap_or(0)
    }

    /// The name and current line number of the active input, for diagnostics
    /// (`stdin` when reading standard input). Used to format GNU-style
    /// `m4:<file>:<line>:` messages.
    pub fn current_location(&self) -> Location {
        match self.input.last() {
            Some(input) => (
                self.last_file.clone().unwrap_or_else(|| input.name.clone()),
                self.last_line,
            ),
            None => (b"stdin".as_slice().into(), 0),
        }
    }
}

#[derive(Clone, Default)]
pub struct InputStateRef(Rc<RefCell<InputState>>);

impl InputStateRef {
    pub fn new(input_state: InputState) -> Self {
        Self(Rc::new(RefCell::new(input_state)))
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

    pub fn current_location(&self) -> Location {
        self.0.borrow().current_location()
    }

    pub fn pushback_position(&self) -> (usize, usize) {
        self.0.borrow().pushback_position()
    }

    /// Attribute text pushed back from now on to `site`, the location of the
    /// call being expanded. Returns the previous setting.
    pub fn set_expansion_site(&self, site: Option<Location>) -> Option<Location> {
        std::mem::replace(&mut self.0.borrow_mut().expansion_site, site)
    }

    pub fn pushback_string_at(&self, s: &[u8], location: &Location) {
        self.0.borrow_mut().pushback_string_at(s, location)
    }

    pub fn take_run(
        &self,
        stop: impl Fn(u8) -> bool,
        out: &mut Vec<u8>,
    ) -> crate::error::Result<usize> {
        self.0.borrow_mut().take_run(stop, out)
    }

    pub fn pushback_vec(&self, s: Vec<u8>) {
        self.0.borrow_mut().pushback_vec(s)
    }

    /// Whether `-s` is on: output must then be written as it is read, a
    /// byte at a time, for its `#line` directives to be right.
    pub fn line_synchronization(&self) -> bool {
        self.0.borrow().line_synchronization
    }

    pub fn take_builtin(&self) -> Option<BuiltinMacro> {
        self.0.borrow_mut().take_builtin()
    }

    pub fn pushback_builtin(&self, builtin: BuiltinMacro) {
        self.0.borrow_mut().pushback_builtin(builtin)
    }

    pub fn pushed_since(&self, depth: usize, len: usize) -> Vec<u8> {
        self.0.borrow().pushed_since(depth, len)
    }
}

/// Text pushed back onto an input, read from `pos` on, with the input line
/// it is attributed to.
struct Segment {
    text: Vec<u8>,
    /// How much of `text` has been read.
    pos: usize,
    line: usize,
    /// The file the text is attributed to, when not the input's own.
    file: Option<Rc<[u8]>>,
    /// A built-in token, read before `text`, which is then empty.
    builtin: Option<BuiltinMacro>,
}

pub struct Input {
    pub input: InputRead,
    /// The name diagnostics and `__file__` give this input: its path as
    /// given, or `stdin`. Shared, because every macro call records it.
    pub name: Rc<[u8]>,
    /// Text pushed back onto this input, read before the input itself:
    /// the top segment first.
    pushback: Vec<Segment>,
    /// Bytes read from the input and not yet consumed: from `buffer_pos`.
    buffer: Vec<u8>,
    buffer_pos: usize,
    pub line_number: usize,
    /// A `<newline>` has been consumed but not yet accounted for. See
    /// [`Input::get_next_character`].
    pending_newline: bool,
}

/// How much of an input is read at a time.
const READ_SIZE: usize = 64 * 1024;

impl Input {
    pub fn new(input: InputRead) -> Self {
        let name: Rc<[u8]> = match &input {
            InputRead::File { path, .. } => path.as_os_str().as_encoded_bytes().into(),
            InputRead::Stdin(_) => b"stdin".as_slice().into(),
        };
        Self {
            input,
            name,
            pushback: Vec::new(),
            buffer: Vec::new(),
            buffer_pos: 0,
            line_number: 1,
            pending_newline: false,
        }
    }

    /// Make sure there is an unconsumed byte in the buffer; `false` at end
    /// of input. A read takes what is available, so a terminal gives a line
    /// at a time.
    fn fill(&mut self) -> std::io::Result<bool> {
        if self.buffer_pos < self.buffer.len() {
            return Ok(true);
        }
        self.buffer.resize(READ_SIZE, 0);
        self.buffer_pos = 0;
        let n = loop {
            let result = match &mut self.input {
                InputRead::File { file, .. } => file.read(&mut self.buffer),
                InputRead::Stdin(s) => s.read(&mut self.buffer),
            };
            match result {
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
                other => break other,
            }
        };
        let n = match n {
            Ok(n) => n,
            Err(e) => {
                self.buffer.clear();
                return Err(e);
            }
        };
        self.buffer.truncate(n);
        Ok(n > 0)
    }

    /// Account for consumed bytes `consumed` in `line_number`.
    ///
    /// `line_number` names the line the character last returned sits on, so
    /// a `<newline>` only advances it once the first character of the next
    /// line is consumed. Advancing eagerly misattributes a diagnostic raised
    /// by a macro call that ends at end-of-line to the following line, since
    /// deciding whether the macro name is followed by `(` already consumes
    /// the `<newline>`.
    fn count_lines(&mut self, consumed: &[u8]) {
        let Some((&last, before)) = consumed.split_last() else {
            return;
        };
        if self.pending_newline {
            self.line_number += 1;
        }
        self.line_number += before.iter().filter(|&&c| c == b'\n').count();
        self.pending_newline = last == b'\n';
    }

    fn get_next_character(&mut self) -> std::io::Result<u8> {
        if !self.fill()? {
            return Ok(EOF);
        }
        let c = self.buffer[self.buffer_pos];
        self.buffer_pos += 1;
        self.count_lines(&[c]);
        Ok(c)
    }

    /// See [`InputState::take_run`].
    fn take_run(&mut self, stop: impl Fn(u8) -> bool, out: &mut Vec<u8>) -> std::io::Result<usize> {
        if !self.fill()? {
            return Ok(0);
        }
        let start = self.buffer_pos;
        let rest = &self.buffer[start..];
        let n = rest.iter().position(|&c| stop(c)).unwrap_or(rest.len());
        out.extend_from_slice(&rest[..n]);
        self.buffer_pos += n;
        let consumed = std::mem::take(&mut self.buffer);
        self.count_lines(&consumed[start..start + n]);
        self.buffer = consumed;
        Ok(n)
    }
}

#[derive(Debug)]
pub enum InputRead {
    File { file: std::fs::File, path: PathBuf },
    Stdin(std::io::Stdin),
}
