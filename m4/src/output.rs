//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::{error::Result, input::InputStateRef, macros::BuiltinMacro, state::StackFrame};
use std::{
    cell::RefCell,
    collections::BTreeMap,
    io::{Seek, Write},
    rc::Rc,
};

#[derive(Default)]
pub struct OutputState {
    pub output: OutputRef,
    pub stack: Vec<StackFrame>,
}

impl OutputState {
    /// Write either to output, or to the buffer for the macro arg currently being parsed.
    pub fn write_all(&mut self, buf: &[u8]) -> crate::Result<()> {
        match self.stack.last_mut() {
            None => self.output.write_all(buf)?,
            Some(frame) => {
                if frame.args.is_empty() {
                    frame.args.push(Vec::new());
                }
                frame
                    .args
                    .last_mut()
                    .expect("at least one argument")
                    .extend_from_slice(buf);
            }
        }
        Ok(())
    }

    /// Take a built-in token (what `defn` of a built-in expands to) where
    /// text would go. As in GNU m4, it becomes the argument being collected
    /// if nothing has been collected for it yet, and is otherwise dropped,
    /// as it is outside any argument.
    pub fn write_builtin(&mut self, builtin: BuiltinMacro) {
        let Some(frame) = self.stack.last_mut() else {
            return;
        };
        if frame.args.is_empty() {
            frame.args.push(Vec::new());
        }
        let index = frame.args.len() - 1;
        if frame.args[index].is_empty() && frame.builtin_arg(index).is_none() {
            frame.builtin_args.push((index, builtin));
        }
    }
}

/// A reference counted reference to [`Output`] which can be cloned.
#[derive(Clone, Default)]
pub(crate) struct OutputRef(Rc<RefCell<Output>>);

impl OutputRef {
    pub fn divert_number(&self) -> i64 {
        (*self.0).borrow().divert_number()
    }

    pub fn divert(&mut self, divert_number: i64) -> Result<()> {
        self.0.borrow_mut().divert(divert_number)
    }

    pub fn undivert_all(&mut self) -> Result<()> {
        self.0.borrow_mut().undivert_all()
    }

    pub fn undivert(&mut self, buffer_number: DivertBufferNumber) -> Result<()> {
        self.0.borrow_mut().undivert(buffer_number)
    }

    pub fn stdout(&self) -> Rc<RefCell<dyn Write>> {
        self.0.borrow().stdout()
    }
}

impl Write for OutputRef {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.borrow_mut().write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.borrow_mut().flush()
    }
}

/// Output that implements [`Write`] and will write to stdout if [`Output::divert_number`] is 0, to
/// one of the [`Output::divert_buffers`] if [`Output::divert_number`] is greater than 0, and will
/// discard if it is < 0.
///
/// NOTE: This currently uses an in-memory set of divert buffers, other implementations use
/// temporary files, so this might change in the future, or become an optional feature.
pub struct Output {
    /// Diversion buffers, created on demand and keyed by a positive diversion
    /// number. POSIX mandates buffers 1–9; larger numbers are
    /// implementation-defined and supported here (matching GNU m4). The
    /// [`BTreeMap`] keeps `undivert`-all in numerical order.
    ///
    /// NOTE: [`Rc`] and [`RefCell`] required because buffers can be diverted into each other via
    /// [`Output`].
    divert_buffers: BTreeMap<usize, Rc<RefCell<DivertableBuffer>>>,
    /// See [`DivertMacro`].
    divert_number: i64,
    /// The real output, usually [`std::io::stdout`].
    stdout: Rc<RefCell<dyn Write>>,
    input: InputStateRef,
}

impl Default for Output {
    fn default() -> Self {
        Self {
            divert_buffers: BTreeMap::new(),
            divert_number: Default::default(),
            stdout: Rc::new(RefCell::new(std::io::stdout())),
            input: InputStateRef::default(),
        }
    }
}

impl Output {
    pub fn new(stdout: Rc<RefCell<dyn Write>>, input: InputStateRef) -> Self {
        Self {
            stdout,
            input,
            divert_buffers: BTreeMap::new(),
            divert_number: Default::default(),
        }
    }

    pub fn stdout(&self) -> Rc<RefCell<dyn Write>> {
        self.stdout.clone()
    }

    pub fn into_ref(self) -> OutputRef {
        OutputRef(Rc::new(RefCell::new(self)))
    }

    pub fn divert_number(&self) -> i64 {
        self.divert_number
    }

    pub fn divert_buffer_number(&self) -> Option<DivertBufferNumber> {
        usize::try_from(self.divert_number)
            .ok()
            .filter(|n| *n >= 1)
            .map(DivertBufferNumber)
    }

    pub fn divert(&mut self, divert_number: i64) -> Result<()> {
        // Any positive buffer number is accepted (POSIX requires 1–9 and leaves
        // larger numbers implementation-defined; GNU m4 supports them).
        if self.divert_number != divert_number {
            self.input.invalidate_syncline_position();
        }
        self.divert_number = divert_number;
        Ok(())
    }

    pub fn undivert_all(&mut self) -> Result<()> {
        let buffer_numbers: Vec<usize> = self.divert_buffers.keys().copied().collect();
        for n in buffer_numbers {
            self.undivert(DivertBufferNumber(n))?;
        }
        Ok(())
    }

    pub fn undivert(&mut self, buffer_number: DivertBufferNumber) -> Result<()> {
        if self.divert_buffer_number() == Some(buffer_number) {
            // It would cause a panic anyway on buffer.borrow_mut()
            log::warn!("Skipping recursive divert");
            return Ok(());
        }
        let buffer = match self.divert_buffers.get(&buffer_number.0) {
            Some(buffer) => buffer.clone(),
            // Nothing was ever diverted to this buffer.
            None => return Ok(()),
        };
        let text = {
            let mut buffer = buffer.borrow_mut();
            let text = std::mem::take(buffer.0.get_mut());
            buffer.0.rewind()?;
            text
        };
        // An existing-but-empty diversion splices nothing, so nothing about the
        // output's position changed. Falling through would call
        // `resume_after_raw_output(true)` — because `None != Some('\n')` — and
        // claim the output had ended mid-line, which swallowed the next `-s`
        // directive entirely. `undivert_all()` at end of input re-visits every
        // buffer key, including ones an earlier `undivert` already emptied, so
        // this fires without the user writing a second `undivert` at all.
        if text.is_empty() {
            return Ok(());
        }

        let n = text.len();
        // Splice the buffer verbatim. Its `#line` directives were already
        // written when the text was diverted, so re-running it through
        // `write_synced` would both duplicate them and renumber the copy
        // against the wrong stream.
        self.write_raw(&text)?;
        // What follows resumes at an input line unrelated to where the
        // diverted text came from.
        self.input
            .resume_after_raw_output(text.last() != Some(&b'\n'));
        log::debug!("Output::undivert({buffer_number:?}): Undiverted {n} bytes.");
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct DivertBufferNumber(usize);

impl std::fmt::Display for DivertBufferNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl TryFrom<usize> for DivertBufferNumber {
    type Error = crate::Error;

    fn try_from(value: usize) -> std::prelude::v1::Result<Self, Self::Error> {
        // A diversion buffer is any positive number (0 selects normal output,
        // negative discards); both are handled by the caller, not here.
        if value < 1 {
            return Err(
                crate::Error::new(crate::ErrorKind::Parsing).add_context(format!(
                    "Unexpected buffer number: {value}. Needs to be 1 or greater"
                )),
            );
        }
        Ok(Self(value))
    }
}

impl Output {
    /// Write `buf` to `output`, letting the input state insert `#line`
    /// directives when line synchronization is enabled. See
    /// [`crate::input::InputState::write_synced`].
    fn write_synced(&mut self, output: &mut dyn Write, buf: &[u8]) -> std::io::Result<usize> {
        self.input.write_synced(output, buf)
    }

    /// Write `buf` to the current diversion target without inserting any `#line`
    /// directives. Used by `undivert`, whose text already carries its own.
    fn write_raw(&mut self, buf: &[u8]) -> std::io::Result<()> {
        match self.diversion_key() {
            // The `let` bindings keep the `Rc` alive for as long as the borrow
            // taken from it; returning the expression directly would drop it
            // while the guard is still live.
            DiversionTarget::Stdout => {
                let stdout = self.stdout.clone();
                let result = stdout.borrow_mut().write_all(buf);
                result
            }
            DiversionTarget::Discard => Ok(()),
            DiversionTarget::Buffer(key) => {
                let buffer = self.divert_buffers.entry(key).or_default().clone();
                let result = buffer.borrow_mut().0.write_all(buf);
                result
            }
        }
    }
}

impl Write for Output {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        log::debug!(
            "writing[{}] {:?}",
            self.divert_number,
            String::from_utf8_lossy(buf)
        );

        match self.diversion_key() {
            // Normal output.
            DiversionTarget::Stdout => {
                let stdout = self.stdout.clone();
                let mut out = stdout.borrow_mut();
                self.write_synced(&mut *out, buf)
            }
            // Negative diversion (or an out-of-range buffer number): discard.
            DiversionTarget::Discard => Ok(buf.len()),
            // Positive diversion: create the buffer on first use.
            DiversionTarget::Buffer(key) => {
                let buffer = self.divert_buffers.entry(key).or_default().clone();
                let mut buffer = buffer.borrow_mut();
                self.write_synced(&mut buffer.0, buf)
            }
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self.diversion_key() {
            DiversionTarget::Stdout => self.stdout.borrow_mut().flush(),
            DiversionTarget::Discard => Ok(()),
            DiversionTarget::Buffer(key) => match self.divert_buffers.get(&key) {
                Some(buffer) => buffer.borrow_mut().0.flush(),
                None => Ok(()),
            },
        }
    }
}

/// Where output for the current diversion number is directed.
enum DiversionTarget {
    Stdout,
    Discard,
    Buffer(usize),
}

impl Output {
    /// Resolve the current `divert_number` to an output target. A positive number
    /// is a buffer key; 0 is normal output; negative (or a value too large for
    /// `usize`, which cannot happen on 64-bit but is handled for portability) is
    /// discarded.
    fn diversion_key(&self) -> DiversionTarget {
        match self.divert_number {
            0 => DiversionTarget::Stdout,
            n if n < 0 => DiversionTarget::Discard,
            n => match usize::try_from(n) {
                Ok(key) => DiversionTarget::Buffer(key),
                Err(_) => DiversionTarget::Discard,
            },
        }
    }
}

#[derive(Default, Clone)]
pub(crate) struct DivertableBuffer(std::io::Cursor<Vec<u8>>);
