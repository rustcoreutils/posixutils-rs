//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::{error::Result, input::InputStateRef, state::StackFrame};
use std::{
    cell::RefCell,
    collections::BTreeMap,
    io::{Seek, Write},
    rc::Rc,
};

/// Sentinel bytes wrapping a built-in macro name, emitted by `defn` for a
/// built-in so that `define`/`pushdef` can reconstruct that built-in under a new
/// name. When such a marker instead reaches real output (a built-in token used
/// outside define/pushdef), it expands to nothing, as in GNU m4.
pub(crate) const BUILTIN_DEFN_PREFIX: u8 = 0x01;
pub(crate) const BUILTIN_DEFN_SUFFIX: u8 = 0x02;

#[derive(Default)]
pub struct OutputState {
    pub output: OutputRef,
    pub stack: Vec<StackFrame>,
    /// Tracks a `defn`-of-built-in marker spanning multiple writes so its bytes
    /// can be dropped from real output.
    pub skipping_builtin_marker: bool,
}

impl OutputState {
    /// Write either to output, or to the buffer for the macro arg currently being parsed.
    pub fn write_all(&mut self, buf: &[u8]) -> crate::Result<()> {
        if self.stack.is_empty() {
            log::trace!("Writing to output: {}", String::from_utf8_lossy(buf));
            // A built-in token (defn marker) that is not consumed by
            // define/pushdef vanishes rather than emitting control bytes.
            let clean = self.filter_builtin_markers(buf);
            if !clean.is_empty() {
                self.output.write_all(&clean)?;
            }
        } else {
            log::trace!(
                "Writing to macro arg in stack: {:?}",
                String::from_utf8_lossy(buf)
            );

            let frame = self.stack.last_mut().expect("Stack not empty");
            let arg_buffer = if let Some(arg_buffer) = frame.args.last_mut() {
                arg_buffer
            } else {
                frame.args.push(Vec::new());
                frame.args.last_mut().expect("At least one arg")
            };

            arg_buffer.extend(buf);
        }
        Ok(())
    }

    /// Drop any `defn`-of-built-in marker byte ranges, carrying the in-marker
    /// state across calls (a marker may be split over multiple writes).
    fn filter_builtin_markers(&mut self, buf: &[u8]) -> Vec<u8> {
        let mut clean = Vec::with_capacity(buf.len());
        for &b in buf {
            if self.skipping_builtin_marker {
                if b == BUILTIN_DEFN_SUFFIX {
                    self.skipping_builtin_marker = false;
                }
            } else if b == BUILTIN_DEFN_PREFIX {
                self.skipping_builtin_marker = true;
            } else {
                clean.push(b);
            }
        }
        clean
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
        let mut buffer = buffer.borrow_mut();
        buffer.0.rewind()?;
        let n = std::io::copy(&mut buffer.0, self)?;
        log::debug!("Output::undivert({buffer_number:?}): Undiverted {n} bytes.");
        buffer.0.get_mut().clear();
        debug_assert!(buffer.0.get_ref().is_empty());
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
    /// Write `buf` to `output`, inserting `#line` directives after each newline
    /// when line synchronization is enabled.
    fn write_synced(&mut self, output: &mut dyn Write, buf: &[u8]) -> std::io::Result<usize> {
        if self.input.sync_lines() {
            let mut n = 0;
            for c in buf {
                n += output.write(&[*c])?;
                if *c == b'\n' {
                    self.input.emit_syncline(output, true)?;
                }
            }
            Ok(n)
        } else {
            output.write(buf)
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
