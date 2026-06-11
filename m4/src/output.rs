use crate::{error::Result, input::InputStateRef, state::StackFrame};
use std::{
    cell::RefCell,
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
    /// Divert buffers 1 through to 9. See [`DivertMacro`].
    ///
    /// NOTE: [`Rc`] and [`RefCell`] required because buffers can be diverted into each other via
    /// [`Output`].
    divert_buffers: [Rc<RefCell<DivertableBuffer>>; 9],
    /// See [`DivertMacro`].
    divert_number: i64,
    /// The real output, usually [`std::io::stdout`].
    stdout: Rc<RefCell<dyn Write>>,
    input: InputStateRef,
}

impl Default for Output {
    fn default() -> Self {
        Self {
            divert_buffers: Default::default(),
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
            divert_buffers: Default::default(),
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
        DivertBufferNumber::try_from(usize::try_from(self.divert_number).ok()?).ok()
    }

    pub fn divert(&mut self, divert_number: i64) -> Result<()> {
        if divert_number > 9 {
            return Err(crate::error::Error::new(
                crate::ErrorKind::InvalidDivertNumber(divert_number),
            ));
        }
        self.divert_number = divert_number;
        Ok(())
    }

    pub fn undivert_all(&mut self) -> Result<()> {
        for buffer_number in (1..=self.divert_buffers.len()).map(DivertBufferNumber::try_from) {
            self.undivert(buffer_number?)?;
        }
        Ok(())
    }

    pub fn undivert(&mut self, buffer_number: DivertBufferNumber) -> Result<()> {
        if self.divert_buffer_number() == Some(buffer_number) {
            // It would cause a panic anyway on buffer.borrow_mut()
            log::warn!("Skipping recursive divert");
            return Ok(());
        }
        let buffer = self.divert_buffers[buffer_number.index()].clone();
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

impl DivertBufferNumber {
    fn index(&self) -> usize {
        self.0 - 1
    }
}

impl std::fmt::Display for DivertBufferNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl TryFrom<usize> for DivertBufferNumber {
    type Error = crate::Error;

    fn try_from(value: usize) -> std::prelude::v1::Result<Self, Self::Error> {
        if !(1..=9).contains(&value) {
            return Err(
                crate::Error::new(crate::ErrorKind::Parsing).add_context(format!(
                    "Unexpected buffer number: {value}. Needs to be from 1 to 9"
                )),
            );
        }
        Ok(Self(value))
    }
}

impl Write for Output {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let output: &mut dyn Write = match self.divert_number {
            0 => &mut *self.stdout.borrow_mut(),
            1..=9 => {
                &mut self.divert_buffers[self
                    .divert_buffer_number()
                    .expect("valid divert buffer number")
                    .index()]
                .borrow_mut()
                .0
            }
            i if i < 0 => return Ok(buf.len()),
            _ => unreachable!("unreachable, was checked in Self::divert()"),
        };

        log::debug!(
            "writing[{}] {:?}",
            self.divert_number,
            String::from_utf8_lossy(buf)
        );

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

    fn flush(&mut self) -> std::io::Result<()> {
        match self.divert_number {
            0 => self.stdout.borrow_mut().flush(),
            1..=9 => self.divert_buffers[self
                .divert_buffer_number()
                .expect("valid divert buffer number")
                .index()]
            .borrow_mut()
            .0
            .flush(),
            i if i < 0 => Ok(()),
            _ => unreachable!("unreachable, was checked in Self::divert()"),
        }
    }
}

#[derive(Default, Clone)]
pub(crate) struct DivertableBuffer(std::io::Cursor<Vec<u8>>);
