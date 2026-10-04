//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The Windows console: the command channel (the console's input buffer in
//! raw mode, its screen buffer for the prompt), the window size, and putting
//! the console back however the process ends.
//!
//! Raw mode is the input buffer without line editing, echo or Ctrl-C
//! processing, delivering keys as the same VT sequences a Unix terminal
//! sends; the screen buffer interprets VT sequences for the output.

use std::ffi::c_void;
use std::fs::File;
use std::io::{self, Read, Write};
use std::os::windows::io::AsRawHandle;
use std::sync::atomic::{AtomicBool, AtomicU32, AtomicUsize, Ordering};

type Handle = *mut c_void;

#[repr(C)]
struct Coord {
    x: i16,
    y: i16,
}

#[repr(C)]
struct SmallRect {
    left: i16,
    top: i16,
    right: i16,
    bottom: i16,
}

#[repr(C)]
struct ConsoleScreenBufferInfo {
    size: Coord,
    cursor_position: Coord,
    attributes: u16,
    window: SmallRect,
    maximum_window_size: Coord,
}

extern "system" {
    fn GetConsoleMode(console: Handle, mode: *mut u32) -> i32;
    fn SetConsoleMode(console: Handle, mode: u32) -> i32;
    fn GetConsoleScreenBufferInfo(output: Handle, info: *mut ConsoleScreenBufferInfo) -> i32;
    fn SetConsoleCtrlHandler(handler: Option<extern "system" fn(u32) -> i32>, add: i32) -> i32;
    fn ReadConsoleW(
        input: Handle,
        buffer: *mut u16,
        to_read: u32,
        read: *mut u32,
        control: *mut c_void,
    ) -> i32;
    fn WriteConsoleW(
        output: Handle,
        buffer: *const u16,
        to_write: u32,
        written: *mut u32,
        reserved: *mut c_void,
    ) -> i32;
}

const ENABLE_PROCESSED_INPUT: u32 = 0x0001;
const ENABLE_LINE_INPUT: u32 = 0x0002;
const ENABLE_ECHO_INPUT: u32 = 0x0004;
const ENABLE_VIRTUAL_TERMINAL_INPUT: u32 = 0x0200;
const ENABLE_PROCESSED_OUTPUT: u32 = 0x0001;
const ENABLE_VIRTUAL_TERMINAL_PROCESSING: u32 = 0x0004;

/// Windows sends no signal for a resized window, so the size is fetched
/// again every time the pager looks; it redraws only when the size changed.
/// Nothing stops and resumes a Windows process.
pub fn take_pending() -> super::Pending {
    super::Pending {
        resumed: false,
        resized: true,
    }
}

/// The console window's size as (columns, rows), asked of standard output.
pub fn terminal_size() -> io::Result<(u16, u16)> {
    let out = io::stdout();
    // SAFETY: the info is plain data the call fills; the handle is valid for
    // the duration of the call.
    let mut info: ConsoleScreenBufferInfo = unsafe { std::mem::zeroed() };
    if unsafe { GetConsoleScreenBufferInfo(out.as_raw_handle(), &mut info) } == 0 {
        return Err(io::Error::last_os_error());
    }
    let w = &info.window;
    let columns = (w.right - w.left + 1).max(0) as u16;
    let rows = (w.bottom - w.top + 1).max(0) as u16;
    Ok((columns, rows))
}

/// The console mode of `file`.
fn console_mode(file: &File) -> io::Result<u32> {
    let mut mode = 0;
    // SAFETY: the handle is open for the duration of the call.
    if unsafe { GetConsoleMode(file.as_raw_handle(), &mut mode) } == 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(mode)
}

/// Give `file` the console mode `mode`.
fn set_console_mode(file: &File, mode: u32) -> io::Result<()> {
    // SAFETY: the handle is open for the duration of the call.
    if unsafe { SetConsoleMode(file.as_raw_handle(), mode) } == 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

// The modes to put back if the process is ended from outside: by Ctrl-Break,
// by closing the console, or by logoff or shutdown. The control handler runs
// on a thread of its own, so this is ordinary shared state, published before
// the handler is installed and withdrawn after it is removed.
static SAVED: AtomicBool = AtomicBool::new(false);
static SAVED_INPUT: AtomicUsize = AtomicUsize::new(0);
static SAVED_INPUT_MODE: AtomicU32 = AtomicU32::new(0);
static SAVED_OUTPUT: AtomicUsize = AtomicUsize::new(0);
static SAVED_OUTPUT_MODE: AtomicU32 = AtomicU32::new(0);

/// Restore the console, then let the next handler -- the system's, which
/// ends the process -- run.
extern "system" fn restore_on_exit(_ctrl_type: u32) -> i32 {
    if SAVED.load(Ordering::SeqCst) {
        // SAFETY: the handles stay open while SAVED is set.
        unsafe {
            SetConsoleMode(
                SAVED_INPUT.load(Ordering::SeqCst) as Handle,
                SAVED_INPUT_MODE.load(Ordering::SeqCst),
            );
            SetConsoleMode(
                SAVED_OUTPUT.load(Ordering::SeqCst) as Handle,
                SAVED_OUTPUT_MODE.load(Ordering::SeqCst),
            );
        }
    }
    0
}

/// The console's input buffer read as UTF-8.
///
/// `ReadConsoleW` rather than `ReadFile`: the bytes `ReadFile` returns are in
/// the console's code page, not UTF-8, and it returns nothing at all for
/// input that carries no characters, which a reader takes for end of input.
/// A console has no end of input, so a read waits until there are characters.
pub struct Reader {
    console: File,
    /// UTF-8 converted but not yet handed out.
    pending: Vec<u8>,
    /// A high surrogate whose pair has not arrived yet.
    high_surrogate: Option<u16>,
}

impl Reader {
    fn new(console: File) -> Self {
        Self {
            console,
            pending: Vec::new(),
            high_surrogate: None,
        }
    }

    /// Read characters from the console into `pending`.
    fn fill(&mut self) -> io::Result<()> {
        let mut units = [0u16; 64];
        while self.pending.is_empty() {
            let mut read = 0;
            // SAFETY: the buffer holds `units.len()` units and outlives the call.
            let ok = unsafe {
                ReadConsoleW(
                    self.console.as_raw_handle(),
                    units.as_mut_ptr(),
                    units.len() as u32,
                    &mut read,
                    std::ptr::null_mut(),
                )
            };
            if ok == 0 {
                return Err(io::Error::last_os_error());
            }
            let mut utf16: Vec<u16> = self.high_surrogate.take().into_iter().collect();
            utf16.extend_from_slice(&units[..read as usize]);
            if utf16.last().is_some_and(|&u| (0xd800..0xdc00).contains(&u)) {
                self.high_surrogate = utf16.pop();
            }
            self.pending
                .extend(String::from_utf16_lossy(&utf16).into_bytes());
        }
        Ok(())
    }
}

impl Read for Reader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        if buf.is_empty() {
            return Ok(0);
        }
        self.fill()?;
        let n = buf.len().min(self.pending.len());
        buf[..n].copy_from_slice(&self.pending[..n]);
        self.pending.drain(..n);
        Ok(n)
    }
}

/// The console's screen buffer written as UTF-8, through `WriteConsoleW`:
/// `WriteFile` would take the bytes in the console's code page.
pub struct Writer {
    console: File,
    /// The start of a UTF-8 character whose remaining bytes have not come.
    partial: Vec<u8>,
}

impl Writer {
    fn new(console: File) -> Self {
        Self {
            console,
            partial: Vec::new(),
        }
    }
}

impl Write for Writer {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let mut bytes = std::mem::take(&mut self.partial);
        bytes.extend_from_slice(buf);
        let text = match std::str::from_utf8(&bytes) {
            Ok(text) => text.to_owned(),
            // A character cut off at the end waits for the next write.
            Err(e) if e.error_len().is_none() => {
                self.partial = bytes.split_off(e.valid_up_to());
                String::from_utf8_lossy(&bytes).into_owned()
            }
            Err(_) => String::from_utf8_lossy(&bytes).into_owned(),
        };
        let units: Vec<u16> = text.encode_utf16().collect();
        let mut done = 0;
        while done < units.len() {
            let mut written = 0;
            // SAFETY: the slice is valid for the call.
            let ok = unsafe {
                WriteConsoleW(
                    self.console.as_raw_handle(),
                    units[done..].as_ptr(),
                    (units.len() - done) as u32,
                    &mut written,
                    std::ptr::null_mut(),
                )
            };
            if ok == 0 {
                return Err(io::Error::last_os_error());
            }
            done += written as usize;
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

/// The console's input buffer, in raw mode while this lives, and its screen
/// buffer, interpreting VT sequences; `Drop` restores both modes.
pub struct CommandIO {
    input: File,
    input_mode: u32,
    output: File,
    output_mode: u32,
}

impl CommandIO {
    /// Open the console as the command channel: POSIX reads commands from
    /// standard error or `/dev/tty`, and on Windows that terminal is the
    /// console, `CONIN$` and `CONOUT$`.
    ///
    /// Returns `(channel, reader, writer)` as on Unix.
    pub fn open() -> io::Result<(Self, Reader, Writer)> {
        // Setting a console mode takes write access as well as read access,
        // on the input buffer and the screen buffer alike.
        let input = File::options().read(true).write(true).open("CONIN$")?;
        let output = File::options().read(true).write(true).open("CONOUT$")?;
        let input_mode = console_mode(&input)?;
        let output_mode = console_mode(&output)?;

        SAVED_INPUT.store(input.as_raw_handle() as usize, Ordering::SeqCst);
        SAVED_INPUT_MODE.store(input_mode, Ordering::SeqCst);
        SAVED_OUTPUT.store(output.as_raw_handle() as usize, Ordering::SeqCst);
        SAVED_OUTPUT_MODE.store(output_mode, Ordering::SeqCst);
        SAVED.store(true, Ordering::SeqCst);
        // SAFETY: the handler only touches the atomics published above.
        unsafe { SetConsoleCtrlHandler(Some(restore_on_exit), 1) };

        let channel = Self {
            input,
            input_mode,
            output,
            output_mode,
        };
        let raw = (input_mode & !(ENABLE_PROCESSED_INPUT | ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT))
            | ENABLE_VIRTUAL_TERMINAL_INPUT;
        set_console_mode(&channel.input, raw)?;
        set_console_mode(
            &channel.output,
            output_mode | ENABLE_PROCESSED_OUTPUT | ENABLE_VIRTUAL_TERMINAL_PROCESSING,
        )?;

        let reader = Reader::new(channel.input.try_clone()?);
        let writer = Writer::new(channel.output.try_clone()?);
        Ok((channel, reader, writer))
    }
}

impl Drop for CommandIO {
    fn drop(&mut self) {
        let _ = set_console_mode(&self.input, self.input_mode);
        let _ = set_console_mode(&self.output, self.output_mode);
        // SAFETY: removes the handler installed by `open`.
        unsafe { SetConsoleCtrlHandler(Some(restore_on_exit), 0) };
        SAVED.store(false, Ordering::SeqCst);
    }
}
