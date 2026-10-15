//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU m4's debug output: the flags `-d` and `debugmode` set, and the stream
//! `--debugfile` and `debugfile` choose, which receives traces and `dumpdef`.

use std::fs::{File, OpenOptions};
use std::io::{BufWriter, Write};
use std::path::Path;

/// `a`: show a traced call's arguments.
pub const SHOW_ARGS: u16 = 1 << 0;
/// `c`: trace before collecting arguments and after the call too.
pub const SHOW_CALL: u16 = 1 << 1;
/// `e`: show a traced call's expansion.
pub const SHOW_EXPANSION: u16 = 1 << 2;
/// `f`: name the input file in a trace.
pub const SHOW_FILE: u16 = 1 << 3;
/// `i`: report input file changes.
pub const SHOW_INPUT: u16 = 1 << 4;
/// `l`: give the input line in a trace.
pub const SHOW_LINE: u16 = 1 << 5;
/// `p`: report path searches.
pub const SHOW_PATH: u16 = 1 << 6;
/// `q`: quote arguments, expansions and `dumpdef` text.
pub const QUOTE: u16 = 1 << 7;
/// `t`: trace every macro call.
pub const TRACE_ALL: u16 = 1 << 8;
/// `x`: number each traced call.
pub const CALL_ID: u16 = 1 << 9;

/// What `-d` with no flags, or `debugmode` with an empty one, means.
pub const DEFAULT: u16 = SHOW_ARGS | SHOW_EXPANSION | QUOTE;

/// Parse a string of debug flags, GNU m4's letters, `V` for all of them.
/// An empty string is [`DEFAULT`]. `None` for a letter that is not a flag.
pub fn parse_flags(flags: &[u8]) -> Option<u16> {
    if flags.is_empty() {
        return Some(DEFAULT);
    }
    flags.iter().try_fold(0, |level, c| {
        let flag = match c {
            b'a' => SHOW_ARGS,
            b'c' => SHOW_CALL,
            b'e' => SHOW_EXPANSION,
            b'f' => SHOW_FILE,
            b'i' => SHOW_INPUT,
            b'l' => SHOW_LINE,
            b'p' => SHOW_PATH,
            b'q' => QUOTE,
            b't' => TRACE_ALL,
            b'x' => CALL_ID,
            b'V' => u16::MAX,
            _ => return None,
        };
        Some(level | flag)
    })
}

/// Where debug output goes.
#[derive(Default)]
pub enum Sink {
    /// Standard error, the default.
    #[default]
    Stderr,
    /// A file, appended to, as GNU m4 does.
    File(BufWriter<File>),
    /// Nowhere: `--debugfile=` or `debugfile()` with an empty name.
    Discard,
}

impl Sink {
    /// The sink a `--debugfile`/`debugfile` argument names: nowhere for the
    /// empty string, else the file, opened for appending.
    pub fn open(name: &[u8]) -> std::io::Result<Self> {
        if name.is_empty() {
            return Ok(Sink::Discard);
        }
        use std::os::unix::ffi::OsStrExt;
        let path = Path::new(std::ffi::OsStr::from_bytes(name));
        let file = OpenOptions::new().create(true).append(true).open(path)?;
        Ok(Sink::File(BufWriter::new(file)))
    }
}

/// The debug state.
#[derive(Default)]
pub struct Debug {
    pub flags: u16,
    pub sink: Sink,
}

impl Debug {
    /// Whether flag `flag` is set.
    pub fn has(&self, flag: u16) -> bool {
        self.flags & flag != 0
    }

    /// Write `bytes` to the debug stream; `stderr` is that stream unless a
    /// file was chosen.
    pub fn write(&mut self, stderr: &mut dyn Write, bytes: &[u8]) -> std::io::Result<()> {
        match &mut self.sink {
            Sink::Stderr => stderr.write_all(bytes),
            Sink::File(file) => file.write_all(bytes),
            Sink::Discard => Ok(()),
        }
    }

    /// Flush a debug file, before it is replaced, before a child process
    /// that may read it runs, and at exit.
    pub fn flush(&mut self) -> std::io::Result<()> {
        match &mut self.sink {
            Sink::File(file) => file.flush(),
            Sink::Stderr | Sink::Discard => Ok(()),
        }
    }
}
