//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The error a mailx command or composition reports.

use std::fmt;
use std::io;

#[derive(Debug)]
pub enum Error {
    /// An I/O failure, with what was being done or the file it was done to.
    Io { context: String, source: io::Error },
    /// Any other diagnostic, already worded for the user.
    Msg(String),
    /// Message composition abandoned by `~q` or `~x`.
    Aborted,
    /// Message composition abandoned by a second interrupt.
    Interrupted,
}

pub type Result<T> = std::result::Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io { context, source } => {
                write!(f, "{}: {}", context, plib::diag::io_error_text(source))
            }
            Error::Msg(msg) => f.write_str(msg),
            Error::Aborted => f.write_str("Aborted"),
            Error::Interrupted => f.write_str("Interrupt"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Io { source, .. } => Some(source),
            _ => None,
        }
    }
}

impl From<String> for Error {
    fn from(msg: String) -> Self {
        Error::Msg(msg)
    }
}

impl From<&str> for Error {
    fn from(msg: &str) -> Self {
        Error::Msg(msg.to_string())
    }
}

/// Attach context to an I/O result: `fs::read(&p).context(&p)?`.
pub trait IoContext<T> {
    fn context(self, context: impl Into<String>) -> Result<T>;
}

impl<T> IoContext<T> for io::Result<T> {
    fn context(self, context: impl Into<String>) -> Result<T> {
        self.map_err(|source| Error::Io {
            context: context.into(),
            source,
        })
    }
}
