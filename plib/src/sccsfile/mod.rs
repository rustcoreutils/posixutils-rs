//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! SCCS files: the s-file and the files that go with it.
//!
//! This module reads and changes SCCS (Source Code Control System) files as
//! POSIX.1-2024 describes them, and as SCCS and CSSC have written them.
//!
//! # File format
//!
//! An s-file is lines. Control lines begin with SOH (0x01, shown as `^A`);
//! the rest is text. In order:
//!
//! - the checksum line, `^Ah<5-digit-sum>`;
//! - the delta table, one entry per delta, newest first;
//! - the list of authorized users;
//! - the flags;
//! - the descriptive text;
//! - the body: every version's text interleaved (the weave), in blocks
//!   marked by `^AI`, `^AD` and `^AE` records.
//!
//! # Design
//!
//! **The file is kept as read.** [`SccsFile`] holds the file's bytes and an
//! index of its lines; the typed header ([`SccsHeader`]) is a view derived
//! from them by the one parser. A file read and written back is the same
//! bytes. A change (`set_flag`, `add_user`, `add_delta`, `remove_delta`,
//! ...) copies every line it does not concern, writes the ones it does and a
//! fresh checksum line, and parses the result again, so the view is always
//! what a reader of the bytes would see. Lines this implementation does not
//! know stay where they are.
//!
//! **One walk over the weave.** [`Walker`] yields the body's records in
//! order, each text line with the verdict of the blocks open around it
//! ([`OpenBlocks`]): the open block of the newest delta that has a say
//! decides, as in SCCS and CSSC. Retrieval, annotated retrieval, the
//! structural check, `add_delta` and `remove_delta` are all that walk.
//! Blocks need not nest; damage to the weave is an error for every reader.
//!
//! **Values are checked where they are made.** A login, a comment line, an
//! MR number, a line of descriptive text and a list of deltas are types
//! ([`Login`], [`CommentLine`], [`MrNumber`], [`TextLine`], [`SidList`])
//! that refuse what their record cannot hold; the records that are written
//! take those types. All of them are bytes, as are file names ([`paths`]):
//! none of it has to be UTF-8.
//!
//! **The checksum is checked on every read** ([`SccsFile::from_path`]),
//! signed or unsigned sum alike; only `admin -z` reads past it.
//!
//! Locking, and the order in which the s-file and p-file are read and
//! written under the lock, are the `posixutils-sccs` crate's transaction
//! type; the lock file itself is [`ZLock`].

mod checksum;
mod cleanup;
mod datetime;
mod edit;
mod file;
mod model;
mod parse;
pub mod paths;
mod pfile;
mod sid;
mod text;
mod uu;
mod value;
mod weave;
mod zlock;

#[cfg(test)]
mod tests;

pub use checksum::*;
pub use cleanup::*;
pub use datetime::*;
pub use edit::*;
pub use file::*;
pub use model::*;
pub use pfile::*;
pub use sid::*;
pub use text::*;
pub use uu::*;
pub use value::*;
pub use weave::*;
pub use zlock::*;

use std::fmt;
use std::io;

/// SOH (Start of Heading) control character that marks SCCS control lines
pub const SOH: u8 = 0x01;

/// Control line tags (the character after SOH)
pub mod control {
    pub const CHECKSUM: u8 = b'h'; // ^Ah - checksum
    pub const STATS: u8 = b's'; // ^As - delta statistics
    pub const DELTA: u8 = b'd'; // ^Ad - delta header
    pub const INCLUDE: u8 = b'i'; // ^Ai - included deltas (header)
    pub const EXCLUDE: u8 = b'x'; // ^Ax - excluded deltas
    pub const IGNORE: u8 = b'g'; // ^Ag - ignored deltas
    pub const MR: u8 = b'm'; // ^Am - modification request
    pub const COMMENT: u8 = b'c'; // ^Ac - comment
    pub const END_DELTA: u8 = b'e'; // ^Ae - end of delta entry
    pub const USER_BEGIN: u8 = b'u'; // ^Au - begin user list
    pub const USER_END: u8 = b'U'; // ^AU - end user list
    pub const FLAG: u8 = b'f'; // ^Af - flag
    pub const DESC_BEGIN: u8 = b't'; // ^At - begin descriptive text
    pub const DESC_END: u8 = b'T'; // ^AT - end descriptive text
                                   // Body control records (uppercase)
    pub const BODY_INSERT: u8 = b'I'; // ^AI - begin insert block
    pub const BODY_DELETE: u8 = b'D'; // ^AD - begin delete block
    pub const BODY_END: u8 = b'E'; // ^AE - end block
}

/// Errors that can occur during SCCS file operations
#[derive(Debug)]
pub enum SccsError {
    /// I/O error
    Io(io::Error),
    /// Invalid file format
    InvalidFormat(String),
    /// Checksum mismatch
    ChecksumMismatch { expected: u16, computed: u16 },
    /// Invalid SID format
    InvalidSid(String),
    /// Invalid delta entry
    InvalidDelta(String),
    /// Invalid flag
    InvalidFlag(String),
    /// Invalid date/time format
    InvalidDateTime(String),
    /// Missing required field
    MissingField(String),
    /// Invalid serial number
    InvalidSerial(String),
    /// Body structure error
    InvalidBody(String),
}

impl fmt::Display for SccsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SccsError::Io(e) => write!(f, "I/O error: {}", crate::diag::io_error_text(e)),
            SccsError::InvalidFormat(s) => write!(f, "Invalid SCCS format: {}", s),
            SccsError::ChecksumMismatch { expected, computed } => {
                write!(
                    f,
                    "corrupted file: checksum error: stored {}, computed {}",
                    expected, computed
                )
            }
            SccsError::InvalidSid(s) => write!(f, "Invalid SID: {}", s),
            SccsError::InvalidDelta(s) => write!(f, "Invalid delta: {}", s),
            SccsError::InvalidFlag(s) => write!(f, "Invalid flag: {}", s),
            SccsError::InvalidDateTime(s) => write!(f, "Invalid date/time: {}", s),
            SccsError::MissingField(s) => write!(f, "Missing field: {}", s),
            SccsError::InvalidSerial(s) => write!(f, "Invalid serial: {}", s),
            SccsError::InvalidBody(s) => write!(f, "Invalid body: {}", s),
        }
    }
}

impl std::error::Error for SccsError {}

impl From<io::Error> for SccsError {
    fn from(e: io::Error) -> Self {
        SccsError::Io(e)
    }
}

/// Result type for SCCS operations
pub type SccsResult<T> = Result<T, SccsError>;
