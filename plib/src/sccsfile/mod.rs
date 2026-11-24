//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! SCCS file format parser and serializer
//!
//! This module provides support for reading and writing SCCS (Source Code
//! Control System) files as defined in POSIX.2024.
//!
//! # File Format
//!
//! SCCS files consist of control lines prefixed with SOH (0x01, displayed as ^A)
//! and text lines. The structure is:
//!
//! - Checksum line: `^Ah<5-digit-sum>`
//! - Delta table: Series of delta entries
//! - User list: Authorized users
//! - Flags: File configuration flags
//! - Descriptive text: Comments about the file
//! - Body: Interleaved delta content with ^AI/^AD/^AE control records

mod checksum;
mod cleanup;
mod datetime;
mod file;
mod model;
mod parse;
pub mod paths;
mod pfile;
mod sid;
mod text;
mod uu;
mod zlock;

#[cfg(test)]
mod tests;

pub use checksum::*;
pub use cleanup::*;
pub use datetime::*;
pub use file::*;
pub use model::*;
pub use pfile::*;
pub use sid::*;
pub use text::*;
pub use uu::*;
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
                    "Checksum mismatch: expected {}, computed {}",
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
