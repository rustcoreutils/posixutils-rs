//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fmt;
use std::io;

/// Error type for pax operations
#[derive(Debug)]
pub enum PaxError {
    /// I/O error
    Io(io::Error),
    /// Invalid archive format
    InvalidFormat(String),
    /// Invalid header field
    InvalidHeader(String),
    /// Path too long for format
    PathTooLong(String),
    /// Pattern matching error
    PatternError(String),
}

impl fmt::Display for PaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PaxError::Io(e) => write!(f, "I/O error: {}", e),
            PaxError::InvalidFormat(msg) => write!(f, "Invalid archive format: {}", msg),
            PaxError::InvalidHeader(msg) => write!(f, "Invalid header: {}", msg),
            PaxError::PathTooLong(path) => write!(f, "Path too long: {}", path),
            PaxError::PatternError(msg) => write!(f, "Pattern error: {}", msg),
        }
    }
}

impl std::error::Error for PaxError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            PaxError::Io(e) => Some(e),
            _ => None,
        }
    }
}

impl From<io::Error> for PaxError {
    fn from(err: io::Error) -> Self {
        PaxError::Io(err)
    }
}

/// Result type for pax operations
pub type PaxResult<T> = Result<T, PaxError>;

/// Check if a PaxError represents end-of-file
///
/// This is preferred over string matching on error messages,
/// which is fragile across different platforms and Rust versions.
pub fn is_eof_error(error: &PaxError) -> bool {
    match error {
        PaxError::Io(e) => e.kind() == io::ErrorKind::UnexpectedEof,
        _ => false,
    }
}
