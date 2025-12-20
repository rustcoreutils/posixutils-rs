//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Error types for the ed editor.

use std::fmt;
use std::io;

/// Errors that can occur during ed operation.
#[derive(Debug)]
pub enum EdError {
    /// Generic error message (displayed as "?")
    Generic(String),
    /// Address out of range
    AddressOutOfRange,
    /// Invalid address
    InvalidAddress,
    /// Invalid command
    InvalidCommand(String),
    /// No filename set
    NoFilename,
    /// No previous pattern
    NoPreviousPattern,
    /// No match found
    NoMatch,
    /// Buffer modified but not saved
    BufferModified,
    /// I/O error
    Io(io::Error),
    /// Syntax error in command
    Syntax(String),
}

impl fmt::Display for EdError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EdError::Generic(msg) => write!(f, "{}", msg),
            EdError::AddressOutOfRange => write!(f, "Invalid address"),
            EdError::InvalidAddress => write!(f, "Invalid address"),
            EdError::InvalidCommand(cmd) => write!(f, "Invalid command: {}", cmd),
            EdError::NoFilename => write!(f, "No current filename"),
            EdError::NoPreviousPattern => write!(f, "No previous pattern"),
            EdError::NoMatch => write!(f, "No match"),
            EdError::BufferModified => write!(f, "Warning: buffer modified"),
            EdError::Io(e) => write!(f, "{}", e),
            EdError::Syntax(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for EdError {}

impl From<io::Error> for EdError {
    fn from(e: io::Error) -> Self {
        EdError::Io(e)
    }
}

/// Result type for ed operations.
pub type EdResult<T> = Result<T, EdError>;
