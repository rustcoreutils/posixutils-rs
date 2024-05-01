//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::error::Error;
use std::fmt;

enum RawMagicFileError {}

#[derive(Debug)]
pub enum CompiledMagicFileError {
    MagicFileNotFound,
    TestFileNotFound,
    InvalidMagicHeader,
    InvalidVersionNumber,
    InvalidSize,
    InvalidNumberOfEntries,
}

impl Error for CompiledMagicFileError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl fmt::Display for CompiledMagicFileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompiledMagicFileError::MagicFileNotFound => {
                write!(f, "Magic file not found!")
            }
            CompiledMagicFileError::TestFileNotFound => {
                write!(f, "Test file not found!")
            }
            CompiledMagicFileError::InvalidMagicHeader => {
                write!(f, "Invalid magic header in compiled magic file")
            }
            CompiledMagicFileError::InvalidVersionNumber => {
                write!(f, "Invalid version number in compiled magic file")
            }
            CompiledMagicFileError::InvalidSize => {
                write!(f, "Invalid size in compiled magic file")
            }
            CompiledMagicFileError::InvalidNumberOfEntries => {
                write!(f, "Invalid number of entries in compiled magic file")
            }
        }
    }
}
