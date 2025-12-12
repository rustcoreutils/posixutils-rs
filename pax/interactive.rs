//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Interactive rename support for -i option
//!
//! When -i is specified, pax prompts the user for each file to be
//! processed. The user can:
//! - Enter a blank line to skip the file
//! - Enter "." to use the original name
//! - Enter any other text to use as the new name

use crate::error::{PaxError, PaxResult};
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;

/// Result of an interactive rename prompt
#[derive(Debug, Clone, PartialEq)]
pub enum RenameResult {
    /// Skip this file (blank input)
    Skip,
    /// Use original name (single period input)
    UseOriginal,
    /// Use new name (any other input)
    Rename(PathBuf),
}

/// Manages interactive prompts to /dev/tty
pub struct InteractivePrompter {
    tty_read: BufReader<File>,
    tty_write: File,
}

impl InteractivePrompter {
    /// Open /dev/tty for interactive prompts
    #[cfg(unix)]
    pub fn new() -> PaxResult<Self> {
        let tty_read = File::open("/dev/tty").map_err(|e| {
            PaxError::Io(std::io::Error::other(format!(
                "cannot open /dev/tty for reading: {}",
                e
            )))
        })?;

        let tty_write = File::options().write(true).open("/dev/tty").map_err(|e| {
            PaxError::Io(std::io::Error::other(format!(
                "cannot open /dev/tty for writing: {}",
                e
            )))
        })?;

        Ok(InteractivePrompter {
            tty_read: BufReader::new(tty_read),
            tty_write,
        })
    }

    #[cfg(not(unix))]
    pub fn new() -> PaxResult<Self> {
        // On non-Unix, use stdin/stderr as fallback
        Err(PaxError::Io(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "interactive mode not supported on this platform",
        )))
    }

    /// Prompt for a rename decision
    ///
    /// Returns:
    /// - `Ok(RenameResult::Skip)` if user enters blank line
    /// - `Ok(RenameResult::UseOriginal)` if user enters "."
    /// - `Ok(RenameResult::Rename(path))` if user enters a new name
    /// - `Err` if EOF is read or I/O error occurs
    pub fn prompt(&mut self, original_path: &str) -> PaxResult<RenameResult> {
        // Write prompt
        write!(self.tty_write, "{} => ", original_path)?;
        self.tty_write.flush()?;

        // Read response
        let mut line = String::new();
        let n = self.tty_read.read_line(&mut line)?;

        // EOF means we should exit immediately
        if n == 0 {
            return Err(PaxError::Io(std::io::Error::new(
                std::io::ErrorKind::UnexpectedEof,
                "EOF on interactive input",
            )));
        }

        let response = line.trim();

        if response.is_empty() {
            Ok(RenameResult::Skip)
        } else if response == "." {
            Ok(RenameResult::UseOriginal)
        } else {
            Ok(RenameResult::Rename(PathBuf::from(response)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rename_result() {
        assert_eq!(RenameResult::Skip, RenameResult::Skip);
        assert_eq!(RenameResult::UseOriginal, RenameResult::UseOriginal);
        assert_eq!(
            RenameResult::Rename(PathBuf::from("foo")),
            RenameResult::Rename(PathBuf::from("foo"))
        );
    }
}
