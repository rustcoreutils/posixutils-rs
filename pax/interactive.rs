//
// Copyright (c) 2025-2026 Jeff Garzik
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

    /// Prompt for a rename decision.
    ///
    /// Takes the pathname rather than its lossy rendering, and writes it to
    /// `/dev/tty` escaped: `/dev/tty` is a terminal by construction, so this
    /// prompt is exactly where an escape sequence in a member name would land.
    ///
    /// Returns:
    /// - `Ok(RenameResult::Skip)` if user enters blank line
    /// - `Ok(RenameResult::UseOriginal)` if user enters "."
    /// - `Ok(RenameResult::Rename(path))` if user enters a new name
    /// - `Err` if EOF is read or I/O error occurs
    pub fn prompt(&mut self, original_path: &std::path::Path) -> PaxResult<RenameResult> {
        // Write prompt
        let mut line = Vec::new();
        crate::escape::push_escaped(
            &mut line,
            crate::rawpath::as_bytes(original_path),
            crate::escape::Style::TTY,
        );
        line.extend_from_slice(b" => ");
        self.tty_write.write_all(&line)?;
        self.tty_write.flush()?;

        // Read response
        let mut line = Vec::new();
        let n = self.tty_read.read_until(b'\n', &mut line)?;

        // EOF means we should exit immediately
        if n == 0 {
            return Err(PaxError::TtyEof);
        }

        Ok(parse_response(&line))
    }
}

/// What a line typed at the prompt asks for.
///
/// The line, less its <newline>, is a pathname: bytes, not text, and nothing
/// else is stripped, so a name with leading or trailing blanks can be given.
fn parse_response(line: &[u8]) -> RenameResult {
    let response = line.strip_suffix(b"\n").unwrap_or(line);
    match response {
        b"" => RenameResult::Skip,
        b"." => RenameResult::UseOriginal,
        name => RenameResult::Rename(crate::rawpath::from_bytes(name)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_response() {
        assert_eq!(parse_response(b"\n"), RenameResult::Skip);
        assert_eq!(parse_response(b".\n"), RenameResult::UseOriginal);
        assert_eq!(
            parse_response(b" a b \n"),
            RenameResult::Rename(PathBuf::from(" a b "))
        );
        assert_eq!(
            parse_response(b"n\xff\n"),
            RenameResult::Rename(crate::rawpath::from_bytes(b"n\xff"))
        );
    }

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
