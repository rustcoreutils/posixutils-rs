//
// Copyright (c) 2024-2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::{DateTime, Local};
use std::{
    io::{self, Write},
    path::Path,
    time::SystemTime,
};

use crate::diff_util::constants::COULD_NOT_UNWRAP_FILENAME;

/// POSIX context diff timestamp format: "%a %b %e %T %Y"
/// Example: "Sat Dec 13 10:26:40 2025"
pub fn system_time_to_context_format(system_time: SystemTime) -> String {
    let dt: DateTime<Local> = system_time.into();
    dt.format("%a %b %e %T %Y").to_string()
}

/// POSIX unified diff timestamp format with fractional seconds and timezone
/// offset: "%Y-%m-%d %H:%M:%S.%f %z"
/// Example: "2025-12-13 10:26:40.123456789 +0000"
pub fn system_time_to_unified_format(system_time: SystemTime) -> String {
    let dt: DateTime<Local> = system_time.into();
    dt.format("%Y-%m-%d %H:%M:%S%.9f %z").to_string()
}

/// Whether `data` should be treated as a binary file rather than diffed.
///
/// The rule is a NUL byte, which is what GNU diff uses; nothing else in the
/// byte range makes a file binary. The previous table also rejected 0x01..0x06,
/// 0x0b, 0x0e..0x1f and 0x7f, so a text file carrying one stray control byte
/// was reported as "Binary files ... differ" instead of being compared.
pub fn is_binary(data: &[u8]) -> bool {
    data.contains(&0)
}

/// Write `prefix`, the raw bytes of `line`, and a newline.
///
/// Line data cannot go through `write!`: a lossy UTF-8 conversion is not
/// length-preserving, so a patch generated from a Latin-1 or Shift-JIS file
/// would no longer apply to the file it came from.
pub fn write_line(out: &mut impl Write, prefix: &[u8], line: &[u8]) -> io::Result<()> {
    out.write_all(prefix)?;
    out.write_all(line)?;
    out.write_all(b"\n")
}

/// Attach `path` to an I/O error, so the diagnostic names the file it happened
/// on.
///
/// An `io::Error` from `open` or `read_dir` does not say which path produced
/// it, so a caller holding two of them could only guess -- and guessed the
/// first one, naming the wrong file for every failure on the second operand.
/// Attaching it here, where the path is still in hand, means no reporting site
/// has to guess.
pub fn io_error_at(path: &Path, error: io::Error) -> io::Error {
    io::Error::new(
        error.kind(),
        format!(
            "{}: {}",
            path.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME),
            plib::diag::io_error_text(&error)
        ),
    )
}

pub fn check_existance(path_buf: &Path) -> io::Result<bool> {
    if !path_buf.exists() {
        eprintln!(
            "diff: {}: No such file or directory",
            path_buf.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME)
        );

        return Ok(false);
    }

    Ok(true)
}
