//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! What text an s-file can hold, and how it divides into lines.

use super::SOH;

/// Why some content cannot be the text of a text (not encoded) s-file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NotText {
    /// The last line has no newline to end it.
    NoFinalNewline,
    /// It holds a NUL.
    Nul,
    /// This line, counted from 1, begins with SOH and would read back as a
    /// control line.
    LeadingSoh(usize),
}

/// What keeps `data` from being stored as the text of an s-file, if anything.
///
/// The classic s-file is lines of text: each ends with a newline, none
/// begins with SOH, and no implementation reads a NUL back out of one. That
/// is also what POSIX asks of the input (a text file, with a leading SOH
/// unspecified). `admin` stores such content encoded instead (the `e` flag);
/// `delta` to a text s-file refuses it, as historical SCCS does ("No newline
/// at end of file (de18)", de14 for a NUL, de20 for the SOH).
///
/// Bytes that are not UTF-8 are text: what a character is belongs to the
/// locale, and a Latin-1 source file is a text file.
pub fn not_storable_as_text(data: &[u8]) -> Option<NotText> {
    if data.is_empty() {
        return None;
    }
    if !data.ends_with(b"\n") {
        return Some(NotText::NoFinalNewline);
    }
    if data.contains(&0x00) {
        return Some(NotText::Nul);
    }
    split_lines(data)
        .iter()
        .position(|line| line.first() == Some(&SOH))
        .map(|n| NotText::LeadingSoh(n + 1))
}

/// Split text into its lines: each ends at a newline, which is not part of
/// it. A CR before the newline *is* part of it, and a final line without a
/// newline is still a line. Empty input has no lines.
///
/// This is how SCCS stores text, and the one way the utilities split it:
/// `str::lines` treats `\r\n` as a terminator and so silently drops the CR
/// of every line of a CRLF file.
pub fn split_lines(data: &[u8]) -> Vec<&[u8]> {
    let mut lines: Vec<&[u8]> = data.split(|&b| b == b'\n').collect();
    if data.is_empty() || data.ends_with(b"\n") {
        lines.pop();
    }
    lines
}
