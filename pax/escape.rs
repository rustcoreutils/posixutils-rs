//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Writing untrusted names to a terminal.
//!
//! A member name comes from the archive, so it can contain anything a pathname
//! can -- including an ANSI escape sequence. Listing a crafted archive used to
//! write those bytes straight out, so the archive could repaint, clear or
//! misreport the terminal of whoever ran `pax -v`.
//!
//! Escaping happens **only when the destination is a terminal**, which is what
//! `ls(1)` does. Piped output stays byte-identical, so nothing that parses
//! `pax -t` changes behaviour, and the attack -- which needs a terminal to land
//! on -- is closed anyway.
//!
//! The rule and the decision are deliberately separate. [`push_escaped`] is a
//! pure transform that takes the [`Style`] as an argument, so it is testable
//! without a pty; the `isatty` lookup is two lines that nothing needs to test.

use std::io::{self, IsTerminal, Write};
use std::path::Path;
use std::sync::OnceLock;

/// Whether names written to one particular stream get escaped.
///
/// A value rather than a hidden global, so a caller says which stream it is
/// writing to and `/dev/tty` -- a terminal by construction -- needs no special
/// case.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Style {
    escape: bool,
}

impl Style {
    /// Escape unconditionally. `/dev/tty` is a terminal by definition, and the
    /// unit tests use this to exercise the rule.
    pub const TTY: Style = Style { escape: true };

    /// Leave the bytes exactly as they are. Used by tests that render a
    /// listing without a terminal in the picture.
    #[cfg(test)]
    pub const RAW: Style = Style { escape: false };
}

/// Whether standard output is a terminal, looked up once.
///
/// `OnceLock` is safe here because pax never reopens its standard streams; if
/// that ever changes, this cache has to go with it.
pub fn stdout_style() -> Style {
    static IS_TTY: OnceLock<bool> = OnceLock::new();
    Style {
        escape: *IS_TTY.get_or_init(|| io::stdout().is_terminal()),
    }
}

/// Whether standard error is a terminal, looked up once.
pub fn stderr_style() -> Style {
    static IS_TTY: OnceLock<bool> = OnceLock::new();
    Style {
        escape: *IS_TTY.get_or_init(|| io::stderr().is_terminal()),
    }
}

/// Append `bytes` to `out`, rendering anything unprintable as `?`.
///
/// One display unit in, one out -- a unit being one valid UTF-8 character or
/// one byte that cannot start one. That invariant is what lets the callers
/// keep their column arithmetic: `print_verbose` pads columns and
/// `-o listopt=%10F` pads to a requested width, and neither has to know
/// whether escaping happened.
///
/// `?` rather than a C-style `\033`, which is `ls(1)`'s default and what keeps
/// the unit count invariant. ESC is a control character, so the injection case
/// this exists for is covered.
pub fn push_escaped(out: &mut Vec<u8>, bytes: &[u8], style: Style) {
    if !style.escape {
        out.extend_from_slice(bytes);
        return;
    }

    let mut i = 0;
    while i < bytes.len() {
        let len = crate::rawpath::unit_len(&bytes[i..]);
        let unit = &bytes[i..i + len];
        match std::str::from_utf8(unit)
            .ok()
            .and_then(|s| s.chars().next())
        {
            Some(c) if !c.is_control() => out.extend_from_slice(unit),
            // A control character, or a byte that is not a character at all.
            _ => out.push(b'?'),
        }
        i += len;
    }
}

/// Write a pathname to `w`, escaped for `style`.
pub fn write_name<W: Write>(w: &mut W, path: &Path, style: Style) -> io::Result<()> {
    write_bytes(w, crate::rawpath::as_bytes(path), style)
}

/// Write raw bytes to `w`, escaped for `style`.
pub fn write_bytes<W: Write>(w: &mut W, bytes: &[u8], style: Style) -> io::Result<()> {
    if !style.escape {
        return w.write_all(bytes);
    }
    let mut buf = Vec::with_capacity(bytes.len());
    push_escaped(&mut buf, bytes, style);
    w.write_all(&buf)
}

/// Write a whole line to standard error as one call, escaped if that is a
/// terminal.
///
/// One `write_all` under a held lock: building the line first and issuing it in
/// a single write is what keeps a caller that also writes to stderr from
/// interleaving with it.
///
/// This does not touch the exit status. `report_error` flags the run; `-s ...p`
/// reports a successful substitution and must not.
pub fn write_stderr_line(line: &[u8]) {
    let mut out = Vec::with_capacity(line.len() + 1);
    push_escaped(&mut out, line, stderr_style());
    out.push(b'\n');
    let stderr = io::stderr();
    let mut lock = stderr.lock();
    let _ = lock.write_all(&out);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn escaped(bytes: &[u8]) -> Vec<u8> {
        let mut out = Vec::new();
        push_escaped(&mut out, bytes, Style::TTY);
        out
    }

    #[test]
    fn test_raw_style_is_the_identity() {
        let mut out = Vec::new();
        push_escaped(&mut out, b"a\x1b[31mb\xffc", Style::RAW);
        assert_eq!(out, b"a\x1b[31mb\xffc");
    }

    /// The motivating case: an archive must not be able to send an escape
    /// sequence to the terminal of whoever listed it.
    #[test]
    fn test_escape_sequence_is_neutralised() {
        assert_eq!(escaped(b"a\x1b[31mRED\x1b[0m"), b"a?[31mRED?[0m");
    }

    #[test]
    fn test_tab_and_newline_are_escaped() {
        assert_eq!(escaped(b"a\tb\nc"), b"a?b?c");
    }

    #[test]
    fn test_invalid_byte_becomes_one_question_mark() {
        assert_eq!(escaped(b"na\xffme"), b"na?me");
    }

    /// A printable multi-byte character is not a control character and must
    /// survive intact, or every non-ASCII name would be mangled on a terminal.
    #[test]
    fn test_printable_multibyte_survives() {
        assert_eq!(escaped("élan".as_bytes()), "élan".as_bytes());
    }

    /// One unit in, one unit out is what the column arithmetic depends on.
    #[test]
    fn test_unit_count_is_preserved() {
        for input in [
            b"a\x1b[31mb".as_slice(),
            b"na\xffme".as_slice(),
            "élan\t".as_bytes(),
            b"\xc0\xaf".as_slice(),
        ] {
            assert_eq!(
                crate::rawpath::unit_starts(&escaped(input)).len(),
                crate::rawpath::unit_starts(input).len(),
                "escaping changed the unit count of {input:?}"
            );
        }
    }
}
