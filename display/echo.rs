//
// Copyright (c) 2024-2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use gettextrs::gettext;
use plib::diag;
use std::io::{self, Write};
use std::os::unix::ffi::OsStrExt;
use std::process::ExitCode;

/// Expand the XSI escape sequences of POSIX.1-2024 `echo` (lines 93172-93186)
/// over the joined operand bytes, appending the trailing <newline> unless it is
/// suppressed by `skip_nl` or by a `\c` sequence.
///
/// Operands are byte strings, not text: the input is never decoded, so a
/// non-UTF-8 operand passes through unchanged and `\0num` can name any byte.
fn translate_bytes(skip_nl: bool, s: &[u8]) -> Vec<u8> {
    let mut output = Vec::with_capacity(s.len() + 1);
    let mut nl = true;
    let mut i = 0;

    while i < s.len() {
        let byte = s[i];
        if byte != b'\\' {
            output.push(byte);
            i += 1;
            continue;
        }

        // A trailing <backslash> introduces no sequence; preserve it.
        let Some(&next) = s.get(i + 1) else {
            output.push(b'\\');
            i += 1;
            continue;
        };
        i += 2;

        match next {
            b'a' => output.push(0x07),
            b'b' => output.push(0x08),
            b'c' => {
                // Suppress the <newline> and ignore all following characters.
                nl = false;
                break;
            }
            b'f' => output.push(0x0c),
            b'n' => output.push(b'\n'),
            b'r' => output.push(b'\r'),
            b't' => output.push(b'\t'),
            b'v' => output.push(0x0b),
            b'\\' => output.push(b'\\'),
            b'0' => {
                // \0num, where num is a zero-, one-, two-, or three-digit
                // octal number naming an 8-bit value.  Accumulate in u16 so
                // that a three-digit value above 377 cannot overflow, then
                // keep the low 8 bits.
                let mut value: u16 = 0;
                let mut digits = 0;
                while digits < 3 {
                    match s.get(i) {
                        Some(&digit @ b'0'..=b'7') => {
                            value = value * 8 + u16::from(digit - b'0');
                            i += 1;
                            digits += 1;
                        }
                        _ => break,
                    }
                }
                output.push((value & 0xff) as u8);
            }
            // Not one of the sequences the spec defines.  POSIX leaves this
            // unspecified; emit the input verbatim rather than losing the
            // <backslash>.
            _ => {
                output.push(b'\\');
                output.push(next);
            }
        }
    }

    if nl && !skip_nl {
        output.push(b'\n');
    }

    output
}

fn main() -> ExitCode {
    diag::init_locale("echo");

    let mut args: Vec<Vec<u8>> = std::env::args_os()
        .skip(1)
        .map(|arg| arg.as_bytes().to_vec())
        .collect();

    // Historical BSD behavior.  POSIX.1-2024 lines 93167-93169 (Austin Group
    // Defect 1222) make the results implementation-defined when the first
    // operand is a '-' followed by characters from the set {'e','E','n'}.
    let skip_nl = if args.first().map(Vec::as_slice) == Some(b"-n".as_slice()) {
        args.remove(0);
        true
    } else {
        false
    };

    let echo_bytes = translate_bytes(skip_nl, &args.join(&b' '));

    let mut stdout = io::stdout().lock();
    if let Err(e) = stdout.write_all(&echo_bytes).and_then(|()| stdout.flush()) {
        diag::error(&format!("{}: {e}", gettext("write error")));
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}

#[cfg(test)]
mod tests {
    use super::translate_bytes;

    #[test]
    fn octal_escape_range() {
        // Three-digit octal values above 377 must not overflow; the low 8
        // bits are kept.
        assert_eq!(translate_bytes(true, br"\0777"), vec![0xff]);
        assert_eq!(translate_bytes(true, br"\0400"), vec![0x00]);
        assert_eq!(translate_bytes(true, br"\0377"), vec![0xff]);
        assert_eq!(translate_bytes(true, br"\0101"), vec![b'A']);
        // A fourth octal digit is an ordinary character.
        assert_eq!(translate_bytes(true, br"\01011"), vec![b'A', b'1']);
    }

    #[test]
    fn unspecified_escape_is_verbatim() {
        assert_eq!(translate_bytes(true, br"\q"), b"\\q".to_vec());
        assert_eq!(translate_bytes(true, br"\1"), b"\\1".to_vec());
    }

    #[test]
    fn non_utf8_operand_passes_through() {
        assert_eq!(translate_bytes(true, &[0xff, 0xfe]), vec![0xff, 0xfe]);
    }
}
