//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The output of POSIX XCU `echo`.
//!
//! Shared by the `echo` utility and by the shell's built-in version, so that
//! the two cannot drift apart.  Operands are byte strings, not text: they are
//! never decoded, so a non-UTF-8 operand passes through unchanged and `\0num`
//! can name any byte.

/// Return the bytes `echo` writes for `args` (the operands, without the
/// utility name).
///
/// Historical BSD behaviour: only an exact first operand `-n` suppresses the
/// trailing <newline>.  POSIX.1-2024 (Austin Group Defect 1222) makes the
/// results implementation-defined when the first operand is a '-' followed by
/// characters from the set {'e','E','n'}; every other operand, `-e` included,
/// is printed as given, as dash does.
pub fn echo_output<A: AsRef<[u8]>>(args: &[A]) -> Vec<u8> {
    let (skip_nl, operands) = match args.split_first() {
        Some((first, rest)) if first.as_ref() == b"-n" => (true, rest),
        _ => (false, args),
    };

    let joined: Vec<u8> = operands
        .iter()
        .map(AsRef::as_ref)
        .collect::<Vec<&[u8]>>()
        .join(&b' ');
    translate_bytes(skip_nl, &joined)
}

/// Expand the XSI escape sequences of POSIX.1-2024 `echo` over the joined
/// operand bytes, appending the trailing <newline> unless it is suppressed by
/// `skip_nl` or by a `\c` sequence.
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

#[cfg(test)]
mod tests {
    use super::{echo_output, translate_bytes};

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

    #[test]
    fn only_exact_first_dash_n_suppresses_newline() {
        assert_eq!(echo_output(&["-n", "a", "b"]), b"a b".to_vec());
        assert_eq!(echo_output(&["a", "-n"]), b"a -n\n".to_vec());
        assert_eq!(echo_output(&["-nn", "a"]), b"-nn a\n".to_vec());
        assert_eq!(echo_output(&["-e", r"a\tb"]), b"-e a\tb\n".to_vec());
        assert_eq!(echo_output(&["-n"]), Vec::<u8>::new());
    }

    #[test]
    fn no_operands_is_a_newline() {
        assert_eq!(echo_output::<&str>(&[]), b"\n".to_vec());
    }

    #[test]
    fn backslash_c_stops_output() {
        assert_eq!(echo_output(&[r"a\cb", "c"]), b"a".to_vec());
    }
}
