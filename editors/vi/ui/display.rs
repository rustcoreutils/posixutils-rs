//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Display calculations and line rendering.

/// Calculate the display width of a character.
pub fn char_width(c: char, current_col: usize, tabstop: usize) -> usize {
    match c {
        '\t' => tabstop - (current_col % tabstop),
        c if c.is_control() => 2, // ^X format
        _ => 1,
    }
}

/// Calculate the display width of a string.
pub fn string_width(s: &str, tabstop: usize) -> usize {
    let mut width = 0;
    for c in s.chars() {
        width += char_width(c, width, tabstop);
    }
    width
}

/// Find the byte offset in a string for a given display column.
pub fn display_col_to_byte_offset(s: &str, display_col: usize, tabstop: usize) -> usize {
    let mut current_display = 0;
    let mut byte_offset = 0;

    for c in s.chars() {
        let width = char_width(c, current_display, tabstop);
        // The character that *occupies* the requested column, not the one
        // after it: a tab spans several columns, and stopping only once the
        // running total had passed `display_col` skipped over it, making every
        // column of a tab but its first unreachable.
        if current_display + width > display_col {
            return byte_offset;
        }
        current_display += width;
        byte_offset += c.len_utf8();
    }

    byte_offset
}

/// Find the display column for a given byte offset.
pub fn byte_offset_to_display_col(s: &str, byte_offset: usize, tabstop: usize) -> usize {
    let mut current_display = 0;
    let mut current_byte = 0;

    for c in s.chars() {
        if current_byte >= byte_offset {
            break;
        }
        let width = char_width(c, current_display, tabstop);
        current_display += width;
        current_byte += c.len_utf8();
    }

    current_display
}

/// Truncate a string to fit within a given display width.
pub fn truncate_to_width(s: &str, max_width: usize, tabstop: usize) -> String {
    let mut result = String::new();
    let mut width = 0;

    for c in s.chars() {
        let char_w = char_width(c, width, tabstop);
        if width + char_w > max_width {
            break;
        }
        result.push(c);
        width += char_w;
    }

    result
}

/// Render a key sequence in caret notation: ASCII control characters as `^X`,
/// DEL as `^?`, everything else as itself.
///
/// Distinct from [`expand_for_display`], which turns a TAB into spaces — right
/// for a buffer line, wrong here, where a TAB *is* the key being described and
/// has to stay visible. Distinct too from `ex`'s `:list` form, which POSIX
/// pins to octal escapes and a trailing `$` (95237-95244) for that command
/// only.
///
/// The caret form applies to ASCII controls alone. `char::is_control` is also
/// true of the C1 range U+0080-U+009F, and `c as u8` truncates those to a
/// single byte, so `^` plus the XOR produced an unrelated character: U+009B
/// rendered as `^Û`. Both routes to that are reachable —
/// `:map <U+009B> dd` directly, and any C1 character through `^V`. Those go to
/// one octal escape per UTF-8 byte, matching `:list`'s house style, which is
/// unambiguous and reversible. Printable non-ASCII stays literal, so a map on
/// `é` still reads as `é`.
pub fn caret_notation(s: &str) -> String {
    let mut out = String::new();
    for c in s.chars() {
        match c {
            '\x7f' => out.push_str("^?"),
            c if c.is_ascii_control() => {
                out.push('^');
                out.push((c as u8 ^ 0x40) as char);
            }
            c if c.is_control() => {
                let mut buf = [0u8; 4];
                for b in c.encode_utf8(&mut buf).as_bytes() {
                    out.push_str(&format!("\\{:03o}", b));
                }
            }
            c => out.push(c),
        }
    }
    out
}

/// Expand a line for display (tabs and control chars).
pub fn expand_for_display(s: &str, tabstop: usize) -> String {
    let mut result = String::new();
    let mut col = 0;

    for c in s.chars() {
        match c {
            '\t' => {
                let spaces = tabstop - (col % tabstop);
                for _ in 0..spaces {
                    result.push(' ');
                }
                col += spaces;
            }
            c if c.is_control() => {
                result.push('^');
                result.push((c as u8 ^ 0x40) as char);
                col += 2;
            }
            c => {
                result.push(c);
                col += 1;
            }
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_char_width() {
        assert_eq!(char_width('a', 0, 8), 1);
        assert_eq!(char_width('\t', 0, 8), 8);
        assert_eq!(char_width('\t', 3, 8), 5);
        assert_eq!(char_width('\x01', 0, 8), 2); // Control char
    }

    #[test]
    fn test_string_width() {
        assert_eq!(string_width("hello", 8), 5);
        assert_eq!(string_width("a\tb", 8), 9); // 'a' + 7 spaces + 'b'
    }

    #[test]
    fn test_display_col_to_byte_offset() {
        assert_eq!(display_col_to_byte_offset("hello", 3, 8), 3);
        // For "a\tb", display col 8 is the 'b', which is byte offset 2
        assert_eq!(display_col_to_byte_offset("a\tb", 8, 8), 2);
    }

    #[test]
    fn test_byte_offset_to_display_col() {
        assert_eq!(byte_offset_to_display_col("hello", 3, 8), 3);
        // Byte offset 2 in "a\tb" is 'b', which is at display col 8
        assert_eq!(byte_offset_to_display_col("a\tb", 2, 8), 8);
    }

    #[test]
    fn test_truncate_to_width() {
        assert_eq!(truncate_to_width("hello world", 5, 8), "hello");
        assert_eq!(truncate_to_width("a\tb", 4, 8), "a"); // Tab would exceed
    }

    #[test]
    fn test_expand_for_display() {
        assert_eq!(expand_for_display("a\tb", 8), "a       b");
        assert_eq!(expand_for_display("a\x01b", 8), "a^Ab");
    }

    #[test]
    fn test_caret_notation() {
        assert_eq!(caret_notation("plain"), "plain");
        assert_eq!(caret_notation("\x01"), "^A");
        assert_eq!(caret_notation("\x1b"), "^[");
        assert_eq!(caret_notation("\r"), "^M");
        assert_eq!(caret_notation("\x7f"), "^?");
        // The difference from `expand_for_display`, and the reason this exists:
        // a TAB in a map's left-hand side is the key itself, so it has to stay
        // visible rather than becoming indistinguishable from spaces.
        assert_eq!(caret_notation("a\tb"), "a^Ib");
        assert_eq!(expand_for_display("a\tb", 8), "a       b");
    }

    /// `char::is_control` is true of the C1 range U+0080-U+009F as well as the
    /// ASCII controls, and `c as u8` truncates those: the caret form turned
    /// U+009B into `^Û`, an unrelated character. They take an octal escape per
    /// UTF-8 byte instead, as `:list` does.
    #[test]
    fn test_caret_notation_does_not_truncate_non_ascii_controls() {
        assert_eq!(caret_notation("\u{9b}"), "\\302\\233");
        assert_eq!(caret_notation("\u{85}"), "\\302\\205");
        assert_eq!(caret_notation("a\u{9b}b"), "a\\302\\233b");

        // Printable non-ASCII is not a control character and stays literal, so
        // a map on `é` still reads as `é`.
        assert_eq!(caret_notation("é"), "é");
        assert_eq!(caret_notation("\u{a0}"), "\u{a0}");
    }
}
