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
        if current_display >= display_col {
            break;
        }
        let width = char_width(c, current_display, tabstop);
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
}
