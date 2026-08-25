//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Line abstraction for the edit buffer.

/// Round `byte_offset` down to the nearest character boundary, clamping to the
/// end of `content`.
///
/// This is the primitive the cursor model was missing.  Normalising a column
/// with a bare `min()` lowers an out-of-range offset but leaves an in-range
/// one alone, so a mid-character offset stayed representable and the next
/// slice panicked.  (`str::floor_char_boundary` is still unstable in the
/// Rust version this crate targets.)
pub fn floor_char_boundary(content: &str, byte_offset: usize) -> usize {
    let mut offset = byte_offset.min(content.len());
    while offset > 0 && !content.is_char_boundary(offset) {
        offset -= 1;
    }
    offset
}

/// Round `byte_offset` up to the nearest character boundary, clamping to the
/// end of `content`.
///
/// Used for the *end* of a character-mode range, so that a range never
/// bisects a character from either side.
pub fn ceil_char_boundary(content: &str, byte_offset: usize) -> usize {
    let mut offset = byte_offset.min(content.len());
    while offset < content.len() && !content.is_char_boundary(offset) {
        offset += 1;
    }
    offset
}

/// Byte offset of the character after the one starting at `byte_offset`, or
/// `None` at (or past) the end of `content`.
///
/// Replaces the `+ 1` that assumed every character is one byte wide.
pub fn next_char_boundary(content: &str, byte_offset: usize) -> Option<usize> {
    let start = floor_char_boundary(content, byte_offset);
    content[start..]
        .chars()
        .next()
        .map(|c| start + c.len_utf8())
}

/// Byte offset of the character before the one at `byte_offset`, or `None` at
/// the start of `content`.
///
/// Replaces the `saturating_sub(1)` that assumed every character is one byte.
pub fn prev_char_boundary(content: &str, byte_offset: usize) -> Option<usize> {
    let end = floor_char_boundary(content, byte_offset);
    content[..end]
        .chars()
        .next_back()
        .map(|c| end - c.len_utf8())
}

/// Find character index for byte offset in string.
///
/// Returns the index of the first character whose byte offset is >= the given byte offset,
/// or the total character count if byte_offset is past the end.
pub fn char_index_at_byte(content: &str, byte_offset: usize) -> usize {
    let mut char_index = 0usize;
    for (b, _) in content.char_indices() {
        if b >= byte_offset {
            return char_index;
        }
        char_index += 1;
    }
    char_index
}

/// Width in display columns of a string's leading <blank> run, plus the byte
/// offset of the first non-<blank>.
///
/// A tab advances to the next `tabstop` boundary rather than counting as a
/// fixed width, so `\t` in column 0 with `tabstop=8` is 8 columns but `\t`
/// after three spaces is only 5.
pub fn leading_blank_width(content: &str, tabstop: usize) -> (usize, usize) {
    let ts = tabstop.max(1);
    let mut width = 0usize;
    let mut offset = 0usize;
    for (byte, c) in content.char_indices() {
        match c {
            '\t' => width += ts - (width % ts),
            ' ' => width += 1,
            _ => return (width, byte),
        }
        offset = byte + c.len_utf8();
    }
    (width, offset)
}

/// Display column of a byte offset within a line, expanding tabs.
pub fn display_col(content: &str, byte_offset: usize, tabstop: usize) -> usize {
    let ts = tabstop.max(1);
    let mut col = 0usize;
    for (byte, c) in content.char_indices() {
        if byte >= byte_offset {
            break;
        }
        if c == '\t' {
            col += ts - (col % ts);
        } else {
            col += 1;
        }
    }
    col
}

/// Render an indent of `width` display columns as tabs followed by spaces.
///
/// This is the form POSIX prescribes for autoindent (ex `autoindent`,
/// 95739-95741: "using first as many <tab> characters as possible, as
/// determined by the editor option tabstop, and then using <space>
/// characters"), and it is permitted when shifting, where leading blanks may be
/// "changed into other <blank> characters" (95631).
pub fn render_indent(width: usize, tabstop: usize) -> String {
    let ts = tabstop.max(1);
    let tabs = width / ts;
    let spaces = width % ts;
    let mut s = String::with_capacity(tabs + spaces);
    for _ in 0..tabs {
        s.push('\t');
    }
    for _ in 0..spaces {
        s.push(' ');
    }
    s
}

/// A single line in the edit buffer.
///
/// Lines do NOT include the trailing newline character.
/// The newline is implicit between lines.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Line {
    /// The content of the line (without newline).
    content: String,
}

impl Line {
    /// Create a new empty line.
    pub fn new() -> Self {
        Self {
            content: String::new(),
        }
    }

    /// Create a line from a string, stripping trailing newlines.
    fn from_string_trimmed(s: &str) -> Self {
        // Strip trailing newline if present
        let content = s.strip_suffix('\n').unwrap_or(s);
        let content = content.strip_suffix('\r').unwrap_or(content);
        Self {
            content: content.to_string(),
        }
    }

    /// Get the content of the line.
    pub fn content(&self) -> &str {
        &self.content
    }

    /// Get the length in bytes.
    pub fn len(&self) -> usize {
        self.content.len()
    }

    /// Check if line is empty.
    pub fn is_empty(&self) -> bool {
        self.content.is_empty()
    }

    /// Get a character at the given byte offset.
    ///
    /// `str::get` yields `None` rather than panicking on a mid-character
    /// offset, so a release build degrades instead of aborting; the assertion
    /// makes the caller's mistake loud in tests.
    pub fn char_at(&self, offset: usize) -> Option<char> {
        debug_assert!(
            self.content
                .is_char_boundary(offset.min(self.content.len())),
            "char_at({}) is inside a character of {:?}",
            offset,
            self.content
        );
        self.content.get(offset..).and_then(|s| s.chars().next())
    }

    /// Round a byte offset down to a character boundary within this line.
    pub fn snap_column(&self, offset: usize) -> usize {
        floor_char_boundary(&self.content, offset)
    }

    /// Byte offset of the character after the one at `offset`.
    pub fn next_char_offset(&self, offset: usize) -> Option<usize> {
        next_char_boundary(&self.content, offset)
    }

    /// Byte offset of the character before the one at `offset`.
    pub fn prev_char_offset(&self, offset: usize) -> Option<usize> {
        prev_char_boundary(&self.content, offset)
    }

    /// Get the byte offset of the last character, or 0 if empty.
    pub fn last_char_offset(&self) -> usize {
        if self.content.is_empty() {
            0
        } else {
            // Find the start of the last character
            let mut offset = self.content.len();
            while offset > 0 && !self.content.is_char_boundary(offset - 1) {
                offset -= 1;
            }
            if offset > 0 {
                offset - 1
            } else {
                0
            }
        }
    }

    /// Get byte offset of the nth character (0-indexed).
    pub fn char_to_byte(&self, char_idx: usize) -> Option<usize> {
        self.content
            .char_indices()
            .nth(char_idx)
            .map(|(offset, _)| offset)
    }

    /// Get character index from byte offset.
    pub fn byte_to_char(&self, byte_offset: usize) -> usize {
        self.content[..byte_offset.min(self.content.len())]
            .chars()
            .count()
    }

    /// Find character index for byte offset in line content.
    ///
    /// Returns the index of the first character whose byte offset is >= the given byte offset,
    /// or the total character count if byte_offset is past the end.
    pub fn char_index_at_byte(&self, byte_offset: usize) -> usize {
        char_index_at_byte(&self.content, byte_offset)
    }

    /// Count characters in the line.
    pub fn char_count(&self) -> usize {
        self.content.chars().count()
    }

    /// Insert a character at the given byte offset.
    pub fn insert_char(&mut self, offset: usize, c: char) {
        debug_assert!(self
            .content
            .is_char_boundary(offset.min(self.content.len())));
        self.content.insert(self.snap_column(offset), c);
    }

    /// Insert a string at the given byte offset.
    pub fn insert_str(&mut self, offset: usize, s: &str) {
        debug_assert!(self
            .content
            .is_char_boundary(offset.min(self.content.len())));
        let at = self.snap_column(offset);
        self.content.insert_str(at, s);
    }

    /// Delete a character at the given byte offset.
    /// Returns the deleted character.
    pub fn delete_char(&mut self, offset: usize) -> Option<char> {
        debug_assert!(self
            .content
            .is_char_boundary(offset.min(self.content.len())));
        let at = self.snap_column(offset);
        if at < self.content.len() {
            Some(self.content.remove(at))
        } else {
            None
        }
    }

    /// Delete a range of bytes.
    pub fn delete_range(&mut self, start: usize, end: usize) -> String {
        // Snap outward so a range can never bisect a character.
        let end = ceil_char_boundary(&self.content, end);
        let start = floor_char_boundary(&self.content, start.min(end));
        self.content.drain(start..end).collect()
    }

    /// Append a string to the line.
    pub fn push_str(&mut self, s: &str) {
        self.content.push_str(s);
    }

    /// Append a character to the line.
    pub fn push(&mut self, c: char) {
        self.content.push(c);
    }

    /// Split the line at the given byte offset.
    /// Returns the portion after the split point.
    pub fn split_off(&mut self, offset: usize) -> Line {
        debug_assert!(self
            .content
            .is_char_boundary(offset.min(self.content.len())));
        let at = self.snap_column(offset);
        let rest = self.content.split_off(at);
        Line { content: rest }
    }

    /// Join another line to the end of this one.
    pub fn join(&mut self, other: &Line) {
        self.content.push_str(&other.content);
    }

    /// Find the first non-blank character offset.
    pub fn first_non_blank(&self) -> usize {
        // On an all-blank line there is no non-blank to move to, so the
        // cursor stays on the last character.  Falling back to 0 sent `^` to
        // the *start* of a line of indentation, which is where `0` goes.
        self.content
            .find(|c: char| !c.is_whitespace())
            .unwrap_or_else(|| self.last_char_offset())
    }

    /// Check if character at offset is a word character.
    pub fn is_word_char(&self, offset: usize) -> bool {
        self.char_at(offset)
            .map(|c| c.is_alphanumeric() || c == '_')
            .unwrap_or(false)
    }

    /// Check if character at offset is a blank.
    pub fn is_blank(&self, offset: usize) -> bool {
        self.char_at(offset)
            .map(|c| c == ' ' || c == '\t')
            .unwrap_or(false)
    }

    /// Check if the line contains only blanks.
    pub fn is_blank_line(&self) -> bool {
        self.content.chars().all(|c| c == ' ' || c == '\t')
    }

    /// Get an iterator over characters with their byte offsets.
    pub fn char_indices(&self) -> impl Iterator<Item = (usize, char)> + '_ {
        self.content.char_indices()
    }

    /// Get an iterator over characters.
    pub fn chars(&self) -> impl Iterator<Item = char> + '_ {
        self.content.chars()
    }

    /// Replace the entire content.
    pub fn set_content(&mut self, content: &str) {
        self.content = content.to_string();
    }

    /// Clear the line.
    pub fn clear(&mut self) {
        self.content.clear();
    }
}

impl Default for Line {
    fn default() -> Self {
        Self::new()
    }
}

impl From<String> for Line {
    fn from(s: String) -> Self {
        Line::from_string_trimmed(&s)
    }
}

impl From<&str> for Line {
    fn from(s: &str) -> Self {
        Line::from_string_trimmed(s)
    }
}

impl std::fmt::Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.content)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_line() {
        let line = Line::new();
        assert!(line.is_empty());
        assert_eq!(line.len(), 0);
    }

    #[test]
    fn test_from_str() {
        let line = Line::from("hello world");
        assert_eq!(line.content(), "hello world");
        assert_eq!(line.len(), 11);
    }

    #[test]
    fn test_from_str_strips_newline() {
        let line = Line::from("hello\n");
        assert_eq!(line.content(), "hello");

        let line = Line::from("hello\r\n");
        assert_eq!(line.content(), "hello");
    }

    #[test]
    fn test_char_at() {
        let line = Line::from("hello");
        assert_eq!(line.char_at(0), Some('h'));
        assert_eq!(line.char_at(4), Some('o'));
        assert_eq!(line.char_at(5), None);
    }

    #[test]
    fn test_insert_char() {
        let mut line = Line::from("hllo");
        line.insert_char(1, 'e');
        assert_eq!(line.content(), "hello");
    }

    #[test]
    fn test_delete_char() {
        let mut line = Line::from("hello");
        let deleted = line.delete_char(1);
        assert_eq!(deleted, Some('e'));
        assert_eq!(line.content(), "hllo");
    }

    #[test]
    fn test_split_off() {
        let mut line = Line::from("hello world");
        let rest = line.split_off(6);
        assert_eq!(line.content(), "hello ");
        assert_eq!(rest.content(), "world");
    }

    #[test]
    fn test_join() {
        let mut line1 = Line::from("hello ");
        let line2 = Line::from("world");
        line1.join(&line2);
        assert_eq!(line1.content(), "hello world");
    }

    #[test]
    fn test_first_non_blank() {
        let line = Line::from("   hello");
        assert_eq!(line.first_non_blank(), 3);

        let line = Line::from("hello");
        assert_eq!(line.first_non_blank(), 0);

        // No non-blank: the cursor stays on the last character rather than
        // jumping to column 0, which is where `0` goes and `^` does not.
        let line = Line::from("   ");
        assert_eq!(line.first_non_blank(), 2);

        let line = Line::from("");
        assert_eq!(line.first_non_blank(), 0);
    }

    #[test]
    fn test_is_word_char() {
        let line = Line::from("hello_world 123");
        assert!(line.is_word_char(0)); // 'h'
        assert!(line.is_word_char(5)); // '_'
        assert!(!line.is_word_char(11)); // ' '
        assert!(line.is_word_char(12)); // '1'
    }

    #[test]
    fn test_blank_line() {
        assert!(Line::from("").is_blank_line());
        assert!(Line::from("   ").is_blank_line());
        assert!(Line::from("\t\t").is_blank_line());
        assert!(!Line::from("  a  ").is_blank_line());
    }

    #[test]
    fn test_unicode() {
        let line = Line::from("héllo");
        assert_eq!(line.char_count(), 5);
        assert_eq!(line.len(), 6); // 'é' is 2 bytes in UTF-8
        assert_eq!(line.char_at(0), Some('h'));
        assert_eq!(line.char_at(1), Some('é'));
    }

    #[test]
    fn test_char_index_at_byte() {
        // ASCII string
        assert_eq!(char_index_at_byte("hello", 0), 0);
        assert_eq!(char_index_at_byte("hello", 2), 2);
        assert_eq!(char_index_at_byte("hello", 5), 5); // past end

        // UTF-8 string: "héllo" where 'é' is 2 bytes
        // Byte layout: h(0), é(1-2), l(3), l(4), o(5)
        let s = "héllo";
        assert_eq!(char_index_at_byte(s, 0), 0); // 'h' at byte 0
        assert_eq!(char_index_at_byte(s, 1), 1); // 'é' starts at byte 1
        assert_eq!(char_index_at_byte(s, 3), 2); // 'l' at byte 3

        // Empty string
        assert_eq!(char_index_at_byte("", 0), 0);
    }
}
