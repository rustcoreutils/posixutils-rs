//! Line abstraction for the edit buffer.

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
    pub fn char_at(&self, offset: usize) -> Option<char> {
        self.content[offset..].chars().next()
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

    /// Count characters in the line.
    pub fn char_count(&self) -> usize {
        self.content.chars().count()
    }

    /// Insert a character at the given byte offset.
    pub fn insert_char(&mut self, offset: usize, c: char) {
        self.content.insert(offset, c);
    }

    /// Insert a string at the given byte offset.
    pub fn insert_str(&mut self, offset: usize, s: &str) {
        self.content.insert_str(offset, s);
    }

    /// Delete a character at the given byte offset.
    /// Returns the deleted character.
    pub fn delete_char(&mut self, offset: usize) -> Option<char> {
        if offset < self.content.len() {
            Some(self.content.remove(offset))
        } else {
            None
        }
    }

    /// Delete a range of bytes.
    pub fn delete_range(&mut self, start: usize, end: usize) -> String {
        let end = end.min(self.content.len());
        let start = start.min(end);
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
        let rest = self.content.split_off(offset);
        Line { content: rest }
    }

    /// Join another line to the end of this one.
    pub fn join(&mut self, other: &Line) {
        self.content.push_str(&other.content);
    }

    /// Find the first non-blank character offset.
    pub fn first_non_blank(&self) -> usize {
        self.content.find(|c: char| !c.is_whitespace()).unwrap_or(0)
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

        let line = Line::from("   ");
        assert_eq!(line.first_non_blank(), 0); // No non-blank found
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
}
