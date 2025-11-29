//! Edit buffer implementation using a line-based structure.

use super::line::Line;
use super::position::{BufferMode, Position, Range};
use crate::error::{Result, ViError};

/// The edit buffer containing the text being edited.
#[derive(Debug)]
pub struct Buffer {
    /// Lines in the buffer.
    lines: Vec<Line>,
    /// Current cursor position.
    cursor: Position,
    /// Whether the buffer has been modified.
    modified: bool,
}

impl Buffer {
    /// Create an empty buffer.
    pub fn new() -> Self {
        Self {
            lines: Vec::new(),
            cursor: Position::start(),
            modified: false,
        }
    }

    /// Create a buffer from text content.
    pub fn from_text(text: &str) -> Self {
        let lines: Vec<Line> = if text.is_empty() {
            Vec::new()
        } else {
            text.lines().map(Line::from).collect()
        };

        Self {
            lines,
            cursor: Position::start(),
            modified: false,
        }
    }

    /// Check if buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.lines.is_empty()
    }

    /// Get number of lines.
    pub fn line_count(&self) -> usize {
        self.lines.len()
    }

    /// Get the current cursor position.
    pub fn cursor(&self) -> Position {
        self.cursor
    }

    /// Get the current line number (1-indexed).
    pub fn current_line_num(&self) -> usize {
        self.cursor.line
    }

    /// Get the current column (0-indexed byte offset).
    pub fn current_column(&self) -> usize {
        self.cursor.column
    }

    /// Check if buffer is modified.
    pub fn is_modified(&self) -> bool {
        self.modified
    }

    /// Mark buffer as saved.
    pub fn mark_saved(&mut self) {
        self.modified = false;
    }

    /// Mark buffer as modified.
    pub fn mark_modified(&mut self) {
        self.modified = true;
    }

    /// Get a reference to a line (1-indexed).
    pub fn line(&self, line_num: usize) -> Option<&Line> {
        if line_num == 0 || line_num > self.lines.len() {
            None
        } else {
            Some(&self.lines[line_num - 1])
        }
    }

    /// Get a mutable reference to a line (1-indexed).
    pub fn line_mut(&mut self, line_num: usize) -> Option<&mut Line> {
        if line_num == 0 || line_num > self.lines.len() {
            None
        } else {
            Some(&mut self.lines[line_num - 1])
        }
    }

    /// Get the current line.
    pub fn current_line(&self) -> Option<&Line> {
        self.line(self.cursor.line)
    }

    /// Get the current line mutably.
    pub fn current_line_mut(&mut self) -> Option<&mut Line> {
        let line_num = self.cursor.line;
        self.line_mut(line_num)
    }

    /// Get the character under the cursor.
    pub fn char_at_cursor(&self) -> Option<char> {
        self.current_line()
            .and_then(|line| line.char_at(self.cursor.column))
    }

    /// Set cursor position, clamping to valid bounds.
    pub fn set_cursor(&mut self, pos: Position) {
        self.cursor.line = pos.line.clamp(1, self.line_count().max(1));

        if let Some(line) = self.line(self.cursor.line) {
            let max_col = if line.is_empty() {
                0
            } else {
                line.last_char_offset()
            };
            self.cursor.column = pos.column.min(max_col);
        } else {
            self.cursor.column = 0;
        }
    }

    /// Set cursor line (1-indexed), clamping to valid bounds.
    pub fn set_line(&mut self, line_num: usize) {
        self.cursor.line = line_num.clamp(1, self.line_count().max(1));
        self.clamp_column();
    }

    /// Set cursor column (0-indexed byte offset), clamping to valid bounds.
    /// In normal mode, cursor must be ON a character (not past end of line).
    pub fn set_column(&mut self, column: usize) {
        self.cursor.column = column;
        self.clamp_column();
    }

    /// Set cursor column for insert mode, allowing cursor to be past end of line.
    /// In insert mode, cursor can be at line.len() (after the last character)
    /// to allow appending at end of line.
    pub fn set_column_for_insert(&mut self, column: usize) {
        if let Some(line) = self.line(self.cursor.line) {
            // In insert mode, allow cursor at line.len() (one past last char)
            self.cursor.column = column.min(line.len());
        } else {
            self.cursor.column = 0;
        }
    }

    /// Clamp column to valid position on current line.
    /// For normal mode: cursor must be ON a character.
    fn clamp_column(&mut self) {
        if let Some(line) = self.line(self.cursor.line) {
            let max_col = if line.is_empty() {
                0
            } else {
                line.last_char_offset()
            };
            self.cursor.column = self.cursor.column.min(max_col);
        } else {
            self.cursor.column = 0;
        }
    }

    /// Move cursor to first non-blank on current line.
    pub fn move_to_first_non_blank(&mut self) {
        if let Some(line) = self.line(self.cursor.line) {
            self.cursor.column = line.first_non_blank();
        }
    }

    /// Insert a character at the cursor position.
    pub fn insert_char(&mut self, c: char) {
        if self.is_empty() {
            self.lines.push(Line::new());
            self.cursor = Position::new(1, 0);
        }

        let col = self.cursor.column;
        let line_idx = self.cursor.line - 1;
        if line_idx < self.lines.len() {
            self.lines[line_idx].insert_char(col, c);
            self.cursor.column += c.len_utf8();
            self.modified = true;
        }
    }

    /// Insert a string at the cursor position.
    pub fn insert_str(&mut self, s: &str) {
        for c in s.chars() {
            if c == '\n' {
                self.insert_newline();
            } else {
                self.insert_char(c);
            }
        }
    }

    /// Insert a newline at the cursor, splitting the line.
    pub fn insert_newline(&mut self) {
        if self.is_empty() {
            self.lines.push(Line::new());
            self.lines.push(Line::new());
            self.cursor = Position::new(2, 0);
            self.modified = true;
            return;
        }

        let line_idx = self.cursor.line - 1;
        let col = self.cursor.column;

        // Split the current line
        let new_line = self.lines[line_idx].split_off(col);

        // Insert the new line after
        self.lines.insert(line_idx + 1, new_line);

        // Move cursor to start of new line
        self.cursor.line += 1;
        self.cursor.column = 0;
        self.modified = true;
    }

    /// Delete the character at the cursor.
    pub fn delete_char(&mut self) -> Option<char> {
        if self.is_empty() {
            return None;
        }

        let col = self.cursor.column;
        let line_idx = self.cursor.line - 1;
        if line_idx >= self.lines.len() {
            return None;
        }

        let line_len = self.lines[line_idx].len();
        if col < line_len {
            let deleted = self.lines[line_idx].delete_char(col);
            self.modified = true;
            self.clamp_column();
            deleted
        } else {
            None
        }
    }

    /// Delete the character before the cursor (backspace).
    pub fn delete_char_before(&mut self) -> Option<char> {
        if self.is_empty() {
            return None;
        }

        if self.cursor.column > 0 {
            // Delete within line
            let line_idx = self.cursor.line - 1;
            if line_idx >= self.lines.len() {
                return None;
            }

            // Find start of previous character
            let content = self.lines[line_idx].content().to_string();
            let mut offset = self.cursor.column;
            while offset > 0 && !content.is_char_boundary(offset - 1) {
                offset -= 1;
            }
            offset = offset.saturating_sub(1);
            self.cursor.column = offset;
            let deleted = self.lines[line_idx].delete_char(offset);
            self.modified = true;
            deleted
        } else if self.cursor.line > 1 {
            // Join with previous line
            let current_idx = self.cursor.line - 1;
            let current_line = self.lines.remove(current_idx);
            let prev_line = &mut self.lines[current_idx - 1];
            let join_col = prev_line.len();
            prev_line.join(&current_line);
            self.cursor.line -= 1;
            self.cursor.column = join_col;
            self.modified = true;
            Some('\n')
        } else {
            None
        }
    }

    /// Delete a line (1-indexed).
    pub fn delete_line(&mut self, line_num: usize) -> Option<Line> {
        if line_num == 0 || line_num > self.lines.len() {
            return None;
        }

        let line = self.lines.remove(line_num - 1);
        self.modified = true;

        // Adjust cursor
        if self.is_empty() {
            self.cursor = Position::start();
        } else if self.cursor.line > self.lines.len() {
            self.cursor.line = self.lines.len();
            self.clamp_column();
        } else {
            self.clamp_column();
        }

        Some(line)
    }

    /// Delete a range of lines (1-indexed, inclusive).
    pub fn delete_lines(&mut self, start: usize, end: usize) -> Vec<Line> {
        let start = start.max(1);
        let end = end.min(self.lines.len());
        if start > end {
            return Vec::new();
        }

        let deleted: Vec<Line> = self.lines.drain((start - 1)..end).collect();
        self.modified = true;

        // Adjust cursor
        if self.is_empty() {
            self.cursor = Position::start();
        } else if self.cursor.line > self.lines.len() {
            self.cursor.line = self.lines.len();
            self.clamp_column();
        } else if self.cursor.line >= start {
            self.cursor.line = start.min(self.lines.len());
            self.clamp_column();
        }

        deleted
    }

    /// Insert a line after the given line number (0 = before first line).
    pub fn insert_line_after(&mut self, line_num: usize, line: Line) {
        let idx = line_num.min(self.lines.len());
        self.lines.insert(idx, line);
        self.modified = true;
    }

    /// Insert an empty line below the given line number (1-indexed).
    pub fn insert_line_below(&mut self, line_num: usize) -> Result<()> {
        if line_num == 0 || line_num > self.lines.len() {
            return Err(ViError::InvalidLine(line_num));
        }
        self.lines.insert(line_num, Line::new());
        self.modified = true;
        Ok(())
    }

    /// Insert an empty line above the given line number (1-indexed).
    pub fn insert_line_above(&mut self, line_num: usize) -> Result<()> {
        if line_num == 0 || line_num > self.lines.len() {
            return Err(ViError::InvalidLine(line_num));
        }
        self.lines.insert(line_num - 1, Line::new());
        self.modified = true;
        Ok(())
    }

    /// Append a line at the end.
    pub fn append_line(&mut self, line: Line) {
        self.lines.push(line);
        self.modified = true;
    }

    /// Replace a line's content.
    pub fn replace_line(&mut self, line_num: usize, content: &str) -> Result<()> {
        if let Some(line) = self.line_mut(line_num) {
            line.set_content(content);
            self.modified = true;
            Ok(())
        } else {
            Err(ViError::InvalidAddress(format!(
                "line {} out of range",
                line_num
            )))
        }
    }

    /// Clear the current line (make it empty).
    pub fn clear_line(&mut self) {
        if let Some(line) = self.line_mut(self.cursor.line) {
            line.set_content("");
            self.cursor.column = 0;
            self.modified = true;
        }
    }

    /// Delete from cursor to end of current line.
    pub fn delete_to_end_of_line(&mut self) -> String {
        let cursor_line = self.cursor.line;
        let cursor_col = self.cursor.column;
        if let Some(line) = self.line_mut(cursor_line) {
            let content = line.content().to_string();
            if cursor_col < content.len() {
                let deleted = content[cursor_col..].to_string();
                line.set_content(&content[..cursor_col]);
                self.modified = true;
                return deleted;
            }
        }
        String::new()
    }

    /// Get text in a range.
    pub fn get_text(&self, range: Range) -> String {
        if self.is_empty() {
            return String::new();
        }

        match range.mode {
            BufferMode::Line => {
                use std::fmt::Write;
                let start = range.start_line().max(1);
                let end = range.end_line().min(self.lines.len());
                self.lines[(start - 1)..end]
                    .iter()
                    .fold(String::new(), |mut acc, l| {
                        writeln!(acc, "{}", l.content()).unwrap();
                        acc
                    })
            }
            BufferMode::Character => {
                if range.start.line == range.end.line {
                    // Single line
                    if let Some(line) = self.line(range.start.line) {
                        let start = range.start.column.min(line.len());
                        let end = range.end.column.min(line.len());
                        line.content()[start..end].to_string()
                    } else {
                        String::new()
                    }
                } else {
                    // Multiple lines
                    let mut result = String::new();

                    // First line (from column to end)
                    if let Some(line) = self.line(range.start.line) {
                        let start = range.start.column.min(line.len());
                        result.push_str(&line.content()[start..]);
                        result.push('\n');
                    }

                    // Middle lines (complete)
                    for line_num in (range.start.line + 1)..range.end.line {
                        if let Some(line) = self.line(line_num) {
                            result.push_str(line.content());
                            result.push('\n');
                        }
                    }

                    // Last line (from start to column)
                    if let Some(line) = self.line(range.end.line) {
                        let end = range.end.column.min(line.len());
                        result.push_str(&line.content()[..end]);
                    }

                    result
                }
            }
        }
    }

    /// Delete text in a range.
    pub fn delete_text(&mut self, range: Range) -> String {
        if self.is_empty() {
            return String::new();
        }

        let deleted = self.get_text(range);

        match range.mode {
            BufferMode::Line => {
                self.delete_lines(range.start_line(), range.end_line());
            }
            BufferMode::Character => {
                if range.start.line == range.end.line {
                    // Single line deletion
                    if let Some(line) = self.line_mut(range.start.line) {
                        let start = range.start.column.min(line.len());
                        let end = range.end.column.min(line.len());
                        line.delete_range(start, end);
                        self.modified = true;
                    }
                } else {
                    // Multi-line deletion
                    let start_line_idx = range.start.line - 1;
                    let end_line_idx = range.end.line - 1;

                    // Get the parts to keep
                    let prefix = if let Some(line) = self.lines.get(start_line_idx) {
                        let col = range.start.column.min(line.len());
                        line.content()[..col].to_string()
                    } else {
                        String::new()
                    };

                    let suffix = if let Some(line) = self.lines.get(end_line_idx) {
                        let col = range.end.column.min(line.len());
                        line.content()[col..].to_string()
                    } else {
                        String::new()
                    };

                    // Remove the lines in range
                    self.lines.drain(start_line_idx..=end_line_idx);

                    // Insert merged line
                    let merged = Line::from(format!("{}{}", prefix, suffix));
                    self.lines.insert(start_line_idx, merged);

                    self.modified = true;
                }

                // Adjust cursor
                self.cursor = range.start;
                self.clamp_column();
            }
        }

        deleted
    }

    /// Join a line with the next line.
    pub fn join_lines(&mut self, line_num: usize, add_space: bool) -> Result<()> {
        if line_num == 0 || line_num >= self.lines.len() {
            return Err(ViError::InvalidAddress("cannot join last line".to_string()));
        }

        let next_line = self.lines.remove(line_num); // Remove line at line_num (0-indexed: line_num)
        let current = &mut self.lines[line_num - 1];

        let join_col = current.len();

        // Add space if needed
        if add_space && !current.is_empty() && !next_line.is_empty() {
            current.push(' ');
        }

        // Skip leading whitespace from next line
        let next_content = next_line.content().trim_start();
        current.push_str(next_content);

        self.cursor.column = join_col;
        self.modified = true;

        Ok(())
    }

    /// Get all text as a string.
    pub fn as_text(&self) -> String {
        if self.is_empty() {
            String::new()
        } else {
            self.lines
                .iter()
                .map(|l| l.content())
                .collect::<Vec<_>>()
                .join("\n")
                + "\n"
        }
    }

    /// Iterate over lines.
    pub fn iter_lines(&self) -> impl Iterator<Item = &Line> {
        self.lines.iter()
    }

    /// Clear the buffer.
    pub fn clear(&mut self) {
        self.lines.clear();
        self.cursor = Position::start();
        self.modified = true;
    }

    /// Replace all content with new text.
    pub fn set_text(&mut self, text: &str) {
        self.lines = if text.is_empty() {
            Vec::new()
        } else {
            text.lines().map(Line::from).collect()
        };
        self.cursor = Position::start();
        self.modified = true;
    }
}

impl Default for Buffer {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for Buffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_text())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_buffer() {
        let buf = Buffer::new();
        assert!(buf.is_empty());
        assert_eq!(buf.line_count(), 0);
    }

    #[test]
    fn test_from_text() {
        let buf = Buffer::from_text("hello\nworld");
        assert_eq!(buf.line_count(), 2);
        assert_eq!(buf.line(1).unwrap().content(), "hello");
        assert_eq!(buf.line(2).unwrap().content(), "world");
    }

    #[test]
    fn test_insert_char() {
        let mut buf = Buffer::new();
        buf.insert_char('h');
        buf.insert_char('i');
        assert_eq!(buf.line_count(), 1);
        assert_eq!(buf.line(1).unwrap().content(), "hi");
    }

    #[test]
    fn test_insert_newline() {
        let mut buf = Buffer::from_text("hello world");
        buf.set_column(5);
        buf.insert_newline();
        assert_eq!(buf.line_count(), 2);
        assert_eq!(buf.line(1).unwrap().content(), "hello");
        assert_eq!(buf.line(2).unwrap().content(), " world");
    }

    #[test]
    fn test_delete_char() {
        let mut buf = Buffer::from_text("hello");
        buf.set_column(0);
        let deleted = buf.delete_char();
        assert_eq!(deleted, Some('h'));
        assert_eq!(buf.line(1).unwrap().content(), "ello");
    }

    #[test]
    fn test_delete_char_before() {
        let mut buf = Buffer::from_text("hello");
        // Set to position 4 (on 'o'), and backspace deletes 'l' before it
        buf.set_column(4);
        let deleted = buf.delete_char_before();
        assert_eq!(deleted, Some('l'));
        assert_eq!(buf.line(1).unwrap().content(), "helo");

        // Also test from end - insert a char then backspace it
        let mut buf2 = Buffer::from_text("hi");
        buf2.set_column(2);
        buf2.insert_char('!'); // Now "hi!" with cursor at 3
        let deleted = buf2.delete_char_before();
        assert_eq!(deleted, Some('!'));
        assert_eq!(buf2.line(1).unwrap().content(), "hi");
    }

    #[test]
    fn test_delete_line() {
        let mut buf = Buffer::from_text("one\ntwo\nthree");
        let deleted = buf.delete_line(2);
        assert_eq!(deleted.unwrap().content(), "two");
        assert_eq!(buf.line_count(), 2);
        assert_eq!(buf.line(2).unwrap().content(), "three");
    }

    #[test]
    fn test_get_text_line_mode() {
        let buf = Buffer::from_text("one\ntwo\nthree");
        let range = Range::lines(Position::new(1, 0), Position::new(2, 0));
        let text = buf.get_text(range);
        assert_eq!(text, "one\ntwo\n");
    }

    #[test]
    fn test_get_text_char_mode_single_line() {
        let buf = Buffer::from_text("hello world");
        let range = Range::chars(Position::new(1, 0), Position::new(1, 5));
        let text = buf.get_text(range);
        assert_eq!(text, "hello");
    }

    #[test]
    fn test_delete_text_char_mode() {
        let mut buf = Buffer::from_text("hello world");
        let range = Range::chars(Position::new(1, 5), Position::new(1, 11));
        let deleted = buf.delete_text(range);
        assert_eq!(deleted, " world");
        assert_eq!(buf.line(1).unwrap().content(), "hello");
    }

    #[test]
    fn test_join_lines() {
        let mut buf = Buffer::from_text("hello\nworld");
        buf.join_lines(1, true).unwrap();
        assert_eq!(buf.line_count(), 1);
        assert_eq!(buf.line(1).unwrap().content(), "hello world");
    }

    #[test]
    fn test_cursor_clamping() {
        let mut buf = Buffer::from_text("hi");
        buf.set_column(100);
        assert_eq!(buf.current_column(), 1); // Clamped to last char offset
    }

    #[test]
    fn test_to_string() {
        let buf = Buffer::from_text("one\ntwo\nthree");
        assert_eq!(buf.to_string(), "one\ntwo\nthree\n");
    }

    #[test]
    fn test_modified_flag() {
        let mut buf = Buffer::from_text("hello");
        assert!(!buf.is_modified());

        buf.insert_char('!');
        assert!(buf.is_modified());

        buf.mark_saved();
        assert!(!buf.is_modified());
    }
}
