//! Screen buffer for efficient display updates.

use super::display::truncate_to_width;
use super::terminal::{Terminal, TerminalSize};
use crate::buffer::Buffer;
use crate::error::Result;
use std::io::{self, Write};

/// A row in the screen buffer.
#[derive(Debug, Clone)]
struct ScreenRow {
    /// Content of the row.
    content: String,
    /// Whether this row needs to be redrawn.
    dirty: bool,
}

impl ScreenRow {
    fn new() -> Self {
        Self {
            content: String::new(),
            dirty: true,
        }
    }

    fn set(&mut self, content: &str) {
        if self.content != content {
            self.content = content.to_string();
            self.dirty = true;
        }
    }
}

/// Screen buffer for managing display output.
pub struct Screen {
    /// Screen rows.
    rows: Vec<ScreenRow>,
    /// Terminal size.
    size: TerminalSize,
    /// First line of buffer shown on screen (1-indexed).
    top_line: usize,
    /// Offset into wrapped rows of top_line (0-indexed).
    /// When a buffer line wraps across multiple display rows,
    /// this indicates which wrapped row to start displaying from.
    top_line_offset: usize,
    /// Message to display on status line.
    message: String,
    /// Whether message is an error.
    message_is_error: bool,
    /// Tab stop width.
    tabstop: usize,
}

impl Screen {
    /// Create a new screen buffer.
    pub fn new(size: TerminalSize) -> Self {
        let mut rows = Vec::with_capacity(size.rows as usize);
        for _ in 0..size.rows {
            rows.push(ScreenRow::new());
        }
        Self {
            rows,
            size,
            top_line: 1,
            top_line_offset: 0,
            message: String::new(),
            message_is_error: false,
            tabstop: 8,
        }
    }

    /// Get the terminal size.
    pub fn size(&self) -> TerminalSize {
        self.size
    }

    /// Resize the screen buffer.
    pub fn resize(&mut self, size: TerminalSize) {
        self.size = size;
        self.rows.resize_with(size.rows as usize, ScreenRow::new);
        for row in &mut self.rows {
            row.dirty = true;
        }
    }

    /// Get the top line number (1-indexed).
    pub fn top_line(&self) -> usize {
        self.top_line
    }

    /// Get the wrapped row offset within the top line (0-indexed).
    pub fn top_line_offset(&self) -> usize {
        self.top_line_offset
    }

    /// Set the top line.
    pub fn set_top_line(&mut self, line: usize) {
        if self.top_line != line {
            self.top_line = line.max(1);
            self.top_line_offset = 0;
            self.mark_all_dirty();
        }
    }

    /// Get number of text rows (excluding status line).
    pub fn text_rows(&self) -> usize {
        self.size.rows.saturating_sub(1) as usize
    }

    /// Set tab stop width.
    pub fn set_tabstop(&mut self, tabstop: usize) {
        if self.tabstop != tabstop {
            self.tabstop = tabstop.max(1);
            self.mark_all_dirty();
        }
    }

    /// Get tab stop width.
    pub fn tabstop(&self) -> usize {
        self.tabstop
    }

    /// Mark all rows as dirty.
    pub fn mark_all_dirty(&mut self) {
        for row in &mut self.rows {
            row.dirty = true;
        }
    }

    /// Set a message to display.
    pub fn set_message(&mut self, msg: &str, is_error: bool) {
        self.message = msg.to_string();
        self.message_is_error = is_error;
    }

    /// Clear the message.
    pub fn clear_message(&mut self) {
        self.message.clear();
        self.message_is_error = false;
    }

    /// Scroll to make a line visible, accounting for wrapped lines.
    pub fn scroll_to_line(&mut self, line: usize, buffer: &Buffer) {
        let text_rows = self.text_rows();
        if text_rows == 0 {
            return;
        }

        let cols = self.size.cols as usize;

        // If target line is above the current top line, scroll up to show it
        if line < self.top_line {
            self.top_line = line;
            self.top_line_offset = 0;
            self.mark_all_dirty();
            return;
        }

        // Compute display row position of the target line relative to top_line
        let mut display_row = 0;
        let mut current_line = self.top_line;

        // Skip wrapped rows from the top line offset
        if let Some(top_content) = buffer.line(self.top_line) {
            let wrapped_count = self.compute_wrapped_row_count(top_content.content(), cols);
            if self.top_line_offset < wrapped_count {
                display_row = wrapped_count - self.top_line_offset;
            }
            current_line += 1;
        }

        // Accumulate display rows until we reach the target line
        while current_line < line && current_line <= buffer.line_count() {
            if let Some(content) = buffer.line(current_line) {
                display_row += self.compute_wrapped_row_count(content.content(), cols);
            } else {
                display_row += 1;
            }
            current_line += 1;
        }

        // Get wrapped row count for the target line
        let target_wrapped_count = if let Some(content) = buffer.line(line) {
            self.compute_wrapped_row_count(content.content(), cols)
        } else {
            1
        };

        // Check if the target line fits in the visible window
        if display_row + target_wrapped_count <= text_rows {
            // Target line is fully visible, no scroll needed
            return;
        }

        // Target line is below the visible area; scroll down
        // Position target line at the bottom of the screen
        let mut new_top_line = line;
        let mut new_top_offset = 0;
        let mut rows_needed = target_wrapped_count;

        // Walk backwards from target line to fill the screen
        while rows_needed < text_rows && new_top_line > 1 {
            let prev_line = new_top_line - 1;
            if let Some(content) = buffer.line(prev_line) {
                let prev_wrapped = self.compute_wrapped_row_count(content.content(), cols);
                if rows_needed + prev_wrapped <= text_rows {
                    rows_needed += prev_wrapped;
                    new_top_line = prev_line;
                } else {
                    // This line doesn't fully fit; show partial
                    new_top_offset = prev_wrapped - (text_rows - rows_needed);
                    new_top_line = prev_line;
                    break;
                }
            } else {
                break;
            }
        }

        if self.top_line != new_top_line || self.top_line_offset != new_top_offset {
            self.top_line = new_top_line.max(1);
            self.top_line_offset = new_top_offset;
            self.mark_all_dirty();
        }
    }

    /// Expand tabs and control characters for display.
    /// Returns the expanded string, potentially longer than max_cols.
    pub fn expand_line(&self, line: &str, max_cols: usize) -> String {
        let mut result = String::new();
        let mut col = 0;

        for c in line.chars() {
            if col >= max_cols {
                break;
            }

            match c {
                '\t' => {
                    let spaces = self.tabstop - (col % self.tabstop);
                    let spaces = spaces.min(max_cols - col);
                    for _ in 0..spaces {
                        result.push(' ');
                    }
                    col += spaces;
                }
                c if c.is_control() => {
                    // Display control chars as ^X
                    if col + 2 <= max_cols {
                        result.push('^');
                        result.push((c as u8 ^ 0x40) as char);
                        col += 2;
                    } else {
                        break;
                    }
                }
                c => {
                    result.push(c);
                    col += 1;
                }
            }
        }

        result
    }

    /// Compute how many wrapped display rows a buffer line occupies.
    fn compute_wrapped_row_count(&self, line: &str, max_cols: usize) -> usize {
        if max_cols == 0 {
            return 1;
        }

        let mut row_count = 0;
        let mut col = 0;

        for c in line.chars() {
            let char_width = match c {
                '\t' => self.tabstop - (col % self.tabstop),
                c if c.is_control() => 2,
                _ => 1,
            };

            // If adding this character would exceed the line width, wrap
            if col + char_width > max_cols && col > 0 {
                row_count += 1;
                col = 0;
            }

            col += char_width;
        }

        // Count the final row
        row_count + 1
    }

    /// Expand and wrap a line into multiple display rows.
    /// Returns a vector of strings, each representing one wrapped display row.
    pub fn expand_and_wrap_line(&self, line: &str, max_cols: usize) -> Vec<String> {
        if max_cols == 0 {
            return vec![String::new()];
        }

        let mut wrapped_rows = Vec::new();
        let mut current_row = String::new();
        let mut col = 0;

        for c in line.chars() {
            let char_width = match c {
                '\t' => self.tabstop - (col % self.tabstop),
                c if c.is_control() => 2, // ^X format
                _ => 1,
            };

            // If adding this character would exceed the line width, wrap
            if col + char_width > max_cols && col > 0 {
                wrapped_rows.push(current_row.clone());
                current_row.clear();
                col = 0;
            }

            // Add the character to the current row
            match c {
                '\t' => {
                    let spaces = self.tabstop - (col % self.tabstop);
                    for _ in 0..spaces {
                        current_row.push(' ');
                    }
                    col += spaces;
                }
                c if c.is_control() => {
                    current_row.push('^');
                    current_row.push((c as u8 ^ 0x40) as char);
                    col += 2;
                }
                c => {
                    current_row.push(c);
                    col += 1;
                }
            }
        }

        // Add the final row if it has content
        if !current_row.is_empty() || wrapped_rows.is_empty() {
            wrapped_rows.push(current_row);
        }

        wrapped_rows
    }

    /// Update the screen from the buffer.
    /// Handles line wrapping per POSIX requirements.
    pub fn update_from_buffer(&mut self, buffer: &Buffer) {
        let text_rows = self.text_rows();
        let cols = self.size.cols as usize;

        let mut screen_row = 0;
        let mut buffer_line = self.top_line;
        let mut skip_wrapped_rows = self.top_line_offset;

        while screen_row < text_rows {
            if buffer_line <= buffer.line_count() {
                if let Some(line) = buffer.line(buffer_line) {
                    // Expand and wrap the line
                    let wrapped = self.expand_and_wrap_line(line.content(), cols);

                    // Skip wrapped rows if we're starting mid-line
                    for wrapped_row in wrapped.iter() {
                        if skip_wrapped_rows > 0 {
                            skip_wrapped_rows -= 1;
                            continue;
                        }
                        if screen_row >= text_rows {
                            break;
                        }
                        self.rows[screen_row].set(wrapped_row);
                        screen_row += 1;
                    }
                    buffer_line += 1;
                } else {
                    self.rows[screen_row].set("~");
                    screen_row += 1;
                    buffer_line += 1;
                }
            } else {
                // Past end of buffer - show tilde
                self.rows[screen_row].set("~");
                screen_row += 1;
                buffer_line += 1;
            }
        }
    }

    /// Render the screen to the terminal.
    pub fn render(&mut self, _terminal: &Terminal, cursor_row: u16, cursor_col: u16) -> Result<()> {
        let mut output = String::new();

        // Hide cursor during update
        output.push_str("\x1b[?25l");

        // Update dirty rows
        for (row_idx, row) in self.rows.iter_mut().enumerate() {
            if row.dirty {
                // Move to row
                output.push_str(&format!("\x1b[{};1H", row_idx + 1));
                // Clear line
                output.push_str("\x1b[K");
                // Write content
                output.push_str(&row.content);
                row.dirty = false;
            }
        }

        // Draw status line (last row)
        let status_row = self.size.rows;
        output.push_str(&format!("\x1b[{};1H", status_row));
        output.push_str("\x1b[K");
        if !self.message.is_empty() {
            // Truncate message to fit (using UTF-8 safe truncation)
            let max_len = self.size.cols as usize;
            let msg = truncate_to_width(&self.message, max_len, self.tabstop);
            output.push_str(&msg);
        }

        // Position cursor
        output.push_str(&format!("\x1b[{};{}H", cursor_row, cursor_col));

        // Show cursor
        output.push_str("\x1b[?25h");

        // Write all at once
        let mut stdout = io::stdout();
        stdout.write_all(output.as_bytes())?;
        stdout.flush()?;

        Ok(())
    }

    /// Render a full refresh (all lines).
    pub fn render_full(
        &mut self,
        terminal: &Terminal,
        cursor_row: u16,
        cursor_col: u16,
    ) -> Result<()> {
        self.mark_all_dirty();
        self.render(terminal, cursor_row, cursor_col)
    }

    /// Calculate display column from buffer column (handling tabs).
    pub fn buffer_col_to_display_col(&self, line: &str, buffer_col: usize) -> usize {
        let mut display_col = 0;
        let mut byte_pos = 0;

        for c in line.chars() {
            if byte_pos >= buffer_col {
                break;
            }
            match c {
                '\t' => {
                    display_col += self.tabstop - (display_col % self.tabstop);
                }
                c if c.is_control() => {
                    display_col += 2; // ^X
                }
                _ => {
                    display_col += 1;
                }
            }
            byte_pos += c.len_utf8();
        }

        display_col
    }

    /// Calculate buffer column from display column.
    pub fn display_col_to_buffer_col(&self, line: &str, display_col: usize) -> usize {
        let mut current_display = 0;
        let mut byte_pos = 0;

        for c in line.chars() {
            if current_display >= display_col {
                break;
            }
            let char_width = match c {
                '\t' => self.tabstop - (current_display % self.tabstop),
                c if c.is_control() => 2,
                _ => 1,
            };
            current_display += char_width;
            byte_pos += c.len_utf8();
        }

        byte_pos
    }
}

impl Default for Screen {
    fn default() -> Self {
        Self::new(TerminalSize::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expand_line_simple() {
        let screen = Screen::new(TerminalSize { rows: 24, cols: 80 });
        let result = screen.expand_line("hello", 80);
        assert_eq!(result, "hello");
    }

    #[test]
    fn test_expand_line_tabs() {
        let screen = Screen::new(TerminalSize { rows: 24, cols: 80 });
        let result = screen.expand_line("a\tb", 80);
        // Default tabstop is 8, so 'a' at col 0, tab fills cols 1-7, 'b' at col 8
        assert_eq!(result, "a       b");
    }

    #[test]
    fn test_buffer_col_to_display_col() {
        let screen = Screen::new(TerminalSize { rows: 24, cols: 80 });
        // "a\tb" - 'a' is at buffer col 0, '\t' at 1, 'b' at 2
        let display = screen.buffer_col_to_display_col("a\tb", 2);
        // After 'a' (1 col) and tab (7 cols to reach col 8), we're at display col 8
        assert_eq!(display, 8);
    }

    #[test]
    fn test_scroll_to_line() {
        use crate::buffer::Buffer;

        let mut screen = Screen::new(TerminalSize { rows: 10, cols: 80 });
        // text_rows = 9 (minus status line)

        // Create a buffer with 100 lines
        let mut lines = String::new();
        for i in 1..=100 {
            if i > 1 {
                lines.push('\n');
            }
            lines.push_str(&format!("Line {}", i));
        }
        let buffer = Buffer::from_text(&lines);

        // Line 5 should be visible without scrolling
        screen.scroll_to_line(5, &buffer);
        assert_eq!(screen.top_line(), 1);

        // Line 15 requires scrolling
        screen.scroll_to_line(15, &buffer);
        assert_eq!(screen.top_line(), 7); // 15 - 9 + 1 = 7
    }

    #[test]
    fn test_expand_and_wrap_line_short() {
        let screen = Screen::new(TerminalSize { rows: 24, cols: 80 });
        let wrapped = screen.expand_and_wrap_line("hello", 80);
        assert_eq!(wrapped.len(), 1);
        assert_eq!(wrapped[0], "hello");
    }

    #[test]
    fn test_expand_and_wrap_line_long() {
        let screen = Screen::new(TerminalSize { rows: 24, cols: 10 });
        let long_line = "this is a very long line that should wrap";
        let wrapped = screen.expand_and_wrap_line(long_line, 10);
        // Should wrap into multiple rows
        assert!(wrapped.len() > 1);
        // First row should be 10 chars
        assert_eq!(wrapped[0], "this is a ");
        assert_eq!(wrapped[1], "very long ");
    }

    #[test]
    fn test_expand_and_wrap_line_with_tabs() {
        let screen = Screen::new(TerminalSize { rows: 24, cols: 10 });
        let line_with_tabs = "ab\tcd\tef";
        let wrapped = screen.expand_and_wrap_line(line_with_tabs, 10);
        // Tab at position 2 expands to 6 spaces (to column 8)
        // So: "ab      cd" (10 chars) wraps, then tab after "cd" continues
        assert!(wrapped.len() >= 2);
    }

    #[test]
    fn test_expand_and_wrap_empty_line() {
        let screen = Screen::new(TerminalSize { rows: 24, cols: 80 });
        let wrapped = screen.expand_and_wrap_line("", 80);
        assert_eq!(wrapped.len(), 1);
        assert_eq!(wrapped[0], "");
    }
}
