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

    /// Set the top line.
    pub fn set_top_line(&mut self, line: usize) {
        if self.top_line != line {
            self.top_line = line.max(1);
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

    /// Scroll to make a line visible.
    pub fn scroll_to_line(&mut self, line: usize, buffer_lines: usize) {
        let text_rows = self.text_rows();
        if text_rows == 0 {
            return;
        }

        // If line is above the screen
        if line < self.top_line {
            self.set_top_line(line);
        }
        // If line is below the screen
        else if line >= self.top_line + text_rows {
            self.set_top_line(line - text_rows + 1);
        }

        // Ensure we don't scroll past the end of the buffer
        if buffer_lines > 0 && self.top_line > buffer_lines {
            self.set_top_line(buffer_lines);
        }
    }

    /// Expand tabs and control characters for display.
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

    /// Update the screen from the buffer.
    pub fn update_from_buffer(&mut self, buffer: &Buffer) {
        let text_rows = self.text_rows();
        let cols = self.size.cols as usize;

        for row in 0..text_rows {
            let buffer_line = self.top_line + row;
            if buffer_line <= buffer.line_count() {
                if let Some(line) = buffer.line(buffer_line) {
                    let content = self.expand_line(line.content(), cols);
                    self.rows[row].set(&content);
                } else {
                    self.rows[row].set("~");
                }
            } else {
                // Past end of buffer - show tilde
                self.rows[row].set("~");
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
        let mut screen = Screen::new(TerminalSize { rows: 10, cols: 80 });
        // text_rows = 9 (minus status line)

        // Line 5 should be visible without scrolling
        screen.scroll_to_line(5, 100);
        assert_eq!(screen.top_line(), 1);

        // Line 15 requires scrolling
        screen.scroll_to_line(15, 100);
        assert_eq!(screen.top_line(), 7); // 15 - 9 + 1 = 7
    }
}
