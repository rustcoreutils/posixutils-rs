//! Operator commands (d, c, y, >, <, !).
//!
//! Operators work on text ranges defined by motions or text objects.

use crate::buffer::{char_index_at_byte, Buffer, BufferMode, Position, Range};
use crate::error::{Result, ViError};
use crate::register::{RegisterContent, Registers};

/// Result of an operator execution.
#[derive(Debug)]
pub struct OperatorResult {
    /// Whether to enter insert mode after.
    pub enter_insert: bool,
    /// New cursor position.
    pub cursor: Position,
    /// Text that was operated on (for undo).
    pub affected_text: Option<String>,
}

impl OperatorResult {
    /// Create a simple result with just a cursor position.
    pub fn cursor(pos: Position) -> Self {
        Self {
            enter_insert: false,
            cursor: pos,
            affected_text: None,
        }
    }

    /// Set enter_insert flag.
    pub fn with_insert(mut self) -> Self {
        self.enter_insert = true;
        self
    }

    /// Set affected text.
    pub fn with_text(mut self, text: String) -> Self {
        self.affected_text = Some(text);
        self
    }
}

/// Delete operator (d).
pub fn delete(
    buffer: &mut Buffer,
    range: Range,
    registers: &mut Registers,
    register: Option<char>,
) -> Result<OperatorResult> {
    let text = buffer.get_text(range);
    if text.is_empty() {
        return Ok(OperatorResult::cursor(buffer.cursor()));
    }

    let linewise = range.mode == BufferMode::Line;
    let is_small = !linewise && !text.contains('\n');

    // Store in register
    let content = RegisterContent::new(text.clone(), linewise);
    registers.delete(register, content, is_small);

    // Perform the delete
    let cursor = if linewise {
        delete_lines(buffer, range)?
    } else {
        delete_chars(buffer, range)?
    };

    Ok(OperatorResult::cursor(cursor).with_text(text))
}

/// Change operator (c).
pub fn change(
    buffer: &mut Buffer,
    range: Range,
    registers: &mut Registers,
    register: Option<char>,
) -> Result<OperatorResult> {
    let text = buffer.get_text(range);
    let linewise = range.mode == BufferMode::Line;
    let is_small = !linewise && !text.contains('\n');

    // Store in register
    let content = RegisterContent::new(text.clone(), linewise);
    registers.delete(register, content, is_small);

    // Perform the delete and prepare for insert
    let cursor = if linewise {
        change_lines(buffer, range)?
    } else {
        delete_chars(buffer, range)?
    };

    Ok(OperatorResult::cursor(cursor).with_insert().with_text(text))
}

/// Yank operator (y).
pub fn yank(
    buffer: &Buffer,
    range: Range,
    registers: &mut Registers,
    register: Option<char>,
) -> Result<OperatorResult> {
    let text = buffer.get_text(range);
    if text.is_empty() {
        return Ok(OperatorResult::cursor(buffer.cursor()));
    }

    let linewise = range.mode == BufferMode::Line;
    let content = RegisterContent::new(text.clone(), linewise);
    registers.yank(register, content);

    // Yank doesn't move cursor (returns to start of range)
    let cursor = range.start;

    Ok(OperatorResult::cursor(cursor).with_text(text))
}

/// Shift right operator (>).
pub fn shift_right(buffer: &mut Buffer, range: Range, count: usize) -> Result<OperatorResult> {
    let start_line = range.start_line();
    let end_line = range.end_line();

    for line_num in start_line..=end_line {
        if let Some(line) = buffer.line(line_num) {
            if !line.is_empty() {
                // Add tabs for shift
                let tab_count = count.max(1);
                let indent = "\t".repeat(tab_count);
                let new_content = format!("{}{}", indent, line.content());
                buffer.replace_line(line_num, &new_content)?;
            }
        }
    }

    // Move to first non-blank of first line
    buffer.set_line(start_line);
    buffer.move_to_first_non_blank();

    Ok(OperatorResult::cursor(buffer.cursor()))
}

/// Shift left operator (<).
pub fn shift_left(buffer: &mut Buffer, range: Range, count: usize) -> Result<OperatorResult> {
    let start_line = range.start_line();
    let end_line = range.end_line();
    let shift_amount = count.max(1) * 8; // Assume shiftwidth of 8

    for line_num in start_line..=end_line {
        if let Some(line) = buffer.line(line_num) {
            let content = line.content();
            if content.is_empty() {
                continue;
            }

            // Count leading whitespace
            let mut removed = 0;
            let mut skip_chars = 0;
            for c in content.chars() {
                if removed >= shift_amount {
                    break;
                }
                if c == '\t' {
                    removed += 8;
                    skip_chars += 1;
                } else if c == ' ' {
                    removed += 1;
                    skip_chars += 1;
                } else {
                    break;
                }
            }

            if skip_chars > 0 {
                let new_content: String = content.chars().skip(skip_chars).collect();
                buffer.replace_line(line_num, &new_content)?;
            }
        }
    }

    // Move to first non-blank of first line
    buffer.set_line(start_line);
    buffer.move_to_first_non_blank();

    Ok(OperatorResult::cursor(buffer.cursor()))
}

/// Put text after cursor (p).
pub fn put_after(
    buffer: &mut Buffer,
    registers: &Registers,
    register: Option<char>,
    count: usize,
) -> Result<OperatorResult> {
    let reg_name = register.unwrap_or('"');
    let content = registers
        .get(reg_name)
        .ok_or(ViError::BufferEmpty(reg_name))?;

    let count = count.max(1);
    let cursor = if content.linewise {
        put_lines_after(buffer, &content.text, count)?
    } else {
        put_chars_after(buffer, &content.text, count)?
    };

    Ok(OperatorResult::cursor(cursor))
}

/// Put text before cursor (P).
pub fn put_before(
    buffer: &mut Buffer,
    registers: &Registers,
    register: Option<char>,
    count: usize,
) -> Result<OperatorResult> {
    let reg_name = register.unwrap_or('"');
    let content = registers
        .get(reg_name)
        .ok_or(ViError::BufferEmpty(reg_name))?;

    let count = count.max(1);
    let cursor = if content.linewise {
        put_lines_before(buffer, &content.text, count)?
    } else {
        put_chars_before(buffer, &content.text, count)?
    };

    Ok(OperatorResult::cursor(cursor))
}

// Helper functions

/// Delete lines in range.
fn delete_lines(buffer: &mut Buffer, range: Range) -> Result<Position> {
    let start = range.start_line().max(1);
    let end = range.end_line().min(buffer.line_count());

    buffer.delete_lines(start, end);

    // Position cursor at first non-blank of resulting line
    let new_line = start.min(buffer.line_count().max(1));
    buffer.set_line(new_line);
    buffer.move_to_first_non_blank();

    Ok(buffer.cursor())
}

/// Change lines - delete but leave one empty line for insert.
fn change_lines(buffer: &mut Buffer, range: Range) -> Result<Position> {
    let start = range.start_line().max(1);
    let end = range.end_line().min(buffer.line_count());

    // Delete lines but keep cursor line empty for insert
    if start == end {
        // Single line - clear it
        buffer.replace_line(start, "")?;
    } else {
        // Multiple lines - delete all but first, then clear first
        buffer.delete_lines(start + 1, end);
        buffer.replace_line(start, "")?;
    }

    buffer.set_line(start);
    buffer.set_column(0);

    Ok(buffer.cursor())
}

/// Delete characters in range.
fn delete_chars(buffer: &mut Buffer, range: Range) -> Result<Position> {
    buffer.delete_text(range);
    buffer.set_cursor(range.start);
    Ok(buffer.cursor())
}

/// Put lines after current line.
fn put_lines_after(buffer: &mut Buffer, text: &str, count: usize) -> Result<Position> {
    let after_line = buffer.cursor().line;

    for _ in 0..count {
        for (i, line) in text.lines().enumerate() {
            let insert_at = after_line + i;
            buffer.insert_line_after(insert_at, crate::buffer::Line::from(line));
        }
    }

    // Move to first non-blank of first inserted line
    buffer.set_line(after_line + 1);
    buffer.move_to_first_non_blank();

    Ok(buffer.cursor())
}

/// Put lines before current line.
fn put_lines_before(buffer: &mut Buffer, text: &str, count: usize) -> Result<Position> {
    let before_line = buffer.cursor().line;

    for _ in 0..count {
        for (i, line) in text.lines().enumerate() {
            let insert_at = before_line + i - 1;
            buffer.insert_line_after(insert_at, crate::buffer::Line::from(line));
        }
    }

    // Move to first non-blank of first inserted line
    buffer.set_line(before_line);
    buffer.move_to_first_non_blank();

    Ok(buffer.cursor())
}

/// Put characters after cursor.
fn put_chars_after(buffer: &mut Buffer, text: &str, count: usize) -> Result<Position> {
    let pos = buffer.cursor();

    // Move cursor right one position (put after)
    if let Some(line) = buffer.line(pos.line) {
        if !line.is_empty() {
            let content = line.content();
            let chars: Vec<(usize, char)> = content.char_indices().collect();
            let idx = char_index_at_byte(content, pos.column);
            if idx + 1 < chars.len() {
                buffer.set_column(chars[idx + 1].0);
            } else {
                buffer.set_column(content.len());
            }
        }
    }

    // Insert text
    for _ in 0..count {
        buffer.insert_str(text);
    }

    // Position on last inserted character
    let new_pos = buffer.cursor();
    if new_pos.column > 0 {
        buffer.set_column(new_pos.column.saturating_sub(1));
    }

    Ok(buffer.cursor())
}

/// Put characters before cursor.
fn put_chars_before(buffer: &mut Buffer, text: &str, count: usize) -> Result<Position> {
    let start_pos = buffer.cursor();

    // Insert text at cursor
    for _ in 0..count {
        buffer.insert_str(text);
    }

    // Position cursor at start of inserted text
    buffer.set_cursor(start_pos);

    Ok(buffer.cursor())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_delete_lines() {
        let mut buf = Buffer::from_text("line1\nline2\nline3");
        let mut regs = Registers::new();

        let range = Range::lines(Position::new(1, 0), Position::new(2, 0));
        delete(&mut buf, range, &mut regs, None).unwrap();

        assert_eq!(buf.line_count(), 1);
        assert_eq!(buf.line(1).unwrap().content(), "line3");

        // Check register
        let content = regs.get('1').unwrap();
        assert!(content.linewise);
        assert!(content.text.contains("line1"));
    }

    #[test]
    fn test_delete_chars() {
        let mut buf = Buffer::from_text("hello world");
        let mut regs = Registers::new();

        // Range end is exclusive, so 0..5 deletes "hello"
        let range = Range::chars(Position::new(1, 0), Position::new(1, 5));
        delete(&mut buf, range, &mut regs, None).unwrap();

        assert_eq!(buf.line(1).unwrap().content(), " world");
    }

    #[test]
    fn test_yank() {
        let buf = Buffer::from_text("hello world");
        let mut regs = Registers::new();

        // Range end is exclusive, so 0..5 yanks "hello"
        let range = Range::chars(Position::new(1, 0), Position::new(1, 5));
        yank(&buf, range, &mut regs, Some('a')).unwrap();

        // Check both named and unnamed register
        assert_eq!(regs.get('a').unwrap().text, "hello");
        assert_eq!(regs.get_unnamed().unwrap().text, "hello");
    }

    #[test]
    fn test_shift_right() {
        let mut buf = Buffer::from_text("hello\nworld");
        let range = Range::lines(Position::new(1, 0), Position::new(2, 0));

        shift_right(&mut buf, range, 1).unwrap();

        assert!(buf.line(1).unwrap().content().starts_with('\t'));
        assert!(buf.line(2).unwrap().content().starts_with('\t'));
    }

    #[test]
    fn test_shift_left() {
        let mut buf = Buffer::from_text("\thello\n\tworld");
        let range = Range::lines(Position::new(1, 0), Position::new(2, 0));

        shift_left(&mut buf, range, 1).unwrap();

        assert!(!buf.line(1).unwrap().content().starts_with('\t'));
        assert!(!buf.line(2).unwrap().content().starts_with('\t'));
    }

    #[test]
    fn test_put_after_lines() {
        let mut buf = Buffer::from_text("line1\nline2");
        let mut regs = Registers::new();
        regs.set_unnamed(RegisterContent::lines("new\n".to_string()));

        put_after(&mut buf, &regs, None, 1).unwrap();

        assert_eq!(buf.line_count(), 3);
        assert_eq!(buf.line(2).unwrap().content(), "new");
    }

    #[test]
    fn test_put_after_chars() {
        let mut buf = Buffer::from_text("hello");
        buf.set_column(0);
        let mut regs = Registers::new();
        regs.set_unnamed(RegisterContent::chars("X".to_string()));

        put_after(&mut buf, &regs, None, 1).unwrap();

        assert_eq!(buf.line(1).unwrap().content(), "hXello");
    }

    #[test]
    fn test_change_lines() {
        let mut buf = Buffer::from_text("line1\nline2\nline3");
        let mut regs = Registers::new();

        let range = Range::lines(Position::new(1, 0), Position::new(2, 0));
        let result = change(&mut buf, range, &mut regs, None).unwrap();

        assert!(result.enter_insert);
        assert_eq!(buf.line_count(), 2);
        assert_eq!(buf.line(1).unwrap().content(), "");
    }
}
