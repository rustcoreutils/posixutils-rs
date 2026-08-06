//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Operator commands (d, c, y, >, <, !).
//!
//! Operators work on text ranges defined by motions or text objects.

use crate::buffer::{
    char_index_at_byte, leading_blank_width, render_indent, Buffer, BufferMode, Position, Range,
};
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

/// Copy the lines about to be shifted into the unnamed buffer, in line mode.
///
/// POSIX 95633/95645 (ex `<`/`>`) and 121138/121151 (vi `<`/`>`) all require
/// this; it was missing entirely.
fn save_shift_lines(
    buffer: &Buffer,
    start_line: usize,
    end_line: usize,
    registers: &mut Registers,
) {
    let mut text = String::new();
    for line_num in start_line..=end_line {
        if let Some(line) = buffer.line(line_num) {
            text.push_str(line.content());
            text.push('\n');
        }
    }
    if !text.is_empty() {
        registers.set_unnamed(RegisterContent::new(text, true));
    }
}

/// Shift right operator (>).
///
/// `shiftwidth` is the number of display columns to shift by, not a repeat
/// count: POSIX 121149-121151 says `>` shifts "one shiftwidth ... as described
/// by the ex > command", and the ex command's amount is the number of command
/// characters times `shiftwidth` (95640-95642). The `count` prefix selects the
/// *text region* instead, which the caller has already resolved into `range`.
pub fn shift_right(
    buffer: &mut Buffer,
    range: Range,
    shiftwidth: usize,
    tabstop: usize,
    registers: &mut Registers,
) -> Result<OperatorResult> {
    let start_line = range.start_line();
    let end_line = range.end_line();

    save_shift_lines(buffer, start_line, end_line, registers);

    for line_num in start_line..=end_line {
        if let Some(line) = buffer.line(line_num) {
            // "Empty lines shall not be changed" (95643-95644).
            if line.is_empty() {
                continue;
            }
            let content = line.content().to_string();
            let (width, offset) = leading_blank_width(&content, tabstop);
            let new_content = format!(
                "{}{}",
                render_indent(width + shiftwidth, tabstop),
                &content[offset..]
            );
            buffer.replace_line(line_num, &new_content)?;
        }
    }

    // Move to first non-blank of first line
    buffer.set_line(start_line);
    buffer.move_to_first_non_blank();

    Ok(OperatorResult::cursor(buffer.cursor()))
}

/// Shift left operator (<).
///
/// See `shift_right` for why `shiftwidth` is a column count and not a repeat
/// count. Shifting past column 0 clamps rather than eating text: only leading
/// <blank> characters may be deleted or changed (95630-95632).
pub fn shift_left(
    buffer: &mut Buffer,
    range: Range,
    shiftwidth: usize,
    tabstop: usize,
    registers: &mut Registers,
) -> Result<OperatorResult> {
    let start_line = range.start_line();
    let end_line = range.end_line();

    save_shift_lines(buffer, start_line, end_line, registers);

    for line_num in start_line..=end_line {
        if let Some(line) = buffer.line(line_num) {
            let content = line.content().to_string();
            if content.is_empty() {
                continue;
            }

            let (width, offset) = leading_blank_width(&content, tabstop);
            if width == 0 {
                continue;
            }
            let new_content = format!(
                "{}{}",
                render_indent(width.saturating_sub(shiftwidth), tabstop),
                &content[offset..]
            );
            buffer.replace_line(line_num, &new_content)?;
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
        let mut regs = Registers::new();
        let range = Range::lines(Position::new(1, 0), Position::new(2, 0));

        // One shiftwidth of 8 columns, tabstop 8 -> exactly one tab.
        shift_right(&mut buf, range, 8, 8, &mut regs).unwrap();

        assert_eq!(buf.line(1).unwrap().content(), "\thello");
        assert_eq!(buf.line(2).unwrap().content(), "\tworld");
    }

    #[test]
    fn test_shift_left() {
        let mut buf = Buffer::from_text("\thello\n\tworld");
        let mut regs = Registers::new();
        let range = Range::lines(Position::new(1, 0), Position::new(2, 0));

        shift_left(&mut buf, range, 8, 8, &mut regs).unwrap();

        assert_eq!(buf.line(1).unwrap().content(), "hello");
        assert_eq!(buf.line(2).unwrap().content(), "world");
    }

    // #V21: shiftwidth is a column count, not a repeat count, and it is
    // independent of tabstop. With sw=4 and ts=8 the two hardcoded 8s in the
    // pre-fix code gave the wrong answer in both directions.
    #[test]
    fn test_shift_right_shiftwidth_differs_from_tabstop() {
        let mut buf = Buffer::from_text("hello");
        let mut regs = Registers::new();
        let range = Range::lines(Position::new(1, 0), Position::new(1, 0));

        // 0 + 4 = 4 columns; below the tabstop, so four spaces and no tab.
        shift_right(&mut buf, range, 4, 8, &mut regs).unwrap();
        assert_eq!(buf.line(1).unwrap().content(), "    hello");

        // 4 + 4 = 8 columns; now exactly one tabstop.
        let range = Range::lines(Position::new(1, 0), Position::new(1, 0));
        shift_right(&mut buf, range, 4, 8, &mut regs).unwrap();
        assert_eq!(buf.line(1).unwrap().content(), "\thello");
    }

    #[test]
    fn test_shift_left_shiftwidth_differs_from_tabstop() {
        // One tab is 8 columns; removing a 4-column shiftwidth leaves 4.
        let mut buf = Buffer::from_text("\thello");
        let mut regs = Registers::new();
        let range = Range::lines(Position::new(1, 0), Position::new(1, 0));

        shift_left(&mut buf, range, 4, 8, &mut regs).unwrap();
        assert_eq!(buf.line(1).unwrap().content(), "    hello");
    }

    // Shifting left past column 0 clamps; only leading blanks may be removed
    // (POSIX 95630-95632). The pre-fix code stripped up to 64 columns.
    #[test]
    fn test_shift_left_clamps_at_column_zero() {
        let mut buf = Buffer::from_text("  hello");
        let mut regs = Registers::new();
        let range = Range::lines(Position::new(1, 0), Position::new(1, 0));

        shift_left(&mut buf, range, 8, 8, &mut regs).unwrap();
        assert_eq!(buf.line(1).unwrap().content(), "hello");
    }

    // "Empty lines shall not be changed" (POSIX 95643-95644).
    #[test]
    fn test_shift_right_leaves_empty_lines_alone() {
        let mut buf = Buffer::from_text("hello\n\nworld");
        let mut regs = Registers::new();
        let range = Range::lines(Position::new(1, 0), Position::new(3, 0));

        shift_right(&mut buf, range, 8, 8, &mut regs).unwrap();

        assert_eq!(buf.line(1).unwrap().content(), "\thello");
        assert_eq!(buf.line(2).unwrap().content(), "");
        assert_eq!(buf.line(3).unwrap().content(), "\tworld");
    }

    // The unshifted lines go to the unnamed buffer in line mode
    // (POSIX 95633-95634, 121138, 121151). This was missing entirely.
    #[test]
    fn test_shift_saves_unshifted_lines_to_unnamed_buffer() {
        let mut buf = Buffer::from_text("hello\nworld");
        let mut regs = Registers::new();
        let range = Range::lines(Position::new(1, 0), Position::new(2, 0));

        shift_right(&mut buf, range, 8, 8, &mut regs).unwrap();

        let unnamed = regs
            .get_unnamed()
            .expect("shift must set the unnamed buffer");
        assert_eq!(unnamed.text, "hello\nworld\n");
        assert!(unnamed.linewise, "the unnamed buffer must be line mode");
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
