//! Motion commands for cursor movement.

use super::text_object::{
    next_bigword_end, next_bigword_start, next_paragraph_start, next_word_end, next_word_start,
    prev_bigword_start, prev_paragraph_start, prev_word_start,
};
use crate::buffer::{char_index_at_byte, Buffer, BufferMode, Position, Range};
use crate::error::{Result, ViError};

/// Result of a motion command.
#[derive(Debug, Clone)]
pub struct MotionResult {
    /// New cursor position.
    pub position: Position,
    /// Text range affected (for operators).
    pub range: Option<Range>,
    /// Whether motion is line-wise.
    pub linewise: bool,
    /// Whether to move to first non-blank.
    pub first_non_blank: bool,
}

impl MotionResult {
    /// Create a motion result with just a position.
    pub fn pos(position: Position) -> Self {
        Self {
            position,
            range: None,
            linewise: false,
            first_non_blank: false,
        }
    }

    /// Create a motion result with a range.
    pub fn with_range(position: Position, range: Range) -> Self {
        Self {
            position,
            range: Some(range),
            linewise: range.mode == BufferMode::Line,
            first_non_blank: false,
        }
    }

    /// Set linewise flag.
    pub fn linewise(mut self) -> Self {
        self.linewise = true;
        self
    }

    /// Set first_non_blank flag.
    pub fn with_first_non_blank(mut self) -> Self {
        self.first_non_blank = true;
        self
    }
}

/// Move cursor left by count characters.
pub fn move_left(buffer: &Buffer, count: usize) -> Result<MotionResult> {
    let pos = buffer.cursor();
    if pos.column == 0 {
        return Err(ViError::MotionFailed(
            "Already at start of line".to_string(),
        ));
    }

    let line = buffer.line(pos.line).ok_or(ViError::EmptyBuffer)?;
    let content = line.content();
    let chars: Vec<(usize, char)> = content.char_indices().collect();

    let current_idx = char_index_at_byte(content, pos.column);
    let new_idx = current_idx.saturating_sub(count);
    let new_col = chars.get(new_idx).map(|(idx, _)| *idx).unwrap_or(0);

    let new_pos = Position::new(pos.line, new_col);
    let range = Range::chars(
        new_pos,
        Position::new(pos.line, pos.column.saturating_sub(1)),
    );

    Ok(MotionResult::with_range(new_pos, range))
}

/// Move cursor right by count characters.
pub fn move_right(buffer: &Buffer, count: usize) -> Result<MotionResult> {
    let pos = buffer.cursor();
    let line = buffer.line(pos.line).ok_or(ViError::EmptyBuffer)?;

    if line.is_empty() {
        return Err(ViError::MotionFailed("Empty line".to_string()));
    }

    let content = line.content();
    let chars: Vec<(usize, char)> = content.char_indices().collect();

    let current_idx = char_index_at_byte(content, pos.column);
    if current_idx + 1 >= chars.len() {
        return Err(ViError::MotionFailed("Already at end of line".to_string()));
    }

    let new_idx = (current_idx + count).min(chars.len() - 1);
    let new_col = chars[new_idx].0;

    let new_pos = Position::new(pos.line, new_col);
    let range = Range::chars(pos, new_pos);

    Ok(MotionResult::with_range(new_pos, range))
}

/// Move cursor down by count lines.
pub fn move_down(buffer: &Buffer, count: usize) -> Result<MotionResult> {
    let pos = buffer.cursor();
    if pos.line + count > buffer.line_count() {
        return Err(ViError::MotionFailed("Already at last line".to_string()));
    }

    let new_line = pos.line + count;
    let new_pos = Position::new(new_line, pos.column);
    let range = Range::lines(pos, new_pos);

    Ok(MotionResult::with_range(new_pos, range).linewise())
}

/// Move cursor up by count lines.
pub fn move_up(buffer: &Buffer, count: usize) -> Result<MotionResult> {
    let pos = buffer.cursor();
    if pos.line <= count {
        return Err(ViError::MotionFailed("Already at first line".to_string()));
    }

    let new_line = pos.line - count;
    let new_pos = Position::new(new_line, pos.column);
    let range = Range::lines(new_pos, pos);

    Ok(MotionResult::with_range(new_pos, range).linewise())
}

/// Move to first character of line.
pub fn move_to_line_start(buffer: &Buffer) -> Result<MotionResult> {
    let pos = buffer.cursor();
    let new_pos = Position::new(pos.line, 0);

    if pos.column == 0 {
        return Ok(MotionResult::pos(new_pos));
    }

    let range = Range::chars(
        new_pos,
        Position::new(pos.line, pos.column.saturating_sub(1)),
    );
    Ok(MotionResult::with_range(new_pos, range))
}

/// Move to first non-blank character of line.
pub fn move_to_first_non_blank(buffer: &Buffer) -> Result<MotionResult> {
    let pos = buffer.cursor();
    let line = buffer.line(pos.line).ok_or(ViError::EmptyBuffer)?;

    let new_col = line.first_non_blank();
    let new_pos = Position::new(pos.line, new_col);

    if new_col == pos.column {
        return Ok(MotionResult::pos(new_pos));
    }

    let (start, end) = if new_col < pos.column {
        (
            new_pos,
            Position::new(pos.line, pos.column.saturating_sub(1)),
        )
    } else {
        (pos, Position::new(pos.line, new_col.saturating_sub(1)))
    };

    let range = Range::chars(start, end);
    Ok(MotionResult::with_range(new_pos, range))
}

/// Move to last character of line.
pub fn move_to_line_end(buffer: &Buffer, count: usize) -> Result<MotionResult> {
    let pos = buffer.cursor();

    if count > 1 {
        let target_line = (pos.line + count - 1).min(buffer.line_count());
        if target_line > buffer.line_count() {
            return Err(ViError::MotionFailed("Line out of range".to_string()));
        }

        let line = buffer.line(target_line).ok_or(ViError::EmptyBuffer)?;
        let new_col = line.last_char_offset();
        let new_pos = Position::new(target_line, new_col);

        // Multi-line motion is line-wise if starting before first non-blank
        let start_line = buffer.line(pos.line).ok_or(ViError::EmptyBuffer)?;
        if pos.column <= start_line.first_non_blank() {
            let range = Range::lines(pos, new_pos);
            return Ok(MotionResult::with_range(new_pos, range).linewise());
        }

        let range = Range::chars(pos, new_pos);
        return Ok(MotionResult::with_range(new_pos, range));
    }

    let line = buffer.line(pos.line).ok_or(ViError::EmptyBuffer)?;
    if line.is_empty() {
        return Err(ViError::MotionFailed("Empty line".to_string()));
    }

    let new_col = line.last_char_offset();
    let new_pos = Position::new(pos.line, new_col);
    let range = Range::chars(pos, new_pos);

    Ok(MotionResult::with_range(new_pos, range))
}

/// Move to line number (G command).
pub fn move_to_line(buffer: &Buffer, line_num: Option<usize>) -> Result<MotionResult> {
    let pos = buffer.cursor();
    let target = line_num.unwrap_or(buffer.line_count());

    if target == 0 || target > buffer.line_count() {
        return Err(ViError::MotionFailed("Invalid line number".to_string()));
    }

    let new_pos = Position::new(target, 0);
    let range = Range::lines(pos, new_pos);

    Ok(MotionResult::with_range(new_pos, range)
        .linewise()
        .with_first_non_blank())
}

/// Helper for forward word/bigword motions (w, W, e, E commands).
fn move_forward_by<F>(buffer: &Buffer, count: usize, finder: F) -> Result<MotionResult>
where
    F: Fn(&Buffer, Position) -> Option<Position>,
{
    let mut pos = buffer.cursor();
    for _ in 0..count {
        match finder(buffer, pos) {
            Some(new_pos) => pos = new_pos,
            None => break,
        }
    }
    if pos == buffer.cursor() {
        return Err(ViError::MotionFailed("At end of buffer".to_string()));
    }
    let range = Range::chars(buffer.cursor(), pos);
    Ok(MotionResult::with_range(pos, range))
}

/// Helper for backward word/bigword motions (b, B commands).
fn move_backward_by<F>(buffer: &Buffer, count: usize, finder: F) -> Result<MotionResult>
where
    F: Fn(&Buffer, Position) -> Option<Position>,
{
    let mut pos = buffer.cursor();
    for _ in 0..count {
        match finder(buffer, pos) {
            Some(new_pos) => pos = new_pos,
            None => break,
        }
    }
    if pos == buffer.cursor() {
        return Err(ViError::MotionFailed("At start of buffer".to_string()));
    }
    let range = Range::chars(pos, buffer.cursor());
    Ok(MotionResult::with_range(pos, range))
}

/// Move to next word start (w command).
pub fn move_word_forward(buffer: &Buffer, count: usize) -> Result<MotionResult> {
    move_forward_by(buffer, count, next_word_start)
}

/// Move to previous word start (b command).
pub fn move_word_backward(buffer: &Buffer, count: usize) -> Result<MotionResult> {
    move_backward_by(buffer, count, prev_word_start)
}

/// Move to word end (e command).
pub fn move_word_end(buffer: &Buffer, count: usize) -> Result<MotionResult> {
    move_forward_by(buffer, count, next_word_end)
}

/// Move to next bigword start (W command).
pub fn move_bigword_forward(buffer: &Buffer, count: usize) -> Result<MotionResult> {
    move_forward_by(buffer, count, next_bigword_start)
}

/// Move to previous bigword start (B command).
pub fn move_bigword_backward(buffer: &Buffer, count: usize) -> Result<MotionResult> {
    move_backward_by(buffer, count, prev_bigword_start)
}

/// Move to bigword end (E command).
pub fn move_bigword_end(buffer: &Buffer, count: usize) -> Result<MotionResult> {
    move_forward_by(buffer, count, next_bigword_end)
}

/// Move to next paragraph ({ command).
pub fn move_paragraph_forward(buffer: &Buffer, count: usize) -> Result<MotionResult> {
    let mut pos = buffer.cursor();

    for _ in 0..count {
        match next_paragraph_start(buffer, pos) {
            Some(new_pos) => pos = new_pos,
            None => break,
        }
    }

    let range = Range::lines(buffer.cursor(), pos);
    Ok(MotionResult::with_range(pos, range).linewise())
}

/// Move to previous paragraph (} command).
pub fn move_paragraph_backward(buffer: &Buffer, count: usize) -> Result<MotionResult> {
    let mut pos = buffer.cursor();

    for _ in 0..count {
        match prev_paragraph_start(buffer, pos) {
            Some(new_pos) => pos = new_pos,
            None => break,
        }
    }

    let range = Range::lines(pos, buffer.cursor());
    Ok(MotionResult::with_range(pos, range).linewise())
}

/// Move to next section boundary (]] command).
pub fn move_section_forward(buffer: &Buffer, count: usize) -> Result<MotionResult> {
    use super::text_object::is_section_boundary;

    let mut pos = buffer.cursor();

    for _ in 0..count {
        // Search forward from next line
        let mut found = false;
        for line_num in (pos.line + 1)..=buffer.line_count() {
            if is_section_boundary(buffer, line_num) {
                pos = Position::new(line_num, 0);
                found = true;
                break;
            }
        }
        if !found {
            // Move to last line if no section found
            pos = Position::new(buffer.line_count(), 0);
            break;
        }
    }

    let range = Range::lines(buffer.cursor(), pos);
    Ok(MotionResult::with_range(pos, range).linewise())
}

/// Move to previous section boundary ([[ command).
pub fn move_section_backward(buffer: &Buffer, count: usize) -> Result<MotionResult> {
    use super::text_object::is_section_boundary;

    let mut pos = buffer.cursor();

    for _ in 0..count {
        // Search backward from previous line
        let mut found = false;
        for line_num in (1..pos.line).rev() {
            if is_section_boundary(buffer, line_num) {
                pos = Position::new(line_num, 0);
                found = true;
                break;
            }
        }
        if !found {
            // Move to first line if no section found
            pos = Position::new(1, 0);
            break;
        }
    }

    let range = Range::lines(pos, buffer.cursor());
    Ok(MotionResult::with_range(pos, range).linewise())
}

/// Find character forward in line (f command).
pub fn find_char_forward(buffer: &Buffer, c: char, count: usize) -> Result<MotionResult> {
    let pos = buffer.cursor();
    let line = buffer.line(pos.line).ok_or(ViError::EmptyBuffer)?;
    let content = line.content();

    let mut found = 0;
    let mut result_col = pos.column;

    for (byte_idx, ch) in content[pos.column..].char_indices() {
        if byte_idx == 0 {
            continue; // Skip current position
        }
        if ch == c {
            found += 1;
            if found == count {
                result_col = pos.column + byte_idx;
                break;
            }
        }
    }

    if found < count {
        return Err(ViError::MotionFailed(format!("'{}' not found", c)));
    }

    let new_pos = Position::new(pos.line, result_col);
    let range = Range::chars(pos, new_pos);
    Ok(MotionResult::with_range(new_pos, range))
}

/// Find character backward in line (F command).
pub fn find_char_backward(buffer: &Buffer, c: char, count: usize) -> Result<MotionResult> {
    let pos = buffer.cursor();
    let line = buffer.line(pos.line).ok_or(ViError::EmptyBuffer)?;
    let content = line.content();

    if pos.column == 0 {
        return Err(ViError::MotionFailed(format!("'{}' not found", c)));
    }

    let chars: Vec<(usize, char)> = content[..pos.column].char_indices().collect();
    let mut found = 0;
    let mut result_col = 0;

    for (byte_idx, ch) in chars.iter().rev() {
        if *ch == c {
            found += 1;
            if found == count {
                result_col = *byte_idx;
                break;
            }
        }
    }

    if found < count {
        return Err(ViError::MotionFailed(format!("'{}' not found", c)));
    }

    let new_pos = Position::new(pos.line, result_col);
    let range = Range::chars(new_pos, pos);
    Ok(MotionResult::with_range(new_pos, range))
}

/// Find till character forward (t command).
pub fn till_char_forward(buffer: &Buffer, c: char, count: usize) -> Result<MotionResult> {
    let result = find_char_forward(buffer, c, count)?;

    // Move one position back
    let line = buffer
        .line(result.position.line)
        .ok_or(ViError::EmptyBuffer)?;
    let content = line.content();
    let chars: Vec<(usize, char)> = content.char_indices().collect();

    let idx = char_index_at_byte(content, result.position.column);
    if idx == 0 {
        return Err(ViError::MotionFailed("Character at cursor".to_string()));
    }

    let new_col = chars[idx - 1].0;
    let new_pos = Position::new(result.position.line, new_col);
    let range = Range::chars(buffer.cursor(), new_pos);

    Ok(MotionResult::with_range(new_pos, range))
}

/// Find till character backward (T command).
pub fn till_char_backward(buffer: &Buffer, c: char, count: usize) -> Result<MotionResult> {
    let result = find_char_backward(buffer, c, count)?;

    // Move one position forward
    let line = buffer
        .line(result.position.line)
        .ok_or(ViError::EmptyBuffer)?;
    let content = line.content();
    let chars: Vec<(usize, char)> = content.char_indices().collect();

    let idx = char_index_at_byte(content, result.position.column);
    if idx + 1 >= chars.len() {
        return Err(ViError::MotionFailed(
            "No character after found".to_string(),
        ));
    }

    let new_col = chars[idx + 1].0;
    let new_pos = Position::new(result.position.line, new_col);
    let range = Range::chars(new_pos, buffer.cursor());

    Ok(MotionResult::with_range(new_pos, range))
}

/// Move to column (| command).
pub fn move_to_column(buffer: &Buffer, col: usize) -> Result<MotionResult> {
    let pos = buffer.cursor();
    let line = buffer.line(pos.line).ok_or(ViError::EmptyBuffer)?;

    // Column is 1-indexed for display, convert to byte offset
    // For now, treat column as display column
    let target_col = col.saturating_sub(1); // Convert to 0-indexed

    let content = line.content();
    let chars: Vec<(usize, char)> = content.char_indices().collect();

    let new_col = if target_col >= chars.len() {
        chars.last().map(|(idx, _)| *idx).unwrap_or(0)
    } else {
        chars.get(target_col).map(|(idx, _)| *idx).unwrap_or(0)
    };

    let new_pos = Position::new(pos.line, new_col);

    if new_col == pos.column {
        return Ok(MotionResult::pos(new_pos));
    }

    let (start, end) = if new_col < pos.column {
        (
            new_pos,
            Position::new(pos.line, pos.column.saturating_sub(1)),
        )
    } else {
        (pos, new_pos)
    };

    let range = Range::chars(start, end);
    Ok(MotionResult::with_range(new_pos, range))
}

/// Find matching bracket (% command).
pub fn find_matching_bracket(buffer: &Buffer) -> Result<MotionResult> {
    let pos = buffer.cursor();
    let line = buffer.line(pos.line).ok_or(ViError::EmptyBuffer)?;
    let content = line.content();

    // Find bracket at or after cursor on current line
    let mut bracket_pos = None;
    let mut bracket_char = None;

    for (byte_idx, c) in content[pos.column..].char_indices() {
        if matches!(c, '(' | ')' | '[' | ']' | '{' | '}') {
            bracket_pos = Some(pos.column + byte_idx);
            bracket_char = Some(c);
            break;
        }
    }

    let (start_col, start_char) = match (bracket_pos, bracket_char) {
        (Some(col), Some(c)) => (col, c),
        _ => return Err(ViError::MotionFailed("No bracket on line".to_string())),
    };

    let (close, open, forward) = match start_char {
        '(' => (')', '(', true),
        ')' => ('(', ')', false),
        '[' => (']', '[', true),
        ']' => ('[', ']', false),
        '{' => ('}', '{', true),
        '}' => ('{', '}', false),
        _ => unreachable!(),
    };

    let mut count = 1;

    if forward {
        // Search forward
        let mut line_num = pos.line;
        let mut col = start_col + 1;

        while line_num <= buffer.line_count() {
            let line = buffer.line(line_num).ok_or(ViError::EmptyBuffer)?;
            let content = line.content();

            let start = if line_num == pos.line { col } else { 0 };

            for (byte_idx, c) in content[start..].char_indices() {
                if c == close {
                    count -= 1;
                    if count == 0 {
                        let new_pos = Position::new(line_num, start + byte_idx);
                        let range = Range::chars(Position::new(pos.line, start_col), new_pos);
                        return Ok(MotionResult::with_range(new_pos, range));
                    }
                } else if c == open {
                    count += 1;
                }
            }

            line_num += 1;
            col = 0;
        }
    } else {
        // Search backward
        let mut line_num = pos.line;
        let mut col = start_col;

        loop {
            let line = buffer.line(line_num).ok_or(ViError::EmptyBuffer)?;
            let content = line.content();

            let search_end = if line_num == pos.line {
                col
            } else {
                content.len()
            };
            let chars: Vec<(usize, char)> = content[..search_end].char_indices().collect();

            for (byte_idx, c) in chars.iter().rev() {
                if *c == close {
                    count -= 1;
                    if count == 0 {
                        let new_pos = Position::new(line_num, *byte_idx);
                        let range = Range::chars(new_pos, Position::new(pos.line, start_col));
                        return Ok(MotionResult::with_range(new_pos, range));
                    }
                } else if *c == open {
                    count += 1;
                }
            }

            if line_num == 1 {
                break;
            }
            line_num -= 1;
            col = 0;
        }
    }

    Err(ViError::MotionFailed(
        "Matching bracket not found".to_string(),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_move_left() {
        let mut buf = Buffer::from_text("hello");
        buf.set_column(3);
        let result = move_left(&buf, 1).unwrap();
        assert_eq!(result.position.column, 2);
    }

    #[test]
    fn test_move_left_at_start() {
        let buf = Buffer::from_text("hello");
        let result = move_left(&buf, 1);
        assert!(result.is_err());
    }

    #[test]
    fn test_move_right() {
        let buf = Buffer::from_text("hello");
        let result = move_right(&buf, 1).unwrap();
        assert_eq!(result.position.column, 1);
    }

    #[test]
    fn test_move_down() {
        let buf = Buffer::from_text("one\ntwo\nthree");
        let result = move_down(&buf, 1).unwrap();
        assert_eq!(result.position.line, 2);
    }

    #[test]
    fn test_move_up() {
        let mut buf = Buffer::from_text("one\ntwo\nthree");
        buf.set_line(2);
        let result = move_up(&buf, 1).unwrap();
        assert_eq!(result.position.line, 1);
    }

    #[test]
    fn test_move_to_line_end() {
        let buf = Buffer::from_text("hello world");
        let result = move_to_line_end(&buf, 1).unwrap();
        assert_eq!(result.position.column, 10); // 'd' is at offset 10
    }

    #[test]
    fn test_move_to_line() {
        let buf = Buffer::from_text("one\ntwo\nthree");
        let result = move_to_line(&buf, Some(3)).unwrap();
        assert_eq!(result.position.line, 3);
    }

    #[test]
    fn test_find_char_forward() {
        let buf = Buffer::from_text("hello world");
        let result = find_char_forward(&buf, 'o', 1).unwrap();
        assert_eq!(result.position.column, 4); // First 'o' in "hello"
    }

    #[test]
    fn test_find_matching_bracket() {
        let buf = Buffer::from_text("(hello)");
        let result = find_matching_bracket(&buf).unwrap();
        assert_eq!(result.position.column, 6); // Closing ')'
    }

    #[test]
    fn test_find_matching_bracket_nested() {
        let buf = Buffer::from_text("((a))");
        let result = find_matching_bracket(&buf).unwrap();
        assert_eq!(result.position.column, 4); // Outermost closing ')'
    }
}
