//! Text object definitions (word, bigword, sentence, paragraph, section).

use crate::buffer::{Buffer, Position};

/// Check if a character is a word character (alphanumeric or underscore).
pub fn is_word_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

/// Check if a character is a blank (space or tab).
pub fn is_blank(c: char) -> bool {
    c == ' ' || c == '\t'
}

/// Check if a character is punctuation (non-blank, non-word).
pub fn is_punct(c: char) -> bool {
    !is_blank(c) && !is_word_char(c) && c != '\n'
}

/// Find the start of the current word.
pub fn word_start(buffer: &Buffer, pos: Position) -> Position {
    let line = match buffer.line(pos.line) {
        Some(l) => l,
        None => return pos,
    };

    if line.is_empty() || pos.column >= line.len() {
        return pos;
    }

    let content = line.content();
    let current_char = content[pos.column..].chars().next().unwrap_or(' ');

    // Determine the type of the current position
    let is_current_word = is_word_char(current_char);
    let is_current_punct = is_punct(current_char);

    // Skip the preliminary scan - we'll find the start directly below

    // Actually find the start by going backward
    let mut result = pos.column;
    let chars: Vec<_> = content.char_indices().collect();

    for i in (0..chars.len()).rev() {
        let (byte_idx, c) = chars[i];
        if byte_idx > pos.column {
            continue;
        }

        if is_current_word {
            if !is_word_char(c) {
                break;
            }
        } else if is_current_punct {
            if !is_punct(c) {
                break;
            }
        } else {
            break;
        }
        result = byte_idx;
    }

    Position::new(pos.line, result)
}

/// Find the end of the current word.
pub fn word_end(buffer: &Buffer, pos: Position) -> Position {
    let line = match buffer.line(pos.line) {
        Some(l) => l,
        None => return pos,
    };

    if line.is_empty() {
        return pos;
    }

    let content = line.content();
    let start_col = pos.column.min(content.len().saturating_sub(1));

    let current_char = content[start_col..].chars().next().unwrap_or(' ');
    let is_current_word = is_word_char(current_char);
    let is_current_punct = is_punct(current_char);

    let mut result = start_col;

    for (byte_idx, c) in content[start_col..].char_indices() {
        let actual_idx = start_col + byte_idx;
        if is_current_word {
            if !is_word_char(c) {
                break;
            }
        } else if is_current_punct {
            if !is_punct(c) {
                break;
            }
        } else {
            // On blank, find next word
            if !is_blank(c) {
                result = actual_idx;
                break;
            }
        }
        result = actual_idx;
    }

    Position::new(pos.line, result)
}

/// Find the start of the next word.
pub fn next_word_start(buffer: &Buffer, pos: Position) -> Option<Position> {
    let mut line_num = pos.line;
    let mut col = pos.column;

    while line_num <= buffer.line_count() {
        let line = buffer.line(line_num)?;
        let content = line.content();

        if line_num == pos.line {
            // Skip current word/punct/blanks
            let mut in_initial = true;
            let start_char = content[col.min(content.len().saturating_sub(1))..]
                .chars()
                .next();
            let initial_word = start_char.map(is_word_char).unwrap_or(false);
            let initial_punct = start_char.map(is_punct).unwrap_or(false);

            for (byte_idx, c) in content[col..].char_indices() {
                let actual_idx = col + byte_idx;

                if in_initial {
                    if (initial_word && !is_word_char(c)) || (initial_punct && !is_punct(c)) {
                        in_initial = false;
                    } else if !initial_word && !initial_punct && !is_blank(c) {
                        return Some(Position::new(line_num, actual_idx));
                    }
                }

                if !in_initial && !is_blank(c) {
                    return Some(Position::new(line_num, actual_idx));
                }
            }
        } else {
            // New line - find first non-blank
            for (byte_idx, c) in content.char_indices() {
                if !is_blank(c) {
                    return Some(Position::new(line_num, byte_idx));
                }
            }
        }

        // Move to next line
        line_num += 1;
        col = 0;
    }

    None
}

/// Find the start of the previous word.
pub fn prev_word_start(buffer: &Buffer, pos: Position) -> Option<Position> {
    let mut line_num = pos.line;
    let mut col = pos.column;

    while line_num >= 1 {
        let line = buffer.line(line_num)?;
        let content = line.content();

        if line_num == pos.line && col > 0 {
            // Move back one position first
            let chars: Vec<_> = content.char_indices().collect();
            let mut idx = chars.len();
            for (i, (byte_idx, _)) in chars.iter().enumerate() {
                if *byte_idx >= col {
                    idx = i;
                    break;
                }
            }
            idx = idx.saturating_sub(1);

            // Skip blanks
            while idx > 0 && is_blank(chars[idx].1) {
                idx -= 1;
            }

            if idx > 0 || !is_blank(chars[0].1) {
                // Find start of word
                let char_type_word = is_word_char(chars[idx].1);
                let char_type_punct = is_punct(chars[idx].1);

                while idx > 0 {
                    let prev_c = chars[idx - 1].1;
                    if char_type_word && !is_word_char(prev_c) {
                        break;
                    }
                    if char_type_punct && !is_punct(prev_c) {
                        break;
                    }
                    if !char_type_word && !char_type_punct {
                        break;
                    }
                    idx -= 1;
                }

                return Some(Position::new(line_num, chars[idx].0));
            }
        } else if line_num < pos.line && !content.is_empty() {
            // Previous line - find last word
            let chars: Vec<_> = content.char_indices().collect();
            let mut idx = chars.len() - 1;

            // Skip trailing blanks
            while idx > 0 && is_blank(chars[idx].1) {
                idx -= 1;
            }

            // Find start of word
            let char_type_word = is_word_char(chars[idx].1);
            let char_type_punct = is_punct(chars[idx].1);

            while idx > 0 {
                let prev_c = chars[idx - 1].1;
                if char_type_word && !is_word_char(prev_c) {
                    break;
                }
                if char_type_punct && !is_punct(prev_c) {
                    break;
                }
                if !char_type_word && !char_type_punct {
                    break;
                }
                idx -= 1;
            }

            return Some(Position::new(line_num, chars[idx].0));
        }

        if line_num == 1 {
            break;
        }
        line_num -= 1;
        col = buffer.line(line_num).map(|l| l.len()).unwrap_or(0);
    }

    None
}

/// Find the end of the current/next word.
pub fn next_word_end(buffer: &Buffer, pos: Position) -> Option<Position> {
    let mut line_num = pos.line;
    let mut col = pos.column;

    // Move forward one position first
    if let Some(line) = buffer.line(line_num) {
        let content = line.content();
        let chars: Vec<_> = content.char_indices().collect();

        let mut idx = 0;
        for (i, (byte_idx, _)) in chars.iter().enumerate() {
            if *byte_idx >= col {
                idx = i;
                break;
            }
        }

        if idx + 1 < chars.len() {
            col = chars[idx + 1].0;
        } else {
            line_num += 1;
            col = 0;
        }
    }

    while line_num <= buffer.line_count() {
        let line = buffer.line(line_num)?;
        let content = line.content();

        if content.is_empty() {
            line_num += 1;
            col = 0;
            continue;
        }

        let chars: Vec<_> = content.char_indices().collect();

        let mut idx = 0;
        for (i, (byte_idx, _)) in chars.iter().enumerate() {
            if *byte_idx >= col {
                idx = i;
                break;
            }
        }

        // Skip blanks
        while idx < chars.len() && is_blank(chars[idx].1) {
            idx += 1;
        }

        if idx >= chars.len() {
            line_num += 1;
            col = 0;
            continue;
        }

        // Find end of word
        let char_type_word = is_word_char(chars[idx].1);
        let char_type_punct = is_punct(chars[idx].1);

        while idx + 1 < chars.len() {
            let next_c = chars[idx + 1].1;
            if char_type_word && !is_word_char(next_c) {
                break;
            }
            if char_type_punct && !is_punct(next_c) {
                break;
            }
            if !char_type_word && !char_type_punct {
                break;
            }
            idx += 1;
        }

        return Some(Position::new(line_num, chars[idx].0));
    }

    None
}

/// Find the start of the next bigword (space-delimited).
pub fn next_bigword_start(buffer: &Buffer, pos: Position) -> Option<Position> {
    let mut line_num = pos.line;
    let mut col = pos.column;

    while line_num <= buffer.line_count() {
        let line = buffer.line(line_num)?;
        let content = line.content();

        let start_col = if line_num == pos.line { col } else { 0 };
        let mut in_word = line_num == pos.line;

        for (byte_idx, c) in content[start_col..].char_indices() {
            let actual_idx = start_col + byte_idx;

            if in_word {
                if is_blank(c) {
                    in_word = false;
                }
            } else if !is_blank(c) {
                return Some(Position::new(line_num, actual_idx));
            }
        }

        line_num += 1;
        col = 0;
    }

    None
}

/// Find the start of the previous bigword.
pub fn prev_bigword_start(buffer: &Buffer, pos: Position) -> Option<Position> {
    let mut line_num = pos.line;
    let mut col = pos.column;

    while line_num >= 1 {
        let line = buffer.line(line_num)?;
        let content = line.content();
        let chars: Vec<_> = content.char_indices().collect();

        if chars.is_empty() {
            if line_num == 1 {
                break;
            }
            line_num -= 1;
            col = buffer.line(line_num).map(|l| l.len()).unwrap_or(0);
            continue;
        }

        let search_end = if line_num == pos.line {
            let mut idx = chars.len();
            for (i, (byte_idx, _)) in chars.iter().enumerate() {
                if *byte_idx >= col {
                    idx = i;
                    break;
                }
            }
            if idx > 0 { idx - 1 } else { 0 }
        } else {
            chars.len() - 1
        };

        let mut idx = search_end;

        // Skip trailing blanks
        while idx > 0 && is_blank(chars[idx].1) {
            idx -= 1;
        }

        // Skip to start of bigword
        while idx > 0 && !is_blank(chars[idx - 1].1) {
            idx -= 1;
        }

        if !is_blank(chars[idx].1) {
            return Some(Position::new(line_num, chars[idx].0));
        }

        if line_num == 1 {
            break;
        }
        line_num -= 1;
        col = buffer.line(line_num).map(|l| l.len()).unwrap_or(0);
    }

    None
}

/// Find the end of the current/next bigword.
pub fn next_bigword_end(buffer: &Buffer, pos: Position) -> Option<Position> {
    let mut line_num = pos.line;
    let mut col = pos.column;

    // Move forward one position first
    if let Some(line) = buffer.line(line_num) {
        let content = line.content();
        let chars: Vec<_> = content.char_indices().collect();

        let mut idx = 0;
        for (i, (byte_idx, _)) in chars.iter().enumerate() {
            if *byte_idx >= col {
                idx = i;
                break;
            }
        }

        if idx + 1 < chars.len() {
            col = chars[idx + 1].0;
        } else {
            line_num += 1;
            col = 0;
        }
    }

    while line_num <= buffer.line_count() {
        let line = buffer.line(line_num)?;
        let content = line.content();

        if content.is_empty() {
            line_num += 1;
            col = 0;
            continue;
        }

        let chars: Vec<_> = content.char_indices().collect();

        let start = if line_num == pos.line {
            let mut idx = 0;
            for (i, (byte_idx, _)) in chars.iter().enumerate() {
                if *byte_idx >= col {
                    idx = i;
                    break;
                }
            }
            idx
        } else {
            0
        };

        let mut idx = start;

        // Skip blanks
        while idx < chars.len() && is_blank(chars[idx].1) {
            idx += 1;
        }

        if idx >= chars.len() {
            line_num += 1;
            col = 0;
            continue;
        }

        // Find end of bigword
        while idx + 1 < chars.len() && !is_blank(chars[idx + 1].1) {
            idx += 1;
        }

        return Some(Position::new(line_num, chars[idx].0));
    }

    None
}

/// Check if a line is a paragraph boundary.
pub fn is_paragraph_boundary(buffer: &Buffer, line_num: usize) -> bool {
    let line = match buffer.line(line_num) {
        Some(l) => l,
        None => return true,
    };

    // Empty line or blank line
    if line.is_empty() || line.is_blank_line() {
        return true;
    }

    // Section boundary is also paragraph boundary
    is_section_boundary(buffer, line_num)
}

/// Check if a line is a section boundary.
pub fn is_section_boundary(buffer: &Buffer, line_num: usize) -> bool {
    let line = match buffer.line(line_num) {
        Some(l) => l,
        None => return true,
    };

    let content = line.content();
    if content.is_empty() {
        return false;
    }

    let first = content.chars().next().unwrap();

    // Form feed or open brace
    if first == '\x0C' || first == '{' {
        return true;
    }

    // nroff macros (lines starting with '.')
    if first == '.' && content.len() >= 3 {
        // Check for section macros like .SH, .NH, etc.
        let second = content.chars().nth(1).unwrap_or(' ');
        let third = content.chars().nth(2).unwrap_or(' ');
        if second.is_ascii_uppercase() && (third.is_ascii_uppercase() || third == ' ') {
            return true;
        }
    }

    false
}

/// Find the start of the next paragraph.
pub fn next_paragraph_start(buffer: &Buffer, pos: Position) -> Option<Position> {
    let mut line_num = pos.line;

    // First, skip past any blank lines at current position
    while line_num <= buffer.line_count() {
        if let Some(line) = buffer.line(line_num) {
            if !line.is_empty() && !line.is_blank_line() {
                break;
            }
        }
        line_num += 1;
    }

    // Now find the next blank line
    while line_num <= buffer.line_count() {
        if is_paragraph_boundary(buffer, line_num) {
            // Skip consecutive blank lines
            while line_num <= buffer.line_count() {
                if let Some(line) = buffer.line(line_num) {
                    if !line.is_empty() && !line.is_blank_line() {
                        return Some(Position::new(line_num, 0));
                    }
                }
                line_num += 1;
            }
            break;
        }
        line_num += 1;
    }

    // End of buffer
    if line_num > buffer.line_count() && buffer.line_count() > 0 {
        Some(Position::new(buffer.line_count(), 0))
    } else {
        None
    }
}

/// Find the start of the previous paragraph.
pub fn prev_paragraph_start(buffer: &Buffer, pos: Position) -> Option<Position> {
    if pos.line <= 1 {
        return Some(Position::new(1, 0));
    }

    let mut line_num = pos.line - 1;

    // Skip blank lines
    while line_num > 0 {
        if let Some(line) = buffer.line(line_num) {
            if !line.is_empty() && !line.is_blank_line() {
                break;
            }
        }
        line_num -= 1;
    }

    // Find previous blank line
    while line_num > 0 {
        if is_paragraph_boundary(buffer, line_num) {
            return Some(Position::new(line_num + 1, 0));
        }
        line_num -= 1;
    }

    Some(Position::new(1, 0))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_word_char() {
        assert!(is_word_char('a'));
        assert!(is_word_char('Z'));
        assert!(is_word_char('5'));
        assert!(is_word_char('_'));
        assert!(!is_word_char(' '));
        assert!(!is_word_char('.'));
    }

    #[test]
    fn test_is_blank() {
        assert!(is_blank(' '));
        assert!(is_blank('\t'));
        assert!(!is_blank('a'));
        assert!(!is_blank('\n'));
    }

    #[test]
    fn test_is_punct() {
        assert!(is_punct('.'));
        assert!(is_punct(','));
        assert!(is_punct('!'));
        assert!(!is_punct('a'));
        assert!(!is_punct(' '));
    }

    #[test]
    fn test_next_word_start() {
        let buf = Buffer::from_text("hello world test");
        // From 'h', next word is 'w' at col 6
        let pos = next_word_start(&buf, Position::new(1, 0));
        assert_eq!(pos, Some(Position::new(1, 6)));

        // From 'w', next word is 't' at col 12
        let pos = next_word_start(&buf, Position::new(1, 6));
        assert_eq!(pos, Some(Position::new(1, 12)));
    }

    #[test]
    fn test_next_word_across_lines() {
        let buf = Buffer::from_text("hello\nworld");
        // From end of first line, next word is 'world'
        let pos = next_word_start(&buf, Position::new(1, 4));
        assert_eq!(pos, Some(Position::new(2, 0)));
    }

    #[test]
    fn test_prev_word_start() {
        let buf = Buffer::from_text("hello world");
        // From 'w', prev word starts at 'h'
        let pos = prev_word_start(&buf, Position::new(1, 6));
        assert_eq!(pos, Some(Position::new(1, 0)));
    }

    #[test]
    fn test_paragraph_boundary() {
        let buf = Buffer::from_text("hello\n\nworld");
        assert!(!is_paragraph_boundary(&buf, 1)); // "hello"
        assert!(is_paragraph_boundary(&buf, 2)); // empty line
        assert!(!is_paragraph_boundary(&buf, 3)); // "world"
    }

    #[test]
    fn test_section_boundary() {
        let buf = Buffer::from_text("hello\n{");
        assert!(!is_section_boundary(&buf, 1));
        assert!(is_section_boundary(&buf, 2)); // line starting with '{'
    }
}
