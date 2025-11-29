//! Position and range types for the edit buffer.

use std::cmp::Ordering;

/// A position in the edit buffer (1-indexed line, 0-indexed column).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Position {
    /// Line number (1-indexed, as in vi).
    pub line: usize,
    /// Column position (0-indexed byte offset within line).
    pub column: usize,
}

impl Position {
    /// Create a new position.
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }

    /// Position at start of first line.
    pub fn start() -> Self {
        Self { line: 1, column: 0 }
    }

    /// Check if this position is at the start of a line.
    pub fn is_line_start(&self) -> bool {
        self.column == 0
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.line.cmp(&other.line) {
            Ordering::Equal => self.column.cmp(&other.column),
            ord => ord,
        }
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Buffer mode determines how text is stored in registers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BufferMode {
    /// Character-wise: text is a sequence of characters.
    #[default]
    Character,
    /// Line-wise: text is complete lines (with newlines).
    Line,
}

/// A range of text in the buffer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Range {
    /// Start position (inclusive).
    pub start: Position,
    /// End position (inclusive for line mode, exclusive for char mode).
    pub end: Position,
    /// Mode of the range.
    pub mode: BufferMode,
}

impl Range {
    /// Create a new range.
    pub fn new(start: Position, end: Position, mode: BufferMode) -> Self {
        // Normalize so start <= end
        if start <= end {
            Self { start, end, mode }
        } else {
            Self {
                start: end,
                end: start,
                mode,
            }
        }
    }

    /// Create a character-mode range.
    pub fn chars(start: Position, end: Position) -> Self {
        Self::new(start, end, BufferMode::Character)
    }

    /// Create a line-mode range.
    pub fn lines(start: Position, end: Position) -> Self {
        Self::new(start, end, BufferMode::Line)
    }

    /// Check if range is empty.
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Check if range spans multiple lines.
    pub fn is_multiline(&self) -> bool {
        self.start.line != self.end.line
    }

    /// Get the start line number.
    pub fn start_line(&self) -> usize {
        self.start.line
    }

    /// Get the end line number.
    pub fn end_line(&self) -> usize {
        self.end.line
    }

    /// Check if a position is within this range.
    pub fn contains(&self, pos: Position) -> bool {
        pos >= self.start && pos <= self.end
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_ordering() {
        let p1 = Position::new(1, 0);
        let p2 = Position::new(1, 5);
        let p3 = Position::new(2, 0);

        assert!(p1 < p2);
        assert!(p2 < p3);
        assert!(p1 < p3);
    }

    #[test]
    fn test_position_equality() {
        let p1 = Position::new(1, 5);
        let p2 = Position::new(1, 5);
        assert_eq!(p1, p2);
    }

    #[test]
    fn test_range_normalization() {
        let start = Position::new(2, 0);
        let end = Position::new(1, 0);
        let range = Range::chars(start, end);

        assert_eq!(range.start, end);
        assert_eq!(range.end, start);
    }

    #[test]
    fn test_range_contains() {
        let range = Range::chars(Position::new(1, 5), Position::new(1, 10));

        assert!(range.contains(Position::new(1, 7)));
        assert!(!range.contains(Position::new(1, 3)));
        assert!(!range.contains(Position::new(2, 7)));
    }

    #[test]
    fn test_range_multiline() {
        let single = Range::chars(Position::new(1, 0), Position::new(1, 10));
        let multi = Range::chars(Position::new(1, 0), Position::new(3, 5));

        assert!(!single.is_multiline());
        assert!(multi.is_multiline());
    }
}
