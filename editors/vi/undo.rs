//! Undo/redo system for vi.
//!
//! POSIX vi has two undo commands:
//! - u: Undo the last change
//! - U: Restore current line to its original state
//!
//! This implementation provides a change-based undo stack.

use crate::buffer::{Buffer, Line, Position};
use crate::error::{Result, ViError};

/// A change record for undo/redo.
#[derive(Debug, Clone)]
pub struct Change {
    /// Type of change.
    pub kind: ChangeKind,
    /// Position where change occurred.
    pub position: Position,
    /// Text that was deleted (for undo of delete).
    pub deleted_text: Option<String>,
    /// Text that was inserted (for undo of insert).
    pub inserted_text: Option<String>,
    /// Whether this was a linewise operation.
    pub linewise: bool,
    /// Number of lines affected.
    pub line_count: usize,
}

/// Types of changes.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ChangeKind {
    /// Text was inserted.
    Insert,
    /// Text was deleted.
    Delete,
    /// Text was replaced (change operation).
    Replace,
    /// Lines were joined.
    Join,
    /// Line was split (newline inserted).
    Split,
    /// Lines were shifted.
    Shift,
}

impl Change {
    /// Create an insert change.
    pub fn insert(pos: Position, text: String, linewise: bool) -> Self {
        let line_count = text.lines().count().max(1);
        Self {
            kind: ChangeKind::Insert,
            position: pos,
            deleted_text: None,
            inserted_text: Some(text),
            linewise,
            line_count,
        }
    }

    /// Create a delete change.
    pub fn delete(pos: Position, text: String, linewise: bool) -> Self {
        let line_count = text.lines().count().max(1);
        Self {
            kind: ChangeKind::Delete,
            position: pos,
            deleted_text: Some(text),
            inserted_text: None,
            linewise,
            line_count,
        }
    }

    /// Create a replace change.
    pub fn replace(pos: Position, old_text: String, new_text: String, linewise: bool) -> Self {
        let line_count = old_text.lines().count().max(1);
        Self {
            kind: ChangeKind::Replace,
            position: pos,
            deleted_text: Some(old_text),
            inserted_text: Some(new_text),
            linewise,
            line_count,
        }
    }
}

/// Undo history manager.
#[derive(Debug)]
pub struct UndoManager {
    /// Stack of changes for undo.
    undo_stack: Vec<Change>,
    /// Stack of changes for redo.
    redo_stack: Vec<Change>,
    /// Whether currently in an undo group.
    in_group: bool,
    /// Current group of changes.
    current_group: Vec<Change>,
    /// Original state of current line for U command.
    line_original: Option<(usize, String)>,
    /// Maximum undo levels (0 = unlimited).
    max_levels: usize,
}

impl UndoManager {
    /// Create a new undo manager.
    pub fn new() -> Self {
        Self {
            undo_stack: Vec::new(),
            redo_stack: Vec::new(),
            in_group: false,
            current_group: Vec::new(),
            line_original: None,
            max_levels: 0, // Unlimited
        }
    }

    /// Start a group of changes that should be undone together.
    pub fn begin_group(&mut self) {
        self.in_group = true;
        self.current_group.clear();
    }

    /// End a group of changes.
    pub fn end_group(&mut self) {
        if self.in_group {
            self.in_group = false;
            // Merge all changes in group into one compound change
            // For simplicity, just push them all onto the stack
            for change in self.current_group.drain(..) {
                self.undo_stack.push(change);
            }
            // Clear redo stack on new change
            self.redo_stack.clear();
        }
    }

    /// Record a change.
    pub fn record(&mut self, change: Change) {
        if self.in_group {
            self.current_group.push(change);
        } else {
            self.undo_stack.push(change);
            // Clear redo stack on new change
            self.redo_stack.clear();

            // Limit undo levels if configured
            if self.max_levels > 0 && self.undo_stack.len() > self.max_levels {
                self.undo_stack.remove(0);
            }
        }
    }

    /// Record insert of text.
    pub fn record_insert(&mut self, pos: Position, text: &str, linewise: bool) {
        self.record(Change::insert(pos, text.to_string(), linewise));
    }

    /// Record delete of text.
    pub fn record_delete(&mut self, pos: Position, text: &str, linewise: bool) {
        self.record(Change::delete(pos, text.to_string(), linewise));
    }

    /// Record replace of text.
    pub fn record_replace(&mut self, pos: Position, old: &str, new: &str, linewise: bool) {
        self.record(Change::replace(
            pos,
            old.to_string(),
            new.to_string(),
            linewise,
        ));
    }

    /// Save original state of a line for U command.
    pub fn save_line_original(&mut self, line_num: usize, content: &str) {
        // Only save if not already saved for this line
        match &self.line_original {
            Some((n, _)) if *n == line_num => {}
            _ => {
                self.line_original = Some((line_num, content.to_string()));
            }
        }
    }

    /// Clear line original (when moving to different line).
    pub fn clear_line_original(&mut self, current_line: usize) {
        if let Some((n, _)) = &self.line_original {
            if *n != current_line {
                self.line_original = None;
            }
        }
    }

    /// Check if undo is available.
    pub fn can_undo(&self) -> bool {
        !self.undo_stack.is_empty()
    }

    /// Check if redo is available.
    pub fn can_redo(&self) -> bool {
        !self.redo_stack.is_empty()
    }

    /// Check if line restore (U) is available.
    pub fn can_restore_line(&self, current_line: usize) -> bool {
        matches!(&self.line_original, Some((n, _)) if *n == current_line)
    }

    /// Perform undo operation.
    pub fn undo(&mut self, buffer: &mut Buffer) -> Result<Position> {
        let change = self.undo_stack.pop().ok_or(ViError::NothingToUndo)?;

        let pos = self.apply_inverse(&change, buffer)?;

        // Push to redo stack
        self.redo_stack.push(change);

        Ok(pos)
    }

    /// Perform redo operation.
    pub fn redo(&mut self, buffer: &mut Buffer) -> Result<Position> {
        let change = self.redo_stack.pop().ok_or(ViError::AtFirstChange)?;

        let pos = self.apply_change(&change, buffer)?;

        // Push back to undo stack
        self.undo_stack.push(change);

        Ok(pos)
    }

    /// Restore current line to original state (U command).
    pub fn restore_line(&mut self, buffer: &mut Buffer, current_line: usize) -> Result<Position> {
        let (line_num, original) = self.line_original.take().ok_or(ViError::NothingToUndo)?;

        if line_num != current_line {
            self.line_original = Some((line_num, original));
            return Err(ViError::NothingToUndo);
        }

        // Get current content for undo
        let current = buffer
            .line(line_num)
            .map(|l| l.content().to_string())
            .unwrap_or_default();

        // Record as change for potential undo
        self.record(Change::replace(
            Position::new(line_num, 0),
            current,
            original.clone(),
            false,
        ));

        // Restore the line
        buffer.replace_line(line_num, &original)?;
        buffer.set_line(line_num);
        buffer.move_to_first_non_blank();

        Ok(buffer.cursor())
    }

    /// Apply a change (for redo).
    fn apply_change(&self, change: &Change, buffer: &mut Buffer) -> Result<Position> {
        match change.kind {
            ChangeKind::Insert => {
                if let Some(text) = &change.inserted_text {
                    buffer.set_cursor(change.position);
                    if change.linewise {
                        // Insert lines
                        for (i, line) in text.lines().enumerate() {
                            buffer
                                .insert_line_after(change.position.line - 1 + i, Line::from(line));
                        }
                    } else {
                        buffer.insert_str(text);
                    }
                }
            }
            ChangeKind::Delete => {
                if let Some(text) = &change.deleted_text {
                    if change.linewise {
                        buffer.delete_lines(
                            change.position.line,
                            change.position.line + change.line_count - 1,
                        );
                    } else {
                        buffer.set_cursor(change.position);
                        // Delete character by character
                        for _ in text.chars() {
                            buffer.delete_char();
                        }
                    }
                }
            }
            ChangeKind::Replace => {
                // First delete old, then insert new
                if let Some(old) = &change.deleted_text {
                    buffer.set_cursor(change.position);
                    for _ in old.chars() {
                        buffer.delete_char();
                    }
                }
                if let Some(new) = &change.inserted_text {
                    buffer.insert_str(new);
                }
            }
            _ => {}
        }

        buffer.set_cursor(change.position);
        Ok(change.position)
    }

    /// Apply inverse of a change (for undo).
    fn apply_inverse(&self, change: &Change, buffer: &mut Buffer) -> Result<Position> {
        match change.kind {
            ChangeKind::Insert => {
                // Undo insert = delete
                if let Some(text) = &change.inserted_text {
                    if change.linewise {
                        buffer.delete_lines(
                            change.position.line,
                            change.position.line + change.line_count - 1,
                        );
                    } else {
                        buffer.set_cursor(change.position);
                        for _ in text.chars() {
                            buffer.delete_char();
                        }
                    }
                }
            }
            ChangeKind::Delete => {
                // Undo delete = insert
                if let Some(text) = &change.deleted_text {
                    buffer.set_cursor(change.position);
                    if change.linewise {
                        for (i, line) in text.lines().enumerate() {
                            buffer
                                .insert_line_after(change.position.line - 1 + i, Line::from(line));
                        }
                    } else {
                        buffer.insert_str(text);
                    }
                }
            }
            ChangeKind::Replace => {
                // Undo replace = delete new, insert old
                if let Some(new) = &change.inserted_text {
                    buffer.set_cursor(change.position);
                    for _ in new.chars() {
                        buffer.delete_char();
                    }
                }
                if let Some(old) = &change.deleted_text {
                    buffer.insert_str(old);
                }
            }
            _ => {}
        }

        buffer.set_cursor(change.position);
        Ok(change.position)
    }

    /// Clear all undo history.
    pub fn clear(&mut self) {
        self.undo_stack.clear();
        self.redo_stack.clear();
        self.current_group.clear();
        self.line_original = None;
    }

    /// Get number of undo levels available.
    pub fn undo_count(&self) -> usize {
        self.undo_stack.len()
    }

    /// Get number of redo levels available.
    pub fn redo_count(&self) -> usize {
        self.redo_stack.len()
    }
}

impl Default for UndoManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_record_and_undo() {
        let mut undo = UndoManager::new();
        let mut buf = Buffer::from_text("hello");

        // Simulate inserting "X" at position 0
        undo.record_insert(Position::new(1, 0), "X", false);
        buf.insert_char('X');

        assert!(undo.can_undo());
        assert!(!undo.can_redo());

        // Undo should restore original
        let pos = undo.undo(&mut buf).unwrap();
        assert_eq!(pos, Position::new(1, 0));
        assert!(!undo.can_undo());
        assert!(undo.can_redo());
    }

    #[test]
    fn test_redo() {
        let mut undo = UndoManager::new();
        let mut buf = Buffer::from_text("hello");

        undo.record_insert(Position::new(1, 0), "X", false);
        buf.insert_char('X');

        // Undo
        undo.undo(&mut buf).unwrap();

        // Redo should reapply
        assert!(undo.can_redo());
        let pos = undo.redo(&mut buf).unwrap();
        assert_eq!(pos, Position::new(1, 0));
        assert!(undo.can_undo());
        assert!(!undo.can_redo());
    }

    #[test]
    fn test_new_change_clears_redo() {
        let mut undo = UndoManager::new();
        let _buf = Buffer::from_text("hello");

        undo.record_insert(Position::new(1, 0), "A", false);
        // Manually move to redo for testing
        let change = undo.undo_stack.pop().unwrap();
        undo.redo_stack.push(change);

        assert!(undo.can_redo());

        // New change should clear redo
        undo.record_insert(Position::new(1, 0), "B", false);
        assert!(!undo.can_redo());
    }

    #[test]
    fn test_line_restore() {
        let mut undo = UndoManager::new();
        let mut buf = Buffer::from_text("hello world");

        // Save original
        undo.save_line_original(1, "hello world");

        // Make some changes
        buf.set_column(0);
        buf.insert_char('X');
        undo.record_insert(Position::new(1, 0), "X", false);

        // Restore should work
        assert!(undo.can_restore_line(1));
        undo.restore_line(&mut buf, 1).unwrap();
        assert_eq!(buf.line(1).unwrap().content(), "hello world");
    }

    #[test]
    fn test_nothing_to_undo() {
        let mut undo = UndoManager::new();
        let mut buf = Buffer::from_text("hello");

        let result = undo.undo(&mut buf);
        assert!(result.is_err());
    }
}
