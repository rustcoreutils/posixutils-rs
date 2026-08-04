//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

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
    /// Text was replaced (change operation), within a single line.
    Replace,
    /// A contiguous run of whole lines was replaced by a different run.
    ///
    /// [`ChangeKind::Replace`] is implemented as a within-line `delete_char`
    /// loop plus `insert_str`, so it cannot express an edit that changes the
    /// *number* of lines. A substitute whose replacement contains `\n` splits
    /// one line into several and needs this variant; without it the edit would
    /// be un-undoable and `u` would corrupt the buffer.
    ReplaceLines,
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

    /// Create a linewise range-replace change.
    ///
    /// `old_lines` are the lines currently occupying `pos.line ..`, and
    /// `new_lines` are what replaces them. Either side may be longer.
    pub fn replace_lines(pos: Position, old_lines: &[String], new_lines: &[String]) -> Self {
        Self {
            kind: ChangeKind::ReplaceLines,
            position: pos,
            deleted_text: Some(old_lines.join("\n")),
            inserted_text: Some(new_lines.join("\n")),
            linewise: true,
            line_count: old_lines.len().max(1),
        }
    }
}

/// Undo history manager.
///
/// Each stack entry is one *command*, not one edit. POSIX (ex `undo`, 95443)
/// says "the global, v, open, and visual commands … are considered single
/// commands", and a change operator is a delete plus an insert session: a
/// single `u` must reverse both. The entries therefore hold the command's edits
/// in application order, and undo applies their inverses in reverse.
#[derive(Debug)]
pub struct UndoManager {
    /// Stack of commands available to undo; each is that command's edits in
    /// application order.
    undo_stack: Vec<Vec<Change>>,
    /// Stack of commands available to redo.
    redo_stack: Vec<Vec<Change>>,
    /// Whether a command group is currently open (see [`UndoManager::begin_group`]).
    in_group: bool,
    /// Original state of current line for U command.
    line_original: Option<(usize, String)>,
    /// Maximum undo levels (0 = unlimited).
    max_levels: usize,
    /// Whether the last buffer-modifying operation was an undo.
    ///
    /// POSIX defines `u` as reversing "the last command that modified the
    /// contents of the edit buffer, **including undo**" (95442), so `u` is its
    /// own inverse. This is what makes it alternate.
    last_was_undo: bool,
}

impl UndoManager {
    /// Create a new undo manager.
    pub fn new() -> Self {
        Self {
            undo_stack: Vec::new(),
            redo_stack: Vec::new(),
            in_group: false,
            line_original: None,
            max_levels: 0, // Unlimited
            last_was_undo: false,
        }
    }

    /// Begin one command's group of edits, so a single `u` reverses all of
    /// them. Used for a change operator and the insert session it opens.
    ///
    /// Nested calls are ignored: the outermost group wins, which is what makes
    /// it safe for `enter_insert` to open a group whether or not an operator
    /// already did.
    pub fn begin_group(&mut self) {
        if self.in_group {
            return;
        }
        self.in_group = true;
        self.undo_stack.push(Vec::new());
        self.redo_stack.clear();
        self.last_was_undo = false;
    }

    /// End the current command's group.
    pub fn end_group(&mut self) {
        if !self.in_group {
            return;
        }
        self.in_group = false;
        // A group that recorded nothing (an insert session that typed nothing,
        // say) must not leave an entry that `u` would consume silently.
        if self.undo_stack.last().is_some_and(Vec::is_empty) {
            self.undo_stack.pop();
        }
        self.trim();
    }

    /// Record a change.
    pub fn record(&mut self, change: Change) {
        // Any new edit invalidates the redo history and ends an undo run.
        self.redo_stack.clear();
        self.last_was_undo = false;

        match self.undo_stack.last_mut() {
            Some(group) if self.in_group => group.push(change),
            _ => {
                self.undo_stack.push(vec![change]);
                self.trim();
            }
        }
    }

    /// Drop the oldest commands past `max_levels`.
    fn trim(&mut self) {
        if self.max_levels > 0 && self.undo_stack.len() > self.max_levels {
            self.undo_stack.remove(0);
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
    /// Record a linewise range replacement, where the run of lines starting at
    /// `pos.line` changes length (e.g. a substitute whose replacement contains
    /// a newline).
    pub fn record_replace_lines(
        &mut self,
        pos: Position,
        old_lines: &[String],
        new_lines: &[String],
    ) {
        self.record(Change::replace_lines(pos, old_lines, new_lines));
    }

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

    /// Reverse the last command, applying its edits' inverses in reverse order.
    ///
    /// The reported position comes from the *first* edit of the command, which
    /// is where POSIX puts the cursor: "set to the first line added or changed"
    /// (95450).
    pub fn undo(&mut self, buffer: &mut Buffer) -> Result<Position> {
        let group = self.undo_stack.pop().ok_or(ViError::NothingToUndo)?;

        let mut pos = buffer.cursor();
        for change in group.iter().rev() {
            pos = self.apply_inverse(change, buffer)?;
        }

        self.redo_stack.push(group);
        self.last_was_undo = true;
        Ok(pos)
    }

    /// Re-apply the last undone command.
    pub fn redo(&mut self, buffer: &mut Buffer) -> Result<Position> {
        let group = self.redo_stack.pop().ok_or(ViError::AtFirstChange)?;

        let mut pos = buffer.cursor();
        for change in group.iter() {
            pos = self.apply_change(change, buffer)?;
        }

        self.undo_stack.push(group);
        self.last_was_undo = false;
        Ok(pos)
    }

    /// The `u` command: reverse the last buffer-modifying command, *including a
    /// previous undo*, so pressing `u` twice returns to where you started
    /// (POSIX ex `undo`, 95442).
    pub fn undo_toggle(&mut self, buffer: &mut Buffer) -> Result<Position> {
        if self.last_was_undo {
            self.redo(buffer)
        } else {
            self.undo(buffer)
        }
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
            ChangeKind::ReplaceLines => {
                let old_count = change
                    .deleted_text
                    .as_ref()
                    .map(|t| t.split('\n').count())
                    .unwrap_or(0);
                let new_lines: Vec<&str> = change
                    .inserted_text
                    .as_deref()
                    .map(|t| t.split('\n').collect())
                    .unwrap_or_default();
                Self::splice_lines(buffer, change.position.line, old_count, &new_lines);
            }
            _ => {}
        }

        buffer.set_cursor(change.position);
        Ok(change.position)
    }

    /// Replace `old_count` lines starting at `start` with `new_lines`.
    ///
    /// The two runs may differ in length, which is exactly what
    /// [`ChangeKind::ReplaceLines`] exists to express.
    fn splice_lines(buffer: &mut Buffer, start: usize, old_count: usize, new_lines: &[&str]) {
        if old_count > 0 {
            let end = (start + old_count - 1).min(buffer.line_count());
            if start <= buffer.line_count() {
                buffer.delete_lines(start, end);
            }
        }
        for (i, text) in new_lines.iter().enumerate() {
            buffer.insert_line_after(start - 1 + i, Line::from(*text));
        }
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
                    // `set_cursor` clamps the column to command-mode bounds
                    // (at most `len - 1`), so text removed from the end of a
                    // line would be restored one column too far left. Position
                    // for insertion instead, which allows `len`.
                    buffer.set_line(change.position.line);
                    buffer.set_column_for_insert(change.position.column);
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
            ChangeKind::ReplaceLines => {
                // Undo = put the original run back in place of the new one.
                let new_count = change
                    .inserted_text
                    .as_ref()
                    .map(|t| t.split('\n').count())
                    .unwrap_or(0);
                let old_lines: Vec<&str> = change
                    .deleted_text
                    .as_deref()
                    .map(|t| t.split('\n').collect())
                    .unwrap_or_default();
                Self::splice_lines(buffer, change.position.line, new_count, &old_lines);
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
        self.in_group = false;
        self.line_original = None;
        self.last_was_undo = false;
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
