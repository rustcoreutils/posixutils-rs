//
// Copyright (c) 2024-2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Buffer management for the ed editor.

use crate::ed::error::{EdError, EdResult};
use std::collections::HashMap;
use std::fs;
use std::io::{self, BufRead, BufReader, Write};

/// Undo record storing previous buffer state.
#[derive(Clone, Debug)]
pub struct UndoRecord {
    /// Lines before the change
    lines: Vec<String>,
    /// Current line before the change
    cur_line: usize,
    /// Whether the buffer was modified before the change
    modified: bool,
}

/// The ed buffer holding all lines.
#[derive(Debug)]
pub struct Buffer {
    /// The remembered filename
    pub pathname: String,
    /// Current line number (1-indexed, 0 means empty buffer)
    pub cur_line: usize,
    /// All lines in the buffer
    lines: Vec<String>,
    /// Whether the buffer has been modified since last write
    pub modified: bool,
    /// Line marks ('a'-'z')
    marks: HashMap<char, usize>,
    /// Single-level undo (POSIX requirement)
    undo_record: Option<UndoRecord>,
    /// Nesting depth of the current undo group.  While non-zero, individual
    /// mutations do not snapshot: the group took one snapshot up front, so
    /// the whole command undoes as a unit.
    undo_depth: usize,
    /// Whether execution is inside a `g`/`v` command.  Distinct from
    /// `undo_depth` because it carries a POSIX *semantic* -- notably that a
    /// substitution may not split a line inside a global -- and so must not
    /// become true merely because some other command grouped its undo.
    in_global: bool,
}

/// A structural change to the buffer's line numbering.
///
/// Every mutator reports one of these so that line *references* -- marks
/// today -- keep denoting the same text.  Historical ed gets this for free by
/// storing marks as pointers into a linked list; with a `Vec<String>` the
/// bookkeeping has to be explicit, and doing it in `delete` alone left a mark
/// silently naming a different line after any other edit.
#[derive(Debug, Clone, Copy)]
enum LineEdit {
    /// `count` lines inserted immediately after line `after`.
    Insert { after: usize, count: usize },
    /// Lines `start..=end` removed.
    Delete { start: usize, end: usize },
    /// Lines `start..=end` replaced by `count` new lines.
    Replace {
        start: usize,
        end: usize,
        count: usize,
    },
    /// Lines `start..=end` moved to immediately after line `dest`.
    Move {
        start: usize,
        end: usize,
        dest: usize,
    },
}

impl Buffer {
    /// Create a new empty buffer.
    pub fn new() -> Buffer {
        Buffer {
            pathname: String::new(),
            cur_line: 0,
            lines: Vec::new(),
            modified: false,
            marks: HashMap::new(),
            undo_record: None,
            undo_depth: 0,
            in_global: false,
        }
    }

    /// Return the number of lines in the buffer.
    pub fn line_count(&self) -> usize {
        self.lines.len()
    }

    /// Return the last line number (same as line_count for 1-indexed).
    pub fn last_line(&self) -> usize {
        self.lines.len()
    }

    /// Set the current line.
    pub fn set_cur_line(&mut self, line: usize) -> EdResult<()> {
        if line > self.last_line() {
            return Err(EdError::AddressOutOfRange);
        }
        self.cur_line = line;
        Ok(())
    }

    /// Get a line by number (1-indexed).
    pub fn get_line(&self, line_num: usize) -> Option<&String> {
        if line_num == 0 || line_num > self.lines.len() {
            None
        } else {
            Some(&self.lines[line_num - 1])
        }
    }

    /// Take the undo snapshot unconditionally.
    fn snapshot(&mut self) {
        self.undo_record = Some(UndoRecord {
            lines: self.lines.clone(),
            cur_line: self.cur_line,
            modified: self.modified,
        });
    }

    /// Save the current state for undo, unless a group is already open --
    /// in which case the group's own snapshot is the one `u` must restore.
    fn save_undo(&mut self) {
        if self.undo_depth == 0 {
            self.snapshot();
        }
    }

    /// Open an undo group: everything until the matching `end_undo_group`
    /// undoes as one command.  Nests, so a group inside a global does not
    /// re-snapshot over the global's state.
    pub fn begin_undo_group(&mut self) {
        if self.undo_depth == 0 {
            self.snapshot();
        }
        self.undo_depth += 1;
    }

    /// Close an undo group.
    pub fn end_undo_group(&mut self) {
        self.undo_depth = self.undo_depth.saturating_sub(1);
    }

    /// Begin a global command: one undo group, plus the `g`/`v` semantics.
    pub fn begin_global(&mut self) {
        self.begin_undo_group();
        self.in_global = true;
    }

    /// End a global command.
    pub fn end_global(&mut self) {
        self.in_global = false;
        self.end_undo_group();
    }

    /// Check if currently in a global command context.
    pub fn is_in_global(&self) -> bool {
        self.in_global
    }

    /// Undo the last change. Returns true if undo was performed.
    pub fn undo(&mut self) -> bool {
        if let Some(record) = self.undo_record.take() {
            // Swap current state with undo record
            let current = UndoRecord {
                lines: std::mem::replace(&mut self.lines, record.lines),
                cur_line: std::mem::replace(&mut self.cur_line, record.cur_line),
                modified: std::mem::replace(&mut self.modified, record.modified),
            };
            self.undo_record = Some(current);
            true
        } else {
            false
        }
    }

    /// Renumber line references to survive a structural change.
    fn apply_line_edit(&mut self, edit: LineEdit) {
        match edit {
            LineEdit::Insert { after, count } => {
                for line in self.marks.values_mut() {
                    if *line > after {
                        *line += count;
                    }
                }
            }
            LineEdit::Delete { start, end } => {
                // A mark on removed text goes with it: `'a` then reports an
                // invalid address rather than naming whatever moved up.
                self.marks.retain(|_, line| *line < start || *line > end);
                let removed = end - start + 1;
                for line in self.marks.values_mut() {
                    if *line > end {
                        *line -= removed;
                    }
                }
            }
            LineEdit::Replace { start, end, count } => {
                self.marks.retain(|_, line| *line < start || *line > end);
                let removed = end - start + 1;
                for line in self.marks.values_mut() {
                    if *line > end {
                        *line = *line - removed + count;
                    }
                }
            }
            LineEdit::Move { start, end, dest } => {
                let moved = end - start + 1;
                // Where the block lands once it has been lifted out.
                let landing = if dest > end { dest - moved } else { dest };
                for line in self.marks.values_mut() {
                    *line = if *line >= start && *line <= end {
                        // Inside the block: keep its offset, follow the text.
                        landing + (*line - start) + 1
                    } else if *line < start {
                        if *line > landing {
                            *line + moved
                        } else {
                            *line
                        }
                    } else {
                        // Below the block, so it first shifts up by `moved`.
                        let lifted = *line - moved;
                        if lifted > landing {
                            *line
                        } else {
                            lifted
                        }
                    };
                }
            }
        }
    }

    /// Append lines after the current line (or at start if empty).
    pub fn append(&mut self, at_line: usize, lines: &[String]) {
        self.save_undo();

        let insert_idx = if at_line == 0 { 0 } else { at_line };

        for (i, line) in lines.iter().enumerate() {
            self.lines.insert(insert_idx + i, line.clone());
        }

        if !lines.is_empty() {
            self.cur_line = insert_idx + lines.len();
            self.modified = true;
            self.apply_line_edit(LineEdit::Insert {
                after: insert_idx,
                count: lines.len(),
            });
        }
    }

    /// Insert lines before the specified line.
    pub fn insert(&mut self, before_line: usize, lines: &[String]) {
        self.save_undo();

        let insert_idx = if before_line == 0 { 0 } else { before_line - 1 };

        for (i, line) in lines.iter().enumerate() {
            self.lines.insert(insert_idx + i, line.clone());
        }

        if !lines.is_empty() {
            self.cur_line = insert_idx + lines.len();
            self.modified = true;
            self.apply_line_edit(LineEdit::Insert {
                after: insert_idx,
                count: lines.len(),
            });
        }
    }

    /// Delete lines from start to end (inclusive, 1-indexed).
    pub fn delete(&mut self, start: usize, end: usize) -> EdResult<()> {
        if start == 0 || start > end || end > self.lines.len() {
            return Err(EdError::AddressOutOfRange);
        }

        self.save_undo();
        self.lines.drain(start - 1..end);

        // POSIX: "The address of the line after the last line deleted shall
        // become the current line number; if the lines deleted were originally
        // at the end of the buffer, the current line number shall be set to
        // the address of the new last line; if no lines remain in the buffer,
        // the current line number shall be set to zero."
        if self.lines.is_empty() {
            self.cur_line = 0;
        } else if start <= self.lines.len() {
            // Line after deleted exists at position 'start'
            self.cur_line = start;
        } else {
            // Deleted at end, current is new last line
            self.cur_line = self.lines.len();
        }

        self.modified = true;
        self.apply_line_edit(LineEdit::Delete { start, end });

        Ok(())
    }

    /// Write lines to a writer.
    pub fn write_lines<W: Write>(
        &self,
        start_line: usize,
        end_line: usize,
        writer: &mut W,
    ) -> io::Result<usize> {
        if start_line == 0 || start_line > end_line || end_line > self.lines.len() {
            return Ok(0);
        }

        let mut byte_count = 0;
        for line in &self.lines[start_line - 1..end_line] {
            writer.write_all(line.as_bytes())?;
            byte_count += line.len();
        }
        Ok(byte_count)
    }

    /// Write buffer to a file.
    pub fn write_to_file(
        &mut self,
        start_line: usize,
        end_line: usize,
        pathname: &str,
    ) -> io::Result<usize> {
        let mut file = fs::File::create(pathname)?;
        let bytes = self.write_lines(start_line, end_line, &mut file)?;

        // If we wrote the whole buffer, mark as unmodified
        if start_line == 1 && end_line == self.lines.len() {
            self.modified = false;
        }

        Ok(bytes)
    }

    /// Read a file into the buffer, replacing contents.
    pub fn read_file(&mut self, pathname: &str) -> io::Result<usize> {
        let file = fs::File::open(pathname)?;
        let mut reader = BufReader::new(file);
        let mut lines = Vec::new();
        let mut byte_count = 0;

        loop {
            let mut line = String::new();
            let rc = reader.read_line(&mut line)?;
            if rc == 0 {
                break;
            }
            byte_count += line.len();
            lines.push(line);
        }

        self.lines = lines;
        self.pathname = String::from(pathname);
        self.cur_line = if self.lines.is_empty() {
            0
        } else {
            self.lines.len()
        };
        self.modified = false;
        self.marks.clear();
        self.undo_record = None;

        Ok(byte_count)
    }

    /// Load buffer from a string (for shell command output).
    /// Does not set pathname.
    pub fn load_from_string(&mut self, content: &str) {
        let mut lines = Vec::new();
        for line in content.lines() {
            lines.push(format!("{}\n", line));
        }
        // Handle case where content doesn't end with newline
        if !content.is_empty() && !content.ends_with('\n') {
            // Last line already has \n from the loop above
        }
        self.lines = lines;
        self.cur_line = if self.lines.is_empty() {
            0
        } else {
            self.lines.len()
        };
        self.modified = false;
        self.marks.clear();
        self.undo_record = None;
    }

    /// Append content from a string after a line (for shell command output).
    pub fn append_from_string(&mut self, after_line: usize, content: &str) {
        let mut lines = Vec::new();
        for line in content.lines() {
            lines.push(format!("{}\n", line));
        }
        if !lines.is_empty() {
            self.append(after_line, &lines);
        }
    }

    /// Read a file and append after a line.
    pub fn read_file_at(&mut self, pathname: &str, after_line: usize) -> io::Result<usize> {
        let file = fs::File::open(pathname)?;
        let mut reader = BufReader::new(file);
        let mut lines = Vec::new();
        let mut byte_count = 0;

        loop {
            let mut line = String::new();
            let rc = reader.read_line(&mut line)?;
            if rc == 0 {
                break;
            }
            byte_count += line.len();
            lines.push(line);
        }

        if !lines.is_empty() {
            self.append(after_line, &lines);
        }

        Ok(byte_count)
    }

    /// Set a mark on a line.
    pub fn set_mark(&mut self, mark: char, line: usize) -> EdResult<()> {
        if line == 0 || line > self.lines.len() {
            return Err(EdError::AddressOutOfRange);
        }
        self.marks.insert(mark, line);
        Ok(())
    }

    /// Get the line number for a mark.
    pub fn get_mark(&self, mark: char) -> Option<usize> {
        self.marks.get(&mark).copied()
    }

    /// Join lines from start to end into a single line.
    pub fn join(&mut self, start: usize, end: usize) -> EdResult<()> {
        if start == 0 || start > end || end > self.lines.len() {
            return Err(EdError::AddressOutOfRange);
        }

        if start == end {
            // Nothing to join
            return Ok(());
        }

        self.save_undo();

        // Collect and join lines
        let mut joined = String::new();
        for i in start..=end {
            let line = &self.lines[i - 1];
            // Strip trailing newline before joining
            let content = line.trim_end_matches('\n');
            joined.push_str(content);
        }
        joined.push('\n');

        // Replace the range with the joined line
        self.lines.drain(start - 1..end);
        self.lines.insert(start - 1, joined);

        self.cur_line = start;
        self.modified = true;
        // The joined text is a new line, so marks on any of the joined lines
        // go with the originals -- matching historical ed, which allocates a
        // fresh node for the join.
        self.apply_line_edit(LineEdit::Replace {
            start,
            end,
            count: 1,
        });

        Ok(())
    }

    /// Move lines from (start, end) to after dest_line.
    pub fn move_lines(&mut self, start: usize, end: usize, dest: usize) -> EdResult<()> {
        if start == 0 || start > end || end > self.lines.len() {
            return Err(EdError::AddressOutOfRange);
        }
        // POSIX (ed, `m`): it is an error if the destination falls within the
        // range of moved lines -- and the range is inclusive, so `dest == end`
        // is inside it.  Letting it through indexed past the drained vector.
        if dest >= start && dest <= end {
            return Err(EdError::InvalidAddress);
        }

        self.save_undo();

        // Extract lines
        let extracted: Vec<String> = self.lines.drain(start - 1..end).collect();
        let num_lines = extracted.len();

        // Adjust destination if it was after the removed lines
        let adjusted_dest = if dest > end { dest - num_lines } else { dest };

        // Insert at destination
        for (i, line) in extracted.into_iter().enumerate() {
            self.lines.insert(adjusted_dest + i, line);
        }

        self.cur_line = adjusted_dest + num_lines;
        self.modified = true;
        self.apply_line_edit(LineEdit::Move { start, end, dest });

        Ok(())
    }

    /// Copy lines from (start, end) to after dest_line.
    pub fn copy_lines(&mut self, start: usize, end: usize, dest: usize) -> EdResult<()> {
        if start == 0 || start > end || end > self.lines.len() {
            return Err(EdError::AddressOutOfRange);
        }

        self.save_undo();

        // Copy lines
        let copied: Vec<String> = self.lines[start - 1..end].to_vec();
        let num_lines = copied.len();

        // Insert at destination. Nothing is removed, so unlike `move_lines`
        // the destination needs no adjustment.
        for (i, line) in copied.into_iter().enumerate() {
            self.lines.insert(dest + i, line);
        }

        self.cur_line = dest + num_lines;
        self.modified = true;
        // The original keeps its mark; the copy is unmarked.
        self.apply_line_edit(LineEdit::Insert {
            after: dest,
            count: num_lines,
        });

        Ok(())
    }

    /// Change lines (delete and prepare for insert).
    pub fn change(&mut self, start: usize, end: usize, new_lines: &[String]) -> EdResult<()> {
        if start == 0 || start > end || end > self.lines.len() {
            return Err(EdError::AddressOutOfRange);
        }

        self.save_undo();

        // Remove old lines
        self.lines.drain(start - 1..end);

        // Insert new lines
        for (i, line) in new_lines.iter().enumerate() {
            self.lines.insert(start - 1 + i, line.clone());
        }

        if new_lines.is_empty() {
            // POSIX: if no lines input, current line is:
            // - the line after the last deleted (now at position start-1)
            // - if deleted at end, the new last line
            // - if buffer empty, 0
            if self.lines.is_empty() {
                self.cur_line = 0;
            } else if start <= self.lines.len() {
                // Line after deleted is at position start
                self.cur_line = start;
            } else {
                // Deleted at end, current is new last line
                self.cur_line = self.lines.len();
            }
        } else {
            self.cur_line = start - 1 + new_lines.len();
        }

        self.modified = true;
        self.apply_line_edit(LineEdit::Replace {
            start,
            end,
            count: new_lines.len(),
        });

        Ok(())
    }
}

impl Default for Buffer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_buffer() {
        let buf = Buffer::new();
        assert_eq!(buf.line_count(), 0);
        assert_eq!(buf.cur_line, 0);
        assert!(!buf.modified);
    }

    #[test]
    fn test_append() {
        let mut buf = Buffer::new();
        buf.append(0, &["line 1\n".to_string(), "line 2\n".to_string()]);
        assert_eq!(buf.line_count(), 2);
        assert_eq!(buf.cur_line, 2);
        assert!(buf.modified);
    }

    #[test]
    fn test_insert() {
        let mut buf = Buffer::new();
        buf.append(0, &["line 2\n".to_string()]);
        buf.insert(1, &["line 1\n".to_string()]);
        assert_eq!(buf.line_count(), 2);
        assert_eq!(buf.get_line(1), Some(&"line 1\n".to_string()));
        assert_eq!(buf.get_line(2), Some(&"line 2\n".to_string()));
    }

    #[test]
    fn test_delete() {
        let mut buf = Buffer::new();
        buf.append(
            0,
            &[
                "line 1\n".to_string(),
                "line 2\n".to_string(),
                "line 3\n".to_string(),
            ],
        );
        buf.delete(2, 2).unwrap();
        assert_eq!(buf.line_count(), 2);
        assert_eq!(buf.get_line(1), Some(&"line 1\n".to_string()));
        assert_eq!(buf.get_line(2), Some(&"line 3\n".to_string()));
    }

    #[test]
    fn test_undo() {
        let mut buf = Buffer::new();
        buf.append(0, &["line 1\n".to_string()]);
        assert_eq!(buf.line_count(), 1);

        buf.delete(1, 1).unwrap();
        assert_eq!(buf.line_count(), 0);

        assert!(buf.undo());
        assert_eq!(buf.line_count(), 1);
    }

    #[test]
    fn test_join() {
        let mut buf = Buffer::new();
        buf.append(0, &["hello\n".to_string(), "world\n".to_string()]);
        buf.join(1, 2).unwrap();
        assert_eq!(buf.line_count(), 1);
        assert_eq!(buf.get_line(1), Some(&"helloworld\n".to_string()));
    }
}
