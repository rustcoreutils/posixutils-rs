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

/// One line of the buffer and the identity it keeps while it exists.
///
/// Historical ed stores marks as pointers into a linked list of lines, so a
/// mark follows its line through every edit and comes back with it on undo.
/// The identity gives a `Vec` the same property: marks name an identity,
/// not a position, so nothing has to renumber them, and an undo that puts
/// the old lines back puts their identities back too.
#[derive(Clone, Debug)]
struct Line {
    id: u64,
    text: String,
}

/// Undo record storing previous buffer state.
#[derive(Clone, Debug)]
pub struct UndoRecord {
    /// Lines before the change
    lines: Vec<Line>,
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
    lines: Vec<Line>,
    /// The identity the next new line gets.
    next_id: u64,
    /// Whether the buffer has been modified since last write
    pub modified: bool,
    /// Line marks ('a'-'z'), by line identity
    marks: HashMap<char, u64>,
    /// Single-level undo (POSIX requirement)
    undo_record: Option<UndoRecord>,
    /// Nesting depth of the current undo group.  While non-zero, individual
    /// mutations do not snapshot: the group takes one snapshot, at its first
    /// mutation, so the whole command undoes as a unit.
    undo_depth: usize,
    /// Whether the open group has taken its snapshot yet.
    ///
    /// Deferring it to the first actual mutation is what keeps a command that
    /// changes nothing from destroying the undo record: snapshotting when the
    /// group opens overwrites the previous state even if the command then
    /// fails, so a substitute that matched nothing made the edit before it
    /// unrecoverable.
    group_snapshotted: bool,
    /// Whether execution is inside a `g`/`v` command.  Distinct from
    /// `undo_depth` because it carries a POSIX *semantic* -- notably that a
    /// substitution may not split a line inside a global -- and so must not
    /// become true merely because some other command grouped its undo.
    in_global: bool,
}

impl Buffer {
    /// Create a new empty buffer.
    pub fn new() -> Buffer {
        Buffer {
            pathname: String::new(),
            cur_line: 0,
            lines: Vec::new(),
            next_id: 0,
            modified: false,
            marks: HashMap::new(),
            undo_record: None,
            undo_depth: 0,
            group_snapshotted: false,
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
            Some(&self.lines[line_num - 1].text)
        }
    }

    /// The identity of line `line_num`, which it keeps through every edit
    /// that does not replace or delete it.
    pub fn line_id(&self, line_num: usize) -> Option<u64> {
        line_num
            .checked_sub(1)
            .and_then(|i| self.lines.get(i))
            .map(|l| l.id)
    }

    /// Where the line with identity `id` is now, if it still exists.
    pub fn line_of_id(&self, id: u64) -> Option<usize> {
        self.lines.iter().position(|l| l.id == id).map(|i| i + 1)
    }

    /// Fresh lines, each with a new identity.
    fn new_lines(&mut self, texts: &[String]) -> Vec<Line> {
        texts
            .iter()
            .map(|text| {
                let id = self.next_id;
                self.next_id += 1;
                Line {
                    id,
                    text: text.clone(),
                }
            })
            .collect()
    }

    /// Replace the whole buffer with `texts`, as a read does: nothing to
    /// undo, no marks.
    fn reset_lines(&mut self, texts: &[String]) {
        self.lines = self.new_lines(texts);
        self.cur_line = self.lines.len();
        self.modified = false;
        self.marks.clear();
        self.undo_record = None;
    }

    /// Take the undo snapshot unconditionally.
    fn snapshot(&mut self) {
        self.undo_record = Some(UndoRecord {
            lines: self.lines.clone(),
            cur_line: self.cur_line,
            modified: self.modified,
        });
    }

    /// Save the current state for undo.
    ///
    /// Inside a group only the first mutation snapshots: that state is what
    /// `u` must restore, and it is taken here rather than when the group opens
    /// so a command that ends up changing nothing leaves the undo record
    /// alone.
    fn save_undo(&mut self) {
        if self.undo_depth == 0 {
            self.snapshot();
        } else if !self.group_snapshotted {
            self.snapshot();
            self.group_snapshotted = true;
        }
    }

    /// Open an undo group: everything until the matching `end_undo_group`
    /// undoes as one command.  Nests, so a group inside a global does not
    /// re-snapshot over the global's state.
    pub fn begin_undo_group(&mut self) {
        if self.undo_depth == 0 {
            self.group_snapshotted = false;
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
    ///
    /// Marks need nothing: the restored lines carry their identities, so a
    /// mark on one of them names it again, and a mark on a line the change
    /// made is gone with that line.
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

    /// The current line after lines were removed from `start` on with
    /// nothing put in their place.
    ///
    /// POSIX: "The address of the line after the last line deleted shall
    /// become the current line number; if the lines deleted were originally
    /// at the end of the buffer, the current line number shall be set to the
    /// address of the new last line; if no lines remain in the buffer, the
    /// current line number shall be set to zero."
    fn cur_after_removal(&self, start: usize) -> usize {
        start.min(self.lines.len())
    }

    /// Check that `dest` is a line `m` or `t` can put lines after (0: before
    /// the first).
    fn check_dest(&self, dest: usize) -> EdResult<()> {
        if dest > self.lines.len() {
            return Err(EdError::AddressOutOfRange);
        }
        Ok(())
    }

    /// Check that `start..=end` is a non-empty range of existing lines.
    fn check_range(&self, start: usize, end: usize) -> EdResult<()> {
        if start == 0 || start > end || end > self.lines.len() {
            return Err(EdError::AddressOutOfRange);
        }
        Ok(())
    }

    /// Append lines after the current line (or at start if empty).
    pub fn append(&mut self, at_line: usize, lines: &[String]) {
        self.save_undo();

        if !lines.is_empty() {
            let new = self.new_lines(lines);
            self.lines.splice(at_line..at_line, new);
            self.cur_line = at_line + lines.len();
            self.modified = true;
        }
    }

    /// Insert lines before the specified line.
    pub fn insert(&mut self, before_line: usize, lines: &[String]) {
        self.append(before_line.saturating_sub(1), lines);
    }

    /// Delete lines from start to end (inclusive, 1-indexed).
    pub fn delete(&mut self, start: usize, end: usize) -> EdResult<()> {
        self.check_range(start, end)?;

        self.save_undo();
        self.lines.drain(start - 1..end);
        self.cur_line = self.cur_after_removal(start);
        self.modified = true;

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
            writer.write_all(line.text.as_bytes())?;
            byte_count += line.text.len();
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

    /// Read every line of `pathname`, and how many bytes that was.
    fn read_lines(pathname: &str) -> io::Result<(Vec<String>, usize)> {
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
        Ok((lines, byte_count))
    }

    /// Read a file into the buffer, replacing contents.
    pub fn read_file(&mut self, pathname: &str) -> io::Result<usize> {
        // POSIX e: the remembered pathname is set to the file whether or not
        // it can be read, so a file that does not exist yet is the one `w`
        // writes.
        self.pathname = String::from(pathname);
        let (lines, byte_count) = Self::read_lines(pathname)?;
        self.reset_lines(&lines);
        Ok(byte_count)
    }

    /// Load buffer from a string (for shell command output).
    /// Does not set pathname.
    pub fn load_from_string(&mut self, content: &str) {
        let lines: Vec<String> = content.lines().map(|l| format!("{}\n", l)).collect();
        self.reset_lines(&lines);
    }

    /// Append content from a string after a line (for shell command output).
    pub fn append_from_string(&mut self, after_line: usize, content: &str) {
        let lines: Vec<String> = content.lines().map(|l| format!("{}\n", l)).collect();
        if !lines.is_empty() {
            self.append(after_line, &lines);
        }
    }

    /// Read a file and append after a line.
    pub fn read_file_at(&mut self, pathname: &str, after_line: usize) -> io::Result<usize> {
        // POSIX r: the file becomes the remembered pathname when none is,
        // whether or not it can be read.
        if self.pathname.is_empty() {
            self.pathname = String::from(pathname);
        }
        let (lines, byte_count) = Self::read_lines(pathname)?;
        if !lines.is_empty() {
            self.append(after_line, &lines);
        }
        Ok(byte_count)
    }

    /// Set a mark on a line.
    pub fn set_mark(&mut self, mark: char, line: usize) -> EdResult<()> {
        let id = self.line_id(line).ok_or(EdError::AddressOutOfRange)?;
        self.marks.insert(mark, id);
        Ok(())
    }

    /// Get the line number for a mark: none once its line is gone, so `'a`
    /// is an invalid address rather than whatever moved into its place.
    /// (A scan of the buffer, linear in its length: no index is kept from
    /// identity to position, since every insertion would have to update it.)
    pub fn get_mark(&self, mark: char) -> Option<usize> {
        self.marks.get(&mark).and_then(|&id| self.line_of_id(id))
    }

    /// Join lines from start to end into a single line.
    pub fn join(&mut self, start: usize, end: usize) -> EdResult<()> {
        self.check_range(start, end)?;

        if start == end {
            // Nothing to join
            return Ok(());
        }

        let mut joined = String::new();
        for line in &self.lines[start - 1..end] {
            joined.push_str(line.text.trim_end_matches('\n'));
        }
        joined.push('\n');

        // The joined text is a new line, so marks on any of the joined lines
        // go with the originals -- matching historical ed, which allocates a
        // fresh node for the join.
        self.change(start, end, &[joined])
    }

    /// Move lines from (start, end) to after dest_line.
    pub fn move_lines(&mut self, start: usize, end: usize, dest: usize) -> EdResult<()> {
        self.check_range(start, end)?;
        self.check_dest(dest)?;
        // POSIX (ed, `m`): it is an error if the destination falls within the
        // range of moved lines -- and the range is inclusive, so `dest == end`
        // is inside it.  Letting it through indexed past the drained vector.
        if dest >= start && dest <= end {
            return Err(EdError::InvalidAddress);
        }

        self.save_undo();

        // The moved lines keep their identities, so marks go with them.
        let extracted: Vec<Line> = self.lines.drain(start - 1..end).collect();
        let num_lines = extracted.len();
        let adjusted_dest = if dest > end { dest - num_lines } else { dest };
        self.lines.splice(adjusted_dest..adjusted_dest, extracted);

        self.cur_line = adjusted_dest + num_lines;
        self.modified = true;

        Ok(())
    }

    /// Copy lines from (start, end) to after dest_line.
    pub fn copy_lines(&mut self, start: usize, end: usize, dest: usize) -> EdResult<()> {
        self.check_range(start, end)?;
        self.check_dest(dest)?;

        // The copies are new lines: the original keeps its mark.
        let texts: Vec<String> = self.lines[start - 1..end]
            .iter()
            .map(|l| l.text.clone())
            .collect();
        self.append(dest, &texts);
        Ok(())
    }

    /// Change lines (delete and prepare for insert).
    pub fn change(&mut self, start: usize, end: usize, new_lines: &[String]) -> EdResult<()> {
        self.check_range(start, end)?;

        self.save_undo();

        // The replacement lines are new: a mark on a replaced line goes.
        let new = self.new_lines(new_lines);
        self.lines.splice(start - 1..end, new);

        self.cur_line = if new_lines.is_empty() {
            // POSIX: with no lines input, as for `d`.
            self.cur_after_removal(start)
        } else {
            start - 1 + new_lines.len()
        };

        self.modified = true;

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
