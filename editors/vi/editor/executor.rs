//! Ex command execution helpers.
//!
//! This module contains `impl Editor` methods for executing individual
//! ex commands. These are split from `editor.rs` to reduce file size
//! while remaining part of the same `Editor` type.

use super::Editor;
use crate::buffer::{Line, Position, Range};
use crate::error::{Result, ViError};
use crate::ex::command::SubstituteFlags;
use crate::ex::AddressRange;

impl Editor {
    /// Execute :join command - join lines together.
    pub(super) fn execute_ex_join(
        &mut self,
        range: &AddressRange,
        count: Option<usize>,
    ) -> Result<()> {
        let current = self.buffer.cursor().line;

        // Determine the range of lines to join
        let (start, mut end) = if range.explicit {
            range.resolve(&self.buffer, current)?
        } else if let Some(c) = count {
            // No address: current line and current + count
            (current, (current + c).min(self.buffer.line_count()))
        } else {
            // No address, no count: current line and next line
            (current, (current + 1).min(self.buffer.line_count()))
        };

        // Apply count to extend the range if both range and count are specified
        if let Some(c) = count {
            if range.explicit {
                end = (end + c - 1).min(self.buffer.line_count());
            }
        }

        if start >= end || start > self.buffer.line_count() {
            return Ok(()); // Nothing to join
        }

        // Build the joined line
        let mut result = String::new();
        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                let content = line.content();
                if result.is_empty() {
                    result = content.to_string();
                } else {
                    // Add space and trimmed content (POSIX: discard leading spaces)
                    let trimmed = content.trim_start();
                    if !trimmed.is_empty() {
                        if !result.is_empty() && !result.ends_with(' ') {
                            result.push(' ');
                        }
                        result.push_str(trimmed);
                    }
                }
            }
        }

        // Delete lines from end to start+1 (in reverse to preserve line numbers)
        for line_num in (start + 1..=end).rev() {
            self.buffer.delete_line(line_num);
        }

        // Replace the first line with the joined result
        if let Some(line) = self.buffer.line_mut(start) {
            *line = Line::from(result.as_str());
        }

        self.buffer.set_line(start);

        let join_count = end - start;
        self.set_message(&format!("{} lines joined", join_count + 1));
        Ok(())
    }

    /// Execute :put command - put text from register after line.
    pub(super) fn execute_ex_put(
        &mut self,
        line: Option<usize>,
        register: Option<char>,
    ) -> Result<()> {
        let target_line = line.unwrap_or_else(|| self.buffer.cursor().line);
        let reg = register.unwrap_or('"');

        let content = self.registers.get(reg).ok_or(ViError::BufferEmpty(reg))?;

        // Insert lines after target
        let lines: Vec<&str> = content.text.lines().collect();
        for (i, line_text) in lines.iter().enumerate() {
            self.buffer
                .insert_line_after(target_line + i, Line::from(*line_text));
        }

        self.buffer.set_line(target_line + lines.len());
        Ok(())
    }

    /// Execute :copy command - copy lines to destination.
    pub(super) fn execute_ex_copy(&mut self, range: &AddressRange, dest: usize) -> Result<()> {
        let current = self.buffer.cursor().line;
        let (start, end) = range.resolve(&self.buffer, current)?;

        // Collect the lines to copy
        let mut lines_to_copy = Vec::new();
        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                lines_to_copy.push(line.content().to_string());
            }
        }

        // Insert after destination line
        let insert_after = if dest == 0 { 0 } else { dest };
        for (i, line_text) in lines_to_copy.iter().enumerate() {
            self.buffer
                .insert_line_after(insert_after + i, Line::from(line_text.as_str()));
        }

        self.buffer.set_line(insert_after + lines_to_copy.len());

        let copy_count = end - start + 1;
        self.set_message(&format!("{} lines copied", copy_count));
        Ok(())
    }

    /// Execute :move command - move lines to destination.
    pub(super) fn execute_ex_move(&mut self, range: &AddressRange, dest: usize) -> Result<()> {
        let current = self.buffer.cursor().line;
        let (start, end) = range.resolve(&self.buffer, current)?;

        // Can't move lines into themselves
        if dest >= start && dest <= end {
            return Err(ViError::InvalidRange(
                "Cannot move lines into themselves".to_string(),
            ));
        }

        // Collect the lines to move
        let mut lines_to_move = Vec::new();
        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                lines_to_move.push(line.content().to_string());
            }
        }

        // Delete original lines (in reverse order)
        for line_num in (start..=end).rev() {
            self.buffer.delete_line(line_num);
        }

        // Adjust destination if it was after the deleted lines
        let adjusted_dest = if dest > end {
            dest - (end - start + 1)
        } else {
            dest
        };

        // Insert at new location
        let insert_after = if adjusted_dest == 0 { 0 } else { adjusted_dest };
        for (i, line_text) in lines_to_move.iter().enumerate() {
            self.buffer
                .insert_line_after(insert_after + i, Line::from(line_text.as_str()));
        }

        self.buffer.set_line(insert_after + lines_to_move.len());

        let move_count = end - start + 1;
        self.set_message(&format!("{} lines moved", move_count));
        Ok(())
    }

    /// Execute :read command - read file into buffer.
    pub(super) fn execute_ex_read(
        &mut self,
        range: &AddressRange,
        file: Option<&str>,
    ) -> Result<()> {
        let current = self.buffer.cursor().line;
        let insert_after = if range.explicit {
            let (_, end) = range.resolve(&self.buffer, current)?;
            end
        } else {
            current
        };

        let path = file
            .map(|s| s.to_string())
            .or_else(|| self.files.current_file().map(|p| p.display().to_string()))
            .ok_or(ViError::NoFileName)?;

        let content = std::fs::read_to_string(&path).map_err(ViError::Io)?;

        let lines: Vec<&str> = content.lines().collect();
        for (i, line_text) in lines.iter().enumerate() {
            self.buffer
                .insert_line_after(insert_after + i, Line::from(*line_text));
        }

        self.buffer.set_line(insert_after + lines.len());

        let bytes = content.len();
        self.set_message(&format!(
            "\"{}\" {} lines, {} bytes",
            path,
            lines.len(),
            bytes
        ));
        Ok(())
    }

    /// Execute :mark command - set a mark.
    pub(super) fn execute_ex_mark(&mut self, line: Option<usize>, name: char) -> Result<()> {
        let target_line = line.unwrap_or_else(|| self.buffer.cursor().line);

        if !name.is_ascii_lowercase() {
            return Err(ViError::MarkNotSet(name));
        }

        let idx = (name as u8 - b'a') as usize;
        self.marks[idx] = Some(Position::new(target_line, 0));
        Ok(())
    }

    /// Execute :z command - adjust window display.
    pub(super) fn execute_ex_z(
        &mut self,
        line: Option<usize>,
        ztype: Option<char>,
        count: Option<usize>,
    ) -> Result<Vec<String>> {
        let scroll = self.options.scroll;
        let count = count.unwrap_or(2 * scroll);
        let mut target_line = line.unwrap_or_else(|| self.buffer.cursor().line);

        // If no type and no line, advance to next line
        if line.is_none() && ztype.is_none() {
            target_line = (target_line + 1).min(self.buffer.line_count());
        }

        // Adjust target based on type
        let start_line = match ztype {
            Some('+') => target_line,
            Some('-') => target_line.saturating_sub(count - 1).max(1),
            Some('.') | Some('=') => {
                // Center on this line
                let half = count / 2;
                target_line.saturating_sub(half).max(1)
            }
            Some('^') => target_line.saturating_sub(2 * count - 1).max(1),
            None => target_line,
            _ => target_line,
        };

        // Collect lines to output
        let mut output = Vec::new();
        let end_line = (start_line + count - 1).min(self.buffer.line_count());

        if let Some('=') = ztype {
            // For '=', print separator line around current line
            let cols = self.terminal.size().cols as usize;
            let separator: String = "-".repeat(40.min(cols / 2));
            let half = count / 2;
            let before_start = target_line.saturating_sub(half).max(1);

            for i in before_start..target_line {
                if let Some(line) = self.buffer.line(i) {
                    output.push(line.content().to_string());
                }
            }
            output.push(separator.clone());
            if let Some(line) = self.buffer.line(target_line) {
                output.push(line.content().to_string());
            }
            output.push(separator);
            for i in (target_line + 1)..=(target_line + half).min(self.buffer.line_count()) {
                if let Some(line) = self.buffer.line(i) {
                    output.push(line.content().to_string());
                }
            }
        } else {
            for i in start_line..=end_line {
                if let Some(line) = self.buffer.line(i) {
                    output.push(line.content().to_string());
                }
            }
        }

        // Update current line
        let new_current = if ztype == Some('=') {
            target_line
        } else {
            end_line
        };
        self.buffer.set_line(new_current);
        self.buffer.move_to_first_non_blank();

        Ok(output)
    }

    /// Execute :< command - shift lines left.
    pub(super) fn execute_ex_shift_left(
        &mut self,
        range: &AddressRange,
        count: Option<usize>,
    ) -> Result<()> {
        let (start, end) = self.resolve_range(range)?;
        let shift_amount = count.unwrap_or(1) * self.options.shiftwidth;

        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                let content = line.content().to_string();
                // Count leading whitespace
                let leading: usize = content
                    .chars()
                    .take_while(|c| c.is_whitespace())
                    .map(|c| if c == '\t' { self.options.tabstop } else { 1 })
                    .sum();

                if leading > 0 {
                    let new_indent = leading.saturating_sub(shift_amount);
                    let trimmed = content.trim_start();
                    let new_content = format!("{}{}", " ".repeat(new_indent), trimmed);
                    let _ = self.buffer.replace_line(line_num, &new_content);
                }
            }
        }

        self.buffer.set_line(end);
        self.buffer.move_to_first_non_blank();
        Ok(())
    }

    /// Execute :> command - shift lines right.
    pub(super) fn execute_ex_shift_right(
        &mut self,
        range: &AddressRange,
        count: Option<usize>,
    ) -> Result<()> {
        let (start, end) = self.resolve_range(range)?;
        let shift_amount = count.unwrap_or(1) * self.options.shiftwidth;

        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                let content = line.content().to_string();
                // Don't shift empty lines
                if !content.is_empty() {
                    let new_content = format!("{}{}", " ".repeat(shift_amount), content);
                    let _ = self.buffer.replace_line(line_num, &new_content);
                }
            }
        }

        self.buffer.set_line(end);
        self.buffer.move_to_first_non_blank();
        Ok(())
    }

    /// Execute := command - print line number.
    pub(super) fn execute_ex_line_number(&self, line: Option<usize>) -> Result<usize> {
        // Default to last line in buffer
        let line_num = line.unwrap_or_else(|| self.buffer.line_count());
        Ok(line_num)
    }

    /// Execute :@ or :* command - execute buffer contents as ex commands.
    pub(super) fn execute_ex_execute(
        &mut self,
        range: &AddressRange,
        buffer: Option<char>,
    ) -> Result<()> {
        // Get the buffer to execute
        let buffer_char = buffer.unwrap_or_else(|| {
            // Use last executed buffer, default to unnamed
            self.last_macro_register.unwrap_or('"')
        });

        // Get buffer contents
        let content = if buffer_char == '"' {
            // Unnamed buffer
            self.registers.get('"').map(|r| r.text.clone())
        } else if buffer_char.is_ascii_alphabetic() {
            self.registers.get(buffer_char).map(|r| r.text.clone())
        } else {
            None
        };

        let content = match content {
            Some(c) if !c.is_empty() => c,
            _ => return Err(ViError::BufferEmpty(buffer_char)),
        };

        // Remember this buffer for @@ / **
        self.last_macro_register = Some(buffer_char);

        // Execute for each line in the range (or just once if no explicit range)
        let (start, end) = if range.explicit {
            self.resolve_range(range)?
        } else {
            let current = self.buffer.cursor().line;
            (current, current)
        };

        for line_num in start..=end {
            self.buffer.set_line(line_num);
            // Execute each line of the buffer content as an ex command
            for cmd_line in content.lines() {
                let cmd_line = cmd_line.trim();
                if !cmd_line.is_empty() {
                    self.execute_ex_input(cmd_line)?;
                }
            }
        }

        Ok(())
    }

    /// Execute :& command - repeat last substitute.
    pub(super) fn execute_ex_repeat_substitute(
        &mut self,
        range: &AddressRange,
        flags: &SubstituteFlags,
    ) -> Result<()> {
        // Get last substitute pattern and replacement
        let (pattern, replacement) = match &self.last_substitution {
            Some(sub) => (sub.pattern.clone(), sub.replacement.clone()),
            None => return Err(ViError::NoPreviousSubstitution),
        };

        // Use provided flags or default to last flags
        self.substitute(range, &pattern, &replacement, flags)
    }

    /// Resolve address range to line numbers.
    pub(super) fn resolve_range(&self, range: &AddressRange) -> Result<(usize, usize)> {
        range.resolve(&self.buffer, self.buffer.cursor().line)
    }

    /// Execute ex delete command (:d).
    pub(super) fn execute_ex_delete(
        &mut self,
        range: &AddressRange,
        register: Option<char>,
        count: Option<usize>,
    ) -> Result<()> {
        use crate::command::delete;

        let (start, end) = self.resolve_range(range)?;
        let end = if let Some(c) = count {
            (start + c - 1).min(self.buffer.line_count())
        } else {
            end
        };

        let start_pos = Position::new(start, 0);
        let end_pos = Position::new(end, 0);
        let del_range = Range::lines(start_pos, end_pos);

        let result = delete(&mut self.buffer, del_range, &mut self.registers, register)?;
        self.buffer.set_cursor(result.cursor);

        let line_count = end - start + 1;
        if line_count > 1 {
            self.set_message(&format!("{} lines deleted", line_count));
        }

        Ok(())
    }

    /// Execute ex yank command (:y).
    pub(super) fn execute_ex_yank(
        &mut self,
        range: &AddressRange,
        register: Option<char>,
        count: Option<usize>,
    ) -> Result<()> {
        use crate::command::yank;

        let (start, end) = self.resolve_range(range)?;
        let end = if let Some(c) = count {
            (start + c - 1).min(self.buffer.line_count())
        } else {
            end
        };

        let start_pos = Position::new(start, 0);
        let end_pos = Position::new(end, 0);
        let yank_range = Range::lines(start_pos, end_pos);

        let _ = yank(&self.buffer, yank_range, &mut self.registers, register);

        let line_count = end - start + 1;
        self.set_message(&format!("{} lines yanked", line_count));

        Ok(())
    }
}
