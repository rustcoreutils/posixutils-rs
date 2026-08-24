//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Ex command execution helpers.
//!
//! This module contains `impl Editor` methods for executing individual
//! ex commands. These are split from `editor.rs` to reduce file size
//! while remaining part of the same `Editor` type.

use super::Editor;
use crate::buffer::{Line, Position, Range};
use crate::error::{Result, ViError};
use crate::ex::address::{AddrCtx, Address};
use crate::ex::command::SubstituteFlags;
use crate::ex::AddressRange;

impl Editor {
    /// Execute :join command - join lines together.
    pub(super) fn execute_ex_join(
        &mut self,
        range: &AddressRange,
        count: Option<usize>,
        force: bool,
    ) -> Result<()> {
        let current = self.buffer.cursor().line;

        // Range/count interaction per ex.md §95043-95057.
        //   count, no address     -> (., . + count)
        //   count, one address    -> (addr, addr + count)
        //   count, two addresses  -> (addr1, addr2 + count - 1)
        //   no count, no address  -> (., . + 1)
        //   no count, one address -> (addr, addr + 1)
        // A resulting second address past the last line is clamped to it.
        let two_addrs = range.end.is_some();
        let (start, mut end) = if range.explicit {
            range.resolve(&self.addr_ctx_at(current))?
        } else {
            (current, current)
        };
        end = match count {
            Some(c) if two_addrs => end + c.saturating_sub(1),
            Some(c) => start + c,
            None if two_addrs => end,
            None => start + 1,
        };
        let end = end.min(self.buffer.line_count());

        if start >= end || start > self.buffer.line_count() {
            return Ok(()); // Nothing to join
        }

        // Build the joined line following ex.md §95060-95070 exactly.
        //
        // `j!` joins with no modification at all. Otherwise, for each
        // subsequent line: discard leading blanks; skip the line if that left
        // it empty; join with no separator if the accumulated text already ends
        // in a blank or the joined line starts with ')'; use TWO spaces if the
        // accumulated text ends in '.'; otherwise a single space.
        let mut result = String::new();
        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                let content = line.content();
                if line_num == start {
                    result = content.to_string();
                    continue;
                }
                if force {
                    result.push_str(content);
                    continue;
                }
                let trimmed = content.trim_start_matches([' ', '\t']);
                if trimmed.is_empty() {
                    continue;
                }
                if result.ends_with([' ', '\t']) || trimmed.starts_with(')') {
                    // join without further modification
                } else if result.ends_with('.') {
                    result.push_str("  ");
                } else {
                    result.push(' ');
                }
                result.push_str(trimmed);
            }
        }

        // Delete lines from end to start+1 (in reverse to preserve line numbers)
        for line_num in (start + 1..=end).rev() {
            self.buffer.delete_line(line_num);
        }

        // Replace the first line with the joined result
        let _ = self.buffer.replace_line(start, &result);

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
    pub(super) fn execute_ex_copy(&mut self, range: &AddressRange, dest: &Address) -> Result<()> {
        // Resolved here, not at parse time: `$`, `.` and a search all depend
        // on the buffer.  `allow_zero` because the destination names the line
        // to insert *after*, so 0 means "before the first line".
        let current = self.buffer.cursor().line;
        let (start, end) = range.resolve(&self.addr_ctx_at(current))?;

        // Collect the lines to copy
        let mut lines_to_copy = Vec::new();
        for line_num in start..=end {
            if let Some(line) = self.buffer.line(line_num) {
                lines_to_copy.push(line.content().to_string());
            }
        }

        // Insert after destination line
        let insert_after = dest.resolve(&self.addr_ctx_allow_zero_at(current))?;
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
    pub(super) fn execute_ex_move(&mut self, range: &AddressRange, dest: &Address) -> Result<()> {
        let current = self.buffer.cursor().line;
        let (start, end) = range.resolve(&self.addr_ctx_at(current))?;
        let dest = dest.resolve(&self.addr_ctx_allow_zero_at(current))?;

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
        // #X15: `0r file` is legal -- line 0 means "insert before line 1".
        let insert_after = if range.explicit {
            let (_, end) = range.resolve(&self.addr_ctx_allow_zero())?;
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
    ///
    /// `z[!][type ...][count][flags]` (95550-95595). `repeats` is how many times
    /// the type character was given: POSIX defines the displacement in terms of
    /// that number, so `z--` and `z++` move further than `z-` and `z+`.
    pub(super) fn execute_ex_z(
        &mut self,
        line: Option<usize>,
        ztype: Option<char>,
        repeats: usize,
        count: Option<usize>,
        full_screen: bool,
    ) -> Result<Vec<String>> {
        // "If count is specified, the value of the window edit option shall be
        // set to count. If count is omitted, it shall default to 2 times the
        // value of the scroll edit option, or if ! was specified, the number of
        // lines in the display minus 1." (95556-95558)
        let count = match count {
            Some(n) => {
                self.options.window = n;
                n
            }
            None if full_screen => (self.terminal.size().rows as usize)
                .saturating_sub(1)
                .max(1),
            None => 2 * self.options.scroll,
        };
        // "If count is zero, nothing shall be written" (95574). Every type's
        // displacement formula also degenerates at zero -- `repeats * count - 1`
        // and `(repeats + 1) * count - 1` would underflow `usize`, aborting the
        // editor in a build with overflow checks and wrapping into a spurious
        // out-of-buffer error without them. Reachable as a literal `z-0`, and
        // via `2 * scroll` when `scroll` is set to 0.
        if count == 0 {
            return Ok(Vec::new());
        }

        let last = self.buffer.line_count();
        let mut target_line = line.unwrap_or_else(|| self.buffer.cursor().line);

        // "If no line is specified, the current line shall be the default; if
        // type is omitted as well, the current line value shall first be
        // incremented by 1. If incrementing the current line would cause it to
        // be greater than the last line ... it shall be an error."
        if line.is_none() && ztype.is_none() {
            if target_line >= last {
                return Err(ViError::InvalidAddress("z: at end of buffer".to_string()));
            }
            target_line += 1;
        }

        // Displacements are defined per type in terms of the number of type
        // characters given; going off either end of the buffer is an error
        // rather than a clamp.
        let repeats = repeats.max(1);
        let start_line = match ztype {
            // "(((number of '+' characters) -1) x count) +1", and lines are
            // written "starting at the new value of line" -- including for a
            // single `+`, whose displacement is 1. Returning `target_line` here
            // re-displayed the line the user was already on.
            Some('+') => {
                let advance = (repeats - 1).saturating_mul(count) + 1;
                let start = target_line + advance;
                if start > last {
                    return Err(ViError::InvalidAddress("z: past end of buffer".to_string()));
                }
                start
            }
            // "(((number of '-' characters) x count) -1)"
            Some('-') => decrement_or_err(target_line, repeats.saturating_mul(count) - 1)?,
            Some('.') | Some('=') => {
                // Centred: half a screen either side.
                target_line.saturating_sub(count / 2).max(1)
            }
            // "(((number of 'ˆ' characters) +1) x count) -1"
            Some('^') => decrement_or_err(target_line, (repeats + 1).saturating_mul(count) - 1)?,
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

    /// Address-resolution context relative to `current`.
    ///
    /// Everything that resolves an ex address should go through this or
    /// [`Self::resolve_range`], so marks and the line-0 rule stay consistent.
    pub(super) fn addr_ctx_at(&self, current: usize) -> AddrCtx<'_> {
        AddrCtx {
            buffer: &self.buffer,
            current,
            marks: &self.marks,
            allow_zero: false,
        }
    }

    /// Context relative to the cursor's line.
    pub(super) fn addr_ctx(&self) -> AddrCtx<'_> {
        self.addr_ctx_at(self.buffer.cursor().line)
    }

    /// Context for the commands that insert *after* an address, where line 0 is
    /// legal and means "before the first line" (#X15).
    pub(super) fn addr_ctx_allow_zero(&self) -> AddrCtx<'_> {
        self.addr_ctx_allow_zero_at(self.buffer.cursor().line)
    }

    /// As [`Self::addr_ctx_allow_zero`], relative to `current`.
    pub(super) fn addr_ctx_allow_zero_at(&self, current: usize) -> AddrCtx<'_> {
        AddrCtx {
            allow_zero: true,
            ..self.addr_ctx_at(current)
        }
    }

    /// Resolve address range to line numbers.
    pub(super) fn resolve_range(&self, range: &AddressRange) -> Result<(usize, usize)> {
        range.resolve(&self.addr_ctx())
    }

    /// Resolve a range down to the single line an insert-class command targets.
    ///
    /// Returns `None` when no address was given, so the caller can apply its own
    /// default. `allow_zero` permits line 0, meaning "before the first line".
    ///
    /// These commands used to receive a `usize` that the *parser* had extracted,
    /// and the parser could only read a literal `Address::Line(n)` -- every other
    /// address form fell back to line 1 (#X25).
    pub(super) fn resolve_target_line(
        &self,
        range: &AddressRange,
        allow_zero: bool,
    ) -> Result<Option<usize>> {
        if !range.explicit {
            return Ok(None);
        }
        let ctx = if allow_zero {
            self.addr_ctx_allow_zero()
        } else {
            self.addr_ctx()
        };
        let (_, end) = range.resolve(&ctx)?;
        Ok(Some(end))
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
        let end = end_from_count(start, end, count, self.buffer.line_count())?;

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
        let end = end_from_count(start, end, count, self.buffer.line_count())?;

        let start_pos = Position::new(start, 0);
        let end_pos = Position::new(end, 0);
        let yank_range = Range::lines(start_pos, end_pos);

        let _ = yank(&self.buffer, yank_range, &mut self.registers, register);

        let line_count = end - start + 1;
        self.set_message(&format!("{} lines yanked", line_count));

        Ok(())
    }
}

/// Resolve the last line an ex command acts on when a trailing `count` was
/// given: the range becomes `count` lines starting at `start`.
///
/// A count must be positive. Zero used to reach `start + count - 1` unguarded,
/// which underflows for `start == 0` and otherwise silently addresses the line
/// *before* `start`, so `:1d 0` walked off the front of the buffer.
fn end_from_count(
    start: usize,
    end: usize,
    count: Option<usize>,
    last_line: usize,
) -> Result<usize> {
    let Some(c) = count else { return Ok(end) };
    if c == 0 {
        return Err(ViError::InvalidCommand(
            "count must be positive".to_string(),
        ));
    }
    Ok((start + c - 1).min(last_line))
}

/// Decrement `line` by `delta`, or fail as POSIX requires when the result
/// would be less than 1 (95564, 95590).
fn decrement_or_err(line: usize, delta: usize) -> Result<usize> {
    line.checked_sub(delta)
        .filter(|n| *n >= 1)
        .ok_or_else(|| ViError::InvalidAddress("z: before start of buffer".to_string()))
}
