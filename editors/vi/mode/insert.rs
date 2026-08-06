//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Insert mode handling.
//!
//! This module manages insert mode operations including:
//! - Basic character insertion
//! - Special character handling (backspace, newline, tab)
//! - Cursor positioning based on insert kind

use super::mode::InsertKind;
use crate::buffer::{
    char_index_at_byte, display_col, leading_blank_width, render_indent, Buffer, Position,
};
use crate::error::{Result, ViError};
use crate::input::Key;
use crate::options::Options;

/// Insert mode state.
#[derive(Debug)]
pub struct InsertState {
    /// The kind of insert mode.
    pub kind: InsertKind,
    /// Starting position (for change operations).
    pub start_pos: Position,
    /// Column to return to on escape (for append operations).
    pub return_column: Option<usize>,
    /// Text inserted in this session (for repeat with '.').
    pub inserted_text: String,
    /// Count for repeating insert.
    pub count: usize,
    /// The terminal's `stty erase` character, when it differs from `^H` (#V12).
    pub erase_char: Option<char>,
    /// The terminal's `stty kill` character, when it differs from `^U` (#V12).
    pub kill_char: Option<char>,
    /// Text inserted by the *previous* insert session, which a NUL re-inserts
    /// (#V15).
    pub previous_insert: String,
    /// A `^V`/`^Q` was entered and the next key is taken literally (#V23).
    ///
    /// Lives here rather than on `Editor` so it dies with the insert session:
    /// a pending literal must not survive an ESC.
    pub pending_literal: bool,
    /// Byte length of the autoindent prefix on the current input line (#V25).
    ///
    /// These are the only characters `^D` may erase (121698-121702), and
    /// distinguishing them from user-typed blanks is what makes `^D`'s
    /// precondition (121772-121776) checkable at all.
    pub ai_len: usize,
    /// The indent that the current input line's autoindent level was derived
    /// from.
    ///
    /// `^^D` discards the current line's indent but requires "the autoindent
    /// level for the next input line [to] be derived from the same line from
    /// which the autoindent level for the current input line was derived"
    /// (121779-121780) — so this survives the erase while `ai_len` does not.
    pub ai_source_indent: String,
    /// Whether the current line was newly created during this insert session.
    ///
    /// On ESC, autoindent characters on such a line are deleted if nothing
    /// else was typed there (121912-121913). Pre-existing lines are untouched.
    pub ai_line_is_new: bool,
    /// The last text-input character, for `^D`'s `'0'`/`'^'` prefix test
    /// (121777-121778). Tracked explicitly rather than sniffed out of
    /// `inserted_text`, which would misread a `0` typed on a previous line.
    pub last_input_char: Option<char>,
}

impl InsertState {
    /// Create a new insert state.
    pub fn new(kind: InsertKind, start_pos: Position, count: usize) -> Self {
        Self {
            kind,
            start_pos,
            return_column: None,
            inserted_text: String::new(),
            count,
            erase_char: None,
            kill_char: None,
            previous_insert: String::new(),
            pending_literal: false,
            ai_len: 0,
            ai_source_indent: String::new(),
            ai_line_is_new: false,
            last_input_char: None,
        }
    }

    /// Set return column for append operations.
    pub fn with_return_column(mut self, col: usize) -> Self {
        self.return_column = Some(col);
        self
    }
}

/// Enter insert mode and position cursor appropriately.
pub fn enter_insert_mode(
    buffer: &mut Buffer,
    kind: InsertKind,
    opts: &Options,
) -> Result<InsertState> {
    let pos = buffer.cursor();
    let mut state = InsertState::new(kind, pos, 1);

    // `o`/`O` create a line, so with autoindent set it is "preceded by the
    // same number of autoindent characters found on the line from which the
    // command was executed" (121501-121502).
    let source_indent = buffer
        .line(pos.line)
        .map(|l| {
            let c = l.content();
            c[..leading_blank_width(c, opts.tabstop).1].to_string()
        })
        .unwrap_or_default();

    match kind {
        InsertKind::Insert => {
            // 'i' - Insert before cursor, no position change needed
        }
        InsertKind::Append => {
            // 'a' - Append after cursor
            if let Some(line) = buffer.line(pos.line) {
                if !line.is_empty() {
                    let content = line.content();
                    let chars: Vec<(usize, char)> = content.char_indices().collect();
                    let idx = char_index_at_byte(content, pos.column);

                    // Move one position forward if not at end
                    if idx + 1 < chars.len() {
                        buffer.set_column_for_insert(chars[idx + 1].0);
                    } else {
                        // At end of line, put cursor after last char
                        buffer.set_column_for_insert(content.len());
                    }
                    state.return_column = Some(pos.column);
                }
            }
        }
        InsertKind::InsertBol => {
            // 'I' - Insert at first non-blank
            if let Some(line) = buffer.line(pos.line) {
                buffer.set_column(line.first_non_blank());
            }
        }
        InsertKind::AppendEol => {
            // 'A' - Append at end of line
            // Cursor goes AFTER the last character so new text is appended
            if let Some(line) = buffer.line(pos.line) {
                buffer.set_column_for_insert(line.len());
            }
        }
        InsertKind::OpenBelow => {
            // 'o' - Open line below
            let line_num = pos.line;
            buffer.insert_line_below(line_num)?;
            buffer.set_line(line_num + 1);
            buffer.set_column(0);
            apply_autoindent(buffer, &mut state, opts, &source_indent);
        }
        InsertKind::OpenAbove => {
            // 'O' - Open line above
            let line_num = pos.line;
            buffer.insert_line_above(line_num)?;
            // Cursor is now at the new line
            buffer.set_column(0);
            apply_autoindent(buffer, &mut state, opts, &source_indent);
        }
        InsertKind::Change | InsertKind::Substitute => {
            // 'c' / 's' - Position is set by caller after delete
        }
    }

    // Re-anchor to where text will actually be typed (#V27).
    //
    // `start_pos` was captured before the match, i.e. before the cursor was
    // repositioned, so it was only correct for `i` (and `c`/`s`, which do not
    // move here). For `a`, `I`, `A`, `o` and `O` it named the pre-command
    // cursor, and both consumers -- the undo record in `handle_insert_key` and
    // the `^U` floor in `delete_to_line_start` -- then worked from the wrong
    // column. `A` was the worst case: `Axy<ESC>u` on "base" removed two
    // characters from column 0 and left "sexy".
    state.start_pos = buffer.cursor();

    Ok(state)
}

/// Process a key in insert mode.
/// Returns Ok(true) if should exit insert mode.
pub fn process_insert_key(
    buffer: &mut Buffer,
    key: Key,
    state: &mut InsertState,
    opts: &Options,
) -> Result<bool> {
    // A pending ^V/^Q consumes this key literally, before any special meaning
    // is consulted (POSIX 121870-121872).
    if state.pending_literal {
        state.pending_literal = false;
        // "If a <control>-V or <control>-Q is entered before a <control>-J or
        // <newline>, the <control>-V or <control>-Q character shall be
        // discarded, and the <control>-J or <newline> shall behave as
        // described in the <newline> command character" (121875-121877).
        if matches!(key, Key::Enter | Key::Ctrl('j')) {
            insert_newline(buffer, state, opts);
            return Ok(false);
        }
        // Route through insert_char so `inserted_text` records the literal and
        // both '.' repeat and the insert-session undo entry pick it up.
        if let Some(c) = key.literal_char() {
            insert_char(buffer, c, state);
        }
        return Ok(false);
    }

    match key {
        Key::Escape => {
            // Exit insert mode
            finalize_insert(buffer, state);
            return Ok(true);
        }
        // A NUL re-inserts the text from the previous insert session (#V15).
        // The reader maps byte 0 to Ctrl('@'), not Char('\0').
        Key::Ctrl('@') => {
            let previous = state.previous_insert.clone();
            for c in previous.chars() {
                if c == '\n' {
                    insert_newline(buffer, state, opts);
                } else {
                    insert_char(buffer, c, state);
                }
            }
        }
        // Honor the terminal's configured erase/kill characters as well as the
        // hardcoded ^H/^U, which only ever covered the common case (#V12).
        Key::Char(c) if Some(c) == state.erase_char => {
            delete_char_before(buffer, state)?;
        }
        Key::Char(c) if Some(c) == state.kill_char => {
            delete_to_line_start(buffer, state);
        }
        Key::Char(c) => {
            insert_char(buffer, c, state);
        }
        Key::Ctrl('h') | Key::Backspace => {
            // Delete character before cursor
            delete_char_before(buffer, state)?;
        }
        Key::Ctrl('w') => {
            // Delete word before cursor
            delete_word_before(buffer, state);
        }
        Key::Ctrl('u') => {
            // Delete to start of line
            delete_to_line_start(buffer, state);
        }
        Key::Ctrl('j') | Key::Enter => {
            // Insert newline
            insert_newline(buffer, state, opts);
        }
        Key::Tab => {
            // Insert tab
            insert_char(buffer, '\t', state);
        }
        Key::Ctrl('t') => {
            // Shift line right (indent)
            indent_line(buffer, state, opts);
        }
        Key::Ctrl('d') => {
            // Shift line left (dedent)
            erase_autoindent(buffer, state, opts);
        }
        // ^V, and its synonym ^Q (121874), take the next character literally.
        // Nothing enters the buffer here, so the current line and column are
        // unchanged (121882-121883).
        Key::Ctrl('v') | Key::Ctrl('q') => {
            state.pending_literal = true;
        }
        Key::Ctrl('[') => {
            // Also escape
            finalize_insert(buffer, state);
            return Ok(true);
        }
        _ => {
            // Ignore other keys
        }
    }

    Ok(false)
}

/// Write the autoindent prefix onto the (empty) current line and park the
/// cursor after it, recording how much of the line is autoindent (#V25).
///
/// The prefix is re-rendered as tabs-then-spaces per the ex `autoindent`
/// option (95739-95741). Does nothing when autoindent is unset.
fn apply_autoindent(
    buffer: &mut Buffer,
    state: &mut InsertState,
    opts: &Options,
    source_indent: &str,
) {
    state.ai_line_is_new = true;
    state.ai_source_indent = source_indent.to_string();
    state.ai_len = 0;

    if !opts.autoindent || source_indent.is_empty() {
        return;
    }

    let width = leading_blank_width(source_indent, opts.tabstop).0;
    let indent = render_indent(width, opts.tabstop);
    for c in indent.chars() {
        buffer.insert_char(c);
    }
    state.ai_len = indent.len();
}

/// The autoindent prefix currently on the cursor's line, as a string.
fn current_autoindent(buffer: &Buffer, state: &InsertState) -> String {
    let line = buffer.cursor().line;
    buffer
        .line(line)
        .map(|l| {
            let c = l.content();
            c[..state.ai_len.min(c.len())].to_string()
        })
        .unwrap_or_default()
}

/// Insert a character at cursor.
fn insert_char(buffer: &mut Buffer, c: char, state: &mut InsertState) {
    buffer.insert_char(c);
    state.inserted_text.push(c);
    state.last_input_char = Some(c);
}

/// Delete character before cursor.
fn delete_char_before(buffer: &mut Buffer, state: &mut InsertState) -> Result<()> {
    let pos = buffer.cursor();

    if pos.column == 0 {
        // At start of line - join with previous line if not first line
        if pos.line > 1 {
            let prev_line = buffer
                .line(pos.line - 1)
                .ok_or(ViError::InvalidLine(pos.line - 1))?;
            let join_col = prev_line.len();
            // join_lines joins line_num with the line after it
            // We want to join current line with previous, so we join pos.line - 1 with pos.line
            // But the function removes line at line_num (0-indexed becomes line_num after 1-index conversion)
            // Actually re-read: join_lines(line_num) joins line_num with next
            // So we need to call with pos.line - 1 to join previous with current
            buffer.join_lines(pos.line - 1, false)?;
            buffer.set_line(pos.line - 1);
            buffer.set_column(join_col);

            // Record in inserted text
            if !state.inserted_text.is_empty() {
                state.inserted_text.pop();
            }
        }
    } else {
        buffer.delete_char_before();

        // Update inserted text
        if !state.inserted_text.is_empty() {
            state.inserted_text.pop();
        }
    }

    Ok(())
}

/// Delete word before cursor.
fn delete_word_before(buffer: &mut Buffer, state: &mut InsertState) {
    let pos = buffer.cursor();

    if pos.column == 0 {
        return;
    }

    let line = match buffer.line(pos.line) {
        Some(l) => l,
        None => return,
    };
    let content = line.content().to_string();

    // Find start of word before cursor
    let chars: Vec<(usize, char)> = content[..pos.column].char_indices().collect();
    if chars.is_empty() {
        return;
    }

    let mut idx = chars.len() - 1;

    // Skip trailing blanks
    while idx > 0 && (chars[idx].1 == ' ' || chars[idx].1 == '\t') {
        idx -= 1;
    }

    // Skip word characters
    let is_word = chars[idx].1.is_alphanumeric() || chars[idx].1 == '_';
    while idx > 0 {
        let prev = chars[idx - 1].1;
        if is_word && !(prev.is_alphanumeric() || prev == '_') {
            break;
        }
        if !is_word && (prev.is_alphanumeric() || prev == '_' || prev == ' ' || prev == '\t') {
            break;
        }
        idx -= 1;
    }

    let delete_from = chars[idx].0;
    let delete_count = pos.column - delete_from;

    // Delete the characters
    buffer.set_column(pos.column);
    for _ in 0..delete_count {
        buffer.delete_char_before();
    }

    // Update inserted text
    for _ in 0..delete_count {
        if !state.inserted_text.is_empty() {
            state.inserted_text.pop();
        }
    }
}

/// Delete to start of inserted text or line.
fn delete_to_line_start(buffer: &mut Buffer, state: &mut InsertState) {
    let pos = buffer.cursor();

    if pos.column == 0 {
        return;
    }

    // Delete back to start of inserted text or start of line
    let delete_to = if pos.column > state.start_pos.column && pos.line == state.start_pos.line {
        state.start_pos.column
    } else {
        0
    };

    let delete_count = pos.column - delete_to;

    for _ in 0..delete_count {
        buffer.delete_char_before();
    }

    // Clear inserted text
    state.inserted_text.clear();
}

/// Insert a newline, carrying the autoindent onto the new line.
fn insert_newline(buffer: &mut Buffer, state: &mut InsertState, opts: &Options) {
    // "If there are no characters other than autoindent characters on the
    // line, all characters on the line shall be discarded" (121819-121820).
    let line_num = buffer.cursor().line;
    let only_autoindent = state.ai_len > 0
        && buffer
            .line(line_num)
            .is_some_and(|l| l.content().len() == state.ai_len);
    if only_autoindent {
        let _ = buffer.replace_line(line_num, "");
        buffer.set_column(0);
    }

    // The indent the *next* line derives from. Normally the line just
    // terminated; after `^^D` the source is preserved instead (121779-121780).
    let source = if state.ai_source_indent.is_empty() || state.ai_len > 0 {
        let cur = current_autoindent(buffer, state);
        if cur.is_empty() {
            state.ai_source_indent.clone()
        } else {
            cur
        }
    } else {
        state.ai_source_indent.clone()
    };

    buffer.insert_newline();
    state.inserted_text.push('\n');
    state.last_input_char = None;

    // 121826-121827: the new line gets an autoindent prefix.
    apply_autoindent(buffer, state, opts, &source);
}

/// Insert blanks at the cursor up to the next shiftwidth boundary (Ctrl-T).
///
/// POSIX 121843-121847: "Behave as if the user entered the minimum number of
/// <blank> characters necessary to move the cursor forward to the column
/// position after the next shiftwidth boundary", with the resulting column
/// being `column + shiftwidth - ((column - 1) % shiftwidth)`.
///
/// The previous implementation inserted exactly one tab at *column 0* and
/// never touched `state.inserted_text`, so it indented the wrong place by the
/// wrong amount and `.`-repeat and undo both lost it (#V24).
fn indent_line(buffer: &mut Buffer, state: &mut InsertState, opts: &Options) {
    let sw = opts.shiftwidth.max(1);
    let ts = opts.tabstop.max(1);

    let pos = buffer.cursor();
    let content = buffer
        .line(pos.line)
        .map(|l| l.content().to_string())
        .unwrap_or_default();

    // Display column of the cursor, expanding tabs against the tabstop.
    let mut col = 0usize;
    for (byte, c) in content.char_indices() {
        if byte >= pos.column {
            break;
        }
        if c == '\t' {
            col += ts - (col % ts);
        } else {
            col += 1;
        }
    }

    // Blanks needed to reach the next shiftwidth boundary. Already being on a
    // boundary advances a full shiftwidth rather than inserting nothing.
    let needed = sw - (col % sw);
    for _ in 0..needed {
        insert_char(buffer, ' ', state);
    }
}

/// Erase autoindent characters (Ctrl-D), per POSIX 121767-121788.
///
/// `^D` is defined **entirely** in terms of autoindent characters, which is why
/// it depends on #V25. The previous implementation was the wrong model on all
/// three counts the spec specifies:
///
///   1. *Precondition.* `^D` applies only when the cursor follows autoindent
///      characters, optionally plus one typed `'0'` or `'^'`. Otherwise it
///      "shall have no special meaning" (121776) — i.e. the character is
///      appended like any other input character — or is discarded in column 1
///      (121774-121775). The old code had no precondition at all.
///   2. *Target.* `^D` erases backwards from the **cursor**. The old code did
///      an unconditional `set_column(0)` and deleted there, which is a `<<`.
///   3. *Units.* Shiftwidth boundaries in display columns, not a hardcoded
///      "one tab or up to 8 spaces".
///
/// Not implemented: "all of the glyphs on columns between the starting cursor
/// position and (inclusively) the ending cursor position shall become
/// erase-columns" (121783-121785). Erase-columns are a screen-model feature
/// this renderer does not have — it redraws from the buffer on every keystroke,
/// so erased characters simply disappear rather than persisting as overwritable
/// glyphs. That is a display deviation only; the buffer contents match.
fn erase_autoindent(buffer: &mut Buffer, state: &mut InsertState, opts: &Options) {
    let sw = opts.shiftwidth.max(1);
    let pos = buffer.cursor();
    let content = buffer
        .line(pos.line)
        .map(|l| l.content().to_string())
        .unwrap_or_default();

    // Does the cursor follow the autoindent run, or the run plus one typed
    // '0' or '^'?
    let marker = matches!(state.last_input_char, Some('0') | Some('^'));
    let expected = state.ai_len + if marker { 1 } else { 0 };
    if state.ai_len == 0 || pos.column != expected {
        // 121774-121776: no special meaning here.
        if display_col(&content, pos.column, opts.tabstop) == 0 {
            return; // column 1 -> discard silently
        }
        insert_char(buffer, '\u{4}', state);
        return;
    }

    // Erase the '0'/'^' itself; it was input, not autoindent. Re-read the line
    // afterwards — using the pre-delete `content` here left the marker in the
    // tail and it reappeared after the erased indent.
    if marker {
        buffer.delete_char_before();
        state.inserted_text.pop();
    }
    let content = buffer
        .line(pos.line)
        .map(|l| l.content().to_string())
        .unwrap_or_default();

    let ai = &content[..state.ai_len.min(content.len())];
    let ai_width = leading_blank_width(ai, opts.tabstop).0;

    // '0' and '^' both go to column 1, discarding the whole indent
    // (121777-121778); anything else backs up one shiftwidth boundary
    // (121781-121782).
    let target_width = if marker {
        0
    } else {
        ai_width.saturating_sub(1) / sw * sw
    };

    let new_indent = render_indent(target_width.min(ai_width), opts.tabstop);
    let rest = &content[state.ai_len.min(content.len())..];
    let new_content = format!("{}{}", new_indent, rest);
    let _ = buffer.replace_line(pos.line, &new_content);
    buffer.set_column_for_insert(new_indent.len());
    state.ai_len = new_indent.len();

    // `^^D` keeps the source indent so the *next* line derives from the same
    // line this one did (121779-121780). `0^D` does not.
    if state.last_input_char == Some('0') {
        state.ai_source_indent.clear();
    }
    state.last_input_char = None;
}

/// Finalize insert mode (handle repeat, position cursor).
fn finalize_insert(buffer: &mut Buffer, state: &mut InsertState) {
    // "Any autoindent characters entered on newly created lines that have no
    // other non-<newline> characters shall be deleted" (121912-121913). Only
    // lines this session created qualify — a pre-existing indented line the
    // user merely visited must keep its whitespace.
    if state.ai_line_is_new && state.ai_len > 0 {
        let line_num = buffer.cursor().line;
        let is_blank = buffer
            .line(line_num)
            .is_some_and(|l| l.content().len() == state.ai_len);
        if is_blank {
            let _ = buffer.replace_line(line_num, "");
            buffer.set_column(0);
            state.ai_len = 0;
        }
    }

    // Handle count > 1 (repeat inserted text)
    if state.count > 1 && !state.inserted_text.is_empty() {
        for _ in 1..state.count {
            for c in state.inserted_text.chars() {
                if c == '\n' {
                    buffer.insert_newline();
                } else {
                    buffer.insert_char(c);
                }
            }
        }
    }

    // Move cursor back one position (vi behavior)
    // In vi, when leaving insert mode, cursor moves back one position
    // and must end up ON a character (not past end of line)
    let pos = buffer.cursor();
    if pos.column > 0 {
        if let Some(line) = buffer.line(pos.line) {
            if line.is_empty() {
                buffer.set_column(0);
            } else {
                let content = line.content();
                let chars: Vec<(usize, char)> = content.char_indices().collect();

                // If cursor is at or past end of line, move to last character
                if pos.column >= content.len() {
                    buffer.set_column(chars.last().map(|(idx, _)| *idx).unwrap_or(0));
                } else {
                    // Find current char index and move back one
                    let idx = char_index_at_byte(content, pos.column);
                    if idx > 0 {
                        buffer.set_column(chars[idx - 1].0);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_char() {
        let mut buf = Buffer::from_text("hello");
        let mut state = InsertState::new(InsertKind::Insert, buf.cursor(), 1);

        insert_char(&mut buf, 'X', &mut state);
        assert_eq!(buf.to_string(), "Xhello\n");
        assert_eq!(state.inserted_text, "X");
    }

    #[test]
    fn test_append_position() {
        let mut buf = Buffer::from_text("hello");
        buf.set_column(2); // On 'l'

        let state = enter_insert_mode(&mut buf, InsertKind::Append, &Options::default()).unwrap();
        assert_eq!(buf.cursor().column, 3); // After 'l'
        assert_eq!(state.return_column, Some(2));
    }

    #[test]
    fn test_insert_bol() {
        let mut buf = Buffer::from_text("   hello");
        buf.set_column(5);

        enter_insert_mode(&mut buf, InsertKind::InsertBol, &Options::default()).unwrap();
        assert_eq!(buf.cursor().column, 3); // First non-blank
    }

    #[test]
    fn test_append_eol() {
        let mut buf = Buffer::from_text("hello");
        buf.set_column(0);

        enter_insert_mode(&mut buf, InsertKind::AppendEol, &Options::default()).unwrap();
        // 'A' should put cursor AFTER the last character (position 5 for "hello")
        // so that inserted text appears after existing text
        assert_eq!(buf.cursor().column, 5); // After last char
    }

    #[test]
    fn test_open_below() {
        let mut buf = Buffer::from_text("line1\nline2");

        enter_insert_mode(&mut buf, InsertKind::OpenBelow, &Options::default()).unwrap();
        assert_eq!(buf.cursor().line, 2);
        assert_eq!(buf.cursor().column, 0);
        assert_eq!(buf.line_count(), 3);
    }

    #[test]
    fn test_open_above() {
        let mut buf = Buffer::from_text("line1\nline2");
        buf.set_line(2);

        enter_insert_mode(&mut buf, InsertKind::OpenAbove, &Options::default()).unwrap();
        assert_eq!(buf.cursor().line, 2);
        assert_eq!(buf.cursor().column, 0);
        assert_eq!(buf.line_count(), 3);
    }

    #[test]
    fn test_delete_char_before() {
        let mut buf = Buffer::from_text("hello");
        buf.set_column(3);
        let mut state = InsertState::new(InsertKind::Insert, buf.cursor(), 1);
        state.inserted_text = "lo".to_string();

        delete_char_before(&mut buf, &mut state).unwrap();
        assert_eq!(buf.to_string(), "helo\n");
        assert_eq!(state.inserted_text, "l");
    }

    #[test]
    fn test_insert_newline() {
        let mut buf = Buffer::from_text("hello");
        buf.set_column(2);
        let mut state = InsertState::new(InsertKind::Insert, buf.cursor(), 1);

        insert_newline(&mut buf, &mut state, &Options::default());
        assert_eq!(buf.line_count(), 2);
        assert_eq!(state.inserted_text, "\n");
    }

    #[test]
    fn test_escape_moves_back() {
        let mut buf = Buffer::from_text("hello");
        buf.set_column(3);
        let mut state = InsertState::new(InsertKind::Insert, Position::new(1, 0), 1);
        state.inserted_text = "abc".to_string();

        finalize_insert(&mut buf, &mut state);
        assert_eq!(buf.cursor().column, 2); // Moved back one
    }

    #[test]
    fn test_escape_from_end_of_line() {
        // When exiting insert mode with cursor past end of line (after 'A' command),
        // cursor should move back to the last character
        let mut buf = Buffer::from_text("hello");
        buf.set_column_for_insert(5); // Position after 'o' (past end)
        let mut state = InsertState::new(InsertKind::AppendEol, Position::new(1, 0), 1);
        state.inserted_text = "".to_string();

        finalize_insert(&mut buf, &mut state);
        assert_eq!(buf.cursor().column, 4); // On 'o', the last character
    }

    #[test]
    fn test_repeat_insert() {
        let mut buf = Buffer::from_text("hello");
        buf.set_column(0);
        let mut state = InsertState::new(InsertKind::Insert, buf.cursor(), 3);
        state.inserted_text = "X".to_string();
        buf.insert_char('X'); // Simulate first insert

        finalize_insert(&mut buf, &mut state);
        assert_eq!(buf.to_string(), "XXXhello\n");
    }
}
