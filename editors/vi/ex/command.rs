//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Ex command definitions.

use super::address::AddressRange;

/// Parsed ex command.
#[derive(Debug)]
pub enum ExCommand {
    /// Write buffer to file (:w, :write).
    Write {
        range: AddressRange,
        file: Option<String>,
        append: bool,
        force: bool,
    },
    /// Quit editor (:q, :quit).
    Quit { force: bool },
    /// Write and quit (:wq, :x).
    WriteQuit {
        range: AddressRange,
        file: Option<String>,
        force: bool,
        /// True for `:x`/`:xit`, false for `:wq`. POSIX (ex.md §95537) makes
        /// `xit` on an unmodified buffer equivalent to `quit`, whereas `wq`
        /// always writes (#X29).
        xit: bool,
    },
    /// Edit file (:e, :edit).
    Edit { file: Option<String>, force: bool },
    /// Read file into buffer (:r, :read).
    Read {
        range: AddressRange,
        file: Option<String>,
    },
    /// Delete lines (:d, :delete).
    Delete {
        range: AddressRange,
        register: Option<char>,
        count: Option<usize>,
    },
    /// Yank lines (:y, :yank).
    Yank {
        range: AddressRange,
        register: Option<char>,
        count: Option<usize>,
    },
    /// Put text from register (:pu, :put).
    Put {
        range: AddressRange,
        register: Option<char>,
    },
    /// Copy lines (:co, :copy, :t).
    Copy { range: AddressRange, dest: usize },
    /// Move lines (:m, :move).
    Move { range: AddressRange, dest: usize },
    /// Substitute (:s, :substitute).
    Substitute {
        range: AddressRange,
        pattern: String,
        replacement: String,
        flags: SubstituteFlags,
    },
    /// Global command (:g, :global).
    Global {
        range: AddressRange,
        pattern: String,
        command: String,
        invert: bool,
    },
    /// Print lines (:p, :print).
    Print {
        range: AddressRange,
        count: Option<usize>,
    },
    /// Print line numbers (:nu, :number).
    Number {
        range: AddressRange,
        count: Option<usize>,
    },
    /// List lines (:l, :list).
    List {
        range: AddressRange,
        count: Option<usize>,
    },
    /// Join lines (:j, :join).
    Join {
        range: AddressRange,
        count: Option<usize>,
        /// `j!` -- join without modifying any line (ex.md §95060-95061).
        force: bool,
    },
    /// Set options (:se, :set).
    Set { args: String },
    /// Show file info (Ctrl-G, :f, :file).
    File { new_name: Option<String> },
    /// Go to line (:number or just address).
    Goto { line: usize },
    /// Mark line (:ma, :mark, :k).
    Mark { range: AddressRange, name: char },
    /// Shell command (:!, :shell).
    Shell { command: String },
    /// Shell read (:<n>r!command).
    ShellRead {
        range: AddressRange,
        command: String,
    },
    /// Shell write (:<range>w!command).
    ShellWrite {
        range: AddressRange,
        command: String,
    },
    /// Shell filter (:<range>!command) - filter lines through command.
    ShellFilter {
        range: AddressRange,
        command: String,
    },
    /// Change directory (:cd, :chdir).
    Cd {
        path: Option<String>,
        /// `chd[ir]!` proceeds even with a modified buffer (94925-94926).
        force: bool,
    },
    /// Push working directory (:pwd).
    Pwd,
    /// Next file in arg list (:n, :next).
    Next { force: bool },
    /// Previous file in arg list (:N, :prev, :previous).
    Previous { force: bool },
    /// Rewind to first file (:rew, :rewind).
    Rewind { force: bool },
    /// Args list (:ar, :args).
    Args,
    /// Undo (:u, :undo).
    Undo,
    /// Redo (:red, :redo).
    Redo,
    /// Map key sequence (:map).
    Map {
        lhs: String,
        rhs: String,
        mode: MapMode,
    },
    /// Unmap key sequence (:unmap).
    Unmap { lhs: String, mode: MapMode },
    /// Abbreviation (:ab, :abbreviate).
    Abbreviate { lhs: String, rhs: String },
    /// Remove abbreviation (:una, :unabbreviate).
    Unabbreviate { lhs: String },
    /// Open tag (:ta, :tag).
    Tag {
        tag: String,
        /// `ta[g]!` discards changes rather than refusing (95408).
        force: bool,
    },
    /// Pop tag stack (:po, :pop).
    Pop,
    /// Display tags (:tags).
    Tags,
    /// Version (:ve, :version).
    Version,
    /// Help (custom, not in POSIX).
    Help,
    /// Preserve the edit buffer for later recovery (:pre, :preserve).
    Preserve,
    /// Recover a buffer saved by a previous session (:rec, :recover).
    Recover {
        file: Option<String>,
        /// `rec[over]!` proceeds even with a modified buffer (95293-95294).
        force: bool,
    },
    /// Source file (execute ex commands from file) (:so, :source).
    Source { file: String },
    /// Append text after line (:a, :append).
    ///
    /// Carries the whole `AddressRange` rather than a pre-extracted `usize`:
    /// the parser could only pull a literal `Address::Line(n)` out and fell
    /// back to line 1 for everything else, so `$a`, `.a`, `/re/a` and `'ma`
    /// all silently targeted the wrong line (#X25).
    Append {
        range: AddressRange,
        /// `a!` toggles the autoindent edit option for this command only
        /// (94894-94896).
        toggle_autoindent: bool,
    },
    /// Insert text before line (:i, :insert).
    Insert {
        range: AddressRange,
        /// `i!` toggles autoindent for this command only (95034-95036).
        toggle_autoindent: bool,
    },
    /// Change lines (:c, :change).
    Change {
        range: AddressRange,
        /// `c[hange][!][count]`: count extends the range as an extra address.
        count: Option<usize>,
        /// `c!` toggles autoindent for this command only (94910-94912).
        toggle_autoindent: bool,
    },
    /// Enter visual mode (:vi, :visual).
    Visual,
    /// Enter open mode (:o, :open).
    Open {
        range: AddressRange,
        /// `o[pen] /pattern/`: `None` means "the last RE used in the editor"
        /// (95212-95214).
        pattern: Option<String>,
    },
    /// Adjust window (:z).
    Z {
        range: AddressRange,
        ztype: Option<char>,
        /// How many times the type character was repeated; `z--` and `z++`
        /// scroll further than `z-` and `z+` (95562-95592).
        type_count: usize,
        count: Option<usize>,
        /// `z!` defaults count to the number of lines in the display minus one
        /// rather than twice the scroll option (95556-95558).
        full_screen: bool,
    },
    /// Shift left (:<).
    ShiftLeft {
        range: AddressRange,
        count: Option<usize>,
    },
    /// Shift right (:>).
    ShiftRight {
        range: AddressRange,
        count: Option<usize>,
    },
    /// Write line number (:=).
    LineNumber { range: AddressRange },
    /// Execute buffer (:@, :*).
    Execute {
        range: AddressRange,
        buffer: Option<char>,
    },
    /// Suspend editor (:suspend, :stop, :sus).
    Suspend {
        /// `su[spend]!` suspends without an automatic write (95405).
        force: bool,
    },
    /// Repeat substitute (:&).
    RepeatSubstitute {
        range: AddressRange,
        flags: SubstituteFlags,
    },
    /// Repeat the previous substitute's replacement against the last RE (`:~`).
    ///
    /// Distinct from `&`: that reuses the previous *pattern* and replacement,
    /// while `~` takes the pattern from the most recent RE, which may have come
    /// from a search (#X18).
    TildeSubstitute {
        range: AddressRange,
        flags: SubstituteFlags,
    },
    /// No operation (empty command).
    Nop,
}

/// Flags for substitute command.
#[derive(Debug, Default, Clone)]
pub struct SubstituteFlags {
    /// Global (all occurrences on line).
    pub global: bool,
    /// Confirm each substitution.
    pub confirm: bool,
    /// Print lines after substitution.
    pub print: bool,
    /// Count matches (don't substitute).
    pub count: bool,
    /// Case insensitive.
    pub ignore_case: bool,
    /// `l` -- print changed lines in unambiguous (list) form.
    pub list: bool,
    /// `#` -- print changed lines with line numbers.
    pub number: bool,
    /// Trailing numeric count: operate on that many lines starting at the last
    /// line of the address range (ex.md substitute synopsis).
    pub line_count: Option<usize>,
}

impl SubstituteFlags {
    /// Parse flags from a string.
    pub fn parse(s: &str) -> Self {
        let mut flags = Self::default();
        for c in s.chars() {
            match c {
                'g' => flags.global = true,
                'c' => flags.confirm = true,
                'p' => flags.print = true,
                'n' => flags.count = true,
                'i' | 'I' => flags.ignore_case = true,
                'l' => flags.list = true,
                '#' => flags.number = true,
                _ => {}
            }
        }
        flags
    }
}

/// Mode for key mappings.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MapMode {
    /// Command mode.
    Command,
    /// Insert mode.
    Insert,
}

impl MapMode {
    /// `map!` / `unmap!` address the text input mode map list (95090-95092);
    /// without the bang they address the command mode list.
    pub fn for_bang(bang: bool) -> Self {
        if bang {
            MapMode::Insert
        } else {
            MapMode::Command
        }
    }
}

/// Result of executing an ex command.
#[derive(Debug)]
pub enum ExResult {
    /// Continue editing.
    Continue,
    /// Switch to a file.
    Edit(String),
    /// Quit editor (optionally with exit code).
    Quit(i32),
    /// Status message to display (suppressed in silent/batch mode).
    StatusMessage(String),
    /// Error message.
    Error(String),
    /// Command output to display (always printed in ex mode, e.g. :p, :nu, :l).
    CommandOutput(Vec<String>),
    /// Enter insert mode at position.
    Insert(usize, usize),
    /// Command needs more input (for :g).
    Pending(String),
    /// Enter visual mode.
    EnterVisual,
    /// Enter open mode at optional line.
    EnterOpen(Option<usize>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_substitute_flags() {
        let flags = SubstituteFlags::parse("gc");
        assert!(flags.global);
        assert!(flags.confirm);
        assert!(!flags.print);
    }
}
