//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Ex command definitions.

use super::address::{Address, AddressRange};

/// `<control>-V`, which quotes the next character in a `map`, `unmap`,
/// `abbreviate` or `unabbreviate` argument (94657-94659).
///
/// One definition because two sides have to agree on it: `handle_ex_key` writes
/// the marker into the command line when the user types `^V`, and the parser
/// reads it to tell an escaped <blank> from a delimiting one before discarding
/// it (95086-95088).
pub const CTRL_V: char = '\x16';

/// Strip the line terminator from a command line read from a file or stdin,
/// keeping a `<control>-V`-escaped one.
///
/// `map Q :wq^V^M` ends in a quoted carriage return that is part of the
/// replacement, immediately followed by the real terminator. Anything that
/// trims `\r` and `\n` off the end blindly — `str::lines`, `BufRead::lines`,
/// `trim_end_matches` — eats the quoted one too, which silently turns the
/// commonest mapping there is into one that does nothing.
///
/// Only the terminator is removed; other trailing <blank>s are left for
/// `parse_ex_command`, which decides what to do with them by command
/// (94655-94659).
pub fn strip_line_terminator(line: &str) -> &str {
    let mut end = 0;
    let mut chars = line.char_indices();
    while let Some((i, c)) = chars.next() {
        if c == CTRL_V {
            // The quoted character is kept whatever it is, and so is the
            // marker, which the argument parser needs.
            match chars.next() {
                Some((j, quoted)) => end = j + quoted.len_utf8(),
                None => end = i + c.len_utf8(),
            }
        } else if c != '\n' && c != '\r' {
            end = i + c.len_utf8();
        }
    }
    &line[..end]
}

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
    Edit {
        file: Option<String>,
        force: bool,
        /// `e[dit][!][+command][file]`: an ex command to run once the buffer
        /// has been replaced (94954-94957).
        command: Option<String>,
    },
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
    Copy { range: AddressRange, dest: Address },
    /// Move lines (:m, :move).
    Move { range: AddressRange, dest: Address },
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
    /// Go to an address given on its own, when it is not a literal line number
    /// (`:$`, `:.+2`, `:'a`, `:/re/`) and so needs the buffer to resolve.
    GotoAddress { range: AddressRange },
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
    Next {
        force: bool,
        /// `n[ext][!][+command][file ...]`: when non-empty, these replace the
        /// argument list (95181-95184).
        files: Vec<String>,
        /// An ex command to run once the buffer has been replaced (95194-95197).
        command: Option<String>,
    },
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
    /// Write the current map list (`:map` / `:map!` with no arguments).
    ///
    /// A separate variant rather than an empty `lhs`, because 95080-95083 makes
    /// the no-argument form a different command: it lists and "does nothing
    /// more". The two were indistinguishable while both parsed to `Map`.
    MapList { mode: MapMode },
    /// Unmap key sequence (:unmap).
    Unmap { lhs: String, mode: MapMode },
    /// Abbreviation (:ab, :abbreviate).
    Abbreviate { lhs: String, rhs: String },
    /// Write the current abbreviation list (`:ab` with no arguments, 94864).
    AbbrevList,
    /// Remove abbreviation (:una, :unabbreviate).
    Unabbreviate { lhs: String },
    /// Open tag (:ta, :tag).
    Tag {
        tag: String,
        /// `ta[g]!` discards changes rather than refusing (95408).
        force: bool,
    },
    /// Pop the tag stack (:po, :pop). Custom, not in POSIX: the spec gives
    /// `:tag` and `^]` but no way back.
    Pop,
    /// List the tag stack (:tags). Custom, not in POSIX -- the `tags` the spec
    /// defines (95941) is the `:set tags=` edit option naming the files `:tag`
    /// searches, not a command.
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
    /// Enter visual mode, or re-edit a file (:vi, :visual).
    Visual {
        range: AddressRange,
        force: bool,
        /// Raw arguments: `[type][count][flags]` in ex command mode, or
        /// `[+command][file]` in open or visual mode (95472-95474). `+` is both
        /// a window type character and the `+command` introducer, so only the
        /// executor, which knows the current mode, can tell them apart.
        args: String,
    },
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

/// Which map table a command addresses. Defined beside the tables themselves,
/// and re-exported here so the parser keeps naming it where the commands are.
pub use crate::maps::MapMode;

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

    /// The terminator goes; a `^V`-escaped carriage return stays, because it is
    /// part of the argument. `map Q :wq^V^M` is the case that matters, and
    /// every blind `\r`/`\n` trim -- `str::lines`, `BufRead::lines`,
    /// `trim_end_matches` -- gets it wrong.
    #[test]
    fn test_strip_line_terminator() {
        assert_eq!(strip_line_terminator("map q dd\n"), "map q dd");
        assert_eq!(strip_line_terminator("map q dd\r\n"), "map q dd");
        assert_eq!(strip_line_terminator("map q dd"), "map q dd");

        assert_eq!(
            strip_line_terminator("map Q :wq\x16\r\n"),
            "map Q :wq\x16\r",
            "a quoted CR survives, marker and all"
        );
        assert_eq!(strip_line_terminator("map Q :wq\x16\r"), "map Q :wq\x16\r");

        // Other trailing blanks are left alone: `parse_ex_command` strips those
        // by the rule the command follows.
        assert_eq!(strip_line_terminator("set number  \n"), "set number  ");

        // A `^V` quoting the terminator itself, with nothing after it.
        assert_eq!(strip_line_terminator("x\x16"), "x\x16");
        assert_eq!(strip_line_terminator(""), "");
    }
}
