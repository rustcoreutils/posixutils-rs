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
        line: Option<usize>,
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
    },
    /// Set options (:se, :set).
    Set { args: String },
    /// Show file info (Ctrl-G, :f, :file).
    File { new_name: Option<String> },
    /// Go to line (:number or just address).
    Goto { line: usize },
    /// Mark line (:ma, :mark, :k).
    Mark { line: Option<usize>, name: char },
    /// Shell command (:!, :shell).
    Shell { command: String },
    /// Shell read (:<n>r!command).
    ShellRead {
        line: Option<usize>,
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
    Cd { path: Option<String> },
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
    Tag { tag: String },
    /// Pop tag stack (:po, :pop).
    Pop,
    /// Display tags (:tags).
    Tags,
    /// Version (:ve, :version).
    Version,
    /// Help (custom, not in POSIX).
    Help,
    /// Source file (execute ex commands from file) (:so, :source).
    Source { file: String },
    /// Append text after line (:a, :append).
    Append { line: usize },
    /// Insert text before line (:i, :insert).
    Insert { line: usize },
    /// Change lines (:c, :change).
    Change { range: AddressRange },
    /// Enter visual mode (:vi, :visual).
    Visual,
    /// Enter open mode (:o, :open).
    Open { line: Option<usize> },
    /// Adjust window (:z).
    Z {
        line: Option<usize>,
        ztype: Option<char>,
        count: Option<usize>,
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
    LineNumber { line: Option<usize> },
    /// Execute buffer (:@, :*).
    Execute {
        range: AddressRange,
        buffer: Option<char>,
    },
    /// Suspend editor (:suspend, :stop, :sus).
    Suspend,
    /// Repeat substitute (:&).
    RepeatSubstitute {
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

/// Result of executing an ex command.
#[derive(Debug)]
pub enum ExResult {
    /// Continue editing.
    Continue,
    /// Switch to a file.
    Edit(String),
    /// Quit editor (optionally with exit code).
    Quit(i32),
    /// Message to display.
    Message(String),
    /// Error message.
    Error(String),
    /// Output to display (like :p output).
    Output(Vec<String>),
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
