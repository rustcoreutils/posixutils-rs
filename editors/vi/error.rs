//! Error types for the vi editor.

use std::fmt;
use std::io;

/// Result type alias for vi operations.
pub type Result<T> = std::result::Result<T, ViError>;

/// All possible errors in the vi editor.
#[derive(Debug)]
pub enum ViError {
    /// I/O error (file operations, terminal).
    Io(io::Error),
    /// Invalid command or motion.
    InvalidCommand(String),
    /// Motion failed (e.g., cursor at buffer boundary).
    MotionFailed(String),
    /// No previous search pattern.
    NoPreviousSearch,
    /// Pattern not found.
    PatternNotFound(String),
    /// No previous substitution.
    NoPreviousSubstitution,
    /// Mark not set.
    MarkNotSet(char),
    /// Buffer is empty.
    EmptyBuffer,
    /// Invalid address.
    InvalidAddress(String),
    /// Invalid range.
    InvalidRange(String),
    /// File not found.
    FileNotFound(String),
    /// File modified since last write.
    FileModified,
    /// Read-only mode.
    ReadOnly,
    /// Invalid option.
    InvalidOption(String),
    /// Invalid regular expression.
    InvalidRegex(String),
    /// No alternate file.
    NoAlternateFile,
    /// No file name.
    NoFileName,
    /// Interrupted by signal.
    Interrupted,
    /// Nothing to undo.
    NothingToUndo,
    /// At first change (cannot undo further).
    AtFirstChange,
    /// Invalid buffer name.
    InvalidBuffer(char),
    /// Buffer is empty (for put operations).
    BufferEmpty(char),
    /// Line too long.
    LineTooLong,
    /// Count out of range.
    CountOutOfRange,
    /// Tag not found.
    TagNotFound(String),
    /// No tags file.
    NoTagsFile,
    /// Invalid line number.
    InvalidLine(usize),
    /// Invalid pattern.
    InvalidPattern(String),
    /// Shell command error.
    ShellError(String),
    /// No previous shell command.
    NoPreviousCommand,
}

impl fmt::Display for ViError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ViError::Io(e) => write!(f, "{}", e),
            ViError::InvalidCommand(s) => write!(f, "Invalid command: {}", s),
            ViError::MotionFailed(s) => write!(f, "{}", s),
            ViError::NoPreviousSearch => write!(f, "No previous search pattern"),
            ViError::PatternNotFound(p) => write!(f, "Pattern not found: {}", p),
            ViError::NoPreviousSubstitution => write!(f, "No previous substitution"),
            ViError::MarkNotSet(c) => write!(f, "Mark not set: '{}'", c),
            ViError::EmptyBuffer => write!(f, "Empty buffer"),
            ViError::InvalidAddress(s) => write!(f, "Invalid address: {}", s),
            ViError::InvalidRange(s) => write!(f, "Invalid range: {}", s),
            ViError::FileNotFound(s) => write!(f, "File not found: {}", s),
            ViError::FileModified => write!(f, "No write since last change"),
            ViError::ReadOnly => write!(f, "Read-only mode"),
            ViError::InvalidOption(s) => write!(f, "Invalid option: {}", s),
            ViError::InvalidRegex(s) => write!(f, "Invalid regular expression: {}", s),
            ViError::NoAlternateFile => write!(f, "No alternate file"),
            ViError::NoFileName => write!(f, "No file name"),
            ViError::Interrupted => write!(f, "Interrupted"),
            ViError::NothingToUndo => write!(f, "Nothing to undo"),
            ViError::AtFirstChange => write!(f, "Already at oldest change"),
            ViError::InvalidBuffer(c) => write!(f, "Invalid buffer: \"{}\"", c),
            ViError::BufferEmpty(c) => write!(f, "Buffer \"{}\" is empty", c),
            ViError::LineTooLong => write!(f, "Line too long"),
            ViError::CountOutOfRange => write!(f, "Count out of range"),
            ViError::TagNotFound(s) => write!(f, "Tag not found: {}", s),
            ViError::NoTagsFile => write!(f, "No tags file"),
            ViError::InvalidLine(n) => write!(f, "Invalid line: {}", n),
            ViError::InvalidPattern(s) => write!(f, "Invalid pattern: {}", s),
            ViError::ShellError(s) => write!(f, "Shell error: {}", s),
            ViError::NoPreviousCommand => write!(f, "No previous command"),
        }
    }
}

impl std::error::Error for ViError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ViError::Io(e) => Some(e),
            _ => None,
        }
    }
}

impl From<io::Error> for ViError {
    fn from(e: io::Error) -> Self {
        ViError::Io(e)
    }
}

impl From<regex::Error> for ViError {
    fn from(e: regex::Error) -> Self {
        ViError::InvalidRegex(e.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = ViError::MarkNotSet('a');
        assert_eq!(format!("{}", err), "Mark not set: 'a'");
    }

    #[test]
    fn test_io_error_conversion() {
        let io_err = io::Error::new(io::ErrorKind::NotFound, "file not found");
        let vi_err: ViError = io_err.into();
        assert!(matches!(vi_err, ViError::Io(_)));
    }
}
