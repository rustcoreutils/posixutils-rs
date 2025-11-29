//! Editor mode definitions.

/// Editor modes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Mode {
    /// Normal/command mode.
    #[default]
    Command,
    /// Text input mode (insert, append, etc.).
    Insert(InsertKind),
    /// Ex command line mode.
    Ex,
    /// Replace mode (R command).
    Replace,
    /// Open mode (single line visual).
    Open,
}

impl Mode {
    /// Check if in command mode.
    pub fn is_command(&self) -> bool {
        matches!(self, Mode::Command)
    }

    /// Check if in any insert/input mode.
    pub fn is_insert(&self) -> bool {
        matches!(self, Mode::Insert(_) | Mode::Replace)
    }

    /// Check if in ex mode.
    pub fn is_ex(&self) -> bool {
        matches!(self, Mode::Ex)
    }
}

/// Kind of insert mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InsertKind {
    /// Insert before cursor (i).
    Insert,
    /// Append after cursor (a).
    Append,
    /// Insert at beginning of line (I).
    InsertBol,
    /// Append at end of line (A).
    AppendEol,
    /// Open line below (o).
    OpenBelow,
    /// Open line above (O).
    OpenAbove,
    /// Change (c).
    Change,
    /// Substitute (s).
    Substitute,
}

impl std::fmt::Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mode::Command => write!(f, "COMMAND"),
            Mode::Insert(_) => write!(f, "INSERT"),
            Mode::Ex => write!(f, "EX"),
            Mode::Replace => write!(f, "REPLACE"),
            Mode::Open => write!(f, "OPEN"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mode_checks() {
        assert!(Mode::Command.is_command());
        assert!(!Mode::Command.is_insert());

        assert!(Mode::Insert(InsertKind::Insert).is_insert());
        assert!(!Mode::Insert(InsertKind::Insert).is_command());

        assert!(Mode::Replace.is_insert());
    }

    #[test]
    fn test_mode_display() {
        assert_eq!(format!("{}", Mode::Command), "COMMAND");
        assert_eq!(format!("{}", Mode::Insert(InsertKind::Insert)), "INSERT");
    }
}
