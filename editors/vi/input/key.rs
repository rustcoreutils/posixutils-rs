//! Key event types for input handling.

/// A key event from the terminal.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Key {
    /// A regular character.
    Char(char),
    /// Control key combination (Ctrl+char).
    Ctrl(char),
    /// Escape key.
    Escape,
    /// Backspace key.
    Backspace,
    /// Delete key.
    Delete,
    /// Enter/Return key.
    Enter,
    /// Tab key.
    Tab,
    /// Arrow up.
    Up,
    /// Arrow down.
    Down,
    /// Arrow left.
    Left,
    /// Arrow right.
    Right,
    /// Home key.
    Home,
    /// End key.
    End,
    /// Page up.
    PageUp,
    /// Page down.
    PageDown,
    /// Insert key.
    Insert,
    /// Function keys F1-F12.
    F(u8),
    /// Unknown/unrecognized key sequence.
    Unknown,
}

impl Key {
    /// Create a key from a byte value.
    pub fn from_byte(b: u8) -> Self {
        match b {
            0 => Key::Ctrl('@'),
            13 => Key::Enter, // CR (carriage return) is Enter key
            1..=26 => Key::Ctrl((b'a' + b - 1) as char),
            27 => Key::Escape,
            28 => Key::Ctrl('\\'),
            29 => Key::Ctrl(']'),
            30 => Key::Ctrl('^'),
            31 => Key::Ctrl('_'),
            127 => Key::Backspace,
            b => Key::Char(b as char),
        }
    }

    /// Check if this is a printable character.
    pub fn is_printable(&self) -> bool {
        matches!(self, Key::Char(c) if !c.is_control())
    }

    /// Get the character if this is a Char key.
    pub fn as_char(&self) -> Option<char> {
        match self {
            Key::Char(c) => Some(*c),
            Key::Tab => Some('\t'),
            Key::Enter => Some('\n'),
            _ => None,
        }
    }

    /// Check if this is an escape key.
    pub fn is_escape(&self) -> bool {
        matches!(self, Key::Escape)
    }

    /// Check if this is a control key.
    pub fn is_ctrl(&self) -> bool {
        matches!(self, Key::Ctrl(_))
    }

    /// Get the control character if this is Ctrl key.
    pub fn ctrl_char(&self) -> Option<char> {
        match self {
            Key::Ctrl(c) => Some(*c),
            _ => None,
        }
    }
}

impl std::fmt::Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Key::Char(c) => write!(f, "{}", c),
            Key::Ctrl(c) => write!(f, "^{}", c.to_ascii_uppercase()),
            Key::Escape => write!(f, "<Esc>"),
            Key::Backspace => write!(f, "<BS>"),
            Key::Delete => write!(f, "<Del>"),
            Key::Enter => write!(f, "<Enter>"),
            Key::Tab => write!(f, "<Tab>"),
            Key::Up => write!(f, "<Up>"),
            Key::Down => write!(f, "<Down>"),
            Key::Left => write!(f, "<Left>"),
            Key::Right => write!(f, "<Right>"),
            Key::Home => write!(f, "<Home>"),
            Key::End => write!(f, "<End>"),
            Key::PageUp => write!(f, "<PageUp>"),
            Key::PageDown => write!(f, "<PageDown>"),
            Key::Insert => write!(f, "<Insert>"),
            Key::F(n) => write!(f, "<F{}>", n),
            Key::Unknown => write!(f, "<Unknown>"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_byte_printable() {
        assert_eq!(Key::from_byte(b'a'), Key::Char('a'));
        assert_eq!(Key::from_byte(b'Z'), Key::Char('Z'));
        assert_eq!(Key::from_byte(b'5'), Key::Char('5'));
    }

    #[test]
    fn test_from_byte_control() {
        assert_eq!(Key::from_byte(1), Key::Ctrl('a'));
        assert_eq!(Key::from_byte(3), Key::Ctrl('c'));
        assert_eq!(Key::from_byte(4), Key::Ctrl('d'));
    }

    #[test]
    fn test_from_byte_special() {
        assert_eq!(Key::from_byte(27), Key::Escape);
        assert_eq!(Key::from_byte(127), Key::Backspace);
    }

    #[test]
    fn test_is_printable() {
        assert!(Key::Char('a').is_printable());
        assert!(!Key::Ctrl('a').is_printable());
        assert!(!Key::Escape.is_printable());
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Key::Ctrl('d')), "^D");
        assert_eq!(format!("{}", Key::Escape), "<Esc>");
    }
}
