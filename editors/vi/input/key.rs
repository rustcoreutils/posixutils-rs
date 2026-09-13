//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

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
            // TAB must become Key::Tab, not Ctrl('i'). The catch-all 1..=26 arm
            // below used to swallow it, so Key::Tab was never constructed and
            // the `Key::Tab` arm in insert mode was unreachable: a typed TAB
            // fell through to the ignore-everything-else arm (#V22). Byte 10
            // deliberately stays Ctrl('j') — insert mode matches that name.
            9 => Key::Tab,
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

    /// The key a `char` names, as [`Key::from_byte`] would name the same byte.
    ///
    /// Used wherever text has to be turned back into keystrokes — a `:map`
    /// right-hand side, a register executed by `@`, the test harness — so that
    /// all of them agree with what a terminal actually delivers. Doing it by
    /// hand is how the two open-coded copies of this drifted: neither produced
    /// [`Key::Tab`] for a TAB, which is the same defect as #V22, and one turned
    /// DEL into a `Char` rather than [`Key::Backspace`].
    ///
    /// A non-ASCII `char` is its own [`Key::Char`]; `from_byte` never sees one,
    /// since the reader decodes UTF-8 before this point.
    ///
    /// One deliberate difference from `from_byte`: `'\n'` is [`Key::Enter`]
    /// here, where byte 10 stays `Ctrl('j')`. A terminal's Enter key sends CR,
    /// so a bare LF arriving as a *byte* really is `^J` and insert mode names
    /// it that way — but a `'\n'` written in a register, a map or a test string
    /// means the line ended, which is the Enter key. The two spellings are
    /// interchangeable in text input mode, where `insert_newline` takes both;
    /// they are not in command mode, where `^J` moves the cursor down.
    pub fn from_map_char(c: char) -> Self {
        match c {
            '\n' => Key::Enter,
            c if c.is_ascii() => Key::from_byte(c as u8),
            c => Key::Char(c),
        }
    }

    /// The character this key produces when entered literally after `^V`.
    ///
    /// POSIX 121870-121872 allows "any subsequent character" to be entered as a
    /// literal, "removing any special meaning that it may have to the editor in
    /// text input mode". Returns `None` for keys with no character
    /// representation (arrows, function keys, `Unknown`).
    pub fn literal_char(&self) -> Option<char> {
        match *self {
            Key::Char(c) => Some(c),
            Key::Tab => Some('\t'),
            Key::Enter => Some('\r'),
            Key::Escape => Some('\x1b'),
            Key::Backspace => Some('\x7f'),
            Key::Ctrl('@') => Some('\0'),
            Key::Ctrl('\\') => Some('\x1c'),
            Key::Ctrl(']') => Some('\x1d'),
            Key::Ctrl('^') => Some('\x1e'),
            Key::Ctrl('_') => Some('\x1f'),
            Key::Ctrl('[') => Some('\x1b'),
            Key::Ctrl(c) if c.is_ascii_lowercase() => Some((c as u8 - b'a' + 1) as char),
            _ => None,
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

    /// #V22: byte 9 must be `Key::Tab`, not `Ctrl('i')`.
    ///
    /// The 1..=26 arm used to swallow TAB, so `Key::Tab` was never constructed
    /// anywhere and insert mode's `Key::Tab` arm was dead code. This asserts
    /// the mapping directly because `Editor::execute_keys` has its own
    /// byte-to-key translation — a headless test cannot reach `from_byte`.
    #[test]
    fn test_from_byte_tab_is_not_ctrl_i() {
        assert_eq!(Key::from_byte(9), Key::Tab);
        assert_ne!(Key::from_byte(9), Key::Ctrl('i'));
        // Byte 10 must stay Ctrl('j') — insert mode matches it by that name.
        assert_eq!(Key::from_byte(10), Key::Ctrl('j'));
        assert_eq!(Key::from_byte(13), Key::Enter);
    }

    /// #V23: the literal-character mapping used by `^V`.
    #[test]
    fn test_literal_char() {
        assert_eq!(Key::Char('x').literal_char(), Some('x'));
        assert_eq!(Key::Tab.literal_char(), Some('\t'));
        assert_eq!(Key::Escape.literal_char(), Some('\x1b'));
        assert_eq!(Key::Ctrl('d').literal_char(), Some('\x04'));
        assert_eq!(Key::Ctrl('a').literal_char(), Some('\x01'));
        assert_eq!(Key::Ctrl('@').literal_char(), Some('\0'));
        assert_eq!(Key::Backspace.literal_char(), Some('\x7f'));
        // Keys with no character representation.
        assert_eq!(Key::Up.literal_char(), None);
        assert_eq!(Key::F(1).literal_char(), None);
        assert_eq!(Key::Unknown.literal_char(), None);
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

    /// Text turned back into keystrokes must name the same keys a terminal
    /// delivers. The two open-coded copies this replaced did not: both made a
    /// TAB into `Ctrl('i')` rather than `Key::Tab`, which is #V22 over again,
    /// and one left DEL as a `Char`.
    #[test]
    fn test_from_map_char_agrees_with_from_byte() {
        for b in 0u8..=127 {
            let c = b as char;
            if c == '\n' {
                continue; // the one deliberate difference, asserted below
            }
            assert_eq!(
                Key::from_map_char(c),
                Key::from_byte(b),
                "byte {b:#04x} disagrees"
            );
        }
        assert_eq!(Key::from_map_char('\t'), Key::Tab);
        assert_eq!(Key::from_map_char('\x7f'), Key::Backspace);
        assert_eq!(Key::from_map_char('\x1b'), Key::Escape);
        assert_eq!(Key::from_map_char('\x01'), Key::Ctrl('a'));
        assert_eq!(Key::from_map_char('é'), Key::Char('é'));
    }

    /// A `'\n'` in a register, a map or a test string means the line ended,
    /// i.e. the Enter key -- a terminal's Enter sends CR. Byte 10 keeps its
    /// `^J` identity, which matters in command mode where `^J` moves down.
    #[test]
    fn test_from_map_char_newline_is_enter() {
        assert_eq!(Key::from_map_char('\n'), Key::Enter);
        assert_eq!(Key::from_map_char('\r'), Key::Enter);
        assert_eq!(Key::from_byte(10), Key::Ctrl('j'));
    }
}
