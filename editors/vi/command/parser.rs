//! Command parsing for vi command mode.

use crate::input::Key;

/// A parsed vi command.
#[derive(Debug, Clone)]
pub struct ParsedCommand {
    /// Optional register specification.
    pub register: Option<char>,
    /// Count prefix (defaults to 1).
    pub count: usize,
    /// Whether a count was explicitly given by the user.
    pub has_count: bool,
    /// The command character.
    pub command: char,
    /// Motion command (for operators).
    pub motion: Option<MotionCommand>,
    /// Extra character argument (for f, F, t, T, r, m, ', `).
    pub char_arg: Option<char>,
}

impl ParsedCommand {
    /// Create a new parsed command.
    pub fn new(command: char) -> Self {
        Self {
            register: None,
            count: 1,
            has_count: false,
            command,
            motion: None,
            char_arg: None,
        }
    }

    /// Set the count.
    pub fn with_count(mut self, count: usize, has_count: bool) -> Self {
        self.count = count.max(1);
        self.has_count = has_count;
        self
    }

    /// Set the register.
    pub fn with_register(mut self, register: char) -> Self {
        self.register = Some(register);
        self
    }

    /// Set the motion.
    pub fn with_motion(mut self, motion: MotionCommand) -> Self {
        self.motion = Some(motion);
        self
    }

    /// Set the char argument.
    pub fn with_char(mut self, c: char) -> Self {
        self.char_arg = Some(c);
        self
    }
}

/// Motion command for operators.
#[derive(Debug, Clone)]
pub struct MotionCommand {
    /// Count for the motion.
    pub count: usize,
    /// The motion character.
    pub motion: char,
    /// Extra character argument.
    pub char_arg: Option<char>,
}

impl MotionCommand {
    pub fn new(motion: char) -> Self {
        Self {
            count: 1,
            motion,
            char_arg: None,
        }
    }

    pub fn with_count(mut self, count: usize) -> Self {
        self.count = count.max(1);
        self
    }

    pub fn with_char(mut self, c: char) -> Self {
        self.char_arg = Some(c);
        self
    }
}

/// State of the command parser.
#[derive(Debug, Clone, PartialEq)]
pub enum ParserState {
    /// Ready for new command.
    Ready,
    /// Accumulating count digits.
    Count,
    /// Got register prefix (").
    Register,
    /// Got operator, waiting for motion.
    Operator,
    /// Waiting for character argument (f, F, t, T, r, m, ', `).
    WaitingChar,
    /// Got motion, waiting for motion's char argument.
    MotionWaitingChar,
    /// Command is complete.
    Complete,
    /// Got an error.
    Error(String),
}

/// Command parser for vi command mode.
#[derive(Debug)]
pub struct CommandParser {
    state: ParserState,
    register: Option<char>,
    count1: usize,
    count2: usize,
    has_count1: bool,
    has_count2: bool,
    command: Option<char>,
    motion: Option<char>,
    char_arg: Option<char>,
}

impl CommandParser {
    /// Create a new parser.
    pub fn new() -> Self {
        Self {
            state: ParserState::Ready,
            register: None,
            count1: 0,
            count2: 0,
            has_count1: false,
            has_count2: false,
            command: None,
            motion: None,
            char_arg: None,
        }
    }

    /// Reset the parser state.
    pub fn reset(&mut self) {
        self.state = ParserState::Ready;
        self.register = None;
        self.count1 = 0;
        self.count2 = 0;
        self.has_count1 = false;
        self.has_count2 = false;
        self.command = None;
        self.motion = None;
        self.char_arg = None;
    }

    /// Get the current state.
    pub fn state(&self) -> &ParserState {
        &self.state
    }

    /// Check if parser is ready for a new command.
    pub fn is_ready(&self) -> bool {
        self.state == ParserState::Ready
    }

    /// Check if command is complete.
    pub fn is_complete(&self) -> bool {
        self.state == ParserState::Complete
    }

    /// Check if there was an error.
    pub fn is_error(&self) -> bool {
        matches!(self.state, ParserState::Error(_))
    }

    /// Process a key and return true if more input is needed.
    pub fn process_key(&mut self, key: Key) -> bool {
        match key {
            Key::Escape => {
                self.reset();
                return false;
            }
            Key::Char(c) => self.process_char(c),
            Key::Ctrl(c) => self.process_ctrl(c),
            _ => {
                // Other keys are not valid in command mode
                self.state = ParserState::Error("Invalid key".to_string());
                return false;
            }
        }

        !matches!(self.state, ParserState::Complete | ParserState::Error(_))
    }

    /// Process a regular character.
    fn process_char(&mut self, c: char) {
        match &self.state {
            ParserState::Ready | ParserState::Count => {
                if c == '"' && self.register.is_none() {
                    // Register prefix
                    self.state = ParserState::Register;
                } else if c.is_ascii_digit() && (c != '0' || self.has_count1) {
                    // Count digit (0 alone is a command, not a count)
                    self.count1 = self.count1 * 10 + (c as usize - '0' as usize);
                    self.has_count1 = true;
                    self.state = ParserState::Count;
                } else {
                    // Command character
                    self.command = Some(c);
                    self.process_command(c);
                }
            }
            ParserState::Register => {
                if c.is_ascii_alphabetic() || c.is_ascii_digit() || c == '"' {
                    self.register = Some(c);
                    self.state = ParserState::Ready;
                } else {
                    self.state = ParserState::Error("Invalid register".to_string());
                }
            }
            ParserState::Operator => {
                // Waiting for motion after operator
                if c.is_ascii_digit() && (c != '0' || self.has_count2) {
                    self.count2 = self.count2 * 10 + (c as usize - '0' as usize);
                    self.has_count2 = true;
                } else {
                    self.motion = Some(c);
                    self.process_motion(c);
                }
            }
            ParserState::WaitingChar => {
                self.char_arg = Some(c);
                self.state = ParserState::Complete;
            }
            ParserState::MotionWaitingChar => {
                self.char_arg = Some(c);
                self.state = ParserState::Complete;
            }
            _ => {}
        }
    }

    /// Process a control character.
    fn process_ctrl(&mut self, c: char) {
        // Control characters that are commands
        let ctrl_commands = [
            'b', 'd', 'e', 'f', 'g', 'h', 'j', 'l', 'm', 'n', 'p', 'r', 'u', 'y', '^', ']',
        ];

        if ctrl_commands.contains(&c) {
            self.command = Some(c);
            // Treat as simple command (no motion, no char arg)
            self.state = ParserState::Complete;
        } else {
            self.state = ParserState::Error("Invalid control command".to_string());
        }
    }

    /// Process a command character.
    fn process_command(&mut self, c: char) {
        // Commands that take a motion
        let operators = ['c', 'd', 'y', '<', '>', '!'];

        // Commands that need a character argument
        let char_commands = ['f', 'F', 't', 'T', 'r', 'm', '\'', '`', '@'];

        // Simple commands (no motion, no char arg)
        // Note: '[' and ']' are NOT in this list because they need to parse as [[ and ]]
        let simple_commands = [
            'a', 'A', 'b', 'B', 'C', 'D', 'e', 'E', 'G', 'h', 'H', 'i', 'I', 'j', 'J', 'k', 'l',
            'L', 'M', 'n', 'N', 'o', 'O', 'p', 'P', 'Q', 'R', 's', 'S', 'u', 'U', 'w', 'W', 'x',
            'X', 'Y', '~', '.', ';', ',', '%', '^', '$', '|', '0', '-', '+', '_', '(', ')', '{',
            '}', '&',
        ];

        if operators.contains(&c) {
            self.state = ParserState::Operator;
        } else if char_commands.contains(&c) {
            self.state = ParserState::WaitingChar;
        } else if simple_commands.contains(&c) {
            self.state = ParserState::Complete;
        } else if c == 'Z' || c == '[' || c == ']' {
            // ZZ, [[, ]] are special doubled commands
            self.state = ParserState::WaitingChar;
        } else if c == 'g' {
            // g-prefixed commands
            self.state = ParserState::WaitingChar;
        } else if c == ':' || c == '/' || c == '?' {
            // Line commands
            self.state = ParserState::Complete;
        } else {
            self.state = ParserState::Error(format!("Unknown command: {}", c));
        }
    }

    /// Process a motion character.
    fn process_motion(&mut self, c: char) {
        // Check if this is the same character as the operator (dd, cc, yy, etc.)
        if Some(c) == self.command {
            self.state = ParserState::Complete;
            return;
        }

        // Motions that need a character argument
        let char_motions = ['f', 'F', 't', 'T', '\'', '`'];

        // Valid motion commands
        let valid_motions = [
            'h', 'j', 'k', 'l', 'w', 'W', 'b', 'B', 'e', 'E', '0', '^', '$', 'G', 'H', 'L', 'M',
            '%', '|', '(', ')', '{', '}', '[', ']', 'n', 'N', '/', '?', '+', '-', '_',
        ];

        if char_motions.contains(&c) {
            self.state = ParserState::MotionWaitingChar;
        } else if valid_motions.contains(&c) {
            self.state = ParserState::Complete;
        } else {
            self.state = ParserState::Error(format!("Invalid motion: {}", c));
        }
    }

    /// Get the parsed command.
    pub fn get_command(&self) -> Option<ParsedCommand> {
        if !self.is_complete() {
            return None;
        }

        let command = self.command?;
        let has_count = self.has_count1 || self.has_count2;
        let count = if has_count {
            let c1 = if self.has_count1 { self.count1 } else { 1 };
            let c2 = if self.has_count2 { self.count2 } else { 1 };
            c1 * c2
        } else {
            1
        };

        let mut cmd = ParsedCommand::new(command).with_count(count, has_count);

        if let Some(r) = self.register {
            cmd = cmd.with_register(r);
        }

        if let Some(m) = self.motion {
            let motion_count = if self.has_count2 { self.count2 } else { 1 };
            let mut motion = MotionCommand::new(m).with_count(motion_count);
            if let Some(c) = self.char_arg {
                motion = motion.with_char(c);
            }
            cmd = cmd.with_motion(motion);
        } else if let Some(c) = self.char_arg {
            cmd = cmd.with_char(c);
        }

        Some(cmd)
    }

    /// Get the effective count (for commands without motion).
    pub fn get_count(&self) -> usize {
        if self.has_count1 {
            self.count1
        } else {
            1
        }
    }
}

impl Default for CommandParser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_command() {
        let mut parser = CommandParser::new();
        parser.process_key(Key::Char('j'));
        assert!(parser.is_complete());
        let cmd = parser.get_command().unwrap();
        assert_eq!(cmd.command, 'j');
        assert_eq!(cmd.count, 1);
    }

    #[test]
    fn test_count_command() {
        let mut parser = CommandParser::new();
        parser.process_key(Key::Char('3'));
        parser.process_key(Key::Char('j'));
        assert!(parser.is_complete());
        let cmd = parser.get_command().unwrap();
        assert_eq!(cmd.command, 'j');
        assert_eq!(cmd.count, 3);
    }

    #[test]
    fn test_operator_motion() {
        let mut parser = CommandParser::new();
        parser.process_key(Key::Char('d'));
        assert!(!parser.is_complete());
        parser.process_key(Key::Char('w'));
        assert!(parser.is_complete());
        let cmd = parser.get_command().unwrap();
        assert_eq!(cmd.command, 'd');
        assert!(cmd.motion.is_some());
        assert_eq!(cmd.motion.unwrap().motion, 'w');
    }

    #[test]
    fn test_operator_doubled() {
        let mut parser = CommandParser::new();
        parser.process_key(Key::Char('d'));
        parser.process_key(Key::Char('d'));
        assert!(parser.is_complete());
        let cmd = parser.get_command().unwrap();
        assert_eq!(cmd.command, 'd');
        assert!(cmd.motion.is_some());
        assert_eq!(cmd.motion.unwrap().motion, 'd');
    }

    #[test]
    fn test_register_command() {
        let mut parser = CommandParser::new();
        parser.process_key(Key::Char('"'));
        parser.process_key(Key::Char('a'));
        parser.process_key(Key::Char('d'));
        parser.process_key(Key::Char('w'));
        assert!(parser.is_complete());
        let cmd = parser.get_command().unwrap();
        assert_eq!(cmd.register, Some('a'));
        assert_eq!(cmd.command, 'd');
    }

    #[test]
    fn test_find_char() {
        let mut parser = CommandParser::new();
        parser.process_key(Key::Char('f'));
        assert!(!parser.is_complete());
        parser.process_key(Key::Char('x'));
        assert!(parser.is_complete());
        let cmd = parser.get_command().unwrap();
        assert_eq!(cmd.command, 'f');
        assert_eq!(cmd.char_arg, Some('x'));
    }

    #[test]
    fn test_escape_resets() {
        let mut parser = CommandParser::new();
        parser.process_key(Key::Char('d'));
        parser.process_key(Key::Escape);
        assert!(parser.is_ready());
    }

    #[test]
    fn test_count_with_motion() {
        let mut parser = CommandParser::new();
        parser.process_key(Key::Char('2'));
        parser.process_key(Key::Char('d'));
        parser.process_key(Key::Char('3'));
        parser.process_key(Key::Char('w'));
        assert!(parser.is_complete());
        let cmd = parser.get_command().unwrap();
        assert_eq!(cmd.count, 6); // 2 * 3
    }
}
