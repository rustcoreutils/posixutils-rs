mod cursor;
mod word;

use crate::vi::cursor::{Cursor, MotionCommand};
use crate::wordexp::pattern::Pattern;

enum CommandOp {
    /// *motion*
    Move(MotionCommand),
    /// <newline>
    Execute,
    /// <ctrl>L
    Redraw,
    /// \#
    CommentLine,
    /// =
    DisplayExpansions,
    /// \
    ExpandUnique,
    /// *
    ExpandAll,
    /// @c
    Alias(char),
    /// ~
    ChangeCase,
    /// .
    RepeatLast,
    /// v
    EditCommand,
    /// a
    InsertAtNextChar,
    /// A
    InsertAtLineEnd,
    /// i
    InsertAtCurrentPosition,
    /// I
    InsertAtLineStart,
    /// R
    ReplaceMode,
    /// c*motion*
    DeleteRange(MotionCommand),
    /// cc | dd
    ClearLine,
    /// C
    DeleteToEnd,
    /// S
    ClearEditLine,
    /// r
    ReplaceWith(char),
    /// _
    AppendLastBigWord,
    /// x
    CutCurrentChars,
    /// X
    CutPreviousChars,
    /// d*motion*
    CutRange(MotionCommand),
    /// D
    CutToEnd,
    /// y
    YankRange(MotionCommand),
    /// Y
    YankToEnd,
    /// p
    PasteAfter,
    /// P
    PasteBefore,
    /// u
    UndoLastCommand,
    /// U
    UndoAll,
    /// k | -
    PreviousShellCommand,
    /// j | +
    NextShellCommand,
    /// G
    OldestShellCommand,
    /// /*pattern*
    SearchPattern(Pattern),
    /// ?*pattern*
    SearchPatternAfter(Pattern),
    /// n
    RepeatLastSearch,
    /// N
    RepeatLastSearchReverse,
}

struct Command {
    count: Option<usize>,
    op: CommandOp,
}

impl Command {
    fn try_from_bytes(bytes: &[u8]) -> Option<Self> {
        if bytes.is_empty() {
            return None;
        }
        let last_digit = bytes
            .iter()
            .position(|c| !c.is_ascii_digit())
            .unwrap_or(bytes.len());
        if last_digit == bytes.len() {
            return None;
        }
        let count = if last_digit != 0 && bytes[last_digit] != b'0' {
            // TODO: handle count too big
            let count = std::str::from_utf8(&bytes[..last_digit])
                .unwrap()
                .parse::<usize>()
                .unwrap();
            Some(count - 1)
        } else {
            None
        };
        let remaining_bytes = &bytes[last_digit..];
        let op = match remaining_bytes[0] {
            b'\n' => CommandOp::Execute,
            b'\x0c' => CommandOp::Redraw, // <ctrl>L
            b'#' => CommandOp::CommentLine,
            b'=' => CommandOp::DisplayExpansions,
            b'\\' => CommandOp::ExpandUnique,
            b'*' => CommandOp::ExpandAll,
            b'@' if remaining_bytes.len() > 1 => CommandOp::Alias(remaining_bytes[1] as char),
            b'~' => CommandOp::ChangeCase,
            b'.' => CommandOp::RepeatLast,
            b'v' => CommandOp::EditCommand,
            b'a' => CommandOp::InsertAtNextChar,
            b'A' => CommandOp::InsertAtLineEnd,
            b'i' => CommandOp::InsertAtCurrentPosition,
            b'I' => CommandOp::InsertAtLineStart,
            b'R' => CommandOp::ReplaceMode,
            b'c' if remaining_bytes.len() > 1 => {
                if let Some(motion) = MotionCommand::try_from_bytes(&remaining_bytes[1..]) {
                    CommandOp::DeleteRange(motion)
                } else if remaining_bytes[1] == b'c' {
                    CommandOp::ClearLine
                } else {
                    return None;
                }
            }
            b'C' => CommandOp::DeleteToEnd,
            b'S' => CommandOp::ClearEditLine,
            b'r' if remaining_bytes.len() > 1 => CommandOp::ReplaceWith(remaining_bytes[1] as char),
            b'_' => CommandOp::AppendLastBigWord,
            b'x' => CommandOp::CutCurrentChars,
            b'X' => CommandOp::CutPreviousChars,
            b'd' if remaining_bytes.len() > 1 => {
                if let Some(motion) = MotionCommand::try_from_bytes(&remaining_bytes[1..]) {
                    CommandOp::CutRange(motion)
                } else if remaining_bytes[1] == b'd' {
                    CommandOp::ClearLine
                } else {
                    return None;
                }
            }
            b'D' => CommandOp::CutToEnd,
            b'y' if remaining_bytes.len() > 1 => {
                let motion = MotionCommand::try_from_bytes(&remaining_bytes[1..])?;
                CommandOp::YankRange(motion)
            }
            b'Y' => CommandOp::YankToEnd,
            b'p' => CommandOp::PasteAfter,
            b'P' => CommandOp::PasteBefore,
            b'u' => CommandOp::UndoLastCommand,
            b'U' => CommandOp::UndoAll,
            b'k' | b'-' => CommandOp::PreviousShellCommand,
            b'j' | b'+' => CommandOp::NextShellCommand,
            b'G' => CommandOp::OldestShellCommand,
            b'/' if remaining_bytes.len() > 1 => {
                todo!()
            }
            b'?' if remaining_bytes.len() > 1 => {
                todo!()
            }
            b'n' => CommandOp::RepeatLastSearch,
            b'N' => CommandOp::RepeatLastSearchReverse,
            _ => CommandOp::Move(MotionCommand::try_from_bytes(remaining_bytes)?),
        };
        Some(Command { op, count })
    }
}

enum EditorMode {
    Insert,
    Command,
}

pub struct CommandError;

pub struct ViEditor {
    input_buffer: Vec<u8>,
    edit_line: Vec<u8>,
    cursor: Cursor,
    mode: EditorMode,
}

impl ViEditor {
    pub fn current_line(&self) -> Vec<u8> {
        let mut result = Vec::new();
        result.extend_from_slice(b"\r\x1b[K");
        result.extend_from_slice(&self.edit_line);
        result.extend_from_slice(b"\x1b[");
        result.extend_from_slice((self.cursor.position + 1).to_string().as_bytes());
        result.push(b'G');
        result
    }

    pub fn process_new_input(&mut self, c: u8) -> Result<(), CommandError> {
        match self.mode {
            EditorMode::Insert => {
                match c {
                    b'\n' => {}
                    b'\x1B' => {
                        // escape
                        self.mode = EditorMode::Command;
                        self.cursor.position = self.cursor.position.min(self.edit_line.len() - 1);
                    }
                    b'\x7F' => {
                        // delete
                        if !self.edit_line.is_empty() {
                            if self.cursor.position != 0 {
                                self.edit_line.remove(self.cursor.position - 1);
                                self.cursor.position -= 1;
                            }
                        }
                    }
                    b'\x04' => {
                        // TODO: EOF
                        println!("EOF")
                    }
                    b'\x16' => {
                        // ^V
                    }
                    b'\x17' => {}
                    other if !other.is_ascii_control() => {
                        if self.cursor.position < self.edit_line.len() {
                            self.edit_line.insert(self.cursor.position, other);
                        } else {
                            self.edit_line.push(other);
                        }
                        self.cursor.position += 1;
                    }
                    _ => {}
                }
            }
            EditorMode::Command => {
                self.input_buffer.push(c);
                if let Some(command) = Command::try_from_bytes(&self.input_buffer) {
                    self.input_buffer.clear();
                    match command.op {
                        CommandOp::Move(motion) => {
                            self.cursor = self
                                .cursor
                                .moved(&self.edit_line, motion, command.count)
                                .map_err(|_| CommandError)?;
                        }
                        CommandOp::Execute => {}
                        CommandOp::Redraw => {}
                        CommandOp::CommentLine => {}
                        CommandOp::DisplayExpansions => {}
                        CommandOp::ExpandUnique => {}
                        CommandOp::ExpandAll => {}
                        CommandOp::Alias(_) => {}
                        CommandOp::ChangeCase => {}
                        CommandOp::RepeatLast => {}
                        CommandOp::EditCommand => {}
                        CommandOp::InsertAtNextChar => {}
                        CommandOp::InsertAtLineEnd => {}
                        CommandOp::InsertAtCurrentPosition => {
                            self.mode = EditorMode::Insert;
                        }
                        CommandOp::InsertAtLineStart => {}
                        CommandOp::ReplaceMode => {}
                        CommandOp::DeleteRange(_) => {}
                        CommandOp::ClearLine => {}
                        CommandOp::DeleteToEnd => {}
                        CommandOp::ClearEditLine => {}
                        CommandOp::ReplaceWith(_) => {}
                        CommandOp::AppendLastBigWord => {}
                        CommandOp::CutCurrentChars => {}
                        CommandOp::CutPreviousChars => {}
                        CommandOp::CutRange(_) => {}
                        CommandOp::CutToEnd => {}
                        CommandOp::YankRange(_) => {}
                        CommandOp::YankToEnd => {}
                        CommandOp::PasteAfter => {}
                        CommandOp::PasteBefore => {}
                        CommandOp::UndoLastCommand => {}
                        CommandOp::UndoAll => {}
                        CommandOp::PreviousShellCommand => {}
                        CommandOp::NextShellCommand => {}
                        CommandOp::OldestShellCommand => {}
                        CommandOp::SearchPattern(_) => {}
                        CommandOp::SearchPatternAfter(_) => {}
                        CommandOp::RepeatLastSearch => {}
                        CommandOp::RepeatLastSearchReverse => {}
                    }
                }
            }
        }
        Ok(())
    }
}

impl Default for ViEditor {
    fn default() -> Self {
        Self {
            mode: EditorMode::Insert,
            input_buffer: Vec::new(),
            cursor: Cursor::default(),
            edit_line: Vec::new(),
        }
    }
}
