mod cursor;
mod word;

use crate::vi::cursor::{Cursor, MotionCommand, MotionError};

#[derive(Clone)]
enum CommandOp {
    /// *motion*
    Move(MotionCommand),
    /// <newline>
    Execute,
    /// *ctrl*-L
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
    ReplaceWith(u8),
    /// _
    AppendLastBigWord,
    /// x
    CutCurrentChars,
    /// X
    CutPreviousChars,
    /// d*motion*
    CutRange(MotionCommand),
    /// dd
    CutLine,
    /// D
    CutToEnd,
    /// y*motion*
    YankRange(MotionCommand),
    /// yy
    YankLine,
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
    SearchPattern(()),
    /// ?*pattern*
    SearchPatternAfter(()),
    /// n
    RepeatLastSearch,
    /// N
    RepeatLastSearchReverse,
}

#[derive(Clone)]
struct Command {
    count: Option<usize>,
    op: CommandOp,
}

enum CommandParseError {
    InvalidCommand,
    IncompleteCommand,
}

impl Command {
    fn parse(bytes: &[u8]) -> Result<Self, CommandParseError> {
        if bytes.is_empty() {
            return Err(CommandParseError::IncompleteCommand);
        }
        let last_digit = bytes
            .iter()
            .position(|c| !c.is_ascii_digit())
            .unwrap_or(bytes.len());
        if last_digit == bytes.len() {
            return Err(CommandParseError::IncompleteCommand);
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
            b'@' => return Err(CommandParseError::IncompleteCommand),
            b'~' => CommandOp::ChangeCase,
            b'.' => CommandOp::RepeatLast,
            b'v' => CommandOp::EditCommand,
            b'a' => CommandOp::InsertAtNextChar,
            b'A' => CommandOp::InsertAtLineEnd,
            b'i' => CommandOp::InsertAtCurrentPosition,
            b'I' => CommandOp::InsertAtLineStart,
            b'R' => CommandOp::ReplaceMode,
            b'c' if remaining_bytes.len() > 1 => {
                match MotionCommand::parse(&remaining_bytes[1..]) {
                    Ok(motion) => CommandOp::DeleteRange(motion),
                    Err(CommandParseError::InvalidCommand) if remaining_bytes[1] == b'c' => {
                        CommandOp::ClearLine
                    }
                    Err(other) => {
                        return Err(other);
                    }
                }
            }
            b'c' => return Err(CommandParseError::IncompleteCommand),
            b'C' => CommandOp::DeleteToEnd,
            b'S' => CommandOp::ClearEditLine,
            b'r' if remaining_bytes.len() > 1 => CommandOp::ReplaceWith(remaining_bytes[1]),
            b'r' => return Err(CommandParseError::IncompleteCommand),
            b'_' => CommandOp::AppendLastBigWord,
            b'x' => CommandOp::CutCurrentChars,
            b'X' => CommandOp::CutPreviousChars,
            b'd' if remaining_bytes.len() > 1 => {
                match MotionCommand::parse(&remaining_bytes[1..]) {
                    Ok(motion) => CommandOp::CutRange(motion),
                    Err(CommandParseError::InvalidCommand) if remaining_bytes[1] == b'd' => {
                        CommandOp::CutLine
                    }
                    Err(other) => {
                        return Err(other);
                    }
                }
            }
            b'd' => return Err(CommandParseError::IncompleteCommand),
            b'D' => CommandOp::CutToEnd,
            b'y' if remaining_bytes.len() > 1 => {
                match MotionCommand::parse(&remaining_bytes[1..]) {
                    Ok(motion) => CommandOp::YankRange(motion),
                    Err(CommandParseError::InvalidCommand) if remaining_bytes[1] == b'y' => {
                        CommandOp::YankLine
                    }
                    Err(other) => {
                        return Err(other);
                    }
                }
            }
            b'y' => return Err(CommandParseError::IncompleteCommand),
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
            _ => CommandOp::Move(MotionCommand::parse(remaining_bytes)?),
        };
        Ok(Command { op, count })
    }
}

#[derive(PartialEq, Eq)]
enum EditorMode {
    Insert,
    Replace,
    Command,
}

pub struct CommandError;

pub enum Action {
    Execute(Vec<u8>),
    Eof,
    Redraw,

    None,
}

pub struct ViEditor {
    input_buffer: Vec<u8>,
    edit_line: Vec<u8>,
    cursor: Cursor,
    mode: EditorMode,
    most_recent_nonmotion_command: Option<Command>,
    save_buffer: Vec<u8>,
}

impl ViEditor {
    fn execute_command(&mut self, command: Command) -> Result<Action, CommandError> {
        if let CommandOp::Move(motion) = command.op {
            self.cursor = self
                .cursor
                .moved(&self.edit_line, motion, command.count)
                .map_err(|_| CommandError)?;
            return Ok(Action::None);
        }
        self.most_recent_nonmotion_command = Some(command.clone());
        match command.op {
            CommandOp::Execute => {
                let mut result = Vec::new();
                std::mem::swap(&mut result, &mut self.edit_line);
                return Ok(Action::Execute(result));
            }
            CommandOp::Redraw => return Ok(Action::Redraw),
            CommandOp::CommentLine => {
                self.edit_line.insert(0, b'#');
            }
            CommandOp::DisplayExpansions => {}
            CommandOp::ExpandUnique => {}
            CommandOp::ExpandAll => {}
            CommandOp::Alias(_) => {
                // I don't know what they mean by alias since it's not
                // a shell alias, and no other types of alias are mentioned
            }
            CommandOp::ChangeCase => {
                let count = self.edit_line.len().min(command.count.unwrap_or(0) + 1);
                for i in 0..count {
                    let c = &mut self.edit_line[self.cursor.position + i];
                    if c.is_ascii_uppercase() {
                        *c = c.to_ascii_lowercase();
                    } else if c.is_ascii_lowercase() {
                        *c = c.to_ascii_uppercase();
                    }
                }
            }
            CommandOp::RepeatLast => {
                if let Some(last_command) = &self.most_recent_nonmotion_command {
                    return self.execute_command(last_command.clone());
                }
            }
            CommandOp::EditCommand => {}
            CommandOp::InsertAtNextChar => {
                self.cursor.position += 1;
                self.mode = EditorMode::Insert;
            }
            CommandOp::InsertAtLineEnd => {
                self.cursor.position = self.edit_line.len();
                self.mode = EditorMode::Insert;
            }
            CommandOp::InsertAtCurrentPosition => {
                self.mode = EditorMode::Insert;
            }
            CommandOp::InsertAtLineStart => {
                self.cursor.position = 0;
                self.mode = EditorMode::Insert;
            }
            CommandOp::ReplaceMode => {
                self.mode = EditorMode::Replace;
            }
            CommandOp::DeleteRange(motion) => {
                match self.cursor.moved(&self.edit_line, motion, command.count) {
                    Ok(range_end) => {
                        let range_end = range_end.position;
                        if self.cursor.position < range_end {
                            self.edit_line.drain(self.cursor.position..range_end);
                        } else {
                            self.edit_line.drain(range_end..self.cursor.position);
                            self.cursor.position = range_end;
                        }
                    }
                    Err(MotionError::AfterEnd) => {
                        self.edit_line.truncate(self.cursor.position);
                    }
                    Err(_) => {
                        return Err(CommandError);
                    }
                }
                self.mode = EditorMode::Insert;
            }
            CommandOp::ClearLine => {
                self.edit_line.clear();
                self.cursor.position = 0;
                self.mode = EditorMode::Insert;
            }
            CommandOp::DeleteToEnd => {
                self.edit_line.truncate(self.cursor.position);
                self.cursor.position = self.edit_line.len();
                self.mode = EditorMode::Insert;
            }
            CommandOp::ClearEditLine => {
                self.edit_line.clear();
                self.cursor.position = 0;
                self.mode = EditorMode::Insert;
            }
            CommandOp::ReplaceWith(c) => {
                let count = self.edit_line.len().min(command.count.unwrap_or(0) + 1);
                for i in 0..count {
                    self.edit_line[self.cursor.position + i] = c;
                }
            }
            CommandOp::AppendLastBigWord => {
                todo!()
            }
            CommandOp::CutCurrentChars => {
                let end = self
                    .edit_line
                    .len()
                    .min(self.cursor.position + command.count.unwrap_or(0) + 1);
                self.save_buffer = self.edit_line.drain(self.cursor.position..end).collect();
                if self.cursor.position != 0 && self.cursor.position == self.edit_line.len() {
                    self.cursor.position -= 1;
                }
            }
            CommandOp::CutPreviousChars => {
                if self.cursor.position == 0 {
                    return Err(CommandError);
                }
                let end = self
                    .cursor
                    .position
                    .saturating_sub(command.count.unwrap_or(0) + 1);
                self.save_buffer = self.edit_line.drain(end..self.cursor.position).collect();
                self.cursor.position = end;
            }
            CommandOp::CutRange(motion) => {
                match self.cursor.moved(&self.edit_line, motion, command.count) {
                    Ok(range_end) => {
                        let range_end = range_end.position;
                        if self.cursor.position < range_end {
                            self.save_buffer = self
                                .edit_line
                                .drain(self.cursor.position..range_end)
                                .collect();
                        } else {
                            self.save_buffer = self
                                .edit_line
                                .drain(range_end..self.cursor.position)
                                .collect();
                            self.cursor.position = range_end;
                        }
                    }
                    Err(MotionError::AfterEnd) => {
                        self.save_buffer = self.edit_line.drain(self.cursor.position..).collect();
                    }
                    Err(_) => {
                        return Err(CommandError);
                    }
                }
            }
            CommandOp::CutLine => {
                self.save_buffer = self.edit_line.drain(..).collect();
                self.cursor.position = 0;
            }
            CommandOp::CutToEnd => {
                self.save_buffer = self.edit_line.drain(self.cursor.position..).collect();
            }
            CommandOp::YankRange(motion) => {
                match self.cursor.moved(&self.edit_line, motion, command.count) {
                    Ok(range_end) => {
                        let range_end = range_end.position;
                        if self.cursor.position < range_end {
                            self.save_buffer =
                                self.edit_line[self.cursor.position..range_end].to_vec();
                        } else {
                            self.save_buffer =
                                self.edit_line[range_end..self.cursor.position].to_vec();
                        }
                    }
                    Err(MotionError::AfterEnd) => {
                        self.save_buffer = self.edit_line[self.cursor.position..].to_vec();
                    }
                    Err(_) => {
                        return Err(CommandError);
                    }
                }
            }
            CommandOp::YankLine => {
                self.save_buffer = self.edit_line.clone();
            }
            CommandOp::YankToEnd => {
                self.save_buffer = self.edit_line[self.cursor.position..].to_vec();
            }
            CommandOp::PasteAfter => {
                if !self.save_buffer.is_empty() {
                    self.edit_line.splice(
                        self.cursor.position..self.cursor.position,
                        self.save_buffer.iter().copied(),
                    );
                    self.cursor.position += 1;
                }
            }
            CommandOp::PasteBefore => {
                if !self.save_buffer.is_empty() {
                    self.edit_line.splice(
                        self.cursor.position..self.cursor.position,
                        self.save_buffer.iter().copied(),
                    );
                }
            }
            CommandOp::UndoLastCommand => {}
            CommandOp::UndoAll => {}
            CommandOp::PreviousShellCommand => {}
            CommandOp::NextShellCommand => {}
            CommandOp::OldestShellCommand => {}
            CommandOp::SearchPattern(_) => {}
            CommandOp::SearchPatternAfter(_) => {}
            CommandOp::RepeatLastSearch => {}
            CommandOp::RepeatLastSearchReverse => {}
            CommandOp::Move(_) => unreachable!(),
        }
        Ok(Action::None)
    }

    pub fn current_line(&self) -> Vec<u8> {
        let mut result = Vec::new();
        result.extend_from_slice(b"\r\x1b[K");
        result.extend_from_slice(&self.edit_line);
        result.extend_from_slice(b"\x1b[");
        result.extend_from_slice((self.cursor.position + 1).to_string().as_bytes());
        result.push(b'G');
        result
    }

    pub fn process_new_input(&mut self, c: u8) -> Result<Action, CommandError> {
        match self.mode {
            EditorMode::Insert | EditorMode::Replace => {
                match c {
                    b'\n' => {}
                    b'\x1B' => {
                        // escape
                        self.mode = EditorMode::Command;
                        self.cursor.position = self
                            .cursor
                            .position
                            .min(self.edit_line.len().saturating_sub(1));
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
                            if self.mode == EditorMode::Replace {
                                self.edit_line[self.cursor.position] = other;
                            } else {
                                self.edit_line.insert(self.cursor.position, other);
                            }
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
                match Command::parse(&self.input_buffer) {
                    Ok(command) => {
                        self.input_buffer.clear();
                        return self.execute_command(command);
                    }
                    Err(CommandParseError::IncompleteCommand) => {
                        // we need more input, nothing to do here
                    }
                    Err(CommandParseError::InvalidCommand) => {
                        self.input_buffer.clear();
                        return Err(CommandError);
                    }
                }
            }
        }
        Ok(Action::None)
    }
}

impl Default for ViEditor {
    fn default() -> Self {
        Self {
            mode: EditorMode::Insert,
            input_buffer: Vec::new(),
            cursor: Cursor::default(),
            edit_line: Vec::new(),
            most_recent_nonmotion_command: None,
            save_buffer: Vec::new(),
        }
    }
}
