use crate::wordexp::pattern::Pattern;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MotionCommand {
    /// l | <space>
    MoveRight,
    /// h
    MoveLeft,
    /// w
    NextWordStart,
    /// W
    NextBigwordStart,
    /// e
    CurrentWordEnd,
    /// E
    CurrentBigwordEnd,
    /// b
    CurrentWordBegin,
    /// B
    CurrentBigwordBegin,
    /// ^
    GotoFirstCharOnLine,
    /// $
    GotoLastCharOnLine,
    /// 0
    GotoFirstCharPosition,
    /// |
    GotoCharPosition,
    /// fc
    MoveToBeforeFirstChar(char),
    /// Fc
    MoveToBeforeFirstCharReverse(char),
    /// tc
    MoveToAfterFirstChar(char),
    /// Tc
    MoveToAfterFirstCharReverse(char),
    /// ;
    RepeatLastMoveToChar,
    /// ,
    RepeatLastMoveToCharReverse,
}

impl MotionCommand {
    fn try_from_bytes(bytes: &[u8]) -> Option<Self> {
        assert!(!bytes.is_empty());
        match bytes[0] {
            b'l' | b' ' => Some(Self::MoveRight),
            b'h' => Some(Self::MoveLeft),
            b'w' => Some(Self::NextWordStart),
            b'W' => Some(Self::NextBigwordStart),
            b'e' => Some(Self::CurrentWordEnd),
            b'E' => Some(Self::CurrentBigwordEnd),
            b'b' => Some(Self::CurrentWordBegin),
            b'B' => Some(Self::CurrentBigwordBegin),
            b'^' => Some(Self::GotoFirstCharOnLine),
            b'$' => Some(Self::GotoLastCharOnLine),
            b'0' => Some(Self::GotoFirstCharPosition),
            b'|' => Some(Self::GotoCharPosition),
            b'f' if bytes.len() > 1 => Some(Self::MoveToBeforeFirstChar(bytes[1] as char)),
            b'F' if bytes.len() > 1 => Some(Self::MoveToBeforeFirstCharReverse(bytes[1] as char)),
            b't' if bytes.len() > 1 => Some(Self::MoveToAfterFirstChar(bytes[1] as char)),
            b'T' if bytes.len() > 1 => Some(Self::MoveToAfterFirstCharReverse(bytes[1] as char)),
            b';' => Some(Self::RepeatLastMoveToChar),
            b',' => Some(Self::RepeatLastMoveToCharReverse),
            _ => None,
        }
    }
}

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
    count: u32,
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
        let count = if last_digit != 0 {
            // TODO: handle count too big
            std::str::from_utf8(&bytes[..last_digit])
                .unwrap()
                .parse()
                .unwrap()
        } else {
            1
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

pub struct ViEditor {
    input_buffer: Vec<u8>,
    edit_line: Vec<u8>,
    cursor_position: usize,
    mode: EditorMode,
}

impl ViEditor {
    fn new_cursor_position(&self, motion_command: MotionCommand, count: u32) -> usize {
        match motion_command {
            MotionCommand::MoveRight => self
                .edit_line
                .len()
                .min(self.cursor_position + count as usize),
            MotionCommand::MoveLeft => self.cursor_position.saturating_sub(count as usize),
            MotionCommand::NextWordStart => {
                todo!()
            }
            MotionCommand::NextBigwordStart => {
                todo!()
            }
            MotionCommand::CurrentWordEnd => {
                todo!()
            }
            MotionCommand::CurrentBigwordEnd => {
                todo!()
            }
            MotionCommand::CurrentWordBegin => {
                todo!()
            }
            MotionCommand::CurrentBigwordBegin => {
                todo!()
            }
            MotionCommand::GotoFirstCharOnLine => {
                todo!()
            }
            MotionCommand::GotoLastCharOnLine => {
                todo!()
            }
            MotionCommand::GotoFirstCharPosition => {
                todo!()
            }
            MotionCommand::GotoCharPosition => {
                todo!()
            }
            MotionCommand::MoveToBeforeFirstChar(_) => {
                todo!()
            }
            MotionCommand::MoveToBeforeFirstCharReverse(_) => {
                todo!()
            }
            MotionCommand::MoveToAfterFirstChar(_) => {
                todo!()
            }
            MotionCommand::MoveToAfterFirstCharReverse(_) => {
                todo!()
            }
            MotionCommand::RepeatLastMoveToChar => {
                todo!()
            }
            MotionCommand::RepeatLastMoveToCharReverse => {
                todo!()
            }
        }
    }

    pub fn current_line(&self) -> Vec<u8> {
        let mut result = Vec::new();
        result.extend_from_slice(b"\r\x1b[K");
        result.extend_from_slice(&self.edit_line);
        result.extend_from_slice(b"\x1b[");
        result.extend_from_slice((self.cursor_position + 1).to_string().as_bytes());
        result.push(b'G');
        result
    }

    pub fn process_new_input(&mut self, c: u8) {
        match self.mode {
            EditorMode::Insert => match c {
                b'\n' => {}
                b'\x1B' => {
                    // escape
                    self.mode = EditorMode::Command;
                }
                b'\x7F' => {
                    // delete
                    if !self.edit_line.is_empty() {
                        if self.cursor_position != 0 {
                            self.edit_line.remove(self.cursor_position - 1);
                            self.cursor_position -= 1;
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
                    if self.cursor_position < self.edit_line.len() {
                        self.edit_line.insert(self.cursor_position, other);
                    } else {
                        self.edit_line.push(other);
                    }
                    self.cursor_position += 1;
                }
                _ => {}
            },
            EditorMode::Command => {
                self.input_buffer.push(c);
                if let Some(command) = Command::try_from_bytes(&self.input_buffer) {
                    self.input_buffer.clear();
                    match command.op {
                        CommandOp::Move(motion) => {
                            self.cursor_position = self.new_cursor_position(motion, command.count);
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
    }
}

impl Default for ViEditor {
    fn default() -> Self {
        Self {
            mode: EditorMode::Insert,
            input_buffer: Vec::new(),
            cursor_position: 0,
            edit_line: Vec::new(),
        }
    }
}
