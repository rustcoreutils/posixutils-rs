mod cursor;
mod word;

use crate::cli::vi::cursor::{Cursor, MotionCommand, MotionError};
use crate::cli::vi::word::{current_bigword, BigWordIter};
use crate::parse::word_parser::parse_word;
use crate::shell::Shell;
use crate::wordexp::expand_word;
use crate::wordexp::pathname::glob;
use crate::wordexp::pattern::FilenamePattern;
use std::borrow::Cow;
use std::ffi::OsString;
use std::path::Path;

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

fn into_expansion_word(word: &[u8]) -> Result<Cow<str>, CommandError> {
    let word = std::str::from_utf8(word).map_err(|_| CommandError)?;
    if word.chars().any(|c| c == '?' || c == '*' || c == '[') {
        Ok(Cow::Borrowed(word))
    } else {
        Ok(Cow::Owned(format!("{word}*")))
    }
}

fn add_terminating_slash_if_directory(mut s: String) -> String {
    if Path::new(&s).is_dir() {
        s.push('/');
    }
    s
}

fn select_longest_pathname_with_longest_common_prefix(pathnames: Vec<OsString>) -> String {
    assert!(!pathnames.is_empty());
    let mut longest = pathnames[0].to_string_lossy().into_owned();
    for pathname in pathnames.iter().skip(1) {
        let pathname = pathname.to_string_lossy();
        while !pathname.starts_with(&longest) {
            longest.pop();
        }
    }
    add_terminating_slash_if_directory(longest)
}

pub struct ViEditor {
    input_buffer: Vec<u8>,
    edit_line: Vec<u8>,
    edit_line_cursor: Cursor,
    history_line_cursor: Cursor,
    mode: EditorMode,
    most_recent_nonmotion_command: Option<Command>,
    save_buffer: Vec<u8>,
    current_command_in_history: usize,
}

impl ViEditor {
    fn edit_current_line(&mut self, shell: &Shell) -> &mut Vec<u8> {
        if self.current_command_in_history != 0 {
            self.edit_line = shell
                .history
                .get_reverse(self.current_command_in_history - 1)
                .unwrap()
                .as_bytes()
                .to_vec();
            self.current_command_in_history = 0;
            self.edit_line_cursor.position = 0;
        }
        &mut self.edit_line
    }

    fn move_current_cursor(
        &mut self,
        shell: &Shell,
        motion: MotionCommand,
        count: Option<usize>,
    ) -> Result<(), CommandError> {
        if self.current_command_in_history != 0 {
            self.history_line_cursor = self
                .history_line_cursor
                .moved(&self.current_line(shell), motion, count)
                .map_err(|_| CommandError)?;
        } else {
            self.edit_line_cursor = self
                .edit_line_cursor
                .moved(&self.edit_line, motion, count)
                .map_err(|_| CommandError)?;
        }
        Ok(())
    }

    fn execute_command(
        &mut self,
        command: Command,
        shell: &mut Shell,
    ) -> Result<Action, CommandError> {
        if let CommandOp::Move(motion) = command.op {
            self.move_current_cursor(shell, motion, command.count)?;
            return Ok(Action::None);
        }
        self.most_recent_nonmotion_command = Some(command.clone());
        match command.op {
            CommandOp::Execute => {
                let mut result = Vec::new();
                std::mem::swap(&mut result, &mut self.edit_line);
                result.push(b'\n');
                self.mode = EditorMode::Insert;
                self.edit_line_cursor.position = 0;
                return Ok(Action::Execute(result));
            }
            CommandOp::Redraw => return Ok(Action::Redraw),
            CommandOp::CommentLine => {
                self.edit_current_line(shell).insert(0, b'#');
            }
            CommandOp::DisplayExpansions => {
                let current_line = self.current_line(shell);
                if let Some(word_range) =
                    current_bigword(current_line, self.edit_line_cursor.position)
                {
                    let word =
                        into_expansion_word(&current_line[word_range.start..word_range.end])?;
                    let parsed_word = parse_word(&word, 0, false).map_err(|_| CommandError)?;
                    let expansions =
                        expand_word(&parsed_word, false, shell).map_err(|_| CommandError)?;
                    if expansions.is_empty() {
                        return Err(CommandError);
                    }
                    print!("\n");
                    for (i, e) in expansions.into_iter().enumerate() {
                        println!("{i}) {}", add_terminating_slash_if_directory(e));
                    }
                }
            }
            CommandOp::ExpandUnique => {
                self.edit_current_line(shell);
                if let Some(word_range) =
                    current_bigword(&self.edit_line, self.edit_line_cursor.position)
                {
                    let word =
                        into_expansion_word(&self.edit_line[word_range.start..word_range.end])?;
                    let pattern =
                        FilenamePattern::try_from(word.into_owned()).map_err(|_| CommandError)?;
                    let expansions = glob(&pattern, Path::new(&shell.current_directory));
                    if expansions.is_empty() {
                        return Err(CommandError);
                    }
                    let replacement =
                        select_longest_pathname_with_longest_common_prefix(expansions);
                    self.edit_line.splice(
                        word_range.start..word_range.end,
                        replacement.as_bytes().iter().copied(),
                    );
                    self.edit_line_cursor.position = word_range.start + replacement.len();
                    self.mode = EditorMode::Insert;
                }
            }
            CommandOp::ExpandAll => {
                self.edit_current_line(shell);
                if let Some(word_range) =
                    current_bigword(&self.edit_line, self.edit_line_cursor.position)
                {
                    let word =
                        into_expansion_word(&self.edit_line[word_range.start..word_range.end])?;
                    let pattern =
                        FilenamePattern::try_from(word.into_owned()).map_err(|_| CommandError)?;
                    let expansions = glob(&pattern, Path::new(&shell.current_directory));
                    if expansions.is_empty() {
                        return Err(CommandError);
                    }
                    let replacement = expansions
                        .iter()
                        .map(|e| {
                            add_terminating_slash_if_directory(e.to_string_lossy().into_owned())
                        })
                        .collect::<Vec<_>>()
                        .join(" ");
                    self.edit_line.splice(
                        word_range.start..word_range.end,
                        replacement.as_bytes().iter().copied(),
                    );
                    self.edit_line_cursor.position = word_range.start + replacement.len();
                    self.mode = EditorMode::Insert;
                }
            }
            CommandOp::Alias(_) => {
                // I don't know what they mean by alias since it's not
                // a shell alias, and no other types of alias are mentioned
            }
            CommandOp::ChangeCase => {
                self.edit_current_line(shell);
                let count = self.edit_line.len().min(command.count.unwrap_or(0) + 1);
                for i in 0..count {
                    let c = &mut self.edit_line[self.edit_line_cursor.position + i];
                    if c.is_ascii_uppercase() {
                        *c = c.to_ascii_lowercase();
                    } else if c.is_ascii_lowercase() {
                        *c = c.to_ascii_uppercase();
                    }
                }
            }
            CommandOp::RepeatLast => {
                if let Some(last_command) = &self.most_recent_nonmotion_command {
                    return self.execute_command(last_command.clone(), shell);
                }
            }
            CommandOp::EditCommand => {}
            CommandOp::InsertAtNextChar => {
                self.edit_line_cursor.position += 1;
                self.mode = EditorMode::Insert;
                self.edit_current_line(shell);
            }
            CommandOp::InsertAtLineEnd => {
                self.edit_line_cursor.position = self.edit_line.len();
                self.mode = EditorMode::Insert;
                self.edit_current_line(shell);
            }
            CommandOp::InsertAtCurrentPosition => {
                self.mode = EditorMode::Insert;
                self.edit_current_line(shell);
            }
            CommandOp::InsertAtLineStart => {
                self.edit_line_cursor.position = 0;
                self.mode = EditorMode::Insert;
                self.edit_current_line(shell);
            }
            CommandOp::ReplaceMode => {
                self.mode = EditorMode::Replace;
                self.edit_current_line(shell);
            }
            CommandOp::DeleteRange(motion) => {
                self.edit_current_line(shell);
                match self
                    .edit_line_cursor
                    .moved(&self.edit_line, motion, command.count)
                {
                    Ok(range_end) => {
                        let range_end = range_end.position;
                        if self.edit_line_cursor.position < range_end {
                            self.edit_line
                                .drain(self.edit_line_cursor.position..range_end);
                        } else {
                            self.edit_line
                                .drain(range_end..self.edit_line_cursor.position);
                            self.edit_line_cursor.position = range_end;
                        }
                    }
                    Err(MotionError::AfterEnd) => {
                        self.edit_line.truncate(self.edit_line_cursor.position);
                    }
                    Err(_) => {
                        return Err(CommandError);
                    }
                }
                self.mode = EditorMode::Insert;
            }
            CommandOp::ClearLine => {
                self.edit_current_line(shell).clear();
                self.edit_line_cursor.position = 0;
                self.mode = EditorMode::Insert;
            }
            CommandOp::DeleteToEnd => {
                self.edit_current_line(shell);
                self.edit_line.truncate(self.edit_line_cursor.position);
                self.edit_line_cursor.position = self.edit_line.len();
                self.mode = EditorMode::Insert;
            }
            CommandOp::ClearEditLine => {
                self.edit_current_line(shell).clear();
                self.edit_line_cursor.position = 0;
                self.mode = EditorMode::Insert;
            }
            CommandOp::ReplaceWith(c) => {
                self.edit_current_line(shell);
                let count = self.edit_line.len().min(command.count.unwrap_or(0) + 1);
                for i in 0..count {
                    self.edit_line[self.edit_line_cursor.position + i] = c;
                }
            }
            CommandOp::AppendLastBigWord => {
                self.edit_current_line(shell);
                let last_command = if let Some(cmd) = shell.history.get_reverse(0) {
                    cmd
                } else {
                    return Err(CommandError);
                };
                let mut words = BigWordIter::new(last_command.as_bytes().iter().copied());
                let word_range = if let Some(count) = command.count {
                    words.nth(count)
                } else {
                    words.last()
                };
                if let Some(word_range) = word_range {
                    let word = last_command[word_range.start..word_range.end].as_bytes();
                    // we are in command mode, so cursor is always less than self.edit_line.len()
                    let position = self.edit_line_cursor.position + 1;
                    self.edit_line
                        .splice(position..position, word.iter().copied());
                    self.edit_line.insert(position, b' ');
                    self.edit_line_cursor.position += word_range.end - word_range.start;
                    self.mode = EditorMode::Insert;
                } else {
                    return Err(CommandError);
                }
            }
            CommandOp::CutCurrentChars => {
                self.edit_current_line(shell);
                let end = self
                    .edit_line
                    .len()
                    .min(self.edit_line_cursor.position + command.count.unwrap_or(0) + 1);
                self.save_buffer = self
                    .edit_line
                    .drain(self.edit_line_cursor.position..end)
                    .collect();
                if self.edit_line_cursor.position != 0
                    && self.edit_line_cursor.position == self.edit_line.len()
                {
                    self.edit_line_cursor.position -= 1;
                }
            }
            CommandOp::CutPreviousChars => {
                self.edit_current_line(shell);
                if self.edit_line_cursor.position == 0 {
                    return Err(CommandError);
                }
                let end = self
                    .edit_line_cursor
                    .position
                    .saturating_sub(command.count.unwrap_or(0) + 1);
                self.save_buffer = self
                    .edit_line
                    .drain(end..self.edit_line_cursor.position)
                    .collect();
                self.edit_line_cursor.position = end;
            }
            CommandOp::CutRange(motion) => {
                self.edit_current_line(shell);
                match self
                    .edit_line_cursor
                    .moved(&self.edit_line, motion, command.count)
                {
                    Ok(range_end) => {
                        let range_end = range_end.position;
                        if self.edit_line_cursor.position < range_end {
                            self.save_buffer = self
                                .edit_line
                                .drain(self.edit_line_cursor.position..range_end)
                                .collect();
                        } else {
                            self.save_buffer = self
                                .edit_line
                                .drain(range_end..self.edit_line_cursor.position)
                                .collect();
                            self.edit_line_cursor.position = range_end;
                        }
                    }
                    Err(MotionError::AfterEnd) => {
                        self.save_buffer = self
                            .edit_line
                            .drain(self.edit_line_cursor.position..)
                            .collect();
                    }
                    Err(_) => {
                        return Err(CommandError);
                    }
                }
            }
            CommandOp::CutLine => {
                self.edit_current_line(shell);
                self.save_buffer = self.edit_line.drain(..).collect();
                self.edit_line_cursor.position = 0;
            }
            CommandOp::CutToEnd => {
                self.edit_current_line(shell);
                self.save_buffer = self
                    .edit_line
                    .drain(self.edit_line_cursor.position..)
                    .collect();
            }
            CommandOp::YankRange(motion) => {
                match self
                    .edit_line_cursor
                    .moved(&self.current_line(shell), motion, command.count)
                {
                    Ok(range_end) => {
                        let range_end = range_end.position;
                        if self.edit_line_cursor.position < range_end {
                            self.save_buffer = self.current_line(shell)
                                [self.edit_line_cursor.position..range_end]
                                .to_vec();
                        } else {
                            self.save_buffer = self.current_line(shell)
                                [range_end..self.edit_line_cursor.position]
                                .to_vec();
                        }
                    }
                    Err(MotionError::AfterEnd) => {
                        self.save_buffer =
                            self.current_line(shell)[self.edit_line_cursor.position..].to_vec();
                    }
                    Err(_) => {
                        return Err(CommandError);
                    }
                }
            }
            CommandOp::YankLine => {
                self.save_buffer = self.current_line(shell).to_vec();
            }
            CommandOp::YankToEnd => {
                self.save_buffer =
                    self.current_line(shell)[self.edit_line_cursor.position..].to_vec();
            }
            CommandOp::PasteAfter => {
                self.edit_current_line(shell);
                if !self.save_buffer.is_empty() {
                    self.edit_line.splice(
                        self.edit_line_cursor.position..self.edit_line_cursor.position,
                        self.save_buffer.iter().copied(),
                    );
                    self.edit_line_cursor.position += 1;
                }
            }
            CommandOp::PasteBefore => {
                self.edit_current_line(shell);
                if !self.save_buffer.is_empty() {
                    self.edit_line.splice(
                        self.edit_line_cursor.position..self.edit_line_cursor.position,
                        self.save_buffer.iter().copied(),
                    );
                }
            }
            CommandOp::UndoLastCommand => {}
            CommandOp::UndoAll => {}
            CommandOp::PreviousShellCommand => {
                let number = command.count.unwrap_or(1);
                if self.current_command_in_history + number > shell.history.entries_count() {
                    return Err(CommandError);
                }
                self.history_line_cursor = Cursor::default();
                self.current_command_in_history += number;
            }
            CommandOp::NextShellCommand => {
                let number = command.count.unwrap_or(1);
                if number > self.current_command_in_history {
                    self.current_command_in_history = 0;
                    return Err(CommandError);
                }
                self.history_line_cursor = Cursor::default();
                self.current_command_in_history -= number;
            }
            CommandOp::OldestShellCommand => {}
            CommandOp::SearchPattern(_) => {}
            CommandOp::SearchPatternAfter(_) => {}
            CommandOp::RepeatLastSearch => {}
            CommandOp::RepeatLastSearchReverse => {}
            CommandOp::Move(_) => unreachable!(),
        }
        Ok(Action::None)
    }

    pub fn current_line<'a>(&'a self, shell: &'a Shell) -> &'a [u8] {
        if self.current_command_in_history == 0 {
            &self.edit_line
        } else {
            shell
                .history
                .get_reverse(self.current_command_in_history - 1)
                .unwrap()
                .as_bytes()
        }
    }

    pub fn cursor_position(&self) -> usize {
        if self.current_command_in_history == 0 {
            self.edit_line_cursor.position
        } else {
            self.history_line_cursor.position
        }
    }

    pub fn process_new_input(&mut self, c: u8, shell: &mut Shell) -> Result<Action, CommandError> {
        match self.mode {
            EditorMode::Insert | EditorMode::Replace => {
                match c {
                    b'\n' => {
                        let mut result = Vec::new();
                        std::mem::swap(&mut result, &mut self.edit_line);
                        result.push(b'\n');
                        self.edit_line_cursor.position = 0;
                        return Ok(Action::Execute(result));
                    }
                    b'\x1B' => {
                        // escape
                        self.mode = EditorMode::Command;
                        self.edit_line_cursor.position = self
                            .edit_line_cursor
                            .position
                            .min(self.edit_line.len().saturating_sub(1));
                    }
                    b'\x7F' => {
                        // delete
                        if !self.edit_line.is_empty() && self.edit_line_cursor.position != 0 {
                            self.edit_line.remove(self.edit_line_cursor.position - 1);
                            self.edit_line_cursor.position -= 1;
                        }
                    }
                    b'\x04' => return Ok(Action::Eof),
                    b'\x16' => {
                        // ^V
                    }
                    b'\x17' => {}
                    other if !other.is_ascii_control() => {
                        if self.edit_line_cursor.position < self.edit_line.len() {
                            if self.mode == EditorMode::Replace {
                                self.edit_line[self.edit_line_cursor.position] = other;
                            } else {
                                self.edit_line.insert(self.edit_line_cursor.position, other);
                            }
                        } else {
                            self.edit_line.push(other);
                        }
                        self.edit_line_cursor.position += 1;
                    }
                    _ => {}
                }
            }
            EditorMode::Command => {
                self.input_buffer.push(c);
                match Command::parse(&self.input_buffer) {
                    Ok(command) => {
                        self.input_buffer.clear();
                        return self.execute_command(command, shell);
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
            edit_line: Vec::new(),
            edit_line_cursor: Cursor::default(),
            history_line_cursor: Cursor::default(),
            most_recent_nonmotion_command: None,
            save_buffer: Vec::new(),
            current_command_in_history: 0,
        }
    }
}
