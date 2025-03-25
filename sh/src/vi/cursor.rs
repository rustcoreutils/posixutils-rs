use crate::vi::word::{current_bigword_end, current_word_end, next_bigword_start, next_word_start};
use crate::vi::CommandParseError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MotionCommand {
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
    GotoLineEnd,
    /// 0
    GotoLineStart,
    /// |
    GotoCharPosition,
    /// fc
    MoveToBeforeFirstChar(u8),
    /// Fc
    MoveToBeforeFirstCharReverse(u8),
    /// tc
    MoveToAfterFirstChar(u8),
    /// Tc
    MoveToAfterFirstCharReverse(u8),
    /// ;
    RepeatLastMoveToChar,
    /// ,
    RepeatLastMoveToCharReverse,
}

impl MotionCommand {
    pub fn parse(bytes: &[u8]) -> Result<Self, CommandParseError> {
        assert!(!bytes.is_empty());
        match bytes[0] {
            b'l' | b' ' => Ok(Self::MoveRight),
            b'h' => Ok(Self::MoveLeft),
            b'w' => Ok(Self::NextWordStart),
            b'W' => Ok(Self::NextBigwordStart),
            b'e' => Ok(Self::CurrentWordEnd),
            b'E' => Ok(Self::CurrentBigwordEnd),
            b'b' => Ok(Self::CurrentWordBegin),
            b'B' => Ok(Self::CurrentBigwordBegin),
            b'^' => Ok(Self::GotoFirstCharOnLine),
            b'$' => Ok(Self::GotoLineEnd),
            b'0' => Ok(Self::GotoLineStart),
            b'|' => Ok(Self::GotoCharPosition),
            b'f' if bytes.len() > 1 => Ok(Self::MoveToBeforeFirstChar(bytes[1])),
            b'f' => Err(CommandParseError::IncompleteCommand),
            b'F' if bytes.len() > 1 => Ok(Self::MoveToBeforeFirstCharReverse(bytes[1])),
            b'F' => Err(CommandParseError::IncompleteCommand),
            b't' if bytes.len() > 1 => Ok(Self::MoveToAfterFirstChar(bytes[1])),
            b't' => Err(CommandParseError::IncompleteCommand),
            b'T' if bytes.len() > 1 => Ok(Self::MoveToAfterFirstCharReverse(bytes[1])),
            b'T' => Err(CommandParseError::IncompleteCommand),
            b';' => Ok(Self::RepeatLastMoveToChar),
            b',' => Ok(Self::RepeatLastMoveToCharReverse),
            _ => Err(CommandParseError::InvalidCommand),
        }
    }
}

fn reverse_goto_char_motion(motion: MotionCommand) -> MotionCommand {
    match motion {
        MotionCommand::MoveToBeforeFirstChar(c) => MotionCommand::MoveToBeforeFirstCharReverse(c),
        MotionCommand::MoveToAfterFirstChar(c) => MotionCommand::MoveToAfterFirstCharReverse(c),
        MotionCommand::MoveToAfterFirstCharReverse(c) => MotionCommand::MoveToBeforeFirstChar(c),
        MotionCommand::MoveToBeforeFirstCharReverse(c) => MotionCommand::MoveToAfterFirstChar(c),
        _ => unreachable!("incorrect motion command"),
    }
}

#[derive(Debug)]
pub enum MotionError {
    BeforeStart,
    AfterEnd,
    CharNotFound,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Cursor {
    pub position: usize,
    last_move_to_char: Option<MotionCommand>,
}

impl Cursor {
    pub fn moved(
        &self,
        line: &[u8],
        motion: MotionCommand,
        count: Option<usize>,
    ) -> Result<Self, MotionError> {
        let mut result = self.clone();
        let line_end = line.len().saturating_sub(1);
        match motion {
            MotionCommand::MoveRight => {
                if self.position == line_end {
                    return Err(MotionError::AfterEnd);
                }
                result.position = line_end.min(self.position + count.unwrap_or(0) + 1);
            }
            MotionCommand::MoveLeft => {
                if self.position == 0 {
                    return Err(MotionError::BeforeStart);
                }
                result.position = self.position.saturating_sub(count.unwrap_or(0) + 1);
            }
            MotionCommand::NextWordStart => {
                if self.position == line_end {
                    return Err(MotionError::AfterEnd);
                }
                result.position = line_end.min(
                    next_word_start(&line[self.position..], count.unwrap_or(0)) + self.position,
                );
            }
            MotionCommand::NextBigwordStart => {
                if self.position == line_end {
                    return Err(MotionError::AfterEnd);
                }
                result.position = line_end.min(
                    next_bigword_start(&line[self.position..], count.unwrap_or(0)) + self.position,
                );
            }
            MotionCommand::CurrentWordEnd => {
                if self.position == line_end {
                    return Err(MotionError::AfterEnd);
                }
                result.position =
                    current_word_end(&line[self.position..], count.unwrap_or(0), false)
                        + self.position
                        - 1;
            }
            MotionCommand::CurrentBigwordEnd => {
                if self.position == line_end {
                    return Err(MotionError::AfterEnd);
                }
                result.position =
                    current_bigword_end(&line[self.position..], count.unwrap_or(0), false)
                        + self.position
                        - 1;
            }
            MotionCommand::CurrentWordBegin => {
                if self.position == 0 {
                    return Err(MotionError::BeforeStart);
                }
                result.position = self.position
                    - current_word_end(&line[..self.position], count.unwrap_or(0), true);
            }
            MotionCommand::CurrentBigwordBegin => {
                if self.position == 0 {
                    return Err(MotionError::BeforeStart);
                }
                result.position = self.position
                    - current_bigword_end(&line[..self.position], count.unwrap_or(0), true);
            }
            MotionCommand::GotoFirstCharOnLine => {
                result.position = line
                    .iter()
                    .position(|c| !c.is_ascii_whitespace())
                    .unwrap_or(0)
            }
            MotionCommand::GotoLineEnd => result.position = line_end,
            MotionCommand::GotoLineStart => result.position = 0,
            MotionCommand::GotoCharPosition => result.position = line_end.min(count.unwrap_or(0)),
            MotionCommand::MoveToBeforeFirstChar(char) => {
                if self.position == line_end {
                    return Err(MotionError::AfterEnd);
                }
                result.last_move_to_char = Some(motion);
                result.position = line[self.position + 1..]
                    .iter()
                    .enumerate()
                    .filter(|(_, c)| **c == char)
                    .nth(count.unwrap_or(0))
                    .map(|(p, _)| p + self.position + 1)
                    .ok_or(MotionError::CharNotFound)?;
            }
            MotionCommand::MoveToBeforeFirstCharReverse(char) => {
                if self.position == 0 {
                    return Err(MotionError::BeforeStart);
                }
                result.last_move_to_char = Some(motion);
                result.position = line[..self.position]
                    .iter()
                    .rev()
                    .enumerate()
                    .filter(|(_, c)| **c == char)
                    .nth(count.unwrap_or(0))
                    .map(|(p, _)| self.position - p - 1)
                    .ok_or(MotionError::CharNotFound)?;
            }
            MotionCommand::MoveToAfterFirstChar(char) => {
                if self.position == line_end {
                    return Err(MotionError::AfterEnd);
                }
                result.last_move_to_char = Some(motion);
                result.position = line[self.position + 1..]
                    .iter()
                    .enumerate()
                    .filter(|(_, c)| **c == char)
                    .nth(count.unwrap_or(0))
                    .map(|(p, _)| p + self.position)
                    .ok_or(MotionError::CharNotFound)?;
            }
            MotionCommand::MoveToAfterFirstCharReverse(char) => {
                if self.position == 0 {
                    return Err(MotionError::BeforeStart);
                }
                result.last_move_to_char = Some(motion);
                result.position = line[..self.position]
                    .iter()
                    .rev()
                    .enumerate()
                    .filter(|(_, c)| **c == char)
                    .nth(count.unwrap_or(0))
                    .map(|(p, _)| self.position - p)
                    .ok_or(MotionError::CharNotFound)?;
            }
            MotionCommand::RepeatLastMoveToChar => {
                if let Some(last) = self.last_move_to_char {
                    return self.moved(line, last, count);
                }
            }
            MotionCommand::RepeatLastMoveToCharReverse => {
                if let Some(last) = self.last_move_to_char {
                    let motion = self.moved(&line, reverse_goto_char_motion(last), count)?;
                    // don't return the reversed motion
                    result.position = motion.position;
                }
            }
        }
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cursor_at_position(position: usize) -> Cursor {
        Cursor {
            position,
            last_move_to_char: None,
        }
    }

    #[test]
    fn test_basic_motions() {
        let line = b"hello world";

        let cursor = cursor_at_position(0);
        let moved = cursor.moved(line, MotionCommand::MoveRight, None).unwrap();
        assert_eq!(moved, cursor_at_position(1));

        let cursor = cursor_at_position(0);
        let moved = cursor
            .moved(line, MotionCommand::MoveRight, Some(3))
            .unwrap();
        assert_eq!(moved, cursor_at_position(4));

        let cursor = cursor_at_position(line.len() - 1);
        assert!(cursor.moved(line, MotionCommand::MoveRight, None).is_err());

        let cursor = cursor_at_position(5);
        let moved = cursor.moved(line, MotionCommand::MoveLeft, None).unwrap();
        assert_eq!(moved, cursor_at_position(4));

        let cursor = cursor_at_position(5);
        let moved = cursor
            .moved(line, MotionCommand::MoveLeft, Some(3))
            .unwrap();
        assert_eq!(moved, cursor_at_position(1));

        let cursor = cursor_at_position(0);
        assert!(cursor.moved(line, MotionCommand::MoveLeft, None).is_err());
    }

    #[test]
    fn test_goto_motions() {
        let line = b"  hello world";

        let cursor = cursor_at_position(5);
        let moved = cursor
            .moved(line, MotionCommand::GotoFirstCharOnLine, None)
            .unwrap();
        assert_eq!(moved, cursor_at_position(2));

        let cursor = cursor_at_position(0);
        let moved = cursor
            .moved(line, MotionCommand::GotoLineEnd, None)
            .unwrap();
        assert_eq!(moved, cursor_at_position(line.len() - 1));

        let cursor = cursor_at_position(5);
        let moved = cursor
            .moved(line, MotionCommand::GotoLineStart, None)
            .unwrap();
        assert_eq!(moved, cursor_at_position(0));

        let cursor = cursor_at_position(0);
        let moved = cursor
            .moved(line, MotionCommand::GotoCharPosition, Some(7))
            .unwrap();
        assert_eq!(moved, cursor_at_position(7));

        let cursor = cursor_at_position(0);
        let moved = cursor
            .moved(line, MotionCommand::GotoCharPosition, Some(100))
            .unwrap();
        assert_eq!(moved, cursor_at_position(line.len() - 1));
    }

    #[test]
    fn test_word_motions() {
        let line = b"word1 word2 big-word3 end";

        let cursor = cursor_at_position(0);
        let moved = cursor
            .moved(line, MotionCommand::NextWordStart, None)
            .unwrap();
        assert_eq!(moved, cursor_at_position(6));

        let cursor = cursor_at_position(0);
        let moved = cursor
            .moved(line, MotionCommand::NextWordStart, Some(2))
            .unwrap();
        assert_eq!(moved, cursor_at_position(12));

        let cursor = cursor_at_position(0);
        let moved = cursor
            .moved(line, MotionCommand::NextBigwordStart, None)
            .unwrap();
        assert_eq!(moved, cursor_at_position(6));

        let cursor = cursor_at_position(0);
        let moved = cursor
            .moved(line, MotionCommand::CurrentWordEnd, None)
            .unwrap();
        assert_eq!(moved, cursor_at_position(4));

        let cursor = cursor_at_position(12);
        let moved = cursor
            .moved(line, MotionCommand::CurrentBigwordEnd, None)
            .unwrap();
        assert_eq!(moved, cursor_at_position(20));

        let cursor = cursor_at_position(10);
        let moved = cursor
            .moved(line, MotionCommand::CurrentWordBegin, None)
            .unwrap();
        assert_eq!(moved, cursor_at_position(6));

        let cursor = cursor_at_position(18);
        let moved = cursor
            .moved(line, MotionCommand::CurrentBigwordBegin, None)
            .unwrap();
        assert_eq!(moved, cursor_at_position(12));

        let cursor = cursor_at_position(23);
        let moved = cursor
            .moved(line, MotionCommand::CurrentBigwordEnd, None)
            .unwrap();
        assert_eq!(moved, cursor_at_position(24));
    }

    #[test]
    fn test_char_find_motions() {
        let line = b"find the letter x and y and z";

        let cursor = cursor_at_position(0);
        let moved = cursor
            .moved(line, MotionCommand::MoveToBeforeFirstChar(b'x'), None)
            .unwrap();
        assert_eq!(moved.position, 16);

        let cursor = cursor_at_position(20);
        let moved = cursor
            .moved(
                line,
                MotionCommand::MoveToBeforeFirstCharReverse(b't'),
                None,
            )
            .unwrap();
        assert_eq!(moved.position, 12);

        let cursor = cursor_at_position(0);
        let moved = cursor
            .moved(line, MotionCommand::MoveToAfterFirstChar(b'x'), None)
            .unwrap();
        assert_eq!(moved.position, 15);

        let cursor = cursor_at_position(20);
        let moved = cursor
            .moved(line, MotionCommand::MoveToAfterFirstCharReverse(b't'), None)
            .unwrap();
        assert_eq!(moved.position, 13);

        let cursor = cursor_at_position(0);
        assert!(cursor
            .moved(line, MotionCommand::MoveToBeforeFirstChar(b'Q'), None)
            .is_err());
    }

    #[test]
    fn test_repeat_char_find_motions() {
        let line = b"test repeating char finds";

        let mut cursor = cursor_at_position(0);
        cursor = cursor
            .moved(line, MotionCommand::MoveToBeforeFirstChar(b'a'), None)
            .unwrap();
        assert_eq!(cursor.position, 9);

        let moved = cursor
            .moved(line, MotionCommand::RepeatLastMoveToChar, None)
            .unwrap();
        assert_eq!(moved.position, 17);

        let mut cursor = cursor_at_position(11);
        cursor = cursor
            .moved(line, MotionCommand::MoveToBeforeFirstChar(b'a'), None)
            .unwrap();
        assert_eq!(moved.position, 17);

        let moved = cursor
            .moved(line, MotionCommand::RepeatLastMoveToCharReverse, None)
            .unwrap();
        assert_eq!(moved.position, 9);
    }
}
