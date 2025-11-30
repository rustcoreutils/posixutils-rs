//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use libc::{getegid, getgid, getuid, setgid, setuid};
use plib::regex::{Regex, RegexFlags};
use std::collections::HashMap;
use std::fs::File;
use std::io::{stdout, BufRead, BufReader, Cursor, Read, Seek, SeekFrom, Write};
use std::ops::{Not, Range};
use std::os::fd::AsRawFd;
use std::path::PathBuf;
use std::process::{exit, ExitStatus};
use std::str::FromStr;
use std::sync::mpsc::{channel, Receiver, TryRecvError};
use std::sync::Mutex;
use std::time::Duration;
use termion::{clear::*, cursor::*, event::*, input::*, raw::*, screen::*, style::*, *};

const LINES_PER_PAGE: u16 = 24;
const NUM_COLUMNS: u16 = 80;
const DEFAULT_EDITOR: &str = "vi";
const CONVERT_STRING_BUF_SIZE: usize = 64;
const PROJECT_NAME: &str = "posixutils-rs";

/// Last acceptable pressed mouse button
static LAST_MOUSE_BUTTON: Mutex<Option<MouseButton>> = Mutex::new(None);
/// Inform terminal input handler thread that program is closing
static NEED_QUIT: Mutex<bool> = Mutex::new(false);

/// more - display files on a page-by-page basis.
#[derive(Parser)]
#[command(version, about = gettext("more - display files on a page-by-page basis"))]
struct Args {
    /// Do not scroll, display text and clean line ends
    #[arg(
        short = 'c',
        long = "print-over",
        help = gettext("Do not scroll, display text and clean line ends")
    )]
    print_over: bool,

    /// Exit on end-of-file
    #[arg(
        short = 'e',
        long = "exit-on-eof",
        help = gettext("Exit on end-of-file")
    )]
    exit_on_eof: bool,

    /// Perform pattern matching in searches without regard to case
    #[arg(
        short = 'i',
        long = "ignore-case",
        help = gettext("Perform pattern matching in searches without regard to case")
    )]
    case_insensitive: bool,

    /// Execute the more command(s) in the command arguments in the order specified
    #[arg(
        short = 'p',
        long = "execute",
        help = gettext("Execute the more command(s) in the command arguments in the order specified")
    )]
    commands: Option<String>,

    /// Squeeze multiple blank lines into one
    #[arg(
        short = 's',
        long = "squeeze",
        help = gettext("Squeeze multiple blank lines into one")
    )]
    squeeze: bool,

    /// Write the screenful of the file containing the tag named by the tagstring argument
    #[arg(
        short = 't',
        long = "tag",
        help = gettext("Write the screenful of the file containing the tag named by the tagstring argument")
    )]
    tag: Option<String>,

    /// Suppress underlining and bold
    #[arg(
        short = 'u',
        long = "plain",
        help = gettext("Suppress underlining and bold")
    )]
    plain: bool,

    /// The number of lines per screenful
    #[arg(
        short = 'n',
        long = "lines",
        help = gettext("The number of lines per screenful")
    )]
    lines: Option<u16>,

    /// Enable interactive session test
    #[arg(
        short = 'd',
        long = "test",
        help = gettext("Enable interactive session test")
    )]
    test: bool,

    /// A pathnames of an input files
    #[arg(
        name = "FILES",
        help = gettext("A pathnames of input files")
    )]
    input_files: Vec<String>,
}

/// Commands that can be executed in interactive mode after appropriate patterns input
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
enum Command {
    /// If [`parse`] can`t recognise patterns in cmd str then it returns this
    Unknown,
    /// Write a summary of implementation-defined commands
    Help,
    /// Scroll forward count lines, with one default screenful
    ScrollForwardOneScreenful(Option<usize>),
    /// Scroll backward count lines, with one default screenful
    ScrollBackwardOneScreenful(Option<usize>),
    /// Scroll forward count lines. Default is one screenful
    ScrollForwardOneLine {
        count: Option<usize>,
        /// Selects a default count relative to an existing <space> input
        is_space: bool,
    },
    /// Scroll backward count lines. The entire count lines shall be written
    ScrollBackwardOneLine(Option<usize>),
    /// Scroll forward count lines. Default is one half of the screen size
    ScrollForwardOneHalfScreenful(Option<usize>),
    /// Display beginning lines count screenful after current screen last line
    SkipForwardOneLine(Option<usize>),
    /// Scroll backward count lines. Default is one half of the screen size
    ScrollBackwardOneHalfScreenful(Option<usize>),
    /// Display the screenful beginning with line count
    GoToBeginningOfFile(Option<usize>),
    /// If count is specified display beginning lines or last of file screenful
    GoToEOF(Option<usize>),
    /// Refresh the screen
    RefreshScreen,
    /// Refresh the screen, discarding any buffered input
    DiscardAndRefresh,
    /// Mark the current position with the letter - one lowercase letter
    MarkPosition(char),
    /// Return to the position that was marked, making it as current position
    ReturnMark(char),
    /// Return to the position from which the last large movement command was executed
    ReturnPreviousPosition,
    /// Display the screenful beginning with the countth line containing the pattern
    SearchForwardPattern {
        count: Option<usize>,
        /// Inverse pattern
        is_not: bool,
        pattern: String,
    },
    /// Display the screenful beginning with the countth previous line containing the pattern
    SearchBackwardPattern {
        count: Option<usize>,
        /// Inverse pattern
        is_not: bool,
        pattern: String,
    },
    /// Repeat the previous search for countth line containing the last pattern
    RepeatSearch(Option<usize>),
    /// Repeat the previous search oppositely for the countth line containing the last pattern
    RepeatSearchReverse(Option<usize>),
    /// Examine a new file. Default [filename] (current file) shall be re-examined
    ExamineNewFile(String),
    /// Examine the next file. If count is specified, the countth next file shall be examined
    ExamineNextFile(Option<usize>),
    /// Examine the previous file. If count is specified, the countth next file shall be examined
    ExaminePreviousFile(Option<usize>),
    /// If tagstring isn't the current file, examine the file, as if :e command was executed.
    /// Display beginning screenful with the tag
    GoToTag(String),
    /// Invoke an editor to edit the current file being examined. Editor shall be taken
    /// from EDITOR, or shall default to vi.
    InvokeEditor,
    /// Write a message for which the information references the first byte of the line
    /// after the last line of the file on the screen
    DisplayPosition,
    /// Exit more
    Quit,
}

/// All more errors
#[derive(Debug, Clone, thiserror::Error)]
enum MoreError {
    /// Errors raised in [`SeekPositions`] level
    #[error("{}", .0)]
    SeekPositions(#[from] SeekPositionsError),
    /// Errors raised in [`SourceContext`] level
    #[error("{}", .0)]
    SourceContext(#[from] SourceContextError),
    /// Attempt set [`String`] on [`Terminal`] that goes beyond
    #[error("Set chars outside screen is forbidden")]
    SetOutside,
    /// Attempt set [`Prompt`] on [`Terminal`] longer that [`Terminal`] width
    #[error("Input too long")]
    InputTooLong,
    /// Read [`std::io::Stdin`] is failed
    #[error("Couldn't read from stdin")]
    InputRead,
    /// Calling [`std::process::Command`] for editor is failed
    #[error("Editor process failed")]
    EditorFailed,
    /// Calling [`std::process::Command`] for ctags is failed
    #[error("Couldn't call ctags")]
    CTagsFailed,
    /// Open, read [`File`] is failed
    #[error("Couldn't read file \'{}\'", .0)]
    FileRead(String),
    /// [`Output`], [`Regex`] parse errors
    #[error("Couldn't parse {}", .0)]
    StringParse(String),
    /// Attempt execute [`Command::UnknownCommand`]
    #[error("Couldn't execute unknown command")]
    UnknownCommand,
    /// [`Terminal`] init is failed
    #[error("Terminal isn't initialized")]
    TerminalInit,
    /// [`Terminal`] size is too small
    #[error("Can't execute commands for too small terminal")]
    TerminalOutput,
    /// [`Terminal`] size read is failed
    #[error("Couldn't get current terminal size")]
    SizeRead,
    /// Attempt update [`SourceContext::current_screen`] without [`Terminal`]
    #[error("Terminal operations is forbidden")]
    MissingTerminal,
    /// Search has no results
    #[error("Couldn't find \'{}\' pattern", .0)]
    PatternNotFound(String),
}

/// All [`SeekPositions`] errors
#[derive(Debug, Clone, thiserror::Error)]
enum SeekPositionsError {
    /// [`Output`], [`Regex`] parse errors
    #[error("Couldn't parse {}", .0)]
    StringParse(String),
    /// Attempt seek buffer out of bounds
    #[error("Couldn't seek to {} position", .0)]
    OutOfRange(u64),
    /// Source open, read errors
    #[error("Couldn't read {}", .0)]
    FileRead(String),
}

/// All [`SourceContext`] errors
#[derive(Debug, Clone, thiserror::Error)]
enum SourceContextError {
    /// Attempt execute previous search when it is [`None`]
    #[error("No previous regular expression")]
    MissingLastSearch,
    /// Attempt move current position to mark when it isn`t set
    #[error("Couldn't find mark for \'{}", .0)]
    MissingMark(char),
}

/// Sets display style for every [`Screen`] char on [`Terminal`]
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
enum StyleType {
    /// Default style
    None,
    /// Underlined text
    Underscore,
    /// Black text, white background
    Negative,
}

/// Buffer that stores content that must be displayed on [`Terminal`]
#[derive(Debug, Clone)]
struct Screen(Vec<Vec<(char, StyleType)>>);

impl Screen {
    /// Creates new [`Screen`]
    fn new(size: (usize, usize)) -> Self {
        let row = [(' ', StyleType::None)];
        let row = row.repeat(size.1);
        let mut matrix = vec![row.clone()];
        let lines_count = size.0.max(2) - 1;
        for _ in 0..lines_count {
            matrix.push(row.clone())
        }
        Self(matrix)
    }

    /// Sets string range on [`Screen`]
    fn set_str(
        &mut self,
        position: (usize, usize),
        string: String,
        style: StyleType,
    ) -> Result<(), MoreError> {
        if position.0 >= self.0.len()
            || (self.0[0].len() as isize - position.1 as isize) < string.len() as isize
        {
            return Err(MoreError::SetOutside);
        }

        let mut chars = string.chars();
        self.0[position.0]
            .iter_mut()
            .skip(position.1)
            .for_each(|(c, st)| {
                if let Some(ch) = chars.next() {
                    *c = ch;
                    *st = style;
                }
            });

        Ok(())
    }

    /// Set string ([`Vec<(char, StyleType)>`]) range on [`Screen`]
    fn set_raw(
        &mut self,
        position: (usize, usize),
        string: Vec<(char, StyleType)>,
    ) -> Result<(), MoreError> {
        if position.0 > self.0.len()
            || (self.0[0].len() as isize - position.1 as isize) < string.len() as isize
        {
            return Err(MoreError::SetOutside);
        }

        let mut chars = string.iter();
        self.0[position.0]
            .iter_mut()
            .skip(position.1)
            .for_each(|c| {
                if let Some(ch) = chars.next() {
                    *c = *ch;
                }
            });

        Ok(())
    }

    /// Fill [`Screen`] with (' ', [StyleType::None])
    fn clear(&mut self) {
        self.0.iter_mut().for_each(|line| {
            line.fill((' ', StyleType::None));
        });
    }
}

/// Defines search, scroll direction
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
enum Direction {
    /// Direction to bigger position
    Forward,
    /// Direction to smaller position
    Backward,
}

impl Not for Direction {
    type Output = Direction;

    fn not(self) -> Self::Output {
        match self {
            Direction::Forward => Direction::Backward,
            Direction::Backward => Direction::Forward,
        }
    }
}

/// Defines set of methods that can be used for any [`Source`]'s.
/// Used for storage and processing of any [`Source`] type in
/// [`SeekPositions`]
trait SeekRead: Seek + Read {}

impl<T: Seek + Read> SeekRead for Box<T> {}
impl SeekRead for File {}
impl SeekRead for Cursor<String> {}

/// Universal cursor that can read lines, seek
/// over any [`SeekRead`] source
struct SeekPositions {
    /// Buffer with previous seek positions of all lines beginnings
    positions: Vec<u64>,
    /// Terminal width for spliting long lines. If [`None`], lines not splited by length
    line_len: Option<usize>,
    /// Stream size
    stream_len: u64,
    /// Count of all lines in source
    lines_count: usize,
    /// Source that handles info for creating [`SeekRead`] buffer
    source: Source,
    /// Buffer for which is seek and read is applied  
    buffer: Box<dyn SeekRead>,
    /// Shrink all sequences of <newline>'s to one <newline>
    squeeze_lines: bool,
    /// Suppress underlining and bold
    plain: bool,
    /// Iteration over [`SeekPositions`] buffer has reached end
    is_ended: bool,
    /// Char positions for stylized text
    style_positions: Vec<(Range<usize>, StyleType)>,
}

impl SeekPositions {
    /// Creates new [`SeekPositions`]
    fn new(
        source: Source,
        line_len: Option<usize>,
        squeeze_lines: bool,
        plain: bool,
    ) -> Result<Self, MoreError> {
        let buffer: Box<dyn SeekRead> = match source.clone() {
            Source::File(path) => {
                let Ok(file) = File::open(path.clone()) else {
                    return Err(MoreError::SeekPositions(SeekPositionsError::FileRead(
                        path.to_str().unwrap_or("<file>").to_string(),
                    )));
                };
                let buffer: Box<dyn SeekRead> = Box::new(file);
                buffer
            }
            Source::Buffer(buffer) => {
                let buffer: Box<dyn SeekRead> = Box::new(buffer);
                buffer
            }
        };
        let mut seek_pos = Self {
            positions: vec![],
            line_len,
            stream_len: 0,
            lines_count: 0,
            source: source.clone(),
            buffer,
            squeeze_lines,
            plain,
            is_ended: false,
            style_positions: vec![],
        };
        (seek_pos.lines_count, seek_pos.stream_len) = seek_pos.lines_count_and_stream_len();
        if seek_pos.lines_count as u64 > seek_pos.stream_len {
            return Err(MoreError::FileRead(source.clone().name()));
        }
        Ok(seek_pos)
    }

    /// Counts all buffer lines and set [`SeekPositions`] to previous state
    fn lines_count_and_stream_len(&mut self) -> (usize, u64) {
        let current_line = self.current_line();
        let _ = self.buffer.rewind();
        let mut count = 0;
        while self.next().is_some() {
            count += 1;
        }
        {
            let mut reader = BufReader::new(&mut self.buffer);
            let mut buf = vec![];
            let _ = reader.read_to_end(&mut buf);
            if !buf.is_empty() {
                count += 1;
            }
        }
        let stream_position = self.buffer.stream_position().unwrap_or(0);
        let _ = self.buffer.rewind();
        let mut i = 0;
        self.positions = vec![0];
        while i < current_line {
            if self.next().is_none() {
                break;
            };
            i += 1;
        }
        (count, stream_position)
    }

    /// Read line from current seek position with removing styling control chars
    fn read_line(&mut self) -> Result<String, MoreError> {
        let current_seek = self.current();
        let mut line = if let Some(next_seek) = self.next() {
            self.next_back();
            let mut line_buf = vec![b' '; (next_seek - current_seek) as usize];
            self.buffer.read_exact(&mut line_buf).map_err(|_| {
                MoreError::SeekPositions(SeekPositionsError::FileRead(self.source.name()))
            })?;
            String::from_utf8(Vec::from_iter(line_buf)).map_err(|_| {
                MoreError::SeekPositions(SeekPositionsError::StringParse(self.source.name()))
            })?
        } else {
            let mut line_buf = vec![];
            let _ = self.buffer.read_to_end(&mut line_buf);
            String::from_utf8(Vec::from_iter(line_buf)).unwrap_or_default()
        };
        if line.is_empty() {
            return Ok(String::new());
        }
        for (rng, _) in self.style_positions.iter().rev() {
            if rng.end > line.len() {
                continue;
            };
            let new = line[rng.clone()]
                .chars()
                .filter(|ch| *ch != '\x08' && *ch != '_' && *ch != '\r')
                .collect::<String>();
            if !new.is_empty() {
                let new = new[..1].to_string();
                if line.len() >= rng.end {
                    line.replace_range(rng.clone(), &new);
                }
            }
        }
        Ok(line)
    }

    /// Returns current seek position
    fn current(&self) -> u64 {
        *self.positions.last().unwrap_or(&0)
    }

    /// Returns current line index
    fn current_line(&self) -> usize {
        self.positions.len()
    }

    /// Sets current line to [`position`]
    fn set_current(&mut self, position: usize) {
        self.is_ended = false;
        while self.current_line() != position {
            if self.current_line() < position && self.next().is_none() {
                self.is_ended = true;
                break;
            } else if self.current_line() > position && self.next_back().is_none() {
                break;
            }
        }
    }

    /// Returns full lines count fo current source
    fn len_lines(&self) -> usize {
        self.lines_count
    }

    /// Returns stream len fo current source
    fn _len(&self) -> u64 {
        self.stream_len
    }

    /// Seek to certain [`position`] over current source
    fn seek(&mut self, position: u64) -> Result<(), MoreError> {
        let err = Err(MoreError::SeekPositions(SeekPositionsError::OutOfRange(
            position,
        )));
        if position > self.stream_len {
            return err;
        }
        loop {
            if self.next() > Some(position) && Some(position) > self.next_back() {
                break;
            } else if self.current() < position {
                if self.next().is_none() {
                    return err;
                }
            } else if self.current() > position {
                if self.next_back().is_none() {
                    return err;
                }
            } else {
                break;
            }
        }
        Ok(())
    }

    /// Returns nth position of choosen [`char`] if it exists
    pub fn find_n_line(&mut self, n: usize) -> Option<u64> {
        let last_seek = self.current();
        let mut n_char_seek = None;
        let _ = self.buffer.rewind();
        let mut i = 0;
        let mut seek_pos = 0;
        {
            let reader = BufReader::new(&mut self.buffer).lines();
            for line in reader {
                if let Ok(line) = line {
                    seek_pos += line.len();
                }
                i += 1;
                if i >= n {
                    n_char_seek = Some(seek_pos as u64);
                    break;
                }
            }
        }
        let _ = self.seek(last_seek);
        n_char_seek
    }

    /// Search 'EOF', '\n', if line length bigger than [`Self::line_len`],
    /// then return next line len as [`Self::line_len`]. Skip next line
    /// after that [`Self::buffer`] seek position will be at last char
    /// position of next line. When this function search line end, it
    /// skips styled text control bytes and add range for replacing this
    /// bytes with text to [`Self::style_positions`]. [`Self::style_positions`]
    /// used in [`Self::read_line`] for formating styled text.
    fn find_next_line_len_with_skip(&mut self) -> usize {
        let mut style_positions = vec![];
        let mut buffer_str = String::new();
        self.is_ended = false;
        let mut line_len = 0;
        let mut need_add_chars = 0;
        let max_line_len = self.line_len.unwrap_or(usize::MAX) as u64;
        let mut n_count = 0;
        let mut r_count = 0;
        {
            let reader = BufReader::new(&mut self.buffer);
            let mut bytes = reader.bytes();
            let mut buf = Vec::with_capacity(CONVERT_STRING_BUF_SIZE);
            loop {
                let Some(Ok(byte)) = bytes.next() else {
                    self.is_ended = true;
                    break;
                };
                match byte {
                    b'\r' => {
                        line_len += 1;
                        buffer_str.push(byte as char);
                        buf.push(byte);
                        r_count += 1;
                        continue;
                    }
                    b'\n' => {
                        line_len += 1;
                        if self.squeeze_lines {
                            n_count += 1;
                            loop {
                                let Some(Ok(byte)) = bytes.next() else {
                                    self.is_ended = true;
                                    break;
                                };
                                match byte {
                                    b'\n' => {
                                        line_len += 1;
                                        n_count += 1;
                                        buffer_str.push(byte as char);
                                    }
                                    b'\r' => {
                                        line_len += 1;
                                        r_count += 1;
                                        buffer_str.push(byte as char);
                                    }
                                    _ => break,
                                }
                            }
                            let diff = 1 + r_count.min(1);
                            if n_count > 1 && line_len > diff {
                                line_len -= diff;
                                for _ in 0..diff {
                                    buffer_str.pop();
                                }
                            }
                        }
                        break;
                    }
                    byte if byte.is_ascii_control() && byte != b'\x08' => {
                        line_len += 1;
                        break;
                    }
                    _ => {
                        if !self.plain {
                            if let Some(mut new_style_positions) = continious_styled_parse(
                                &mut buffer_str,
                                &mut need_add_chars,
                                line_len,
                            ) {
                                style_positions.append(&mut new_style_positions);
                            } else {
                                buffer_str.push(byte as char);
                                line_len += 1;
                                continue;
                            }
                        }
                        buffer_str.push(byte as char);
                        line_len += 1;
                    }
                }
                buf.push(byte);
                if buf.len() >= CONVERT_STRING_BUF_SIZE {
                    if let Err(err) = std::str::from_utf8(&buf) {
                        buf = buf[err.valid_up_to()..].to_vec();
                    } else {
                        buf.clear();
                    }
                }
                if line_len as u64 >= max_line_len + need_add_chars {
                    if line_len > buffer_str.len() {
                        line_len -= buffer_str.len() + 1;
                    }
                    if let Err(err) = std::str::from_utf8(&buf) {
                        line_len -= buf.len() - err.valid_up_to();
                    }
                    break;
                }
            }
        }
        if !self.plain {
            let mut new_style_positions = last_styled_parse(
                &mut buffer_str,
                &mut line_len,
                max_line_len as usize,
                self.is_ended,
            );
            if buffer_str.len() < 3 && !self.is_ended {
                style_positions.pop();
            }
            style_positions.append(&mut new_style_positions);
            self.style_positions = style_positions;
        }
        line_len
    }
}

/// Parse last chars before current position in [`SeekPositions::buffer`]
/// for finding styled control sequences during
/// [`SeekPositions::find_next_line_len_with_skip`] loop
///
/// # Arguments
///
/// * `buffer_str` - last chars in [SeekPositions::buffer] stream that can
///   contain styling sequences.
/// * `need_add_chars` - text styling control char count. This count will be
///   used for checking if line length is bigger than max line length.
/// * `line_len` - next line length at that moment in
///   [`SeekPositions::find_next_line_len_with_skip`] loop.
fn continious_styled_parse(
    buffer_str: &mut String,
    need_add_chars: &mut u64,
    line_len: usize,
) -> Option<Vec<(Range<usize>, StyleType)>> {
    let check_styled = |(i, ch, first): (usize, &char, char)| {
        (i % 2 == 0 && *ch == first) || (i % 2 == 1 && *ch == '\x08')
    };
    let mut style_positions = vec![];
    let buffer = buffer_str.chars().collect::<Vec<char>>();
    if buffer.len() == 3 && (buffer.starts_with(&['_', '\x08']) || buffer.ends_with(&['\x08', '_']))
    {
        style_positions.push(((line_len - buffer.len())..line_len, StyleType::Underscore));
        *need_add_chars += 2;
        buffer_str.clear();
    } else if buffer.len() < 3 {
        let is_styled = buffer
            .iter()
            .enumerate()
            .map(|(i, ch)| (i, ch, buffer[0]))
            .all(check_styled);
        if is_styled {
            return None;
        } else {
            buffer_str.remove(0);
        }
    } else if buffer.len() >= 3 {
        let is_styled = buffer
            .iter()
            .enumerate()
            .map(|(i, ch)| (i, ch, buffer[0]))
            .all(check_styled);
        if !is_styled {
            style_positions.push((
                (line_len - buffer.len())..(line_len - 1),
                StyleType::Negative,
            ));
            *need_add_chars += ((buffer.len() as f32) / 2.0).floor() as u64;
        } else {
            return None;
        }
        let last = buffer_str.chars().last();
        buffer_str.clear();
        if let Some(last) = last {
            buffer_str.push(last);
        }
    }
    Some(style_positions)
}

/// Parse last chars before current position in [`SeekPositions::buffer`]
/// for finding styled control sequences after
/// [`SeekPositions::find_next_line_len_with_skip`] loop
///
/// # Arguments
///
/// * `buffer_str` - last chars in [SeekPositions::buffer] stream that can
///   contain styling sequences.
/// * `line_len` - next line length at that moment in
///   [`SeekPositions::find_next_line_len_with_skip`] loop.
///
/// * `max_line_len` - line that length bigger than this value will be splited
/// * `is_ended` - indicate that [SeekPositions::buffer] stream reached `EOF`
fn last_styled_parse(
    buffer_str: &mut str,
    line_len: &mut usize,
    max_line_len: usize,
    is_ended: bool,
) -> Vec<(Range<usize>, StyleType)> {
    let check_styled = |(i, ch, first): (usize, &char, char)| {
        (i % 2 == 0 && *ch == first) || (i % 2 == 1 && *ch == '\x08')
    };
    let mut style_positions = vec![];
    let l = buffer_str
        .chars()
        .filter(|ch| *ch == '\n' || *ch == '\r' || *ch == ' ')
        .count();
    let buffer = buffer_str
        .chars()
        .filter(|ch| *ch != '\n' && *ch != '\r' && *ch != ' ')
        .collect::<Vec<char>>();
    if !buffer.is_empty() && buffer.len() < 3 && max_line_len > l + 2 && *line_len >= max_line_len {
        *line_len -= l + 2;
    } else if buffer.len() == 3
        && (buffer.starts_with(&['_', '\x08']) || buffer.ends_with(&['\x08', '_']))
    {
        style_positions.push((
            (*line_len - buffer.len() - l - (!is_ended as usize))
                ..(*line_len - (!is_ended as usize)),
            StyleType::Underscore,
        ));
    } else if buffer.len() >= 3
        && buffer
            .iter()
            .enumerate()
            .map(|(i, ch)| (i, ch, buffer[0]))
            .all(check_styled)
    {
        style_positions.push((
            (*line_len - buffer.len() - l - (!is_ended as usize))
                ..(*line_len - (!is_ended as usize)),
            StyleType::Negative,
        ));
    }
    style_positions
}

impl Iterator for SeekPositions {
    type Item = u64;

    /// Iter over [`SeekRead`] buffer lines in forward direction
    fn next(&mut self) -> Option<Self::Item> {
        let current_position = *self.positions.last().unwrap_or(&0);
        if self.buffer.seek(SeekFrom::Start(current_position)).is_err() {
            return None;
        }
        let line_len = self.find_next_line_len_with_skip();
        let Ok(stream_position) = self.buffer.stream_position() else {
            return None;
        };
        let next_position = current_position + line_len as u64;
        if self.is_ended || next_position >= stream_position {
            let _ = self.buffer.seek(SeekFrom::Start(current_position));
            None
        } else {
            if self.buffer.seek(SeekFrom::Start(next_position)).is_err() {
                return None;
            };
            self.positions.push(next_position);
            Some(next_position)
        }
    }
}

impl DoubleEndedIterator for SeekPositions {
    /// Iter over [`SeekRead`] buffer lines in backward direction
    fn next_back(&mut self) -> Option<Self::Item> {
        let _ = self.positions.pop();
        let _ = self
            .buffer
            .seek(SeekFrom::Start(*self.positions.last().unwrap_or(&0)));
        self.positions.last().cloned()
    }
}

/// Inforamtion about [`SeekRead`] source for [`SeekPositions`]
#[derive(Debug, Clone)]
enum Source {
    /// Path to file that can be used for seek and read with [`SeekPositions`]
    File(PathBuf),
    /// [`Cursor`] on [`String`] that can be used for seek and read with [`SeekPositions`]
    Buffer(Cursor<String>),
}

impl Source {
    /// Returns [`String`] that identify [`Source`]
    fn name(&mut self) -> String {
        match self {
            Source::File(path) => path.to_str().unwrap_or("<file>").to_owned(),
            Source::Buffer(cursor) => {
                let current_pos = cursor.stream_position().unwrap_or(0);
                let _ = cursor.seek(SeekFrom::Start(0));
                let mut line = String::new();
                if BufRead::read_line(cursor, &mut line).is_err() {
                    line = "<buffer>".to_owned();
                }
                if line.len() > 15 {
                    if let Some(sub) = line.get(..15) {
                        line = sub.to_owned() + "...";
                    }
                }
                let _ = cursor.seek(SeekFrom::Start(current_pos));
                line
            }
        }
    }
}

/// Context of more current source, last search, flags etc
struct SourceContext {
    /// Current [`Source`] for seek and read
    current_source: Source,
    /// Last [`Source`] that was handled previously
    last_source: Source,
    /// [`SeekPositions`] used for seek and read over [`Source`]
    seek_positions: SeekPositions,
    /// Current [`Source`] header lines count
    header_lines_count: Option<usize>,
    /// Used by more [`Terminal`] size
    terminal_size: Option<(usize, usize)>,
    /// Last writen screen from previous [`Source`]
    previous_source_screen: Option<Screen>,
    /// Current [`Screen`]
    screen: Option<Screen>,
    /// Position of last line
    last_line: usize,
    /// Current search pattern
    current_pattern: String,
    /// Last search settings
    last_search: Option<(Regex, bool, Direction)>,
    /// Storage for marks that were set during current [`Source`] processing
    marked_positions: HashMap<char, usize>,
    /// Flag that [`true`] if input files count is more that 1
    is_many_files: bool,
    /// Shrink all sequences of <newline>'s to one <newline>
    squeeze_lines: bool,
    /// Suppress underlining and bold
    plain: bool,
    /// Is source reached end
    is_ended_file: bool,
}

impl SourceContext {
    /// New [`SourceContext`]
    pub fn new(
        source: Source,
        terminal_size: Option<(usize, usize)>,
        is_many_files: bool,
        squeeze_lines: bool,
        plain: bool,
    ) -> Result<Self, MoreError> {
        Ok(Self {
            current_source: source.clone(),
            last_source: source.clone(),
            seek_positions: SeekPositions::new(
                source.clone(),
                terminal_size.map(|size| size.1),
                squeeze_lines,
                plain,
            )?,
            header_lines_count: if let Source::File(path) = source {
                let header = format_file_header(path, terminal_size.map(|(_, c)| c))?;
                Some(header.len())
            } else {
                None
            },
            terminal_size,
            previous_source_screen: None,
            screen: terminal_size.map(|t| Screen::new((t.0 - 1, t.1))),
            last_line: 0,
            current_pattern: "".to_string(),
            last_search: None,
            marked_positions: HashMap::new(),
            is_many_files,
            squeeze_lines,
            plain,
            is_ended_file: false,
        })
    }

    /// Returns current [`Screen`]
    pub fn screen(&self) -> Option<Screen> {
        self.screen.clone()
    }

    /// Sets new [`Source`]
    fn set_source(&mut self, source: Source) -> Result<(), MoreError> {
        self.seek_positions = SeekPositions::new(
            source.clone(),
            self.seek_positions.line_len,
            self.squeeze_lines,
            self.plain,
        )?;
        self.last_source = self.current_source.clone();
        self.current_source = source;
        self.marked_positions.clear();
        self.last_search = None;
        self.last_line = 0;
        self.previous_source_screen = self.screen.clone();
        self.goto_beginning(None);
        Ok(())
    }

    /// Updates current [`Screen`]
    /// Calculate lines count that need get from: previous file
    /// [`Screen`], header, current file content. Then fill
    /// [`SourceContext::screen`] with given lines
    fn update_screen(&mut self) -> Result<(), MoreError> {
        let Some(terminal_size) = self.terminal_size else {
            return Err(MoreError::MissingTerminal);
        };
        let Some(screen) = self.screen.as_mut() else {
            return Err(MoreError::MissingTerminal);
        };
        screen.clear();

        let mut content_lines = vec![];
        let mut header_lines = vec![];
        let mut previous_lines = vec![];
        if self.is_many_files {
            if let Source::File(path) = &self.current_source {
                header_lines = format_file_header(path.clone(), Some(terminal_size.1))?;
            }
        }

        let mut current_line = self.seek_positions.current_line();
        let mut content_lines_len = current_line;
        let mut remain = if terminal_size.0 > content_lines_len + 1 {
            terminal_size.0 - 1 - content_lines_len
        } else {
            0
        };
        if terminal_size.0 < content_lines_len + 1 {
            content_lines_len = terminal_size.0 - 1;
        }

        remain = if remain > header_lines.len() {
            remain - header_lines.len()
        } else {
            let l = header_lines.len();
            header_lines = header_lines[(l - remain)..].to_vec();
            0
        };
        let header_lines_len = header_lines.len();
        if remain > 0 {
            if let Some(previous_source_screen) = &self.previous_source_screen {
                let l = previous_source_screen.0.len();
                previous_lines = previous_source_screen.0[(l - remain)..].to_vec();
            } else {
                if current_line + remain < self.seek_positions.len_lines() {
                    current_line += remain;
                    self.seek_positions.set_current(current_line);
                } else {
                    current_line = self.seek_positions.len_lines();
                    self.seek_positions.set_current(current_line);
                }
                content_lines_len = current_line;
            }
        }

        let mut i = 0;
        while i < content_lines_len - 1 {
            let line = self.seek_positions.read_line()?;
            content_lines.push(line);
            if self.seek_positions.next_back().is_none() {
                break;
            }
            i += 1;
        }
        let line = self.seek_positions.read_line()?;
        content_lines.push(line);

        content_lines.reverse();

        let mut style_lines = vec![];
        let mut add_style_line = |seek_positions: &mut SeekPositions| {
            let mut deleted_count = 0;
            style_lines.push(
                seek_positions
                    .style_positions
                    .iter()
                    .cloned()
                    .map(|(rng, st)| {
                        deleted_count += rng.end - rng.start - 1;
                        (rng.end - deleted_count - 1, st)
                    })
                    .collect::<Vec<_>>(),
            );
        };
        while self.seek_positions.next().is_some()
            && self.seek_positions.current_line() <= current_line
        {
            add_style_line(&mut self.seek_positions);
        }
        add_style_line(&mut self.seek_positions);
        self.seek_positions.set_current(current_line);
        let previous_lines_len = previous_lines.len();
        for (i, line) in previous_lines.into_iter().enumerate() {
            screen.set_raw((i, 0), line)?
        }

        for (i, line) in header_lines.into_iter().enumerate() {
            screen.set_str((i + previous_lines_len, 0), line, StyleType::None)?;
        }

        for (i, line) in content_lines.into_iter().enumerate() {
            screen.set_str(
                (i + previous_lines_len + header_lines_len, 0),
                line,
                StyleType::None,
            )?;
            let Some(style_positions) = style_lines.get(i) else {
                continue;
            };
            let Some(line) = screen.0.get_mut(i + previous_lines_len + header_lines_len) else {
                continue;
            };
            for (pos, st) in style_positions {
                if line.len() > *pos {
                    line[*pos].1 = *st;
                }
            }
        }

        self.is_ended_file = self.seek_positions.is_ended;
        Ok(())
    }

    /// Scroll over [`SeekPositions`] in [`direction`] on [`count`] lines
    pub fn scroll(&mut self, count: usize, direction: Direction) {
        let mut count: isize = count as isize;
        if direction == Direction::Backward {
            count = -count;
        }
        let header_lines_count = self.header_lines_count.unwrap_or(0);
        let next_line = self.seek_positions.current_line() as isize + count;
        let next_line = if next_line < 0 { 0 } else { next_line as usize };
        let terminal_size = self.terminal_size.unwrap_or((1 + header_lines_count, 0));
        let lines_count = if terminal_size.0 == 1 + header_lines_count {
            terminal_size.0
        } else {
            terminal_size.0 - 1 - header_lines_count
        };
        self.seek_positions.set_current(if next_line < lines_count {
            lines_count
        } else {
            next_line
        });
        self.is_ended_file = self.seek_positions.is_ended;
    }

    /// Seek to buffer beginning with line count
    pub fn goto_beginning(&mut self, count: Option<usize>) {
        let terminal_size = self.terminal_size.unwrap_or((1, 0));
        let header_lines_count = self.header_lines_count.unwrap_or(0);
        let next_line = if terminal_size.0 <= 1 + header_lines_count {
            terminal_size.0
        } else {
            terminal_size.0 - 1 - header_lines_count
        };
        if self.seek_positions.len_lines() < next_line {
            self.seek_positions
                .set_current(self.seek_positions.len_lines() + 1)
        } else {
            self.seek_positions.set_current(next_line)
        };
        if let Some(count) = count {
            self.scroll(count, Direction::Forward);
        }
        self.is_ended_file = self.seek_positions.is_ended;
    }

    /// Seek to buffer end
    pub fn goto_eof(&mut self, count: Option<usize>) {
        if count.is_some() {
            self.goto_beginning(count);
            return;
        }
        self.seek_positions
            .set_current(self.seek_positions.len_lines() + 1);
        self.is_ended_file = self.seek_positions.is_ended;
    }

    /// Seek to previous line
    pub fn return_previous(&mut self) {
        self.seek_positions.set_current(self.last_line);
        self.is_ended_file = self.seek_positions.is_ended;
    }

    /// Search first line with pattern relatively to current line in buffer
    pub fn search(
        &mut self,
        count: Option<usize>,
        pattern: Regex,
        is_not: bool,
        direction: Direction,
    ) -> Result<(), MoreError> {
        let last_line = self.seek_positions.current_line();
        let mut last_string: Option<String> = None;
        let mut result = Ok(());
        loop {
            let string = self.seek_positions.read_line()?;
            let mut haystack = string.clone();
            if let Some(last_string) = last_string {
                haystack = match direction {
                    Direction::Forward => last_string.to_owned() + haystack.as_str(),
                    Direction::Backward => haystack + &last_string,
                };
            }
            let has_match = if is_not {
                !pattern.is_match(&haystack)
            } else {
                pattern.is_match(&haystack)
            };
            if has_match {
                let Some((rows, _)) = self.terminal_size else {
                    break;
                };
                let mut new_position = self.seek_positions.current_line() + (rows - 4);
                if let Some(count) = count {
                    new_position += count;
                    if new_position > (rows - 4) {
                        new_position -= rows - 4;
                    }
                }
                self.seek_positions.set_current(new_position);
                result = Ok(());
                break;
            }
            if match direction {
                Direction::Forward => self.seek_positions.next(),
                Direction::Backward => {
                    let next_back = self.seek_positions.next_back();
                    if next_back.is_none() {
                        result = Ok(());
                    }
                    next_back
                }
            }
            .is_none()
            {
                self.seek_positions.set_current(last_line);
                result = Err(MoreError::PatternNotFound(self.current_pattern.clone()));
                break;
            }
            last_string = Some(string);
        }

        self.last_search = Some((pattern, is_not, direction));
        if result.is_ok() {
            self.is_ended_file = self.seek_positions.is_ended;
        }
        result
    }

    /// Repeat previous search if exists
    pub fn repeat_search(
        &mut self,
        count: Option<usize>,
        is_reversed: bool,
    ) -> Result<(), MoreError> {
        if let Some((pattern, is_not, direction)) = &self.last_search {
            let direction = if is_reversed {
                !direction.clone()
            } else {
                direction.clone()
            };
            self.search(count, pattern.clone(), *is_not, direction)
        } else {
            Err(MoreError::SourceContext(
                SourceContextError::MissingLastSearch,
            ))
        }
    }

    /// Set mark with current line
    pub fn set_mark(&mut self, letter: char) {
        self.marked_positions
            .insert(letter, self.seek_positions.current_line());
    }

    /// Seek to line that marked with letter
    pub fn goto_mark(&mut self, letter: char) -> Result<(), MoreError> {
        if let Some(position) = self.marked_positions.get(&letter) {
            self.seek_positions.set_current(*position);
            self.is_ended_file = self.seek_positions.is_ended;
            Ok(())
        } else {
            Err(MoreError::SourceContext(SourceContextError::MissingMark(
                letter,
            )))
        }
    }

    /// Update all fields that depends from terminal size: current screen,
    /// line len, buffer lines count etc
    pub fn resize(&mut self, terminal_size: (usize, usize)) -> Result<(), MoreError> {
        if self.terminal_size.is_none() {
            return Err(MoreError::MissingTerminal);
        }
        let previous_seek = self.seek_positions.current();
        {
            let mut temp_seek_positions = SeekPositions::new(
                self.seek_positions.source.clone(),
                Some(terminal_size.1),
                self.squeeze_lines,
                self.plain,
            )?;
            std::mem::swap(&mut self.seek_positions, &mut temp_seek_positions);
        }
        self.seek_positions.seek(previous_seek)?;
        self.previous_source_screen = None;
        let line_count = if terminal_size.0 == 1 {
            terminal_size.0
        } else {
            terminal_size.0 - 1
        };
        self.screen = Some(Screen::new((line_count, terminal_size.1)));
        self.terminal_size = Some(terminal_size);
        self.update_screen()
    }

    /// Reset current file: seek to beggining, flush last state fields, update screen
    pub fn reset(&mut self) -> Result<(), MoreError> {
        self.goto_beginning(None);
        self.marked_positions.clear();
        self.last_search = None;
        self.last_line = self.seek_positions.current_line();
        self.previous_source_screen = None;
        self.update_screen()
    }
}

/// Wrapper over termios
struct Terminal {
    /// Struct that keep terminal in raw mod
    _raw_terminal: Option<RawTerminal<std::io::Stdout>>,
    /// Struct that keep terminal in buffer mod
    _alternate_screen: Option<AlternateScreen<std::io::Stdout>>,
    /// Stream for sending commands into terminal
    tty: std::io::Stdout,
    /// Terminal size in char rows and cols
    size: (u16, u16),
    /// Set terminal height as lines
    lines: Option<u16>,
    /// Suppress underlining and bold
    plain: bool,
    /// Input stream
    input_stream: Receiver<Result<String, MoreError>>,
}

impl Terminal {
    fn new(is_test: bool, lines: Option<u16>, plain: bool) -> Result<Self, MoreError> {
        let mut _raw_terminal = None;
        let mut _alternate_screen = None;
        if !is_test {
            if !termion::is_tty(&std::io::stdout().as_raw_fd()) {
                return Err(MoreError::TerminalInit);
            }
            let raw_terminal = stdout()
                .into_raw_mode()
                .map_err(|_| MoreError::TerminalInit)?;
            raw_terminal
                .activate_raw_mode()
                .map_err(|_| MoreError::TerminalInit)?;
            _raw_terminal = Some(raw_terminal);
            _alternate_screen = Some(
                stdout()
                    .into_alternate_screen()
                    .map_err(|_| MoreError::TerminalInit)?,
            );
        }

        let (sender, receiver) = channel();
        let mut terminal = Self {
            _raw_terminal,
            _alternate_screen,
            tty: stdout(),
            size: (
                u16::from_str(&std::env::var("LINES").unwrap_or_default())
                    .unwrap_or(LINES_PER_PAGE),
                u16::from_str(&std::env::var("COLUMNS").unwrap_or_default()).unwrap_or(NUM_COLUMNS),
            ),
            lines,
            plain,
            input_stream: receiver,
        };

        let _ = terminal.resize();
        let _ = std::thread::spawn(move || {
            let sender = sender;
            while !*NEED_QUIT.lock().unwrap() {
                let result = getch();
                match result {
                    Ok(Some(new_input)) => sender.send(Ok(new_input)).unwrap(),
                    Err(err) => sender.send(Err(err)).unwrap(),
                    _ => {}
                }
            }
        });

        Ok(terminal)
    }

    /// Display [`Screen`] on [`Terminal`]
    pub fn display(&mut self, screen: Screen) -> Result<(), MoreError> {
        if screen.0.len() > self.size.0 as usize || screen.0[0].len() > self.size.1 as usize {
            let _ = self.set_style(StyleType::None);
            return Err(MoreError::SetOutside);
        }
        let mut style = StyleType::None;
        for (i, line) in screen.0.iter().enumerate() {
            self.write_ch(' ', 0, i as u16);
            self.clear_current_line();
            for (j, (ch, st)) in line.iter().enumerate() {
                if style != *st {
                    let _ = self.set_style(if !self.plain { *st } else { StyleType::None });
                    style = *st;
                }
                self.write_ch(*ch, j as u16, i as u16);
            }
        }
        let _ = self.set_style(StyleType::None);
        Ok(())
    }

    /// Get input without blocking
    fn get_input(&mut self) -> Result<Option<String>, MoreError> {
        let mut input = String::new();
        loop {
            match self.input_stream.try_recv() {
                Ok(Ok(new_input)) => input += new_input.as_str(),
                Ok(Err(err)) => return Err(err),
                Err(TryRecvError::Disconnected) => return Err(MoreError::InputRead),
                Err(TryRecvError::Empty) => break,
            }
        }
        Ok(if input.is_empty() { None } else { Some(input) })
    }

    fn set_style(&mut self, style: StyleType) -> std::io::Result<()> {
        let _ = write!(self.tty, "{}", Reset);
        match style {
            StyleType::Underscore => write!(self.tty, "{}", Underline),
            StyleType::Negative => write!(self.tty, "{}", Invert),
            _ => Ok(()),
        }
    }

    // Display prompt in bottom row
    pub fn display_prompt(&mut self, prompt: Prompt) -> Result<(), MoreError> {
        let line = prompt.format();
        if line.len() > self.size.1 as usize {
            let _ = self.set_style(StyleType::None);
            return Err(MoreError::InputTooLong);
        }

        let mut style = StyleType::None;
        let _ = write!(
            self.tty,
            "{}",
            if let Prompt::Input(_) = prompt {
                Show.to_string()
            } else {
                Hide.to_string()
            }
        );
        let line_position = if self.size.0 == 1 {
            self.size.0
        } else {
            self.size.0 - 1
        };
        self.write_ch(' ', 1, line_position);
        self.clear_current_line();
        for (i, (ch, st)) in line.iter().enumerate() {
            if style != *st {
                let _ = self.set_style(if !self.plain { *st } else { StyleType::None });
                style = *st;
            }
            self.write_ch(*ch, i as u16, line_position);
        }

        let _ = self.set_style(StyleType::None);
        Ok(())
    }

    /// Clear terminal content
    pub fn _clear(&mut self) {
        let _ = write!(self.tty, "{}", All);
    }

    /// Clear terminal content
    pub fn clear_current_line(&mut self) {
        let _ = write!(self.tty, "{}", CurrentLine);
    }

    /// Write error to [`Stderr`]
    fn _write_err(&self, string: String) {
        eprint!("{string}");
    }

    /// Write string to terminal
    fn write(&mut self, string: String, x: u16, y: u16) {
        let _ = write!(self.tty, "{}{string}", Goto(x + 1, y + 1));
    }

    /// Write string to terminal
    fn write_ch(&mut self, ch: char, x: u16, y: u16) {
        let _ = write!(self.tty, "{}{ch}", Goto(x + 1, y + 1));
    }

    /// Update terminal size for wrapper
    fn resize(&mut self) -> Result<(), MoreError> {
        let (x, y) = terminal_size().map_err(|_| MoreError::SizeRead)?;
        if self.size != (y, x) {
            if y < 2 {
                return Err(MoreError::TerminalOutput);
            }
            let mut lines = self.lines.unwrap_or(y);
            if lines > y || lines < 1 {
                lines = y;
            }
            self.size = (lines, x);
        }
        Ok(())
    }

    /// Prepare resources for closing terminal
    fn close(&mut self) {
        *NEED_QUIT.lock().unwrap() = true;
        let _ = write!(self.tty, "{}{}", Show, Reset);
        self._raw_terminal = None;
        self._alternate_screen = None;
    }
}

/// Get char from [`Stdin`]
fn getch() -> Result<Option<String>, MoreError> {
    let Some(result) = std::io::stdin().lock().events_and_raw().next() else {
        return Ok(None);
    };
    result
        .map(|(event, bytes)| match event {
            Event::Mouse(mouse_event) => {
                let button = match mouse_event {
                    MouseEvent::Press(button, _, _) => Some(button),
                    _ => *LAST_MOUSE_BUTTON.lock().unwrap(),
                };
                let last_mouse_button = if let MouseEvent::Release(..) = mouse_event {
                    None
                } else {
                    button
                };
                *LAST_MOUSE_BUTTON.lock().unwrap() = last_mouse_button;
                match button {
                    Some(MouseButton::WheelDown) => Some("\n".to_string()),
                    Some(MouseButton::WheelUp) => Some("k".to_string()),
                    _ => None,
                }
            }
            Event::Key(Key::Up) => Some("k".to_string()),
            Event::Key(Key::Down) => Some("\n".to_string()),
            Event::Key(key) => {
                let mut s = String::from_utf8(bytes).ok();
                if key == Key::Char('\n') {
                    if let Some(s) = &mut s {
                        s.clear();
                        s.push('\n');
                    }
                }
                s
            }
            _ => None,
        })
        .map_err(|_| MoreError::InputRead)
}

/// String that was printed in bottom terminal row
#[derive(Debug, Clone)]
enum Prompt {
    /// --More--
    More(Option<u8>),
    /// --More--(Next file)
    Eof(String),
    /// Current state info
    DisplayPosition(String),
    /// User input for pattern searching
    Input(String),
    /// Inform user about raised errors, program state
    Error(String),
    ExitKeys,
    /// Message that inform user that session is ended
    Exit,
}

impl Prompt {
    // Format Prompt for displaying on terminal
    fn format(&self) -> Vec<(char, StyleType)> {
        let mut line = vec![];
        let string = match self {
            Prompt::More(Some(percent)) => format!("-- More --({}%)", percent),
            Prompt::More(None) => "-- More --".to_string(),
            Prompt::Eof(next_file) => format!("-- More --(Next file: {next_file})"),
            Prompt::DisplayPosition(position) => position.clone(),
            Prompt::Input(input) => input.clone(),
            Prompt::Error(error) => error.clone(),
            Prompt::ExitKeys => {
                "[Press space to continue, \"q\", \":q\" or \"ZZ\" to quit.]".to_string()
            }
            Prompt::Exit => "Press Enter to exit ...".to_string(),
        };

        let style = match self {
            Prompt::More(_) | Prompt::Eof(_) => StyleType::Negative,
            Prompt::Error(error) if error.starts_with("No previous") => StyleType::Negative,
            _ => StyleType::None,
        };

        string.chars().for_each(|ch| line.push((ch, style)));
        line
    }
}

/// Compiles [`pattern`] as a POSIX BRE regex
fn compile_regex(pattern: String, ignore_case: bool) -> Result<Regex, MoreError> {
    // Normalize backslash escapes
    let pattern = pattern.replace("\\\\", "\\");

    let flags = if ignore_case {
        RegexFlags::bre().ignore_case()
    } else {
        RegexFlags::bre()
    };

    // plib::regex handles macOS empty pattern workaround internally
    Regex::new(&pattern, flags).map_err(|_| MoreError::StringParse(pattern))
}

/// More state
struct MoreControl {
    /// Program arguments
    args: Args,
    /// Terminal for displaying content in interactive session
    terminal: Option<Terminal>,
    /// Context of reading current [`Source`]
    context: SourceContext,
    /// [`MoreControl`] buffer for user commands input
    commands_buffer: String,
    /// Current prompt for displaying
    prompt: Option<Prompt>,
    /// Current file
    current_position: Option<usize>,
    /// Last file
    last_position: Option<usize>,
    /// Last source state
    last_source_before_usage: Option<(Source, u64)>,
    /// List of [`PathBuf`] for every input file
    file_pathes: Vec<PathBuf>,
    /// Default count for half screen scroll [`Command`]s
    count_default: Option<usize>,
    /// If true [`MoreControl::process_()`] is called
    is_new_file: bool,
    /// Last search has succeess match
    is_matched: bool,
    /// Buffered stdin content for '-' operand (stdin can only be read once)
    stdin_buffer: Option<String>,
}

impl MoreControl {
    /// Init [`MoreControl`]
    fn new(args: Args) -> Result<Self, MoreError> {
        let terminal = Terminal::new(args.test, args.lines, args.plain).ok();
        let mut current_position = None;
        let mut file_pathes = vec![];
        // Buffer stdin if '-' appears in file list (stdin can only be read once)
        let stdin_buffer: Option<String> = if args.input_files.iter().any(|f| f == "-") {
            let mut buf = String::new();
            use std::io::Read;
            std::io::stdin()
                .read_to_string(&mut buf)
                .map_err(|_| MoreError::InputRead)?;
            Some(buf)
        } else {
            None
        };

        let source = if args.input_files.is_empty()
            || (args.input_files.len() == 1 && args.input_files[0] == *"-")
        {
            if let Some(buf) = stdin_buffer.clone() {
                Source::Buffer(Cursor::new(buf))
            } else {
                let Some(Ok(buf)) = BufReader::new(std::io::stdin().lock()).lines().next() else {
                    return Err(MoreError::InputRead);
                };
                Source::Buffer(Cursor::new(buf))
            }
        } else {
            for file_string in &args.input_files {
                if file_string == "-" {
                    // Use a special path marker for stdin entries
                    file_pathes.push(PathBuf::from("-"));
                } else {
                    let path = to_path(file_string.clone())?;
                    file_pathes.push(path);
                }
            }
            current_position = Some(0);
            let first_file = &file_pathes[0];
            if first_file == &PathBuf::from("-") {
                Source::Buffer(Cursor::new(stdin_buffer.clone().unwrap_or_default()))
            } else {
                Source::File(first_file.clone())
            }
        };
        let size = terminal
            .as_ref()
            .map(|terminal| (terminal.size.0 as usize, terminal.size.1 as usize));
        let context = SourceContext::new(
            source,
            size,
            args.input_files.len() > 1,
            args.squeeze,
            args.plain,
        )?;
        Ok(Self {
            args,
            terminal,
            context,
            current_position,
            last_position: None,
            count_default: None,
            commands_buffer: String::new(),
            prompt: None,
            last_source_before_usage: None,
            file_pathes,
            is_new_file: false,
            is_matched: false,
            stdin_buffer,
        })
    }

    /// Print all input files in output if terminal isn't available
    fn print_all_input(&mut self) {
        let input_files = self.file_pathes.clone();
        if input_files.is_empty() || (input_files.len() == 1 && self.args.input_files[0] == *"-") {
            while self.context.seek_positions.next().is_some() {
                let Ok(line) = self
                    .context
                    .seek_positions
                    .read_line()
                    .inspect_err(|e| self.handle_error(e.clone()))
                else {
                    break;
                };
                print!("{line}")
            }
        } else {
            for file_path in &input_files {
                // Handle '-' as stdin
                let source = if *file_path == PathBuf::from("-") {
                    let buf = self.stdin_buffer.clone().unwrap_or_default();
                    Source::Buffer(Cursor::new(buf))
                } else {
                    Source::File(file_path.clone())
                };

                let Ok(_) = self
                    .context
                    .set_source(source)
                    .inspect_err(|e| self.handle_error(e.clone()))
                else {
                    return;
                };
                if input_files.len() > 1 {
                    // Format header differently for stdin vs file
                    if *file_path == PathBuf::from("-") {
                        const STDIN_LABEL: &str = "(standard input)";
                        let header_width = STDIN_LABEL.len() + 2; // 2 for border padding
                        let border = ":".repeat(header_width);
                        println!("{border}");
                        println!("{STDIN_LABEL}");
                        println!("{border}");
                    } else {
                        let Ok(header) = format_file_header(
                            file_path.clone(),
                            self.context.terminal_size.map(|ts| ts.1),
                        )
                        .inspect_err(|e| self.handle_error(e.clone())) else {
                            return;
                        };
                        for line in header {
                            println!("{line}");
                        }
                    }
                }

                loop {
                    let Ok(line) = self
                        .context
                        .seek_positions
                        .read_line()
                        .inspect_err(|e| self.handle_error(e.clone()))
                    else {
                        break;
                    };
                    print!("{line}");
                    if self.context.seek_positions.next().is_none() {
                        break;
                    }
                }
            }
        }
    }

    /// Display current state in terminal
    fn display(&mut self) -> Result<(), MoreError> {
        let Some(terminal) = self.terminal.as_mut() else {
            return Err(MoreError::MissingTerminal);
        };
        self.context.update_screen()?;
        let result = if let Some(screen) = self.context.screen() {
            let prompt = match &self.prompt {
                Some(Prompt::More(_)) | None => Prompt::More(if self.file_pathes.len() == 1 {
                    Some(
                        ((self.context.seek_positions.current_line() as f32
                            / self.context.seek_positions.lines_count as f32)
                            * 100.0) as u8,
                    )
                } else {
                    None
                }),
                Some(prompt) => prompt.clone(),
            };
            if let Prompt::Input(_) = prompt {
            } else {
                terminal.display(screen)?;
            };
            terminal.display_prompt(prompt)?;
            if self.is_matched {
                terminal.write("".to_owned(), 0, 0);
                terminal.clear_current_line();
                terminal.write("...skipping".to_owned(), 0, 0);
                self.is_matched = false;
            }
            Ok(())
        } else {
            Err(MoreError::MissingTerminal)
        };
        let _ = terminal.tty.flush();
        result
    }

    /// Get input with blocking. While blocking can be updated screen
    fn get_input_with_update(&mut self) -> Result<Option<String>, MoreError> {
        loop {
            self.resize()?;
            if let Some(terminal) = self.terminal.as_mut() {
                if let Some(chars) = terminal.get_input()? {
                    return Ok(Some(chars));
                }
            } else {
                return Err(MoreError::MissingTerminal);
            }
            std::thread::sleep(Duration::from_secs_f32(0.08));
        }
    }

    /// Read input and handle signals
    fn handle_events(&mut self) -> Result<(), MoreError> {
        if let Some(chars) = self.get_input_with_update()? {
            if "\x03\x04\x1C".contains(chars.get(0..1).unwrap_or("0")) {
                self.prompt = Some(Prompt::ExitKeys);
            }
            self.commands_buffer.push_str(&chars);
        }
        Ok(())
    }

    /// Call editor for current file as child process and handle output
    fn invoke_editor(&mut self) -> Result<(), MoreError> {
        let Source::File(ref file_path) = self.context.current_source else {
            return Err(MoreError::FileRead("<none>".to_owned()));
        };
        let editor = if let Ok(editor) = std::env::var("EDITOR") {
            editor
        } else {
            DEFAULT_EDITOR.to_string()
        };
        let editor = editor.as_str();
        let is_editor_vi_or_ex = editor == "vi" || editor == "ex";
        let Some(file_path) = file_path.as_os_str().to_str() else {
            return Err(MoreError::FileRead(
                file_path.to_str().unwrap_or("<file>").to_owned(),
            ));
        };

        let args: &[&str] = if is_editor_vi_or_ex {
            &[
                &format!("+{}", self.context.seek_positions.current_line()),
                "--",
                file_path,
            ]
        } else {
            &[file_path]
        };

        let _ = unsafe { getegid() != getuid() || getegid() != getgid() };
        let _ = unsafe { setgid(getgid()) < 0 || setuid(getuid()) < 0 };
        match std::process::Command::new(editor).args(args).status() {
            Ok(exit) if !ExitStatus::success(&exit) => Err(MoreError::EditorFailed),
            Err(_) => Err(MoreError::EditorFailed),
            _ => Ok(()),
        }
    }

    /// Find tag position with ctag and seek to it
    ///
    /// For correct usage apply "ctags --fields=+n -R *" before this function
    ///
    /// Calls `find` with `grep` to find any occurrence of `tagstring` pattern in all
    /// tags files in current folder and subfolders. Then parse result, finds file
    /// and line, opens file an seek to line position.
    fn goto_tag(&mut self, tagstring: String) -> Result<(), MoreError> {
        if tagstring.is_empty() {
            return Err(MoreError::FileRead(String::new()));
        };
        let parse_error = Err(MoreError::StringParse(tagstring.clone() + " ctags output"));
        let pattern = if tagstring.contains(['^']) {
            tagstring.clone()
        } else {
            format!("^{}", tagstring)
        };
        let output = std::process::Command::new("find")
            .args([".", "-name", "tags", "-type", "f"])
            .output();
        let Ok(output) = output else {
            return Err(MoreError::CTagsFailed);
        };
        let Ok(output) = std::str::from_utf8(&output.stdout) else {
            return parse_error;
        };
        let mut outputs = String::new();
        let mut tags_path: Option<String> = None;
        for file in output.split('\n') {
            let output = std::process::Command::new("grep")
                .args([pattern.as_str(), file])
                .output();
            let Ok(output) = output else {
                continue;
            };
            let Ok(output) = std::str::from_utf8(&output.stdout) else {
                continue;
            };
            if !output.is_empty() {
                outputs.push_str(output);
                if let Some(folder) = to_path(file.to_owned())?.parent() {
                    if !folder.exists() {
                        return Err(MoreError::FileRead(file.to_owned()));
                    }
                    tags_path = folder.to_str().map(|s| s.to_owned());
                }
                break;
            }
        }
        if outputs.is_empty() {
            return Err(MoreError::PatternNotFound(tagstring));
        }
        let lines = outputs.split("\n").collect::<Vec<&str>>();
        let Some(line) = lines.first() else {
            return Err(MoreError::FileRead(tagstring));
        };
        let fields = line.split("\t").collect::<Vec<&str>>();
        if fields.len() < 2 {
            return parse_error;
        };
        let path = to_path(tags_path.unwrap_or_default() + "/" + fields[1])?;
        let line;
        if let Some(line_str) = fields.iter().find(|w| w.starts_with("line:")) {
            let Some(line_str) = line_str.split(":").last() else {
                return parse_error;
            };
            let Ok(l) = line_str.parse::<usize>() else {
                return parse_error;
            };
            line = l;
        } else {
            return parse_error;
        }

        if let Some(tag) = &self.args.tag {
            if *tag == tagstring {
                if !self.file_pathes.contains(&path) {
                    self.file_pathes.insert(0, path.clone());
                }
                self.current_position = Some(0);
            }
        }

        self.context.set_source(Source::File(path))?;
        if let Some(n_char_seek) = self.context.seek_positions.find_n_line(line) {
            self.context.seek_positions.seek(n_char_seek)?;
            if let Some(terminal) = &self.terminal {
                let new_position =
                    self.context.seek_positions.current_line() + (terminal.size.0 as usize - 2);
                self.context.seek_positions.set_current(new_position);
                self.context.is_ended_file = self.context.seek_positions.is_ended;
                return Ok(());
            };
            Ok(())
        } else {
            Err(MoreError::PatternNotFound(tagstring))
        }
    }

    /// Set [`MoreControl::prompt`] to [`Prompt::DisplayPosition`]
    fn set_position_prompt(&mut self) -> Result<(), MoreError> {
        let Some(terminal_size) = self.context.terminal_size else {
            return Err(MoreError::MissingTerminal);
        };
        let mut filename = "<error>";
        let mut file_size = 0;
        if let Source::File(path) = &self.context.current_source {
            if let Some(file_string) = path.file_name() {
                if let Some(file_string) = file_string.to_str() {
                    filename = file_string;
                }
            }
            if let Ok(metadata) = path.metadata() {
                file_size = metadata.len();
            }
        }
        let current_position = self
            .current_position
            .map(|cp| (cp + 1).to_string())
            .unwrap_or("?".to_string());
        let input_files_count = self.file_pathes.len();
        let current_line = self.context.seek_positions.current_line();
        let byte_number = self.context.seek_positions.current();

        let line = if self.context.seek_positions.lines_count >= terminal_size.0 {
            format!(
                "{} {}/{} {} {}/{} {}%",
                filename,
                current_position,
                input_files_count,
                current_line,
                byte_number,
                file_size,
                ((current_line as f32 / self.context.seek_positions.lines_count as f32) * 100.0)
                    as usize
            )
        } else {
            format!("{} {}/{}", filename, current_position, input_files_count)
        };
        self.prompt = Some(Prompt::DisplayPosition(line));
        Ok(())
    }

    /// Set as current [`Source`] previous/next file
    fn scroll_file_position(
        &mut self,
        count: Option<usize>,
        direction: Direction,
    ) -> Result<bool, MoreError> {
        let mut count = count.unwrap_or(1) as isize;
        let mut result = Ok(false);
        if self.current_position.is_none() && self.last_position.is_some() {
            self.current_position = self.last_position;
        }
        if let Some(current_position) = self.current_position {
            let current_position = current_position as isize;
            if direction == Direction::Backward {
                count = -count;
            }
            let mut current_position = current_position + count;
            if current_position >= self.file_pathes.len() as isize {
                result = Ok(true);
                current_position = self.file_pathes.len() as isize - 1;
            } else if current_position < 0 {
                current_position = 0;
            }
            let current_position = current_position as usize;
            if let Some(file_path) = self.file_pathes.get(current_position) {
                if let Some(file_string) = file_path.as_os_str().to_str() {
                    if let Err(e) = self.examine_file(file_string.to_string()) {
                        result = Err(e);
                    }
                    self.current_position = Some(current_position);
                }
            }
        } else {
            self.current_position = Some(0);
            if let Some(file_path) = self.file_pathes.first() {
                if let Some(file_string) = file_path.as_os_str().to_str() {
                    if let Err(e) = self.examine_file(file_string.to_string()) {
                        result = Err(e);
                    }
                }
            }
        }
        result
    }

    fn if_eof_set_default(&mut self) {
        if let Some(Prompt::Eof(_)) = self.prompt {
            self.prompt = Some(Prompt::More(if self.file_pathes.len() == 1 {
                Some(100)
            } else {
                None
            }));
        }
    }

    /// Check if need go to next file
    ///
    /// If current file is ended, then check any case from:
    /// * if current file is commands usage, then [`Self::refresh`] called;
    /// * if current file is last, then display last prompt, wait last input and exit;
    /// * if has next file and [`Self::prompt`] is [`Prompt::Eof`], go to next file;
    /// * if has next file and [`Self::prompt`] isn't [`Prompt::Eof`], set [`Self::prompt`] as [`Prompt::Eof`];
    fn if_eof_and_prompt_goto_next_file(&mut self) -> Result<(), MoreError> {
        if self.context.is_ended_file {
            if self.last_source_before_usage.is_some() {
                return self.refresh();
            }

            if self.current_position == Some(self.file_pathes.len() - 1)
                && self.context.seek_positions.current_line()
                    == self.context.seek_positions.len_lines()
            {
                if self.args.exit_on_eof {
                    self.exit(None);
                }
                self.prompt = Some(Prompt::Exit);
                self.display()?;
                self.get_input_with_update()?;
                self.exit(None);
            }

            let next_position = self
                .current_position
                .unwrap_or(self.last_position.unwrap_or(0))
                + 1;

            if let Some(next_file) = self.file_pathes.get(next_position) {
                let name_and_ext = name_and_ext(next_file.clone())?;
                if let Some(Prompt::Eof(_)) = self.prompt {
                    if self
                        .scroll_file_position(Some(1), Direction::Forward)
                        .is_err()
                    {
                        self.prompt = Some(Prompt::Exit);
                        self.display()?;
                        self.get_input_with_update()?;
                        self.exit(None);
                    }
                    self.prompt = Some(Prompt::More(if self.file_pathes.len() == 1 {
                        Some(
                            (self.context.seek_positions.current_line() as f32
                                / self.context.seek_positions.lines_count as f32)
                                as u8,
                        )
                    } else {
                        None
                    }));
                } else {
                    self.prompt = Some(Prompt::Eof(name_and_ext));
                }
            } else {
                self.prompt = Some(Prompt::Exit);
                self.display()?;
                self.get_input_with_update()?;
                self.exit(None);
            }
        }
        Ok(())
    }

    /// Prepare all required resource to drop and exit
    fn exit(&mut self, error_message: Option<String>) {
        if let Some(terminal) = &mut self.terminal {
            terminal.close();
        }
        self.terminal = None;
        if let Some(ref error_message) = error_message {
            eprintln!("{error_message}");
            println!("{error_message}");
        }
        exit(error_message.is_some() as i32);
    }

    /// Set current file by [`file_string`] path
    fn examine_file(&mut self, file_string: String) -> Result<(), MoreError> {
        if file_string.is_empty() {
            self.context.reset()?;
        }

        if file_string.as_str() == "#" {
            if let Source::File(last_source_path) = &self.context.last_source {
                if let Ok(last_source_path) = last_source_path.canonicalize() {
                    let last_source_path = last_source_path.as_path();
                    let current_position = self
                        .file_pathes
                        .iter()
                        .position(|p| **p == *last_source_path);
                    if let Some(current_position) = current_position {
                        self.current_position = Some(current_position);
                    } else {
                        self.current_position = Some(0)
                    };
                } else {
                    self.current_position = Some(0);
                }
                self.context.goto_eof(None);
                let _ = self.context.update_screen();
                let _ = self.context.set_source(self.context.last_source.clone());
                self.last_position = None;
            }
        } else if file_string == "-" {
            // Handle stdin as a source using the buffered content
            self.context.goto_eof(None);
            let _ = self.context.update_screen();
            let buf = self.stdin_buffer.clone().unwrap_or_default();
            self.context.set_source(Source::Buffer(Cursor::new(buf)))?;
            self.last_position = self.current_position;
        } else {
            self.context.goto_eof(None);
            let _ = self.context.update_screen();
            self.context
                .set_source(Source::File(to_path(file_string)?))?;
            self.last_position = self.current_position;
        }
        self.is_new_file = true;
        Ok(())
    }

    /// return last state before help call, refresh current file and display result state
    fn refresh(&mut self) -> Result<(), MoreError> {
        if let Some((source, seek)) = &self.last_source_before_usage {
            self.context.set_source(source.clone())?;
            self.context.seek_positions.seek(*seek)?;
            self.last_source_before_usage = None;
        }
        self.display()
    }

    /// Update size of terminal for all depended resources
    fn resize(&mut self) -> Result<bool, MoreError> {
        let mut size = None;
        if let Some(terminal) = self.terminal.as_mut() {
            let _ = terminal.resize();
            size = Some((terminal.size.0 as usize, terminal.size.1 as usize));
        };
        if let Some(size) = size {
            if Some(size) != self.context.terminal_size {
                self.context.resize(size)?;
                let _ = self.refresh();
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// Execute [`Command`]
    fn execute(&mut self, command: Command) -> Result<(), MoreError> {
        match command {
            Command::Help => {
                let string = commands_usage();
                self.last_position = self.current_position;
                self.last_source_before_usage = Some((
                    self.context.seek_positions.source.clone(),
                    self.context.seek_positions.current(),
                ));
                self.context
                    .set_source(Source::Buffer(Cursor::new(string)))?;
                self.context.goto_beginning(None);
            }
            Command::ScrollForwardOneScreenful(count) => {
                let count = count.unwrap_or(self.context.terminal_size.unwrap_or((2, 0)).0 - 1);
                self.context.scroll(count, Direction::Forward);
                self.if_eof_and_prompt_goto_next_file()?;
            }
            Command::ScrollBackwardOneScreenful(count) => {
                let count = count.unwrap_or(self.context.terminal_size.unwrap_or((2, 0)).0 - 1);
                self.context.scroll(count, Direction::Backward);
                self.if_eof_set_default();
            }
            Command::ScrollForwardOneLine { count, is_space } => {
                let count = count.unwrap_or(if is_space {
                    self.context.terminal_size.unwrap_or((1, 0)).0
                } else {
                    1
                });
                self.context.scroll(count, Direction::Forward);
                self.if_eof_and_prompt_goto_next_file()?;
            }
            Command::ScrollBackwardOneLine(count) => {
                let count = count.unwrap_or(1);
                self.context.scroll(count, Direction::Backward);
                self.if_eof_set_default();
            }
            Command::ScrollForwardOneHalfScreenful(count) => {
                if count.is_some() {
                    self.count_default = count;
                };
                let count = count.unwrap_or_else(|| {
                    if let Some(count_default) = self.count_default {
                        count_default
                    } else {
                        let lines = self
                            .context
                            .terminal_size
                            .unwrap_or((LINES_PER_PAGE as usize, 0))
                            .0 as f32;
                        (((lines - 1.0) / 2.0).floor()) as usize
                    }
                });
                self.context.scroll(count, Direction::Forward);
                self.if_eof_and_prompt_goto_next_file()?;
            }
            Command::SkipForwardOneLine(count) => {
                let count = count.unwrap_or(1);
                self.context.scroll(count, Direction::Forward);
                self.if_eof_and_prompt_goto_next_file()?;
            }
            Command::ScrollBackwardOneHalfScreenful(count) => {
                if count.is_some() {
                    self.count_default = count;
                };
                let count = count.unwrap_or_else(|| {
                    if let Some(count_default) = self.count_default {
                        count_default
                    } else {
                        let lines = self
                            .context
                            .terminal_size
                            .unwrap_or((LINES_PER_PAGE as usize, 0))
                            .0 as f32;
                        (((lines - 1.0) / 2.0).floor()) as usize
                    }
                });
                self.context.scroll(count, Direction::Backward);
                self.if_eof_set_default();
            }
            Command::GoToBeginningOfFile(count) => {
                self.context.goto_beginning(count);
                self.if_eof_set_default();
            }
            Command::GoToEOF(count) => {
                self.context.goto_eof(count);
                self.if_eof_and_prompt_goto_next_file()?;
            }
            Command::RefreshScreen => self.refresh()?,
            Command::DiscardAndRefresh => {
                self.commands_buffer.clear();
                self.if_eof_set_default();
                self.refresh()?;
            }
            Command::MarkPosition(letter) => {
                self.context.set_mark(letter);
            }
            Command::ReturnMark(letter) => {
                self.context.goto_mark(letter)?;
            }
            Command::ReturnPreviousPosition => {
                self.context.return_previous();
                self.if_eof_set_default();
            }
            Command::SearchForwardPattern {
                count,
                is_not,
                pattern,
            } => {
                self.context.current_pattern = pattern.clone();
                let re = compile_regex(pattern, self.args.case_insensitive)?;
                self.context.search(count, re, is_not, Direction::Forward)?;
                self.is_matched = true;
                self.if_eof_set_default();
            }
            Command::SearchBackwardPattern {
                count,
                is_not,
                pattern,
            } => {
                self.context.current_pattern = pattern.clone();
                let re = compile_regex(pattern, self.args.case_insensitive)?;
                self.context
                    .search(count, re, is_not, Direction::Backward)?;
                self.is_matched = true;
                self.if_eof_set_default();
            }
            Command::RepeatSearch(count) => {
                self.context.repeat_search(count, false)?;
                self.is_matched = true;
                self.if_eof_set_default();
            }
            Command::RepeatSearchReverse(count) => {
                self.context.repeat_search(count, true)?;
                self.is_matched = true;
                self.if_eof_set_default();
            }
            Command::ExamineNewFile(filename) => self.examine_file(filename)?,
            Command::ExamineNextFile(count) => {
                if self.scroll_file_position(count, Direction::Forward)? {
                    self.prompt = Some(Prompt::Exit);
                    self.display()?;
                    self.get_input_with_update()?;
                    self.exit(None);
                }
            }
            Command::ExaminePreviousFile(count) => {
                if self.scroll_file_position(count, Direction::Backward)? {
                    self.prompt = Some(Prompt::Exit);
                    self.display()?;
                    self.get_input_with_update()?;
                    self.exit(None);
                }
            }
            Command::GoToTag(tagstring) => {
                self.goto_tag(tagstring)?;
                self.if_eof_set_default();
            }
            Command::InvokeEditor => self.invoke_editor()?,
            Command::DisplayPosition => self.set_position_prompt()?,
            Command::Quit => self.exit(None),
            _ => return Err(MoreError::UnknownCommand),
        };

        Ok(())
    }

    /// Handle errors that raised from commands execution
    fn handle_error(&mut self, error: MoreError) {
        let mut error_str = error.to_string();
        if let Some(terminal) = &mut self.terminal {
            if error_str.len() > terminal.size.1 as usize {
                if let Some(s) = error_str.get(..(terminal.size.1 as usize)) {
                    error_str = s.to_owned();
                }
            }
        }
        match error {
            MoreError::SeekPositions(ref seek_positions_error) => match seek_positions_error {
                SeekPositionsError::StringParse(_) | SeekPositionsError::OutOfRange(_) => {
                    self.exit(Some(error_str.clone()));
                }
                SeekPositionsError::FileRead(_) => {
                    self.prompt = Some(Prompt::Error(error_str.clone()));
                }
            },
            MoreError::SourceContext(ref source_context_error) => match source_context_error {
                SourceContextError::MissingLastSearch | SourceContextError::MissingMark(_) => {
                    self.prompt = Some(Prompt::Error(error_str.clone()));
                }
            },
            MoreError::InputTooLong | MoreError::PatternNotFound(_) => {
                self.prompt = Some(Prompt::Error(error_str.clone()));
            }
            MoreError::StringParse(_) => {
                self.commands_buffer.clear();
                self.prompt = Some(Prompt::Error(error_str.clone()));
            }
            MoreError::SetOutside
            | MoreError::EditorFailed
            | MoreError::CTagsFailed
            | MoreError::FileRead(_)
            | MoreError::SizeRead
            | MoreError::InputRead
            | MoreError::TerminalOutput
            | MoreError::MissingTerminal => {
                self.exit(Some(error_str.clone()));
            }
            MoreError::UnknownCommand => {
                self.prompt = Some(Prompt::Error(error_str.clone()));
            }
            MoreError::TerminalInit => {}
        }
        if self.args.test {
            self.exit(Some(error_str.clone()));
        }
    }

    /// Process input command sequence
    fn process_p(&mut self) -> Result<(), MoreError> {
        self.is_new_file = false;
        let Some(ref commands_str) = self.args.commands else {
            return Ok(());
        };
        let mut commands_str = commands_str.clone();
        loop {
            let (command, remainder, _) = parse(commands_str.clone())?;
            if command == Command::Unknown {
                return Err(MoreError::UnknownCommand);
            }
            let is_empty = remainder.is_empty();
            commands_str = remainder.clone();
            self.execute(command)?;
            if self.is_new_file {
                if let Some(ref mut commands_str) = self.args.commands {
                    *commands_str = remainder;
                }
                if commands_str.is_empty() {
                    self.is_new_file = false;
                }
                break;
            }
            if is_empty {
                break;
            }
        }
        Ok(())
    }

    /// Interactive session loop: handle events, parse, execute
    /// next command, display result. Catch errors if needed
    fn loop_(&mut self) -> ! {
        if let Some(tagstring) = &self.args.tag {
            let _ = self
                .execute(Command::GoToTag(tagstring.clone()))
                .inspect_err(|e| self.handle_error(e.clone()));
        }
        let _ = self
            .process_p()
            .inspect_err(|e| self.handle_error(e.clone()));
        let _ = self.display().inspect_err(|e| self.handle_error(e.clone()));
        loop {
            if self.is_new_file {
                let _ = self
                    .process_p()
                    .inspect_err(|e| self.handle_error(e.clone()));
                let _ = self.display().inspect_err(|e| self.handle_error(e.clone()));
                continue;
            }
            match self.handle_events() {
                Err(e) => {
                    self.handle_error(e);
                    continue;
                }
                Ok(_) => {
                    if let Some(Prompt::ExitKeys) = self.prompt {
                        let _ = self.display();
                        self.prompt = Some(Prompt::More(if self.file_pathes.len() == 1 {
                            Some(
                                (self.context.seek_positions.current_line() as f32
                                    / self.context.seek_positions.lines_count as f32)
                                    as u8,
                            )
                        } else {
                            None
                        }));
                        continue;
                    }
                }
            }
            if let Ok((command, mut remainder, next_possible)) =
                parse(self.commands_buffer.clone()).inspect_err(|e| self.handle_error(e.clone()))
            {
                if let Some(Prompt::Eof(_)) = self.prompt {
                } else if next_possible != Command::Unknown {
                    self.prompt = Some(Prompt::Input(self.commands_buffer.clone()));
                    let _ = self.display().inspect_err(|e| self.handle_error(e.clone()));
                } else {
                    self.prompt = Some(Prompt::More(if self.file_pathes.len() == 1 {
                        Some(
                            (self.context.seek_positions.current_line() as f32
                                / self.context.seek_positions.lines_count as f32)
                                as u8,
                        )
                    } else {
                        None
                    }));
                }
                match command {
                    Command::Unknown => {
                        continue;
                    }
                    _ => remainder.clear(),
                }
                self.commands_buffer = remainder;
                let _ = self
                    .execute(command)
                    .inspect_err(|e| self.handle_error(e.clone()));
                let _ = self.display().inspect_err(|e| self.handle_error(e.clone()));
            }
        }
    }
}

/// If [`String`] contains existed [`PathBuf`] than returns [`PathBuf`]
fn to_path(file_string: String) -> Result<PathBuf, MoreError> {
    let file_path = PathBuf::from_str(file_string.as_str())
        .map_err(|_| MoreError::FileRead(file_string.clone()))?;
    file_path
        .metadata()
        .map_err(|_| MoreError::FileRead(file_string.clone()))?;
    Ok(file_path)
}

/// Get formated file name and extension from [`PathBuf`]
fn name_and_ext(path: PathBuf) -> Result<String, MoreError> {
    let file_name = path.file_name().ok_or(MoreError::FileRead(
        path.to_str().unwrap_or("<file>").to_owned(),
    ))?;
    let file_name = file_name.to_str().ok_or(MoreError::FileRead(
        path.to_str().unwrap_or("<file>").to_owned(),
    ))?;
    Ok(file_name.to_string())
}

/// Format file header that can be displayed if input files count more than 1
fn format_file_header(
    file_path: PathBuf,
    line_len: Option<usize>,
) -> Result<Vec<String>, MoreError> {
    let name_and_ext = name_and_ext(file_path)?;

    let (mut name_and_ext, border) = if let Some(line_len) = line_len {
        let header_width = if name_and_ext.len() < 14 {
            14
        } else if name_and_ext.len() > line_len - 4 {
            line_len
        } else {
            name_and_ext.len() + 4
        };

        (
            name_and_ext
                .chars()
                .collect::<Vec<char>>()
                .chunks(line_len)
                .map(String::from_iter)
                .collect::<Vec<String>>(),
            ":".repeat(header_width),
        )
    } else {
        (
            vec![name_and_ext.clone()],
            ":".repeat(name_and_ext.len() + 4),
        )
    };

    name_and_ext.insert(0, border.clone());
    name_and_ext.push(border);
    Ok(name_and_ext)
}

/// Parse count argument of future [`Command`]
fn parse_count(chars: &[char], i: &mut usize, count: &mut Option<usize>) {
    let mut count_str = String::new();
    loop {
        let Some(ch) = chars.get(*i) else {
            break;
        };
        if !ch.is_numeric() {
            break;
        }
        count_str.push(*ch);
        *i += 1;
    }
    if let Ok(new_count) = count_str.parse::<usize>() {
        *count = Some(new_count);
    }
}

/// Parse search commands
fn parse_search_command(
    commands_str: &str,
    chars: &[char],
    i: &mut usize,
    next_possible_command: &mut Command,
    count: Option<usize>,
    direction: Direction,
) -> Option<Command> {
    *i += 1;
    let ch = chars.get(*i)?;
    let is_not = *ch == '!';
    if is_not {
        *i += 1;
    }
    let pattern = commands_str
        .chars()
        .skip(*i)
        .take_while(|c| {
            *i += 1;
            *c != '\n'
        })
        .collect::<_>();
    let ch = chars.get(*i - 1)?;
    if *ch == '\n' {
        match direction {
            Direction::Forward => Some(Command::SearchForwardPattern {
                count,
                is_not,
                pattern,
            }),
            Direction::Backward => Some(Command::SearchBackwardPattern {
                count,
                is_not,
                pattern,
            }),
        }
    } else {
        *next_possible_command = match direction {
            Direction::Forward => Command::SearchForwardPattern {
                count: None,
                is_not: false,
                pattern: "".to_string(),
            },
            Direction::Backward => Command::SearchBackwardPattern {
                count: None,
                is_not: false,
                pattern: "".to_string(),
            },
        };
        Some(Command::Unknown)
    }
}

/// Parse transition commands as examine file,
/// go to file with tagstring, quit
fn parse_transition_commands(
    commands_str: &str,
    chars: &[char],
    i: &mut usize,
    next_possible_command: &mut Command,
    count: Option<usize>,
) -> Option<Command> {
    *i += 1;
    let ch = chars.get(*i)?;
    Some(match *ch {
        'e' => {
            *i += 1;
            let ch = chars.get(*i)?;
            if *ch == ' ' {
                *i += 1;
            }
            let filename = commands_str
                .chars()
                .skip(*i)
                .take_while(|c| {
                    *i += 1;
                    *c != '\n'
                })
                .collect::<_>();
            let ch = chars.get(*i - 1)?;
            if *ch == '\n' {
                Command::ExamineNewFile(filename)
            } else {
                *next_possible_command = Command::ExamineNewFile("".to_string());
                Command::Unknown
            }
        }
        'n' => Command::ExamineNextFile(count),
        'p' => Command::ExaminePreviousFile(count),
        't' => {
            *i += 1;
            let ch = chars.get(*i)?;
            if *ch == ' ' {
                *i += 1;
            }
            let tagstring = commands_str
                .chars()
                .skip(*i)
                .take_while(|c| {
                    *i += 1;
                    *c != '\n'
                })
                .collect::<_>();
            let ch = chars.get(*i - 1)?;
            if *ch == '\n' {
                Command::GoToTag(tagstring)
            } else {
                *next_possible_command = Command::GoToTag(" ".to_string());
                Command::Unknown
            }
        }
        'q' => Command::Quit,
        _ => Command::Unknown,
    })
}

/// Parses [`String`] into [`Command`] and returns result with reminder
fn parse(commands_str: String) -> Result<(Command, String, Command), MoreError> {
    let mut command = Command::Unknown;
    let mut count: Option<usize> = None;
    let mut next_possible_command = Command::Unknown;

    let mut i = 0;
    let chars = commands_str.chars().collect::<Vec<_>>();
    let commands_str_len = commands_str.len();

    while command == Command::Unknown && i < commands_str_len {
        let Some(ch) = chars.get(i) else {
            break;
        };
        command = match ch {
            ch if ch.is_numeric() => {
                parse_count(&chars, &mut i, &mut count);
                continue;
            }
            'h' => Command::Help,
            'f' | '\x06' => Command::ScrollForwardOneScreenful(count),
            'b' | '\x02' => Command::ScrollBackwardOneScreenful(count),
            ' ' => Command::ScrollForwardOneLine {
                count,
                is_space: true,
            },
            'j' | '\n' => Command::ScrollForwardOneLine {
                count,
                is_space: false,
            },
            'k' => Command::ScrollBackwardOneLine(count),
            'd' | '\x04' => Command::ScrollForwardOneHalfScreenful(count),
            's' => Command::SkipForwardOneLine(count),
            'u' | '\x15' => Command::ScrollBackwardOneHalfScreenful(count),
            'g' => Command::GoToBeginningOfFile(count),
            'G' => Command::GoToEOF(count),
            'r' | '\x0C' => Command::RefreshScreen,
            'R' => Command::DiscardAndRefresh,
            'm' => {
                i += 1;
                let Some(ch) = chars.get(i) else {
                    break;
                };
                if ch.is_ascii_lowercase() {
                    Command::MarkPosition(*ch)
                } else {
                    next_possible_command = Command::MarkPosition(' ');
                    Command::Unknown
                }
            }
            '/' => {
                if let Some(command) = parse_search_command(
                    &commands_str,
                    &chars,
                    &mut i,
                    &mut next_possible_command,
                    count,
                    Direction::Forward,
                ) {
                    command
                } else {
                    break;
                }
            }
            '?' => {
                if let Some(command) = parse_search_command(
                    &commands_str,
                    &chars,
                    &mut i,
                    &mut next_possible_command,
                    count,
                    Direction::Backward,
                ) {
                    command
                } else {
                    break;
                }
            }
            'n' => Command::RepeatSearch(count),
            'N' => Command::RepeatSearchReverse(count),
            '\'' => {
                i += 1;
                let Some(ch) = chars.get(i) else {
                    break;
                };
                match *ch {
                    '\'' => Command::ReturnPreviousPosition,
                    ch if ch.is_ascii_lowercase() => Command::ReturnMark(ch),
                    _ => {
                        next_possible_command = Command::ReturnMark(' ');
                        Command::Unknown
                    }
                }
            }
            ':' => {
                if let Some(command) = parse_transition_commands(
                    &commands_str,
                    &chars,
                    &mut i,
                    &mut next_possible_command,
                    count,
                ) {
                    command
                } else {
                    break;
                }
            }
            'Z' => {
                i += 1;
                let Some(ch) = chars.get(i) else {
                    break;
                };
                match *ch {
                    'Z' => Command::Quit,
                    _ => Command::Unknown,
                }
            }
            'v' => Command::InvokeEditor,
            '=' | '\x07' => Command::DisplayPosition,
            'q' => Command::Quit,
            _ => Command::Unknown,
        };

        i += 1;
    }

    let remainder = if i >= commands_str.len() && command == Command::Unknown {
        commands_str
    } else {
        commands_str.chars().skip(i).collect::<String>()
    };
    Ok((command, remainder, next_possible_command))
}

/// Commands usage as &[`str`]
const COMMAND_USAGE: &str = "h                              Write a summary of implementation-defined commands
[count]f or
[count]ctrl-F                  Scroll forward count lines, with one default screenful ([count] - unsigned integer)
[count]b or
[count]ctrl-B                  Scroll backward count lines, with one default screenful
[count]<space> or 
[count]j or
[count]<newline>               Scroll forward count lines. Default is one screenful
[count]k                       Scroll backward count lines. The entire count lines shall be written
[count]d or
[count]ctrl-D                  Scroll forward count lines. Default is one half of the screen size
[count]s                       Display beginning lines count screenful after current screen last line
[count]u or
[count]ctrl-U                  Scroll backward count lines. Default is one half of the screen size
[count]g                       Display the screenful beginning with line count
[count]G                       If count is specified display beginning lines or last of file screenful
r or
ctrl-L                         Refresh the screen
R                              Refresh the screen, discarding any buffered input
mletter                        Mark the current position with the letter - one lowercase letter
'letter                        Return to the position that was marked, making it as current position
''                             Return to the position from which the last large movement command was executed
[count]/[!]pattern<newline>    Display the screenful beginning with the countth line containing the pattern
[count]?[!]pattern<newline>    Display the screenful beginning with the countth previous line containing the pattern
[count]n                       Repeat the previous search for countth line containing the last pattern
[count]N                       Repeat the previous search oppositely for the countth line containing the last pattern
:e [filename]<newline>         Examine a new file. Default [filename] (current file) shall be re-examined
[count]:n                      Examine the next file. If count is specified, the countth next file shall be examined
[count]:p                      Examine the previous file. If count is specified, the countth next file shall be examined
:t tagstring<newline>          If tagstring isn't the current file, examine the file, as if :e command was executed. Display beginning screenful with the tag
v                              Invoke an editor to edit the current file being examined. Editor shall be taken from EDITOR, or shall default to vi.
= or
ctrl-G                         Write a message for which the information references the first byte of the line after the last line of the file on the screen
q or
:q or
ZZ                             Exit more\n
For more see: https://pubs.opengroup.org/onlinepubs/9699919799.2018edition/utilities/more.html\n";

/// Returns formated [`COMMAND_USAGE`]
pub fn commands_usage() -> String {
    let mut buf = String::new();
    let delimiter = "-".repeat(79) + "\n";
    let delimiter = delimiter.as_str();
    buf.push_str(delimiter);
    buf.push_str(COMMAND_USAGE);
    buf.push_str(delimiter);
    buf
}

/// Parse arguments with MORE environment variable support.
/// POSIX specifies: "more $MORE options operands"
/// Command line options override those from MORE.
fn parse_args_with_more_env() -> Args {
    // Get the MORE environment variable
    if let Ok(more_env) = std::env::var("MORE") {
        // Parse MORE variable into args
        let more_args: Vec<String> = more_env.split_whitespace().map(String::from).collect();

        if !more_args.is_empty() {
            // Get actual command line args (skip program name)
            let cmd_args: Vec<String> = std::env::args().collect();

            // Build combined args: program name, MORE args, then command line args
            let mut combined_args = vec![cmd_args[0].clone()];
            combined_args.extend(more_args);
            combined_args.extend(cmd_args.into_iter().skip(1));

            return Args::parse_from(combined_args);
        }
    }

    Args::parse()
}

fn main() {
    let _ = setlocale(
        LocaleCategory::LcAll,
        std::env::var("LC_ALL").unwrap_or("".to_string()),
    );
    let _ = textdomain(PROJECT_NAME);
    let _ = bind_textdomain_codeset(PROJECT_NAME, "UTF-8");

    let args = parse_args_with_more_env();
    match MoreControl::new(args) {
        Ok(mut ctl) => {
            if ctl.terminal.is_none() {
                ctl.print_all_input();
            } else {
                ctl.loop_();
            }
        }
        Err(error) => {
            eprintln!("{}", error);
            println!("{}", error);
            std::process::exit(1);
        }
    }
}
