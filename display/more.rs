//
// Copyright (c) 2024-2026 Jeff Garzik
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
use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::{stdout, BufRead, BufReader, Cursor, Read, Seek, SeekFrom, Write};
use std::mem::MaybeUninit;
use std::ops::Not;
use std::os::fd::{AsRawFd, FromRawFd, RawFd};
use std::path::{Path, PathBuf};
use std::process::{exit, ExitStatus};
use std::str::FromStr;
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::sync::mpsc::{channel, Receiver, TryRecvError};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use termion::{clear::*, cursor::*, event::*, input::*, screen::*, style::*, *};

const LINES_PER_PAGE: u16 = 24;
const NUM_COLUMNS: u16 = 80;
const DEFAULT_EDITOR: &str = "vi";
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

    /// Enable interactive session test.  Not part of the POSIX option set:
    /// hidden from `--help` so it does not read as a supported interface.
    #[arg(
        short = 'd',
        long = "test",
        hide = true,
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
///
/// `Display` is written by hand rather than derived so that every diagnostic
/// passes through `gettext`, which POSIX 107344-107346 requires of
/// `LC_MESSAGES`-affected text.  (Derive attributes are evaluated at compile
/// time and cannot call it.)
#[derive(Debug, Clone, thiserror::Error)]
enum MoreError {
    /// Errors raised in [`SeekPositions`] level
    SeekPositions(#[from] SeekPositionsError),
    /// Errors raised in [`SourceContext`] level
    SourceContext(#[from] SourceContextError),
    /// Attempt set [`String`] on [`Terminal`] that goes beyond
    SetOutside,
    /// Read [`std::io::Stdin`] is failed
    InputRead,
    /// Stdout is a terminal but no readable channel exists for user commands
    /// (neither stderr nor `/dev/tty`).  POSIX requires `more` to terminate
    /// with an error in this case (see INPUT FILES in the POSIX.1-2024 spec).
    NoCommandSource,
    /// Calling [`std::process::Command`] for editor is failed
    EditorFailed,
    /// Open, read [`File`] is failed
    FileRead(String),
    /// [`Output`], [`Regex`] parse errors
    StringParse(String),
    /// Attempt execute [`Command::UnknownCommand`]
    UnknownCommand,
    /// [`Terminal`] init is failed
    TerminalInit,
    /// [`Terminal`] size is too small
    TerminalOutput,
    /// [`Terminal`] size read is failed
    SizeRead,
    /// Attempt update [`SourceContext::current_screen`] without [`Terminal`]
    MissingTerminal,
    /// Search has no results
    PatternNotFound(String),
}

impl std::fmt::Display for MoreError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SeekPositions(e) => write!(f, "{e}"),
            Self::SourceContext(e) => write!(f, "{e}"),
            Self::SetOutside => write!(f, "{}", gettext("Set chars outside screen is forbidden")),
            Self::InputRead => write!(f, "{}", gettext("Couldn't read from stdin")),
            Self::NoCommandSource => write!(
                f,
                "{}",
                gettext("Cannot read user commands: neither stderr nor /dev/tty is available")
            ),
            Self::EditorFailed => write!(f, "{}", gettext("Editor process failed")),
            Self::FileRead(name) => {
                write!(f, "{} \'{name}\'", gettext("Couldn't read file"))
            }
            Self::StringParse(what) => write!(f, "{} {what}", gettext("Couldn't parse")),
            Self::UnknownCommand => write!(f, "{}", gettext("Couldn't execute unknown command")),
            Self::TerminalInit => write!(f, "{}", gettext("Terminal isn't initialized")),
            Self::TerminalOutput => write!(
                f,
                "{}",
                gettext("Can't execute commands for too small terminal")
            ),
            Self::SizeRead => write!(f, "{}", gettext("Couldn't get current terminal size")),
            Self::MissingTerminal => write!(f, "{}", gettext("Terminal operations is forbidden")),
            Self::PatternNotFound(pattern) => {
                write!(
                    f,
                    "{} \'{pattern}\' {}",
                    gettext("Couldn't find"),
                    gettext("pattern")
                )
            }
        }
    }
}

/// All [`SeekPositions`] errors
#[derive(Debug, Clone, thiserror::Error)]
enum SeekPositionsError {
    /// Attempt seek buffer out of bounds
    OutOfRange(u64),
    /// Source open, read errors
    FileRead(String),
}

impl std::fmt::Display for SeekPositionsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OutOfRange(position) => {
                write!(
                    f,
                    "{} {position} {}",
                    gettext("Couldn't seek to"),
                    gettext("position")
                )
            }
            Self::FileRead(name) => write!(f, "{} {name}", gettext("Couldn't read")),
        }
    }
}

/// All [`SourceContext`] errors
#[derive(Debug, Clone, thiserror::Error)]
enum SourceContextError {
    /// Attempt execute previous search when it is [`None`]
    MissingLastSearch,
    /// Attempt move current position to mark when it isn`t set
    MissingMark(char),
}

impl std::fmt::Display for SourceContextError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingLastSearch => write!(f, "{}", gettext("No previous regular expression")),
            Self::MissingMark(mark) => {
                write!(f, "{} \'{mark}", gettext("Couldn't find mark for"))
            }
        }
    }
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
    ///
    /// The screen is a grid of cells, so the bound is the number of cells the
    /// text occupies, not its length in bytes: an 80-column line of non-ASCII
    /// text is many more bytes than columns and used to be rejected outright.
    /// Text that does not fit is written up to the edge rather than refused —
    /// a caller asking to draw past the margin wants the visible part drawn.
    fn set_str(
        &mut self,
        position: (usize, usize),
        string: String,
        style: StyleType,
    ) -> Result<(), MoreError> {
        if position.0 >= self.0.len() {
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
    ///
    /// Bounded by cells and clamped, for the same reasons as [`Self::set_str`].
    fn set_raw(
        &mut self,
        position: (usize, usize),
        string: Vec<(char, StyleType)>,
    ) -> Result<(), MoreError> {
        if position.0 >= self.0.len() {
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
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
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
/// Continuation cell occupying the second column of a double-width character.
/// [`Terminal::display`] skips these; the character itself was already drawn.
const CELL_CONT: char = '\u{0}';

/// Default <tab> stop interval.
const TAB_STOP: usize = 8;

/// How a source line is turned into display cells.
#[derive(Clone, Copy)]
struct RenderOpts {
    /// `-u`: resolve overstrike sequences as usual, but draw everything
    /// unstyled.  Layout is identical with and without it.
    plain: bool,
}

/// One source line (or one folded piece of one) as it appears on screen.
struct RenderedLine {
    /// One entry per display column.
    cells: Vec<(char, StyleType)>,
    /// Source bytes this consumed.  Kept separate from the column count:
    /// a byte can occupy four columns (`\377`) and a character can occupy
    /// two, so the two quantities are unrelated.
    bytes: usize,
    /// The piece ended because a <newline> was consumed.
    ended_at_newline: bool,
}

/// A decoded input unit: either a character, or a byte that is not part of a
/// valid character.
#[derive(Clone, Copy, PartialEq)]
enum Glyph {
    Ch(char),
    Byte(u8),
}

/// A unit after overstrike resolution: what to draw, how, and how many source
/// bytes it accounts for (including any discarded overstrike material).
struct Piece {
    glyph: Glyph,
    style: StyleType,
    bytes: usize,
}

/// Decode one UTF-8 character from the front of `src`.
///
/// Returns `None` when `src` holds only part of a sequence, so the caller can
/// fetch more input.  UTF-8 is decoded regardless of `LC_CTYPE`: text is
/// overwhelmingly UTF-8 even under `LC_ALL=C`, and escaping it there would
/// make ordinary content unreadable.  Bytes that are not valid UTF-8 are
/// reported one at a time and end up as octal escapes.
fn decode_glyph(src: &[u8]) -> Option<(Glyph, usize)> {
    let first = src[0];
    let len = match first {
        0x00..=0x7f => 1,
        0xc2..=0xdf => 2,
        0xe0..=0xef => 3,
        0xf0..=0xf4 => 4,
        _ => return Some((Glyph::Byte(first), 1)),
    };

    if src.len() < len {
        return None;
    }

    match std::str::from_utf8(&src[..len]) {
        Ok(text) => text.chars().next().map(|c| (Glyph::Ch(c), len)),
        Err(_) => Some((Glyph::Byte(first), 1)),
    }
}

/// Display width of a character, never less than one column.
fn glyph_width(c: char) -> usize {
    match plib::locale::wcwidth_char(c) {
        w if w > 0 => w as usize,
        // Combining marks and characters the locale cannot measure still need
        // somewhere to go; a column each is the safe answer.
        _ => 1,
    }
}

/// Render a non-printable character in the `ed` list format that POSIX
/// 107450-107452 points at: the named escapes of XBD Table 5-1, otherwise one
/// three-digit octal number per byte of the character.
///
/// Unlike `ed`'s `l` command this does not escape a literal <backslash> or
/// mark the end of line: `l` renders whole lines unambiguously, whereas 107450
/// scopes escaping to "other non-printable characters" only.  Escaping every
/// backslash in a pager would be noise.
fn escape_glyph(glyph: Glyph, out: &mut Vec<char>) {
    let named = |c: char| -> Option<&'static str> {
        Some(match c {
            '\x07' => "\\a",
            '\x08' => "\\b",
            '\x0c' => "\\f",
            '\r' => "\\r",
            '\x0b' => "\\v",
            _ => return None,
        })
    };

    match glyph {
        Glyph::Ch(c) => {
            if let Some(text) = named(c) {
                out.extend(text.chars());
                return;
            }
            let mut buf = [0u8; 4];
            for byte in c.encode_utf8(&mut buf).as_bytes() {
                out.extend(format!("\\{byte:03o}").chars());
            }
        }
        Glyph::Byte(b) => out.extend(format!("\\{b:03o}").chars()),
    }
}

/// Resolve the overstrike conventions of POSIX 107432-107447 over a decoded
/// line, producing the units to draw.
///
/// Bytes that are discarded (backspaces and the characters they erase) are
/// attributed to the *following* unit, so that re-rendering from any byte
/// offset reproduces the same units — which is what keeps the seek positions
/// exact across folds.
fn resolve_overstrike(glyphs: &[(Glyph, usize)], plain: bool) -> Vec<Piece> {
    let mut pieces: Vec<Piece> = Vec::with_capacity(glyphs.len());
    let mut carried = 0usize;
    let mut i = 0;

    let is = |idx: usize, c: char| glyphs.get(idx).map(|g| g.0) == Some(Glyph::Ch(c));
    let run = |mut idx: usize, c: char| {
        let mut n = 0;
        while is(idx, c) {
            n += 1;
            idx += 1;
        }
        n
    };
    let bytes_of = |from: usize, to: usize| -> usize {
        glyphs[from..to.min(glyphs.len())].iter().map(|g| g.1).sum()
    };

    while i < glyphs.len() {
        let (glyph, len) = glyphs[i];

        // A backspace with nothing to erase, or one left over after the forms
        // below: discard it together with the character it follows
        // (107446-107447).
        if glyph == Glyph::Ch('\x08') {
            carried += len;
            if let Some(prev) = pieces.pop() {
                carried += prev.bytes;
            }
            i += 1;
            continue;
        }

        let width = match glyph {
            Glyph::Ch(c) if !c.is_control() => glyph_width(c),
            _ => 1,
        };

        // "_"*n <backspace>*n <char of width n> -- underline written the other
        // way round.  Checked before the embolden form so that "_\b_"
        // underlines rather than emboldening.
        if glyph == Glyph::Ch('_') {
            let underscores = run(i, '_');
            let after = i + underscores;
            if run(after, '\x08') >= underscores {
                let target = after + underscores;
                if let Some(&(Glyph::Ch(c), _)) = glyphs.get(target) {
                    if !c.is_control() && glyph_width(c) == underscores {
                        pieces.push(Piece {
                            glyph: Glyph::Ch(c),
                            style: if plain {
                                StyleType::None
                            } else {
                                StyleType::Underscore
                            },
                            bytes: carried + bytes_of(i, target + 1),
                        });
                        carried = 0;
                        i = target + 1;
                        continue;
                    }
                }
            }
        }

        let backspaces = run(i + 1, '\x08');
        if backspaces == width {
            let after = i + 1 + backspaces;

            // <char> <backspace>*n "_"*n -- underline.
            if run(after, '_') >= width {
                pieces.push(Piece {
                    glyph,
                    style: if plain {
                        StyleType::None
                    } else {
                        StyleType::Underscore
                    },
                    bytes: carried + bytes_of(i, after + width),
                });
                carried = 0;
                i = after + width;
                continue;
            }

            // <char> <backspace>*n <same char> -- embolden.  Further
            // <backspace>*n <same char> pairs are absorbed, so "a\ba\ba\ba" is
            // a single emboldened 'a' (107443-107445).
            if glyphs.get(after).map(|g| g.0) == Some(glyph) {
                let mut end = after + 1;
                loop {
                    let next = end + width;
                    if run(end, '\x08') >= width && glyphs.get(next).map(|g| g.0) == Some(glyph) {
                        end = next + 1;
                    } else {
                        break;
                    }
                }
                pieces.push(Piece {
                    glyph,
                    style: if plain {
                        StyleType::None
                    } else {
                        StyleType::Negative
                    },
                    bytes: carried + bytes_of(i, end),
                });
                carried = 0;
                i = end;
                continue;
            }
        }

        pieces.push(Piece {
            glyph,
            style: StyleType::None,
            bytes: carried + len,
        });
        carried = 0;
        i += 1;
    }

    // Trailing material that erased everything after it still has to be
    // accounted for, or the cursor would never move past it.
    if carried > 0 {
        pieces.push(Piece {
            glyph: Glyph::Ch('\0'),
            style: StyleType::None,
            bytes: carried,
        });
    }

    pieces
}

/// Render one display line from the front of `src`.
///
/// Stops at a <newline>, when `cols` columns are filled, or when the input
/// runs out.  Returns `None` when more input is needed to decide — the caller
/// then supplies a longer slice.  At end of input it always returns a line.
///
/// `bytes` and the column count are tracked separately: this is the whole
/// point of the function.  A control byte occupies four columns as `\001`, a
/// <tab> occupies up to eight, and a double-width character occupies two, so
/// no single number can serve as both a fold width and a seek offset.
fn render_display_line(
    src: &[u8],
    cols: Option<usize>,
    opts: RenderOpts,
    at_eof: bool,
) -> Option<RenderedLine> {
    // Decode the whole slice up front; overstrike needs lookahead.
    let mut glyphs: Vec<(Glyph, usize)> = Vec::new();
    let mut consumed = 0usize;
    let mut ended_at_newline = false;

    while consumed < src.len() {
        let Some((glyph, len)) = decode_glyph(&src[consumed..]) else {
            if at_eof {
                // A truncated sequence at end of input is just bytes.
                glyphs.push((Glyph::Byte(src[consumed]), 1));
                consumed += 1;
                continue;
            }
            return None;
        };

        // A <carriage-return> immediately before the <newline> that ends the
        // line is ignored rather than written (107448-107449).  Anywhere else
        // it is an ordinary non-printable.  Resolved here, before overstrike,
        // so it can never take part in a backspace sequence.
        if glyph == Glyph::Ch('\r') {
            match src.get(consumed + len) {
                Some(b'\n') => {
                    consumed += len + 1;
                    ended_at_newline = true;
                    break;
                }
                None if !at_eof => return None,
                None => {
                    consumed += len;
                    break;
                }
                _ => {}
            }
        }

        consumed += len;
        if glyph == Glyph::Ch('\n') {
            ended_at_newline = true;
            break;
        }
        glyphs.push((glyph, len));
    }

    if !ended_at_newline && !at_eof {
        // The line may continue in bytes we have not been given yet.
        return None;
    }

    let pieces = resolve_overstrike(&glyphs, opts.plain);

    // Lay the pieces out, stopping at the column budget.  A piece is atomic:
    // if its expansion does not fit it is not emitted and its bytes are not
    // consumed, so it begins the next display line rather than being split or
    // dropped (107452-107453).
    let budget = cols.unwrap_or(usize::MAX);
    let mut cells: Vec<(char, StyleType)> = Vec::new();
    let mut bytes = 0usize;
    let mut folded = false;

    for piece in &pieces {
        let mut expansion: Vec<char> = Vec::new();
        match piece.glyph {
            // The placeholder used for trailing discarded material.
            Glyph::Ch('\0') => {}
            Glyph::Ch('\t') => {
                let next_stop = (cells.len() / TAB_STOP + 1) * TAB_STOP;
                expansion.resize(next_stop.saturating_sub(cells.len()).max(1), ' ');
            }
            Glyph::Ch(c) if !c.is_control() => {
                expansion.push(c);
                expansion.resize(glyph_width(c), CELL_CONT);
            }
            other => escape_glyph(other, &mut expansion),
        }

        // Nothing emitted yet and it still does not fit? Emit anyway rather
        // than making no progress; the screen layer clamps.
        if !expansion.is_empty() && cells.len() + expansion.len() > budget && !cells.is_empty() {
            folded = true;
            break;
        }

        for ch in expansion {
            cells.push((ch, piece.style));
        }
        bytes += piece.bytes;
    }

    if !folded && ended_at_newline {
        // The <newline> (and any <carriage-return> before it) is consumed by
        // this piece even though it draws nothing.
        bytes = consumed;
    }

    Some(RenderedLine {
        cells,
        bytes,
        ended_at_newline: ended_at_newline && !folded,
    })
}

trait SeekRead: Seek + Read {}

impl<T: Seek + Read> SeekRead for Box<T> {}
impl SeekRead for File {}
impl SeekRead for Cursor<String> {}
impl SeekRead for SpillReader {}

/// Backing store that makes a non-seekable input seekable.
///
/// `more` must be able to seek: it shows a screenful at a time and lets the
/// user move back.  Holding the whole input in memory would cap the input size
/// at available RAM, which for a pager fed by a pipe is the wrong bound
/// entirely.  Instead, bytes are appended to an unlinked temporary file as
/// they are read, and seeks are served from that file, so only the portion the
/// user has actually reached is ever stored.
struct Spill {
    /// Upstream reader; `None` once it has reported end-of-file.
    upstream: Option<Box<dyn Read + Send>>,
    /// Everything read so far.  `tempfile()` unlinks on creation, so the file
    /// cannot outlive the process on any exit path, including a signal.
    file: File,
    /// Bytes pulled from `upstream` and written to `file`.
    written: u64,
}

impl Spill {
    fn new(upstream: Box<dyn Read + Send>) -> std::io::Result<Self> {
        Ok(Self {
            upstream: Some(upstream),
            file: tempfile::tempfile()?,
            written: 0,
        })
    }

    /// Total size, known only once upstream has been drained.
    fn total(&self) -> Option<u64> {
        self.upstream.is_none().then_some(self.written)
    }

    /// Pull from upstream until `target` bytes are stored, or EOF.
    fn fill_to(&mut self, target: u64) -> std::io::Result<()> {
        let mut chunk = [0u8; 8192];
        while self.written < target {
            let Some(upstream) = self.upstream.as_mut() else {
                break;
            };
            let n = upstream.read(&mut chunk)?;
            if n == 0 {
                self.upstream = None;
                break;
            }
            self.file.seek(SeekFrom::Start(self.written))?;
            self.file.write_all(&chunk[..n])?;
            self.written += n as u64;
        }
        Ok(())
    }

    /// Read at an absolute offset, pulling more input first if needed.
    /// A short read of 0 therefore means genuine end-of-input.
    fn read_at(&mut self, pos: u64, buf: &mut [u8]) -> std::io::Result<usize> {
        self.fill_to(pos.saturating_add(buf.len() as u64))?;
        if pos >= self.written {
            return Ok(0);
        }
        let n = buf.len().min((self.written - pos) as usize);
        self.file.seek(SeekFrom::Start(pos))?;
        self.file.read_exact(&mut buf[..n])?;
        Ok(n)
    }
}

/// A [`Spill`] shared by every cursor over the same input.
#[derive(Clone)]
struct SharedSpill(Arc<Mutex<Spill>>);

impl std::fmt::Debug for SharedSpill {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("SharedSpill")
    }
}

impl SharedSpill {
    fn new(upstream: Box<dyn Read + Send>) -> std::io::Result<Self> {
        Ok(Self(Arc::new(Mutex::new(Spill::new(upstream)?))))
    }

    fn total(&self) -> Option<u64> {
        self.0.lock().ok()?.total()
    }

    fn reader(&self) -> SpillReader {
        SpillReader {
            spill: self.clone(),
            pos: 0,
        }
    }
}

/// An independent seekable cursor over a [`SharedSpill`].
struct SpillReader {
    spill: SharedSpill,
    pos: u64,
}

impl Read for SpillReader {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let mut spill = self
            .spill
            .0
            .lock()
            .map_err(|_| std::io::Error::other("spill lock poisoned"))?;
        let n = spill.read_at(self.pos, buf)?;
        drop(spill);
        self.pos += n as u64;
        Ok(n)
    }
}

impl Seek for SpillReader {
    fn seek(&mut self, from: SeekFrom) -> std::io::Result<u64> {
        self.pos = match from {
            SeekFrom::Start(n) => n,
            SeekFrom::Current(delta) => self.pos.saturating_add_signed(delta),
            SeekFrom::End(delta) => {
                // Locating the end is the one operation that must drain the
                // input; it only happens because the user asked to go there.
                let mut spill = self
                    .spill
                    .0
                    .lock()
                    .map_err(|_| std::io::Error::other("spill lock poisoned"))?;
                spill.fill_to(u64::MAX)?;
                spill.written.saturating_add_signed(delta)
            }
        };
        Ok(self.pos)
    }
}

/// Universal cursor that can read lines, seek
/// over any [`SeekRead`] source
struct SeekPositions {
    /// Buffer with previous seek positions of all lines beginnings
    positions: Vec<u64>,
    /// Terminal width for spliting long lines. If [`None`], lines not splited by length
    line_len: Option<usize>,
    /// Total size of the source in bytes, when it is knowable without reading
    /// everything.  `None` for a source whose length cannot be established up
    /// front; POSIX 107631-107632 permits omitting the figures derived from it.
    total_bytes: Option<u64>,
    /// Source that handles info for creating [`SeekRead`] buffer
    source: Source,
    /// Buffer for which is seek and read is applied  
    buffer: Box<dyn SeekRead>,
    /// Shrink all sequences of <newline>'s to one <newline>
    squeeze_lines: bool,
    /// `-u`: suppress underlining and bold
    plain: bool,
    /// Iteration over [`SeekPositions`] buffer has reached end
    is_ended: bool,
}

impl SeekPositions {
    /// Creates new [`SeekPositions`]
    fn new(
        source: Source,
        line_len: Option<usize>,
        squeeze_lines: bool,
        plain: bool,
    ) -> Result<Self, MoreError> {
        // Sizing is O(1): the file's metadata, or the buffer's length.  more
        // must never read a source through in order to start displaying it —
        // the input can be arbitrarily large.
        let (buffer, total_bytes): (Box<dyn SeekRead>, Option<u64>) = match source.clone() {
            Source::File(path) => {
                let Ok(file) = File::open(path.clone()) else {
                    return Err(MoreError::SeekPositions(SeekPositionsError::FileRead(
                        path.to_str().unwrap_or("<file>").to_string(),
                    )));
                };
                let total = file.metadata().ok().map(|meta| meta.len());
                (Box::new(file), total)
            }
            Source::Buffer(buffer) => {
                let total = Some(buffer.get_ref().len() as u64);
                (Box::new(buffer), total)
            }
            // Size is unknown until the input has been drained, which more
            // must not do just to start displaying.
            Source::Stream(spill) => (Box::new(spill.reader()), None),
        };

        Ok(Self {
            // The first line starts at offset 0, so the cursor begins on line
            // 1.  (The scan this replaced left `positions` in exactly this
            // state as a side effect; stating it is clearer than deriving it.)
            positions: vec![0],
            line_len,
            total_bytes,
            source,
            buffer,
            squeeze_lines,
            plain,
            is_ended: false,
        })
    }

    /// Render the display line at `start`.
    ///
    /// The renderer is a pure function over bytes, so this only has to feed
    /// it enough of them: it asks for more whenever the slice ends mid-line
    /// or mid-character, which terminates because the request grows.
    fn render_at(&mut self, start: u64) -> Option<RenderedLine> {
        let mut chunk = 1024usize;
        loop {
            if self.buffer.seek(SeekFrom::Start(start)).is_err() {
                return None;
            }

            let mut buf = vec![0u8; chunk];
            let mut filled = 0usize;
            while filled < chunk {
                match self.buffer.read(&mut buf[filled..]) {
                    Ok(0) => break,
                    Ok(n) => filled += n,
                    Err(_) => return None,
                }
            }
            buf.truncate(filled);
            let at_eof = filled < chunk;

            let opts = RenderOpts { plain: self.plain };
            if let Some(line) = render_display_line(&buf, self.line_len, opts, at_eof) {
                if at_eof && line.bytes >= filled {
                    self.is_ended = true;
                }
                return Some(line);
            }
            if at_eof {
                return None;
            }
            chunk *= 4;
        }
    }

    /// The display line at the current position, as cells.
    fn render_current_line(&mut self) -> Option<RenderedLine> {
        let start = self.current();
        self.render_at(start)
    }

    /// The display line at the current position, as text.
    ///
    /// Used for searching, so it is the text the user can see: escapes and
    /// tab expansion included, overstrike control characters removed.
    fn read_line(&mut self) -> Result<String, MoreError> {
        Ok(self
            .render_current_line()
            .map(|line| {
                line.cells
                    .iter()
                    .map(|(c, _)| *c)
                    .filter(|c| *c != CELL_CONT)
                    .collect()
            })
            .unwrap_or_default())
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

    /// Total size of the source, if that is knowable.
    ///
    /// A file or an in-memory buffer knows it up front; a stream only learns
    /// it once its input has been drained, so it is queried rather than
    /// cached.
    fn total_bytes(&self) -> Option<u64> {
        match &self.source {
            Source::Stream(spill) => spill.total(),
            _ => self.total_bytes,
        }
    }

    /// Percentage of the source preceding the current position, when the
    /// total size is known.
    ///
    /// POSIX 107630-107632 asks for "what percentage of the file precedes the
    /// current position", but permits omitting it when reading from standard
    /// input.  Measuring in *bytes* rather than lines is what keeps this O(1):
    /// a total line count cannot be had without reading the whole source.
    fn percent(&self) -> Option<u8> {
        let total = self.total_bytes()?;
        if total == 0 {
            return None;
        }
        Some(((u128::from(self.current()) * 100 / u128::from(total)).min(100)) as u8)
    }

    /// Drive the source to its end.
    ///
    /// This is the only operation that must read everything, and it is only
    /// ever reached because the user asked to go to the end (`G`) or because
    /// the display ran off it.  `set_current` stops and sets `is_ended` when
    /// the source is exhausted, so `usize::MAX` means "as far as this goes".
    fn goto_end(&mut self) {
        self.set_current(usize::MAX);
    }

    /// Seek to certain [`position`] over current source
    fn seek(&mut self, position: u64) -> Result<(), MoreError> {
        let err = Err(MoreError::SeekPositions(SeekPositionsError::OutOfRange(
            position,
        )));
        if self.total_bytes().is_some_and(|total| position > total) {
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
}

impl Iterator for SeekPositions {
    type Item = u64;

    /// Iter over [`SeekRead`] buffer lines in forward direction
    fn next(&mut self) -> Option<Self::Item> {
        let current_position = self.current();
        let line = self.render_at(current_position)?;
        if line.bytes == 0 {
            self.is_ended = true;
            return None;
        }

        let mut next_position = current_position + line.bytes as u64;

        // `-s`: a run of empty lines is displayed as one, so the run's bytes
        // are consumed together.  This lives here rather than in the renderer
        // because it spans lines.
        if self.squeeze_lines && line.ended_at_newline && line.cells.is_empty() {
            while let Some(extra) = self.render_at(next_position) {
                if extra.bytes == 0 || !extra.ended_at_newline || !extra.cells.is_empty() {
                    break;
                }
                next_position += extra.bytes as u64;
            }
        }

        if self
            .total_bytes()
            .is_some_and(|total| next_position >= total)
        {
            self.is_ended = true;
            let _ = self.buffer.seek(SeekFrom::Start(current_position));
            return None;
        }

        if self.buffer.seek(SeekFrom::Start(next_position)).is_err() {
            return None;
        }
        self.positions.push(next_position);
        Some(next_position)
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
    /// A non-seekable input (standard input) made seekable by spilling what
    /// has been read to a temporary file.  Shared, so that every cursor over
    /// the same input reads the same bytes exactly once.
    Stream(SharedSpill),
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
                // set_current clamps at the end of the source, so there is
                // no need to know the total line count to avoid overrunning.
                self.seek_positions.set_current(current_line + remain);
                current_line = self.seek_positions.current_line();
                content_lines_len = current_line;
            }
        }

        // Collect the visible lines as cells.  Style travels inside the cells,
        // so there is no second pass to map byte ranges onto column indices.
        let mut i = 0;
        // Guard the subtraction: a source with nothing to show yields zero
        // content lines, and `0 - 1` would wrap to usize::MAX.
        while i + 1 < content_lines_len {
            content_lines.push(self.seek_positions.render_current_line());
            if self.seek_positions.next_back().is_none() {
                break;
            }
            i += 1;
        }
        content_lines.push(self.seek_positions.render_current_line());
        content_lines.reverse();

        self.seek_positions.set_current(current_line);

        let previous_lines_len = previous_lines.len();
        for (i, line) in previous_lines.into_iter().enumerate() {
            screen.set_raw((i, 0), line)?
        }

        for (i, line) in header_lines.into_iter().enumerate() {
            screen.set_str((i + previous_lines_len, 0), line, StyleType::None)?;
        }

        for (i, line) in content_lines.into_iter().enumerate() {
            let cells = line.map(|l| l.cells).unwrap_or_default();
            screen.set_raw((i + previous_lines_len + header_lines_len, 0), cells)?;
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
        self.seek_positions.set_current(next_line);
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
        self.seek_positions.goto_end();
        self.is_ended_file = self.seek_positions.is_ended;
    }

    /// Number of content lines a screenful holds, used to decide whether a
    /// movement counts as "large" for the `''` command.
    pub fn screen_lines(&self) -> usize {
        self.terminal_size
            .map(|(lines, _)| lines.saturating_sub(1).max(1))
            .unwrap_or(1)
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
            let direction = if is_reversed { !*direction } else { *direction };
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

// === Signal handlers ============================================
//
// Why this exists:
//   * POSIX `more` ASYNCHRONOUS EVENTS mandates handling SIGCONT (refetch
//     winsize + redraw) and SIGWINCH (same, after Austin Group Defect 1185).
//   * Without lifecycle-signal handlers, a SIGINT / SIGTERM / SIGHUP /
//     SIGQUIT skips Rust's `Drop`, so the cooked-mode termios captured by
//     [`CommandIO`] is never restored — leaving the user's shell unusable.
//
// Constraints:
//   * Handlers run in async-signal context.  Only a tiny subset of libc is
//     safe: `tcsetattr`, `_exit`, `raise`, `sigaction`, `write`, atomics.
//     No mallocs, no Rust `Mutex` (parking-lot or std), no `println!`.
//   * Cleanup handlers (TERM/INT/HUP/QUIT) call `tcsetattr` and `_exit`
//     directly — there is no main-loop tick guaranteed before process death.
//   * Event handlers (CONT/WINCH) just set an atomic flag; the pager loop
//     polls these and performs the user-visible work in normal context.
//
// State synchronization:
//   * `SIG_STATE_INIT` is the gate: a handler that observes `false` must
//     not touch [`SAVED_FD`], [`SAVED_COOKED`], or [`SAVED_RAW`].
//   * State is written **before** `SIG_STATE_INIT` is set to `true`
//     (publish), and `SIG_STATE_INIT` is cleared **before** the state is
//     torn down (unpublish).  Both orderings use `SeqCst`.
//   * The termios snapshots are written exactly once by
//     [`publish_signal_state`] and read by handlers; they are not mutated
//     thereafter, so a non-atomic memcpy is safe under this discipline.

static SIG_STATE_INIT: AtomicBool = AtomicBool::new(false);
static SAVED_FD: AtomicI32 = AtomicI32::new(-1);
static mut SAVED_COOKED: MaybeUninit<libc::termios> = MaybeUninit::uninit();
static mut SAVED_RAW: MaybeUninit<libc::termios> = MaybeUninit::uninit();

/// Set by the SIGCONT handler.  Main loop swaps this to `false` and forces a
/// full redraw plus winsize refetch (POSIX: SIGCONT must refresh the screen
/// regardless of whether the size actually changed).
static SIGCONT_PENDING: AtomicBool = AtomicBool::new(false);
/// Set by the SIGWINCH handler.  Main loop swaps this and re-fetches winsize.
static SIGWINCH_PENDING: AtomicBool = AtomicBool::new(false);

/// Publish the fd + cooked/raw termios snapshots that the handlers read.
/// Call this **once**, before [`install_signal_handlers`].
///
/// # Safety
///
/// Caller must guarantee no handler is currently running on the static state
/// (which is true at startup before `install_signal_handlers`).
unsafe fn publish_signal_state(fd: RawFd, cooked: libc::termios, raw: libc::termios) {
    // Write through raw pointers to avoid forming references to the
    // mutable statics (deny-by-default in Rust 2024; warning under 2021).
    let cooked_ptr = std::ptr::addr_of_mut!(SAVED_COOKED);
    let raw_ptr = std::ptr::addr_of_mut!(SAVED_RAW);
    (*cooked_ptr).write(cooked);
    (*raw_ptr).write(raw);
    SAVED_FD.store(fd, Ordering::SeqCst);
    SIG_STATE_INIT.store(true, Ordering::SeqCst);
}

/// Clear the published signal state.  Call this **after**
/// [`uninstall_signal_handlers`] so no in-flight handler races us.
fn unpublish_signal_state() {
    SIG_STATE_INIT.store(false, Ordering::SeqCst);
    SAVED_FD.store(-1, Ordering::SeqCst);
    // No need to clear SAVED_COOKED / SAVED_RAW — handlers gate on
    // SIG_STATE_INIT before reading them.
}

/// Try to restore the cooked-mode termios.  Async-signal-safe: only calls
/// `tcsetattr` and atomic loads.  No-op if state hasn't been published.
fn try_restore_cooked() {
    if !SIG_STATE_INIT.load(Ordering::SeqCst) {
        return;
    }
    let fd = SAVED_FD.load(Ordering::SeqCst);
    if fd < 0 {
        return;
    }
    // SAFETY: SIG_STATE_INIT was true ⇒ publish_signal_state ran ⇒
    // SAVED_COOKED is initialized and never mutated thereafter.  We hand a
    // raw pointer to libc to avoid forming a reference to mutable static.
    unsafe {
        let ptr = std::ptr::addr_of!(SAVED_COOKED) as *const libc::termios;
        libc::tcsetattr(fd, libc::TCSAFLUSH, ptr);
    }
}

/// Try to (re)apply the raw-mode termios.  Used by the SIGCONT handler to
/// re-engage raw mode after a SIGTSTP/SIGCONT job-control cycle.
fn try_apply_raw() {
    if !SIG_STATE_INIT.load(Ordering::SeqCst) {
        return;
    }
    let fd = SAVED_FD.load(Ordering::SeqCst);
    if fd < 0 {
        return;
    }
    // SAFETY: see [`try_restore_cooked`].
    unsafe {
        let ptr = std::ptr::addr_of!(SAVED_RAW) as *const libc::termios;
        libc::tcsetattr(fd, libc::TCSAFLUSH, ptr);
    }
}

/// Handler for SIGINT, SIGTERM, SIGHUP, SIGQUIT.  Restores cooked-mode
/// termios and then `_exit`s with `128 + signum` (the POSIX shell
/// convention for signal-caused exits).  Drop impls are intentionally
/// skipped — by the time these signals arrive we want to die *now*, before
/// any further harm.
extern "C" fn restore_and_exit(signum: libc::c_int) {
    try_restore_cooked();
    unsafe { libc::_exit(128 + signum) };
}

/// Handler for SIGTSTP (job-control stop, typically Ctrl-Z).  Restores
/// cooked mode so the parent shell finds the terminal usable, then resets
/// SIGTSTP to its default disposition and re-raises it so the kernel
/// actually suspends us.  On resume, [`handle_cont`] re-engages raw mode
/// and re-installs this handler.
extern "C" fn handle_tstp(_signum: libc::c_int) {
    try_restore_cooked();
    unsafe {
        let mut act: libc::sigaction = std::mem::zeroed();
        act.sa_sigaction = libc::SIG_DFL;
        libc::sigemptyset(&mut act.sa_mask);
        libc::sigaction(libc::SIGTSTP, &act, std::ptr::null_mut());
        libc::raise(libc::SIGTSTP);
    }
}

/// Handler for SIGCONT (resume from suspend).  Re-applies raw mode (we
/// were stopped in cooked mode by [`handle_tstp`]) and sets the pending
/// flag so the main loop refreshes the screen.  Also re-installs the
/// SIGTSTP handler that [`handle_tstp`] reset to `SIG_DFL`.
extern "C" fn handle_cont(_signum: libc::c_int) {
    try_apply_raw();
    SIGCONT_PENDING.store(true, Ordering::SeqCst);
    unsafe { install_tstp_handler() };
}

/// Handler for SIGWINCH (terminal-window-size change).  Just sets the
/// pending flag — the main loop reads the new winsize via `tcgetwinsize`
/// and triggers a redraw.
extern "C" fn handle_winch(_signum: libc::c_int) {
    SIGWINCH_PENDING.store(true, Ordering::SeqCst);
}

unsafe fn install_handler_raw(
    signum: libc::c_int,
    handler: extern "C" fn(libc::c_int),
    flags: libc::c_int,
) {
    let mut act: libc::sigaction = std::mem::zeroed();
    act.sa_sigaction = handler as libc::sighandler_t;
    act.sa_flags = flags;
    libc::sigemptyset(&mut act.sa_mask);
    libc::sigaction(signum, &act, std::ptr::null_mut());
}

unsafe fn install_tstp_handler() {
    install_handler_raw(libc::SIGTSTP, handle_tstp, 0);
}

/// Install handlers for every signal we care about.  Call **after**
/// [`publish_signal_state`].
fn install_signal_handlers() {
    unsafe {
        // Lifecycle: restore termios + _exit.  No SA_RESTART — we want
        // these to interrupt syscalls and cause immediate termination.
        install_handler_raw(libc::SIGINT, restore_and_exit, 0);
        install_handler_raw(libc::SIGTERM, restore_and_exit, 0);
        install_handler_raw(libc::SIGHUP, restore_and_exit, 0);
        install_handler_raw(libc::SIGQUIT, restore_and_exit, 0);
        // Job control.
        install_tstp_handler();
        // Async events: SA_RESTART so the input thread's read() keeps going.
        install_handler_raw(libc::SIGCONT, handle_cont, libc::SA_RESTART);
        install_handler_raw(libc::SIGWINCH, handle_winch, libc::SA_RESTART);
    }
}

/// Restore SIG_DFL for every signal we installed.  Call **before**
/// [`unpublish_signal_state`].
fn uninstall_signal_handlers() {
    unsafe {
        for signum in &[
            libc::SIGINT,
            libc::SIGTERM,
            libc::SIGHUP,
            libc::SIGQUIT,
            libc::SIGTSTP,
            libc::SIGCONT,
            libc::SIGWINCH,
        ] {
            let mut act: libc::sigaction = std::mem::zeroed();
            act.sa_sigaction = libc::SIG_DFL;
            libc::sigemptyset(&mut act.sa_mask);
            libc::sigaction(*signum, &act, std::ptr::null_mut());
        }
    }
}

// === End of signal handlers ====================================

/// Terminal channel used for reading user commands and writing the prompt.
///
/// POSIX.1-2024 (`more`, INPUT FILES / STDERR sections) requires that when
/// standard output is a terminal, user commands are read from **standard
/// error**; if standard error is not readable, the implementation may attempt
/// to open the controlling terminal (`/dev/tty`); if neither is available it
/// must terminate with an error.  The same channel is used to write the
/// prompt and cursor-control sequences (`STDERR` section).
///
/// This struct owns a file descriptor opened on the chosen channel and places
/// it in raw mode for the lifetime of the pager.  Reader and writer are
/// independent `File` handles created from duplicated file descriptors so the
/// input thread can take the reader by value while the main thread writes the
/// prompt to the writer.  `Drop` restores the original termios and closes the
/// owned fd; each `File` closes its own duplicate.
struct CommandIO {
    /// Owned fd in raw mode; closed (after termios restoration) by Drop.
    fd: RawFd,
    /// Cooked-mode termios captured at construction, restored by Drop.
    original_termios: libc::termios,
}

impl CommandIO {
    /// Open a command-input channel per the POSIX-literal precedence rule:
    /// try stderr first (must be a terminal and readable); fall back to
    /// `/dev/tty` (opened `O_RDWR | O_NOCTTY`); error if neither is usable.
    ///
    /// Returns `(channel, reader, writer)`.  The caller hands `reader` to the
    /// input thread and `writer` to the prompt-rendering code; `channel`
    /// retains the original-termios state for restoration on Drop.  Only
    /// meaningful when standard output is a terminal — callers must check
    /// `is_stdout_tty()` first and skip this in filter mode.
    fn open() -> Result<(Self, File, File), MoreError> {
        let fd = pick_command_fd()?;
        // Save cooked-mode termios for restoration in Drop.
        let mut original = unsafe { std::mem::zeroed::<libc::termios>() };
        if unsafe { libc::tcgetattr(fd, &mut original) } != 0 {
            unsafe { libc::close(fd) };
            return Err(MoreError::NoCommandSource);
        }
        // Derive raw-mode termios.
        let mut raw = original;
        unsafe { libc::cfmakeraw(&mut raw) };
        // VMIN=1 / VTIME=0 — block until at least one byte is available.
        raw.c_cc[libc::VMIN] = 1;
        raw.c_cc[libc::VTIME] = 0;
        // Publish (fd, cooked, raw) for the signal handlers BEFORE applying
        // raw mode.  This ordering matters: if a signal fires during the
        // tcsetattr below, the handler must already see the cooked snapshot
        // so it can restore on _exit.  We then install handlers so they take
        // over from the default dispositions before any raw-mode I/O begins.
        unsafe { publish_signal_state(fd, original, raw) };
        install_signal_handlers();
        // Now apply raw mode.
        if unsafe { libc::tcsetattr(fd, libc::TCSAFLUSH, &raw) } != 0 {
            uninstall_signal_handlers();
            unpublish_signal_state();
            unsafe { libc::close(fd) };
            return Err(MoreError::NoCommandSource);
        }
        // Duplicate fd twice so reader and writer each own a distinct
        // descriptor — they are closed when each File is dropped.  The
        // original fd remains owned by `self` for termios restoration.
        let read_fd = unsafe { libc::dup(fd) };
        let write_fd = unsafe { libc::dup(fd) };
        if read_fd < 0 || write_fd < 0 {
            if read_fd >= 0 {
                unsafe { libc::close(read_fd) };
            }
            if write_fd >= 0 {
                unsafe { libc::close(write_fd) };
            }
            unsafe { libc::tcsetattr(fd, libc::TCSAFLUSH, &original) };
            uninstall_signal_handlers();
            unpublish_signal_state();
            unsafe { libc::close(fd) };
            return Err(MoreError::NoCommandSource);
        }
        let reader = unsafe { File::from_raw_fd(read_fd) };
        let writer = unsafe { File::from_raw_fd(write_fd) };
        Ok((
            Self {
                fd,
                original_termios: original,
            },
            reader,
            writer,
        ))
    }
}

impl Drop for CommandIO {
    fn drop(&mut self) {
        // Order matters:
        //   1. Restore termios while handlers are still installed (so an
        //      in-flight signal sees consistent state).
        //   2. Uninstall handlers (revert to SIG_DFL).  Now any subsequent
        //      signal takes the kernel's default action on the cooked tty.
        //   3. Unpublish the static state so a handler that races us
        //      cannot read freed fd values.
        //   4. Close the owned fd.
        unsafe {
            libc::tcsetattr(self.fd, libc::TCSAFLUSH, &self.original_termios);
        }
        uninstall_signal_handlers();
        unpublish_signal_state();
        unsafe {
            libc::close(self.fd);
        }
    }
}

/// True if the given fd refers to a terminal AND is open for reading.
/// POSIX requires both checks before we use a descriptor as a command source.
fn is_readable_terminal(fd: RawFd) -> bool {
    if unsafe { libc::isatty(fd) } != 1 {
        return false;
    }
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFL) };
    if flags < 0 {
        return false;
    }
    let access_mode = flags & libc::O_ACCMODE;
    access_mode == libc::O_RDONLY || access_mode == libc::O_RDWR
}

/// True if standard output is connected to a terminal.  Filter mode is
/// entered iff this returns false.
fn is_stdout_tty() -> bool {
    unsafe { libc::isatty(libc::STDOUT_FILENO) == 1 }
}

/// Pick the file descriptor to use for reading user commands.
///
/// Returns an owned fd opened `O_RDWR` (or duped from stderr) that the caller
/// must close.  Used by `CommandIO::open`.
fn pick_command_fd() -> Result<RawFd, MoreError> {
    // 1. stderr: only if it is itself a terminal AND open for reading.
    if is_readable_terminal(libc::STDERR_FILENO) {
        let dup = unsafe { libc::dup(libc::STDERR_FILENO) };
        if dup >= 0 {
            return Ok(dup);
        }
    }
    // 2. /dev/tty fallback (POSIX permits this).
    let path = c"/dev/tty";
    let fd = unsafe { libc::open(path.as_ptr(), libc::O_RDWR | libc::O_NOCTTY) };
    if fd >= 0 {
        return Ok(fd);
    }
    Err(MoreError::NoCommandSource)
}

/// Wraps stdout for content rendering (alternate-screen-buffered when
/// interactive) and holds the prompt-output writer plus the mpsc receiver
/// fed by the spawned input thread.  Raw mode lives on the command channel
/// (see [`CommandIO`]); this struct deliberately does not own a raw-mode
/// guard on stdout — termios state is per-controlling-terminal, not per-fd,
/// so putting stdout into raw mode would be redundant and would race with
/// the command channel's termios restore on Drop.
struct Terminal {
    /// Alternate-screen guard wrapping stdout.  Drops at end of pager to
    /// restore the user's original screen.  Raw mode itself is handled by
    /// [`CommandIO`] on the command channel (stderr / `/dev/tty`); putting
    /// stdout into raw mode here would be both redundant and incorrect,
    /// since termios state is per-controlling-terminal, not per-fd.
    _alternate_screen: Option<AlternateScreen<std::io::Stdout>>,
    /// Content output channel — stdout per POSIX `more` STDOUT section.
    /// All file content goes here, wrapped in an alternate-screen buffer
    /// when interactive so the original screen is restored on exit.
    tty: std::io::Stdout,
    /// UI-chrome output channel — stderr (or `/dev/tty`) per POSIX `more`
    /// STDERR section, so that piping `more`'s stdout to a file does not
    /// embed prompt text in the content.  In `--test` mode this is wired
    /// to stdout (pre-refactor behavior; tests assert `stderr == ""`).
    /// In filter mode it is `io::sink()` (never used: the loop that calls
    /// `display_prompt` only runs when `Terminal` is present).
    prompt_out: Box<dyn Write + Send>,
    /// Terminal size in char rows and cols
    size: (u16, u16),
    /// Set terminal height as lines
    lines: Option<u16>,
    /// Input stream
    input_stream: Receiver<Result<String, MoreError>>,
    /// Held only when no input thread is spawned (`--test` or filter mode),
    /// so the receiver in `input_stream` does not see `Disconnected` and
    /// surface a spurious `MoreError::InputRead`.  Has no other purpose.
    _input_keepalive: Option<std::sync::mpsc::Sender<Result<String, MoreError>>>,
}

/// Where the input thread should source user commands.
enum CommandReader {
    /// No input thread spawned — filter mode (stdout not a terminal).
    None,
    /// `--test` mode: read commands from stdin (the test harness pipes them in).
    Stdin,
    /// Interactive mode: raw-mode terminal fd (stderr or `/dev/tty`).
    Tty(File),
}

impl Terminal {
    /// Construct a `Terminal`.
    ///
    /// `command_reader` selects the input thread's reader: `Tty(File)` for
    /// real interactive use (caller has already opened a [`CommandIO`] and
    /// split off the reader), `Stdin` for the test harness, or `None` for
    /// filter mode where no command thread is needed.
    ///
    /// `prompt_out` is the writer used for prompt text and cursor sequences
    /// (stderr/`/dev/tty` in interactive mode, stdout in tests, a sink in
    /// filter mode).
    fn new(
        is_test: bool,
        lines: Option<u16>,
        command_reader: CommandReader,
        prompt_out: Box<dyn Write + Send>,
    ) -> Result<Self, MoreError> {
        let mut _alternate_screen = None;
        if !is_test {
            if !termion::is_tty(&std::io::stdout().as_raw_fd()) {
                return Err(MoreError::TerminalInit);
            }
            _alternate_screen = Some(
                stdout()
                    .into_alternate_screen()
                    .map_err(|_| MoreError::TerminalInit)?,
            );
        }

        let (sender, receiver) = channel();
        // Keep the sender alive in any branch that doesn't move it to a
        // spawned thread, so `try_recv` returns Empty (not Disconnected) —
        // otherwise the pager loop would surface a spurious InputRead error.
        let keepalive = match command_reader {
            CommandReader::None => Some(sender.clone()),
            _ => None,
        };
        let mut terminal = Self {
            _alternate_screen,
            tty: stdout(),
            prompt_out,
            size: (
                env_screen_size("LINES").unwrap_or(LINES_PER_PAGE),
                env_screen_size("COLUMNS").unwrap_or(NUM_COLUMNS),
            ),
            lines,
            input_stream: receiver,
            _input_keepalive: keepalive,
        };

        let _ = terminal.resize();
        match command_reader {
            CommandReader::None => { /* sender held in _input_keepalive */ }
            CommandReader::Stdin => {
                let _ = std::thread::spawn(move || {
                    run_input_thread(std::io::stdin(), sender);
                });
            }
            CommandReader::Tty(reader) => {
                let _ = std::thread::spawn(move || {
                    run_input_thread(reader, sender);
                });
            }
        }

        Ok(terminal)
    }

    /// Display [`Screen`] on [`Terminal`]
    pub fn display(&mut self, screen: Screen) -> Result<(), MoreError> {
        if screen.0.len() > self.size.0 as usize || screen.0[0].len() > self.size.1 as usize {
            let _ = set_style_on(&mut self.tty, StyleType::None);
            return Err(MoreError::SetOutside);
        }
        let mut style = StyleType::None;
        for (i, line) in screen.0.iter().enumerate() {
            write_ch_on(&mut self.tty, ' ', 0, i as u16);
            clear_current_line_on(&mut self.tty);
            for (j, (ch, st)) in line.iter().enumerate() {
                // The second column of a double-width character holds a
                // continuation marker; the character itself was drawn in the
                // column before it.
                if *ch == CELL_CONT {
                    continue;
                }
                if style != *st {
                    let _ = set_style_on(&mut self.tty, *st);
                    style = *st;
                }
                write_ch_on(&mut self.tty, *ch, j as u16, i as u16);
            }
        }
        let _ = set_style_on(&mut self.tty, StyleType::None);
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

    // Display prompt in bottom row
    pub fn display_prompt(&mut self, prompt: Prompt) -> Result<(), MoreError> {
        let mut line = prompt.format();
        // The prompt is measured in cells, and a prompt too wide for the
        // terminal is truncated rather than refused: a narrow window must
        // still get a prompt, and `line` here is already a cell vector.
        let width = self.size.1 as usize;
        if line.len() > width {
            line.truncate(width);
        }

        let mut style = StyleType::None;
        let _ = write!(
            self.prompt_out,
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
        write_ch_on(&mut self.prompt_out, ' ', 1, line_position);
        clear_current_line_on(&mut self.prompt_out);
        for (i, (ch, st)) in line.iter().enumerate() {
            if style != *st {
                let _ = set_style_on(&mut self.prompt_out, *st);
                style = *st;
            }
            write_ch_on(&mut self.prompt_out, *ch, i as u16, line_position);
        }

        let _ = set_style_on(&mut self.prompt_out, StyleType::None);
        Ok(())
    }

    /// Update terminal size for wrapper
    fn resize(&mut self) -> Result<(), MoreError> {
        let (x, y) = terminal_size().map_err(|_| MoreError::SizeRead)?;

        // POSIX 107336 and 107357: COLUMNS and LINES "override the
        // system-selected" sizes.  They are consulted here, on every resize,
        // rather than only at startup, so a window change cannot silently
        // discard them.
        let columns = env_screen_size("COLUMNS").unwrap_or(x);
        let rows = env_screen_size("LINES").unwrap_or(y);

        if rows < 2 {
            return Err(MoreError::TerminalOutput);
        }

        // 107359: "The -n option shall take precedence over the LINES
        // variable", bounded by the screen actually available.
        let mut lines = self.lines.unwrap_or(rows);
        if lines > rows || lines < 1 {
            lines = rows;
        }

        self.size = (lines, columns);
        Ok(())
    }

    /// Prepare resources for closing terminal
    fn close(&mut self) {
        *NEED_QUIT.lock().unwrap() = true;
        // Cursor-show + style-reset belong on the prompt channel (stderr in
        // interactive mode) — that is where the cursor was hidden and styled.
        let _ = write!(self.prompt_out, "{}{}", Show, Reset);
        self._alternate_screen = None;
    }
}

/// Translate a parsed terminal event into the command string the pager loop
/// consumes.  Returns `None` for events that don't map to a command (idle
/// mouse motion, modifier-only keys, etc.).
fn event_to_command(event: Event, bytes: Vec<u8>) -> Option<String> {
    match event {
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
    }
}

/// Emit an ANSI style escape sequence to `out`.  Used by both content
/// rendering (writer = stdout) and prompt rendering (writer = stderr).
fn set_style_on<W: Write + ?Sized>(out: &mut W, style: StyleType) -> std::io::Result<()> {
    write!(out, "{}", Reset)?;
    match style {
        StyleType::Underscore => write!(out, "{}", Underline),
        StyleType::Negative => write!(out, "{}", Invert),
        _ => Ok(()),
    }
}

/// Position the cursor at (`x`, `y`) (1-based, converted from 0-based) and
/// write a single character.
fn write_ch_on<W: Write + ?Sized>(out: &mut W, ch: char, x: u16, y: u16) {
    let _ = write!(out, "{}{ch}", Goto(x + 1, y + 1));
}

/// Erase the current cursor line.
fn clear_current_line_on<W: Write + ?Sized>(out: &mut W) {
    let _ = write!(out, "{}", CurrentLine);
}

/// Position the cursor at (`x`, `y`) (1-based, converted from 0-based) and
/// write a string.
fn write_str_on<W: Write + ?Sized>(out: &mut W, s: &str, x: u16, y: u16) {
    let _ = write!(out, "{}{s}", Goto(x + 1, y + 1));
}

/// Background input thread body.  Takes ownership of the reader (so it can
/// drive `events_and_raw` as a long-lived iterator) and pumps commands into
/// the mpsc channel until the main loop signals quit.
///
/// EOF on the reader is **not** treated as a fatal end-of-stream: the test
/// harness pipes a short fixed command sequence into stdin and then closes
/// it; on a real terminal in raw mode reads block until input arrives.  In
/// both cases we want the sender to stay alive (a dropped sender surfaces
/// as `Disconnected` → spurious `InputRead` errors in the pager loop), so
/// after the iterator finishes we idle waiting for shutdown rather than
/// returning.
fn run_input_thread<R: Read>(
    reader: R,
    sender: std::sync::mpsc::Sender<Result<String, MoreError>>,
) {
    let mut events = reader.events_and_raw();
    while !*NEED_QUIT.lock().unwrap() {
        let Some(result) = events.next() else {
            // Reader is exhausted (typically only in tests).  Hold the
            // sender alive and wait for the main loop to signal quit.
            drop(events);
            while !*NEED_QUIT.lock().unwrap() {
                std::thread::sleep(Duration::from_millis(100));
            }
            return;
        };
        match result {
            Ok((event, bytes)) => {
                if let Some(cmd) = event_to_command(event, bytes) {
                    if sender.send(Ok(cmd)).is_err() {
                        return;
                    }
                }
            }
            Err(_) => {
                // Send the error and then idle — same rationale as the EOF
                // branch: keep the channel non-Disconnected.
                let _ = sender.send(Err(MoreError::InputRead));
                while !*NEED_QUIT.lock().unwrap() {
                    std::thread::sleep(Duration::from_millis(100));
                }
                return;
            }
        }
    }
}

/// String that was printed in bottom terminal row
#[derive(Debug, Clone)]
enum Prompt {
    /// `[<filename>: ]-- More --[(NN%)]`
    ///
    /// POSIX `more` STDERR section: "The prompt shall contain the name of
    /// the file currently being examined".  `filename` is `None` when the
    /// content source is stdin (no filename to display); the prompt
    /// degrades to the historical `-- More --(NN%)` form in that case.
    More {
        filename: Option<String>,
        percent: Option<u8>,
    },
    /// `[<filename>: ]-- More --(Next file: <next_file>)`
    Eof {
        filename: Option<String>,
        next_file: String,
    },
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
            Prompt::More { filename, percent } => {
                let core = match percent {
                    Some(p) => format!("-- More --({}%)", p),
                    None => "-- More --".to_string(),
                };
                match filename {
                    Some(name) => format!("{name}: {core}"),
                    None => core,
                }
            }
            Prompt::Eof {
                filename,
                next_file,
            } => {
                let core = format!("-- More --(Next file: {next_file})");
                match filename {
                    Some(name) => format!("{name}: {core}"),
                    None => core,
                }
            }
            Prompt::DisplayPosition(position) => position.clone(),
            Prompt::Input(input) => input.clone(),
            Prompt::Error(error) => error.clone(),
            Prompt::ExitKeys => {
                "[Press space to continue, \"q\", \":q\" or \"ZZ\" to quit.]".to_string()
            }
            Prompt::Exit => "Press Enter to exit ...".to_string(),
        };

        let style = match self {
            Prompt::More { .. } | Prompt::Eof { .. } => StyleType::Negative,
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
    /// Set when a per-file error occurred that must affect the final exit
    /// status without aborting the session (POSIX CONSEQUENCES OF ERRORS,
    /// 107643-107646: the `:n` and `:p` commands).
    had_error: bool,
    /// Spill-backed view of standard input.  Present when stdin is the
    /// implicit content source (no file operands) or when `-` appears in the
    /// operand list.  Shared by `print_all_input` (multi-file filter mode
    /// containing a `-` operand) and by `:e -` (re-examine stdin), so the
    /// stream is read exactly once no matter how many cursors want it.
    /// `None` when no operand references stdin.
    stdin_spill: Option<SharedSpill>,
    /// Command-input/prompt channel (stderr or /dev/tty) used when stdout is
    /// a terminal.  `None` in filter mode and under `--test`.  Declared last
    /// so its `Drop` (which restores cooked-mode termios) runs **after**
    /// `terminal`'s `Drop` releases the alternate screen and the spawned
    /// input thread has been told to quit.
    command_io: Option<CommandIO>,
}

impl MoreControl {
    /// Init [`MoreControl`]
    fn new(args: Args) -> Result<Self, MoreError> {
        let mut current_position = None;
        let mut file_pathes = vec![];

        // STEP 1: Attach to stdin FIRST — before anything puts the terminal
        // in raw mode.  Stdin is content when no file operands are given
        // (implicit stdin) or when `-` appears in the operand list.  We
        // attach here, while termios is still in cooked mode, so that an
        // interactive user (no piped input, no operands) can still press
        // Ctrl-D to signal EOF — raw mode disables that line-discipline
        // shortcut, and reading stdin under raw mode would hang the terminal
        // with no recovery path.
        //
        // Nothing is read yet: the stream is wrapped in a spill file and
        // pulled from only as the display demands it.  Reading it through
        // here would bound the input size by available memory, which for a
        // pager fed by a pipe is the wrong bound entirely.
        let needs_stdin = args.input_files.is_empty() || args.input_files.iter().any(|f| f == "-");
        let stdin_spill: Option<SharedSpill> = if needs_stdin {
            Some(SharedSpill::new(Box::new(std::io::stdin())).map_err(|_| MoreError::InputRead)?)
        } else {
            None
        };

        // STEP 2: Now that stdin is fully consumed (or wasn't needed),
        // select the user-command and prompt-output channels:
        //   * `--test` mode → commands from stdin (test harness pipes them);
        //     prompts to stdout (preserves the pre-refactor behavior — tests
        //     use `stdout.contains()` and would fail if prompt bytes landed
        //     in stderr where they assert `stderr == ""`).
        //   * stdout is a real terminal → open the POSIX command channel
        //     (stderr → /dev/tty); commands from its raw-mode reader;
        //     prompts to its writer (so prompts appear on stderr per spec).
        //   * otherwise → filter mode, no command thread, no prompts.
        // POSIX requires a fatal error if stdout is a terminal but no command
        // channel can be opened — `CommandIO::open` returns
        // `MoreError::NoCommandSource`, which propagates here.
        let (command_io, command_reader, prompt_out): (
            Option<CommandIO>,
            CommandReader,
            Box<dyn Write + Send>,
        ) = if args.test {
            (None, CommandReader::Stdin, Box::new(stdout()))
        } else if is_stdout_tty() {
            let (io, reader, writer) = CommandIO::open()?;
            (Some(io), CommandReader::Tty(reader), Box::new(writer))
        } else {
            (None, CommandReader::None, Box::new(std::io::sink()))
        };
        let terminal = Terminal::new(args.test, args.lines, command_reader, prompt_out).ok();

        let source = if args.input_files.is_empty()
            || (args.input_files.len() == 1 && args.input_files[0] == *"-")
        {
            // Implicit stdin or single explicit '-' operand. Empty stdin is
            // valid (yields an empty Buffer); it is not an error.
            stdin_source(stdin_spill.as_ref())
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
                stdin_source(stdin_spill.as_ref())
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
            had_error: false,
            last_source_before_usage: None,
            file_pathes,
            is_new_file: false,
            is_matched: false,
            stdin_spill,
            command_io,
        })
    }

    /// Print all input files in output if terminal isn't available
    fn print_all_input(&mut self) {
        let input_files = self.file_pathes.clone();
        if input_files.is_empty() || (input_files.len() == 1 && self.args.input_files[0] == *"-") {
            let source = self.context.current_source.clone();
            match source_reader(&source) {
                Ok(mut reader) => {
                    let mut out = stdout().lock();
                    if filter_copy(&mut reader, self.args.squeeze, &mut out).is_err() {
                        self.handle_error(MoreError::InputRead);
                    }
                }
                Err(e) => self.handle_error(e),
            }
        } else {
            for file_path in &input_files {
                // Handle '-' as stdin
                let source = if file_path.as_os_str() == "-" {
                    stdin_source(self.stdin_spill.as_ref())
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
                    if file_path.as_os_str() == "-" {
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

                let current = self.context.current_source.clone();
                match source_reader(&current) {
                    Ok(mut reader) => {
                        let mut out = stdout().lock();
                        if filter_copy(&mut reader, self.args.squeeze, &mut out).is_err() {
                            self.handle_error(MoreError::InputRead);
                        }
                    }
                    Err(e) => self.handle_error(e),
                }
            }
        }
    }

    /// Filename for the current source, or `None` when reading from stdin.
    ///
    /// Used to satisfy the POSIX requirement that the prompt include the
    /// name of the file currently being examined.  For file sources we use
    /// the basename (matches historical `more` behavior — the full path is
    /// already visible via the `=` command); for buffer sources (stdin and
    /// `-`-operand pipelines) we have no name to show and return `None`,
    /// causing the prompt to degrade to the historical filename-less form.
    fn current_filename(&self) -> Option<String> {
        match &self.context.current_source {
            Source::File(path) => path
                .file_name()
                .and_then(|s| s.to_str())
                .map(|s| s.to_owned()),
            Source::Buffer(_) | Source::Stream(_) => None,
        }
    }

    /// Display current state in terminal
    fn display(&mut self) -> Result<(), MoreError> {
        // Snapshot current_filename before taking a mutable borrow of
        // self.terminal — the helper reads through &self.context.
        let filename = self.current_filename();
        let Some(terminal) = self.terminal.as_mut() else {
            return Err(MoreError::MissingTerminal);
        };
        self.context.update_screen()?;
        let result = if let Some(screen) = self.context.screen() {
            let prompt = match &self.prompt {
                Some(Prompt::More { .. }) | None => Prompt::More {
                    filename: filename.clone(),
                    percent: if self.file_pathes.len() == 1 {
                        self.context.seek_positions.percent()
                    } else {
                        None
                    },
                },
                Some(prompt) => prompt.clone(),
            };
            if let Prompt::Input(_) = prompt {
            } else {
                terminal.display(screen)?;
            };
            // Content goes to stdout and the prompt to stderr, which are
            // separately buffered handles onto the same terminal.  Flush the
            // content before writing the prompt, or the two interleave and
            // escape sequences from one land inside the other.
            let _ = terminal.tty.flush();
            terminal.display_prompt(prompt)?;
            if self.is_matched {
                write_str_on(&mut terminal.prompt_out, "", 0, 0);
                clear_current_line_on(&mut terminal.prompt_out);
                write_str_on(&mut terminal.prompt_out, "...skipping", 0, 0);
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
            // Handle kernel-delivered events.  The handlers themselves are
            // async-signal-safe and just set atomic flags; the visible work
            // (winsize refetch, screen redraw, raw-mode re-engage on resume)
            // happens here in normal context.
            self.handle_pending_signals()?;
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

    /// Drain pending-signal flags set by the async handlers and perform the
    /// user-visible work.
    ///
    /// * `SIGCONT` (Austin Group Defect 1185 / POSIX ASYNCHRONOUS EVENTS):
    ///   the screen must always be refreshed regardless of whether the
    ///   terminal-window size changed.
    /// * `SIGWINCH`: re-fetch the size; the redraw is a side effect of
    ///   `MoreControl::resize` when the size differs from what we cached.
    ///
    /// Polling these flags (rather than handling everything inside the
    /// signal handler) is the only safe way to do non-trivial work — Rust
    /// `Drop`, allocation, and `Mutex` are all banned in handler context.
    fn handle_pending_signals(&mut self) -> Result<(), MoreError> {
        let cont = SIGCONT_PENDING.swap(false, Ordering::SeqCst);
        let winch = SIGWINCH_PENDING.swap(false, Ordering::SeqCst);
        if cont {
            // Force-refresh independent of size change.  `resize` is the
            // single entry point that refetches winsize and re-renders.
            let _ = self.resize()?;
            let _ = self.refresh();
        } else if winch {
            // resize() returns true when the size actually changed and
            // already triggers a refresh in that case.
            let _ = self.resize()?;
        }
        Ok(())
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
        // POSIX 107617-107618: "If the last pathname component in EDITOR is
        // either vi or ex" -- so compare the basename, not the whole string,
        // which would miss /usr/bin/vi.
        let is_editor_vi_or_ex = Path::new(editor)
            .file_name()
            .map(|name| name == "vi" || name == "ex")
            .unwrap_or(false);
        let Some(file_path) = file_path.as_os_str().to_str() else {
            return Err(MoreError::FileRead(
                file_path.to_str().unwrap_or("<file>").to_owned(),
            ));
        };

        // POSIX 107618-107620: vi and ex take "-c linenumber", where
        // linenumber is the line displayed as the first line of the screen.
        let line_number = self.context.seek_positions.current_line().to_string();
        let args: &[&str] = if is_editor_vi_or_ex {
            &["-c", line_number.as_str(), "--", file_path]
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

        // POSIX 107607-107608: ":t tagstring" names *the tag*, so the lookup
        // is a literal comparison against a tags file's first field -- not a
        // regular-expression search, which would both mismatch tags
        // containing regex metacharacters and match unrelated ones.
        let mut matched: Option<String> = None;
        let mut tags_path: Option<String> = None;
        for file in find_tags_files(Path::new(".")) {
            let Ok(contents) = std::fs::read_to_string(&file) else {
                continue;
            };
            let Some(line) = contents
                .lines()
                .find(|line| line.split('\t').next() == Some(tagstring.as_str()))
            else {
                continue;
            };
            if let Some(folder) = file.parent() {
                if !folder.exists() {
                    return Err(MoreError::FileRead(file.to_string_lossy().into_owned()));
                }
                tags_path = folder.to_str().map(|s| s.to_owned());
            }
            matched = Some(line.to_owned());
            break;
        }
        let Some(line) = matched else {
            return Err(MoreError::PatternNotFound(tagstring));
        };
        let fields = line.split("\t").collect::<Vec<&str>>();
        if fields.len() < 2 {
            return parse_error;
        };
        // Tags files normally name files relative to their own directory, but
        // an absolute entry must not be glued onto that prefix; Path::join
        // handles both.
        let path = Path::new(&tags_path.unwrap_or_else(|| ".".to_owned())).join(fields[1]);
        let path = to_path(path.to_string_lossy().into_owned())?;
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
        if self.context.terminal_size.is_none() {
            return Err(MoreError::MissingTerminal);
        }
        // POSIX 107628: the message includes "the name of the file currently
        // being examined" -- the pathname as given, not just its last
        // component, which would be ambiguous across directories.
        let mut filename = String::from("<error>");
        let mut file_size = 0;
        if let Source::File(path) = &self.context.current_source {
            filename = path.to_string_lossy().into_owned();
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

        // POSIX 107631-107632: "If more is reading from standard input, or the
        // file is shorter than a single screen, the line number, the byte
        // number, the total bytes, and the percentage need not be written."
        // Those figures are exactly the ones that need a known total size, so
        // the short form is used precisely when the size is not knowable.
        let line = if let Some(percent) = self.context.seek_positions.percent() {
            format!(
                "{} {}/{} {} {}/{} {}%",
                filename,
                current_position,
                input_files_count,
                current_line,
                byte_number,
                file_size,
                percent
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
        if let Some(Prompt::Eof { .. }) = self.prompt {
            self.prompt = Some(Prompt::More {
                filename: self.current_filename(),
                percent: if self.file_pathes.len() == 1 {
                    Some(100)
                } else {
                    None
                },
            });
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
                && self.context.seek_positions.is_ended
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
                if let Some(Prompt::Eof { .. }) = self.prompt {
                    if self
                        .scroll_file_position(Some(1), Direction::Forward)
                        .is_err()
                    {
                        self.prompt = Some(Prompt::Exit);
                        self.display()?;
                        self.get_input_with_update()?;
                        self.exit(None);
                    }
                    self.prompt = Some(Prompt::More {
                        filename: self.current_filename(),
                        percent: if self.file_pathes.len() == 1 {
                            self.context.seek_positions.percent()
                        } else {
                            None
                        },
                    });
                } else {
                    self.prompt = Some(Prompt::Eof {
                        filename: self.current_filename(),
                        next_file: name_and_ext,
                    });
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
        // Drop CommandIO before `process::exit` (which skips destructors)
        // so that the cooked-mode termios is restored on the controlling
        // terminal — otherwise the user's shell would be left in raw mode.
        self.command_io = None;
        if let Some(ref error_message) = error_message {
            eprintln!("{error_message}");
            println!("{error_message}");
        }
        exit((error_message.is_some() || self.had_error) as i32);
    }

    /// Move `count` files in `direction` and examine what is found there.
    ///
    /// POSIX CONSEQUENCES OF ERRORS (107643-107646): "If an error is
    /// encountered accessing a file when using the :n command, more shall
    /// attempt to examine the next file in the argument list, but the final
    /// exit status shall be affected", and correspondingly for `:p`.  So a
    /// file that cannot be opened is reported, remembered for the exit
    /// status, and skipped -- it does not end the session.
    ///
    /// Returns true when the move ran past the end of the file list.
    fn examine_adjacent_file(
        &mut self,
        count: Option<usize>,
        direction: Direction,
    ) -> Result<bool, MoreError> {
        let mut count = count;

        loop {
            match self.scroll_file_position(count, direction) {
                Ok(past_end) => return Ok(past_end),
                Err(MoreError::FileRead(name)) => {
                    self.had_error = true;
                    self.prompt = Some(Prompt::Error(MoreError::FileRead(name).to_string()));

                    // scroll_file_position has already moved to the file that
                    // failed; stop once there is nothing further to try.
                    let position = self.current_position.unwrap_or(0);
                    let exhausted = match direction {
                        Direction::Forward => position + 1 >= self.file_pathes.len(),
                        Direction::Backward => position == 0,
                    };
                    if exhausted {
                        return Ok(false);
                    }

                    count = Some(1);
                }
                Err(e) => return Err(e),
            }
        }
    }

    /// Set current file by [`file_string`] path
    fn examine_file(&mut self, file_string: String) -> Result<(), MoreError> {
        if file_string.is_empty() {
            self.context.reset()?;
        }

        // POSIX 107600-107602: the filename operand of `:e` is subject to
        // shell word expansion.  "#" and "-" are more's own tokens and are
        // recognized before expansion.
        let file_string = if file_string.is_empty() || file_string == "#" || file_string == "-" {
            file_string
        } else {
            word_expand(&file_string)?
        };

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
            self.context
                .set_source(stdin_source(self.stdin_spill.as_ref()))?;
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
        // POSIX 107563-107565: `''` returns "to the position from which the
        // last large movement command was executed (where a 'large movement'
        // is defined as any movement of more than a screenful of lines)".
        // Rather than enumerate which commands qualify, measure the move.
        let line_before = self.context.seek_positions.current_line();
        let track_movement = !matches!(command, Command::ReturnPreviousPosition);

        let result = self.execute_command(command);

        if track_movement && result.is_ok() {
            let line_after = self.context.seek_positions.current_line();
            let screenful = self.context.screen_lines();
            if line_after.abs_diff(line_before) > screenful {
                self.context.last_line = line_before;
            }
        }

        result
    }

    fn execute_command(&mut self, command: Command) -> Result<(), MoreError> {
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
                // POSIX 107528: "Display the screenful beginning with the line
                // count lines after the last line on the current screen" --
                // so the top moves past the whole current screen, not just by
                // count lines.  scroll() clamps at the end of the file, which
                // gives 107529-107530's "the last screenful shall be written".
                let count = count.unwrap_or(1);
                let screenful = self.context.screen_lines();
                self.context
                    .scroll(screenful.saturating_sub(1) + count, Direction::Forward);
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
                if self.examine_adjacent_file(count, Direction::Forward)? {
                    self.prompt = Some(Prompt::Exit);
                    self.display()?;
                    self.get_input_with_update()?;
                    self.exit(None);
                }
            }
            Command::ExaminePreviousFile(count) => {
                if self.examine_adjacent_file(count, Direction::Backward)? {
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
                SeekPositionsError::OutOfRange(_) => {
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
            MoreError::PatternNotFound(_) => {
                self.prompt = Some(Prompt::Error(error_str.clone()));
            }
            MoreError::StringParse(_) => {
                self.commands_buffer.clear();
                self.prompt = Some(Prompt::Error(error_str.clone()));
            }
            // POSIX 107595-107597 (`:e`) and 107643-107646 (`:n`/`:p`): a
            // file that cannot be accessed is reported, and more carries on.
            MoreError::FileRead(_) => {
                self.prompt = Some(Prompt::Error(error_str.clone()));
            }
            MoreError::SetOutside
            | MoreError::EditorFailed
            | MoreError::SizeRead
            | MoreError::InputRead
            | MoreError::NoCommandSource
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

            // POSIX 107291-107293: "If any of the commands fail for any
            // reason, an informational message to this effect shall be
            // written, and no further commands specified using the -p option
            // shall be executed for this file."  The remaining commands are
            // kept, so the next file starts from the beginning of the list.
            if let Err(e) = self.execute(command) {
                self.handle_error(e);
                break;
            }
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
                        self.prompt = Some(Prompt::More {
                            filename: self.current_filename(),
                            percent: if self.file_pathes.len() == 1 {
                                self.context.seek_positions.percent()
                            } else {
                                None
                            },
                        });
                        continue;
                    }
                }
            }
            if let Ok((command, mut remainder, next_possible)) =
                parse(self.commands_buffer.clone()).inspect_err(|e| self.handle_error(e.clone()))
            {
                if let Some(Prompt::Eof { .. }) = self.prompt {
                } else if next_possible != Command::Unknown {
                    self.prompt = Some(Prompt::Input(self.commands_buffer.clone()));
                    let _ = self.display().inspect_err(|e| self.handle_error(e.clone()));
                } else {
                    self.prompt = Some(Prompt::More {
                        filename: self.current_filename(),
                        percent: if self.file_pathes.len() == 1 {
                            self.context.seek_positions.percent()
                        } else {
                            None
                        },
                    });
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

/// `wordexp_t` of `<wordexp.h>`.  Not exposed by the `libc` crate, but the
/// layout is fixed by POSIX and identical on the platforms this project
/// targets (glibc and macOS both declare exactly these three members, in
/// this order).
#[repr(C)]
struct WordExp {
    we_wordc: libc::size_t,
    we_wordv: *mut *mut libc::c_char,
    we_offs: libc::size_t,
}

extern "C" {
    fn wordexp(
        words: *const libc::c_char,
        pwordexp: *mut WordExp,
        flags: libc::c_int,
    ) -> libc::c_int;
    fn wordfree(pwordexp: *mut WordExp);
}

/// Apply shell word expansions (XCU Section 2.6) to `word`, which POSIX
/// 107592-107594 requires for the `:e` filename operand.
///
/// The spec leaves the effects unspecified when the expansion yields more
/// than one pathname; treat that, like a failed expansion, as an error, so
/// that 107595-107597's "the current file and screen shall not change"
/// applies.
fn word_expand(word: &str) -> Result<String, MoreError> {
    let failed = || MoreError::FileRead(word.to_owned());
    let c_word = CString::new(word).map_err(|_| failed())?;

    // SAFETY: the structure is zeroed before use, as wordexp(3) requires
    // when called without WRDE_APPEND/WRDE_DOOFFS; `c_word` outlives the
    // call; and `wordfree` runs on every path where `wordexp` succeeded.
    unsafe {
        let mut we: WordExp = std::mem::zeroed();
        if wordexp(c_word.as_ptr(), &mut we, 0) != 0 {
            return Err(failed());
        }

        let expanded = if we.we_wordc == 1 && !we.we_wordv.is_null() {
            CStr::from_ptr(*we.we_wordv)
                .to_str()
                .map(str::to_owned)
                .map_err(|_| failed())
        } else {
            Err(failed())
        };

        wordfree(&mut we);
        expanded
    }
}

/// Read a screen dimension from the environment, per POSIX 107336/107357.
///
/// A variable that is unset, empty, unparsable, or zero is treated as absent
/// so that the system-selected size is used instead.
fn env_screen_size(name: &str) -> Option<u16> {
    std::env::var(name)
        .ok()
        .and_then(|value| u16::from_str(value.trim()).ok())
        .filter(|&size| size > 0)
}

/// Copy a source straight to standard output, honoring only `-s`.
///
/// POSIX 107650: "When the standard output is not a terminal, only the -s
/// filter-modification option is effective."  So this is a byte-for-byte
/// copy: no decoding (arbitrary binary passes through unharmed), no line
/// index, and nothing proportional to the input held in memory.
fn filter_copy(reader: &mut dyn Read, squeeze: bool, out: &mut dyn Write) -> std::io::Result<u64> {
    if !squeeze {
        return std::io::copy(reader, out);
    }

    // Collapsing runs of empty lines needs to know whether a line is empty
    // before its <newline> is written, but an empty line is at most a lone
    // <carriage-return>.  So at most two bytes are ever held back; anything
    // longer proves the line is not empty and streams through immediately.
    let mut chunk = [0u8; 8192];
    let mut pending: Vec<u8> = Vec::with_capacity(2);
    let mut line_started = false;
    let mut previous_blank = false;
    let mut written = 0u64;

    loop {
        let read = reader.read(&mut chunk)?;
        if read == 0 {
            break;
        }
        for &byte in &chunk[..read] {
            if byte == b'\n' {
                let blank = !line_started && pending.iter().all(|&b| b == b'\r');
                if blank && previous_blank {
                    pending.clear();
                    continue;
                }
                out.write_all(&pending)?;
                out.write_all(b"\n")?;
                written += pending.len() as u64 + 1;
                previous_blank = blank;
                pending.clear();
                line_started = false;
            } else if line_started {
                out.write_all(&[byte])?;
                written += 1;
            } else {
                pending.push(byte);
                if pending.len() > 1 || byte != b'\r' {
                    out.write_all(&pending)?;
                    written += pending.len() as u64;
                    pending.clear();
                    line_started = true;
                }
            }
        }
    }

    if !pending.is_empty() {
        out.write_all(&pending)?;
        written += pending.len() as u64;
    }

    Ok(written)
}

/// A readable handle on a [`Source`], for the filter path.
fn source_reader(source: &Source) -> Result<Box<dyn Read>, MoreError> {
    match source {
        Source::File(path) => File::open(path)
            .map(|f| Box::new(f) as Box<dyn Read>)
            .map_err(|_| MoreError::FileRead(path.to_str().unwrap_or("<file>").to_owned())),
        Source::Buffer(cursor) => Ok(Box::new(cursor.clone())),
        Source::Stream(spill) => Ok(Box::new(spill.reader())),
    }
}

/// The [`Source`] for a `-` operand or implicit standard input.
///
/// Falls back to an empty buffer when stdin was never attached, which keeps
/// the "no operand references stdin" case from needing a separate path.
fn stdin_source(spill: Option<&SharedSpill>) -> Source {
    match spill {
        Some(spill) => Source::Stream(spill.clone()),
        None => Source::Buffer(Cursor::new(String::new())),
    }
}

/// Collect every file named `tags` at or below `root`.
///
/// Replaces a `find . -name tags -type f` subprocess; `more` needs no
/// external program to walk a directory.
fn find_tags_files(root: &Path) -> Vec<PathBuf> {
    let mut found = Vec::new();
    let mut pending = vec![root.to_path_buf()];

    while let Some(dir) = pending.pop() {
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            // Do not follow symbolic links: a link loop would not terminate.
            let Ok(file_type) = entry.file_type() else {
                continue;
            };
            if file_type.is_dir() {
                pending.push(path);
            } else if file_type.is_file() && entry.file_name() == "tags" {
                found.push(path);
            }
        }
    }

    found
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
        // Measure the name in characters, not bytes, and guard the
        // subtraction so a terminal narrower than the border does not wrap.
        let name_width = name_and_ext.chars().count();
        let header_width = if name_width < 14 {
            14
        } else if name_width + 4 > line_len {
            line_len
        } else {
            name_width + 4
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
    while let Some(ch) = chars.get(*i) {
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
For more see: https://pubs.opengroup.org/onlinepubs/9799919799/utilities/more.html\n";

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

#[cfg(test)]
mod tests {
    use super::{find_tags_files, word_expand};
    use std::path::Path;

    #[test]
    fn word_expand_applies_shell_expansions() {
        // POSIX 107592-107594: the `:e` filename is subject to shell word
        // expansions.
        std::env::set_var("POSIXUTILS_MORE_TEST_VAR", "/tmp/expanded");
        assert_eq!(
            word_expand("$POSIXUTILS_MORE_TEST_VAR").unwrap(),
            "/tmp/expanded"
        );
        assert_eq!(word_expand("plain_name").unwrap(), "plain_name");
        assert!(word_expand("~").unwrap().starts_with('/'));
    }

    #[test]
    fn word_expand_rejects_multiple_pathnames() {
        // "if more than a single pathname results, the effects are
        // unspecified" -- treated as an error so the current file is kept.
        std::env::set_var("POSIXUTILS_MORE_TEST_TWO", "one two");
        assert!(word_expand("$POSIXUTILS_MORE_TEST_TWO").is_err());
        assert!(word_expand("'unterminated").is_err());
    }

    use super::{render_display_line, RenderOpts, StyleType, CELL_CONT};

    /// Render `src` and return the drawn text, the styles, and the bytes taken.
    fn render(src: &[u8], cols: Option<usize>) -> (String, Vec<StyleType>, usize) {
        let opts = RenderOpts { plain: false };
        let line = render_display_line(src, cols, opts, true).expect("at eof");
        let text = line
            .cells
            .iter()
            .map(|(c, _)| *c)
            .filter(|c| *c != CELL_CONT)
            .collect();
        let styles = line.cells.iter().map(|(_, st)| *st).collect();
        (text, styles, line.bytes)
    }

    fn text_of(src: &[u8]) -> String {
        render(src, None).0
    }

    #[test]
    fn render_expands_non_printable_characters() {
        // POSIX 107450-107452: the ed(1) list format -- named escapes, then
        // one three-digit octal number per byte.
        assert_eq!(text_of(b"ccc\x01ddd\n"), "ccc\\001ddd");
        assert_eq!(text_of(b"a\x7fb\n"), "a\\177b");
        assert_eq!(text_of(b"a\x07b\n"), "a\\ab");
        assert_eq!(text_of(b"a\x0bb\n"), "a\\vb");
        // Invalid bytes are escaped one at a time; valid UTF-8 is not, in any
        // locale.
        assert_eq!(text_of(b"a\xffb\n"), "a\\377b");
        assert_eq!(text_of("café\n".as_bytes()), "café");
        // A literal backslash is printable and is left alone.
        assert_eq!(text_of(b"a\\b\n"), "a\\b");
    }

    #[test]
    fn render_expands_tabs_to_stops() {
        assert_eq!(text_of(b"a\tb\n"), "a       b");
        assert_eq!(text_of(b"\tx\n"), "        x");
        assert_eq!(text_of(b"abcdefg\th\n"), "abcdefg h");
        assert_eq!(text_of(b"abcdefgh\ti\n"), "abcdefgh        i");
    }

    #[test]
    fn render_ignores_carriage_return_at_end_of_line() {
        // 107448-107449: a <carriage-return> ending a line is ignored rather
        // than written as a non-printable character.
        assert_eq!(text_of(b"zzz\r\n"), "zzz");
        assert_eq!(text_of(b"zzz\r"), "zzz");
        // Anywhere else it is an ordinary non-printable.
        assert_eq!(text_of(b"a\rb\n"), "a\\rb");
    }

    #[test]
    fn render_resolves_overstrike() {
        // 107432-107445. Underline, both orders.
        let (text, styles, _) = render(b"A\x08_\n", None);
        assert_eq!(text, "A");
        assert_eq!(styles, vec![StyleType::Underscore]);

        let (text, styles, _) = render(b"_\x08A\n", None);
        assert_eq!(text, "A");
        assert_eq!(styles, vec![StyleType::Underscore]);

        // "_\b_" underlines rather than emboldening.
        let (_, styles, _) = render(b"_\x08_\n", None);
        assert_eq!(styles, vec![StyleType::Underscore]);

        // Embolden, and "a\ba\ba\ba" is one emboldened 'a' (107443-107445).
        let (text, styles, _) = render(b"a\x08a\n", None);
        assert_eq!(text, "a");
        assert_eq!(styles, vec![StyleType::Negative]);

        let (text, styles, _) = render(b"a\x08a\x08a\x08a\n", None);
        assert_eq!(text, "a");
        assert_eq!(styles, vec![StyleType::Negative]);

        // Any other backspace discards itself and the character before it.
        assert_eq!(text_of(b"A\x08B\n"), "B");
        assert_eq!(text_of(b"\x08B\n"), "B");
    }

    #[test]
    fn render_plain_keeps_layout_but_drops_style() {
        let opts = RenderOpts { plain: true };
        let line = render_display_line(b"A\x08_ b\n", None, opts, true).unwrap();
        let text: String = line.cells.iter().map(|(c, _)| *c).collect();
        assert_eq!(text, "A b", "-u must not change layout");
        assert!(line.cells.iter().all(|(_, st)| *st == StyleType::None));
    }

    #[test]
    fn render_folds_without_splitting_or_dropping() {
        // A unit that does not fit begins the next line rather than being
        // split or discarded (107452-107453).
        let (text, _, bytes) = render(b"abcdefghij\n", Some(4));
        assert_eq!(text, "abcd");
        assert_eq!(bytes, 4);

        // An escape is four columns wide and is not split across the fold.
        let (text, _, bytes) = render(b"ab\x01cd\n", Some(4));
        assert_eq!(text, "ab");
        assert_eq!(bytes, 2);
        let (text, _, _) = render(b"\x01cd\n", Some(4));
        assert_eq!(text, "\\001");
    }

    #[test]
    fn render_byte_accounting_round_trips() {
        // Every byte of the source must be accounted for exactly once, or
        // the seek offsets built from these counts would drift.
        let src: &[u8] =
            b"plain\nA\x08_ under\na\x08a bold\n\ttab\n\x01\xff\n\r\ncaf\xc3\xa9\n\n\n";
        for cols in [None, Some(3), Some(7), Some(40)] {
            let mut offset = 0usize;
            let mut guard = 0;
            while offset < src.len() {
                let opts = RenderOpts { plain: false };
                let line = render_display_line(&src[offset..], cols, opts, true).expect("at eof");
                assert!(
                    line.bytes > 0,
                    "no progress at offset {offset} cols {cols:?}"
                );
                offset += line.bytes;
                guard += 1;
                assert!(guard < 1000, "runaway at cols {cols:?}");
            }
            assert_eq!(offset, src.len(), "byte drift at cols {cols:?}");
        }
    }

    #[test]
    fn env_screen_size_rejects_unusable_values() {
        use super::env_screen_size;

        // POSIX 107336/107357: COLUMNS and LINES override the system size.
        // A variable that is unset, empty, unparsable, or zero is treated as
        // absent so the system-selected size is used instead.
        std::env::set_var("POSIXUTILS_MORE_SIZE", "40");
        assert_eq!(env_screen_size("POSIXUTILS_MORE_SIZE"), Some(40));
        std::env::set_var("POSIXUTILS_MORE_SIZE", " 40 ");
        assert_eq!(env_screen_size("POSIXUTILS_MORE_SIZE"), Some(40));
        for bad in ["", "0", "abc", "-5", "99999999"] {
            std::env::set_var("POSIXUTILS_MORE_SIZE", bad);
            assert_eq!(env_screen_size("POSIXUTILS_MORE_SIZE"), None, "for {bad:?}");
        }
        std::env::remove_var("POSIXUTILS_MORE_SIZE");
        assert_eq!(env_screen_size("POSIXUTILS_MORE_SIZE"), None);
    }

    #[test]
    fn find_tags_files_walks_subdirectories() {
        let dir = std::env::temp_dir().join("posixutils_more_tags_walk");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("a/b")).unwrap();
        std::fs::write(dir.join("tags"), "").unwrap();
        std::fs::write(dir.join("a/b/tags"), "").unwrap();
        std::fs::write(dir.join("a/not_tags"), "").unwrap();

        let mut found = find_tags_files(&dir);
        found.sort();
        assert_eq!(found.len(), 2, "found {found:?}");
        assert!(found.iter().all(|p| p.file_name() == Some("tags".as_ref())));

        assert!(find_tags_files(Path::new(&dir.join("missing"))).is_empty());

        let _ = std::fs::remove_dir_all(&dir);
    }
}
