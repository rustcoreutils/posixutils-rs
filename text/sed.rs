//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::gettext;
use plib::regex::{Regex as PlibRegex, RegexFlags};
use std::rc::Rc;
use std::sync::Mutex;
use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Debug},
    fs::File,
    io::{BufRead, BufReader, Error, ErrorKind, Write},
    ops::Range,
    path::PathBuf,
};

static ERE: Mutex<bool> = Mutex::new(false);

#[derive(Parser, Debug, Clone)]
#[command(version, about = gettext("sed - stream editor"))]
struct Args {
    #[arg(short = 'E', help=gettext("Match using extended regular expressions."))]
    ere: bool,

    #[arg(short = 'n', help=gettext("Suppress the default output. Only lines explicitly selected for output are written."))]
    quiet: bool,

    #[arg(short = 'e', help=gettext("Add the editing commands specified by the script option-argument to the end of the script of editing commands."))]
    script: Vec<String>,

    #[arg(short = 'f', name = "SCRIPT_FILE", help=gettext("Add the editing commands in the file script_file to the end of the script of editing commands."))]
    script_file: Vec<PathBuf>,

    #[arg(help=gettext("A pathname of a file whose contents are read and edited."))]
    file: Vec<String>,
}

impl Args {
    // Get ordered script sources from [-e script] and [-f script_file] manually.
    fn get_raw_script() -> Result<String, SedError> {
        let mut raw_scripts: Vec<String> = vec![];

        let args: Vec<String> = std::env::args().skip(1).collect();
        let mut args_iter = args.iter();

        while let Some(arg) = args_iter.next() {
            match arg.as_str() {
                "-e" => {
                    // Can unwrap because `-e` is already validated by `clap`.
                    let e_script = args_iter.next().unwrap();
                    for raw_script_line in e_script.split('\n') {
                        raw_scripts.push(raw_script_line.to_string());
                    }
                    if let Some(script) = raw_scripts.last_mut() {
                        *script += "\n;";
                    }
                }
                "-f" => {
                    // Can unwrap because `-f` is already validated by `clap`.
                    let script_file =
                        File::open(args_iter.next().unwrap()).map_err(SedError::Io)?;
                    let reader = BufReader::new(script_file);
                    for line in reader.lines() {
                        let raw_script = line.map_err(SedError::Io)?;
                        raw_scripts.push(raw_script);
                    }
                    if let Some(script) = raw_scripts.last_mut() {
                        *script += "\n;";
                    }
                }
                _ => continue,
            }
        }

        Ok(raw_scripts.join("\n"))
    }

    /// Creates [`Sed`] from [`Args`], if [`Script`]
    /// parsing is failed, then returns error
    fn try_to_sed(mut self: Args) -> Result<Sed, SedError> {
        let mut raw_script = Self::get_raw_script()?;

        if raw_script.is_empty() {
            if self.file.is_empty() {
                return Err(SedError::NoScripts);
            } else {
                // Neither [-e script] nor [-f script_file] is supplied and [file...] is not empty
                // then consider first [file...] as single script. Preserve embedded
                // newlines verbatim: they delimit commands and terminate the text of
                // multi-line `a`/`i`/`c` commands and `:`/`b`/`t` labels.
                raw_script = self.file.remove(0);
            }
        }

        // If no [file...] were supplied or single file is considered to to be script, then
        // sed must read input from STDIN.
        if self.file.is_empty() {
            self.file.push("-".to_string());
        }

        // POSIX: the -E/-r flag selects ERE for *all* regexes in the script.
        // This must be set BEFORE parsing, because Script::parse compiles every
        // regex (addresses and `s` patterns) at parse time.
        *ERE.lock().unwrap() = self.ere;

        let script = Script::parse(raw_script)?;
        script.check_labels()?;

        Ok(Sed {
            quiet: self.quiet,
            script,
            input_sources: self.file,
            pattern_space: String::new(),
            hold_space: String::new(),
            current_file: None,
            current_line: 0,
            has_replacements_since_t: false,
            last_regex: None,
            is_last_line: false,
            current_end: None,
            next_line: String::new(),
            append_queue: Vec::new(),
        })
    }
}

/// Errors that can be returned by [`Sed`] and its inner functions
#[derive(thiserror::Error, Debug)]
enum SedError {
    /// Sed didn't get script for processing input files
    #[error("none script was supplied")]
    NoScripts,
    /// [`Script`] doesn't contain label that used in
    /// [`Command::BranchToLabel`] or [`Command::Test`]
    #[error("script doesn't contain label '{}'", .0)]
    NoLabel(String),
    /// [`Command::Replace`] pattern is empty and script doesn't has last regex
    #[error("no previous regular expression")]
    NoRegex,
    /// Files, stdin read/write errors
    #[error("{0}")]
    Io(#[from] std::io::Error),
    /// Sed can`t parse raw script string.
    /// Can't parse string, reason is:
    #[error("{}{}", .0, format_error_position(*(.1)))]
    ScriptParse(String, Option<(usize, usize)>),
    /// Runtime error when processing file
    #[error("read {}: {}", .0, .1)]
    Runtime(String, String),
}

/// Define line number or range limits of [`Address`]
/// for applying [`Command`]
#[derive(Clone)]
enum AddressToken {
    /// Line number
    Number(usize),
    /// Last line
    Last,
    /// Context related line number that
    /// calculated from this BRE match
    Pattern(Regex, String),
    /// Used for handling char related exceptions, when parsing [`AddressRange`]
    Delimiter,
}

impl PartialEq for AddressToken {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (AddressToken::Number(a), AddressToken::Number(b)) => a == b,
            (AddressToken::Last, AddressToken::Last) => true,
            (AddressToken::Pattern(..), AddressToken::Pattern(..)) => true,
            (AddressToken::Delimiter, AddressToken::Delimiter) => true,
            _ => false,
        }
    }
}

impl Eq for AddressToken {}

impl Debug for AddressToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AddressToken::Number(n) => f
                .debug_struct("AddressToken::Number")
                .field("0", n)
                .finish(),
            AddressToken::Last => f.debug_struct("AddressToken::Last").finish(),
            AddressToken::Pattern(_, pattern) => f
                .debug_struct(&format!("AddressToken::Pattern({pattern})"))
                .finish(),
            AddressToken::Delimiter => f.debug_struct("AddressToken::Delimiter").finish(),
        }
    }
}

/// List of [`AddressToken`]s that defines line position or range
#[derive(Debug, Clone)]
struct AddressRange {
    /// Address range limits
    limits: Vec<AddressToken>,
    /// A two-address range has selected its first line and not yet its last
    active: bool,
    /// Inverse fulfillment of [`AddressRange`] conditions
    is_negative: bool,
}

impl AddressRange {
    fn new(limits: Vec<AddressToken>, is_negative: bool) -> Result<Option<Self>, SedError> {
        match limits.len() {
            0 => Ok(None),
            1 | 2 => Ok(Some(Self {
                limits,
                active: false,
                is_negative,
            })),
            _ => Err(SedError::ScriptParse(
                "address isn't empty, position or range".to_string(),
                None,
            )),
        }
    }

    /// Whether this address selects the current line (before `!`). POSIX: a
    /// range starts at a line matching its first address and ends at the
    /// next line matching its second; the second is not checked against the
    /// starting line, and a second line number at or before the starting line
    /// selects only that line.
    fn selects(
        &mut self,
        line: &LineState,
        last_regex: &mut Option<Regex>,
    ) -> Result<bool, SedError> {
        let AddressRange { limits, active, .. } = self;
        match limits.as_slice() {
            [only] => only.matches(line, last_regex),
            [start, end] => {
                if *active {
                    let ended = match end {
                        AddressToken::Number(n) => line.number >= *n,
                        token => token.matches(line, last_regex)?,
                    };
                    *active = !ended;
                    Ok(true)
                } else if start.matches(line, last_regex)? {
                    *active = match end {
                        AddressToken::Number(n) => *n > line.number,
                        AddressToken::Last => !line.last,
                        _ => true,
                    };
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            _ => unreachable!(),
        }
    }
}

/// What an address is tested against: the current line's number (from 1),
/// whether it is the last line, and the pattern space
struct LineState<'a> {
    number: usize,
    last: bool,
    text: &'a str,
}

impl AddressToken {
    fn matches(&self, line: &LineState, last_regex: &mut Option<Regex>) -> Result<bool, SedError> {
        Ok(match self {
            AddressToken::Number(n) => *n == line.number,
            AddressToken::Last => line.last,
            AddressToken::Pattern(re, pattern) => {
                // POSIX: an empty RE is the last RE used, by an address or an
                // `s` command; any RE used becomes the last one.
                let re = if pattern.is_empty() {
                    last_regex.clone().ok_or(SedError::NoRegex)?
                } else {
                    re.clone()
                };
                let matched = re.0.is_match(line.text);
                *last_regex = Some(re);
                matched
            }
            AddressToken::Delimiter => unreachable!(),
        })
    }
}

/// Address define line position or range for
/// applying [`Command`]
#[derive(Debug, Clone)]
struct Address(
    /// List of [`AddressRange`]s. If conditions for every
    /// item in this list are met then [`Command`] with
    /// this [`Address`] is processed
    Vec<AddressRange>,
);

/// [`Command::Replace`] optional flags
#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
enum ReplaceFlag {
    /// Substitute for the nth occurrence only of the
    /// BRE found within the pattern space
    ReplaceNth(usize), // n
    /// Globally substitute for all non-overlapping
    /// instances of the BRE rather than just the first one
    ReplaceAll, // g
    /// Write the pattern space to standard output if
    /// a replacement was made
    PrintPatternIfReplace, // p
    /// Write. Append the pattern space to wfile if a
    /// replacement was made
    AppendToIfReplace(PathBuf), // w
    /// Case-insensitive matching (POSIX.1-2024 `i`/`I` flag)
    CaseInsensitive, // i / I
}

/// Newtype for implementing [`Debug`] trait for Regex; shared, so recording
/// the last RE used costs no recompilation
#[derive(Clone)]
struct Regex(Rc<PlibRegex>);

impl Debug for Regex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Regex").field(&self.0.as_str()).finish()
    }
}

/// Atomic parts of [`Script`], that can process input
/// files line by line
#[derive(Debug, Clone)]
enum Command {
    /// Execute a list of sed editing commands only
    /// when the pattern space is selected
    Block(Option<Address>, Vec<Command>), // {
    /// A flattened block: when its address does not select the pattern
    /// space, skip the block's next `usize` commands
    BlockStart(Option<Address>, usize), // {
    /// Write text to standard output as described previously
    PrintTextAfter(Option<Address>, String), // a
    /// Branch to the : command verb bearing the label
    /// argument. If label is not specified, branch to
    /// the end of the script
    BranchToLabel(Option<Address>, Option<String>), // b
    /// Delete the pattern space. With a 0 or 1 address
    /// or at the end of a 2-address range, place text
    /// on the output and start the next cycle
    DeletePatternAndPrintText(Option<Address>, String), // c
    /// Delete the pattern space and start the next cycle (d)
    /// If the pattern space contains no <newline>,
    /// delete the pattern space and start new cycle (D)
    DeletePattern(Option<Address>, bool), // d/D
    /// Replace the contents of the pattern
    /// space by the contents of the hold space
    ReplacePatternWithHold(Option<Address>), // g
    /// Append to the pattern space a <newline>
    /// followed by the contents of the hold space
    AppendHoldToPattern(Option<Address>), // G
    /// Replace the contents of the hold space
    /// with the contents of the pattern space
    ReplaceHoldWithPattern(Option<Address>), // h
    /// Append to the hold space a <newline> followed
    /// by the contents of the pattern space
    AppendPatternToHold(Option<Address>), // H
    /// Write text to standard output
    PrintTextBefore(Option<Address>, String), // i
    /// Write the pattern space to standard
    /// output in a visually unambiguous form.
    /// Non-POSIX EXTENSION (`I`); kept for backwards compatibility.
    PrintPatternBinary(Option<Address>), // I
    /// POSIX `l`: write the pattern space to standard output in a
    /// visually unambiguous form, with an optional line-wrap width.
    PrintPatternList(Option<Address>, Option<usize>), // l
    /// Write the pattern space to standard output
    /// and replace pattern space with next line,
    /// then continue current cycle
    PrintPatternAndReplaceWithNext(Option<Address>), // n
    /// Append the next line of input, less its
    /// terminating <newline>, to the pattern space
    AppendNextToPattern(Option<Address>), // N
    /// Write the pattern space to standard output (p).
    /// Write the pattern space, up to the first <newline>,
    /// to standard output (P).
    PrintPattern(Option<Address>, bool), // p/P
    /// Branch to the end of the script and quit without
    /// starting a new cycle
    Quit(Option<Address>), // q
    /// Copy the contents of rfile to standard output
    PrintFile(Option<Address>, PathBuf), // r
    /// Substitute the replacement string for instances
    /// of the BRE in the pattern space
    Replace(Option<Address>, Regex, String, String, Vec<ReplaceFlag>), // s
    /// Test. Branch to the : command verb bearing the
    /// label if any substitutions have been made since
    /// the most recent reading of an input line or
    /// 't' execution
    Test(Option<Address>, Option<String>), // t
    /// Append (write) the pattern space to wfile
    AppendPatternToFile(Option<Address>, PathBuf), // w
    /// Exchange the contents of the pattern and hold spaces
    ExchangeSpaces(Option<Address>), // x
    /// Replace all occurrences of characters in string1
    /// with the corresponding characters in string2
    ReplaceCharSet(Option<Address>, String, String), // y
    /// Do nothing. This command bears a label to which
    /// the b and t commands branch.
    BearBranchLabel(String), // :
    /// Write the following to standard output:
    /// "%d\n", <current line number>
    PrintStandard(Option<Address>), // =
    /// Ignore remainder of the line (treat it as a comment)
    IgnoreComment, // #
    /// Char sequence that can`t be recognised as `Command`
    _Unknown,
}

impl Command {
    fn get_mut_address(&mut self) -> Option<(&mut Option<Address>, usize)> {
        let (address, i) = match self {
            Command::Block(address, ..) => (address, 2),
            Command::BlockStart(address, ..) => (address, 2),
            Command::PrintTextAfter(address, ..) => (address, 1),
            Command::BranchToLabel(address, ..) => (address, 2),
            Command::DeletePatternAndPrintText(address, ..) => (address, 2),
            Command::DeletePattern(address, ..) => (address, 2),
            Command::ReplacePatternWithHold(address) => (address, 2),
            Command::AppendHoldToPattern(address) => (address, 2),
            Command::ReplaceHoldWithPattern(address) => (address, 2),
            Command::AppendPatternToHold(address) => (address, 2),
            Command::PrintTextBefore(address, ..) => (address, 1),
            Command::PrintPatternBinary(address) => (address, 2),
            Command::PrintPatternList(address, ..) => (address, 2),
            Command::PrintPatternAndReplaceWithNext(address) => (address, 2),
            Command::AppendNextToPattern(address) => (address, 2),
            Command::PrintPattern(address, ..) => (address, 2),
            Command::Quit(address) => (address, 1),
            Command::PrintFile(address, ..) => (address, 1),
            Command::Replace(address, ..) => (address, 2),
            Command::Test(address, ..) => (address, 2),
            Command::AppendPatternToFile(address, ..) => (address, 2),
            Command::ExchangeSpaces(address) => (address, 2),
            Command::ReplaceCharSet(address, ..) => (address, 2),
            Command::PrintStandard(address) => (address, 1),
            _ => return None,
        };

        Some((address, i))
    }

    /// If [`Command`] address has more [`AddressToken`]
    /// then it can have, return error
    fn check_address(&mut self) -> Result<(), SedError> {
        let Some((address, max_len)) = self.get_mut_address() else {
            return Ok(());
        };
        if address.is_none() {
            return Ok(());
        }
        for condition in &address.as_ref().unwrap().0 {
            if condition.limits.len() > max_len {
                let message = match max_len {
                    0 => unreachable!(),
                    1 => "isn't position",
                    2 => "isn't position or range",
                    _ => "has more boundaries than can be handled",
                };
                return Err(SedError::ScriptParse(
                    format!("address {} in command {:?}", message, self),
                    None,
                ));
            }
        }
        Ok(())
    }

    /// Check if [`Command`] apply conditions are met for current line
    fn need_execute(
        &mut self,
        line: &LineState,
        last_regex: &mut Option<Regex>,
    ) -> Result<bool, SedError> {
        let Some((Some(address), _)) = self.get_mut_address() else {
            return Ok(true);
        };
        let mut need_execute = true;
        for range in address.0.iter_mut() {
            need_execute &= range.selects(line, last_regex)? != range.is_negative;
        }
        Ok(need_execute)
    }
}

/// Find every match of `re` in `haystack`, left to right and
/// non-overlapping; each entry holds the capture ranges, the whole match
/// first. An empty match adjacent to the previous match is not a match (so
/// `s/b*/x/g` on `abc` gives `xaxcx`).
fn find_matches(re: &PlibRegex, haystack: &str) -> Vec<Vec<Range<usize>>> {
    let mut matches: Vec<Vec<Range<usize>>> = vec![];
    let mut offset = 0;
    while let Some(caps) = re.captures_at(haystack, offset) {
        let whole = caps[0].start..caps[0].end;
        let adjacent = whole.is_empty() && matches.last().is_some_and(|m| m[0].end == whole.start);
        if !adjacent {
            matches.push(caps.iter().map(|m| m.start..m.end).collect());
        }
        offset = if whole.is_empty() {
            match haystack[whole.end..].chars().next() {
                Some(ch) => whole.end + ch.len_utf8(),
                None => break,
            }
        } else {
            whole.end
        };
    }
    matches
}

/// Parse sequence of digits as [`usize`]
fn parse_number(chars: &[char], i: &mut usize) -> Result<Option<usize>, SedError> {
    let mut number_str = String::new();
    loop {
        let Some(ch) = chars.get(*i) else {
            return Err(SedError::ScriptParse(
                "script ended unexpectedly".to_string(),
                None,
            ));
        };
        if !ch.is_ascii_digit() {
            break;
        }
        number_str.push(*ch);
        *i += 1;
    }

    if number_str.is_empty() {
        return Ok(None);
    }

    let number = number_str.parse::<usize>().map_err(|_| {
        let position = get_current_line_and_col(chars, *i);
        SedError::ScriptParse("can't parse number".to_string(), position)
    })?;
    Ok(Some(number))
}

/// Scan the text of a delimited RE or replacement, starting at `chars[*i]`
/// (just after the opening `delim`). On success `*i` is left on the closing
/// delimiter. Returns `None` when the text is unterminated.
///
/// POSIX: a backslash followed by the delimiter stands for the literal
/// delimiter, and `\n` in an RE matches a <newline>. A backslash followed by a
/// <newline> is a literal <newline>. Every other `\x` pair is kept intact
/// for the regex compiler or the replacement expander; in a replacement whose
/// delimiter is `&`, `\&` stays escaped so it remains a literal `&`.
fn scan_delimited(chars: &[char], i: &mut usize, delim: char, is_re: bool) -> Option<String> {
    let mut text = String::new();
    loop {
        let ch = *chars.get(*i)?;
        if ch == delim {
            return Some(text);
        }
        match ch {
            '\n' => return None,
            '\\' => {
                let next = *chars.get(*i + 1)?;
                if next == '\n' {
                    text.push('\n');
                } else if next == delim && (is_re || delim != '&') {
                    text.push(delim);
                } else if next == 'n' && is_re {
                    text.push('\n');
                } else {
                    text.push('\\');
                    text.push(next);
                }
                *i += 2;
            }
            _ => {
                text.push(ch);
                *i += 1;
            }
        }
    }
}

/// Parse a context address `/RE/` or `\cREc` starting at `chars[*i]`;
/// leaves `*i` on the closing delimiter.
fn parse_pattern_token(
    chars: &[char],
    i: &mut usize,
    tokens: &mut Vec<AddressToken>,
) -> Result<(), SedError> {
    let position = get_current_line_and_col(chars, *i);
    if chars[*i] == '\\' {
        *i += 1;
    }
    let unterminated = || SedError::ScriptParse("unterminated address regex".to_string(), position);
    let delim = *chars.get(*i).ok_or_else(unterminated)?;
    if delim == '\\' || delim == '\n' {
        return Err(SedError::ScriptParse(
            format!("pattern spliter is '{}'", delim),
            position,
        ));
    }
    *i += 1;
    let pattern = scan_delimited(chars, i, delim, true).ok_or_else(unterminated)?;
    let re = compile_regex(pattern.clone())?;
    tokens.push(AddressToken::Pattern(Regex(Rc::new(re)), pattern));
    Ok(())
}

/// Highlight future [`Address`] string and split it on [`AddressToken`]s
fn to_address_tokens(chars: &[char], i: &mut usize) -> Result<Vec<AddressToken>, SedError> {
    let mut tokens = vec![];
    loop {
        let Some(ch) = chars.get(*i) else {
            return Err(SedError::ScriptParse(
                "script ended unexpectedly".to_string(),
                None,
            ));
        };
        match ch {
            ch if ch.is_ascii_digit() => {
                let Some(number) = parse_number(chars, i)? else {
                    unreachable!();
                };
                tokens.push(AddressToken::Number(number));
                continue;
            }
            '\\' | '/' => parse_pattern_token(chars, i, &mut tokens)?,
            '$' => tokens.push(AddressToken::Last),
            ',' => tokens.push(AddressToken::Delimiter),
            ' ' | '\t' => {}
            _ => break,
        }
        *i += 1;
    }

    Ok(tokens)
}

/// Convert [`AddressToken`]s to [`Address`]
fn tokens_to_address(
    tokens: Vec<AddressToken>,
    is_negative: bool,
) -> Result<Option<Address>, SedError> {
    if tokens
        .iter()
        .enumerate()
        .filter(|(i, _)| i % 2 == 1)
        .any(|(_, token)| !matches!(token, AddressToken::Delimiter))
        || tokens.last() == Some(&AddressToken::Delimiter)
    {
        return Err(SedError::ScriptParse(
            "address bound can be only one pattern, number or '$'".to_string(),
            None,
        ));
    }

    let tokens = tokens
        .into_iter()
        .filter(|token| !matches!(token, AddressToken::Delimiter))
        .collect::<Vec<_>>();
    if let Some(range) = AddressRange::new(tokens, is_negative)? {
        if range
            .limits
            .iter()
            .any(|token| AddressToken::Number(0) == *token)
        {
            return Err(SedError::ScriptParse(
                "address lower bound must be bigger than 0".to_string(),
                None,
            ));
        }
        return Ok(Some(Address(vec![range])));
    }
    Ok(None)
}

/// Get current line and column in script parse process
fn get_current_line_and_col(chars: &[char], i: usize) -> Option<(usize, usize)> {
    let mut j = 0;
    let lines_positions = chars
        .split(|c| *c == '\n')
        .map(|line| {
            let k = j;
            j += line.len() + 1;
            (line, k)
        })
        .collect::<Vec<_>>();
    let (line, _) = lines_positions
        .iter()
        .enumerate()
        .find(|(_, (_, line_start))| {
            if i >= *line_start {
                return false;
            }
            true
        })?;
    let line = line.saturating_sub(1);
    let col = i - lines_positions[line].1 + 1;
    Some((line, col))
}

/// Format string for current script line and column
fn format_error_position(position: Option<(usize, usize)>) -> String {
    if let Some((line, col)) = position {
        format!(" (line: {}, col: {})", line, col)
    } else {
        String::new()
    }
}

/// Parse count argument of future [`Command`]
fn parse_address(
    chars: &[char],
    i: &mut usize,
    address: &mut Option<Address>,
) -> Result<(), SedError> {
    let tokens = to_address_tokens(chars, i)?;
    let mut is_negative = false;
    while let Some(ch) = chars.get(*i) {
        match ch {
            '!' => {
                is_negative = true;
                *i += 1;
                break;
            }
            ' ' | '\t' => (),
            _ => break,
        }
        *i += 1;
    }
    match tokens_to_address(tokens, is_negative) {
        Ok(new_address) => *address = new_address,
        Err(SedError::ScriptParse(message, position)) => {
            let position = if position.is_some() {
                position
            } else {
                get_current_line_and_col(chars, *i)
            };
            return Err(SedError::ScriptParse(message, position));
        }
        _ => unreachable!(),
    }
    Ok(())
}

/// Parse text attribute of a, c, i [`Command`]s that formated as:
/// a\
/// text
fn parse_text_attribute(chars: &[char], i: &mut usize) -> Result<Option<String>, SedError> {
    *i += 1;
    let Some(ch) = chars.get(*i) else {
        return Err(SedError::ScriptParse(
            "script ended unexpectedly".to_string(),
            None,
        ));
    };
    if *ch != '\\' {
        let position = get_current_line_and_col(chars, *i);
        return Err(SedError::ScriptParse(
            "text must be separated with '\\'".to_string(),
            position,
        ));
    }
    *i += 1;
    // POSIX multi-line form: `a\` followed by a <newline> begins the text on the
    // following line(s); a line ending in `\` continues to the next line.
    // Do NOT skip the newline when it is immediately followed by a command
    // delimiter / end-of-script, which is the empty `a\` (missing text) case.
    if chars.get(*i) == Some(&'\n') && !matches!(chars.get(*i + 1), Some(';') | None) {
        *i += 1;
    }
    let mut text = String::new();
    while let Some(ch) = chars.get(*i) {
        match *ch {
            '\n' => {
                // An unescaped <newline> terminates the text: the text ends at
                // the first line that does NOT end in a backslash. Back up one
                // so the caller's `i += 1` lands on this newline, which is then
                // processed as a command delimiter; the next line is the next
                // command. (Only when text was collected; an empty text is an
                // error and keeps the index on the newline for diagnostics.)
                if !text.is_empty() {
                    *i -= 1;
                }
                break;
            }
            '\\' => {
                *i += 1;
                match chars.get(*i) {
                    // `\` before a newline is a continuation: emit a real
                    // newline and keep reading the next line.
                    Some('\n') => {
                        text.push('\n');
                        *i += 1;
                    }
                    // Any other escaped char: drop the backslash, keep the char.
                    Some(c) => {
                        text.push(*c);
                        *i += 1;
                    }
                    None => break,
                }
                continue;
            }
            _ => (),
        }
        text.push(*ch);
        *i += 1;
    }
    if text.is_empty() {
        Ok(None)
    } else {
        Ok(Some(text))
    }
}

/// Parse the label of a b, t or : [`Command`]. As in GNU sed, the label ends
/// at a <newline>, `;`, `#` (which then begins a comment) or `}` (which
/// closes the enclosing block).
fn parse_word_attribute(chars: &[char], i: &mut usize) -> Result<Option<String>, SedError> {
    let mut label = String::new();
    while let Some(ch) = chars.get(*i) {
        match ch {
            '\n' | ';' | '#' | '}' => {
                *i -= 1;
                break;
            }
            _ => label.push(*ch),
        }
        *i += 1;
        if *i > chars.len() {
            break;
        }
    }
    let label = label.trim().to_string();
    if label.contains(' ') {
        let position = get_current_line_and_col(chars, *i);
        return Err(SedError::ScriptParse(
            "label can't contain ' '".to_string(),
            position,
        ));
    }
    Ok(if label.is_empty() { None } else { Some(label) })
}

/// Parse rfile attribute of r [`Command`]
fn parse_path_attribute(chars: &[char], i: &mut usize) -> Result<PathBuf, SedError> {
    *i += 1;
    let mut path = String::new();
    while let Some(ch) = chars.get(*i) {
        match ch {
            '\n' | ';' => {
                *i -= 1;
                break;
            }
            _ => path.push(*ch),
        }
        *i += 1;
        if *i >= chars.len() {
            break;
        }
    }
    let path = path.trim();
    if path.is_empty() {
        let position = get_current_line_and_col(chars, *i);
        return Err(SedError::ScriptParse(
            "missing filename in r/R/w/W commands".to_string(),
            position,
        ));
    }
    let file = PathBuf::from(path);
    if file.exists() {
        if file.is_file() {
            Ok(file)
        } else {
            Err(SedError::Io(Error::new(
                ErrorKind::InvalidInput,
                format!("{} isn't file", file.display()),
            )))
        }
    } else {
        Ok(file)
    }
}

/// Parse s, y [`Command`]s that formated as:
/// x/string1/string2/
fn parse_replace_command(
    chars: &[char],
    i: &mut usize,
    command: String,
) -> Result<(String, String), SedError> {
    *i += 1;
    let Some(splitter) = chars.get(*i) else {
        return Err(SedError::ScriptParse(
            "script ended unexpectedly".to_string(),
            None,
        ));
    };
    if splitter.is_alphanumeric() || " \n;{".contains(*splitter) {
        let position = get_current_line_and_col(chars, *i);
        return Err(SedError::ScriptParse(
            format!("unterminated `{}' command", command),
            position,
        ));
    }
    let delim = *splitter;
    *i += 1;
    let position = get_current_line_and_col(chars, *i);
    let parse_error =
        || SedError::ScriptParse(format!("unterminated `{}' command", command), position);
    let is_s = command == "s";
    let pattern = scan_delimited(chars, i, delim, is_s).ok_or_else(parse_error)?;
    *i += 1;
    let replacement = scan_delimited(chars, i, delim, false).ok_or_else(parse_error)?;
    *i += 1;
    Ok((pattern, replacement))
}

/// Unescape a `y///` operand at parse time, building the real character list.
///
/// POSIX: inside a `y` operand a backslash may escape the delimiter, a
/// backslash, or `n` (newline). GNU additionally recognises the usual C
/// escapes (`\t`, `\r`). Any other `\X` yields the literal `X`.
fn unescape_transliteration(s: &str) -> Vec<char> {
    let mut out = vec![];
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('r') => out.push('\r'),
                Some('\\') => out.push('\\'),
                Some(other) => out.push(other),
                None => out.push('\\'),
            }
        } else {
            out.push(c);
        }
    }
    out
}

/// Parse [`Command::Replace`] flags
fn parse_replace_flags(chars: &[char], i: &mut usize) -> Result<Vec<ReplaceFlag>, SedError> {
    let mut flags = vec![];
    let mut flag_map = HashMap::from([('n', 0), ('g', 0), ('p', 0), ('w', 0)]);
    let mut w_start_position = None;
    while let Some(ch) = chars.get(*i) {
        match ch {
            _ if ch.is_ascii_digit() => {
                let n = ch.to_digit(10).unwrap() as usize;
                *flag_map.get_mut(&'n').unwrap() += 1;
                flags.push(ReplaceFlag::ReplaceNth(n));
            }
            'g' => {
                *flag_map.get_mut(&'g').unwrap() += 1;
                flags.push(ReplaceFlag::ReplaceAll)
            }
            'p' => {
                *flag_map.get_mut(&'p').unwrap() += 1;
                flags.push(ReplaceFlag::PrintPatternIfReplace)
            }
            'i' | 'I' => flags.push(ReplaceFlag::CaseInsensitive),
            'w' => {
                if w_start_position.is_none() {
                    w_start_position = Some(*i);
                }
                *flag_map.get_mut(&'w').unwrap() += 1;
                flags.push(ReplaceFlag::AppendToIfReplace(PathBuf::new()));
                *i += 1;
                break;
            }
            ' ' => {}
            _ => {
                *i -= 1;
                break;
            }
        }
        *i += 1;
    }

    let eq_w = |f| matches!(f, ReplaceFlag::AppendToIfReplace(_));
    let w_flag_position = flags.iter().cloned().position(eq_w);
    let is_w_last = || w_flag_position.unwrap() == (flags.len() - 1);
    if w_flag_position.is_some() && !is_w_last() {
        let position = get_current_line_and_col(chars, *i);
        return Err(SedError::ScriptParse(
            "w flag must be last flag".to_string(),
            position,
        ));
    } else if flag_map.values().any(|k| *k > 1) && is_w_last() {
        let position = get_current_line_and_col(chars, *i);
        return Err(SedError::ScriptParse(
            "flags can't be repeated".to_string(),
            position,
        ));
    }
    if let Some(w_start_position) = w_start_position {
        *i = w_start_position;
        let path = parse_path_attribute(chars, i)?;
        flags.push(ReplaceFlag::AppendToIfReplace(path));
    }

    let is_replace_nth = |f| matches!(f, ReplaceFlag::ReplaceNth(_));
    if flags.iter().cloned().any(is_replace_nth) && flags.contains(&ReplaceFlag::ReplaceAll) {
        let position = get_current_line_and_col(chars, *i);
        return Err(SedError::ScriptParse(
            "n and g flags can't be used together".to_string(),
            position,
        ));
    }
    Ok(flags)
}

/// Compiles [`pattern`] as a POSIX regex (BRE or ERE based on ERE flag)
fn compile_regex(pattern: String) -> Result<PlibRegex, SedError> {
    compile_regex_icase(pattern, false)
}

/// Compiles [`pattern`] as a POSIX regex, optionally case-insensitive
/// (REG_ICASE), used by the `s///i` flag (POSIX.1-2024, Defect 779).
fn compile_regex_icase(pattern: String, icase: bool) -> Result<PlibRegex, SedError> {
    // Check ERE flag to determine regex mode
    let ere = ERE.lock().unwrap();
    let mut flags = if *ere {
        RegexFlags::ere()
    } else {
        RegexFlags::bre()
    };
    if icase {
        flags = flags.ignore_case();
    }

    // plib::regex handles macOS empty pattern workaround internally
    PlibRegex::new(&pattern, flags).map_err(|e| {
        SedError::ScriptParse(format!("can't compile pattern '{}': {}", pattern, e), None)
    })
}

/// Default line-wrap width for the `l` command (GNU `sed` default is 70).
const DEFAULT_L_WIDTH: usize = 70;

/// Render the pattern space for the `l` command in a visually-unambiguous form
/// (POSIX `l`), byte-for-byte compatible with GNU `sed`:
///   * `\\` for backslash; `\a \b \f \n \r \t \v` for the C escapes;
///   * any other non-printable byte as three-digit octal `\NNN`;
///   * printable ASCII bytes verbatim;
///   * a trailing `$`, and folding at `width` columns with a trailing `\`.
///
/// A `width` of 0 disables folding.  Operates on raw bytes so multibyte/high
/// bytes are escaped octally exactly like GNU.
fn format_l(line: &str, width: usize) -> String {
    let mut out = String::new();
    let mut col = 0usize;
    for &b in line.as_bytes() {
        let esc: String = match b {
            0x07 => "\\a".to_string(),
            0x08 => "\\b".to_string(),
            0x0C => "\\f".to_string(),
            b'\n' => "\\n".to_string(),
            0x0D => "\\r".to_string(),
            0x09 => "\\t".to_string(),
            0x0B => "\\v".to_string(),
            b'\\' => "\\\\".to_string(),
            0x20..=0x7E => (b as char).to_string(),
            _ => format!("\\{:03o}", b),
        };
        let olen = esc.len();
        if width > 0 && col + olen >= width {
            out.push_str("\\\n");
            col = 0;
        }
        out.push_str(&esc);
        col += olen;
    }
    out.push_str("$\n");
    out
}

fn print_multiline_binary(line: &str, width: Option<usize>) {
    print!("{}", format_l(line, width.unwrap_or(DEFAULT_L_WIDTH)));
}

/// Skip a comment: move `i` from its `#` to the <newline> that ends it (or
/// to the end of the script).
fn skip_comment(chars: &[char], i: &mut usize) {
    *i = chars
        .iter()
        .skip(*i)
        .position(|ch| *ch == '\n')
        .map_or(chars.len(), |p| *i + p);
}

/// Parse commands from `chars[*pos]` up to the end of the script or, inside
/// a block opened by the `{` at index `open`, up to its matching `}` (on
/// which `*pos` is left).
fn parse_commands(
    chars: &[char],
    pos: &mut usize,
    open: Option<usize>,
) -> Result<Vec<Command>, SedError> {
    let mut commands = vec![];
    let mut address = None;
    let mut i = *pos;
    let mut last_commands_count = 0;
    let mut command_added = false;

    while let Some(ch) = chars.get(i) {
        match *ch {
            ' ' | '\t' => {}
            // A comment runs to the <newline>, which still separates commands.
            '#' => {
                skip_comment(chars, &mut i);
                continue;
            }
            // A bare <newline> is a command separator, exactly like `;`.
            ';' | '\n' => {
                if address.is_some() && !command_added {
                    let position = get_current_line_and_col(chars, i);
                    return Err(SedError::ScriptParse(
                        "address hasn't command".to_string(),
                        position,
                    ));
                }
                address = None;
                command_added = false
            }
            '}' => {
                let position = get_current_line_and_col(chars, i);
                if open.is_none() {
                    return Err(SedError::ScriptParse(
                        "unneccessary '}'".to_string(),
                        position,
                    ));
                }
                if address.is_some() && !command_added {
                    return Err(SedError::ScriptParse(
                        "address hasn't command".to_string(),
                        position,
                    ));
                }
                *pos = i;
                return Ok(commands);
            }
            _ if command_added => {
                let position = get_current_line_and_col(chars, i);
                return Err(SedError::ScriptParse(
                    "commands must be delimited with ';'".to_string(),
                    position,
                ));
            }
            ch if ch.is_ascii_digit() || "\\$/".contains(ch) => {
                parse_address(chars, &mut i, &mut address)?;
                continue;
            }
            '{' => {
                let open = i;
                i += 1;
                let mut block = parse_commands(chars, &mut i, Some(open))?;
                for cmd in block.iter_mut() {
                    cmd.check_address()?;
                }
                commands.push(Command::Block(address.clone(), block));
            }
            'a' => {
                if let Some(text) = parse_text_attribute(chars, &mut i)? {
                    commands.push(Command::PrintTextAfter(address.clone(), text));
                } else {
                    let position = get_current_line_and_col(chars, i);
                    return Err(SedError::ScriptParse(
                        "missing text argument".to_string(),
                        position,
                    ));
                }
            }
            'b' => {
                i += 1;
                let label = parse_word_attribute(chars, &mut i)?;
                commands.push(Command::BranchToLabel(address.clone(), label));
            }
            'c' => {
                if let Some(text) = parse_text_attribute(chars, &mut i)? {
                    commands.push(Command::DeletePatternAndPrintText(address.clone(), text));
                } else {
                    let position = get_current_line_and_col(chars, i);
                    return Err(SedError::ScriptParse(
                        "missing text argument".to_string(),
                        position,
                    ));
                }
            }
            'd' => commands.push(Command::DeletePattern(address.clone(), false)),
            'D' => commands.push(Command::DeletePattern(address.clone(), true)),
            'g' => commands.push(Command::ReplacePatternWithHold(address.clone())),
            'G' => commands.push(Command::AppendHoldToPattern(address.clone())),
            'h' => commands.push(Command::ReplaceHoldWithPattern(address.clone())),
            'H' => commands.push(Command::AppendPatternToHold(address.clone())),
            'i' => {
                if let Some(text) = parse_text_attribute(chars, &mut i)? {
                    commands.push(Command::PrintTextBefore(address.clone(), text));
                } else {
                    let position = get_current_line_and_col(chars, i);
                    return Err(SedError::ScriptParse(
                        "missing text argument".to_string(),
                        position,
                    ));
                }
            }
            'I' => commands.push(Command::PrintPatternBinary(address.clone())),
            'l' => {
                // Optional numeric line-wrap argument: `l n`.
                i += 1;
                while let Some(c) = chars.get(i) {
                    if *c == ' ' {
                        i += 1;
                    } else {
                        break;
                    }
                }
                let mut n_str = String::new();
                while let Some(c) = chars.get(i) {
                    if c.is_ascii_digit() {
                        n_str.push(*c);
                        i += 1;
                    } else {
                        break;
                    }
                }
                let width = if n_str.is_empty() {
                    None
                } else {
                    Some(n_str.parse::<usize>().map_err(|_| {
                        SedError::ScriptParse("can't parse number".to_string(), None)
                    })?)
                };
                i -= 1;
                commands.push(Command::PrintPatternList(address.clone(), width));
            }
            'n' => commands.push(Command::PrintPatternAndReplaceWithNext(address.clone())),
            'N' => commands.push(Command::AppendNextToPattern(address.clone())),
            'p' => commands.push(Command::PrintPattern(address.clone(), false)),
            'P' => commands.push(Command::PrintPattern(address.clone(), true)),
            'q' => commands.push(Command::Quit(address.clone())),
            'r' => {
                let rfile = parse_path_attribute(chars, &mut i)?;
                commands.push(Command::PrintFile(address.clone(), rfile))
            }
            's' => {
                let (pattern, replacement) = parse_replace_command(chars, &mut i, "s".to_string())?;
                let flags = parse_replace_flags(chars, &mut i)?;
                let icase = flags.contains(&ReplaceFlag::CaseInsensitive);
                let re = compile_regex_icase(pattern.clone(), icase)?;
                commands.push(Command::Replace(
                    address.clone(),
                    Regex(Rc::new(re)),
                    pattern.clone(),
                    replacement.clone(),
                    flags,
                ));
            }
            't' => {
                i += 1;
                let label = parse_word_attribute(chars, &mut i)?;
                commands.push(Command::Test(address.clone(), label));
            }
            'w' => {
                match parse_path_attribute(chars, &mut i) {
                    Ok(wfile) => {
                        commands.push(Command::AppendPatternToFile(address.clone(), wfile));
                    }
                    Err(SedError::ScriptParse(msg, _)) if msg.starts_with("missing") => {}
                    Err(err) => return Err(err),
                };
            }
            'x' => commands.push(Command::ExchangeSpaces(address.clone())),
            'y' => {
                let (string1, string2) = parse_replace_command(chars, &mut i, "y".to_string())?;
                // Resolve escapes (\n, \\, \<delim>, ...) into real chars at
                // parse time so transliteration sees actual characters.
                let from = unescape_transliteration(&string1);
                let to = unescape_transliteration(&string2);
                if from.len() != to.len() {
                    let position = get_current_line_and_col(chars, i);
                    return Err(SedError::ScriptParse(
                        "number of characters in the two arrays does not match".to_string(),
                        position,
                    ));
                }
                // Leave `i` on the closing delimiter for the loop's step.
                i -= 1;
                commands.push(Command::ReplaceCharSet(
                    address.clone(),
                    from.into_iter().collect(),
                    to.into_iter().collect(),
                ));
            }
            ':' => {
                i += 1;
                let Some(label) = parse_word_attribute(chars, &mut i)? else {
                    let position = get_current_line_and_col(chars, i);
                    return Err(SedError::ScriptParse(
                        "label doesn't have name".to_string(),
                        position,
                    ));
                };
                commands.push(Command::BearBranchLabel(label))
            }
            '=' => commands.push(Command::PrintStandard(address.clone())),
            _ => {
                let position = get_current_line_and_col(chars, i);
                return Err(SedError::ScriptParse(
                    format!("unknown character '{}'", ch),
                    position,
                ));
            }
        }

        if last_commands_count < commands.len() {
            last_commands_count = commands.len();
            command_added = true;
        }
        i += 1;
    }

    if let Some(open) = open {
        return Err(SedError::ScriptParse(
            "'{' not have pair for closing block".to_string(),
            get_current_line_and_col(chars, open),
        ));
    }
    *pos = i;
    Ok(commands)
}

/// Contains [`Command`] sequence of all [`Sed`] session
/// that applied all to every line of input files
#[derive(Debug)]
struct Script(Vec<Command>);

impl Script {
    /// Try parse raw script string to sequence of [`Command`]s
    /// formated as [`Script`]
    fn parse(raw_script: impl AsRef<str>) -> Result<Script, SedError> {
        let chars = raw_script.as_ref().chars().collect::<Vec<_>>();
        let mut commands = vec![];
        if let Some(slice) = raw_script.as_ref().get(0..2) {
            if slice.get(0..1) == Some("#") && slice.get(1..2) == Some("n") {
                commands.push(Command::IgnoreComment);
            }
        }
        let mut i = 0;
        commands.extend(parse_commands(&chars, &mut i, None)?);
        for cmd in commands.iter_mut() {
            cmd.check_address()?;
        }
        Ok(Script(flatten_commands(commands)))
    }

    /// Raise error if "b" or "t" commands without ":<label>" pair
    fn check_labels(&self) -> Result<(), SedError> {
        let labels = self
            .0
            .iter()
            .cloned()
            .filter_map(|cmd| match cmd {
                Command::BearBranchLabel(label) => Some(label),
                _ => None,
            })
            .collect::<Vec<_>>();

        let cmd_labels_set = self
            .0
            .iter()
            .cloned()
            .filter_map(|cmd| match cmd {
                Command::BranchToLabel(_, label) | Command::Test(_, label) => label,
                _ => None,
            })
            .collect::<HashSet<_>>();

        let labels_set = labels.iter().cloned().collect::<HashSet<_>>();
        if let Some(label) = cmd_labels_set.difference(&labels_set).next() {
            return Err(SedError::ScriptParse(
                format!("can't find label for jump to `{}'", label),
                None,
            ));
        }
        // NOTE: duplicate labels are NOT an error. GNU `sed` accepts repeated
        // `:label` definitions (a branch resolves to the first match), so we
        // match that behavior rather than rejecting duplicates.
        Ok(())
    }
}

/// Replace every [`Command::Block`] with a [`Command::BlockStart`] followed by
/// its flattened commands. The block's address is tested once, when the
/// block is entered, as POSIX requires, so commands inside it that change the
/// pattern space do not deselect the rest of the block.
fn flatten_commands(commands: Vec<Command>) -> Vec<Command> {
    let mut flat = vec![];
    for cmd in commands {
        if let Command::Block(address, inner) = cmd {
            let inner = flatten_commands(inner);
            flat.push(Command::BlockStart(address, inner.len()));
            flat.extend(inner);
        } else {
            flat.push(cmd);
        }
    }
    flat
}

/// Expand an `s` replacement for one match: `&` is the whole match, `\1`..`\9`
/// the subexpressions, `\n` a <newline>, `\&` and `\\` literal; GNU's
/// `\t`, `\r`, `\a`, `\f` and `\v` controls are kept. Any other `\X` is `X`.
fn expand_replacement(replacement: &str, haystack: &str, caps: &[Range<usize>]) -> String {
    let group = |n: usize| caps.get(n).map_or("", |r| &haystack[r.clone()]);
    let mut out = String::new();
    let mut chars = replacement.chars();
    while let Some(c) = chars.next() {
        match c {
            '&' => out.push_str(group(0)),
            '\\' => match chars.next() {
                Some(d @ '1'..='9') => out.push_str(group(d as usize - '0' as usize)),
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('r') => out.push('\r'),
                Some('a') => out.push('\x07'),
                Some('f') => out.push('\x0C'),
                Some('v') => out.push('\x0B'),
                Some(other) => out.push(other),
                None => {}
            },
            _ => out.push(c),
        }
    }
    out
}

/// Execute [`Command::Replace`] for current [`Sed`] line
fn execute_replace(pattern_space: &mut String, command: Command) -> Result<bool, SedError> {
    let Command::Replace(_, re, _, replacement, flags) = command else {
        unreachable!();
    };
    let matches = find_matches(&re.0, pattern_space);
    let selected: Vec<&Vec<Range<usize>>> = if flags.contains(&ReplaceFlag::ReplaceAll) {
        matches.iter().collect()
    } else {
        let n = flags
            .iter()
            .find_map(|f| match f {
                ReplaceFlag::ReplaceNth(n) => Some(*n),
                _ => None,
            })
            .unwrap_or(1);
        matches.get(n.saturating_sub(1)).into_iter().collect()
    };
    let replace = !selected.is_empty();
    if replace {
        let mut result = String::new();
        let mut copied = 0;
        for caps in selected {
            result.push_str(&pattern_space[copied..caps[0].start]);
            result.push_str(&expand_replacement(&replacement, pattern_space, caps));
            copied = caps[0].end;
        }
        result.push_str(&pattern_space[copied..]);
        *pattern_space = result;
    }

    if flags.contains(&ReplaceFlag::PrintPatternIfReplace) && replace {
        println!("{}", *pattern_space);
    }

    if let Some(wfile) = flags.iter().find_map(|flag| {
        let ReplaceFlag::AppendToIfReplace(wfile) = flag else {
            return None;
        };
        Some(wfile)
    }) {
        if replace && wfile.components().next().is_some() {
            // Relative wfile paths are resolved against the current working
            // directory (the wfile was pre-created/truncated at startup).
            if let Ok(mut file) = std::fs::OpenOptions::new()
                .append(true)
                .create(true)
                .open(wfile)
                .map_err(SedError::Io)
            {
                let _ = file.write(pattern_space.as_bytes());
            }
        }
    }

    Ok(replace)
}

/// Set of states that are returned from [`Sed::execute`]
/// for controling [`Sed`] [`Script`] execution loop for
/// current input file
#[derive(Debug)]
enum ControlFlowInstruction {
    /// End [`Sed`] [`Command`] execution loop for current file
    Break,
    /// Skip end of [`Script`], go to next line of current input
    /// file and start again [`Script`], [`Sed`] cycle
    Continue,
    /// If string exist then go to label in [`Script`], else go
    /// to end of [`Script`] (end current cycle)
    Goto(Option<String>),
    /// Not read next line in current input file and start new cycle
    NotReadNext,
    /// Skip the next commands: those of a block that was not selected
    Skip(usize),
    /// Read next line in current input file and continue current cycle
    ReadNext,
    /// Append next line to current pattern space and continue current cycle  
    AppendNext,
    // /// Skip print after cycle
    // SkipPrint
}

/// An item queued by `a` (append text) or `r` (read file), to be written to
/// output just before the next line of input is read (POSIX deferred output).
#[derive(Debug, Clone)]
enum AppendItem {
    /// Text from an `a\` command.
    Text(String),
    /// Contents of a file from an `r` command (read lazily at flush time).
    File(PathBuf),
}

/// Main program structure. Process input
/// files by [`Script`] [`Command`]s
struct Sed {
    /// Suppress default behavior of editing [`Command`]s
    /// to print result
    quiet: bool,
    /// [`Script`] that applied for every line of every input file
    script: Script,
    /// List of input files that need process with [`Script`]
    input_sources: Vec<String>,
    /// Buffer with current line of processed input file,
    /// but it can be changed with [`Command`]s in cycle limits.
    /// Сleared every cycle
    pattern_space: String,
    /// Buffer that can be filled with certain [`Command`]s during
    /// [`Script`] processing. It's not cleared after the cycle is
    /// complete
    hold_space: String,
    /// Current processed input file
    current_file: Option<Box<dyn BufRead>>,
    /// Current line of current processed input file
    current_line: usize,
    /// [`true`] if since last t at least one replacement [`Command`]
    /// was performed in cycle limits
    has_replacements_since_t: bool,
    /// Last regex pattern in applied [`Command`]
    last_regex: Option<Regex>,
    /// Next line for processing
    next_line: String,
    /// Indicate that current processed line is last
    is_last_line: bool,
    /// Contains chars '\n\r' line end of current line for processed file
    current_end: Option<String>,
    /// Output deferred by `a`/`r`, flushed just before the next input line.
    append_queue: Vec<AppendItem>,
}

impl Sed {
    /// Executes one command for `line` string argument
    /// and updates [`Sed`] state
    fn execute(
        &mut self,
        command_position: usize,
    ) -> Result<Option<ControlFlowInstruction>, SedError> {
        let Some(command) = self.script.0.get(command_position) else {
            return Ok(Some(ControlFlowInstruction::Continue));
        };
        let mut instruction = None;
        let current_command = command.clone();
        match current_command {
            Command::PrintTextAfter(_, text) => {
                // a: queue text for deferred output (written just before the
                // next input line is read). It must NOT enter the pattern space,
                // so later commands in this cycle cannot see or alter it.
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                self.append_queue.push(AppendItem::Text(text));
            }
            Command::BranchToLabel(_, label) => {
                // b
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                instruction = Some(ControlFlowInstruction::Goto(label.clone()));
            }
            Command::DeletePatternAndPrintText(address, text) => {
                // c
                let _ = self.execute_c(command_position, address, text);
            }
            Command::DeletePattern(_, to_first_line) => {
                // dD
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                instruction = self.execute_d(to_first_line);
            }
            Command::ReplacePatternWithHold(_) => {
                // g
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                if let Some(hold_space) = self.hold_space.strip_suffix('\n') {
                    self.current_end = Some("\n".to_string());
                    self.pattern_space = hold_space.to_string();
                } else {
                    self.pattern_space = self.hold_space.clone();
                }
            }
            Command::AppendHoldToPattern(_) => {
                // G
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                self.execute_upper_g();
            }
            Command::ReplaceHoldWithPattern(_) => {
                // h
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                self.hold_space = self.pattern_space.clone()
                    + self.current_end.clone().unwrap_or_default().as_str();
            }
            Command::AppendPatternToHold(_) => {
                // H
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                if self.hold_space.strip_suffix("\n").is_none() {
                    self.hold_space = self.hold_space.clone() + "\n";
                }
                self.hold_space += &(self.pattern_space.clone()
                    + self.current_end.clone().unwrap_or_default().as_str());
            }
            Command::PrintTextBefore(_, text) => {
                // i
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                println!("{text}");
            }
            Command::PrintPatternBinary(_) => {
                // I (extension)
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                print_multiline_binary(&self.pattern_space, None);
            }
            Command::PrintPatternList(_, width) => {
                // l
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                print_multiline_binary(&self.pattern_space, width);
            }
            Command::PrintPatternAndReplaceWithNext(_) => {
                // n
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                instruction = Some(ControlFlowInstruction::ReadNext);
            }
            Command::AppendNextToPattern(_address) => {
                // N
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                instruction = Some(ControlFlowInstruction::AppendNext);
            }
            Command::PrintPattern(_, to_first_line) => {
                // pP
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                self.execute_p(to_first_line);
            }
            Command::Quit(_) => {
                // q
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                instruction = Some(ControlFlowInstruction::Break);
            }
            Command::PrintFile(_, rfile) => {
                // r
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                self.execute_r(rfile);
            }
            Command::Replace(address, ref regex, pattern, replacement, flags) => {
                // s
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                self.execute_s(address, regex, pattern, replacement, flags)?;
            }
            Command::Test(_, label) => {
                // t
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                if self.has_replacements_since_t {
                    instruction = Some(ControlFlowInstruction::Goto(label.clone()));
                }
                self.has_replacements_since_t = false;
            }
            Command::AppendPatternToFile(_, wfile) => {
                // w
                if !self.need_execute(command_position)? || wfile.components().next().is_none() {
                    return Ok(None);
                }
                self.execute_w(wfile)?;
            }
            Command::ExchangeSpaces(_) => {
                // x
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                self.execute_x();
            }
            Command::ReplaceCharSet(_, string1, string2) => {
                // y
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                self.execute_y(string1, string2);
            }
            Command::PrintStandard(_) => {
                // =
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                // POSIX/GNU: `=` writes the line number unconditionally,
                // even under -n.
                println!("{}", self.current_line + 1);
            }
            Command::IgnoreComment if !self.quiet => {
                // #
                self.quiet = true;
            }
            Command::_Unknown => {}
            Command::BlockStart(_, len) => {
                // {
                if !self.need_execute(command_position)? {
                    instruction = Some(ControlFlowInstruction::Skip(len));
                }
            }
            Command::Block(..) => unreachable!(),
            _ => {}
        }
        Ok(instruction)
    }

    fn execute_c(
        &mut self,
        command_position: usize,
        address: Option<Address>,
        text: String,
    ) -> Result<(), SedError> {
        if address.is_none() {
            self.pattern_space.clear();
            self.current_end = None;
            println!("{text}");
        } else {
            let mut need_execute = self.need_execute(command_position)?;
            if need_execute {
                println!("{text}");
            }
            loop {
                need_execute = self.need_execute(command_position)?;
                if need_execute {
                    let mut line = self.next_line.clone();
                    self.next_line = self.read_line()?;
                    self.is_last_line = self.next_line.is_empty();
                    self.current_line += 1;
                    if line.is_empty() {
                        break;
                    }
                    if let Some(l) = line.strip_suffix("\n") {
                        line = l.to_string();
                        self.current_end = Some("\n".to_string());
                    } else {
                        self.current_end = None;
                    }
                    self.pattern_space = line;
                } else {
                    break;
                }
            }
        }
        Ok(())
    }

    fn execute_d(&mut self, to_first_line: bool) -> Option<ControlFlowInstruction> {
        // D
        if let Some(newline) = self.pattern_space.find('\n').filter(|_| to_first_line) {
            // Delete through the first <newline>.
            self.pattern_space.drain(..=newline);
            Some(ControlFlowInstruction::NotReadNext)
        } else {
            // d
            self.pattern_space.clear();
            Some(ControlFlowInstruction::Continue)
        }
    }

    fn execute_upper_g(&mut self) {
        self.pattern_space = self.pattern_space.clone()
            + &self.current_end.clone().unwrap_or_default()
            + &self.hold_space;
        if self.hold_space.is_empty() {
            self.pattern_space += "\n";
        }
        if self.pattern_space.strip_suffix('\n').is_some() {
            self.pattern_space.pop();
            self.current_end = Some("\n".to_string());
        } else {
            self.current_end = None;
        }
    }

    fn execute_p(&mut self, to_first_line: bool) {
        if !self.pattern_space.is_empty() {
            if to_first_line {
                let end = self
                    .pattern_space
                    .chars()
                    .enumerate()
                    .find(|(_, ch)| *ch == '\n')
                    .map(|pair| pair.0)
                    .unwrap_or(self.pattern_space.len());
                print!("{}", &self.pattern_space[0..end]);
            } else {
                print!("{}", self.pattern_space);
            }
        }
        if let Some(end) = &self.current_end {
            print!("{end}");
        }
    }

    fn execute_r(&mut self, rfile: PathBuf) {
        // POSIX/GNU: like `a`, `r` is deferred to just before the next input
        // line is read. The file is read lazily at flush time; if it is
        // unreadable, GNU silently outputs nothing for it.
        self.append_queue.push(AppendItem::File(rfile));
    }

    /// Flush the deferred `a`/`r` output queue. Called just before the next
    /// input line is read (and at the end of each cycle). Emits a separating
    /// newline first when the previous output line lacked a terminator, exactly
    /// like GNU's `output_missing_newline`.
    fn flush_appends(&mut self) {
        if self.append_queue.is_empty() {
            return;
        }
        if self.current_end.is_none() {
            println!();
        }
        for item in std::mem::take(&mut self.append_queue) {
            match item {
                AppendItem::Text(text) => println!("{text}"),
                AppendItem::File(path) => {
                    if let Ok(contents) = std::fs::read_to_string(&path) {
                        print!("{contents}");
                    }
                }
            }
        }
    }

    fn execute_s(
        &mut self,
        address: Option<Address>,
        regex: &Regex,
        pattern: String,
        replacement: String,
        flags: Vec<ReplaceFlag>,
    ) -> Result<(), SedError> {
        let mut regex = regex.clone();
        if pattern.is_empty() {
            if let Some(last_regex) = &self.last_regex {
                regex = last_regex.clone();
            } else {
                return Err(SedError::NoRegex);
            }
        }
        // POSIX `t`: any substitution since the last input line or `t`.
        self.has_replacements_since_t |= execute_replace(
            &mut self.pattern_space,
            Command::Replace(address, regex.clone(), pattern, replacement, flags),
        )?;
        self.last_regex = Some(regex);
        Ok(())
    }

    /// Pre-create (truncate) every wfile named in the script before processing.
    /// POSIX requires each wfile to be created before processing begins, so a
    /// `w`/`s///w` whose address never matches still yields an empty file.
    /// Relative paths are resolved against the current working directory.
    fn create_wfiles(&mut self) {
        let mut paths: Vec<PathBuf> = Vec::new();
        for cmd in &self.script.0 {
            match cmd {
                Command::AppendPatternToFile(_, path) if path.components().next().is_some() => {
                    paths.push(path.clone())
                }
                Command::Replace(_, _, _, _, flags) => {
                    for flag in flags {
                        if let ReplaceFlag::AppendToIfReplace(path) = flag {
                            if path.components().next().is_some() {
                                paths.push(path.clone());
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        for path in paths {
            // Best-effort: errors (e.g. missing parent dir) are surfaced later
            // at write time, preserving existing runtime diagnostics.
            let _ = File::create(&path);
        }
    }

    fn execute_w(&mut self, wfile: PathBuf) -> Result<(), SedError> {
        let _ = match std::fs::OpenOptions::new()
            .append(true)
            .create(true)
            .open(wfile.clone())
        {
            Ok(mut file) => file.write(self.pattern_space.as_bytes()),
            Err(err) => {
                return Err(SedError::Io(Error::new(
                    ErrorKind::NotFound,
                    format!(
                        "can't find '{}': {}",
                        wfile.display(),
                        plib::diag::io_error_text(&err).to_lowercase()
                    ),
                )));
            }
        };
        Ok(())
    }

    fn execute_x(&mut self) {
        if let Some(hold_space) = self.hold_space.strip_suffix('\n') {
            let tmp = hold_space.to_string();
            self.hold_space =
                self.pattern_space.clone() + self.current_end.clone().unwrap_or_default().as_str();
            self.current_end = Some("\n".to_string());
            self.pattern_space = tmp;
        } else {
            let tmp = self.hold_space.clone();
            self.hold_space =
                self.pattern_space.clone() + self.current_end.clone().unwrap_or_default().as_str();
            self.pattern_space = tmp.clone();
            self.current_end = if !tmp.is_empty() {
                None
            } else {
                Some("\n".to_string())
            };
        }
    }

    fn execute_y(&mut self, string1: String, string2: String) {
        let mut replace_positions = vec![];
        for (a, b) in string1.chars().zip(string2.chars()) {
            let positions = self
                .pattern_space
                .chars()
                .enumerate()
                .filter_map(|(i, ch)| if ch == a { Some(i) } else { None })
                .collect::<Vec<_>>();
            replace_positions.push((b.to_string(), positions));
        }
        for (ch, positions) in replace_positions {
            for position in positions {
                self.pattern_space
                    .replace_range(position..(position + 1), &ch);
            }
        }
        // Escapes were already resolved when the y operands were parsed; the
        // pattern space must NOT be post-processed (that corrupted literal \n).
        self.has_replacements_since_t = true;
    }

    /// Read next line from current file
    fn read_line(&mut self) -> Result<String, SedError> {
        let Some(current_file) = self.current_file.as_mut() else {
            return Err(SedError::Io(std::io::Error::new(
                ErrorKind::NotFound,
                "current file is none",
            )));
        };
        let mut line = String::new();
        if let Err(err) = current_file.read_line(&mut line) {
            return Err(SedError::Io(err));
        }
        Ok(line)
    }

    fn need_execute(&mut self, command_position: usize) -> Result<bool, SedError> {
        let Some(command) = self.script.0.get_mut(command_position) else {
            return Ok(false);
        };

        let need_execute = command.need_execute(
            &LineState {
                number: self.current_line + 1,
                last: self.is_last_line,
                text: &self.pattern_space,
            },
            &mut self.last_regex,
        )?;

        Ok(need_execute)
    }

    /// Executes all commands of [`Sed`]'s [`Script`] for `line` string argument
    fn process_line(&mut self) -> Result<Option<ControlFlowInstruction>, SedError> {
        let mut global_instruction = None;
        let mut i = 0;
        let script_len = self.script.0.len();
        while i < script_len {
            if let Some(instruction) = self.execute(i)? {
                global_instruction = None;
                match instruction {
                    ControlFlowInstruction::Goto(label) => {
                        if let Some(label) = label {
                            let label_position = self.script.0.iter().position(|cmd| {
                                if let Command::BearBranchLabel(l) = cmd {
                                    label == *l
                                } else {
                                    false
                                }
                            });
                            if let Some(label_position) = label_position {
                                i = label_position;
                            } else {
                                return Err(SedError::NoLabel(label));
                            }
                        } else {
                            break;
                        }
                    }
                    ControlFlowInstruction::Break => {
                        global_instruction = Some(ControlFlowInstruction::Break);
                        break;
                    }
                    ControlFlowInstruction::Continue => {
                        if self.pattern_space.is_empty() {
                            // `d`: no auto-print, but deferred a/r output is
                            // still flushed at end of cycle.
                            self.flush_appends();
                            return Ok(None);
                        } else {
                            break;
                        }
                    }
                    ControlFlowInstruction::NotReadNext => {
                        // POSIX `D` restarts the cycle without reading input and
                        // WITHOUT touching the hold space.
                        i = 0;
                        continue;
                    }
                    ControlFlowInstruction::Skip(len) => i += len,
                    ControlFlowInstruction::AppendNext => {
                        // Reading a new input line flushes deferred a/r output.
                        self.flush_appends();
                        let mut line = self.next_line.clone();
                        self.next_line = self.read_line()?;
                        self.is_last_line = self.next_line.is_empty();
                        self.current_line += 1;
                        if line.is_empty() {
                            return Ok(None);
                        }
                        if let Some(l) = line.strip_suffix("\n") {
                            self.current_end = Some("\n".to_string());
                            line = l.to_string();
                        } else {
                            self.current_end = None;
                        }
                        self.pattern_space += "\n";
                        self.pattern_space += &line;
                    }
                    ControlFlowInstruction::ReadNext => {
                        let mut line = self.next_line.clone();
                        self.next_line = self.read_line()?;
                        self.is_last_line = self.next_line.is_empty();
                        self.current_line += 1;
                        if line.is_empty() {
                            break;
                        }
                        if !self.quiet {
                            print!("{}", self.pattern_space);
                            print!("{}", self.current_end.clone().unwrap_or_default());
                        }
                        // Reading a new input line flushes deferred a/r output.
                        self.flush_appends();
                        if let Some(l) = line.strip_suffix("\n") {
                            line = l.to_string();
                        }
                        self.current_end = Some("\n".to_string());
                        self.pattern_space = line;
                    }
                }
            }

            i += 1;
        }

        if !self.quiet {
            if !self.pattern_space.is_empty() {
                print!("{}", self.pattern_space.trim_end_matches('\r'));
            }
            if let Some(end) = &self.current_end {
                print!("{end}");
            }
        }

        // Flush deferred a/r output at end of cycle, before the next line read.
        self.flush_appends();

        Ok(global_instruction)
    }

    /// Executes all commands of [`Sed`]'s [`Script`]
    /// for all content of `reader` file argument
    fn process_input(&mut self) -> Result<(), SedError> {
        self.pattern_space.clear();
        self.hold_space.clear();
        self.current_line = 0;
        self.is_last_line = false;
        let mut line;
        self.current_end = None;
        self.next_line = self.read_line()?;
        loop {
            line = self.next_line.clone();
            self.next_line = self.read_line()?;
            self.is_last_line = self.next_line.is_empty();
            if line.is_empty() {
                break;
            }
            if let Some(l) = line.strip_suffix("\n") {
                self.current_end = Some("\n".to_string());
                line = l.to_string();
            } else {
                self.current_end = None;
            }
            self.has_replacements_since_t = false;
            self.pattern_space = line;
            if let Some(ControlFlowInstruction::Break) = self.process_line()? {
                break;
            }
            self.current_line += 1;
        }

        Ok(())
    }

    /// Main [`Sed`] function. Executes all commands of
    /// own [`Script`] for all content of all input files
    fn sed(&mut self) -> Result<(), SedError> {
        // ERE flag is set in `try_to_sed` before parsing (regexes are compiled
        // at parse time). Pre-create/truncate every wfile named in the script,
        // as required by POSIX (each wfile is created before processing begins).
        self.create_wfiles();
        for mut input in std::mem::take(&mut self.input_sources) {
            self.current_file = Some(if input == "-" {
                Box::new(BufReader::new(std::io::stdin()))
            } else {
                match File::open(&input) {
                    Ok(file) => Box::new(BufReader::new(file)),
                    Err(err) => {
                        if input == "-" {
                            input = "stdin".to_string();
                        }
                        eprintln!("sed: read {input}: {err}");
                        continue;
                    }
                }
            });
            match self.process_input() {
                Ok(_) => {}
                Err(err) => {
                    if input == "-" {
                        input = "stdin".to_string();
                    }
                    return Err(SedError::Runtime(input, format!("{}", err)));
                }
            };
        }

        Ok(())
    }
}

/// Exit code:
///     0 - Successful completion.
///     >0 - An error occurred.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("sed");

    let args = Args::parse();

    let exit_code = Args::try_to_sed(args)
        .and_then(|mut sed| sed.sed())
        .map(|_| 0)
        .unwrap_or_else(|err| {
            eprintln!("sed: {err}");
            1
        });

    std::process::exit(exit_code);
}
