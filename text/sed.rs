//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::{HashMap, HashSet};
use std::ffi::CString;
use std::fmt::{self, Debug};
use std::fs::File;
use std::io::{BufRead, BufReader, Error, ErrorKind, Write};
use std::mem::MaybeUninit;
use std::ops::Range;
use std::path::PathBuf;
use std::sync::Mutex;

use clap::{command, Parser};
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use libc::{
    ioctl, regcomp, regex_t, regexec, regmatch_t, winsize, REG_EXTENDED, STDERR_FILENO,
    STDIN_FILENO, STDOUT_FILENO, TIOCGWINSZ,
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
                // then consider first [file...] as single script.
                for raw_script_line in self.file.remove(0).split('\n') {
                    raw_script.push_str(raw_script_line);
                }
            }
        }

        // If no [file...] were supplied or single file is considered to to be script, then
        // sed must read input from STDIN.
        if self.file.is_empty() {
            self.file.push("-".to_string());
        }

        let script = Script::parse(raw_script)?;
        script.check_labels()?;

        Ok(Sed {
            ere: self.ere,
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
    Pattern(regex_t, String),
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
    /// Defines what range limits is passed
    /// in current processing file for current [`Command`]
    passed: Option<(bool, bool)>,
    /// Defines what range limits is currently raised
    /// in current processing file for current [`Command`]
    on_limits: Option<(bool, bool)>,
    /// Inverse fulfillment of [`AddressRange`] conditions
    is_negative: bool,
}

impl AddressRange {
    fn new(limits: Vec<AddressToken>, is_negative: bool) -> Result<Option<Self>, SedError> {
        let state = match limits.len() {
            i if i > 2 => {
                return Err(SedError::ScriptParse(
                    "address isn't empty, position or range".to_string(),
                    None,
                ))
            }
            2 => Some((false, false)),
            0 => return Ok(None),
            _ => None,
        };

        Ok(Some(Self {
            limits,
            passed: state,
            on_limits: state,
            is_negative,
        }))
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
}

/// Newtype for implementing [`Debug`] trait for regex_t
#[derive(Clone)]
struct Regex(regex_t);

impl Debug for Regex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Regex").finish()
    }
}

/// Atomic parts of [`Script`], that can process input
/// files line by line
#[derive(Debug, Clone)]
enum Command {
    /// Execute a list of sed editing commands only
    /// when the pattern space is selected
    Block(Option<Address>, Vec<Command>), // {
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
    /// output in a visually unambiguous form
    PrintPatternBinary(Option<Address>), // I
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
            Command::PrintPatternAndReplaceWithNext(address) => (address, 2),
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
        line_number: usize,
        line: &str,
        last_line: bool,
    ) -> Result<bool, SedError> {
        let Some((address, _)) = self.get_mut_address() else {
            return Ok(true);
        };

        if address.is_none() {
            return Ok(true);
        }

        let mut need_execute = true;
        for range in address.as_mut().unwrap().0.iter_mut() {
            if let Some(AddressToken::Pattern(..)) = range.limits.first() {
                if let Some(AddressToken::Pattern(..)) = range.limits.get(1) {
                    if range.passed == Some((true, true)) {
                        range.passed = Some((false, false));
                    }
                }
            }

            let mut reached_now = vec![];
            for (i, token) in range.limits.iter().enumerate() {
                reached_now.push(match token {
                    AddressToken::Number(position) => *position == line_number + 1,
                    AddressToken::Pattern(re, pattern) => {
                        !(match_pattern(*re, pattern.clone(), line, line_number + 1)?.is_empty())
                    }
                    AddressToken::Last => match i {
                        0 => last_line,
                        1 => false,
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                });
            }

            match range.limits.len() {
                1 => need_execute &= reached_now[0],
                2 => {
                    let (mut old_a, mut old_b) = range.passed.unwrap();
                    if !old_a && old_b {
                        range.passed = Some((false, false));
                        (old_a, old_b) = range.passed.unwrap();
                    }
                    range.passed = Some((reached_now[0] || old_a, reached_now[1] || old_b));
                    let (a, b) = range.passed.unwrap();
                    range.on_limits = Some((reached_now[0], reached_now[1]));
                    let mut result = (old_a && !old_b && reached_now[1]) || (a && !b);
                    if range.is_negative {
                        result = !result;
                    }
                    need_execute &= result;
                }
                _ => unreachable!(),
            }
        }

        Ok(need_execute)
    }
}

/// Function [`regexec`] can return from 1 to 9 range that can be nested.
/// [`delete_nested_ranges`] function deletes ranges that contain another ranges.
fn delete_nested_ranges(mut ranges: Vec<(usize, Range<usize>)>) -> Vec<(usize, Range<usize>)> {
    let mut result: Vec<(usize, Range<usize>)> = Vec::new();

    ranges.sort_by(|(_, a), (_, b)| (a.end - a.start).cmp(&(b.end - b.start)));
    for (i, Range { start, end }) in ranges.into_iter() {
        if result.iter().any(|(_, r)| r.start >= start && end >= r.end) {
            continue;
        }
        result.push((i, start..end));
    }

    result
}

/// Function [`regexec`] can return from 1 to 9 range. Some
/// of them cam be invalid for usage. So this function filter
/// invalid ranges.
fn filter_groups(groups: &mut Vec<(usize, Range<usize>)>, pattern: &str, haystack: &str) {
    groups.retain(|(_, m)| {
        if m.start != m.end {
            true
        } else {
            m.start == 0 || m.start == haystack.len()
        }
    });
    if pattern != "^" && pattern != "^$" {
        groups.retain(|(_, r)| *r != (0..0));
    }
    let end_range = haystack.len()..haystack.len();
    if pattern != "$" && pattern != "^$" {
        groups.retain(|(_, r)| *r != end_range);
    }
    if (pattern == "^$" && !haystack.is_empty()) || !["^$", "^", "$"].contains(&pattern) {
        groups.retain(|(_, r)| *r != (0..0) && *r != end_range);
    }
}

/// Filter groups of [`Range<usize>`] for replace in
/// pattern space when patterns like "^\{1,13\}$" appears
fn filter_groups_when_line_size_check_in_pattern(
    match_subranges: &mut [Vec<(usize, Range<usize>)>],
    pattern: &str,
) {
    if ["^", "$", r#"\{"#, r#"\{"#]
        .iter()
        .all(|pat| pattern.contains(pat))
    {
        let mut lengths = vec![];
        let mut i = 0;
        loop {
            let Some(slice) = pattern.get(i..) else {
                break;
            };
            let Some(mut a) = slice.find(r#"\{"#) else {
                break;
            };
            let Some(mut b) = slice.find(r#"\}"#) else {
                break;
            };
            a += i;
            b += i;
            let max_len = if let Some(comma_pos) = pattern.get(a..).unwrap().find(",") {
                if let Some(s) = pattern.get((comma_pos + a + 1)..b) {
                    s.parse::<usize>().ok()
                } else {
                    Some(pattern.len())
                }
            } else {
                let s = pattern.get((a + 2)..b).unwrap();
                s.parse::<usize>().ok()
            };
            if let Some(max_len) = max_len {
                lengths.push(max_len);
            }

            i = b + 2;
        }

        for length in &lengths {
            match_subranges.iter_mut().for_each(|groups| {
                groups.retain(|(_, r)| r.end <= *length);
            });
        }
    }
}

fn delete_groups_duplicates(
    match_subranges: Vec<Vec<(usize, Range<usize>)>>,
) -> Vec<HashMap<usize, Range<usize>>> {
    let match_subranges = match_subranges.into_iter().collect::<HashSet<_>>();
    match_subranges
        .into_iter()
        .map(delete_nested_ranges)
        .map(|m| m.into_iter().enumerate().map(|(i, (_, r))| (i + 1, r)))
        .map(|m| m.into_iter().collect::<HashMap<_, _>>())
        .filter(|m| !m.is_empty())
        .collect::<Vec<_>>()
}

fn sort_groups(match_subranges: &mut [HashMap<usize, Range<usize>>]) {
    match_subranges.sort_by(|a, b| {
        a.iter()
            .next()
            .unwrap()
            .1
            .start
            .cmp(&b.iter().next().unwrap().1.start)
    });
}

/// Get [`Vec<Range<usize>>`] from finding match in haystack with RE
///
/// Arguments:
/// [`haystack`] - &[`str`] for searching pattern matches
/// [`re`] - pattern for search in haystack
/// [`line_number`] - current line number in input file, used in error message
fn match_pattern(
    re: regex_t,
    pattern: String,
    haystack: &str,
    line_number: usize,
) -> Result<Vec<HashMap<usize, std::ops::Range<usize>>>, SedError> {
    let match_t: regmatch_t = unsafe { MaybeUninit::zeroed().assume_init() };
    let mut match_subranges = vec![];
    let mut i = 0;
    let mut last_offset = 0;
    let c_input = CString::new(haystack).map_err(|err| {
        SedError::ScriptParse(
            format!(
                "line {} contains nul byte in {} position",
                line_number,
                err.nul_position()
            ),
            None,
        )
    })?;
    let mut c_input = c_input.as_ptr();
    while i <= haystack.len() {
        let mut pmatch = vec![match_t; 9];
        unsafe {
            c_input = c_input.add(last_offset);
            let _ = regexec(&re as *const regex_t, c_input, 9, pmatch.as_mut_ptr(), 0);
        }

        let mut groups = pmatch
            .to_vec()
            .iter()
            .enumerate()
            .filter(|(_, m)| !((m.rm_so < 0) && (m.rm_eo < 0)))
            .map(|(j, m)| (j + 1, ((m.rm_so as usize) + i)..((m.rm_eo as usize) + i)))
            .collect::<Vec<_>>();

        filter_groups(&mut groups, &pattern, haystack);
        last_offset = groups
            .iter()
            .map(|(_, r)| r.end - r.start)
            .max()
            .unwrap_or(0);
        if last_offset == 0 {
            last_offset = 1;
        }
        i += last_offset;
        match_subranges.push(groups);
    }
    filter_groups_when_line_size_check_in_pattern(&mut match_subranges, &pattern);
    let mut match_subranges = delete_groups_duplicates(match_subranges);
    sort_groups(&mut match_subranges);

    Ok(match_subranges)
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

/// Get pattern from chars by start..end range and validate it
fn get_pattern_token(
    chars: &[char],
    start: usize,
    end: Option<usize>,
    splitter: char,
    position: Option<(usize, usize)>,
) -> Result<String, SedError> {
    let Some(end) = end else {
        return Err(SedError::ScriptParse(
            "unterminated address regex".to_string(),
            position,
        ));
    };

    let Some(pattern) = chars.get(start..end) else {
        return Err(SedError::ScriptParse(
            "unterminated address regex".to_string(),
            position,
        ));
    };

    let mut pattern = pattern.iter().collect::<String>();
    if splitter == '/' {
        pattern = pattern.replace(r"\/", "/");
    }

    if pattern == "\\"
        || pattern.contains('\n')
        || pattern
            .chars()
            .collect::<Vec<_>>()
            .windows(2)
            .any(|chars| chars[0] == '\\' && !"().*$^".contains(chars[1]))
    {
        return Err(SedError::ScriptParse(
            "pattern can't consist more than 1 line".to_string(),
            position,
        ));
    }

    Ok(pattern)
}

/// Parse [`Address`] BRE as [`AddressToken`]
fn parse_pattern_token(
    chars: &[char],
    i: &mut usize,
    tokens: &mut Vec<AddressToken>,
) -> Result<(), SedError> {
    let position = get_current_line_and_col(chars, *i);
    *i += 1;
    let Some(ch) = chars.get(*i) else {
        return Err(SedError::ScriptParse(
            "unterminated address regex".to_string(),
            position,
        ));
    };

    if "\\\n".contains(*ch) {
        return Err(SedError::ScriptParse(
            format!("pattern spliter is '{}'", ch),
            position,
        ));
    }

    let splitter = ch;
    let mut next_position = None;
    let mut j = *i + 1;
    while j < chars.len() {
        let Some(ch) = chars.get(j) else {
            return Err(SedError::ScriptParse(
                "unterminated address regex".to_string(),
                position,
            ));
        };
        if ch == splitter {
            let Some(previous) = chars.get(j - 1) else {
                return Err(SedError::ScriptParse(
                    "unterminated address regex".to_string(),
                    position,
                ));
            };
            if *previous == '\\' && *splitter == '/' {
                j += 1;
                continue;
            }
            next_position = Some(j);
            break;
        }
        j += 1;
    }

    let pattern = get_pattern_token(chars, *i + 1, next_position, *splitter, position)?;
    let re = compile_regex(pattern.clone())?;
    tokens.push(AddressToken::Pattern(re, pattern));
    if let Some(next_position) = next_position {
        *i = next_position;
    }

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
            '\\' => parse_pattern_token(chars, i, &mut tokens)?,
            '$' => tokens.push(AddressToken::Last),
            ',' => tokens.push(AddressToken::Delimiter),
            ' ' => {}
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
    loop {
        let Some(ch) = chars.get(*i) else {
            break;
        };
        match ch {
            '!' => {
                is_negative = true;
                *i += 1;
                break;
            }
            ' ' => (),
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
    let mut text = String::new();
    loop {
        let Some(ch) = chars.get(*i) else {
            break;
        };
        match *ch {
            '\n' => {
                *i += 1;
                break;
            }
            '\\' => {
                *i += 1;
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

/// Parse label, xfile attributes of b, r, t, w [`Command`]s that formated as:
/// b [label], r  rfile
fn parse_word_attribute(chars: &[char], i: &mut usize) -> Result<Option<String>, SedError> {
    let mut label = String::new();
    loop {
        let Some(ch) = chars.get(*i) else {
            break;
        };
        match ch {
            '\n' | ';' => {
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
    loop {
        let Some(ch) = chars.get(*i) else {
            break;
        };
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

/// Parse `{ ... }` like [`Script`] part
fn parse_block(chars: &[char], i: &mut usize) -> Result<Vec<Command>, SedError> {
    let block_limits = chars
        .iter()
        .enumerate()
        .skip(*i)
        .filter(|pair| *pair.1 == '{' || *pair.1 == '}')
        .collect::<Vec<_>>();

    let mut j = 0;
    let mut k = 0;
    loop {
        let Some(ch) = block_limits.get(k) else {
            break;
        };
        match ch.1 {
            '{' => j += 1,
            '}' => j -= 1,
            _ => unreachable!(),
        }
        if j <= 0 {
            break;
        }
        k += 1;
        if k >= block_limits.len() {
            break;
        }
    }

    let commands = if j == 0 {
        let block = chars[(*i + 1)..block_limits[k].0]
            .iter()
            .collect::<String>();
        match Script::parse(block) {
            Ok(script) => script.0,
            Err(err) => {
                return if let SedError::ScriptParse(message, Some(position)) = err {
                    let (line, col) = position;
                    let (block_start_line, block_start_col) =
                        get_current_line_and_col(chars, *i + 1).unwrap_or((0, 0));
                    let position = (
                        block_start_line + line,
                        if line == 0 {
                            block_start_col + col
                        } else {
                            col
                        },
                    );
                    return Err(SedError::ScriptParse(message, Some(position)));
                } else {
                    Err(err)
                };
            }
        }
    } else {
        let position = get_current_line_and_col(chars, *i);
        return Err(SedError::ScriptParse(
            "'{' not have pair for closing block".to_string(),
            position,
        ));
    };
    *i = block_limits[k].0;
    Ok(commands)
}

/// Parse s, y [`Command`]s that formated as:
/// x/string1/string2/
fn parse_replace_command(
    chars: &[char],
    i: &mut usize,
    command: String,
) -> Result<(String, String), SedError> {
    *i += 1;
    let first_position = *i + 1;
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
    *i += 1;
    let mut splitters = chars
        .iter()
        .enumerate()
        .skip(*i)
        .filter(|pair| pair.1 == splitter)
        .map(|pair| pair.0)
        .collect::<Vec<_>>();

    if *splitter == '/' {
        splitters.retain(|j| {
            if let Some(previous_ch) = chars.get(j.checked_sub(1).unwrap_or(0)) {
                *previous_ch != '\\'
            } else {
                true
            }
        })
    }

    let position = get_current_line_and_col(chars, *i);
    let parse_error = Err(SedError::ScriptParse(
        format!("unterminated `{}' command", command),
        position,
    ));
    if splitters.len() < 2 {
        return parse_error;
    };
    let Some(pattern) = chars.get(first_position..splitters[0]) else {
        return parse_error;
    };
    let Some(replacement) = chars.get((splitters[0] + 1)..splitters[1]) else {
        return parse_error;
    };
    if pattern.contains(&'\n') || replacement.contains(&'\n') {
        return parse_error;
    }

    *i = splitters[1] + 1;
    let pattern = pattern.iter().collect::<String>();
    let replacement = replacement.iter().collect::<String>();
    let result = (pattern.replace("\\/", "/"), replacement.replace("\\/", "/"));

    Ok(result)
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

/// Compiles [`pattern`] as [`regex_t`]
fn compile_regex(pattern: String) -> Result<regex_t, SedError> {
    #[cfg(target_os = "macos")]
    let mut pattern = pattern.replace("\\\\", "\\");
    #[cfg(all(unix, not(target_os = "macos")))]
    let pattern = pattern.replace("\\\\", "\\");
    let mut cflags = 0;
    let ere = ERE.lock().unwrap();
    if *ere {
        cflags |= REG_EXTENDED;
    }

    // macOS version of [regcomp](regcomp) from `libc` provides additional check
    // for empty regex. In this case, an error
    // [REG_EMPTY](https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man3/regcomp.3.html)
    // will be returned. Therefore, an empty pattern is replaced with ".*".
    #[cfg(target_os = "macos")]
    {
        pattern = if pattern == "" {
            String::from(".*")
        } else {
            pattern
        };
    }

    let c_pattern = CString::new(pattern.clone()).map_err(|err| {
        SedError::ScriptParse(
            format!(
                "pattern '{}' contains nul byte in {} position",
                pattern,
                err.nul_position()
            ),
            None,
        )
    })?;
    let mut regex = unsafe { std::mem::zeroed::<regex_t>() };

    if unsafe { regcomp(&mut regex, c_pattern.as_ptr(), cflags) } == 0 {
        Ok(regex)
    } else {
        Err(SedError::ScriptParse(
            format!("can't compile pattern '{}'", pattern),
            None,
        ))
    }
}

fn screen_width() -> Option<usize> {
    let mut ws: *mut winsize = std::ptr::null_mut();
    if unsafe { ioctl(STDIN_FILENO, TIOCGWINSZ, &mut ws) != 0 }
        && unsafe { ioctl(STDOUT_FILENO, TIOCGWINSZ, &mut ws) != 0 }
        && unsafe { ioctl(STDERR_FILENO, TIOCGWINSZ, &mut ws) != 0 }
    {
        return None;
    }
    Some(unsafe { *ws }.ws_col as usize)
}

fn print_multiline_binary(line: &str) {
    let line = line
        .chars()
        .flat_map(|ch| {
            if b"\x07\x08\x09\x0B\x0C\x0D".contains(&(ch as u8)) {
                match ch as u8 {
                    b'\x07' => vec!['\\', 'a'],
                    b'\x08' => vec!['\\', 'b'],
                    b'\x09' => vec!['\\', 't'],
                    b'\x0B' => vec!['\\', 'v'],
                    b'\x0C' => vec!['\\', 'f'],
                    b'\x0D' => vec!['\\', 'r'],
                    _ => unreachable!(),
                }
            } else if ch == '\n' {
                vec![ch]
            } else if ch.is_ascii() {
                format!(r#"\x{:02x}"#, ch as u8).chars().collect::<Vec<_>>()
            } else {
                vec![ch]
            }
        })
        .collect::<String>();
    if let Some(width) = screen_width() {
        if width >= 1 {
            let line = line.chars().collect::<Vec<_>>();
            let mut chunks = line.chunks(width - 1).peekable();
            loop {
                let Some(chunk) = chunks.next() else {
                    break;
                };
                if chunk.strip_suffix(&['\n']).is_some() {
                    if chunks.peek().is_some() {
                        println!("\\\n");
                    } else {
                        println!("$\n");
                    }
                } else if chunks.peek().is_some() {
                    println!("\\");
                } else {
                    println!("$");
                }
            }
        }
    } else if let Some(line) = line.strip_suffix(&['\n']) {
        println!("{}$", line);
    } else {
        print!("{}$", line);
    }
}

/// Find first label in [`Script`] that has duplicates
fn find_first_repeated_label(vec: Vec<String>) -> Option<String> {
    let mut counts = HashMap::new();
    for item in &vec {
        *counts.entry(item).or_insert(0) += 1;
    }

    // Collect elements with count > 1
    counts
        .into_iter()
        .filter(|&(_, count)| count > 1)
        .map(|(item, _)| item.clone())
        .next()
}

// Skip [`Script`] fragment from '#' to '\n' chars (comment)
fn skip_comment(chars: &[char], i: &mut usize) {
    if let Some(p) = chars.iter().skip(*i).position(|ch| *ch == '\n') {
        *i = p;
    } else {
        *i = chars.len()
    }
}

/// Filter comments (line ends after '#') in raw script
fn filter_comments(raw_script: impl AsRef<str>) -> String {
    let mut raw_script_without_comments = String::new();
    for line in raw_script.as_ref().lines() {
        let mut j = 0;
        let chars = line.chars().collect::<Vec<_>>();
        let mut split_positions = vec![0];
        loop {
            let Some(remain) = chars.get(j..) else {
                break;
            };
            let Some(a) = remain.iter().position(|ch| ['s', 'y'].contains(ch)) else {
                break;
            };
            let mut b = a;
            if parse_replace_command(remain, &mut b, String::new()).is_err() {
                break;
            }
            split_positions.push(j + a);
            j += b;
            split_positions.push(j);
        }
        split_positions.push(line.len());
        let mut is_s = false;
        for pair in split_positions.windows(2) {
            let part = line.get(pair[0]..pair[1]).unwrap_or("");
            if !is_s && part.contains('#') {
                raw_script_without_comments += part.split('#').next().unwrap_or("");
                break;
            } else {
                raw_script_without_comments += part;
            }
            is_s = !is_s;
        }
        raw_script_without_comments += "\n";
    }
    raw_script_without_comments
}

/// Contains [`Command`] sequence of all [`Sed`] session
/// that applied all to every line of input files
#[derive(Debug)]
struct Script(Vec<Command>);

impl Script {
    /// Try parse raw script string to sequence of [`Command`]s
    /// formated as [`Script`]
    fn parse(raw_script: impl AsRef<str>) -> Result<Script, SedError> {
        let mut commands = vec![];
        let mut address = None;
        let mut i = 0;
        let mut last_commands_count = 0;
        let mut command_added = false;
        let chars = filter_comments(&raw_script).chars().collect::<Vec<_>>();

        if let Some(slice) = raw_script.as_ref().get(0..2) {
            if slice.get(0..1) == Some("#") && slice.get(1..2) == Some("n") {
                commands.push(Command::IgnoreComment);
            }
        }

        loop {
            let Some(ch) = chars.get(i) else {
                break;
            };
            match *ch {
                ' ' | '\n' => {}
                ';' => {
                    if address.is_some() && !command_added {
                        let position = get_current_line_and_col(&chars, i);
                        return Err(SedError::ScriptParse(
                            "address hasn't command".to_string(),
                            position,
                        ));
                    }
                    address = None;
                    command_added = false
                }
                '}' => {
                    let position = get_current_line_and_col(&chars, i);
                    return Err(SedError::ScriptParse(
                        "unneccessary '}'".to_string(),
                        position,
                    ));
                }
                _ if command_added => {
                    let position = get_current_line_and_col(&chars, i);
                    return Err(SedError::ScriptParse(
                        "commands must be delimited with ';'".to_string(),
                        position,
                    ));
                }
                ch if ch.is_ascii_digit() || "\\$".contains(ch) => {
                    parse_address(&chars, &mut i, &mut address)?;
                    continue;
                }
                '{' => {
                    commands.push(Command::Block(
                        address.clone(),
                        parse_block(&chars, &mut i)?,
                    ));
                }
                'a' => {
                    if let Some(text) = parse_text_attribute(&chars, &mut i)? {
                        commands.push(Command::PrintTextAfter(address.clone(), text));
                    } else {
                        let position = get_current_line_and_col(&chars, i);
                        return Err(SedError::ScriptParse(
                            "missing text argument".to_string(),
                            position,
                        ));
                    }
                }
                'b' => {
                    i += 1;
                    let label = parse_word_attribute(&chars, &mut i)?;
                    commands.push(Command::BranchToLabel(address.clone(), label));
                }
                'c' => {
                    if let Some(text) = parse_text_attribute(&chars, &mut i)? {
                        commands.push(Command::DeletePatternAndPrintText(address.clone(), text));
                    } else {
                        let position = get_current_line_and_col(&chars, i);
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
                    if let Some(text) = parse_text_attribute(&chars, &mut i)? {
                        commands.push(Command::PrintTextBefore(address.clone(), text));
                    } else {
                        let position = get_current_line_and_col(&chars, i);
                        return Err(SedError::ScriptParse(
                            "missing text argument".to_string(),
                            position,
                        ));
                    }
                }
                'I' => commands.push(Command::PrintPatternBinary(address.clone())),
                'n' => commands.push(Command::PrintPatternAndReplaceWithNext(address.clone())),
                'N' => commands.push(Command::AppendNextToPattern(address.clone())),
                'p' => commands.push(Command::PrintPattern(address.clone(), false)),
                'P' => commands.push(Command::PrintPattern(address.clone(), true)),
                'q' => commands.push(Command::Quit(address.clone())),
                'r' => {
                    let rfile = parse_path_attribute(&chars, &mut i)?;
                    commands.push(Command::PrintFile(address.clone(), rfile))
                }
                's' => {
                    let (pattern, replacement) =
                        parse_replace_command(&chars, &mut i, "s".to_string())?;
                    let re = compile_regex(pattern.clone())?;
                    let flags = parse_replace_flags(&chars, &mut i)?;
                    commands.push(Command::Replace(
                        address.clone(),
                        Regex(re),
                        pattern.clone(),
                        replacement.clone(),
                        flags,
                    ));
                }
                't' => {
                    i += 1;
                    let label = parse_word_attribute(&chars, &mut i)?;
                    commands.push(Command::Test(address.clone(), label));
                }
                'w' => {
                    match parse_path_attribute(&chars, &mut i) {
                        Ok(wfile) => {
                            commands.push(Command::AppendPatternToFile(address.clone(), wfile));
                        }
                        Err(SedError::ScriptParse(msg, _)) if msg.starts_with("missing") => {}
                        Err(err) => return Err(err),
                    };
                }
                'x' => commands.push(Command::ExchangeSpaces(address.clone())),
                'y' => {
                    let (string1, string2) =
                        parse_replace_command(&chars, &mut i, "y".to_string())?;
                    if string1.len() != string2.len() {
                        let position = get_current_line_and_col(&chars, i);
                        return Err(SedError::ScriptParse(
                            "number of characters in the two arrays does not match".to_string(),
                            position,
                        ));
                    }
                    commands.push(Command::ReplaceCharSet(address.clone(), string1, string2));
                }
                ':' => {
                    i += 1;
                    let Some(label) = parse_word_attribute(&chars, &mut i)? else {
                        let position = get_current_line_and_col(&chars, i);
                        return Err(SedError::ScriptParse(
                            "label doesn't have name".to_string(),
                            position,
                        ));
                    };
                    commands.push(Command::BearBranchLabel(label))
                }
                '=' => commands.push(Command::PrintStandard(address.clone())),
                '#' => skip_comment(&chars, &mut i),
                _ => {
                    let position = get_current_line_and_col(&chars, i);
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

        for cmd in commands.iter_mut() {
            cmd.check_address()?;
        }

        commands = flatten_commands(commands);

        Ok(Script(commands))
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
        } else if labels.len() > labels_set.len() {
            let label = match find_first_repeated_label(labels) {
                Some(label) => format!("label {}", label),
                None => "some label".to_string(),
            };
            return Err(SedError::ScriptParse(
                format!("{} is repeated", label),
                None,
            ));
        }
        Ok(())
    }
}

/// Replace every [`Command::Block`] with inner [`Command`]
/// set and adding [`AddressRange`] for every inner [`Command`]
fn flatten_commands(mut commands: Vec<Command>) -> Vec<Command> {
    let is_block = |cmd: &Command| matches!(cmd, Command::Block(..));

    while commands.iter().any(is_block) {
        commands = commands
            .into_iter()
            .flat_map(|cmd| {
                if let Command::Block(block_address, mut block_commands) = cmd {
                    let Some(block_address) = block_address else {
                        return block_commands;
                    };
                    block_commands.iter_mut().for_each(|cmd| {
                        if let Some((address, _)) = cmd.get_mut_address() {
                            if let Some(address) = address {
                                address.0.extend(block_address.0.clone());
                            } else {
                                *address = Some(block_address.clone());
                            }
                        }
                    });
                    block_commands
                } else {
                    vec![cmd]
                }
            })
            .collect::<Vec<_>>();
    }

    commands
}

/// Returns all positions of '&' in [Vec<char>]
fn get_ampersand_positions(chars: Vec<char>) -> Vec<usize> {
    let pairs = chars.windows(2).enumerate();
    let mut ampersand_positions = pairs
        .filter_map(|(i, chars)| {
            if chars[0] != '\\' && chars[1] == '&' {
                return Some(i + 1);
            }
            None
        })
        .rev()
        .collect::<Vec<_>>();

    if let Some(ch) = chars.first() {
        if *ch == '&' {
            ampersand_positions.push(0);
        }
    }
    ampersand_positions
}

/// Returns all groups positions (like "\1") in [Vec<char>]
fn get_group_positions(chars: Vec<char>) -> Vec<(usize, usize)> {
    let pairs = chars.windows(2).enumerate();
    let mut group_positions = pairs
        .filter_map(|(i, chars)| {
            if chars[0] == '\\' && chars[1].is_ascii_digit() {
                return Some((i, chars[1].to_digit(10).unwrap() as usize));
            }
            None
        })
        .rev()
        .collect::<Vec<_>>();

    if let Some(ch) = chars.first() {
        if ch.is_ascii_digit() {
            group_positions.push((0, ch.to_digit(10).unwrap() as usize));
        }
    }
    group_positions
}

/// Construct replace string and replace
/// ranges with new content in pattern space
fn update_pattern_space(
    pattern_space: &mut String,
    replacement: &str,
    ranges: &HashMap<usize, Range<usize>>,
) -> bool {
    let pairs = replacement.chars().collect::<Vec<_>>();
    let ampersand_positions = get_ampersand_positions(pairs.clone());
    let group_positions = get_group_positions(pairs);
    let mut local_replacement = replacement.to_string();
    if let Some((_, range)) = ranges.iter().next() {
        let value = (*pattern_space).get(range.clone());
        for position in ampersand_positions.clone() {
            local_replacement.replace_range(position..(position + 1), value.unwrap());
        }
    }

    if ranges.is_empty() {
        return false;
    }

    let main_range = ranges.iter().map(|(_, r)| r.start).min().unwrap()
        ..ranges.iter().map(|(_, r)| r.end).max().unwrap();

    if !group_positions.is_empty() {
        for (position, group) in group_positions {
            let replace_str = if let Some(range) = ranges.get(&group) {
                pattern_space.get(range.clone()).unwrap()
            } else {
                &"".to_string()
            };
            local_replacement.replace_range(position..(position + 2), replace_str);
        }
        local_replacement = local_replacement.replace("\\", "");
        pattern_space.replace_range(main_range.clone(), &local_replacement);
    } else if !ranges.is_empty() {
        local_replacement = local_replacement.replace("\\", "");
        pattern_space.replace_range(main_range.clone(), &local_replacement);
        return true;
    }
    false
}

/// Execute [`Command::Replace`] for current [`Sed`] line
fn execute_replace(
    pattern_space: &mut String,
    command: Command,
    line_number: usize,
) -> Result<bool, SedError> {
    let mut replace = false;
    let Command::Replace(_, re, pattern, replacement, flags) = command else {
        unreachable!();
    };
    let match_subranges = match_pattern(re.0, pattern, pattern_space, line_number)?;
    let is_replace_n = |f: &ReplaceFlag| {
        let ReplaceFlag::ReplaceNth(_) = f.clone() else {
            return false;
        };
        true
    };
    if !match_subranges.is_empty()
        && !flags.iter().any(is_replace_n)
        && !flags.contains(&ReplaceFlag::ReplaceAll)
    {
        replace = update_pattern_space(pattern_space, &replacement, &match_subranges[0]);
    } else if let Some(ReplaceFlag::ReplaceNth(n)) =
        flags.iter().find(|f: &&ReplaceFlag| is_replace_n(f))
    {
        if let Some(ranges) = match_subranges.get(*n - 1) {
            replace = update_pattern_space(pattern_space, &replacement, ranges);
        }
    } else if flags.contains(&ReplaceFlag::ReplaceAll) {
        for ranges in match_subranges.iter().rev() {
            let r = update_pattern_space(pattern_space, &replacement, ranges);
            if !replace {
                replace = r;
            }
        }
    }

    if flags.contains(&ReplaceFlag::PrintPatternIfReplace) && !match_subranges.is_empty() && replace
    {
        println!("{}", *pattern_space);
    }

    if let Some(wfile) = flags.iter().find_map(|flag| {
        let ReplaceFlag::AppendToIfReplace(wfile) = flag else {
            return None;
        };
        Some(wfile)
    }) {
        if replace && wfile.components().next().is_some() {
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
    /// Read next line in current input file and continue current cycle
    ReadNext,
    /// Append next line to current pattern space and continue current cycle  
    AppendNext,
    // /// Skip print after cycle
    // SkipPrint
}

/// Main program structure. Process input
/// files by [`Script`] [`Command`]s
struct Sed {
    /// Use extended regular expresions
    ere: bool,
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
    /// Last regex_t in applied [`Command`]  
    last_regex: Option<Regex>,
    /// Next line for processing
    next_line: String,
    /// Indicate that current processed line is last
    is_last_line: bool,
    /// Contains chars '\n\r' line end of current line for processed file
    current_end: Option<String>,
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
                // a
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                self.pattern_space += &("\n".to_string() + &text);
                self.current_end = Some("\n".to_string());
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
                // I
                if !self.need_execute(command_position)? {
                    return Ok(None);
                }
                print_multiline_binary(&self.pattern_space);
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
                if !self.quiet {
                    println!("{}", self.current_line + 1);
                }
            }
            Command::IgnoreComment if !self.quiet => {
                // #
                self.quiet = true;
            }
            Command::_Unknown => {}
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
        if to_first_line && self.pattern_space.contains('\n') {
            self.pattern_space = self
                .pattern_space
                .chars()
                .skip_while(|ch| *ch == '\n')
                .collect::<String>();
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
        if let Ok(file) = File::open(rfile) {
            let reader = BufReader::new(file);
            for line in reader.lines() {
                let Ok(line) = line else {
                    break;
                };
                self.pattern_space += "\n";
                self.pattern_space += &line;
            }
            if self.current_end.is_none() {
                self.current_end = Some("\n".to_string());
            }
        } else {
            self.current_end = Some("\n".to_string());
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
        self.has_replacements_since_t = execute_replace(
            &mut self.pattern_space,
            Command::Replace(address, regex.clone(), pattern, replacement, flags),
            self.current_line,
        )?;
        self.last_regex = Some(regex.clone());
        Ok(())
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
                        err.to_string().to_lowercase()
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
        self.pattern_space = self.pattern_space.replace("\\n", "\n");
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

        let need_execute =
            command.need_execute(self.current_line, &self.pattern_space, self.is_last_line)?;

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
                            return Ok(None);
                        } else {
                            break;
                        }
                    }
                    ControlFlowInstruction::NotReadNext => {
                        self.hold_space.clear();
                        i = 0;
                    }
                    ControlFlowInstruction::AppendNext => {
                        let mut line = self.next_line.clone();
                        self.next_line = self.read_line()?;
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
                        self.current_line += 1;
                        if line.is_empty() {
                            break;
                        }
                        if !self.quiet {
                            print!("{}", self.pattern_space);
                            print!("{}", self.current_end.clone().unwrap_or_default());
                        }
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
        *ERE.lock().unwrap() = self.ere;
        for mut input in self.input_sources.drain(..).collect::<Vec<_>>() {
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
    setlocale(LocaleCategory::LcAll, "");
    let project_name = std::env::var("PROJECT_NAME")
        .ok()
        .unwrap_or("posixutil-text".to_string());
    textdomain(&*project_name)?;
    bind_textdomain_codeset(&*project_name, "UTF-8")?;

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
