//
// Copyright (c) 2024-2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Command parsing for the ed editor.

use crate::ed::error::{EdError, EdResult};
use std::iter;

/// Token produced by the lexer.
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    CurrentLine,
    LastLine,
    Number(usize),
    Mark(char),
    RegexForward(String),
    RegexBack(String),
    Offset(isize),
    AddressSeparator(char),
    Command(char),
    Rest(String),
}

/// Tokenize an ed command line.
pub fn tokenize(input: &str) -> EdResult<Vec<Token>> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut iter = input.chars().peekable();

    while let Some(ch) = iter.next() {
        match ch {
            ch if ch.is_whitespace() => continue,
            '.' => tokens.push(Token::CurrentLine),
            '$' => tokens.push(Token::LastLine),
            '0'..='9' => {
                // Addresses are resolved in signed arithmetic, so parse as
                // `isize`: a line number that does not fit is out of range,
                // not a reason to abort the editor.
                let digits: String = iter::once(ch)
                    .chain(iter::from_fn(|| {
                        iter.by_ref().next_if(|s| s.is_ascii_digit())
                    }))
                    .collect();
                let n: isize = digits.parse().map_err(|_| EdError::AddressOutOfRange)?;
                tokens.push(Token::Number(n as usize));
            }
            '\'' => match iter.next() {
                None => return Err(EdError::Syntax("missing mark character".to_string())),
                Some(mark_ch) => {
                    if mark_ch.is_ascii_lowercase() {
                        tokens.push(Token::Mark(mark_ch));
                    } else {
                        return Err(EdError::Syntax(format!(
                            "invalid mark character: {}",
                            mark_ch
                        )));
                    }
                }
            },
            ',' | ';' => {
                tokens.push(Token::AddressSeparator(ch));
            }
            '/' | '?' => {
                let mut bre = String::new();
                let mut escaped = false;
                loop {
                    let bre_ch_res = iter.next();
                    if bre_ch_res.is_none() {
                        // Unterminated is OK for search - uses previous pattern
                        if ch == '/' {
                            tokens.push(Token::RegexForward(bre));
                        } else {
                            tokens.push(Token::RegexBack(bre));
                        }
                        break;
                    }

                    let bre_ch = bre_ch_res.unwrap();
                    match bre_ch {
                        '/' | '?' if bre_ch == ch => {
                            if escaped {
                                bre.push(bre_ch);
                                escaped = false;
                            } else {
                                if ch == '/' {
                                    tokens.push(Token::RegexForward(bre));
                                } else {
                                    tokens.push(Token::RegexBack(bre));
                                }
                                break;
                            }
                        }
                        '\\' => {
                            if escaped {
                                bre.push('\\');
                                escaped = false;
                            } else {
                                escaped = true;
                            }
                        }
                        _ => {
                            if escaped {
                                bre.push('\\');
                                escaped = false;
                            }
                            bre.push(bre_ch);
                        }
                    }
                }
            }
            '+' | '-' => {
                let sign = ch;
                let mut offset_str = String::new();
                while let Some(&next_ch) = iter.peek() {
                    if next_ch.is_ascii_digit() {
                        offset_str.push(iter.next().unwrap());
                    } else {
                        break;
                    }
                }

                let n: isize = if offset_str.is_empty() {
                    if sign == '+' {
                        1
                    } else {
                        -1
                    }
                } else {
                    let unsigned: isize =
                        offset_str.parse().map_err(|_| EdError::AddressOutOfRange)?;
                    if sign == '-' {
                        -unsigned
                    } else {
                        unsigned
                    }
                };

                tokens.push(Token::Offset(n));
            }
            'a'..='z' | 'A'..='Z' | '=' | '!' | '#' | '&' => {
                // Command character - collect the rest of the line
                let rest: String = iter.collect();
                tokens.push(Token::Command(ch));
                if !rest.is_empty() {
                    tokens.push(Token::Rest(rest));
                }
                break;
            }
            _ => return Err(EdError::Syntax(format!("unrecognized character: {}", ch))),
        }
    }

    Ok(tokens)
}

/// Parsed address information.
#[derive(Clone, Debug, PartialEq)]
pub enum AddressInfo {
    Null,
    Current,
    Last,
    Line(usize),
    Mark(char),
    RegexForward(String),
    RegexBack(String),
    Offset(isize),
}

/// A parsed address with optional offsets.
#[derive(Clone, Debug, PartialEq)]
pub struct Address {
    pub info: AddressInfo,
    pub offsets: Vec<isize>,
    /// If true, this address was preceded by a semicolon separator,
    /// meaning it should be resolved relative to the first address
    /// (POSIX requirement for `;` separator)
    pub relative_to_first: bool,
}

impl Address {
    /// `.` with no offsets, as a defaulted address parses.
    pub fn current_line() -> Self {
        Address {
            info: AddressInfo::Current,
            offsets: Vec::new(),
            relative_to_first: false,
        }
    }

    pub fn new() -> Self {
        Address {
            info: AddressInfo::Null,
            offsets: Vec::new(),
            relative_to_first: false,
        }
    }
}

impl Default for Address {
    fn default() -> Self {
        Self::new()
    }
}

/// How lines are written: by `p`, `n` and `l`, by the print suffix those
/// letters form after another command, and by the `s` flags.  They combine:
/// `nl` numbers a line and lists it.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct PrintFlags {
    pub print: bool,
    pub number: bool,
    pub list: bool,
}

impl PrintFlags {
    /// Whether anything is to be written.
    pub fn any(self) -> bool {
        self.print || self.number || self.list
    }

    /// Add one of `l`, `n` or `p`; false for any other character.
    pub fn add(&mut self, ch: char) -> bool {
        match ch {
            'p' => self.print = true,
            'n' => self.number = true,
            'l' => self.list = true,
            _ => return false,
        }
        true
    }
}

/// POSIX (ed): "any command other than e, E, f, q, Q, r, w, and ! can be
/// suffixed by a command from l, n, or p".  Anything else after the command
/// is an error: ignoring it made `1dx` a delete and `fromstdin` an `f`.
fn parse_suffix(rest: &str) -> EdResult<PrintFlags> {
    let mut flags = PrintFlags::default();
    for ch in rest.trim().chars() {
        if !flags.add(ch) {
            return Err(EdError::Syntax(format!("invalid command suffix: {}", rest)));
        }
    }
    Ok(flags)
}

/// A parsed command and the print suffix that followed it.
#[derive(Clone, Debug, PartialEq)]
pub struct Parsed {
    pub cmd: Command,
    pub suffix: PrintFlags,
}

/// Parsed ed command.
#[derive(Clone, Debug, PartialEq)]
pub enum Command {
    /// (.)a - append text after line
    Append(Address),
    /// (.,.)c - change lines
    Change(Address, Address),
    /// (.,.)d - delete lines
    Delete(Address, Address),
    /// e file - edit file
    Edit(Option<String>, bool), // (filename, force)
    /// f [file] - set/print filename
    Filename(Option<String>),
    /// (1,$)g/re/command-list - global
    Global(Address, Address, String, String, bool), // (addr1, addr2, pattern, commands, interactive)
    /// (1,$)G/re/ - interactive global
    GlobalInteractive(Address, Address, String),
    /// h - print last error
    Help,
    /// H - toggle help mode
    HelpMode,
    /// (.)i - insert text before line
    Insert(Address),
    /// (.,.+1)j - join lines
    Join(Address, Address),
    /// (.)k<x> - mark line
    Mark(Address, char),
    /// (.,.)m<addr> - move lines
    Move(Address, Address, Address),
    /// (.,.)p, n, l - write lines, as the flags say
    Print(Address, Address, PrintFlags),
    /// P - toggle prompt
    PromptToggle,
    /// q - quit
    Quit(bool), // force
    /// ($)r [file] - read file
    Read(Address, Option<String>),
    /// (.,.)s/re/replacement/flags - substitute
    Substitute(Address, Address, String, String, String), // pattern, replacement, flags
    /// (.,.)t<addr> - copy lines
    Copy(Address, Address, Address),
    /// u - undo
    Undo,
    /// (1,$)v/re/command-list - global not matching
    GlobalNot(Address, Address, String, String, bool),
    /// (1,$)V/re/ - interactive global not matching
    GlobalNotInteractive(Address, Address, String),
    /// (1,$)w [file] - write; `W` appends, and a `q` glued to either
    /// (`wq`, `Wq`) also quits.
    Write {
        addr1: Address,
        addr2: Address,
        filename: Option<String>,
        append: bool,
        quit: bool,
    },
    /// ($)= - print line number
    LineNumber(Address),
    /// !command - shell escape
    Shell(String),
    /// (.+1)z[n] - scroll
    Scroll(Address, Option<usize>),
    /// (.)# - comment (null command for address)
    Null(Address),
    /// & - repeat last substitute
    RepeatSubstitute(Address, Address),
    /// Empty command (just address or newline)
    Goto(Address),
}

/// Default addressing behavior for each command.
#[derive(Clone, Copy, Debug)]
pub enum DefaultAddressing {
    Current,        // .,.
    CurrentPlusOne, // .,.+1 (for join)
    Full,           // 1,$
    Last,           // $
    None,           // no default
}

impl DefaultAddressing {
    pub fn get_range(self) -> (AddressInfo, AddressInfo) {
        match self {
            DefaultAddressing::Current => (AddressInfo::Current, AddressInfo::Current),
            DefaultAddressing::CurrentPlusOne => (AddressInfo::Current, AddressInfo::Offset(1)),
            DefaultAddressing::Full => (AddressInfo::Line(1), AddressInfo::Last),
            DefaultAddressing::Last => (AddressInfo::Last, AddressInfo::Last),
            DefaultAddressing::None => (AddressInfo::Null, AddressInfo::Null),
        }
    }
}

/// Get the default addressing for a command character.
pub fn get_default_addressing(cmd: char) -> DefaultAddressing {
    match cmd {
        'a' | 'i' | 'k' | '=' | 'r' | 'z' => DefaultAddressing::Current,
        'c' | 'd' | 'l' | 'm' | 'n' | 'p' | 's' | 't' | '#' | '&' => DefaultAddressing::Current,
        'j' => DefaultAddressing::CurrentPlusOne,
        'g' | 'G' | 'v' | 'V' | 'w' | 'W' => DefaultAddressing::Full,
        _ => DefaultAddressing::None,
    }
}

/// Parser state.
enum ParseState {
    Address,
    SepOffCommand,
    Command,
}

/// Normalize the address vector based on command requirements.
fn normalize_addrvec(
    av: &mut Vec<Address>,
    n_addr: usize,
    separator: Option<char>,
    default_addr: DefaultAddressing,
) {
    if av.is_empty() {
        av.push(Address::new());
    }

    // Ensure the vector has the correct number of addresses
    while av.len() < n_addr {
        av.push(av[0].clone());
    }
    while av.len() > n_addr {
        av.remove(0);
    }

    // Patch Null addresses for second address
    if av.len() > 1 && av[1].info == AddressInfo::Null {
        if av[0].info == AddressInfo::Null {
            av[1].info = AddressInfo::Last;
        } else {
            av[1] = av[0].clone();
        }
    }

    // Patch Null addresses for first address
    if av[0].info == AddressInfo::Null {
        match separator {
            Some(',') => av[0].info = AddressInfo::Line(1),
            Some(';') => av[0].info = AddressInfo::Current,
            None => {
                let default_range = default_addr.get_range();
                av[0].info = default_range.0;
                if n_addr > 1 {
                    av[1].info = default_range.1;
                }
            }
            _ => {}
        }
    }
}

/// Parse a command line into a command and its print suffix.
pub fn parse(line: &str) -> EdResult<Parsed> {
    let tokens = tokenize(line)?;
    parse_tokens(tokens)
}

/// Parse tokens into a command and its print suffix.
pub fn parse_tokens(mut tokens: Vec<Token>) -> EdResult<Parsed> {
    let mut addr = Address::new();
    let mut addr_dirty = false;
    let mut addrvec: Vec<Address> = Vec::new();
    let mut state = ParseState::Address;
    let mut separator: Option<char> = None;

    while !tokens.is_empty() {
        let token = tokens.remove(0);
        match state {
            ParseState::Address => {
                addr_dirty = true;
                state = ParseState::SepOffCommand;
                match token {
                    Token::CurrentLine => addr.info = AddressInfo::Current,
                    Token::LastLine => addr.info = AddressInfo::Last,
                    Token::Number(v) => addr.info = AddressInfo::Line(v),
                    Token::Mark(ch) => addr.info = AddressInfo::Mark(ch),
                    Token::RegexForward(s) => addr.info = AddressInfo::RegexForward(s),
                    Token::RegexBack(s) => addr.info = AddressInfo::RegexBack(s),
                    Token::AddressSeparator(ch) => {
                        addrvec.push(addr);
                        addr = Address::new();
                        // Mark this address as relative to first if preceded by semicolon
                        // (POSIX: semicolon sets current line to first address before evaluating second)
                        addr.relative_to_first = ch == ';';
                        state = ParseState::Address;
                        separator = Some(ch);
                    }
                    Token::Offset(i) => {
                        if addr.info == AddressInfo::Null {
                            addr.info = AddressInfo::Current;
                        }
                        addr.offsets.push(i);
                    }
                    Token::Command(_) | Token::Rest(_) => {
                        tokens.insert(0, token);
                        state = ParseState::Command;
                        if addr_dirty {
                            addrvec.push(addr);
                            addr = Address::new();
                            addr_dirty = false;
                        }
                    }
                }
            }
            ParseState::SepOffCommand => {
                match token {
                    Token::AddressSeparator(ch) => {
                        state = ParseState::Address;
                        separator = Some(ch);
                        // When we push the current address and create a new one,
                        // the new address needs to know if it should be relative to first
                        if addr_dirty {
                            addrvec.push(addr);
                            addr = Address::new();
                            addr.relative_to_first = ch == ';';
                            addr_dirty = false;
                        }
                    }
                    Token::Offset(i) => addr.offsets.push(i),
                    Token::Command(_) | Token::Rest(_) => {
                        tokens.insert(0, token);
                        state = ParseState::Command;
                        // Push current address before transitioning to command state
                        if addr_dirty {
                            addrvec.push(addr);
                            addr = Address::new();
                            addr_dirty = false;
                        }
                    }
                    _ => return Err(EdError::Syntax("unexpected token".to_string())),
                }
            }
            ParseState::Command => match token {
                Token::Command(cmd) => {
                    // Untrimmed: `w` distinguishes `wq` (write-quit) from
                    // `w q` (write to a file named `q`) by literal adjacency,
                    // so the leading space has to survive to `parse_command`.
                    let rest = if !tokens.is_empty() {
                        if let Token::Rest(s) = tokens.remove(0) {
                            s
                        } else {
                            String::new()
                        }
                    } else {
                        String::new()
                    };

                    return parse_command(cmd, rest, addrvec, separator);
                }
                _ => return Err(EdError::Syntax("expected command".to_string())),
            },
        }
    }

    // No command found - could be just an address (goto)
    if addr_dirty {
        addrvec.push(addr);
    }

    if !addrvec.is_empty() {
        // Just an address - go to that line
        normalize_addrvec(&mut addrvec, 1, separator, DefaultAddressing::Current);
        return Ok(Parsed {
            cmd: Command::Goto(addrvec[0].clone()),
            suffix: PrintFlags::default(),
        });
    }

    // Empty line - print next line
    Ok(Parsed {
        cmd: Command::Goto(Address {
            info: AddressInfo::Offset(1),
            offsets: Vec::new(),
            relative_to_first: false,
        }),
        suffix: PrintFlags::default(),
    })
}

/// Commands that take no address (`e E f q Q h H P u !`) refuse one,
/// including a bare separator: `5u` used to undo and `,q` to quit.
fn no_address(addrvec: &[Address], separator: Option<char>) -> EdResult<()> {
    if separator.is_some() || addrvec.iter().any(|a| a.info != AddressInfo::Null) {
        return Err(EdError::Syntax("unexpected address".to_string()));
    }
    Ok(())
}

/// The pathname argument of `e E f r w W`: empty, or separated from the
/// command by a blank.  `fromstdin` is not `f romstdin`.
fn filename_arg(rest_raw: &str) -> EdResult<Option<String>> {
    if rest_raw.is_empty() {
        return Ok(None);
    }
    if !rest_raw.starts_with([' ', '\t']) {
        return Err(EdError::Syntax(format!(
            "invalid command suffix: {}",
            rest_raw
        )));
    }
    let name = rest_raw.trim();
    Ok((!name.is_empty()).then(|| name.to_string()))
}

/// Parse a specific command with its arguments.
fn parse_command(
    cmd: char,
    rest_raw: String,
    mut addrvec: Vec<Address>,
    separator: Option<char>,
) -> EdResult<Parsed> {
    // Every command but `w`/`W` and the pathname commands wants the argument
    // with surrounding blanks removed.
    let rest = rest_raw.trim().to_string();
    let mut suffix = PrintFlags::default();
    let cmd = match cmd {
        'a' => {
            normalize_addrvec(&mut addrvec, 1, separator, get_default_addressing('a'));
            suffix = parse_suffix(&rest)?;
            Command::Append(addrvec[0].clone())
        }
        'c' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('c'));
            suffix = parse_suffix(&rest)?;
            Command::Change(addrvec[0].clone(), addrvec[1].clone())
        }
        'd' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('d'));
            suffix = parse_suffix(&rest)?;
            Command::Delete(addrvec[0].clone(), addrvec[1].clone())
        }
        'e' | 'E' => {
            no_address(&addrvec, separator)?;
            Command::Edit(filename_arg(&rest_raw)?, cmd == 'E')
        }
        'f' => {
            no_address(&addrvec, separator)?;
            Command::Filename(filename_arg(&rest_raw)?)
        }
        'g' | 'v' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing(cmd));
            // Leading blanks only: the list's last command may end in text.
            let (pattern, commands) = parse_global_args(rest_raw.trim_start())?;
            if cmd == 'g' {
                Command::Global(
                    addrvec[0].clone(),
                    addrvec[1].clone(),
                    pattern,
                    commands,
                    false,
                )
            } else {
                Command::GlobalNot(
                    addrvec[0].clone(),
                    addrvec[1].clone(),
                    pattern,
                    commands,
                    false,
                )
            }
        }
        'G' | 'V' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('G'));
            let pattern = parse_global_pattern(&rest)?;
            if cmd == 'G' {
                Command::GlobalInteractive(addrvec[0].clone(), addrvec[1].clone(), pattern)
            } else {
                Command::GlobalNotInteractive(addrvec[0].clone(), addrvec[1].clone(), pattern)
            }
        }
        'h' | 'H' | 'P' | 'u' => {
            no_address(&addrvec, separator)?;
            suffix = parse_suffix(&rest)?;
            match cmd {
                'h' => Command::Help,
                'H' => Command::HelpMode,
                'P' => Command::PromptToggle,
                _ => Command::Undo,
            }
        }
        'i' => {
            normalize_addrvec(&mut addrvec, 1, separator, get_default_addressing('i'));
            suffix = parse_suffix(&rest)?;
            Command::Insert(addrvec[0].clone())
        }
        'j' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('j'));
            suffix = parse_suffix(&rest)?;
            Command::Join(addrvec[0].clone(), addrvec[1].clone())
        }
        'k' => {
            normalize_addrvec(&mut addrvec, 1, separator, get_default_addressing('k'));
            let mut chars = rest.chars();
            let mark = chars
                .next()
                .filter(|c| c.is_ascii_lowercase())
                .ok_or_else(|| EdError::Syntax("invalid mark".to_string()))?;
            suffix = parse_suffix(chars.as_str())?;
            Command::Mark(addrvec[0].clone(), mark)
        }
        'l' | 'n' | 'p' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing(cmd));
            let mut flags = parse_suffix(&rest)?;
            flags.add(cmd);
            Command::Print(addrvec[0].clone(), addrvec[1].clone(), flags)
        }
        'm' | 't' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing(cmd));
            let (dest, flags) = parse_destination(&rest)?;
            suffix = flags;
            if cmd == 'm' {
                Command::Move(addrvec[0].clone(), addrvec[1].clone(), dest)
            } else {
                Command::Copy(addrvec[0].clone(), addrvec[1].clone(), dest)
            }
        }
        'q' | 'Q' => {
            no_address(&addrvec, separator)?;
            if !rest.is_empty() {
                return Err(EdError::Syntax(format!("invalid command suffix: {}", rest)));
            }
            Command::Quit(cmd == 'Q')
        }
        'r' => {
            normalize_addrvec(&mut addrvec, 1, separator, DefaultAddressing::Last);
            Command::Read(addrvec[0].clone(), filename_arg(&rest_raw)?)
        }
        's' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('s'));
            let (pattern, replacement, flags) = parse_substitute_args(&rest_raw)?;
            Command::Substitute(
                addrvec[0].clone(),
                addrvec[1].clone(),
                pattern,
                replacement,
                flags,
            )
        }
        'w' | 'W' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('w'));
            // A `q` immediately after the command letter is the write-quit
            // form; anything after a blank is a pathname, even a bare "q".
            let (quit, args) = match rest_raw.strip_prefix('q') {
                Some(after) => (true, after),
                None => (false, rest_raw.as_str()),
            };
            Command::Write {
                addr1: addrvec[0].clone(),
                addr2: addrvec[1].clone(),
                filename: filename_arg(args)?,
                append: cmd == 'W',
                quit,
            }
        }
        'z' => {
            normalize_addrvec(&mut addrvec, 1, separator, get_default_addressing('z'));
            let digits = rest.len() - rest.trim_start_matches(|c: char| c.is_ascii_digit()).len();
            let count = if digits == 0 {
                None
            } else {
                Some(
                    rest[..digits]
                        .parse()
                        .map_err(|_| EdError::Syntax("invalid count".to_string()))?,
                )
            };
            suffix = parse_suffix(&rest[digits..])?;
            Command::Scroll(addrvec[0].clone(), count)
        }
        '=' => {
            normalize_addrvec(&mut addrvec, 1, separator, DefaultAddressing::Last);
            suffix = parse_suffix(&rest)?;
            Command::LineNumber(addrvec[0].clone())
        }
        '!' => {
            no_address(&addrvec, separator)?;
            Command::Shell(rest)
        }
        // A comment: the rest of the line is ignored.
        '#' => {
            normalize_addrvec(&mut addrvec, 1, separator, get_default_addressing('#'));
            Command::Null(addrvec[0].clone())
        }
        '&' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('&'));
            suffix = parse_suffix(&rest)?;
            Command::RepeatSubstitute(addrvec[0].clone(), addrvec[1].clone())
        }
        _ => return Err(EdError::InvalidCommand(cmd.to_string())),
    };
    Ok(Parsed { cmd, suffix })
}

/// Parse the destination address of `m`/`t`, and the print suffix after it.
fn parse_destination(s: &str) -> EdResult<(Address, PrintFlags)> {
    let mut addr = Address::new();
    let mut suffix = PrintFlags::default();
    let mut tokens = tokenize(s)?.into_iter();
    while let Some(token) = tokens.next() {
        match token {
            Token::CurrentLine => addr.info = AddressInfo::Current,
            Token::LastLine => addr.info = AddressInfo::Last,
            Token::Number(n) => addr.info = AddressInfo::Line(n),
            Token::Mark(c) => addr.info = AddressInfo::Mark(c),
            Token::RegexForward(s) => addr.info = AddressInfo::RegexForward(s),
            Token::RegexBack(s) => addr.info = AddressInfo::RegexBack(s),
            Token::Offset(i) => {
                if addr.info == AddressInfo::Null {
                    addr.info = AddressInfo::Current;
                }
                addr.offsets.push(i);
            }
            // The tokenizer ends at the first letter and hands back the rest
            // of the line, which is all suffix.
            Token::Command(ch) => {
                let rest = match tokens.next() {
                    Some(Token::Rest(r)) => r,
                    _ => String::new(),
                };
                suffix = parse_suffix(&format!("{}{}", ch, rest.trim()))?;
            }
            _ => return Err(EdError::Syntax("invalid destination".to_string())),
        }
    }
    if addr.info == AddressInfo::Null {
        addr.info = AddressInfo::Current;
    }
    Ok((addr, suffix))
}

/// Parse substitute command arguments: /pattern/replacement/flags
///
/// The flags are everything after the third delimiter, unsplit, so text
/// after them (`s/a/b/g/x`) reaches the flag parser and is refused.  With the
/// third delimiter missing, POSIX makes `s/s1/s2` mean `s/s1/s2/p`.
fn parse_substitute_args(s: &str) -> EdResult<(String, String, String)> {
    let (pattern, replacement, flags) = split_substitute(s)?;
    Ok((
        pattern,
        replacement,
        flags.unwrap_or_else(|| "p".to_string()),
    ))
}

/// Whether `line` is an `s` whose replacement has no closing delimiter.
///
/// In a `g`/`v` command list such a line continues onto the next: the list
/// arrives with its escaped newlines already turned into plain ones, so
/// `g/x/s/a/b\<newline>c/` reaches the list as `s/a/b` and `c/`, and only
/// joining them gives the substitute the newline it was asked to insert.
pub fn continues_replacement(line: &str) -> bool {
    let mut tokens = match tokenize(line) {
        Ok(t) => t.into_iter(),
        Err(_) => return false,
    };
    while let Some(token) = tokens.next() {
        if token == Token::Command('s') {
            return match tokens.next() {
                Some(Token::Rest(rest)) => matches!(split_substitute(&rest), Ok((_, _, None))),
                _ => false,
            };
        }
    }
    false
}

/// Characters a BRE gives a meaning to unescaped.
const BRE_SPECIAL: &str = ".[*^$";
/// Characters a replacement gives a meaning to unescaped.
const REPLACEMENT_SPECIAL: &str = "&%";

/// The delimiter that opens an `s`, `g`, `v`, `G` or `V` argument: any
/// character but a blank, a newline or a backslash.
fn take_delimiter(chars: &mut std::str::Chars) -> EdResult<char> {
    match chars.next() {
        None => Err(EdError::Syntax("no pattern delimiter".to_string())),
        Some(c) if c.is_whitespace() || c == '\\' => Err(EdError::Syntax(format!(
            "invalid pattern delimiter: {:?}",
            c
        ))),
        Some(c) => Ok(c),
    }
}

/// One delimited field: its text and whether the closing delimiter was
/// there.
///
/// POSIX: an escaped delimiter is the delimiter as a literal character,
/// "losing any special meaning".  Where the delimiter is a character the
/// field gives a meaning to (`special`: `.` in an RE, `&` in a replacement),
/// dropping the backslash would hand it that meaning back -- `s.a\.b.X.`
/// matched `axb` -- so there the backslash stays.  Any other escape is kept
/// for the RE or replacement to interpret.
fn delimited_field(chars: &mut std::str::Chars, delim: char, special: &str) -> (String, bool) {
    let mut text = String::new();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next() {
                Some(c) if c == delim && !special.contains(c) => text.push(c),
                Some(c) => {
                    text.push('\\');
                    text.push(c);
                }
                None => text.push('\\'),
            }
        } else if ch == delim {
            return (text, true);
        } else {
            text.push(ch);
        }
    }
    (text, false)
}

/// Split `/pattern/replacement/flags`; the flags are `None` when the
/// replacement's closing delimiter is missing.
fn split_substitute(s: &str) -> EdResult<(String, String, Option<String>)> {
    let mut chars = s.chars();
    let delim = take_delimiter(&mut chars)?;
    let (pattern, closed) = delimited_field(&mut chars, delim, BRE_SPECIAL);
    if !closed {
        return Err(EdError::Syntax("missing pattern delimiter".to_string()));
    }
    let (replacement, closed) = delimited_field(&mut chars, delim, REPLACEMENT_SPECIAL);
    let flags = closed.then(|| chars.as_str().trim().to_string());
    Ok((pattern, replacement, flags))
}

/// Parse global command pattern and command list.
fn parse_global_args(s: &str) -> EdResult<(String, String)> {
    let mut chars = s.chars();
    let delim = take_delimiter(&mut chars)?;
    let (pattern, _) = delimited_field(&mut chars, delim, BRE_SPECIAL);
    let commands = chars.as_str();
    // An empty command list is `p`.
    let commands = if commands.is_empty() { "p" } else { commands };
    Ok((pattern, commands.to_string()))
}

/// Parse global command pattern only.
fn parse_global_pattern(s: &str) -> EdResult<String> {
    let mut chars = s.chars();
    let delim = take_delimiter(&mut chars)?;
    Ok(delimited_field(&mut chars, delim, BRE_SPECIAL).0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_simple() {
        let tokens = tokenize("1,5p").unwrap();
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], Token::Number(1));
        assert_eq!(tokens[1], Token::AddressSeparator(','));
        assert_eq!(tokens[2], Token::Number(5));
        assert_eq!(tokens[3], Token::Command('p'));
    }

    #[test]
    fn test_parse_quit() {
        let cmd = parse("q").unwrap().cmd;
        assert_eq!(cmd, Command::Quit(false));
    }

    #[test]
    fn test_parse_force_quit() {
        let cmd = parse("Q").unwrap().cmd;
        assert_eq!(cmd, Command::Quit(true));
    }

    #[test]
    fn test_parse_print() {
        let cmd = parse("1,5p").unwrap().cmd;
        match cmd {
            Command::Print(a1, a2, flags) => {
                assert_eq!(a1.info, AddressInfo::Line(1));
                assert_eq!(a2.info, AddressInfo::Line(5));
                assert_eq!(
                    flags,
                    PrintFlags {
                        print: true,
                        ..PrintFlags::default()
                    }
                );
            }
            _ => panic!("expected Print command"),
        }
    }

    #[test]
    fn test_parse_append() {
        let cmd = parse("a").unwrap().cmd;
        match cmd {
            Command::Append(a) => {
                assert_eq!(a.info, AddressInfo::Current);
            }
            _ => panic!("expected Append command"),
        }
    }

    #[test]
    fn test_parse_substitute() {
        let cmd = parse("1,5s/foo/bar/g").unwrap().cmd;
        match cmd {
            Command::Substitute(a1, a2, pat, repl, flags) => {
                assert_eq!(a1.info, AddressInfo::Line(1));
                assert_eq!(a2.info, AddressInfo::Line(5));
                assert_eq!(pat, "foo");
                assert_eq!(repl, "bar");
                assert_eq!(flags, "g");
            }
            _ => panic!("expected Substitute command"),
        }
    }

    #[test]
    fn test_parse_print_suffix() {
        let parsed = parse("1dpn").unwrap();
        assert!(matches!(parsed.cmd, Command::Delete(..)));
        assert!(parsed.suffix.print && parsed.suffix.number && !parsed.suffix.list);
        assert!(parse("1dx").is_err());
    }

    #[test]
    fn test_parse_substitute_unterminated() {
        let Command::Substitute(_, _, pat, repl, flags) = parse("s/a/b").unwrap().cmd else {
            panic!("expected Substitute command");
        };
        assert_eq!(
            (pat.as_str(), repl.as_str(), flags.as_str()),
            ("a", "b", "p")
        );
        assert!(parse("s/a").is_err());
    }
}
