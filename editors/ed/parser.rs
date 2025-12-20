//
// Copyright (c) 2024 Jeff Garzik
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
                let n: usize = iter::once(ch)
                    .chain(iter::from_fn(|| {
                        iter.by_ref().next_if(|s| s.is_ascii_digit())
                    }))
                    .collect::<String>()
                    .parse()
                    .unwrap();
                tokens.push(Token::Number(n));
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
                    let unsigned: isize = offset_str.parse().unwrap();
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
    pub fn new() -> Self {
        Address {
            info: AddressInfo::Null,
            offsets: Vec::new(),
            relative_to_first: false,
        }
    }

    pub fn current() -> Self {
        Address {
            info: AddressInfo::Current,
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

/// Print mode for print commands.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PrintMode {
    Normal,
    Numbered,
    List,
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
    /// (.,.)l - list lines unambiguously
    ListLines(Address, Address),
    /// (.,.)m<addr> - move lines
    Move(Address, Address, Address),
    /// (.,.)n - print with line numbers
    Number(Address, Address),
    /// (.)p - print lines
    Print(Address, Address, PrintMode),
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
    /// (1,$)w [file] - write
    Write(Address, Address, Option<String>, bool), // (addr1, addr2, filename, append)
    /// (1,$)wq [file] - write and quit
    WriteQuit(Address, Address, Option<String>),
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

/// Parse a command line into a Command.
pub fn parse(line: &str) -> EdResult<Command> {
    let tokens = tokenize(line)?;
    parse_tokens(tokens)
}

/// Parse tokens into a Command.
pub fn parse_tokens(mut tokens: Vec<Token>) -> EdResult<Command> {
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
                    let rest = if !tokens.is_empty() {
                        if let Token::Rest(s) = tokens.remove(0) {
                            s.trim().to_string()
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
        return Ok(Command::Goto(addrvec[0].clone()));
    }

    // Empty line - print next line
    Ok(Command::Goto(Address {
        info: AddressInfo::Offset(1),
        offsets: Vec::new(),
        relative_to_first: false,
    }))
}

/// Parse a specific command with its arguments.
fn parse_command(
    cmd: char,
    rest: String,
    mut addrvec: Vec<Address>,
    separator: Option<char>,
) -> EdResult<Command> {
    match cmd {
        'a' => {
            normalize_addrvec(&mut addrvec, 1, separator, get_default_addressing('a'));
            Ok(Command::Append(addrvec[0].clone()))
        }
        'c' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('c'));
            Ok(Command::Change(addrvec[0].clone(), addrvec[1].clone()))
        }
        'd' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('d'));
            Ok(Command::Delete(addrvec[0].clone(), addrvec[1].clone()))
        }
        'e' | 'E' => {
            if !addrvec.is_empty() && addrvec.iter().any(|a| a.info != AddressInfo::Null) {
                return Err(EdError::Syntax("unexpected address".to_string()));
            }
            let filename = if rest.is_empty() { None } else { Some(rest) };
            Ok(Command::Edit(filename, cmd == 'E'))
        }
        'f' => {
            if !addrvec.is_empty() && addrvec.iter().any(|a| a.info != AddressInfo::Null) {
                return Err(EdError::Syntax("unexpected address".to_string()));
            }
            let filename = if rest.is_empty() { None } else { Some(rest) };
            Ok(Command::Filename(filename))
        }
        'g' | 'v' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing(cmd));
            let (pattern, commands) = parse_global_args(&rest)?;
            Ok(if cmd == 'g' {
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
            })
        }
        'G' | 'V' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('G'));
            let pattern = parse_global_pattern(&rest)?;
            Ok(if cmd == 'G' {
                Command::GlobalInteractive(addrvec[0].clone(), addrvec[1].clone(), pattern)
            } else {
                Command::GlobalNotInteractive(addrvec[0].clone(), addrvec[1].clone(), pattern)
            })
        }
        'h' => Ok(Command::Help),
        'H' => Ok(Command::HelpMode),
        'i' => {
            normalize_addrvec(&mut addrvec, 1, separator, get_default_addressing('i'));
            Ok(Command::Insert(addrvec[0].clone()))
        }
        'j' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('j'));
            Ok(Command::Join(addrvec[0].clone(), addrvec[1].clone()))
        }
        'k' => {
            normalize_addrvec(&mut addrvec, 1, separator, get_default_addressing('k'));
            if rest.len() != 1 || !rest.chars().next().unwrap().is_ascii_lowercase() {
                return Err(EdError::Syntax("invalid mark".to_string()));
            }
            Ok(Command::Mark(
                addrvec[0].clone(),
                rest.chars().next().unwrap(),
            ))
        }
        'l' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('l'));
            Ok(Command::ListLines(addrvec[0].clone(), addrvec[1].clone()))
        }
        'm' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('m'));
            let dest = parse_destination(&rest)?;
            Ok(Command::Move(addrvec[0].clone(), addrvec[1].clone(), dest))
        }
        'n' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('n'));
            Ok(Command::Number(addrvec[0].clone(), addrvec[1].clone()))
        }
        'p' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('p'));
            Ok(Command::Print(
                addrvec[0].clone(),
                addrvec[1].clone(),
                PrintMode::Normal,
            ))
        }
        'P' => Ok(Command::PromptToggle),
        'q' | 'Q' => {
            if !addrvec.is_empty() && addrvec.iter().any(|a| a.info != AddressInfo::Null) {
                return Err(EdError::Syntax("unexpected address".to_string()));
            }
            Ok(Command::Quit(cmd == 'Q'))
        }
        'r' => {
            normalize_addrvec(&mut addrvec, 1, separator, DefaultAddressing::Last);
            let filename = if rest.is_empty() { None } else { Some(rest) };
            Ok(Command::Read(addrvec[0].clone(), filename))
        }
        's' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('s'));
            let (pattern, replacement, flags) = parse_substitute_args(&rest)?;
            Ok(Command::Substitute(
                addrvec[0].clone(),
                addrvec[1].clone(),
                pattern,
                replacement,
                flags,
            ))
        }
        't' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('t'));
            let dest = parse_destination(&rest)?;
            Ok(Command::Copy(addrvec[0].clone(), addrvec[1].clone(), dest))
        }
        'u' => Ok(Command::Undo),
        'w' | 'W' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('w'));
            let filename = if rest.is_empty() { None } else { Some(rest) };
            Ok(Command::Write(
                addrvec[0].clone(),
                addrvec[1].clone(),
                filename,
                cmd == 'W',
            ))
        }
        'x' => {
            // wq alias
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('w'));
            let filename = if rest.is_empty() { None } else { Some(rest) };
            Ok(Command::WriteQuit(
                addrvec[0].clone(),
                addrvec[1].clone(),
                filename,
            ))
        }
        'z' => {
            normalize_addrvec(&mut addrvec, 1, separator, get_default_addressing('z'));
            let count = if rest.is_empty() {
                None
            } else {
                Some(
                    rest.parse()
                        .map_err(|_| EdError::Syntax("invalid count".to_string()))?,
                )
            };
            Ok(Command::Scroll(addrvec[0].clone(), count))
        }
        '=' => {
            normalize_addrvec(&mut addrvec, 1, separator, DefaultAddressing::Last);
            Ok(Command::LineNumber(addrvec[0].clone()))
        }
        '!' => Ok(Command::Shell(rest)),
        '#' => {
            normalize_addrvec(&mut addrvec, 1, separator, get_default_addressing('#'));
            Ok(Command::Null(addrvec[0].clone()))
        }
        '&' => {
            normalize_addrvec(&mut addrvec, 2, separator, get_default_addressing('&'));
            Ok(Command::RepeatSubstitute(
                addrvec[0].clone(),
                addrvec[1].clone(),
            ))
        }
        _ => Err(EdError::InvalidCommand(cmd.to_string())),
    }
}

/// Parse destination address for m/t commands.
fn parse_destination(s: &str) -> EdResult<Address> {
    if s.is_empty() {
        return Ok(Address::current());
    }
    let tokens = tokenize(s)?;
    if tokens.is_empty() {
        return Ok(Address::current());
    }

    let mut addr = Address::new();
    for token in tokens {
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
            _ => return Err(EdError::Syntax("invalid destination".to_string())),
        }
    }
    if addr.info == AddressInfo::Null {
        addr.info = AddressInfo::Current;
    }
    Ok(addr)
}

/// Parse substitute command arguments: /pattern/replacement/flags
fn parse_substitute_args(s: &str) -> EdResult<(String, String, String)> {
    if s.is_empty() {
        return Err(EdError::Syntax("no pattern delimiter".to_string()));
    }

    let delim = s.chars().next().unwrap();
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut escaped = false;
    let chars = s.chars().skip(1);

    for ch in chars {
        if escaped {
            if ch == delim {
                current.push(ch);
            } else {
                current.push('\\');
                current.push(ch);
            }
            escaped = false;
        } else if ch == '\\' {
            escaped = true;
        } else if ch == delim {
            parts.push(current);
            current = String::new();
        } else {
            current.push(ch);
        }
    }
    if escaped {
        current.push('\\');
    }
    parts.push(current);

    let pattern = parts.first().cloned().unwrap_or_default();
    let replacement = parts.get(1).cloned().unwrap_or_default();
    let flags = parts.get(2).cloned().unwrap_or_default();

    Ok((pattern, replacement, flags))
}

/// Parse global command pattern and command list.
fn parse_global_args(s: &str) -> EdResult<(String, String)> {
    if s.is_empty() {
        return Err(EdError::Syntax("no pattern delimiter".to_string()));
    }

    let delim = s.chars().next().unwrap();
    let mut pattern = String::new();
    let mut escaped = false;
    let mut chars = s.chars().skip(1).peekable();

    for ch in chars.by_ref() {
        if escaped {
            if ch == delim {
                pattern.push(ch);
            } else {
                pattern.push('\\');
                pattern.push(ch);
            }
            escaped = false;
        } else if ch == '\\' {
            escaped = true;
        } else if ch == delim {
            break;
        } else {
            pattern.push(ch);
        }
    }

    let commands: String = chars.collect();
    let commands = if commands.is_empty() {
        "p".to_string() // Default to print
    } else {
        commands
    };

    Ok((pattern, commands))
}

/// Parse global command pattern only.
fn parse_global_pattern(s: &str) -> EdResult<String> {
    if s.is_empty() {
        return Err(EdError::Syntax("no pattern delimiter".to_string()));
    }

    let delim = s.chars().next().unwrap();
    let mut pattern = String::new();
    let mut escaped = false;

    for ch in s.chars().skip(1) {
        if escaped {
            if ch == delim {
                pattern.push(ch);
            } else {
                pattern.push('\\');
                pattern.push(ch);
            }
            escaped = false;
        } else if ch == '\\' {
            escaped = true;
        } else if ch == delim {
            break;
        } else {
            pattern.push(ch);
        }
    }

    Ok(pattern)
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
        let cmd = parse("q").unwrap();
        assert_eq!(cmd, Command::Quit(false));
    }

    #[test]
    fn test_parse_force_quit() {
        let cmd = parse("Q").unwrap();
        assert_eq!(cmd, Command::Quit(true));
    }

    #[test]
    fn test_parse_print() {
        let cmd = parse("1,5p").unwrap();
        match cmd {
            Command::Print(a1, a2, mode) => {
                assert_eq!(a1.info, AddressInfo::Line(1));
                assert_eq!(a2.info, AddressInfo::Line(5));
                assert_eq!(mode, PrintMode::Normal);
            }
            _ => panic!("expected Print command"),
        }
    }

    #[test]
    fn test_parse_append() {
        let cmd = parse("a").unwrap();
        match cmd {
            Command::Append(a) => {
                assert_eq!(a.info, AddressInfo::Current);
            }
            _ => panic!("expected Append command"),
        }
    }

    #[test]
    fn test_parse_substitute() {
        let cmd = parse("1,5s/foo/bar/g").unwrap();
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
}
