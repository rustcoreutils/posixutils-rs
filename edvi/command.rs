//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::iter;

#[derive(Debug)]
struct SyntaxError {
    message: String,
}

#[derive(Debug, PartialEq)]
enum Token {
    CurrentLine,
    LastLine,
    Number(u64),
    Mark(char),
    RegexForward(String),
    RegexBack(String),
    Offset(isize),

    AddressSeparator(char),
    Command(char),
}

impl SyntaxError {
    fn new(message: String) -> Self {
        SyntaxError { message }
    }
}

fn tokenizer(input: &str) -> Result<Vec<Token>, SyntaxError> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut iter = input.chars().peekable();

    while let Some(ch) = iter.next() {
        match ch {
            ch if ch.is_whitespace() => continue,
            '.' => tokens.push(Token::CurrentLine),
            '$' => tokens.push(Token::LastLine),
            'a'..='z' | 'A'..='Z' => {
                tokens.push(Token::Command(ch));
            }
            '0'..='9' => {
                let n: u64 = iter::once(ch)
                    .chain(iter::from_fn(|| {
                        iter.by_ref().next_if(|s| s.is_ascii_digit())
                    }))
                    .collect::<String>()
                    .parse()
                    .unwrap();

                tokens.push(Token::Number(n));
            }
            '\'' => match iter.next() {
                None => return Err(SyntaxError::new(String::from("missing mark char"))),
                Some(mark_ch) => {
                    if mark_ch.is_ascii_alphabetic() {
                        tokens.push(Token::Mark(mark_ch));
                    } else {
                        return Err(SyntaxError::new(format!(
                            "unrecognized mark character {}",
                            ch
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
                        return Err(SyntaxError::new(String::from("unterminated regex")));
                    }

                    let bre_ch = bre_ch_res.unwrap();
                    match bre_ch {
                        // regex terminator
                        '/' | '?' => {
                            if escaped {
                                bre.push(bre_ch);
                            } else {
                                if ch == '/' {
                                    tokens.push(Token::RegexForward(bre));
                                } else {
                                    tokens.push(Token::RegexBack(bre));
                                }
                                break;
                            }
                        }

                        // escape char
                        '\\' => {
                            if escaped {
                                bre.push(bre_ch);
                            } else {
                                escaped = true;
                            }
                        }

                        // everything else
                        _ => bre.push(bre_ch),
                    }
                }
            }

            '+' | '-' => {
                let sign = ch;
                let mut offset = String::new();
                offset.push(sign);
                while let Some(&next_ch) = iter.peek() {
                    if next_ch.is_ascii_digit() {
                        offset.push(iter.next().unwrap());
                    } else {
                        break;
                    }
                }

                let n: isize = if offset.len() == 1 {
                    // No digits followed, so it's an implicit +1 or -1
                    if sign == '+' {
                        1
                    } else {
                        -1
                    }
                } else {
                    offset
                        .parse()
                        .map_err(|_| SyntaxError::new(String::from("invalid offset")))?
                };

                tokens.push(Token::Offset(n));
            }
            _ => return Err(SyntaxError::new(format!("unrecognized character {}", ch))),
        }
    }

    Ok(tokens)
}

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

#[derive(Clone, Debug, PartialEq)]
pub struct Address {
    pub info: AddressInfo,
    pub offsets: Vec<isize>,
}

impl Address {
    fn new() -> Self {
        Address {
            info: AddressInfo::Null,
            offsets: Vec::new(),
        }
    }

    fn add_offset(&mut self, offset: isize) {
        self.offsets.push(offset);
    }
}

#[derive(Copy, Clone)]
pub enum DefaultAddressing {
    Current,
    Full,
    None,
}

pub const DEFAULT_ADDRESSING: [DefaultAddressing; 256] = {
    let mut array = [DefaultAddressing::None; 256];

    array[b'd' as usize] = DefaultAddressing::Current; // 'd' -> Default to current line
    array[b'p' as usize] = DefaultAddressing::Current; // 'p' -> Default to current line
    array[b'w' as usize] = DefaultAddressing::Full; // 'w' -> Default to full range

    array
};

impl DefaultAddressing {
    pub fn get_range(&self) -> (AddressInfo, AddressInfo) {
        match self {
            DefaultAddressing::Current => (AddressInfo::Current, AddressInfo::Current),
            DefaultAddressing::Full => (AddressInfo::Line(1), AddressInfo::Last),
            DefaultAddressing::None => (AddressInfo::Null, AddressInfo::Null),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Command {
    Insert(Address, bool),
    // Change(String),
    // Copy(usize),
    Delete(Address, Address),
    // Global(String, String, bool, bool, bool),
    // GlobalNotMatched(String, Vec<Command>),
    // InteractiveGlobalNotMatched(String, Vec<Command>),
    // Move(isize),
    // NoOp,
    Print(PrintMode, Address, Address),
    // Read(String),
    Quit,
    Write(Address, Address, Option<String>, bool),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PrintMode {
    Normal,
    // List,
    // Numbered,
}

enum ParseState {
    Address,
    SepOffCommand,
    Command,
}

fn normalize_addrvec(
    av: &mut Vec<Address>,
    n_addr: usize,
    separator: Option<char>,
    default_addr: DefaultAddressing,
) {
    assert!(av.len() > 0);

    // Ensure the vector has the correct number of addresses
    while av.len() < n_addr {
        av.push(av[0].clone());
    }

    while av.len() > n_addr {
        av.remove(0);
    }

    // Patch Null addresses
    if (av.len() > 1) && (av[1].info == AddressInfo::Null) {
        if av[0].info == AddressInfo::Null {
            av[1].info = AddressInfo::Last;
        } else {
            av[1] = av[0].clone();
        }
    }
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
            _ => {
                panic!("unexpected separator");
            }
        }
    }
}

impl Command {
    fn parse(mut tokens: Vec<Token>) -> Result<Command, String> {
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
                        Token::Number(v) => addr.info = AddressInfo::Line(v as usize),
                        Token::Mark(ch) => addr.info = AddressInfo::Mark(ch),
                        Token::RegexForward(s) => addr.info = AddressInfo::RegexForward(s),
                        Token::RegexBack(s) => addr.info = AddressInfo::RegexBack(s),
                        Token::AddressSeparator(ch) => {
                            addrvec.push(addr);
                            addr = Address::new();
                            state = ParseState::Address;
                            separator = Some(ch);
                        }
                        Token::Offset(i) => addr.add_offset(i),
                        Token::Command(_) => {
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
                        }
                        Token::Offset(isize) => addr.add_offset(isize),
                        Token::Command(_) => {
                            tokens.insert(0, token);
                            state = ParseState::Command;
                        }
                        _ => return Err(String::from("unexpected token")),
                    }
                    if addr_dirty {
                        addrvec.push(addr);
                        addr = Address::new();
                        addr_dirty = false;
                    }
                }
                ParseState::Command => match token {
                    Token::Command('a') => {
                        normalize_addrvec(&mut addrvec, 1, separator, DefaultAddressing::Current);
                        return Ok(Command::Insert(addrvec[0].clone(), false));
                    }
                    Token::Command('d') => {
                        normalize_addrvec(&mut addrvec, 2, separator, DefaultAddressing::Current);
                        return Ok(Command::Delete(addrvec[0].clone(), addrvec[1].clone()));
                    }
                    Token::Command('i') => {
                        normalize_addrvec(&mut addrvec, 1, separator, DefaultAddressing::Current);
                        return Ok(Command::Insert(addrvec[0].clone(), true));
                    }
                    Token::Command('q') => {
                        if !addrvec.is_empty() {
                            return Err("quit command takes no address".to_string());
                        }
                        return Ok(Command::Quit);
                    }
                    Token::Command('p') => {
                        normalize_addrvec(&mut addrvec, 2, separator, DefaultAddressing::Current);
                        return Ok(Command::Print(
                            PrintMode::Normal,
                            addrvec[0].clone(),
                            addrvec[1].clone(),
                        ));
                    }
                    Token::Command('w') => {
                        normalize_addrvec(&mut addrvec, 2, separator, DefaultAddressing::Full);
                        return Ok(Command::Write(
                            addrvec[0].clone(),
                            addrvec[1].clone(),
                            None,
                            false,
                        ));
                    }
                    Token::Command(_) => return Err("unrecognized command".to_string()),
                    _ => return Err(String::from("unexpected token")),
                },
            }
        }

        if addr_dirty {
            addrvec.push(addr);
        }

        Err("address-and-command parse error".to_string())
    }

    pub fn from_line(line: &str) -> Result<Command, String> {
        match tokenizer(line) {
            Err(e) => Err(e.message),
            Ok(tokens) => Self::parse(tokens),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn append_basic() {
        let line = "a";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Insert(
                Address {
                    info: AddressInfo::Current,
                    offsets: Vec::new()
                },
                false
            )
        );

        let line = "2a";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Insert(
                Address {
                    info: AddressInfo::Line(2),
                    offsets: Vec::new(),
                },
                false
            )
        );
    }

    #[test]
    fn append_with_line_number() {
        let line = "2a";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Insert(
                Address {
                    info: AddressInfo::Line(2),
                    offsets: Vec::new(),
                },
                false
            )
        );
    }

    #[test]
    fn append_with_current_line() {
        let line = ".a";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Insert(
                Address {
                    info: AddressInfo::Current,
                    offsets: Vec::new(),
                },
                false
            )
        );
    }

    #[test]
    fn append_with_last_line() {
        let line = "$a";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Insert(
                Address {
                    info: AddressInfo::Last,
                    offsets: Vec::new(),
                },
                false
            )
        );
    }

    #[test]
    fn append_with_mark() {
        let line = "'xa";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Insert(
                Address {
                    info: AddressInfo::Mark('x'),
                    offsets: Vec::new(),
                },
                false
            )
        );
    }

    #[test]
    fn append_with_forward_regex() {
        let line = "/pattern/a";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Insert(
                Address {
                    info: AddressInfo::RegexForward("pattern".to_string()),
                    offsets: Vec::new(),
                },
                false
            )
        );
    }

    #[test]
    fn append_with_backward_regex() {
        let line = "?pattern?a";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Insert(
                Address {
                    info: AddressInfo::RegexBack("pattern".to_string()),
                    offsets: Vec::new(),
                },
                false
            )
        );
    }

    #[test]
    fn append_with_offset() {
        let line = "1+a";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Insert(
                Address {
                    info: AddressInfo::Line(1),
                    offsets: vec![1],
                },
                false
            )
        );

        let line = "1-2a";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Insert(
                Address {
                    info: AddressInfo::Line(1),
                    offsets: vec![-2],
                },
                false
            )
        );
    }

    #[test]
    fn print_with_current_line() {
        let line = ".p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::Current,
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Current,
                    offsets: Vec::new(),
                }
            )
        );
    }

    #[test]
    fn print_with_last_line() {
        let line = "$p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::Last,
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Last,
                    offsets: Vec::new(),
                }
            )
        );
    }

    #[test]
    fn print_with_line_number() {
        let line = "2p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::Line(2),
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Line(2),
                    offsets: Vec::new(),
                }
            )
        );
    }

    #[test]
    fn print_with_mark() {
        let line = "'xp";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::Mark('x'),
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Mark('x'),
                    offsets: Vec::new(),
                }
            )
        );
    }

    #[test]
    fn print_with_forward_regex() {
        let line = "/pattern/p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::RegexForward("pattern".to_string()),
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::RegexForward("pattern".to_string()),
                    offsets: Vec::new(),
                }
            )
        );
    }

    #[test]
    fn print_with_backward_regex() {
        let line = "?pattern?p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::RegexBack("pattern".to_string()),
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::RegexBack("pattern".to_string()),
                    offsets: Vec::new(),
                }
            )
        );
    }

    #[test]
    fn print_with_offset() {
        let line = "1+2p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::Line(1),
                    offsets: vec![2],
                },
                Address {
                    info: AddressInfo::Line(1),
                    offsets: vec![2],
                }
            )
        );

        let line = "1-2p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::Line(1),
                    offsets: vec![-2],
                },
                Address {
                    info: AddressInfo::Line(1),
                    offsets: vec![-2],
                }
            )
        );
    }

    #[test]
    fn print_with_address_range() {
        let line = "1,5p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::Line(1),
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Line(5),
                    offsets: Vec::new(),
                }
            )
        );

        let line = "1;5p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::Line(1),
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Line(5),
                    offsets: Vec::new(),
                }
            )
        );
    }

    #[test]
    fn print_with_many_addresses() {
        let line = "1,2,3,4,5p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::Line(4),
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Line(5),
                    offsets: Vec::new(),
                }
            )
        );
    }

    #[test]
    fn print_whole_file() {
        let line = ",p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::Line(1),
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Last,
                    offsets: Vec::new(),
                }
            )
        );

        let line = ";p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::Current,
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Last,
                    offsets: Vec::new(),
                }
            )
        );
    }

    #[test]
    fn print_implicit_addresses() {
        let line = ",222p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::Line(1),
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Line(222),
                    offsets: Vec::new(),
                }
            )
        );

        let line = ";222p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::Current,
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Line(222),
                    offsets: Vec::new(),
                }
            )
        );

        let line = "222,p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::Line(222),
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Line(222),
                    offsets: Vec::new(),
                }
            )
        );

        let line = "222;p";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Print(
                PrintMode::Normal,
                Address {
                    info: AddressInfo::Line(222),
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Line(222),
                    offsets: Vec::new(),
                }
            )
        );
    }

    #[test]
    fn delete_1() {
        let line = "d";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Delete(
                Address {
                    info: AddressInfo::Current,
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Current,
                    offsets: Vec::new(),
                },
            )
        );
    }

    #[test]
    fn delete_1d() {
        let line = "1d";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Delete(
                Address {
                    info: AddressInfo::Line(1),
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Line(1),
                    offsets: Vec::new(),
                }
            )
        );
    }

    #[test]
    fn delete_2() {
        let line = "2,d";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Delete(
                Address {
                    info: AddressInfo::Line(2),
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Line(2),
                    offsets: Vec::new(),
                }
            )
        );
    }

    #[test]
    fn delete_dotdollar() {
        let line = ".,$d";
        let cmd = Command::from_line(line).expect("parse error");
        assert_eq!(
            cmd,
            Command::Delete(
                Address {
                    info: AddressInfo::Current,
                    offsets: Vec::new(),
                },
                Address {
                    info: AddressInfo::Last,
                    offsets: Vec::new(),
                }
            )
        );
    }
}
