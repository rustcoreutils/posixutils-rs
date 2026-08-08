//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use byteorder::{BigEndian, ByteOrder, LittleEndian, NativeEndian, WriteBytesExt};
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::io::input_stream;
use plib::locale::mb_char_slices;
use posixutils_i18n::bytes::{trim, trim_end};
use std::{
    cell::RefCell,
    collections::BTreeMap,
    fmt::Display,
    fs::File,
    io::{self, Cursor, Read, Seek, Write},
    path::{Path, PathBuf},
    rc::Rc,
};

// Maximum set number. POSIX requires this to come from `<limits.h>`; both the
// POSIX minimum and the glibc/Linux and macOS values for `NL_SETMAX` are 255,
// so the constant matches the platform limit on every supported target.
const NL_SETMAX: u32 = 255;
// Maximum message number, per `<limits.h>` `NL_MSGMAX` (255 minimum in POSIX;
// 32767 on glibc/Linux and macOS).
const NL_MSGMAX: usize = 32767;
const NL_SETD: u32 = 1; // the default set number for the messages that are not in any set
const GLIBC_MAGIC: u32 = 0x960408de;

#[cfg(target_os = "macos")]
pub mod osx {
    pub const OSX_MAGIC: &[u8; 8] = b"*nazgul*";
    pub const OSX_MAJOR_VER: i32 = 1;
    pub const OSX_MINOR_VER: i32 = 0;
    pub const OSX_BYTE_ORDER: i32 = 0x01; // denotes BIG ENDIAN for now
    pub const OSX_NOT_INVALID_FLAG: i32 = 0;
    pub const FIRST_SET_OFFSET: i64 = 32;
}

#[derive(Parser)]
#[command(version, about = gettext("gencat - generate a formatted message catalog"))]
struct Args {
    #[arg(help = gettext("A pathname of the formatted message catalog"))]
    catfile: PathBuf,

    #[arg(required = true, help = gettext("A pathname of a message text source file"))]
    msgfile: Vec<PathBuf>,
}

/// In memory representation of a message
#[derive(Debug)]
pub struct Msg {
    /// The message id
    pub msg_id: usize,

    /// The message text.
    ///
    /// Bytes, not a `String`: POSIX has `LC_CTYPE` decide how the source file's
    /// bytes are interpreted, so a catalog may legitimately hold text in a
    /// codeset that is not UTF-8. Reading it as a `String` rejected such a file
    /// outright.
    pub msg: Vec<u8>,

    pub hconst: String,

    /// The offset of message within the buffer or file
    pub offset: i64,

    /// The next message in the set
    pub next: Option<Rc<RefCell<Msg>>>,

    /// The previous message in the set
    pub prev: Option<Rc<RefCell<Msg>>>,
}

/// In memory representation of a set
#[derive(Debug, Clone)]
pub struct Set {
    /// The set identifier
    pub set_id: u32,

    pub hconst: String,

    /// The first message in the set
    pub first_msg: Option<Rc<RefCell<Msg>>>,

    /// The last message in the set
    pub last_msg: Option<Rc<RefCell<Msg>>>,

    /// The next set in the catalog
    pub next: Option<Rc<RefCell<Set>>>,

    /// The previous set in the catalog
    pub prev: Option<Rc<RefCell<Set>>>,
}

impl Set {
    #[cfg(target_os = "macos")]
    fn get_msgs_count(&self) -> i32 {
        let mut count = 0;
        let mut current = self.first_msg.clone();

        while let Some(msg) = current {
            count += 1;
            current = (*msg).borrow().next.clone();
        }

        count
    }
}

/// In memory representation of a catalog
#[derive(Debug)]
pub struct Cat {
    /// The first set in the catalog
    pub first_set: Option<Rc<RefCell<Set>>>,

    /// The last set in the catalog
    pub last_set: Option<Rc<RefCell<Set>>>,
}

impl Cat {
    pub fn find_set(&self, set_id: u32) -> Option<Rc<RefCell<Set>>> {
        self.all_sets()
            .into_iter()
            .find(|set| set.borrow().set_id == set_id)
    }

    pub fn all_sets(&self) -> Vec<Rc<RefCell<Set>>> {
        let mut sets = Vec::new();
        let mut current_set = self.first_set.clone();
        while let Some(set) = current_set {
            sets.push(set.clone());
            current_set = set.borrow().next.clone();
        }
        sets
    }

    pub fn total_messages(&self) -> usize {
        self.all_sets()
            .iter()
            .map(|set| {
                let set = set.borrow();
                let mut count = 0;
                let mut current_msg = set.first_msg.clone();
                while let Some(msg) = current_msg {
                    count += 1;
                    current_msg = msg.borrow().next.clone();
                }
                count
            })
            .sum()
    }

    pub fn total_sets(&self) -> i32 {
        let mut count = 0;
        let mut current_set = self.first_set.clone();

        while let Some(set) = current_set {
            count += 1;
            current_set = (*set).borrow().next.clone();
        }

        count
    }

    pub fn delete_set(&mut self, set_id: u32) {
        let mut current = self.first_set.clone();
        let mut prev: Option<Rc<RefCell<Set>>> = None;

        while let Some(ref set) = current {
            let next = set.borrow().next.clone();
            if set.borrow().set_id == set_id {
                if let Some(prev) = prev.as_ref() {
                    prev.borrow_mut().next = next.clone();
                } else {
                    self.first_set = next.clone();
                }

                if set.borrow().set_id == self.last_set.as_ref().unwrap().borrow().set_id {
                    self.last_set = prev;
                }

                return;
            } else {
                prev = Some(set.clone());
                current = next;
            }
        }

        // if we don't find the set to delete, we'll simply ignore
    }
}

#[derive(Debug)]
pub struct MessageCatalog {
    pub cat: Cat,
}

/// Magic Header structure for the catalog file
#[cfg(target_os = "macos")]
#[derive(bytemuck::NoUninit, Clone, Copy)]
#[repr(C)]
struct CatFileMagicHeader {
    /// Magic cookie "*nazgul*"
    magic: [u8; 8],

    /// Currently : 1
    major_ver: i32,

    /// Currently : 0
    minor_ver: i32,

    /// Informational flags, currently used to set the endianess
    flags: i32,

    /// Number of valid Sets
    num_sets: i32,

    /// Offset of first set on disk
    first_set: i64,
}

#[derive(Debug)]
pub enum ParseError {
    NoSetNumber(usize, String),
    InvalidSetNumber(usize, u32),
    InvalidMsgNumber(usize, usize),
    SetNumberNotInAscending(usize, u32, u32),
    InvalidLine(usize, String),
    InvalidQuoteChar(usize, String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidLine(line_num, line_content) => {
                write!(f, "Invalid line {line_num} with content {line_content}")
            }
            ParseError::NoSetNumber(line_num, parse_int_error) => {
                write!(
                    f,
                    "No set number available at line {line_num} with content {parse_int_error}"
                )
            }
            ParseError::InvalidSetNumber(line_num, set_num) => {
                write!(f, "Invalid set number {set_num} at line {line_num}")
            }
            ParseError::InvalidMsgNumber(line_num, msg_num) => {
                write!(f, "Invalid message number {msg_num} at line {line_num}")
            }
            ParseError::SetNumberNotInAscending(line_num, prev_set_num, current_set_num) => {
                write!(
                    f,
                    "Set number {current_set_num} is not in ascending order after {prev_set_num} at line {line_num}"
                )
            }
            ParseError::InvalidQuoteChar(line_num, line_content) => {
                write!(
                    f,
                    "Invalid quote character at line {line_num} with content {line_content}"
                )
            }
        }
    }
}

impl std::error::Error for ParseError {}

/// Split `input` into lines on <newline>, discarding a trailing <carriage-return>
/// so a CRLF-terminated source parses the same as an LF-terminated one.
fn split_lines(input: &[u8]) -> Vec<&[u8]> {
    input
        .split(|&b| b == b'\n')
        .map(|line| line.strip_suffix(b"\r".as_slice()).unwrap_or(line))
        .collect()
}

/// Split `s` at the first <blank>, returning the field before it and everything
/// after that one separator. `None` for the second element means there was no
/// <blank> at all.
///
/// Exactly one byte is consumed, because 98956-98957 says "the fields of a
/// message text source line are separated by a single <blank> character. Any
/// other <blank> characters are considered to be part of the subsequent field."
/// Skipping the whole run instead silently left-trimmed every indented message
/// — menu and usage text, typically — out of the compiled catalog.
fn split_field(s: &[u8]) -> (&[u8], Option<&[u8]>) {
    match s.iter().position(|b| b.is_ascii_whitespace()) {
        None => (s, None),
        Some(i) => (&s[..i], Some(&s[i + 1..])),
    }
}

/// Parse an ASCII decimal field, for the grammar's numeric tokens.
fn parse_number<T: std::str::FromStr>(field: &[u8]) -> Result<T, ()> {
    std::str::from_utf8(field)
        .map_err(|_| ())?
        .parse()
        .map_err(|_| ())
}

/// Return `true` if `s` ends with an unescaped (odd count of) trailing
/// backslash, indicating a line continuation per the POSIX gencat grammar.
///
/// Byte-oriented: `\` is a single byte in every encoding POSIX admits, and a
/// trailing byte of a multibyte character can never be `0x5C` in a
/// POSIX-conforming codeset, so counting bytes is safe here.
fn ends_with_continuation(s: &[u8]) -> bool {
    s.iter().rev().take_while(|&&c| c == b'\\').count() % 2 == 1
}

/// Process the C-style escape sequences defined for gencat message text:
/// `\n \t \v \b \r \f \\` and `\ddd` (1-3 octal digits). A backslash
/// followed by any other character is discarded, leaving that character
/// (per "the <backslash> shall be ignored"). A trailing backslash with no
/// following character is dropped.
///
/// Byte-oriented, so a message may hold text in any encoding the current
/// `LC_CTYPE` describes: bytes that are not part of an escape are copied
/// through untouched, and `\ddd` names a byte, not a code point. Scanning is
/// per character (`mb_char_slices`) so a multibyte character whose trailing
/// byte happens to be `0x5C` cannot be mistaken for a backslash.
fn process_escapes(s: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(s.len());
    let chars = mb_char_slices(s);
    let mut i = 0;
    while i < chars.len() {
        let ch = chars[i];
        i += 1;
        if ch != b"\\" {
            out.extend_from_slice(ch);
            continue;
        }
        let Some(&next) = chars.get(i) else {
            // A trailing backslash with no following character is dropped.
            break;
        };
        i += 1;
        match next {
            b"n" => out.push(b'\n'),
            b"t" => out.push(b'\t'),
            b"v" => out.push(0x0b),
            b"b" => out.push(0x08),
            b"r" => out.push(b'\r'),
            b"f" => out.push(0x0c),
            b"\\" => out.push(b'\\'),
            [d @ b'0'..=b'7'] => {
                let mut val = u32::from(d - b'0');
                for _ in 0..2 {
                    match chars.get(i) {
                        Some([n @ b'0'..=b'7']) => {
                            val = val * 8 + u32::from(n - b'0');
                            i += 1;
                        }
                        _ => break,
                    }
                }
                out.push(val as u8);
            }
            // "the <backslash> shall be ignored", leaving the character.
            other => out.extend_from_slice(other),
        }
    }
    out
}

/// For set if it's $set NUMBER #COMMENT
impl MessageCatalog {
    pub fn new(
        #[cfg_attr(target_os = "macos", allow(unused_variables))] build_default: bool,
    ) -> Self {
        let message_catalog = MessageCatalog {
            cat: Cat {
                first_set: None,
                last_set: None,
            },
        };

        let message_catalog_to_use: MessageCatalog = {
            #[cfg(target_os = "macos")]
            {
                message_catalog
            }

            #[cfg(not(target_os = "macos"))]
            {
                if build_default {
                    let mut message_catalog_mut = message_catalog;

                    message_catalog_mut.add_set(NL_SETD, String::from("Default Set"));

                    message_catalog_mut
                } else {
                    message_catalog
                }
            }
        };

        message_catalog_to_use
    }

    /// Parse the message file and override the catalog file(if it already exists)
    pub fn parse(
        input_path: &Path,
        catfile_catalog: Option<MessageCatalog>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let mut file = input_stream(input_path, true)?;

        let mut input = Vec::new();
        file.read_to_end(&mut input)?;

        Self::parse_bytes(&input, catfile_catalog)
    }

    /// Parse message-text source from an in-memory buffer. Split out from
    /// [`parse`](Self::parse) so the grammar can be unit-tested directly.
    ///
    /// Takes bytes rather than a `&str`: the grammar's own tokens (`$set`,
    /// `$quote`, digits, <blank>) are ASCII, but the message text is whatever
    /// the current `LC_CTYPE` says it is, so it must survive verbatim.
    pub fn parse_bytes(
        input: &[u8],
        catfile_catalog: Option<MessageCatalog>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let mut catalog = match catfile_catalog {
            Some(catalog) => catalog,
            None => MessageCatalog::new(true),
        };

        let mut current_set = catalog.cat.first_set.clone();
        let mut last_set_id = NL_SETD;

        // $quote CHAR:
        // regarding it, hmm..we can have multiple quotes but only the latest encountered quote character will be used
        // btw this behavour was observed in the GNU implementation that's why we are settling for it
        //
        // Eg
        // $quote "
        // $quote '
        //
        // Here, the quote character will be ' and not " as it was the last encountered quote character
        // also if empty(i.e no character), then it will be unset
        //
        // i guess this will be useful when we have some qutoe character and sets and then we have another quote character
        // and sets
        let mut quote_char: Option<&[u8]> = None;

        let lines: Vec<&[u8]> = split_lines(input);
        let mut line_num = 0;
        while line_num < lines.len() {
            let line = trim(lines[line_num]);

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with(b"$ ") {
                line_num += 1;
                continue;
            }

            if line.starts_with(b"$quote") {
                if let Some(rem) = line.strip_prefix(b"$quote".as_slice()) {
                    let c = trim(rem);
                    if c.is_empty() {
                        // unset quote character
                        quote_char = None;
                    } else if mb_char_slices(c).len() == 1 {
                        // A single *character* under the current LC_CTYPE, which
                        // may be more than one byte.
                        quote_char = Some(c);
                    } else {
                        return Err(Box::new(ParseError::InvalidQuoteChar(
                            line_num + 1,
                            String::from_utf8_lossy(line).into_owned(),
                        )));
                    }
                }
            } else if line.starts_with(b"$delset") {
                // the gnu implementation seems to just remove those sets from the "array" (both - little endian and big endian)
                // and it preserves those datain the string pool too(weird, they(GNU devs) might need to improve on that)
                // but we are going to remove the set from the catalog itself(check the hexdump btw for $delset)
                //
                // The question remains, are we diverting from the specification?
                // Hmm..no coz the specification doesn't mention anything about the implementation details(it was GNU's choice to do that)
                // and the GNU implementation is just one of the many ways to implement it, and the catopen will interpret it the sameway
                if let Some(rem) = line.strip_prefix(b"$delset".as_slice()) {
                    // Only the first token is the set number; any remaining text
                    // on the line is a comment.
                    let (first, _) = split_field(trim(rem));
                    let set_id = parse_number::<u32>(first).map_err(|_| {
                        Box::new(ParseError::InvalidLine(
                            line_num + 1,
                            String::from_utf8_lossy(line).into_owned(),
                        ))
                    })?;

                    if set_id == 0 || set_id > NL_SETMAX {
                        return Err(Box::new(ParseError::InvalidSetNumber(line_num, set_id)));
                    }

                    catalog.cat.delete_set(set_id);
                }
            } else if line.starts_with(b"$set") {
                if let Some(rem) = line.strip_prefix(b"$set".as_slice()) {
                    let (number, comment) = split_field(trim(rem));

                    if number.is_empty() {
                        return Err(Box::new(ParseError::NoSetNumber(
                            line_num,
                            String::from_utf8_lossy(line).into_owned(),
                        )));
                    }

                    let set_id = parse_number::<u32>(number).map_err(|_| {
                        Box::new(ParseError::InvalidLine(
                            line_num + 1,
                            String::from_utf8_lossy(line).into_owned(),
                        ))
                    })?;

                    if set_id == 0 || set_id > NL_SETMAX {
                        return Err(Box::new(ParseError::InvalidSetNumber(line_num, set_id)));
                    }

                    if set_id < last_set_id {
                        return Err(Box::new(ParseError::SetNumberNotInAscending(
                            line_num + 1,
                            last_set_id,
                            set_id,
                        )));
                    }

                    last_set_id = set_id;

                    if let Some(existing_set) = catalog.cat.find_set(set_id) {
                        // we'll reuse the same set if we collide
                        current_set = Some(existing_set);
                    } else {
                        let comment = comment
                            .map(|c| String::from_utf8_lossy(c).into_owned())
                            .unwrap_or_default();
                        current_set = Some(catalog.add_set(set_id, comment));
                    }
                }
            } else {
                let (number, rest) = split_field(line);
                // The message-number field must be a number; anything else is
                // an invalid line.
                let msg_id = match parse_number::<usize>(number) {
                    Ok(n) => n,
                    Err(_) => {
                        return Err(Box::new(ParseError::InvalidLine(
                            line_num + 1,
                            String::from_utf8_lossy(line).into_owned(),
                        )));
                    }
                };

                // Message numbers must be within [1, NL_MSGMAX]. (Ascending
                // order within a set is advisory only — the POSIX merge rule
                // explicitly permits a repeated number to replace an earlier
                // message, so strict ordering is not enforced.)
                if msg_id == 0 || msg_id > NL_MSGMAX {
                    return Err(Box::new(ParseError::InvalidMsgNumber(line_num + 1, msg_id)));
                }

                let Some(rest) = rest else {
                    // A message-number with no separating <blank> and no text
                    // deletes the message from the current set.
                    if let Some(set) = current_set.as_ref() {
                        catalog.delete_msg(set, msg_id);
                    }
                    line_num += 1;
                    continue;
                };

                // Assemble the message text, honoring trailing-backslash line
                // continuation (the backslash is discarded and the following
                // physical line is appended without an intervening newline).
                let mut text = rest.to_vec();
                while ends_with_continuation(&text) {
                    text.pop();
                    line_num += 1;
                    match lines.get(line_num) {
                        Some(next) => text.extend_from_slice(trim_end(next)),
                        None => break,
                    }
                }

                // Strip a surrounding pair of quote characters, if set.
                if let Some(q) = quote_char {
                    if text.len() >= 2 * q.len() && text.starts_with(q) && text.ends_with(q) {
                        text = text[q.len()..text.len() - q.len()].to_vec();
                    }
                }

                let text = process_escapes(&text);
                catalog.add_msg(current_set.as_ref().unwrap(), msg_id, text);
            }
            line_num += 1;
        }

        Ok(catalog)
    }

    fn add_set(&mut self, set_id: u32, hconst: String) -> Rc<RefCell<Set>> {
        let new_set = Rc::new(RefCell::new(Set {
            set_id,
            hconst,
            first_msg: None,
            last_msg: None,
            next: None,
            prev: None,
        }));

        match self.cat.last_set.take() {
            Some(last_set) => {
                last_set.borrow_mut().next = Some(Rc::clone(&new_set));
                new_set.borrow_mut().prev = Some(Rc::clone(&last_set));
                self.cat.last_set = Some(Rc::clone(&new_set));
            }
            None => {
                self.cat.first_set = Some(Rc::clone(&new_set));
                self.cat.last_set = Some(Rc::clone(&new_set));
            }
        }

        new_set
    }

    fn add_msg(&self, set: &Rc<RefCell<Set>>, msg_id: usize, msg: Vec<u8>) {
        // If a message with this id already exists in the set, the new text
        // replaces the old per the POSIX merge rule (rather than appending a
        // duplicate).
        {
            let mut current = set.borrow().first_msg.clone();
            while let Some(existing) = current {
                if existing.borrow().msg_id == msg_id {
                    existing.borrow_mut().msg = msg;
                    return;
                }
                current = existing.borrow().next.clone();
            }
        }

        let new_msg = Rc::new(RefCell::new(Msg {
            msg_id,
            msg,
            hconst: String::new(),
            offset: 0,
            next: None,
            prev: None,
        }));

        let mut set = set.borrow_mut();
        match set.last_msg.take() {
            Some(last_msg) => {
                last_msg.borrow_mut().next = Some(Rc::clone(&new_msg));
                new_msg.borrow_mut().prev = Some(last_msg);
                set.last_msg = Some(Rc::clone(&new_msg));
            }
            None => {
                set.first_msg = Some(Rc::clone(&new_msg));
                set.last_msg = Some(Rc::clone(&new_msg));
            }
        }
    }

    /// Remove the message with `msg_id` from `set`, if present (the
    /// delete-by-number form of a message-text source line).
    fn delete_msg(&self, set: &Rc<RefCell<Set>>, msg_id: usize) {
        let mut set = set.borrow_mut();
        let mut current = set.first_msg.clone();
        let mut prev: Option<Rc<RefCell<Msg>>> = None;

        while let Some(msg) = current {
            let next = msg.borrow().next.clone();
            if msg.borrow().msg_id == msg_id {
                match prev.as_ref() {
                    Some(p) => p.borrow_mut().next = next.clone(),
                    None => set.first_msg = next.clone(),
                }
                if let Some(n) = next.as_ref() {
                    n.borrow_mut().prev = prev.clone();
                } else {
                    // deleted the tail
                    set.last_msg = prev.clone();
                }
                return;
            }
            prev = Some(msg);
            current = next;
        }
    }

    /// Slot for a message in the catalog's open-addressed array.
    ///
    /// Shared by the sizing pass and the fill pass so the two cannot disagree about
    /// where a message lands — which is what let the fill probe run off the end of
    /// an array sized by the other hash.
    #[cfg(not(target_os = "macos"))]
    fn catalog_hash(set_id: u32, msg_id: usize, size: usize) -> usize {
        ((set_id + 1) as usize * msg_id) % size
    }

    #[cfg(not(target_os = "macos"))]
    fn compute_optimal_size(&self) -> (usize, usize) {
        let mut best_total = usize::MAX;
        let mut best_size = usize::MAX;
        let mut best_depth = usize::MAX;

        // starting with a size that would give an average depth of 5
        let mut act_size = 1 + self.cat.total_messages() / 5;

        while act_size <= best_total {
            let mut deep = vec![0; act_size];
            let mut act_depth = 1;

            for set in self.cat.all_sets() {
                let set = set.borrow();
                let mut current_msg = set.first_msg.clone();

                while let Some(msg) = current_msg {
                    let msg = msg.borrow();
                    // Must be the *same* hash `fill_arrays` uses. It was
                    // `msg_id * set_id` here and `msg_id * (set_id + 1)` there,
                    // so the depth measured while sizing did not describe the
                    // collisions that actually occurred, and the array — sized
                    // `best_size * best_depth * 3` — was overrun by the probe.
                    // `$set 1` with messages 1 and 2 was enough: both hash to
                    // slot 0 at `best_size` 2, giving depth 2 against an array
                    // sized for depth 1.
                    let idx = Self::catalog_hash(set.set_id, msg.msg_id, act_size);
                    deep[idx] += 1;

                    if deep[idx] > act_depth {
                        act_depth = deep[idx];

                        if act_depth * act_size > best_total {
                            break;
                        }
                    }
                    current_msg = msg.next.clone();
                }
            }

            if act_depth * act_size <= best_total {
                best_total = act_depth * act_size;
                best_size = act_size;
                best_depth = act_depth;
            }

            act_size += 1;
        }

        if best_size == usize::MAX {
            best_size = 1;
            best_depth = 1;
        }

        (best_size, best_depth)
    }

    #[cfg(not(target_os = "macos"))]
    fn fill_arrays(&self, array: &mut [u32], string_pool: &mut Vec<u8>, best_size: usize) {
        for set in self.cat.all_sets().iter().rev() {
            let set = set.borrow();
            let mut current_msg = set.first_msg.clone();

            while let Some(msg) = current_msg {
                let msg = msg.borrow();
                let mut idx = Self::catalog_hash(set.set_id, msg.msg_id, best_size) * 3;

                while array[idx] != 0 {
                    idx += best_size * 3;
                }

                array[idx] = set.set_id + 1;
                array[idx + 1] = msg.msg_id as u32;
                array[idx + 2] = string_pool.len() as u32;

                // add the message to the string pool
                string_pool.extend_from_slice(&msg.msg);
                string_pool.push(0); // Null terminator

                current_msg = msg.next.clone();
            }
        }
    }

    /// Read the **GNU based** binary catalog file and build [MessageCatalog]
    #[allow(clippy::needless_range_loop)]
    pub fn read_catfile<T: Read>(
        mut input: T,
    ) -> Result<MessageCatalog, Box<dyn std::error::Error>> {
        let mut catalog = MessageCatalog::new(false);
        let mut set_msg: BTreeMap<u32, BTreeMap<u32, Vec<u8>>> = BTreeMap::new();

        let mut buf: Vec<u8> = Vec::new();
        input.read_to_end(&mut buf)?;
        let mut ptr = 0;

        // The header is three u32 words (magic, plane size, plane depth);
        // a shorter file cannot be a valid catalog.
        if buf.len() < 12 {
            return Err(Box::new(io::Error::new(
                io::ErrorKind::InvalidData,
                gettext("gencat: existing catalog file is truncated or not a message catalog"),
            )));
        }

        let header = NativeEndian::read_u32(&buf[ptr..(ptr + 4)]);
        ptr += 4;

        if header != GLIBC_MAGIC {
            return Err(Box::new(io::Error::new(
                io::ErrorKind::InvalidData,
                gettext("gencat: existing catalog file has an invalid magic number"),
            )));
        }

        let plane_size = NativeEndian::read_u32(&buf[ptr..(ptr + 4)]);
        ptr += 4;

        let plane_depth = NativeEndian::read_u32(&buf[ptr..(ptr + 4)]);
        ptr += 4;

        // little-endian array
        let array_size = (plane_size * plane_depth * 3) as usize;
        let mut le_array = vec![0u32; array_size];
        for i in 0..array_size {
            le_array[i] = LittleEndian::read_u32(&buf[ptr..(ptr + 4)]);
            ptr += 4;
        }

        let mut be_array = vec![0u32; array_size];
        for i in 0..array_size {
            be_array[i] = BigEndian::read_u32(&buf[ptr..(ptr + 4)]);
            ptr += 4;
        }

        // TODO: probably compare be_array and le_array as they should be the same

        let string_pool = &buf[ptr..];

        // Process the array to reconstruct sets and messages
        for i in (0..array_size).step_by(3) {
            if le_array[i] != 0 {
                // as we already had incremented the set_id by 1 while inserting
                // we will be now reducing it by 1 for now
                let set_id = le_array[i] - 1;
                let msg_id = le_array[i + 1];
                let string_offset = le_array[i + 2] as usize;

                let msg_end = string_pool[string_offset..]
                    .iter()
                    .position(|&c| c == 0)
                    .unwrap_or(string_pool.len() - string_offset)
                    + string_offset;

                // Kept as bytes: an existing catalog may hold text in any
                // codeset, and re-encoding it through a lossy UTF-8 conversion
                // would corrupt it on merge.
                let msg = string_pool[string_offset..msg_end].to_vec();
                set_msg.entry(set_id).or_default().insert(msg_id, msg);
            }
        }

        for (set_id, messages) in set_msg.iter() {
            let set = catalog.add_set(*set_id, String::new());
            for (msg_id, msg) in messages.iter() {
                catalog.add_msg(&set, *msg_id as usize, msg.clone());
            }
        }

        Ok(catalog)
    }

    /// Write to the cat file **for GNU only**
    #[cfg(not(target_os = "macos"))]
    pub fn write_catfile<T: Write + Seek>(
        &self,
        file: &mut T,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let (best_size, best_depth) = self.compute_optimal_size();

        file.write_u32::<NativeEndian>(GLIBC_MAGIC)?;
        file.write_u32::<NativeEndian>(best_size as u32)?;
        file.write_u32::<NativeEndian>(best_depth as u32)?;

        let array_size: u32 = (best_size * best_depth * 3) as u32;
        let mut array = vec![0u32; array_size as usize];
        let mut string_pool = Vec::new();

        self.fill_arrays(&mut array, &mut string_pool, best_size);

        // little-endian array
        for &value in &array {
            file.write_u32::<LittleEndian>(value)?;
        }

        // big-endian array
        for &value in &array {
            file.write_u32::<BigEndian>(value)?;
        }

        // Write string pool
        file.write_all(&string_pool)?;

        Ok(())
    }

    /// Write to the cat file **for OSX only**
    #[cfg(target_os = "macos")]
    pub fn write_catfile<T: Write + Seek>(
        &self,
        file: &mut T,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let header = CatFileMagicHeader {
            magic: *osx::OSX_MAGIC,
            major_ver: osx::OSX_MAJOR_VER.to_be(),
            minor_ver: osx::OSX_MINOR_VER.to_be(),
            flags: osx::OSX_BYTE_ORDER.to_be(),
            num_sets: self.cat.total_sets().to_be(),
            // for now we have set it to 32, but we will change this later on as we lay the first set details
            first_set: osx::FIRST_SET_OFFSET.to_be(),
        };

        file.write_all(bytemuck::bytes_of(&header))?;

        // the position of the first_set value within the file that we need to change later on
        let first_set_pos = file.stream_position()? - 8;

        //file.write_u64::<BigEndian>(0)?;
        let cat = &self.cat;
        let mut current_set = self.cat.first_set.clone();

        while let Some(set) = current_set {
            let set = (*set).borrow();
            let set_pos = file.stream_position()?;

            if set.set_id == 1 {
                file.seek(io::SeekFrom::Start(first_set_pos))?;
                file.write_u64::<BigEndian>(set_pos)?;
                file.seek(io::SeekFrom::Start(set_pos))?;
            }

            file.write_u32::<BigEndian>(set.set_id)?;

            let next_set_offset = file.stream_position()?;
            file.write_i64::<BigEndian>(0)?; // just a placeholder(hmm, but might use it if there's no next set)

            let first_msg_offset_pos = file.stream_position()?;
            file.write_i64::<BigEndian>(0)?; // just a placeholder

            let data_offset_pos = file.stream_position()?;
            file.write_i64::<BigEndian>(0)?; // just a placeholder

            let data_length_pos = file.stream_position()?;
            file.write_i32::<BigEndian>(0)?; // just a placeholder

            let num_msgs = set.get_msgs_count();
            file.write_i32::<BigEndian>(num_msgs)?;
            file.write_i32::<BigEndian>(osx::OSX_NOT_INVALID_FLAG)?;

            // We'll write the string data now
            let data_offset = file.stream_position()?;
            let mut data_length = 0;
            let mut current_msg = set.first_msg.clone();

            while let Some(msg) = current_msg {
                let mut msg = msg.borrow_mut();
                let msg_offset = file.stream_position()? - data_offset;
                msg.offset = msg_offset as i64;
                file.write_all(&msg.msg)?;

                // null terminator
                file.write_u8(0)?;
                data_length += msg.msg.len() as i32 + 1;
                current_msg = msg.next.clone();
            }

            let first_msg_offset = file.stream_position()?;
            let mut current_msg = set.first_msg.clone();
            while let Some(msg) = current_msg {
                let msg = (*msg).borrow();

                file.write_i32::<BigEndian>(msg.msg_id as i32)?;
                file.write_i64::<BigEndian>(msg.offset)?;
                file.write_i32::<BigEndian>(osx::OSX_NOT_INVALID_FLAG)?;

                current_msg = msg.next.clone();
            }

            let current_pos = file.stream_position()?;

            // go back and write first msg offset
            file.seek(io::SeekFrom::Start(first_msg_offset_pos))?;
            file.write_i64::<BigEndian>(first_msg_offset as i64)?;

            // go back and write data offset
            file.seek(io::SeekFrom::Start(data_offset_pos))?;
            file.write_i64::<BigEndian>(data_offset as i64)?;

            // go back and write data length
            file.seek(io::SeekFrom::Start(data_length_pos))?;
            file.write_i32::<BigEndian>(data_length)?;

            let last_set = cat.last_set.as_ref().unwrap();
            let last_set = (**last_set).borrow();

            // if not last then we need to write the next set offset
            if set.set_id != last_set.set_id {
                // go back and write next set offset
                file.seek(io::SeekFrom::Start(next_set_offset))?;
                file.write_i64::<BigEndian>(current_pos as i64)?;
            }

            file.seek(io::SeekFrom::Start(current_pos))?;

            current_set = set.next.clone();
        }
        Ok(())
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    // the parsed catalog file, if it exists(which surely doesn't if we provide "-" a stdout)
    // and after we provide a file path, we cannot read the catalog file if the file doesn't exist
    let mut catfile_catalog: Option<MessageCatalog> = None;
    let catfile_path_str = args.catfile.as_os_str();

    if catfile_path_str != "-" && args.catfile.exists() {
        let catfile_catalog_file = File::open(&args.catfile)?;
        match MessageCatalog::read_catfile(catfile_catalog_file) {
            Ok(catalog) => catfile_catalog = Some(catalog),
            Err(err) => {
                eprintln!("Error: {err}");
                std::process::exit(1);
            }
        }
    }

    // Merge each msgfile, in command-line order, into the catalog.
    let mut catalog_acc: Option<MessageCatalog> = catfile_catalog;
    let mut parse_failed = false;
    for msgfile in &args.msgfile {
        match MessageCatalog::parse(msgfile, catalog_acc.take()) {
            Ok(catalog) => catalog_acc = Some(catalog),
            Err(err) => {
                exit_code = 1;
                parse_failed = true;
                eprintln!("Error: {err}");
                break;
            }
        }
    }

    if !parse_failed {
        if let Some(catalog) = catalog_acc {
            let mut buffer = Cursor::new(Vec::new());
            catalog.write_catfile(&mut buffer)?;

            if catfile_path_str == "-" {
                io::stdout().write_all(buffer.get_ref())?;
            } else {
                let mut file = File::create(&args.catfile)?;
                file.write_all(buffer.get_ref())?;
            }
        }
    }

    std::process::exit(exit_code)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn count_msgs(cat: &MessageCatalog, set_id: u32) -> usize {
        let Some(set) = cat.cat.find_set(set_id) else {
            return 0;
        };
        let mut n = 0;
        let mut cur = set.borrow().first_msg.clone();
        while let Some(m) = cur {
            n += 1;
            cur = m.borrow().next.clone();
        }
        n
    }

    /// The stored bytes for a message, or `None` if it is absent.
    fn msg_bytes(cat: &MessageCatalog, set_id: u32, msg_id: usize) -> Option<Vec<u8>> {
        let set = cat.cat.find_set(set_id)?;
        let mut cur = set.borrow().first_msg.clone();
        while let Some(m) = cur {
            if m.borrow().msg_id == msg_id {
                return Some(m.borrow().msg.clone());
            }
            cur = m.borrow().next.clone();
        }
        None
    }

    /// The stored text, for assertions over messages that are valid UTF-8.
    fn msg_text(cat: &MessageCatalog, set_id: u32, msg_id: usize) -> Option<String> {
        msg_bytes(cat, set_id, msg_id).map(|b| String::from_utf8(b).expect("utf-8 message"))
    }

    #[test]
    fn process_escapes_handles_c_sequences() {
        assert_eq!(process_escapes(br"Hello\tWorld"), b"Hello\tWorld");
        assert_eq!(process_escapes(br"a\nb\r"), b"a\nb\r");
        assert_eq!(process_escapes(br"\\"), b"\\");
        assert_eq!(process_escapes(br"\101"), b"A"); // octal 101 == 'A'
        assert_eq!(process_escapes(br"\1"), b"\x01");
        assert_eq!(process_escapes(br"\q"), b"q"); // unknown escape: backslash dropped
    }

    #[test]
    fn ends_with_continuation_counts_backslashes() {
        assert!(ends_with_continuation(br"abc\"));
        assert!(!ends_with_continuation(br"abc\\"));
        assert!(ends_with_continuation(br"abc\\\"));
        assert!(!ends_with_continuation(b"abc"));
    }

    #[test]
    fn gc1_message_escapes_expanded_in_catalog() {
        let cat = MessageCatalog::parse_bytes(b"$set 1\n1 Hello\\tWorld\n", None).unwrap();
        assert_eq!(msg_text(&cat, 1, 1).as_deref(), Some("Hello\tWorld"));
    }

    #[test]
    fn gc3_line_continuation_joins_text() {
        let cat = MessageCatalog::parse_bytes(b"$set 1\n1 Hello\\\nWorld\n", None).unwrap();
        assert_eq!(msg_text(&cat, 1, 1).as_deref(), Some("HelloWorld"));
    }

    #[test]
    fn gc4_colliding_msgid_is_replaced_not_appended() {
        let cat = MessageCatalog::parse_bytes(b"$set 1\n1 First\n1 Second\n", None).unwrap();
        assert_eq!(msg_text(&cat, 1, 1).as_deref(), Some("Second"));
        assert_eq!(count_msgs(&cat, 1), 1);
    }

    #[test]
    fn gc6_bare_number_deletes_message() {
        let cat = MessageCatalog::parse_bytes(b"$set 1\n1 Hello\n2 Bye\n1\n", None).unwrap();
        assert_eq!(msg_text(&cat, 1, 1), None);
        assert_eq!(msg_text(&cat, 1, 2).as_deref(), Some("Bye"));
        assert_eq!(count_msgs(&cat, 1), 1);
    }

    #[test]
    fn gc5_multiple_msgfiles_merge_in_order() {
        // main() threads the accumulated catalog through successive parses;
        // emulate that here.
        let first = MessageCatalog::parse_bytes(b"$set 1\n1 First\n", None).unwrap();
        let merged = MessageCatalog::parse_bytes(b"$set 2\n1 Second\n", Some(first)).unwrap();
        assert_eq!(msg_text(&merged, 1, 1).as_deref(), Some("First"));
        assert_eq!(msg_text(&merged, 2, 1).as_deref(), Some("Second"));
    }

    #[test]
    fn gc7_delset_allows_trailing_comment_and_validates_range() {
        let cat = MessageCatalog::parse_bytes(b"$set 1\n1 X\n$delset 1 this is a comment\n", None)
            .unwrap();
        assert!(cat.cat.find_set(1).is_none());
        assert!(MessageCatalog::parse_bytes(b"$delset 0\n", None).is_err());
        assert!(MessageCatalog::parse_bytes(b"$delset 99999\n", None).is_err());
    }

    #[test]
    fn gc8_message_number_range_enforced() {
        assert!(MessageCatalog::parse_bytes(b"$set 1\n0 X\n", None).is_err());
        assert!(MessageCatalog::parse_bytes(b"$set 1\n99999 X\n", None).is_err());
        assert!(MessageCatalog::parse_bytes(b"$set 1\n1 X\n", None).is_ok());
    }

    #[test]
    fn gc2_bad_magic_is_error_not_panic() {
        let data = b"not a valid catalog file at all";
        assert!(MessageCatalog::read_catfile(&data[..]).is_err());
        // Truncated input must also error rather than panic on slicing.
        let short = b"\x00\x00";
        assert!(MessageCatalog::read_catfile(&short[..]).is_err());
    }
}
