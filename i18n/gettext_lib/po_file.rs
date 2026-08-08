//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! .po (Portable Object) file parser
//!
//! The .po file format is a human-readable format for translated message catalogs.
//! This module provides functionality for parsing .po files.
//!
//! PO file format:
//! - Comments: lines starting with #
//!   - #  - translator comments
//!   - #. - extracted comments
//!   - #: - reference (file:line)
//!   - #, - flags (fuzzy, c-format, etc.)
//!   - #| - previous msgid
//! - msgctxt "context" - message context (optional)
//! - msgid "original" - original string
//! - msgid_plural "plural" - plural original (optional)
//! - msgstr "translation" - translation (for singular)
//! - msgstr[N] "translation" - plural translations

use crate::bytes::trim;
use std::io::{BufRead, BufReader, Read};

/// A single message entry from a .po file
#[derive(Debug, Clone, Default)]
pub struct PoEntry {
    /// Translator comments (# ...)
    pub translator_comments: Vec<String>,
    /// Extracted comments (#. ...)
    pub extracted_comments: Vec<String>,
    /// Reference comments (#: file:line)
    pub reference_comments: Vec<String>,
    /// Flags (#, fuzzy, c-format)
    pub flags: Vec<String>,
    /// Previous msgid (#| msgid "...")
    pub previous_msgid: Option<Vec<u8>>,
    /// Message context
    pub msgctxt: Option<Vec<u8>>,
    /// Original string (msgid)
    ///
    /// The message strings are bytes, not `String`s: POSIX has `LC_CTYPE`
    /// decide how the source file's bytes are interpreted, so a `.po` may
    /// legitimately be in a codeset that is not UTF-8. Reading it as a `String`
    /// rejected such a file outright, and `\xNN`/`\ooo` escapes above 0x7F were
    /// UTF-8 encoded into two bytes on the way into the `.mo`.
    pub msgid: Vec<u8>,
    /// Plural original (msgid_plural)
    pub msgid_plural: Option<Vec<u8>>,
    /// Translations (msgstr or msgstr[N])
    /// For singular: single element
    /// For plural: multiple elements indexed by plural form
    pub msgstr: Vec<Vec<u8>>,
    /// Whether this entry is marked as fuzzy
    pub is_fuzzy: bool,
    /// Whether this entry is obsolete (#~ ...)
    pub is_obsolete: bool,
    /// The domain this entry belongs to, set by a preceding `domain` directive.
    /// `None` means the default domain (`messages`).
    pub domain: Option<String>,
}

impl PoEntry {
    /// Check if this is the header entry (empty msgid)
    pub fn is_header(&self) -> bool {
        self.msgid.is_empty() && self.msgctxt.is_none()
    }

    /// Check if this is a plural entry
    pub fn is_plural(&self) -> bool {
        self.msgid_plural.is_some()
    }
}

/// Parsed .po file
#[derive(Debug, Clone, Default)]
pub struct PoFile {
    /// Header entry (msgid = "")
    pub header: Option<PoEntry>,
    /// Message entries
    pub entries: Vec<PoEntry>,
    /// Domain (from domain directive, if present)
    pub domain: Option<String>,
}

/// Error type for .po file parsing
#[derive(Debug)]
pub enum PoError {
    /// I/O error
    Io(std::io::Error),
    /// Parse error with line number
    Parse(usize, String),
    /// Invalid escape sequence
    InvalidEscape(usize, char),
    /// Unterminated string
    UnterminatedString(usize),
}

impl std::fmt::Display for PoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PoError::Io(e) => write!(f, "I/O error: {}", e),
            PoError::Parse(line, msg) => write!(f, "line {}: {}", line, msg),
            PoError::InvalidEscape(line, c) => write!(f, "line {}: invalid escape \\{}", line, c),
            PoError::UnterminatedString(line) => write!(f, "line {}: unterminated string", line),
        }
    }
}

impl std::error::Error for PoError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            PoError::Io(e) => Some(e),
            _ => None,
        }
    }
}

impl From<std::io::Error> for PoError {
    fn from(e: std::io::Error) -> Self {
        PoError::Io(e)
    }
}

/// Does a line start the `domain domainname` directive (rather than, e.g., a
/// `msgid` that merely begins with "domain")?
fn is_domain_directive(line: &[u8]) -> bool {
    if let Some(rest) = line.strip_prefix(b"domain".as_slice()) {
        rest.is_empty() || matches!(rest.first(), Some(b' ' | b'\t' | b'"'))
    } else {
        false
    }
}

/// Parser for .po files
pub struct PoParser<R> {
    reader: BufReader<R>,
    line_number: usize,
    current_line: Vec<u8>,
    peeked: Option<Vec<u8>>,
    /// Domain set by the most recent `domain` directive (`None` = default).
    current_domain: Option<String>,
}

impl<R: Read> PoParser<R> {
    /// Create a new parser from a reader
    pub fn new(reader: R) -> Self {
        PoParser {
            reader: BufReader::new(reader),
            line_number: 0,
            current_line: Vec::new(),
            peeked: None,
            current_domain: None,
        }
    }

    /// Parse the .po file
    pub fn parse(&mut self) -> Result<PoFile, PoError> {
        let mut po_file = PoFile::default();

        while let Some(entry) = self.parse_entry()? {
            // Keep header entries in the entry list (tagged with their domain so
            // each domain's header is emitted) while also exposing the first one
            // via `header` for charset()/plural_forms().
            if entry.is_header() && po_file.header.is_none() {
                po_file.header = Some(entry.clone());
            }
            po_file.entries.push(entry);
        }

        Ok(po_file)
    }

    /// Parse the domain name from a `domain` directive line.
    fn parse_domain_name(&mut self, line: &[u8]) -> Result<String, PoError> {
        let rest = trim(&line["domain".len()..]);
        // A domain name becomes a filename, so it is kept as text; the quoted
        // form still goes through the byte unescaper first.
        if rest.starts_with(b"\"") {
            let bytes = self.parse_quoted_string(rest)?;
            Ok(String::from_utf8_lossy(&bytes).into_owned())
        } else if rest.is_empty() {
            Err(PoError::Parse(
                self.line_number,
                "expected domain name".to_string(),
            ))
        } else {
            Ok(String::from_utf8_lossy(rest).into_owned())
        }
    }

    /// Read the next line, handling the peeked line
    fn next_line(&mut self) -> Result<Option<Vec<u8>>, PoError> {
        if let Some(line) = self.peeked.take() {
            return Ok(Some(line));
        }

        self.current_line.clear();
        let bytes = self.reader.read_until(b'\n', &mut self.current_line)?;
        if bytes == 0 {
            return Ok(None);
        }

        self.line_number += 1;

        // Remove trailing newline (and any <blank>s, as before)
        let end = self
            .current_line
            .iter()
            .rposition(|b| !b.is_ascii_whitespace())
            .map(|i| i + 1)
            .unwrap_or(0);
        Ok(Some(self.current_line[..end].to_vec()))
    }

    /// Peek at the next line without consuming it
    fn peek_line(&mut self) -> Result<Option<&[u8]>, PoError> {
        if self.peeked.is_none() {
            self.peeked = self.next_line()?;
        }
        Ok(self.peeked.as_deref())
    }

    /// Put back a line to be read again
    fn unread_line(&mut self, line: Vec<u8>) {
        self.peeked = Some(line);
    }

    /// Parse a single entry
    fn parse_entry(&mut self) -> Result<Option<PoEntry>, PoError> {
        let mut entry = PoEntry::default();

        // Skip empty lines and collect comments
        loop {
            let line = match self.next_line()? {
                Some(line) => line,
                None => return Ok(None),
            };

            if line.is_empty() {
                continue;
            }

            if let Some(rest) = line.strip_prefix(b"#".as_slice()) {
                self.parse_comment(&mut entry, rest);
            } else if is_domain_directive(&line) {
                // A `domain` directive opens a new section: record the domain
                // for the entries that follow.
                self.current_domain = Some(self.parse_domain_name(&line)?);
                continue;
            } else if line.starts_with(b"msgctxt") || line.starts_with(b"msgid") {
                self.unread_line(line);
                break;
            } else {
                // Unexpected content, skip
                continue;
            }
        }

        // Parse the message content
        while let Some(line) = self.peek_line()? {
            if line.is_empty() {
                break;
            }

            // A `domain` directive ends this entry and opens a new section; leave
            // it unread so the next entry picks it up.
            if is_domain_directive(line) {
                break;
            }

            // Check if this is the start of a new entry
            if line.starts_with(b"#")
                && !entry.msgid.is_empty()
                && entry.msgstr.iter().any(|s| !s.is_empty())
            {
                break;
            }

            let line = self.next_line()?.unwrap();

            if let Some(rest) = line.strip_prefix(b"#".as_slice()) {
                self.parse_comment(&mut entry, rest);
            } else if let Some(rest) = line.strip_prefix(b"msgctxt".as_slice()) {
                entry.msgctxt = Some(self.parse_string_value(rest)?);
            } else if let Some(rest) = line.strip_prefix(b"msgid_plural".as_slice()) {
                entry.msgid_plural = Some(self.parse_string_value(rest)?);
            } else if let Some(rest) = line.strip_prefix(b"msgid".as_slice()) {
                entry.msgid = self.parse_string_value(rest)?;
            } else if let Some(rest) = line.strip_prefix(b"msgstr[".as_slice()) {
                // Plural form: msgstr[N] "..."
                if let Some(idx_end) = rest.iter().position(|&b| b == b']') {
                    let idx: usize = std::str::from_utf8(&rest[..idx_end])
                        .ok()
                        .and_then(|d| d.parse().ok())
                        .ok_or_else(|| {
                            PoError::Parse(self.line_number, "invalid msgstr index".to_string())
                        })?;
                    let value = self.parse_string_value(&rest[idx_end + 1..])?;

                    // Ensure the vector is large enough
                    while entry.msgstr.len() <= idx {
                        entry.msgstr.push(Vec::new());
                    }
                    entry.msgstr[idx] = value;
                }
            } else if let Some(rest) = line.strip_prefix(b"msgstr".as_slice()) {
                entry.msgstr = vec![self.parse_string_value(rest)?];
            } else if line.starts_with(b"\"") {
                // Continuation string
                let value = self.parse_quoted_string(&line)?;
                self.append_to_last_string(&mut entry, &value);
            }
        }

        // Check if we actually parsed an entry
        if entry.msgid.is_empty() && entry.msgctxt.is_none() && entry.msgstr.is_empty() {
            return Ok(None);
        }

        // Ensure msgstr has at least one entry
        if entry.msgstr.is_empty() {
            entry.msgstr.push(Vec::new());
        }

        // Tag with the domain in effect when this entry was parsed.
        entry.domain = self.current_domain.clone();

        Ok(Some(entry))
    }

    /// Parse a comment line
    fn parse_comment(&mut self, entry: &mut PoEntry, rest: &[u8]) {
        // Comment text never reaches the `.mo`, so it is kept as (lossily
        // decoded) text rather than bytes.
        let text = |b: &[u8]| String::from_utf8_lossy(trim(b)).into_owned();
        if let Some(content) = rest.strip_prefix(b".".as_slice()) {
            // Extracted comment
            entry.extracted_comments.push(text(content));
        } else if let Some(content) = rest.strip_prefix(b":".as_slice()) {
            // Reference comment
            entry.reference_comments.push(text(content));
        } else if let Some(content) = rest.strip_prefix(b",".as_slice()) {
            // Flags
            for flag in content.split(|&b| b == b',') {
                let flag = text(flag);
                if flag == "fuzzy" {
                    entry.is_fuzzy = true;
                }
                entry.flags.push(flag);
            }
        } else if let Some(content) = rest.strip_prefix(b"|".as_slice()) {
            // Previous msgid
            let content = trim(content);
            if let Some(rest) = content.strip_prefix(b"msgid".as_slice()) {
                if let Ok(value) = self.parse_string_value(rest) {
                    entry.previous_msgid = Some(value);
                }
            }
        } else if rest.starts_with(b"~") {
            // Obsolete entry
            entry.is_obsolete = true;
        } else {
            // Translator comment
            entry.translator_comments.push(text(rest));
        }
    }

    /// Parse a string value after a keyword (e.g., after "msgid ")
    fn parse_string_value(&mut self, rest: &[u8]) -> Result<Vec<u8>, PoError> {
        let rest = trim(rest);
        if !rest.starts_with(b"\"") {
            return Err(PoError::Parse(
                self.line_number,
                "expected quoted string".to_string(),
            ));
        }

        let mut result = self.parse_quoted_string(rest)?;

        // Handle multi-line strings
        loop {
            match self.peek_line()? {
                Some(line) if line.starts_with(b"\"") => {
                    let line = self.next_line()?.unwrap();
                    result.extend_from_slice(&self.parse_quoted_string(&line)?);
                }
                _ => break,
            }
        }

        Ok(result)
    }

    /// Parse a quoted string (including escape sequences).
    ///
    /// Byte-oriented: bytes outside an escape are copied through untouched, so
    /// text in any codeset survives, and `\\ooo`/`\\xhh` name a *byte*. Pushing
    /// them through `char` UTF-8 encoded every value above 0x7F into two bytes
    /// on the way into the `.mo` file.
    fn parse_quoted_string(&self, s: &[u8]) -> Result<Vec<u8>, PoError> {
        let s = trim(s);
        if !s.starts_with(b"\"") {
            return Err(PoError::Parse(
                self.line_number,
                "expected quoted string".to_string(),
            ));
        }

        let s = &s[1..]; // Skip opening quote
        let mut result = Vec::with_capacity(s.len());
        let mut i = 0;

        loop {
            let Some(&c) = s.get(i) else {
                return Err(PoError::UnterminatedString(self.line_number));
            };
            i += 1;
            match c {
                b'"' => break, // End of string
                b'\\' => {
                    let Some(&esc) = s.get(i) else {
                        return Err(PoError::UnterminatedString(self.line_number));
                    };
                    i += 1;
                    match esc {
                        b'n' => result.push(b'\n'),
                        b't' => result.push(b'\t'),
                        b'r' => result.push(b'\r'),
                        b'\\' => result.push(b'\\'),
                        b'"' => result.push(b'"'),
                        b'a' => result.push(0x07),
                        b'b' => result.push(0x08),
                        b'f' => result.push(0x0C),
                        b'v' => result.push(0x0B),
                        // Octal escape: \o, \oo, or \ooo (this digit included).
                        d @ b'0'..=b'7' => {
                            let mut value = u32::from(d - b'0');
                            for _ in 0..2 {
                                match s.get(i) {
                                    Some(&c @ b'0'..=b'7') => {
                                        value = value * 8 + u32::from(c - b'0');
                                        i += 1;
                                    }
                                    _ => break,
                                }
                            }
                            result.push(value as u8);
                        }
                        // Hex escape: \xh or \xhh.
                        b'x' => {
                            let mut value: u32 = 0;
                            let mut digits = 0;
                            while digits < 2 {
                                match s.get(i) {
                                    Some(c) if c.is_ascii_hexdigit() => {
                                        value = value * 16 + char::from(*c).to_digit(16).unwrap();
                                        i += 1;
                                        digits += 1;
                                    }
                                    _ => break,
                                }
                            }
                            if digits == 0 {
                                return Err(PoError::InvalidEscape(self.line_number, 'x'));
                            }
                            result.push(value as u8);
                        }
                        c => {
                            // Unknown escape sequence is an error.
                            return Err(PoError::InvalidEscape(self.line_number, char::from(c)));
                        }
                    }
                }
                c => result.push(c),
            }
        }

        Ok(result)
    }

    /// Append a value to the last string being built
    fn append_to_last_string(&self, entry: &mut PoEntry, value: &[u8]) {
        // Determine which string to append to based on parsing state
        if !entry.msgstr.is_empty() {
            if let Some(last) = entry.msgstr.last_mut() {
                last.extend_from_slice(value);
            }
        } else if entry.msgid_plural.is_some() {
            if let Some(ref mut plural) = entry.msgid_plural {
                plural.extend_from_slice(value);
            }
        } else {
            entry.msgid.extend_from_slice(value);
        }
    }
}

impl PoFile {
    /// Parse a .po file from a string
    pub fn parse(s: &str) -> Result<Self, PoError> {
        let mut parser = PoParser::new(s.as_bytes());
        parser.parse()
    }

    /// Parse a .po file from a reader
    pub fn parse_from<R: Read>(reader: R) -> Result<Self, PoError> {
        let mut parser = PoParser::new(reader);
        parser.parse()
    }

    /// Get the charset from the header, accepting both the `Content-Type:
    /// ...; charset=...` form and the bare `charset=...` header line.
    pub fn charset(&self) -> Option<String> {
        // The header's own field names and values are ASCII by definition, so
        // it is read as text even though the entry is stored as bytes.
        let header = self.header.as_ref()?.msgstr.first()?;
        let header = String::from_utf8_lossy(header);
        for line in header.lines() {
            if let Some(rest) = line.strip_prefix("Content-Type:") {
                for part in rest.split(';') {
                    if let Some(charset) = part.trim().strip_prefix("charset=") {
                        return Some(charset.trim().to_string());
                    }
                }
            } else if let Some(charset) = line.trim().strip_prefix("charset=") {
                return Some(charset.trim().to_string());
            }
        }
        None
    }

    /// Get the plural forms from the header
    pub fn plural_forms(&self) -> Option<String> {
        let header = self.header.as_ref()?.msgstr.first()?;
        let header = String::from_utf8_lossy(header);
        for line in header.lines() {
            if let Some(rest) = line.strip_prefix("Plural-Forms:") {
                return Some(rest.trim().to_string());
            }
        }
        None
    }

    /// Get all non-fuzzy entries
    pub fn non_fuzzy_entries(&self) -> impl Iterator<Item = &PoEntry> {
        self.entries.iter().filter(|e| !e.is_fuzzy)
    }

    /// Get all entries including fuzzy ones
    pub fn all_entries(&self) -> impl Iterator<Item = &PoEntry> {
        self.entries.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple() {
        let input = r#"
msgid "Hello"
msgstr "Hola"
"#;
        let po = PoFile::parse(input).unwrap();
        assert_eq!(po.entries.len(), 1);
        assert_eq!(po.entries[0].msgid, b"Hello");
        assert_eq!(po.entries[0].msgstr[0], b"Hola");
    }

    #[test]
    fn test_parse_header() {
        let input = r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"
"Plural-Forms: nplurals=2; plural=(n != 1);\n"

msgid "Hello"
msgstr "Hola"
"#;
        let po = PoFile::parse(input).unwrap();
        assert!(po.header.is_some());
        assert_eq!(po.charset().as_deref(), Some("UTF-8"));
        assert!(po.plural_forms().unwrap().contains("nplurals=2"));
    }

    #[test]
    fn test_parse_multiline() {
        let input = r#"
msgid ""
"Hello "
"World"
msgstr "Hola Mundo"
"#;
        let po = PoFile::parse(input).unwrap();
        assert_eq!(po.entries[0].msgid, b"Hello World");
    }

    #[test]
    fn test_parse_plural() {
        let input = r#"
msgid "One item"
msgid_plural "%d items"
msgstr[0] "Un elemento"
msgstr[1] "%d elementos"
"#;
        let po = PoFile::parse(input).unwrap();
        assert_eq!(po.entries.len(), 1);
        assert!(po.entries[0].is_plural());
        assert_eq!(po.entries[0].msgid, b"One item");
        assert_eq!(po.entries[0].msgid_plural, Some(b"%d items".to_vec()));
        assert_eq!(po.entries[0].msgstr.len(), 2);
        assert_eq!(po.entries[0].msgstr[0], b"Un elemento");
        assert_eq!(po.entries[0].msgstr[1], b"%d elementos");
    }

    #[test]
    fn test_parse_fuzzy() {
        let input = r#"
#, fuzzy
msgid "Test"
msgstr "Prueba"
"#;
        let po = PoFile::parse(input).unwrap();
        assert!(po.entries[0].is_fuzzy);
        assert!(po.entries[0].flags.contains(&"fuzzy".to_string()));
    }

    #[test]
    fn test_parse_comments() {
        let input = r#"
# Translator comment
#. Extracted comment
#: file.c:123
#, c-format
msgid "Test %d"
msgstr "Prueba %d"
"#;
        let po = PoFile::parse(input).unwrap();
        let entry = &po.entries[0];
        assert!(!entry.translator_comments.is_empty());
        assert!(!entry.extracted_comments.is_empty());
        assert!(!entry.reference_comments.is_empty());
        assert!(entry.flags.contains(&"c-format".to_string()));
    }

    #[test]
    fn test_escape_sequences() {
        let input = r#"
msgid "Line1\nLine2\tTabbed"
msgstr "Linea1\nLinea2\tTabulado"
"#;
        let po = PoFile::parse(input).unwrap();
        assert_eq!(po.entries[0].msgid, b"Line1\nLine2\tTabbed");
        assert_eq!(po.entries[0].msgstr[0], b"Linea1\nLinea2\tTabulado");
    }

    #[test]
    fn test_parse_domain_directive() {
        let input = r#"
msgid "Hello"
msgstr "Hola"

domain "other"

msgid "Bye"
msgstr "Adios"
"#;
        let po = PoFile::parse(input).unwrap();
        assert_eq!(po.entries.len(), 2);
        // First entry is in the default domain; second in "other".
        let hello = po.entries.iter().find(|e| e.msgid == b"Hello").unwrap();
        let bye = po.entries.iter().find(|e| e.msgid == b"Bye").unwrap();
        assert_eq!(hello.domain, None);
        assert_eq!(bye.domain, Some("other".to_string()));
    }

    #[test]
    fn test_escape_octal_hex_and_control() {
        // \a \b \f \v controls, \101 octal ('A'), \x42 hex ('B').
        let input = "msgid \"x\"\nmsgstr \"\\a\\b\\f\\v\\101\\x42\"\n";
        let po = PoFile::parse(input).unwrap();
        assert_eq!(po.entries[0].msgstr[0], b"\x07\x08\x0C\x0BAB".to_vec());
    }

    #[test]
    fn test_unknown_escape_is_error() {
        let input = "msgid \"x\"\nmsgstr \"a\\qb\"\n";
        assert!(matches!(
            PoFile::parse(input),
            Err(PoError::InvalidEscape(_, 'q'))
        ));
    }

    #[test]
    fn test_bare_charset_header() {
        let input = "msgid \"\"\nmsgstr \"\"\n\"charset=UTF-8\\n\"\n";
        let po = PoFile::parse(input).unwrap();
        assert_eq!(po.charset().as_deref(), Some("UTF-8"));
    }

    #[test]
    fn test_context() {
        let input = r#"
msgctxt "menu"
msgid "File"
msgstr "Archivo"
"#;
        let po = PoFile::parse(input).unwrap();
        assert_eq!(po.entries[0].msgctxt, Some(b"menu".to_vec()));
        assert_eq!(po.entries[0].msgid, b"File");
    }
}
