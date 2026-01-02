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
    pub previous_msgid: Option<String>,
    /// Message context
    pub msgctxt: Option<String>,
    /// Original string (msgid)
    pub msgid: String,
    /// Plural original (msgid_plural)
    pub msgid_plural: Option<String>,
    /// Translations (msgstr or msgstr[N])
    /// For singular: single element
    /// For plural: multiple elements indexed by plural form
    pub msgstr: Vec<String>,
    /// Whether this entry is marked as fuzzy
    pub is_fuzzy: bool,
    /// Whether this entry is obsolete (#~ ...)
    pub is_obsolete: bool,
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

/// Parser for .po files
pub struct PoParser<R> {
    reader: BufReader<R>,
    line_number: usize,
    current_line: String,
    peeked: Option<String>,
}

impl<R: Read> PoParser<R> {
    /// Create a new parser from a reader
    pub fn new(reader: R) -> Self {
        PoParser {
            reader: BufReader::new(reader),
            line_number: 0,
            current_line: String::new(),
            peeked: None,
        }
    }

    /// Parse the .po file
    pub fn parse(&mut self) -> Result<PoFile, PoError> {
        let mut po_file = PoFile::default();

        while let Some(entry) = self.parse_entry()? {
            if entry.is_header() {
                po_file.header = Some(entry);
            } else {
                po_file.entries.push(entry);
            }
        }

        Ok(po_file)
    }

    /// Read the next line, handling the peeked line
    fn next_line(&mut self) -> Result<Option<String>, PoError> {
        if let Some(line) = self.peeked.take() {
            return Ok(Some(line));
        }

        self.current_line.clear();
        let bytes = self.reader.read_line(&mut self.current_line)?;
        if bytes == 0 {
            return Ok(None);
        }

        self.line_number += 1;

        // Remove trailing newline
        let line = self.current_line.trim_end().to_string();
        Ok(Some(line))
    }

    /// Peek at the next line without consuming it
    fn peek_line(&mut self) -> Result<Option<&str>, PoError> {
        if self.peeked.is_none() {
            self.peeked = self.next_line()?;
        }
        Ok(self.peeked.as_deref())
    }

    /// Put back a line to be read again
    fn unread_line(&mut self, line: String) {
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

            if let Some(rest) = line.strip_prefix('#') {
                self.parse_comment(&mut entry, rest);
            } else if line.starts_with("msgctxt")
                || line.starts_with("msgid")
                || line.starts_with("domain")
            {
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

            // Check if this is the start of a new entry
            if line.starts_with('#')
                && !entry.msgid.is_empty()
                && entry.msgstr.iter().any(|s| !s.is_empty())
            {
                break;
            }

            let line = self.next_line()?.unwrap();

            if line.starts_with("domain ") {
                // Domain directive - skip for now
                continue;
            } else if let Some(rest) = line.strip_prefix('#') {
                self.parse_comment(&mut entry, rest);
            } else if let Some(rest) = line.strip_prefix("msgctxt") {
                entry.msgctxt = Some(self.parse_string_value(rest)?);
            } else if let Some(rest) = line.strip_prefix("msgid_plural") {
                entry.msgid_plural = Some(self.parse_string_value(rest)?);
            } else if let Some(rest) = line.strip_prefix("msgid") {
                entry.msgid = self.parse_string_value(rest)?;
            } else if let Some(rest) = line.strip_prefix("msgstr[") {
                // Plural form: msgstr[N] "..."
                if let Some(idx_end) = rest.find(']') {
                    let idx: usize = rest[..idx_end].parse().map_err(|_| {
                        PoError::Parse(self.line_number, "invalid msgstr index".to_string())
                    })?;
                    let value = self.parse_string_value(&rest[idx_end + 1..])?;

                    // Ensure the vector is large enough
                    while entry.msgstr.len() <= idx {
                        entry.msgstr.push(String::new());
                    }
                    entry.msgstr[idx] = value;
                }
            } else if let Some(rest) = line.strip_prefix("msgstr") {
                entry.msgstr = vec![self.parse_string_value(rest)?];
            } else if line.starts_with('"') {
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
            entry.msgstr.push(String::new());
        }

        Ok(Some(entry))
    }

    /// Parse a comment line
    fn parse_comment(&mut self, entry: &mut PoEntry, rest: &str) {
        if let Some(content) = rest.strip_prefix('.') {
            // Extracted comment
            entry.extracted_comments.push(content.trim().to_string());
        } else if let Some(content) = rest.strip_prefix(':') {
            // Reference comment
            entry.reference_comments.push(content.trim().to_string());
        } else if let Some(content) = rest.strip_prefix(',') {
            // Flags
            for flag in content.split(',') {
                let flag = flag.trim();
                if flag == "fuzzy" {
                    entry.is_fuzzy = true;
                }
                entry.flags.push(flag.to_string());
            }
        } else if let Some(content) = rest.strip_prefix('|') {
            // Previous msgid
            let content = content.trim();
            if let Some(rest) = content.strip_prefix("msgid") {
                if let Ok(value) = self.parse_string_value(rest) {
                    entry.previous_msgid = Some(value);
                }
            }
        } else if let Some(_content) = rest.strip_prefix('~') {
            // Obsolete entry
            entry.is_obsolete = true;
        } else {
            // Translator comment
            entry.translator_comments.push(rest.trim().to_string());
        }
    }

    /// Parse a string value after a keyword (e.g., after "msgid ")
    fn parse_string_value(&mut self, rest: &str) -> Result<String, PoError> {
        let rest = rest.trim();
        if !rest.starts_with('"') {
            return Err(PoError::Parse(
                self.line_number,
                "expected quoted string".to_string(),
            ));
        }

        let mut result = self.parse_quoted_string(rest)?;

        // Handle multi-line strings
        loop {
            match self.peek_line()? {
                Some(line) if line.starts_with('"') => {
                    let line = self.next_line()?.unwrap();
                    result.push_str(&self.parse_quoted_string(&line)?);
                }
                _ => break,
            }
        }

        Ok(result)
    }

    /// Parse a quoted string (including escape sequences)
    fn parse_quoted_string(&self, s: &str) -> Result<String, PoError> {
        let s = s.trim();
        if !s.starts_with('"') {
            return Err(PoError::Parse(
                self.line_number,
                "expected quoted string".to_string(),
            ));
        }

        let s = &s[1..]; // Skip opening quote
        let mut result = String::new();
        let mut chars = s.chars().peekable();

        loop {
            match chars.next() {
                None => {
                    return Err(PoError::UnterminatedString(self.line_number));
                }
                Some('"') => {
                    // End of string
                    break;
                }
                Some('\\') => {
                    // Escape sequence
                    match chars.next() {
                        None => {
                            return Err(PoError::UnterminatedString(self.line_number));
                        }
                        Some('n') => result.push('\n'),
                        Some('t') => result.push('\t'),
                        Some('r') => result.push('\r'),
                        Some('\\') => result.push('\\'),
                        Some('"') => result.push('"'),
                        Some('0') => result.push('\0'),
                        Some(c) => {
                            // Unknown escape - keep as-is
                            result.push('\\');
                            result.push(c);
                        }
                    }
                }
                Some(c) => {
                    result.push(c);
                }
            }
        }

        Ok(result)
    }

    /// Append a value to the last string being built
    fn append_to_last_string(&self, entry: &mut PoEntry, value: &str) {
        // Determine which string to append to based on parsing state
        if !entry.msgstr.is_empty() {
            if let Some(last) = entry.msgstr.last_mut() {
                last.push_str(value);
            }
        } else if entry.msgid_plural.is_some() {
            if let Some(ref mut plural) = entry.msgid_plural {
                plural.push_str(value);
            }
        } else {
            entry.msgid.push_str(value);
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

    /// Get the charset from the header
    pub fn charset(&self) -> Option<&str> {
        self.header.as_ref().and_then(|h| {
            h.msgstr.first().and_then(|s| {
                for line in s.lines() {
                    if let Some(rest) = line.strip_prefix("Content-Type:") {
                        for part in rest.split(';') {
                            if let Some(charset) = part.trim().strip_prefix("charset=") {
                                return Some(charset.trim());
                            }
                        }
                    }
                }
                None
            })
        })
    }

    /// Get the plural forms from the header
    pub fn plural_forms(&self) -> Option<&str> {
        self.header.as_ref().and_then(|h| {
            h.msgstr.first().and_then(|s| {
                for line in s.lines() {
                    if let Some(rest) = line.strip_prefix("Plural-Forms:") {
                        return Some(rest.trim());
                    }
                }
                None
            })
        })
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
        assert_eq!(po.entries[0].msgid, "Hello");
        assert_eq!(po.entries[0].msgstr[0], "Hola");
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
        assert_eq!(po.charset(), Some("UTF-8"));
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
        assert_eq!(po.entries[0].msgid, "Hello World");
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
        assert_eq!(po.entries[0].msgid, "One item");
        assert_eq!(po.entries[0].msgid_plural, Some("%d items".to_string()));
        assert_eq!(po.entries[0].msgstr.len(), 2);
        assert_eq!(po.entries[0].msgstr[0], "Un elemento");
        assert_eq!(po.entries[0].msgstr[1], "%d elementos");
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
        assert_eq!(po.entries[0].msgid, "Line1\nLine2\tTabbed");
        assert_eq!(po.entries[0].msgstr[0], "Linea1\nLinea2\tTabulado");
    }

    #[test]
    fn test_context() {
        let input = r#"
msgctxt "menu"
msgid "File"
msgstr "Archivo"
"#;
        let po = PoFile::parse(input).unwrap();
        assert_eq!(po.entries[0].msgctxt, Some("menu".to_string()));
        assert_eq!(po.entries[0].msgid, "File");
    }
}
