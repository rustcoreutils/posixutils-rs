//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU .mo file format reader and writer
//!
//! The .mo (Machine Object) file format is the binary format used by GNU gettext
//! for storing translated message catalogs. This module provides functionality
//! for reading and writing .mo files.

use std::collections::HashMap;
use std::io::{self, Read, Write};

/// Magic number for little-endian .mo files
pub const MO_MAGIC_LE: u32 = 0x950412de;

/// Magic number for big-endian .mo files
pub const MO_MAGIC_BE: u32 = 0xde120495;

/// Header of a .mo file
#[derive(Debug, Clone, Default)]
pub struct MoHeader {
    /// Magic number (determines endianness)
    pub magic: u32,
    /// File format revision (0 or 1)
    pub revision: u32,
    /// Number of strings
    pub nstrings: u32,
    /// Offset of table with original strings
    pub orig_tab_offset: u32,
    /// Offset of table with translation strings
    pub trans_tab_offset: u32,
    /// Size of hashing table
    pub hash_tab_size: u32,
    /// Offset of hashing table
    pub hash_tab_offset: u32,
}

impl MoHeader {
    /// Size of the header in bytes
    pub const SIZE: usize = 28;
}

/// A string descriptor (length + offset pair) in the .mo file
#[derive(Debug, Clone, Copy, Default)]
pub struct StringDescriptor {
    /// Length of the string (excluding null terminator)
    pub length: u32,
    /// Offset of the string from the start of the file
    pub offset: u32,
}

impl StringDescriptor {
    /// Size of a string descriptor in bytes
    pub const SIZE: usize = 8;
}

/// Parsed .mo file
#[derive(Debug, Clone)]
pub struct MoFile {
    /// File header
    pub header: MoHeader,
    /// Whether the file is little-endian
    pub is_little_endian: bool,
    /// Message pairs (msgid -> msgstr)
    /// For plural messages, msgid contains null-separated singular and plural forms,
    /// and msgstr contains null-separated translations for each plural form
    pub messages: HashMap<String, String>,
    /// Parsed plural forms from the header entry
    pub plural_info: Option<PluralInfo>,
    /// Character set from the header entry
    pub charset: Option<String>,
}

/// Plural form information parsed from the .mo header entry
#[derive(Debug, Clone)]
pub struct PluralInfo {
    /// Number of plural forms
    pub nplurals: usize,
    /// Plural expression string (to be parsed by plural.rs)
    pub plural_expr: String,
}

/// Error type for .mo file operations
#[derive(Debug)]
pub enum MoError {
    /// Invalid magic number
    InvalidMagic(u32),
    /// Invalid file format
    InvalidFormat(String),
    /// I/O error
    Io(io::Error),
    /// String is not valid UTF-8
    InvalidUtf8(std::string::FromUtf8Error),
}

impl std::fmt::Display for MoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MoError::InvalidMagic(magic) => write!(f, "invalid magic number: 0x{:08x}", magic),
            MoError::InvalidFormat(msg) => write!(f, "invalid format: {}", msg),
            MoError::Io(e) => write!(f, "I/O error: {}", e),
            MoError::InvalidUtf8(e) => write!(f, "invalid UTF-8: {}", e),
        }
    }
}

impl std::error::Error for MoError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            MoError::Io(e) => Some(e),
            MoError::InvalidUtf8(e) => Some(e),
            _ => None,
        }
    }
}

impl From<io::Error> for MoError {
    fn from(e: io::Error) -> Self {
        MoError::Io(e)
    }
}

impl From<std::string::FromUtf8Error> for MoError {
    fn from(e: std::string::FromUtf8Error) -> Self {
        MoError::InvalidUtf8(e)
    }
}

impl MoFile {
    /// Read a .mo file from bytes
    pub fn read(data: &[u8]) -> Result<Self, MoError> {
        if data.len() < MoHeader::SIZE {
            return Err(MoError::InvalidFormat("file too small".to_string()));
        }

        // Read magic number to determine endianness
        let magic = u32::from_le_bytes([data[0], data[1], data[2], data[3]]);
        let is_little_endian = match magic {
            MO_MAGIC_LE => true,
            MO_MAGIC_BE => false,
            _ => {
                // Try big-endian interpretation
                let magic_be = u32::from_be_bytes([data[0], data[1], data[2], data[3]]);
                if magic_be == MO_MAGIC_LE {
                    false
                } else {
                    return Err(MoError::InvalidMagic(magic));
                }
            }
        };

        // Read header
        let header = Self::read_header(data, is_little_endian)?;

        // Validate header
        if header.revision > 1 {
            return Err(MoError::InvalidFormat(format!(
                "unsupported revision: {}",
                header.revision
            )));
        }

        // Read messages
        let messages = Self::read_messages(data, &header, is_little_endian)?;

        // Parse header entry (empty msgid)
        let (plural_info, charset) = if let Some(header_str) = messages.get("") {
            Self::parse_header_entry(header_str)
        } else {
            (None, None)
        };

        Ok(MoFile {
            header,
            is_little_endian,
            messages,
            plural_info,
            charset,
        })
    }

    /// Read a .mo file from a reader
    pub fn read_from<R: Read>(mut reader: R) -> Result<Self, MoError> {
        let mut data = Vec::new();
        reader.read_to_end(&mut data)?;
        Self::read(&data)
    }

    /// Read the header from the data
    fn read_header(data: &[u8], le: bool) -> Result<MoHeader, MoError> {
        Ok(MoHeader {
            magic: Self::read_u32(data, 0, le),
            revision: Self::read_u32(data, 4, le),
            nstrings: Self::read_u32(data, 8, le),
            orig_tab_offset: Self::read_u32(data, 12, le),
            trans_tab_offset: Self::read_u32(data, 16, le),
            hash_tab_size: Self::read_u32(data, 20, le),
            hash_tab_offset: Self::read_u32(data, 24, le),
        })
    }

    /// Read messages from the file
    fn read_messages(
        data: &[u8],
        header: &MoHeader,
        le: bool,
    ) -> Result<HashMap<String, String>, MoError> {
        let mut messages = HashMap::new();

        for i in 0..header.nstrings as usize {
            // Read original string descriptor
            let orig_offset = header.orig_tab_offset as usize + i * StringDescriptor::SIZE;
            let orig_desc = Self::read_string_descriptor(data, orig_offset, le)?;

            // Read translation string descriptor
            let trans_offset = header.trans_tab_offset as usize + i * StringDescriptor::SIZE;
            let trans_desc = Self::read_string_descriptor(data, trans_offset, le)?;

            // Read the actual strings
            let msgid = Self::read_string(data, &orig_desc)?;
            let msgstr = Self::read_string(data, &trans_desc)?;

            messages.insert(msgid, msgstr);
        }

        Ok(messages)
    }

    /// Read a string descriptor from the data
    fn read_string_descriptor(
        data: &[u8],
        offset: usize,
        le: bool,
    ) -> Result<StringDescriptor, MoError> {
        if offset + StringDescriptor::SIZE > data.len() {
            return Err(MoError::InvalidFormat(
                "string descriptor out of bounds".to_string(),
            ));
        }

        Ok(StringDescriptor {
            length: Self::read_u32(data, offset, le),
            offset: Self::read_u32(data, offset + 4, le),
        })
    }

    /// Read a string from the data
    fn read_string(data: &[u8], desc: &StringDescriptor) -> Result<String, MoError> {
        let start = desc.offset as usize;
        let end = start + desc.length as usize;

        if end > data.len() {
            return Err(MoError::InvalidFormat(
                "string data out of bounds".to_string(),
            ));
        }

        let bytes = &data[start..end];
        Ok(String::from_utf8(bytes.to_vec())?)
    }

    /// Read a u32 value with the specified endianness
    fn read_u32(data: &[u8], offset: usize, le: bool) -> u32 {
        let bytes = [
            data[offset],
            data[offset + 1],
            data[offset + 2],
            data[offset + 3],
        ];
        if le {
            u32::from_le_bytes(bytes)
        } else {
            u32::from_be_bytes(bytes)
        }
    }

    /// Parse the header entry (msgid = "") to extract plural forms and charset
    fn parse_header_entry(header_str: &str) -> (Option<PluralInfo>, Option<String>) {
        let mut plural_info = None;
        let mut charset = None;

        for line in header_str.lines() {
            let line = line.trim();

            // Parse Plural-Forms: nplurals=N; plural=EXPR;
            if let Some(rest) = line.strip_prefix("Plural-Forms:") {
                let rest = rest.trim();
                let mut nplurals = None;
                let mut plural_expr = None;

                for part in rest.split(';') {
                    let part = part.trim();
                    if let Some(val) = part.strip_prefix("nplurals=") {
                        nplurals = val.trim().parse().ok();
                    } else if let Some(val) = part.strip_prefix("plural=") {
                        plural_expr = Some(val.trim().to_string());
                    }
                }

                if let (Some(n), Some(expr)) = (nplurals, plural_expr) {
                    plural_info = Some(PluralInfo {
                        nplurals: n,
                        plural_expr: expr,
                    });
                }
            }

            // Parse Content-Type: text/plain; charset=UTF-8
            if let Some(rest) = line.strip_prefix("Content-Type:") {
                for part in rest.split(';') {
                    let part = part.trim();
                    if let Some(val) = part.strip_prefix("charset=") {
                        charset = Some(val.trim().to_string());
                    }
                }
            }
        }

        (plural_info, charset)
    }

    /// Look up a message by its msgid
    pub fn gettext(&self, msgid: &str) -> Option<&str> {
        self.messages.get(msgid).map(|s| s.as_str())
    }

    /// Look up a plural message
    ///
    /// For plural messages, the msgid in the .mo file is the singular form,
    /// and the msgstr contains null-separated translations for each plural form.
    pub fn ngettext(&self, msgid: &str, msgid_plural: &str, n: u64) -> Option<&str> {
        // The key for plural messages is "msgid\0msgid_plural"
        let key = format!("{}\0{}", msgid, msgid_plural);

        if let Some(msgstr) = self.messages.get(&key) {
            // msgstr contains null-separated plural forms
            let forms: Vec<&str> = msgstr.split('\0').collect();
            let index = self.get_plural_index(n);

            if index < forms.len() {
                return Some(forms[index]);
            }
        }

        // Fallback: also try just the singular msgid
        if let Some(msgstr) = self.messages.get(msgid) {
            let forms: Vec<&str> = msgstr.split('\0').collect();
            let index = self.get_plural_index(n);

            if index < forms.len() {
                return Some(forms[index]);
            }
        }

        None
    }

    /// Get the plural form index for a count
    fn get_plural_index(&self, n: u64) -> usize {
        if let Some(ref info) = self.plural_info {
            // The plural expression will be evaluated by plural.rs
            // For now, use the simple Germanic plural rule as fallback
            if n == 1 {
                0
            } else {
                1.min(info.nplurals - 1)
            }
        } else {
            // Default Germanic plural rule: n != 1
            if n == 1 {
                0
            } else {
                1
            }
        }
    }

    /// Write the .mo file to a writer
    pub fn write_to<W: Write>(&self, writer: &mut W) -> Result<(), MoError> {
        let data = self.write_to_bytes()?;
        writer.write_all(&data)?;
        Ok(())
    }

    /// Write the .mo file to bytes
    pub fn write_to_bytes(&self) -> Result<Vec<u8>, MoError> {
        let mut data = Vec::new();

        // Collect and sort messages (empty msgid first, then lexicographically)
        let mut entries: Vec<(&str, &str)> = self
            .messages
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_str()))
            .collect();
        entries.sort_by(|a, b| a.0.cmp(b.0));

        let nstrings = entries.len() as u32;

        // Calculate offsets
        let header_size = MoHeader::SIZE as u32;
        let orig_tab_offset = header_size;
        let trans_tab_offset = orig_tab_offset + nstrings * StringDescriptor::SIZE as u32;
        let strings_offset = trans_tab_offset + nstrings * StringDescriptor::SIZE as u32;

        // Build string data and descriptors
        let mut orig_descriptors = Vec::new();
        let mut trans_descriptors = Vec::new();
        let mut string_data = Vec::new();

        for (msgid, msgstr) in &entries {
            // Original string
            orig_descriptors.push(StringDescriptor {
                length: msgid.len() as u32,
                offset: strings_offset + string_data.len() as u32,
            });
            string_data.extend_from_slice(msgid.as_bytes());
            string_data.push(0); // null terminator

            // Translation string
            trans_descriptors.push(StringDescriptor {
                length: msgstr.len() as u32,
                offset: strings_offset + string_data.len() as u32,
            });
            string_data.extend_from_slice(msgstr.as_bytes());
            string_data.push(0); // null terminator
        }

        // Write header (always little-endian for simplicity)
        data.extend_from_slice(&MO_MAGIC_LE.to_le_bytes());
        data.extend_from_slice(&0u32.to_le_bytes()); // revision
        data.extend_from_slice(&nstrings.to_le_bytes());
        data.extend_from_slice(&orig_tab_offset.to_le_bytes());
        data.extend_from_slice(&trans_tab_offset.to_le_bytes());
        data.extend_from_slice(&0u32.to_le_bytes()); // hash_tab_size
        data.extend_from_slice(&0u32.to_le_bytes()); // hash_tab_offset

        // Write original string descriptors
        for desc in &orig_descriptors {
            data.extend_from_slice(&desc.length.to_le_bytes());
            data.extend_from_slice(&desc.offset.to_le_bytes());
        }

        // Write translation string descriptors
        for desc in &trans_descriptors {
            data.extend_from_slice(&desc.length.to_le_bytes());
            data.extend_from_slice(&desc.offset.to_le_bytes());
        }

        // Write string data
        data.extend_from_slice(&string_data);

        Ok(data)
    }
}

impl Default for MoFile {
    fn default() -> Self {
        MoFile {
            header: MoHeader::default(),
            is_little_endian: true,
            messages: HashMap::new(),
            plural_info: None,
            charset: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_roundtrip() {
        let mut mo = MoFile::default();
        mo.messages.insert("Hello".to_string(), "Hola".to_string());
        mo.messages.insert("World".to_string(), "Mundo".to_string());
        mo.messages.insert(
            "".to_string(),
            "Content-Type: text/plain; charset=UTF-8\nPlural-Forms: nplurals=2; plural=(n != 1);\n"
                .to_string(),
        );

        let bytes = mo.write_to_bytes().unwrap();
        let mo2 = MoFile::read(&bytes).unwrap();

        assert_eq!(mo2.messages.get("Hello"), Some(&"Hola".to_string()));
        assert_eq!(mo2.messages.get("World"), Some(&"Mundo".to_string()));
        assert!(mo2.plural_info.is_some());
        assert_eq!(mo2.plural_info.as_ref().unwrap().nplurals, 2);
        assert_eq!(mo2.charset, Some("UTF-8".to_string()));
    }

    #[test]
    fn test_gettext() {
        let mut mo = MoFile::default();
        mo.messages.insert("Hello".to_string(), "Hola".to_string());

        assert_eq!(mo.gettext("Hello"), Some("Hola"));
        assert_eq!(mo.gettext("NotFound"), None);
    }

    #[test]
    fn test_parse_header_entry() {
        let header =
            "Content-Type: text/plain; charset=UTF-8\nPlural-Forms: nplurals=3; plural=(n==1 ? 0 : n%10>=2 && n%10<=4 ? 1 : 2);\n";

        let (plural_info, charset) = MoFile::parse_header_entry(header);

        assert!(plural_info.is_some());
        let info = plural_info.unwrap();
        assert_eq!(info.nplurals, 3);
        assert!(info.plural_expr.contains("n==1"));

        assert_eq!(charset, Some("UTF-8".to_string()));
    }
}
