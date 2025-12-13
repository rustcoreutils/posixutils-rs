//
// Copyright (c) 2024 Hemi Labs, Inc.
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! SCCS file format parser and serializer
//!
//! This module provides support for reading and writing SCCS (Source Code
//! Control System) files as defined in POSIX.2024.
//!
//! # File Format
//!
//! SCCS files consist of control lines prefixed with SOH (0x01, displayed as ^A)
//! and text lines. The structure is:
//!
//! - Checksum line: `^Ah<5-digit-sum>`
//! - Delta table: Series of delta entries
//! - User list: Authorized users
//! - Flags: File configuration flags
//! - Descriptive text: Comments about the file
//! - Body: Interleaved delta content with ^AI/^AD/^AE control records

use std::collections::HashSet;
use std::fmt;
use std::io::{self, Read, Write};
use std::path::Path;
use std::str::FromStr;

// =============================================================================
// Constants
// =============================================================================

/// SOH (Start of Heading) control character that marks SCCS control lines
pub const SOH: u8 = 0x01;

/// Control line tags (the character after SOH)
pub mod control {
    pub const CHECKSUM: u8 = b'h'; // ^Ah - checksum
    pub const STATS: u8 = b's'; // ^As - delta statistics
    pub const DELTA: u8 = b'd'; // ^Ad - delta header
    pub const INCLUDE: u8 = b'i'; // ^Ai - included deltas (header)
    pub const EXCLUDE: u8 = b'x'; // ^Ax - excluded deltas
    pub const IGNORE: u8 = b'g'; // ^Ag - ignored deltas
    pub const MR: u8 = b'm'; // ^Am - modification request
    pub const COMMENT: u8 = b'c'; // ^Ac - comment
    pub const END_DELTA: u8 = b'e'; // ^Ae - end of delta entry
    pub const USER_BEGIN: u8 = b'u'; // ^Au - begin user list
    pub const USER_END: u8 = b'U'; // ^AU - end user list
    pub const FLAG: u8 = b'f'; // ^Af - flag
    pub const DESC_BEGIN: u8 = b't'; // ^At - begin descriptive text
    pub const DESC_END: u8 = b'T'; // ^AT - end descriptive text
                                   // Body control records (uppercase)
    pub const BODY_INSERT: u8 = b'I'; // ^AI - begin insert block
    pub const BODY_DELETE: u8 = b'D'; // ^AD - begin delete block
    pub const BODY_END: u8 = b'E'; // ^AE - end block
}

// =============================================================================
// Error Types
// =============================================================================

/// Errors that can occur during SCCS file operations
#[derive(Debug)]
pub enum SccsError {
    /// I/O error
    Io(io::Error),
    /// Invalid file format
    InvalidFormat(String),
    /// Checksum mismatch
    ChecksumMismatch { expected: u16, computed: u16 },
    /// Invalid SID format
    InvalidSid(String),
    /// Invalid delta entry
    InvalidDelta(String),
    /// Invalid flag
    InvalidFlag(String),
    /// Invalid date/time format
    InvalidDateTime(String),
    /// Missing required field
    MissingField(String),
    /// Invalid serial number
    InvalidSerial(String),
    /// Body structure error
    InvalidBody(String),
}

impl fmt::Display for SccsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SccsError::Io(e) => write!(f, "I/O error: {}", e),
            SccsError::InvalidFormat(s) => write!(f, "Invalid SCCS format: {}", s),
            SccsError::ChecksumMismatch { expected, computed } => {
                write!(
                    f,
                    "Checksum mismatch: expected {}, computed {}",
                    expected, computed
                )
            }
            SccsError::InvalidSid(s) => write!(f, "Invalid SID: {}", s),
            SccsError::InvalidDelta(s) => write!(f, "Invalid delta: {}", s),
            SccsError::InvalidFlag(s) => write!(f, "Invalid flag: {}", s),
            SccsError::InvalidDateTime(s) => write!(f, "Invalid date/time: {}", s),
            SccsError::MissingField(s) => write!(f, "Missing field: {}", s),
            SccsError::InvalidSerial(s) => write!(f, "Invalid serial: {}", s),
            SccsError::InvalidBody(s) => write!(f, "Invalid body: {}", s),
        }
    }
}

impl std::error::Error for SccsError {}

impl From<io::Error> for SccsError {
    fn from(e: io::Error) -> Self {
        SccsError::Io(e)
    }
}

/// Result type for SCCS operations
pub type SccsResult<T> = Result<T, SccsError>;

// =============================================================================
// SID (SCCS Identification)
// =============================================================================

/// SCCS Identification String: release.level[.branch.sequence]
///
/// Examples: "1.1", "1.2", "1.1.1.1" (branch)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct Sid {
    pub rel: u16, // Release number (required, >= 1)
    pub lev: u16, // Level number
    pub br: u16,  // Branch number (0 = trunk)
    pub seq: u16, // Sequence number (0 = trunk)
}

impl Sid {
    /// Create a new SID with all components
    pub fn new(rel: u16, lev: u16, br: u16, seq: u16) -> Self {
        Self { rel, lev, br, seq }
    }

    /// Create a trunk SID (no branch)
    pub fn trunk(rel: u16, lev: u16) -> Self {
        Self {
            rel,
            lev,
            br: 0,
            seq: 0,
        }
    }

    /// Check if this is a trunk SID (no branch component)
    pub fn is_trunk(&self) -> bool {
        self.br == 0 && self.seq == 0
    }

    /// Check if this is a partial SID (for -r option parsing)
    pub fn is_partial(&self) -> bool {
        self.lev == 0
    }

    /// Check if this SID is on the same branch as another
    pub fn same_branch(&self, other: &Sid) -> bool {
        self.rel == other.rel && self.lev == other.lev && self.br == other.br
    }
}

impl fmt::Display for Sid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.br == 0 && self.seq == 0 {
            write!(f, "{}.{}", self.rel, self.lev)
        } else {
            write!(f, "{}.{}.{}.{}", self.rel, self.lev, self.br, self.seq)
        }
    }
}

impl FromStr for Sid {
    type Err = SccsError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split('.').collect();
        match parts.len() {
            1 => {
                // Just release (partial SID)
                let rel = parts[0]
                    .parse()
                    .map_err(|_| SccsError::InvalidSid(s.to_string()))?;
                Ok(Sid::new(rel, 0, 0, 0))
            }
            2 => {
                // release.level
                let rel = parts[0]
                    .parse()
                    .map_err(|_| SccsError::InvalidSid(s.to_string()))?;
                let lev = parts[1]
                    .parse()
                    .map_err(|_| SccsError::InvalidSid(s.to_string()))?;
                Ok(Sid::trunk(rel, lev))
            }
            3 => {
                // release.level.branch (partial branch SID)
                let rel = parts[0]
                    .parse()
                    .map_err(|_| SccsError::InvalidSid(s.to_string()))?;
                let lev = parts[1]
                    .parse()
                    .map_err(|_| SccsError::InvalidSid(s.to_string()))?;
                let br = parts[2]
                    .parse()
                    .map_err(|_| SccsError::InvalidSid(s.to_string()))?;
                Ok(Sid::new(rel, lev, br, 0))
            }
            4 => {
                // release.level.branch.sequence
                let rel = parts[0]
                    .parse()
                    .map_err(|_| SccsError::InvalidSid(s.to_string()))?;
                let lev = parts[1]
                    .parse()
                    .map_err(|_| SccsError::InvalidSid(s.to_string()))?;
                let br = parts[2]
                    .parse()
                    .map_err(|_| SccsError::InvalidSid(s.to_string()))?;
                let seq = parts[3]
                    .parse()
                    .map_err(|_| SccsError::InvalidSid(s.to_string()))?;
                Ok(Sid::new(rel, lev, br, seq))
            }
            _ => Err(SccsError::InvalidSid(s.to_string())),
        }
    }
}

impl Ord for Sid {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.rel, self.lev, self.br, self.seq).cmp(&(other.rel, other.lev, other.br, other.seq))
    }
}

impl PartialOrd for Sid {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

// =============================================================================
// Delta Statistics
// =============================================================================

/// Statistics for a delta (from ^As line)
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct DeltaStats {
    pub inserted: u32,
    pub deleted: u32,
    pub unchanged: u32,
}

impl DeltaStats {
    pub fn new(inserted: u32, deleted: u32, unchanged: u32) -> Self {
        Self {
            inserted,
            deleted,
            unchanged,
        }
    }
}

impl FromStr for DeltaStats {
    type Err = SccsError;

    /// Parse from "NNNNN/NNNNN/NNNNN" format
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split('/').collect();
        if parts.len() != 3 {
            return Err(SccsError::InvalidFormat(format!(
                "Invalid stats format: {}",
                s
            )));
        }
        Ok(Self {
            inserted: parts[0]
                .parse()
                .map_err(|_| SccsError::InvalidFormat(s.to_string()))?,
            deleted: parts[1]
                .parse()
                .map_err(|_| SccsError::InvalidFormat(s.to_string()))?,
            unchanged: parts[2]
                .parse()
                .map_err(|_| SccsError::InvalidFormat(s.to_string()))?,
        })
    }
}

impl fmt::Display for DeltaStats {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:05}/{:05}/{:05}",
            self.inserted, self.deleted, self.unchanged
        )
    }
}

// =============================================================================
// Delta Type
// =============================================================================

/// Type of delta entry
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DeltaType {
    #[default]
    Normal, // 'D' - normal delta
    Removed, // 'R' - removed delta
}

impl DeltaType {
    pub fn from_char(c: char) -> SccsResult<Self> {
        match c {
            'D' => Ok(DeltaType::Normal),
            'R' => Ok(DeltaType::Removed),
            _ => Err(SccsError::InvalidDelta(format!(
                "Unknown delta type: {}",
                c
            ))),
        }
    }

    pub fn to_char(&self) -> char {
        match self {
            DeltaType::Normal => 'D',
            DeltaType::Removed => 'R',
        }
    }
}

// =============================================================================
// SCCS Date/Time
// =============================================================================

/// SCCS date/time format
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SccsDateTime {
    pub year: u16, // 2-digit year
    pub month: u8,
    pub day: u8,
    pub hour: u8,
    pub minute: u8,
    pub second: u8,
}

impl SccsDateTime {
    /// Create current date/time
    pub fn now() -> Self {
        use std::time::{SystemTime, UNIX_EPOCH};
        let duration = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default();
        Self::from_unix_timestamp(duration.as_secs() as i64)
    }

    /// Create from Unix timestamp
    pub fn from_unix_timestamp(secs: i64) -> Self {
        // Simple implementation - for a full implementation, use chrono
        let days = secs / 86400;
        let time_of_day = secs % 86400;

        let hour = (time_of_day / 3600) as u8;
        let minute = ((time_of_day % 3600) / 60) as u8;
        let second = (time_of_day % 60) as u8;

        // Simplified date calculation (doesn't handle leap years perfectly)
        let mut year = 1970i32;
        let mut remaining_days = days;

        while remaining_days >= 365 {
            let days_in_year = if year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) {
                366
            } else {
                365
            };
            if remaining_days >= days_in_year {
                remaining_days -= days_in_year;
                year += 1;
            } else {
                break;
            }
        }

        let days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
        let mut month = 1u8;
        for (i, &days_in_month) in days_in_months.iter().enumerate() {
            let dim = if i == 1 && year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) {
                29
            } else {
                days_in_month
            };
            if remaining_days >= dim as i64 {
                remaining_days -= dim as i64;
                month += 1;
            } else {
                break;
            }
        }

        let day = remaining_days as u8 + 1;

        Self {
            year: (year % 100) as u16,
            month,
            day,
            hour,
            minute,
            second,
        }
    }

    /// Parse from "YY/MM/DD" and "HH:MM:SS" strings
    pub fn parse(date: &str, time: &str) -> SccsResult<Self> {
        let date_parts: Vec<&str> = date.split('/').collect();
        let time_parts: Vec<&str> = time.split(':').collect();

        if date_parts.len() != 3 || time_parts.len() != 3 {
            return Err(SccsError::InvalidDateTime(format!("{} {}", date, time)));
        }

        Ok(Self {
            year: date_parts[0]
                .parse()
                .map_err(|_| SccsError::InvalidDateTime(date.to_string()))?,
            month: date_parts[1]
                .parse()
                .map_err(|_| SccsError::InvalidDateTime(date.to_string()))?,
            day: date_parts[2]
                .parse()
                .map_err(|_| SccsError::InvalidDateTime(date.to_string()))?,
            hour: time_parts[0]
                .parse()
                .map_err(|_| SccsError::InvalidDateTime(time.to_string()))?,
            minute: time_parts[1]
                .parse()
                .map_err(|_| SccsError::InvalidDateTime(time.to_string()))?,
            second: time_parts[2]
                .parse()
                .map_err(|_| SccsError::InvalidDateTime(time.to_string()))?,
        })
    }

    /// Format as date string "YY/MM/DD"
    pub fn date_string(&self) -> String {
        format!("{:02}/{:02}/{:02}", self.year, self.month, self.day)
    }

    /// Format as time string "HH:MM:SS"
    pub fn time_string(&self) -> String {
        format!("{:02}:{:02}:{:02}", self.hour, self.minute, self.second)
    }
}

impl fmt::Display for SccsDateTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.date_string(), self.time_string())
    }
}

// =============================================================================
// Delta Entry
// =============================================================================

/// A single delta entry in the delta table
#[derive(Debug, Clone, Default)]
pub struct DeltaEntry {
    pub delta_type: DeltaType,
    pub sid: Sid,
    pub datetime: SccsDateTime,
    pub user: String,
    pub serial: u16,      // Serial number (unique ID within file)
    pub pred_serial: u16, // Predecessor serial (0 for first delta)
    pub stats: DeltaStats,
    pub included: Vec<u16>,      // Serial numbers of included deltas (^Ai)
    pub excluded: Vec<u16>,      // Serial numbers of excluded deltas (^Ax)
    pub ignored: Vec<u16>,       // Serial numbers of ignored deltas (^Ag)
    pub mr_numbers: Vec<String>, // Modification request numbers (^Am)
    pub comments: Vec<String>,   // Comment lines (^Ac)
}

// =============================================================================
// SCCS Flags
// =============================================================================

/// SCCS file flags (from ^Af lines)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SccsFlag {
    /// 'b' - Allow branches
    BranchEnabled,
    /// 'c' <ceil> - Ceiling release
    Ceiling(u16),
    /// 'd' <sid> - Default SID for get
    DefaultSid(Sid),
    /// 'e' <value> - Encoded file flag
    Encoded(u8),
    /// 'f' <floor> - Floor release
    Floor(u16),
    /// 'i' [str] - ID keyword error
    IdKeywordError(Option<String>),
    /// 'j' - Joint edit allowed
    JointEdit,
    /// 'l' <list> - Locked releases
    LockedReleases(Vec<u16>),
    /// 'm' <name> - Module name
    ModuleName(String),
    /// 'n' - Create null deltas
    NullDelta,
    /// 'q' <text> - User-defined flag for %Q%
    QText(String),
    /// 't' <type> - Module type for %Y%
    ModuleType(String),
    /// 'v' [pgm] - MR validation program
    MrValidation(Option<String>),
    /// Unknown flag (for lossless preservation)
    Unknown(char, String),
}

impl SccsFlag {
    /// Parse a flag from its character and value
    pub fn parse(flag_char: char, value: &str) -> SccsResult<Self> {
        let value = value.trim();
        match flag_char {
            'b' => Ok(SccsFlag::BranchEnabled),
            'c' => {
                let ceil = value
                    .parse()
                    .map_err(|_| SccsError::InvalidFlag(format!("c{}", value)))?;
                Ok(SccsFlag::Ceiling(ceil))
            }
            'd' => {
                let sid = value.parse()?;
                Ok(SccsFlag::DefaultSid(sid))
            }
            'e' => {
                let enc = value.parse().unwrap_or(0);
                Ok(SccsFlag::Encoded(enc))
            }
            'f' => {
                let floor = value
                    .parse()
                    .map_err(|_| SccsError::InvalidFlag(format!("f{}", value)))?;
                Ok(SccsFlag::Floor(floor))
            }
            'i' => {
                if value.is_empty() {
                    Ok(SccsFlag::IdKeywordError(None))
                } else {
                    Ok(SccsFlag::IdKeywordError(Some(value.to_string())))
                }
            }
            'j' => Ok(SccsFlag::JointEdit),
            'l' => {
                if value == "a" {
                    // 'a' means all releases
                    Ok(SccsFlag::LockedReleases(vec![]))
                } else {
                    let releases: Result<Vec<u16>, _> =
                        value.split(',').map(|s| s.trim().parse()).collect();
                    Ok(SccsFlag::LockedReleases(releases.map_err(|_| {
                        SccsError::InvalidFlag(format!("l{}", value))
                    })?))
                }
            }
            'm' => Ok(SccsFlag::ModuleName(value.to_string())),
            'n' => Ok(SccsFlag::NullDelta),
            'q' => Ok(SccsFlag::QText(value.to_string())),
            't' => Ok(SccsFlag::ModuleType(value.to_string())),
            'v' => {
                if value.is_empty() {
                    Ok(SccsFlag::MrValidation(None))
                } else {
                    Ok(SccsFlag::MrValidation(Some(value.to_string())))
                }
            }
            _ => Ok(SccsFlag::Unknown(flag_char, value.to_string())),
        }
    }

    /// Get the flag character
    pub fn flag_char(&self) -> char {
        match self {
            SccsFlag::BranchEnabled => 'b',
            SccsFlag::Ceiling(_) => 'c',
            SccsFlag::DefaultSid(_) => 'd',
            SccsFlag::Encoded(_) => 'e',
            SccsFlag::Floor(_) => 'f',
            SccsFlag::IdKeywordError(_) => 'i',
            SccsFlag::JointEdit => 'j',
            SccsFlag::LockedReleases(_) => 'l',
            SccsFlag::ModuleName(_) => 'm',
            SccsFlag::NullDelta => 'n',
            SccsFlag::QText(_) => 'q',
            SccsFlag::ModuleType(_) => 't',
            SccsFlag::MrValidation(_) => 'v',
            SccsFlag::Unknown(c, _) => *c,
        }
    }

    /// Get the flag value as string
    pub fn value_string(&self) -> String {
        match self {
            SccsFlag::BranchEnabled => String::new(),
            SccsFlag::Ceiling(n) => n.to_string(),
            SccsFlag::DefaultSid(sid) => sid.to_string(),
            SccsFlag::Encoded(n) => n.to_string(),
            SccsFlag::Floor(n) => n.to_string(),
            SccsFlag::IdKeywordError(s) => s.clone().unwrap_or_default(),
            SccsFlag::JointEdit => String::new(),
            SccsFlag::LockedReleases(v) => {
                if v.is_empty() {
                    "a".to_string()
                } else {
                    v.iter()
                        .map(|n| n.to_string())
                        .collect::<Vec<_>>()
                        .join(",")
                }
            }
            SccsFlag::ModuleName(s) => s.clone(),
            SccsFlag::NullDelta => String::new(),
            SccsFlag::QText(s) => s.clone(),
            SccsFlag::ModuleType(s) => s.clone(),
            SccsFlag::MrValidation(s) => s.clone().unwrap_or_default(),
            SccsFlag::Unknown(_, s) => s.clone(),
        }
    }
}

// =============================================================================
// Body Records
// =============================================================================

/// A record in the body section
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BodyRecord {
    /// ^AI <serial> - Begin insert block
    Insert(u16),
    /// ^AD <serial> - Begin delete block
    Delete(u16),
    /// ^AE <serial> - End block
    End(u16),
    /// Plain text line
    Text(String),
}

// =============================================================================
// SCCS Header
// =============================================================================

/// SCCS file header (everything before the body)
#[derive(Debug, Clone, Default)]
pub struct SccsHeader {
    pub checksum: u16,
    pub deltas: Vec<DeltaEntry>,
    pub users: Vec<String>,
    pub flags: Vec<SccsFlag>,
    pub descriptive_text: Vec<String>,
}

// =============================================================================
// SCCS File
// =============================================================================

/// Complete SCCS file representation
#[derive(Debug, Clone, Default)]
pub struct SccsFile {
    pub header: SccsHeader,
    pub body: Vec<BodyRecord>,
}

impl SccsFile {
    /// Parse SCCS file from bytes
    pub fn from_bytes(data: &[u8]) -> SccsResult<Self> {
        let mut parser = SccsParser::new(data);
        parser.parse()
    }

    /// Parse SCCS file from a reader
    pub fn from_reader<R: Read>(mut reader: R) -> SccsResult<Self> {
        let mut data = Vec::new();
        reader.read_to_end(&mut data)?;
        Self::from_bytes(&data)
    }

    /// Parse SCCS file from a file path
    pub fn from_path<P: AsRef<Path>>(path: P) -> SccsResult<Self> {
        let data = std::fs::read(path)?;
        Self::from_bytes(&data)
    }

    /// Serialize to bytes
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        self.write_to(&mut output).unwrap();
        output
    }

    /// Write to a writer
    pub fn write_to<W: Write>(&self, writer: &mut W) -> io::Result<()> {
        // First, serialize content (everything after checksum line)
        let mut content = Vec::new();
        self.write_content(&mut content)?;

        // Compute checksum of content
        let checksum = compute_checksum(&content);

        // Write checksum line
        writeln!(writer, "\x01h{:05}", checksum)?;

        // Write content
        writer.write_all(&content)?;
        Ok(())
    }

    fn write_content<W: Write>(&self, writer: &mut W) -> io::Result<()> {
        // Write delta table
        for delta in &self.header.deltas {
            // Stats line
            writeln!(writer, "\x01s {}", delta.stats)?;

            // Delta header line
            writeln!(
                writer,
                "\x01d {} {} {} {} {} {} {}",
                delta.delta_type.to_char(),
                delta.sid,
                delta.datetime.date_string(),
                delta.datetime.time_string(),
                delta.user,
                delta.serial,
                delta.pred_serial
            )?;

            // Included deltas
            if !delta.included.is_empty() {
                let serials: Vec<String> = delta.included.iter().map(|s| s.to_string()).collect();
                writeln!(writer, "\x01i {}", serials.join(" "))?;
            }

            // Excluded deltas
            if !delta.excluded.is_empty() {
                let serials: Vec<String> = delta.excluded.iter().map(|s| s.to_string()).collect();
                writeln!(writer, "\x01x {}", serials.join(" "))?;
            }

            // Ignored deltas
            if !delta.ignored.is_empty() {
                let serials: Vec<String> = delta.ignored.iter().map(|s| s.to_string()).collect();
                writeln!(writer, "\x01g {}", serials.join(" "))?;
            }

            // MR numbers
            for mr in &delta.mr_numbers {
                writeln!(writer, "\x01m {}", mr)?;
            }

            // Comments
            for comment in &delta.comments {
                writeln!(writer, "\x01c {}", comment)?;
            }

            // End of delta
            writeln!(writer, "\x01e")?;
        }

        // User list
        writeln!(writer, "\x01u")?;
        for user in &self.header.users {
            writeln!(writer, "{}", user)?;
        }
        writeln!(writer, "\x01U")?;

        // Flags
        for flag in &self.header.flags {
            let value = flag.value_string();
            if value.is_empty() {
                writeln!(writer, "\x01f {}", flag.flag_char())?;
            } else {
                writeln!(writer, "\x01f {} {}", flag.flag_char(), value)?;
            }
        }

        // Descriptive text
        writeln!(writer, "\x01t")?;
        for line in &self.header.descriptive_text {
            writeln!(writer, "{}", line)?;
        }
        writeln!(writer, "\x01T")?;

        // Body
        for record in &self.body {
            match record {
                BodyRecord::Insert(serial) => writeln!(writer, "\x01I {}", serial)?,
                BodyRecord::Delete(serial) => writeln!(writer, "\x01D {}", serial)?,
                BodyRecord::End(serial) => writeln!(writer, "\x01E {}", serial)?,
                BodyRecord::Text(line) => writeln!(writer, "{}", line)?,
            }
        }

        Ok(())
    }

    // =========================================================================
    // Delta Graph Operations
    // =========================================================================

    /// Find delta by serial number
    pub fn find_delta_by_serial(&self, serial: u16) -> Option<&DeltaEntry> {
        self.header.deltas.iter().find(|d| d.serial == serial)
    }

    /// Find delta by SID
    pub fn find_delta_by_sid(&self, sid: &Sid) -> Option<&DeltaEntry> {
        self.header.deltas.iter().find(|d| &d.sid == sid)
    }

    /// Get the highest (most recent) delta on trunk
    pub fn get_trunk_head(&self) -> Option<&DeltaEntry> {
        self.header
            .deltas
            .iter()
            .filter(|d| d.sid.is_trunk() && d.delta_type == DeltaType::Normal)
            .max_by(|a, b| a.sid.cmp(&b.sid))
    }

    /// Get the highest serial number in use
    pub fn max_serial(&self) -> u16 {
        self.header
            .deltas
            .iter()
            .map(|d| d.serial)
            .max()
            .unwrap_or(0)
    }

    /// Compute the "applied set" - serial numbers of deltas needed to reconstruct a version
    pub fn compute_applied_set(&self, target_serial: u16) -> SccsResult<HashSet<u16>> {
        let mut applied = HashSet::new();
        let mut stack = vec![target_serial];
        let mut visited = HashSet::new();

        while let Some(serial) = stack.pop() {
            if visited.contains(&serial) || serial == 0 {
                continue;
            }
            visited.insert(serial);

            let delta = self
                .find_delta_by_serial(serial)
                .ok_or_else(|| SccsError::InvalidSerial(format!("Serial {} not found", serial)))?;

            // Skip removed deltas
            if delta.delta_type == DeltaType::Removed {
                continue;
            }

            // Add this delta
            applied.insert(serial);

            // Process predecessor
            if delta.pred_serial > 0 {
                stack.push(delta.pred_serial);
            }

            // Process included deltas
            for &inc in &delta.included {
                stack.push(inc);
            }
        }

        // Remove excluded deltas
        if let Some(target) = self.find_delta_by_serial(target_serial) {
            for &exc in &target.excluded {
                applied.remove(&exc);
            }
        }

        Ok(applied)
    }

    // =========================================================================
    // Body Evaluation
    // =========================================================================

    /// Evaluate the body to produce file content for a given set of deltas
    pub fn evaluate_body(&self, applied_set: &HashSet<u16>) -> Vec<String> {
        let mut output = Vec::new();
        let mut stack: Vec<(bool, u16)> = Vec::new(); // (is_insert, serial)

        for record in &self.body {
            match record {
                BodyRecord::Insert(serial) => {
                    stack.push((true, *serial));
                }
                BodyRecord::Delete(serial) => {
                    stack.push((false, *serial));
                }
                BodyRecord::End(_) => {
                    stack.pop();
                }
                BodyRecord::Text(line) => {
                    if is_line_visible(&stack, applied_set) {
                        output.push(line.clone());
                    }
                }
            }
        }

        output
    }

    // =========================================================================
    // Flag Access
    // =========================================================================

    /// Check if branch flag is set
    pub fn branch_enabled(&self) -> bool {
        self.header
            .flags
            .iter()
            .any(|f| matches!(f, SccsFlag::BranchEnabled))
    }

    /// Get module name (from 'm' flag or derived from filename)
    pub fn module_name(&self) -> Option<&str> {
        for flag in &self.header.flags {
            if let SccsFlag::ModuleName(name) = flag {
                return Some(name);
            }
        }
        None
    }

    /// Get module type (from 't' flag)
    pub fn module_type(&self) -> Option<&str> {
        for flag in &self.header.flags {
            if let SccsFlag::ModuleType(t) = flag {
                return Some(t);
            }
        }
        None
    }

    /// Check if joint edit is allowed
    pub fn joint_edit(&self) -> bool {
        self.header
            .flags
            .iter()
            .any(|f| matches!(f, SccsFlag::JointEdit))
    }
}

/// Check if a line should be visible given the current stack state
fn is_line_visible(stack: &[(bool, u16)], applied_set: &HashSet<u16>) -> bool {
    for &(is_insert, serial) in stack {
        if is_insert {
            // Insert block: line visible only if serial is in applied set
            if !applied_set.contains(&serial) {
                return false;
            }
        } else {
            // Delete block: line hidden if serial is in applied set
            if applied_set.contains(&serial) {
                return false;
            }
        }
    }
    true
}

// =============================================================================
// Parser
// =============================================================================

struct SccsParser<'a> {
    data: &'a [u8],
    pos: usize,
    line_num: usize,
}

impl<'a> SccsParser<'a> {
    fn new(data: &'a [u8]) -> Self {
        Self {
            data,
            pos: 0,
            line_num: 0,
        }
    }

    fn parse(&mut self) -> SccsResult<SccsFile> {
        // Skip leading whitespace/empty lines
        self.skip_whitespace();

        // Parse checksum
        let checksum = self.parse_checksum()?;

        // Parse delta table
        let deltas = self.parse_delta_table()?;

        // Parse user list
        let users = self.parse_user_list()?;

        // Parse flags
        let flags = self.parse_flags()?;

        // Parse descriptive text
        let descriptive_text = self.parse_descriptive_text()?;

        // Parse body
        let body = self.parse_body()?;

        Ok(SccsFile {
            header: SccsHeader {
                checksum,
                deltas,
                users,
                flags,
                descriptive_text,
            },
            body,
        })
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.data.len()
            && (self.data[self.pos] == b'\n' || self.data[self.pos] == b'\r')
        {
            if self.data[self.pos] == b'\n' {
                self.line_num += 1;
            }
            self.pos += 1;
        }
    }

    fn next_line(&mut self) -> Option<&'a [u8]> {
        if self.pos >= self.data.len() {
            return None;
        }

        let start = self.pos;
        while self.pos < self.data.len() && self.data[self.pos] != b'\n' {
            self.pos += 1;
        }
        let end = self.pos;

        // Skip the newline
        if self.pos < self.data.len() {
            self.pos += 1;
        }
        self.line_num += 1;

        // Handle \r\n
        let line = if end > start && self.data[end - 1] == b'\r' {
            &self.data[start..end - 1]
        } else {
            &self.data[start..end]
        };

        Some(line)
    }

    fn peek_line(&self) -> Option<&'a [u8]> {
        if self.pos >= self.data.len() {
            return None;
        }

        let start = self.pos;
        let mut end = self.pos;
        while end < self.data.len() && self.data[end] != b'\n' {
            end += 1;
        }

        let line = if end > start && self.data[end - 1] == b'\r' {
            &self.data[start..end - 1]
        } else {
            &self.data[start..end]
        };

        Some(line)
    }

    fn parse_checksum(&mut self) -> SccsResult<u16> {
        let line = self
            .next_line()
            .ok_or(SccsError::MissingField("checksum".into()))?;

        // Must start with ^Ah
        if line.len() < 3 || line[0] != SOH || line[1] != control::CHECKSUM {
            return Err(SccsError::InvalidFormat(
                "File does not start with checksum line".into(),
            ));
        }

        let sum_str = std::str::from_utf8(&line[2..])
            .map_err(|_| SccsError::InvalidFormat("Invalid checksum encoding".into()))?;

        sum_str
            .trim()
            .parse()
            .map_err(|_| SccsError::InvalidFormat(format!("Invalid checksum: {}", sum_str)))
    }

    fn parse_delta_table(&mut self) -> SccsResult<Vec<DeltaEntry>> {
        let mut deltas = Vec::new();

        while let Some(line) = self.peek_line() {
            // Check for end of delta table (^Au starts user list)
            if line.len() >= 2 && line[0] == SOH && line[1] == control::USER_BEGIN {
                break;
            }

            // Parse delta entry starting with ^As
            if line.len() >= 2 && line[0] == SOH && line[1] == control::STATS {
                let delta = self.parse_delta_entry()?;
                deltas.push(delta);
            } else {
                // Skip unknown lines
                self.next_line();
            }
        }

        Ok(deltas)
    }

    fn parse_delta_entry(&mut self) -> SccsResult<DeltaEntry> {
        let mut entry = DeltaEntry::default();

        // Parse ^As line (stats)
        let stats_line = self
            .next_line()
            .ok_or(SccsError::MissingField("stats".into()))?;
        if stats_line.len() < 3 || stats_line[0] != SOH || stats_line[1] != control::STATS {
            return Err(SccsError::InvalidDelta("Expected stats line".into()));
        }
        let stats_str = std::str::from_utf8(&stats_line[3..])
            .map_err(|_| SccsError::InvalidDelta("Invalid stats encoding".into()))?;
        entry.stats = DeltaStats::from_str(stats_str.trim())?;

        // Parse ^Ad line (delta header)
        let delta_line = self
            .next_line()
            .ok_or(SccsError::MissingField("delta header".into()))?;
        if delta_line.len() < 3 || delta_line[0] != SOH || delta_line[1] != control::DELTA {
            return Err(SccsError::InvalidDelta("Expected delta line".into()));
        }
        let delta_str = std::str::from_utf8(&delta_line[3..])
            .map_err(|_| SccsError::InvalidDelta("Invalid delta encoding".into()))?;

        // Parse: <type> <sid> <date> <time> <user> <serial> <pred>
        let parts: Vec<&str> = delta_str.split_whitespace().collect();
        if parts.len() < 7 {
            return Err(SccsError::InvalidDelta(format!(
                "Invalid delta format: {}",
                delta_str
            )));
        }

        entry.delta_type = DeltaType::from_char(parts[0].chars().next().unwrap_or('D'))?;
        entry.sid = parts[1].parse()?;
        entry.datetime = SccsDateTime::parse(parts[2], parts[3])?;
        entry.user = parts[4].to_string();
        entry.serial = parts[5]
            .parse()
            .map_err(|_| SccsError::InvalidDelta("Invalid serial".into()))?;
        entry.pred_serial = parts[6]
            .parse()
            .map_err(|_| SccsError::InvalidDelta("Invalid predecessor".into()))?;

        // Parse optional lines until ^Ae
        loop {
            let line = self
                .next_line()
                .ok_or(SccsError::MissingField("delta end".into()))?;

            if line.len() < 2 || line[0] != SOH {
                continue;
            }

            match line[1] {
                control::END_DELTA => break,
                control::INCLUDE => {
                    let serials_str = std::str::from_utf8(&line[3..]).unwrap_or("");
                    for s in serials_str.split_whitespace() {
                        if let Ok(serial) = s.parse() {
                            entry.included.push(serial);
                        }
                    }
                }
                control::EXCLUDE => {
                    let serials_str = std::str::from_utf8(&line[3..]).unwrap_or("");
                    for s in serials_str.split_whitespace() {
                        if let Ok(serial) = s.parse() {
                            entry.excluded.push(serial);
                        }
                    }
                }
                control::IGNORE => {
                    let serials_str = std::str::from_utf8(&line[3..]).unwrap_or("");
                    for s in serials_str.split_whitespace() {
                        if let Ok(serial) = s.parse() {
                            entry.ignored.push(serial);
                        }
                    }
                }
                control::MR => {
                    let mr = std::str::from_utf8(&line[3..]).unwrap_or("").trim();
                    if !mr.is_empty() {
                        entry.mr_numbers.push(mr.to_string());
                    }
                }
                control::COMMENT => {
                    let comment = std::str::from_utf8(&line[3..]).unwrap_or("").to_string();
                    entry.comments.push(comment);
                }
                _ => {} // Skip unknown
            }
        }

        Ok(entry)
    }

    fn parse_user_list(&mut self) -> SccsResult<Vec<String>> {
        let mut users = Vec::new();

        // Consume ^Au line
        let line = self
            .next_line()
            .ok_or(SccsError::MissingField("user list".into()))?;
        if line.len() < 2 || line[0] != SOH || line[1] != control::USER_BEGIN {
            return Err(SccsError::InvalidFormat("Expected user list start".into()));
        }

        // Read user names until ^AU
        loop {
            let line = self
                .next_line()
                .ok_or(SccsError::MissingField("user list end".into()))?;

            if line.len() >= 2 && line[0] == SOH && line[1] == control::USER_END {
                break;
            }

            let user = std::str::from_utf8(line).unwrap_or("").trim();
            if !user.is_empty() {
                users.push(user.to_string());
            }
        }

        Ok(users)
    }

    fn parse_flags(&mut self) -> SccsResult<Vec<SccsFlag>> {
        let mut flags = Vec::new();

        while let Some(line) = self.peek_line() {
            // Check for end of flags (^At starts descriptive text)
            if line.len() >= 2 && line[0] == SOH && line[1] == control::DESC_BEGIN {
                break;
            }

            // Parse ^Af lines
            if line.len() >= 4 && line[0] == SOH && line[1] == control::FLAG {
                self.next_line();
                let flag_str = std::str::from_utf8(&line[3..]).unwrap_or("").trim();
                if !flag_str.is_empty() {
                    let flag_char = flag_str.chars().next().unwrap();
                    let value = if flag_str.len() > 1 {
                        flag_str[1..].trim()
                    } else {
                        ""
                    };
                    flags.push(SccsFlag::parse(flag_char, value)?);
                }
            } else {
                self.next_line();
            }
        }

        Ok(flags)
    }

    fn parse_descriptive_text(&mut self) -> SccsResult<Vec<String>> {
        let mut text = Vec::new();

        // Consume ^At line
        let line = self
            .next_line()
            .ok_or(SccsError::MissingField("descriptive text".into()))?;
        if line.len() < 2 || line[0] != SOH || line[1] != control::DESC_BEGIN {
            return Err(SccsError::InvalidFormat(
                "Expected descriptive text start".into(),
            ));
        }

        // Read text until ^AT
        loop {
            let line = self
                .next_line()
                .ok_or(SccsError::MissingField("descriptive text end".into()))?;

            if line.len() >= 2 && line[0] == SOH && line[1] == control::DESC_END {
                break;
            }

            let text_line = std::str::from_utf8(line).unwrap_or("");
            text.push(text_line.to_string());
        }

        Ok(text)
    }

    fn parse_body(&mut self) -> SccsResult<Vec<BodyRecord>> {
        let mut body = Vec::new();

        while let Some(line) = self.next_line() {
            if line.len() >= 2 && line[0] == SOH {
                let serial_str = std::str::from_utf8(&line[3..]).unwrap_or("").trim();
                let serial: u16 = serial_str.parse().unwrap_or(0);

                match line[1] {
                    control::BODY_INSERT => {
                        body.push(BodyRecord::Insert(serial));
                    }
                    control::BODY_DELETE => {
                        body.push(BodyRecord::Delete(serial));
                    }
                    control::BODY_END => {
                        body.push(BodyRecord::End(serial));
                    }
                    _ => {
                        // Unknown control line in body, treat as text
                        let text = std::str::from_utf8(line).unwrap_or("");
                        body.push(BodyRecord::Text(text.to_string()));
                    }
                }
            } else {
                // Text line
                let text = std::str::from_utf8(line).unwrap_or("");
                body.push(BodyRecord::Text(text.to_string()));
            }
        }

        Ok(body)
    }
}

// =============================================================================
// Checksum
// =============================================================================

/// Compute SCCS checksum (sum of all bytes modulo 65536)
pub fn compute_checksum(data: &[u8]) -> u16 {
    let mut sum: u32 = 0;
    for &byte in data {
        sum = sum.wrapping_add(byte as u32);
    }
    (sum & 0xFFFF) as u16
}

// =============================================================================
// Path Utilities
// =============================================================================

pub mod paths {
    use std::path::{Path, PathBuf};

    /// Get the g-file (gotten file) path from s-file path
    /// /path/to/s.foo -> /path/to/foo
    /// /path/to/SCCS/s.foo -> /path/to/foo
    pub fn gfile_from_sfile(sfile: &Path) -> Option<PathBuf> {
        let name = sfile.file_name()?.to_str()?;
        if let Some(gname) = name.strip_prefix("s.") {
            // Determine g-file directory
            let parent = sfile.parent().unwrap_or(Path::new("."));

            // If s-file is in SCCS/ directory, g-file goes to parent of SCCS/
            if parent.file_name().map(|n| n == "SCCS").unwrap_or(false) {
                let gfile_dir = parent.parent().unwrap_or(Path::new("."));
                Some(gfile_dir.join(gname))
            } else {
                // g-file goes to same directory as s-file
                Some(parent.join(gname))
            }
        } else {
            None
        }
    }

    /// Get the s-file path from g-file path
    /// foo -> SCCS/s.foo (if SCCS exists) or s.foo
    pub fn sfile_from_gfile(gfile: &Path) -> PathBuf {
        let name = gfile
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown");
        let sname = format!("s.{}", name);

        // Check for SCCS subdirectory
        let parent = gfile.parent().unwrap_or(Path::new("."));
        let sccs_dir = parent.join("SCCS");
        if sccs_dir.is_dir() {
            sccs_dir.join(&sname)
        } else {
            parent.join(&sname)
        }
    }

    /// Get the p-file (lock file) path from s-file
    /// s.foo -> p.foo (in same directory as s-file)
    pub fn pfile_from_sfile(sfile: &Path) -> PathBuf {
        let name = sfile
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("s.unknown");
        let pname = format!("p.{}", &name[2..]);
        sfile.with_file_name(pname)
    }

    /// Get the z-file (lock semaphore) path from s-file
    pub fn zfile_from_sfile(sfile: &Path) -> PathBuf {
        let name = sfile
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("s.unknown");
        let zname = format!("z.{}", &name[2..]);
        sfile.with_file_name(zname)
    }

    /// Get the l-file (delta summary) path from s-file
    pub fn lfile_from_sfile(sfile: &Path) -> PathBuf {
        let name = sfile
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("s.unknown");
        let lname = format!("l.{}", &name[2..]);
        sfile.with_file_name(lname)
    }

    /// Get the x-file (temporary) path from s-file
    pub fn xfile_from_sfile(sfile: &Path) -> PathBuf {
        let name = sfile
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("s.unknown");
        let xname = format!("x.{}", &name[2..]);
        sfile.with_file_name(xname)
    }

    /// Check if a path is an SCCS s-file
    pub fn is_sfile(path: &Path) -> bool {
        path.file_name()
            .and_then(|n| n.to_str())
            .map(|n| n.starts_with("s."))
            .unwrap_or(false)
    }

    /// Get module name from s-file path
    /// SCCS/s.foo.c -> foo.c
    pub fn module_name(sfile: &Path) -> Option<String> {
        let name = sfile.file_name()?.to_str()?;
        name.strip_prefix("s.").map(|s| s.to_string())
    }
}

// =============================================================================
// P-file Support
// =============================================================================

/// Entry in a p-file (edit lock)
#[derive(Debug, Clone)]
pub struct PfileEntry {
    pub old_sid: Sid, // SID being edited (got SID)
    pub new_sid: Sid, // SID that will be created
    pub user: String, // User who holds the lock
    pub datetime: SccsDateTime,
    pub included: Option<String>, // -i option argument
    pub excluded: Option<String>, // -x option argument
}

impl PfileEntry {
    /// Parse a p-file entry from a line
    pub fn parse(line: &str) -> SccsResult<Self> {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() < 4 {
            return Err(SccsError::InvalidFormat(format!(
                "Invalid p-file entry: {}",
                line
            )));
        }

        let old_sid: Sid = parts[0].parse()?;
        let new_sid: Sid = parts[1].parse()?;
        let user = parts[2].to_string();
        let datetime = SccsDateTime::parse(parts[3], parts.get(4).unwrap_or(&"00:00:00"))?;

        // Parse optional -i and -x
        let mut included = None;
        let mut excluded = None;
        let mut i = 5;
        while i < parts.len() {
            if parts[i] == "-i" && i + 1 < parts.len() {
                included = Some(parts[i + 1].to_string());
                i += 2;
            } else if parts[i] == "-x" && i + 1 < parts.len() {
                excluded = Some(parts[i + 1].to_string());
                i += 2;
            } else {
                i += 1;
            }
        }

        Ok(Self {
            old_sid,
            new_sid,
            user,
            datetime,
            included,
            excluded,
        })
    }

    /// Format as a p-file line
    pub fn to_line(&self) -> String {
        let mut line = format!(
            "{} {} {} {}",
            self.old_sid, self.new_sid, self.user, self.datetime
        );
        if let Some(ref inc) = self.included {
            line.push_str(&format!(" -i {}", inc));
        }
        if let Some(ref exc) = self.excluded {
            line.push_str(&format!(" -x {}", exc));
        }
        line
    }
}

/// Parse all entries from a p-file
pub fn parse_pfile(contents: &str) -> SccsResult<Vec<PfileEntry>> {
    contents
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(PfileEntry::parse)
        .collect()
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sid_parse() {
        assert_eq!("1.1".parse::<Sid>().unwrap(), Sid::trunk(1, 1));
        assert_eq!("1.2.3.4".parse::<Sid>().unwrap(), Sid::new(1, 2, 3, 4));
        assert_eq!("2".parse::<Sid>().unwrap(), Sid::new(2, 0, 0, 0));
    }

    #[test]
    fn test_sid_display() {
        assert_eq!(Sid::trunk(1, 1).to_string(), "1.1");
        assert_eq!(Sid::new(1, 2, 3, 4).to_string(), "1.2.3.4");
    }

    #[test]
    fn test_delta_stats() {
        let stats = DeltaStats::from_str("00003/00001/00010").unwrap();
        assert_eq!(stats.inserted, 3);
        assert_eq!(stats.deleted, 1);
        assert_eq!(stats.unchanged, 10);
        assert_eq!(stats.to_string(), "00003/00001/00010");
    }

    #[test]
    fn test_checksum() {
        let data = b"hello world";
        let sum = compute_checksum(data);
        assert!(sum > 0);
    }

    #[test]
    fn test_path_utilities() {
        use std::path::Path;

        assert!(paths::is_sfile(Path::new("s.foo")));
        assert!(paths::is_sfile(Path::new("SCCS/s.foo.c")));
        assert!(!paths::is_sfile(Path::new("foo.c")));

        assert_eq!(
            paths::gfile_from_sfile(Path::new("s.foo.c")),
            Some(PathBuf::from("foo.c"))
        );

        assert_eq!(
            paths::pfile_from_sfile(Path::new("SCCS/s.foo")),
            PathBuf::from("SCCS/p.foo")
        );

        assert_eq!(
            paths::module_name(Path::new("s.foo.c")),
            Some("foo.c".to_string())
        );
    }

    #[test]
    fn test_sccs_flag_parse() {
        let flag = SccsFlag::parse('b', "").unwrap();
        assert!(matches!(flag, SccsFlag::BranchEnabled));

        let flag = SccsFlag::parse('c', "5").unwrap();
        assert!(matches!(flag, SccsFlag::Ceiling(5)));

        let flag = SccsFlag::parse('m', "mymodule").unwrap();
        assert!(matches!(flag, SccsFlag::ModuleName(ref s) if s == "mymodule"));
    }

    // Fixture tests - verify parser can read cssc-generated SCCS files

    fn fixtures_path() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .join("sccs/tests/fixtures")
    }

    #[test]
    fn test_parse_simple_fixture() {
        let path = fixtures_path().join("s.simple");
        if !path.exists() {
            eprintln!("Skipping test: fixture not found at {:?}", path);
            return;
        }

        let sccs = SccsFile::from_path(&path).expect("Failed to parse s.simple");

        // Should have exactly one delta
        assert_eq!(sccs.header.deltas.len(), 1, "Expected 1 delta in s.simple");

        // Delta should be 1.1
        let delta = &sccs.header.deltas[0];
        assert_eq!(delta.sid.to_string(), "1.1");
        assert_eq!(delta.serial, 1);
        assert_eq!(delta.pred_serial, 0);

        // Body should have content
        assert!(!sccs.body.is_empty(), "Body should not be empty");

        // Evaluate body for delta 1.1
        let applied = sccs
            .compute_applied_set(1)
            .expect("Failed to compute applied set");
        let content = sccs.evaluate_body(&applied);

        // Should have 3 lines: line1, line2, line3
        assert_eq!(content.len(), 3, "Expected 3 lines of content");
        assert_eq!(content[0], "line1");
        assert_eq!(content[1], "line2");
        assert_eq!(content[2], "line3");
    }

    #[test]
    fn test_parse_multi_fixture() {
        let path = fixtures_path().join("s.multi");
        if !path.exists() {
            eprintln!("Skipping test: fixture not found at {:?}", path);
            return;
        }

        let sccs = SccsFile::from_path(&path).expect("Failed to parse s.multi");

        // Should have multiple deltas
        assert!(
            sccs.header.deltas.len() >= 2,
            "Expected multiple deltas in s.multi"
        );

        // Find trunk head
        let head = sccs.get_trunk_head();
        assert!(head.is_some(), "Should have a trunk head");

        // Verify we can evaluate body for the latest version
        let head_serial = head.unwrap().serial;
        let applied = sccs
            .compute_applied_set(head_serial)
            .expect("Failed to compute applied set");
        let content = sccs.evaluate_body(&applied);
        assert!(
            !content.is_empty(),
            "Content should not be empty for latest version"
        );
    }

    #[test]
    fn test_parse_keywords_fixture() {
        let path = fixtures_path().join("s.keywords");
        if !path.exists() {
            eprintln!("Skipping test: fixture not found at {:?}", path);
            return;
        }

        let sccs = SccsFile::from_path(&path).expect("Failed to parse s.keywords");

        // Should have at least one delta
        assert!(
            !sccs.header.deltas.is_empty(),
            "Should have at least one delta"
        );

        // Evaluate body
        let applied = sccs
            .compute_applied_set(1)
            .expect("Failed to compute applied set");
        let content = sccs.evaluate_body(&applied);

        // Content should contain keyword markers
        let content_str = content.join("\n");
        assert!(
            content_str.contains("%"),
            "Content should contain SCCS keyword markers"
        );
    }

    #[test]
    fn test_parse_branched_fixture() {
        let path = fixtures_path().join("s.branched");
        if !path.exists() {
            eprintln!("Skipping test: fixture not found at {:?}", path);
            return;
        }

        let sccs = SccsFile::from_path(&path).expect("Failed to parse s.branched");

        // Should have multiple deltas including branch
        assert!(sccs.header.deltas.len() >= 2, "Expected multiple deltas");

        // Should have 'b' flag enabled for branches
        assert!(sccs.branch_enabled(), "Branch flag should be enabled");

        // Look for a branch delta (SID with 4 components)
        let has_branch = sccs.header.deltas.iter().any(|d| !d.sid.is_trunk());
        assert!(has_branch, "Should have at least one branch delta");

        // Verify we can get different versions
        for delta in &sccs.header.deltas {
            let applied = sccs.compute_applied_set(delta.serial).expect(&format!(
                "Failed to compute applied set for serial {}",
                delta.serial
            ));
            let content = sccs.evaluate_body(&applied);
            // Each version should produce some content
            assert!(
                !content.is_empty(),
                "Version {} should have content",
                delta.sid
            );
        }
    }

    #[test]
    fn test_round_trip_serialization() {
        let path = fixtures_path().join("s.simple");
        if !path.exists() {
            eprintln!("Skipping test: fixture not found at {:?}", path);
            return;
        }

        let sccs = SccsFile::from_path(&path).expect("Failed to parse s.simple");

        // Serialize to bytes
        let serialized = sccs.to_bytes();

        // Parse again
        let reparsed =
            SccsFile::from_bytes(&serialized).expect("Failed to reparse serialized data");

        // Compare structure (not byte-for-byte due to checksum recalculation)
        assert_eq!(sccs.header.deltas.len(), reparsed.header.deltas.len());
        assert_eq!(sccs.header.flags.len(), reparsed.header.flags.len());
        assert_eq!(sccs.body.len(), reparsed.body.len());

        // Verify content is identical
        let applied = sccs.compute_applied_set(1).unwrap();
        let content1 = sccs.evaluate_body(&applied);

        let applied2 = reparsed.compute_applied_set(1).unwrap();
        let content2 = reparsed.evaluate_body(&applied2);

        assert_eq!(
            content1, content2,
            "Content should be identical after round-trip"
        );
    }
}
