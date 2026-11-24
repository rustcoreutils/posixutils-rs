//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The parsed records of an s-file: delta entries, flags, the header.

use super::{
    CommentLine, DeltaStats, DeltaType, Login, MrNumber, SccsDateTime, SccsError, SccsResult, Sid,
};

/// A single delta entry in the delta table
#[derive(Debug, Clone)]
pub struct DeltaEntry {
    pub delta_type: DeltaType,
    pub sid: Sid,
    pub datetime: SccsDateTime,
    /// Login of the delta's author. Like every text an s-file stores, it
    /// is the bytes written, which need not be UTF-8.
    pub user: Login,
    pub serial: u16,      // Serial number (unique ID within file)
    pub pred_serial: u16, // Predecessor serial (0 for first delta)
    pub stats: DeltaStats,
    pub included: Vec<u16>,         // Serial numbers of included deltas (^Ai)
    pub excluded: Vec<u16>,         // Serial numbers of excluded deltas (^Ax)
    pub ignored: Vec<u16>,          // Serial numbers of ignored deltas (^Ag)
    pub mr_numbers: Vec<MrNumber>,  // Modification request numbers (^Am)
    pub comments: Vec<CommentLine>, // Comment lines (^Ac)
}

impl DeltaEntry {
    /// A delta with what its `^Ad` line holds, and nothing else yet: no
    /// counts, lists, MRs or comment.
    pub fn new(
        delta_type: DeltaType,
        sid: Sid,
        datetime: SccsDateTime,
        user: Login,
        serial: u16,
        pred_serial: u16,
    ) -> Self {
        DeltaEntry {
            delta_type,
            sid,
            datetime,
            user,
            serial,
            pred_serial,
            stats: DeltaStats::default(),
            included: Vec::new(),
            excluded: Vec::new(),
            ignored: Vec::new(),
            mr_numbers: Vec::new(),
            comments: Vec::new(),
        }
    }
}

/// The release list carried by the `l` flag (POSIX 84047-84054).
///
/// The `a` form locks *every* release, which is the opposite of locking none.
/// Encoding `a` as an empty `Vec` made the two indistinguishable, so `:LK:`
/// reported `none` for the most restrictive setting a file can carry. The
/// distinction belongs in the type, not in a convention about vector length.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReleaseLock {
    /// `l a` - every release is locked.
    All,
    /// `l <list>` - only the named releases are locked.
    List(Vec<u16>),
}

impl ReleaseLock {
    /// True if deltas to `rel` are forbidden.
    pub fn locks(&self, rel: u16) -> bool {
        match self {
            ReleaseLock::All => true,
            ReleaseLock::List(v) => v.contains(&rel),
        }
    }

    /// The flag value as it appears in the s-file and in `prs -d:LK:`.
    pub fn value_string(&self) -> String {
        match self {
            ReleaseLock::All => "a".to_string(),
            ReleaseLock::List(v) => v
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(","),
        }
    }
}

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
    IdKeywordError(Option<Vec<u8>>),
    /// 'j' - Joint edit allowed
    JointEdit,
    /// 'l' <list> - Locked releases (`a` == all, see [`ReleaseLock`])
    LockedReleases(ReleaseLock),
    /// 'm' <name> - Module name
    ModuleName(Vec<u8>),
    /// 'n' - Create null deltas
    NullDelta,
    /// 'q' <text> - User-defined flag for %Q%
    QText(Vec<u8>),
    /// 't' <type> - Module type for %Y%
    ModuleType(Vec<u8>),
    /// 'v' [pgm] - MR validation program
    MrValidation(Option<Vec<u8>>),
    /// Unknown flag (for lossless preservation)
    Unknown(char, Vec<u8>),
}

impl SccsFlag {
    /// Parse a flag from its character and value. The value is the bytes the
    /// s-file holds: a text value keeps them as they are, and a numeric one
    /// that is not a number is an error rather than a zero.
    pub fn parse(flag_char: char, value: &[u8]) -> SccsResult<Self> {
        let value = value.trim_ascii();
        let number = |what: char| -> SccsResult<&str> {
            std::str::from_utf8(value).map_err(|_| {
                SccsError::InvalidFlag(format!("{}{}", what, String::from_utf8_lossy(value)))
            })
        };
        let optional = || (!value.is_empty()).then(|| value.to_vec());
        match flag_char {
            'b' => Ok(SccsFlag::BranchEnabled),
            'c' => {
                let text = number('c')?;
                let ceil = text
                    .parse()
                    .map_err(|_| SccsError::InvalidFlag(format!("c{}", text)))?;
                Ok(SccsFlag::Ceiling(ceil))
            }
            'd' => {
                let sid = number('d')?.parse()?;
                Ok(SccsFlag::DefaultSid(sid))
            }
            'e' => {
                let enc = number('e').ok().and_then(|t| t.parse().ok()).unwrap_or(0);
                Ok(SccsFlag::Encoded(enc))
            }
            'f' => {
                let text = number('f')?;
                let floor = text
                    .parse()
                    .map_err(|_| SccsError::InvalidFlag(format!("f{}", text)))?;
                Ok(SccsFlag::Floor(floor))
            }
            'i' => Ok(SccsFlag::IdKeywordError(optional())),
            'j' => Ok(SccsFlag::JointEdit),
            'l' => {
                let text = number('l')?;
                if text == "a" {
                    Ok(SccsFlag::LockedReleases(ReleaseLock::All))
                } else {
                    let releases: Result<Vec<u16>, _> =
                        text.split(',').map(|s| s.trim().parse()).collect();
                    Ok(SccsFlag::LockedReleases(ReleaseLock::List(
                        releases.map_err(|_| SccsError::InvalidFlag(format!("l{}", text)))?,
                    )))
                }
            }
            'm' => Ok(SccsFlag::ModuleName(value.to_vec())),
            'n' => Ok(SccsFlag::NullDelta),
            'q' => Ok(SccsFlag::QText(value.to_vec())),
            't' => Ok(SccsFlag::ModuleType(value.to_vec())),
            'v' => Ok(SccsFlag::MrValidation(optional())),
            _ => Ok(SccsFlag::Unknown(flag_char, value.to_vec())),
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

    /// The flag's value as the s-file stores it: empty for a flag that
    /// takes none.
    pub fn value(&self) -> Vec<u8> {
        match self {
            SccsFlag::BranchEnabled | SccsFlag::JointEdit | SccsFlag::NullDelta => Vec::new(),
            SccsFlag::Ceiling(n) | SccsFlag::Floor(n) => n.to_string().into_bytes(),
            SccsFlag::DefaultSid(sid) => sid.to_string().into_bytes(),
            SccsFlag::Encoded(n) => n.to_string().into_bytes(),
            SccsFlag::LockedReleases(lock) => lock.value_string().into_bytes(),
            SccsFlag::IdKeywordError(s) | SccsFlag::MrValidation(s) => {
                s.clone().unwrap_or_default()
            }
            SccsFlag::ModuleName(s)
            | SccsFlag::QText(s)
            | SccsFlag::ModuleType(s)
            | SccsFlag::Unknown(_, s) => s.clone(),
        }
    }

    /// Human-readable description of this flag for the `prs` `:FL:` data
    /// keyword, matching historical SCCS / CSSC output. Value-bearing flags
    /// render as `name<tab>value`. Returns `None` for the `e` (encoded) flag,
    /// which is not part of the `:FL:` listing.
    pub fn prs_fl_line(&self) -> Option<Vec<u8>> {
        let (name, has_value) = match self {
            SccsFlag::BranchEnabled => ("branch".to_string(), false),
            SccsFlag::Ceiling(_) => ("ceiling".to_string(), true),
            SccsFlag::DefaultSid(_) => ("default SID".to_string(), true),
            SccsFlag::Encoded(_) => return None,
            SccsFlag::Floor(_) => ("floor".to_string(), true),
            SccsFlag::IdKeywordError(_) => ("id keywd err/warn".to_string(), false),
            SccsFlag::JointEdit => ("joint edit".to_string(), false),
            SccsFlag::LockedReleases(_) => ("locked releases".to_string(), true),
            SccsFlag::ModuleName(_) => ("module".to_string(), true),
            SccsFlag::NullDelta => ("null delta".to_string(), false),
            SccsFlag::QText(_) => ("csect name".to_string(), true),
            SccsFlag::ModuleType(_) => ("type".to_string(), true),
            SccsFlag::MrValidation(_) => ("validate MRs".to_string(), true),
            SccsFlag::Unknown(c, _) => (c.to_string(), true),
        };
        let mut line = name.into_bytes();
        if has_value {
            line.push(b'\t');
            line.extend_from_slice(&self.value());
        }
        Some(line)
    }
}

/// One line of an s-file: where its bytes are in the file's image, without
/// the newline that ends it, and what the parser took it for.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Line {
    pub(super) start: usize,
    pub(super) len: usize,
    pub(super) kind: Kind,
}

/// What a line of an s-file is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Kind {
    /// `^Ah`: the checksum, the first line.
    Checksum,
    /// `^As`, `^Ad`: the two lines that open a delta's entry.
    Stats,
    DeltaHead,
    /// `^Ai`, `^Ax`, `^Ag`, `^Am`, `^Ac`: the optional lines of an entry.
    Include,
    Exclude,
    Ignore,
    Mr,
    Comment,
    /// `^Ae`: the end of an entry.
    DeltaEnd,
    /// `^Au`, a line of the user list, `^AU`.
    UsersBegin,
    User,
    UsersEnd,
    /// `^Af`, with the flag's letter.
    Flag(char),
    /// `^At`, a line of the descriptive text, `^AT`.
    DescBegin,
    Desc,
    DescEnd,
    /// `^AI n`, `^AD n`, `^AE n`: the block markers of the body.
    Insert(u16),
    Delete(u16),
    End(u16),
    /// A line of the body's text.
    Text,
    /// A line of the body that begins like a control line and is neither
    /// text nor a usable block marker: a marker with no serial number, or
    /// some other control line. The body cannot be read past it.
    BadControl,
    /// A header line no record accounts for. It is read around, and kept
    /// where it is when the file is rewritten.
    Stray,
}

/// SCCS file header (everything before the body)
#[derive(Debug, Clone, Default)]
pub struct SccsHeader {
    pub checksum: u16,
    pub deltas: Vec<DeltaEntry>,
    pub users: Vec<Vec<u8>>,
    pub flags: Vec<SccsFlag>,
    pub descriptive_text: Vec<Vec<u8>>,
}
