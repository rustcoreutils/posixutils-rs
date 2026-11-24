//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! SIDs, and the per-delta statistics and type of a delta-table entry.

use std::fmt;
use std::str::FromStr;

use super::{SccsError, SccsResult};

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

impl DeltaStats {
    /// The most the s-file records for one count: its fields are five digits.
    pub const MAX_RECORDED: u32 = 99_999;
}

/// The `^As` line's `inserted/deleted/unchanged`, each count recorded as at
/// most 99 999: "if this file contains more than 99 999 lines, the number of
/// lines recorded in the header for this file shall be 99 999 for this
/// delta" (POSIX admin 84132-84133). The struct keeps the true counts, which
/// `delta` reports to the user.
impl fmt::Display for DeltaStats {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let cap = |n: u32| n.min(Self::MAX_RECORDED);
        write!(
            f,
            "{:05}/{:05}/{:05}",
            cap(self.inserted),
            cap(self.deleted),
            cap(self.unchanged)
        )
    }
}

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
