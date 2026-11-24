//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The p-file: one line per edit in progress.

use super::{SccsDateTime, SccsError, SccsResult, Sid};

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

        // Parse optional -i and -x, accepting both the attached form (`-i1.2`,
        // as written by SCCS / CSSC) and the separated form (`-i 1.2`).
        let mut included = None;
        let mut excluded = None;
        let mut i = 5;
        while i < parts.len() {
            if let Some(rest) = parts[i].strip_prefix("-i") {
                if !rest.is_empty() {
                    included = Some(rest.to_string());
                    i += 1;
                } else if i + 1 < parts.len() {
                    included = Some(parts[i + 1].to_string());
                    i += 2;
                } else {
                    i += 1;
                }
            } else if let Some(rest) = parts[i].strip_prefix("-x") {
                if !rest.is_empty() {
                    excluded = Some(rest.to_string());
                    i += 1;
                } else if i + 1 < parts.len() {
                    excluded = Some(parts[i + 1].to_string());
                    i += 2;
                } else {
                    i += 1;
                }
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
            line.push_str(&format!(" -i{}", inc));
        }
        if let Some(ref exc) = self.excluded {
            line.push_str(&format!(" -x{}", exc));
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
