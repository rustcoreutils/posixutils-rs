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

use super::{Login, SccsDateTime, SccsError, SccsResult, Sid, SidList};

/// Entry in a p-file (edit lock)
#[derive(Debug, Clone)]
pub struct PfileEntry {
    pub old_sid: Sid, // SID being edited (got SID)
    pub new_sid: Sid, // SID that will be created
    pub user: Login,  // User who holds the lock
    pub datetime: SccsDateTime,
    pub included: Option<SidList>, // -i option argument
    pub excluded: Option<SidList>, // -x option argument
}

impl PfileEntry {
    /// Parse a p-file entry from a line.
    ///
    /// Unlike an s-file, a p-file is written whole from its entries each
    /// time one is added or removed, so what is read here is written back.
    /// Each field is therefore checked as it would be if it were new.
    pub fn parse(line: &[u8]) -> SccsResult<Self> {
        let invalid = || {
            SccsError::InvalidFormat(format!(
                "Invalid p-file entry: {}",
                String::from_utf8_lossy(line)
            ))
        };
        let parts: Vec<&[u8]> = line
            .split(u8::is_ascii_whitespace)
            .filter(|part| !part.is_empty())
            .collect();
        if parts.len() < 4 {
            return Err(invalid());
        }
        // Every field but the login is ASCII.
        let text = |part: &[u8]| {
            std::str::from_utf8(part)
                .map(str::to_owned)
                .map_err(|_| invalid())
        };

        let old_sid: Sid = text(parts[0])?.parse()?;
        let new_sid: Sid = text(parts[1])?.parse()?;
        let user = Login::new(parts[2])?;
        let time = match parts.get(4) {
            Some(time) => text(time)?,
            None => "00:00:00".to_owned(),
        };
        let datetime = SccsDateTime::parse(&text(parts[3])?, &time)?;

        // Parse optional -i and -x, accepting both the attached form (`-i1.2`,
        // as written by SCCS / CSSC) and the separated form (`-i 1.2`).
        let mut included = None;
        let mut excluded = None;
        let mut i = 5;
        while i < parts.len() {
            let slot = match parts[i].get(..2) {
                Some(b"-i") => &mut included,
                Some(b"-x") => &mut excluded,
                _ => {
                    i += 1;
                    continue;
                }
            };
            let attached = &parts[i][2..];
            if !attached.is_empty() {
                *slot = Some(SidList::new(attached)?);
            } else if let Some(next) = parts.get(i + 1) {
                *slot = Some(SidList::new(next)?);
                i += 1;
            }
            i += 1;
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

    /// Format as a p-file line, without its newline.
    pub fn to_line(&self) -> Vec<u8> {
        let mut line = format!("{} {} ", self.old_sid, self.new_sid).into_bytes();
        line.extend_from_slice(&self.user);
        line.extend_from_slice(format!(" {}", self.datetime).as_bytes());
        for (option, list) in [(&b" -i"[..], &self.included), (b" -x", &self.excluded)] {
            if let Some(list) = list {
                line.extend_from_slice(option);
                line.extend_from_slice(list);
            }
        }
        line
    }
}

/// Parse all entries from a p-file
pub fn parse_pfile(contents: &[u8]) -> SccsResult<Vec<PfileEntry>> {
    contents
        .split(|&b| b == b'\n')
        .filter(|line| !line.trim_ascii().is_empty())
        .map(PfileEntry::parse)
        .collect()
}
