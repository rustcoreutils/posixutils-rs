//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The p-file protocol: one reader, one writer, one mode.
//!
//! The read-parse-filter preamble was written out at seven sites. The write
//! side was worse: `get -e` appended and chmod'ed 0644, `delta` truncated with
//! `File::create` and set no mode at all, and `unget` used `fs::write` with
//! its own line formatter. So a p-file created 0644 silently changed mode the
//! first time a partial `unget` or a `delta` left other entries pending.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use plib::sccsfile::{paths, PfileEntry, Sid};

/// The mode a p-file carries. It records a pending edit for other users to
/// see, so it is world-readable and owner-writable.
const PFILE_MODE: u32 = 0o644;

/// Read the p-file beside `sfile`, or an empty list if there is none.
pub fn read(sfile: &Path) -> io::Result<Vec<PfileEntry>> {
    let path = paths::pfile_from_sfile(sfile);
    let contents = match fs::read_to_string(&path) {
        Ok(c) => c,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => return Err(e),
    };
    plib::sccsfile::parse_pfile(&contents)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))
}

/// The p-file's path, for callers that need to test existence or remove it.
pub fn path(sfile: &Path) -> PathBuf {
    paths::pfile_from_sfile(sfile)
}

/// Replace the p-file with `entries`, removing it entirely when none remain.
///
/// An empty p-file and an absent one mean different things to `sact` and to
/// `get -e`, so the empty case must unlink rather than truncate.
pub fn write(sfile: &Path, entries: &[PfileEntry]) -> io::Result<()> {
    let path = paths::pfile_from_sfile(sfile);
    if entries.is_empty() {
        return match fs::remove_file(&path) {
            Ok(()) => Ok(()),
            Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(()),
            Err(e) => Err(e),
        };
    }

    let mut body = String::new();
    for e in entries {
        body.push_str(&e.to_line());
        body.push('\n');
    }
    crate::sfio::write_replacing(&path, body.as_bytes(), PFILE_MODE)
}

/// Append one pending-edit record.
pub fn append(sfile: &Path, entry: &PfileEntry) -> io::Result<()> {
    let mut entries = read(sfile)?;
    entries.push(entry.clone());
    write(sfile, &entries)
}

/// The entry whose new SID is `new_sid`, if the edit is already pending.
///
/// `get -e` uses this to refuse a second edit of the same SID; the `j` flag is
/// what allows the concurrent case, and it is checked by the caller.
pub fn find_by_new_sid<'a>(entries: &'a [PfileEntry], new_sid: &Sid) -> Option<&'a PfileEntry> {
    entries.iter().find(|e| e.new_sid == *new_sid)
}
