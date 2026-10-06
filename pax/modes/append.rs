//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Append mode implementation - add files to existing archives
//!
//! Append mode works by:
//! 1. Opening the existing archive for read+write
//! 2. Detecting the archive format
//! 3. Walking the members as reading does, to find where the end-of-archive
//!    marker begins
//! 4. Positioning write cursor there
//! 5. Writing new entries using existing write infrastructure
//! 6. Writing new end-of-archive marker
//!
//! Note: Only ustar and pax formats are supported. Appending to cpio
//! is problematic due to device/inode conflicts (per POSIX).

use crate::archive::{ArchiveFormat, ArchiveReader};
use crate::blocked_io::BlockedWriter;
use crate::error::{PaxError, PaxResult};
use crate::formats::ustar::BLOCK_SIZE;
use crate::formats::PaxReader;
use crate::modes::write::WriteOptions;
use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom};
use std::path::PathBuf;

/// Append files to an existing archive
pub fn append_to_archive(
    archive_path: &PathBuf,
    files: &[PathBuf],
    options: &mut WriteOptions,
    requested_format: Option<ArchiveFormat>,
    record_size: usize,
    update: bool,
) -> PaxResult<()> {
    // Open archive for read+write
    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(archive_path)?;

    // Walk the archive once, the way reading it does: that finds what format
    // it is in, where its end-of-archive indicator begins, and -- for -u --
    // what times it already records.
    let scan = match archive_family(&mut file)? {
        ArchiveFormat::Cpio => None,
        _ => Some(scan_tar_archive(&mut file, update)?),
    };
    let format = match &scan {
        None => ArchiveFormat::Cpio,
        Some(scan) if scan.has_extended_header => ArchiveFormat::Pax,
        Some(_) => ArchiveFormat::Ustar,
    };

    // Per POSIX, an explicit `-x` that names a format different from the existing
    // archive is an error — pax must not silently coerce the new members into the
    // archive's format.
    if let Some(requested) = requested_format {
        if requested != format {
            return Err(PaxError::InvalidFormat(format!(
                "cannot append in {} format to an existing {} archive",
                requested, format
            )));
        }
    }

    // Only support ustar and pax for append
    let Some(scan) = scan else {
        return Err(PaxError::InvalidFormat(
            "appending to cpio archives is not supported".to_string(),
        ));
    };

    // -u decides per member during the traversal below, not on the operands:
    // an operand is usually a directory, and what has to be compared is each
    // member name it expands to, after -s has had its say.
    options.update_times = scan.mtimes;

    // Seek to the append position
    file.seek(SeekFrom::Start(scan.end))?;

    // Append through the same traversal engine that -w uses. This file used to
    // carry its own 441-line copy of it, which had gone stale: it had never
    // gained -s substitutions, -o invalid= handling, -o linkdata, or the
    // sub-second/atime/ctime fields, and it built the pax writer with
    // PaxWriter::new so every -o option was discarded.
    {
        let blocked = BlockedWriter::new(&mut file, record_size);
        crate::modes::write::create_archive(blocked, files, format, options)?;
    }

    // Discard whatever remains of the old archive. The previous end-of-archive
    // marker and its record padding sit beyond what we just wrote whenever the
    // appended members are shorter, and would otherwise survive as trailing
    // garbage past the new marker.
    let end = file.stream_position()?;
    file.set_len(end)?;

    Ok(())
}

/// What walking an existing tar-family archive found.
struct TarScan {
    /// Where the end-of-archive indicator begins, which is where new members go.
    end: u64,
    /// Whether any member carries a pax extended header.
    has_extended_header: bool,
    /// The latest modification time recorded for each member name, when -u
    /// asked for them.
    mtimes: Option<HashMap<PathBuf, u64>>,
}

/// Walk a tar-family archive member by member with the reader `pax -r` uses.
///
/// Finding the end by any other parser is how -a used to destroy data: one
/// that stepped over members by the ustar size field alone did not know that a
/// pax `size=` record overrides it -- which is how a member over 8 GiB is
/// written -- or that some types carry no data at all, so it took a point in
/// the middle of a member for the end and wrote over the rest of the archive.
/// Member data is seeked over, not read.
fn scan_tar_archive(file: &mut File, want_mtimes: bool) -> PaxResult<TarScan> {
    file.seek(SeekFrom::Start(0))?;
    let mut archive = PaxReader::seekable(&mut *file);
    let mut mtimes: Option<HashMap<PathBuf, u64>> = want_mtimes.then(HashMap::new);

    // A name can appear more than once -- that is what appending does -- and
    // the most recent copy is the one an extraction would produce, so it is
    // the one -u has to compare against.
    while let Some(entry) = archive.read_entry()? {
        if let Some(mtimes) = mtimes.as_mut() {
            // A directory is stored as "dir/" but named "dir" while being
            // written, so the trailing slash comes off here and the lookup
            // side spells it the same way.
            let name = entry.path.to_string_lossy();
            let key = PathBuf::from(name.trim_end_matches('/'));
            mtimes
                .entry(key)
                .and_modify(|t| *t = std::cmp::max(*t, entry.mtime))
                .or_insert(entry.mtime);
        }
    }

    Ok(TarScan {
        end: archive.end_of_archive(),
        has_extended_header: archive.saw_extended_header(),
        mtimes,
    })
}

/// The format family of the archive, from its first block.
///
/// Within the tar family, what the archive already *is* -- which is what new
/// members must be written as and what an explicit `-x` has to match -- is
/// deliberately not the question the reader asks. Any ustar-magic archive is
/// *read* as pax, because a pax archive is a ustar archive with extra headers
/// and the pax reader handles a member without them identically. Appending has
/// to preserve what is there instead: writing pax members into a plain ustar
/// archive would change its format, and refusing to append ustar members to one
/// would be wrong the other way. So the archive counts as pax only if some
/// member actually carries an extended header, which `scan_tar_archive`
/// reports. Judging that from the first block alone was a bug: a pax archive
/// whose first member needs no extended header begins with an ordinary ustar
/// header.
fn archive_family(file: &mut File) -> PaxResult<ArchiveFormat> {
    let mut header = [0u8; BLOCK_SIZE];
    file.read_exact(&mut header)?;
    file.seek(SeekFrom::Start(0))?;
    crate::detect_format_from_bytes(&header)
}
