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
use crate::formats::ustar::{is_zero_block, BLOCK_SIZE};
use crate::formats::PaxReader;
use crate::modes::write::WriteOptions;
use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::ops::Range;
use std::path::PathBuf;

/// Append files to an existing archive
pub fn append_to_archive(
    archive_path: &PathBuf,
    files: &mut crate::modes::write::FileNames<'_>,
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

    let family = archive_family(&mut file)?;

    // Per POSIX, an explicit `-x` that names a format different from the
    // existing archive is an error -- pax must not silently coerce the new
    // members into the archive's format. ustar and pax are one family here: a
    // pax archive whose members needed no extended header is byte for byte a
    // ustar archive, and a ustar member is a valid pax member.
    if let Some(requested) = requested_format {
        if is_tar(requested) != is_tar(family) {
            return Err(PaxError::InvalidFormat(format!(
                "cannot append in {} format to an existing {} archive",
                requested,
                if is_tar(family) { "tar" } else { "cpio" }
            )));
        }
    }
    if !is_tar(family) {
        return Err(PaxError::InvalidFormat(
            "appending to cpio archives is not supported".to_string(),
        ));
    }

    // Walk the archive once, the way reading it does: that finds where its
    // end-of-archive indicator begins, whether it uses pax headers, and -- for
    // -u -- what times it already records.
    let scan = scan_tar_archive(&mut file, update)?;
    let format = requested_format.unwrap_or(if scan.has_extended_header {
        ArchiveFormat::Pax
    } else {
        ArchiveFormat::Ustar
    });

    // -u decides per member during the traversal below, not on the operands:
    // an operand is usually a directory, and what has to be compared is each
    // member name it expands to, after -s has had its say.
    options.update_times = scan.mtimes;
    options.archive_id = Some(crate::modes::write::file_id(&file.metadata()?));

    // Read before they are written over: the global headers that follow a
    // dangling `x` header go back in at the new end, ahead of the new members.
    let globals = read_ranges(&mut file, &scan.trailing_globals)?;

    // Seek to the append position
    file.seek(SeekFrom::Start(scan.end))?;

    // Append through the same traversal engine that -w uses. This file used to
    // carry its own 441-line copy of it, which had gone stale: it had never
    // gained -s substitutions, -o invalid= handling, -o linkdata, or the
    // sub-second/atime/ctime fields, and it built the pax writer with
    // PaxWriter::new so every -o option was discarded.
    let written = {
        let mut blocked = BlockedWriter::new(&mut file, record_size);
        blocked
            .write_all(&globals)
            .map_err(PaxError::ArchiveWrite)?;
        crate::modes::write::create_archive(blocked, files, format, options)
    };
    // EOF on /dev/tty under -i ends the run with the archive finished, and
    // the end of it is still the end of the file.
    if !matches!(written, Ok(()) | Err(PaxError::TtyEof)) {
        return written;
    }

    // Discard whatever remains of the old archive. The previous end-of-archive
    // marker and its record padding sit beyond what we just wrote whenever the
    // appended members are shorter, and would otherwise survive as trailing
    // garbage past the new marker.
    let end = file.stream_position()?;
    file.set_len(end)?;

    // A failure to close is the last word on whether the archive was written.
    crate::blocked_io::close_file(file).map_err(PaxError::ArchiveWrite)?;
    written
}

/// What walking an existing tar-family archive found.
struct TarScan {
    /// Where the end-of-archive indicator begins, which is where new members go.
    end: u64,
    /// Whether any member carries a pax extended header.
    has_extended_header: bool,
    /// The `g` headers past `end`; see `PaxReader::trailing_global_headers`.
    trailing_globals: Vec<Range<u64>>,
    /// The latest modification time recorded for each member name, when -u
    /// asked for them.
    mtimes: Option<HashMap<PathBuf, i64>>,
}

/// Walk a tar-family archive member by member with the reader `pax -r` uses.
///
/// Finding the end by any other parser is how -a used to destroy data: one
/// that stepped over members by the ustar size field alone did not know that a
/// pax `size=` record overrides it -- which is how a member over 8 GiB is
/// written -- or that some types carry no data at all, so it took a point in
/// the middle of a member for the end and wrote over the rest of the archive.
/// Member data is seeked over, not read.
///
/// Unlike reading, the walk steps over a lone zero block: one is not the
/// end-of-archive indicator, and writing there destroyed every member after
/// it. And it is silent -- what the reader says about members it cannot
/// extract is about extraction, which this is not.
fn scan_tar_archive(file: &mut File, want_mtimes: bool) -> PaxResult<TarScan> {
    crate::error::quietly(|| walk_tar_archive(file, want_mtimes))
}

fn walk_tar_archive(file: &mut File, want_mtimes: bool) -> PaxResult<TarScan> {
    file.seek(SeekFrom::Start(0))?;
    let mut archive = PaxReader::seekable(&mut *file).stepping_over_lone_zero_blocks();
    let mut mtimes: Option<HashMap<PathBuf, i64>> = want_mtimes.then(HashMap::new);

    // A name can appear more than once -- that is what appending does -- and
    // the most recent copy is the one an extraction would produce, so it is
    // the one -u has to compare against.
    while let Some(entry) = archive.read_entry()? {
        if let Some(mtimes) = mtimes.as_mut() {
            // Keyed by the name's bytes: a lossy rendering made distinct
            // names that are not UTF-8 one name.
            let key = crate::rawpath::trim_trailing_slashes(&entry.path).to_path_buf();
            mtimes
                .entry(key)
                .and_modify(|t| *t = std::cmp::max(*t, entry.mtime))
                .or_insert(entry.mtime);
        }
    }

    Ok(TarScan {
        end: archive.end_of_archive(),
        has_extended_header: archive.saw_extended_header(),
        trailing_globals: archive.trailing_global_headers().to_vec(),
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
///
/// A first block of zeros is how an archive with no members begins -- its
/// end-of-archive indicator -- but it is also how a disk image or any other
/// file can begin, and writing there destroys it. Such a file counts as an
/// archive only if there is nothing but zeros in it.
fn archive_family(file: &mut File) -> PaxResult<ArchiveFormat> {
    let mut header = [0u8; BLOCK_SIZE];
    file.read_exact(&mut header)?;
    if is_zero_block(&header) && !rest_is_zero(file)? {
        return Err(PaxError::InvalidFormat(
            "unable to detect archive format".to_string(),
        ));
    }
    file.seek(SeekFrom::Start(0))?;
    crate::detect_format_from_bytes(&header)
}

/// Whether everything from the current position to the end of `file` is zero.
fn rest_is_zero(file: &mut File) -> PaxResult<bool> {
    let mut buf = vec![0u8; 64 * 1024];
    loop {
        match file.read(&mut buf) {
            Ok(0) => return Ok(true),
            Ok(n) if buf[..n].iter().all(|&b| b == 0) => {}
            Ok(_) => return Ok(false),
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => {}
            Err(e) => return Err(e.into()),
        }
    }
}

/// Whether `format` is one of the tar family, ustar and pax.
fn is_tar(format: ArchiveFormat) -> bool {
    format != ArchiveFormat::Cpio
}

/// The bytes of `file` in `ranges`, in order.
fn read_ranges(file: &mut File, ranges: &[Range<u64>]) -> PaxResult<Vec<u8>> {
    let mut out = Vec::new();
    for range in ranges {
        file.seek(SeekFrom::Start(range.start))?;
        let start = out.len();
        out.resize(start + (range.end - range.start) as usize, 0);
        file.read_exact(&mut out[start..])?;
    }
    Ok(out)
}
