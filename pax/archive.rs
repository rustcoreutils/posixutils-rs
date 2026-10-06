//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::error::{PaxError, PaxResult};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Type of archive entry
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum EntryType {
    /// Regular file
    #[default]
    Regular,
    /// Directory
    Directory,
    /// Symbolic link
    Symlink,
    /// Hard link to another file
    Hardlink,
    /// Block device
    BlockDevice,
    /// Character device
    CharDevice,
    /// FIFO (named pipe)
    Fifo,
    /// Socket (not typically stored in archives, but recognized)
    Socket,
}

/// The header fields that describe a header rather than the file it names.
///
/// POSIX pax listopt rule 7 admits every field name in the ustar Header Block
/// and Octet-Oriented cpio Archive Entry tables as a `%(keyword)`, and defines
/// the value as the one "from the applicable header field". These four have no
/// other use -- nothing extracts them -- so they are recorded only so a
/// listing can report them.
///
/// Which variant a member carries also decides which of those keywords can be
/// answered at all: a cpio header has no `typeflag`, `version` or `chksum`,
/// and a ustar header no `c_dev`, `c_ino` or `c_nlink`, so a keyword from the
/// other table renders as nothing rather than as a fabricated zero.
///
/// A writer must never consult this. It describes the header a member was
/// *read* from, and carrying a foreign checksum or typeflag through a copy
/// would write a header that disagrees with its own contents.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceHeader {
    /// A ustar header block, which a pax member also has.
    Ustar {
        /// `magic` (offset 257, 6 octets), as stored -- "ustar\0" from a
        /// conforming writer, "ustar " from GNU tar.
        magic: [u8; 6],
        /// `version` (263, 2), as stored.
        version: [u8; 2],
        /// `chksum` (148, 8), the value the header stored.
        chksum: u64,
        /// `typeflag` (156, 1), as stored.
        typeflag: u8,
    },
    /// A cpio header, in whichever of the four flavors the reader matched.
    Cpio {
        /// The flavor, which is what `c_magic` reports. Kept as the enum
        /// rather than the magic digits because the ODC and binary forms
        /// share "070707" and only this distinguishes them.
        format: crate::formats::cpio::CpioFormat,
    },
}

/// Metadata for an archive entry
#[derive(Debug, Clone, Default)]
pub struct ArchiveEntry {
    /// Path of the file within the archive
    pub path: PathBuf,
    /// File mode (permissions)
    pub mode: u32,
    /// User ID
    pub uid: u32,
    /// Group ID
    pub gid: u32,
    /// File size in bytes
    pub size: u64,
    /// Modification time (seconds since epoch). Signed, like `time_t`: a
    /// file can predate 1970, and a pax `mtime` record can say so.
    pub mtime: i64,
    /// Modification time nanoseconds (for pax format)
    pub mtime_nsec: u32,
    /// Access time (seconds since epoch, for pax format)
    pub atime: Option<i64>,
    /// Access time nanoseconds (for pax format)
    pub atime_nsec: u32,
    /// Inode change time (seconds since epoch). Not a POSIX pax keyword -- see
    /// `ExtendedHeader::ctime` -- but carried so archives that do record one can
    /// be listed, and so `-o times` can write one.
    pub ctime: Option<i64>,
    /// Change time nanoseconds (for pax format)
    pub ctime_nsec: u32,
    /// Type of entry
    pub entry_type: EntryType,
    /// Link target for symlinks and hardlinks
    pub link_target: Option<PathBuf>,
    /// User name (optional), as bytes.
    ///
    /// Not a `String`: under `hdrcharset=BINARY` POSIX defines the `uname` and
    /// `gname` extended-header records as "unencoded binary data from the
    /// underlying system", and a lossy decode would replace a byte the archive
    /// deliberately preserved. A user or group name is a byte string on Unix
    /// for the same reason a pathname is -- see `crate::rawpath`.
    pub uname: Option<Vec<u8>>,
    /// Group name (optional), as bytes. See `uname`.
    pub gname: Option<Vec<u8>>,
    /// Device ID (for hard link tracking)
    pub dev: u64,
    /// Inode number (for hard link tracking)
    pub ino: u64,
    /// Number of hard links
    pub nlink: u32,
    /// Device major number (for block/char devices)
    pub devmajor: u32,
    /// Device minor number (for block/char devices)
    pub devminor: u32,
    /// Sum of the member's data bytes, modulo 2^32.
    ///
    /// Only the cpio "crc" format (magic 070702) needs one, and it needs it in
    /// the header -- which is written before the data. A writer that returns
    /// true from `ArchiveWriter::needs_data_checksum` asks its caller to fill
    /// this in first; every other format leaves it `None`.
    pub data_checksum: Option<u32>,
    /// The pax extended-header records this member carried that no field
    /// above already holds: `charset`, `hdrcharset`, `comment` and whatever
    /// implementation extensions the archive used.
    ///
    /// POSIX listopt rule 7 admits all of them as a `%(keyword)`, which is the
    /// only thing that reads them -- none has any effect on extraction. A
    /// `Vec` rather than a map because it is empty for almost every member and
    /// one to three entries long otherwise, and because the order records
    /// arrive in is the order that decides precedence.
    pub ext_records: Vec<(String, String)>,
    /// The header this member was read from, when it was read from one.
    ///
    /// `None` for an entry built from a file on disk, which has no header yet.
    /// Only `-o listopt=%(keyword)` consults it; see `SourceHeader`.
    pub source_header: Option<SourceHeader>,
}

impl ArchiveEntry {
    /// Create a new archive entry with default values
    pub fn new(path: PathBuf, entry_type: EntryType) -> Self {
        ArchiveEntry {
            path,
            mode: 0o644,
            uid: 0,
            gid: 0,
            size: 0,
            mtime: 0,
            mtime_nsec: 0,
            atime: None,
            atime_nsec: 0,
            ctime: None,
            ctime_nsec: 0,
            entry_type,
            link_target: None,
            uname: None,
            gname: None,
            dev: 0,
            ino: 0,
            nlink: 1,
            devmajor: 0,
            devminor: 0,
            data_checksum: None,
            ext_records: Vec::new(),
            source_header: None,
        }
    }

    /// The modification time as the unsigned seconds a ustar or cpio header
    /// field holds.
    ///
    /// POSIX: "Portable file timestamps cannot be negative. If pax encounters
    /// a file with a negative timestamp in copy or write mode, it can reject
    /// the file". Those formats have no way to say "before 1970", and storing
    /// the two's-complement bits instead dated such a file centuries ahead,
    /// silently, so the member is refused with a diagnostic. (The pax format
    /// writes an `mtime` record instead.)
    pub fn unsigned_mtime(&self) -> PaxResult<u64> {
        u64::try_from(self.mtime).map_err(|_| {
            PaxError::InvalidHeader(format!(
                "modification time {} is before 1970, which this format cannot record",
                self.mtime
            ))
        })
    }

    /// The value of an extended-header record this member carried.
    pub fn ext_record(&self, keyword: &str) -> Option<&str> {
        self.ext_records
            .iter()
            .find(|(k, _)| k == keyword)
            .map(|(_, v)| v.as_str())
    }

    /// Record an extended-header value, replacing any already held under the
    /// same keyword.
    ///
    /// Replacing is what gives POSIX's keyword precedence: a global `g` header
    /// is applied before the per-file `x` header, and `-o keyword:=value`
    /// after both, so the last writer of a keyword wins.
    pub fn set_ext_record(&mut self, keyword: &str, value: &str) {
        match self.ext_records.iter_mut().find(|(k, _)| k == keyword) {
            Some(slot) => slot.1 = value.to_string(),
            None => self
                .ext_records
                .push((keyword.to_string(), value.to_string())),
        }
    }

    /// Check if this entry is a special device file
    pub fn is_device(&self) -> bool {
        matches!(
            self.entry_type,
            EntryType::BlockDevice | EntryType::CharDevice
        )
    }

    /// Check if this is a directory
    pub fn is_dir(&self) -> bool {
        self.entry_type == EntryType::Directory
    }
}

/// Trait for reading archives
pub trait ArchiveReader {
    /// Read the next entry from the archive
    /// Returns None when the archive is exhausted
    fn read_entry(&mut self) -> PaxResult<Option<ArchiveEntry>>;

    /// Read the data for the current entry
    fn read_data(&mut self, buf: &mut [u8]) -> PaxResult<usize>;

    /// Skip the data for the current entry
    fn skip_data(&mut self) -> PaxResult<()>;

    /// Whether the reader applies the `-o keyword=value` and
    /// `-o keyword:=value` records itself. The pax reader has to, because they
    /// rank among the archive's own extended headers; any other reader leaves
    /// them to the caller (see `OptionRecords::apply`).
    fn applies_option_records(&self) -> bool {
        false
    }
}

/// Trait for writing archives
pub trait ArchiveWriter {
    /// Write an entry header to the archive
    fn write_entry(&mut self, entry: &ArchiveEntry) -> PaxResult<()>;

    /// Write data for the current entry
    fn write_data(&mut self, data: &[u8]) -> PaxResult<()>;

    /// Finish writing data for the current entry (handles padding)
    fn finish_entry(&mut self) -> PaxResult<()>;

    /// Write the archive trailer
    fn finish(&mut self) -> PaxResult<()>;

    /// Whether this format can record that a member is a hard link to another
    /// member, so that only the first occurrence needs to carry the data.
    ///
    /// cpio cannot: it has no link typeflag, and a `Hardlink` entry degrades to
    /// a regular file. A writer that returns `false` is given the full contents
    /// of every link instead, which POSIX permits ("the data shall be restored
    /// from the original file") and which is the only way not to lose it.
    fn supports_hardlinks(&self) -> bool {
        true
    }

    /// Whether a hard-link member may also carry the file's data, which is
    /// what `-o linkdata` asks for.
    ///
    /// Only the pax format allows it ("data blocks for files of typeflag 1
    /// ... may be included"); a ustar typeflag 1 header records no data, so
    /// there the option has nothing to act on.
    fn hardlinks_may_carry_data(&self) -> bool {
        false
    }

    /// Whether this format has a file type for a socket.
    ///
    /// cpio does (`C_ISSOCK`). The tar formats do not, and POSIX requires an
    /// attempt to archive one in ustar to be diagnosed; a writer that returns
    /// `false` is never handed one.
    fn supports_sockets(&self) -> bool {
        false
    }

    /// Whether `write_entry` needs `ArchiveEntry::data_checksum` filled in.
    ///
    /// True only for the cpio "crc" format, whose c_check field sits in the
    /// header ahead of the data it covers. Callers that say true here must read
    /// the member's contents once to sum them before handing over the entry.
    fn needs_data_checksum(&self) -> bool {
        false
    }
}

/// Tracks hard links during archive creation
#[derive(Debug, Default)]
pub struct HardLinkTracker {
    /// Maps (dev, ino) to the first path seen
    seen: HashMap<(u64, u64), PathBuf>,
}

impl HardLinkTracker {
    /// Create a new tracker
    pub fn new() -> Self {
        HardLinkTracker {
            seen: HashMap::new(),
        }
    }

    /// The name a multiply-linked file was first stored under, if one of its
    /// names already has been.
    pub fn lookup(&self, dev: u64, ino: u64, nlink: u32) -> Option<PathBuf> {
        if nlink <= 1 {
            return None;
        }
        self.seen.get(&(dev, ino)).cloned()
    }

    /// Note that a file's first name has been stored, as `stored`: the archive
    /// member path when writing, the destination path when copying.
    ///
    /// Separate from `lookup` because it must only happen once that name
    /// really is in the archive or the destination. Recording a file before
    /// its data was read made every later name of an unreadable file a link
    /// to a member that was never written.
    pub fn record(&mut self, dev: u64, ino: u64, nlink: u32, stored: &Path) {
        if nlink > 1 {
            self.seen
                .entry((dev, ino))
                .or_insert_with(|| stored.to_path_buf());
        }
    }
}

/// The link sets of a cpio archive: members that are names of one file.
///
/// cpio has no link typeflag. Each name of a multiply-linked file is a member
/// of its own, and what ties them together is a shared (c_dev, c_ino) with a
/// c_nlink above one -- POSIX says such files "shall be" linked again when they
/// are restored. Directories are left out: their link count only counts their
/// subdirectories. Only cpio records a link count, so for every other format
/// this finds nothing.
///
/// `T` is what the caller remembers about a set: the names extraction created
/// for it, the first name a listing showed. A set is forgotten once all c_nlink
/// of its names have gone by, so what is held is bounded by the sets still
/// open rather than by the size of the archive.
#[derive(Debug)]
pub struct LinkSets<T> {
    /// Maps (dev, ino) to what was remembered and how many names are to come
    sets: HashMap<(u64, u64), (T, u32)>,
}

impl<T> Default for LinkSets<T> {
    fn default() -> Self {
        LinkSets {
            sets: HashMap::new(),
        }
    }
}

impl<T> LinkSets<T> {
    /// The set `entry` is a name of, or `None` for a member that is not one of
    /// several names of a file.
    pub fn key(entry: &ArchiveEntry) -> Option<(u64, u64)> {
        (entry.entry_type == EntryType::Regular && entry.nlink > 1)
            .then_some((entry.dev, entry.ino))
    }

    /// What was remembered about a set an earlier name started.
    pub fn get_mut(&mut self, key: (u64, u64)) -> Option<&mut T> {
        self.sets.get_mut(&key).map(|(value, _)| value)
    }

    /// Start a set at its first name; `nlink` counts that name too.
    pub fn insert(&mut self, key: (u64, u64), nlink: u32, value: T) {
        self.sets.insert(key, (value, nlink.saturating_sub(1)));
    }

    /// Note that one more name of a started set has gone by, forgetting the
    /// set once the last has.
    pub fn name_seen(&mut self, key: (u64, u64)) {
        if let Some((_, remaining)) = self.sets.get_mut(&key) {
            *remaining = remaining.saturating_sub(1);
            if *remaining == 0 {
                self.sets.remove(&key);
            }
        }
    }
}

/// Archive format type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArchiveFormat {
    /// POSIX ustar tar format
    Ustar,
    /// POSIX cpio format
    Cpio,
    /// POSIX pax format (extended tar with extended headers)
    Pax,
}

impl std::fmt::Display for ArchiveFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArchiveFormat::Ustar => write!(f, "ustar"),
            ArchiveFormat::Cpio => write!(f, "cpio"),
            ArchiveFormat::Pax => write!(f, "pax"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn member(entry_type: EntryType, ino: u64, nlink: u32) -> ArchiveEntry {
        ArchiveEntry {
            entry_type,
            ino,
            nlink,
            ..Default::default()
        }
    }

    #[test]
    fn test_link_sets_group_names_and_forget_complete_sets() {
        let mut sets: LinkSets<&str> = LinkSets::default();
        // Only a regular file with more than one name belongs to a set.
        assert_eq!(
            LinkSets::<&str>::key(&member(EntryType::Regular, 4, 1)),
            None
        );
        assert_eq!(
            LinkSets::<&str>::key(&member(EntryType::Directory, 4, 3)),
            None
        );

        let key = LinkSets::<&str>::key(&member(EntryType::Regular, 4, 3)).unwrap();
        sets.insert(key, 3, "a");
        assert_eq!(sets.get_mut(key).copied(), Some("a"));
        sets.name_seen(key);
        assert_eq!(sets.get_mut(key).copied(), Some("a"));
        // The third name completes the set.
        sets.name_seen(key);
        assert_eq!(sets.get_mut(key), None);
    }
}
