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
use std::path::PathBuf;
use std::sync::Arc;

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
    /// The member's POSIX.1e access ACL, in the text form a pax archive carries it in
    /// (`SCHILY.acl.access`: `user::rw-,user:alice:r--:1000,...`). Only one that says more
    /// than the mode is written; only the pax format has a place for it.
    pub acl_access: Option<String>,
    /// A directory member's default ACL (`SCHILY.acl.default`), in the same form.
    pub acl_default: Option<String>,
    /// The member's NFSv4-style ACL -- a macOS one, or a Linux NFSv4 mount's -- in the text
    /// form libarchive writes (`SCHILY.acl.ace`: `owner@:rwxpaARWcCos::allow,...`). Only one
    /// that says more than the mode is written.
    pub acl_ace: Option<String>,
    /// The member's extended attributes, as the records of a pax archive spell them
    /// (`XattrRecord`), in the order they came: decoded only where `-p e` applies them. Only
    /// the pax format has a place for them.
    pub xattrs: Vec<XattrRecord>,
    /// The pax extended-header records this member carried that no field
    /// above already holds: `charset`, `hdrcharset`, `comment` and whatever
    /// implementation extensions the archive used.
    ///
    /// POSIX listopt rule 7 admits all of them as a `%(keyword)`, which is the
    /// only thing that reads them -- none has any effect on extraction.
    pub ext_records: ExtRecords,
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
            acl_access: None,
            acl_default: None,
            acl_ace: None,
            xattrs: Vec::new(),
            ext_records: ExtRecords::default(),
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
        self.ext_records.get(keyword)
    }

    /// Record an extended-header value, replacing any already held under the
    /// same keyword.
    ///
    /// Replacing is what gives POSIX's keyword precedence: a global `g` header
    /// is applied before the per-file `x` header, and `-o keyword:=value`
    /// after both, so the last writer of a keyword wins.
    pub fn set_ext_record(&mut self, keyword: &str, value: &str) {
        self.ext_records
            .own
            .insert(keyword.to_string(), Some(value.to_string()));
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

/// One extended attribute, as a pax extended-header record carries it: `keyword` is the
/// record's whole keyword -- `SCHILY.xattr.` and the name as it stands, as GNU tar and star
/// write it, or `LIBARCHIVE.xattr.` and the name %-encoded, with the value base64 -- held as
/// bytes, since a name need not be UTF-8.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct XattrRecord {
    pub keyword: Vec<u8>,
    pub value: Vec<u8>,
}

/// The extended-header records a member carried that no typed field of
/// `ArchiveEntry` holds, as a map from keyword to value.
///
/// Two layers, because a pax archive's global `g` records apply to every
/// member after them: copying them into each member cost time and memory
/// proportional to the global records times the members, so they are held
/// once and shared. A member's own records lie over them.
#[derive(Debug, Clone, Default)]
pub struct ExtRecords {
    /// The global records in force for this member, shared with every other
    /// member they apply to.
    shared: Arc<HashMap<String, String>>,
    /// The member's own records. `None` is a keyword its own header deleted,
    /// which hides the shared value.
    own: HashMap<String, Option<String>>,
}

impl ExtRecords {
    /// The value in force for `keyword`.
    pub fn get(&self, keyword: &str) -> Option<&str> {
        match self.own.get(keyword) {
            Some(value) => value.as_deref(),
            None => self.shared.get(keyword).map(String::as_str),
        }
    }

    /// Lay this member's records over `shared`.
    pub fn share(&mut self, shared: &Arc<HashMap<String, String>>) {
        self.shared = Arc::clone(shared);
    }

    /// Hide the shared value of `keyword`, unless this member set its own.
    pub fn hide(&mut self, keyword: &str) {
        self.own.entry(keyword.to_string()).or_insert(None);
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

    /// Done reading: the archive was read to its end when `reached_end`,
    /// and otherwise stopped part way. See `ArchiveStream::finish`.
    fn finish(&mut self, _reached_end: bool) -> PaxResult<()> {
        Ok(())
    }
}

/// A boxed reader, as `formats::open_reader` returns for a format detected at
/// run time.
impl<T: ArchiveReader + ?Sized> ArchiveReader for Box<T> {
    fn read_entry(&mut self) -> PaxResult<Option<ArchiveEntry>> {
        (**self).read_entry()
    }

    fn read_data(&mut self, buf: &mut [u8]) -> PaxResult<usize> {
        (**self).read_data(buf)
    }

    fn skip_data(&mut self) -> PaxResult<()> {
        (**self).skip_data()
    }

    fn applies_option_records(&self) -> bool {
        (**self).applies_option_records()
    }

    fn finish(&mut self, reached_end: bool) -> PaxResult<()> {
        (**self).finish(reached_end)
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

    /// Whether the format has a place for a member's ACLs and extended
    /// attributes (`ArchiveEntry::acl_access`, `ArchiveEntry::xattrs`). Only the
    /// pax format does; a writer that returns `false` is handed none, and no
    /// file's are read for it.
    fn supports_acls(&self) -> bool {
        false
    }
}

/// Tracks hard links during archive creation and copying.
///
/// A file is remembered for the whole run, not only until `nlink` of its
/// names have gone by: a name list can reach the same name twice (`find tree |
/// pax -w` lists it and walks it), and a file forgotten before the repeat would
/// be stored again in full -- splitting a hard-linked pair on extraction,
/// depending on the order.
///
/// `T` is what is remembered of the first name: the archive member path when
/// writing; when copying, the destination path and the identity of the copy
/// made there.
#[derive(Debug)]
pub struct HardLinkTracker<T = PathBuf> {
    /// What was remembered of each file's first name, by (dev, ino)
    stored: HashMap<(u64, u64), T>,
}

impl<T> Default for HardLinkTracker<T> {
    fn default() -> Self {
        HardLinkTracker {
            stored: HashMap::new(),
        }
    }
}

impl<T> HardLinkTracker<T> {
    /// Create a new tracker
    pub fn new() -> Self {
        Self::default()
    }

    /// What was remembered of the name a multiply-linked file was first
    /// stored under, if one of its names already has been.
    pub fn lookup(&self, dev: u64, ino: u64, nlink: u32) -> Option<&T> {
        if nlink <= 1 {
            return None;
        }
        self.stored.get(&(dev, ino))
    }

    /// `lookup`, to change what was remembered.
    pub fn lookup_mut(&mut self, dev: u64, ino: u64, nlink: u32) -> Option<&mut T> {
        if nlink <= 1 {
            return None;
        }
        self.stored.get_mut(&(dev, ino))
    }

    /// What was remembered of the file `(dev, ino)`.
    pub fn by_key_mut(&mut self, key: (u64, u64)) -> Option<&mut T> {
        self.stored.get_mut(&key)
    }

    /// Note that a file's first name has been stored, and what to remember of
    /// it (`stored`).
    ///
    /// Separate from `lookup` because it must only happen once that name
    /// really is in the archive or the destination. Recording a file before
    /// its data was read made every later name of an unreadable file a link
    /// to a member that was never written.
    pub fn record(&mut self, dev: u64, ino: u64, nlink: u32, stored: T) {
        if nlink > 1 {
            self.stored.entry((dev, ino)).or_insert(stored);
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
/// A set is remembered for the whole archive, as GNU cpio does, not only until
/// c_nlink of its names have gone by: a name list may name a file more often
/// than it has links, and the repeat would otherwise be extracted as a file of
/// its own, splitting the set.
///
/// The key alone is not trusted. Writers that truncate inode numbers to the
/// field -- GNU cpio, and this pax before archive-local numbering -- give
/// unrelated files the same one. A member bringing data that differs in size,
/// mode or modification time from the data the set already has is therefore
/// no name of it.
///
/// `T` is what the caller remembers about a set: the names extraction created
/// for it, the first name a listing showed.
#[derive(Debug)]
pub struct LinkSets<T> {
    sets: HashMap<(u64, u64), LinkSet<T>>,
}

/// One set of [`LinkSets`].
#[derive(Debug)]
struct LinkSet<T> {
    /// What the caller remembered
    value: T,
    /// The (size, mode, mtime) of the first of its names to carry data. newc
    /// stores the data with the last name only, the earlier ones empty.
    data: Option<(u64, u32, i64)>,
    /// Whether its format stores the data with the last name only
    /// (`defers_link_data`).
    defers_data: bool,
    /// The c_nlink its first name recorded.
    nlink: u32,
    /// How many of its names have been read (`count_name`).
    names: u32,
}

impl<T> LinkSet<T> {
    /// Whether data may still come on a later name: none has come yet, the
    /// format stores it with the last name, and not every name has been read.
    /// A file every name of which is empty is empty: in odc and the old
    /// binary format each name carries the data, so the first one shows it,
    /// and in newc the last one does.
    fn awaits_data(&self) -> bool {
        self.data.is_none() && self.defers_data && self.names < self.nlink
    }
}

/// Whether `entry`'s format stores a link set's data with its last name only,
/// the earlier ones empty: newc and crc. odc and the old binary format store it
/// with every name.
fn defers_link_data(entry: &ArchiveEntry) -> bool {
    use crate::formats::cpio::CpioFormat;
    matches!(
        entry.source_header,
        Some(SourceHeader::Cpio {
            format: CpioFormat::Newc | CpioFormat::NewcCrc
        })
    )
}

/// The (size, mode, mtime) a member carrying data brings, if it brings any.
fn data_shape(entry: &ArchiveEntry) -> Option<(u64, u32, i64)> {
    (entry.size > 0).then_some((entry.size, entry.mode, entry.mtime))
}

impl<T> Default for LinkSets<T> {
    fn default() -> Self {
        LinkSets {
            sets: HashMap::new(),
        }
    }
}

impl<T> LinkSets<T> {
    /// The key of the set `entry` would be a name of, or `None` for a member
    /// that is not one of several names of a file.
    pub fn key(entry: &ArchiveEntry) -> Option<(u64, u64)> {
        (entry.entry_type == EntryType::Regular && entry.nlink > 1)
            .then_some((entry.dev, entry.ino))
    }

    /// What was remembered about the set an earlier name started, when
    /// `entry` is a later name of it. A member whose data differs from the
    /// set's is not, and gets `None` like a member of no set.
    pub fn find_mut(&mut self, entry: &ArchiveEntry) -> Option<&mut T> {
        let set = self.sets.get_mut(&Self::key(entry)?)?;
        match (set.data, data_shape(entry)) {
            (Some(have), Some(this)) if have != this => return None,
            (None, this) => set.data = this,
            _ => {}
        }
        Some(&mut set.value)
    }

    /// Count `entry` as a name read of the set an earlier name started, if
    /// it is one, whether or not it is extracted.
    pub fn count_name(&mut self, entry: &ArchiveEntry) {
        if let Some(set) = Self::key(entry).and_then(|key| self.sets.get_mut(&key)) {
            set.names = set.names.saturating_add(1);
        }
    }

    /// What was remembered about the set whose key (`key`) is `key`.
    pub fn by_key_mut(&mut self, key: (u64, u64)) -> Option<&mut T> {
        self.sets.get_mut(&key).map(|set| &mut set.value)
    }

    /// Whether no set started so far awaits data on a later name.
    pub fn all_settled(&self) -> bool {
        !self.sets.values().any(LinkSet::awaits_data)
    }

    /// What was remembered about the set `entry` is a name of, once no data
    /// can still come for it on a later name.
    pub fn settled_mut(&mut self, entry: &ArchiveEntry) -> Option<&mut T> {
        let set = self.sets.get_mut(&Self::key(entry)?)?;
        (!set.awaits_data()).then_some(&mut set.value)
    }

    /// Start a set at `entry`, its first name. Nothing happens for a member
    /// that is not one of several names of a file, or whose set has already
    /// started -- which `find_mut` turned away for its differing data.
    pub fn insert(&mut self, entry: &ArchiveEntry, value: impl FnOnce() -> T) {
        if let Some(key) = Self::key(entry) {
            self.sets.entry(key).or_insert_with(|| LinkSet {
                value: value(),
                data: data_shape(entry),
                defers_data: defers_link_data(entry),
                nlink: entry.nlink,
                names: 1,
            });
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
    use std::path::Path;

    fn member(entry_type: EntryType, ino: u64, nlink: u32) -> ArchiveEntry {
        ArchiveEntry {
            entry_type,
            ino,
            nlink,
            ..Default::default()
        }
    }

    fn named(ino: u64, nlink: u32, size: u64) -> ArchiveEntry {
        ArchiveEntry {
            size,
            ..member(EntryType::Regular, ino, nlink)
        }
    }

    #[test]
    fn test_link_sets_group_names_for_the_whole_archive() {
        let mut sets: LinkSets<&str> = LinkSets::default();
        // Only a regular file with more than one name belongs to a set.
        sets.insert(&member(EntryType::Regular, 4, 1), || "solo");
        assert_eq!(sets.find_mut(&member(EntryType::Regular, 4, 1)), None);
        sets.insert(&member(EntryType::Directory, 5, 3), || "dir");
        assert_eq!(sets.find_mut(&member(EntryType::Directory, 5, 3)), None);

        sets.insert(&named(6, 2, 0), || "a");
        // More names than the link count still join the set.
        for _ in 0..3 {
            assert_eq!(sets.find_mut(&named(6, 2, 0)).copied(), Some("a"));
        }
    }

    /// A set awaits data only while its format stores it with the last name,
    /// none has come, and not every name has been read: an empty file's set
    /// settles too.
    #[test]
    fn test_link_sets_settle_when_no_data_can_follow() {
        use crate::formats::cpio::CpioFormat;
        let in_format = |format, ino, size| ArchiveEntry {
            source_header: Some(SourceHeader::Cpio { format }),
            ..named(ino, 3, size)
        };
        let mut sets: LinkSets<&str> = LinkSets::default();

        // odc: every name carries the data, so an empty first name is final.
        sets.insert(&in_format(CpioFormat::Odc, 1, 0), || "odc");
        assert_eq!(sets.settled_mut(&named(1, 3, 0)).copied(), Some("odc"));

        // newc: waits for its last name, or for one bringing data.
        let newc = |ino, size| in_format(CpioFormat::Newc, ino, size);
        sets.insert(&newc(2, 0), || "empty");
        sets.insert(&newc(3, 0), || "data");
        assert!(!sets.all_settled());
        for _ in 0..2 {
            assert_eq!(sets.settled_mut(&newc(2, 0)), None);
            sets.count_name(&newc(2, 0));
            sets.find_mut(&newc(2, 0));
        }
        assert_eq!(sets.settled_mut(&newc(2, 0)).copied(), Some("empty"));
        assert!(!sets.all_settled());
        sets.count_name(&newc(3, 4));
        sets.find_mut(&newc(3, 4));
        assert_eq!(sets.settled_mut(&newc(3, 0)).copied(), Some("data"));
        assert!(sets.all_settled());
    }

    /// Members sharing a key whose data differs are unrelated files; the
    /// newc set whose data arrives with its last name is still one file.
    #[test]
    fn test_link_sets_turn_away_differing_data() {
        let mut sets: LinkSets<&str> = LinkSets::default();
        sets.insert(&named(7, 2, 5), || "a");
        assert_eq!(sets.find_mut(&named(7, 2, 9)), None);
        // Turned away, it does not replace the set either.
        sets.insert(&named(7, 2, 9), || "b");
        assert_eq!(sets.find_mut(&named(7, 2, 5)).copied(), Some("a"));
        // A name with no data of its own joins whatever the set has.
        assert_eq!(sets.find_mut(&named(7, 2, 0)).copied(), Some("a"));

        sets.insert(&named(8, 3, 0), || "c");
        assert_eq!(sets.find_mut(&named(8, 3, 0)).copied(), Some("c"));
        assert_eq!(sets.find_mut(&named(8, 3, 6)).copied(), Some("c"));
        // From then on the set has data, and differing data is turned away.
        assert_eq!(sets.find_mut(&named(8, 3, 4)), None);
    }

    #[test]
    fn test_hard_link_tracker_keeps_the_first_name() {
        let mut links = HardLinkTracker::new();
        // A file with one name is never remembered.
        links.record(1, 7, 1, PathBuf::from("solo"));
        assert_eq!(links.lookup(1, 7, 1), None);

        links.record(1, 9, 2, PathBuf::from("a"));
        // Recording a second time keeps the first name, and the file stays
        // remembered however many of its names go by.
        links.record(1, 9, 2, PathBuf::from("b"));
        for _ in 0..3 {
            assert_eq!(
                links.lookup(1, 9, 2).map(PathBuf::as_path),
                Some(Path::new("a"))
            );
        }
    }
}
