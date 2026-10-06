//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX pax format implementation
//!
//! The pax format extends ustar with extended headers that can contain:
//! - Long paths (> 256 characters)
//! - Large file sizes (> 8GB)
//! - Large UID/GID values (> 2097151)
//! - Subsecond timestamps
//! - UTF-8 encoded filenames
//! - Access times (atime)
//! - Additional metadata
//!
//! Extended header format:
//! - typeflag 'x' for per-file extended headers
//! - typeflag 'g' for global extended headers
//! - Data format: "%d %s=%s\n" (length, keyword, value)

use crate::archive::{ArchiveEntry, ArchiveReader, ArchiveWriter, EntryType};
use crate::error::{PaxError, PaxResult};
use crate::formats::ustar::{
    calculate_checksum, entry_type_to_flag, long_name_record, member_data_size,
    parse_header as parse_ustar_header, parse_numeric, try_split_path, ustar_path_bytes,
    verify_checksum, write_field, LoneZeroBlock, LongNameGroup, SizeRule, BLOCK_SIZE, CHKSUM_OFF,
    DEVMAJOR_OFF, DEVMINOR_OFF, GID_OFF, GNAME_LEN, GNAME_OFF, LINKNAME_LEN, LINKNAME_OFF,
    MAGIC_OFF, MODE_OFF, MTIME_OFF, NAME_LEN, NAME_OFF, PREFIX_LEN, PREFIX_OFF, SIZE_OFF,
    TYPEFLAG_OFF, UID_OFF, UNAME_LEN, UNAME_OFF, VERSION_OFF, ZERO_BLOCK,
};
use crate::formats::{ArchiveStream, MAX_EXTENDED_HEADER, MAX_NAME};
use crate::options::FormatOptions;
use std::collections::{HashMap, HashSet};
use std::ffi::OsString;
use std::io::{Read, Seek, Write};
use std::ops::Range;
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::PathBuf;
use std::sync::Arc;

// The header block layout, the typeflags and the zero block all come from
// `formats::ustar`: a pax archive *is* a ustar archive with extra headers, and
// a second copy of an offset is a second thing to get wrong.

// Extended header typeflags, which are pax's own.
const PAX_XHDR: u8 = b'x'; // Per-file extended header
const PAX_GHDR: u8 = b'g'; // Global extended header

/// A pax extended-header timestamp, held exactly as integer seconds plus
/// nanoseconds. `f64` cannot represent nanosecond precision for present-day
/// epochs (a 10-digit second count leaves too few mantissa bits), so the
/// fractional `mtime`/`atime` records are carried losslessly here instead.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct PaxTime {
    pub sec: i64,
    pub nsec: u32,
}

/// Extended header keywords as per POSIX
#[derive(Debug, Clone, Default)]
pub struct ExtendedHeader {
    /// atime - file access time
    pub atime: Option<PaxTime>,
    /// mtime - file modification time
    pub mtime: Option<PaxTime>,
    /// ctime - inode change time.
    ///
    /// Not a POSIX keyword: IEEE Std 1003.1-2001/Cor 2-2004 (XCU/TC2/D6/25)
    /// removed it because `st_ctime` is an inode change time, not the file
    /// creation time the keyword claimed to hold. It survives as a widely
    /// written extension (star, GNU tar), and rule 7 of the listopt format
    /// admits implementation extensions, so it is parsed for listing and
    /// written under `-o times`. It is never restored on extract -- POSIX
    /// gives no portable way to set it.
    pub ctime: Option<PaxTime>,
    /// path - file pathname, as raw bytes.
    ///
    /// Not a String: a pathname is a byte string on Unix, and a member whose
    /// name is not valid UTF-8 must round-trip unchanged when
    /// `hdrcharset=BINARY` says so.
    pub path: Option<Vec<u8>>,
    /// linkpath - link target pathname, as raw bytes
    pub linkpath: Option<Vec<u8>>,
    /// size - file size
    pub size: Option<u64>,
    /// uid - user ID
    pub uid: Option<u32>,
    /// gid - group ID
    pub gid: Option<u32>,
    /// uname - user name, as raw bytes.
    ///
    /// Not a `String`, for the reason `path` is not: under `hdrcharset=BINARY`
    /// POSIX defines this record as "unencoded binary data from the underlying
    /// system", and a lossy decode would destroy the bytes the archive set out
    /// to preserve.
    pub uname: Option<Vec<u8>>,
    /// gname - group name, as raw bytes. See `uname`.
    pub gname: Option<Vec<u8>>,
    /// hdrcharset - character encoding for path/linkpath/uname/gname
    /// Values: "BINARY" (ISO/IEC 646:1991 aka ASCII, non-UTF-8 bytes allowed)
    ///         "ISO-IR 10646 2000 UTF-8" (default, UTF-8 encoded)
    pub hdrcharset: Option<String>,
    /// Additional custom keywords
    pub extra: HashMap<String, String>,
    /// Keywords a zero-length record (`keyword=`) deleted.
    ///
    /// POSIX: such a record "shall delete any header block field, previously
    /// entered extended header value, or global extended header value of the
    /// same name". The field above is cleared too; this remembers the deletion
    /// so that it reaches the global values and header block underneath when
    /// the headers are merged and applied.
    pub deleted: HashSet<String>,
}

/// The extended-header keywords `ExtendedHeader` holds in a typed field, as
/// opposed to the ones that land in `extra`.
///
/// One list: `serialize` needs it twice and `set_keyword` has an arm per name,
/// and the three had been written out separately, so adding a keyword to one
/// and not the others was a silent mistake. `holds`, `clear` and `merge` also
/// name every field; `test_standard_keywords_are_typed` pins them together.
const STANDARD_KEYWORDS: &[&str] = &[
    "hdrcharset",
    "atime",
    "mtime",
    "ctime",
    "path",
    "linkpath",
    "size",
    "uid",
    "gid",
    "uname",
    "gname",
];

impl ExtendedHeader {
    /// Create a new empty extended header
    pub fn new() -> Self {
        Self::default()
    }

    /// Whether this header already carries a value for one of
    /// [`STANDARD_KEYWORDS`], and so has already written its record.
    fn holds(&self, keyword: &str) -> bool {
        match keyword {
            "hdrcharset" => self.hdrcharset.is_some(),
            "atime" => self.atime.is_some(),
            "mtime" => self.mtime.is_some(),
            "ctime" => self.ctime.is_some(),
            "path" => self.path.is_some(),
            "linkpath" => self.linkpath.is_some(),
            "size" => self.size.is_some(),
            "uid" => self.uid.is_some(),
            "gid" => self.gid.is_some(),
            "uname" => self.uname.is_some(),
            "gname" => self.gname.is_some(),
            _ => false,
        }
    }

    /// Drop this header's value for `keyword`, typed or not.
    fn clear(&mut self, keyword: &str) {
        match keyword {
            "hdrcharset" => self.hdrcharset = None,
            "atime" => self.atime = None,
            "mtime" => self.mtime = None,
            "ctime" => self.ctime = None,
            "path" => self.path = None,
            "linkpath" => self.linkpath = None,
            "size" => self.size = None,
            "uid" => self.uid = None,
            "gid" => self.gid = None,
            "uname" => self.uname = None,
            "gname" => self.gname = None,
            _ => {
                self.extra.remove(keyword);
            }
        }
    }

    /// Record a zero-length `keyword=` record: the value is gone, and so is
    /// any value it would have overridden. See `deleted`.
    fn delete(&mut self, keyword: &str) {
        self.clear(keyword);
        self.deleted.insert(keyword.to_string());
    }

    /// Layer `later` over this header, keyword by keyword: what `later` sets
    /// replaces this header's value, what it deletes is deleted here, and
    /// everything it does not name is left alone.
    ///
    /// This is both how a `g` header joins the global values already in force
    /// ("the last one given ... shall take precedence", and only for the
    /// keywords it gives) and how an `x` header overrides them.
    fn merge(&mut self, later: &ExtendedHeader) {
        for keyword in &later.deleted {
            self.delete(keyword);
        }
        macro_rules! take {
            ($($field:ident),*) => {$(
                if later.$field.is_some() {
                    self.$field.clone_from(&later.$field);
                    self.deleted.remove(stringify!($field));
                }
            )*};
        }
        take!(hdrcharset, atime, mtime, ctime, path, linkpath, size, uid, gid, uname, gname);
        for (keyword, value) in &later.extra {
            self.extra.insert(keyword.clone(), value.clone());
            self.deleted.remove(keyword);
        }
    }

    /// Forget every value, and every deletion, whose keyword `keep` rejects,
    /// as though the archive had never carried the record.
    fn retain(&mut self, keep: impl Fn(&str) -> bool) {
        for &keyword in STANDARD_KEYWORDS {
            if !keep(keyword) {
                self.clear(keyword);
            }
        }
        self.extra.retain(|keyword, _| keep(keyword));
        self.deleted.retain(|keyword| keep(keyword));
    }

    /// This header without its extension records (`extra`): the typed fields,
    /// and which of them were deleted.
    ///
    /// What a member's records start from in place of a full copy of the
    /// global ones: the extensions are shared instead (see `ExtRecords`), and
    /// a deletion of one matters only when this header is merged into
    /// another, which a member's records never are.
    fn typed_only(&self) -> ExtendedHeader {
        ExtendedHeader {
            atime: self.atime,
            mtime: self.mtime,
            ctime: self.ctime,
            path: self.path.clone(),
            linkpath: self.linkpath.clone(),
            size: self.size,
            uid: self.uid,
            gid: self.gid,
            uname: self.uname.clone(),
            gname: self.gname.clone(),
            hdrcharset: self.hdrcharset.clone(),
            extra: HashMap::new(),
            deleted: STANDARD_KEYWORDS
                .iter()
                .filter(|keyword| self.deleted.contains(**keyword))
                .map(|keyword| keyword.to_string())
                .collect(),
        }
    }

    /// The records a set of `-o` operands stands for, in a stable order so
    /// that a bad value is always the same one reported. `assign` is the
    /// operator they were given with (`=` or `:=`), for the diagnostic.
    fn from_options(options: &HashMap<String, String>, assign: &str) -> PaxResult<Self> {
        let mut sorted: Vec<_> = options.iter().collect();
        sorted.sort();
        let mut header = ExtendedHeader::new();
        for (keyword, value) in sorted {
            header
                .apply_record(format!("{keyword}={value}").as_bytes())
                .map_err(|e| {
                    let reason = match e {
                        PaxError::InvalidHeader(reason) => reason,
                        other => other.to_string(),
                    };
                    PaxError::InvalidFormat(format!("-o {keyword}{assign}{value}: {reason}"))
                })?;
        }
        Ok(header)
    }

    /// Refuse a `path` or `linkpath` record longer than a reader accepts --
    /// this one included, so writing it made an archive pax could not read
    /// back.
    fn check_name_limit(&self) -> PaxResult<()> {
        for name in [&self.path, &self.linkpath].into_iter().flatten() {
            if name.len() as u64 > MAX_NAME {
                return Err(PaxError::PathTooLong(format!(
                    "{} bytes, over the {MAX_NAME} byte limit",
                    name.len()
                )));
            }
        }
        Ok(())
    }

    /// Parse extended header records from data
    pub fn parse(data: &[u8]) -> PaxResult<Self> {
        let mut header = ExtendedHeader::new();
        let mut pos = 0;

        while pos < data.len() {
            // Stop at null bytes (padding from some tar implementations)
            if data[pos] == 0 {
                break;
            }

            let span = parse_record_span(data, pos)?;
            header.apply_record(&data[span.value.clone()])?;
            pos = span.next;
        }

        Ok(header)
    }

    /// Interpret one `keyword=value` record body (the record without its length
    /// field, <space> and trailing <newline>).
    fn apply_record(&mut self, record: &[u8]) -> PaxResult<()> {
        let Some(eq_pos) = record.iter().position(|&b| b == b'=') else {
            return Ok(()); // no separator: not a record we can use
        };

        // Keyword must be valid UTF-8
        let keyword = std::str::from_utf8(&record[..eq_pos]).map_err(|_| {
            PaxError::InvalidHeader("invalid UTF-8 in extended header keyword".to_string())
        })?;

        // Value: try UTF-8 first, but SCHILY.xattr.* and some others can be binary
        // For binary-capable keywords, skip if not valid UTF-8
        let value_bytes = &record[eq_pos + 1..];
        // A zero-length value is a deletion, not a value: an empty time or id
        // is not something to parse, and a pax writer emits one on purpose
        // (`-o mtime:=`).
        if value_bytes.is_empty() {
            self.delete(keyword);
            return Ok(());
        }
        self.deleted.remove(keyword);
        // A pathname keyword keeps its bytes whatever they are; under
        // hdrcharset=BINARY they are deliberately not UTF-8.
        //
        // Its length is held to the limit every other header's pathname is
        // (a GNU long name, a cpio name): a record may otherwise run to the
        // whole extended header, and a short compressed archive could then
        // hand extraction a name of millions of components to walk.
        if matches!(keyword, "path" | "linkpath") && value_bytes.len() as u64 > MAX_NAME {
            return Err(PaxError::InvalidHeader(format!(
                "{keyword} record of {} bytes exceeds the {MAX_NAME} byte limit",
                value_bytes.len()
            )));
        }
        match keyword {
            "path" => {
                self.path = Some(value_bytes.to_vec());
                return Ok(());
            }
            "linkpath" => {
                self.linkpath = Some(value_bytes.to_vec());
                return Ok(());
            }
            // The other two records hdrcharset governs. Under BINARY they are
            // the underlying system's bytes, and the lossy decode below turned
            // every one that is not UTF-8 into U+FFFD -- irreversibly, under a
            // header that had just promised to preserve them.
            "uname" => {
                self.uname = Some(value_bytes.to_vec());
                return Ok(());
            }
            "gname" => {
                self.gname = Some(value_bytes.to_vec());
                return Ok(());
            }
            _ => {}
        }
        if let Ok(value) = std::str::from_utf8(value_bytes) {
            self.set_keyword(keyword, value)
        } else if keyword.starts_with("SCHILY.xattr.") {
            // Binary extended attributes - skip silently (we don't support xattrs)
            Ok(())
        } else {
            // Other keywords with invalid UTF-8 - try lossy conversion
            let value = String::from_utf8_lossy(value_bytes);
            self.set_keyword(keyword, &value)
        }
    }

    /// Set a keyword value
    fn set_keyword(&mut self, keyword: &str, value: &str) -> PaxResult<()> {
        match keyword {
            "atime" => {
                self.atime = Some(parse_pax_time(value)?);
            }
            "mtime" => {
                self.mtime = Some(parse_pax_time(value)?);
            }
            "ctime" => {
                self.ctime = Some(parse_pax_time(value)?);
            }
            "path" => {
                self.path = Some(value.as_bytes().to_vec());
            }
            "linkpath" => {
                self.linkpath = Some(value.as_bytes().to_vec());
            }
            "size" => {
                self.size =
                    Some(value.parse().map_err(|_| {
                        PaxError::InvalidHeader(format!("invalid size: {}", value))
                    })?);
            }
            "uid" => {
                self.uid = Some(
                    value
                        .parse()
                        .map_err(|_| PaxError::InvalidHeader(format!("invalid uid: {}", value)))?,
                );
            }
            "gid" => {
                self.gid = Some(
                    value
                        .parse()
                        .map_err(|_| PaxError::InvalidHeader(format!("invalid gid: {}", value)))?,
                );
            }
            "uname" => {
                self.uname = Some(value.as_bytes().to_vec());
            }
            "gname" => {
                self.gname = Some(value.as_bytes().to_vec());
            }
            "hdrcharset" => {
                self.hdrcharset = Some(value.to_string());
            }
            _ => {
                // Store unknown keywords for potential future use
                self.extra.insert(keyword.to_string(), value.to_string());
            }
        }
        Ok(())
    }

    /// Serialize extended header to bytes, respecting format options
    ///
    /// This method filters out keywords that match delete patterns and applies
    /// per-file overrides from the options (keyword:=value).
    pub fn serialize(&self, options: &FormatOptions) -> Vec<u8> {
        let mut data = Vec::new();
        let per_file = options.per_file_options();

        // Write a record unless the keyword is deleted, honoring any per-file
        // `keyword:=value` override. Plain functions rather than closures so the
        // text and raw-bytes forms can both append to `data`.
        fn write_if_allowed_bytes(
            data: &mut Vec<u8>,
            options: &FormatOptions,
            per_file: &HashMap<String, String>,
            keyword: &str,
            default_value: &[u8],
        ) {
            if options.should_delete_keyword(keyword) {
                return;
            }
            match per_file.get(keyword) {
                Some(v) => write_pax_record_bytes(data, keyword, v.as_bytes()),
                None => write_pax_record_bytes(data, keyword, default_value),
            }
        }

        fn write_if_allowed(
            data: &mut Vec<u8>,
            options: &FormatOptions,
            per_file: &HashMap<String, String>,
            keyword: &str,
            default_value: &str,
        ) {
            write_if_allowed_bytes(data, options, per_file, keyword, default_value.as_bytes());
        }

        macro_rules! rec {
            ($kw:expr, $val:expr) => {
                write_if_allowed(&mut data, options, per_file, $kw, $val)
            };
        }
        macro_rules! rec_bytes {
            ($kw:expr, $val:expr) => {
                write_if_allowed_bytes(&mut data, options, per_file, $kw, $val)
            };
        }

        // Write hdrcharset first so readers know the encoding of subsequent fields
        if let Some(ref charset) = self.hdrcharset {
            rec!("hdrcharset", charset);
        }
        if let Some(atime) = self.atime {
            rec!("atime", &format_pax_time(atime));
        }
        if let Some(mtime) = self.mtime {
            rec!("mtime", &format_pax_time(mtime));
        }
        if let Some(ctime) = self.ctime {
            rec!("ctime", &format_pax_time(ctime));
        }
        if let Some(ref path) = self.path {
            rec_bytes!("path", path);
        }
        if let Some(ref linkpath) = self.linkpath {
            rec_bytes!("linkpath", linkpath);
        }
        if let Some(size) = self.size {
            rec!("size", &size.to_string());
        }
        if let Some(uid) = self.uid {
            rec!("uid", &uid.to_string());
        }
        if let Some(gid) = self.gid {
            rec!("gid", &gid.to_string());
        }
        if let Some(ref uname) = self.uname {
            rec_bytes!("uname", uname);
        }
        if let Some(ref gname) = self.gname {
            rec_bytes!("gname", gname);
        }
        // Sorted: iterating a HashMap made the record order differ between runs
        // of the same command, so two invocations produced different bytes for
        // the same input -- hostile to reproducible builds and to diffing.
        let mut extra: Vec<_> = self.extra.iter().collect();
        extra.sort_by(|a, b| a.0.cmp(b.0));
        for (key, value) in extra {
            rec!(key.as_str(), value.as_str());
        }

        // Per-file overrides (`-o keyword:=value`) for *standard* keywords whose
        // value is absent from this entry: write_if_allowed above already merges
        // an override when the entry carried the field, but a forced value such
        // as `-o gname:=other` / `-o uid:=N` on an entry with no gname/uid must
        // still produce a record.
        for &keyword in STANDARD_KEYWORDS {
            if self.holds(keyword) || options.should_delete_keyword(keyword) {
                continue;
            }
            if let Some(value) = per_file.get(keyword) {
                write_pax_record(&mut data, keyword, value);
            }
        }

        // Also write any per-file options that weren't already present in the header
        // (e.g., user can add custom keywords via -o keyword:=value)
        let mut per_file_sorted: Vec<_> = per_file.iter().collect();
        per_file_sorted.sort_by(|a, b| a.0.cmp(b.0));
        for (key, value) in per_file_sorted {
            if options.should_delete_keyword(key) {
                continue;
            }
            // Skip standard keywords that were already handled above
            if !STANDARD_KEYWORDS.contains(&key.as_str()) && !self.extra.contains_key(key) {
                write_pax_record(&mut data, key, value);
            }
        }

        data
    }

    /// Apply these records over the header block fields already in `entry`.
    ///
    /// A keyword with no value here leaves the header block field in force.
    /// That includes a deleted one, with two exceptions: `uname` and `gname`,
    /// whose header block fields can be deleted too -- the owner is then named
    /// by its numeric id, which is what an archive without the name means.
    /// The other fields have no "absent": deleting `size` would desync the
    /// archive, `path` would leave the member nameless, and a time or an id
    /// of zero would be an invented value rather than a missing one, so their
    /// header block value stands.
    fn apply_to(&self, entry: &mut ArchiveEntry) {
        if let Some(ref path) = self.path {
            entry.path = PathBuf::from(OsString::from_vec(path.clone()));
        }
        if let Some(ref linkpath) = self.linkpath {
            entry.link_target = Some(PathBuf::from(OsString::from_vec(linkpath.clone())));
        }
        if let Some(size) = self.size {
            entry.size = size;
        }
        if let Some(uid) = self.uid {
            entry.uid = uid;
        }
        if let Some(gid) = self.gid {
            entry.gid = gid;
        }
        if let Some(ref uname) = self.uname {
            entry.uname = Some(uname.clone());
        }
        if let Some(ref gname) = self.gname {
            entry.gname = Some(gname.clone());
        }
        if let Some(mtime) = self.mtime {
            entry.mtime = mtime.sec;
            entry.mtime_nsec = mtime.nsec;
        }
        if let Some(atime) = self.atime {
            entry.atime = Some(atime.sec);
            entry.atime_nsec = atime.nsec;
        }
        // Carried onto the entry so `-o listopt=%(ctime)T` can report it. The
        // extractor never applies it to the filesystem.
        if let Some(ctime) = self.ctime {
            entry.ctime = Some(ctime.sec);
            entry.ctime_nsec = ctime.nsec;
        }
        // The records nothing above holds: `charset`, `comment`, `hdrcharset`
        // and any implementation extension. None affects extraction; POSIX
        // listopt rule 7 admits every one of them as a `%(keyword)`, and
        // without this the listing had no way to report what the archive said.
        if let Some(ref hdrcharset) = self.hdrcharset {
            entry.set_ext_record("hdrcharset", hdrcharset);
        }
        for (keyword, value) in &self.extra {
            entry.set_ext_record(keyword, value);
        }
        // A deleted record also deletes the global value beneath it, which
        // the entry holds shared rather than here.
        for keyword in &self.deleted {
            entry.ext_records.hide(keyword);
        }
        if self.deleted.contains("uname") {
            entry.uname = None;
        }
        if self.deleted.contains("gname") {
            entry.gname = None;
        }
    }

    /// The extended-header records this member needs, if any.
    ///
    /// A record is written only where the ustar header cannot carry the value:
    /// a pathname or link target with no faithful ustar spelling, a size or an
    /// id too large for its octal field, a user or group name outside the
    /// portable character set or too long for its field, a time with
    /// sub-second precision.
    ///
    /// `options` supplies the two things the operator can change. `-o times`
    /// forces atime and mtime records for every member rather than only where
    /// one is needed, and `-o hdrcharset=` both widens the rule for which
    /// names need a record and decides whether this member declares a charset
    /// of its own.
    pub fn from_entry(entry: &ArchiveEntry, options: &FormatOptions) -> Self {
        let mut header = ExtendedHeader::new();
        let include_times = options.include_times;

        // `-o hdrcharset=BINARY` is the operator saying the names in this
        // archive are the underlying system's bytes rather than UTF-8, and
        // under it the `path` record is what carries those bytes. POSIX's
        // RATIONALE is explicit about the consequence: "an extended header
        // path record is always required to be generated if the prefix or
        // name fields contain non-ASCII characters even when
        // hdrcharset=binary is also in effect for that file." So the trigger
        // widens from "has no faithful UTF-8 reading" to "is not ASCII": a
        // UTF-8 name that would fit the ustar fields still needs the record.
        let binary = options.hdrcharset() == Some(crate::options::BINARY_CHARSET);
        let needs_record = |bytes: &[u8]| {
            if binary {
                !bytes.is_ascii()
            } else {
                std::str::from_utf8(bytes).is_err()
            }
        };

        // Path needs an extended header whenever it cannot be represented
        // exactly by the ustar name/prefix pair. Length alone is not the test:
        // a path under NAME_LEN + PREFIX_LEN + 1 bytes still fails to split
        // when it has no '/' at a position that leaves a <= NAME_LEN tail
        // (e.g. a 190-byte "dir/<185-byte-basename>"). Without this record the
        // ustar fallback in split_path() silently truncates the name.
        let path_bytes = crate::rawpath::as_bytes(&entry.path);
        let ustar_spelling = ustar_path_bytes(entry);
        let path_is_binary = needs_record(path_bytes);
        if try_split_path(&ustar_spelling).is_none() || path_is_binary {
            // A non-UTF-8 name has no faithful ustar spelling, so it always
            // needs the record regardless of length. A directory's carries
            // the trailing slash its header fields would, as bsdtar's does,
            // so it lists the same whichever of the two names it.
            header.path = Some(ustar_spelling);
        }

        // Link path needs extended header if too long
        let mut link_is_binary = false;
        if let Some(ref link) = entry.link_target {
            let link_bytes = link.as_os_str().as_bytes();
            link_is_binary = needs_record(link_bytes);
            if link_bytes.len() > LINKNAME_LEN || link_is_binary {
                header.linkpath = Some(link_bytes.to_vec());
            }
        }

        // POSIX -o invalid=binary: a member whose name cannot be represented in
        // the header character set is announced with hdrcharset=BINARY, and its
        // pathname records carry unencoded bytes. Without this the name was run
        // through to_string_lossy and every invalid byte became U+FFFD --
        // irreversibly, and identically to -o invalid=write.
        //
        // Not needed when the operator asked for a charset: that value is
        // already written once as a global `g` record, and repeating it in
        // every member's `x` header would say nothing new. A name that is not
        // valid UTF-8 still forces the per-file record, which overrides the
        // global one -- announcing such a member as UTF-8 would declare an
        // encoding its bytes are not in.
        //
        // hdrcharset governs four records -- path, linkpath, uname and gname --
        // so any of the four forces the declaration, not just the two
        // pathnames. A user or group name is bytes from the local database and
        // need not be UTF-8 either.
        let not_utf8 = |bytes: &[u8]| std::str::from_utf8(bytes).is_err();
        let name_is_binary = entry.uname.as_deref().is_some_and(not_utf8)
            || entry.gname.as_deref().is_some_and(not_utf8);
        if !binary && (path_is_binary || link_is_binary || name_is_binary) {
            header.hdrcharset = Some(crate::options::BINARY_CHARSET.to_string());
        }

        // Size > 8GB needs extended header. So does a hard link's data
        // (`-o linkdata`): a reader takes typeflag 1 to have any only in a
        // pax archive, which only an extended header makes one.
        if entry.size > 0o77777777777 || (entry.entry_type == EntryType::Hardlink && entry.size > 0)
        {
            header.size = Some(entry.size);
        }

        // UID/GID > 2097151 needs extended header
        if entry.uid > 0o7777777 {
            header.uid = Some(entry.uid);
        }
        if entry.gid > 0o7777777 {
            header.gid = Some(entry.gid);
        }

        // POSIX: an mtime record "for each file ... if the file's
        // modification time cannot be represented exactly in the ustar header
        // logical record" -- a fraction of a second, or a time outside the
        // octal field's range, before 1970 included. Also under `-o times`.
        if include_times || entry.mtime_nsec > 0 || !(0..=USTAR_TIME_MAX).contains(&entry.mtime) {
            header.mtime = Some(PaxTime {
                sec: entry.mtime,
                nsec: entry.mtime_nsec,
            });
        }

        // Include atime only under `-o times`; it is not part of the default
        // extended-record set, so an ordinary file produces no `x` header.
        if include_times {
            let (sec, nsec) = match entry.atime {
                Some(atime) => (atime, entry.atime_nsec),
                None => (entry.mtime, entry.mtime_nsec),
            };
            header.atime = Some(PaxTime { sec, nsec });

            // ctime alongside it. POSIX's `times` keyword names only atime and
            // mtime, but every implementation that writes one writes all three,
            // and an archive without it cannot answer `%(ctime)T`.
            if let Some(ctime) = entry.ctime {
                header.ctime = Some(PaxTime {
                    sec: ctime,
                    nsec: entry.ctime_nsec,
                });
            }
        }

        // uname/gname with non-ASCII characters
        if let Some(ref uname) = entry.uname {
            if !uname.is_ascii() || uname.len() > UNAME_LEN {
                header.uname = Some(uname.clone());
            }
        }
        if let Some(ref gname) = entry.gname {
            if !gname.is_ascii() || gname.len() > GNAME_LEN {
                header.gname = Some(gname.clone());
            }
        }

        header
    }
}

/// Where one extended-header record's value lies, and where the next record
/// begins.
///
/// The offsets are computed once, here, and handed to the caller. Returning the
/// *declared length* instead and letting the caller work out `pos + len` is how
/// the bounds check below came to be bypassed: the check lived here but the
/// slice was built there, from the same addition done a second time.
struct RecordSpan {
    value: std::ops::Range<usize>,
    next: usize,
}

/// Parse the `"%d "` length prefix of the record starting at `pos`.
///
/// The length field is attacker-controlled, so every arithmetic step on it is
/// checked. `pos + record_len` in particular wraps for a length near
/// `usize::MAX`, and a release build does not trap on that -- the wrapped sum
/// compares below `data.len()`, the bounds check passes, and the slice that
/// follows has a start beyond its end.
fn parse_record_span(data: &[u8], pos: usize) -> PaxResult<RecordSpan> {
    let bad_len = || PaxError::InvalidHeader("invalid extended header length".to_string());

    let space_pos = data[pos..]
        .iter()
        .position(|&b| b == b' ')
        .ok_or_else(|| PaxError::InvalidHeader("invalid extended header format".to_string()))?;

    // POSIX: "%d", a decimal number -- digits only. `parse` would also take
    // a leading '+'.
    let len_field = &data[pos..pos + space_pos];
    if len_field.is_empty() || !len_field.iter().all(u8::is_ascii_digit) {
        return Err(bad_len());
    }
    let record_len: usize = std::str::from_utf8(len_field)
        .ok()
        .and_then(|digits| digits.parse().ok())
        .ok_or_else(bad_len)?;

    // The record must extend past its own length field, its <space>, and the
    // trailing <newline>; otherwise there is no value and the end underflows.
    if record_len <= space_pos + 1 {
        return Err(bad_len());
    }

    let next = pos.checked_add(record_len).ok_or_else(bad_len)?;
    if next > data.len() {
        return Err(PaxError::InvalidHeader(
            "extended header record extends past end".to_string(),
        ));
    }

    let value_start = pos + space_pos + 1;
    // record_len > space_pos + 1 puts the <newline> at or after value_start.
    let value_end = next - 1;
    if value_end < value_start {
        return Err(bad_len());
    }
    // The length has to land just past the record's <newline>. Taking
    // whatever byte is there for it dropped the last byte of the value.
    if data[value_end] != b'\n' {
        return Err(PaxError::InvalidHeader(
            "extended header record does not end in a newline".to_string(),
        ));
    }

    Ok(RecordSpan {
        value: value_start..value_end,
        next,
    })
}

/// Parse a pax time: decimal seconds since the Epoch with an optional
/// fraction, and an optional leading '-' for a time before it.
///
/// The value is the signed decimal number, so "-1.5" is a second and a half
/// before the Epoch. `PaxTime` holds it the way `timespec` does, as whole
/// seconds rounded down plus a non-negative fraction: -2 s + 0.5 s. Taking
/// the fraction as an addition to the truncated "-1" made it -0.5, and "-0.5"
/// itself came out as +0.5, since "-0" parses as zero.
fn parse_pax_time(s: &str) -> PaxResult<PaxTime> {
    let invalid = || PaxError::InvalidHeader(format!("invalid pax time: {}", s));
    let (sec_str, frac_str) = s.split_once('.').unwrap_or((s, ""));
    let mut sec: i64 = sec_str.parse().map_err(|_| invalid())?;

    // Take up to 9 fractional digits, zero-padded to nanoseconds.
    let mut frac = String::with_capacity(9);
    let mut dropped = false;
    for c in frac_str.chars() {
        if !c.is_ascii_digit() {
            return Err(invalid());
        }
        if frac.len() < 9 {
            frac.push(c);
        } else {
            dropped |= c != '0';
        }
    }
    while frac.len() < 9 {
        frac.push('0');
    }
    let mut nsec: u32 = frac.parse().map_err(|_| invalid())?;

    if !sec_str.starts_with('-') {
        // Dropping digits rounded down, as `timespec` does.
        return Ok(PaxTime { sec, nsec });
    }
    // Before the Epoch, dropping digits rounded the magnitude down and so the
    // time up; one more nanosecond of magnitude rounds it down instead.
    if dropped {
        nsec += 1;
        if nsec == NSEC_PER_SEC {
            sec = sec.checked_sub(1).ok_or_else(invalid)?;
            nsec = 0;
        }
    }
    if nsec == 0 {
        return Ok(PaxTime { sec, nsec });
    }
    Ok(PaxTime {
        sec: sec.checked_sub(1).ok_or_else(invalid)?,
        nsec: NSEC_PER_SEC - nsec,
    })
}

const NSEC_PER_SEC: u32 = 1_000_000_000;

/// Format time for pax extended header, preserving exact nanoseconds: the
/// signed decimal value, so a time before the Epoch has a leading '-' on the
/// whole number (see `parse_pax_time`).
fn format_pax_time(time: PaxTime) -> String {
    if time.nsec == 0 {
        return format!("{}", time.sec);
    }
    let (sign, whole, nsec) = if time.sec < 0 {
        // -2 s + 0.5 s is -1.5: one second fewer, and the fraction's complement.
        ("-", (time.sec + 1).unsigned_abs(), NSEC_PER_SEC - time.nsec)
    } else {
        ("", time.sec as u64, time.nsec)
    };
    let frac = format!("{:09}", nsec);
    format!("{sign}{whole}.{}", frac.trim_end_matches('0'))
}

/// Write a pax extended header record whose value is raw bytes.
///
/// The record length counts bytes, not characters, so this is also the correct
/// path for any value that merely happens to be UTF-8.
fn write_pax_record_bytes(data: &mut Vec<u8>, keyword: &str, value: &[u8]) {
    let mut content = Vec::with_capacity(keyword.len() + value.len() + 3);
    content.push(b' ');
    content.extend_from_slice(keyword.as_bytes());
    content.push(b'=');
    content.extend_from_slice(value);
    content.push(b'\n');

    // Length includes itself, so it has to be solved for.
    let mut len = content.len() + 1;
    loop {
        let total = len.to_string().len() + content.len();
        if total == len {
            break;
        }
        len = total;
    }

    data.extend_from_slice(len.to_string().as_bytes());
    data.extend_from_slice(&content);
}

/// Write a pax extended header record
fn write_pax_record(data: &mut Vec<u8>, keyword: &str, value: &str) {
    // Record format: "%d %s=%s\n"
    // Length includes itself, so we need to calculate iteratively
    let content = format!(" {}={}\n", keyword, value);

    // Start with an estimate
    let mut len = content.len() + 1; // +1 for at least one digit
    loop {
        let len_str = len.to_string();
        let total = len_str.len() + content.len();
        if total == len {
            break;
        }
        len = total;
    }

    data.extend_from_slice(len.to_string().as_bytes());
    data.extend_from_slice(content.as_bytes());
}

/// The `-o keyword=value` and `-o keyword:=value` operands of read and list
/// mode, as the extended-header records POSIX says they act as.
///
/// `keyword=value` acts "as if they had been at the beginning of the archive
/// as typeflag g global extended header records", and `keyword:=value` "as if
/// they were included as records at the end of each extended header", so the
/// first is overridden by the archive's own records and the second overrides
/// them.
#[derive(Debug, Clone, Default)]
pub struct OptionRecords {
    global: ExtendedHeader,
    per_file: ExtendedHeader,
}

impl OptionRecords {
    /// Parse the operands' values as the records they stand for, so that a
    /// value no archive record could carry is refused up front.
    pub fn new(options: &FormatOptions) -> PaxResult<Self> {
        Ok(OptionRecords {
            global: ExtendedHeader::from_options(options.global_options(), "=")?,
            per_file: ExtendedHeader::from_options(options.per_file_options(), ":=")?,
        })
    }

    /// Apply the records to a member of a format with no extended headers
    /// (cpio, pre-POSIX tar), where the global records and then the per-file
    /// ones simply override the header fields.
    pub fn apply(&self, entry: &mut ArchiveEntry) {
        let mut records = self.global.clone();
        records.merge(&self.per_file);
        records.apply_to(entry);
    }
}

/// pax archive reader
pub struct PaxReader<R: Read> {
    reader: ArchiveStream<R>,
    current_size: u64,
    bytes_read: u64,
    /// The global values in force: `-o keyword=value` first, then every `g`
    /// header read so far, each layered over the last. Its extension records
    /// are kept in `global_extra` instead.
    global_header: ExtendedHeader,
    /// The extension records of `global_header`, less any `-o delete=`
    /// removes: held once and shared by every member they apply to, since
    /// cloning them into each one made a large `g` header cost its size
    /// times the number of members.
    global_extra: Arc<HashMap<String, String>>,
    /// The bytes of keyword and value in `global_extra`, held to
    /// `MAX_EXTENDED_HEADER` as a whole: each `g` header is capped, but they
    /// accumulate.
    global_extra_bytes: usize,
    /// `-o keyword:=value`, appended to every member's extended header.
    per_file_options: ExtendedHeader,
    /// `-o` options consulted on read (`delete=` keyword removal).
    options: FormatOptions,
    /// Where the member being read begins, counting any extended header that
    /// describes it. Once `read_entry` has returned `None` this is where the
    /// end-of-archive indicator begins.
    member_offset: u64,
    /// The `g` headers read since `member_offset` while an `x` header was
    /// pending, as byte ranges of the archive. See
    /// [`trailing_global_headers`](Self::trailing_global_headers).
    pending_globals: Vec<Range<u64>>,
    /// Whether any `x` or `g` header has been read.
    saw_extended_header: bool,
    /// What a single zero block between members means.
    lone_zero: LoneZeroBlock,
}

impl<R: Read> PaxReader<R> {
    /// A pax reader over an archive stream
    pub fn from_stream(reader: ArchiveStream<R>) -> Self {
        PaxReader {
            reader,
            current_size: 0,
            bytes_read: 0,
            global_header: ExtendedHeader::new(),
            global_extra: Arc::default(),
            global_extra_bytes: 0,
            per_file_options: ExtendedHeader::new(),
            options: FormatOptions::default(),
            member_offset: 0,
            pending_globals: Vec::new(),
            saw_extended_header: false,
            lone_zero: LoneZeroBlock::Stop,
        }
    }

    /// Read on past a single zero block, as append mode must, rather than
    /// taking it for the end of the archive. See [`LoneZeroBlock`].
    pub fn stepping_over_lone_zero_blocks(mut self) -> Self {
        self.lone_zero = LoneZeroBlock::StepOver;
        self
    }

    /// The offset at which the end-of-archive indicator begins, once
    /// `read_entry` has returned `None`.
    ///
    /// An `x` header with no member after it is left out, so that whatever is
    /// written there next is not described by it. A trailing `g` header is
    /// not: it applies to every member that follows, appended ones included.
    pub fn end_of_archive(&self) -> u64 {
        self.member_offset
    }

    /// The `g` headers that lie past [`end_of_archive`](Self::end_of_archive),
    /// once `read_entry` has returned `None`, as byte ranges of the archive.
    ///
    /// They come after a dangling `x` header, which is why the end of the
    /// archive is before them. They still apply to whatever is appended, so
    /// append writes them again at the new end, without the `x`.
    pub fn trailing_global_headers(&self) -> &[Range<u64>] {
        &self.pending_globals
    }

    /// Whether the archive has used any pax extended header so far.
    pub fn saw_extended_header(&self) -> bool {
        self.saw_extended_header
    }

    /// Attach the `-o` format options of read and list mode: `delete=`, and
    /// the keyword records described at [`OptionRecords`].
    pub fn with_options(mut self, options: FormatOptions) -> PaxResult<Self> {
        let records = OptionRecords::new(&options)?;
        self.options = options;
        self.global_header = ExtendedHeader::new();
        self.global_extra = Arc::default();
        self.global_extra_bytes = 0;
        self.merge_global(records.global)?;
        self.per_file_options = records.per_file;
        Ok(self)
    }

    /// Layer a `g` header -- or the `-o keyword=value` records that act as
    /// one -- over the global values in force.
    ///
    /// Its extension records go straight into the shared map, which no member
    /// still holds by the time the next header is read, so this costs the
    /// size of `later` rather than of everything global so far.
    ///
    /// Distinct keywords accumulate there, one `g` header after another, so
    /// the whole is held to the limit a single header is.
    fn merge_global(&mut self, mut later: ExtendedHeader) -> PaxResult<()> {
        let extra = std::mem::take(&mut later.extra);
        let shared = Arc::make_mut(&mut self.global_extra);
        let mut bytes = self.global_extra_bytes;
        for keyword in &later.deleted {
            if let Some(value) = shared.remove(keyword) {
                bytes -= keyword.len() + value.len();
            }
        }
        for (keyword, value) in extra {
            if self.options.should_delete_keyword(&keyword) {
                continue;
            }
            let keyword_len = keyword.len();
            bytes += keyword_len + value.len();
            if let Some(old) = shared.insert(keyword, value) {
                // Already counted, with the value just replaced.
                bytes -= keyword_len + old.len();
            }
        }
        self.global_extra_bytes = bytes;
        if bytes as u64 > MAX_EXTENDED_HEADER {
            return Err(PaxError::InvalidHeader(format!(
                "global extended header records exceed the {MAX_EXTENDED_HEADER} byte limit"
            )));
        }
        // Only a typed keyword's deletion has anything left to act on: an
        // extension's was carried out on the shared map above, and keeping
        // it would be one more thing that accumulates.
        later
            .deleted
            .retain(|keyword| STANDARD_KEYWORDS.contains(&keyword.as_str()));
        self.global_header.merge(&later);
        Ok(())
    }

    /// Read a raw header block
    fn read_header_block(&mut self) -> PaxResult<Option<[u8; BLOCK_SIZE]>> {
        let Some(header) =
            crate::formats::ustar::next_header_block(&mut self.reader, self.lone_zero)?
        else {
            return Ok(None);
        };

        // Verify checksum
        if !verify_checksum(&header) {
            return Err(PaxError::InvalidHeader("checksum mismatch".to_string()));
        }

        Ok(Some(header))
    }

    /// The records that describe the next member, in POSIX's order of
    /// precedence: the global values, then its own `x` header, then the
    /// `-o keyword:=value` records appended to the end of it.
    ///
    /// `-o delete=` removes the archive's records, so a removed keyword falls
    /// back to the header block value; the operator's own `:=` records are
    /// applied whatever it matches.
    ///
    /// The global extension records are not among them: they are shared
    /// through `global_extra` rather than copied into every member.
    fn member_records(&self, extended_header: Option<&ExtendedHeader>) -> ExtendedHeader {
        let mut records = self.global_header.typed_only();
        if let Some(ext) = extended_header {
            records.merge(ext);
        }
        records.retain(|keyword| !self.options.should_delete_keyword(keyword));
        records.merge(&self.per_file_options);
        records
    }

    /// The rule for which members carry data. A hard link may only in a pax
    /// archive, and an archive is one only once it has used an extended
    /// header: the ustar magic alone is no sign, and in a ustar archive a
    /// link's size field -- which the pre-POSIX convention filled with the
    /// linked file's size -- is followed by the next member's header.
    fn size_rule(&self) -> SizeRule {
        if self.saw_extended_header {
            SizeRule::Pax
        } else {
            SizeRule::Ustar
        }
    }

    /// Read extended header data
    fn read_extended_header(&mut self, size: u64) -> PaxResult<ExtendedHeader> {
        let data = crate::formats::read_declared(
            &mut self.reader,
            size,
            crate::formats::MAX_EXTENDED_HEADER,
            "pax extended header",
        )?;

        // Skip padding to block boundary using a stack buffer
        let padding = padding_needed(size);
        if padding > 0 {
            let mut pad = [0u8; BLOCK_SIZE];
            self.reader.read_exact(&mut pad[..padding])?;
        }

        ExtendedHeader::parse(&data)
    }
}

impl<R: Read + Seek> PaxReader<R> {
    /// A reader over a seekable file, positioned at the start of the archive,
    /// that seeks over member data instead of reading it.
    pub fn seekable(reader: R) -> Self {
        Self::from_stream(ArchiveStream::seekable(reader))
    }
}

impl<R: Read> ArchiveReader for PaxReader<R> {
    fn read_entry(&mut self) -> PaxResult<Option<ArchiveEntry>> {
        // Skip any remaining data from previous entry
        self.skip_data()?;

        // What describes the next member: its `x` header, and the GNU
        // long-name records ahead of it. Either can come first, and both
        // describe the member that follows them, not each other.
        let mut extended_header: Option<ExtendedHeader> = None;
        let mut long_names = LongNameGroup::default();

        loop {
            if extended_header.is_none() && long_names.is_empty() {
                self.member_offset = self.reader.offset();
                self.pending_globals.clear();
            }
            let Some(header) = self.read_header_block()? else {
                // With no member after them, the records end the archive,
                // which begins where they do.
                if !long_names.is_empty() {
                    long_names.report(None);
                }
                return Ok(None);
            };

            let typeflag = header[TYPEFLAG_OFF];
            let describes_pending = extended_header.is_some() || !long_names.is_empty();

            // A GNU long-name record describes the member that follows, whose
            // own name field is truncated to 100 bytes. There can be more
            // than one.
            if let Some(what) = long_name_record(typeflag) {
                long_names.consume(&mut self.reader, &header, what)?;
                continue;
            }

            match typeflag {
                PAX_GHDR => {
                    // Global extended header - affects all subsequent files,
                    // for the keywords it names; the rest stay in force.
                    let start = self.reader.offset() - BLOCK_SIZE as u64;
                    let size = parse_numeric(&header[SIZE_OFF..SIZE_OFF + 12])?;
                    let global = self.read_extended_header(size)?;
                    self.merge_global(global)?;
                    self.saw_extended_header = true;
                    if describes_pending {
                        self.pending_globals.push(start..self.reader.offset());
                    }
                }
                PAX_XHDR => {
                    // Per-file extended header
                    let size = parse_numeric(&header[SIZE_OFF..SIZE_OFF + 12])?;
                    extended_header = Some(self.read_extended_header(size)?);
                    self.saw_extended_header = true;
                }
                _ => {
                    let records = self.member_records(extended_header.as_ref());
                    let rule = self.size_rule();
                    if !long_names.superseded(records.path.is_some(), records.linkpath.is_some()) {
                        // The records and the member are dropped together,
                        // the member by the size its own records give it.
                        long_names.report(Some(&header));
                        self.current_size = member_data_size(&header, rule, records.size)?;
                        self.bytes_read = 0;
                        self.skip_data()?;
                        extended_header = None;
                        long_names = LongNameGroup::default();
                        continue;
                    }

                    // Regular file entry - parse and apply extended headers
                    let mut entry = parse_ustar_header(&header, rule)?;
                    records.apply_to(&mut entry);
                    entry.ext_records.share(&self.global_extra);

                    // A `size=` record replaces the size field, not the rule
                    // for which types carry data: a directory or FIFO has
                    // none whichever of the two records its size.
                    entry.size = rule.data_size(entry.entry_type, entry.size);

                    self.current_size = entry.size;
                    self.bytes_read = 0;

                    return Ok(Some(entry));
                }
            }
        }
    }

    fn read_data(&mut self, buf: &mut [u8]) -> PaxResult<usize> {
        let remaining = self.current_size.saturating_sub(self.bytes_read);
        if remaining == 0 {
            return Ok(0);
        }

        let to_read = std::cmp::min(buf.len() as u64, remaining) as usize;
        let n = self.reader.read(&mut buf[..to_read])?;
        self.bytes_read += n as u64;
        Ok(n)
    }

    fn skip_data(&mut self) -> PaxResult<()> {
        let total_bytes = round_up_block(self.current_size);
        let to_skip = total_bytes.saturating_sub(self.bytes_read);

        if to_skip > 0 {
            self.reader.skip(to_skip)?;
        }

        self.bytes_read = total_bytes;
        Ok(())
    }

    fn applies_option_records(&self) -> bool {
        true
    }

    fn finish(&mut self, reached_end: bool) -> PaxResult<()> {
        self.reader.finish(reached_end)
    }
}

/// pax archive writer
pub struct PaxWriter<W: Write> {
    writer: W,
    bytes_written: u64,
    current_size: u64,
    sequence: u64,          // For generating unique names for extended header files
    options: FormatOptions, // Format-specific options
    global_header_written: bool, // Track if global header has been written
    /// Skip data writes for a symlink, which has no data blocks
    skip_data: bool,
}

impl<W: Write> PaxWriter<W> {
    /// Create a new pax writer with specified options.
    ///
    /// There is deliberately no options-free constructor: the one caller that
    /// used it was append mode, where it silently discarded every `-o` option
    /// the user had passed.
    pub fn with_options(writer: W, options: FormatOptions) -> Self {
        PaxWriter {
            writer,
            bytes_written: 0,
            current_size: 0,
            sequence: 0,
            options,
            global_header_written: false,
            skip_data: false,
        }
    }

    /// Write global extended header (typeflag 'g') if there are global options
    ///
    /// Global extended headers apply to all subsequent files in the archive.
    /// This is called once before the first entry when -o keyword=value options
    /// are specified (not keyword:=value which are per-file).
    fn write_global_header(&mut self) -> PaxResult<()> {
        if self.global_header_written {
            return Ok(());
        }
        self.global_header_written = true;

        // Get global options, filtering out special non-header keywords
        let global_opts = self.options.global_options();
        let special_keywords = ["invalid", "listopt", "exthdr.name", "globexthdr.name"];

        // Build extended header data from global options
        let mut data = Vec::new();
        let mut global_sorted: Vec<_> = global_opts.iter().collect();
        global_sorted.sort_by(|a, b| a.0.cmp(b.0));
        for (key, value) in global_sorted {
            // Skip special keywords that aren't actual extended header fields
            if special_keywords.contains(&key.as_str()) {
                continue;
            }
            // Skip if this keyword should be deleted
            if self.options.should_delete_keyword(key) {
                continue;
            }
            write_pax_record(&mut data, key, value);
        }

        // If no actual header data to write, skip
        if data.is_empty() {
            return Ok(());
        }

        // Create a header for the global extended header block
        let mut header = [0u8; BLOCK_SIZE];

        // Named by the globexthdr.name template. Its default is under
        // $TMPDIR, which can be too long for the header; the same name without
        // the directory is used then.
        self.sequence += 1;
        let glob_name = self.options.expand_globexthdr_name(self.sequence);
        let file_name = std::path::Path::new(&glob_name).file_name();
        write_header_name(&mut header, glob_name.as_bytes(), || {
            file_name.unwrap_or_default().as_bytes().to_vec()
        });

        // Mode, uid, gid (use reasonable defaults)
        write_octal(&mut header[MODE_OFF..], 0o644, 8);
        write_octal(&mut header[UID_OFF..], 0, 8);
        write_octal(&mut header[GID_OFF..], 0, 8);

        // Size of global header data
        write_octal(&mut header[SIZE_OFF..], data.len() as u64, 12);

        // Mtime (use current time)
        let mtime = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);
        write_octal(&mut header[MTIME_OFF..], mtime, 12);

        // Typeflag 'g' for global extended header
        header[TYPEFLAG_OFF] = PAX_GHDR;

        // Magic and version
        header[MAGIC_OFF..MAGIC_OFF + 6].copy_from_slice(b"ustar\0");
        header[VERSION_OFF..VERSION_OFF + 2].copy_from_slice(b"00");

        // Calculate and write checksum
        let checksum = calculate_checksum(&header);
        write_octal(&mut header[CHKSUM_OFF..], checksum as u64, 8);

        // Write header
        self.writer.write_all(&header)?;

        // Write global header data
        self.writer.write_all(&data)?;

        // Pad to block boundary using static buffer
        let padding = padding_needed(data.len() as u64);
        if padding > 0 {
            self.writer.write_all(&ZERO_BLOCK[..padding])?;
        }

        Ok(())
    }

    /// Write extended header block
    fn write_extended_header(
        &mut self,
        ext_header: &ExtendedHeader,
        entry: &ArchiveEntry,
    ) -> PaxResult<()> {
        // Serialize with options to respect delete patterns
        let data = ext_header.serialize(&self.options);
        if data.is_empty() {
            return Ok(());
        }

        // Create a header for the extended header block
        let mut header = [0u8; BLOCK_SIZE];

        // Named by the exthdr.name template, from the member's pathname. A
        // name too long for the header is formed by the same template from
        // the file's name alone, as though it were at the top level.
        self.sequence += 1;
        let ext_name = self.options.expand_exthdr_name(&entry.path, self.sequence);
        write_header_name(&mut header, ext_name.as_bytes(), || {
            let file_name = entry.path.file_name().unwrap_or(entry.path.as_os_str());
            self.options
                .expand_exthdr_name(std::path::Path::new(file_name), self.sequence)
                .into_bytes()
        });

        // Mode, uid, gid (use reasonable defaults)
        write_octal(&mut header[MODE_OFF..], 0o644, 8);
        write_octal(&mut header[UID_OFF..], 0, 8);
        write_octal(&mut header[GID_OFF..], 0, 8);

        // Size of extended header data
        write_octal(&mut header[SIZE_OFF..], data.len() as u64, 12);

        // Mtime (use entry's mtime)
        write_octal(&mut header[MTIME_OFF..], ustar_time(entry.mtime), 12);

        // Typeflag 'x' for per-file extended header
        header[TYPEFLAG_OFF] = PAX_XHDR;

        // Magic and version
        header[MAGIC_OFF..MAGIC_OFF + 6].copy_from_slice(b"ustar\0");
        header[VERSION_OFF..VERSION_OFF + 2].copy_from_slice(b"00");

        // Calculate and write checksum
        let checksum = calculate_checksum(&header);
        write_octal(&mut header[CHKSUM_OFF..], checksum as u64, 8);

        // Write header
        self.writer.write_all(&header)?;

        // Write extended header data
        self.writer.write_all(&data)?;

        // Pad to block boundary using static buffer
        let padding = padding_needed(data.len() as u64);
        if padding > 0 {
            self.writer.write_all(&ZERO_BLOCK[..padding])?;
        }

        Ok(())
    }
}

impl<W: Write> ArchiveWriter for PaxWriter<W> {
    fn write_entry(&mut self, entry: &ArchiveEntry) -> PaxResult<()> {
        // Write global header if this is the first entry and we have global options
        self.write_global_header()?;

        // Build extended header (respecting -o times option). Emit an `x`
        // extended header only when some field actually needs one; a pax archive
        // with no extended records is a valid ustar archive and reads back
        // identically, so there is no need to force an mtime record.
        let ext_header = ExtendedHeader::from_entry(entry, &self.options);
        ext_header.check_name_limit()?;

        // Built before anything is written, so that a member this format
        // cannot hold is refused without leaving its `x` header behind to
        // describe whatever member comes next.
        let header = build_ustar_header(entry)?;
        self.write_extended_header(&ext_header, entry)?;
        self.writer.write_all(&header)?;
        self.bytes_written = 0;
        self.current_size = entry.size;
        // A symlink has no data blocks. A hard link has them exactly when the
        // caller gave it a size, which is `-o linkdata`.
        self.skip_data = entry.entry_type == EntryType::Symlink;
        Ok(())
    }

    fn hardlinks_may_carry_data(&self) -> bool {
        true
    }

    fn write_data(&mut self, data: &[u8]) -> PaxResult<()> {
        // Symlinks have no data blocks
        if self.skip_data {
            return Ok(());
        }
        self.writer.write_all(data)?;
        self.bytes_written += data.len() as u64;
        Ok(())
    }

    fn finish_entry(&mut self) -> PaxResult<()> {
        // Pad to block boundary using static buffer
        let padding = padding_needed(self.bytes_written);
        if padding > 0 {
            self.writer.write_all(&ZERO_BLOCK[..padding])?;
        }
        self.skip_data = false;
        Ok(())
    }

    fn finish(&mut self) -> PaxResult<()> {
        // Write two zero blocks using static buffer
        self.writer.write_all(&ZERO_BLOCK)?;
        self.writer.write_all(&ZERO_BLOCK)?;
        self.writer.flush()?;
        Ok(())
    }
}

// ============================================================================
// Helper functions (shared with ustar where needed)
// ============================================================================

/// Build a ustar header block from an ArchiveEntry
fn build_ustar_header(entry: &ArchiveEntry) -> PaxResult<[u8; BLOCK_SIZE]> {
    let mut header = [0u8; BLOCK_SIZE];

    // Split path into name and prefix if needed
    let (name, prefix) = split_path(entry)?;

    // Write fields
    write_field(&mut header[NAME_OFF..], &name, NAME_LEN);
    write_octal(&mut header[MODE_OFF..], entry.mode as u64, 8);
    write_octal(
        &mut header[UID_OFF..],
        std::cmp::min(entry.uid as u64, 0o7777777),
        8,
    );
    write_octal(
        &mut header[GID_OFF..],
        std::cmp::min(entry.gid as u64, 0o7777777),
        8,
    );
    // A symlink records size 0 (no data blocks). A hard link records the
    // size of the data it carries: none, unless `-o linkdata` -- pax, unlike
    // ustar, "may" include data blocks for typeflag 1.
    let header_size = match entry.entry_type {
        EntryType::Symlink => 0,
        _ => std::cmp::min(entry.size, 0o77777777777),
    };
    write_octal(&mut header[SIZE_OFF..], header_size, 12);
    // Outside the field's range the `mtime` record holds the real value.
    write_octal(&mut header[MTIME_OFF..], ustar_time(entry.mtime), 12);

    // Typeflag
    header[TYPEFLAG_OFF] = entry_type_to_flag(entry.entry_type)?;

    // Linkname
    if let Some(ref target) = entry.link_target {
        let link_bytes = crate::rawpath::as_bytes(target);
        // Truncate on a character boundary where there is one; the full target
        // is in the `linkpath` extended record whenever it exceeds
        // LINKNAME_LEN, so this field is only a fallback for a reader that
        // ignores extended headers.
        let truncated = &link_bytes[..floor_char_boundary(link_bytes, LINKNAME_LEN)];
        write_field(&mut header[LINKNAME_OFF..], truncated, LINKNAME_LEN);
    }

    // Magic and version
    header[MAGIC_OFF..MAGIC_OFF + 6].copy_from_slice(b"ustar\0");
    header[VERSION_OFF..VERSION_OFF + 2].copy_from_slice(b"00");

    // uname and gname
    if let Some(ref uname) = entry.uname {
        write_field(&mut header[UNAME_OFF..], uname, UNAME_LEN);
    }
    if let Some(ref gname) = entry.gname {
        write_field(&mut header[GNAME_OFF..], gname, GNAME_LEN);
    }

    // Device major/minor (always written for POSIX compliance)
    write_octal(&mut header[DEVMAJOR_OFF..], entry.devmajor as u64, 8);
    write_octal(&mut header[DEVMINOR_OFF..], entry.devminor as u64, 8);

    // Prefix
    write_field(&mut header[PREFIX_OFF..], &prefix, PREFIX_LEN);

    // Calculate and write checksum
    let checksum = calculate_checksum(&header);
    write_octal(&mut header[CHKSUM_OFF..], checksum as u64, 8);

    Ok(header)
}

/// Split path into the ustar name and prefix fields.
///
/// Unlike ustar, a path that does not fit is not an error here: the real path
/// is already recorded in a `path=` extended header record by
/// `ExtendedHeader::from_entry`, so these fields are only a fallback for a
/// reader that ignores extended headers. Truncate on a UTF-8 character
/// boundary so a multi-byte character straddling NAME_LEN does not panic.
fn split_path(entry: &ArchiveEntry) -> PaxResult<(Vec<u8>, Vec<u8>)> {
    let path = ustar_path_bytes(entry);

    if let Some(split) = try_split_path(&path) {
        return Ok(split);
    }

    Ok((
        path[..floor_char_boundary(&path, NAME_LEN)].to_vec(),
        Vec::new(),
    ))
}

/// Put an extended header's name in the name and prefix fields, split as a
/// member's pathname would be.
///
/// When it does not fit, `shorter` supplies the name to use instead, and that
/// is cut to the name field if it does not fit either. Nothing reads these
/// names back -- a reader that knows the format consumes the header, and one
/// that does not extracts it as a file -- so what matters is only that the
/// fallback keeps the template's shape, and with it a relative name for a
/// relative member.
fn write_header_name(
    header: &mut [u8; BLOCK_SIZE],
    name: &[u8],
    shorter: impl FnOnce() -> Vec<u8>,
) {
    let (name, prefix) = try_split_path(name).unwrap_or_else(|| {
        let short = shorter();
        try_split_path(&short).unwrap_or_else(|| {
            let end = floor_char_boundary(&short, NAME_LEN);
            (short[..end].to_vec(), Vec::new())
        })
    });
    write_field(&mut header[NAME_OFF..], &name, NAME_LEN);
    write_field(&mut header[PREFIX_OFF..], &prefix, PREFIX_LEN);
}

/// Largest index `<= max` that does not cut a UTF-8 character of `bytes` in
/// half.
///
/// Truncating mid-character produces a field a legacy reader renders as
/// mojibake, so back up over continuation bytes. At most three of them can
/// precede a lead byte, and stopping there is what keeps this well-behaved on
/// a name that is not UTF-8 at all -- where every byte may look like a
/// continuation and there is no boundary to find.
fn floor_char_boundary(bytes: &[u8], max: usize) -> usize {
    if max >= bytes.len() {
        return bytes.len();
    }
    let mut end = max;
    for _ in 0..3 {
        if end == 0 || bytes[end] & 0xC0 != 0x80 {
            break;
        }
        end -= 1;
    }
    end
}

/// Write an octal number to a field
fn write_octal(buf: &mut [u8], val: u64, width: usize) {
    let s = format!("{:0width$o} ", val, width = width - 2);
    let bytes = s.as_bytes();
    let len = std::cmp::min(bytes.len(), width);
    buf[..len].copy_from_slice(&bytes[..len]);
}

/// The largest time the ustar header's 12-byte octal `mtime` field holds.
const USTAR_TIME_MAX: i64 = 0o77777777777;

/// A time as the ustar `mtime` field can hold it: clamped into its range.
///
/// Only a fallback for a reader that ignores extended headers -- a time
/// outside the range also gets an `mtime` record. Writing the value as it was
/// did not fail but wrapped: the two's-complement bits of a time before 1970
/// truncated to eleven octal digits read back as a date in the 2500s.
fn ustar_time(time: i64) -> u64 {
    time.clamp(0, USTAR_TIME_MAX) as u64
}

/// Round up to next block boundary
fn round_up_block(size: u64) -> u64 {
    // A `size=` extended-header record can declare u64::MAX, and rounding that
    // up overflows to 0 -- after which the skip length underflows and the
    // reader walks the rest of the archive as member data.
    size.div_ceil(BLOCK_SIZE as u64)
        .saturating_mul(BLOCK_SIZE as u64)
}

/// Calculate padding needed to reach block boundary
fn padding_needed(bytes: u64) -> usize {
    let remainder = (bytes % BLOCK_SIZE as u64) as usize;
    if remainder == 0 {
        0
    } else {
        BLOCK_SIZE - remainder
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// STANDARD_KEYWORDS, `set_keyword` and `holds` describe the same set of
    /// typed fields from three directions, and used to be three separately
    /// written-out lists. A keyword added to one and not the others is silent:
    /// it would be parsed into `extra`, then serialized twice, or be unable to
    /// take a `-o keyword:=value` override. This is what keeps them together.
    /// `hdrcharset` governs the path, linkpath, uname and gname records
    /// alike, so a value that is not valid UTF-8 in *any* of the four has to
    /// bring the BINARY declaration with it. Only the two pathnames did, so a
    /// host whose passwd database holds a non-UTF-8 account name wrote
    /// `uname=<raw bytes>` under a header declaring POSIX's implicit UTF-8 --
    /// bytes a conforming reader must then decode as UTF-8.
    #[test]
    fn test_binary_user_name_declares_the_header_charset() {
        let opts = FormatOptions::default();

        let mut entry = ArchiveEntry::new(PathBuf::from("ascii.txt"), EntryType::Regular);
        entry.uname = Some(b"us\xffr".to_vec());
        let header = ExtendedHeader::from_entry(&entry, &opts);
        assert_eq!(
            header.hdrcharset.as_deref(),
            Some("BINARY"),
            "a uname that is not UTF-8 must be announced as BINARY"
        );
        assert_eq!(header.uname.as_deref(), Some(b"us\xffr".as_slice()));

        // The group name alone is enough too.
        let mut entry = ArchiveEntry::new(PathBuf::from("ascii.txt"), EntryType::Regular);
        entry.gname = Some(b"gr\xffup".to_vec());
        assert_eq!(
            ExtendedHeader::from_entry(&entry, &opts)
                .hdrcharset
                .as_deref(),
            Some("BINARY")
        );

        // A name that is non-ASCII but valid UTF-8 needs the record, because
        // the ustar field is limited to the portable character set -- but no
        // declaration, since UTF-8 is the default the archive already implies.
        let mut entry = ArchiveEntry::new(PathBuf::from("ascii.txt"), EntryType::Regular);
        entry.uname = Some("ünïcode".as_bytes().to_vec());
        let header = ExtendedHeader::from_entry(&entry, &opts);
        assert_eq!(header.hdrcharset, None);
        assert_eq!(header.uname.as_deref(), Some("ünïcode".as_bytes()));

        // And a plain ASCII name needs neither.
        let mut entry = ArchiveEntry::new(PathBuf::from("ascii.txt"), EntryType::Regular);
        entry.uname = Some(b"root".to_vec());
        let header = ExtendedHeader::from_entry(&entry, &opts);
        assert_eq!(header.hdrcharset, None);
        assert_eq!(header.uname, None);
    }

    #[test]
    fn test_standard_keywords_are_typed() {
        for &keyword in STANDARD_KEYWORDS {
            let mut header = ExtendedHeader::new();
            // "1" parses as a time, a size, an id and a name alike.
            header.set_keyword(keyword, "1").unwrap();
            assert!(
                header.extra.is_empty(),
                "{keyword} is in STANDARD_KEYWORDS but set_keyword put it in `extra`"
            );
            assert!(
                header.holds(keyword),
                "{keyword} is in STANDARD_KEYWORDS but `holds` does not see it"
            );

            let mut merged = ExtendedHeader::new();
            merged.merge(&header);
            assert!(
                merged.holds(keyword),
                "{keyword} is in STANDARD_KEYWORDS but `merge` does not carry it"
            );
            merged.clear(keyword);
            assert!(
                !merged.holds(keyword),
                "{keyword} is in STANDARD_KEYWORDS but `clear` does not drop it"
            );
        }

        // And the converse: a keyword outside the list does land in `extra`,
        // which is what makes `holds` the right test for "already written".
        let mut header = ExtendedHeader::new();
        header.set_keyword("charset", "BINARY").unwrap();
        assert!(!header.holds("charset"));
        assert_eq!(header.extra.len(), 1);
    }

    #[test]
    fn test_write_pax_record() {
        let mut data = Vec::new();
        write_pax_record(&mut data, "path", "/some/path");
        let s = String::from_utf8(data).unwrap();
        // Record format: "len path=/some/path\n"
        // len includes itself + " " + "path=/some/path\n" = 2 + 1 + 16 = 19 chars
        assert_eq!(s, "19 path=/some/path\n");
    }

    #[test]
    fn test_parse_pax_time() {
        assert_eq!(
            parse_pax_time("1234567890").unwrap(),
            PaxTime {
                sec: 1234567890,
                nsec: 0
            }
        );
        // Exact nanoseconds, including the full 9-digit tail that f64 lost.
        assert_eq!(
            parse_pax_time("1577880000.123456789").unwrap(),
            PaxTime {
                sec: 1577880000,
                nsec: 123456789
            }
        );
        // Fewer than 9 fractional digits are zero-padded on the right.
        assert_eq!(
            parse_pax_time("1234567890.5").unwrap(),
            PaxTime {
                sec: 1234567890,
                nsec: 500000000
            }
        );
    }

    /// A time before the Epoch is the signed decimal value: "-1.5" is a
    /// second and a half before it, held as -2 s + 0.5 s.
    #[test]
    fn test_negative_pax_times_roundtrip() {
        for (text, sec, nsec) in [
            ("-86400", -86400, 0),
            ("-1.5", -2, 500_000_000),
            ("-0.5", -1, 500_000_000),
            ("-0.000000001", -1, 999_999_999),
            ("-10.25", -11, 750_000_000),
        ] {
            let time = PaxTime { sec, nsec };
            assert_eq!(parse_pax_time(text).unwrap(), time, "parse {text}");
            assert_eq!(format_pax_time(time), text, "format {text}");
        }
        assert!(parse_pax_time(&format!("{}.5", i64::MIN)).is_err());
    }

    /// Digits past the ninth are dropped, which rounds toward zero. For a
    /// time before the Epoch that is upward, so the time held is the one
    /// rounded down -- the same direction as for a time after it.
    #[test]
    fn test_negative_pax_time_beyond_nanoseconds_rounds_down() {
        for (text, sec, nsec) in [
            ("-1.0000000001", -2, 999_999_999),
            ("-0.9999999999", -1, 0),
            ("-1.0000000000", -1, 0),
            ("1.0000000009", 1, 0),
        ] {
            assert_eq!(
                parse_pax_time(text).unwrap(),
                PaxTime { sec, nsec },
                "parse {text}"
            );
        }
    }

    /// The reader's limit on a `path` record is the writer's too: a name it
    /// would refuse to read back is refused when written.
    #[test]
    fn test_path_record_beyond_the_name_limit_is_refused_on_write() {
        let long = "n".repeat(MAX_NAME as usize + 1);
        let entry = ArchiveEntry::new(PathBuf::from(&long), EntryType::Regular);
        let mut out = Vec::new();
        let mut writer = PaxWriter::with_options(&mut out, FormatOptions::default());
        assert!(writer.write_entry(&entry).is_err());
        assert!(out.is_empty(), "no header may be left behind");
    }

    /// A time the ustar field cannot hold gets an `mtime` record, and the
    /// field itself is clamped rather than wrapped.
    #[test]
    fn test_out_of_range_mtime_gets_a_record() {
        let opts = FormatOptions::default();
        for mtime in [-86400, USTAR_TIME_MAX + 1] {
            let mut entry = ArchiveEntry::new(PathBuf::from("f"), EntryType::Regular);
            entry.mtime = mtime;
            let header = ExtendedHeader::from_entry(&entry, &opts);
            assert_eq!(
                header.mtime,
                Some(PaxTime {
                    sec: mtime,
                    nsec: 0
                })
            );
        }
        assert_eq!(ustar_time(-86400), 0);
        assert_eq!(ustar_time(i64::MAX), USTAR_TIME_MAX as u64);

        let mut entry = ArchiveEntry::new(PathBuf::from("f"), EntryType::Regular);
        entry.mtime = 1_000_000_000;
        assert_eq!(ExtendedHeader::from_entry(&entry, &opts).mtime, None);
    }

    #[test]
    fn test_format_pax_time() {
        assert_eq!(
            format_pax_time(PaxTime {
                sec: 1234567890,
                nsec: 0
            }),
            "1234567890"
        );
        assert_eq!(
            format_pax_time(PaxTime {
                sec: 1234567890,
                nsec: 500000000
            }),
            "1234567890.5"
        );
        // Full nanosecond precision survives the round-trip exactly.
        assert_eq!(
            format_pax_time(PaxTime {
                sec: 1577880000,
                nsec: 123456789
            }),
            "1577880000.123456789"
        );
    }

    #[test]
    fn test_extended_header_roundtrip() {
        let mut ext = ExtendedHeader::new();
        ext.path = Some(b"/very/long/path/that/exceeds/ustar/limits".to_vec());
        ext.size = Some(10000000000);
        ext.mtime = Some(PaxTime {
            sec: 1234567890,
            nsec: 123456789,
        });

        let data = ext.serialize(&FormatOptions::default());
        let parsed = ExtendedHeader::parse(&data).unwrap();

        assert_eq!(parsed.path, ext.path);
        assert_eq!(parsed.size, ext.size);
        // Nanoseconds round-trip exactly (no f64 precision loss).
        assert_eq!(parsed.mtime, ext.mtime);
    }

    #[test]
    fn test_extended_header_from_entry() {
        let mut entry = ArchiveEntry::new(PathBuf::from("test.txt"), EntryType::Regular);
        entry.uid = 3000000; // > 2097151
        entry.mtime_nsec = 500000000; // 0.5 seconds

        let ext = ExtendedHeader::from_entry(&entry, &FormatOptions::default());
        assert!(ext.uid.is_some());
        assert!(ext.mtime.is_some());
    }

    /// A pax archive of one empty member whose name needs a `path=` record.
    fn archive_with_extended_header() -> Vec<u8> {
        let mut out = Vec::new();
        let mut writer = PaxWriter::with_options(&mut out, FormatOptions::default());
        let name = "n".repeat(NAME_LEN + 1);
        let entry = ArchiveEntry::new(PathBuf::from(name), EntryType::Regular);
        writer.write_entry(&entry).unwrap();
        writer.finish_entry().unwrap();
        writer.finish().unwrap();
        out
    }

    /// An extended header whose templated name is too long for the header is
    /// named by the same template from the file's name alone -- still
    /// relative, never an unrelated fixed name.
    #[test]
    fn test_long_extended_header_name_keeps_the_template() {
        let mut out = Vec::new();
        let mut writer = PaxWriter::with_options(&mut out, FormatOptions::default());
        let path = format!("{}/{}", "x".repeat(200), "y".repeat(90));
        let entry = ArchiveEntry::new(PathBuf::from(path), EntryType::Regular);
        writer.write_entry(&entry).unwrap();
        assert_eq!(out[TYPEFLAG_OFF], PAX_XHDR);
        let name = crate::formats::ustar::path_field(&out[NAME_OFF..NAME_OFF + NAME_LEN]);
        let prefix = crate::formats::ustar::path_field(&out[PREFIX_OFF..PREFIX_OFF + PREFIX_LEN]);
        assert_eq!(name, "y".repeat(90).as_bytes());
        assert_eq!(
            prefix,
            format!("./PaxHeaders.{}", std::process::id()).as_bytes()
        );
    }

    fn end_of(archive: &[u8]) -> (u64, bool) {
        let mut reader = PaxReader::seekable(std::io::Cursor::new(archive));
        while reader.read_entry().unwrap().is_some() {}
        (reader.end_of_archive(), reader.saw_extended_header())
    }

    /// -a writes where `end_of_archive` says, so it has to be the first block
    /// of the end-of-archive indicator.
    #[test]
    fn test_end_of_archive_is_the_indicator() {
        let archive = archive_with_extended_header();
        let indicator = archive.len() as u64 - 2 * BLOCK_SIZE as u64;
        assert_eq!(end_of(&archive), (indicator, true));
    }

    /// An `x` header with no member after it would describe whatever is
    /// appended next, so the end is placed before it.
    #[test]
    fn test_end_of_archive_drops_a_dangling_extended_header() {
        let archive = archive_with_extended_header();
        // The extended header is everything before the member's own header
        // block and the two-block indicator.
        let dangling = archive.len() - 3 * BLOCK_SIZE;
        let mut truncated = archive[..dangling].to_vec();
        truncated.extend_from_slice(&[0u8; 2 * BLOCK_SIZE]);
        assert_eq!(end_of(&truncated), (0, true));
    }
}
