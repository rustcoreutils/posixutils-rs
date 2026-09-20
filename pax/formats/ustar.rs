//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX ustar (tar) format implementation
//!
//! Header format (512 bytes):
//! - name:     100 bytes (offset 0)
//! - mode:       8 bytes (offset 100)
//! - uid:        8 bytes (offset 108)
//! - gid:        8 bytes (offset 116)
//! - size:      12 bytes (offset 124)
//! - mtime:     12 bytes (offset 136)
//! - chksum:     8 bytes (offset 148)
//! - typeflag:   1 byte  (offset 156)
//! - linkname: 100 bytes (offset 157)
//! - magic:      6 bytes (offset 257) "ustar\0"
//! - version:    2 bytes (offset 263) "00"
//! - uname:     32 bytes (offset 265)
//! - gname:     32 bytes (offset 297)
//! - devmajor:   8 bytes (offset 329)
//! - devminor:   8 bytes (offset 337)
//! - prefix:   155 bytes (offset 345)

use crate::archive::{ArchiveEntry, ArchiveReader, ArchiveWriter, EntryType};
use crate::error::{PaxError, PaxResult};
use std::io::{Read, Write};
use std::path::PathBuf;

const BLOCK_SIZE: usize = 512;
/// Static zero buffer for padding and end-of-archive markers
static ZERO_BLOCK: [u8; BLOCK_SIZE] = [0u8; BLOCK_SIZE];
const NAME_LEN: usize = 100;
const PREFIX_LEN: usize = 155;
const LINKNAME_LEN: usize = 100;
const UNAME_LEN: usize = 32;
const GNAME_LEN: usize = 32;

// Header field offsets
const NAME_OFF: usize = 0;
const MODE_OFF: usize = 100;
const UID_OFF: usize = 108;
const GID_OFF: usize = 116;
const SIZE_OFF: usize = 124;
const MTIME_OFF: usize = 136;
const CHKSUM_OFF: usize = 148;
const TYPEFLAG_OFF: usize = 156;
const LINKNAME_OFF: usize = 157;
const MAGIC_OFF: usize = 257;
const VERSION_OFF: usize = 263;
const UNAME_OFF: usize = 265;
const GNAME_OFF: usize = 297;
const PREFIX_OFF: usize = 345;

// Type flags
const REGTYPE: u8 = b'0';
const AREGTYPE: u8 = b'\0';
const LNKTYPE: u8 = b'1';
const SYMTYPE: u8 = b'2';
const CHRTYPE: u8 = b'3';
const BLKTYPE: u8 = b'4';
const DIRTYPE: u8 = b'5';
const FIFOTYPE: u8 = b'6';
const CONTTYPE: u8 = b'7';

// Device number field offsets and lengths
const DEVMAJOR_OFF: usize = 329;
const DEVMINOR_OFF: usize = 337;

/// ustar archive reader
pub struct UstarReader<R: Read> {
    reader: R,
    current_size: u64,
    bytes_read: u64,
}

impl<R: Read> UstarReader<R> {
    /// Create a new ustar reader
    pub fn new(reader: R) -> Self {
        UstarReader {
            reader,
            current_size: 0,
            bytes_read: 0,
        }
    }
}

impl<R: Read> ArchiveReader for UstarReader<R> {
    fn read_entry(&mut self) -> PaxResult<Option<ArchiveEntry>> {
        // Skip any remaining data from previous entry
        self.skip_data()?;

        loop {
            let Some(header) = next_header_block(&mut self.reader)? else {
                return Ok(None);
            };

            // Verify checksum
            if !verify_checksum(&header) {
                return Err(PaxError::InvalidHeader("checksum mismatch".to_string()));
            }

            // A GNU long-name record describes the member that follows, whose
            // own name field is truncated. The records and the member are
            // dropped together -- there can be more than one record.
            if long_name_record(header[TYPEFLAG_OFF]).is_some() {
                skip_long_name_records(&mut self.reader, header)?;
                continue;
            }

            let entry = parse_header(&header, SizeRule::Ustar)?;
            self.current_size = entry.size;
            self.bytes_read = 0;

            return Ok(Some(entry));
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
        // Calculate total bytes including padding to block boundary
        let total_bytes = round_up_block(self.current_size);
        let to_skip = total_bytes.saturating_sub(self.bytes_read);

        if to_skip > 0 {
            skip_bytes(&mut self.reader, to_skip)?;
        }

        // Reset state - we've finished with this entry's data
        self.bytes_read = total_bytes;
        Ok(())
    }
}

/// ustar archive writer
pub struct UstarWriter<W: Write> {
    writer: W,
    bytes_written: u64,
    current_size: u64,
    /// Skip data writes for symlinks/hardlinks (they have no data in ustar format)
    skip_data: bool,
}

impl<W: Write> UstarWriter<W> {
    /// Create a new ustar writer
    pub fn new(writer: W) -> Self {
        UstarWriter {
            writer,
            bytes_written: 0,
            current_size: 0,
            skip_data: false,
        }
    }
}

impl<W: Write> ArchiveWriter for UstarWriter<W> {
    fn write_entry(&mut self, entry: &ArchiveEntry) -> PaxResult<()> {
        let header = build_header(entry)?;
        self.writer.write_all(&header)?;
        self.bytes_written = 0;
        self.current_size = entry.size;
        // Per POSIX, symlinks and hardlinks have no data blocks in ustar format
        self.skip_data = matches!(entry.entry_type, EntryType::Symlink | EntryType::Hardlink);
        Ok(())
    }

    fn write_data(&mut self, data: &[u8]) -> PaxResult<()> {
        // Symlinks/hardlinks have no data blocks in ustar format
        if self.skip_data {
            return Ok(());
        }
        self.writer.write_all(data)?;
        self.bytes_written += data.len() as u64;
        Ok(())
    }

    fn finish_entry(&mut self) -> PaxResult<()> {
        // Pad to block boundary using static zero buffer
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
// Header parsing functions
// ============================================================================

/// Check if a block is all zeros
pub(crate) fn is_zero_block(block: &[u8]) -> bool {
    block.iter().all(|&b| b == 0)
}

/// Parse a header block into an ArchiveEntry
/// Which format's rules govern a header's size field.
///
/// POSIX (pax, "No data logical records are stored for types 1, 2, or 5") makes
/// the size field of those three headers not a data length. Honouring it anyway
/// steps the reader over blocks that are the *next member's header*, so that
/// member becomes invisible -- to us, but not to GNU tar, which reads them as
/// headers. A member visible to one tool and not the other is how content gets
/// past a scanner.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum SizeRule {
    /// POSIX ustar: types 1, 2 and 5 are followed by no data blocks.
    Ustar,
    /// pax interchange format, which differs in exactly one place: data blocks
    /// for a typeflag 1 (hard link) "may be included, which means that the size
    /// field may be greater than zero". That is `-o linkdata`, and bsdtar
    /// writes it.
    Pax,
}

impl SizeRule {
    /// The number of data bytes that actually follow this header.
    fn data_size(self, entry_type: EntryType, declared: u64) -> u64 {
        match entry_type {
            // A directory's size field is a directory size limit, not a
            // length: POSIX says a system that does not implement such
            // limiting "should ignore the size field". Nothing malformed
            // about a non-zero one, so no diagnostic.
            EntryType::Directory => 0,
            EntryType::Symlink => 0,
            EntryType::Hardlink if self == SizeRule::Ustar => 0,
            _ => declared,
        }
    }

    /// Whether a non-zero size field on this type is malformed, as opposed to
    /// merely ignorable. POSIX requires types 1 and 2 to record zero.
    fn size_must_be_zero(self, entry_type: EntryType) -> bool {
        match entry_type {
            EntryType::Symlink => true,
            EntryType::Hardlink => self == SizeRule::Ustar,
            _ => false,
        }
    }
}

pub(crate) fn parse_header(header: &[u8; BLOCK_SIZE], rule: SizeRule) -> PaxResult<ArchiveEntry> {
    let name = parse_path_field(&header[NAME_OFF..NAME_OFF + NAME_LEN]);
    let prefix = parse_path_field(&header[PREFIX_OFF..PREFIX_OFF + PREFIX_LEN]);

    let path = build_path(&prefix, &name);

    let mode = parse_octal(&header[MODE_OFF..MODE_OFF + 8])? as u32;
    let uid = parse_octal(&header[UID_OFF..UID_OFF + 8])? as u32;
    let gid = parse_octal(&header[GID_OFF..GID_OFF + 8])? as u32;
    let declared_size = parse_octal(&header[SIZE_OFF..SIZE_OFF + 12])?;
    let mtime = parse_octal(&header[MTIME_OFF..MTIME_OFF + 12])?;

    let typeflag = header[TYPEFLAG_OFF];
    let flag = parse_typeflag(typeflag);
    let entry_type = flag.entry_type();
    let size = rule.data_size(entry_type, declared_size);

    let linkname = parse_path_field(&header[LINKNAME_OFF..LINKNAME_OFF + LINKNAME_LEN]);
    let link_target = if !linkname.is_empty() {
        Some(PathBuf::from(linkname))
    } else {
        None
    };

    // POSIX: "If conversion to a regular file occurs, the pax utility shall
    // produce an error indicating that the conversion took place."
    match flag {
        TypeFlag::Known(_) | TypeFlag::RegularByDefinition => {}
        TypeFlag::Unimplemented(what) => crate::error::report_error(
            path.display(),
            format!("is {what}, which is not supported; extracting as a regular file"),
        ),
        TypeFlag::Unknown => crate::error::report_error(
            path.display(),
            format!(
                "has unrecognized type {}; extracting as a regular file",
                show_typeflag(typeflag)
            ),
        ),
    }

    if declared_size != 0 && rule.size_must_be_zero(entry_type) {
        crate::error::report_error(
            path.display(),
            format!(
                "header of type {} records {} bytes of data, which POSIX \
                 requires to be zero; ignoring the size field",
                typeflag as char, declared_size
            ),
        );
    }

    let uname = parse_string(&header[UNAME_OFF..UNAME_OFF + UNAME_LEN]);
    let gname = parse_string(&header[GNAME_OFF..GNAME_OFF + GNAME_LEN]);

    // Parse device major/minor for block/char devices
    let devmajor = parse_octal(&header[DEVMAJOR_OFF..DEVMAJOR_OFF + 8])? as u32;
    let devminor = parse_octal(&header[DEVMINOR_OFF..DEVMINOR_OFF + 8])? as u32;

    Ok(ArchiveEntry {
        path,
        mode,
        uid,
        gid,
        size,
        mtime,
        entry_type,
        link_target,
        uname: if uname.is_empty() { None } else { Some(uname) },
        gname: if gname.is_empty() { None } else { Some(gname) },
        devmajor,
        devminor,
        ..Default::default()
    })
}

/// Parse a NUL-terminated or space-padded string field.
///
/// Used for the space-padded fields (uname, gname) and as the basis for the
/// numeric fields, so trailing whitespace is stripped.
pub(crate) fn parse_string(bytes: &[u8]) -> String {
    let end = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
    String::from_utf8_lossy(&bytes[..end])
        .trim_end()
        .to_string()
}

/// Parse a path field (name, prefix, linkname).
///
/// These are NUL-terminated and a trailing <space> is a legitimate pathname
/// character, so only the NUL terminator delimits the value — unlike the
/// space-padded fields, no whitespace is trimmed.
pub(crate) fn parse_path_field(bytes: &[u8]) -> String {
    let end = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
    String::from_utf8_lossy(&bytes[..end]).to_string()
}

/// Parse an octal number from bytes
pub(crate) fn parse_octal(bytes: &[u8]) -> PaxResult<u64> {
    let s = parse_string(bytes);
    if s.is_empty() {
        return Ok(0);
    }
    // Reject if the octal string contains a sign
    if s.starts_with('+') || s.starts_with('-') {
        return Err(PaxError::InvalidHeader(format!("invalid octal: {}", s)));
    }
    u64::from_str_radix(&s, 8).map_err(|_| PaxError::InvalidHeader(format!("invalid octal: {}", s)))
}

/// What a header's typeflag means, and why.
///
/// POSIX requires an unrecognised type to be extracted as a regular file *and*
/// an error produced saying the conversion took place. Collapsing every
/// unhandled flag straight to `Regular` loses the second half, and with it the
/// only sign that a member did not come back as what it went in as. The
/// distinction between "this is a regular file" and "we could not tell, so it
/// is one now" has to survive as far as the caller that knows the pathname.
pub(crate) enum TypeFlag {
    /// A ustar type with its own meaning, handled as such.
    Known(EntryType),
    /// A type POSIX defines as a regular file: typeflag 7, "reserved to
    /// represent a file to which an implementation has associated some
    /// high-performance attribute", which implementations without such
    /// extensions treat as type 0. No diagnostic; nothing was lost.
    RegularByDefinition,
    /// A GNU extension that is not implemented, named because its failure mode
    /// is silent corruption rather than a missing file -- a sparse member
    /// extracted as a regular file gets its sparse map as contents, and a
    /// volume label becomes a file named after the label.
    Unimplemented(&'static str),
    /// Anything else, including a value reserved for a future version of the
    /// standard.
    Unknown,
}

impl TypeFlag {
    /// The type to extract as. Everything that is not a known type is a
    /// regular file, per POSIX.
    pub(crate) fn entry_type(&self) -> EntryType {
        match self {
            TypeFlag::Known(t) => *t,
            _ => EntryType::Regular,
        }
    }
}

/// A typeflag as a diagnostic renders it.
///
/// Most are a printable character and read best as one. A value reserved for a
/// future version of the standard need not be printable at all, and writing it
/// raw would put a control byte -- an escape sequence, even -- on the terminal
/// of whoever listed the archive.
fn show_typeflag(flag: u8) -> String {
    if flag.is_ascii_graphic() {
        format!("'{}'", flag as char)
    } else {
        format!("'\\{flag:03o}'")
    }
}

/// The GNU long-name record types, which describe the *next* member rather
/// than a file of their own.
pub(crate) fn long_name_record(flag: u8) -> Option<&'static str> {
    match flag {
        b'L' => Some("a GNU long name record"),
        b'K' => Some("a GNU long link target record"),
        _ => None,
    }
}

/// Classify a header's typeflag.
pub(crate) fn parse_typeflag(flag: u8) -> TypeFlag {
    match flag {
        REGTYPE | AREGTYPE => TypeFlag::Known(EntryType::Regular),
        LNKTYPE => TypeFlag::Known(EntryType::Hardlink),
        SYMTYPE => TypeFlag::Known(EntryType::Symlink),
        CHRTYPE => TypeFlag::Known(EntryType::CharDevice),
        BLKTYPE => TypeFlag::Known(EntryType::BlockDevice),
        DIRTYPE => TypeFlag::Known(EntryType::Directory),
        FIFOTYPE => TypeFlag::Known(EntryType::Fifo),
        CONTTYPE => TypeFlag::RegularByDefinition,
        // The GNU extensions we can name. A member of one of these types is
        // not a regular file, and extracting it as one silently produces a
        // wrong file rather than no file.
        b'S' => TypeFlag::Unimplemented("a GNU sparse file"),
        b'V' => TypeFlag::Unimplemented("a GNU volume label"),
        b'L' => TypeFlag::Unimplemented("a GNU long name record"),
        b'K' => TypeFlag::Unimplemented("a GNU long link target record"),
        b'M' => TypeFlag::Unimplemented("a GNU multi-volume continuation"),
        b'D' | b'N' => TypeFlag::Unimplemented("a GNU incremental-dump record"),
        _ => TypeFlag::Unknown,
    }
}

/// Build full path from prefix and name
pub(crate) fn build_path(prefix: &str, name: &str) -> PathBuf {
    if prefix.is_empty() {
        PathBuf::from(name)
    } else {
        PathBuf::from(format!("{}/{}", prefix, name))
    }
}

/// Read one 512-byte block, or `None` at end of file.
fn read_block(reader: &mut impl Read) -> PaxResult<Option<[u8; BLOCK_SIZE]>> {
    let mut block = [0u8; BLOCK_SIZE];
    match reader.read_exact(&mut block) {
        Ok(()) => Ok(Some(block)),
        Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => Ok(None),
        Err(e) => Err(e.into()),
    }
}

/// The next header block, or `None` at the end of the archive.
///
/// The end-of-archive indicator is *two* 512-byte blocks of zeros (POSIX). A
/// single one is not an end marker, and stopping at it silently hides every
/// member that follows -- from us, while GNU tar reads them and says so. Stop
/// at it as GNU tar does, but say so too, so the truncation is visible rather
/// than looking like a clean end of archive.
///
/// GNU tar names the offset ("A lone zero block at 5"). This does not: the
/// only counter available here would see header blocks and not the data blocks
/// between them, so any number it produced would send someone to the wrong
/// place in the file. Counting correctly means threading a byte position
/// through every read and skip in both readers, which is more machinery than
/// a diagnostic detail is worth.
pub(crate) fn next_header_block(reader: &mut impl Read) -> PaxResult<Option<[u8; BLOCK_SIZE]>> {
    let Some(block) = read_block(reader)? else {
        return Ok(None);
    };
    if !is_zero_block(&block) {
        return Ok(Some(block));
    }

    match read_block(reader)? {
        // Two zero blocks, or one followed by end of file: a proper end.
        None => Ok(None),
        Some(next) if is_zero_block(&next) => Ok(None),
        Some(_) => {
            crate::error::report_error(
                "archive",
                "a lone zero block where the end-of-archive indicator should be; \
                 the rest of the archive is not being read",
            );
            Ok(None)
        }
    }
}

/// Read and discard a GNU `L`/`K` long-name record, returning its recorded
/// value.
///
/// These carry the real pathname (or link target) of the member that follows,
/// whose own header holds the value truncated to 100 bytes. A member may be
/// preceded by more than one: GNU tar writes `K` and then `L` when it has both
/// a long link target and a long name, so a reader that assumes exactly one
/// record mistakes the second record's *header* for the member, and then reads
/// the real member header as an ordinary one -- restoring it under the
/// truncated name it was trying to avoid. `skip_long_name_records` consumes the
/// whole run.
pub(crate) fn consume_long_name_record(
    reader: &mut impl Read,
    header: &[u8; BLOCK_SIZE],
    what: &str,
) -> PaxResult<Vec<u8>> {
    let size = parse_octal(&header[SIZE_OFF..SIZE_OFF + 12])?;
    let data = crate::formats::read_declared(reader, size, crate::formats::MAX_NAME, what)?;
    let padding = (BLOCK_SIZE - (size as usize % BLOCK_SIZE)) % BLOCK_SIZE;
    if padding > 0 {
        let mut pad = [0u8; BLOCK_SIZE];
        reader.read_exact(&mut pad[..padding])?;
    }

    let end = data.iter().position(|&b| b == 0).unwrap_or(data.len());
    Ok(data[..end].to_vec())
}

/// Consume every long-name record preceding a member, then the member itself,
/// and report the whole group as unsupported.
///
/// `header` is the first record's header. Returns once the member has been
/// stepped over, so the caller's next read is the following member.
///
/// Implementing the extension is separate work. What this avoids is the
/// alternative: extracting `././@LongLink` as a file of its own and the member
/// under a name truncated to 100 bytes -- two wrong files, and the name that
/// got path-checked is not the name the archive meant.
pub(crate) fn skip_long_name_records(
    reader: &mut impl Read,
    mut header: [u8; BLOCK_SIZE],
) -> PaxResult<()> {
    let mut long_name: Option<Vec<u8>> = None;
    let mut kinds: Vec<&'static str> = Vec::new();

    while let Some(what) = long_name_record(header[TYPEFLAG_OFF]) {
        let value = consume_long_name_record(reader, &header, what)?;
        // The `L` record holds the name; `K` holds the link target, which is
        // not what the member is called.
        if header[TYPEFLAG_OFF] == b'L' {
            long_name = Some(value);
        }
        kinds.push(what);

        let Some(next) = next_header_block(reader)? else {
            // The archive ends after the record, with no member to skip.
            report_long_name_group(&long_name, &header, &kinds);
            return Ok(());
        };
        if !verify_checksum(&next) {
            return Err(PaxError::InvalidHeader("checksum mismatch".to_string()));
        }
        header = next;
    }

    // `header` is now the member the records described.
    report_long_name_group(&long_name, &header, &kinds);
    skip_member_data(reader, &header)
}

/// Name the member that is being skipped, and the extensions that describe it.
fn report_long_name_group(
    long_name: &Option<Vec<u8>>,
    member: &[u8; BLOCK_SIZE],
    kinds: &[&'static str],
) {
    // The long name if the archive gave one, otherwise the truncated name in
    // the member's own header -- which is all there is to go on for a lone `K`.
    let name = match long_name {
        Some(n) => String::from_utf8_lossy(n).into_owned(),
        None => parse_path_field(&member[NAME_OFF..NAME_OFF + NAME_LEN]),
    };
    crate::error::report_error(
        name,
        format!(
            "uses {}, which is not supported; skipping the member",
            kinds.join(" and ")
        ),
    );
}

/// Step over a member's data blocks without interpreting them.
pub(crate) fn skip_member_data(reader: &mut impl Read, header: &[u8; BLOCK_SIZE]) -> PaxResult<()> {
    let size = parse_octal(&header[SIZE_OFF..SIZE_OFF + 12])?;
    skip_bytes(reader, round_up_block(size))
}

/// Verify header checksum
pub(crate) fn verify_checksum(header: &[u8; BLOCK_SIZE]) -> bool {
    let stored = match parse_octal(&header[CHKSUM_OFF..CHKSUM_OFF + 8]) {
        Ok(v) => v as u32,
        Err(_) => return false,
    };

    let calculated = calculate_checksum(header);
    stored == calculated
}

/// Calculate header checksum
pub(crate) fn calculate_checksum(header: &[u8; BLOCK_SIZE]) -> u32 {
    let mut sum: u32 = 0;
    for (i, &byte) in header.iter().enumerate() {
        if (CHKSUM_OFF..CHKSUM_OFF + 8).contains(&i) {
            sum += b' ' as u32;
        } else {
            sum += byte as u32;
        }
    }
    sum
}

// ============================================================================
// Header building functions
// ============================================================================

/// Build a header block from an ArchiveEntry
fn build_header(entry: &ArchiveEntry) -> PaxResult<[u8; BLOCK_SIZE]> {
    let mut header = [0u8; BLOCK_SIZE];

    // Split path into name and prefix if needed
    let (name, prefix) = split_path(entry)?;

    // Write fields
    write_string(&mut header[NAME_OFF..], &name, NAME_LEN);
    write_octal(&mut header[MODE_OFF..], entry.mode as u64, 8)?;
    write_octal(&mut header[UID_OFF..], entry.uid as u64, 8)?;
    write_octal(&mut header[GID_OFF..], entry.gid as u64, 8)?;
    // Per POSIX, symlinks and hardlinks must have size=0 (no data blocks)
    let header_size = match entry.entry_type {
        EntryType::Symlink | EntryType::Hardlink => 0,
        _ => entry.size,
    };
    write_octal(&mut header[SIZE_OFF..], header_size, 12)?;
    write_octal(&mut header[MTIME_OFF..], entry.mtime, 12)?;

    // Typeflag
    header[TYPEFLAG_OFF] = entry_type_to_flag(&entry.entry_type);

    // Linkname
    if let Some(ref target) = entry.link_target {
        let link_str = target.to_string_lossy();
        if link_str.len() > LINKNAME_LEN {
            return Err(PaxError::PathTooLong(link_str.to_string()));
        }
        write_string(&mut header[LINKNAME_OFF..], &link_str, LINKNAME_LEN);
    }

    // Magic and version
    header[MAGIC_OFF..MAGIC_OFF + 6].copy_from_slice(b"ustar\0");
    header[VERSION_OFF..VERSION_OFF + 2].copy_from_slice(b"00");

    // uname and gname
    if let Some(ref uname) = entry.uname {
        write_string(&mut header[UNAME_OFF..], uname, UNAME_LEN);
    }
    if let Some(ref gname) = entry.gname {
        write_string(&mut header[GNAME_OFF..], gname, GNAME_LEN);
    }

    // Device major/minor (always written for POSIX compliance)
    write_octal(&mut header[DEVMAJOR_OFF..], entry.devmajor as u64, 8)?;
    write_octal(&mut header[DEVMINOR_OFF..], entry.devminor as u64, 8)?;

    // Prefix
    write_string(&mut header[PREFIX_OFF..], &prefix, PREFIX_LEN);

    // Calculate and write checksum
    let checksum = calculate_checksum(&header);
    write_octal(&mut header[CHKSUM_OFF..], checksum as u64, 8)?;

    Ok(header)
}

/// Split path into name (max 100) and prefix (max 155)
pub(crate) fn split_path(entry: &ArchiveEntry) -> PaxResult<(String, String)> {
    let path_str = ustar_path_string(entry);
    try_split_path(&path_str).ok_or(PaxError::PathTooLong(path_str))
}

/// The member name as ustar spells it: a directory carries a trailing slash.
pub(crate) fn ustar_path_string(entry: &ArchiveEntry) -> String {
    let path_str = entry.path.to_string_lossy();
    if entry.is_dir() && !path_str.ends_with('/') {
        format!("{}/", path_str)
    } else {
        path_str.into_owned()
    }
}

/// Split a path into the ustar name (max 100) and prefix (max 155) fields.
///
/// `None` when the path cannot be represented exactly. What to do then is the
/// caller's to decide and is the one place the two formats differ: ustar has
/// nowhere else to put the name and fails, while pax writes a `path=` extended
/// header record and leaves these fields as a fallback for readers that ignore
/// it.
pub(crate) fn try_split_path(path_str: &str) -> Option<(String, String)> {
    if path_str.len() <= NAME_LEN {
        return Some((path_str.to_string(), String::new()));
    }

    // Split at the highest '/' that leaves a name of at most NAME_LEN bytes.
    // '/' is ASCII, so an index holding it is always a char boundary.
    for i in (1..=PREFIX_LEN.min(path_str.len().saturating_sub(1))).rev() {
        if path_str.as_bytes()[i] == b'/' && path_str.len() - (i + 1) <= NAME_LEN {
            return Some((path_str[i + 1..].to_string(), path_str[..i].to_string()));
        }
    }

    None
}

/// Convert EntryType to typeflag
fn entry_type_to_flag(entry_type: &EntryType) -> u8 {
    match entry_type {
        EntryType::Regular => REGTYPE,
        EntryType::Directory => DIRTYPE,
        EntryType::Symlink => SYMTYPE,
        EntryType::Hardlink => LNKTYPE,
        EntryType::CharDevice => CHRTYPE,
        EntryType::BlockDevice => BLKTYPE,
        EntryType::Fifo => FIFOTYPE,
        EntryType::Socket => REGTYPE, // Sockets typically not stored; fallback to regular
    }
}

/// Write a string to a field, NUL-terminated if space permits
pub(crate) fn write_string(buf: &mut [u8], s: &str, max_len: usize) {
    let bytes = s.as_bytes();
    let len = std::cmp::min(bytes.len(), max_len);
    buf[..len].copy_from_slice(&bytes[..len]);
}

/// Write an octal number to a fixed-width ustar numeric field.
///
/// A field of `width` bytes holds `width - 1` zero-filled octal digits followed
/// by a single NUL terminator. A value too large to fit is rejected with an
/// error rather than silently truncating its high-order digits, which would
/// corrupt the archive (e.g. a size field for a file ≥8 GiB).
fn write_octal(buf: &mut [u8], val: u64, width: usize) -> PaxResult<()> {
    let digits = width - 1;
    let s = format!("{:0digits$o}", val, digits = digits);
    if s.len() > digits {
        return Err(PaxError::InvalidHeader(format!(
            "value {} too large for {}-byte ustar numeric field",
            val, width
        )));
    }
    buf[..digits].copy_from_slice(s.as_bytes());
    buf[digits] = 0;
    Ok(())
}

// ============================================================================
// Utility functions
// ============================================================================

/// Round up to next block boundary
fn round_up_block(size: u64) -> u64 {
    // A `size=` extended-header record can declare u64::MAX, and rounding that
    // up overflows to 0 -- after which the skip length underflows and the
    // reader walks the rest of the archive as member data.
    size.div_ceil(BLOCK_SIZE as u64)
        .saturating_mul(BLOCK_SIZE as u64)
}

/// Calculate padding needed to reach block boundary
fn padding_needed(bytes_written: u64) -> usize {
    let remainder = (bytes_written % BLOCK_SIZE as u64) as usize;
    if remainder == 0 {
        0
    } else {
        BLOCK_SIZE - remainder
    }
}

/// Skip bytes in a reader
fn skip_bytes<R: Read>(reader: &mut R, count: u64) -> PaxResult<()> {
    let mut remaining = count;
    let mut buf = [0u8; 4096];
    while remaining > 0 {
        let to_read = std::cmp::min(remaining, buf.len() as u64) as usize;
        reader.read_exact(&mut buf[..to_read])?;
        remaining -= to_read as u64;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_octal() {
        assert_eq!(parse_octal(b"000644 \0").unwrap(), 0o644);
        assert_eq!(parse_octal(b"0000755\0").unwrap(), 0o755);
        assert_eq!(parse_octal(b"       \0").unwrap(), 0);
    }

    #[test]
    fn test_write_octal_round_trip_and_overflow() {
        // A small value: (width-1) octal digits, zero-filled, NUL-terminated.
        let mut buf = [0u8; 8];
        write_octal(&mut buf, 0o644, 8).unwrap();
        assert_eq!(&buf, b"0000644\0");
        assert_eq!(parse_octal(&buf).unwrap(), 0o644);

        // The widest size that fits a 12-byte field is 11 octal digits = 8 GiB-1.
        let mut buf = [0u8; 12];
        let max = 0o77_777_777_777_u64; // 11 octal sevens = 8 GiB - 1
        write_octal(&mut buf, max, 12).unwrap();
        assert_eq!(parse_octal(&buf).unwrap(), max);

        // One larger needs 12 digits and must be rejected, not truncated.
        let mut buf = [0u8; 12];
        assert!(write_octal(&mut buf, max + 1, 12).is_err());
    }

    #[test]
    fn test_parse_string() {
        assert_eq!(parse_string(b"hello\0\0\0\0\0"), "hello");
        assert_eq!(parse_string(b"test    "), "test");
        assert_eq!(parse_string(b"\0\0\0\0"), "");
    }

    #[test]
    fn test_split_path_short() {
        let entry = ArchiveEntry::new(PathBuf::from("short.txt"), EntryType::Regular);
        let (name, prefix) = split_path(&entry).unwrap();
        assert_eq!(name, "short.txt");
        assert_eq!(prefix, "");
    }

    #[test]
    fn test_checksum() {
        let mut header = [0u8; BLOCK_SIZE];
        header[NAME_OFF..NAME_OFF + 4].copy_from_slice(b"test");
        let checksum = calculate_checksum(&header);
        assert!(checksum > 0);
    }

    #[test]
    fn test_round_up_block() {
        assert_eq!(round_up_block(0), 0);
        assert_eq!(round_up_block(1), 512);
        assert_eq!(round_up_block(512), 512);
        assert_eq!(round_up_block(513), 1024);
    }

    #[test]
    fn test_padding_needed() {
        assert_eq!(padding_needed(0), 0);
        assert_eq!(padding_needed(100), 412);
        assert_eq!(padding_needed(512), 0);
        assert_eq!(padding_needed(600), 424);
    }
}
