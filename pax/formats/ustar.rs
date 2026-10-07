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
//! This module owns the header layout. The pax interchange format is an
//! extension of ustar and reads the same 512-byte block, so it imports these
//! offsets rather than restating them -- two copies of a field offset is one
//! copy that can be wrong.
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

use crate::archive::{ArchiveEntry, ArchiveReader, ArchiveWriter, EntryType, SourceHeader};
use crate::error::{PaxError, PaxResult};
use crate::formats::ArchiveStream;
use std::io::{Read, Write};

pub(crate) const BLOCK_SIZE: usize = 512;
/// Static zero buffer for padding and end-of-archive markers
pub(crate) static ZERO_BLOCK: [u8; BLOCK_SIZE] = [0u8; BLOCK_SIZE];
pub(crate) const NAME_LEN: usize = 100;
pub(crate) const PREFIX_LEN: usize = 155;
pub(crate) const LINKNAME_LEN: usize = 100;
const MAGIC_LEN: usize = 6;
const VERSION_LEN: usize = 2;
pub(crate) const UNAME_LEN: usize = 32;
pub(crate) const GNAME_LEN: usize = 32;

// Header field offsets
pub(crate) const NAME_OFF: usize = 0;
pub(crate) const MODE_OFF: usize = 100;
pub(crate) const UID_OFF: usize = 108;
pub(crate) const GID_OFF: usize = 116;
pub(crate) const SIZE_OFF: usize = 124;
pub(crate) const MTIME_OFF: usize = 136;
pub(crate) const CHKSUM_OFF: usize = 148;
pub(crate) const TYPEFLAG_OFF: usize = 156;
pub(crate) const LINKNAME_OFF: usize = 157;
pub(crate) const MAGIC_OFF: usize = 257;
pub(crate) const VERSION_OFF: usize = 263;
pub(crate) const UNAME_OFF: usize = 265;
pub(crate) const GNAME_OFF: usize = 297;
pub(crate) const PREFIX_OFF: usize = 345;

// Type flags
pub(crate) const REGTYPE: u8 = b'0';
const AREGTYPE: u8 = b'\0';
pub(crate) const LNKTYPE: u8 = b'1';
pub(crate) const SYMTYPE: u8 = b'2';
pub(crate) const CHRTYPE: u8 = b'3';
pub(crate) const BLKTYPE: u8 = b'4';
pub(crate) const DIRTYPE: u8 = b'5';
pub(crate) const FIFOTYPE: u8 = b'6';
const CONTTYPE: u8 = b'7';

// Device number field offsets and lengths
pub(crate) const DEVMAJOR_OFF: usize = 329;
pub(crate) const DEVMINOR_OFF: usize = 337;

/// ustar archive reader
pub struct UstarReader<R: Read> {
    reader: ArchiveStream<R>,
    current_size: u64,
    bytes_read: u64,
}

impl<R: Read> UstarReader<R> {
    /// A ustar reader over an archive stream
    pub fn from_stream(reader: ArchiveStream<R>) -> Self {
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
            let Some(header) = next_header_block(&mut self.reader, LoneZeroBlock::Stop)? else {
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
                let Some(size) = consume_long_name_group(
                    &mut self.reader,
                    header,
                    SizeRule::Ustar,
                    LoneZeroBlock::Stop,
                )?
                else {
                    return Ok(None);
                };
                self.current_size = size;
                self.bytes_read = 0;
                self.skip_data()?;
                continue;
            }

            let entry = parse_header(&header, SizeRule::Ustar, |_| {})?;
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
            self.reader.skip(to_skip)?;
        }

        // Reset state - we've finished with this entry's data
        self.bytes_read = total_bytes;
        Ok(())
    }

    fn finish(&mut self, reached_end: bool) -> PaxResult<()> {
        self.reader.finish(reached_end)
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
        self.skip_data = !stores_data(entry.entry_type);
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
    ///
    /// `declared` is the size the archive records for the member -- the ustar
    /// field, or a pax `size=` record that overrides it. Either way the type
    /// decides whether any data follows.
    pub(crate) fn data_size(self, entry_type: EntryType, declared: u64) -> u64 {
        match entry_type {
            // A directory's size field is a directory size limit, not a
            // length: POSIX says a system that does not implement such
            // limiting "should ignore the size field". Nothing malformed
            // about a non-zero one, so no diagnostic.
            EntryType::Directory => 0,
            EntryType::Symlink => 0,
            EntryType::Hardlink if self == SizeRule::Ustar => 0,
            // Types 3, 4 and 6: "no data logical records shall be stored on
            // the medium. Additionally, for type 6, the size field shall be
            // ignored when reading." A device's size field has no meaning
            // either, and libarchive ignores it for all three, so none of them
            // is diagnosed.
            EntryType::CharDevice | EntryType::BlockDevice | EntryType::Fifo => 0,
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

/// Parse a header block into an ArchiveEntry.
///
/// `overrides` applies what the archive says about the member outside its
/// header block -- a pax archive's extended-header records -- before the
/// member's type and data length are settled, since both depend on its final
/// name and size. A plain ustar reader passes a closure that does nothing.
pub(crate) fn parse_header(
    header: &[u8; BLOCK_SIZE],
    rule: SizeRule,
    overrides: impl FnOnce(&mut ArchiveEntry),
) -> PaxResult<ArchiveEntry> {
    let name = path_field(&header[NAME_OFF..NAME_OFF + NAME_LEN]);
    // An old GNU header has no prefix field: GNU tar keeps the access and
    // change times in those bytes, and joining them onto the name turned
    // `dir/file` into `14524770401/dir/file`.
    let prefix = if is_old_gnu(header) {
        b"".as_slice()
    } else {
        path_field(&header[PREFIX_OFF..PREFIX_OFF + PREFIX_LEN])
    };

    let path = crate::rawpath::join(prefix, name);

    let mode = parse_u32_field(&header[MODE_OFF..MODE_OFF + 8], "mode")?;
    let uid = parse_u32_field(&header[UID_OFF..UID_OFF + 8], "uid")?;
    let gid = parse_u32_field(&header[GID_OFF..GID_OFF + 8], "gid")?;
    let declared_size = parse_numeric(&header[SIZE_OFF..SIZE_OFF + 12])?;
    let mtime = parse_signed_numeric(&header[MTIME_OFF..MTIME_OFF + 12])?;

    let typeflag = header[TYPEFLAG_OFF];
    let flag = parse_typeflag(typeflag);
    let entry_type = flag.entry_type();

    let linkname = path_field(&header[LINKNAME_OFF..LINKNAME_OFF + LINKNAME_LEN]);
    let link_target = if linkname.is_empty() {
        None
    } else {
        Some(crate::rawpath::from_bytes(linkname))
    };

    // POSIX: "If conversion to a regular file occurs, the pax utility shall
    // produce an error indicating that the conversion took place."
    match flag {
        TypeFlag::Known(_) | TypeFlag::RegularByDefinition => {}
        TypeFlag::Unimplemented(what) => crate::error::report_error(
            &path,
            format!("is {what}, which is not supported; extracting as a regular file"),
        ),
        TypeFlag::Unknown => crate::error::report_error(
            &path,
            format!(
                "has unrecognized type {}; extracting as a regular file",
                show_typeflag(typeflag)
            ),
        ),
    }

    if declared_size != 0 && rule.size_must_be_zero(entry_type) {
        crate::error::report_error(
            &path,
            format!(
                "header of type {} records {} bytes of data, which POSIX \
                 requires to be zero; ignoring the size field",
                typeflag as char, declared_size
            ),
        );
    }

    let uname = name_field(&header[UNAME_OFF..UNAME_OFF + UNAME_LEN]);
    let gname = name_field(&header[GNAME_OFF..GNAME_OFF + GNAME_LEN]);

    // Parse device major/minor for block/char devices
    let devmajor = parse_u32_field(&header[DEVMAJOR_OFF..DEVMAJOR_OFF + 8], "devmajor")?;
    let devminor = parse_u32_field(&header[DEVMINOR_OFF..DEVMINOR_OFF + 8], "devminor")?;

    let mut entry = ArchiveEntry {
        path,
        mode,
        uid,
        gid,
        size: declared_size,
        mtime,
        entry_type,
        link_target,
        uname: if uname.is_empty() {
            None
        } else {
            Some(uname.to_vec())
        },
        gname: if gname.is_empty() {
            None
        } else {
            Some(gname.to_vec())
        },
        devmajor,
        devminor,
        // ustar has no link-count field, and `Default` would leave this 0 --
        // a member with no names at all, which `pax -v` then printed. A
        // member that exists has at least one name; cpio's `header_nlink`
        // floors the same field for the same reason.
        nlink: 1,
        // Kept only so `-o listopt=%(magic)s` and the other three can report
        // what this header actually held. The checksum is read leniently
        // because `multivolume::read_entry` parses a header without verifying
        // it first, and a junk field there must not newly fail the read.
        source_header: Some(SourceHeader::Ustar {
            magic: header[MAGIC_OFF..MAGIC_OFF + MAGIC_LEN]
                .try_into()
                .expect("slice of MAGIC_LEN"),
            version: header[VERSION_OFF..VERSION_OFF + VERSION_LEN]
                .try_into()
                .expect("slice of VERSION_LEN"),
            chksum: parse_octal(&header[CHKSUM_OFF..CHKSUM_OFF + 8]).unwrap_or(0),
            typeflag,
        }),
        ..Default::default()
    };
    overrides(&mut entry);
    settle_type_and_size(&mut entry, typeflag, rule);
    Ok(entry)
}

/// Decide a member's type and data length from its final name and size.
///
/// The old-style directory rule runs here, after any `path` and `size`
/// records, not on the header block's own fields: the 100-byte name field of
/// a long name can end in a slash where the name does not, and a `size`
/// record can give data to a header whose size field is empty. Either way the
/// member is a file, and deciding on the raw fields made it a directory --
/// whose data, unread, then parsed as further members.
fn settle_type_and_size(entry: &mut ArchiveEntry, typeflag: u8, rule: SizeRule) {
    let name = crate::rawpath::as_bytes(&entry.path);
    if is_old_style_directory(typeflag, name, entry.size) {
        entry.entry_type = EntryType::Directory;
    }
    // A `size` record replaces the size field, not the rule for which types
    // carry data: a directory or FIFO has none whichever of the two records
    // its size.
    entry.size = rule.data_size(entry.entry_type, entry.size);
}

/// Whether this is an old GNU header: magic "ustar " and version " \0", as
/// GNU tar wrote before POSIX ustar, with no prefix field.
fn is_old_gnu(header: &[u8; BLOCK_SIZE]) -> bool {
    &header[MAGIC_OFF..VERSION_OFF + VERSION_LEN] == b"ustar  \0"
}

/// Whether a header is a directory by the convention that predates typeflag
/// 5: a regular-file header whose name ends in a slash. GNU and BSD tar both
/// still read it so. Only an empty one: a header that records data is a
/// regular file whatever it is called, and its data has to be stepped over
/// as such.
fn is_old_style_directory(typeflag: u8, name: &[u8], declared_size: u64) -> bool {
    matches!(typeflag, REGTYPE | AREGTYPE) && name.ends_with(b"/") && declared_size == 0
}

/// A numeric field that holds a 32-bit value: a mode, an id, a device number.
///
/// The base-256 form can hold far more. Truncating it is not a value the
/// archive gave: a uid of 2^32 became 0 -- root, on a setuid file.
fn parse_u32_field(bytes: &[u8], what: &str) -> PaxResult<u32> {
    let value = parse_numeric(bytes)?;
    u32::try_from(value)
        .map_err(|_| PaxError::InvalidHeader(format!("{what} field out of range: {value}")))
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

/// The value of a path field (name, prefix, linkname), as bytes.
///
/// These are NUL-terminated and a trailing <space> is a legitimate pathname
/// character, so only the NUL terminator delimits the value -- unlike the
/// space-padded fields, no whitespace is trimmed. Nor is anything decoded: a
/// pathname is a byte string, and deciding it is text is how a member named
/// `na\377me.txt` came back as something else. See `crate::rawpath`.
pub(crate) fn path_field(bytes: &[u8]) -> &[u8] {
    let end = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
    &bytes[..end]
}

/// The value of a user or group name field (uname, gname), as bytes.
///
/// POSIX makes these NUL-terminated character strings, and historical writers
/// space-pad them, so both delimit the value -- which is what `parse_string`
/// has always done for these two fields. Bytes rather than a `String` because
/// under `hdrcharset=BINARY` a name is the underlying system's bytes and need
/// not decode.
pub(crate) fn name_field(bytes: &[u8]) -> &[u8] {
    let value = path_field(bytes);
    let end = value
        .iter()
        .rposition(|b| !b.is_ascii_whitespace())
        .map_or(0, |i| i + 1);
    &value[..end]
}

/// Parse an octal number from bytes
pub(crate) fn parse_octal(bytes: &[u8]) -> PaxResult<u64> {
    let s = parse_string(bytes);
    // Historical writers pad on the left with spaces ("%6o "), as format
    // detection has always accepted.
    let s = s.trim_start_matches(' ');
    if s.is_empty() {
        return Ok(0);
    }
    // Reject if the octal string contains a sign
    if s.starts_with('+') || s.starts_with('-') {
        return Err(PaxError::InvalidHeader(format!("invalid octal: {}", s)));
    }
    u64::from_str_radix(s, 8).map_err(|_| PaxError::InvalidHeader(format!("invalid octal: {}", s)))
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

/// Read one 512-byte block, or `None` at end of file. A partial block is an
/// archive truncated inside a header, and an error.
fn read_block(reader: &mut impl Read) -> PaxResult<Option<[u8; BLOCK_SIZE]>> {
    let mut block = [0u8; BLOCK_SIZE];
    Ok(crate::formats::read_header(reader, &mut block)?.then_some(block))
}

/// The block after a zero block, or `None` when the archive ends inside it.
///
/// An archive cut off inside the second block of its end-of-archive
/// indicator is complete: the first zero block already ended it, and bsdtar
/// reads it as such. Only a cut inside something that is not zeros is a
/// truncated header.
fn read_block_after_zero(reader: &mut impl Read) -> PaxResult<Option<[u8; BLOCK_SIZE]>> {
    let mut block = [0u8; BLOCK_SIZE];
    match crate::formats::read_up_to(reader, &mut block)? {
        BLOCK_SIZE => Ok(Some(block)),
        _ if is_zero_block(&block) => Ok(None),
        _ => Err(crate::formats::truncated_header()),
    }
}

/// What [`next_header_block`] makes of a single zero block followed by a
/// header.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LoneZeroBlock {
    /// End the archive there, and say so: reading and listing.
    Stop,
    /// Read on from the header after it, silently. Append mode writes where
    /// the archive ends, and stopping at a lone zero block put that in front
    /// of members it then destroyed.
    StepOver,
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
/// only counter available here sees header blocks and not the data blocks
/// between them, so any number it produced would send someone to the wrong
/// place in the file. Counting correctly means threading a byte position
/// through every read and skip in both readers, which is more machinery than
/// a diagnostic detail is worth.
pub(crate) fn next_header_block(
    reader: &mut impl Read,
    lone_zero: LoneZeroBlock,
) -> PaxResult<Option<[u8; BLOCK_SIZE]>> {
    let Some(block) = read_block(reader)? else {
        return Ok(None);
    };
    if !is_zero_block(&block) {
        return Ok(Some(block));
    }

    match read_block_after_zero(reader)? {
        // Two zero blocks, or one followed by end of file: a proper end.
        None => Ok(None),
        Some(next) if is_zero_block(&next) => Ok(None),
        Some(next) if lone_zero == LoneZeroBlock::StepOver => Ok(Some(next)),
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

/// Parse an unsigned numeric header field: octal digits, or the base-256 form
/// GNU tar writes for a value too large for them.
pub(crate) fn parse_numeric(bytes: &[u8]) -> PaxResult<u64> {
    match parse_base256(bytes)? {
        None => parse_octal(bytes),
        Some(v) => u64::try_from(v)
            .map_err(|_| PaxError::InvalidHeader(format!("negative numeric field: {v}"))),
    }
}

/// Parse a signed numeric header field (mtime): octal digits, or base-256,
/// where a negative value is a time before the epoch.
pub(crate) fn parse_signed_numeric(bytes: &[u8]) -> PaxResult<i64> {
    match parse_base256(bytes)? {
        Some(v) => Ok(v),
        None => {
            let v = parse_octal(bytes)?;
            i64::try_from(v)
                .map_err(|_| PaxError::InvalidHeader(format!("numeric field out of range: {v}")))
        }
    }
}

/// A GNU base-256 field, `None` when the field is not one.
///
/// The high bit of the first byte marks the form, and the field is then a
/// big-endian two's complement number in the remaining bits: GNU tar writes a
/// non-negative N as 256^len/2 + N (first byte 0x80) and a negative one as
/// 256^len - N (first byte 0xff), so bit 6 of the first byte is the sign.
fn parse_base256(bytes: &[u8]) -> PaxResult<Option<i64>> {
    let Some((&first, rest)) = bytes.split_first() else {
        return Ok(None);
    };
    if first & 0x80 == 0 {
        return Ok(None);
    }
    let out_of_range =
        || PaxError::InvalidHeader("base-256 numeric field out of range".to_string());
    let mut value = i64::from(first & 0x3f) - i64::from(first & 0x40);
    for &b in rest {
        value = value
            .checked_mul(256)
            .and_then(|v| v.checked_add(i64::from(b)))
            .ok_or_else(out_of_range)?;
    }
    Ok(Some(value))
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
/// truncated name it was trying to avoid. [`LongNameGroup`] gathers the whole
/// run.
fn consume_long_name_record(
    reader: &mut impl Read,
    header: &[u8; BLOCK_SIZE],
    what: &str,
) -> PaxResult<Vec<u8>> {
    let size = parse_numeric(&header[SIZE_OFF..SIZE_OFF + 12])?;
    let data = crate::formats::read_declared(reader, size, crate::formats::MAX_NAME, what)?;
    let padding = (BLOCK_SIZE - (size as usize % BLOCK_SIZE)) % BLOCK_SIZE;
    if padding > 0 {
        let mut pad = [0u8; BLOCK_SIZE];
        reader.read_exact(&mut pad[..padding])?;
    }

    let end = data.iter().position(|&b| b == 0).unwrap_or(data.len());
    Ok(data[..end].to_vec())
}

/// The GNU long-name records read so far ahead of a member, which describe
/// that member rather than files of their own.
///
/// Implementing the extension is separate work. What this avoids is the
/// alternative: extracting `././@LongLink` as a file of its own and the member
/// under a name truncated to 100 bytes -- two wrong files, and the name that
/// got path-checked is not the name the archive meant. So the member they
/// describe is skipped, unless something else names it in full (see
/// [`superseded`](Self::superseded)).
#[derive(Default)]
pub(crate) struct LongNameGroup {
    /// The `L` record's value: the member's name.
    name: Option<Vec<u8>>,
    /// Whether there was a `K` record, the member's link target.
    link: bool,
    /// What each record was, for the diagnostic.
    kinds: Vec<&'static str>,
    /// The last record's own name field, which is all there is to name the
    /// group by when no `L` record or member follows.
    record_name: Vec<u8>,
}

impl LongNameGroup {
    pub(crate) fn is_empty(&self) -> bool {
        self.kinds.is_empty()
    }

    /// Read the record whose header is `header`, of the kind `what`.
    pub(crate) fn consume(
        &mut self,
        reader: &mut impl Read,
        header: &[u8; BLOCK_SIZE],
        what: &'static str,
    ) -> PaxResult<()> {
        let value = consume_long_name_record(reader, header, what)?;
        // The `L` record holds the name; `K` holds the link target, which is
        // not what the member is called.
        if header[TYPEFLAG_OFF] == b'L' {
            self.name = Some(value);
        } else {
            self.link = true;
        }
        self.kinds.push(what);
        self.record_name = path_field(&header[NAME_OFF..NAME_OFF + NAME_LEN]).to_vec();
        Ok(())
    }

    /// Whether the member's pax records name everything these records do --
    /// a `path` for an `L`, a `linkpath` for a `K` -- so that they are moot:
    /// a pax record overrides the header it describes, however that header's
    /// own fields were spelt.
    pub(crate) fn superseded(&self, path: bool, linkpath: bool) -> bool {
        (self.name.is_none() || path) && (!self.link || linkpath)
    }

    /// Name the member that is being skipped, and the extensions that
    /// describe it. `member` is its header, or `None` when the archive ended
    /// before one.
    pub(crate) fn report(&self, member: Option<&[u8; BLOCK_SIZE]>) {
        // The long name if the archive gave one, otherwise the truncated name
        // in the member's own header -- which is all there is to go on for a
        // lone `K`.
        let name = match (&self.name, member) {
            (Some(n), _) => n.as_slice(),
            (None, Some(member)) => path_field(&member[NAME_OFF..NAME_OFF + NAME_LEN]),
            (None, None) => &self.record_name,
        };
        crate::error::report_error(
            &crate::rawpath::from_bytes(name),
            format!(
                "uses {}, which is not supported; skipping the member",
                self.kinds.join(" and ")
            ),
        );
    }
}

/// Consume every long-name record preceding a member, and the member's header,
/// and report the whole group as unsupported.
///
/// `header` is the first record's header. Returns the length of the member's
/// data, which the caller steps over -- by seeking, where it can -- so that its
/// next read is the following member; or `None` when the archive ends after
/// the records, with no member for them to describe. That is the end of the
/// archive, and the caller must not read on past the indicator just consumed.
fn consume_long_name_group(
    reader: &mut impl Read,
    mut header: [u8; BLOCK_SIZE],
    rule: SizeRule,
    lone_zero: LoneZeroBlock,
) -> PaxResult<Option<u64>> {
    let mut group = LongNameGroup::default();

    while let Some(what) = long_name_record(header[TYPEFLAG_OFF]) {
        group.consume(reader, &header, what)?;

        let Some(next) = next_header_block(reader, lone_zero)? else {
            // The archive ends after the record, with no member to skip.
            group.report(None);
            return Ok(None);
        };
        if !verify_checksum(&next) {
            return Err(PaxError::InvalidHeader("checksum mismatch".to_string()));
        }
        header = next;
    }

    // `header` is now the member the records described.
    group.report(Some(&header));
    member_data_size(&header, rule, None).map(Some)
}

/// The length of the data that follows a member header, by the same rule
/// `parse_header` applies, without interpreting the rest of the header.
/// `size_record` is a pax `size` record for the member, which replaces the
/// size field.
pub(crate) fn member_data_size(
    header: &[u8; BLOCK_SIZE],
    rule: SizeRule,
    size_record: Option<u64>,
) -> PaxResult<u64> {
    let declared = match size_record {
        Some(size) => size,
        None => parse_numeric(&header[SIZE_OFF..SIZE_OFF + 12])?,
    };
    let entry_type = parse_typeflag(header[TYPEFLAG_OFF]).entry_type();
    Ok(rule.data_size(entry_type, declared))
}

/// Verify header checksum.
///
/// POSIX sums the bytes as unsigned, but historical implementations summed
/// them as signed chars, and the two differ for a header with any byte above
/// 127. Either is accepted, as GNU and BSD tar accept them.
pub(crate) fn verify_checksum(header: &[u8; BLOCK_SIZE]) -> bool {
    let Ok(stored) = parse_octal(&header[CHKSUM_OFF..CHKSUM_OFF + 8]) else {
        return false;
    };
    stored == u64::from(calculate_checksum(header))
        || i64::try_from(stored).is_ok_and(|s| s == signed_checksum(header))
}

/// The checksum as a historical implementation computed it: the header bytes
/// summed as signed chars, the checksum field as spaces.
fn signed_checksum(header: &[u8; BLOCK_SIZE]) -> i64 {
    header
        .iter()
        .enumerate()
        .map(|(i, &byte)| {
            if (CHKSUM_OFF..CHKSUM_OFF + 8).contains(&i) {
                i64::from(b' ')
            } else {
                i64::from(byte as i8)
            }
        })
        .sum()
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

/// Whether a member of this type is followed by data blocks. Per POSIX,
/// symlinks and hardlinks have none in ustar format, whatever size the entry
/// carries -- write mode sets a symlink's to its target length for cpio, where
/// the target *is* the data.
pub(crate) fn stores_data(entry_type: EntryType) -> bool {
    !matches!(entry_type, EntryType::Symlink | EntryType::Hardlink)
}

/// Build a header block from an ArchiveEntry
pub(crate) fn build_header(entry: &ArchiveEntry) -> PaxResult<[u8; BLOCK_SIZE]> {
    let mut header = [0u8; BLOCK_SIZE];

    // Split path into name and prefix if needed
    let (name, prefix) = split_path(entry)?;

    // Write fields
    write_field(&mut header[NAME_OFF..], &name, NAME_LEN);
    write_octal(&mut header[MODE_OFF..], entry.mode as u64, 8)?;
    write_octal(&mut header[UID_OFF..], entry.uid as u64, 8)?;
    write_octal(&mut header[GID_OFF..], entry.gid as u64, 8)?;
    let header_size = if stores_data(entry.entry_type) {
        entry.size
    } else {
        0
    };
    write_octal(&mut header[SIZE_OFF..], header_size, 12)?;
    write_octal(&mut header[MTIME_OFF..], entry.unsigned_mtime()?, 12)?;

    // Typeflag
    header[TYPEFLAG_OFF] = entry_type_to_flag(entry.entry_type)?;

    // Linkname
    if let Some(ref target) = entry.link_target {
        let link_bytes = crate::rawpath::as_bytes(target);
        if link_bytes.len() > LINKNAME_LEN {
            return Err(PaxError::PathTooLong(
                String::from_utf8_lossy(link_bytes).into_owned(),
            ));
        }
        write_field(&mut header[LINKNAME_OFF..], link_bytes, LINKNAME_LEN);
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
    write_octal(&mut header[DEVMAJOR_OFF..], entry.devmajor as u64, 8)?;
    write_octal(&mut header[DEVMINOR_OFF..], entry.devminor as u64, 8)?;

    // Prefix
    write_field(&mut header[PREFIX_OFF..], &prefix, PREFIX_LEN);

    // Calculate and write checksum
    let checksum = calculate_checksum(&header);
    write_octal(&mut header[CHKSUM_OFF..], checksum as u64, 8)?;

    Ok(header)
}

/// Split path into name (max 100) and prefix (max 155)
pub(crate) fn split_path(entry: &ArchiveEntry) -> PaxResult<(Vec<u8>, Vec<u8>)> {
    let path = ustar_path_bytes(entry);
    try_split_path(&path).ok_or_else(|| {
        // A fatal diagnostic about a name that is already too long is not a
        // round trip, so rendering it for the message costs nothing.
        PaxError::PathTooLong(String::from_utf8_lossy(&path).into_owned())
    })
}

/// The member name as ustar spells it: a directory carries a trailing slash.
pub(crate) fn ustar_path_bytes(entry: &ArchiveEntry) -> Vec<u8> {
    let mut bytes = crate::rawpath::as_bytes(&entry.path).to_vec();
    if entry.is_dir() && bytes.last() != Some(&b'/') {
        bytes.push(b'/');
    }
    bytes
}

/// Split a path into the ustar name (max 100) and prefix (max 155) fields.
///
/// `None` when the path cannot be represented exactly. What to do then is the
/// caller's to decide and is the one place the two formats differ: ustar has
/// nowhere else to put the name and fails, while pax writes a `path=` extended
/// header record and leaves these fields as a fallback for readers that ignore
/// it.
pub(crate) fn try_split_path(path: &[u8]) -> Option<(Vec<u8>, Vec<u8>)> {
    split_name_prefix(path).map(|(name, prefix)| (name.to_vec(), prefix.to_vec()))
}

/// The same split, as borrowed halves, for a caller that only needs to look at
/// them -- `-o listopt=%(name)s` and `%(prefix)s`.
pub(crate) fn split_name_prefix(path: &[u8]) -> Option<(&[u8], &[u8])> {
    if path.len() <= NAME_LEN {
        return Some((path, b""));
    }

    // Split at the highest '/' that leaves a name of at most NAME_LEN bytes,
    // and of at least one byte before any trailing '/': a directory's own
    // trailing slash is no place to split, and left the name field empty --
    // which an old reader takes for the end of the archive.
    for i in (1..=PREFIX_LEN.min(path.len().saturating_sub(2))).rev() {
        if path[i] == b'/' && path.len() - (i + 1) <= NAME_LEN {
            return Some((&path[i + 1..], &path[..i]));
        }
    }

    None
}

/// The typeflag of a member of this type, for every writer of a tar header.
///
/// A socket has none. Writing it as a regular file, as this used to, put an
/// empty file in the archive where there had been a socket; write mode
/// diagnoses one before it gets here (`ArchiveWriter::supports_sockets`), and
/// this refuses one that does.
pub(crate) fn entry_type_to_flag(entry_type: EntryType) -> PaxResult<u8> {
    Ok(match entry_type {
        EntryType::Regular => REGTYPE,
        EntryType::Directory => DIRTYPE,
        EntryType::Symlink => SYMTYPE,
        EntryType::Hardlink => LNKTYPE,
        EntryType::CharDevice => CHRTYPE,
        EntryType::BlockDevice => BLKTYPE,
        EntryType::Fifo => FIFOTYPE,
        EntryType::Socket => {
            return Err(PaxError::InvalidFormat(
                "a socket cannot be stored in a tar archive".to_string(),
            ))
        }
    })
}

/// Write a string to a field, NUL-terminated if space permits
pub(crate) fn write_field(buf: &mut [u8], bytes: &[u8], max_len: usize) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    /// uname and gname are bytes now, so they stop going through
    /// `parse_string`. `name_field` has to keep its delimiting: POSIX
    /// NUL-terminates these fields and historical writers space-pad them, so
    /// both end the value -- and a byte that does not decode has to survive,
    /// which is the whole point of the change.
    #[test]
    fn test_name_field_delimiters() {
        assert_eq!(name_field(b"root\0\0\0\0"), b"root");
        assert_eq!(name_field(b"root    "), b"root");
        assert_eq!(name_field(b"root \0  "), b"root");
        assert_eq!(name_field(b"        "), b"");
        assert_eq!(name_field(b"\0root"), b"");
        // Not UTF-8, and preserved rather than replaced.
        assert_eq!(name_field(b"gr\xffup\0"), b"gr\xffup");
    }

    #[test]
    fn test_parse_base256() {
        // GNU tar's encoding: 0x80 then the value, big-endian.
        let mut field = [0u8; 8];
        field[0] = 0x80;
        field[5..].copy_from_slice(&[0x2d, 0xc6, 0xc0]);
        assert_eq!(parse_numeric(&field).unwrap(), 3_000_000);

        let mut size = [0u8; 12];
        size[0] = 0x80;
        size[4..].copy_from_slice(&(1u64 << 36).to_be_bytes());
        assert_eq!(parse_numeric(&size).unwrap(), 1 << 36);

        // A negative mtime is two's complement, first byte 0xff.
        let mut mtime = [0xffu8; 12];
        mtime[11] = 0xfe;
        assert_eq!(parse_signed_numeric(&mtime).unwrap(), -2);
        assert!(parse_numeric(&mtime).is_err());

        // Octal is unchanged.
        assert_eq!(parse_numeric(b"0000755\0").unwrap(), 0o755);
        assert_eq!(parse_signed_numeric(b"00000000017\0").unwrap(), 0o17);
    }

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
        assert_eq!(name, b"short.txt");
        assert_eq!(prefix, b"");
    }

    #[test]
    fn test_checksum() {
        let mut header = [0u8; BLOCK_SIZE];
        header[NAME_OFF..NAME_OFF + 4].copy_from_slice(b"test");
        let checksum = calculate_checksum(&header);
        assert!(checksum > 0);
    }

    /// POSIX: no data records follow types 3, 4 and 6, whatever their size
    /// field says, under either rule.
    #[test]
    fn test_special_files_carry_no_data() {
        for rule in [SizeRule::Ustar, SizeRule::Pax] {
            for t in [
                EntryType::CharDevice,
                EntryType::BlockDevice,
                EntryType::Fifo,
            ] {
                assert_eq!(rule.data_size(t, 1024), 0);
                assert!(!rule.size_must_be_zero(t));
            }
            assert_eq!(rule.data_size(EntryType::Regular, 1024), 1024);
        }
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
