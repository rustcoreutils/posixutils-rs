//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! System V (`ar`) archive helpers shared by `ar`, `strip`, and `nm`.
//!
//! POSIX (RATIONALE 84538-84542) does not mandate a particular `ar` archive
//! format, but in practice the System V layout — magic `"!<arch>\n"`, 60-byte
//! ASCII member headers, optional `"/"` symbol-table member, optional `"//"`
//! long-name string-table member — is the lingua franca on Linux/Unix.
//!
//! This module centralises the byte-layout of the optional `"/"` member so
//! that `ar` (which generates archives from scratch) and `strip` (which
//! rewrites archives after modifying members) emit identical output.
//!
//! Reading symbol names from object-file member payloads requires an
//! object-file parser; callers do that with whatever parser they already
//! use (the `object` crate is the typical choice in this repo) and pass
//! the resulting [`MemberInfo`] list here for serialization.

use std::io::{self, Write};

/// The fixed size of a System V archive member header in bytes.
pub const MEMBER_HEADER_SIZE: u64 = 60;

/// System V archive magic: `"!<arch>\n"`.
pub const MAGIC: &[u8; 8] = b"!<arch>\n";

/// The 2-byte terminator that follows every member header: `` "`\n" ``.
pub const TERMINATOR: &[u8; 2] = b"`\n";

/// Per-member info needed to emit a `"/"` symbol-table member.
#[derive(Debug, Clone)]
pub struct MemberInfo {
    /// Payload size in bytes (the file size as written into the archive,
    /// *before* any odd-byte newline padding).
    pub size: u64,
    /// Symbol names exported by this member (text/data/TLS symbols only).
    /// Order is preserved in the emitted symbol-table member.
    pub symbols: Vec<String>,
}

impl MemberInfo {
    /// Number of bytes the member's symbol names contribute to the
    /// symbol-table string region (each name plus a NUL terminator).
    pub fn symbol_bytes(&self) -> u64 {
        self.symbols.iter().map(|s| s.len() as u64 + 1).sum()
    }
}

/// Pad an ASCII metadata field with trailing spaces to fixed width `N`.
/// Returns an error if `s` is longer than `N`.
pub fn pad_metadata_field<const N: usize>(s: &str) -> io::Result<[u8; N]> {
    if s.len() > N {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!(
                "archive metadata field too long: {} bytes, max {}",
                s.len(),
                N
            ),
        ));
    }
    let mut buf = [b' '; N];
    buf[..s.len()].copy_from_slice(s.as_bytes());
    Ok(buf)
}

/// Longest member name that fits inline in the 16-byte header name field.
/// The System V layout stores short names as `"<name>/"`, so one byte of the
/// field is spent on the terminator.
pub const MAX_INLINE_NAME: usize = 15;

/// Builder for the System V `"//"` long-name string-table member.
///
/// Member names longer than [`MAX_INLINE_NAME`] cannot fit the 16-byte header
/// field. System V stores them in a dedicated `"//"` member as `"<name>/\n"`
/// records, and the owning member's name field then carries `"/<offset>"`.
/// Both `ar` (building archives) and `strip` (rewriting them) must agree on
/// this layout, which is why it lives here rather than in either utility.
///
/// Push every member name in archive order, then use [`NameTable::offset`]
/// when writing each member header:
///
/// ```
/// use plib::archive::NameTable;
/// let mut names = NameTable::new();
/// names.push(b"short.o");
/// names.push(b"a_very_long_member_name.o");
/// assert_eq!(names.offset(0), None);        // stored inline
/// assert_eq!(names.offset(1), Some(0));     // stored in the "//" member
/// assert!(!names.is_empty());
/// ```
#[derive(Debug, Default, Clone)]
pub struct NameTable {
    table: Vec<u8>,
    offsets: Vec<Option<usize>>,
}

impl NameTable {
    /// Create an empty table.
    pub fn new() -> Self {
        Self::default()
    }

    /// Record the next member's name, in archive order.
    pub fn push(&mut self, name: &[u8]) {
        if name.len() > MAX_INLINE_NAME {
            self.offsets.push(Some(self.table.len()));
            self.table.extend_from_slice(name);
            self.table.extend_from_slice(b"/\n");
        } else {
            self.offsets.push(None);
        }
    }

    /// Offset into the `"//"` member for the `i`-th pushed name, or `None`
    /// when the name is short enough to be stored inline.
    pub fn offset(&self, i: usize) -> Option<usize> {
        self.offsets.get(i).copied().flatten()
    }

    /// True when no pushed name required the string table.
    pub fn is_empty(&self) -> bool {
        self.table.is_empty()
    }

    /// Bytes the `"//"` member occupies in the archive, header and alignment
    /// pad included; 0 when the table is empty and no member is emitted.
    ///
    /// Pass this as `prefix_bytes` to [`write_sysv_symtab`] so symbol-table
    /// offsets account for the `"//"` member sitting between the `"/"` member
    /// and the first file member.
    pub fn member_bytes(&self) -> u64 {
        if self.table.is_empty() {
            0
        } else {
            MEMBER_HEADER_SIZE + self.table.len() as u64 + (self.table.len() % 2) as u64
        }
    }

    /// Write the `"//"` member. Writes nothing when the table is empty.
    pub fn write<W: Write>(&self, w: &mut W) -> io::Result<()> {
        if self.table.is_empty() {
            return Ok(());
        }
        w.write_all(&pad_metadata_field::<16>("//")?)?;
        w.write_all(&pad_metadata_field::<12>("")?)?; // date
        w.write_all(&pad_metadata_field::<6>("")?)?; // uid
        w.write_all(&pad_metadata_field::<6>("")?)?; // gid
        w.write_all(&pad_metadata_field::<8>("")?)?; // mode
        w.write_all(&pad_metadata_field::<10>(&self.table.len().to_string())?)?;
        w.write_all(TERMINATOR)?;
        w.write_all(&self.table)?;
        if self.table.len() % 2 != 0 {
            w.write_all(b"\n")?;
        }
        Ok(())
    }
}

/// Build the 16-byte header name field for a member.
///
/// `long_name_offset` comes from [`NameTable::offset`]: `None` stores the name
/// inline as `"<name>/"`, `Some(off)` stores the reference `"/<off>"`.
pub fn format_name_field(name: &[u8], long_name_offset: Option<usize>) -> io::Result<[u8; 16]> {
    let mut field = [b' '; 16];
    match long_name_offset {
        Some(offset) => {
            let encoded = format!("/{}", offset);
            if encoded.len() > field.len() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("archive name-table offset too large: {}", offset),
                ));
            }
            field[..encoded.len()].copy_from_slice(encoded.as_bytes());
        }
        None => {
            if name.len() > MAX_INLINE_NAME {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!(
                        "archive member name too long for an inline header field: {} bytes, max {}",
                        name.len(),
                        MAX_INLINE_NAME
                    ),
                ));
            }
            field[..name.len()].copy_from_slice(name);
            field[name.len()] = b'/';
        }
    }
    Ok(field)
}

/// Write a System V `"/"` archive symbol-table member to `w`.
///
/// The caller is responsible for emitting the archive magic before this
/// call and the file-member contents after it. The byte layout is:
///
/// 1. Member header (60 bytes ASCII): name `"/"` padded to 16, mtime 0,
///    uid 0, gid 0, mode 0, size = N. Trailer `"`\n"`.
/// 2. 4 bytes big-endian: total number of symbols.
/// 3. For each symbol: 4 bytes big-endian member offset (absolute, from the
///    archive magic).
/// 4. For each symbol: NUL-terminated name.
/// 5. Optional NUL pad to keep total payload 2-byte aligned.
///
/// Member offsets assume the symbol-table member comes immediately after the
/// archive magic, followed by `members` in order. `prefix_bytes` accounts for
/// any bytes between this member and the first file member — e.g. a `"//"`
/// long-name string-table member — shifting the offsets so they stay correct;
/// pass 0 when there is no such prefix.
pub fn write_sysv_symtab<W: Write>(
    w: &mut W,
    members: &[MemberInfo],
    prefix_bytes: u64,
) -> io::Result<()> {
    let symbol_count: u64 = members.iter().map(|m| m.symbols.len() as u64).sum();
    let symbol_bytes: u64 = members.iter().map(|m| m.symbol_bytes()).sum();

    let mut table_size = (4 + symbol_count * 4 + symbol_bytes) as u32;
    // 2-byte alignment for the member payload.
    if table_size % 2 != 0 {
        table_size += 1;
    }

    let mut offsets = Vec::with_capacity(symbol_count as usize * 4);
    let mut name_blob = Vec::with_capacity(symbol_bytes as usize);
    // Offsets are computed in u64 and narrowed to the table's 32-bit field per
    // symbol; an archive large enough to overflow u32 (>4 GiB) cannot be
    // represented by a SysV symbol table, so error rather than truncate.
    let mut next_member_offset =
        MAGIC.len() as u64 + MEMBER_HEADER_SIZE + table_size as u64 + prefix_bytes;

    for member in members {
        for symbol in &member.symbols {
            let off: u32 = next_member_offset.try_into().map_err(|_| {
                io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "archive too large for 32-bit SysV symbol-table offsets",
                )
            })?;
            offsets.extend_from_slice(&off.to_be_bytes());
            name_blob.extend(symbol.as_bytes());
            name_blob.push(0);
        }
        // System V archives pad each member payload to a 2-byte boundary
        // (typically with `\n`). The pad byte sits AFTER `member.size` and
        // BEFORE the next member's header, so the offset of the next
        // member's header advances by the unpadded payload plus the pad
        // byte (if the payload size is odd).
        let padded = member.size + (member.size % 2);
        next_member_offset += MEMBER_HEADER_SIZE + padded;
    }

    let mut payload = Vec::with_capacity(table_size as usize);
    payload.extend(&(symbol_count as u32).to_be_bytes());
    payload.extend(&offsets);
    payload.extend(&name_blob);
    if payload.len() % 2 != 0 {
        payload.push(0);
    }

    // Member header for the "/" member.
    w.write_all(&pad_metadata_field::<16>("/")?)?;
    w.write_all(&pad_metadata_field::<12>("0")?)?;
    w.write_all(&pad_metadata_field::<6>("0")?)?;
    w.write_all(&pad_metadata_field::<6>("0")?)?;
    w.write_all(&pad_metadata_field::<8>("0")?)?;
    w.write_all(&pad_metadata_field::<10>(&table_size.to_string())?)?;
    w.write_all(TERMINATOR)?;
    w.write_all(&payload)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn name_table_empty_when_all_names_short() {
        let mut t = NameTable::new();
        t.push(b"short.o");
        t.push(b"exactly15chars_"); // 15 bytes: still inline
        assert!(t.is_empty());
        assert_eq!(t.member_bytes(), 0);
        assert_eq!(t.offset(0), None);
        assert_eq!(t.offset(1), None);
        let mut out = Vec::new();
        t.write(&mut out).unwrap();
        assert!(out.is_empty(), "no // member when every name fits inline");
    }

    #[test]
    fn name_table_records_long_names() {
        let mut t = NameTable::new();
        t.push(b"short.o");
        t.push(b"a_sixteen_byte_x"); // 16 bytes: one over the inline limit
        t.push(b"another_long_member_name.o");
        assert_eq!(t.offset(0), None);
        assert_eq!(t.offset(1), Some(0));
        // "a_sixteen_byte_x/\n" is 18 bytes.
        assert_eq!(t.offset(2), Some(18));

        let mut out = Vec::new();
        t.write(&mut out).unwrap();
        assert_eq!(&out[..2], b"//");
        assert_eq!(out.len() as u64, t.member_bytes());
        assert_eq!(out.len() % 2, 0, "// member must be 2-byte aligned");
        let payload = &out[MEMBER_HEADER_SIZE as usize..];
        assert!(payload.starts_with(b"a_sixteen_byte_x/\n"));
    }

    #[test]
    fn name_field_inline_and_referenced() {
        assert_eq!(
            &format_name_field(b"lib.o", None).unwrap(),
            b"lib.o/          "
        );
        assert_eq!(
            &format_name_field(b"ignored", Some(18)).unwrap(),
            b"/18             "
        );
    }

    #[test]
    fn name_field_rejects_overlong_inline_name() {
        // 16 bytes leaves no room for the "/" terminator.
        assert!(format_name_field(b"a_sixteen_byte_x", None).is_err());
    }

    #[test]
    fn pad_field_short() {
        assert_eq!(&pad_metadata_field::<4>("hi").unwrap(), b"hi  ");
    }

    #[test]
    fn pad_field_exact() {
        assert_eq!(&pad_metadata_field::<2>("hi").unwrap(), b"hi");
    }

    #[test]
    fn pad_field_too_long() {
        assert!(pad_metadata_field::<2>("toolong").is_err());
    }

    #[test]
    fn empty_archive_symbol_table_minimal() {
        let mut out = Vec::new();
        write_sysv_symtab(&mut out, &[], 0).unwrap();
        // Header is 60 bytes; payload is 4 bytes (count = 0).
        assert_eq!(out.len(), 60 + 4);
        // Count of symbols, big-endian.
        assert_eq!(&out[60..64], &[0, 0, 0, 0]);
        // Member name is "/" padded with spaces.
        assert!(out.starts_with(b"/               "));
        // Terminator at byte 58..60.
        assert_eq!(&out[58..60], b"`\n");
    }

    #[test]
    fn one_member_one_symbol() {
        let members = vec![MemberInfo {
            size: 100,
            symbols: vec!["main".to_string()],
        }];
        let mut out = Vec::new();
        write_sysv_symtab(&mut out, &members, 0).unwrap();
        // Payload: 4 (count) + 4 (offset) + 5 ("main\0") = 13, padded to 14.
        let payload_start = 60;
        let payload = &out[payload_start..];
        assert_eq!(&payload[0..4], &[0, 0, 0, 1]); // count
                                                   // Offset = MAGIC (8) + member header (60) + payload size (14) = 82.
        assert_eq!(u32::from_be_bytes(payload[4..8].try_into().unwrap()), 82);
        assert_eq!(&payload[8..12], b"main");
        assert_eq!(payload[12], 0); // NUL
        assert_eq!(payload.len(), 14); // padded
        assert_eq!(payload[13], 0); // pad byte
    }

    #[test]
    fn member_info_symbol_bytes() {
        let m = MemberInfo {
            size: 0,
            symbols: vec!["a".to_string(), "bc".to_string()],
        };
        assert_eq!(m.symbol_bytes(), 2 + 3); // "a\0" + "bc\0"
    }

    #[test]
    fn odd_sized_member_pad_advances_next_offset() {
        // Two members, the first one odd-sized. The symbol-table offset
        // for the second member's symbol must include the 1-byte newline
        // pad between member 0's payload and member 1's header.
        let members = vec![
            MemberInfo {
                size: 7, // odd → +1 pad byte before member 1's header
                symbols: vec!["m0".to_string()],
            },
            MemberInfo {
                size: 10, // even
                symbols: vec!["m1".to_string()],
            },
        ];
        let mut out = Vec::new();
        write_sysv_symtab(&mut out, &members, 0).unwrap();
        let payload = &out[60..];
        // Count = 2.
        assert_eq!(&payload[0..4], &[0, 0, 0, 2]);
        // Payload: 4 (count) + 8 (2 * offset) + 6 ("m0\0m1\0") = 18; even, no pad.
        // table_size = 18.
        // Offset0 = MAGIC (8) + table_member_header (60) + table_size (18) = 86.
        // Member 0 lays out as: 60 header + 7 payload + 1 pad = 68 bytes.
        // Offset1 = 86 + 68 = 154.
        let off0 = u32::from_be_bytes(payload[4..8].try_into().unwrap());
        let off1 = u32::from_be_bytes(payload[8..12].try_into().unwrap());
        assert_eq!(off0, 86, "offset for first symbol wrong");
        assert_eq!(
            off1, 154,
            "offset for second symbol must account for member-0 pad"
        );
    }
}
