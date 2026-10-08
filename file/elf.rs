//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Built-in ELF identification, one of the default system tests.
//!
//! The text has the shape GNU file prints, cut down to what Debian's build
//! tools read from it: dh_strip matches `ELF.*shared`,
//! `ELF.*(executable|shared)` and `not stripped`; dh_shlibdeps matches `ELF`
//! and `statically linked`.

use std::io::{self, Read, SeekFrom};

use crate::magic::ReadSeek;

const ET_REL: u16 = 1;
const ET_EXEC: u16 = 2;
const ET_DYN: u16 = 3;
const ET_CORE: u16 = 4;
const PT_DYNAMIC: u32 = 2;
const PT_INTERP: u32 = 3;
const SHT_SYMTAB: u32 = 2;

/// The header fields the description needs, already widened.
struct Header {
    class64: bool,
    msb: bool,
    e_type: u16,
    phoff: u64,
    phentsize: u64,
    phnum: u64,
    shoff: u64,
    shentsize: u64,
    shnum: u64,
}

impl Header {
    fn uint(&self, bytes: &[u8]) -> u64 {
        let mut v = 0u64;
        if self.msb {
            for b in bytes {
                v = (v << 8) | u64::from(*b);
            }
        } else {
            for b in bytes.iter().rev() {
                v = (v << 8) | u64::from(*b);
            }
        }
        v
    }

    /// A word-sized field: 8 bytes in ELFCLASS64, 4 in ELFCLASS32.
    fn word(&self, bytes: &[u8], off64: usize, off32: usize) -> u64 {
        if self.class64 {
            self.uint(&bytes[off64..off64 + 8])
        } else {
            self.uint(&bytes[off32..off32 + 4])
        }
    }

    fn half(&self, bytes: &[u8], off64: usize, off32: usize) -> u64 {
        let at = if self.class64 { off64 } else { off32 };
        self.uint(&bytes[at..at + 2])
    }

    fn parse(bytes: &[u8]) -> Option<Header> {
        if bytes.len() < 16 || &bytes[..4] != b"\x7fELF" {
            return None;
        }
        let class64 = match bytes[4] {
            1 => false,
            2 => true,
            _ => return None,
        };
        let msb = match bytes[5] {
            1 => false,
            2 => true,
            _ => return None,
        };
        let ehsize = if class64 { 64 } else { 52 };
        if bytes.len() < ehsize {
            return None;
        }
        let mut h = Header {
            class64,
            msb,
            e_type: 0,
            phoff: 0,
            phentsize: 0,
            phnum: 0,
            shoff: 0,
            shentsize: 0,
            shnum: 0,
        };
        h.e_type = h.uint(&bytes[16..18]) as u16;
        h.phoff = h.word(bytes, 32, 28);
        h.shoff = h.word(bytes, 40, 32);
        h.phentsize = h.half(bytes, 54, 42);
        h.phnum = h.half(bytes, 56, 44);
        h.shentsize = h.half(bytes, 58, 46);
        h.shnum = h.half(bytes, 60, 48);
        Some(h)
    }
}

/// Read `len` bytes at `off`; `None` if the file ends first.
fn read_at(r: &mut dyn ReadSeek, off: u64, len: usize) -> Option<Vec<u8>> {
    let mut buf = vec![0u8; len];
    r.seek(SeekFrom::Start(off)).ok()?;
    r.read_exact(&mut buf).ok()?;
    Some(buf)
}

/// The `p_type` of every program header that can be read.
fn segment_types(r: &mut dyn ReadSeek, h: &Header) -> Vec<u32> {
    let mut types = Vec::new();
    if h.phoff == 0 || h.phentsize < 4 {
        return types;
    }
    for i in 0..h.phnum {
        let Some(off) = i
            .checked_mul(h.phentsize)
            .and_then(|o| o.checked_add(h.phoff))
        else {
            break;
        };
        match read_at(r, off, 4) {
            Some(b) => types.push(h.uint(&b) as u32),
            None => break,
        }
    }
    types
}

/// Whether a section header of type SHT_SYMTAB exists: what "not stripped"
/// means.
fn has_symtab(r: &mut dyn ReadSeek, h: &Header, file_len: u64) -> bool {
    if h.shoff == 0 || h.shentsize < 8 {
        return false;
    }
    let mut count = h.shnum;
    // An object with 0xff00 or more sections stores the count in section 0's
    // sh_size and leaves e_shnum zero.
    if count == 0 {
        let size_off = if h.class64 { 32 } else { 20 };
        let width = if h.class64 { 8 } else { 4 };
        match h
            .shoff
            .checked_add(size_off)
            .and_then(|off| read_at(r, off, width))
        {
            Some(b) => count = h.uint(&b),
            None => return false,
        }
    }
    // Every header must lie inside the file, which bounds the loop.
    count = count.min(file_len / h.shentsize);
    for i in 0..count {
        let Some(off) = i
            .checked_mul(h.shentsize)
            .and_then(|o| o.checked_add(h.shoff))
            .and_then(|o| o.checked_add(4))
        else {
            break;
        };
        match read_at(r, off, 4) {
            Some(b) if h.uint(&b) as u32 == SHT_SYMTAB => return true,
            Some(_) => {}
            None => break,
        }
    }
    false
}

/// Describe `r` if it is an ELF file, else `None`.
pub fn describe(r: &mut dyn ReadSeek) -> Option<String> {
    let mut head = Vec::with_capacity(64);
    r.seek(SeekFrom::Start(0)).ok()?;
    Read::take(&mut *r, 64).read_to_end(&mut head).ok()?;
    let h = Header::parse(&head)?;
    let file_len = r.seek(SeekFrom::End(0)).unwrap_or(0);

    let mut out = format!(
        "ELF {}-bit {}",
        if h.class64 { 64 } else { 32 },
        if h.msb { "MSB" } else { "LSB" }
    );
    match h.e_type {
        ET_REL => out.push_str(" relocatable"),
        ET_EXEC => out.push_str(" executable"),
        ET_DYN => out.push_str(" shared object"),
        ET_CORE => out.push_str(" core file"),
        _ => {}
    }
    if h.e_type == ET_EXEC || h.e_type == ET_DYN {
        let segs = segment_types(r, &h);
        let dynamic = segs.iter().any(|t| *t == PT_DYNAMIC || *t == PT_INTERP);
        out.push_str(if dynamic {
            ", dynamically linked"
        } else {
            ", statically linked"
        });
    }
    if h.e_type != ET_CORE {
        out.push_str(if has_symtab(r, &h, file_len) {
            ", not stripped"
        } else {
            ", stripped"
        });
    }
    Some(out)
}

/// Describe the file opened by `make_reader`, swallowing open errors: the
/// caller falls through to its other tests.
pub fn describe_with<F>(make_reader: &F) -> Option<String>
where
    F: Fn() -> io::Result<Box<dyn ReadSeek>>,
{
    let mut r = make_reader().ok()?;
    describe(&mut *r)
}
