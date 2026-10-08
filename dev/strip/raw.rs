//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! ELF headers read and written field by field, in either class and byte
//! order, for the rewrites object's ELF builder cannot do.

use gettextrs::gettext;
use object::elf;
use std::error::Error;

pub type Result<T> = std::result::Result<T, Box<dyn Error>>;

/// ELF class and byte order, and the field accessors that depend on them.
#[derive(Clone, Copy)]
pub struct Layout {
    big_endian: bool,
    pub is_64: bool,
}

impl Layout {
    pub fn of(data: &[u8]) -> Result<Self> {
        // e_ident[EI_CLASS] and e_ident[EI_DATA]
        let class = data.get(4);
        let order = data.get(5);
        match (class, order) {
            (Some(&c), Some(&d))
                if (c == elf::ELFCLASS32 || c == elf::ELFCLASS64)
                    && (d == elf::ELFDATA2LSB || d == elf::ELFDATA2MSB) =>
            {
                Ok(Layout {
                    big_endian: d == elf::ELFDATA2MSB,
                    is_64: c == elf::ELFCLASS64,
                })
            }
            _ => Err(gettext("unsupported ELF class or byte order").into()),
        }
    }

    /// Size of an address or offset field.
    pub fn word(self) -> usize {
        if self.is_64 {
            8
        } else {
            4
        }
    }

    pub fn get(self, data: &[u8], offset: usize, size: usize) -> Result<u64> {
        let bytes = offset
            .checked_add(size)
            .and_then(|end| data.get(offset..end))
            .ok_or_else(|| gettext("truncated ELF file"))?;
        let fold = |v: u64, b: &u8| (v << 8) | u64::from(*b);
        Ok(if self.big_endian {
            bytes.iter().fold(0, fold)
        } else {
            bytes.iter().rev().fold(0, fold)
        })
    }

    pub fn set(self, data: &mut [u8], offset: usize, size: usize, value: u64) {
        for i in 0..size {
            let shift = if self.big_endian { size - 1 - i } else { i };
            data[offset + i] = (value >> (8 * shift)) as u8;
        }
    }

    pub fn put(self, out: &mut Vec<u8>, size: usize, value: u64) {
        let at = out.len();
        out.resize(at + size, 0);
        self.set(out, at, size, value);
    }

    /// Sizes of the section header fields, in order: name, type, flags,
    /// addr, offset, size, link, info, addralign, entsize.
    pub fn shdr_fields(self) -> [usize; 10] {
        let w = self.word();
        [4, 4, w, w, w, w, 4, 4, w, w]
    }

    pub fn ehdr_shoff(self) -> usize {
        24 + 2 * self.word()
    }

    pub fn ehdr_shentsize(self) -> usize {
        34 + 3 * self.word()
    }
}

/// A section header, field by field.
#[derive(Clone)]
pub struct Shdr {
    pub name: u64,
    pub sh_type: u32,
    pub flags: u64,
    pub offset: u64,
    pub size: u64,
    pub link: u32,
    pub info: u32,
    pub addralign: u64,
    raw: [u64; 10],
}

impl Shdr {
    fn read(layout: Layout, data: &[u8], mut at: usize) -> Result<Shdr> {
        let mut raw = [0u64; 10];
        for (field, size) in raw.iter_mut().zip(layout.shdr_fields()) {
            *field = layout.get(data, at, size)?;
            at += size;
        }
        Ok(Shdr {
            name: raw[0],
            sh_type: raw[1] as u32,
            flags: raw[2],
            offset: raw[4],
            size: raw[5],
            link: raw[6] as u32,
            info: raw[7] as u32,
            addralign: raw[8],
            raw,
        })
    }

    pub fn write(&self, layout: Layout, out: &mut Vec<u8>) {
        let mut raw = self.raw;
        raw[0] = self.name;
        raw[1] = u64::from(self.sh_type);
        raw[4] = self.offset;
        raw[5] = self.size;
        raw[6] = u64::from(self.link);
        raw[7] = u64::from(self.info);
        for (value, size) in raw.into_iter().zip(layout.shdr_fields()) {
            layout.put(out, size, value);
        }
    }

    pub fn is_reloc(&self) -> bool {
        self.sh_type == elf::SHT_REL || self.sh_type == elf::SHT_RELA
    }

    /// Whether `info` holds a section index.
    pub fn info_is_section(&self) -> bool {
        self.flags & u64::from(elf::SHF_INFO_LINK) != 0 || (self.is_reloc() && self.info != 0)
    }

    pub fn has_file_bytes(&self) -> bool {
        self.sh_type != elf::SHT_NOBITS
    }

    pub fn end(&self) -> u64 {
        self.offset.saturating_add(self.size)
    }
}

/// The section header table of an ELF file.
pub struct SectionTable {
    pub layout: Layout,
    pub shoff: usize,
    pub shentsize: usize,
    pub shstrndx: usize,
    pub shdrs: Vec<Shdr>,
}

impl SectionTable {
    /// `None` for a file without section headers.
    pub fn read(data: &[u8]) -> Result<Option<SectionTable>> {
        let layout = Layout::of(data)?;
        let at = layout.ehdr_shentsize();
        let shoff = layout.get(data, layout.ehdr_shoff(), layout.word())? as usize;
        let shentsize = layout.get(data, at, 2)? as usize;
        let shnum = layout.get(data, at + 2, 2)? as usize;
        let shstrndx = layout.get(data, at + 4, 2)? as usize;
        if shoff == 0 {
            return Ok(None);
        }
        if shnum == 0 || shstrndx == elf::SHN_XINDEX as usize || shstrndx >= shnum {
            return Err(gettext("extended section numbering is not supported").into());
        }
        let shdrs = (0..shnum)
            .map(|i| Shdr::read(layout, data, shoff + i * shentsize))
            .collect::<Result<Vec<_>>>()?;
        Ok(Some(SectionTable {
            layout,
            shoff,
            shentsize,
            shstrndx,
            shdrs,
        }))
    }

    /// Rewrite header `i` of `data` in place.
    pub fn store(&self, data: &mut [u8], i: usize) {
        let mut bytes = Vec::with_capacity(self.shentsize);
        self.shdrs[i].write(self.layout, &mut bytes);
        let at = self.shoff + i * self.shentsize;
        data[at..at + bytes.len()].copy_from_slice(&bytes);
    }
}

/// Whether `data` is a linked ELF file (anything but a relocatable object).
pub fn is_linked(data: &[u8]) -> Result<bool> {
    let layout = Layout::of(data)?;
    Ok(layout.get(data, 16, 2)? != u64::from(elf::ET_REL))
}

pub fn name_at(strtab: &[u8], offset: u64) -> &[u8] {
    let rest = usize::try_from(offset)
        .ok()
        .and_then(|o| strtab.get(o..))
        .unwrap_or_default();
    rest.split(|&b| b == 0).next().unwrap_or_default()
}

pub fn file_bytes<'a>(data: &'a [u8], shdr: &Shdr) -> Result<&'a [u8]> {
    if !shdr.has_file_bytes() {
        return Ok(&[]);
    }
    usize::try_from(shdr.offset)
        .ok()
        .zip(usize::try_from(shdr.end()).ok())
        .and_then(|(start, end)| data.get(start..end))
        .ok_or_else(|| gettext("section extends past the end of the file").into())
}
