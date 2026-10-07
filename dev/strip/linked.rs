//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Strip a linked ELF file -- an executable or a shared object -- the way
//! GNU strip does: only sections the loader never maps go, so every byte of
//! every loadable segment stays as it was. The kept non-loadable sections,
//! a new section-name table and a new section header table follow the
//! loadable image.

use crate::{is_debug_section, Level, Options};
use gettextrs::gettext;
use object::elf;
use std::error::Error;

type Result<T> = std::result::Result<T, Box<dyn Error>>;

/// ELF class and byte order, and the field accessors that depend on them.
#[derive(Clone, Copy)]
struct Layout {
    big_endian: bool,
    is_64: bool,
}

impl Layout {
    fn of(data: &[u8]) -> Result<Self> {
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
    fn word(self) -> usize {
        if self.is_64 {
            8
        } else {
            4
        }
    }

    fn get(self, data: &[u8], offset: usize, size: usize) -> Result<u64> {
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

    fn set(self, data: &mut [u8], offset: usize, size: usize, value: u64) {
        for i in 0..size {
            let shift = if self.big_endian { size - 1 - i } else { i };
            data[offset + i] = (value >> (8 * shift)) as u8;
        }
    }

    fn put(self, out: &mut Vec<u8>, size: usize, value: u64) {
        let at = out.len();
        out.resize(at + size, 0);
        self.set(out, at, size, value);
    }

    /// Sizes of the section header fields, in order: name, type, flags,
    /// addr, offset, size, link, info, addralign, entsize.
    fn shdr_fields(self) -> [usize; 10] {
        let w = self.word();
        [4, 4, w, w, w, w, 4, 4, w, w]
    }

    fn ehdr_shoff(self) -> usize {
        24 + 2 * self.word()
    }

    fn ehdr_shentsize(self) -> usize {
        34 + 3 * self.word()
    }
}

/// A section header, field by field.
#[derive(Clone)]
struct Shdr {
    name: u64,
    sh_type: u32,
    flags: u64,
    offset: u64,
    size: u64,
    link: u32,
    info: u32,
    addralign: u64,
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

    fn write(&self, layout: Layout, out: &mut Vec<u8>) {
        let mut raw = self.raw;
        raw[0] = self.name;
        raw[4] = self.offset;
        raw[5] = self.size;
        raw[6] = u64::from(self.link);
        raw[7] = u64::from(self.info);
        for (value, size) in raw.into_iter().zip(layout.shdr_fields()) {
            layout.put(out, size, value);
        }
    }

    fn is_reloc(&self) -> bool {
        self.sh_type == elf::SHT_REL || self.sh_type == elf::SHT_RELA
    }

    /// Whether `info` holds a section index.
    fn info_is_section(&self) -> bool {
        self.flags & u64::from(elf::SHF_INFO_LINK) != 0 || (self.is_reloc() && self.info != 0)
    }

    fn has_file_bytes(&self) -> bool {
        self.sh_type != elf::SHT_NOBITS
    }

    fn end(&self) -> u64 {
        self.offset.saturating_add(self.size)
    }
}

/// Whether `data` is a linked ELF file (anything but a relocatable object).
pub fn is_linked(data: &[u8]) -> Result<bool> {
    let layout = Layout::of(data)?;
    Ok(layout.get(data, 16, 2)? != u64::from(elf::ET_REL))
}

fn name_at(strtab: &[u8], offset: u64) -> &[u8] {
    let rest = usize::try_from(offset)
        .ok()
        .and_then(|o| strtab.get(o..))
        .unwrap_or_default();
    rest.split(|&b| b == 0).next().unwrap_or_default()
}

fn file_bytes<'a>(data: &'a [u8], shdr: &Shdr) -> Result<&'a [u8]> {
    if !shdr.has_file_bytes() {
        return Ok(&[]);
    }
    usize::try_from(shdr.offset)
        .ok()
        .zip(usize::try_from(shdr.end()).ok())
        .and_then(|(start, end)| data.get(start..end))
        .ok_or_else(|| gettext("section extends past the end of the file").into())
}

/// The sections of a linked file and what happens to them.
struct Plan<'a> {
    layout: Layout,
    data: &'a [u8],
    shdrs: Vec<Shdr>,
    names: Vec<&'a [u8]>,
    shstrndx: usize,
    symtab: Option<usize>,
    delete: Vec<bool>,
}

impl<'a> Plan<'a> {
    fn read(data: &'a [u8]) -> Result<Option<Plan<'a>>> {
        let layout = Layout::of(data)?;
        let w = layout.word();
        let shoff = layout.get(data, layout.ehdr_shoff(), w)? as usize;
        let shentsize = layout.get(data, layout.ehdr_shentsize(), 2)? as usize;
        let shnum = layout.get(data, layout.ehdr_shentsize() + 2, 2)? as usize;
        let shstrndx = layout.get(data, layout.ehdr_shentsize() + 4, 2)? as usize;
        if shoff == 0 {
            // No section headers: nothing a section-based strip can remove.
            return Ok(None);
        }
        if shnum == 0 || shstrndx == elf::SHN_XINDEX as usize || shstrndx >= shnum {
            return Err(gettext("extended section numbering is not supported").into());
        }
        let shdrs = (0..shnum)
            .map(|i| Shdr::read(layout, data, shoff + i * shentsize))
            .collect::<Result<Vec<_>>>()?;
        let shstrtab = file_bytes(data, &shdrs[shstrndx])?;
        let names = shdrs.iter().map(|s| name_at(shstrtab, s.name)).collect();
        let symtab = shdrs.iter().position(|s| s.sh_type == elf::SHT_SYMTAB);
        Ok(Some(Plan {
            layout,
            data,
            shdrs,
            names,
            shstrndx,
            symtab,
            delete: vec![false; shnum],
        }))
    }

    /// Whether section `i` belongs to the static symbol table: the table,
    /// its strings, its extended indices, and relocations and groups that
    /// index it.
    fn is_symtab_part(&self, i: usize) -> bool {
        let Some(t) = self.symtab else {
            return false;
        };
        let s = &self.shdrs[i];
        i == t
            || self.shdrs[t].link as usize == i
            || (s.link as usize == t
                && (s.is_reloc()
                    || s.sh_type == elf::SHT_SYMTAB_SHNDX
                    || s.sh_type == elf::SHT_GROUP))
    }

    fn choose(&mut self, opts: &Options, drop_symtab: bool) {
        for i in 1..self.shdrs.len() {
            // The section-name table is always rewritten, never removed.
            if i == self.shstrndx {
                continue;
            }
            let name = self.names[i];
            self.delete[i] = is_debug_section(name)
                || opts.removes_section(name)
                || (drop_symtab && self.is_symtab_part(i));
        }
        // A relocation section goes with the section it applies to.
        for i in 1..self.shdrs.len() {
            let s = &self.shdrs[i];
            if s.is_reloc()
                && s.info_is_section()
                && self.delete.get(s.info as usize) == Some(&true)
            {
                self.delete[i] = true;
            }
        }
    }

    /// New section indices; `None` for a removed section.
    fn new_indices(&self) -> Vec<Option<u32>> {
        let mut next = 0u32;
        self.delete
            .iter()
            .map(|&gone| {
                (!gone).then(|| {
                    next += 1;
                    next - 1
                })
            })
            .collect()
    }

    fn remap(&self, new_index: &[Option<u32>], old: u32) -> u32 {
        new_index.get(old as usize).copied().flatten().unwrap_or(0)
    }

    /// The end of everything the loader maps, and of any kept section that
    /// overlaps it: these bytes are copied unchanged.
    fn image_end(&self) -> Result<usize> {
        let layout = self.layout;
        let data = self.data;
        let w = layout.word();
        let phoff = layout.get(data, 24 + w, w)? as usize;
        let phentsize = layout.get(data, layout.ehdr_shentsize() - 4, 2)? as usize;
        let phnum = layout.get(data, layout.ehdr_shentsize() - 2, 2)? as usize;
        let ehsize = layout.get(data, layout.ehdr_shentsize() - 6, 2)?;
        let mut end = ehsize.max((phoff + phentsize * phnum) as u64);
        // p_offset and p_filesz
        let (off_at, filesz_at) = if layout.is_64 { (8, 32) } else { (4, 16) };
        for i in 0..phnum {
            let ph = phoff + i * phentsize;
            let offset = layout.get(data, ph + off_at, w)?;
            let filesz = layout.get(data, ph + filesz_at, w)?;
            end = end.max(offset.saturating_add(filesz));
        }
        for (s, &gone) in self.shdrs.iter().zip(&self.delete) {
            if !gone && s.has_file_bytes() && s.flags & u64::from(elf::SHF_ALLOC) != 0 {
                end = end.max(s.end());
            }
        }
        loop {
            let grown = self
                .shdrs
                .iter()
                .zip(&self.delete)
                .filter(|(s, &gone)| !gone && s.has_file_bytes() && s.size > 0)
                .filter(|(s, _)| s.offset < end && s.end() > end)
                .map(|(s, _)| s.end())
                .max();
            match grown {
                Some(e) => end = e,
                None => break,
            }
        }
        usize::try_from(end)
            .ok()
            .filter(|&e| e <= data.len())
            .ok_or_else(|| gettext("loadable segment extends past the end of the file").into())
    }

    /// The kept symbol table without its debugging symbols and the symbols
    /// `-N` names, with section indices renumbered; and its new `sh_info`.
    fn filtered_symtab(
        &self,
        t: usize,
        opts: &Options,
        new_index: &[Option<u32>],
    ) -> Result<(Vec<u8>, u32)> {
        let layout = self.layout;
        let symtab = &self.shdrs[t];
        let bytes = file_bytes(self.data, symtab)?;
        let strtab = file_bytes(self.data, &self.shdrs[symtab.link as usize])?;
        let (entsize, info_at, shndx_at) = if layout.is_64 {
            (24, 4, 6)
        } else {
            (16, 12, 14)
        };
        let mut out = Vec::with_capacity(bytes.len());
        let mut kept_locals = 0u32;
        let mut dropped = false;
        for (i, sym) in bytes.chunks_exact(entsize).enumerate() {
            let name = name_at(strtab, layout.get(sym, 0, 4)?);
            let st_type = layout.get(sym, info_at, 1)? as u8 & 0xf;
            let shndx = layout.get(sym, shndx_at, 2)? as u32;
            if shndx == u32::from(elf::SHN_XINDEX) {
                return Err(gettext("extended symbol section indices are not supported").into());
            }
            let regular = shndx != 0 && shndx < u32::from(elf::SHN_LORESERVE);
            let drop = i > 0
                && (st_type == elf::STT_FILE
                    || opts.strips_symbol(name)
                    || (regular && self.delete.get(shndx as usize) == Some(&true)));
            if drop {
                dropped = true;
                continue;
            }
            if (i as u32) < symtab.info {
                kept_locals += 1;
            }
            let at = out.len();
            out.extend_from_slice(sym);
            if regular {
                layout.set(
                    &mut out,
                    at + shndx_at,
                    2,
                    u64::from(self.remap(new_index, shndx)),
                );
            }
        }
        let indexed_by_relocs = (1..self.shdrs.len()).any(|i| {
            !self.delete[i] && self.shdrs[i].is_reloc() && self.shdrs[i].link as usize == t
        });
        if dropped && indexed_by_relocs {
            return Err(gettext(
                "cannot remove symbols from a linked file whose relocations index its symbol table",
            )
            .into());
        }
        Ok((out, kept_locals))
    }

    /// The new section-name table and each kept section's name offset.
    /// When the symbol table shares its strings with the section names,
    /// the table is kept whole so symbol names stay valid.
    fn section_names(&self, symtab_kept: bool) -> (Vec<u8>, Vec<u64>) {
        let shares =
            symtab_kept && self.symtab.map(|t| self.shdrs[t].link as usize) == Some(self.shstrndx);
        if shares {
            let whole = file_bytes(self.data, &self.shdrs[self.shstrndx]).unwrap_or_default();
            return (whole.to_vec(), self.shdrs.iter().map(|s| s.name).collect());
        }
        let mut table = vec![0u8];
        let offsets = self
            .names
            .iter()
            .zip(&self.delete)
            .enumerate()
            .map(|(i, (name, &gone))| {
                if i == 0 || gone {
                    return 0;
                }
                let at = table.len() as u64;
                table.extend_from_slice(name);
                table.push(0);
                at
            })
            .collect();
        (table, offsets)
    }

    fn write(&self, opts: &Options, drop_symtab: bool) -> Result<Vec<u8>> {
        let layout = self.layout;
        let new_index = self.new_indices();
        let end = self.image_end()?;
        let symtab_kept = !drop_symtab && self.symtab.is_some_and(|t| !self.delete[t]);
        let symtab = match self.symtab {
            Some(t) if symtab_kept => Some((t, self.filtered_symtab(t, opts, &new_index)?)),
            _ => None,
        };
        let (shstrtab, name_offsets) = self.section_names(symtab_kept);

        let mut out = self.data[..end].to_vec();
        let mut shdrs = self.shdrs.clone();
        for (s, &name) in shdrs.iter_mut().zip(&name_offsets) {
            s.name = name;
        }

        // Kept non-loadable sections past the image, in file order.
        let mut tail: Vec<usize> = (1..shdrs.len())
            .filter(|&i| {
                !self.delete[i]
                    && i != self.shstrndx
                    && shdrs[i].flags & u64::from(elf::SHF_ALLOC) == 0
                    && shdrs[i].offset >= end as u64
            })
            .collect();
        tail.sort_by_key(|&i| shdrs[i].offset);
        for i in tail {
            let bytes = match &symtab {
                Some((t, (filtered, _))) if *t == i => filtered.as_slice(),
                _ => file_bytes(self.data, &shdrs[i])?,
            };
            align(&mut out, shdrs[i].addralign);
            shdrs[i].offset = out.len() as u64;
            if shdrs[i].has_file_bytes() {
                shdrs[i].size = bytes.len() as u64;
                out.extend_from_slice(bytes);
            }
        }
        if let Some((t, (filtered, locals))) = &symtab {
            let in_image = shdrs[*t].offset < end as u64;
            if in_image && filtered.as_slice() != file_bytes(self.data, &self.shdrs[*t])? {
                return Err(
                    gettext("cannot rewrite a symbol table inside the loadable image").into(),
                );
            }
            shdrs[*t].info = *locals;
        }
        shdrs[self.shstrndx].offset = out.len() as u64;
        shdrs[self.shstrndx].size = shstrtab.len() as u64;
        out.extend_from_slice(&shstrtab);

        align(&mut out, layout.word() as u64);
        let shoff = out.len();
        let mut count = 0u64;
        for (i, s) in shdrs.iter_mut().enumerate() {
            if self.delete[i] {
                continue;
            }
            if i != 0 {
                s.link = self.remap(&new_index, s.link);
                if s.info_is_section() {
                    s.info = self.remap(&new_index, s.info);
                }
            }
            s.write(layout, &mut out);
            count += 1;
        }
        let shentsize_at = layout.ehdr_shentsize();
        layout.set(&mut out, layout.ehdr_shoff(), layout.word(), shoff as u64);
        layout.set(
            &mut out,
            shentsize_at,
            2,
            layout.shdr_fields().iter().sum::<usize>() as u64,
        );
        layout.set(&mut out, shentsize_at + 2, 2, count);
        layout.set(
            &mut out,
            shentsize_at + 4,
            2,
            u64::from(self.remap(&new_index, self.shstrndx as u32)),
        );
        Ok(out)
    }
}

fn align(out: &mut Vec<u8>, alignment: u64) {
    let a = alignment.max(1) as usize;
    out.resize(out.len().div_ceil(a) * a, 0);
}

/// Strip a linked file. `--strip-unneeded` removes from it what the
/// default does: none of its relocations need the static symbol table.
pub fn strip(data: &[u8], opts: &Options) -> Result<Vec<u8>> {
    let Some(mut plan) = Plan::read(data)? else {
        return Ok(data.to_vec());
    };
    let drop_symtab = opts.level != Level::Debug;
    plan.choose(opts, drop_symtab);
    plan.write(opts, drop_symtab)
}
