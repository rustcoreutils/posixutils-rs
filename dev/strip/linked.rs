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

use crate::raw::{file_bytes, name_at, Layout, Result, SectionTable, Shdr};
use crate::{is_debug_section, Level, Options};
use gettextrs::gettext;
use object::elf;

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
        // No section headers: nothing a section-based strip can remove.
        let Some(table) = SectionTable::read(data)? else {
            return Ok(None);
        };
        let SectionTable {
            layout,
            shstrndx,
            shdrs,
            ..
        } = table;
        let shnum = shdrs.len();
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
