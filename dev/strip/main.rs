//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

#[path = "../armap.rs"]
mod armap;
mod linked;
mod raw;

use clap::Parser;
use gettextrs::gettext;
use object::{
    archive,
    build::{
        elf::{Builder, Section, SectionData},
        Id,
    },
    elf, Endian,
};
use plib::diag;
use std::{
    collections::HashSet,
    ffi::{OsStr, OsString},
    fs::{File, Metadata, OpenOptions},
    io::{Read, Seek, SeekFrom, Write},
};

#[derive(Parser)]
#[command(version, about = gettext("strip - remove unnecessary information from strippable files"),
          long_about = gettext("strip - remove unnecessary information from strippable files\n\n\
Supported input formats:\n  \
  * ELF relocatable objects, executables, and shared objects\n  \
  * System V / GNU `ar` archives of the above\n\n\
Relocatable objects (ET_REL) keep their symbol table and relocations so they\n\
remain linkable; only debugging information is removed. Executables and shared\n\
objects additionally lose their symbol table.\n\n\
Other formats -- Mach-O, COFF/PE, XCOFF, and BSD-variant archives -- are\n\
rejected with a diagnostic and a non-zero exit rather than being modified or\n\
silently passed through.\n\n\
The options are GNU extensions that debhelper's dh_strip passes."))]
struct Args {
    /// Remove only debugging sections and symbols
    #[arg(long)]
    strip_debug: bool,

    /// Remove every symbol that no relocation needs
    #[arg(long, conflicts_with = "strip_debug")]
    strip_unneeded: bool,

    /// Remove the sections whose names match PATTERN (`*` and `?` wildcards)
    #[arg(short = 'R', long = "remove-section", value_name = "PATTERN")]
    remove_section: Vec<String>,

    /// Remove the symbol SYMBOL
    #[arg(short = 'N', value_name = "SYMBOL")]
    strip_symbol: Vec<String>,

    /// Write archive members with zero timestamps and owners and mode 0644
    #[arg(long)]
    enable_deterministic_archives: bool,

    // POSIX SYNOPSIS makes the `file...` operand required (>= 1).
    #[arg(num_args = 1.., required = true)]
    input_files: Vec<OsString>,
}

/// How much of the symbol table goes.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Level {
    /// The default: an executable or shared object loses its symbol table.
    All,
    /// `--strip-unneeded`: keep only the symbols a relocation needs.
    Unneeded,
    /// `--strip-debug`: drop only debugging symbols.
    Debug,
}

struct Options {
    level: Level,
    remove_sections: Vec<String>,
    strip_symbols: Vec<String>,
    deterministic: bool,
}

impl Options {
    fn removes_section(&self, name: &[u8]) -> bool {
        self.remove_sections
            .iter()
            .any(|p| wildcard_match(p.as_bytes(), name))
    }

    fn strips_symbol(&self, name: &[u8]) -> bool {
        self.strip_symbols.iter().any(|s| s.as_bytes() == name)
    }
}

/// Match `name` against a `-R` pattern. GNU strip uses fnmatch(3); the only
/// wildcards dh_strip passes are `*`, so `*` and `?` are all this supports.
fn wildcard_match(pattern: &[u8], name: &[u8]) -> bool {
    match pattern.split_first() {
        None => name.is_empty(),
        Some((b'*', rest)) => (0..=name.len()).any(|i| wildcard_match(rest, &name[i..])),
        Some((b'?', rest)) => !name.is_empty() && wildcard_match(rest, &name[1..]),
        Some((c, rest)) => name.first() == Some(c) && wildcard_match(rest, &name[1..]),
    }
}

fn is_debug_section(name: &[u8]) -> bool {
    // names taken from the GNU binutils sources
    name.starts_with(b".debug")
        || name.starts_with(b".gnu.debuglto_.debug_")
        || name.starts_with(b".gnu.linkonce.wi.")
        || name.starts_with(b".zdebug")
        || name.starts_with(b".line")
        || name.starts_with(b".stab")
        || name.starts_with(b".gdb_index")
}

type StripResult = Result<Vec<u8>, Box<dyn std::error::Error>>;

/// Strip one ELF file. `display` names it in diagnostics.
fn strip(data: &[u8], opts: &Options, display: &str) -> StripResult {
    if raw::is_linked(data)? {
        return linked::strip(data, opts);
    }
    strip_relocatable(data, opts, display)
}

/// Strip a relocatable object (`.o`), renumbering its sections, symbols
/// and relocations through object's ELF builder.
fn strip_relocatable(data: &[u8], opts: &Options, display: &str) -> StripResult {
    let (groups, retyped) = hide_groups(data)?;
    let mut builder = Builder::read(retyped.as_deref().unwrap_or(data))?;
    for section in &mut builder.sections {
        let name = section.name.as_slice();
        if is_debug_section(name) || opts.removes_section(name) {
            section.delete = true;
        }
    }
    delete_relocations_of_deleted_sections(&mut builder);
    let signatures = prune_groups(&mut builder, &groups);
    select_symbols(&mut builder, opts, display, &signatures)?;
    restore_groups(&mut builder, &groups)?;
    let mut contents = Vec::new();
    builder.write(&mut contents)?;
    Ok(contents)
}

/// The section indices of the SHT_GROUP sections (COMDAT groups, in every
/// C++ object), which object's builder refuses to read, and a copy of
/// `data` in which they are retyped SHT_PROGBITS so it reads them as data.
fn hide_groups(data: &[u8]) -> raw::Result<(Vec<usize>, Option<Vec<u8>>)> {
    let Some(mut table) = raw::SectionTable::read(data)? else {
        return Ok((Vec::new(), None));
    };
    let groups: Vec<usize> = (0..table.shdrs.len())
        .filter(|&i| table.shdrs[i].sh_type == elf::SHT_GROUP)
        .collect();
    if groups.is_empty() {
        return Ok((groups, None));
    }
    let mut copy = data.to_vec();
    for &i in &groups {
        table.shdrs[i].sh_type = elf::SHT_PROGBITS;
        table.store(&mut copy, i);
    }
    Ok((groups, Some(copy)))
}

/// A group section's words: its flags, then its member section indices.
fn group_words(builder: &Builder, section: &Section) -> Vec<u32> {
    match &section.data {
        SectionData::Data(bytes) => bytes
            .as_chunks::<4>()
            .0
            .iter()
            .map(|w| builder.endian.read_u32_bytes(*w))
            .collect(),
        _ => Vec::new(),
    }
}

/// Whether builder section `section` was section header `index`.
fn is_header(section: &Section, index: usize) -> bool {
    section.id().index() + 1 == index
}

/// Delete the groups whose every member was removed, and return the
/// symbol indices of the kept groups' signatures, which must stay.
fn prune_groups(builder: &mut Builder, groups: &[usize]) -> HashSet<usize> {
    let kept = kept_sections(builder);
    let mut empty = Vec::new();
    let mut signatures = HashSet::new();
    for section in builder.sections.iter() {
        if !groups.iter().any(|&g| is_header(section, g)) {
            continue;
        }
        let words = group_words(builder, section);
        let mut members = words.iter().skip(1);
        if members.any(|&m| kept.contains(&(m as usize).wrapping_sub(1))) {
            signatures.insert((section.sh_info as usize).wrapping_sub(1));
        } else {
            empty.push(section.id());
        }
    }
    for id in empty {
        builder.sections.get_mut(id).delete = true;
    }
    signatures
}

/// Turn the kept groups back into SHT_GROUP sections, with their member
/// and signature indices renumbered the way the builder will write them:
/// sections in order, local symbols before the others.
fn restore_groups(builder: &mut Builder, groups: &[usize]) -> raw::Result<()> {
    if groups.is_empty() {
        return Ok(());
    }
    let new_section: std::collections::HashMap<usize, u32> = builder
        .sections
        .iter()
        .enumerate()
        .map(|(n, s)| (s.id().index() + 1, n as u32 + 1))
        .collect();
    let locals = builder
        .symbols
        .iter()
        .filter(|s| s.st_bind() == elf::STB_LOCAL);
    let others = builder
        .symbols
        .iter()
        .filter(|s| s.st_bind() != elf::STB_LOCAL);
    let new_symbol: std::collections::HashMap<usize, u32> = locals
        .chain(others)
        .enumerate()
        .map(|(n, s)| (s.id().index() + 1, n as u32 + 1))
        .collect();

    let mut rebuilt = Vec::new();
    for section in builder.sections.iter() {
        if !groups.iter().any(|&g| is_header(section, g)) {
            continue;
        }
        let words = group_words(builder, section);
        let mut bytes = Vec::with_capacity(words.len() * 4);
        for (n, &word) in words.iter().enumerate() {
            let word = if n == 0 {
                word
            } else {
                match new_section.get(&(word as usize)) {
                    Some(&index) => index,
                    None => continue,
                }
            };
            bytes.extend_from_slice(&builder.endian.write_u32_bytes(word));
        }
        let signature = new_symbol
            .get(&(section.sh_info as usize))
            .copied()
            .ok_or_else(|| gettext("a COMDAT group lost its signature symbol"))?;
        rebuilt.push((section.id(), bytes, signature));
    }
    for (id, bytes, signature) in rebuilt {
        let section = builder.sections.get_mut(id);
        section.sh_type = elf::SHT_GROUP;
        section.sh_info = signature;
        section.data = SectionData::Data(bytes.into());
    }
    Ok(())
}

/// Indices of the sections that survive.
fn kept_sections(builder: &Builder) -> HashSet<usize> {
    builder.sections.iter().map(|s| s.id().index()).collect()
}

/// A relocation section goes with the section it applies to.
fn delete_relocations_of_deleted_sections(builder: &mut Builder) {
    let kept = kept_sections(builder);
    for section in &mut builder.sections {
        let is_reloc = matches!(
            section.data,
            SectionData::Relocation(_) | SectionData::DynamicRelocation(_)
        );
        if is_reloc
            && section
                .sh_info_section
                .is_some_and(|target| !kept.contains(&target.index()))
        {
            section.delete = true;
        }
    }
}

/// Mark the symbols of a relocatable object that the options remove.
/// #ST4: the object must remain linkable, so by default it loses only what
/// --strip-debug removes, and --strip-unneeded removes just the local
/// symbols no relocation names. A symbol a kept relocation names always
/// stays: deleting it would make the builder silently drop the relocation.
/// So does a group signature (`signatures`).
fn select_symbols(
    builder: &mut Builder,
    opts: &Options,
    display: &str,
    signatures: &HashSet<usize>,
) -> Result<(), Box<dyn std::error::Error>> {
    let unneeded = opts.level == Level::Unneeded;
    let kept = kept_sections(builder);
    let mut needed = signatures.clone();
    for section in &builder.sections {
        if let SectionData::Relocation(relocs) = &section.data {
            needed.extend(relocs.iter().filter_map(|r| r.symbol).map(|s| s.index()));
        }
    }
    for symbol in &mut builder.symbols {
        let name = symbol.name.as_slice();
        let in_removed_section = symbol.section.is_some_and(|s| !kept.contains(&s.index()));
        if needed.contains(&symbol.id().index()) {
            if in_removed_section {
                return Err(format!(
                    "{}: {}",
                    String::from_utf8_lossy(name),
                    gettext("symbol named in a relocation is in a removed section")
                )
                .into());
            }
            if opts.strips_symbol(name) {
                diag::warning(&format!(
                    "{}: {} `{}' {}",
                    display,
                    gettext("not stripping symbol"),
                    String::from_utf8_lossy(name),
                    gettext("because it is named in a relocation")
                ));
            }
            continue;
        }
        // STT_FILE symbols are debugging symbols to GNU strip.
        symbol.delete = in_removed_section
            || opts.strips_symbol(name)
            || symbol.st_type() == elf::STT_FILE
            || (unneeded && symbol.st_bind() == elf::STB_LOCAL);
    }
    Ok(())
}

/// One member of the rewritten archive: header metadata + payload.
struct StrippedMember {
    identifier: Vec<u8>,
    mtime: u64,
    uid: u32,
    gid: u32,
    mode: u32,
    data: Vec<u8>,
    /// Symbol names exported by the stripped payload (text/data/TLS only).
    /// Used to regenerate the `"/"` archive symbol-table member so the
    /// resulting archive is still usable for link editing per POSIX 84371-84376.
    symbols: Vec<String>,
}

fn strip_archive(data: &[u8], opts: &Options, display: &str) -> StripResult {
    // #ST11: `!<arch>\n` is shared by the System V and BSD layouts, and the
    // variant is only known once headers have been parsed. The writer below
    // only speaks System V, so rewriting a BSD archive (the macOS default)
    // would emit a malformed hybrid: BSD stores long names inline after the
    // header and uses a `__.SYMDEF` symbol table, neither of which we produce.
    // Probe the headers first so we refuse before doing any work, and so the
    // variant diagnostic wins over any per-member parse error.
    let mut probe = ar::Archive::new(data);
    while let Some(entry) = probe.next_entry() {
        entry?;
    }
    if probe.variant() == ar::Variant::BSD {
        return Err(gettext(
            "BSD-variant archives are not supported (only System V/GNU archives can be rewritten)",
        )
        .into());
    }

    let mut archive = ar::Archive::new(data);
    let mut members: Vec<StrippedMember> = Vec::new();

    while let Some(entry) = archive.next_entry() {
        let mut entry = entry?;
        let mut data = Vec::new();
        entry.read_to_end(&mut data)?;
        let header = entry.header();

        // #ST1: only ELF members are stripped; any other member (a non-object
        // file legitimately stored in the archive) is preserved unmodified.
        // Dropping it would be silent data loss. The `ar` crate already hides
        // the archive's own "/" symbol-table and "//" name-table members.
        let (data, symbols) = if is_elf(&data) {
            let member = format!(
                "{}({})",
                display,
                String::from_utf8_lossy(header.identifier())
            );
            let new_data = strip(&data, opts, &member)?;
            let symbols = armap::member_symbols(&new_data);
            (new_data, symbols)
        } else {
            (data, Vec::new())
        };
        let mut member = StrippedMember {
            identifier: header.identifier().to_vec(),
            mtime: header.mtime(),
            uid: header.uid(),
            gid: header.gid(),
            mode: header.mode(),
            data,
            symbols,
        };
        if opts.deterministic {
            member.mtime = 0;
            member.uid = 0;
            member.gid = 0;
            member.mode = 0o644;
        }
        members.push(member);
    }

    // Emit: magic + "/" symbol-table member (regenerated per POSIX 84371-84376) +
    // member-by-member { 60-byte header, payload, optional NUL pad to keep
    // 2-byte alignment }.
    let mut result = Vec::new();
    result.write_all(plib::archive::MAGIC)?;

    let infos: Vec<plib::archive::MemberInfo> = members
        .iter()
        .map(|m| plib::archive::MemberInfo {
            size: m.data.len() as u64,
            symbols: m.symbols.clone(),
        })
        .collect();

    // #ST10: names longer than the 16-byte header field go into a System V
    // "//" string-table member. The `ar` crate resolves long names on read, so
    // truncating them here silently renamed members. Shared with `dev/ar.rs`
    // through plib so both tools emit the same layout.
    let mut names = plib::archive::NameTable::new();
    for m in &members {
        names.push(&m.identifier);
    }

    plib::archive::write_sysv_symtab(&mut result, &infos, names.member_bytes())?;
    names.write(&mut result)?;

    for (i, m) in members.iter().enumerate() {
        write_member(&mut result, m, names.offset(i))?;
    }

    Ok(result)
}

fn write_member(
    w: &mut Vec<u8>,
    m: &StrippedMember,
    long_name_offset: Option<usize>,
) -> Result<(), Box<dyn std::error::Error>> {
    w.write_all(&plib::archive::format_name_field(
        &m.identifier,
        long_name_offset,
    )?)?;
    w.write_all(&plib::archive::pad_metadata_field::<12>(
        &m.mtime.to_string(),
    )?)?;
    w.write_all(&plib::archive::pad_metadata_field::<6>(&m.uid.to_string())?)?;
    w.write_all(&plib::archive::pad_metadata_field::<6>(&m.gid.to_string())?)?;
    w.write_all(&plib::archive::pad_metadata_field::<8>(&format!(
        "{:o}",
        m.mode
    ))?)?;
    w.write_all(&plib::archive::pad_metadata_field::<10>(
        &m.data.len().to_string(),
    )?)?;
    w.write_all(plib::archive::TERMINATOR)?;
    w.write_all(&m.data)?;
    if !m.data.len().is_multiple_of(2) {
        w.write_all(b"\n")?;
    }
    Ok(())
}

fn is_elf(data: &[u8]) -> bool {
    data.starts_with(&elf::ELFMAG)
}

fn is_archive(data: &[u8]) -> bool {
    data.starts_with(&archive::MAGIC)
}

/// Open the operand `file` to read it and write the stripped result back
/// into it. Its path is resolved here, once; everything after goes through
/// this descriptor, so a rename or symbolic link planted later cannot
/// redirect the write. A symbolic link operand is followed, as GNU strip
/// follows it. O_NONBLOCK keeps the open of a FIFO from waiting for a
/// writer before [`read_operand`] rejects it, and O_NOCTTY keeps a terminal
/// from becoming the controlling one.
fn open_operand(file: &OsStr) -> std::io::Result<File> {
    let mut options = OpenOptions::new();
    options.read(true).write(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.custom_flags(libc::O_NONBLOCK | libc::O_NOCTTY);
    }
    options.open(file)
}

/// The contents and metadata of the open operand, which must be a regular
/// file: GNU strip refuses anything else.
fn read_operand(file: &mut File) -> std::io::Result<(Vec<u8>, Metadata)> {
    let metadata = file.metadata()?;
    if !metadata.is_file() {
        return Err(std::io::Error::other(gettext("not an ordinary file")));
    }
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;
    Ok((contents, metadata))
}

/// Replace the contents of the open operand with `bytes` in place. The
/// inode stays the operand's own, so its hard links, owner and mode stay
/// too, as with GNU strip. The kernel clears the set-user-ID and
/// set-group-ID bits when a non-root user writes a file; they are put
/// back as they were (`before`).
fn write_operand(file: &mut File, before: &Metadata, bytes: &[u8]) -> std::io::Result<()> {
    file.seek(SeekFrom::Start(0))?;
    file.write_all(bytes)?;
    file.set_len(bytes.len() as u64)?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mode = before.permissions().mode() & 0o7777;
        if file.metadata()?.permissions().mode() & 0o7777 != mode {
            file.set_permissions(std::fs::Permissions::from_mode(mode))?;
        }
    }
    #[cfg(not(unix))]
    let _ = before;
    Ok(())
}

fn strip_file(file: &OsStr, opts: &Options) {
    let display = file.to_string_lossy();
    let opened = open_operand(file)
        .and_then(|mut fd| read_operand(&mut fd).map(|(contents, meta)| (fd, contents, meta)));
    let (mut fd, contents, metadata) = match opened {
        Ok(opened) => opened,
        Err(err) => {
            diag::error(&format!(
                "{}: {}: {}",
                display,
                gettext("error reading"),
                diag::io_error_text(&err)
            ));
            return;
        }
    };
    let stripped_contents = if is_elf(&contents) {
        strip(&contents, opts, &display)
    } else if is_archive(&contents) {
        strip_archive(&contents, opts, &display)
    } else {
        // #ST3: only ELF objects/executables and ar archives are supported.
        // strip rewrites via object::build::elf::Builder, and the object crate's
        // `build` (read-modify-write) module is ELF-only — there is no
        // build::macho. (object reads Mach-O fine, which is why nm/strings work
        // on it; only the in-place rewrite strip needs is missing.) A faithful
        // Mach-O strip would need a hand-rolled __LINKEDIT/load-command rewrite,
        // so other formats (Mach-O, COFF/PE, XCOFF) are rejected here rather
        // than silently passed through.
        diag::error(&format!(
            "{}: {}",
            file.to_string_lossy(),
            gettext("unsupported file format (only ELF objects/executables and ar archives are supported)"),
        ));
        return;
    };
    match stripped_contents {
        Ok(stripped_contents) => {
            if let Err(err) = write_operand(&mut fd, &metadata, &stripped_contents) {
                diag::error(&format!(
                    "{}: {}: {}",
                    display,
                    gettext("error writing file"),
                    diag::io_error_text(&err)
                ));
            }
        }
        Err(err) => {
            diag::error(&format!(
                "{}: {}",
                file.to_string_lossy(),
                diag::error_text(err.as_ref())
            ));
        }
    }
}

fn main() {
    diag::init_locale("strip");

    let args = Args::parse();
    let level = if args.strip_unneeded {
        Level::Unneeded
    } else if args.strip_debug {
        Level::Debug
    } else {
        Level::All
    };
    let opts = Options {
        level,
        remove_sections: args.remove_section,
        strip_symbols: args.strip_symbol,
        deterministic: args.enable_deterministic_archives,
    };

    for file in args.input_files {
        strip_file(&file, &opts);
    }
    std::process::exit(diag::exit_status());
}
