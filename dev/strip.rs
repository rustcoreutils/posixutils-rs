//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::gettext;
use object::{
    archive,
    build::elf::{Builder, Section, SectionData},
    elf, Object, ObjectSymbol, SymbolKind,
};
use plib::diag;
use std::{
    ffi::{OsStr, OsString},
    io::{Read, Write},
    path::Path,
};

#[derive(Parser)]
#[command(version, about = gettext("strip - remove unnecessary information from strippable files"))]
struct Args {
    input_files: Vec<OsString>,
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

fn strip_section(section: &Section) -> bool {
    is_debug_section(section.name.as_slice())
        // by removing all the symbols in the symbol table,
        // the below sections will all be empty
        || section.sh_type == elf::SHT_GROUP
        || section.sh_type == elf::SHT_RELA
        || section.sh_type == elf::SHT_REL
        // after we removed all symbols, the
        // symbol table contains only the undefined
        // symbol entry, which can be removed
        || section.name == ".symtab".into()
        // this section contains the strings of the symbol
        // table which are no longer used
        // after we removed all the symbols
        || matches!(section.data, SectionData::String)
}

type StripResult = Result<Vec<u8>, Box<dyn std::error::Error>>;

fn strip(data: &[u8]) -> StripResult {
    let mut builder = Builder::read(data)?;

    for symbol in &mut builder.symbols {
        symbol.delete = true;
    }
    for section in &mut builder.sections {
        if strip_section(section) {
            section.delete = true;
        }
    }
    builder.delete_orphans();
    let mut contents = Vec::new();
    builder.write(&mut contents)?;
    Ok(contents)
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

fn extract_member_symbols(data: &[u8]) -> Vec<String> {
    match object::read::File::parse(data) {
        Ok(file) => file
            .symbols()
            .filter(|s| {
                matches!(
                    s.kind(),
                    SymbolKind::Text | SymbolKind::Data | SymbolKind::Tls
                )
            })
            .filter_map(|s| s.name().ok().map(|n| n.to_string()))
            .collect(),
        Err(_) => Vec::new(),
    }
}

fn strip_archive(data: &[u8], file_name: &OsStr) -> StripResult {
    let mut archive = ar::Archive::new(data);
    let mut members: Vec<StrippedMember> = Vec::new();

    while let Some(entry) = archive.next_entry() {
        let mut entry = entry?;
        let mut data = Vec::new();
        entry.read_to_end(&mut data)?;
        let header = entry.header();

        if is_elf(&data) {
            let new_data = strip(&data)?;
            let symbols = extract_member_symbols(&new_data);
            members.push(StrippedMember {
                identifier: header.identifier().to_vec(),
                mtime: header.mtime(),
                uid: header.uid(),
                gid: header.gid(),
                mode: header.mode(),
                data: new_data,
                symbols,
            });
        } else {
            diag::error(&format!(
                "({}){}: {}",
                file_name.to_string_lossy(),
                String::from_utf8_lossy(header.identifier()),
                gettext("file format not recognized"),
            ));
        }
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
    plib::archive::write_sysv_symbol_table(&mut result, &infos)?;

    for m in &members {
        write_member(&mut result, m)?;
    }

    Ok(result)
}

fn write_member(w: &mut Vec<u8>, m: &StrippedMember) -> Result<(), Box<dyn std::error::Error>> {
    let mut name_field = [b' '; 16];
    let take = m.identifier.len().min(16);
    name_field[..take].copy_from_slice(&m.identifier[..take]);
    w.write_all(&name_field)?;
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
    if m.data.len() % 2 != 0 {
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

fn strip_file(file: &OsStr) {
    let contents = match std::fs::read(file) {
        Ok(contents) => contents,
        Err(err) => {
            diag::error(&format!(
                "{}: {}: {}",
                file.to_string_lossy(),
                gettext("error reading"),
                err
            ));
            return;
        }
    };
    let stripped_contents = if is_elf(&contents) {
        strip(&contents)
    } else if is_archive(&contents) {
        strip_archive(&contents, file)
    } else {
        diag::error(&format!(
            "{}: {}",
            file.to_string_lossy(),
            gettext("file format not recognized"),
        ));
        return;
    };
    match stripped_contents {
        Ok(stripped_contents) => {
            if let Err(err) = plib::io::write_atomic(Path::new(file), &stripped_contents) {
                diag::error(&format!(
                    "{}: {}: {}",
                    file.to_string_lossy(),
                    gettext("error writing file"),
                    err
                ));
            }
        }
        Err(err) => {
            diag::error(&format!("{}: {}", file.to_string_lossy(), err));
        }
    }
}

fn main() {
    diag::init_locale("strip");

    let args = Args::parse();

    for file in args.input_files {
        strip_file(&file);
    }
    std::process::exit(diag::exit_status());
}
