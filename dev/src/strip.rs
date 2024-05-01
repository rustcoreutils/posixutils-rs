//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{
    ffi::{OsStr, OsString},
    io::Read,
};

use clap::Parser;
use object::{
    archive,
    build::elf::{Builder, Section, SectionData},
    elf,
};

/// strip - remove unnecessary information from strippable files
#[derive(Parser)]
#[command(author, version, about, long_about)]
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

fn strip_archive(data: &[u8], file_name: &OsStr) -> StripResult {
    let mut archive = ar::Archive::new(data);
    let mut result = Vec::new();
    let mut stripped_archive = ar::Builder::new(&mut result);
    while let Some(entry) = archive.next_entry() {
        let mut entry = entry?;
        let mut data = Vec::new();
        entry.read_to_end(&mut data)?;
        let header = entry.header();

        if is_elf(&data) {
            let new_data = strip(&data)?;
            let mut new_header = header.clone();
            new_header.set_size(new_data.len() as u64);
            stripped_archive.append(&new_header, &*new_data)?;
        } else {
            eprintln!(
                "strip: ({}){}: file format not recognized",
                file_name.to_string_lossy(),
                String::from_utf8_lossy(header.identifier()),
            );
        }
    }
    Ok(result)
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
            eprintln!("strip: error reading {}: {}", file.to_string_lossy(), err);
            return;
        }
    };
    let stripped_contents = if is_elf(&contents) {
        strip(&contents)
    } else if is_archive(&contents) {
        strip_archive(&contents, file)
    } else {
        eprintln!(
            "strip: {}: file format not recognized",
            file.to_string_lossy()
        );
        return;
    };
    match stripped_contents {
        Ok(stripped_contents) => {
            if let Err(err) = std::fs::write(file, stripped_contents) {
                eprintln!(
                    "strip: error writing file {}: {}",
                    file.to_string_lossy(),
                    err
                );
            }
        }
        Err(err) => {
            eprintln!("strip: {}: {}", file.to_string_lossy(), err);
        }
    }
}

fn main() {
    let args = Args::parse();
    for file in args.input_files {
        strip_file(&file);
    }
}
