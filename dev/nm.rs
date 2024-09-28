//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - vary output based on args
// - sort output
//

use object::{
    Object, ObjectSection, ObjectSymbol, SectionIndex, SectionKind, Symbol, SymbolKind,
    SymbolSection,
};

use clap::{Parser, ValueEnum};
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use std::collections::HashMap;
use std::fs;

#[derive(ValueEnum, Clone)]
enum OutputType {
    D,
    O,
    X,
}

/// nm - write the name list of an object file
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Write the full pathname or library name of an object on each line.
    #[arg(short = 'A', long = "print-file-name")]
    print_name: bool,

    /// Write only external (global) and static symbol information.
    #[arg(short = 'e', long = "external")]
    external_only: bool,

    /// Produce full output.
    #[arg(short, long)]
    full: bool,

    /// Write only external (global) symbol information.
    #[arg(short, long = "extern-only")]
    global: bool,

    /// Write numeric values in octal (equivalent to -t o).
    #[arg(short, long)]
    octal: bool,

    /// Write numeric values in hexadecimal (equivalent to -t x).
    #[arg(short = 'x', long)]
    hex: bool,

    /// Write information in a portable output format
    #[arg(short = 'P', long = "portability")]
    portable: bool,

    /// Write each numeric value in the specified format.
    #[arg(short = 't', long = "format", value_enum, default_value = "d")]
    out_type: OutputType,

    /// Write only undefined symbols.
    #[arg(short, long = "undefined-only")]
    undef: bool,

    /// Sort output by value instead of by symbol name.
    #[arg(short, long)]
    value_sort: bool,

    /// Input object file
    file: String,
}

fn print_symbol(symbol: &Symbol<'_, '_>, section_kinds: &HashMap<SectionIndex, SectionKind>) {
    if let SymbolKind::Section | SymbolKind::File = symbol.kind() {
        return;
    }

    let mut kind = match symbol.section() {
        SymbolSection::Undefined => 'U',
        SymbolSection::Absolute => 'A',
        SymbolSection::Common => 'C',
        SymbolSection::Section(index) => match section_kinds.get(&index) {
            Some(SectionKind::Text) => 't',
            Some(SectionKind::Data) | Some(SectionKind::Tls) | Some(SectionKind::TlsVariables) => {
                'd'
            }
            Some(SectionKind::ReadOnlyData) | Some(SectionKind::ReadOnlyString) => 'r',
            Some(SectionKind::UninitializedData) | Some(SectionKind::UninitializedTls) => 'b',
            Some(SectionKind::Common) => 'C',
            _ => '?',
        },
        _ => '?',
    };

    if symbol.is_global() {
        kind = kind.to_ascii_uppercase();
    }

    if symbol.is_undefined() {
        print!("{:16} ", "");
    } else {
        print!("{:016x} ", symbol.address());
    }
    println!("{} {}", kind, symbol.name().unwrap_or("<unknown>"),);
}

fn show_object_file(args: &Args) -> Result<(), Box<dyn std::error::Error>> {
    let file_path = &args.file;
    {
        let filedata = match fs::read(file_path) {
            Ok(file) => file,
            Err(err) => {
                println!("Failed to open file '{}': {}", file_path, err,);
                return Err(Box::new(err));
            }
        };
        let file = match object::File::parse(&*filedata) {
            Ok(file) => file,
            Err(err) => {
                println!("Failed to parse file '{}': {}", file_path, err);
                return Err(Box::new(err));
            }
        };

        let section_kinds = file.sections().map(|s| (s.index(), s.kind())).collect();

        for symbol in file.symbols() {
            print_symbol(&symbol, &section_kinds);
        }
        for symbol in file.dynamic_symbols() {
            print_symbol(&symbol, &section_kinds);
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME"))?;
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8")?;

    show_object_file(&args)?;

    Ok(())
}
