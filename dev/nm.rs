//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::{Parser, ValueEnum};
use gettextrs::gettext;
use object::{
    Object, ObjectSection, ObjectSymbol, SectionIndex, SectionKind, Symbol, SymbolKind,
    SymbolScope, SymbolSection,
};
use plib::{diag, locale};
use std::collections::HashMap;
use std::fs;

#[derive(ValueEnum, Clone, Copy, PartialEq)]
enum OutputType {
    /// decimal
    D,
    /// octal
    O,
    /// hexadecimal
    X,
}

/// nm - write the name list of an object file
#[derive(Parser)]
#[command(version, about = gettext("nm - write the name list of an object file"))]
struct Args {
    #[arg(short = 'A', help = gettext("Write the full pathname or library name of an object on each line"))]
    print_name: bool,

    #[arg(short = 'e', help = gettext("Write only external (global) and static symbol information"))]
    external_only: bool,

    #[arg(short = 'f', help = gettext("Produce full output (include redundant .text/.data/.bss symbols)"))]
    full: bool,

    #[arg(short = 'g', conflicts_with = "undef", help = gettext("Write only external (global) symbol information"))]
    global: bool,

    #[arg(short = 'o', help = gettext("Write numeric values in octal (equivalent to -t o)"))]
    octal: bool,

    #[arg(short = 'x', help = gettext("Write numeric values in hexadecimal (equivalent to -t x)"))]
    hex: bool,

    #[arg(short = 'P', help = gettext("Write information in a portable output format"))]
    portable: bool,

    #[arg(short = 't', value_enum, help = gettext("Write each numeric value in the specified format (d, o, or x)"))]
    out_type: Option<OutputType>,

    #[arg(short = 'u', help = gettext("Write only undefined symbols"))]
    undef: bool,

    #[arg(short = 'v', help = gettext("Sort output by value instead of by symbol name"))]
    value_sort: bool,

    #[arg(required = true, num_args = 1.., help = gettext("Input object file(s) or library"))]
    files: Vec<String>,
}

/// A collected symbol, decoupled from the borrowed object so it can be sorted.
struct SymInfo {
    name: String,
    value: u64,
    size: u64,
    type_char: char,
    undefined: bool,
}

/// The single-character symbol type, per POSIX STDOUT 108768-108779 plus the
/// usual implementation extensions (`C` common, `r` read-only data).
fn classify(symbol: &Symbol<'_, '_>, section_kinds: &HashMap<SectionIndex, SectionKind>) -> char {
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
    kind
}

/// Whether a symbol passes the active filters (-f/-g/-u/-e).
fn included(symbol: &Symbol<'_, '_>, args: &Args) -> bool {
    // The unnamed null symbol (index 0) is not part of the name list.
    if symbol.name().map(str::is_empty).unwrap_or(true) {
        return false;
    }
    match symbol.kind() {
        // File-name symbols are never part of the name list.
        SymbolKind::File => return false,
        // The redundant section symbols (.text/.data/.bss) are suppressed by
        // default and only shown with -f (#N10).
        SymbolKind::Section if !args.full => return false,
        _ => {}
    }
    if args.global && !symbol.is_global() {
        return false;
    }
    if args.undef && !symbol.is_undefined() {
        return false;
    }
    if args.external_only {
        let external_or_static = symbol.is_global()
            || matches!(
                symbol.scope(),
                SymbolScope::Linkage | SymbolScope::Compilation
            );
        if !external_or_static {
            return false;
        }
    }
    true
}

/// Collect and sort the symbols of one parsed object.
fn collect_symbols(file: &object::File<'_>, args: &Args) -> Vec<SymInfo> {
    let section_kinds: HashMap<SectionIndex, SectionKind> =
        file.sections().map(|s| (s.index(), s.kind())).collect();

    // Prefer the regular symbol table; fall back to the dynamic table when a
    // file has only the latter (e.g. a stripped shared object).
    let mut symbols: Vec<_> = file.symbols().collect();
    if symbols.is_empty() {
        symbols = file.dynamic_symbols().collect();
    }

    let mut out: Vec<SymInfo> = symbols
        .iter()
        .filter(|s| included(s, args))
        .map(|s| SymInfo {
            name: s.name().unwrap_or("<unknown>").to_string(),
            value: s.address(),
            size: s.size(),
            type_char: classify(s, &section_kinds),
            undefined: s.is_undefined(),
        })
        .collect();

    // POSIX STDOUT 108765-108766: sort by symbol name using the current
    // locale's collation (#N5); -v sorts by value instead (#N9).
    if args.value_sort {
        out.sort_by(|a, b| {
            a.value
                .cmp(&b.value)
                .then_with(|| locale::strcoll(&a.name, &b.name))
        });
    } else {
        out.sort_by(|a, b| locale::strcoll(&a.name, &b.name).then_with(|| a.value.cmp(&b.value)));
    }
    out
}

/// Resolve the numeric radix from -t/-o/-x (#N6/#N7); -P defaults to hex.
fn resolve_radix(args: &Args) -> OutputType {
    if args.octal {
        OutputType::O
    } else if args.hex {
        OutputType::X
    } else {
        args.out_type.unwrap_or(if args.portable {
            OutputType::X
        } else {
            OutputType::D
        })
    }
}

fn fmt_value(value: u64, radix: OutputType) -> String {
    match radix {
        OutputType::D => value.to_string(),
        OutputType::O => format!("{:o}", value),
        OutputType::X => format!("{:x}", value),
    }
}

/// Print the collected symbols. `prefix` is the per-line `-A` prefix
/// (`"file: "` / `"file[member]: "`), or empty when `-A` is not set.
fn print_symbols(symbols: &[SymInfo], args: &Args, radix: OutputType, prefix: &str) {
    for s in symbols {
        if args.portable {
            // POSIX STDOUT 108784-108792: "<name> <type> <value> <size>" with
            // the library/object-name prefix glued to the name. Undefined
            // symbols carry blank value/size columns.
            let (value, size) = if s.undefined {
                (String::new(), String::new())
            } else {
                (fmt_value(s.value, radix), fmt_value(s.size, radix))
            };
            println!("{}{} {} {} {}", prefix, s.name, s.type_char, value, size);
        } else {
            // Default (POSIX-unspecified) format: value, type, name. Undefined
            // symbols leave the value column blank.
            print!("{}", prefix);
            if s.undefined {
                print!("{:>16} ", "");
            } else {
                match radix {
                    OutputType::X => print!("{:016x} ", s.value),
                    OutputType::O => print!("{:016o} ", s.value),
                    OutputType::D => print!("{:16} ", s.value),
                }
            }
            println!("{} {}", s.type_char, s.name);
        }
    }
}

/// Process one parsed object (a standalone file or an archive member).
fn show_object(file: &object::File<'_>, args: &Args, radix: OutputType, prefix: &str) {
    let symbols = collect_symbols(file, args);
    print_symbols(&symbols, args, radix, prefix);
}

fn process_input(args: &Args, path: &str, radix: OutputType, multiple: bool) -> Result<(), ()> {
    // FUTURE DIRECTIONS 108854-108858 (#N17): refuse a pathname containing a
    // newline rather than emit a corrupted, unparseable line.
    if path.contains('\n') {
        diag::error(&format!(
            "{}: {}",
            gettext("pathname contains a newline"),
            path.escape_debug()
        ));
        return Err(());
    }

    let data = match fs::read(path) {
        Ok(d) => d,
        Err(err) => {
            diag::error(&format!(
                "{}: {}: {}",
                path,
                gettext("failed to open file"),
                err
            ));
            return Err(());
        }
    };

    // Archive (`.a`) input: emit one stanza per member (#N2).
    if let Ok(archive) = object::read::archive::ArchiveFile::parse(&*data) {
        let mut ok = true;
        for member in archive.members() {
            let member = match member {
                Ok(m) => m,
                Err(_) => {
                    diag::error(&format!("{}: {}", path, gettext("invalid archive member")));
                    ok = false;
                    continue;
                }
            };
            let member_name = String::from_utf8_lossy(member.name()).to_string();
            let member_data = match member.data(&*data) {
                Ok(d) => d,
                Err(_) => {
                    diag::error(&format!(
                        "{}[{}]: {}",
                        path,
                        member_name,
                        gettext("cannot read archive member")
                    ));
                    ok = false;
                    continue;
                }
            };
            let obj = match object::File::parse(member_data) {
                Ok(o) => o,
                Err(_) => continue, // non-object members carry no symbols
            };
            if args.print_name {
                show_object(&obj, args, radix, &format!("{}[{}]: ", path, member_name));
            } else {
                println!("{}[{}]:", path, member_name);
                show_object(&obj, args, radix, "");
            }
        }
        return if ok { Ok(()) } else { Err(()) };
    }

    // Plain object file / executable.
    let file = match object::File::parse(&*data) {
        Ok(f) => f,
        Err(err) => {
            diag::error(&format!(
                "{}: {}: {}",
                path,
                gettext("failed to parse file"),
                err
            ));
            return Err(());
        }
    };

    if args.print_name {
        show_object(&file, args, radix, &format!("{}: ", path));
    } else {
        // A header is written when more than one file operand is given
        // (POSIX STDOUT 108800-108807).
        if multiple {
            println!("{}:", path);
        }
        show_object(&file, args, radix, "");
    }
    Ok(())
}

fn main() {
    diag::init_locale("nm");

    let args = Args::parse();
    let radix = resolve_radix(&args);
    let multiple = args.files.len() > 1;

    for path in &args.files {
        // Errors are logged inside process_input; exit_status() reflects them.
        let _ = process_input(&args, path, radix, multiple);
    }

    std::process::exit(diag::exit_status());
}
