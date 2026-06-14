//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

#[derive(Parser)]
#[command(version, about = gettext("what - identify SCCS files"))]
struct Args {
    #[arg(
        short = 's',
        long,
        help = gettext("Display at most one identification string per file")
    )]
    single: bool,

    #[arg(help = gettext("Input files"))]
    files: Vec<PathBuf>,
}

/// Scan raw bytes for `@(#)` identification strings. Operates on bytes (never
/// UTF-8-decodes) so it works on any file type, including binaries — the
/// primary use case for `what`. Each identification string runs from after the
/// `@(#)` marker up to (but not including) a terminator byte: `"`, `>`,
/// `<newline>`, `\`, or NUL.
fn process_file(data: &[u8], single: bool, out: &mut impl Write) -> io::Result<bool> {
    const MARKER: &[u8] = b"@(#)";
    let is_terminator = |b: u8| matches!(b, b'"' | b'>' | b'\n' | b'\\' | 0);

    let mut found = false;
    let mut i = 0;
    while i + MARKER.len() <= data.len() {
        if &data[i..i + MARKER.len()] == MARKER {
            let start = i + MARKER.len();
            let mut end = start;
            while end < data.len() && !is_terminator(data[end]) {
                end += 1;
            }
            out.write_all(b"\t")?;
            out.write_all(&data[start..end])?;
            out.write_all(b"\n")?;
            found = true;
            if single {
                return Ok(true);
            }
            i = end;
        } else {
            i += 1;
        }
    }
    Ok(found)
}

fn main() -> io::Result<()> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();
    let mut any_found = false;

    let stdout = io::stdout();
    let mut out = stdout.lock();

    for file in &args.files {
        let path = Path::new(file);
        match fs::read(path) {
            Ok(data) => {
                // Print the pathname first per POSIX format.
                writeln!(out, "{}:", file.display())?;
                if process_file(&data, args.single, &mut out)? {
                    any_found = true;
                }
            }
            Err(_) => {
                eprintln!("what: {}: {}", gettext("Cannot open file"), file.display());
            }
        }
    }

    // Exit 0 if any matches found, 1 otherwise
    std::process::exit(if any_found { 0 } else { 1 });
}
