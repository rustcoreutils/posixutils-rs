//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
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

/// Scan a byte stream for `@(#)` identification strings. Operates on bytes
/// (never UTF-8-decodes) so it works on any file type, including large binaries
/// — the primary use case for `what`. The input is consumed through a buffered
/// reader and only the in-progress identification string is held in memory, so
/// peak memory is bounded by the longest ident rather than the file size. Each
/// identification string runs from after the `@(#)` marker up to (but not
/// including) a terminator byte: `"`, `>`, `<newline>`, `\`, or NUL.
fn process_file<R: BufRead>(mut reader: R, single: bool, out: &mut impl Write) -> io::Result<bool> {
    const MARKER: &[u8] = b"@(#)";
    let is_terminator = |b: u8| matches!(b, b'"' | b'>' | b'\n' | b'\\' | 0);

    let mut found = false;
    let mut mpos = 0usize; // bytes of MARKER matched so far
    let mut in_ident = false;
    let mut ident: Vec<u8> = Vec::new();

    loop {
        let chunk = reader.fill_buf()?;
        if chunk.is_empty() {
            break; // EOF
        }
        let n = chunk.len();
        let mut stop = false;

        for &b in chunk {
            if in_ident {
                if is_terminator(b) {
                    out.write_all(b"\t")?;
                    out.write_all(&ident)?;
                    out.write_all(b"\n")?;
                    found = true;
                    ident.clear();
                    in_ident = false;
                    mpos = 0;
                    if single {
                        stop = true;
                        break;
                    }
                } else {
                    ident.push(b);
                }
            } else if b == MARKER[mpos] {
                mpos += 1;
                if mpos == MARKER.len() {
                    in_ident = true;
                    mpos = 0;
                }
            } else {
                // Mismatch: the only self-overlap in MARKER is its first byte.
                mpos = if b == MARKER[0] { 1 } else { 0 };
            }
        }

        reader.consume(n);
        if stop {
            return Ok(true);
        }
    }

    // EOF reached while still accumulating an ident (no terminator): emit it.
    if in_ident {
        out.write_all(b"\t")?;
        out.write_all(&ident)?;
        out.write_all(b"\n")?;
        found = true;
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
        match File::open(path) {
            Ok(f) => {
                // Print the pathname first per POSIX format.
                writeln!(out, "{}:", file.display())?;
                let reader = BufReader::new(f);
                if process_file(reader, args.single, &mut out)? {
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
