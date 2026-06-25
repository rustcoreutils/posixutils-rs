//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, BufWriter, Read, Write};
use std::path::{Path, PathBuf};

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::io::input_stream_dashed;
use plib::locale::{mb_char_slices, wcwidth_char};

const TABSTOP: usize = 8;

/// fold - filter for folding lines
#[derive(Parser, Clone)]
#[command(version, about = gettext("fold - filter for folding lines"))]
struct Args {
    #[arg(short, long, help = gettext("Count width in bytes rather than column positions."))]
    bytes: bool,

    #[arg(short, long, help = gettext("Break on spaces."))]
    spaces: bool,

    #[arg(short, long, default_value_t = 80, value_parser = clap::value_parser!(u64).range(1..),
          help = gettext("Specify the maximum line length, in column positions (or bytes if -b is specified)."))]
    width: u64,

    #[arg(help = gettext("Files to read as input."))]
    files: Vec<PathBuf>,
}

/// Column reached after appending character `ch` (a single locale-segmented
/// character) to a line whose current column is `col`.
///
/// In `-b` (byte) mode every character contributes its byte length and the
/// control-character column semantics do not apply. Otherwise `<backspace>`
/// decrements (never below zero), `<carriage-return>` resets to zero, `<tab>`
/// advances to the next 8-column stop, and any other character advances by its
/// display width under `LC_CTYPE`.
fn char_advance(col: usize, ch: &[u8], bytes_mode: bool) -> usize {
    if bytes_mode {
        return col + ch.len();
    }
    match ch {
        b"\x08" => col.saturating_sub(1),
        b"\r" => 0,
        b"\t" => col + (TABSTOP - (col % TABSTOP)),
        _ => {
            let w = std::str::from_utf8(ch)
                .ok()
                .and_then(|s| s.chars().next())
                .map(wcwidth_char)
                .unwrap_or(1);
            col + if w > 0 { w as usize } else { 0 }
        }
    }
}

/// Index of the last `<blank>` (`<space>` or `<tab>`) byte in `v`, or `None`.
///
/// POSIX `<blank>` is space and tab only; blank bytes are ASCII and never occur
/// inside a multibyte character, so a byte scan is safe.
fn find_last_blank(v: &[u8]) -> Option<usize> {
    v.iter().rposition(|&b| b == b' ' || b == b'\t')
}

fn fold_file(args: &Args, pathname: &Path) -> io::Result<()> {
    // open file, or stdin ("-" or no operand)
    let mut file = input_stream_dashed(pathname)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    let width = args.width as usize;
    let mut out = BufWriter::new(io::stdout());
    let mut line: Vec<u8> = Vec::new();
    let mut col: usize = 0;

    for ch in mb_char_slices(&data) {
        if ch == b"\n" {
            line.extend_from_slice(ch);
            out.write_all(&line)?;
            line.clear();
            col = 0;
            continue;
        }

        let mut next = char_advance(col, ch, args.bytes);

        // Insert breaks while appending this character would exceed the width
        // and the line is non-empty (a single over-wide character on an empty
        // line is emitted as-is; the spec leaves that case undefined).
        while next > width && !line.is_empty() {
            let mut folded = false;
            if args.spaces {
                if let Some(b) = find_last_blank(&line) {
                    out.write_all(&line[..=b])?;
                    out.write_all(b"\n")?;
                    let remainder = line[b + 1..].to_vec();
                    line = remainder;
                    // Recompute the column over the kept remainder (its
                    // characters are complete: blanks are char boundaries).
                    col = 0;
                    for rc in mb_char_slices(&line) {
                        col = char_advance(col, rc, args.bytes);
                    }
                    folded = true;
                }
            }
            if !folded {
                out.write_all(&line)?;
                out.write_all(b"\n")?;
                line.clear();
                col = 0;
            }
            next = char_advance(col, ch, args.bytes);
        }

        line.extend_from_slice(ch);
        col = next;
    }

    // Trailing partial line (input without a final newline).
    if !line.is_empty() {
        out.write_all(&line)?;
    }
    out.flush()?;

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    // if no files, read from stdin
    if args.files.is_empty() {
        args.files.push(PathBuf::new());
    }

    let mut exit_code = 0;

    for filename in &args.files {
        if let Err(e) = fold_file(&args, filename) {
            exit_code = 1;
            eprintln!("{}: {}", filename.display(), e);
        }
    }

    std::process::exit(exit_code)
}
