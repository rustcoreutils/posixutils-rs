//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{
    ffi::OsStr,
    io::{self, Read},
    ops::AddAssign,
    path::{Path, PathBuf},
};

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::io::input_stream_dashed;
use plib::locale::{isspace, MbDecoder};
use plib::BUFSZ;

/// wc - word, line, and byte or character count
#[derive(Parser)]
#[command(version, about = gettext("wc - word, line, and byte or character count"))]
struct Args {
    #[arg(short = 'c', long, help = gettext("Count number of bytes in each file"))]
    bytes: bool,

    #[arg(short, long, help = gettext("Count number of lines in each file"))]
    lines: bool,

    #[arg(short = 'm', long, help = gettext("Count number of characters in each file"))]
    chars: bool,

    #[arg(short, long, help = gettext("Count number of words in each file"))]
    words: bool,

    #[arg(help = gettext("Files to read as input."))]
    files: Vec<PathBuf>,
}

#[derive(Default)]
struct CountInfo {
    words: usize,
    chars: usize,
    nl: usize,
}

impl AddAssign for CountInfo {
    fn add_assign(&mut self, rhs: Self) {
        self.words += rhs.words;
        self.chars += rhs.chars;
        self.nl += rhs.nl;
    }
}

/// The field values printed for a row, in POSIX column order
/// (lines, words, bytes-or-characters), filtered to the requested counts.
fn row_values(args: &Args, count: &CountInfo) -> Vec<usize> {
    let mut v = Vec::with_capacity(3);
    if args.lines {
        v.push(count.nl);
    }
    if args.words {
        v.push(count.words);
    }
    if args.bytes || args.chars {
        v.push(count.chars);
    }
    v
}

/// Format one output row: each numeric field right-justified to `width`,
/// space-separated, optionally followed by the source name. A `None` name (the
/// no-operand standard input) prints no name.
fn format_row(args: &Args, count: &CountInfo, name: Option<&OsStr>, width: usize) -> String {
    let mut output: String = row_values(args, count)
        .iter()
        .map(|v| format!("{v:>width$}"))
        .collect::<Vec<_>>()
        .join(" ");
    if let Some(name) = name {
        output.push(' ');
        output.push_str(name.to_string_lossy().as_ref());
    }
    output
}

/// Count lines, words, and bytes-or-characters of one input.
///
/// Bytes and newlines are counted directly from the byte stream. Characters
/// (`-m`) and word boundaries (`-w`) are decoded under `LC_CTYPE` with a
/// streaming [`MbDecoder`], so multibyte characters spanning read boundaries
/// are handled and `iswspace(3)` (not an ASCII table) determines word breaks.
fn wc_count(args: &Args, chars_mode: bool, pathname: &Path) -> io::Result<CountInfo> {
    let mut file = input_stream_dashed(pathname)?;
    let mut count = CountInfo::default();

    let need_decode = args.words || chars_mode;
    let mut decoder = MbDecoder::new();
    let mut was_space = true;

    let mut buffer = [0; BUFSZ];
    loop {
        let n_read = file.read(&mut buffer[..])?;
        if n_read == 0 {
            break;
        }
        let bufslice = &buffer[0..n_read];

        if args.lines {
            count.nl += bufslice.iter().filter(|&&b| b == b'\n').count();
        }
        if chars_mode {
            // characters counted in the decode loop below
        } else if args.bytes {
            count.chars += n_read;
        }

        if need_decode {
            for ch in decoder.decode(bufslice) {
                if chars_mode {
                    count.chars += 1;
                }
                if args.words {
                    let is_space = ch.map(isspace).unwrap_or(false);
                    if !is_space && was_space {
                        count.words += 1;
                    }
                    was_space = is_space;
                }
            }
        }
    }

    // A trailing incomplete multibyte sequence at end of input counts as one
    // (non-space) character.
    if need_decode && decoder.pending() > 0 {
        if chars_mode {
            count.chars += 1;
        }
        if args.words && was_space {
            count.words += 1;
        }
    }

    Ok(count)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    let mut chars_mode = false;

    // Assign defaults, per POSIX
    if !args.bytes && !args.lines && !args.chars && !args.words {
        args.bytes = true;
        args.lines = true;
        args.words = true;
    } else if args.chars {
        args.bytes = false;
        chars_mode = true;
    }

    let mut exit_code = 0;
    let mut totals = CountInfo::default();

    // Each row carries its counts and the name to display: `None` for the
    // no-operand standard input (no name printed), `Some` for every file
    // operand (including "-", shown literally), per POSIX.
    let mut rows: Vec<(CountInfo, Option<std::ffi::OsString>)> = Vec::new();

    if args.files.is_empty() {
        match wc_count(&args, chars_mode, &PathBuf::new()) {
            Ok(count) => rows.push((count, None)),
            Err(e) => {
                exit_code = 1;
                eprintln!("wc: stdin: {}", e);
            }
        }
    } else {
        for filename in &args.files {
            match wc_count(&args, chars_mode, filename) {
                Ok(count) => {
                    totals += CountInfo {
                        words: count.words,
                        chars: count.chars,
                        nl: count.nl,
                    };
                    rows.push((count, Some(filename.as_os_str().to_owned())));
                }
                Err(e) => {
                    exit_code = 1;
                    eprintln!("wc: {}: {}", filename.display(), e);
                }
            }
        }
    }

    let multi_file = args.files.len() > 1;

    // Field width adapts to the largest value printed (across files and the
    // total), with a minimum of one digit.
    let max_value = rows
        .iter()
        .flat_map(|(c, _)| row_values(&args, c))
        .chain(if multi_file {
            row_values(&args, &totals)
        } else {
            Vec::new()
        })
        .max()
        .unwrap_or(0);
    let width = max_value.to_string().len();

    for (count, name) in &rows {
        println!("{}", format_row(&args, count, name.as_deref(), width));
    }

    if multi_file {
        println!(
            "{}",
            format_row(&args, &totals, Some(OsStr::new("total")), width)
        );
    }

    std::process::exit(exit_code)
}
