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

/// expand - convert tabs to spaces
#[derive(Parser)]
#[command(version, about = gettext("expand - convert tabs to spaces"))]
struct Args {
    // `--tablist` is a non-POSIX long-option alias kept for convenience; POSIX
    // specifies only `-t tablist`.
    #[arg(short, long, help = gettext("Tab stops, either a single positive decimal integer or a list of tabstops separated by commas."))]
    tablist: Option<String>,

    #[arg(help = gettext("Files to read as input."))]
    files: Vec<PathBuf>,
}

enum TabList {
    /// Uniform tab width: stops every N columns.
    UniStop(usize),
    /// Explicit 1-based stop positions, ascending.
    Stops(Vec<usize>),
}

fn parse_tablist(tablist: &str) -> Result<TabList, &'static str> {
    // A single integer sets a uniform tab width. It must be positive: a width
    // of zero is rejected rather than panicking on a modulo-by-zero.
    if let Ok(tab) = tablist.parse::<usize>() {
        if tab == 0 {
            return Err("tab size must be a positive integer");
        }
        return Ok(TabList::UniStop(tab));
    }

    let mut v: Vec<usize> = Vec::new();
    for token in tablist.split([' ', ',']) {
        let n = token
            .parse::<usize>()
            .map_err(|_| "invalid tab stop in list")?;
        // Each tab stop must be a positive integer (POSIX).
        if n == 0 {
            return Err("tab stop must be a positive integer");
        }
        if let Some(&last) = v.last() {
            if n <= last {
                return Err("tab stops must be in strictly ascending order");
            }
        }
        v.push(n);
    }

    Ok(TabList::Stops(v))
}

/// Next tab stop strictly greater than the current 0-based column `p`.
fn next_stop(tablist: &TabList, p: usize) -> usize {
    match tablist {
        TabList::UniStop(n) => ((p / n) + 1) * n,
        TabList::Stops(list) => {
            // `list` holds 1-based stop positions; column position P (1-based)
            // is 0-based index P-1.
            for &s in list {
                let stop0 = s - 1;
                if stop0 > p {
                    return stop0;
                }
            }
            // Past the last explicit stop, each column is its own tab stop.
            p + 1
        }
    }
}

fn expand_file(tablist: &TabList, pathname: &Path) -> io::Result<()> {
    // open file, or stdin ("-" or no operand)
    let mut file = input_stream_dashed(pathname)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    let mut writer = BufWriter::new(io::stdout());
    // 0-based column = number of column positions consumed on the current line.
    let mut p: usize = 0;

    for ch in mb_char_slices(&data) {
        match ch {
            b"\t" => {
                let stop = next_stop(tablist, p);
                for _ in p..stop {
                    writer.write_all(b" ")?;
                }
                p = stop;
            }
            b"\x08" => {
                // backspace: column count decrements, never below zero
                writer.write_all(ch)?;
                p = p.saturating_sub(1);
            }
            b"\r" | b"\n" => {
                writer.write_all(ch)?;
                p = 0;
            }
            _ => {
                writer.write_all(ch)?;
                // Advance by the character's display width under LC_CTYPE.
                // Non-printable / undecodable bytes do not advance the column.
                let w = std::str::from_utf8(ch)
                    .ok()
                    .and_then(|s| s.chars().next())
                    .map(wcwidth_char)
                    .unwrap_or(1);
                if w > 0 {
                    p += w as usize;
                }
            }
        }
    }

    writer.flush()?;

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    let tablist = {
        if let Some(ref tablist) = args.tablist {
            match parse_tablist(tablist) {
                Ok(tl) => tl,
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(1);
                }
            }
        } else {
            TabList::UniStop(8)
        }
    };

    // if no files, read from stdin
    if args.files.is_empty() {
        args.files.push(PathBuf::new());
    }

    let mut exit_code = 0;

    for filename in &args.files {
        if let Err(e) = expand_file(&tablist, filename) {
            exit_code = 1;
            eprintln!("{}: {}", filename.display(), e);
        }
    }

    std::process::exit(exit_code)
}
