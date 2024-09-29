//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::ffi::OsStr;
use std::io::{self, Read};
use std::path::PathBuf;

/// wc - word, line, and byte or character count
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Count number of bytes in each file
    #[arg(short = 'c', long)]
    bytes: bool,

    /// Count number of lines in each file
    #[arg(short, long)]
    lines: bool,

    /// Count number of characters in each file
    #[arg(short = 'm', long)]
    chars: bool,

    /// Count number of lines in each file
    #[arg(short, long)]
    words: bool,

    /// Files to read as input.
    files: Vec<PathBuf>,
}

struct CountInfo {
    words: usize,
    chars: usize,
    nl: usize,
}

impl CountInfo {
    fn new() -> CountInfo {
        CountInfo {
            words: 0,
            chars: 0,
            nl: 0,
        }
    }

    fn accum(&mut self, count: &CountInfo) {
        self.words = self.words + count.words;
        self.chars = self.chars + count.chars;
        self.nl = self.nl + count.nl;
    }
}

const fn create_table() -> [bool; 256] {
    let mut table = [false; 256];
    table[9] = true;
    table[10] = true;
    table[11] = true;
    table[12] = true;
    table[13] = true;
    table[32] = true;
    table
}

const BYTE_TABLE: [bool; 256] = create_table();

fn build_display_str(args: &Args, count: &CountInfo, filename: &OsStr) -> String {
    let mut output = String::with_capacity(filename.len() + (3 * 10));

    let multi_file = args.files.len() > 1;
    let only_lines = (args.words == false) && (args.bytes == false) && (args.chars == false);
    let only_words = (args.lines == false) && (args.bytes == false) && (args.chars == false);
    let only_bytechars = (args.lines == false) && (args.words == false);

    if args.lines {
        let numstr = match only_lines {
            true => format!("{}", count.nl),
            false => format!("{:>8}", count.nl),
        };
        output.push_str(&numstr);
    }
    if args.words {
        if !output.is_empty() {
            output.push(' ');
        }
        let numstr = match only_words {
            true => format!("{}", count.words),
            false => format!("{:>8}", count.words),
        };
        output.push_str(&numstr);
    }
    if args.bytes || args.chars {
        if !output.is_empty() {
            output.push(' ');
        }
        let numstr = match only_bytechars {
            true => format!("{}", count.chars),
            false => format!("{:>8}", count.chars),
        };
        output.push_str(&numstr);
    }

    if multi_file {
        output.push(' ');

        if filename.is_empty() {
            output.push_str("(stdin)");
        } else {
            output.push_str(filename.to_string_lossy().as_ref());
        }
    }

    output
}

fn wc_file_bytes(count: &mut CountInfo, pathname: &PathBuf, chars_mode: bool) -> io::Result<()> {
    let mut file = plib::io::input_stream(pathname, false)?;

    let mut buffer = [0; plib::BUFSZ];
    let mut was_space = true;

    loop {
        let n_read = file.read(&mut buffer[..])?;
        if n_read == 0 {
            break;
        }

        let bufslice = &buffer[0..n_read];

        if !chars_mode {
            // number of bytes read
            count.chars += n_read;
        } else {
            // number of UTF-8 unicode codepoints in this slice of bytes
            count.chars += bufslice.iter().filter(|&ch| (ch >> 6) != 0b10).count();
        }

        for ch_u8 in bufslice {
            let is_space = BYTE_TABLE[*ch_u8 as usize];
            count.nl += (ch_u8 == &10) as usize;
            count.words += (!is_space && was_space) as usize;
            was_space = is_space;
        }
    }

    Ok(())
}

fn wc_file(
    args: &Args,
    chars_mode: bool,
    pathname: &PathBuf,
    count: &mut CountInfo,
) -> io::Result<()> {
    wc_file_bytes(count, pathname, chars_mode)?;

    let output = build_display_str(args, count, pathname.as_os_str());

    println!("{}", output);

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
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

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;
    let mut totals = CountInfo::new();

    // input via stdin
    if args.files.is_empty() {
        let mut count = CountInfo::new();

        if let Err(e) = wc_file(&args, chars_mode, &PathBuf::new(), &mut count) {
            exit_code = 1;
            eprintln!("stdin: {}", e);
        }

    // input files
    } else {
        for filename in &args.files {
            let mut count = CountInfo::new();

            if let Err(e) = wc_file(&args, chars_mode, filename, &mut count) {
                exit_code = 1;
                eprintln!("{}: {}", filename.display(), e);
            }

            totals.accum(&count);
        }
    }

    if args.files.len() > 1 {
        let output = build_display_str(&args, &totals, OsStr::new("total"));
        println!("{}", output);
    }

    std::process::exit(exit_code)
}
