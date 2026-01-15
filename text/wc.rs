//
// Copyright (c) 2024 Jeff Garzik
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
use plib::io::input_stream;
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

/// is_space
const fn create_table() -> [bool; 256] {
    let mut table = [false; 256];
    table[b'\t' as usize] = true;
    table[b'\n' as usize] = true;
    table[11 /* \v */] = true;
    table[12 /* \f */] = true;
    table['\r' as usize] = true;
    table[' ' as usize] = true;
    table
}

const BYTE_TABLE: [bool; 256] = create_table();

fn build_display_str(args: &Args, count: &CountInfo, filename: &OsStr) -> String {
    let mut output = String::with_capacity(filename.len() + (3 * 10));

    if args.lines {
        let only_lines = !args.words && !args.bytes && !args.chars;
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
        let only_words = !args.lines && !args.bytes && !args.chars;
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
        let only_bytechars = !args.lines && !args.words;
        let numstr = match only_bytechars {
            true => format!("{}", count.chars),
            false => format!("{:>8}", count.chars),
        };
        output.push_str(&numstr);
    }

    let multi_file = args.files.len() > 1;
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

fn wc_file_bytes(count: &mut CountInfo, pathname: &Path, chars_mode: bool) -> io::Result<()> {
    let mut file = input_stream(pathname, false)?;

    let mut buffer = [0; BUFSZ];
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
    pathname: &Path,
    count: &mut CountInfo,
) -> io::Result<()> {
    wc_file_bytes(count, pathname, chars_mode)?;

    let output = build_display_str(args, count, pathname.as_os_str());

    println!("{}", output);

    Ok(())
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

    // input via stdin
    if args.files.is_empty() {
        let mut count = CountInfo::default();

        if let Err(e) = wc_file(&args, chars_mode, &PathBuf::new(), &mut count) {
            exit_code = 1;
            eprintln!("stdin: {}", e);
        }

    // input files
    } else {
        for filename in &args.files {
            let mut count = CountInfo::default();

            if let Err(e) = wc_file(&args, chars_mode, filename, &mut count) {
                exit_code = 1;
                eprintln!("{}: {}", filename.display(), e);
            }

            totals += count;
        }
    }

    if args.files.len() > 1 {
        let output = build_display_str(&args, &totals, OsStr::new("total"));
        println!("{}", output);
    }

    std::process::exit(exit_code)
}
