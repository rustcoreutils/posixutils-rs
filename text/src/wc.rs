//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::ffi::OsStr;
use std::io::{self, BufRead, Read};
use std::path::PathBuf;

/// wc - word, line, and byte or character count
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
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
        if output.len() > 0 {
            output.push(' ');
        }
        let numstr = match only_words {
            true => format!("{}", count.words),
            false => format!("{:>8}", count.words),
        };
        output.push_str(&numstr);
    }
    if args.bytes || args.chars {
        if output.len() > 0 {
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

        if filename == "" {
            output.push_str("(stdin)");
        } else {
            output.push_str(filename.to_string_lossy().as_ref());
        }
    }

    output
}

fn wc_file_bytes(count: &mut CountInfo, pathname: &PathBuf) -> io::Result<()> {
    let mut file = plib::io::input_stream(pathname, false)?;

    let mut buffer = [0; plib::BUFSZ];
    let mut in_word = false;

    loop {
        let n_read = file.read(&mut buffer[..])?;
        if n_read == 0 {
            break;
        }

        count.chars = count.chars + n_read;

        let bufslice = &buffer[0..n_read];

        for ch_u8 in bufslice {
            let ch = *ch_u8 as char;

            if ch == '\n' {
                count.nl = count.nl + 1;
                if in_word {
                    in_word = false;
                    count.words = count.words + 1;
                }
            } else if ch.is_whitespace() {
                if in_word {
                    in_word = false;
                    count.words = count.words + 1;
                }
            } else {
                if !in_word {
                    in_word = true;
                }
            }
        }
    }

    if in_word {
        count.words = count.words + 1;
    }

    Ok(())
}

fn wc_file_chars(args: &Args, count: &mut CountInfo, pathname: &PathBuf) -> io::Result<()> {
    let mut reader = plib::io::input_reader(pathname, false)?;

    loop {
        let mut buffer = String::new();
        let n_read = reader.read_line(&mut buffer)?;
        if n_read == 0 {
            break;
        }

        count.nl = count.nl + 1;
        count.chars = count.chars + n_read;

        if args.words {
            let mut in_word = false;

            for ch in buffer.chars() {
                if ch.is_whitespace() {
                    if in_word {
                        in_word = false;
                        count.words = count.words + 1;
                    }
                } else {
                    if !in_word {
                        in_word = true;
                    }
                }
            }
            if in_word {
                count.words = count.words + 1;
            }
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
    if chars_mode {
        wc_file_chars(args, count, pathname)?;
    } else {
        wc_file_bytes(count, pathname)?;
    }

    let output = build_display_str(&args, count, pathname.as_os_str());

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
        let output = build_display_str(&args, &totals, &OsStr::new("total"));
        println!("{}", output);
    }

    std::process::exit(exit_code)
}
