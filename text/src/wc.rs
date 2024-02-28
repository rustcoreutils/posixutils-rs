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
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::fs;
use std::io::{self, BufRead, Read};

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
    files: Vec<String>,
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

fn build_display_str(args: &Args, count: &CountInfo, filename: &str) -> String {
    let mut output = String::new();
    output.reserve(filename.len() + (3 * 10));

    if args.lines {
        let numstr = format!("{:>8}", count.nl);
        output.push_str(&numstr);
    }
    if args.words {
        if output.len() > 0 {
            output.push(' ');
        }
        let numstr = format!("{:>8}", count.words);
        output.push_str(&numstr);
    }
    if args.bytes || args.chars {
        if output.len() > 0 {
            output.push(' ');
        }
        let numstr = format!("{:>8}", count.chars);
        output.push_str(&numstr);
    }

    output.push(' ');

    if filename == "" {
        output.push_str("(stdin)");
    } else {
        output.push_str(filename);
    }

    output
}

fn wc_file_bytes(count: &mut CountInfo, filename: &str) -> io::Result<()> {
    let mut file: Box<dyn Read>;
    if filename == "" {
        file = Box::new(io::stdin().lock());
    } else {
        file = Box::new(fs::File::open(filename)?);
    }

    let mut buffer = [0; 4096];
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

fn wc_file_chars(args: &Args, count: &mut CountInfo, filename: &str) -> io::Result<()> {
    let file: Box<dyn Read>;
    if filename == "" {
        file = Box::new(io::stdin().lock());
    } else {
        file = Box::new(fs::File::open(filename)?);
    }

    let mut reader = io::BufReader::new(file);

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

fn wc_file(args: &Args, chars_mode: bool, filename: &str, count: &mut CountInfo) -> io::Result<()> {
    if chars_mode {
        wc_file_chars(args, count, filename)?;
    } else {
        wc_file_bytes(count, filename)?;
    }

    let output = build_display_str(&args, count, filename);

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

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;
    let mut totals = CountInfo::new();

    // input via stdin
    if args.files.is_empty() {
        let mut count = CountInfo::new();

        match wc_file(&args, chars_mode, "", &mut count) {
            Ok(()) => {}
            Err(e) => {
                exit_code = 1;
                eprintln!("stdin: {}", e);
            }
        }

    // input files
    } else {
        for filename in &args.files {
            let mut count = CountInfo::new();

            match wc_file(&args, chars_mode, filename, &mut count) {
                Ok(()) => {}
                Err(e) => {
                    exit_code = 1;
                    eprintln!("{}: {}", filename, e);
                }
            }

            totals.accum(&count);
        }
    }

    if args.files.len() > 1 {
        let output = build_display_str(&args, &totals, "total");
        println!("{}", output);
    }

    std::process::exit(exit_code)
}
