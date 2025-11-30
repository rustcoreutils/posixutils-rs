//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, BufRead};
use std::path::PathBuf;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::io::input_reader;

/// asa - interpret carriage-control characters
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Files to read as input.
    files: Vec<PathBuf>,
}

fn asa_file(pathname: &PathBuf) -> io::Result<()> {
    let mut reader = input_reader(pathname, false)?;
    let mut first_line = true;
    let mut had_output = false;

    loop {
        let mut raw_line = String::new();
        let n_read = reader.read_line(&mut raw_line)?;
        if n_read == 0 {
            break;
        }

        // Get first character as control character
        let ch = match raw_line.chars().next() {
            Some(c) => c,
            None => continue, // empty line shouldn't happen, but handle gracefully
        };

        // Extract line content: skip first char, exclude trailing newline
        let line_end = if raw_line.ends_with('\n') {
            raw_line.len() - 1
        } else {
            raw_line.len()
        };
        let line = if line_end > 1 {
            &raw_line[1..line_end]
        } else {
            "" // control char only, no content
        };

        // POSIX: '+' as first character in input is equivalent to space
        let effective_ch = if first_line && ch == '+' { ' ' } else { ch };

        match effective_ch {
            '+' => {
                // Overprint: return to column 1 of current line
                print!("\r{}", line);
            }
            '0' => {
                // Double-space: newline before content (blank line)
                if !first_line {
                    println!();
                }
                println!();
                print!("{}", line);
            }
            '-' => {
                // Triple-space (non-POSIX extension): two blank lines before
                if !first_line {
                    println!();
                }
                println!();
                println!();
                print!("{}", line);
            }
            '1' => {
                // New page: form-feed
                if !first_line {
                    println!();
                }
                print!("\x0c{}", line);
            }
            _ => {
                // Space and other chars: normal single-spaced output
                if !first_line {
                    println!();
                }
                print!("{}", line);
            }
        };

        first_line = false;
        had_output = true;
    }

    // Final newline if we had any output
    if had_output {
        println!();
    }

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
        if let Err(e) = asa_file(filename) {
            exit_code = 1;
            eprintln!("{}: {}", filename.display(), e);
        }
    }

    std::process::exit(exit_code)
}
