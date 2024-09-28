//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - fix correctness
// - add tests
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::io::{self, BufRead};
use std::path::PathBuf;

/// asa - interpret carriage-control characters
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Files to read as input.
    files: Vec<PathBuf>,
}

struct AsaState {
    first_line: bool,
    lines: Vec<String>,
}

impl AsaState {
    fn new() -> AsaState {
        AsaState {
            first_line: true,
            lines: Vec::new(),
        }
    }

    fn push(&mut self, line: &str) {
        self.lines.push(line.to_string());
        if self.first_line {
            self.first_line = false;
        }
    }

    fn formfeed(&mut self) {
        if !self.first_line {
            print!("\x0c"); // formfeed
        }
    }

    fn flush(&mut self) {
        let mut nl = String::new();
        for line in &self.lines {
            print!("{}{}", nl, line);

            // do not prefix with newline on first line
            if nl.is_empty() {
                nl = "\n".to_string();
            }
        }

        self.lines.clear();
    }
}

fn asa_file(pathname: &PathBuf) -> io::Result<()> {
    let mut reader = plib::io::input_reader(pathname, false)?;
    let mut line_no: usize = 0;
    let mut state = AsaState::new();

    loop {
        line_no += 1;

        let mut raw_line = String::new();
        let n_read = reader.read_line(&mut raw_line)?;
        if n_read == 0 {
            break;
        }

        if raw_line.len() < 2 {
            eprintln!("{} {}", gettext("malformed line"), line_no);
            continue;
        }

        let ch = raw_line.chars().next().unwrap();

        // exclude first char, and trailing newline
        let mut line_len = raw_line.len() - 1;
        if raw_line.ends_with('\n') {
            line_len -= 1;
        }
        let line = &raw_line[1..line_len];

        match ch {
            '+' => {
                state.push(line);
            }
            '0' => {
                state.flush();
                println!();
                state.push(line);
            }
            '-' => {
                state.flush();
                println!();
                println!();
                state.push(line);
            }
            '1' => {
                state.flush();
                state.formfeed();
                state.push(line);
            }
            _ => {
                state.flush();
                state.push(line);
            }
        };
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let mut args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME"))?;
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8")?;

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
