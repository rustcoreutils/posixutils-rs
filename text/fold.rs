//
// Copyright (c) 2024 Hemi Labs, Inc.
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
use std::io::{self, Read, Write};
use std::path::PathBuf;

const TABSTOP: usize = 8;

/// fold - filter for folding lines
#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about)]
struct Args {
    /// Count width in bytes rather than column positions.
    #[arg(short, long)]
    bytes: bool,

    /// Break on spaces.
    #[arg(short, long)]
    spaces: bool,

    /// Specify the maximum line length, in column positions (or bytes if -b is specified).
    #[arg(short, long, default_value_t = 80, value_parser = clap::value_parser!(u64).range(1..))]
    width: u64,

    /// Files to read as input.
    files: Vec<PathBuf>,
}

struct OutputState {
    args: Args,
    column: usize,
    data: Vec<u8>,
}

impl OutputState {
    fn new(args: &Args) -> OutputState {
        OutputState {
            args: args.clone(),
            column: 0,
            data: Vec::new(),
        }
    }

    fn push(&mut self, byte: u8) {
        self.data.push(byte);
    }

    fn incr_column(&mut self, ch: char) {
        if self.args.bytes {
            self.column += 1;
        } else {
            match ch {
                '\x08' => {
                    if self.column > 0 {
                        self.column -= 1;
                    }
                }
                '\t' => {
                    self.column += TABSTOP - (self.column % TABSTOP);
                }
                '\r' => {
                    self.column = 0;
                }
                _ => {
                    self.column += 1;
                }
            }
        }
    }

    fn write_line(&mut self) -> io::Result<()> {
        io::stdout().write_all(&self.data)?;

        self.column = 0;
        self.data.clear();

        Ok(())
    }
}

fn find_last_blank(v: &[u8]) -> Option<usize> {
    for (pos, chv) in v.iter().rev().enumerate() {
        let ch = *chv as char;
        if ch.is_whitespace() {
            return Some(pos);
        }
    }

    None
}

fn fold_file(args: &Args, pathname: &PathBuf) -> io::Result<()> {
    // open file, or stdin
    let mut file = plib::io::input_stream(pathname, false)?;

    let mut raw_buffer = [0; plib::BUFSZ];
    let mut state = OutputState::new(args);

    loop {
        // read a chunk of file data
        let n_read = file.read(&mut raw_buffer[..])?;
        if n_read == 0 {
            break;
        }

        // slice of buffer containing file data
        let buf = &raw_buffer[0..n_read];

        // loop for each character in buffer, which may include partial lines
        for chv in buf {
            let ch = *chv as char;

            if ch == '\n' {
                state.push(*chv);
                state.write_line()?;
                continue;
            }

            loop {
                state.incr_column(ch);

                if state.column <= args.width as usize {
                    state.push(*chv);
                    break;
                }

                if args.spaces {
                    if let Some(blankpos) = find_last_blank(&state.data) {
                        let mut spill: Vec<u8> = Vec::new();
                        let rhs = &state.data[blankpos + 1..];
                        spill.extend_from_slice(rhs);
                        state.data.truncate(blankpos + 1);
                        state.push(b'\n');
                        state.write_line()?;
                        for dchv in &spill {
                            let dch = *dchv as char;
                            state.incr_column(dch);
                        }
                        state.data = spill;
                        continue;
                    }
                }

                if state.data.is_empty() {
                    state.push(*chv);
                    break;
                }

                state.push(b'\n');
                state.write_line()?;
            }
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let mut args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

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
