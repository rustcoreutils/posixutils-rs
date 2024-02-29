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
use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, Write};

/// paste - merge corresponding or subsequent lines of files
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// One or more input files.
    files: Vec<String>,
}

struct PasteFile {
    filename: String,
    rdr: BufReader<File>,
    eof: bool,
    last: bool,
}

struct PasteInfo {
    delim: char,
    inputs: Vec<PasteFile>,
}

impl PasteInfo {
    fn new() -> PasteInfo {
        PasteInfo {
            delim: '\t',
            inputs: Vec::new(),
        }
    }
}

fn open_inputs(args: &Args, info: &mut PasteInfo) -> io::Result<()> {
    for filename in &args.files {
        let f_res = fs::File::open(filename);

        match f_res {
            Err(e) => {
                eprintln!("{}: {}", filename, e);
                return Err(e);
            }
            Ok(f) => {
                info.inputs.push(PasteFile {
                    filename: filename.to_string(),
                    rdr: BufReader::new(f),
                    eof: false,
                    last: false,
                });
            }
        }
    }

    let idx = info.inputs.len() - 1;
    info.inputs[idx].last = true;

    Ok(())
}

fn paste_files(info: &mut PasteInfo) -> io::Result<()> {
    loop {
        let mut output = String::new();
        let mut have_data = false;
        for input in &mut info.inputs {
            if !input.eof {
                let mut buffer = String::new();
                let n_read_res = input.rdr.read_line(&mut buffer);
                if let Err(e) = n_read_res {
                    eprintln!("{}: {}", input.filename, e);
                    return Err(e);
                }
                let n_read = n_read_res.unwrap();
                if n_read == 0 {
                    input.eof = true;
                } else {
                    have_data = true;
                    output.push_str(&buffer[0..buffer.len() - 1]);
                }
            }

            if input.last {
                output.push('\n');
            } else {
                output.push(info.delim);
            }
        }

        if !have_data {
            break;
        }

        match io::stdout().write_all(output.as_bytes()) {
            Ok(()) => {}
            Err(e) => {
                eprintln!("stdout: {}", e);
                return Err(e);
            }
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut state = PasteInfo::new();

    open_inputs(&args, &mut state)?;
    paste_files(&mut state)?;

    Ok(())
}
