//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - stdin ("-")
// - fix:  empty-string delimiters \0
// - improve:  don't open all files at once in --serial mode
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
    /// Concatenate all of the lines from each input file into one line of output per file, in command line order.
    #[arg(short, long)]
    serial: bool,

    /// Delimiter list
    #[arg(short, long)]
    delims: Option<String>,

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
    inputs: Vec<PasteFile>,
}

impl PasteInfo {
    fn new() -> PasteInfo {
        PasteInfo { inputs: Vec::new() }
    }
}

struct DelimInfo {
    cur_delim: usize,
    delims: String,
}

impl DelimInfo {
    fn new() -> DelimInfo {
        DelimInfo {
            cur_delim: 0,
            delims: String::from("\t"),
        }
    }

    fn delim(&mut self) -> char {
        let ch = self.delims.chars().nth(self.cur_delim).unwrap();

        self.advance();

        ch
    }

    fn advance(&mut self) {
        if self.delims.len() > 1 {
            self.cur_delim = self.cur_delim + 1;
            if self.cur_delim >= self.delims.len() {
                self.cur_delim = 0;
            }
        }
    }
}

fn xlat_delim_str(s: &str) -> String {
    let mut output = String::with_capacity(s.len() + 10);

    let mut in_escape = false;
    for ch in s.chars() {
        if in_escape {
            let out_ch = match ch {
                'n' => '\n',
                't' => '\t',
                '0' => '\0',
                _ => ch,
            };

            output.push(out_ch);
            in_escape = false;
        } else if ch == '\\' {
            in_escape = true;
        } else {
            output.push(ch);
        }
    }

    output
}

fn open_inputs(args: &Args, info: &mut PasteInfo) -> io::Result<()> {
    // open each input
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

    // mark final input
    let idx = info.inputs.len() - 1;
    info.inputs[idx].last = true;

    Ok(())
}

fn paste_files_serial(mut info: PasteInfo, mut dinfo: DelimInfo) -> io::Result<()> {
    // loop serially for each input file
    for input in &mut info.inputs {
        let mut first_line = true;

        // for each input line
        loop {
            // read line
            let mut buffer = String::new();
            let n_read_res = input.rdr.read_line(&mut buffer);
            if let Err(e) = n_read_res {
                eprintln!("{}: {}", input.filename, e);
                return Err(e);
            }
            let n_read = n_read_res.unwrap();

            // if EOF, output line terminator and end inner loop
            if n_read == 0 {
                println!("");
                break;

            // output line segment
            } else {
                let slice = &buffer[0..buffer.len() - 1];

                if first_line {
                    print!("{}", slice);
                } else {
                    print!("{}{}", dinfo.delim(), slice);
                }
            }

            if first_line {
                first_line = false;
            }
        }
    }

    Ok(())
}

fn paste_files(mut info: PasteInfo, mut dinfo: DelimInfo) -> io::Result<()> {
    // for each input line, across N files
    loop {
        let mut output = String::new();
        let mut have_data = false;

        // for each input line
        for input in &mut info.inputs {
            // if not already at EOF, read and process a line
            if !input.eof {
                // read input line
                let mut buffer = String::new();
                let n_read_res = input.rdr.read_line(&mut buffer);
                if let Err(e) = n_read_res {
                    eprintln!("{}: {}", input.filename, e);
                    return Err(e);
                }
                let n_read = n_read_res.unwrap();

                // if at EOF, note and continue
                if n_read == 0 {
                    input.eof = true;

                // otherwise add to output line, sans trailing NL
                } else {
                    have_data = true;
                    output.push_str(&buffer[0..buffer.len() - 1]);
                }
            }

            // final record, output line end
            if input.last {
                output.push('\n');

            // next delimiter
            } else {
                output.push(dinfo.delim());
            }
        }

        if !have_data {
            break;
        }

        // output all segments to stdout at once (one write per line)
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
    let mut delim_state = DelimInfo::new();
    match &args.delims {
        None => {}
        Some(dlm) => {
            delim_state.delims = xlat_delim_str(dlm);
        }
    }

    open_inputs(&args, &mut state)?;

    if args.serial {
        paste_files_serial(state, delim_state)?;
    } else {
        paste_files(state, delim_state)?;
    }

    Ok(())
}
