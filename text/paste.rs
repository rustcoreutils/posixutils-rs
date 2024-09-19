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
// -- Probably fixed
// - fix:  empty-string delimiters \0
// -- Probably fixed
// - improve:  don't open all files at once in --serial mode
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::error::Error;
use std::fs;
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
    rdr: Box<dyn BufRead>,
    eof: bool,
    last: bool,
}

struct PasteInfo {
    pub inputs: Vec<PasteFile>,
}

struct DelimInfo {
    cur_delim: usize,
    delims_len: usize,
    delims: Box<[char]>,
}

impl DelimInfo {
    fn new(delims: Box<[char]>) -> DelimInfo {
        let delims_len = delims.len();

        DelimInfo {
            cur_delim: 0,
            delims,
            delims_len,
        }
    }

    fn delim(&mut self) -> Option<char> {
        let delims_len = self.delims_len;

        if delims_len == 0 {
            None
        } else {
            let cur_delim = self.cur_delim;

            // Non-empty, so unwrap
            let ch = self.delims.get(cur_delim).unwrap().to_owned();

            // Advance function
            {
                if delims_len > 1 {
                    let cur_delim_plus_one = cur_delim + 1;

                    let new_cur_delim = if cur_delim_plus_one >= delims_len {
                        0
                    } else {
                        cur_delim_plus_one
                    };

                    self.cur_delim = new_cur_delim;
                }
            }

            Some(ch)
        }
    }
}

fn xlat_delim_str(s: &str) -> Box<[char]> {
    // Plus 10?
    let mut output = Vec::<char>::with_capacity(s.len() + 10);

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

    output.into_boxed_slice()
}

fn open_inputs(args: &Args) -> Result<PasteInfo, Box<dyn Error>> {
    let files = &args.files;

    let mut vec = Vec::with_capacity(files.len());

    // open each input
    for filename in files {
        // POSIX says only to read from stdin if "-" is passed as a file. Most implementations
        // automatically read from stdin if no files are passed to `paste`.
        // https://pubs.opengroup.org/onlinepubs/9799919799/utilities/paste.html
        match filename.as_str() {
            "-" => vec.push(PasteFile {
                filename: format!("Pipe: standard input (opened as '{filename}'"),
                eof: false,
                last: false,
                rdr: Box::new(io::stdin().lock()),
            }),
            "" => {
                eprintln!("paste: FILE is an empty string, skipping");
            }
            _ => {
                let f_res = fs::File::open(filename);

                match f_res {
                    Err(er) => {
                        eprintln!("{filename}: {er}");

                        return Err(er.into());
                    }
                    Ok(f) => {
                        vec.push(PasteFile {
                            filename: format!("File: {filename}"),
                            rdr: Box::new(BufReader::new(f)),
                            eof: false,
                            last: false,
                        });
                    }
                }
            }
        }
    }

    if vec.is_empty() {
        eprintln!(
            "paste: No valid [FILES] were specified. Use '-' if you are trying to read from stdin."
        );

        return Err(Box::<_>::from("Execution failed"));
    }

    // mark final input
    if let Some(pa) = vec.last_mut() {
        pa.last = true;
    }

    Ok(PasteInfo { inputs: vec })
}

fn paste_files_serial(mut info: PasteInfo, mut delim_info: DelimInfo) -> io::Result<()> {
    // loop serially for each input file
    for input in &mut info.inputs {
        let mut first_line = true;

        // for each input line
        loop {
            // read line
            let mut buffer = String::new();

            let n_read = match input.rdr.read_line(&mut buffer) {
                Ok(us) => us,
                Err(er) => {
                    eprintln!("{}: {}", input.filename, er);

                    return Err(er);
                }
            };

            // if EOF, output line terminator and end inner loop
            if n_read == 0 {
                println!();
                break;

            // output line segment
            } else {
                let mut chars = buffer.chars();

                // TODO
                // Check that the removed character is a newline?
                let _: Option<char> = chars.next_back();

                let slice = chars.as_str();

                if first_line {
                    print!("{slice}");
                } else {
                    let delimiter = match delim_info.delim() {
                        Some(ch) => ch.to_string(),
                        None => String::new(),
                    };

                    print!("{delimiter}{slice}");
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

                let n_read = match input.rdr.read_line(&mut buffer) {
                    Ok(us) => us,
                    Err(er) => {
                        eprintln!("{}: {}", input.filename, er);

                        return Err(er);
                    }
                };

                // if at EOF, note and continue
                if n_read == 0 {
                    input.eof = true;

                // otherwise add to output line, sans trailing NL
                } else {
                    have_data = true;

                    let mut chars = buffer.chars();

                    // TODO
                    // Check that the removed character is a newline
                    let _: Option<char> = chars.next_back();

                    output.push_str(chars.as_str());
                }
            }

            // final record, output line end
            if input.last {
                output.push('\n');
            } else {
                // next delimiter
                #[allow(clippy::collapsible_else_if)]
                if let Some(ch) = dinfo.delim() {
                    output.push(ch);
                }
            }
        }

        if !have_data {
            break;
        }

        // output all segments to stdout at once (one write per line)
        match io::stdout().write_all(output.as_bytes()) {
            Ok(()) => {}
            Err(e) => {
                eprintln!("stdout: {e}");
                return Err(e);
            }
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let paste_info = open_inputs(&args)?;

    let delim_state = match args.delims {
        None => {
            // Default when no delimiter argument is provided
            DelimInfo::new(Box::new(['\t']))
        }
        Some(st) => {
            // Delimiters parsed from "-d"/"--delims" argument
            //
            // Support for empty delimiter list:
            //
            // bsdutils: no, supports "-d" but requires the delimiter list to be non-empty
            // Busybox: does not support "-d" at all
            // GNU Core Utilities: yes
            // toybox: yes
            // uutils's coreutils: no, supports "-d", but panics on an empty delimiter list
            //
            // POSIX seems to almost forbid this:
            // "These elements specify one or more delimiters to use, instead of the default <tab>, to replace the <newline> of the input lines."
            // https://pubs.opengroup.org/onlinepubs/9799919799/utilities/paste.html
            DelimInfo::new(xlat_delim_str(&st))
        }
    };

    if args.serial {
        paste_files_serial(paste_info, delim_state)?;
    } else {
        paste_files(paste_info, delim_state)?;
    }

    Ok(())
}
