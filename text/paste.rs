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
use std::cell::Cell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::error::Error;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::rc::Rc;

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
    rdr: Rc<Cell<Option<Box<dyn BufRead>>>>,
    eof: bool,
    last: bool,
}

impl PasteFile {
    fn new(filename: String, rdr: Rc<Cell<Option<Box<dyn BufRead>>>>) -> PasteFile {
        PasteFile {
            filename,
            rdr,
            eof: false,
            last: false,
        }
    }
}

struct PasteInfo {
    pub inputs: Vec<PasteFile>,
}

// TODO
// Use an iterator instead of an index
enum DelimInfo {
    NoDelimiters,
    SingleDelimiter(char),
    MultipleDelimiters {
        cur_delim: usize,
        delims: Box<[char]>,
        number_of_delims: usize,
    },
}

impl DelimInfo {
    fn new(delims: Box<[char]>) -> DelimInfo {
        let mut iter = delims.iter();

        let Some(first_delimiter) = iter.next() else {
            // Otherwise there are no delimiters
            return Self::NoDelimiters;
        };

        let Some(_) = iter.next() else {
            // There is only one delimiter
            return Self::SingleDelimiter(first_delimiter.to_owned());
        };

        // Otherwise there are more than one delimiters
        let number_of_delims = delims.len();

        Self::MultipleDelimiters {
            delims,
            cur_delim: 0,
            number_of_delims,
        }
    }

    fn delim(&mut self) -> Option<char> {
        match *self {
            DelimInfo::NoDelimiters => None,
            DelimInfo::SingleDelimiter(ch) => Some(ch.to_owned()),
            DelimInfo::MultipleDelimiters {
                ref mut cur_delim,
                ref delims,
                number_of_delims,
            } => {
                let cur_delim_to_owned = cur_delim.to_owned();

                // Advance function
                {
                    let cur_delim_to_owned_plus_one = cur_delim_to_owned + 1;

                    let new_cur_delim = if cur_delim_to_owned_plus_one >= number_of_delims {
                        0
                    } else {
                        cur_delim_to_owned_plus_one
                    };

                    *cur_delim = new_cur_delim;
                }

                // Unwrap because indexing here should never fail
                Some(delims.get(cur_delim_to_owned).unwrap().to_owned())
            }
        }
    }

    fn reset(&mut self) {
        match *self {
            DelimInfo::MultipleDelimiters {
                ref mut cur_delim, ..
            } => {
                *cur_delim = 0;
            }
            _ => {
                // Nothing to do for these cases
            }
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

fn open_inputs(files: Vec<String>) -> Result<PasteInfo, Box<dyn Error>> {
    let files_len = files.len();

    let mut inputs = Vec::<PasteFile>::with_capacity(files_len);

    let mut hash_map =
        HashMap::<String, Rc<Cell<Option<Box<dyn BufRead>>>>>::with_capacity(files_len);

    // open each input
    for file in files {
        // POSIX says only to read from stdin if "-" is passed as a file. Most implementations
        // automatically read from stdin if no files are passed to `paste`.
        // https://pubs.opengroup.org/onlinepubs/9799919799/utilities/paste.html
        match file.as_str() {
            "-" => {
                let filename = format!("Pipe: standard input (opened as '{file}')");

                // TODO
                // Duplication
                let en = hash_map.entry(file);

                let rdr = match en {
                    Entry::Occupied(oc) => oc.get().clone(),
                    Entry::Vacant(va) => {
                        let box_buf_read: Box<dyn BufRead> = Box::new(io::stdin().lock());

                        let rc = Rc::new(Cell::new(Some(box_buf_read)));

                        let rc_clone = rc.clone();

                        va.insert(rc_clone);

                        rc
                    }
                };

                inputs.push(PasteFile::new(filename, rdr));
            }
            "" => {
                eprintln!("paste: FILE is an empty string, skipping");
            }
            _ => {
                let filename = format!("File: {file}");

                // TODO
                // Duplication
                let en = hash_map.entry(file);

                let rdr = match en {
                    Entry::Occupied(oc) => oc.get().clone(),
                    Entry::Vacant(va) => {
                        let key = va.key();

                        let open_result = File::open(key);

                        let file = match open_result {
                            Err(er) => {
                                eprintln!("{key}: {er}");

                                return Err(Box::new(er));
                            }
                            Ok(fi) => fi,
                        };

                        let box_buf_read: Box<dyn BufRead> = Box::new(BufReader::new(file));

                        let rc = Rc::new(Cell::new(Some(box_buf_read)));

                        let rc_clone = rc.clone();

                        va.insert(rc_clone);

                        rc
                    }
                };

                inputs.push(PasteFile::new(filename, rdr));
            }
        }
    }

    if inputs.is_empty() {
        eprintln!(
            "paste: No valid [FILES] were specified. Use '-' if you are trying to read from stdin."
        );

        return Err(Box::from("Execution failed"));
    }

    // mark final input
    if let Some(pa) = inputs.last_mut() {
        pa.last = true;
    }

    Ok(PasteInfo { inputs })
}

fn paste_files_serial(
    mut paste_info: PasteInfo,
    mut delim_info: DelimInfo,
) -> Result<(), Box<dyn Error>> {
    // loop serially for each input file
    for input in &mut paste_info.inputs {
        let mut first_line = true;

        // for each input line
        loop {
            // read line
            let mut buffer = String::new();

            let rc = &input.rdr;

            // TODO
            // unwrap
            // Take the `BufRead` out to use it
            let mut buf_read_box = rc.take().unwrap();

            let read_line_result = match buf_read_box.read_line(&mut buffer) {
                Ok(us) => us,
                Err(er) => {
                    eprintln!("{}: {}", input.filename, er);

                    return Err(Box::new(er));
                }
            };

            // Put the `BufRead` back
            rc.set(Some(buf_read_box));

            // if EOF, output line terminator and end inner loop
            if read_line_result == 0 {
                println!();

                break;
            } else {
                // output line segment

                let mut chars = buffer.chars();

                // TODO
                // Check that the removed character is a newline?
                // Update: checking if it was a newline in this manner fixes "paste_multiple_stdin_serial_test_two"
                // But this seems hacky
                let slice = match chars.next_back() {
                    // `chars` is correct, since character that was removed was a newline character
                    Some('\n') => chars.as_str(),
                    // `chars` is wrong, since it is now missing the final character (which was not a newline
                    // character), so use `buffer`, unmodified
                    _ => buffer.as_str(),
                };

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

fn paste_files(mut paste_info: PasteInfo, mut delim_info: DelimInfo) -> io::Result<()> {
    // for each input line, across N files

    // Re-use buffers to avoid repeated allocations
    let mut buffer = String::new();
    let mut output = String::new();

    loop {
        // Equivalent to allocating a new String here
        output.clear();

        let mut have_data = false;

        // for each input line
        for paste_file in &mut paste_info.inputs {
            // if not already at EOF, read and process a line
            if !paste_file.eof {
                let rc = &paste_file.rdr;

                // TODO
                // unwrap
                // Take the `BufRead` out to use it
                let mut buf_read_box = rc.take().unwrap();

                // Equivalent to allocating a new String here
                buffer.clear();

                let read_line_result = match buf_read_box.read_line(&mut buffer) {
                    Ok(us) => us,
                    Err(er) => {
                        eprintln!("{}: {}", paste_file.filename, er);

                        return Err(er);
                    }
                };

                // Put the `BufRead` back
                rc.set(Some(buf_read_box));

                // if at EOF, note and continue
                if read_line_result == 0 {
                    paste_file.eof = true;

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
            if paste_file.last {
                output.push('\n');
            } else {
                // next delimiter
                #[allow(clippy::collapsible_else_if)]
                if let Some(ch) = delim_info.delim() {
                    output.push(ch);
                }
            }
        }

        if !have_data {
            break;
        }

        // output all segments to stdout at once (one write per line)
        io::stdout().write_all(output.as_bytes())?;

        delim_info.reset();
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let Args {
        delims,
        files,
        serial,
    } = args;

    let paste_info = open_inputs(files)?;

    let delim_state = match delims {
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

    if serial {
        paste_files_serial(paste_info, delim_state)?;
    } else {
        paste_files(paste_info, delim_state)?;
    }

    Ok(())
}
