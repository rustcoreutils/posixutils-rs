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
// -- Fixed
// - fix:  empty-string delimiters \0
// -- Fixed
// - improve:  don't open all files at once in --serial mode
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::cell::{OnceCell, RefCell};
use std::error::Error;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Stdin, Write};
use std::iter::Cycle;
use std::rc::Rc;
use std::slice::Iter;

/// paste - merge corresponding or subsequent lines of files
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Concatenate all of the lines from each input file into one line of output per file, in command line order
    #[arg(short, long)]
    serial: bool,

    /// Delimiter list
    // Other implementations use "delimiters"
    #[arg(alias = "delimiters", short, long)]
    delims: Option<String>,

    /// One or more input files
    files: Vec<String>,
}

enum Source {
    File {
        buf_reader: BufReader<File>,
        file_description: String,
    },
    StandardInput(Rc<RefCell<Stdin>>),
}

impl Source {
    fn read_until_new_line(&mut self, vec: &mut Vec<u8>) -> Result<usize, Box<dyn Error>> {
        const READ_UNTIL_BYTE: u8 = b'\n';

        let (source_description, result) = match self {
            Self::File {
                buf_reader,
                file_description,
            } => (
                file_description.as_str(),
                buf_reader.read_until(READ_UNTIL_BYTE, vec),
            ),
            Self::StandardInput(st) => (
                "Pipe: standard input",
                st.try_borrow()?.lock().read_until(READ_UNTIL_BYTE, vec),
            ),
        };

        match result {
            Ok(us) => Ok(us),
            Err(er) => Err(Box::from(format!("{source_description}: {er}"))),
        }
    }
}

struct PasteFile {
    eof: bool,
    last: bool,
    source: Source,
}

impl PasteFile {
    fn new(source: Source) -> PasteFile {
        PasteFile {
            eof: false,
            last: false,
            source,
        }
    }
}

struct PasteInfo {
    pub inputs: Vec<PasteFile>,
}

enum DelimsInfo<'a> {
    NoDelimiters,
    SingleDelimiter(&'a [u8]),
    MultipleDelimiters {
        delimiters: &'a [Box<[u8]>],
        delimiters_iterator: Cycle<Iter<'a, Box<[u8]>>>,
    },
}

impl<'a> DelimsInfo<'a> {
    fn new(parsed_delims_argument_ref: &'a [Box<[u8]>]) -> DelimsInfo<'a> {
        match parsed_delims_argument_ref {
            [] => Self::NoDelimiters,
            [only_delimiter] => {
                // -d '\0' has the same effect as -d ''
                if only_delimiter.is_empty() {
                    Self::NoDelimiters
                } else {
                    Self::SingleDelimiter(only_delimiter)
                }
            }
            _ => Self::MultipleDelimiters {
                delimiters: parsed_delims_argument_ref,
                delimiters_iterator: parsed_delims_argument_ref.iter().cycle(),
            },
        }
    }

    fn write(&mut self, write: &mut impl io::Write) -> io::Result<()> {
        match *self {
            DelimsInfo::SingleDelimiter(sl) => {
                write.write_all(sl)?;
            }
            DelimsInfo::MultipleDelimiters {
                ref mut delimiters_iterator,
                ..
            } => {
                // Unwrap because advancing here should never fail
                let bo = delimiters_iterator.next().unwrap();

                write.write_all(bo)?;
            }
            _ => {}
        }

        Ok(())
    }

    fn reset(&mut self) {
        match self {
            DelimsInfo::MultipleDelimiters {
                delimiters,
                ref mut delimiters_iterator,
                ..
            } => {
                *delimiters_iterator = delimiters.iter().cycle();
            }
            _ => {
                // Nothing to do for these cases
            }
        }
    }
}

/// `delims`: Delimiters parsed from "-d"/"--delims" argument
// Support for empty delimiter list:
//
// bsdutils: no, supports "-d" but requires the delimiter list to be non-empty
// BusyBox: does not support "-d" at all
// GNU Core Utilities: yes
// toybox: yes
// uutils's coreutils: no, supports "-d", but panics on an empty delimiter list
//
// POSIX seems to almost forbid this:
// "These elements specify one or more delimiters to use, instead of the default <tab>, to replace the <newline> of the input lines."
// https://pubs.opengroup.org/onlinepubs/9799919799/utilities/paste.html
fn parse_delims_argument(delims: Option<String>) -> Result<Box<[Box<[u8]>]>, String> {
    const BACKSLASH: char = '\\';

    fn add_normal_delimiter(ve: &mut Vec<Box<[u8]>>, byte: u8) {
        ve.push(Box::new([byte]));
    }

    let Some(delims_string) = delims else {
        // Default when no delimiter argument is provided
        return Ok(Box::new([Box::new([b'\t'])]));
    };

    let mut buffer = [0_u8; 4];

    let mut add_other_delimiter = |ve: &mut Vec<Box<[u8]>>, ch: char| {
        let encoded = ch.encode_utf8(&mut buffer);

        ve.push(Box::from(encoded.as_bytes()));
    };

    let mut vec = Vec::<Box<[u8]>>::with_capacity(delims_string.len());

    let mut chars = delims_string.chars();

    while let Some(char) = chars.next() {
        match char {
            BACKSLASH => match chars.next() {
                Some('0') => {
                    vec.push(Box::<[u8; 0]>::new([]));
                }
                Some(BACKSLASH) => {
                    // '\'
                    // U+005C
                    add_normal_delimiter(&mut vec, b'\\');
                }
                Some('n') => {
                    // U+000A
                    add_normal_delimiter(&mut vec, b'\n');
                }
                Some('t') => {
                    // U+0009
                    add_normal_delimiter(&mut vec, b'\t');
                }
                Some(other_char) => {
                    // "If any other characters follow the <backslash>, the results are unspecified."
                    // https://pubs.opengroup.org/onlinepubs/9799919799/utilities/paste.html
                    // GNU Core Utilities: ignores the backslash
                    // BusyBox, toybox: includes the backslash as one of the delimiters
                    add_other_delimiter(&mut vec, other_char);
                }
                None => {
                    return Err(format!(
                        "delimiter list ends with an unescaped backslash: {delims_string}"
                    ));
                }
            },
            not_backslash => {
                add_other_delimiter(&mut vec, not_backslash);
            }
        }
    }

    Ok(vec.into_boxed_slice())
}

fn open_inputs(files: Vec<String>) -> Result<PasteInfo, Box<dyn Error>> {
    let stdin_once_cell = OnceCell::<Rc<RefCell<Stdin>>>::new();

    let mut paste_file_vec = Vec::<PasteFile>::with_capacity(files.len());

    // open each input
    for file in files {
        // POSIX says only to read from stdin if "-" is passed as a file. Most implementations
        // automatically read from stdin if no files are passed to `paste`.
        // https://pubs.opengroup.org/onlinepubs/9799919799/utilities/paste.html
        match file.as_str() {
            "-" => {
                paste_file_vec.push(PasteFile::new(Source::StandardInput(
                    stdin_once_cell
                        .get_or_init(|| Rc::new(RefCell::new(io::stdin())))
                        .clone(),
                )));
            }
            "" => {
                eprintln!("FILE is an empty string, skipping");
            }
            st => {
                let open_result = File::open(st);

                let buf_reader = match open_result {
                    Err(er) => {
                        return Err(Box::from(format!("{st}: {er}")));
                    }
                    Ok(fi) => BufReader::new(fi),
                };

                let filename = format!("File: {st}");

                paste_file_vec.push(PasteFile::new(Source::File {
                    buf_reader,
                    file_description: filename,
                }));
            }
        }
    }

    if paste_file_vec.is_empty() {
        return Err(Box::from(
            "No valid [FILES] were specified. Use '-' if you are trying to read from stdin.",
        ));
    }

    // mark final input
    if let Some(pa) = paste_file_vec.last_mut() {
        pa.last = true;
    }

    Ok(PasteInfo {
        inputs: paste_file_vec,
    })
}

fn paste_files_serial(
    mut paste_info: PasteInfo,
    mut delims_info: DelimsInfo,
) -> Result<(), Box<dyn Error>> {
    let mut stdout_lock = io::stdout().lock();

    // Re-use buffers to avoid repeated allocations
    let mut buffer = Vec::new();

    // loop serially for each input file
    for paste_file in &mut paste_info.inputs {
        let mut first_line = true;

        // for each input line
        loop {
            // Equivalent to allocating a new Vec here
            buffer.clear();

            let read_line_result = paste_file.source.read_until_new_line(&mut buffer)?;

            // if EOF, output line terminator and end inner loop
            if read_line_result == 0 {
                stdout_lock.write_all(b"\n")?;

                break;
            } else {
                if !first_line {
                    delims_info.write(&mut stdout_lock)?;
                }

                // output line segment
                let mut iter = buffer.iter();

                // TODO
                // Check that the removed character is a newline?
                // Update: checking if it was a newline in this manner fixes "paste_multiple_stdin_serial_test_two"
                // But this seems hacky
                let slice = match iter.next_back() {
                    // `iter` is correct, since the byte that was removed was a newline character
                    Some(b'\n') => iter.as_slice(),
                    // `iter` is wrong, since it is now missing the final character (which was not a newline
                    // character), so use `buffer`, unmodified
                    _ => buffer.as_slice(),
                };

                stdout_lock.write_all(slice)?;
            }

            if first_line {
                first_line = false;
            }
        }

        // See https://pubs.opengroup.org/onlinepubs/9799919799/utilities/paste.html:
        //
        //    When the -s option is specified:
        //    The last <newline> in a file shall not be modified.
        //    The delimiter shall be reset to the first element of list after each file operand is processed.
        delims_info.reset();
    }

    Ok(())
}

fn paste_files(
    mut paste_info: PasteInfo,
    mut delims_info: DelimsInfo,
) -> Result<(), Box<dyn Error>> {
    // for each input line, across N files

    // Re-use buffers to avoid repeated allocations
    let mut buffer = Vec::new();
    let mut output = Vec::new();

    loop {
        // Equivalent to allocating a new Vec here
        output.clear();

        let mut have_data = false;

        // for each input line
        for paste_file in &mut paste_info.inputs {
            // if not already at EOF, read and process a line
            if !paste_file.eof {
                // Equivalent to allocating a new Vec here
                buffer.clear();

                let read_line_result = paste_file.source.read_until_new_line(&mut buffer)?;

                if read_line_result == 0 {
                    // if at EOF, note and continue
                    paste_file.eof = true;
                } else {
                    // otherwise add to output line, sans trailing NL
                    have_data = true;

                    let mut iter = buffer.iter();

                    // See note above
                    let slice = match iter.next_back() {
                        Some(b'\n') => iter.as_slice(),
                        _ => buffer.as_slice(),
                    };

                    output.extend_from_slice(slice);
                }
            }

            // final record, output line end
            if paste_file.last {
                output.push(b'\n');
            } else {
                // next delimiter
                delims_info.write(&mut output)?;
            }
        }

        if !have_data {
            break;
        }

        // output all segments to stdout at once (one write per line)
        io::stdout().write_all(output.as_slice())?;

        delims_info.reset();
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

    let parsed_delims_argument = match parse_delims_argument(delims) {
        Ok(bo) => bo,
        Err(st) => {
            eprintln!("paste: {st}");

            // TODO
            // `std::process::exit` should not be used
            std::process::exit(1);
        }
    };

    let paste_info = match open_inputs(files) {
        Ok(pa) => pa,
        Err(bo) => {
            eprintln!("paste: {bo}");

            // TODO
            // `std::process::exit` should not be used
            std::process::exit(1);
        }
    };

    let delims_info = DelimsInfo::new(&parsed_delims_argument);

    if serial {
        paste_files_serial(paste_info, delims_info)?;
    } else {
        paste_files(paste_info, delims_info)?;
    }

    Ok(())
}
