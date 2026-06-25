//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::gettext;
use std::cell::{OnceCell, RefCell};
use std::error::Error;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Stdin, Write};
use std::iter::Cycle;
use std::rc::Rc;
use std::slice::Iter;

/// paste - merge corresponding or subsequent lines of files
#[derive(Parser)]
#[command(version, about = gettext("paste - merge corresponding or subsequent lines of files"))]
struct Args {
    #[arg(short, long, help = gettext("Concatenate all of the lines from each input file into one line of output per file, in command line order"))]
    serial: bool,

    // Other implementations use "delimiters" as the long form, so mirror that
    #[arg(short, long, help = gettext("Delimiter list"))]
    delimiters: Option<String>,

    #[arg(help = gettext("One or more input files"))]
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

enum DelimiterState<'a> {
    NoDelimiters,
    SingleDelimiter(&'a [u8]),
    MultipleDelimiters {
        delimiters: &'a [Box<[u8]>],
        delimiters_iterator: Cycle<Iter<'a, Box<[u8]>>>,
    },
}

impl<'a> DelimiterState<'a> {
    fn new(parsed_delimiters_argument_ref: &'a [Box<[u8]>]) -> DelimiterState<'a> {
        match parsed_delimiters_argument_ref {
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
                delimiters: parsed_delimiters_argument_ref,
                delimiters_iterator: parsed_delimiters_argument_ref.iter().cycle(),
            },
        }
    }

    fn write(&mut self, write: &mut impl io::Write) -> io::Result<()> {
        match *self {
            DelimiterState::SingleDelimiter(sl) => {
                write.write_all(sl)?;
            }
            DelimiterState::MultipleDelimiters {
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
            DelimiterState::MultipleDelimiters {
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

/// `delimiters`: Delimiters parsed from "-d"/"--delimiters" argument
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
fn parse_delimiters_argument(delimiters: Option<String>) -> Result<Box<[Box<[u8]>]>, String> {
    const BACKSLASH: char = '\\';

    fn add_normal_delimiter(ve: &mut Vec<Box<[u8]>>, byte: u8) {
        ve.push(Box::new([byte]));
    }

    let Some(delimiters_string) = delimiters else {
        // Default when no delimiter argument is provided
        return Ok(Box::new([Box::new([b'\t'])]));
    };

    let mut buffer = [0_u8; 4];

    let mut add_other_delimiter = |ve: &mut Vec<Box<[u8]>>, ch: char| {
        let encoded = ch.encode_utf8(&mut buffer);

        ve.push(Box::from(encoded.as_bytes()));
    };

    let mut vec = Vec::<Box<[u8]>>::with_capacity(delimiters_string.len());

    let mut chars = delimiters_string.chars();

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
                        "delimiter list ends with an unescaped backslash: {delimiters_string}"
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

/// Open one operand as a [`Source`]. `-` is standard input (shared across all
/// `-` operands), an empty string yields `None` (skip), and any other name is
/// opened as a file.
fn open_source(
    file: &str,
    stdin_once_cell: &OnceCell<Rc<RefCell<Stdin>>>,
) -> Result<Option<Source>, Box<dyn Error>> {
    // POSIX says only to read from stdin if "-" is passed as a file.
    match file {
        "-" => Ok(Some(Source::StandardInput(
            stdin_once_cell
                .get_or_init(|| Rc::new(RefCell::new(io::stdin())))
                .clone(),
        ))),
        "" => Ok(None),
        st => {
            let fi = File::open(st)
                .map_err(|er| -> Box<dyn Error> { Box::from(format!("{st}: {er}")) })?;
            Ok(Some(Source::File {
                buf_reader: BufReader::new(fi),
                file_description: format!("File: {st}"),
            }))
        }
    }
}

fn open_inputs(files: Vec<String>) -> Result<PasteInfo, Box<dyn Error>> {
    let stdin_once_cell = OnceCell::<Rc<RefCell<Stdin>>>::new();

    let mut paste_file_vec = Vec::<PasteFile>::with_capacity(files.len());

    // Parallel mode opens every input before producing output (CONSEQUENCES OF
    // ERRORS: no output is written if a file cannot be opened), so a failure
    // here aborts before anything is printed.
    for file in files {
        match open_source(&file, &stdin_once_cell)? {
            Some(source) => paste_file_vec.push(PasteFile::new(source)),
            None => plib::diag::error("file operand is an empty string, skipping"),
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

/// Concatenate every line of one source onto a single output line (serial mode).
fn write_serial(
    source: &mut Source,
    delimiter_state: &mut DelimiterState,
    stdout_lock: &mut io::StdoutLock,
    buffer: &mut Vec<u8>,
) -> Result<(), Box<dyn Error>> {
    let mut first_line = true;

    loop {
        buffer.clear();

        if source.read_until_new_line(buffer)? == 0 {
            // EOF: terminate the output line.
            stdout_lock.write_all(b"\n")?;
            break;
        }

        if !first_line {
            delimiter_state.write(stdout_lock)?;
        }

        // Drop the trailing newline if present; keep the line otherwise.
        let mut iter = buffer.iter();
        let slice = match iter.next_back() {
            Some(b'\n') => iter.as_slice(),
            _ => buffer.as_slice(),
        };
        stdout_lock.write_all(slice)?;

        first_line = false;
    }

    // The delimiter resets to the first element after each file operand.
    delimiter_state.reset();
    Ok(())
}

fn paste_files_serial(
    files: &[String],
    mut delimiter_state: DelimiterState,
) -> Result<(), Box<dyn Error>> {
    if files.is_empty() {
        return Err(Box::from(
            "No valid [FILES] were specified. Use '-' if you are trying to read from stdin.",
        ));
    }

    let stdin_once_cell = OnceCell::<Rc<RefCell<Stdin>>>::new();
    let mut stdout_lock = io::stdout().lock();
    let mut buffer = Vec::new();

    // Serial mode follows the POSIX Section 1.4 default for CONSEQUENCES OF
    // ERRORS: a file that cannot be opened is diagnosed and processing
    // continues with the next operand (the final exit status is non-zero).
    for file in files {
        match open_source(file, &stdin_once_cell) {
            Ok(Some(mut source)) => {
                write_serial(
                    &mut source,
                    &mut delimiter_state,
                    &mut stdout_lock,
                    &mut buffer,
                )?;
            }
            Ok(None) => plib::diag::error("file operand is an empty string, skipping"),
            Err(e) => plib::diag::error(&format!("{e}")),
        }
    }

    Ok(())
}

fn paste_files(
    mut paste_info: PasteInfo,
    mut delimiter_state: DelimiterState,
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
                delimiter_state.write(&mut output)?;
            }
        }

        if !have_data {
            break;
        }

        // output all segments to stdout at once (one write per line)
        io::stdout().write_all(output.as_slice())?;

        delimiter_state.reset();
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("paste");

    let args = Args::parse();

    let Args {
        delimiters,
        files,
        serial,
    } = args;

    let parsed_delimiters_argument = match parse_delimiters_argument(delimiters) {
        Ok(bo) => bo,
        Err(st) => {
            plib::diag::error(&st);
            std::process::exit(1);
        }
    };

    let delimiter_state = DelimiterState::new(&parsed_delimiters_argument);

    if serial {
        if let Err(e) = paste_files_serial(&files, delimiter_state) {
            plib::diag::error(&format!("{e}"));
        }
    } else {
        match open_inputs(files) {
            Ok(paste_info) => {
                if let Err(e) = paste_files(paste_info, delimiter_state) {
                    plib::diag::error(&format!("{e}"));
                }
            }
            Err(e) => plib::diag::error(&format!("{e}")),
        }
    }

    std::process::exit(plib::diag::exit_status())
}
