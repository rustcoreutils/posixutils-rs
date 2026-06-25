//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::error::Error;
use std::io::{self, Read, StdoutLock, Write};
use std::path::{Path, PathBuf};

use clap::Parser;
use gettextrs::gettext;
use plib::io::input_stream_dashed;
use plib::BUFSZ;

const N_C_GROUP: &str = "N_C_GROUP";

/// head - copy the first part of files
/// If neither -n nor -c are specified, copies the first 10 lines of each file (-n 10).
#[derive(Parser)]
#[command(version, about = gettext("head - copy the first part of files"))]
struct Args {
    #[arg(long = "lines", short, value_parser = clap::value_parser!(usize), group = N_C_GROUP,
          help = gettext("The first <N> lines of each input file shall be copied to standard output (mutually exclusive with -c)"))]
    n: Option<usize>,

    // Note: -c was added to POSIX in POSIX.1-2024, but has been supported on most platforms since the late 1990s
    // https://pubs.opengroup.org/onlinepubs/9799919799/utilities/head.html
    #[arg(long = "bytes", short = 'c', value_parser = clap::value_parser!(usize), group = N_C_GROUP,
          help = gettext("The first <N> bytes of each input file shall be copied to standard output (mutually exclusive with -n)"))]
    bytes_to_copy: Option<usize>,

    #[arg(help = gettext("Files to read as input."))]
    files: Vec<PathBuf>,
}

enum CountType {
    Bytes(usize),
    Lines(usize),
}

fn head_file(
    count_type: &CountType,
    pathname: &Path,
    first: bool,
    want_header: bool,
    stdout_lock: &mut StdoutLock,
) -> Result<(), Box<dyn Error>> {
    const BUFFER_SIZE: usize = BUFSZ;

    // print file header
    if want_header {
        if first {
            writeln!(stdout_lock, "==> {} <==", pathname.display())?;
        } else {
            writeln!(stdout_lock, "\n==> {} <==", pathname.display())?;
        }
    }

    // open file, or stdin ("-" or no operand)
    let mut file = input_stream_dashed(pathname)?;

    let mut raw_buffer = [0_u8; BUFFER_SIZE];

    match *count_type {
        CountType::Bytes(bytes_to_copy) => {
            let mut bytes_remaining = bytes_to_copy;

            loop {
                let number_of_bytes_read = {
                    // Do not read more bytes than necessary
                    let read_up_to_n_bytes = BUFFER_SIZE.min(bytes_remaining);

                    file.read(&mut raw_buffer[..read_up_to_n_bytes])?
                };

                if number_of_bytes_read == 0_usize {
                    // Reached EOF
                    break;
                }

                let bytes_to_write = &raw_buffer[..number_of_bytes_read];

                stdout_lock.write_all(bytes_to_write)?;

                bytes_remaining -= number_of_bytes_read;

                if bytes_remaining == 0_usize {
                    break;
                }
            }
        }
        CountType::Lines(0) => {
            // -n 0 selects no lines: produce no output (the header, if any, has
            // already been written).
        }
        CountType::Lines(n) => {
            let mut nl = 0_usize;

            loop {
                // read a chunk of file data
                let n_read = file.read(&mut raw_buffer)?;

                if n_read == 0_usize {
                    // Reached EOF
                    break;
                }

                // slice of buffer containing file data
                let buf = &raw_buffer[..n_read];

                let mut position = 0_usize;

                // count newlines
                for &byte in buf {
                    position += 1;

                    // LF character encountered
                    if byte == b'\n' {
                        nl += 1;

                        // if user-specified limit reached, stop
                        if nl >= n {
                            break;
                        }
                    }
                }

                // output full or partial buffer
                let bytes_to_write = &raw_buffer[..position];

                stdout_lock.write_all(bytes_to_write)?;

                // if user-specified limit reached, stop
                if nl >= n {
                    break;
                }
            }
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("head");

    let mut args = Args::parse();

    // POSIX makes "the number is a positive integer" a constraint on the
    // application, not a reason for head to fail: a count of zero selects
    // nothing and produces empty output (as GNU, BusyBox, uutils, and toybox
    // do), rather than exiting non-zero.
    let count_type = match (args.n, args.bytes_to_copy) {
        (None, None) => {
            // If no arguments are provided, the default is 10 lines
            CountType::Lines(10_usize)
        }
        (Some(n), None) => CountType::Lines(n),
        (None, Some(bytes_to_copy)) => CountType::Bytes(bytes_to_copy),
        (Some(_), Some(_)) => {
            // Will be caught by clap
            unreachable!();
        }
    };

    let files = &mut args.files;

    // if no files, read from stdin
    if files.is_empty() {
        files.push(PathBuf::new());
    }

    let want_header = files.len() > 1;

    let mut first = true;

    let mut stdout_lock = io::stdout().lock();

    for filename in files {
        if let Err(e) = head_file(&count_type, filename, first, want_header, &mut stdout_lock) {
            plib::diag::error(&format!("{}: {}", filename.display(), e));
        }

        first = false;
    }

    std::process::exit(plib::diag::exit_status())
}
