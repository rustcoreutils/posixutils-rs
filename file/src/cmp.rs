//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::fs;
use std::io::{self, ErrorKind, Read};
use std::path::PathBuf;
use std::process::ExitCode;

/// cmp - compare two files
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Write the byte number (decimal) and the differing bytes (octal) for each difference.
    #[arg(short = 'l', long, group = "verbosity")]
    verbose: bool,

    /// Write nothing for differing files; return exit status only.
    #[arg(short, long, alias = "quiet", group = "verbosity")]
    silent: bool,

    /// A pathname of the first file to be compared. If file1 is '-', the standard input shall be used.
    file1: PathBuf,

    /// A pathname of the second file to be compared. If file2 is '-', the standard input shall be used.
    file2: PathBuf,
}

/// Create a buffered reader for the input file.
fn create_reader(path: &PathBuf) -> io::Result<io::BufReader<Box<dyn Read>>> {
    let readable: Box<dyn Read> = if path.as_os_str() == "-" {
        Box::new(io::stdin().lock())
    } else {
        Box::new(fs::File::open(path)?)
    };
    Ok(io::BufReader::new(readable))
}

/// Reads a single byte from a `BufReader`.
///
/// Returns the byte that was read as `Ok(Some(byte))`. When encountering EOF,
/// this function returns `Ok(None)`.
fn getc(reader: &mut io::BufReader<Box<dyn Read>>) -> io::Result<Option<u8>> {
    let mut byte: u8 = 0;
    match reader.read_exact(std::array::from_mut(&mut byte)) {
        Ok(_) => Ok(Some(byte)),
        Err(e) => {
            if e.kind() == ErrorKind::UnexpectedEof {
                Ok(None)
            } else {
                Err(e)
            }
        }
    }
}

// Helper function to allow using `?` in error handling.
fn cmp_main(args: &Args) -> io::Result<u8> {
    // Also guards against using stdin for both inputs
    if args.file1 == args.file2 {
        return Ok(0);
    }

    let mut reader1 = create_reader(&args.file1)?;
    let mut reader2 = create_reader(&args.file2)?;

    let mut lines: u64 = 1;
    let mut bytes: u64 = 0;

    loop {
        let c1 = getc(&mut reader1)?;
        let c2 = getc(&mut reader2)?;

        bytes += 1;

        match (c1, c2) {
            (Some(c1), Some(c2)) => {
                if c1 != c2 {
                    if args.silent {
                        // Don't print anything
                    } else if args.verbose {
                        // `{:o}` for the required octal representation output
                        println!("{} {:o} {:o}", &bytes, c1, c2);
                    } else {
                        println!(
                            "{} {} differ: char {}, line {}",
                            args.file1.as_os_str().to_string_lossy(),
                            args.file2.as_os_str().to_string_lossy(),
                            bytes,
                            lines
                        );
                    }
                    return Ok(1);
                }
            }
            (None, None) => break,

            // (Some, EOF) or (EOF, Some)
            (c1, _) => {
                eprintln!(
                    "cmp: EOF on {}",
                    if c1.is_none() {
                        &args.file1
                    } else {
                        &args.file2
                    }
                    .as_os_str()
                    .to_string_lossy()
                );
                return Ok(1);
            }
        }

        if c1.map(char::from) == Some('\n') {
            lines += 1;
        }
    }

    Ok(0)
}

fn main() -> ExitCode {
    let args = Args::parse();

    // Initialize translation system
    textdomain(PROJECT_NAME).unwrap();
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8").unwrap();

    match cmp_main(&args) {
        Ok(x) => ExitCode::from(x),
        Err(_) => {
            // Catches and prevents `io::Error` messages from being written to
            // stderr which may be against the specification.
            ExitCode::from(2)
        }
    }
}
