//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, ErrorKind, Read};
use std::path::PathBuf;
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::io::input_reader;

#[derive(Parser)]
#[command(version, about = gettext("cmp - compare two files"))]
struct Args {
    #[arg(
        short = 'l',
        long,
        group = "verbosity",
        help = gettext(
            "Write the byte number (decimal) and the differing bytes (octal) for each difference"
        )
    )]
    verbose: bool,

    #[arg(
        short,
        long,
        alias = "quiet",
        group = "verbosity",
        help = gettext("Write nothing for differing files; return exit status only")
    )]
    silent: bool,

    #[arg(
        help = gettext(
            "A pathname of the first file to be compared. If file1 is '-', the standard input shall be used"
        )
    )]
    file1: PathBuf,

    #[arg(
        help = gettext(
            "A pathname of the second file to be compared. If file2 is '-', the standard input shall be used"
        )
    )]
    file2: PathBuf,
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

    let mut reader1 = input_reader(&args.file1, true)?;
    let mut reader2 = input_reader(&args.file2, true)?;

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
    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME")).unwrap();
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8").unwrap();

    let args = Args::parse();

    match cmp_main(&args) {
        Ok(x) => ExitCode::from(x),
        Err(_) => {
            // Catches and prevents `io::Error` messages from being written to
            // stderr which may be against the specification.
            ExitCode::from(2)
        }
    }
}
