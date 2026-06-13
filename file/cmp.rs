//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
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
    // Comparing standard input to itself is undefined; treat as identical
    // rather than attempting to read stdin twice.
    if args.file1 == args.file2 && args.file1.as_os_str() == "-" {
        return Ok(0);
    }

    let open = |p: &PathBuf| {
        input_reader(p, true)
            .map_err(|e| io::Error::new(e.kind(), format!("{}: {}", p.display(), e)))
    };
    let mut reader1 = open(&args.file1)?;
    let mut reader2 = open(&args.file2)?;

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
                            "{} {} differ: {} {}, {} {}",
                            args.file1.as_os_str().to_string_lossy(),
                            args.file2.as_os_str().to_string_lossy(),
                            gettext("char"),
                            bytes,
                            gettext("line"),
                            lines
                        );
                    }
                    return Ok(1);
                }
            }
            (None, None) => break,

            // (Some, EOF) or (EOF, Some)
            (c1, _) => {
                // -s suppresses all output, including this diagnostic.
                if !args.silent {
                    eprintln!(
                        "cmp: {} {}",
                        gettext("EOF on"),
                        if c1.is_none() {
                            &args.file1
                        } else {
                            &args.file2
                        }
                        .as_os_str()
                        .to_string_lossy()
                    );
                }
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
    textdomain("posixutils-rs").unwrap();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").unwrap();

    let args = Args::parse();

    match cmp_main(&args) {
        Ok(x) => ExitCode::from(x),
        Err(e) => {
            // With -s it is unspecified whether a diagnostic is written; we keep
            // quiet. Otherwise report the error on stderr.
            if !args.silent {
                eprintln!("cmp: {}", e);
            }
            ExitCode::from(2)
        }
    }
}
