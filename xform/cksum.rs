//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - investigate whether Rust crates such as crc32fast provide this
//   functionality more efficiently.  It was tested, and did not work;
//   However, it is theorized that the polynomial was correct,
//   and the source of the error was that the final input data size
//   was not appended to the CRC calculation.  The likely solution is
//   a Rust crate + our finalize() function.
//

mod crc32;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::io::input_stream;
use plib::BUFSZ;
use std::io::{self, Read};
use std::path::PathBuf;

/// cksum - write file checksums and sizes
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Files to read as input.  Use "-" or no-args for stdin.
    files: Vec<PathBuf>,
}

fn cksum_file(filename: &PathBuf) -> io::Result<()> {
    let mut file = input_stream(filename, false)?;

    let mut buffer = [0; BUFSZ];
    let mut n_bytes: usize = 0;
    let mut crc: u32 = 0;

    loop {
        let n_read = file.read(&mut buffer[..])?;
        if n_read == 0 {
            break;
        }

        n_bytes += n_read;
        crc = crc32::update(crc, &buffer[0..n_read]);
    }

    let filename_prefix = {
        if filename.as_os_str() == "" {
            ""
        } else {
            " "
        }
    };
    println!(
        "{} {}{}{}",
        crc32::finalize(crc, n_bytes),
        n_bytes,
        filename_prefix,
        filename.display()
    );

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME"))?;
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8")?;

    let mut args = Args::parse();

    // if no file args, read from stdin
    if args.files.is_empty() {
        args.files.push(PathBuf::new());
    }

    let mut exit_code = 0;

    for filename in &args.files {
        if let Err(e) = cksum_file(filename) {
            exit_code = 1;
            eprintln!("{}: {}", filename.display(), e);
        }
    }

    std::process::exit(exit_code)
}
