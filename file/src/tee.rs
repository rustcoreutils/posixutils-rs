//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;
extern crate libc;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use libc::{signal, SIGINT, SIG_IGN};
use plib::PROJECT_NAME;
use std::fs::{File, OpenOptions};
use std::io::{self, Read, Write};

/// tee - duplicate standard input
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Append the output to the files.
    #[arg(short, long)]
    append: bool,

    /// Ignore the SIGINT signal.
    #[arg(short, long)]
    ignore: bool,

    /// One or more output files.
    files: Vec<String>,
}

struct TeeFile {
    filename: String,
    f: File,
}

struct TeeInfo {
    outputs: Vec<TeeFile>,
}

impl TeeInfo {
    fn new() -> TeeInfo {
        TeeInfo {
            outputs: Vec::new(),
        }
    }
}

fn open_outputs(args: &Args, info: &mut TeeInfo) -> io::Result<()> {
    for filename in &args.files {
        let f_res = OpenOptions::new()
            .read(false)
            .write(true)
            .create(true)
            .truncate(!args.append)
            .append(args.append)
            .open(filename);

        match f_res {
            Err(e) => {
                eprintln!("{}: {}", filename, e);
                return Err(e);
            }
            Ok(f) => {
                info.outputs.push(TeeFile {
                    filename: filename.to_string(),
                    f,
                });
            }
        }
    }

    Ok(())
}

fn tee_stdin(info: &mut TeeInfo) -> io::Result<()> {
    let mut buffer = [0; plib::BUFSZ];

    loop {
        let n_read_res = io::stdin().read(&mut buffer[..]);
        if let Err(e) = n_read_res {
            eprintln!("stdin: {}", e);
            return Err(e);
        }
        let n_read = n_read_res.unwrap();
        if n_read == 0 {
            break;
        }

        let bufslice = &buffer[0..n_read];

        for output in &mut info.outputs {
            let res = output.f.write_all(bufslice);
            if let Err(e) = res {
                eprintln!("{}: {}", output.filename, e);
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

    if args.ignore {
        unsafe {
            signal(SIGINT, SIG_IGN);
        }
    }

    let mut state = TeeInfo::new();

    open_outputs(&args, &mut state)?;
    tee_stdin(&mut state)?;

    Ok(())
}
