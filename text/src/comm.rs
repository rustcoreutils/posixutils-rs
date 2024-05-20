//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::fs;
use std::io::{self, BufRead, Write};
use std::path::PathBuf;

const NO1: u32 = 1 << 0;
const NO2: u32 = 1 << 1;
const NODUP: u32 = 1 << 2;

/// comm - select or reject lines common to two files
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Suppress the output column of lines unique to file1.
    #[arg(short = '1', long)]
    no1: bool,

    /// Suppress the output column of lines unique to file2.
    #[arg(short = '2', long)]
    no2: bool,

    /// Suppress the output column of lines duplicated in file1 and file2.
    #[arg(short = '3', long)]
    no_dup: bool,

    /// Comparison file1
    file1: PathBuf,

    /// Comparison file2
    file2: PathBuf,
}

fn line_out(lead_dup: &'static str, outmask: u32, curtype: u32, s: &str) -> io::Result<()> {
    if (outmask & curtype) != 0 {
        return Ok(());
    }

    match curtype {
        NO1 => {
            io::stdout().write_all(s.as_bytes())?;
        }
        NO2 => {
            let lead_f2 = {
                if (outmask & NO1) != 0 {
                    ""
                } else {
                    "\t"
                }
            };
            let newstr = format!("{}{}", lead_f2, s);
            io::stdout().write_all(newstr.as_bytes())?;
        }
        NODUP => {
            let newstr = format!("{}{}", lead_dup, s);
            io::stdout().write_all(newstr.as_bytes())?;
        }
        _ => {
            panic!("should never occur");
        }
    }

    Ok(())
}

fn open_file(pathname: &PathBuf) -> io::Result<io::BufReader<fs::File>> {
    Ok(io::BufReader::new(fs::File::open(pathname)?))
}

fn comm_file(
    mask: u32,
    lead_dup: &'static str,
    file1name: &PathBuf,
    file2name: &PathBuf,
) -> io::Result<()> {
    // open files, or stdin
    let mut rdr1 = open_file(file1name)?;
    let mut rdr2 = open_file(file2name)?;

    let mut buf1 = String::new();
    let mut buf2 = String::new();
    let mut want1 = true;
    let mut want2 = true;

    loop {
        if want1 && buf1.is_empty() {
            if rdr1.read_line(&mut buf1)? == 0 {
                want1 = false;
            }
        }
        if want2 && buf2.is_empty() {
            if rdr2.read_line(&mut buf2)? == 0 {
                want2 = false;
            }
        }

        if buf1.is_empty() && buf2.is_empty() {
            break;
        }

        if buf1.is_empty() {
            line_out(lead_dup, mask, NO2, &buf2)?;
            buf2.clear();
        } else if buf2.is_empty() {
            line_out(lead_dup, mask, NO1, &buf1)?;
            buf1.clear();
        } else if buf1 < buf2 {
            line_out(lead_dup, mask, NO1, &buf1)?;
            buf1.clear();
        } else if buf2 < buf1 {
            line_out(lead_dup, mask, NO2, &buf2)?;
            buf2.clear();
        } else {
            line_out(lead_dup, mask, NODUP, &buf1)?;
            buf1.clear();
            buf2.clear();
        }
    }

    Ok(())
}

fn args_mask(args: &Args) -> u32 {
    let mut mask = 0;
    if args.no1 {
        mask |= NO1;
    }
    if args.no2 {
        mask |= NO2;
    }
    if args.no_dup {
        mask |= NODUP;
    }

    mask
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mask = args_mask(&args);

    const MASK2: u32 = NO1 | NO2;
    let lead_dup = {
        if (mask & MASK2) == 0 {
            "\t\t"
        } else if (mask & MASK2) == MASK2 {
            ""
        } else {
            "\t"
        }
    };

    let mut exit_code = 0;

    if let Err(e) = comm_file(mask, lead_dup, &args.file1, &args.file2) {
        exit_code = 1;
        eprintln!("{}", e);
    }

    std::process::exit(exit_code)
}
