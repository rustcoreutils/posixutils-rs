//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - Just a start with the core algorithm; -C and -U both need context output
// - Implement -r (recurse)
// - Research and implement -f alternate output format properly
//

extern crate clap;
extern crate diff;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::fs;
use std::io;

/// diff - compare two files
#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about)]
struct Args {
    /// Cause EOL whitespace to be treated as blanks
    #[arg(short = 'b', long = "ignore-space-change")]
    ignore_eol_space: bool,

    /// Output 3 lines of copied context
    #[arg(short)]
    context3: bool,

    /// Output <N> lines of copied context
    #[arg(short='C', value_parser = clap::value_parser!(u32).range(1..))]
    context: Option<u32>,

    /// Produce output in a form suitable as input for the ed utility
    #[arg(short, long)]
    ed: bool,

    /// Produce output in an alternative form, similar in format to -e
    #[arg(short)]
    fed: bool,

    /// Apply diff recursively to files and directories of the same name
    #[arg(short, long)]
    recurse: bool,

    /// Output 3 lines of unified context
    #[arg(short)]
    unified3: bool,

    /// Output <N> lines of unified context
    #[arg(short='U', value_parser = clap::value_parser!(u32).range(1..))]
    unified: Option<u32>,

    /// First comparison file (or directory, if -r is specified)
    file1: String,

    /// Second comparison file (or directory, if -r is specified)
    file2: String,
}

enum OutputFormat {
    Ed,
    Fed,
    Context(u32),
    Unified(u32),
}

fn diff_file_fed(left: &str, right: &str) {
    let mut left_line = 0;
    let mut right_line = 0;

    for diff in diff::lines(&left, &right) {
        match diff {
            diff::Result::Left(l) => {
                left_line += 1;
                println!("{}a{}", left_line, right_line);
                println!("> {}", l);
            }
            diff::Result::Both(_, _) => {
                left_line += 1;
                right_line += 1;
            }
            diff::Result::Right(r) => {
                right_line += 1;
                println!("{}c{}", left_line, right_line);
                println!("< {}", r);
            }
        }
    }
}

fn diff_file_ed(left: &str, right: &str) {
    let mut left_line = 0;
    let mut right_line = 0;

    for diff in diff::lines(&left, &right) {
        match diff {
            diff::Result::Left(l) => {
                left_line += 1;
                println!("{}a{}", left_line, right_line);
                println!("> {}", l);
            }
            diff::Result::Both(_, _) => {
                left_line += 1;
                right_line += 1;
            }
            diff::Result::Right(r) => {
                right_line += 1;
                println!("{}c{}", left_line, right_line);
                println!("< {}", r);
            }
        }
    }
}

fn diff_file_context(left: &str, right: &str, _n: u32) {
    for diff in diff::lines(&left, &right) {
        match diff {
            diff::Result::Left(l) => {
                println!("< {}", l);
            }
            diff::Result::Both(l, _) => {
                println!("  {}", l);
            }
            diff::Result::Right(r) => {
                println!("> {}", r);
            }
        }
    }
}

fn diff_file_unified(left: &str, right: &str, _n: u32) {
    for diff in diff::lines(&left, &right) {
        match diff {
            diff::Result::Left(l) => {
                println!("-{}", l);
            }
            diff::Result::Both(l, _) => {
                println!(" {}", l);
            }
            diff::Result::Right(r) => {
                println!("+{}", r);
            }
        }
    }
}

fn diff_files(out_fmt: OutputFormat, filename1: &str, filename2: &str) -> io::Result<()> {
    let left = fs::read_to_string(filename1)?;
    let right = fs::read_to_string(filename2)?;

    match out_fmt {
        OutputFormat::Ed => diff_file_ed(&left, &right),
        OutputFormat::Fed => diff_file_fed(&left, &right),
        OutputFormat::Context(n) => diff_file_context(&left, &right, n),
        OutputFormat::Unified(n) => diff_file_unified(&left, &right, n),
    }

    Ok(())
}

fn parse_output_format(args: &Args) -> OutputFormat {
    let mut args = args.clone();

    if args.context3 {
        args.context = Some(3);
    }
    if args.unified3 {
        args.unified = Some(3);
    }

    if args.ed {
        OutputFormat::Ed
    } else if args.fed {
        OutputFormat::Fed
    } else if let Some(n) = args.context {
        OutputFormat::Context(n)
    } else if let Some(n) = args.unified {
        OutputFormat::Unified(n)
    } else {
        OutputFormat::Unified(3)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    let out_fmt = parse_output_format(&args);

    if let Err(e) = diff_files(out_fmt, &args.file1, &args.file2) {
        exit_code = 1;
        eprintln!("diff: {}", e);
    }

    std::process::exit(exit_code)
}
