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

mod diff_util;

use std::path::PathBuf;
use std::{fs, io};

use clap::Parser;
use diff_util::common::{FormatOptions, OutputFormat};
use diff_util::diff_exit_status::DiffExitStatus;
use diff_util::dir_diff::DirDiff;
use diff_util::file_diff::FileDiff;
use diff_util::functions::check_existance;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};

/// diff - compare two files
#[derive(Parser, Clone)]
#[command(version, about)]
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
    #[arg(short='U', value_parser = clap::value_parser!(u32).range(0..))]
    unified: Option<u32>,

    /// First comparison file (or directory, if -r is specified)
    file1: String,

    #[arg(long, value_parser= clap::value_parser!(String))]
    label: Option<String>,

    #[arg(long, value_parser= clap::value_parser!(String))]
    label2: Option<String>,

    /// Second comparison file (or directory, if -r is specified)
    file2: String,
}

impl From<&Args> for OutputFormat {
    fn from(args: &Args) -> Self {
        let mut args = args.clone();

        if args.context3 {
            args.context = Some(3);
        }

        if args.unified3 {
            args.unified = Some(3);
        }

        if args.ed {
            OutputFormat::EditScript
        } else if args.fed {
            OutputFormat::ForwardEditScript
        } else if let Some(n) = args.context {
            let n = if n == 0 { 1 } else { n };
            OutputFormat::Context(n as usize)
        } else if let Some(n) = args.unified {
            OutputFormat::Unified(n as usize)
        } else {
            OutputFormat::Default
        }
    }
}

fn check_difference(args: Args) -> io::Result<DiffExitStatus> {
    let path1 = PathBuf::from(&args.file1);
    let path2 = PathBuf::from(&args.file2);

    let path1_exists = check_existance(&path1)?;
    let path2_exists = check_existance(&path2)?;

    if !path1_exists || !path2_exists {
        return Ok(DiffExitStatus::Trouble);
    }

    if path1 == path2 {
        return Ok(DiffExitStatus::Trouble);
    }

    let output_format: OutputFormat = (&args).into();

    let format_options = FormatOptions::try_new(
        args.ignore_eol_space,
        output_format,
        args.label,
        args.label2,
    );
    let format_options = format_options.unwrap();

    let path1_is_file = fs::metadata(&path1)?.is_file();
    let path2_is_file = fs::metadata(&path2)?.is_file();

    if path1_is_file && path2_is_file {
        FileDiff::file_diff(path1, path2, &format_options, None)
    } else if !path1_is_file && !path2_is_file {
        DirDiff::dir_diff(path1, path2, &format_options, args.recurse)
    } else {
        FileDiff::file_dir_diff(path1, path2, &format_options)
    }
}

fn main() -> DiffExitStatus {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").unwrap();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").unwrap();

    let args = Args::parse();

    let result = check_difference(args);

    match result {
        Ok(diff_exit_status) => diff_exit_status,
        Err(error) => {
            eprintln!("diff: {}", error);

            DiffExitStatus::Trouble
        }
    }
}
