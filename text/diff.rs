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

use std::{
    fs::{self, File},
    io::{self, Read, Write},
    path::PathBuf,
};

use clap::Parser;
use diff_util::{
    common::{FormatOptions, OutputFormat},
    diff_exit_status::DiffExitStatus,
    dir_diff::DirDiff,
    file_diff::FileDiff,
    functions::check_existance,
};
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};

/// diff - compare two files
#[derive(Parser, Clone)]
#[command(version, about = gettext("diff - compare two files"))]
struct Args {
    #[arg(short = 'b', long = "ignore-space-change", help = gettext("Cause EOL whitespace to be treated as blanks"))]
    ignore_eol_space: bool,

    #[arg(short, help = gettext("Output 3 lines of copied context"))]
    context3: bool,

    #[arg(short='C', value_parser = clap::value_parser!(u32).range(1..), help = gettext("Output <N> lines of copied context"))]
    context: Option<u32>,

    #[arg(short, long, help = gettext("Produce output in a form suitable as input for the ed utility"))]
    ed: bool,

    #[arg(short, help = gettext("Produce output in an alternative form, similar in format to -e"))]
    fed: bool,

    #[arg(short, long, help = gettext("Apply diff recursively to files and directories of the same name"))]
    recurse: bool,

    #[arg(short, help = gettext("Output 3 lines of unified context"))]
    unified3: bool,

    #[arg(short='U', value_parser = clap::value_parser!(u32).range(0..), help = gettext("Output <N> lines of unified context"))]
    unified: Option<u32>,

    #[arg(help = gettext("First comparison file (or directory, if -r is specified)"))]
    file1: String,

    #[arg(long, value_parser= clap::value_parser!(String), help = gettext("Label for first file"))]
    label: Option<String>,

    #[arg(long, value_parser= clap::value_parser!(String), help = gettext("Label for second file"))]
    label2: Option<String>,

    #[arg(help = gettext("Second comparison file (or directory, if -r is specified)"))]
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

/// Read stdin and write to a temporary file, returning the path
fn read_stdin_to_temp() -> io::Result<PathBuf> {
    let mut stdin_content = Vec::new();
    io::stdin().read_to_end(&mut stdin_content)?;

    // Create a temporary file
    let temp_path = std::env::temp_dir().join(format!("diff_stdin_{}", std::process::id()));
    let mut temp_file = File::create(&temp_path)?;
    temp_file.write_all(&stdin_content)?;

    Ok(temp_path)
}

fn check_difference(args: Args) -> io::Result<DiffExitStatus> {
    let is_stdin1 = args.file1 == "-";
    let is_stdin2 = args.file2 == "-";

    // Cannot compare stdin to itself
    if is_stdin1 && is_stdin2 {
        eprintln!("diff: cannot compare stdin to itself");
        return Ok(DiffExitStatus::Trouble);
    }

    // Handle stdin by reading to temp file
    let (path1, temp_path1) = if is_stdin1 {
        let temp = read_stdin_to_temp()?;
        (temp.clone(), Some(temp))
    } else {
        (PathBuf::from(&args.file1), None)
    };

    let (path2, temp_path2) = if is_stdin2 {
        let temp = read_stdin_to_temp()?;
        (temp.clone(), Some(temp))
    } else {
        (PathBuf::from(&args.file2), None)
    };

    // Check existence (skip for stdin which is now a temp file)
    if !is_stdin1 && !check_existance(&path1)? {
        return Ok(DiffExitStatus::Trouble);
    }
    if !is_stdin2 && !check_existance(&path2)? {
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

    let result = if path1_is_file && path2_is_file {
        FileDiff::file_diff(path1, path2, &format_options, None)
    } else if !path1_is_file && !path2_is_file {
        DirDiff::dir_diff(path1, path2, &format_options, args.recurse)
    } else {
        FileDiff::file_dir_diff(path1, path2, &format_options)
    };

    // Clean up temp files
    if let Some(temp) = temp_path1 {
        let _ = fs::remove_file(temp);
    }
    if let Some(temp) = temp_path2 {
        let _ = fs::remove_file(temp);
    }

    result
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
