//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod magic;

use std::fs::read_link;
use std::os::unix::fs::FileTypeExt;
use std::path::PathBuf;
use std::{fs, io};

use clap::{CommandFactory, FromArgMatches, Parser};
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

use crate::magic::{get_type_from_magic_file_dbs, DEFAULT_MAGIC_FILE};

#[derive(Parser)]
#[command(
    version,
    disable_help_flag = true,
    about = gettext("file - determine file type")
)]
struct Args {
    #[arg(long, action = clap::ArgAction::HelpLong)]
    help: Option<bool>,

    #[arg(
        short = 'd',
        long,
        help = gettext(
            "Apply default position-sensitive system tests and context-sensitive system tests to the file"
        )
    )]
    default_tests: bool,

    #[arg(
        short = 'h',
        long,
        help = gettext("Identify symbolic link with non existent file as symbolic link")
    )]
    identify_as_symbolic_link: bool,

    #[arg(
        short = 'i',
        long,
        help = gettext("Don't perform further classification on regular file")
    )]
    no_further_file_classification: bool,

    #[arg(
        short = 'm',
        help = gettext("File containing position-sensitive tests")
    )]
    test_file1: Option<PathBuf>,

    #[arg(
        short = 'M',
        help = gettext("File containing additional position-sensitive tests")
    )]
    test_file2: Option<PathBuf>,

    files: Vec<String>,
}

fn get_magic_files(args: &Args, matches: &clap::ArgMatches) -> Vec<PathBuf> {
    if args.no_further_file_classification {
        return Vec::new();
    }

    // Collect (index, path) for each present flag, then sort by CLI position
    let mut indexed_files: Vec<(usize, PathBuf)> = Vec::new();

    if let Some(ref path) = args.test_file2 {
        if let Some(idx) = matches.index_of("test_file2") {
            indexed_files.push((idx, path.clone()));
        }
    }
    if let Some(ref path) = args.test_file1 {
        if let Some(idx) = matches.index_of("test_file1") {
            indexed_files.push((idx, path.clone()));
        }
    }
    if args.default_tests {
        if let Some(idx) = matches.index_of("default_tests") {
            indexed_files.push((idx, PathBuf::from(DEFAULT_MAGIC_FILE)));
        }
    }

    indexed_files.sort_by_key(|(idx, _)| *idx);

    let mut magic_files: Vec<PathBuf> = indexed_files.into_iter().map(|(_, p)| p).collect();

    // Per POSIX: if only -m (no -M, no -d), append default magic file after
    if args.test_file1.is_some() && args.test_file2.is_none() && !args.default_tests {
        magic_files.push(PathBuf::from(DEFAULT_MAGIC_FILE));
    }

    // If nothing specified at all, use default magic file
    if magic_files.is_empty() {
        magic_files.push(PathBuf::from(DEFAULT_MAGIC_FILE));
    }

    magic_files
}

/// Classify a non-symlink file given its (followed) metadata, returning the
/// `<type>` string for the `"%s: %s"` output. `met` describes the file whose
/// contents `path` opens (the symlink target, when following a link).
fn classify(path: &str, met: &fs::Metadata, args: &Args, magic_files: &[PathBuf]) -> String {
    let file_type = met.file_type();

    if file_type.is_char_device() {
        return "character special".to_string();
    }
    if file_type.is_dir() {
        return "directory".to_string();
    }
    if file_type.is_fifo() {
        return "fifo".to_string();
    }
    if file_type.is_socket() {
        return "socket".to_string();
    }
    if file_type.is_block_device() {
        return "block special".to_string();
    }
    if file_type.is_file() {
        if args.no_further_file_classification {
            return "regular file".to_string();
        }
        if met.len() == 0 {
            return "empty".to_string();
        }
        return match get_type_from_magic_file_dbs(std::path::Path::new(path), magic_files) {
            Some(f_type) => f_type,
            None => "data".to_string(),
        };
    }
    // Any other (unknown) type.
    "data".to_string()
}

fn analyze_file(path: &str, args: &Args, magic_files: &[PathBuf]) {
    let path = if path == "-" {
        let mut buf = String::new();
        io::stdin().read_line(&mut buf).unwrap();
        buf.trim().to_string()
    } else {
        path.to_string()
    };

    let lmet = match fs::symlink_metadata(&path) {
        Ok(met) => met,
        Err(_) => {
            // Per spec this is reported but does not affect the exit status.
            println!("{path}: cannot open");
            return;
        }
    };

    if lmet.file_type().is_symlink() {
        // `metadata` follows the link; Ok means the target exists.
        let target_meta = fs::metadata(&path);
        let target = read_link(&path).ok();

        // Identify the link itself when -h is given, or by default when the
        // link is broken (POSIX: a dangling link is treated as if -h).
        if args.identify_as_symbolic_link || target_meta.is_err() {
            match (&target, target_meta.is_ok()) {
                (Some(t), true) => println!("{path}: symbolic link to {}", t.display()),
                (Some(t), false) => println!("{path}: broken symbolic link to {}", t.display()),
                (None, _) => println!("{path}: symbolic link"),
            }
            return;
        }

        // Default: resolve the link and classify the referenced file's type.
        let tmet = target_meta.unwrap();
        println!("{path}: {}", classify(&path, &tmet, args, magic_files));
        return;
    }

    println!("{path}: {}", classify(&path, &lmet, args, magic_files));
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let matches = Args::command().get_matches();
    let args = Args::from_arg_matches(&matches)?;

    let magic_files = get_magic_files(&args, &matches);

    // A magic file named explicitly via -m/-M that cannot be opened is a
    // genuine error and sets a non-zero exit status. (A missing or unreadable
    // operand file is NOT an error per the spec.)
    let mut had_error = false;
    for mf in [&args.test_file1, &args.test_file2].into_iter().flatten() {
        if fs::metadata(mf).is_err() {
            eprintln!("file: {}: cannot open magic file", mf.display());
            had_error = true;
        }
    }

    for file in &args.files {
        analyze_file(file, &args, &magic_files);
    }

    if had_error {
        std::process::exit(1);
    }
    Ok(())
}
