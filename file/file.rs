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

fn analyze_file(path: &str, args: &Args, magic_files: &[PathBuf]) {
    let path = if path == "-" {
        let mut buf = String::new();
        io::stdin().read_line(&mut buf).unwrap();
        buf.trim().to_string()
    } else {
        path.to_string()
    };

    let met = match fs::symlink_metadata(&path) {
        Ok(met) => met,
        Err(_) => {
            println!("{path}: cannot open");
            return;
        }
    };

    let file_type = met.file_type();

    if file_type.is_symlink() {
        if args.identify_as_symbolic_link {
            println!("{path}: symbolic link");
            return;
        }
        match read_link(&path) {
            Ok(file_p) => {
                // trace the file pointed by symbolic link
                if file_p.exists() {
                    println!("{path}: symbolic link to {}", file_p.display());
                } else {
                    println!("{path}: broken symbolic link to {}", file_p.display());
                }
            }
            Err(_) => {
                println!("{path}: symbolic link");
            }
        }
        return;
    }
    if file_type.is_char_device() {
        println!("{path}: character special");
        return;
    }
    if file_type.is_dir() {
        println!("{path}: directory");
        return;
    }
    if file_type.is_fifo() {
        println!("{path}: fifo");
        return;
    }
    if file_type.is_socket() {
        println!("{path}: socket");
        return;
    }
    if file_type.is_block_device() {
        println!("{path}: block special");
        return;
    }
    if file_type.is_file() {
        if args.no_further_file_classification {
            debug_assert!(magic_files.is_empty());
            println!("{path}: regular file");
            return;
        }
        if met.len() == 0 {
            println!("{path}: empty");
            return;
        }
        match get_type_from_magic_file_dbs(std::path::Path::new(&path), magic_files) {
            Some(f_type) => println!("{path}: {f_type}"),
            None => println!("{path}: data"),
        }
        return;
    }
    unreachable!();
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let matches = Args::command().get_matches();
    let args = Args::from_arg_matches(&matches)?;

    let magic_files = get_magic_files(&args, &matches);

    for file in &args.files {
        analyze_file(file, &args, &magic_files);
    }

    Ok(())
}
