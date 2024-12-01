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

use clap::Parser;
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

fn get_magic_files(args: &Args) -> Vec<PathBuf> {
    // set priority according to the occurence of flags in the args lowest index will get highest priority
    let mut magic_files: Vec<PathBuf> = Vec::new();

    if args.no_further_file_classification {
        return magic_files;
    }

    let default_magic_file = PathBuf::from(DEFAULT_MAGIC_FILE);

    if let Some(test_file2) = &args.test_file2 {
        magic_files.push(test_file2.clone());

        if args.default_tests && args.test_file1.is_some() {
            let raw_args: Vec<String> = std::env::args().collect();
            let m_index = raw_args.iter().position(|x| x == "-m");
            let h_index = raw_args.iter().position(|x| x == "-h");

            if m_index > h_index {
                magic_files.push(args.test_file1.as_ref().unwrap().clone());
                magic_files.push(default_magic_file);
            } else {
                magic_files.push(default_magic_file);
                magic_files.push(args.test_file1.as_ref().unwrap().clone());
            }
        } else if args.test_file1.is_some() {
            magic_files.push(args.test_file1.as_ref().unwrap().clone());
        } else if args.default_tests {
            magic_files.push(default_magic_file);
        }
    } else if let Some(test_file1) = &args.test_file1 {
        magic_files.push(test_file1.clone());

        if args.test_file2.is_none() && !args.default_tests {
            magic_files.push(default_magic_file);
        }
    } else {
        magic_files.push(default_magic_file);
    }

    magic_files
}

fn analyze_file(mut path: String, args: &Args, magic_files: &Vec<PathBuf>) {
    if path == "-" {
        path = String::new();
        io::stdin().read_line(&mut path).unwrap();
        path = path.trim().to_string();
    }

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
                    println!("{path}: symbolic link to {}", file_p.to_str().unwrap());
                } else {
                    println!(
                        "{path}: broken symbolic link to {}",
                        file_p.to_str().unwrap()
                    );
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
            assert!(magic_files.is_empty());
            println!("{path}: regular file");
            return;
        }
        if met.len() == 0 {
            println!("{path}: empty");
            return;
        }
        match get_type_from_magic_file_dbs(&PathBuf::from(&path), magic_files) {
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

    let args = Args::parse();

    let magic_files = get_magic_files(&args);

    for file in &args.files {
        analyze_file(file.clone(), &args, &magic_files);
    }

    Ok(())
}
