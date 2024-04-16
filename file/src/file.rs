//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::{
    fs::{self, read_link, File, OpenOptions},
    io::{self, BufReader, Read, Seek},
    os::unix::fs::FileTypeExt,
};

/// file - determine file type
#[derive(Parser, Debug)]
#[command(author, version, about, long_about, disable_help_flag = true)]
struct FileArgs {
    /// Apply default position-sensitive system tests and context-sensitive system tests to the file.
    #[clap(short = 'd', long, default_value_t)]
    default_tests: bool,

    /// Identify symbolic link with non existent file as symbolic link
    #[clap(short = 'h', long)]
    identify_as_symbolic_link: bool,

    /// Don't perform further classification on regular file
    #[clap(short = 'i', long)]
    no_further_file_classification: bool,

    /// File containing position-sensitive tests
    #[clap(short = 'm')]
    test_file1: Option<String>,

    /// File containing additional position-sensitive tests
    #[clap(short = 'M')]
    test_file2: Option<String>,

    files: Vec<String>,
}

/// Get file type from the OS default magic file
fn get_file_type_from_default_magic_file(
    f_path: &str,
) -> Result<String, Box<dyn std::error::Error>> {
    let cookie = magic::Cookie::open(magic::cookie::Flags::ERROR)?;

    let database = Default::default();
    let cookie = cookie.load(&database)?;
    let f_type = cookie.file(f_path)?;
    Ok(f_type)
}

fn get_file_type_from_custom_magic_file(
    magic_f_path: &str,
    f_path: &str,
) -> Result<String, Box<dyn std::error::Error>> {
    let cookie = magic::Cookie::open(magic::cookie::Flags::ERROR)?;

    let database = [magic_f_path].try_into()?;
    let cookie = cookie.load(&database)?;
    let f_type = cookie.file(f_path)?;
    Ok(f_type)
}

fn analyze_file(mut path: String, args: &FileArgs) {
    if path == "-" {
        path = String::new();
        io::stdin().read_line(&mut path).unwrap();
    }

    //if let Some(test_file1) = &args.test_file1 {
    //if args.test_file2.is_none() && !args.default_tests {
    //apply_default_system_test();
    //apply_provided_test();
    //}
    //} else if let Some(test_file2) = &args.test_file2 {
    //if args.default_tests || args.test_file1.is_some() {
    //apply_default_system_test();
    //apply_provided_test();
    //}
    //}

    // perform file type test
    match OpenOptions::new().read(true).open(&path) {
        Ok(_) => {
            let metadata = fs::symlink_metadata(&path).unwrap();
            let file_type = metadata.file_type();

            if file_type.is_symlink() {
                if args.identify_as_symbolic_link {
                    println!("{path}: symbolic link");
                } else {
                    match read_link(&path) {
                        Ok(file) => {
                            let file_p = file.to_str().unwrap();
                            println!("{path}: symbolic link to {file_p}");
                        }
                        Err(_) => {
                            println!("{path}: symbolic link");
                        }
                    }
                }
            } else if file_type.is_char_device() {
                println!("{path}: character special");
            } else if file_type.is_dir() {
                println!("{path}: directory");
            } else if file_type.is_fifo() {
                println!("{path}: fifo");
            } else if file_type.is_socket() {
                println!("{path}: socket");
            }
            if file_type.is_block_device() {
                println!("{path}: block special");
            } else if file_type.is_file() {
                if args.no_further_file_classification {
                    println!("{path}: regular file");
                } else {
                    if metadata.len() == 0 {
                        println!("{path}: empty");
                    } else {
                        match get_file_type_from_default_magic_file(&path) {
                            Ok(f_type) => {
                                println!("{path}: {f_type}");
                            }
                            Err(_) => {
                                println!("{path}: data");
                            }
                        }
                    }
                }
            }
        }

        Err(err) => {
            if !args.no_further_file_classification && err.kind() == io::ErrorKind::PermissionDenied
            {
                println!("{path}: cannot open");
            } else {
                let err = err.to_string();
                println!("{path}: {err}");
            }
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = FileArgs::parse();

    // Initialize translation system
    textdomain(PROJECT_NAME).unwrap();
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8").unwrap();

    for file in &args.files {
        analyze_file(file.clone(), &args);
    }

    Ok(())
}
