//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::PathBuf;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};

/// join - relational database operator
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Additional lines to include when there are no matches
    #[arg(short, default_value_t = 0)]
    additional: u8,

    /// Replace empty output fields with the specified string
    #[arg(short)]
    empty: Option<String>,

    /// Output fields in specified order
    #[arg(short, value_delimiter = ',')]
    order: Option<Vec<String>>,

    /// Field separator character
    #[arg(short = 't', default_value_t = ' ')]
    separator: char,

    /// Output only unpairable lines from file_number
    #[arg(short = 'v', default_value_t = 0)]
    unpairable: u8,

    /// Join on the specified field of file 1
    #[arg(short = '1', default_value_t = 1)]
    field1: usize,

    /// Join on the specified field of file 2
    #[arg(short = '2', default_value_t = 1)]
    field2: usize,

    /// File 1
    file1: PathBuf,

    /// File 2
    file2: PathBuf,
}

fn parse_fields(line: &str, sep: char) -> Vec<String> {
    line.split(sep).map(|s| s.to_string()).collect()
}

fn process_files2(
    file1_path: &PathBuf,
    file2_path: &PathBuf,
    sep: char,
    field1: usize,
    field2: usize,
    a: u8,
    e: Option<String>,
    o: Option<Vec<String>>,
    v: u8,
) -> Result<(), Box<dyn std::error::Error>> {
    // code to support stdin
    let stdin = io::stdin();
    let file1: Box<dyn BufRead> = if file1_path.to_str() == Some("-") {
        Box::new(stdin.lock())
    } else {
        Box::new(BufReader::new(File::open(file1_path)?))
    };

    let mut matched_keys = HashMap::new();
    for line1 in file1.lines() {
        let line1 = line1?;
        let fields1 = parse_fields(&line1, sep);
        let key1 = &fields1[field1 - 1];

        let mut found_match = false;

        let file2: Box<dyn BufRead> = if file2_path.to_str() == Some("-") {
            Box::new(stdin.lock())
        } else {
            Box::new(BufReader::new(File::open(file2_path)?))
        };
        for line2 in file2.lines() {
            let line = line2?;
            let fields2 = parse_fields(&line, sep);
            let key2 = &fields2[field2 - 1];

            if key1 == key2 {
                found_match = true;
                matched_keys.insert(key2.clone(), true);

                if let Some(order) = &o {
                    let mut res: Vec<String> = Vec::new();
                    for num in order {
                        let f_num: Vec<&str> = num.split('.').collect();
                        assert_eq!(f_num.len(), 2);
                        let mut n: usize = f_num[1].parse()?;
                        n -= 1;
                        if f_num[0] == "1" {
                            if fields1.len() <= n {
                                if let Some(e) = &e {
                                    res.push(e.to_string());
                                }
                            } else {
                                res.push(fields1[n].clone());
                            }
                        } else if f_num[0] == "2" {
                            if fields2.len() <= n {
                                if let Some(e) = &e {
                                    res.push(e.to_string());
                                }
                            } else {
                                res.push(fields2[n].clone());
                            }
                        } else {
                            // TODO:
                            panic!("f_num[0] not in (1, 2)");
                        }
                    }
                    if v == 0 {
                        println!("{}", res.join(" "));
                    }
                } else {
                    if v == 0 {
                        println!("{} {}", fields1.join(" "), fields2[1..].join(" "));
                    }
                }
            }
        }

        if !found_match && a == 1 {
            println!("{}", fields1.join(" "));
        }
    }

    let file1 = BufReader::new(File::open(file1_path)?);
    let file2 = BufReader::new(File::open(file2_path)?);
    if v == 1 {
        for line1 in file1.lines() {
            let line1 = line1?;
            let fields1 = parse_fields(&line1, sep);
            let key1 = &fields1[field1 - 1];
            if !matched_keys.contains_key(key1) {
                println!("{}", fields1.join(" "));
            }
        }
    } else if v == 2 {
        for line2 in file2.lines() {
            let line2 = line2?;
            let fields2 = parse_fields(&line2, sep);
            let key2 = &fields2[field2 - 1];
            if !matched_keys.contains_key(key2) {
                println!("{}", fields2.join(" "));
            }
        }
    }

    Ok(())
}

fn join(args: Args) -> Result<(), Box<dyn std::error::Error>> {
    process_files2(
        &args.file1,
        &args.file2,
        args.separator,
        args.field1,
        args.field2,
        args.additional,
        args.empty,
        args.order,
        args.unpairable,
    )?;

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    if let Err(err) = join(args) {
        exit_code = 1;
        eprint!("{}", err);
    }

    std::process::exit(exit_code)
}
