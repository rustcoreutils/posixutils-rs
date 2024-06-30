//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;
extern crate gettextrs;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::PathBuf;

/// join - relational database operator
#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about)]
struct Args {
    /// Produce a line for each unpairable line in file1 or file2
    #[arg(short)]
    a_file: Option<u8>,

    /// Replace empty output fields with this string
    #[arg(short = 'e', long = "empty")]
    replace_empty: Option<String>,

    /// Construct the output line to comprise the specified fields
    #[arg(short)]
    output_format: Option<String>,

    /// Use the specified character as the input and output field separator
    #[arg(short = 't')]
    field_separator: Option<char>,

    /// Produce a line only for each unpairable line in file1 or file2
    #[arg(short = 'v')]
    v_file: Option<u8>,

    /// Join on the specified field of file1
    #[arg(short = '1')]
    field1: Option<usize>,

    /// Join on the specified field of file2
    #[arg(short = '2')]
    field2: Option<usize>,

    /// Path to file1
    file1: PathBuf,

    /// Path to file2
    file2: PathBuf,
}

struct OutputField {
    file_number: usize,
    field_number: usize,
}

struct Config {
    args: Args,
    ofld: Vec<OutputField>,
}

impl Config {
    fn new(args: &Args) -> Config {
        let ofld = {
            if args.output_format.is_none() {
                Vec::new()
            } else {
                parse_output_fields(
                    &args
                        .output_format
                        .as_ref()
                        .expect("output_format is required"),
                )
                .expect("Invalid output format")
            }
        };
        Config {
            args: args.clone(),
            ofld,
        }
    }
}

fn parse_output_fields(o_arg: &str) -> Result<Vec<OutputField>, String> {
    let mut output_fields = Vec::new();

    for field in o_arg.split(',') {
        let mut parts = field.split('.');
        let file_number = match parts.next() {
            Some(num) => match num.parse::<usize>() {
                Ok(n) => n,
                Err(_) => return Err(format!("Invalid file number: {}", num)),
            },
            None => return Err("Missing file number".to_string()),
        };

        let field_number = match parts.next() {
            Some(num) => match num.parse::<usize>() {
                Ok(n) => n,
                Err(_) => return Err(format!("Invalid field number: {}", num)),
            },
            None => return Err("Missing field number".to_string()),
        };

        if parts.next().is_some() {
            return Err("Too many parts in field specification".to_string());
        }

        output_fields.push(OutputField {
            file_number,
            field_number,
        });
    }

    Ok(output_fields)
}

fn print_unpaired_line(line: &str, args: &Args) -> io::Result<()> {
    if let Some(replace_empty) = &args.replace_empty {
        let replaced_line = line.replace("", replace_empty);
        println!("{}", replaced_line);
    } else {
        println!("{}", line);
    }
    Ok(())
}

fn print_joined_line(
    fields1: Vec<&str>,
    fields2: Vec<&str>,
    separator: char,
    args: &Args,
) -> io::Result<()> {
    let mut output_fields = Vec::new();

    if let Some(output_format) = &args.output_format {
        for field in output_format.split(',') {
            let mut parts = field.split('.');
            let file_number = parts.next().unwrap().parse::<u8>().unwrap();
            let field_number = parts.next().unwrap().parse::<usize>().unwrap() - 1;

            let field_value = match file_number {
                1 => fields1.get(field_number).unwrap_or(&""),
                2 => fields2.get(field_number).unwrap_or(&""),
                0 => fields1.get(0).unwrap_or(&""), // Join field
                _ => "",
            };

            output_fields.push(field_value);
        }
    } else {
        output_fields.push(fields1[0]); // Join field
        output_fields.extend(&fields1[1..]);
        output_fields.extend(&fields2[1..]);
    }

    println!("{}", output_fields.join(&separator.to_string()));
    Ok(())
}

fn join_files<R: BufRead>(
    file1: R,
    file2: R,
    separator: char,
    field1: usize,
    field2: usize,
    cfg: &Config,
) -> io::Result<()> {
    let mut file1_lines = file1.lines().peekable();
    let mut file2_lines = file2.lines().peekable();

    while let (Some(line1), Some(line2)) = (file1_lines.peek(), file2_lines.peek()) {
        let fields1: Vec<&str> = line1.as_ref().unwrap().split(separator).collect();
        let fields2: Vec<&str> = line2.as_ref().unwrap().split(separator).collect();

        let key1 = fields1.get(field1).unwrap_or(&"");
        let key2 = fields2.get(field2).unwrap_or(&"");

        match key1.cmp(key2) {
            std::cmp::Ordering::Less => {
                if let Some(1) = cfg.args.a_file {
                    // Print unpaired line from file1
                    print_unpaired_line(line1.as_ref().unwrap(), &cfg.args)?;
                }
                file1_lines.next();
            }
            std::cmp::Ordering::Greater => {
                if let Some(2) = cfg.args.a_file {
                    // Print unpaired line from file2
                    print_unpaired_line(line2.as_ref().unwrap(), &cfg.args)?;
                }
                file2_lines.next();
            }
            std::cmp::Ordering::Equal => {
                // Print the joined line
                print_joined_line(fields1, fields2, separator, &cfg.args)?;
                file1_lines.next();
                file2_lines.next();
            }
        }
    }

    // Handle remaining unpaired lines
    if let Some(1) = cfg.args.a_file {
        for line1 in file1_lines {
            print_unpaired_line(&line1?, &cfg.args)?;
        }
    }
    if let Some(2) = cfg.args.a_file {
        for line2 in file2_lines {
            print_unpaired_line(&line2?, &cfg.args)?;
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Parse command line arguments
    let args = Args::parse();
    let config = Config::new(&args);

    // Set locale and text domain for localization
    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let file1 = BufReader::new(File::open(&args.file1)?);
    let file2 = BufReader::new(File::open(&args.file2)?);

    let field_separator = args.field_separator.unwrap_or(' ');

    // Join on the specified fields or default to the first field
    let field1 = args.field1.unwrap_or(1) - 1;
    let field2 = args.field2.unwrap_or(1) - 1;

    // TODO: Implement the streaming join logic
    join_files(file1, file2, field_separator, field1, field2, &config)?;

    Ok(())
}
