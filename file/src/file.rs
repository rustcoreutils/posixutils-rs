//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//

use clap::{builder::Str, Parser};
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use regex::Regex;
use std::{
    error::Error,
    fmt,
    fs::{self, read_link, File, OpenOptions},
    io::{self, BufRead, BufReader, ErrorKind, Read, Seek},
    num::ParseIntError,
    os::unix::fs::FileTypeExt,
    path::{Path, PathBuf},
    str::Chars,
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

fn parse_magic_file_and_test(
    magic_file: &Path,
    test_file: &Path,
) -> Result<(), Box<dyn std::error::Error>> {
    let mf = File::open(magic_file)?;
    let mf_reader = BufReader::new(mf);

    let tf = File::open(test_file)?;
    let tf_reader = BufReader::new(tf);

    for line in mf_reader.lines() {
        let line = line?;
        let raw_magic_line = RawMagicFileLine::parse(line);
    }

    Ok(())
}

/// Errors that can occur during parsing of a raw magic line.
#[derive(Debug)]
enum RawMagicLineParseError {
    /// Indicates that the offset format is invalid.
    InvalidOffsetFormat,

    /// Indicates that the type format is invalid.
    InvalidTypeFormat,

    /// Indicates that a regular expression used for parsing is invalid.
    InvalidRegex,
}

impl Error for RawMagicLineParseError {}

impl fmt::Display for RawMagicLineParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                RawMagicLineParseError::InvalidOffsetFormat => "Invalid offset!",
                RawMagicLineParseError::InvalidTypeFormat => "Invalid type!",
                RawMagicLineParseError::InvalidRegex => "Invalid Regex!",
            }
        )
    }
}

struct RawMagicFileLine {
    offset: Offset,
    _type: Type,
    value: String,
    message: String,
}
impl RawMagicFileLine {
    fn parse(input: String) -> Result<RawMagicFileLine, RawMagicLineParseError> {
        let input = Self::normalize_whitespace(input)?;
        let fields: Vec<&str> = input.splitn(4, " ").collect();

        let offset = Offset::parse(fields[0].to_string())?;
        let _type = Type::parse(fields[1].to_string())?;
        let value = Self::parse_value(fields[2].to_string())?;
        let message = fields[3].to_string();

        Ok(RawMagicFileLine {
            offset,
            _type,
            value,
            message,
        })
    }

    fn normalize_whitespace(input: String) -> Result<String, RawMagicLineParseError> {
        let re = Regex::new(r"[ \t]+").map_err(|_| RawMagicLineParseError::InvalidRegex)?;
        Ok(re.replacen(&input, 3, " ").to_string())
    }

    fn parse_value(input: String) -> Result<String, RawMagicLineParseError> {
        Ok(String::from("abc"))
    }

    //fn test_file(test_file_reader) {}
}

/// It removes the number it parses
fn parse_number(input: &mut String) -> Option<u64> {
    if let Some(hex_num) = parse_hexadecimal(input) {
        Some(hex_num)
    } else if let Some(dec_oct_num) = parse_decimal_octal(input) {
        Some(dec_oct_num)
    } else {
        None
    }
}

fn parse_hexadecimal(input: &mut String) -> Option<u64> {
    let re = Regex::new(r"^0[xX]([0-9A-F]+)").ok()?;

    let _input = input.clone();
    let captures = re.captures(&_input)?;
    let expr_match = captures.get(0)?;
    let group_match = captures.get(1)?;

    *input = input.replacen(expr_match.as_str(), "", 1);
    u64::from_str_radix(group_match.as_str(), 16).ok()
}

fn parse_decimal_octal(input: &mut String) -> Option<u64> {
    let re = Regex::new(r"^(0?[0-9]+)").ok()?;

    let _input = input.clone();
    let captures = re.captures(&_input)?;
    let expr_match = captures.get(0)?;
    let group_match = captures.get(1)?;

    *input = input.replacen(expr_match.as_str(), "", 1);

    let radix = if group_match.as_str().starts_with('0') {
        8
    } else {
        10
    };

    u64::from_str_radix(group_match.as_str(), radix).ok()
}
/// Type field of the magic file
enum Type {
    Decimal,
    String,
    Unsigned,
}

impl Type {
    pub fn parse(mut input: String) -> Result<Type, RawMagicLineParseError> {
        Self::replace_verbose_type_string_with_short_ones(&mut input);

        let tsc = Self::get_type_specification_char(&mut input)
            .ok_or(RawMagicLineParseError::InvalidTypeFormat)?;

        let mut get_no_of_bytes_represented = || {
            let no_of_bytes = Self::get_no_of_bytes_represented(&mut input)
                .ok_or(RawMagicLineParseError::InvalidTypeFormat);
        };

        match tsc {
            'd' => {
                let no_of_bytes = get_no_of_bytes_represented();
                let mask = Self::get_mask(&mut input);
            }

            'u' => {
                let no_of_bytes = get_no_of_bytes_represented();
                let mask = Self::get_mask(&mut input);
            }

            's' => {}
            _ => return Err(RawMagicLineParseError::InvalidTypeFormat),
        }
        unimplemented!();
        //Ok(())
    }

    /// As strings "byte", "short", "long" and "string" can also be represented by dC, dS, dL and s
    /// So, we'll replace the verbose ones with short ones in the right
    fn replace_verbose_type_string_with_short_ones(input: &mut String) {
        if input.starts_with("byte") {
            input.replacen("byte", "dC", 1);
        } else if input.starts_with("short") {
            input.replacen("short", "dS", 1);
        } else if input.starts_with("long") {
            input.replacen("long", "dL", 1);
        } else if input.starts_with("string") {
            input.replacen("long", "s", 1);
        }
    }

    fn get_type_specification_char(input: &mut String) -> Option<char> {
        if let Some(first_char) = input.chars().next() {
            input.remove(0);
            Some(first_char)
        } else {
            None
        }
    }

    fn get_no_of_bytes_represented(input: &mut String) -> Option<u64> {
        let re = Regex::new(r"^([CSIL]|\d+)").ok()?;
        let _input = input.clone();
        let captures = re.captures(&_input)?;

        let group_match = captures.get(1)?;

        let size = if group_match.as_str().len() == 1 {
            match group_match.as_str().chars().next()? {
                'C' => 1,
                'S' => 2,
                'I' => 4,
                'L' => 8,
                _ => return None,
            }
        } else {
            input.drain(0..group_match.end());
            group_match.as_str().parse().ok()?
        };

        Some(size)
    }

    fn get_mask(input: &mut String) -> Option<u64> {
        if let Some(start) = input.chars().next() {
            if start == '&' {
                input.remove(0);
                return parse_number(input);
            }
        }
        None
    }

    //fn get_mask(input: &mut String) -> Option<u64> {}
}

/// Offset field of the magic file
#[derive(Debug)]
struct Offset {
    num: u64,
    is_continuation: bool,
}

impl Offset {
    pub fn parse(mut input: String) -> Result<Self, RawMagicLineParseError> {
        let is_continuation;
        if input.len() >= 1 {
            is_continuation = input.remove(0) == '>';
        } else {
            return Err(RawMagicLineParseError::InvalidOffsetFormat);
        }

        let num = parse_number(&mut input).ok_or(RawMagicLineParseError::InvalidOffsetFormat)?;

        if !input.is_empty() {
            return Err(RawMagicLineParseError::InvalidOffsetFormat);
        }

        Ok(Offset {
            num,
            is_continuation,
        })
    }
}

/// It's the default magic file(text based)
fn default_magic_file() -> String {
    return "/etc/magic".to_string();
}

fn get_type_from_magic_file_dbs(test_file: &str, magic_file_dbs: &Vec<String>) -> Option<String> {
    return "abc".to_string();
}

fn analyze_file(mut path: String, args: &FileArgs) {
    // set priority according to the occurence of flags in the args
    // lowest index will get highest priority
    let mut magic_file_dbs = Vec::new();

    if path == "-" {
        path = String::new();
        io::stdin().read_line(&mut path).unwrap();
        path = path.trim().to_string();
    }

    if let Some(test_file2) = &args.test_file2 {
        magic_file_dbs.push(test_file2.clone());

        if args.default_tests && args.test_file1.is_some() {
            let raw_args: Vec<String> = std::env::args().collect();
            let m_index = raw_args.iter().position(|x| x == "-m");
            let h_index = raw_args.iter().position(|x| x == "-h");

            if m_index > h_index {
                magic_file_dbs.push(args.test_file1.as_ref().unwrap().clone());
                magic_file_dbs.push(default_magic_file());
            } else {
                magic_file_dbs.push(default_magic_file());
                magic_file_dbs.push(args.test_file1.as_ref().unwrap().clone());
            }
        } else if args.test_file1.is_some() {
            magic_file_dbs.push(args.test_file1.as_ref().unwrap().clone());
        } else if args.default_tests {
            magic_file_dbs.push(default_magic_file());
        }
    } else if let Some(test_file1) = &args.test_file1 {
        magic_file_dbs.push(test_file1.clone());

        if args.test_file2.is_none() && !args.default_tests {
            magic_file_dbs.push(default_magic_file());
        }
    } else {
        magic_file_dbs.push(default_magic_file());
    }

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
                        match get_type_from_magic_file_dbs(&path, &magic_file_dbs) {
                            Some(f_type) => {
                                println!("{path}: {f_type}");
                            }
                            None => {
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
                println!("{path} : {err}");
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
