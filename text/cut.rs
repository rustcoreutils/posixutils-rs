//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, BufRead, Error, Read};

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::path::PathBuf;

/// cut - cut out selected fields of each line of a file
#[derive(Parser, Clone)]
#[command(version, about = gettext("cut - cut out selected fields of each line of a file"))]
struct Args {
    /// Cut based on a list of bytes
    #[arg(short = 'b', long)]
    bytes: Option<String>,

    /// Cut based on a list of characters
    #[arg(short = 'c', long)]
    characters: Option<String>,

    /// Cut based on a list of fields
    #[arg(short = 'f', long)]
    fields: Option<String>,

    /// Set the field delimiter
    #[arg(short = 'd', long)]
    delimiter: Option<char>,

    /// Suppress lines with no delimiter characters
    #[structopt(short = 's', long)]
    suppress: bool,

    /// Do not split characters
    #[structopt(short = 'n')]
    no_split: bool,

    /// Input files
    filenames: Vec<PathBuf>,
}

fn validate_args(args: &Args) -> Result<(), String> {
    // Check if one of 'bytes', 'characters', or 'fields' is specified
    if args.bytes.is_none() && args.characters.is_none() && args.fields.is_none() {
        return Err("Please specify one of '-b', '-c', or '-f'".to_string());
    }

    // Check if -s flag is used only with -f
    if args.suppress && args.fields.is_none() {
        return Err("Option '-s' may only be used with '-f'".to_string());
    }

    // Check if fields are not empty when specified
    if let Some(fields) = &args.fields {
        if fields.is_empty() {
            return Err("Option '-f' cannot be empty".to_string());
        }
    }
    // Check if characters are not empty when specified
    if let Some(characters) = &args.characters {
        if characters.is_empty() {
            return Err("Option '-c' cannot be empty".to_string());
        }
    }
    // Check if bytes are not empty when specified
    if let Some(bytes) = &args.bytes {
        if bytes.is_empty() {
            return Err("Option '-b' cannot be empty".to_string());
        }
    }

    Ok(())
}

#[derive(Clone)]
enum ParseVariat {
    Bytes(Vec<(i32, i32)>),
    Characters(Vec<(i32, i32)>),
    Fields(Vec<(i32, i32)>),
}

/// Helper function to determine if the given bytes form a valid UTF-8 character boundary.
///
/// This function checks if the first byte of the provided byte slice `bytes` represents
/// the start of a valid UTF-8 character. If the bytes are valid UTF-8, it returns true;
/// otherwise, it returns false.
///
/// # Arguments
///
/// * `bytes` - A slice of bytes to be checked.
///
/// # Returns
///
/// A boolean value indicating whether the provided bytes form a valid UTF-8 character boundary.
///
fn is_character_boundary(bytes: &[u8]) -> bool {
    // Check if the first byte of `bytes` is a valid UTF-8 character boundary
    match std::str::from_utf8(bytes) {
        Ok(s) => s.chars().next().is_some(),
        Err(_) => false,
    }
}

/// Cuts out selected bytes from the given line based on the specified ranges.
///
/// This function takes a slice of bytes representing the input `line`, along with optional
/// delimiter character `delim` and a vector of tuples `ranges` representing the start and
/// end indices of the byte ranges to cut. The boolean `n` flag indicates whether character
/// boundaries should be respected when selecting byte ranges.
///
/// # Arguments
///
/// * `line` - A slice of bytes representing the input line.
/// * `delim` - An optional character delimiter.
/// * `ranges` - A vector of tuples representing the start and end indices of the byte ranges to cut.
/// * `n` - A boolean flag indicating whether character boundaries should be respected.
///
/// # Returns
///
/// A vector containing the selected bytes from the input line based on the specified ranges.
///
fn cut_bytes(line: &[u8], delim: Option<char>, ranges: &Vec<(i32, i32)>, n: bool) -> Vec<u8> {
    let mut result = Vec::new();

    for (start, end) in ranges {
        let mut start = *start as usize;
        let mut end = *end as usize;

        if n {
            if start != 0 && !is_character_boundary(&line[start..]) {
                start -= 1;
            }
            if end != 0 && !is_character_boundary(&line[end..]) {
                end -= 1;
            }
        }

        if line.get(start).is_some() {
            if start == end {
                result.push(line[start]);
                if let Some(delim) = delim {
                    for byte in delim.to_string().as_bytes() {
                        result.push(*byte);
                    }
                }
            } else {
                for byte in line.iter().take(end + 1).skip(start) {
                    result.push(*byte);
                }
                if let Some(delim) = delim {
                    for byte in delim.to_string().as_bytes() {
                        result.push(*byte);
                    }
                }
            }
        }
    }
    if delim.is_some() {
        result.pop();
    }

    result
}

/// Cuts out selected characters from the given line based on the specified ranges.
///
/// This function takes a string `line` representing the input line, along with an optional
/// delimiter character `delim` and a vector of tuples `ranges` representing the start and
/// end indices of the character ranges to cut.
///
/// # Arguments
///
/// * `line` - A string representing the input line.
/// * `delim` - An optional character delimiter.
/// * `ranges` - A vector of tuples representing the start and end indices of the character ranges to cut.
///
/// # Returns
///
/// A string containing the selected characters from the input line based on the specified ranges.
///
fn cut_characters(line: &str, delim: Option<char>, ranges: &Vec<(i32, i32)>) -> String {
    let mut result = String::new();
    let chars: Vec<char> = line.chars().collect();

    for (start, end) in ranges {
        let start = *start as usize;
        let end = *end as usize;

        if chars.get(start).is_some() {
            if start == end {
                result.push(chars[start]);
                if let Some(delim) = delim {
                    result.push(delim);
                }
            } else {
                let chars = line.chars();
                for char in chars.take(end + 1).skip(start) {
                    result.push(char);
                }
                if let Some(delim) = delim {
                    result.push(delim);
                }
            }
        }
    }
    if delim.is_some() {
        result.pop();
    }
    result
}

/// Cuts out selected fields from the given line based on the specified ranges.
///
/// This function takes a string `line` representing the input line, a delimiter character `delim`,
/// a vector of tuples `ranges` representing the start and end indices of the fields to cut, and
/// a boolean `suppress` flag indicating whether to suppress output if no fields are found.
///
/// # Arguments
///
/// * `line` - A string representing the input line.
/// * `delim` - A character delimiter used to split the line into fields.
/// * `ranges` - A vector of tuples representing the start and end indices of the fields to cut.
/// * `suppress` - A boolean flag indicating whether to suppress output if no fields are found.
///
/// # Returns
///
/// A tuple containing the resulting string with the selected fields and a boolean indicating
/// whether the output was suppressed.
///
fn cut_fields(line: &str, delim: char, ranges: &Vec<(i32, i32)>, suppress: bool) -> (String, bool) {
    let mut result = String::new();
    let mut skip = false;
    let delim_escaped = delim.escape_debug().to_string();
    let mut fields: Vec<&str>;
    if delim_escaped.len() > 1 {
        fields = line.split(&delim_escaped).collect();
    } else {
        fields = line.split(delim).collect();
    }

    if fields.len() == 1 {
        fields = vec![];
    }

    for (start, end) in ranges {
        let start = *start as usize;
        let end = *end as usize;

        if fields.get(start).is_some() {
            if start == end {
                result.push_str(fields[start]);
                result.push(delim);
            } else {
                for i in fields.iter().take(end + 1).skip(start) {
                    result.push_str(i);
                    result.push(delim);
                }
            }
        }
    }
    result.pop();
    if result.is_empty() && fields.is_empty() && !suppress {
        result.push_str(line);
    }
    if result.is_empty() && fields.is_empty() && suppress {
        skip = true;
    }
    (result, skip)
}

/// Processes files according to the provided arguments, cutting out selected fields, characters, or bytes.
///
/// # Arguments
///
/// * `args` - A struct containing the command-line arguments.
///
/// # Returns
///
/// A `Result` indicating success or failure. If an error occurs during file processing, it is returned as `Err`.
///
fn cut_files(args: Args) -> Result<(), Box<dyn std::error::Error>> {
    validate_args(&args).map_err(|err| Box::new(Error::other(err)))?;

    // open files, or stdin

    let filenames = args.filenames;
    let filenames_len = filenames.len();
    let readers: Vec<Box<dyn Read>> =
        if filenames_len == 0 || (filenames_len == 1 && filenames[0].as_os_str() == "-") {
            vec![Box::new(io::stdin().lock())]
        } else {
            let mut bufs: Vec<Box<dyn Read>> = Vec::with_capacity(filenames_len);
            for file in &filenames {
                bufs.push(Box::new(std::fs::File::open(file)?))
            }
            bufs
        };

    // Process each file
    for file in readers {
        let reader = io::BufReader::new(file);

        let parse_option;

        if let Some(bytes_list) = &args.bytes {
            let ranges: Vec<(i32, i32)> =
                read_range(bytes_list).map_err(|err| Box::new(Error::other(err)))?;

            parse_option = ParseVariat::Bytes(ranges);
        } else if let Some(characters_list) = &args.characters {
            let ranges: Vec<(i32, i32)> =
                read_range(characters_list).map_err(|err| Box::new(Error::other(err)))?;

            parse_option = ParseVariat::Characters(ranges);
        } else if let Some(fields_list) = &args.fields {
            let ranges: Vec<(i32, i32)> =
                read_range(fields_list).map_err(|err| Box::new(Error::other(err)))?;
            parse_option = ParseVariat::Fields(ranges);
        } else {
            return Err(Box::new(Error::other("Invalid arguments")));
        }

        for line in reader.lines() {
            let line = line?;
            match parse_option.clone() {
                ParseVariat::Bytes(ranges) => {
                    let bytes = cut_bytes(line.as_bytes(), args.delimiter, &ranges, args.no_split);
                    match String::from_utf8(bytes) {
                        Ok(string) => println!("{}", string),
                        Err(e) => eprintln!("Conversion error to string: {}", e),
                    }
                }
                ParseVariat::Characters(ranges) => {
                    println!("{}", cut_characters(&line, args.delimiter, &ranges))
                }
                ParseVariat::Fields(ranges) => {
                    if let Some(delim) = args.delimiter {
                        let result = cut_fields(&line, delim, &ranges, args.suppress);
                        if !result.1 {
                            println!("{}", result.0)
                        }
                    } else {
                        println!("{}", line);
                    }
                }
            }
        }
    }
    Ok(())
}

fn read_range(line: &str) -> Result<Vec<(i32, i32)>, String> {
    let ranges: Vec<&str> = line.split(',').collect();
    for range in &ranges {
        if *range == "-" {
            return Err("Invalid range, no endpopint".to_string());
        }
    }
    let res: Result<Vec<(i32, i32)>, String> = ranges
        .iter()
        .map(|range| {
            let nums: Vec<&str> = range.split('-').collect();

            let start = if nums[0].is_empty() {
                0
            } else {
                match nums[0].parse::<i32>() {
                    Err(err) => return Err(err.to_string()),
                    Ok(num) => num - 1,
                }
            };

            let end = if range.len() == 1 {
                start
            } else if nums[1].is_empty() {
                i32::MAX - 1
            } else {
                match nums[1].parse::<i32>() {
                    Err(err) => return Err(err.to_string()),
                    Ok(num) => num - 1,
                }
            };
            Ok((start, end))
        })
        .collect();
    let mut ranges = res?;
    for (start, end) in &ranges {
        if start > end {
            // Invalid range
            return Err("Invalid decreasing range".to_string());
        }
        if *start < 0 {
            return Err("Invalid start position (from 0)".to_string());
        }
    }
    ranges.sort_by(|a, b| a.0.cmp(&b.0));
    let mut filtered_ranges = Vec::new();

    // Filter the ranges, leaving only those that are not fully included in the others
    for &current in &ranges {
        if !ranges
            .iter()
            .any(|&other| other.0 <= current.0 && other.1 >= current.1 && other != current)
        {
            filtered_ranges.push(current);
        }
    }

    let mut merged_ranges = Vec::new();
    let mut current_range = filtered_ranges[0];

    for &next_range in &filtered_ranges[1..] {
        if next_range.0 <= current_range.1 {
            // Ranges overlap, update current range
            current_range = (current_range.0, next_range.1.max(current_range.1));
        } else {
            // Ranges do not overlap, add the current range to the merged ranges
            merged_ranges.push(current_range);
            current_range = next_range;
        }
    }

    // Add the last merged range
    merged_ranges.push(current_range);

    Ok(merged_ranges)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    if let Err(err) = cut_files(args) {
        exit_code = 1;
        eprintln!("{}", err);
    }

    std::process::exit(exit_code)
}
