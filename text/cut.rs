//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, BufRead, Error, Write};

use clap::Parser;
use gettextrs::gettext;
use plib::io::input_stream_dashed;
use std::path::PathBuf;

/// cut - cut out selected fields of each line of a file
#[derive(Parser, Clone)]
#[command(version, about = gettext("cut - cut out selected fields of each line of a file"))]
struct Args {
    #[arg(short = 'b', long, help = gettext("Cut based on a list of bytes"))]
    bytes: Option<String>,

    #[arg(short = 'c', long, help = gettext("Cut based on a list of characters"))]
    characters: Option<String>,

    #[arg(short = 'f', long, help = gettext("Cut based on a list of fields"))]
    fields: Option<String>,

    #[arg(short = 'd', long, help = gettext("Set the field delimiter"))]
    delimiter: Option<char>,

    #[structopt(short = 's', long, help = gettext("Suppress lines with no delimiter characters"))]
    suppress: bool,

    #[structopt(short = 'n', help = gettext("Do not split characters"))]
    no_split: bool,

    #[arg(help = gettext("Input files"))]
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

/// Byte offsets at which each character of `line` begins under the current
/// `LC_CTYPE`, plus `line.len()` as a trailing sentinel. In the `C` locale every
/// byte begins a character, so `-n` becomes a no-op.
fn char_boundaries(line: &[u8]) -> Vec<usize> {
    let mut boundaries = Vec::new();
    let mut offset = 0;
    for ch in plib::locale::mb_char_slices(line) {
        boundaries.push(offset);
        offset += ch.len();
    }
    boundaries.push(line.len());
    boundaries
}

/// For `-b -n`: move byte index `i` left to the first byte of the character it
/// falls within.
fn snap_low(boundaries: &[usize], i: usize) -> usize {
    let pos = boundaries.partition_point(|&b| b <= i);
    boundaries[pos - 1]
}

/// For `-b -n`: ensure inclusive byte index `i` is the last byte of a character.
/// If it falls inside a character, back up to the last byte of the preceding
/// character; returns `None` if that drops the range entirely.
fn snap_high(boundaries: &[usize], i: usize, len: usize) -> Option<usize> {
    if i + 1 >= len || boundaries.binary_search(&(i + 1)).is_ok() {
        return Some(i);
    }
    snap_low(boundaries, i).checked_sub(1)
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
    let mut result = Vec::with_capacity(line.len());

    // Character boundaries are only needed (and computed) for -n.
    let boundaries = if n { char_boundaries(line) } else { Vec::new() };

    for (start, end) in ranges {
        let mut start = *start as usize;
        let mut end = *end as usize;

        if n {
            // Do not split multibyte characters: snap the low byte to a
            // character start and the high byte to a character end; drop the
            // range if that leaves it empty.
            start = snap_low(&boundaries, start);
            match snap_high(&boundaries, end, line.len()) {
                Some(e) if e >= start => end = e,
                _ => continue,
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
    // Split on the raw delimiter character (each occurrence is significant).
    let mut fields: Vec<&str> = line.split(delim).collect();

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
    // Usage errors abort before any processing.
    validate_args(&args).map_err(|err| Box::new(Error::other(err)))?;

    // Parse the byte/character/field list once.
    let parse_option = if let Some(bytes_list) = &args.bytes {
        ParseVariat::Bytes(read_range(bytes_list).map_err(|err| Box::new(Error::other(err)))?)
    } else if let Some(characters_list) = &args.characters {
        ParseVariat::Characters(
            read_range(characters_list).map_err(|err| Box::new(Error::other(err)))?,
        )
    } else if let Some(fields_list) = &args.fields {
        ParseVariat::Fields(read_range(fields_list).map_err(|err| Box::new(Error::other(err)))?)
    } else {
        return Err(Box::new(Error::other("Invalid arguments")));
    };

    // For -f, the default delimiter is <tab> when -d is not given.
    let field_delim = args.delimiter.unwrap_or('\t');

    // No operands read stdin; otherwise each operand is processed in order, with
    // a "-" reading stdin at its position (POSIX Guideline 13). A file that
    // cannot be opened is diagnosed and processing continues (only the exit
    // status is affected).
    let sources: Vec<PathBuf> = if args.filenames.is_empty() {
        vec![PathBuf::from("-")]
    } else {
        args.filenames.clone()
    };

    let mut stdout = io::stdout().lock();
    for source in &sources {
        let mut reader = match input_stream_dashed(source) {
            Ok(r) => io::BufReader::new(r),
            Err(e) => {
                plib::diag::error(&format!("{}: {}", source.display(), e));
                continue;
            }
        };

        // Read lines as raw bytes so -b can select arbitrary bytes; -c/-f
        // interpret the bytes as text (lossily for non-UTF-8 input).
        let mut buf: Vec<u8> = Vec::new();
        loop {
            buf.clear();
            match reader.read_until(b'\n', &mut buf) {
                Ok(0) => break,
                Ok(_) => {}
                Err(e) => {
                    plib::diag::error(&format!("{}: {}", source.display(), e));
                    break;
                }
            }
            if buf.last() == Some(&b'\n') {
                buf.pop();
            }

            match &parse_option {
                ParseVariat::Bytes(ranges) => {
                    // Write the selected bytes verbatim: they need not be valid
                    // characters, so do not route through a UTF-8 string.
                    let mut bytes = cut_bytes(&buf, args.delimiter, ranges, args.no_split);
                    bytes.push(b'\n');
                    stdout.write_all(&bytes)?;
                }
                ParseVariat::Characters(ranges) => {
                    let line = String::from_utf8_lossy(&buf);
                    writeln!(stdout, "{}", cut_characters(&line, args.delimiter, ranges))?;
                }
                ParseVariat::Fields(ranges) => {
                    let line = String::from_utf8_lossy(&buf);
                    let (result, skip) = cut_fields(&line, field_delim, ranges, args.suppress);
                    if !skip {
                        writeln!(stdout, "{}", result)?;
                    }
                }
            }
        }
    }
    Ok(())
}

fn read_range(line: &str) -> Result<Vec<(i32, i32)>, String> {
    // A list is comma- or blank-separated (POSIX).
    let ranges: Vec<&str> = line
        .split([',', ' ', '\t'])
        .filter(|s| !s.is_empty())
        .collect();
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

            let end = if nums.len() == 1 {
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
    ranges.sort_by_key(|a| a.0);
    // Upper bound: all ranges could be non-overlapping
    let mut filtered_ranges = Vec::with_capacity(ranges.len());

    // Filter the ranges, leaving only those that are not fully included in the others
    for &current in &ranges {
        if !ranges
            .iter()
            .any(|&other| other.0 <= current.0 && other.1 >= current.1 && other != current)
        {
            filtered_ranges.push(current);
        }
    }

    // Upper bound: all filtered ranges could be non-overlapping
    let mut merged_ranges = Vec::with_capacity(filtered_ranges.len());
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
    plib::diag::init_locale("cut");

    let args = Args::parse();

    if let Err(err) = cut_files(args) {
        plib::diag::error(&format!("{}", err));
    }

    std::process::exit(plib::diag::exit_status())
}
