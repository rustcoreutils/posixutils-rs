//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - err on line num == 0
//

extern crate clap;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use regex::Regex;
use std::fs::{self, File, OpenOptions};
use std::io::{self, BufRead, Error, ErrorKind, Read, Write};
use std::path::PathBuf;

/// csplit - split files based on context
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Name the created files prefix 00, prefix 01, ..., prefixn.
    #[arg(short = 'f', long, default_value = "xx")]
    prefix: String,

    /// Leave previously created files intact. By default, csplit shall remove created files if an error occurs.
    #[arg(short, long, default_value_t = false)]
    keep: bool,

    /// Use number decimal digits to form filenames for the file pieces.
    #[arg(short, long, default_value_t = 2)]
    num: u8,

    /// Suppress the output of file size messages.
    #[arg(short, long, default_value_t = false)]
    suppress: bool,

    /// File to read as input.
    filename: PathBuf,

    /// Operands defining context on which to split.
    operands: Vec<String>,
}

#[derive(Debug)]
enum Operand {
    Rx(Regex, isize, bool),
    LineNum(usize),
    Repeat(usize),
}

#[derive(Debug)]
struct SplitOps {
    ops: Vec<Operand>,
}

/// A structure that presents settings for creating files
struct OutputState {
    /// A String representing the prefix to be added to file name.
    prefix: String,
    /// An usize representing the current line number in the file reader.
    in_line_no: usize,
    ///  A String representing the suffix to be added to file name.
    suffix: String,
    /// A u32 representing the length of the suffix.
    suffix_len: u8,
    /// representing an output file if set to None, the output stream is not redirected to any file.
    outf: Option<File>,
}

impl OutputState {
    fn new(prefix: &str, suffix_len: u8) -> OutputState {
        OutputState {
            prefix: String::from(prefix),
            in_line_no: 1,
            suffix: String::new(),
            suffix_len,
            outf: None,
        }
    }

    /// Increments the suffix of the output filename.
    ///
    /// This function increments the suffix of the output filename in numeric order, starting from "00"
    /// and incrementing by 1 each time until the maximum value specified by `suffix_len` is reached.
    /// If the maximum suffix is reached, an error is returned.
    ///
    /// # Arguments
    ///
    /// * `self` - A mutable reference to the `OutputState` struct.
    ///
    /// # Returns
    ///
    /// * `Result<(), &'static str>` - `Ok(())` if the suffix is successfully incremented, otherwise an error message.
    ///
    /// # Panics
    ///
    /// This function will panic if `suffix_len` is less than 2, as the minimum suffix length is 2.
    ///
    fn incr_suffix(&mut self) -> Result<(), &'static str> {
        assert!(self.suffix_len >= 2); // Minimum suffix length is 2

        if self.suffix.is_empty() {
            self.suffix = format!("{:01$}", 0, self.suffix_len as usize);
            return Ok(());
        }

        let mut number: u32 = self.suffix.parse().unwrap_or(0);
        number += 1;

        if number >= 10_u32.pow(self.suffix_len.into()) {
            return Err("maximum suffix reached");
        }

        self.suffix = format!("{:01$}", number, self.suffix_len as usize);
        Ok(())
    }

    /// Opens the output file for writing.
    ///
    /// This function opens the output file for writing. If the output file is already open, it does nothing.
    /// Otherwise, it increments the suffix of the output filename and creates a new file with the updated filename.
    ///
    /// # Arguments
    ///
    /// * `self` - A mutable reference to the `OutputState` struct.
    ///
    /// # Returns
    ///
    /// * `io::Result<()>` - `Ok(())` if the output file is successfully opened or already open, otherwise an error indicating the failure to open the file.
    ///
    /// # Errors
    ///
    /// Returns an error if there is a problem creating or opening the output file.
    ///
    fn open_output(&mut self) -> io::Result<String> {
        if self.outf.is_some() {
            return Ok(String::new());
        }

        let inc_res = self.incr_suffix();
        if let Err(e) = inc_res {
            return Err(Error::new(ErrorKind::Other, e));
        }

        let out_fn = format!("{}{}", self.prefix, self.suffix);
        let f = OpenOptions::new()
            .read(false)
            .write(true)
            .create(true)
            .truncate(true)
            .open(&out_fn)?;
        self.outf = Some(f);

        Ok(out_fn)
    }

    fn close_output(&mut self) {
        if self.outf.is_some() {
            self.outf = None;
        }
    }
}

/// Splits a file based on specified conditions.
///
/// This function splits a file based on the provided splitting options and writes
/// the resulting parts to separate output files. It reads the input file line by
/// line, applies the splitting options to determine where to split the file, and
/// writes the parts to output files accordingly.
///
/// # Arguments
///
/// * `args` - The arguments specifying the file to split and the splitting options.
/// * `ctx` - The context containing the splitting operations.
/// * `new_files` - A mutable reference to a vector containing the names of the new output files.
///
/// # Returns
///
/// * `io::Result<()>` - `Ok(())` if the file is successfully split and the parts are written to output files,
///   or an `io` error.
///
fn csplit_file(args: &Args, ctx: SplitOps, new_files: &mut Vec<String>) -> io::Result<()> {
    let mut split_options = ctx.ops;
    // open file, or stdin
    let file: Box<dyn Read> = {
        if args.filename == PathBuf::from("-") {
            Box::new(io::stdin().lock())
        } else {
            Box::new(fs::File::open(&args.filename)?)
        }
    };
    let mut state = OutputState::new(&args.prefix, args.num);
    let mut reader = io::BufReader::new(file);

    let mut lines = String::new();

    loop {
        let mut line = String::new();
        let n_read = reader.read_line(&mut line)?;
        if n_read == 0 {
            process_lines(&mut lines, &mut state, new_files, args.suppress)?;
            break;
        }

        if split_options.is_empty() {
            lines.push_str(&line);
            continue;
        }
        match split_options.first().unwrap() {
            Operand::LineNum(num) => {
                if *num == state.in_line_no {
                    if lines.ends_with('\n') && lines != "\n" {
                        lines.pop();
                    }
                    process_lines(&mut lines, &mut state, new_files, args.suppress)?;
                    state.in_line_no = 1;
                    lines = String::new();
                    lines.push_str(&line);

                    if split_options.len() > 1 {
                        if let Operand::Repeat(repeat) = &mut split_options[1] {
                            *repeat -= 1;
                            if *repeat == 0 {
                                split_options.remove(0);
                                split_options.remove(0);
                            }
                        }
                    }
                } else {
                    lines.push_str(&line);
                }
            }
            Operand::Rx(regex, offset, skip) => {
                if line == "\n" {
                    line = String::new();
                }
                if regex.is_match(&line) {
                    match offset.cmp(&0) {
                        std::cmp::Ordering::Less => {
                            let mut lines_vec: Vec<&str> = lines.lines().collect();

                            let mut removed_lines_string = String::new();
                            let length = lines_vec.len();
                            if length >= offset.unsigned_abs() {
                                let removed_lines =
                                    lines_vec.split_off(length - offset.unsigned_abs());
                                removed_lines_string = removed_lines.join("\n");

                                removed_lines_string.push('\n');
                            }

                            lines = lines_vec.join("\n");
                            if !lines.is_empty() {
                                if *skip {
                                    lines.clear();
                                } else {
                                    if lines.ends_with('\n') {
                                        lines.pop();
                                    }
                                    process_lines(
                                        &mut lines,
                                        &mut state,
                                        new_files,
                                        args.suppress,
                                    )?;
                                }
                            }

                            lines = removed_lines_string;

                            if line.is_empty() {
                                lines.push('\n');
                            } else {
                                lines.push_str(&line);
                            }
                        }
                        std::cmp::Ordering::Equal => {
                            if *skip {
                                lines.clear();
                                if line.is_empty() {
                                    line = "\n".to_string();
                                }
                                lines.push_str(&line);
                            } else {
                                if lines.ends_with('\n') {
                                    lines.pop();
                                }
                                process_lines(&mut lines, &mut state, new_files, args.suppress)?;

                                if line.is_empty() {
                                    lines = "\n".to_string();
                                } else {
                                    lines = line;
                                }
                            }
                        }
                        std::cmp::Ordering::Greater => {
                            if line.is_empty() {
                                lines.push('\n');
                            } else {
                                lines.push_str(&line);
                            }
                            for _ in 0..*offset - 1 {
                                let mut new_line = String::new();
                                let n_read = reader.read_line(&mut new_line)?;
                                if n_read == 0 {
                                    process_lines(
                                        &mut lines,
                                        &mut state,
                                        new_files,
                                        args.suppress,
                                    )?;
                                    break;
                                }
                                lines.push_str(&new_line);
                            }

                            if *skip {
                                lines.clear();
                            } else {
                                if lines.ends_with('\n') {
                                    lines.pop();
                                }
                                process_lines(&mut lines, &mut state, new_files, args.suppress)?;
                                lines = String::new();
                            }
                        }
                    }

                    match split_options.len() {
                        1 => {
                            split_options.remove(0);
                        }
                        us if us > 1 => {
                            if let Operand::Repeat(repeat) = &mut split_options[1] {
                                *repeat -= 1;
                                if *repeat == 0 {
                                    split_options.remove(0);
                                    split_options.remove(0);
                                }
                            } else {
                                split_options.remove(0);
                            }
                        }
                        _ => {}
                    }
                } else {
                    if line.is_empty() {
                        line = "\n".to_string();
                    }
                    lines.push_str(&line);
                }
            }
            _ => {}
        }

        state.in_line_no += 1;
    }

    Ok(())
}

/// Processes lines and writes them to output.
///
/// This function is responsible for processing lines and writing them to the output
/// according to the specified conditions. It opens the output stream, writes the lines
/// to a file, adds the file name to the list of new files, and closes the output stream.
///
/// # Arguments
///
/// * `lines` - The lines to be processed and written.
/// * `state` - The current output state.
/// * `new_files` - The list of new files.
///
/// # Returns
///
/// * `io::Result<()>` - `Ok(())` if the lines are successfully processed and written to output,
///   or an `io` error.
///
fn process_lines(
    lines: &mut String,
    state: &mut OutputState,
    new_files: &mut Vec<String>,
    suppress: bool,
) -> io::Result<()> {
    let file_name = state.open_output()?;
    state.outf.as_mut().unwrap().write_all(lines.as_bytes())?;
    new_files.push(file_name);
    if !suppress {
        println!("{}\n", lines.len());
    }

    state.close_output();
    Ok(())
}

/// Finds the position of the delimiter in the input string, or None if the delimiter is not found.
///
/// # Arguments
///
/// * `s` - The input string to search in.
/// * `delim` - The character to search for.
///
/// # Returns
///
/// * `Option<usize>` - Some(position) if the delimiter is found, None otherwise.
///
/// ```
fn escaped_end_pos(s: &str, delim: char) -> Option<usize> {
    let mut first = true;
    let mut escaped = false;
    for (i, ch) in s.chars().enumerate() {
        if first {
            if ch != delim {
                return None;
            }
            first = false;
        } else if escaped {
            escaped = false;
        } else if ch == '\\' {
            escaped = true;
        } else if ch == delim {
            return Some(i);
        }
    }

    None
}

/// Parses an operation string of the form `/regex/offset` or `%regex/offset` or `{n}` or `1..9`
///
/// # Arguments
///
/// * `opstr` - The string to parse
/// * `delim` - The character that indicates the start of the operation string. If it is `'%'`, the operation is in skip mode.
///
/// # Returns
///
/// * `Result<Operand, std::io::Error>` - `Ok(Operand)` if the operation string is parsed successfully, otherwise an error indicating the failure to parse the operation string.
fn parse_op_rx(opstr: &str, delim: char) -> io::Result<Operand> {
    // delimiter indicates skip-mode
    let is_skip = delim == '%';

    // find where regex string ends, and (optionally) offset begins
    let res = escaped_end_pos(opstr, delim);
    if res.is_none() {
        return Err(Error::new(ErrorKind::Other, "invalid regex str"));
    }

    // parse string sandwiched between two delimiter chars
    let end_pos = res.unwrap();
    let re_str = &opstr[1..end_pos];
    let res = Regex::new(re_str);
    if res.is_err() {
        return Err(Error::new(ErrorKind::Other, "invalid regex"));
    }
    let re = res.unwrap();

    // reference offset string
    let mut offset_str = &opstr[end_pos + 1..];

    // if empty, we are done
    if offset_str.is_empty() {
        return Ok(Operand::Rx(re, 0, is_skip));
    }

    // skip optional leading '+'
    if offset_str.starts_with('+') {
        offset_str = &opstr[end_pos + 2..];
    }

    // parse offset number, positive or negative
    match offset_str.parse::<isize>() {
        Ok(n) => Ok(Operand::Rx(re, n, is_skip)),
        Err(_e) => Err(Error::new(ErrorKind::Other, "invalid regex offset")),
    }
}

/// Parses a repeat operand from a string.
///
/// This function parses a repeat operand from the input string. The repeat operand is specified
/// within curly braces, indicating the number of times a certain pattern should be repeated.
///
/// # Arguments
///
/// * `opstr` - A string slice containing the operand to parse.
///
/// # Returns
///
/// * `io::Result<Operand>` - The parsed operand if successful, otherwise an error indicating
///   the failure to parse the operand.
///
/// # Errors
///
/// Returns an error if the input string does not match the expected format or if there is a
/// problem parsing the operand.
///
fn parse_op_repeat(opstr: &str) -> io::Result<Operand> {
    // a regex fully describes what must be parsed
    let re = Regex::new(r"^\{(\d*|[*])}$").unwrap();

    // grab and parse capture #1, if matched
    match re.captures(opstr) {
        None => {}
        Some(caps) => {
            let numstr = caps.get(1).unwrap().as_str();
            if numstr == "*" {
                return Ok(Operand::Repeat(usize::MAX));
            }
            match numstr.parse::<usize>() {
                Ok(n) => return Ok(Operand::Repeat(n)),
                Err(_e) => {}
            }
        }
    }

    // error cases fall through to here
    Err(Error::new(ErrorKind::Other, "invalid repeating operand"))
}

/// Parses a line number operand from a string.
///
/// This function parses a line number operand from the input string. The line number operand
/// specifies a simple positive integer indicating the line number at which to perform a split.
///
/// # Arguments
///
/// * `opstr` - A string slice containing the operand to parse.
///
/// # Returns
///
/// * `io::Result<Operand>` - The parsed operand if successful, otherwise an error indicating
///   the failure to parse the operand.
///
/// # Errors
///
/// Returns an error if the input string cannot be parsed as a positive integer or if there is
/// a problem parsing the operand.
///
fn parse_op_linenum(opstr: &str) -> io::Result<Operand> {
    // parse simple positive integer
    match opstr.parse::<usize>() {
        Ok(n) => Ok(Operand::LineNum(n)),
        Err(e) => {
            let msg = format!("{}", e);
            Err(Error::new(ErrorKind::Other, msg))
        }
    }
}

/// Parses operands from command-line arguments.
///
/// This function parses operands from the command-line arguments provided in the `Args` struct.
/// It iterates over each operand string, determines its type based on the first character,
/// and delegates parsing to specialized functions for regex patterns, line numbers, or repeats.
///
/// # Arguments
///
/// * `args` - A reference to the `Args` struct containing the command-line arguments.
///
/// # Returns
///
/// * `io::Result<SplitOps>` - The parsed operands wrapped in a `SplitOps` struct if successful,
///   otherwise an error indicating the failure to parse the operands.
///
/// # Errors
///
/// Returns an error if any of the operand strings are invalid or if there is a problem parsing
/// the operands.
///
fn parse_operands(args: &Args) -> io::Result<SplitOps> {
    let mut ops = Vec::new();

    for opstr in &args.operands {
        let first_ch = opstr.chars().next().unwrap();

        let op = {
            match first_ch {
                '/' => parse_op_rx(opstr, '/')?,
                '%' => parse_op_rx(opstr, '%')?,
                '{' => parse_op_repeat(opstr)?,
                '1'..='9' => parse_op_linenum(opstr)?,
                _ => return Err(Error::new(ErrorKind::Other, "invalid operand")),
            }
        };

        ops.push(op);
    }

    Ok(SplitOps { ops })
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let ctx = parse_operands(&args)?;

    let mut exit_code = 0;
    let mut new_files = vec![];
    if let Err(err) = csplit_file(&args, ctx, &mut new_files) {
        exit_code = 1;
        eprintln!("{}: {}", args.filename.display(), err);
        if !args.keep {
            for file_name in new_files.iter() {
                fs::remove_file(file_name).unwrap();
            }
        }
    }

    std::process::exit(exit_code)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_escaped_end_pos() {
        // Test with escape characters
        assert_eq!(escaped_end_pos("/def/", '/'), Some(4));

        // Test with multiple escape characters
        assert_eq!(escaped_end_pos("/defabc\\\\/", '/'), Some(9));

        // Test with no delimiter found
        assert_eq!(escaped_end_pos("abcdef", '/'), None);

        // Test with delimiter at the beginning of the string
        assert_eq!(escaped_end_pos("/abcdef", '/'), None);

        // Test with empty string
        assert_eq!(escaped_end_pos("", '/'), None);
    }

    #[test]
    fn test_incr_suffix() {
        // Test incrementing suffix with length 2
        let mut state = OutputState::new("prefix", 2);
        assert_eq!(state.suffix, "");

        for i in 0..12 {
            assert!(state.incr_suffix().is_ok());
            assert_eq!(state.suffix, format!("{:02}", i));
        }

        // Test incrementing suffix with length 3
        let mut state = OutputState::new("prefix", 3);
        assert_eq!(state.suffix, "");

        for i in 0..12 {
            assert!(state.incr_suffix().is_ok());
            assert_eq!(state.suffix, format!("{:03}", i));
        }
    }

    #[test]
    fn test_parse_op_rx_valid_without_offset() {
        let opstr = "/pattern/";
        let delim = '/';
        match parse_op_rx(opstr, delim) {
            Ok(Operand::Rx(regex, offset, is_skip)) => {
                assert_eq!(regex.as_str(), "pattern");
                assert_eq!(offset, 0);
                assert!(!is_skip);
            }
            _ => panic!("Expected Ok(Operand::Rx)"),
        }
    }

    #[test]
    fn test_parse_op_rx_valid_with_positive_offset() {
        let opstr = "/pattern/+3";
        let delim = '/';
        match parse_op_rx(opstr, delim) {
            Ok(Operand::Rx(regex, offset, is_skip)) => {
                assert_eq!(regex.as_str(), "pattern");
                assert_eq!(offset, 3);
                assert!(!is_skip);
            }
            _ => panic!("Expected Ok(Operand::Rx)"),
        }
    }

    #[test]
    fn test_parse_op_rx_valid_with_negative_offset() {
        let opstr = "/pattern/-2";
        let delim = '/';
        match parse_op_rx(opstr, delim) {
            Ok(Operand::Rx(regex, offset, is_skip)) => {
                assert_eq!(regex.as_str(), "pattern");
                assert_eq!(offset, -2);
                assert!(!is_skip);
            }
            _ => panic!("Expected Ok(Operand::Rx)"),
        }
    }

    #[test]
    fn test_parse_op_rx_valid_with_leading_plus() {
        let opstr = "/pattern/+5";
        let delim = '/';
        match parse_op_rx(opstr, delim) {
            Ok(Operand::Rx(regex, offset, is_skip)) => {
                assert_eq!(regex.as_str(), "pattern");
                assert_eq!(offset, 5);
                assert!(!is_skip);
            }
            _ => panic!("Expected Ok(Operand::Rx)"),
        }
    }

    #[test]
    fn test_parse_op_rx_valid_with_skip_mode() {
        let opstr = "%pattern%";
        let delim = '%';
        match parse_op_rx(opstr, delim) {
            Ok(Operand::Rx(regex, offset, is_skip)) => {
                assert_eq!(regex.as_str(), "pattern");
                assert_eq!(offset, 0);
                assert!(is_skip);
            }
            _ => panic!("Expected Ok(Operand::Rx)"),
        }
    }

    #[test]
    fn test_parse_op_rx_invalid() {
        let opstr = "/pattern";
        let delim = '/';
        match parse_op_rx(opstr, delim) {
            Err(e) => {
                assert_eq!(e.kind(), ErrorKind::Other);
                assert_eq!(e.to_string(), "invalid regex str");
            }
            _ => panic!("Expected Err"),
        }
    }

    #[test]
    fn test_parse_op_repeat_valid() {
        let opstr = "{5}";
        match parse_op_repeat(opstr) {
            Ok(Operand::Repeat(n)) => assert_eq!(n, 5),
            _ => panic!("Expected Ok(Operand::Repeat)"),
        }
    }

    #[test]
    fn test_parse_op_repeat_invalid_non_numeric() {
        let opstr = "{abc}";
        match parse_op_repeat(opstr) {
            Err(e) => {
                assert_eq!(e.kind(), ErrorKind::Other);
                assert_eq!(e.to_string(), "invalid repeating operand");
            }
            _ => panic!("Expected Err"),
        }
    }

    #[test]
    fn test_parse_op_repeat_invalid_missing_braces() {
        let opstr = "5";
        match parse_op_repeat(opstr) {
            Err(e) => {
                assert_eq!(e.kind(), ErrorKind::Other);
                assert_eq!(e.to_string(), "invalid repeating operand");
            }
            _ => panic!("Expected Err"),
        }
    }

    #[test]
    fn test_parse_op_linenum_valid() {
        let opstr = "10";
        match parse_op_linenum(opstr) {
            Ok(Operand::LineNum(n)) => assert_eq!(n, 10),
            _ => panic!("Expected Ok(Operand::LineNum)"),
        }
    }

    #[test]
    fn test_parse_op_linenum_invalid_non_numeric() {
        let opstr = "abc";
        match parse_op_linenum(opstr) {
            Err(e) => {
                assert_eq!(e.kind(), ErrorKind::Other);
                assert_eq!(e.to_string(), "invalid digit found in string");
            }
            _ => panic!("Expected Err"),
        }
    }

    #[test]
    fn test_parse_operands() {
        // Test valid operands
        let args = Args {
            prefix: String::from("xx"),
            keep: false,
            num: 2,
            suppress: false,
            filename: PathBuf::from("test.txt"),
            operands: vec![
                String::from("/pattern/+1"),
                String::from("%skip%10"),
                String::from("15"),
                String::from("{3}"),
            ],
        };

        match parse_operands(&args) {
            Ok(ops) => {
                assert_eq!(ops.ops.len(), 4);
                match &ops.ops[0] {
                    Operand::Rx(re, offset, _) => {
                        assert_eq!(re.as_str(), "pattern");
                        assert_eq!(*offset, 1);
                    }
                    _ => panic!("Expected Operand::Rx"),
                }
                match &ops.ops[1] {
                    Operand::Rx(re, offset, _) => {
                        assert_eq!(re.as_str(), "skip");
                        assert_eq!(*offset, 10);
                    }
                    _ => panic!("Expected Operand::Rx"),
                }
                match &ops.ops[2] {
                    Operand::LineNum(n) => assert_eq!(*n, 15),
                    _ => panic!("Expected Operand::LineNum"),
                }
                match &ops.ops[3] {
                    Operand::Repeat(n) => assert_eq!(*n, 3),
                    _ => panic!("Expected Operand::Repeat"),
                }
            }
            _ => panic!("Expected Ok(SplitOps)"),
        }
    }

    #[test]
    fn test_split_text_file() {
        // Test valid operands
        let args = Args {
            prefix: String::from("txt"),
            keep: false,
            num: 2,
            suppress: false,
            filename: PathBuf::from("tests/assets/test_file.txt"),
            operands: vec![String::from("5"), String::from("{3}")],
        };

        let ctx = parse_operands(&args).unwrap();
        let mut new_files = vec![];
        match csplit_file(&args, ctx, &mut new_files) {
            Ok(_) => {}
            Err(e) => {
                if !args.keep {
                    for file_name in new_files.iter() {
                        fs::remove_file(file_name).unwrap();
                    }
                }
                panic!("{e}");
            }
        }

        let mut file = File::open("txt00").unwrap();

        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        let expected = String::from("1sdfghnm\n2sadsgdhjmf\n3zcxbncvm vbm\n4asdbncv");

        assert_eq!(contents, expected);

        let mut file = File::open("txt03").unwrap();

        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        let expected = String::from("13\n14\n15\n16\n17");

        assert_eq!(contents, expected);

        fs::remove_file("txt00").unwrap();
        fs::remove_file("txt01").unwrap();
        fs::remove_file("txt02").unwrap();
        fs::remove_file("txt03").unwrap();
    }

    #[test]
    fn test_split_c_file_1() {
        // Test valid operands
        let args = Args {
            prefix: String::from("c_file"),
            keep: false,
            num: 2,
            suppress: false,
            filename: PathBuf::from("tests/assets/test_file_c"),
            operands: vec![
                String::from(r"%main\(%"),
                String::from("/^}/+1"),
                String::from("{3}"),
            ],
        };

        let ctx = parse_operands(&args).unwrap();

        let mut new_files = vec![];
        match csplit_file(&args, ctx, &mut new_files) {
            Ok(_) => {}
            Err(e) => {
                if !args.keep {
                    for file_name in new_files.iter() {
                        fs::remove_file(file_name).unwrap();
                    }
                }
                panic!("{e}");
            }
        }

        let mut file = File::open("c_file00").unwrap();

        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        let expected =
            String::from("int main() {\n    printf(\"Hello, world!\\n\");\n    return 0;\n}");

        assert_eq!(contents, expected);

        let mut file = File::open("c_file03").unwrap();

        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        let expected =
            String::from("\nvoid func3() {\n    printf(\"This is function 3\\n\");\n}\n");

        assert_eq!(contents, expected);

        fs::remove_file("c_file00").unwrap();
        fs::remove_file("c_file01").unwrap();
        fs::remove_file("c_file02").unwrap();
        fs::remove_file("c_file03").unwrap();
    }

    #[test]
    fn test_split_c_file_2() {
        // Test valid operands
        let args = Args {
            prefix: String::from("c_file_2_"),
            keep: false,
            num: 2,
            suppress: false,
            filename: PathBuf::from("tests/assets/test_file_c"),
            operands: vec![
                String::from(r"%main\(%+1"),
                String::from("/^}/+1"),
                String::from("{3}"),
            ],
        };

        let ctx = parse_operands(&args).unwrap();

        let mut new_files = vec![];
        match csplit_file(&args, ctx, &mut new_files) {
            Ok(_) => {}
            Err(e) => {
                if !args.keep {
                    for file_name in new_files.iter() {
                        fs::remove_file(file_name).unwrap();
                    }
                }
                panic!("{e}");
            }
        }

        let mut file = File::open("c_file_2_00").unwrap();

        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        let expected = String::from("    printf(\"Hello, world!\\n\");\n    return 0;\n}");

        assert_eq!(contents, expected);

        let mut file = File::open("c_file_2_03").unwrap();

        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        let expected =
            String::from("\nvoid func3() {\n    printf(\"This is function 3\\n\");\n}\n");

        assert_eq!(contents, expected);

        fs::remove_file("c_file_2_00").unwrap();
        fs::remove_file("c_file_2_01").unwrap();
        fs::remove_file("c_file_2_02").unwrap();
        fs::remove_file("c_file_2_03").unwrap();
    }

    #[test]
    fn test_split_c_file_3() {
        // Test valid operands
        let args = Args {
            prefix: String::from("c_file_3_"),
            keep: false,
            num: 2,
            suppress: false,
            filename: PathBuf::from("tests/assets/test_file_c"),
            operands: vec![
                String::from(r"%main\(%-1"),
                String::from("/^}/+1"),
                String::from("{3}"),
            ],
        };

        let ctx = parse_operands(&args).unwrap();

        let mut new_files = vec![];
        match csplit_file(&args, ctx, &mut new_files) {
            Ok(_) => {}
            Err(e) => {
                if !args.keep {
                    for file_name in new_files.iter() {
                        fs::remove_file(file_name).unwrap();
                    }
                }
                panic!("{e}");
            }
        }

        let mut file = File::open("c_file_3_00").unwrap();

        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        let expected =
            String::from("\nint main() {\n    printf(\"Hello, world!\\n\");\n    return 0;\n}");

        assert_eq!(contents, expected);

        let mut file = File::open("c_file_3_03").unwrap();

        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        let expected =
            String::from("\nvoid func3() {\n    printf(\"This is function 3\\n\");\n}\n");

        assert_eq!(contents, expected);

        fs::remove_file("c_file_3_00").unwrap();
        fs::remove_file("c_file_3_01").unwrap();
        fs::remove_file("c_file_3_02").unwrap();
        fs::remove_file("c_file_3_03").unwrap();
    }

    #[test]
    fn test_split_c_file_4() {
        // Test valid operands
        let args = Args {
            prefix: String::from("c_file_4_"),
            keep: false,
            num: 2,
            suppress: false,
            filename: PathBuf::from("tests/assets/test_file_c"),
            operands: vec![
                String::from(r"%main\(%"),
                String::from("/^}/"),
                String::from("{3}"),
            ],
        };

        let ctx = parse_operands(&args).unwrap();

        let mut new_files = vec![];
        match csplit_file(&args, ctx, &mut new_files) {
            Ok(_) => {}
            Err(e) => {
                if !args.keep {
                    for file_name in new_files.iter() {
                        fs::remove_file(file_name).unwrap();
                    }
                }
                panic!("{e}");
            }
        }

        let mut file = File::open("c_file_4_00").unwrap();

        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        let expected =
            String::from("int main() {\n    printf(\"Hello, world!\\n\");\n    return 0;");

        assert_eq!(contents, expected);

        let mut file = File::open("c_file_4_03").unwrap();

        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        let expected =
            String::from("}\n\nvoid func3() {\n    printf(\"This is function 3\\n\");\n}\n");

        assert_eq!(contents, expected);

        fs::remove_file("c_file_4_00").unwrap();
        fs::remove_file("c_file_4_01").unwrap();
        fs::remove_file("c_file_4_02").unwrap();
        fs::remove_file("c_file_4_03").unwrap();
    }

    #[test]
    fn test_split_c_file_5() {
        // Test valid operands
        let args = Args {
            prefix: String::from("c_file_5_"),
            keep: false,
            num: 2,
            suppress: false,
            filename: PathBuf::from("tests/assets/test_file_c"),
            operands: vec![
                String::from(r"%main\(%"),
                String::from("/^}/-1"),
                String::from("{3}"),
            ],
        };

        let ctx = parse_operands(&args).unwrap();

        let mut new_files = vec![];
        match csplit_file(&args, ctx, &mut new_files) {
            Ok(_) => {}
            Err(e) => {
                if !args.keep {
                    for file_name in new_files.iter() {
                        fs::remove_file(file_name).unwrap();
                    }
                }
                panic!("{e}");
            }
        }

        let mut file = File::open("c_file_5_00").unwrap();

        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        let expected = String::from("int main() {\n    printf(\"Hello, world!\\n\");");

        assert_eq!(contents, expected);

        let mut file = File::open("c_file_5_03").unwrap();

        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        let expected =
            String::from("    printf(\"This is function 2\\n\");\n}\n\nvoid func3() {\n    printf(\"This is function 3\\n\");\n}\n");

        assert_eq!(contents, expected);

        fs::remove_file("c_file_5_00").unwrap();
        fs::remove_file("c_file_5_01").unwrap();
        fs::remove_file("c_file_5_02").unwrap();
        fs::remove_file("c_file_5_03").unwrap();
    }
}
