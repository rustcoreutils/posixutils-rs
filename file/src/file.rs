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
    io::{self, BufRead, BufReader, ErrorKind, Read, Seek, SeekFrom},
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

#[derive(Debug)]
enum ComparisonOperator {
    Equal,
    LessThan,
    GreaterThan,
    AllSet,
    AnyUnset,
    FileLargeEnough,
}

#[derive(Debug)]
pub enum Value {
    String(String),
    Number(ComparisonOperator, u64),
}

impl Value {
    pub fn parse(mut input: String, _type: Type) -> Result<Value, RawMagicLineParseError> {
        match _type {
            Type::String => {
                let mut result = String::new();
                let mut chars = input.chars();

                while let Some(c) = chars.next() {
                    if c == '\\' {
                        if let Some(escaped) = chars.next() {
                            let replacement = match escaped {
                                '\\' => '\\',
                                'a' => '\x07', // Alert (bell)
                                'b' => '\x08', // Backspace
                                'f' => '\x0C', // Form feed
                                'n' => '\n',   // Newline
                                'r' => '\r',   // Carriage return
                                't' => '\t',   // Horizontal tab
                                'v' => '\x0B', // Vertical tab
                                ' ' => ' ',    // Space
                                _ => escaped,  // Treat any other character as itself
                            };
                            result.push(replacement);
                        } else {
                            result.push('\\');
                        }
                    } else {
                        result.push(c);
                    }
                }
                Ok(Value::String(result))
            }
            _ => {
                let (comp, num) =
                    Self::parse_number(&mut input).ok_or(RawMagicLineParseError::InvalidValue)?;
                Ok(Value::Number(comp, num))
            }
        }
    }

    fn parse_number(input: &mut String) -> Option<(ComparisonOperator, u64)> {
        let regex = Regex::new(r"^[=<>&^x]").unwrap();
        let comparision_op = match regex.find(&input) {
            Some(mat) => {
                let comp = mat.as_str().chars().next().unwrap();
                input.replace_range(..1, ""); // Remove the matched operator
                match comp {
                    '=' => ComparisonOperator::Equal,
                    '<' => ComparisonOperator::LessThan,
                    '>' => ComparisonOperator::GreaterThan,
                    '&' => ComparisonOperator::AllSet,
                    '^' => ComparisonOperator::AnyUnset,
                    'x' => ComparisonOperator::FileLargeEnough,
                    _ => return None,
                }
            }
            None => ComparisonOperator::Equal,
        };

        let num = parse_number(input)?;
        Some((comparision_op, num))
    }
}

fn test_file_against_magic_db_file(
    tf_reader: &mut BufReader<File>,
    raw_magic_file_lines: &Vec<RawMagicFileLine>,
) -> Result<String, Box<dyn std::error::Error>> {
    let mut value = String::new();

    for raw_magic_file_line in raw_magic_file_lines {
        raw_magic_file_line.test(tf_reader);
        tf_reader.rewind();
    }

    Ok(value)
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

    /// Indicates that a the value field is invalid
    InvalidValue,
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
                RawMagicLineParseError::InvalidValue => "Invalid value!",
            }
        )
    }
}

/// It removes parsed data from input string
/// if the parsing was successful
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
///
/// It explains the type of hold by the "value" field
#[derive(Debug, Clone, Copy)]
enum Type {
    Decimal(u64, Option<u64>),
    Unsigned(u64, Option<u64>),
    String,
}

impl Type {
    pub fn parse(mut input: String) -> Result<Type, RawMagicLineParseError> {
        Self::replace_verbose_type_string_with_short_ones(&mut input);

        let tsc = Self::parse_type_specification_char(&mut input)
            .ok_or(RawMagicLineParseError::InvalidTypeFormat)?;

        let _type = match tsc {
            'd' | 'u' => {
                // parse decimal and unsigned type
                let no_of_bytes = Self::parse_no_of_bytes_represented(&mut input)?;
                println!("{input}");
                let mask = Self::parse_mask(&mut input);
                if tsc == 'u' {
                    Ok(Type::Decimal(no_of_bytes, mask))
                } else {
                    Ok(Type::Unsigned(no_of_bytes, mask))
                }
            }
            's' => Ok(Type::String),
            _ => return Err(RawMagicLineParseError::InvalidTypeFormat),
        };

        // the input string should be empty after all the steps being followed
        if input.len() != 0 {
            return Err(RawMagicLineParseError::InvalidTypeFormat);
        }

        _type
    }

    /// As strings "byte", "short", "long" and "string" can also be represented by dC, dS, dL and s
    /// So, we'll replace the verbose ones with short ones
    fn replace_verbose_type_string_with_short_ones(input: &mut String) {
        if input.starts_with("byte") {
            input.replace_range(0..4, "dC");
        } else if input.starts_with("short") {
            input.replace_range(0..5, "dS");
        } else if input.starts_with("long") {
            input.replace_range(0..4, "dL");
        } else if input.starts_with("string") {
            input.replace_range(0..6, "s");
        }
    }

    /// Get the type specification character
    /// 'd', 's' or 'u'
    fn parse_type_specification_char(input: &mut String) -> Option<char> {
        input
            .chars()
            .next()
            .and_then(|first_char| match first_char {
                'd' | 's' | 'u' => {
                    input.remove(0);
                    Some(first_char)
                }
                _ => None,
            })
    }

    /// Parses the number of bytes represented by the type.
    fn parse_no_of_bytes_represented(input: &mut String) -> Result<u64, RawMagicLineParseError> {
        let re =
            Regex::new(r"^([CSIL]|\d+)").map_err(|_| RawMagicLineParseError::InvalidTypeFormat)?;

        let _input = input.clone();
        let captures = re
            .captures(&_input)
            .ok_or(RawMagicLineParseError::InvalidTypeFormat)?;

        let mat = captures
            .get(1)
            .ok_or(RawMagicLineParseError::InvalidTypeFormat)?;

        let size = match mat.as_str() {
            "C" => 1,
            "S" => 2,
            "I" => 4,
            "L" => 8,
            _ => mat
                .as_str()
                .parse::<u64>()
                .map_err(|_| RawMagicLineParseError::InvalidTypeFormat)?,
        };

        input.replace_range(0..mat.as_str().len(), "");

        Ok(size)
    }

    fn parse_mask(input: &mut String) -> Option<u64> {
        if let Some(start) = input.chars().next() {
            if start == '&' {
                input.remove(0);
                return parse_number(input);
            }
        }
        None
    }
}

/// Offset field of the magic file
#[derive(Debug)]
struct Offset {
    num: u64,
    is_continuation: bool,
}

impl Offset {
    pub fn parse(mut input: String) -> Result<Self, RawMagicLineParseError> {
        let is_continuation = match input.chars().next() {
            Some('>') => {
                input.remove(0);
                true
            }
            _ => false,
        };

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

#[derive(Debug)]
struct RawMagicFileLine {
    offset: Offset,
    _type: Type,
    value: Value,
    message: String,
}

impl RawMagicFileLine {
    pub fn parse(input: String) -> Result<Self, RawMagicLineParseError> {
        let input = Self::normalize_whitespace(input)?;
        let fields: Vec<&str> = input.splitn(4, " ").collect();

        let offset = Offset::parse(fields[0].to_string())?;
        let _type = Type::parse(fields[1].to_string())?;
        let value = Value::parse(fields[2].to_string(), _type)?;
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

    fn test(&self, tf_reader: &mut BufReader<File>) -> Option<String> {
        let offset = self.offset.num;
        tf_reader.seek(SeekFrom::Start(offset)).unwrap();

        match self._type {
            Type::Unsigned(size, mask) => {
                let mut x = vec![0; size as usize];
                tf_reader.read_exact(&mut x);
                //let x = u64::from_be_bytes(x.try_into().unwrap());
                //println!("{x}");
                //println!("{:?}", self.value);
                None
            }
            Type::Decimal(size, mask) => {
                if self.number_test(size, mask, tf_reader) {
                    Some(self.message.clone())
                } else {
                    None
                }
            }
            Type::String => None,
        }
    }

    fn string_test() {}

    fn number_test(&self, size: u64, mask: Option<u64>, tf_reader: &mut BufReader<File>) -> bool {
        let mut buf = vec![0; size as usize];
        if let Err(_) = tf_reader.read_exact(&mut buf) {
            return false;
        }

        buf.reverse();

        let mut array_buf = [0u8; 8];
        array_buf[(8 - buf.len())..8].copy_from_slice(&buf);
        let tf_val = u64::from_be_bytes(array_buf);

        match &self.value {
            Value::Number(op, val) => match op {
                ComparisonOperator::Equal => *val == tf_val,
                ComparisonOperator::LessThan => *val < tf_val,
                ComparisonOperator::GreaterThan => *val > tf_val,
                ComparisonOperator::AllSet => *val == (*val & tf_val),
                ComparisonOperator::AnyUnset => (*val ^ tf_val) != 0,
                ComparisonOperator::FileLargeEnough => {
                    // we'll directly return true here because
                    // if the file wasn't large enough to hold the value at the given offset
                    // then the read_exact will already thrown an error
                    true
                }
            },
            _ => false,
        }
    }
}
/// Parse a magic database file content line by line and test it against another file
///
/// Given paths to a magic file database and a test file, this function reads both files
/// line by line. It parses each line of the magic database file and tests it against
/// the content of the test file.
fn parse_magic_file_and_test(
    magic_file: &PathBuf,
    test_file: &PathBuf,
) -> Result<String, Box<dyn std::error::Error>> {
    let mf_reader = BufReader::new(File::open(magic_file)?);
    let mut tf_reader = BufReader::new(File::open(test_file)?);

    let mut result = None;
    for line in mf_reader.lines() {
        if let Ok(raw_magic_file_line) = RawMagicFileLine::parse(line?) {
            if result.is_none() && !raw_magic_file_line.offset.is_continuation {
                result = raw_magic_file_line.test(&mut tf_reader);
            } else if let Some(ref r) = result {
                if raw_magic_file_line.offset.is_continuation {
                    result = raw_magic_file_line.test(&mut tf_reader).or(Some(r.clone()));
                } else {
                    return Ok(result.unwrap());
                }
            }
        }
    }

    Err(Box::new(io::Error::new(
        ErrorKind::NotFound,
        "Couldn't match any magic number",
    )))
}

/// Get type for the file from the magic file databases(traversed in order of argument)
fn get_type_from_magic_file_dbs(test_file: &str, magic_file_dbs: &[String]) -> Option<String> {
    magic_file_dbs.iter().find_map(|magic_file| {
        parse_magic_file_and_test(&PathBuf::from(magic_file), &PathBuf::from(test_file)).ok()
    })
}

/// The default magic file(text based)
fn default_magic_file() -> String {
    // Through different indirect resources, currently "/et/magic" is used as the default magic
    // file database
    return "/etc/magic".to_string();
}

fn analyze_file(mut path: String, args: &FileArgs) {
    // set priority according to the occurence of flags in the args lowest index will get highest priority
    let mut magic_files = Vec::new();

    if path == "-" {
        path = String::new();
        io::stdin().read_line(&mut path).unwrap();
        path = path.trim().to_string();
    }

    if let Some(test_file2) = &args.test_file2 {
        magic_files.push(test_file2.clone());

        if args.default_tests && args.test_file1.is_some() {
            let raw_args: Vec<String> = std::env::args().collect();
            let m_index = raw_args.iter().position(|x| x == "-m");
            let h_index = raw_args.iter().position(|x| x == "-h");

            if m_index > h_index {
                magic_files.push(args.test_file1.as_ref().unwrap().clone());
                magic_files.push(default_magic_file());
            } else {
                magic_files.push(default_magic_file());
                magic_files.push(args.test_file1.as_ref().unwrap().clone());
            }
        } else if args.test_file1.is_some() {
            magic_files.push(args.test_file1.as_ref().unwrap().clone());
        } else if args.default_tests {
            magic_files.push(default_magic_file());
        }
    } else if let Some(test_file1) = &args.test_file1 {
        magic_files.push(test_file1.clone());

        if args.test_file2.is_none() && !args.default_tests {
            magic_files.push(default_magic_file());
        }
    } else {
        magic_files.push(default_magic_file());
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
                        match get_type_from_magic_file_dbs(&path, &magic_files) {
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

        Err(_) => {
            println!("{path} : cannot open");
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
