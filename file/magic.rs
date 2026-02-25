//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use regex::Regex;
use std::{
    error::Error,
    fmt,
    fs::File,
    io::{self, BufRead, BufReader, ErrorKind, Read, Seek, SeekFrom},
    path::PathBuf,
    sync::LazyLock,
};

// Pre-compiled static regexes for performance
static COMP_OP_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^[=<>&^x]").expect("invalid regex"));
static HEX_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^0[xX]([0-9A-F]+)").expect("invalid regex"));
static DEC_OCT_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(0?[0-9]+)").expect("invalid regex"));
static TYPE_SIZE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^([CSIL]|\d+)").expect("invalid regex"));
static WHITESPACE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"[ \t]+").expect("invalid regex"));

#[cfg(target_os = "macos")]
/// Default raw (text based) magic file
pub const DEFAULT_MAGIC_FILE: &str = "/usr/share/file/magic/magic";
#[cfg(not(target_os = "macos"))]
/// Default raw (text based) magic file
pub const DEFAULT_MAGIC_FILE: &str = "/etc/magic";

/// Get type for the file from the magic file databases (traversed in order of argument)
pub fn get_type_from_magic_file_dbs(
    test_file: &PathBuf,
    magic_file_dbs: &[PathBuf],
) -> Option<String> {
    magic_file_dbs.iter().find_map(|magic_file| {
        parse_magic_file_and_test(&PathBuf::from(magic_file), &PathBuf::from(test_file)).ok()
    })
}

/// Errors that can occur during parsing of a raw magic line.
#[allow(clippy::enum_variant_names)]
#[derive(Debug)]
enum RawMagicLineParseError {
    /// Indicates that the offset format is invalid.
    InvalidOffsetFormat,

    /// Indicates that the type format is invalid.
    InvalidTypeFormat,

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
                RawMagicLineParseError::InvalidValue => "Invalid value!",
            }
        )
    }
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
enum Value {
    String(Vec<u8>),
    Number(ComparisonOperator, u64),
}

impl Value {
    fn parse(mut input: String, _type: Type) -> Result<Value, RawMagicLineParseError> {
        match _type {
            Type::String => {
                let mut result = Vec::new();
                let bytes = input.as_bytes();
                let mut i = 0;

                while i < bytes.len() {
                    if bytes[i] == b'\\' {
                        i += 1;
                        if i >= bytes.len() {
                            result.push(b'\\');
                            break;
                        }
                        match bytes[i] {
                            b'\\' => result.push(b'\\'),
                            b'a' => result.push(0x07),
                            b'b' => result.push(0x08),
                            b'f' => result.push(0x0C),
                            b'n' => result.push(b'\n'),
                            b'r' => result.push(b'\r'),
                            b't' => result.push(b'\t'),
                            b'v' => result.push(0x0B),
                            b' ' => result.push(b' '),
                            b'x' => {
                                // hex escape: \xNN (1-2 hex digits)
                                i += 1;
                                let start = i;
                                while i < bytes.len()
                                    && i - start < 2
                                    && bytes[i].is_ascii_hexdigit()
                                {
                                    i += 1;
                                }
                                if i > start {
                                    let hex_str = std::str::from_utf8(&bytes[start..i]).unwrap();
                                    let val = u8::from_str_radix(hex_str, 16).unwrap();
                                    result.push(val);
                                } else {
                                    result.push(b'\\');
                                    result.push(b'x');
                                }
                                continue;
                            }
                            c if c.is_ascii_digit() && c < b'8' => {
                                // octal escape: \NNN (1-3 octal digits)
                                let start = i;
                                while i < bytes.len()
                                    && i - start < 3
                                    && bytes[i] >= b'0'
                                    && bytes[i] <= b'7'
                                {
                                    i += 1;
                                }
                                let oct_str = std::str::from_utf8(&bytes[start..i]).unwrap();
                                let val = u8::from_str_radix(oct_str, 8).unwrap();
                                result.push(val);
                                continue;
                            }
                            other => {
                                result.push(b'\\');
                                result.push(other);
                            }
                        }
                        i += 1;
                    } else {
                        result.push(bytes[i]);
                        i += 1;
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
        let comparision_op = match COMP_OP_RE.find(input) {
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

/// Parses Hexadecimal, Octal and Unsigned Decimal
// TODO
#[allow(clippy::manual_map)] // TODO remove this
#[allow(clippy::needless_match)] // TODO remove this
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
    let _input = input.clone();
    let captures = HEX_RE.captures(&_input)?;
    let expr_match = captures.get(0)?;
    let group_match = captures.get(1)?;

    *input = input.replacen(expr_match.as_str(), "", 1);
    u64::from_str_radix(group_match.as_str(), 16).ok()
}

fn parse_decimal_octal(input: &mut String) -> Option<u64> {
    let _input = input.clone();
    let captures = DEC_OCT_RE.captures(&_input)?;
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
    fn parse(mut input: String) -> Result<Type, RawMagicLineParseError> {
        Self::normalize_type_aliases(&mut input);

        let tsc = Self::parse_type_specification_char(&mut input)
            .ok_or(RawMagicLineParseError::InvalidTypeFormat)?;

        let _type = match tsc {
            'd' | 'u' => {
                // parse decimal and unsigned type
                let no_of_bytes = Self::parse_no_of_bytes_represented(&mut input)?;
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
        if !input.is_empty() {
            return Err(RawMagicLineParseError::InvalidTypeFormat);
        }

        _type
    }

    /// As strings "byte", "short", "long" and "string" can also be represented by dC, dS, dL and s
    /// So, we'll replace the verbose ones with short ones
    fn normalize_type_aliases(input: &mut String) {
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
        let _input = input.clone();
        let captures = TYPE_SIZE_RE
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
    fn parse(mut input: String) -> Result<Self, RawMagicLineParseError> {
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
    fn parse(input: String) -> Result<Self, RawMagicLineParseError> {
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
        Ok(WHITESPACE_RE.replacen(&input, 3, " ").to_string())
    }

    fn test(&self, tf_reader: &mut BufReader<File>) -> Option<String> {
        let offset = self.offset.num;

        // put back the cursor position to beginning and start working
        let _ = tf_reader.rewind();
        let _ = tf_reader.seek(SeekFrom::Start(offset));

        match self._type {
            Type::Unsigned(size, mask) => {
                if self.number_test(size, mask, tf_reader) {
                    Some(self.message.clone())
                } else {
                    None
                }
            }
            Type::Decimal(size, mask) => {
                if self.number_test(size, mask, tf_reader) {
                    Some(self.message.clone())
                } else {
                    None
                }
            }

            Type::String => {
                if self.string_test(tf_reader) {
                    Some(self.message.clone())
                } else {
                    None
                }
            }
        }
    }

    fn string_test(&self, tf_reader: &mut BufReader<File>) -> bool {
        if let Value::String(val) = &self.value {
            let mut buf = vec![0u8; val.len()];
            if tf_reader.read_exact(&mut buf).is_err() {
                return false;
            }
            buf == *val
        } else {
            false
        }
    }

    fn number_test(&self, size: u64, mask: Option<u64>, tf_reader: &mut BufReader<File>) -> bool {
        let mut buf = vec![0; size as usize];
        if tf_reader.read_exact(&mut buf).is_err() {
            return false;
        }

        buf.reverse();

        let mut array_buf = [0u8; 8];
        array_buf[(8 - buf.len())..8].copy_from_slice(&buf);
        let mut tf_val = u64::from_be_bytes(array_buf);

        if let Some(mask) = mask {
            tf_val &= mask;
        }

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
        } else {
            // TODO: Empty branch
        }
    }

    result.ok_or(Box::new(io::Error::new(
        ErrorKind::NotFound,
        "Couldn't match any magic number",
    )))
}
