//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{
    error::Error,
    fmt,
    fs::File,
    io::{self, BufRead, BufReader, Read, Seek, SeekFrom},
    path::{Path, PathBuf},
};

#[cfg(target_os = "macos")]
/// Default raw (text based) magic file
pub const DEFAULT_MAGIC_FILE: &str = "/usr/share/file/magic/magic";
#[cfg(not(target_os = "macos"))]
/// Default raw (text based) magic file
pub const DEFAULT_MAGIC_FILE: &str = "/etc/magic";

/// Get type for the file from the magic file databases (traversed in order of argument)
pub fn get_type_from_magic_file_dbs(
    test_file: &Path,
    magic_file_dbs: &[PathBuf],
) -> Option<String> {
    for magic_file in magic_file_dbs {
        match parse_magic_file_and_test(magic_file, test_file) {
            Ok(Some(result)) => return Some(result),
            Ok(None) => continue,
            Err(e) => {
                eprintln!("file: {}: {}", magic_file.display(), e);
                continue;
            }
        }
    }
    None
}

/// Errors that can occur during parsing of a raw magic line.
#[derive(Debug)]
enum RawMagicLineParseError {
    /// Indicates that the line does not have enough fields.
    MalformedLine,

    /// Indicates that the offset format is invalid.
    Offset,

    /// Indicates that the type format is invalid.
    Type,

    /// Indicates that the value field is invalid
    Value,
}

impl Error for RawMagicLineParseError {}

impl fmt::Display for RawMagicLineParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                RawMagicLineParseError::MalformedLine => "Malformed magic line!",
                RawMagicLineParseError::Offset => "Invalid offset!",
                RawMagicLineParseError::Type => "Invalid type!",
                RawMagicLineParseError::Value => "Invalid value!",
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
    fn parse(input: &str, ty: Type) -> Result<Value, RawMagicLineParseError> {
        match ty {
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
                let (comp, num) = Self::parse_number(input).ok_or(RawMagicLineParseError::Value)?;
                Ok(Value::Number(comp, num))
            }
        }
    }

    fn parse_number(input: &str) -> Option<(ComparisonOperator, u64)> {
        let (comparison_op, rest) = parse_comp_op(input);
        let (num, _) = parse_number(rest)?;
        Some((comparison_op, num))
    }
}

fn parse_comp_op(input: &str) -> (ComparisonOperator, &str) {
    match input.as_bytes().first() {
        Some(b'=') => (ComparisonOperator::Equal, &input[1..]),
        Some(b'<') => (ComparisonOperator::LessThan, &input[1..]),
        Some(b'>') => (ComparisonOperator::GreaterThan, &input[1..]),
        Some(b'&') => (ComparisonOperator::AllSet, &input[1..]),
        Some(b'^') => (ComparisonOperator::AnyUnset, &input[1..]),
        Some(b'x') => (ComparisonOperator::FileLargeEnough, &input[1..]),
        _ => (ComparisonOperator::Equal, input),
    }
}

/// Parses hexadecimal: 0xNN or 0XNN
fn parse_hex(input: &str) -> Option<(u64, &str)> {
    let bytes = input.as_bytes();
    if bytes.len() < 3 || bytes[0] != b'0' || (bytes[1] != b'x' && bytes[1] != b'X') {
        return None;
    }
    let rest = &input[2..];
    let end = rest
        .bytes()
        .position(|b| !b.is_ascii_hexdigit())
        .unwrap_or(rest.len());
    if end == 0 {
        return None;
    }
    let val = u64::from_str_radix(&rest[..end], 16).ok()?;
    Some((val, &rest[end..]))
}

/// Parses decimal or octal (leading 0 = octal)
fn parse_dec_oct(input: &str) -> Option<(u64, &str)> {
    let bytes = input.as_bytes();
    if bytes.is_empty() || !bytes[0].is_ascii_digit() {
        return None;
    }
    let end = input
        .bytes()
        .position(|b| !b.is_ascii_digit())
        .unwrap_or(input.len());
    let num_str = &input[..end];
    let radix = if num_str.starts_with('0') && num_str.len() > 1 {
        8
    } else {
        10
    };
    let val = u64::from_str_radix(num_str, radix).ok()?;
    Some((val, &input[end..]))
}

/// Tries hex then decimal/octal
fn parse_number(input: &str) -> Option<(u64, &str)> {
    parse_hex(input).or_else(|| parse_dec_oct(input))
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
    fn parse(input: &str) -> Result<Type, RawMagicLineParseError> {
        let normalized = Self::normalize_type_aliases(input);
        let s = normalized.as_str();

        let (tsc, rest) = parse_type_spec_char(s).ok_or(RawMagicLineParseError::Type)?;

        match tsc {
            'd' | 'u' => {
                let (no_of_bytes, rest) = parse_type_size(rest)?;
                let (mask, rest) = parse_mask(rest);
                if !rest.is_empty() {
                    return Err(RawMagicLineParseError::Type);
                }
                if tsc == 'd' {
                    Ok(Type::Decimal(no_of_bytes, mask))
                } else {
                    Ok(Type::Unsigned(no_of_bytes, mask))
                }
            }
            's' => {
                if !rest.is_empty() {
                    return Err(RawMagicLineParseError::Type);
                }
                Ok(Type::String)
            }
            _ => Err(RawMagicLineParseError::Type),
        }
    }

    /// As strings "byte", "short", "long" and "string" can also be represented by dC, dS, dL and s
    /// So, we'll replace the verbose ones with short ones
    fn normalize_type_aliases(input: &str) -> String {
        if let Some(rest) = input.strip_prefix("byte") {
            format!("dC{rest}")
        } else if let Some(rest) = input.strip_prefix("short") {
            format!("dS{rest}")
        } else if let Some(rest) = input.strip_prefix("long") {
            format!("dL{rest}")
        } else if let Some(rest) = input.strip_prefix("string") {
            format!("s{rest}")
        } else {
            input.to_string()
        }
    }
}

fn parse_type_spec_char(input: &str) -> Option<(char, &str)> {
    let first = input.chars().next()?;
    match first {
        'd' | 's' | 'u' => Some((first, &input[1..])),
        _ => None,
    }
}

fn parse_type_size(input: &str) -> Result<(u64, &str), RawMagicLineParseError> {
    match input.as_bytes().first() {
        Some(b'C') => Ok((1, &input[1..])),
        Some(b'S') => Ok((2, &input[1..])),
        Some(b'I') => Ok((4, &input[1..])),
        Some(b'L') => Ok((8, &input[1..])),
        Some(b) if b.is_ascii_digit() => {
            let end = input
                .bytes()
                .position(|b| !b.is_ascii_digit())
                .unwrap_or(input.len());
            let val = input[..end]
                .parse::<u64>()
                .map_err(|_| RawMagicLineParseError::Type)?;
            Ok((val, &input[end..]))
        }
        _ => Err(RawMagicLineParseError::Type),
    }
}

fn parse_mask(input: &str) -> (Option<u64>, &str) {
    if input.as_bytes().first() == Some(&b'&') {
        if let Some((num, rest)) = parse_number(&input[1..]) {
            return (Some(num), rest);
        }
    }
    (None, input)
}

/// Offset field of the magic file
#[derive(Debug)]
struct Offset {
    num: u64,
    is_continuation: bool,
}

impl Offset {
    fn parse(input: &str) -> Result<Self, RawMagicLineParseError> {
        let (is_continuation, rest) = if let Some(stripped) = input.strip_prefix('>') {
            (true, stripped)
        } else {
            (false, input)
        };

        let (num, remaining) = parse_number(rest).ok_or(RawMagicLineParseError::Offset)?;

        if !remaining.is_empty() {
            return Err(RawMagicLineParseError::Offset);
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
    ty: Type,
    value: Value,
    message: String,
}

impl RawMagicFileLine {
    fn parse(input: &str) -> Result<Self, RawMagicLineParseError> {
        let input = Self::normalize_whitespace(input);
        let fields: Vec<&str> = input.splitn(4, ' ').collect();

        if fields.len() < 4 {
            return Err(RawMagicLineParseError::MalformedLine);
        }

        let offset = Offset::parse(fields[0])?;
        let ty = Type::parse(fields[1])?;
        let value = Value::parse(fields[2], ty)?;
        let message = fields[3].to_string();

        Ok(RawMagicFileLine {
            offset,
            ty,
            value,
            message,
        })
    }

    /// Collapse the first 3 whitespace runs to single spaces, preserving the rest.
    fn normalize_whitespace(input: &str) -> String {
        let mut result = String::with_capacity(input.len());
        let mut replacements = 0;
        let mut in_whitespace = false;

        for ch in input.chars() {
            if (ch == ' ' || ch == '\t') && replacements < 3 {
                if !in_whitespace {
                    in_whitespace = true;
                    result.push(' ');
                }
            } else {
                if in_whitespace {
                    replacements += 1;
                    in_whitespace = false;
                }
                result.push(ch);
            }
        }

        result
    }

    fn test(&self, tf_reader: &mut BufReader<File>) -> Option<String> {
        if tf_reader.seek(SeekFrom::Start(self.offset.num)).is_err() {
            return None;
        }

        match self.ty {
            Type::Unsigned(size, mask) | Type::Decimal(size, mask) => self
                .number_test(size, mask, tf_reader)
                .then(|| self.message.clone()),
            Type::String => self.string_test(tf_reader).then(|| self.message.clone()),
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
        if size == 0 || size > 8 {
            return false;
        }

        let size = size as usize;
        let mut buf = vec![0; size];
        if tf_reader.read_exact(&mut buf).is_err() {
            return false;
        }

        let mut array_buf = [0u8; 8];
        if cfg!(target_endian = "little") {
            array_buf[..size].copy_from_slice(&buf);
        } else {
            array_buf[(8 - size)..].copy_from_slice(&buf);
        }
        let mut tf_val = u64::from_ne_bytes(array_buf);

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
    magic_file: &Path,
    test_file: &Path,
) -> Result<Option<String>, io::Error> {
    let mf_reader = BufReader::new(File::open(magic_file)?);
    let mut tf_reader = BufReader::new(File::open(test_file)?);

    let mut result = None;
    for line in mf_reader.lines() {
        if let Ok(raw_magic_file_line) = RawMagicFileLine::parse(&line?) {
            if result.is_none() && !raw_magic_file_line.offset.is_continuation {
                result = raw_magic_file_line.test(&mut tf_reader);
            } else if let Some(ref r) = result {
                if raw_magic_file_line.offset.is_continuation {
                    result = raw_magic_file_line.test(&mut tf_reader).or(Some(r.clone()));
                } else {
                    return Ok(result);
                }
            }
        }
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_hex() {
        assert_eq!(parse_hex("0xFF"), Some((255, "")));
        assert_eq!(parse_hex("0X1A2B"), Some((0x1A2B, "")));
        assert_eq!(parse_hex("0xFFrest"), Some((255, "rest")));
        assert_eq!(parse_hex("123"), None);
        assert_eq!(parse_hex("0x"), None);
    }

    #[test]
    fn test_parse_dec_oct() {
        assert_eq!(parse_dec_oct("123"), Some((123, "")));
        assert_eq!(parse_dec_oct("0777"), Some((0o777, "")));
        assert_eq!(parse_dec_oct("0"), Some((0, "")));
        assert_eq!(parse_dec_oct("42rest"), Some((42, "rest")));
        assert_eq!(parse_dec_oct("abc"), None);
        // Invalid octal digits (8, 9) cause from_str_radix to fail → None
        assert_eq!(parse_dec_oct("089"), None);
    }

    #[test]
    fn test_parse_comp_op() {
        let (op, rest) = parse_comp_op("=123");
        assert!(matches!(op, ComparisonOperator::Equal));
        assert_eq!(rest, "123");

        let (op, rest) = parse_comp_op("<5");
        assert!(matches!(op, ComparisonOperator::LessThan));
        assert_eq!(rest, "5");

        let (op, rest) = parse_comp_op(">5");
        assert!(matches!(op, ComparisonOperator::GreaterThan));
        assert_eq!(rest, "5");

        let (op, rest) = parse_comp_op("&0xFF");
        assert!(matches!(op, ComparisonOperator::AllSet));
        assert_eq!(rest, "0xFF");

        let (op, rest) = parse_comp_op("^0x0F");
        assert!(matches!(op, ComparisonOperator::AnyUnset));
        assert_eq!(rest, "0x0F");

        let (op, rest) = parse_comp_op("x1");
        assert!(matches!(op, ComparisonOperator::FileLargeEnough));
        assert_eq!(rest, "1");

        // default: no operator prefix
        let (op, rest) = parse_comp_op("42");
        assert!(matches!(op, ComparisonOperator::Equal));
        assert_eq!(rest, "42");
    }

    #[test]
    fn test_parse_type() {
        // Unsigned byte
        let ty = Type::parse("uC").unwrap();
        assert!(matches!(ty, Type::Unsigned(1, None)));

        // Decimal short
        let ty = Type::parse("dS").unwrap();
        assert!(matches!(ty, Type::Decimal(2, None)));

        // String
        let ty = Type::parse("string").unwrap();
        assert!(matches!(ty, Type::String));

        // Aliases
        let ty = Type::parse("byte").unwrap();
        assert!(matches!(ty, Type::Decimal(1, None)));

        let ty = Type::parse("short").unwrap();
        assert!(matches!(ty, Type::Decimal(2, None)));

        let ty = Type::parse("long").unwrap();
        assert!(matches!(ty, Type::Decimal(8, None)));

        // With mask
        let ty = Type::parse("uL&0xFF").unwrap();
        assert!(matches!(ty, Type::Unsigned(8, Some(255))));

        // Invalid
        assert!(Type::parse("xyz").is_err());
    }

    #[test]
    fn test_parse_string_escapes() {
        let val = Value::parse(r"\x97\x4A\x42\x32", Type::String).unwrap();
        if let Value::String(bytes) = val {
            assert_eq!(bytes, vec![0x97, 0x4A, 0x42, 0x32]);
        } else {
            panic!("expected String variant");
        }

        // Octal escapes
        let val = Value::parse(r"\037\036", Type::String).unwrap();
        if let Value::String(bytes) = val {
            assert_eq!(bytes, vec![0o37, 0o36]);
        } else {
            panic!("expected String variant");
        }

        // Character escapes
        let val = Value::parse(r"\n\t\\", Type::String).unwrap();
        if let Value::String(bytes) = val {
            assert_eq!(bytes, vec![b'\n', b'\t', b'\\']);
        } else {
            panic!("expected String variant");
        }
    }

    #[test]
    fn test_normalize_whitespace() {
        // First 3 whitespace runs collapsed
        let result = RawMagicFileLine::normalize_whitespace("0\tstring\t\\037\\036\tsome message");
        assert_eq!(result, "0 string \\037\\036 some message");

        // 4th+ whitespace preserved
        let result =
            RawMagicFileLine::normalize_whitespace("0\tstring\thello\tmessage\twith\ttabs");
        assert_eq!(result, "0 string hello message\twith\ttabs");

        // Trailing whitespace within first 3 runs is collapsed
        let result = RawMagicFileLine::normalize_whitespace("a\tb\tc\t");
        assert_eq!(result, "a b c ");
    }

    #[test]
    fn test_malformed_line() {
        // Too few fields
        assert!(RawMagicFileLine::parse("0 short").is_err());

        // Invalid content
        assert!(RawMagicFileLine::parse("invalid line").is_err());
    }

    #[test]
    fn test_number_test_size_too_large() {
        // Construct a magic line with Unsigned(16, None) — size > 8 should not panic
        let line = RawMagicFileLine {
            offset: Offset {
                num: 0,
                is_continuation: false,
            },
            ty: Type::Unsigned(16, None),
            value: Value::Number(ComparisonOperator::Equal, 0),
            message: "should not match".to_string(),
        };

        // Create a temp file with some data
        let tmp = std::env::temp_dir().join("posixutils_test_size_guard");
        std::fs::write(&tmp, b"0123456789abcdef0123456789abcdef").unwrap();
        let f = File::open(&tmp).unwrap();
        let mut reader = BufReader::new(f);

        // Should return None (no match), not panic
        assert!(line.test(&mut reader).is_none());

        std::fs::remove_file(&tmp).unwrap();
    }
}
