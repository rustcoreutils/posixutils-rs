//! Ex command address parsing.
//!
//! Addresses specify line numbers or patterns for ex commands.

use crate::buffer::Buffer;
use crate::error::{Result, ViError};
use regex::Regex;

/// An ex command address.
#[derive(Debug, Clone)]
pub enum Address {
    /// Current line (.).
    Current,
    /// Last line ($).
    Last,
    /// Absolute line number.
    Line(usize),
    /// Pattern search forward (/pattern/).
    SearchForward { pattern: String, regex: Regex },
    /// Pattern search backward (?pattern?).
    SearchBackward { pattern: String, regex: Regex },
    /// Previous context (').
    Mark(char),
    /// Relative offset from current address (+n or -n).
    Relative(i32),
}

// Custom PartialEq that ignores regex (compares only pattern strings)
impl PartialEq for Address {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Address::Current, Address::Current) => true,
            (Address::Last, Address::Last) => true,
            (Address::Line(a), Address::Line(b)) => a == b,
            (
                Address::SearchForward { pattern: a, .. },
                Address::SearchForward { pattern: b, .. },
            ) => a == b,
            (
                Address::SearchBackward { pattern: a, .. },
                Address::SearchBackward { pattern: b, .. },
            ) => a == b,
            (Address::Mark(a), Address::Mark(b)) => a == b,
            (Address::Relative(a), Address::Relative(b)) => a == b,
            _ => false,
        }
    }
}

impl Address {
    /// Resolve this address to a line number.
    pub fn resolve(&self, buffer: &Buffer, current: usize) -> Result<usize> {
        match self {
            Address::Current => Ok(current),
            Address::Last => Ok(buffer.line_count().max(1)),
            Address::Line(n) => {
                if *n == 0 || *n > buffer.line_count() {
                    Err(ViError::InvalidAddress(format!("line {}", n)))
                } else {
                    Ok(*n)
                }
            }
            Address::SearchForward { pattern, regex } => {
                // Search forward for pattern (using pre-compiled regex)
                for line_num in (current + 1)..=buffer.line_count() {
                    if let Some(line) = buffer.line(line_num) {
                        if regex.is_match(line.content()) {
                            return Ok(line_num);
                        }
                    }
                }
                // Wrap around
                for line_num in 1..current {
                    if let Some(line) = buffer.line(line_num) {
                        if regex.is_match(line.content()) {
                            return Ok(line_num);
                        }
                    }
                }
                Err(ViError::PatternNotFound(pattern.clone()))
            }
            Address::SearchBackward { pattern, regex } => {
                // Search backward for pattern (using pre-compiled regex)
                for line_num in (1..current).rev() {
                    if let Some(line) = buffer.line(line_num) {
                        if regex.is_match(line.content()) {
                            return Ok(line_num);
                        }
                    }
                }
                // Wrap around
                for line_num in (current + 1..=buffer.line_count()).rev() {
                    if let Some(line) = buffer.line(line_num) {
                        if regex.is_match(line.content()) {
                            return Ok(line_num);
                        }
                    }
                }
                Err(ViError::PatternNotFound(pattern.clone()))
            }
            Address::Mark(c) => {
                // TODO: Implement marks
                Err(ViError::MarkNotSet(*c))
            }
            Address::Relative(offset) => {
                let base = current as i32;
                let result = base + offset;
                if result < 1 || result > buffer.line_count() as i32 {
                    Err(ViError::InvalidAddress(
                        "relative address out of range".to_string(),
                    ))
                } else {
                    Ok(result as usize)
                }
            }
        }
    }
}

/// A range of addresses for ex commands.
#[derive(Debug, Clone, PartialEq)]
pub struct AddressRange {
    /// Start address (None means current line).
    pub start: Option<Address>,
    /// End address (None means same as start).
    pub end: Option<Address>,
    /// Whether this range was explicitly specified.
    pub explicit: bool,
}

impl AddressRange {
    /// Create an empty range (means use default).
    pub fn empty() -> Self {
        Self {
            start: None,
            end: None,
            explicit: false,
        }
    }

    /// Create a range with a single address.
    pub fn single(addr: Address) -> Self {
        Self {
            start: Some(addr),
            end: None,
            explicit: true,
        }
    }

    /// Create a range from start to end.
    pub fn range(start: Address, end: Address) -> Self {
        Self {
            start: Some(start),
            end: Some(end),
            explicit: true,
        }
    }

    /// Create a range for the whole file (1,$).
    pub fn all() -> Self {
        Self {
            start: Some(Address::Line(1)),
            end: Some(Address::Last),
            explicit: true,
        }
    }

    /// Create a range for current line only.
    pub fn current() -> Self {
        Self {
            start: Some(Address::Current),
            end: None,
            explicit: true,
        }
    }

    /// Resolve this range to actual line numbers.
    pub fn resolve(&self, buffer: &Buffer, current: usize) -> Result<(usize, usize)> {
        let start = match &self.start {
            Some(addr) => addr.resolve(buffer, current)?,
            None => current,
        };

        let end = match &self.end {
            Some(addr) => addr.resolve(buffer, start)?,
            None => start,
        };

        if start > end {
            Err(ViError::InvalidRange("start > end".to_string()))
        } else {
            Ok((start, end))
        }
    }
}

/// Parse an address from a string.
pub fn parse_address(input: &str) -> Option<(Address, &str)> {
    let input = input.trim_start();
    if input.is_empty() {
        return None;
    }

    let mut chars = input.chars().peekable();
    let first = *chars.peek()?;

    match first {
        '.' => Some((Address::Current, &input[1..])),
        '$' => Some((Address::Last, &input[1..])),
        '/' => {
            // Forward search
            let end = input[1..].find('/')?;
            let pattern = input[1..end + 1].to_string();
            let regex = Regex::new(&pattern).ok()?;
            Some((Address::SearchForward { pattern, regex }, &input[end + 2..]))
        }
        '?' => {
            // Backward search
            let end = input[1..].find('?')?;
            let pattern = input[1..end + 1].to_string();
            let regex = Regex::new(&pattern).ok()?;
            Some((
                Address::SearchBackward { pattern, regex },
                &input[end + 2..],
            ))
        }
        '\'' => {
            // Mark
            let mark = input.chars().nth(1)?;
            Some((Address::Mark(mark), &input[2..]))
        }
        '+' | '-' => {
            // Relative address
            let (num, rest) = parse_offset(input);
            Some((Address::Relative(num), rest))
        }
        '0'..='9' => {
            // Absolute line number
            let (num, rest) = parse_number(input)?;
            Some((Address::Line(num), rest))
        }
        _ => None,
    }
}

/// Parse a number from start of string.
fn parse_number(input: &str) -> Option<(usize, &str)> {
    let end = input
        .char_indices()
        .find(|(_, c)| !c.is_ascii_digit())
        .map(|(i, _)| i)
        .unwrap_or(input.len());

    if end == 0 {
        return None;
    }

    let num: usize = input[..end].parse().ok()?;
    Some((num, &input[end..]))
}

/// Parse a relative offset (+n or -n).
fn parse_offset(input: &str) -> (i32, &str) {
    let mut chars = input.chars().peekable();
    let sign = match chars.next() {
        Some('+') => 1,
        Some('-') => -1,
        _ => return (0, input),
    };

    let rest = &input[1..];
    if let Some((num, remainder)) = parse_number(rest) {
        (sign * num as i32, remainder)
    } else {
        // +/- without number means +1/-1
        (sign, rest)
    }
}

/// Parse an address range.
pub fn parse_address_range(input: &str) -> (AddressRange, &str) {
    let input = input.trim_start();

    // Check for % (whole file)
    if let Some(rest) = input.strip_prefix('%') {
        return (AddressRange::all(), rest);
    }

    // Try to parse first address
    let (first_addr, rest) = match parse_address(input) {
        Some((addr, rest)) => (Some(addr), rest),
        None => (None, input),
    };

    // Check for any offsets after first address
    let (first_addr, rest) = if let Some(addr) = first_addr {
        let (offset, rest) = if rest.starts_with('+') || rest.starts_with('-') {
            parse_offset(rest)
        } else {
            (0, rest)
        };
        if offset != 0 {
            // Combine address with offset (simplified - just use offset)
            (Some(addr), rest)
        } else {
            (Some(addr), rest)
        }
    } else {
        (first_addr, rest)
    };

    let rest = rest.trim_start();

    // Check for , or ; separator
    if rest.starts_with(',') || rest.starts_with(';') {
        let rest = &rest[1..];

        // Try to parse second address
        let (second_addr, rest) = match parse_address(rest) {
            Some((addr, rest)) => (Some(addr), rest),
            None => (None, rest),
        };

        match (first_addr.clone(), second_addr) {
            (Some(start), Some(end)) => (AddressRange::range(start, end), rest),
            _ => {
                if let Some(start) = first_addr {
                    // Start, but no end - means from start to current
                    (AddressRange::range(start, Address::Current), rest)
                } else {
                    (AddressRange::empty(), rest)
                }
            }
        }
    } else if let Some(addr) = first_addr {
        (AddressRange::single(addr), rest)
    } else {
        (AddressRange::empty(), rest)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_current() {
        let (addr, rest) = parse_address(".").unwrap();
        assert_eq!(addr, Address::Current);
        assert_eq!(rest, "");
    }

    #[test]
    fn test_parse_last() {
        let (addr, rest) = parse_address("$").unwrap();
        assert_eq!(addr, Address::Last);
        assert_eq!(rest, "");
    }

    #[test]
    fn test_parse_line_number() {
        let (addr, rest) = parse_address("42").unwrap();
        assert_eq!(addr, Address::Line(42));
        assert_eq!(rest, "");
    }

    #[test]
    fn test_parse_relative() {
        let (addr, rest) = parse_address("+5").unwrap();
        assert_eq!(addr, Address::Relative(5));
        assert_eq!(rest, "");

        let (addr, rest) = parse_address("-3").unwrap();
        assert_eq!(addr, Address::Relative(-3));
        assert_eq!(rest, "");
    }

    #[test]
    fn test_parse_range_all() {
        let (range, rest) = parse_address_range("%");
        assert_eq!(range.start, Some(Address::Line(1)));
        assert_eq!(range.end, Some(Address::Last));
        assert_eq!(rest, "");
    }

    #[test]
    fn test_parse_range_simple() {
        let (range, rest) = parse_address_range("1,5");
        assert_eq!(range.start, Some(Address::Line(1)));
        assert_eq!(range.end, Some(Address::Line(5)));
        assert_eq!(rest, "");
    }

    #[test]
    fn test_resolve_address() {
        let buf = Buffer::from_text("line1\nline2\nline3");

        assert_eq!(Address::Current.resolve(&buf, 2).unwrap(), 2);
        assert_eq!(Address::Last.resolve(&buf, 1).unwrap(), 3);
        assert_eq!(Address::Line(2).resolve(&buf, 1).unwrap(), 2);
        assert_eq!(Address::Relative(1).resolve(&buf, 2).unwrap(), 3);
        assert_eq!(Address::Relative(-1).resolve(&buf, 2).unwrap(), 1);
    }
}
