//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Ex command address parsing.
//!
//! Addresses specify line numbers or patterns for ex commands.

use crate::buffer::{Buffer, Position};
use crate::error::{Result, ViError};
use plib::regex::{Regex, RegexFlags};

/// Everything address resolution needs from the editor.
///
/// Previously `resolve` took only `(&Buffer, usize)`, which is why marks could
/// not be resolved (#X16) and why line 0 could not be permitted for the
/// insert-class commands (#X15) — neither piece of state was reachable. Passing
/// a context keeps the call sites one-liners and gives later additions somewhere
/// to go.
pub struct AddrCtx<'a> {
    /// The buffer being addressed.
    pub buffer: &'a Buffer,
    /// The current line, i.e. what `.` resolves to.
    pub current: usize,
    /// Mark table, indexed `a`..`z`.
    pub marks: &'a [Option<Position>; 26],
    /// Permit line 0. POSIX allows it for the commands that insert *after* an
    /// address (`a`, `i`, `r`, `put`), where 0 means "before the first line".
    pub allow_zero: bool,
}

impl<'a> AddrCtx<'a> {
    /// Context for the common case: no marks available, line 0 rejected.
    pub fn simple(buffer: &'a Buffer, current: usize) -> Self {
        const NO_MARKS: [Option<Position>; 26] = [None; 26];
        Self {
            buffer,
            current,
            marks: &NO_MARKS,
            allow_zero: false,
        }
    }

    /// Same context, but resolving relative to a different current line.
    fn with_current(&self, current: usize) -> AddrCtx<'a> {
        AddrCtx {
            buffer: self.buffer,
            current,
            marks: self.marks,
            allow_zero: self.allow_zero,
        }
    }
}

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
    /// A base address with a trailing +n/-n offset (e.g. `.+2`, `/re/-1`).
    Offset(Box<Address>, i32),
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
            (Address::Offset(a, ao), Address::Offset(b, bo)) => a == b && ao == bo,
            _ => false,
        }
    }
}

impl Address {
    /// Resolve this address to a line number.
    pub fn resolve(&self, ctx: &AddrCtx) -> Result<usize> {
        let buffer = ctx.buffer;
        let current = ctx.current;
        match self {
            Address::Current => Ok(current),
            Address::Last => Ok(buffer.line_count().max(1)),
            Address::Line(n) => {
                // Line 0 is legal only where the command inserts *after* the
                // address, in which case it means "before line 1" (#X15).
                let too_small = *n == 0 && !ctx.allow_zero;
                if too_small || *n > buffer.line_count() {
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
                // #X16: marks are now reachable through the context.
                let idx = match c {
                    'a'..='z' => (*c as u8 - b'a') as usize,
                    _ => return Err(ViError::MarkNotSet(*c)),
                };
                match ctx.marks[idx] {
                    Some(pos) if pos.line >= 1 && pos.line <= buffer.line_count() => Ok(pos.line),
                    _ => Err(ViError::MarkNotSet(*c)),
                }
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
            Address::Offset(base, offset) => {
                let b = base.resolve(ctx)? as i32;
                let result = b + offset;
                if result < 1 || result > buffer.line_count() as i32 {
                    Err(ViError::InvalidAddress("address out of range".to_string()))
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
    /// True when the two addresses were separated by `;` rather than `,`.
    ///
    /// POSIX: `;` sets the current line to the first address before the second
    /// is evaluated, so `2;+1` is line 3. With `,` the second address is still
    /// evaluated against the *original* current line, so `2,+1` is
    /// current+1 (#X10). Both used to behave like `;`.
    pub semi: bool,
}

impl AddressRange {
    /// Create an empty range (means use default).
    pub fn empty() -> Self {
        Self {
            start: None,
            end: None,
            explicit: false,
            semi: false,
        }
    }

    /// Create a range with a single address.
    pub fn single(addr: Address) -> Self {
        Self {
            start: Some(addr),
            end: None,
            explicit: true,
            semi: false,
        }
    }

    /// Create a range from start to end.
    pub fn range(start: Address, end: Address) -> Self {
        Self {
            start: Some(start),
            end: Some(end),
            explicit: true,
            semi: false,
        }
    }

    /// Create a range for the whole file (1,$).
    pub fn all() -> Self {
        Self {
            start: Some(Address::Line(1)),
            end: Some(Address::Last),
            explicit: true,
            semi: false,
        }
    }

    /// Create a range for current line only.
    pub fn current() -> Self {
        Self {
            start: Some(Address::Current),
            end: None,
            explicit: true,
            semi: false,
        }
    }

    /// Create a range whose addresses were separated by `;`.
    pub fn range_semi(start: Address, end: Address, semi: bool) -> Self {
        Self {
            start: Some(start),
            end: Some(end),
            explicit: true,
            semi,
        }
    }

    /// Resolve this range to actual line numbers.
    pub fn resolve(&self, ctx: &AddrCtx) -> Result<(usize, usize)> {
        let start = match &self.start {
            Some(addr) => addr.resolve(ctx)?,
            None => ctx.current,
        };

        // `;` re-bases the second address on the first; `,` does not (#X10).
        let end = match &self.end {
            Some(addr) => {
                if self.semi {
                    addr.resolve(&ctx.with_current(start))?
                } else {
                    addr.resolve(ctx)?
                }
            }
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
            let regex = Regex::new(&pattern, RegexFlags::bre()).ok()?;
            Some((Address::SearchForward { pattern, regex }, &input[end + 2..]))
        }
        '?' => {
            // Backward search
            let end = input[1..].find('?')?;
            let pattern = input[1..end + 1].to_string();
            let regex = Regex::new(&pattern, RegexFlags::bre()).ok()?;
            Some((
                Address::SearchBackward { pattern, regex },
                &input[end + 2..],
            ))
        }
        '\'' | '`' => {
            // Mark. Both `'x` and `` `x `` are accepted. Slice by char
            // boundary: `&input[2..]` panicked on a multibyte mark (#X30).
            let mut it = input.char_indices();
            it.next()?; // the quote
            let (_, mark) = it.next()?;
            let rest = it.next().map(|(i, _)| &input[i..]).unwrap_or("");
            Some((Address::Mark(mark), rest))
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

    // Accumulate every `addr[,;]addr[,;]addr...` in the chain rather than
    // handling a single separator. POSIX discards all but the last two
    // addresses (#X17); previously `1,2,3p` reached the command splitter with a
    // leading `,` still attached and died as "Invalid command". `ed` does the
    // same thing in `normalize_addrvec` (ed/parser.rs).
    let mut addrs: Vec<Option<Address>> = Vec::new();
    let mut seps: Vec<char> = Vec::new();
    let mut rest = input;

    loop {
        // One address, plus any trailing +n/-n offset.
        let (addr, next) = match parse_address(rest) {
            Some((addr, next)) => {
                if next.starts_with('+') || next.starts_with('-') {
                    let (offset, next) = parse_offset(next);
                    if offset != 0 {
                        (Some(Address::Offset(Box::new(addr), offset)), next)
                    } else {
                        (Some(addr), next)
                    }
                } else {
                    (Some(addr), next)
                }
            }
            None => (None, rest),
        };
        addrs.push(addr);
        rest = next.trim_start();

        match rest.strip_prefix([',', ';']) {
            Some(after) => {
                seps.push(rest.chars().next().unwrap_or(','));
                rest = after.trim_start();
            }
            None => break,
        }
    }

    // Nothing parsed at all: no address was present.
    if addrs.len() == 1 && addrs[0].is_none() && seps.is_empty() {
        return (AddressRange::empty(), rest);
    }

    // Keep only the final two addresses and the separator between them.
    while addrs.len() > 2 {
        addrs.remove(0);
        seps.remove(0);
    }
    let semi = seps.last() == Some(&';');

    match addrs.len() {
        1 => match addrs.pop().flatten() {
            Some(addr) => (AddressRange::single(addr), rest),
            None => (AddressRange::empty(), rest),
        },
        _ => {
            let end = addrs.pop().flatten();
            let start = addrs.pop().flatten();
            // An omitted address around a separator defaults per `ed`:
            // `,` → line 1 on the left and `$`-relative current on the right;
            // `;` → the current line.
            let start = start.unwrap_or(if semi {
                Address::Current
            } else {
                Address::Line(1)
            });
            let end = end.unwrap_or(Address::Current);
            (AddressRange::range_semi(start, end, semi), rest)
        }
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

        assert_eq!(
            Address::Current.resolve(&AddrCtx::simple(&buf, 2)).unwrap(),
            2
        );
        assert_eq!(Address::Last.resolve(&AddrCtx::simple(&buf, 1)).unwrap(), 3);
        assert_eq!(
            Address::Line(2).resolve(&AddrCtx::simple(&buf, 1)).unwrap(),
            2
        );
        assert_eq!(
            Address::Relative(1)
                .resolve(&AddrCtx::simple(&buf, 2))
                .unwrap(),
            3
        );
        assert_eq!(
            Address::Relative(-1)
                .resolve(&AddrCtx::simple(&buf, 2))
                .unwrap(),
            1
        );
    }
}
