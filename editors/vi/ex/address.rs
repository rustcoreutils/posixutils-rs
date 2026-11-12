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
use crate::bytes::TextRegex;
use crate::error::{Result, ViError};

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
    /// The last RE used in the editor: what an empty `//` or `??` means.
    pub last_regex: Option<&'a str>,
    /// The `ignorecase` edit option.
    pub ignorecase: bool,
    /// The `magic` edit option.
    pub magic: bool,
    /// The `wrapscan` edit option: whether a search goes on past the end of
    /// the buffer (POSIX 94492, 94504).
    pub wrapscan: bool,
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
            last_regex: None,
            ignorecase: false,
            magic: true,
            wrapscan: true,
        }
    }

    /// Compile a search address's pattern, as a search would be compiled:
    /// the editor's options apply, and an empty pattern is the last RE.
    fn compile<'c>(&self, pattern: &str, cache: &'c RegexCache) -> Result<&'c TextRegex> {
        if let Some(regex) = cache.get() {
            return Ok(regex);
        }
        let pattern = if pattern.is_empty() {
            self.last_regex.ok_or(ViError::NoPreviousSearch)?
        } else {
            pattern
        };
        let regex = crate::search::compile_pattern(pattern, self.ignorecase, self.magic)?;
        Ok(cache.get_or_init(|| regex))
    }

    /// Same context, but resolving relative to a different current line.
    fn with_current(&self, current: usize) -> AddrCtx<'a> {
        AddrCtx { current, ..*self }
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
    SearchForward {
        pattern: String,
        compiled: RegexCache,
    },
    /// Pattern search backward (?pattern?).
    SearchBackward {
        pattern: String,
        compiled: RegexCache,
    },
    /// Previous context (').
    Mark(char),
    /// Relative offset from current address (+n or -n).
    Relative(i64),
    /// A base address with a trailing +n/-n offset (e.g. `.+2`, `/re/-1`).
    Offset(Box<Address>, i64),
}

/// A search address's regex, compiled the first time it is resolved.  An
/// address lives for one command, through which the editor's options and
/// last RE stay what they were, so one compilation serves every resolve --
/// a `:g` body resolves its addresses once per marked line.
pub type RegexCache = std::cell::OnceCell<TextRegex>;

// Equality is the pattern's: the compiled cache is not part of what an
// address is.
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
            Address::SearchForward { pattern, compiled } => {
                let regex = ctx.compile(pattern, compiled)?;
                for line_num in (current + 1)..=buffer.line_count() {
                    if let Some(line) = buffer.line(line_num) {
                        if regex.is_match(line.content()) {
                            return Ok(line_num);
                        }
                    }
                }
                // Wrap around, if wrapscan allows
                let wrap = if ctx.wrapscan { 1..current } else { 1..1 };
                for line_num in wrap {
                    if let Some(line) = buffer.line(line_num) {
                        if regex.is_match(line.content()) {
                            return Ok(line_num);
                        }
                    }
                }
                Err(ViError::PatternNotFound(pattern.clone()))
            }
            Address::SearchBackward { pattern, compiled } => {
                let regex = ctx.compile(pattern, compiled)?;
                for line_num in (1..current).rev() {
                    if let Some(line) = buffer.line(line_num) {
                        if regex.is_match(line.content()) {
                            return Ok(line_num);
                        }
                    }
                }
                // Wrap around, if wrapscan allows
                let wrap = if ctx.wrapscan {
                    current + 1..buffer.line_count() + 1
                } else {
                    1..1
                };
                for line_num in wrap.rev() {
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
            Address::Relative(offset) => offset_line(current, *offset, buffer.line_count()),
            Address::Offset(base, offset) => {
                offset_line(base.resolve(ctx)?, *offset, buffer.line_count())
            }
        }
    }
}

/// `line` moved by `offset`, if that is a line of a buffer of `last` lines.
///
/// Checked: an offset too large for the arithmetic is out of range, where
/// the `i32` it used to be parsed into wrapped `+18446744073709551615` to
/// `-1` and addressed the line before.
fn offset_line(line: usize, offset: i64, last: usize) -> Result<usize> {
    i64::try_from(line)
        .ok()
        .and_then(|l| l.checked_add(offset))
        .and_then(|n| usize::try_from(n).ok())
        .filter(|&n| n >= 1 && n <= last)
        .ok_or_else(|| ViError::InvalidAddress("address out of range".to_string()))
}

impl Address {
    /// This address's search pattern, with its direction, if it searches.
    fn search(&self) -> Option<(&str, char)> {
        match self {
            Address::SearchForward { pattern, .. } => Some((pattern, '/')),
            Address::SearchBackward { pattern, .. } => Some((pattern, '?')),
            Address::Offset(base, _) => base.search(),
            _ => None,
        }
    }
}

impl Address {
    /// This address's search pattern and direction, if it is non-empty.
    pub fn search_pattern(&self) -> Option<(String, char)> {
        self.search()
            .filter(|(p, _)| !p.is_empty())
            .map(|(p, d)| (p.to_string(), d))
    }
}

impl AddressRange {
    /// The non-empty search patterns this range names, in order, with their
    /// directions: each becomes the last RE used, as a search's would.
    pub fn search_patterns(&self) -> Vec<(String, char)> {
        [&self.start, &self.end]
            .into_iter()
            .flatten()
            .filter_map(|a| a.search())
            .filter(|(p, _)| !p.is_empty())
            .map(|(p, d)| (p.to_string(), d))
            .collect()
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

/// Scan a delimited RE that starts just after its opening `delim`: the
/// pattern, with an escaped delimiter as the delimiter itself, and what
/// follows the closing delimiter -- or the end of the line, where POSIX lets
/// the trailing delimiter be omitted.
///
/// Neither `\delim` nor a delimiter inside a bracket expression ends the
/// RE (95689-95695): `/x\/y/` and `/[/]/` are one pattern each.  Finding the
/// first delimiter character, as before, split both.
pub fn scan_pattern(input: &str, delim: char) -> (String, &str) {
    let mut pattern = String::new();
    let mut chars = input.char_indices().peekable();
    while let Some((i, c)) = chars.next() {
        if c == delim {
            return (pattern, &input[i + c.len_utf8()..]);
        }
        match c {
            '\\' => match chars.next() {
                // An escaped delimiter is that character, unescaped -- but
                // kept escaped where unescaped it would mean something else
                // to the RE.
                Some((_, d)) if d == delim && !".[*^$".contains(d) => pattern.push(d),
                Some((_, d)) => {
                    pattern.push('\\');
                    pattern.push(d);
                }
                None => pattern.push('\\'),
            },
            '[' => {
                // A bracket expression runs to its `]`, which is literal as
                // its first character (after any `^`); `[:class:]` and the
                // like are inside it whole.
                pattern.push('[');
                let mut first = true;
                if let Some(&(_, '^')) = chars.peek() {
                    pattern.push('^');
                    chars.next();
                }
                while let Some((_, b)) = chars.next() {
                    pattern.push(b);
                    if b == '[' {
                        if let Some(&(_, k @ (':' | '.' | '='))) = chars.peek() {
                            // `[:alpha:]`: copy through its closing `k]`.
                            pattern.push(k);
                            chars.next();
                            let mut prev = '\0';
                            for (_, x) in chars.by_ref() {
                                pattern.push(x);
                                if prev == k && x == ']' {
                                    break;
                                }
                                prev = x;
                            }
                        }
                    } else if b == ']' && !first {
                        break;
                    }
                    first = false;
                }
            }
            c => pattern.push(c),
        }
    }
    (pattern, "")
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
        '/' | '?' => {
            let (pattern, rest) = scan_pattern(&input[1..], first);
            let compiled = RegexCache::new();
            let addr = if first == '/' {
                Address::SearchForward { pattern, compiled }
            } else {
                Address::SearchBackward { pattern, compiled }
            };
            Some((addr, rest))
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
fn parse_offset(input: &str) -> (i64, &str) {
    let mut chars = input.chars().peekable();
    let sign = match chars.next() {
        Some('+') => 1,
        Some('-') => -1,
        _ => return (0, input),
    };

    let rest = &input[1..];
    let digits = rest.len() - rest.trim_start_matches(|c: char| c.is_ascii_digit()).len();
    if digits > 0 {
        // Too large for an offset is as out of range as any offset past the
        // buffer: saturate, and let resolving report it.
        let num = rest[..digits].parse::<i64>().unwrap_or(i64::MAX);
        (sign * num, &rest[digits..])
    } else {
        // +/- without number means +1/-1
        (sign, rest)
    }
}

/// Parse one address together with any trailing `+n`/`-n` offsets.
///
/// `parse_address` deliberately stops at the base address, so every caller
/// that wants `.+2` or `/re/-1` has to fold the offset in itself.  Both do it
/// here.
pub fn parse_address_with_offset(input: &str) -> Option<(Address, &str)> {
    let (addr, mut rest) = parse_address(input)?;
    let mut addr = addr;
    while rest.starts_with('+') || rest.starts_with('-') {
        let (offset, next) = parse_offset(rest);
        rest = next;
        if offset != 0 {
            addr = Address::Offset(Box::new(addr), offset);
        }
    }
    Some((addr, rest))
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
        let (addr, next) = match parse_address_with_offset(rest) {
            Some((addr, next)) => (Some(addr), next),
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
