//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Search and pattern matching for vi.
//!
//! POSIX vi supports:
//! - / forward search
//! - ? backward search
//! - n repeat last search
//! - N repeat in opposite direction
//! - Pattern matching with BRE (Basic Regular Expressions)

use crate::buffer::{floor_char_boundary, next_char_boundary, Buffer, Position};
use crate::error::{Result, ViError};
use crate::options::Options;
use plib::regex::{Match, Regex, RegexFlags};

/// Direction of search.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SearchDirection {
    /// Search forward (/).
    Forward,
    /// Search backward (?).
    Backward,
}

impl SearchDirection {
    /// Get the opposite direction.
    pub fn opposite(self) -> Self {
        match self {
            SearchDirection::Forward => SearchDirection::Backward,
            SearchDirection::Backward => SearchDirection::Forward,
        }
    }
}

/// Search state for the editor.
#[derive(Debug)]
pub struct SearchState {
    /// Last search pattern.
    pattern: Option<String>,
    /// Compiled regex.
    regex: Option<Regex>,
    /// Last search direction.
    direction: SearchDirection,
    /// Whether search wraps at file boundaries.
    wrapscan: bool,
    /// Whether search is case-insensitive.
    ignorecase: bool,
    /// Whether magic mode is on (special chars in patterns).
    magic: bool,
}

impl Default for SearchState {
    fn default() -> Self {
        Self::new()
    }
}

impl SearchState {
    /// Create a new search state.
    pub fn new() -> Self {
        Self {
            pattern: None,
            regex: None,
            direction: SearchDirection::Forward,
            wrapscan: true,
            ignorecase: false,
            magic: true,
        }
    }

    /// Update search options from editor options.
    pub fn update_options(&mut self, opts: &Options) {
        self.wrapscan = opts.wrapscan;
        self.ignorecase = opts.ignorecase;
        self.magic = opts.magic;
    }

    /// Maximum allowed pattern length to prevent ReDoS attacks.
    const MAX_PATTERN_LEN: usize = 4096;

    /// Set a new search pattern.
    pub fn set_pattern(&mut self, pattern: &str, direction: SearchDirection) -> Result<()> {
        // Limit pattern length to prevent ReDoS attacks
        if pattern.len() > Self::MAX_PATTERN_LEN {
            return Err(ViError::InvalidPattern(format!(
                "Pattern too long (max {} characters)",
                Self::MAX_PATTERN_LEN
            )));
        }

        // Convert the vi pattern to a POSIX BRE for the libc engine.
        let regex_pattern = self.convert_pattern(pattern)?;

        let mut flags = RegexFlags::bre();
        if self.ignorecase {
            flags = flags.ignore_case();
        }
        let regex = Regex::new(&regex_pattern, flags)
            .map_err(|e| ViError::InvalidPattern(e.to_string()))?;

        self.pattern = Some(pattern.to_string());
        self.regex = Some(regex);
        self.direction = direction;

        Ok(())
    }

    /// Convert a vi search pattern to a POSIX Basic Regular Expression for the
    /// libc engine (`plib::regex`).
    fn convert_pattern(&self, pattern: &str) -> Result<String> {
        if self.magic {
            // vi "magic" mode is exactly POSIX BRE — `. * ^ $ [ ] \` are
            // special, `\( \) \{ \}` group/repeat, and `+ ? | ( ) { }` are
            // literal — plus the `\<`/`\>` word-boundary escapes, which glibc
            // BRE supports natively. So the pattern passes through unchanged.
            Ok(pattern.to_string())
        } else {
            // "nomagic": only `^` and `$` keep their special meaning; every
            // other BRE metacharacter must be escaped so libc treats it
            // literally. Backslash escapes are preserved as-is (so `\(`, `\{`,
            // `\<`, etc. remain special).
            let mut result = String::new();
            let mut chars = pattern.chars();

            while let Some(c) = chars.next() {
                match c {
                    '\\' => {
                        result.push('\\');
                        if let Some(next) = chars.next() {
                            result.push(next);
                        }
                    }
                    '^' | '$' => result.push(c),
                    // Make the BRE metacharacters literal.
                    '.' | '*' | '[' | ']' => {
                        result.push('\\');
                        result.push(c);
                    }
                    _ => result.push(c),
                }
            }
            Ok(result)
        }
    }

    /// Get the current pattern.
    pub fn pattern(&self) -> Option<&str> {
        self.pattern.as_deref()
    }

    /// Get the current direction.
    pub fn direction(&self) -> SearchDirection {
        self.direction
    }

    /// Check if a pattern is set.
    pub fn has_pattern(&self) -> bool {
        self.regex.is_some()
    }

    /// Search forward from a position.
    pub fn search_forward(&self, buffer: &Buffer, from: Position) -> Result<Position> {
        let regex = self.regex.as_ref().ok_or(ViError::NoPreviousSearch)?;

        let line_count = buffer.line_count();
        let start_line = from.line;
        let start_col = from.column;

        // Search from current position to end of file
        for line_num in start_line..=line_count {
            if let Some(line) = buffer.line(line_num) {
                let content = line.content();
                let search_start = if line_num == start_line {
                    // Start after the character under the cursor.
                    next_char_boundary(content, start_col).unwrap_or(content.len())
                } else {
                    0
                };

                // `<=`, not `<`: an empty line has search_start == len == 0,
                // and a pattern like `^$` matches exactly there.
                if search_start <= content.len() {
                    // Past the start of the line the slice's first byte is not
                    // a beginning of line, so `^` must not match there -- the
                    // same rule `captures_at` needs, and for the same reason.
                    let found = if search_start == 0 {
                        regex.find(&content[search_start..])
                    } else {
                        regex.find_notbol(&content[search_start..])
                    };
                    if let Some(mat) = found {
                        return Ok(Position::new(line_num, search_start + mat.start));
                    }
                }
            }
        }

        // Wrap to beginning if wrapscan is on
        if self.wrapscan {
            for line_num in 1..start_line {
                if let Some(line) = buffer.line(line_num) {
                    let content = line.content();
                    if let Some(mat) = regex.find(content) {
                        return Ok(Position::new(line_num, mat.start));
                    }
                }
            }

            // Check start line before the starting column
            if let Some(line) = buffer.line(start_line) {
                let content = line.content();
                let search_end = floor_char_boundary(content, start_col);
                if search_end > 0 {
                    if let Some(mat) = regex.find(&content[..search_end]) {
                        return Ok(Position::new(start_line, mat.start));
                    }
                }
            }
        }

        Err(ViError::PatternNotFound(
            self.pattern.clone().unwrap_or_default(),
        ))
    }

    /// Search backward from a position.
    pub fn search_backward(&self, buffer: &Buffer, from: Position) -> Result<Position> {
        let regex = self.regex.as_ref().ok_or(ViError::NoPreviousSearch)?;

        let start_line = from.line;
        let start_col = from.column;

        // Search from current position to beginning of file
        for line_num in (1..=start_line).rev() {
            if let Some(line) = buffer.line(line_num) {
                let content = line.content();
                let search_end = if line_num == start_line {
                    floor_char_boundary(content, start_col)
                } else {
                    content.len()
                };

                // Find last match before search_end
                if search_end > 0 {
                    if let Some(pos) = find_last_match(regex, &content[..search_end]) {
                        return Ok(Position::new(line_num, pos));
                    }
                }
            }
        }

        // Wrap to end if wrapscan is on
        if self.wrapscan {
            let line_count = buffer.line_count();
            for line_num in ((start_line + 1)..=line_count).rev() {
                if let Some(line) = buffer.line(line_num) {
                    let content = line.content();
                    if let Some(pos) = find_last_match(regex, content) {
                        return Ok(Position::new(line_num, pos));
                    }
                }
            }

            // Check start line after the starting column
            if let Some(line) = buffer.line(start_line) {
                let content = line.content();
                let search_start = next_char_boundary(content, start_col).unwrap_or(content.len());
                if search_start < content.len() {
                    // As above: this slice does not begin a line.
                    if let Some(pos) = find_last_match_notbol(regex, &content[search_start..]) {
                        return Ok(Position::new(start_line, search_start + pos));
                    }
                }
            }
        }

        Err(ViError::PatternNotFound(
            self.pattern.clone().unwrap_or_default(),
        ))
    }

    /// Search in the current direction.
    pub fn search(&self, buffer: &Buffer, from: Position) -> Result<Position> {
        match self.direction {
            SearchDirection::Forward => self.search_forward(buffer, from),
            SearchDirection::Backward => self.search_backward(buffer, from),
        }
    }

    /// Search in the opposite direction.
    pub fn search_opposite(&self, buffer: &Buffer, from: Position) -> Result<Position> {
        match self.direction {
            SearchDirection::Forward => self.search_backward(buffer, from),
            SearchDirection::Backward => self.search_forward(buffer, from),
        }
    }

    /// Find all matches in a line.
    pub fn find_all_in_line(&self, line: &str) -> Vec<(usize, usize)> {
        let mut matches = Vec::new();
        if let Some(regex) = &self.regex {
            for mat in regex.find_iter(line) {
                matches.push((mat.start, mat.end));
            }
        }
        matches
    }
}

/// Find the last match of a regex in a string.
fn find_last_match(regex: &Regex, text: &str) -> Option<usize> {
    let mut last_pos = None;
    for mat in regex.find_iter(text) {
        last_pos = Some(mat.start);
    }
    last_pos
}

/// As [`find_last_match`], for a slice that does not begin a line: `^` must
/// not match at its start.
///
/// There is no NOTBOL form of `find_iter`, so walk the slice by hand -- every
/// step searches a remainder, which is never a beginning of line.
fn find_last_match_notbol(regex: &Regex, text: &str) -> Option<usize> {
    let mut last_pos = None;
    let mut offset = 0usize;
    while offset <= text.len() {
        let Some(mat) = regex.find_notbol(&text[offset..]) else {
            break;
        };
        let abs = offset + mat.start;
        last_pos = Some(abs);
        // Advance past this match, keeping `offset` on a character boundary so
        // the next slice is valid; an empty match advances by one character.
        let next = if mat.end > mat.start {
            offset + mat.end
        } else {
            match text[abs..].chars().next() {
                Some(c) => abs + c.len_utf8(),
                None => break,
            }
        };
        if next <= offset {
            break;
        }
        offset = next;
    }
    last_pos
}

/// Pending case conversion applied to replacement text as it is emitted.
#[derive(Clone, Copy, PartialEq)]
enum CaseMode {
    None,
    /// `\u` / `\l`: applies to the next character only.
    OneUpper,
    OneLower,
    /// `\U` / `\L`: applies until `\e` or `\E`.
    Upper,
    Lower,
}

/// Build a `:s` replacement string for one match from its capture groups.
///
/// Supports `&` (whole match), `\1`-`\9` (back-references), `\&` (literal `&`),
/// `\\` (literal backslash), the `\n`/`\t` vi conveniences, `~` (the previous
/// replacement, passed in as `prev`), and the POSIX case escapes
/// `\u \l \U \L \e \E` (ex.md §95726-95732).
fn build_replacement(template: &str, input: &str, matches: &[Match], prev: &str) -> String {
    let mut result = String::with_capacity(template.len() + 16);
    let mut chars = template.chars();
    // `sticky` is the \U/\L mode; `one_shot` is a pending \u/\l for one char.
    let mut sticky = CaseMode::None;
    let mut one_shot = CaseMode::None;

    // Emit `s` honoring any pending case conversion.
    let push = |result: &mut String, s: &str, sticky: &mut CaseMode, one: &mut CaseMode| {
        for ch in s.chars() {
            let converted = match *one {
                CaseMode::OneUpper => {
                    *one = CaseMode::None;
                    ch.to_uppercase().collect::<String>()
                }
                CaseMode::OneLower => {
                    *one = CaseMode::None;
                    ch.to_lowercase().collect::<String>()
                }
                _ => match *sticky {
                    CaseMode::Upper => ch.to_uppercase().collect::<String>(),
                    CaseMode::Lower => ch.to_lowercase().collect::<String>(),
                    _ => ch.to_string(),
                },
            };
            result.push_str(&converted);
        }
    };

    while let Some(c) = chars.next() {
        match c {
            '&' => {
                let m = matches[0];
                let text = input[m.start..m.end].to_string();
                push(&mut result, &text, &mut sticky, &mut one_shot);
            }
            // `~` stands for the previous replacement text (ex.md §95724).
            '~' => push(&mut result, prev, &mut sticky, &mut one_shot),
            '\\' => match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('&') => push(&mut result, "&", &mut sticky, &mut one_shot),
                Some('~') => push(&mut result, "~", &mut sticky, &mut one_shot),
                Some('\\') => push(&mut result, "\\", &mut sticky, &mut one_shot),
                Some('u') => one_shot = CaseMode::OneUpper,
                Some('l') => one_shot = CaseMode::OneLower,
                Some('U') => sticky = CaseMode::Upper,
                Some('L') => sticky = CaseMode::Lower,
                Some('e') | Some('E') => sticky = CaseMode::None,
                Some(d @ '1'..='9') => {
                    let idx = d as usize - '0' as usize;
                    if let Some(m) = matches.get(idx) {
                        if m.end > m.start {
                            let text = input[m.start..m.end].to_string();
                            push(&mut result, &text, &mut sticky, &mut one_shot);
                        }
                    }
                }
                Some(other) => {
                    let text = other.to_string();
                    push(&mut result, &text, &mut sticky, &mut one_shot);
                }
                None => result.push('\\'),
            },
            _ => {
                let text = c.to_string();
                push(&mut result, &text, &mut sticky, &mut one_shot);
            }
        }
    }

    result
}

/// Substitute engine for :s command.
#[derive(Debug)]
/// Everything `:s` needs, replacing what had grown to seven positional
/// parameters.
pub struct SubstituteConfig<'a> {
    /// The BRE to match.
    pub pattern: &'a str,
    /// Replacement template.
    pub replacement: &'a str,
    /// `g` -- every match on the line, not just the first.
    pub global: bool,
    /// `c` -- confirm each substitution.
    pub confirm: bool,
    /// `p` -- print each changed line.
    pub print: bool,
    /// `l` -- print each changed line in unambiguous (`list`) form.
    pub list: bool,
    /// `#` -- print each changed line with its line number.
    pub number: bool,
    /// `n` -- report the match count without substituting.
    pub count_only: bool,
    /// The `ignorecase` edit option.
    pub ignorecase: bool,
    /// Previous replacement text, which `~` expands to (ex.md §95724).
    pub prev_replacement: &'a str,
}

impl<'a> SubstituteConfig<'a> {
    /// Minimal config: just a pattern and replacement, every flag off.
    pub fn new(pattern: &'a str, replacement: &'a str) -> Self {
        Self {
            pattern,
            replacement,
            global: false,
            confirm: false,
            print: false,
            list: false,
            number: false,
            count_only: false,
            ignorecase: false,
            prev_replacement: "",
        }
    }

    /// Substitute every match on the line (`g`).
    pub fn with_global(mut self, v: bool) -> Self {
        self.global = v;
        self
    }

    /// Report the match count without substituting (`n`).
    pub fn with_count_only(mut self, v: bool) -> Self {
        self.count_only = v;
        self
    }

    /// Honor the `ignorecase` edit option.
    pub fn with_ignorecase(mut self, v: bool) -> Self {
        self.ignorecase = v;
        self
    }
}

pub struct Substitutor {
    /// Pattern regex.
    regex: Regex,
    /// Replacement string.
    replacement: String,
    /// Global flag (all matches on line).
    global: bool,
    /// Confirm flag.
    confirm: bool,
    /// Print flag.
    print: bool,
    /// List flag (`l`).
    list: bool,
    /// Number flag (`#`).
    number: bool,
    /// Count flag (count matches, don't substitute).
    count_only: bool,
    /// Previous replacement, for `~`.
    prev_replacement: String,
}

impl Substitutor {
    /// Create a new substitutor.
    pub fn new(cfg: SubstituteConfig<'_>) -> Result<Self> {
        // The :s pattern is a POSIX BRE (vi magic mode); compile via libc.
        let mut flags = RegexFlags::bre();
        if cfg.ignorecase {
            flags = flags.ignore_case();
        }
        let regex =
            Regex::new(cfg.pattern, flags).map_err(|e| ViError::InvalidPattern(e.to_string()))?;

        Ok(Self {
            regex,
            replacement: cfg.replacement.to_string(),
            global: cfg.global,
            confirm: cfg.confirm,
            print: cfg.print,
            list: cfg.list,
            number: cfg.number,
            count_only: cfg.count_only,
            prev_replacement: cfg.prev_replacement.to_string(),
        })
    }

    /// Substitute in a single line.
    /// Returns (new_line, substitution_count).
    pub fn substitute_line(&self, line: &str) -> (String, usize) {
        if self.count_only {
            let count = self.regex.find_iter(line).count();
            return (line.to_string(), count);
        }

        let mut result = String::new();
        let mut last_end = 0usize;
        let mut pos = 0usize;
        let mut count = 0usize;

        while let Some(caps) = self.regex.captures_at(line, pos) {
            let m = caps[0];
            let (ms, me) = (m.start, m.end);
            result.push_str(&line[last_end..ms]);
            result.push_str(&build_replacement(
                &self.replacement,
                line,
                &caps,
                &self.prev_replacement,
            ));
            last_end = me;
            count += 1;

            // Advance past this match, keeping `pos` on a char boundary.
            let next = if me > ms {
                me
            } else {
                line[me..]
                    .chars()
                    .next()
                    .map(|c| me + c.len_utf8())
                    .unwrap_or(me + 1)
            };
            if next > line.len() {
                break;
            }
            pos = next;

            if !self.global {
                break;
            }
        }

        if count == 0 {
            return (line.to_string(), 0);
        }
        result.push_str(&line[last_end..]);
        (result, count)
    }

    /// Check if confirm mode is on.
    pub fn needs_confirm(&self) -> bool {
        self.confirm
    }

    /// Check if print mode is on.
    pub fn should_print(&self) -> bool {
        self.print
    }

    /// True when the changed line should be shown in `list` (`l`) form.
    pub fn should_list(&self) -> bool {
        self.list
    }

    /// True when the changed line should be shown with its line number (`#`).
    pub fn should_number(&self) -> bool {
        self.number
    }

    /// Substitute in a single line, asking `confirm` whether to apply each
    /// match. Used for the `c` flag; `substitute_line` is the unconditional
    /// form. Returns (new_line, substitutions_applied).
    pub fn substitute_line_confirmed<F>(&self, line: &str, mut confirm: F) -> (String, usize)
    where
        F: FnMut(&str, usize, usize) -> bool,
    {
        let mut result = String::new();
        let mut last_end = 0usize;
        let mut pos = 0usize;
        let mut count = 0usize;

        while let Some(caps) = self.regex.captures_at(line, pos) {
            let m = caps[0];
            let (ms, me) = (m.start, m.end);
            result.push_str(&line[last_end..ms]);
            if confirm(line, ms, me) {
                result.push_str(&build_replacement(
                    &self.replacement,
                    line,
                    &caps,
                    &self.prev_replacement,
                ));
                count += 1;
            } else {
                result.push_str(&line[ms..me]);
            }
            last_end = me;

            let next = if me > ms {
                me
            } else {
                line[me..]
                    .chars()
                    .next()
                    .map(|c| me + c.len_utf8())
                    .unwrap_or(me + 1)
            };
            if next > line.len() {
                break;
            }
            pos = next;

            if !self.global {
                break;
            }
        }

        result.push_str(&line[last_end..]);
        (result, count)
    }

    /// Check if count-only mode is on.
    pub fn is_count_only(&self) -> bool {
        self.count_only
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_buffer(text: &str) -> Buffer {
        Buffer::from_text(text)
    }

    #[test]
    fn test_search_forward_simple() {
        let buffer = make_buffer("hello world\nfoo bar\nhello again");
        let mut search = SearchState::new();
        search
            .set_pattern("hello", SearchDirection::Forward)
            .unwrap();

        // Search from start should find "hello" at beginning of line 1
        let pos = search.search_forward(&buffer, Position::new(1, 0)).unwrap();
        // Search starts after current position, so first match at (1,0) is skipped
        // and we find "hello" on line 3
        assert_eq!(pos, Position::new(3, 0));

        // Search from line 3 should wrap to line 1
        let pos = search.search_forward(&buffer, Position::new(3, 0)).unwrap();
        assert_eq!(pos, Position::new(1, 0));
    }

    #[test]
    fn test_search_forward_wrap() {
        let buffer = make_buffer("hello world\nfoo bar");
        let mut search = SearchState::new();
        search
            .set_pattern("hello", SearchDirection::Forward)
            .unwrap();

        // Start after the only match - should wrap
        let pos = search.search_forward(&buffer, Position::new(2, 0)).unwrap();
        assert_eq!(pos, Position::new(1, 0));
    }

    #[test]
    fn test_search_backward_simple() {
        let buffer = make_buffer("hello world\nfoo bar\nhello again");
        let mut search = SearchState::new();
        search
            .set_pattern("hello", SearchDirection::Backward)
            .unwrap();

        let pos = search
            .search_backward(&buffer, Position::new(3, 5))
            .unwrap();
        assert_eq!(pos, Position::new(3, 0));

        let pos = search
            .search_backward(&buffer, Position::new(3, 0))
            .unwrap();
        assert_eq!(pos, Position::new(1, 0));
    }

    #[test]
    fn test_search_no_wrap() {
        let buffer = make_buffer("hello world\nfoo bar");
        let mut search = SearchState::new();
        search.wrapscan = false;
        search
            .set_pattern("hello", SearchDirection::Forward)
            .unwrap();

        // Start after the only match - should fail without wrap
        let result = search.search_forward(&buffer, Position::new(2, 0));
        assert!(result.is_err());
    }

    #[test]
    fn test_search_ignorecase() {
        let buffer = make_buffer("Hello World");
        let mut search = SearchState::new();
        // Must set ignorecase BEFORE set_pattern since regex is compiled in set_pattern
        search.ignorecase = true;
        search
            .set_pattern("hello", SearchDirection::Forward)
            .unwrap();

        // Search finds "Hello" even though pattern is "hello" (case insensitive)
        // Buffer: "Hello World" - match is at column 0
        // Search from (1, 0) searches starting at column 1, so we look for any match
        // Actually, the match "Hello" starts at 0, but search starts at 1...
        // Let's search from end of line to wrap back
        let pos = search.search_forward(&buffer, Position::new(1, 5)).unwrap();
        assert_eq!(pos, Position::new(1, 0));
    }

    #[test]
    fn test_search_not_found() {
        let buffer = make_buffer("hello world");
        let mut search = SearchState::new();
        search.set_pattern("xyz", SearchDirection::Forward).unwrap();

        let result = search.search_forward(&buffer, Position::new(1, 0));
        assert!(result.is_err());
    }

    #[test]
    fn test_substitute_simple() {
        let sub = Substitutor::new(SubstituteConfig::new("foo", "bar")).unwrap();
        let (result, count) = sub.substitute_line("foo baz foo");
        assert_eq!(result, "bar baz foo");
        assert_eq!(count, 1);
    }

    #[test]
    fn test_substitute_global() {
        let sub = Substitutor::new(SubstituteConfig::new("foo", "bar").with_global(true)).unwrap();
        let (result, count) = sub.substitute_line("foo baz foo");
        assert_eq!(result, "bar baz bar");
        assert_eq!(count, 2);
    }

    #[test]
    fn test_substitute_count_only() {
        let sub = Substitutor::new(
            SubstituteConfig::new("foo", "bar")
                .with_global(true)
                .with_count_only(true),
        )
        .unwrap();
        let (result, count) = sub.substitute_line("foo baz foo");
        assert_eq!(result, "foo baz foo"); // Unchanged
        assert_eq!(count, 2);
    }

    #[test]
    fn test_substitute_ampersand() {
        let sub = Substitutor::new(SubstituteConfig::new("foo", "[&]")).unwrap();
        let (result, _) = sub.substitute_line("foo bar");
        assert_eq!(result, "[foo] bar");
    }

    #[test]
    fn test_substitute_no_match() {
        let sub = Substitutor::new(SubstituteConfig::new("xyz", "abc")).unwrap();
        let (result, count) = sub.substitute_line("foo bar");
        assert_eq!(result, "foo bar");
        assert_eq!(count, 0);
    }

    #[test]
    fn test_substitute_bre_grouping_backref() {
        // POSIX BRE \(...\) grouping with \1/\2 back-references in pattern and
        // replacement. The old ERE engine could not do in-pattern back-refs.
        let sub = Substitutor::new(SubstituteConfig::new(r"\(a\)\(b\)", r"\2\1").with_global(true))
            .unwrap();
        let (result, count) = sub.substitute_line("ab ab");
        assert_eq!(result, "ba ba");
        assert_eq!(count, 2);
    }

    #[test]
    fn test_substitute_bre_interval() {
        // BRE \{n\} interval.
        let sub = Substitutor::new(SubstituteConfig::new(r"a\{2\}", "X")).unwrap();
        let (result, count) = sub.substitute_line("caab");
        assert_eq!(result, "cXb");
        assert_eq!(count, 1);
    }

    #[test]
    fn test_substitute_bre_plus_is_literal() {
        // In BRE, '+' is an ordinary character (not "one or more").
        let sub = Substitutor::new(SubstituteConfig::new("a+", "X")).unwrap();
        let (result, count) = sub.substitute_line("a+b");
        assert_eq!(result, "Xb");
        assert_eq!(count, 1);
    }

    #[test]
    fn test_search_bre_grouping() {
        let buffer = make_buffer("apple\nbanana\ncherry");
        let mut search = SearchState::new();
        // \(rr\) — BRE grouping; matches "cherry".
        search
            .set_pattern(r"\(rr\)", SearchDirection::Forward)
            .unwrap();
        let pos = search.search_forward(&buffer, Position::new(1, 0)).unwrap();
        assert_eq!(pos.line, 3);
    }

    #[test]
    fn test_no_previous_search() {
        let search = SearchState::new();
        let buffer = make_buffer("hello");
        let result = search.search(&buffer, Position::new(1, 0));
        assert!(result.is_err());
    }

    #[test]
    fn test_search_direction_opposite() {
        assert_eq!(
            SearchDirection::Forward.opposite(),
            SearchDirection::Backward
        );
        assert_eq!(
            SearchDirection::Backward.opposite(),
            SearchDirection::Forward
        );
    }

    #[test]
    fn test_pattern_length_limit() {
        let mut search = SearchState::new();

        // Pattern at exactly the limit should succeed
        let pattern = "a".repeat(SearchState::MAX_PATTERN_LEN);
        assert!(search
            .set_pattern(&pattern, SearchDirection::Forward)
            .is_ok());

        // Pattern exceeding the limit should fail
        let pattern = "a".repeat(SearchState::MAX_PATTERN_LEN + 1);
        let result = search.set_pattern(&pattern, SearchDirection::Forward);
        assert!(result.is_err());
        if let Err(ViError::InvalidPattern(msg)) = result {
            assert!(msg.contains("too long"));
        } else {
            panic!("Expected InvalidPattern error");
        }
    }
}
