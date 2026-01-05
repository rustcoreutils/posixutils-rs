//! Search and pattern matching for vi.
//!
//! POSIX vi supports:
//! - / forward search
//! - ? backward search
//! - n repeat last search
//! - N repeat in opposite direction
//! - Pattern matching with BRE (Basic Regular Expressions)

use crate::buffer::{Buffer, Position};
use crate::error::{Result, ViError};
use crate::options::Options;
use regex::{Regex, RegexBuilder};

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

        // Convert vi pattern to regex
        let regex_pattern = self.convert_pattern(pattern)?;

        let regex = RegexBuilder::new(&regex_pattern)
            .case_insensitive(self.ignorecase)
            .build()
            .map_err(|e| ViError::InvalidPattern(e.to_string()))?;

        self.pattern = Some(pattern.to_string());
        self.regex = Some(regex);
        self.direction = direction;

        Ok(())
    }

    /// Convert vi pattern to regex pattern.
    fn convert_pattern(&self, pattern: &str) -> Result<String> {
        if !self.magic {
            // In nomagic mode, only ^ and $ are special
            // All other special chars need to be escaped
            let mut result = String::new();
            let mut chars = pattern.chars().peekable();

            while let Some(c) = chars.next() {
                match c {
                    '\\' => {
                        // In nomagic, \. means literal period, but \( is special
                        if let Some(&next) = chars.peek() {
                            match next {
                                '(' | ')' | '{' | '}' | '+' | '?' | '|' | '.' | '*' => {
                                    // These become special when escaped in nomagic
                                    result.push(chars.next().unwrap());
                                }
                                _ => {
                                    result.push('\\');
                                    result.push(chars.next().unwrap());
                                }
                            }
                        } else {
                            result.push('\\');
                        }
                    }
                    '^' | '$' => result.push(c),
                    '.' | '*' | '+' | '?' | '(' | ')' | '{' | '}' | '[' | ']' | '|' => {
                        result.push('\\');
                        result.push(c);
                    }
                    _ => result.push(c),
                }
            }
            Ok(result)
        } else {
            // Magic mode - some vi patterns differ from regex
            // POSIX BRE: * . ^ $ [ \ are special
            // \( \) \{ \} are for grouping/repetition
            let mut result = String::new();
            let mut chars = pattern.chars().peekable();

            while let Some(c) = chars.next() {
                match c {
                    '\\' => {
                        if let Some(&next) = chars.peek() {
                            match next {
                                '<' => {
                                    // Word boundary start
                                    chars.next();
                                    result.push_str(r"\b");
                                }
                                '>' => {
                                    // Word boundary end
                                    chars.next();
                                    result.push_str(r"\b");
                                }
                                '(' | ')' => {
                                    // Grouping in BRE
                                    result.push(chars.next().unwrap());
                                }
                                '{' | '}' => {
                                    // Repetition in BRE
                                    result.push(chars.next().unwrap());
                                }
                                _ => {
                                    result.push('\\');
                                    result.push(chars.next().unwrap());
                                }
                            }
                        } else {
                            result.push('\\');
                        }
                    }
                    // These are special in BRE magic mode
                    '.' | '*' | '^' | '$' | '[' | ']' => {
                        result.push(c);
                    }
                    // These need escaping in regex but not BRE
                    '+' | '?' | '|' | '(' | ')' | '{' | '}' => {
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
                    // Start after current position
                    char_to_byte_offset(content, start_col + 1)
                } else {
                    0
                };

                if search_start < content.len() {
                    if let Some(mat) = regex.find(&content[search_start..]) {
                        let byte_pos = search_start + mat.start();
                        let char_pos = byte_to_char_offset(content, byte_pos);
                        return Ok(Position::new(line_num, char_pos));
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
                        let char_pos = byte_to_char_offset(content, mat.start());
                        return Ok(Position::new(line_num, char_pos));
                    }
                }
            }

            // Check start line before the starting column
            if let Some(line) = buffer.line(start_line) {
                let content = line.content();
                let search_end = char_to_byte_offset(content, start_col);
                if search_end > 0 {
                    if let Some(mat) = regex.find(&content[..search_end]) {
                        let char_pos = byte_to_char_offset(content, mat.start());
                        return Ok(Position::new(start_line, char_pos));
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
                    char_to_byte_offset(content, start_col)
                } else {
                    content.len()
                };

                // Find last match before search_end
                if search_end > 0 {
                    if let Some(pos) = find_last_match(regex, &content[..search_end]) {
                        let char_pos = byte_to_char_offset(content, pos);
                        return Ok(Position::new(line_num, char_pos));
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
                        let char_pos = byte_to_char_offset(content, pos);
                        return Ok(Position::new(line_num, char_pos));
                    }
                }
            }

            // Check start line after the starting column
            if let Some(line) = buffer.line(start_line) {
                let content = line.content();
                let search_start = char_to_byte_offset(content, start_col + 1);
                if search_start < content.len() {
                    if let Some(pos) = find_last_match(regex, &content[search_start..]) {
                        let byte_pos = search_start + pos;
                        let char_pos = byte_to_char_offset(content, byte_pos);
                        return Ok(Position::new(start_line, char_pos));
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
                matches.push((mat.start(), mat.end()));
            }
        }
        matches
    }
}

/// Find the last match of a regex in a string.
fn find_last_match(regex: &Regex, text: &str) -> Option<usize> {
    let mut last_pos = None;
    for mat in regex.find_iter(text) {
        last_pos = Some(mat.start());
    }
    last_pos
}

/// Convert character offset to byte offset.
fn char_to_byte_offset(s: &str, char_offset: usize) -> usize {
    s.char_indices()
        .nth(char_offset)
        .map(|(i, _)| i)
        .unwrap_or(s.len())
}

/// Convert byte offset to character offset.
fn byte_to_char_offset(s: &str, byte_offset: usize) -> usize {
    s[..byte_offset.min(s.len())].chars().count()
}

/// Substitute engine for :s command.
#[derive(Debug)]
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
    /// Count flag (count matches, don't substitute).
    count_only: bool,
}

impl Substitutor {
    /// Create a new substitutor.
    pub fn new(
        pattern: &str,
        replacement: &str,
        global: bool,
        confirm: bool,
        print: bool,
        count_only: bool,
        ignorecase: bool,
    ) -> Result<Self> {
        let regex = RegexBuilder::new(pattern)
            .case_insensitive(ignorecase)
            .build()
            .map_err(|e| ViError::InvalidPattern(e.to_string()))?;

        Ok(Self {
            regex,
            replacement: replacement.to_string(),
            global,
            confirm,
            print,
            count_only,
        })
    }

    /// Substitute in a single line.
    /// Returns (new_line, substitution_count).
    pub fn substitute_line(&self, line: &str) -> (String, usize) {
        if self.count_only {
            let count = self.regex.find_iter(line).count();
            return (line.to_string(), count);
        }

        if self.global {
            // Replace all occurrences
            let replacement = self.expand_replacement(&self.replacement);
            let new_line = self.regex.replace_all(line, replacement.as_str());
            let count = self.regex.find_iter(line).count();
            (new_line.into_owned(), count)
        } else {
            // Replace first occurrence only
            let replacement = self.expand_replacement(&self.replacement);
            if self.regex.is_match(line) {
                let new_line = self.regex.replace(line, replacement.as_str());
                (new_line.into_owned(), 1)
            } else {
                (line.to_string(), 0)
            }
        }
    }

    /// Expand special sequences in replacement string.
    fn expand_replacement(&self, replacement: &str) -> String {
        let mut result = String::new();
        let mut chars = replacement.chars().peekable();

        while let Some(c) = chars.next() {
            match c {
                '\\' => {
                    if let Some(&next) = chars.peek() {
                        match next {
                            'n' => {
                                chars.next();
                                result.push('\n');
                            }
                            't' => {
                                chars.next();
                                result.push('\t');
                            }
                            '&' => {
                                chars.next();
                                result.push('&');
                            }
                            '\\' => {
                                chars.next();
                                result.push('\\');
                            }
                            '1'..='9' => {
                                // Group reference - pass through for regex
                                result.push('$');
                                result.push(chars.next().unwrap());
                            }
                            _ => {
                                result.push(chars.next().unwrap());
                            }
                        }
                    } else {
                        result.push('\\');
                    }
                }
                '&' => {
                    // Entire match - convert to $0
                    result.push_str("$0");
                }
                _ => result.push(c),
            }
        }

        result
    }

    /// Check if confirm mode is on.
    pub fn needs_confirm(&self) -> bool {
        self.confirm
    }

    /// Check if print mode is on.
    pub fn should_print(&self) -> bool {
        self.print
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
        let sub = Substitutor::new("foo", "bar", false, false, false, false, false).unwrap();
        let (result, count) = sub.substitute_line("foo baz foo");
        assert_eq!(result, "bar baz foo");
        assert_eq!(count, 1);
    }

    #[test]
    fn test_substitute_global() {
        let sub = Substitutor::new("foo", "bar", true, false, false, false, false).unwrap();
        let (result, count) = sub.substitute_line("foo baz foo");
        assert_eq!(result, "bar baz bar");
        assert_eq!(count, 2);
    }

    #[test]
    fn test_substitute_count_only() {
        let sub = Substitutor::new("foo", "bar", true, false, false, true, false).unwrap();
        let (result, count) = sub.substitute_line("foo baz foo");
        assert_eq!(result, "foo baz foo"); // Unchanged
        assert_eq!(count, 2);
    }

    #[test]
    fn test_substitute_ampersand() {
        let sub = Substitutor::new("foo", "[&]", false, false, false, false, false).unwrap();
        let (result, _) = sub.substitute_line("foo bar");
        assert_eq!(result, "[foo] bar");
    }

    #[test]
    fn test_substitute_no_match() {
        let sub = Substitutor::new("xyz", "abc", false, false, false, false, false).unwrap();
        let (result, count) = sub.substitute_line("foo bar");
        assert_eq!(result, "foo bar");
        assert_eq!(count, 0);
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
    fn test_byte_char_offset_conversion() {
        let s = "héllo";
        assert_eq!(char_to_byte_offset(s, 0), 0);
        assert_eq!(char_to_byte_offset(s, 1), 1);
        assert_eq!(char_to_byte_offset(s, 2), 3); // 'é' is 2 bytes

        assert_eq!(byte_to_char_offset(s, 0), 0);
        assert_eq!(byte_to_char_offset(s, 1), 1);
        assert_eq!(byte_to_char_offset(s, 3), 2);
    }

    #[test]
    fn test_pattern_length_limit() {
        let mut search = SearchState::new();

        // Pattern at exactly the limit should succeed
        let pattern = "a".repeat(SearchState::MAX_PATTERN_LEN);
        assert!(
            search
                .set_pattern(&pattern, SearchDirection::Forward)
                .is_ok()
        );

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
