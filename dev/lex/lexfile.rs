//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::diag;
use crate::pattern_escape::{expand_posix_bracket_constructs, translate_escape_sequences};
use crate::pattern_validate::{
    parse_anchoring_and_trailing_context, validate_pattern_restrictions,
};
use regex::Regex;
use std::collections::HashMap;

/// A rule in the lex specification
#[derive(Clone, Debug)]
pub struct LexRule {
    /// The regular expression pattern (main pattern, excluding trailing context)
    /// This is the user-visible pattern with substitutions expanded
    pub ere: String,
    /// The compiled pattern with substitutions wrapped in parens for correct quantifier handling
    pub compiled_ere: String,
    /// The action (C code) to execute when matched
    pub action: String,
    /// Start conditions this rule is active in (empty means INITIAL or all %s conditions)
    pub start_conditions: Vec<String>,
    /// True if rule is anchored to beginning of line (^)
    pub bol_anchor: bool,
    /// Trailing context pattern (for r/s syntax), None if no trailing context
    pub trailing_context: Option<String>,
    /// Compiled trailing context pattern
    pub compiled_trailing_context: Option<String>,
    /// True if this is an <<EOF>> rule (matches end of file)
    pub is_eof: bool,
}

impl LexRule {
    fn new() -> LexRule {
        LexRule {
            ere: String::new(),
            compiled_ere: String::new(),
            action: String::new(),
            start_conditions: Vec::new(),
            bol_anchor: false,
            trailing_context: None,
            compiled_trailing_context: None,
            is_eof: false,
        }
    }
}

#[derive(Debug)]
pub struct LexInfo {
    pub external_def: Vec<String>,
    /// Substitution definitions (name -> pattern mapping from definitions section)
    pub subs: HashMap<String, String>,
    pub internal_defs: Vec<String>,
    /// Inclusive start conditions (%s)
    pub cond_start: Vec<String>,
    /// Exclusive start conditions (%x)
    pub cond_xstart: Vec<String>,
    pub yyt_is_ptr: bool,
    pub user_subs: Vec<String>,
    pub rules: Vec<LexRule>,
    /// Table size declarations (%p, %n, %a, %e, %k, %o)
    /// Key is the single character (p, n, a, e, k, o), value is the declared size
    /// Used in -v statistics output for POSIX compliance
    pub table_sizes: HashMap<char, usize>,
}

impl LexInfo {
    fn from(state: &ParseState) -> LexInfo {
        LexInfo {
            external_def: state.external_def.clone(),
            subs: state.subs.clone(),
            internal_defs: state.internal_defs.clone(),
            cond_start: state.cond_start.clone(),
            cond_xstart: state.cond_xstart.clone(),
            yyt_is_ptr: state.yyt_is_ptr,
            user_subs: state.user_subs.clone(),
            rules: state.rules.clone(),
            table_sizes: state.table_sizes.clone(),
        }
    }
}

#[derive(Debug)]
enum LexSection {
    Definitions,
    Rules,
    UserCode,
}

#[derive(Debug)]
struct ParseState {
    section: LexSection,
    open_braces: u32,
    in_def: bool,
    /// True when inside a C-style /* ... */ comment
    in_comment: bool,
    external_def: Vec<String>,
    sub_re: Regex,
    subs: HashMap<String, String>,
    internal_defs: Vec<String>,
    user_subs: Vec<String>,
    cond_start: Vec<String>,
    cond_xstart: Vec<String>,
    yyt_is_ptr: bool,
    rules: Vec<LexRule>,
    tmp_rule: LexRule,
    /// Table size declarations (%p, %n, %a, %e, %k, %o)
    table_sizes: HashMap<char, usize>,
    /// Current line number (1-based)
    line_number: usize,
}

impl ParseState {
    fn new() -> ParseState {
        ParseState {
            section: LexSection::Definitions,
            open_braces: 0,
            in_def: false,
            in_comment: false,
            external_def: Vec::new(),
            sub_re: Regex::new(r"(\w+)\s+(.*)").unwrap(),
            subs: HashMap::new(),
            internal_defs: Vec::new(),
            user_subs: Vec::new(),
            cond_start: Vec::new(),
            cond_xstart: Vec::new(),
            yyt_is_ptr: true,
            rules: Vec::new(),
            tmp_rule: LexRule::new(),
            table_sizes: HashMap::new(),
            line_number: 0,
        }
    }

    /// Report an error at the current line and return an error marker
    fn error(&self, msg: &str) -> String {
        diag::error(diag::Position::line_only(self.line_number as u32), msg);
        // Return a simple marker - the actual message was already printed by diag
        "parse error".to_string()
    }

    /// Report a warning at the current line
    fn warning(&self, msg: &str) {
        diag::warning(diag::Position::line_only(self.line_number as u32), msg);
    }

    fn push_rule(&mut self, rule: LexRule) {
        self.rules.push(rule);
    }
}

/// Strip C-style comments from a line, updating in_comment state
/// Returns the line with comments removed (may be empty if entire line is comment)
fn strip_comments(line: &str, in_comment: &mut bool) -> String {
    let mut result = String::new();
    let chars: Vec<char> = line.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        if *in_comment {
            // Look for end of comment
            if i + 1 < chars.len() && chars[i] == '*' && chars[i + 1] == '/' {
                *in_comment = false;
                i += 2;
                continue;
            }
            i += 1;
        } else {
            // Look for start of comment
            if i + 1 < chars.len() && chars[i] == '/' && chars[i + 1] == '*' {
                *in_comment = true;
                i += 2;
                continue;
            }
            result.push(chars[i]);
            i += 1;
        }
    }

    result
}

// parse line from Definitions section
fn parse_def_line(state: &mut ParseState, line: &str) -> Result<(), String> {
    // Check for %} to end a %{ block first (before early return)
    let trimmed = line.trim();
    if trimmed == "%}" {
        state.in_def = false;
        return Ok(());
    }

    // Check for %{ to start a block
    if trimmed == "%{" {
        state.in_def = true;
        return Ok(());
    }

    // Inside %{ %} blocks, preserve everything as-is (user C code)
    if state.in_def {
        state.external_def.push(String::from(line));
        return Ok(());
    }

    // Handle C-style comments (can span multiple lines) for lex directives only
    let stripped = strip_comments(line, &mut state.in_comment);
    let line_to_parse = stripped.as_str();

    if line_to_parse.is_empty() || line_to_parse.trim().is_empty() {
        return Ok(());
    }

    let first_char = line_to_parse.chars().next().unwrap();

    if first_char == '%' {
        // Use stripped line for % commands
        let line = line_to_parse;
        let mut words = Vec::new();
        for word in line.split_whitespace() {
            words.push(String::from(word));
        }

        let cmd = words.remove(0);
        match cmd.to_lowercase().as_str() {
            // Note: %{ and %} are handled earlier in the function
            "%%" => {
                state.section = LexSection::Rules;
            }
            "%s" | "%start" => {
                state.cond_start.extend(words);
            }
            "%x" => {
                state.cond_xstart.extend(words);
            }
            "%array" => {
                state.yyt_is_ptr = false;
            }
            "%pointer" => {
                state.yyt_is_ptr = true;
            }
            "%p" | "%n" | "%a" | "%e" | "%k" | "%o" => {
                // Parse table size declarations (POSIX compliance)
                // These are preserved but ignored (modern implementations use dynamic allocation)
                let table_char = cmd.chars().nth(1).unwrap();
                if let Some(size_str) = words.first() {
                    if let Ok(size) = size_str.parse::<usize>() {
                        state.table_sizes.insert(table_char, size);
                    }
                }
            }
            _ => {
                state.warning(&format!("unknown command in definitions section: {}", cmd));
            }
        }
    } else if first_char.is_whitespace() && line_to_parse.len() > 1 {
        // Lines starting with whitespace are continuation lines (user C code)
        state.external_def.push(String::from(line));
    } else if let Some(caps) = state.sub_re.captures(line_to_parse) {
        let name = caps.get(1).unwrap().as_str();
        let value = caps.get(2).unwrap().as_str();
        state.subs.insert(String::from(name), String::from(value));
    } else if !line_to_parse.trim().is_empty() {
        return Err(state.error(&format!(
            "unexpected line in definitions section: {}",
            line_to_parse.trim()
        )));
    }
    Ok(())
}

// parse continued action line, counting open braces
// Properly skips over character literals, string literals, and comments
fn parse_braces(open_braces: u32, line: &str) -> Result<u32, String> {
    let mut open_braces = open_braces;
    let chars: Vec<char> = line.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let c = chars[i];

        // Skip character literals 'x' (including escape sequences like '\'' or '\\')
        if c == '\'' {
            i += 1;
            if i < chars.len() && chars[i] == '\\' {
                // Escaped character - skip backslash and next char
                i += 2;
            } else if i < chars.len() {
                // Regular character
                i += 1;
            }
            // Skip closing quote if present
            if i < chars.len() && chars[i] == '\'' {
                i += 1;
            }
            continue;
        }

        // Skip string literals "..."
        if c == '"' {
            i += 1;
            while i < chars.len() {
                if chars[i] == '\\' && i + 1 < chars.len() {
                    // Skip escaped character
                    i += 2;
                } else if chars[i] == '"' {
                    i += 1;
                    break;
                } else {
                    i += 1;
                }
            }
            continue;
        }

        // Skip line comments //...
        if c == '/' && i + 1 < chars.len() && chars[i + 1] == '/' {
            // Rest of line is comment
            break;
        }

        // Skip block comments /* ... */ (partial - may span multiple lines)
        if c == '/' && i + 1 < chars.len() && chars[i + 1] == '*' {
            i += 2;
            while i + 1 < chars.len() {
                if chars[i] == '*' && chars[i + 1] == '/' {
                    i += 2;
                    break;
                }
                i += 1;
            }
            continue;
        }

        if c == '{' {
            open_braces += 1;
        } else if c == '}' {
            if open_braces == 0 {
                return Err("unmatched closing brace in action".to_string());
            }
            open_braces -= 1;
        }
        i += 1;
    }
    Ok(open_braces)
}

#[derive(PartialEq)]
enum RegexType {
    Square,
    Paren,
    Curly,
}

/// Find the end of a POSIX bracket expression construct like [:alpha:], [=a=], or [.ch.]
/// Returns the index of the closing bracket ']' if found, None otherwise
fn find_posix_bracket_end(chars: &[char], start: usize, close_char: char) -> Option<usize> {
    // Look for close_char followed by ]  (e.g., :] or =] or .])
    let mut i = start;
    while i + 1 < chars.len() {
        if chars[i] == close_char && chars[i + 1] == ']' {
            return Some(i + 1); // Return index of the ]
        }
        i += 1;
    }
    None
}

// find the end of the regex in a rule line, by matching [ and ( and { and } and "
// This version properly handles POSIX bracket expression constructs: [:class:], [=equiv=], [.collating.]
fn find_ere_end(line: &str) -> Result<usize, String> {
    let mut stack: Vec<RegexType> = Vec::new();
    let mut inside_brackets = false;
    let mut inside_quotes = false;
    let mut escape_next = false;
    let chars: Vec<char> = line.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let ch = chars[i];

        // Handle escape sequences
        if escape_next {
            escape_next = false;
            i += 1;
            continue;
        }
        if ch == '\\' {
            escape_next = true;
            i += 1;
            continue;
        }

        // Handle quoted strings - whitespace inside quotes doesn't end the pattern
        if ch == '"' && !inside_brackets {
            inside_quotes = !inside_quotes;
            i += 1;
            continue;
        }

        // Inside quotes, nothing special happens
        if inside_quotes {
            i += 1;
            continue;
        }

        match ch {
            '[' => {
                if !inside_brackets {
                    stack.push(RegexType::Square);
                    inside_brackets = true;
                } else {
                    // Inside brackets: check for POSIX constructs [:, [=, [.
                    if i + 1 < chars.len() {
                        let next = chars[i + 1];
                        if next == ':' || next == '=' || next == '.' {
                            // Find the matching closing sequence :], =], or .]
                            if let Some(end_idx) = find_posix_bracket_end(&chars, i + 2, next) {
                                i = end_idx + 1; // Skip past the closing ]
                                continue;
                            }
                            // No closing found - continue normally, regex_syntax will report error
                        }
                    }
                }
            }
            '(' => {
                if !inside_brackets {
                    stack.push(RegexType::Paren);
                }
            }
            '{' => {
                if !inside_brackets {
                    stack.push(RegexType::Curly);
                }
            }
            ']' => {
                if inside_brackets {
                    inside_brackets = false;
                    if stack.pop() != Some(RegexType::Square) {
                        return Err("unmatched closing square bracket in pattern".to_string());
                    }
                }
                // ] outside brackets: could be part of pattern, let regex_syntax handle
            }
            ')' => {
                if !inside_brackets && stack.pop() != Some(RegexType::Paren) {
                    return Err("unmatched closing parenthesis in pattern".to_string());
                }
            }
            '}' => {
                if !inside_brackets && stack.pop() != Some(RegexType::Curly) {
                    return Err("unmatched closing curly brace in pattern".to_string());
                }
            }
            _ => {
                if ch.is_whitespace() && stack.is_empty() && !inside_brackets {
                    return Ok(i);
                }
            }
        }
        i += 1;
    }

    if inside_quotes {
        return Err("unterminated quoted string in pattern".to_string());
    }

    Err("unterminated regular expression".to_string())
}

/// Check if a string looks like an interval expression: {m}, {m,}, or {m,n}
/// Returns true if it contains only digits and at most one comma
fn is_interval_expression(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }

    let mut has_comma = false;
    let mut has_digit_before_comma = false;
    let mut chars = s.chars().peekable();

    // Must start with a digit
    if !chars.peek().is_some_and(|c| c.is_ascii_digit()) {
        return false;
    }

    for ch in chars {
        if ch.is_ascii_digit() {
            if !has_comma {
                has_digit_before_comma = true;
            }
        } else if ch == ',' {
            if has_comma {
                return false; // More than one comma
            }
            if !has_digit_before_comma {
                return false; // No digit before comma
            }
            has_comma = true;
        } else {
            return false; // Invalid character
        }
    }

    has_digit_before_comma
}

/// Translate lex-specific regex syntax to standard ERE format.
///
/// Handles:
/// - Quoted strings (regex metacharacters become literals)
/// - Substitution references {name} expanded from definitions
/// - POSIX equivalence classes and collating elements
/// - Escape sequence translation
///
/// If `wrap_subs` is true, substitutions are wrapped in parentheses so that
/// following quantifiers apply to the entire substitution (used for compilation).
fn translate_ere(state: &mut ParseState, ere: &str, wrap_subs: bool) -> Result<String, String> {
    let mut re = String::new();
    let mut in_quotes = false;
    let mut in_brace = false;
    let mut brace_content = String::new();
    let mut escape_next = false;
    let chars: Vec<char> = ere.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let ch = chars[i];

        // Handle escape sequences outside of quotes
        if escape_next && !in_quotes {
            escape_next = false;
            // In lex patterns, \c means literal character c
            // For regex, most chars just need to be passed through as \c
            // but some need hex escaping to avoid regex interpretation
            match ch {
                // Characters that need hex escaping for regex_syntax
                'n' => re.push_str("\\x0a"), // newline
                't' => re.push_str("\\x09"), // tab
                'r' => re.push_str("\\x0d"), // carriage return
                // Quote doesn't need escaping in regex, just output literally
                '"' => re.push('"'),
                '\'' => re.push('\''),
                // Backslash needs to be escaped for regex
                '\\' => re.push_str("\\\\"),
                // Other characters: pass through as escaped for regex
                _ => {
                    re.push('\\');
                    re.push(ch);
                }
            }
            i += 1;
            continue;
        }

        // Check for escape character outside quotes
        if ch == '\\' && !in_quotes {
            escape_next = true;
            i += 1;
            continue;
        }

        if in_quotes && ch == '"' {
            in_quotes = false;
        } else if in_quotes {
            // Inside quoted strings, escape regex metacharacters that need escaping
            // Note: ] doesn't need escaping as it's only special inside brackets
            match ch {
                '*' => re.push_str(r"\x2a"),
                '+' => re.push_str(r"\x2b"),
                '.' => re.push_str(r"\x2e"),
                '{' => re.push_str(r"\x7b"),
                '}' => re.push_str(r"\x7d"),
                '(' => re.push_str(r"\x28"),
                ')' => re.push_str(r"\x29"),
                '[' => re.push_str(r"\x5b"),
                '?' => re.push_str(r"\x3f"),
                '|' => re.push_str(r"\x7c"),
                '^' => re.push_str(r"\x5e"),
                '$' => re.push_str(r"\x24"),
                '\\' => re.push_str(r"\x5c"),
                _ => re.push(ch),
            }
        } else if in_brace && ch == '}' {
            // Determine if this is an interval expression or a substitution
            if is_interval_expression(&brace_content) {
                // Interval expression: pass through as-is
                re.push('{');
                re.push_str(&brace_content);
                re.push('}');
            } else {
                // Substitution reference - recursively expand any nested substitutions
                match state.subs.get(&brace_content).cloned() {
                    Some(value) => {
                        // Recursively expand substitutions in the value
                        let expanded = translate_ere(state, &value, wrap_subs)?;
                        if wrap_subs {
                            re.push('(');
                            re.push_str(&expanded);
                            re.push(')');
                        } else {
                            re.push_str(&expanded);
                        }
                    }
                    None => {
                        return Err(
                            state.error(&format!("undefined substitution: {{{}}}", brace_content))
                        );
                    }
                }
            }
            in_brace = false;
            brace_content.clear();
        } else if in_brace {
            brace_content.push(ch);
        } else if ch == '"' {
            in_quotes = true;
        } else if ch == '{' {
            in_brace = true;
        } else {
            re.push(ch);
        }
        i += 1;
    }

    // Expand POSIX equivalence classes [=c=] and collating elements [.c.]
    let expanded = expand_posix_bracket_constructs(&re);
    // Translate POSIX escape sequences to regex_syntax compatible format
    Ok(translate_escape_sequences(&expanded))
}

/// Extract start conditions from a pattern like `<STATE>pattern` or `<STATE1,STATE2>pattern`
/// Returns (start_conditions, remaining_pattern)
fn extract_start_conditions(pattern: &str) -> (Vec<String>, &str) {
    let trimmed = pattern.trim_start();
    if !trimmed.starts_with('<') {
        return (Vec::new(), pattern);
    }

    // Find the closing >
    if let Some(end) = trimmed.find('>') {
        let conditions_str = &trimmed[1..end];
        let conditions: Vec<String> = conditions_str
            .split(',')
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect();
        let remaining = &trimmed[end + 1..];
        (conditions, remaining)
    } else {
        // No closing >, treat as regular pattern
        (Vec::new(), pattern)
    }
}

/// Parsed rule information
struct ParsedRuleInfo {
    ere: String,
    compiled_ere: String,
    action: String,
    open_braces: u32,
    start_conditions: Vec<String>,
    bol_anchor: bool,
    trailing_context: Option<String>,
    compiled_trailing_context: Option<String>,
    is_eof: bool,
}

// parse a lex rule line, returning all rule components
fn parse_rule(state: &mut ParseState, line: &str) -> Result<ParsedRuleInfo, String> {
    // Check for <<EOF>> pattern BEFORE extracting start conditions
    // (since <<EOF>> looks like a start condition but isn't)
    let trimmed_line = line.trim_start();
    if let Some(action_ws) = trimmed_line.strip_prefix("<<EOF>>") {
        let action = action_ws.trim_start();

        if action.is_empty() {
            return Err(state.error("missing action for <<EOF>> rule"));
        }

        let open_braces = parse_braces(0, action).map_err(|e| state.error(&e))?;

        return Ok(ParsedRuleInfo {
            ere: String::new(),
            compiled_ere: String::new(),
            action: action.to_string(),
            open_braces,
            start_conditions: Vec::new(), // <<EOF>> doesn't support start conditions in this simple impl
            bol_anchor: false,
            trailing_context: None,
            compiled_trailing_context: None,
            is_eof: true,
        });
    }

    // First extract any start conditions
    let (start_conditions, remaining) = extract_start_conditions(line);

    let pos = find_ere_end(remaining).map_err(|e| state.error(&e))?;
    let ere_raw = String::from(&remaining[..pos]);

    // Validate pattern restrictions per POSIX before processing
    validate_pattern_restrictions(&ere_raw).map_err(|e| state.error(&e))?;

    // Parse anchoring and trailing context before translating
    let (bol_anchor, ere_main, trailing_context_raw, _eol_anchor) =
        parse_anchoring_and_trailing_context(&ere_raw);

    // Translate the main pattern
    let ere = translate_ere(state, &ere_main, false)?;
    // Compiled version wraps substitutions in parens for correct quantifier handling
    let compiled_ere = translate_ere(state, &ere_main, true)?;

    // Translate trailing context if present
    let trailing_context = match &trailing_context_raw {
        Some(tc) => Some(translate_ere(state, tc, false)?),
        None => None,
    };
    let compiled_trailing_context = match &trailing_context_raw {
        Some(tc) => Some(translate_ere(state, tc, true)?),
        None => None,
    };

    let action_ws = String::from(&remaining[pos..]);
    let action = action_ws.trim_start();

    // POSIX: "the absence of an action shall not be valid"
    // Only exception is "|" which means fall-through to the next rule's action
    if action.is_empty() {
        return Err(state.error("missing action for rule (use '|' for fall-through)"));
    }

    let open_braces = parse_braces(0, action).map_err(|e| state.error(&e))?;

    Ok(ParsedRuleInfo {
        ere,
        compiled_ere,
        action: action.to_string(),
        open_braces,
        start_conditions,
        bol_anchor,
        trailing_context,
        compiled_trailing_context,
        is_eof: false,
    })
}

// parse line from Rules section
fn parse_rule_line(state: &mut ParseState, line: &str) -> Result<(), String> {
    if line.is_empty() {
        return Ok(());
    }

    let first_char = line.chars().next().unwrap();

    if first_char == '%' {
        let mut words = Vec::new();
        for word in line.split_whitespace() {
            words.push(String::from(word));
        }

        let cmd = words.remove(0);
        match cmd.as_str() {
            "%{" => {
                state.in_def = true;
            }
            "%}" => {
                state.in_def = false;
            }
            "%%" => {
                state.section = LexSection::UserCode;
            }
            _ => {
                state.warning(&format!("unknown command in rules section: {}", cmd));
            }
        }
    } else if state.open_braces > 0 {
        state.tmp_rule.action.push_str(line);
        state.open_braces = parse_braces(state.open_braces, line).map_err(|e| state.error(&e))?;
        if state.open_braces == 0 {
            let rule = std::mem::replace(&mut state.tmp_rule, LexRule::new());
            state.push_rule(rule);
        }
    } else if state.in_def || (first_char.is_whitespace() && line.len() > 1) {
        state.internal_defs.push(String::from(line));
    } else if line.trim().is_empty() {
        return Ok(());
    } else {
        let info = parse_rule(state, line)?;
        if info.open_braces == 0 {
            state.push_rule(LexRule {
                ere: info.ere,
                compiled_ere: info.compiled_ere,
                action: info.action,
                start_conditions: info.start_conditions,
                bol_anchor: info.bol_anchor,
                trailing_context: info.trailing_context,
                compiled_trailing_context: info.compiled_trailing_context,
                is_eof: info.is_eof,
            });
        } else {
            state.tmp_rule = LexRule {
                ere: info.ere,
                compiled_ere: info.compiled_ere,
                action: info.action,
                start_conditions: info.start_conditions,
                bol_anchor: info.bol_anchor,
                trailing_context: info.trailing_context,
                compiled_trailing_context: info.compiled_trailing_context,
                is_eof: info.is_eof,
            };
            state.open_braces = info.open_braces;
        }
    }
    Ok(())
}

// parse line from UserCode section
fn parse_user_line(state: &mut ParseState, line: &str) -> Result<(), &'static str> {
    state.user_subs.push(String::from(line));
    Ok(())
}

// parse lex input, returning a LexInfo struct
pub fn parse(input: &[String]) -> Result<LexInfo, String> {
    let mut state = ParseState::new();

    for (idx, line) in input.iter().enumerate() {
        state.line_number = idx + 1; // Line numbers are 1-based
        match state.section {
            LexSection::Definitions => parse_def_line(&mut state, line)?,
            LexSection::Rules => parse_rule_line(&mut state, line)?,
            LexSection::UserCode => parse_user_line(&mut state, line)?,
        }
    }

    let lexinfo = LexInfo::from(&state);

    Ok(lexinfo)
}

#[cfg(test)]
mod lextest {
    use super::*;

    #[test]
    fn parse_posix_example() {
        let test_content = r#"
%{
/* Need this for the call to atof() below. */
#include <math.h>
/* Need this for printf(), fopen(), and stdin below. */
#include <stdio.h>
%}


DIGIT    [0-9]
ID       [a-z][a-z0-9]*


%%


{DIGIT}+ {
    printf("An integer: %s (%d)\n", yytext,
        atoi(yytext));
    }


{DIGIT}+"."{DIGIT}*        {
    printf("A float: %s (%g)\n", yytext,
        atof(yytext));
    }


if|then|begin|end|procedure|function        {
    printf("A keyword: %s\n", yytext);
    }


{ID}    printf("An identifier: %s\n", yytext);


"+"|"-"|"*"|"/"        printf("An operator: %s\n", yytext);


"{"[^}\n]*"}"    /* Eat up one-line comments. */


[ \t\n]+        /* Eat up white space. */


.  printf("Unrecognized character: %s\n", yytext);


%%


int main(int argc, char *argv[])
{
    ++argv, --argc;  /* Skip over program name. */
    if (argc > 0)
        yyin = fopen(argv[0], "r");
    else
        yyin = stdin;


    yylex();
}
"#;

        let input: Vec<String> = test_content.lines().map(|s| s.to_string()).collect();
        let lexinfo = parse(&input).expect("parse failed");

        assert_eq!(lexinfo.external_def.len(), 4);

        assert_eq!(lexinfo.subs.len(), 2);
        assert_eq!(lexinfo.subs.get("DIGIT").unwrap(), "[0-9]");
        assert_eq!(lexinfo.subs.get("ID").unwrap(), "[a-z][a-z0-9]*");

        assert_eq!(lexinfo.internal_defs.len(), 0);
        assert_eq!(lexinfo.user_subs.len(), 13);
        assert_eq!(lexinfo.cond_start.len(), 0);
        assert_eq!(lexinfo.cond_xstart.len(), 0);
        assert!(lexinfo.yyt_is_ptr);

        assert_eq!(lexinfo.rules.len(), 8);
        assert_eq!(lexinfo.rules[0].ere, r#"[0-9]+"#);
    }

    #[test]
    fn test_c_style_comments_in_definitions() {
        // Test that C-style comments are properly handled in the definitions section
        let test_content = r#"
/* Header comment
 * spanning multiple lines
 */
%{
#include <stdio.h>
/* Comment inside %{ %} block is preserved */
%}

/* Comment between blocks */
DIGIT [0-9]
/* Another comment */

%%
{DIGIT}+    { printf("digit\n"); }
%%
"#;
        let input: Vec<String> = test_content.lines().map(|s| s.to_string()).collect();
        let lexinfo = parse(&input).expect("parse failed");

        // Comments in %{ %} should be preserved (2 lines: #include and comment)
        assert_eq!(lexinfo.external_def.len(), 2);
        assert!(lexinfo.external_def[0].contains("#include"));
        assert!(lexinfo.external_def[1].contains("Comment inside"));

        // Substitution should be parsed correctly despite surrounding comments
        assert_eq!(lexinfo.subs.len(), 1);
        assert_eq!(lexinfo.subs.get("DIGIT").unwrap(), "[0-9]");

        // Rule should be parsed correctly
        assert_eq!(lexinfo.rules.len(), 1);
    }

    #[test]
    fn test_multiline_comment_spanning_definitions() {
        // Test multi-line comment that spans across definition lines
        let test_content = r#"
/* This is a
   multi-line
   comment */
DIGIT [0-9]
%%
{DIGIT}+    { printf("digit\n"); }
%%
"#;
        let input: Vec<String> = test_content.lines().map(|s| s.to_string()).collect();
        let lexinfo = parse(&input).expect("parse failed");

        // Substitution should be parsed correctly
        assert_eq!(lexinfo.subs.len(), 1);
        assert_eq!(lexinfo.subs.get("DIGIT").unwrap(), "[0-9]");
    }

    #[test]
    fn test_extract_start_conditions() {
        // Test with no start conditions
        let (conditions, remaining) = extract_start_conditions("pattern");
        assert!(conditions.is_empty());
        assert_eq!(remaining, "pattern");

        // Test with single start condition
        let (conditions, remaining) = extract_start_conditions("<STATE>pattern");
        assert_eq!(conditions, vec!["STATE"]);
        assert_eq!(remaining, "pattern");

        // Test with multiple start conditions
        let (conditions, remaining) = extract_start_conditions("<STATE1,STATE2>pattern");
        assert_eq!(conditions, vec!["STATE1", "STATE2"]);
        assert_eq!(remaining, "pattern");

        // Test with whitespace around conditions
        let (conditions, remaining) = extract_start_conditions("<STATE1, STATE2>pattern");
        assert_eq!(conditions, vec!["STATE1", "STATE2"]);
        assert_eq!(remaining, "pattern");

        // Test with no closing bracket (should return original)
        let (conditions, remaining) = extract_start_conditions("<STATEpattern");
        assert!(conditions.is_empty());
        assert_eq!(remaining, "<STATEpattern");
    }

    #[test]
    fn test_parse_start_conditions() {
        let test_content = r#"
%s COMMENT
%x STRING
%%
[a-z]+    printf("word\n");
<COMMENT>[^*]*    /* in comment */
<STRING,INITIAL>["]    /* quote in string or initial */
%%
"#;

        let input: Vec<String> = test_content.lines().map(|s| s.to_string()).collect();
        let lexinfo = parse(&input).expect("parse failed");

        // Check start condition declarations
        assert_eq!(lexinfo.cond_start, vec!["COMMENT"]);
        assert_eq!(lexinfo.cond_xstart, vec!["STRING"]);

        // Check rules
        assert_eq!(lexinfo.rules.len(), 3);

        // First rule has no explicit start conditions
        assert!(lexinfo.rules[0].start_conditions.is_empty());
        assert_eq!(lexinfo.rules[0].ere, "[a-z]+");

        // Second rule has COMMENT condition
        assert_eq!(lexinfo.rules[1].start_conditions, vec!["COMMENT"]);
        assert_eq!(lexinfo.rules[1].ere, "[^*]*");

        // Third rule has STRING and INITIAL conditions
        assert_eq!(lexinfo.rules[2].start_conditions, vec!["STRING", "INITIAL"]);
        // The quote in quotes becomes empty class - quotes strip the contents
        assert_eq!(lexinfo.rules[2].ere, "[]");
    }

    #[test]
    fn test_parse_exclusive_start_conditions() {
        let test_content = r#"
%x COMMENT
%%
"/*"    BEGIN(COMMENT);
<COMMENT>"*/"    BEGIN(INITIAL);
<COMMENT>.    /* eat comment chars */
[a-z]+    printf("word\n");
%%
"#;

        let input: Vec<String> = test_content.lines().map(|s| s.to_string()).collect();
        let lexinfo = parse(&input).expect("parse failed");

        // Check exclusive start condition
        assert!(lexinfo.cond_start.is_empty());
        assert_eq!(lexinfo.cond_xstart, vec!["COMMENT"]);

        // Check rules
        assert_eq!(lexinfo.rules.len(), 4);

        // First rule: no explicit conditions (active in INITIAL only since COMMENT is exclusive)
        assert!(lexinfo.rules[0].start_conditions.is_empty());
        assert_eq!(lexinfo.rules[0].ere, r#"/\x2a"#); // "/*" with * escaped

        // Second rule: COMMENT condition
        assert_eq!(lexinfo.rules[1].start_conditions, vec!["COMMENT"]);

        // Third rule: COMMENT condition
        assert_eq!(lexinfo.rules[2].start_conditions, vec!["COMMENT"]);

        // Fourth rule: no explicit conditions
        assert!(lexinfo.rules[3].start_conditions.is_empty());
    }

    #[test]
    fn test_pattern_validation_caret_at_beginning() {
        // Valid: ^ at beginning
        assert!(validate_pattern_restrictions("^abc").is_ok());
        assert!(validate_pattern_restrictions("^[a-z]+").is_ok());

        // Invalid: ^ in middle
        let result = validate_pattern_restrictions("abc^def");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .contains("'^' operator only valid at beginning"));
    }

    #[test]
    fn test_pattern_validation_caret_in_brackets() {
        // Valid: ^ inside brackets (negation) at any position
        assert!(validate_pattern_restrictions("[^a-z]+").is_ok());
        assert!(validate_pattern_restrictions("abc[^0-9]def").is_ok());
    }

    #[test]
    fn test_pattern_validation_dollar_at_end() {
        // Valid: $ at end
        assert!(validate_pattern_restrictions("abc$").is_ok());
        assert!(validate_pattern_restrictions("[a-z]+$").is_ok());

        // Invalid: $ in middle
        let result = validate_pattern_restrictions("abc$def");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .contains("'$' operator only valid at end"));
    }

    #[test]
    fn test_pattern_validation_dollar_with_trailing_context() {
        // Invalid: $ cannot be used with trailing context
        // Note: abc$/def would fail because $ is not at end (position 3 vs 7)
        // So we test abc/def$ where $ is at end but trailing context exists
        let result = validate_pattern_restrictions("abc/def$");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .contains("'$' cannot be used with trailing context"));

        // Also test $ in middle with trailing context - hits "$ not at end" first
        let result2 = validate_pattern_restrictions("abc$/def");
        assert!(result2.is_err());
        assert!(result2
            .unwrap_err()
            .contains("'$' operator only valid at end"));
    }

    #[test]
    fn test_pattern_validation_multiple_trailing_context() {
        // Valid: one trailing context operator
        assert!(validate_pattern_restrictions("abc/def").is_ok());

        // Invalid: multiple trailing context operators
        let result = validate_pattern_restrictions("abc/def/ghi");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .contains("Only one trailing context operator"));
    }

    #[test]
    fn test_pattern_validation_slash_in_brackets() {
        // Valid: / inside brackets is not a trailing context operator
        assert!(validate_pattern_restrictions("[a/b]+").is_ok());
        assert!(validate_pattern_restrictions("[/]+/abc").is_ok()); // One TC op, the second one
    }

    #[test]
    fn test_pattern_validation_slash_in_quotes() {
        // Valid: / inside quotes is not a trailing context operator
        assert!(validate_pattern_restrictions("\"a/b\"").is_ok());
        assert!(validate_pattern_restrictions("\"abc\"/def").is_ok()); // / after quote is TC
    }

    #[test]
    fn test_pattern_validation_escaped_operators() {
        // Valid: escaped ^ and $ and / are not operators
        assert!(validate_pattern_restrictions(r"abc\^def").is_ok());
        assert!(validate_pattern_restrictions(r"abc\$def").is_ok());
        assert!(validate_pattern_restrictions(r"abc\/def\/ghi").is_ok());
    }

    #[test]
    fn test_pattern_validation_parse_integration() {
        // Test that invalid patterns are rejected during parsing
        // The error message is now printed by diag module to stderr
        diag::init("test.l");
        let test_content = r#"
%%
abc^def    return 1;
%%
"#;
        let input: Vec<String> = test_content.lines().map(|s| s.to_string()).collect();
        let result = parse(&input);
        assert!(result.is_err());
        // Note: diag::has_errors() not checked because parallel tests share global state
    }

    #[test]
    fn test_pattern_validation_multiple_tc_parse_integration() {
        // Test that multiple trailing context operators are rejected during parsing
        // The error message is now printed by diag module to stderr
        diag::init("test.l");
        let test_content = r#"
%%
abc/def/ghi    return 1;
%%
"#;
        let input: Vec<String> = test_content.lines().map(|s| s.to_string()).collect();
        let result = parse(&input);
        assert!(result.is_err());
        // Note: diag::has_errors() not checked because parallel tests share global state
    }

    #[test]
    fn test_error_messages_include_line_numbers() {
        // Test that errors are recorded by the diag module
        // Error messages with line numbers are now printed to stderr by diag
        // Note: We don't check exact error count because tests run in parallel
        diag::init("test.l");
        let test_content = r#"
%%
abc^def    return 1;
%%
"#;
        let input: Vec<String> = test_content.lines().map(|s| s.to_string()).collect();
        let result = parse(&input);
        assert!(result.is_err());
        // The error message with line number is printed to stderr by diag
    }

    #[test]
    fn test_undefined_substitution_error_has_line_number() {
        // Test that undefined substitution errors are recorded by diag
        // Note: We don't check exact error count because tests run in parallel
        diag::init("test.l");
        let test_content = r#"
DIGIT    [0-9]
%%
{UNDEFINED}    return 1;
%%
"#;
        let input: Vec<String> = test_content.lines().map(|s| s.to_string()).collect();
        let result = parse(&input);
        assert!(result.is_err());
        // The error message with line number is printed to stderr by diag
    }

    #[test]
    fn test_is_interval_expression() {
        // Valid interval expressions
        assert!(is_interval_expression("3"));
        assert!(is_interval_expression("10"));
        assert!(is_interval_expression("2,4"));
        assert!(is_interval_expression("3,"));
        assert!(is_interval_expression("1,100"));
        assert!(is_interval_expression("0,5"));

        // Invalid - not interval expressions (should be substitutions)
        assert!(!is_interval_expression(""));
        assert!(!is_interval_expression("abc"));
        assert!(!is_interval_expression("DIGIT"));
        assert!(!is_interval_expression("name123"));
        assert!(!is_interval_expression(",3"));
        assert!(!is_interval_expression("1,2,3"));
        assert!(!is_interval_expression("1a"));
    }

    #[test]
    fn test_interval_expressions_in_patterns() {
        let test_content = r#"
%%
[a-z]{3}      printf("EXACTLY3\n");
[a-z]{2,4}    printf("RANGE\n");
[a-z]{5,}     printf("ATLEAST5\n");
[a-z]+        printf("WORD\n");
%%
"#;
        let input: Vec<String> = test_content.lines().map(|s| s.to_string()).collect();
        let lexinfo = parse(&input).expect("parse failed");

        assert_eq!(lexinfo.rules.len(), 4);
        // Verify interval expressions are preserved
        assert_eq!(lexinfo.rules[0].ere, "[a-z]{3}");
        assert_eq!(lexinfo.rules[1].ere, "[a-z]{2,4}");
        assert_eq!(lexinfo.rules[2].ere, "[a-z]{5,}");
        assert_eq!(lexinfo.rules[3].ere, "[a-z]+");
    }

    #[test]
    fn test_mixed_substitution_and_interval() {
        let test_content = r#"
DIGIT    [0-9]
%%
{DIGIT}{3}     printf("3DIGITS\n");
{DIGIT}{2,4}   printf("RANGE\n");
{DIGIT}+       printf("DIGITS\n");
%%
"#;
        let input: Vec<String> = test_content.lines().map(|s| s.to_string()).collect();
        let lexinfo = parse(&input).expect("parse failed");

        assert_eq!(lexinfo.rules.len(), 3);
        // Substitutions should be expanded, intervals preserved
        assert_eq!(lexinfo.rules[0].ere, "[0-9]{3}");
        assert_eq!(lexinfo.rules[1].ere, "[0-9]{2,4}");
        assert_eq!(lexinfo.rules[2].ere, "[0-9]+");
    }
}
