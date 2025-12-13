//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use regex::Regex;
use std::collections::HashMap;

/// A rule in the lex specification
#[derive(Clone, Debug)]
pub struct LexRule {
    /// The regular expression pattern (main pattern, excluding trailing context)
    pub ere: String,
    /// The action (C code) to execute when matched
    pub action: String,
    /// Start conditions this rule is active in (empty means INITIAL or all %s conditions)
    pub start_conditions: Vec<String>,
    /// True if rule is anchored to beginning of line (^)
    pub bol_anchor: bool,
    /// Trailing context pattern (for r/s syntax), None if no trailing context
    pub trailing_context: Option<String>,
}

impl LexRule {
    fn new() -> LexRule {
        LexRule {
            ere: String::new(),
            action: String::new(),
            start_conditions: Vec::new(),
            bol_anchor: false,
            trailing_context: None,
        }
    }
}

#[derive(Debug)]
pub struct LexInfo {
    pub external_def: Vec<String>,
    /// Substitution definitions (used during parsing)
    #[allow(dead_code)]
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
    /// These are preserved for POSIX compliance but ignored in modern dynamic allocation
    #[allow(dead_code)]
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

    /// Format an error message with line number
    fn error(&self, msg: &str) -> String {
        format!("lex: line {}: error: {}", self.line_number, msg)
    }

    /// Format a warning message with line number
    fn warning(&self, msg: &str) -> String {
        format!("lex: line {}: warning: {}", self.line_number, msg)
    }

    fn push_rule(
        &mut self,
        ere: &str,
        action: &str,
        start_conditions: Vec<String>,
        bol_anchor: bool,
        trailing_context: Option<String>,
    ) {
        self.rules.push(LexRule {
            ere: String::from(ere),
            action: String::from(action),
            start_conditions,
            bol_anchor,
            trailing_context,
        });
    }
}

// parse line from Definitions section
fn parse_def_line(state: &mut ParseState, line: &str) -> Result<(), String> {
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
        match cmd.to_lowercase().as_str() {
            "%{" => {
                state.in_def = true;
            }
            "%}" => {
                state.in_def = false;
            }
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
                eprintln!(
                    "{}",
                    state.warning(&format!("unknown command in definitions section: {}", cmd))
                );
            }
        }
    } else if state.in_def || (first_char.is_whitespace() && line.len() > 1) {
        state.external_def.push(String::from(line));
    } else if let Some(caps) = state.sub_re.captures(line) {
        let name = caps.get(1).unwrap().as_str();
        let value = caps.get(2).unwrap().as_str();
        state.subs.insert(String::from(name), String::from(value));
    } else if !line.trim().is_empty() {
        return Err(state.error(&format!(
            "unexpected line in definitions section: {}",
            line.trim()
        )));
    }
    Ok(())
}

// parse continued action line, counting open braces
fn parse_braces(open_braces: u32, line: &str) -> Result<u32, String> {
    let mut open_braces = open_braces;
    for c in line.chars() {
        if c == '{' {
            open_braces += 1;
        } else if c == '}' {
            if open_braces == 0 {
                return Err("unmatched closing brace in action".to_string());
            }
            open_braces -= 1;
        }
    }
    Ok(open_braces)
}

#[derive(PartialEq)]
enum RegexType {
    Square,
    Paren,
    Curly,
}

// find the end of the regex in a rule line, by matching [ and ( and { and }
fn find_ere_end(line: &str) -> Result<usize, String> {
    let mut stack: Vec<RegexType> = Vec::new();
    let mut inside_brackets = false;

    for (i, ch) in line.chars().enumerate() {
        match ch {
            '[' => {
                if !inside_brackets {
                    stack.push(RegexType::Square);
                    inside_brackets = true;
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
                inside_brackets = false;
                if stack.pop() != Some(RegexType::Square) {
                    return Err("unmatched closing square bracket in pattern".to_string());
                }
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
                if ch.is_whitespace() && stack.is_empty() {
                    return Ok(i);
                }
            }
        }
    }

    Err("unterminated regular expression".to_string())
}

/// Translate POSIX escape sequences to forms that regex_syntax understands
///
/// This handles several POSIX compliance issues:
/// 1. Octal escapes (\NNN): regex_syntax interprets these as backreferences,
///    so we convert them to hex escapes (\xNN)
/// 2. Backspace (\b): regex_syntax interprets \b as word boundary,
///    so we convert to \x08
/// 3. Single-digit hex escapes (\xN): regex_syntax requires two digits,
///    so we pad to \x0N
fn translate_escape_sequences(input: &str) -> String {
    let mut result = String::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == '\\' && i + 1 < chars.len() {
            let next = chars[i + 1];

            // Handle \b -> \x08 (backspace, not word boundary)
            // In POSIX lex, \b means backspace (0x08), but regex_syntax
            // interprets \b as word boundary assertion
            if next == 'b' {
                result.push_str("\\x08");
                i += 2;
                continue;
            }

            // Handle \xN -> \x0N (pad single hex digit)
            // POSIX says \x followed by "the longest sequence of hexadecimal-digit
            // characters", which can be just one digit. regex_syntax requires two.
            if next == 'x' && i + 2 < chars.len() {
                let hex_start = i + 2;
                let mut hex_digits = String::new();
                let mut j = hex_start;

                // Collect hex digits
                while j < chars.len() && chars[j].is_ascii_hexdigit() {
                    hex_digits.push(chars[j]);
                    j += 1;
                }

                if hex_digits.len() == 1 {
                    // Single hex digit - pad with leading zero
                    result.push_str(&format!("\\x0{}", hex_digits));
                    i = j;
                    continue;
                } else if hex_digits.len() >= 2 {
                    // Two or more hex digits - take first two, leave rest as literal
                    result.push_str(&format!("\\x{}", &hex_digits[..2]));
                    // Any digits beyond the first two become literal characters
                    result.push_str(&hex_digits[2..]);
                    i = j;
                    continue;
                }
                // No hex digits after \x - leave as-is (will error in regex_syntax)
            }

            // Handle octal escapes (\NNN) -> \xNN
            // Check if this is an octal escape (digit 0-7)
            if next.is_ascii_digit() && next < '8' {
                // Consume up to 3 octal digits
                let mut octal_str = String::new();
                let mut j = i + 1;
                while j < chars.len() && octal_str.len() < 3 {
                    let c = chars[j];
                    if c.is_ascii_digit() && c < '8' {
                        octal_str.push(c);
                        j += 1;
                    } else {
                        break;
                    }
                }

                // Convert octal to byte value
                if let Ok(byte_val) = u8::from_str_radix(&octal_str, 8) {
                    // Output as hex escape \xNN
                    result.push_str(&format!("\\x{:02x}", byte_val));
                    i = j;
                    continue;
                }
            }
        }
        result.push(chars[i]);
        i += 1;
    }

    result
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

// translate lex-specific regex syntax to regex crate syntax
fn translate_ere(state: &mut ParseState, ere: &str) -> Result<String, String> {
    let mut re = String::new();
    let mut in_quotes = false;
    let mut in_brace = false;
    let mut brace_content = String::new();

    for ch in ere.chars() {
        if in_quotes && ch == '"' {
            in_quotes = false;
        } else if in_quotes {
            match ch {
                '*' => re.push_str(r"\x2a"),
                '+' => re.push_str(r"\x2b"),
                '.' => re.push_str(r"\x2e"),
                '{' => re.push_str(r"\x7b"),
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
                // Substitution reference
                match state.subs.get(&brace_content) {
                    Some(value) => re.push_str(value),
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
    }

    // Translate POSIX escape sequences to regex_syntax compatible format
    Ok(translate_escape_sequences(&re))
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

/// Parse anchoring and trailing context from a pattern
/// Returns (bol_anchor, main_pattern, trailing_context, eol_anchor)
fn parse_anchoring_and_trailing_context(pattern: &str) -> (bool, String, Option<String>, bool) {
    let mut main_pattern = pattern.to_string();
    let mut bol_anchor = false;
    let mut eol_anchor = false;
    let mut trailing_context = None;

    // Check for ^ at the beginning (BOL anchor)
    if main_pattern.starts_with('^') {
        bol_anchor = true;
        main_pattern = main_pattern[1..].to_string();
    }

    // Check for $ at the end (EOL anchor) - must be unescaped and outside brackets/quotes
    // $ is equivalent to /\n (trailing context with newline)
    if pattern_ends_with_unescaped_dollar(&main_pattern) {
        eol_anchor = true;
        main_pattern.pop(); // Remove the $
    }

    // Look for unescaped / (trailing context operator)
    // The / must not be inside brackets, quotes, or escaped
    if let Some(slash_pos) = find_trailing_context_slash(&main_pattern) {
        let tc = main_pattern[slash_pos + 1..].to_string();
        main_pattern = main_pattern[..slash_pos].to_string();
        trailing_context = Some(tc);
    }

    // If EOL anchor ($) was found, convert to trailing context /\n
    if eol_anchor {
        if trailing_context.is_some() {
            // Can't have both $ and trailing context - $ is already a form of trailing context
            // POSIX says this is undefined, but we'll treat $ as /\n
        }
        trailing_context = Some("\\n".to_string());
    }

    (bol_anchor, main_pattern, trailing_context, eol_anchor)
}

/// Check if pattern ends with an unescaped $ (not inside brackets or quotes)
fn pattern_ends_with_unescaped_dollar(pattern: &str) -> bool {
    if !pattern.ends_with('$') {
        return false;
    }

    let chars: Vec<char> = pattern.chars().collect();
    let len = chars.len();

    // Count backslashes before the final $
    let mut backslash_count = 0;
    let mut i = len - 2; // Start from character before $
    while i < len && chars[i] == '\\' {
        backslash_count += 1;
        if i == 0 {
            break;
        }
        i = i.saturating_sub(1);
    }

    // $ is escaped if preceded by odd number of backslashes
    if backslash_count % 2 == 1 {
        return false;
    }

    // Check if $ is inside brackets (simplified check)
    let mut in_brackets = false;
    let mut in_quotes = false;
    for (idx, ch) in chars.iter().enumerate() {
        if idx == len - 1 {
            break; // Don't count the final $
        }
        match ch {
            '"' if !in_brackets => in_quotes = !in_quotes,
            '[' if !in_quotes => in_brackets = true,
            ']' if !in_quotes => in_brackets = false,
            _ => {}
        }
    }

    !in_brackets && !in_quotes
}

/// Find the position of trailing context operator / (not inside brackets, quotes, or escaped)
fn find_trailing_context_slash(pattern: &str) -> Option<usize> {
    let chars: Vec<char> = pattern.chars().collect();
    let mut in_brackets = false;
    let mut in_quotes = false;
    let mut escape_next = false;

    for (idx, ch) in chars.iter().enumerate() {
        if escape_next {
            escape_next = false;
            continue;
        }
        match ch {
            '\\' => escape_next = true,
            '"' if !in_brackets => in_quotes = !in_quotes,
            '[' if !in_quotes => in_brackets = true,
            ']' if !in_quotes && in_brackets => in_brackets = false,
            '/' if !in_brackets && !in_quotes => return Some(idx),
            _ => {}
        }
    }

    None
}

/// Count the number of unescaped trailing context operators / in a pattern
fn count_trailing_context_slashes(pattern: &str) -> usize {
    let chars: Vec<char> = pattern.chars().collect();
    let mut in_brackets = false;
    let mut in_quotes = false;
    let mut escape_next = false;
    let mut count = 0;

    for ch in chars.iter() {
        if escape_next {
            escape_next = false;
            continue;
        }
        match ch {
            '\\' => escape_next = true,
            '"' if !in_brackets => in_quotes = !in_quotes,
            '[' if !in_quotes => in_brackets = true,
            ']' if !in_quotes && in_brackets => in_brackets = false,
            '/' if !in_brackets && !in_quotes => count += 1,
            _ => {}
        }
    }

    count
}

/// Validate pattern restrictions per POSIX specification
/// Returns Ok(()) if valid, Err(message) if invalid
fn validate_pattern_restrictions(pattern: &str) -> Result<(), String> {
    let chars: Vec<char> = pattern.chars().collect();
    let mut in_brackets = false;
    let mut in_quotes = false;
    let mut escape_next = false;
    let mut dollar_positions: Vec<usize> = Vec::new();
    let mut caret_positions: Vec<usize> = Vec::new();

    // First pass: find trailing context operator
    let has_trailing_context = find_trailing_context_slash(pattern).is_some();

    // Second pass: find all unescaped ^ and $ positions
    for (idx, ch) in chars.iter().enumerate() {
        if escape_next {
            escape_next = false;
            continue;
        }
        match ch {
            '\\' => escape_next = true,
            '"' if !in_brackets => in_quotes = !in_quotes,
            '[' if !in_quotes => in_brackets = true,
            ']' if !in_quotes && in_brackets => in_brackets = false,
            '^' if !in_brackets && !in_quotes => {
                // ^ inside [] is a negation character, which is valid anywhere
                caret_positions.push(idx);
            }
            '$' if !in_brackets && !in_quotes => {
                dollar_positions.push(idx);
            }
            _ => {}
        }
    }

    // Validate ^ positions - only valid at beginning
    for pos in &caret_positions {
        if *pos != 0 {
            return Err(format!(
                "'^' operator only valid at beginning of pattern, found at position {}",
                pos
            ));
        }
    }

    // Validate $ positions - only valid at end (and not with trailing context)
    for pos in &dollar_positions {
        // $ must be at the very end of the pattern
        if *pos != chars.len() - 1 {
            return Err(format!(
                "'$' operator only valid at end of pattern, found at position {}",
                pos
            ));
        }
        // $ cannot be used with trailing context
        if has_trailing_context {
            return Err(
                "'$' cannot be used with trailing context '/'; $ is equivalent to /\\n".to_string(),
            );
        }
    }

    // Validate only one trailing context operator
    let tc_count = count_trailing_context_slashes(pattern);
    if tc_count > 1 {
        return Err(format!(
            "Only one trailing context operator '/' allowed per pattern, found {}",
            tc_count
        ));
    }

    Ok(())
}

/// Parsed rule information
struct ParsedRuleInfo {
    ere: String,
    action: String,
    open_braces: u32,
    start_conditions: Vec<String>,
    bol_anchor: bool,
    trailing_context: Option<String>,
}

// parse a lex rule line, returning all rule components
fn parse_rule(state: &mut ParseState, line: &str) -> Result<ParsedRuleInfo, String> {
    // First extract any start conditions
    let (start_conditions, remaining) = extract_start_conditions(line);

    let pos = find_ere_end(remaining).map_err(|e| state.error(&e))?;
    let ere_raw = String::from(&remaining[..pos]);

    // Validate pattern restrictions per POSIX before processing
    validate_pattern_restrictions(&ere_raw).map_err(|e| state.error(&e))?;

    // Parse anchoring and trailing context before translating
    let (bol_anchor, ere_main, trailing_context, _eol_anchor) =
        parse_anchoring_and_trailing_context(&ere_raw);

    // Translate the main pattern (translate_ere already adds line numbers)
    let ere = translate_ere(state, &ere_main)?;

    // Translate trailing context if present (translate_ere already adds line numbers)
    let trailing_context = match trailing_context {
        Some(tc) => Some(translate_ere(state, &tc)?),
        None => None,
    };

    let action_ws = String::from(&remaining[pos..]);
    let action = action_ws.trim_start();
    let open_braces = parse_braces(0, action).map_err(|e| state.error(&e))?;

    Ok(ParsedRuleInfo {
        ere,
        action: action.to_string(),
        open_braces,
        start_conditions,
        bol_anchor,
        trailing_context,
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
                eprintln!(
                    "{}",
                    state.warning(&format!("unknown command in rules section: {}", cmd))
                );
            }
        }
    } else if state.open_braces > 0 {
        state.tmp_rule.action.push_str(line);
        state.open_braces = parse_braces(state.open_braces, line).map_err(|e| state.error(&e))?;
        if state.open_braces == 0 {
            let ere = state.tmp_rule.ere.clone();
            let action = state.tmp_rule.action.clone();
            let start_conditions = state.tmp_rule.start_conditions.clone();
            let bol_anchor = state.tmp_rule.bol_anchor;
            let trailing_context = state.tmp_rule.trailing_context.clone();
            state.push_rule(
                &ere,
                &action,
                start_conditions,
                bol_anchor,
                trailing_context,
            );
            state.tmp_rule = LexRule::new();
        }
    } else if state.in_def || (first_char.is_whitespace() && line.len() > 1) {
        state.internal_defs.push(String::from(line));
    } else if line.trim().is_empty() {
        return Ok(());
    } else {
        let info = parse_rule(state, line)?;
        if info.open_braces == 0 {
            state.push_rule(
                &info.ere,
                &info.action,
                info.start_conditions,
                info.bol_anchor,
                info.trailing_context,
            );
        } else {
            state.tmp_rule = LexRule {
                ere: info.ere,
                action: info.action,
                start_conditions: info.start_conditions,
                bol_anchor: info.bol_anchor,
                trailing_context: info.trailing_context,
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
        let test_content = r#"
%%
abc^def    return 1;
%%
"#;
        let input: Vec<String> = test_content.lines().map(|s| s.to_string()).collect();
        let result = parse(&input);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .contains("'^' operator only valid at beginning"));
    }

    #[test]
    fn test_pattern_validation_multiple_tc_parse_integration() {
        // Test that multiple trailing context operators are rejected during parsing
        let test_content = r#"
%%
abc/def/ghi    return 1;
%%
"#;
        let input: Vec<String> = test_content.lines().map(|s| s.to_string()).collect();
        let result = parse(&input);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .contains("Only one trailing context operator"));
    }

    #[test]
    fn test_error_messages_include_line_numbers() {
        // Test that error messages include line numbers
        let test_content = r#"
%%
abc^def    return 1;
%%
"#;
        let input: Vec<String> = test_content.lines().map(|s| s.to_string()).collect();
        let result = parse(&input);
        assert!(result.is_err());
        let err_msg = result.unwrap_err();
        // Should contain "line 3:" since the error is on line 3
        assert!(
            err_msg.contains("line 3:"),
            "Error message should contain line number: {}",
            err_msg
        );
        assert!(
            err_msg.contains("error:"),
            "Error message should contain 'error:': {}",
            err_msg
        );
    }

    #[test]
    fn test_undefined_substitution_error_has_line_number() {
        let test_content = r#"
DIGIT    [0-9]
%%
{UNDEFINED}    return 1;
%%
"#;
        let input: Vec<String> = test_content.lines().map(|s| s.to_string()).collect();
        let result = parse(&input);
        assert!(result.is_err());
        let err_msg = result.unwrap_err();
        assert!(
            err_msg.contains("line 4:"),
            "Error message should contain line number: {}",
            err_msg
        );
        assert!(
            err_msg.contains("undefined substitution"),
            "Error message should mention undefined substitution: {}",
            err_msg
        );
    }

    #[test]
    fn test_translate_escape_sequences_octal_basic() {
        // \101 = 65 decimal = 'A'
        assert_eq!(translate_escape_sequences(r"\101"), r"\x41");
        // \141 = 97 decimal = 'a'
        assert_eq!(translate_escape_sequences(r"\141"), r"\x61");
        // \0 = 0 = NUL
        assert_eq!(translate_escape_sequences(r"\0"), r"\x00");
        // \7 = 7 = BEL
        assert_eq!(translate_escape_sequences(r"\7"), r"\x07");
    }

    #[test]
    fn test_translate_escape_sequences_octal_two_digits() {
        // \12 = 10 decimal = newline
        assert_eq!(translate_escape_sequences(r"\12"), r"\x0a");
        // \40 = 32 decimal = space
        assert_eq!(translate_escape_sequences(r"\40"), r"\x20");
        // \77 = 63 decimal = '?'
        assert_eq!(translate_escape_sequences(r"\77"), r"\x3f");
    }

    #[test]
    fn test_translate_escape_sequences_octal_three_digits() {
        // \377 = 255 decimal = max byte value
        assert_eq!(translate_escape_sequences(r"\377"), r"\xff");
        // \000 = 0
        assert_eq!(translate_escape_sequences(r"\000"), r"\x00");
        // \177 = 127 decimal = DEL
        assert_eq!(translate_escape_sequences(r"\177"), r"\x7f");
    }

    #[test]
    fn test_translate_escape_sequences_octal_in_pattern() {
        // Octal escape in the middle of a pattern
        assert_eq!(translate_escape_sequences(r"a\101b"), r"a\x41b");
        // Multiple octal escapes
        assert_eq!(translate_escape_sequences(r"\101\102\103"), r"\x41\x42\x43");
        // Mixed with other escapes (note: \b becomes \x08)
        assert_eq!(translate_escape_sequences(r"\101\n\102"), r"\x41\n\x42");
    }

    #[test]
    fn test_translate_escape_sequences_non_octal() {
        // \8 and \9 are not valid octal - should be left alone
        assert_eq!(translate_escape_sequences(r"\8"), r"\8");
        assert_eq!(translate_escape_sequences(r"\9"), r"\9");
        // \n, \t, etc. should be left alone
        assert_eq!(translate_escape_sequences(r"\n"), r"\n");
        assert_eq!(translate_escape_sequences(r"\t"), r"\t");
        // \x with two hex digits - normalized format
        assert_eq!(translate_escape_sequences(r"\x41"), r"\x41");
    }

    #[test]
    fn test_translate_escape_sequences_octal_stops_at_three_digits() {
        // \1234 should be \x53 (octal 123 = 83) followed by '4'
        assert_eq!(translate_escape_sequences(r"\1234"), r"\x534");
        // \0009 should be \x00 followed by '9'
        assert_eq!(translate_escape_sequences(r"\0009"), r"\x009");
    }

    #[test]
    fn test_translate_escape_sequences_octal_stops_at_non_octal() {
        // \128 - octal 12 = 10, then '8' (not octal)
        assert_eq!(translate_escape_sequences(r"\128"), r"\x0a8");
        // \1a - octal 1 = 1, then 'a'
        assert_eq!(translate_escape_sequences(r"\1a"), r"\x01a");
    }

    #[test]
    fn test_translate_escape_sequences_backspace() {
        // \b in POSIX lex means backspace (0x08), not word boundary
        assert_eq!(translate_escape_sequences(r"\b"), r"\x08");
        // \b in a pattern
        assert_eq!(translate_escape_sequences(r"a\bc"), r"a\x08c");
        // Multiple \b
        assert_eq!(translate_escape_sequences(r"\b\b"), r"\x08\x08");
    }

    #[test]
    fn test_translate_escape_sequences_hex_padding() {
        // Single hex digit should be padded
        assert_eq!(translate_escape_sequences(r"\x4"), r"\x04");
        assert_eq!(translate_escape_sequences(r"\xa"), r"\x0a");
        assert_eq!(translate_escape_sequences(r"\xF"), r"\x0F");
        // Two hex digits - no change
        assert_eq!(translate_escape_sequences(r"\x41"), r"\x41");
        assert_eq!(translate_escape_sequences(r"\xff"), r"\xff");
        // More than two hex digits - take first two, rest as literal
        // \x412 -> \x41 (hex byte 0x41='A') + literal '2'
        assert_eq!(translate_escape_sequences(r"\x412"), r"\x412");
        // \xabc -> \xab (hex byte 0xAB) + literal 'c'
        assert_eq!(translate_escape_sequences(r"\xabc"), r"\xabc");
    }

    #[test]
    fn test_translate_escape_sequences_hex_in_pattern() {
        // \x4b has two hex digits ('4' and 'b'), so no padding needed
        assert_eq!(translate_escape_sequences(r"a\x4b"), r"a\x4b");
        assert_eq!(translate_escape_sequences(r"\x4B"), r"\x4B");
        // Single hex digit followed by non-hex character
        assert_eq!(translate_escape_sequences(r"\x4g"), r"\x04g");
        assert_eq!(translate_escape_sequences(r"a\x4z"), r"a\x04z");
    }

    #[test]
    fn test_translate_escape_sequences_mixed() {
        // Mix of octal, hex, and backspace escapes
        assert_eq!(
            translate_escape_sequences(r"\101\x42\b\103"),
            r"\x41\x42\x08\x43"
        );
        // Complex pattern
        assert_eq!(
            translate_escape_sequences(r"hello\bworld\x0a"),
            r"hello\x08world\x0a"
        );
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
