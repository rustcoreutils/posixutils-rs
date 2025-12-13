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
        }
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
                // do nothing; skip these
            }
            _ => {
                eprintln!("Unexpected command in definitions section: {}", cmd);
            }
        }
    } else if state.in_def || (first_char.is_whitespace() && line.len() > 1) {
        state.external_def.push(String::from(line));
    } else if let Some(caps) = state.sub_re.captures(line) {
        let name = caps.get(1).unwrap().as_str();
        let value = caps.get(2).unwrap().as_str();
        state.subs.insert(String::from(name), String::from(value));
    } else if !line.trim().is_empty() {
        let msg = format!("Unexpected line in definitions section: {}", line);
        return Err(msg);
    }
    Ok(())
}

// parse continued action line, counting open braces
fn parse_braces(open_braces: u32, line: &str) -> Result<u32, &'static str> {
    let mut open_braces = open_braces;
    for c in line.chars() {
        if c == '{' {
            open_braces += 1;
        } else if c == '}' {
            if open_braces == 0 {
                return Err("Unmatched closing brace");
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
fn find_ere_end(line: &str) -> Result<usize, &'static str> {
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
                    return Err("Unmatched closing square bracket");
                }
            }
            ')' => {
                if !inside_brackets && stack.pop() != Some(RegexType::Paren) {
                    return Err("Unmatched closing parenthesis");
                }
            }
            '}' => {
                if !inside_brackets && stack.pop() != Some(RegexType::Curly) {
                    return Err("Unmatched closing curly brace");
                }
            }
            _ => {
                if ch.is_whitespace() && stack.is_empty() {
                    return Ok(i);
                }
            }
        }
    }

    Err("Unterminated regular expression")
}

// translate lex-specific regex syntax to regex crate syntax
fn translate_ere(state: &mut ParseState, ere: &str) -> Result<String, String> {
    let mut re = String::new();
    let mut in_quotes = false;
    let mut in_sub = false;
    let mut sub_name = String::new();

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
        } else if in_sub && ch == '}' {
            match state.subs.get(&sub_name) {
                Some(value) => re.push_str(value),
                None => {
                    let msg = format!("Unknown substitution: {}", sub_name);
                    return Err(msg);
                }
            }
            in_sub = false;
            sub_name.clear();
        } else if in_sub {
            sub_name.push(ch);
        } else if ch == '"' {
            in_quotes = true;
        } else if ch == '{' {
            in_sub = true;
        } else {
            re.push(ch);
        }
    }

    Ok(re)
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

    let pos = find_ere_end(remaining)?;
    let ere_raw = String::from(&remaining[..pos]);

    // Parse anchoring and trailing context before translating
    let (bol_anchor, ere_main, trailing_context, _eol_anchor) =
        parse_anchoring_and_trailing_context(&ere_raw);

    // Translate the main pattern
    let ere = translate_ere(state, &ere_main)?;

    // Translate trailing context if present
    let trailing_context = match trailing_context {
        Some(tc) => Some(translate_ere(state, &tc)?),
        None => None,
    };

    let action_ws = String::from(&remaining[pos..]);
    let action = action_ws.trim_start();
    let open_braces = parse_braces(0, action)?;

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
                eprintln!("Unexpected command in Rules section: {}", cmd);
            }
        }
    } else if state.open_braces > 0 {
        state.tmp_rule.action.push_str(line);
        state.open_braces = parse_braces(state.open_braces, line)?;
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

    for line in input {
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
}
