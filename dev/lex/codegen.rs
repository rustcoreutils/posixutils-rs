//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Direct-coded C code generation for the lexical analyzer.
//!
//! This module generates POSIX-compliant C code from the DFA using a direct-coded
//! approach where each DFA state becomes a C label with switch-based transitions.
//! This is inspired by re2c's code generation strategy.

use crate::dfa::{Dfa, DfaInput, DfaState};
use crate::lexfile::LexInfo;
use std::collections::BTreeMap;
use std::io::{self, Write};

/// Check if `text` contains `word` as a standalone C identifier.
/// This avoids false positives from substrings (e.g., "REJECTED" matching "REJECT").
fn contains_identifier(text: &str, word: &str) -> bool {
    let mut start = 0;
    while let Some(pos) = text[start..].find(word) {
        let abs_pos = start + pos;
        let before_ok = abs_pos == 0
            || !text[..abs_pos]
                .chars()
                .next_back()
                .is_some_and(|c| c.is_alphanumeric() || c == '_');
        let after_ok = abs_pos + word.len() >= text.len()
            || !text[abs_pos + word.len()..]
                .chars()
                .next()
                .is_some_and(|c| c.is_alphanumeric() || c == '_');
        if before_ok && after_ok {
            return true;
        }
        start = abs_pos + 1;
    }
    false
}

/// Rule metadata for code generation
#[derive(Clone, Default)]
pub struct RuleMetadata {
    /// True if rule is anchored to beginning of line (^)
    pub bol_anchor: bool,
    /// Fixed length of main pattern if known (for trailing context rules)
    /// When present, yyleng is set to this value (excluding trailing context)
    pub main_pattern_len: Option<usize>,
    /// True if this rule has trailing context
    pub has_trailing_context: bool,
    /// True if this rule has variable-length trailing context
    /// (main pattern length is not fixed, requires runtime tracking)
    pub has_variable_trailing_context: bool,
}

/// Configuration for code generation
pub struct CodeGenConfig {
    /// Whether yytext is a pointer (true) or array (false)
    pub yytext_is_pointer: bool,
    /// Buffer size for yytext when using array
    pub yytext_size: usize,
    /// Start condition names (INITIAL is always index 0)
    pub start_conditions: Vec<String>,
    /// Metadata for each rule (indexed by rule number)
    pub rule_metadata: Vec<RuleMetadata>,
}

impl Default for CodeGenConfig {
    fn default() -> Self {
        CodeGenConfig {
            yytext_is_pointer: true,
            yytext_size: 8192,
            start_conditions: vec!["INITIAL".to_string()],
            rule_metadata: Vec::new(),
        }
    }
}

/// EOF rule with its start conditions
struct EofRule {
    rule_idx: usize,
    start_conditions: Vec<String>,
}

/// Pre-computed feature flags to avoid redundant iteration over rules/metadata
struct FeatureFlags {
    has_reject: bool,
    has_start_conditions: bool,
    has_bol_anchors: bool,
    has_trailing_context: bool,
    has_var_tc: bool,
    /// All EOF rules with their start conditions
    eof_rules: Vec<EofRule>,
    needs_accept_lists: bool,
}

impl FeatureFlags {
    /// Compute all feature flags in a single pass over rules and metadata
    fn compute(lexinfo: &LexInfo, config: &CodeGenConfig) -> Self {
        let has_start_conditions = config.start_conditions.len() > 1;

        // Single pass over rule_metadata
        let mut has_bol_anchors = false;
        let mut has_trailing_context = false;
        let mut has_var_tc = false;
        for meta in &config.rule_metadata {
            has_bol_anchors |= meta.bol_anchor;
            has_trailing_context |= meta.has_trailing_context;
            has_var_tc |= meta.has_variable_trailing_context;
        }

        // Single pass over rules
        let mut has_reject = false;
        let mut eof_rules = Vec::new();
        for (idx, rule) in lexinfo.rules.iter().enumerate() {
            if contains_identifier(&rule.action, "REJECT") {
                has_reject = true;
            }
            if rule.is_eof {
                eof_rules.push(EofRule {
                    rule_idx: idx,
                    start_conditions: rule.start_conditions.clone(),
                });
            }
        }

        let needs_accept_lists = has_reject || has_start_conditions || has_bol_anchors;

        FeatureFlags {
            has_reject,
            has_start_conditions,
            has_bol_anchors,
            has_trailing_context,
            has_var_tc,
            eof_rules,
            needs_accept_lists,
        }
    }
}

/// Generate the complete lex.yy.c output using direct-coded generation
pub fn generate<W: Write>(
    output: &mut W,
    dfa: &Dfa,
    lexinfo: &LexInfo,
    config: &CodeGenConfig,
) -> io::Result<()> {
    // Log generation info
    eprintln!(
        "lex: {} states, {} equivalence classes -> direct-coded generation",
        dfa.states.len(),
        dfa.char_classes.num_classes
    );

    // Pre-compute all feature flags in a single pass
    let flags = FeatureFlags::compute(lexinfo, config);

    write_header(output)?;
    write_includes(output)?;
    write_external_definitions(output, lexinfo)?;
    write_macros_and_types(output, lexinfo, config, &flags)?;
    write_char_class_table(output, dfa)?;
    write_num_states(output, dfa)?;
    // Only generate accept lists if needed for REJECT or alternate rule finding
    if flags.needs_accept_lists {
        write_accepting_list_table(output, dfa)?;
    }
    write_rule_condition_table(output, lexinfo, config, &flags)?;
    write_rule_metadata_tables(output, lexinfo, config, &flags)?;
    write_main_pattern_end_table(output, dfa, config)?;
    write_helper_functions(output, lexinfo)?;
    write_yylex_direct_coded(output, dfa, lexinfo, config, &flags)?;
    write_user_subroutines(output, lexinfo)?;

    Ok(())
}

fn write_header<W: Write>(output: &mut W) -> io::Result<()> {
    writeln!(output, "/* Generated by lex-rs - POSIX compatible lex */")?;
    writeln!(output, "/* Direct-coded scanner (re2c-style) */")?;
    writeln!(
        output,
        "/* DO NOT EDIT - This file was automatically generated */\n"
    )?;
    Ok(())
}

fn write_includes<W: Write>(output: &mut W) -> io::Result<()> {
    writeln!(
        output,
        r#"#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Forward declarations */
int yywrap(void);
"#
    )?;
    Ok(())
}

fn write_external_definitions<W: Write>(output: &mut W, lexinfo: &LexInfo) -> io::Result<()> {
    if !lexinfo.external_def.is_empty() {
        writeln!(output, "/* User external definitions */")?;
        for line in &lexinfo.external_def {
            write!(output, "{}", line)?;
        }
        writeln!(output)?;
    }
    Ok(())
}

fn write_macros_and_types<W: Write>(
    output: &mut W,
    lexinfo: &LexInfo,
    config: &CodeGenConfig,
    flags: &FeatureFlags,
) -> io::Result<()> {
    writeln!(
        output,
        r#"/* Lex macros and types */
#ifndef YY_BUF_SIZE
#define YY_BUF_SIZE 16384
#endif

#ifndef ECHO
#define ECHO fwrite(yytext, yyleng, 1, yyout)
#endif

#ifndef YY_INPUT
#define YY_INPUT(buf, result, max_size) \
    do {{ \
        if (yyin == NULL) yyin = stdin; \
        if (feof(yyin)) {{ result = 0; }} \
        else {{ \
            result = fread(buf, 1, max_size, yyin); \
        }} \
    }} while (0)
#endif
"#
    )?;

    // Generate start condition defines
    writeln!(output, "/* Start condition states */")?;
    for (idx, name) in config.start_conditions.iter().enumerate() {
        writeln!(output, "#define {} {}", name, idx)?;
    }
    writeln!(output)?;

    // BEGIN and YY_START macros - only use yy_start_state if there are multiple conditions
    let has_start_conditions = flags.has_start_conditions;
    if has_start_conditions {
        writeln!(
            output,
            r#"#ifndef BEGIN
#define BEGIN(x) (yy_start_state = (x))
#endif

#ifndef YY_START
#define YY_START yy_start_state
#endif
"#
        )?;
    } else {
        // With only INITIAL, BEGIN is a no-op and YY_START is always 0
        writeln!(
            output,
            r#"#ifndef BEGIN
#define BEGIN(x) ((void)0)
#endif

#ifndef YY_START
#define YY_START INITIAL
#endif
"#
        )?;
    }

    // yytext declaration - dynamically allocated for long token support
    if config.yytext_is_pointer {
        writeln!(
            output,
            r#"/* yytext as pointer - dynamically allocated */
static char *yy_yytext_buf = NULL;
static size_t yy_yytext_size = 0;
char *yytext = NULL;
"#
        )?;
    } else {
        writeln!(output, "/* yytext as array */")?;
        writeln!(output, "#define YYLMAX {}", config.yytext_size)?;
        writeln!(output, "char yytext[YYLMAX];\n")?;
    }

    writeln!(
        output,
        r#"/* Standard lex variables */
int yyleng;
FILE *yyin = NULL;
FILE *yyout = NULL;"#
    )?;
    // Only generate yy_start_state when there are multiple start conditions
    if has_start_conditions {
        writeln!(output, "static int yy_start_state = INITIAL;")?;
    }
    writeln!(
        output,
        r#"
/* Input buffer - dynamically allocated for long token support */
static unsigned char *yy_buffer = NULL;
static size_t yy_buffer_size = 0;
static unsigned char *YYCURSOR = NULL;
static unsigned char *YYLIMIT = NULL;
static unsigned char *YYTOKEN = NULL;
static unsigned char *YYMARKER = NULL;
static int yyaccept = -1;

/* Beginning of line tracking */
static int yy_at_bol = 1; /* Start at beginning of line */

/* REJECT support */
static int yy_reject_flag = 0;
static int yy_full_match_state = 0;  /* DFA state where match occurred */
"#
    )?;

    // REJECT history stack for shorter match support - only emit if any rule uses REJECT
    if flags.has_reject {
        writeln!(
            output,
            r#"
/* REJECT history stack for shorter match fallback */
#define YY_REJECT_STACK_SIZE 64
static struct {{
    unsigned char *marker;  /* Position of this accept */
    int state;              /* DFA state that accepted */
    int rule_idx;           /* Current index within accept list */
}} yy_reject_stack[YY_REJECT_STACK_SIZE];
static int yy_reject_top = 0;
"#
        )?;
    }

    writeln!(
        output,
        r#"/* Buffer refill resume state */
static int yy_resume_state = 0;  /* DFA state to resume from after refill */

#ifndef REJECT
#define REJECT {{ yy_reject_flag = 1; goto yy_find_next_match; }}
#endif

/* yymore support */
static int yy_more_flag = 0;
static int yy_more_len = 0;

#ifndef yymore
#define yymore() (yy_more_flag = 1)
#endif

/* yyless - return characters to input */
#ifndef yyless
#define yyless(n) do {{ \
    YYCURSOR = YYTOKEN + (n); \
    yyleng = (n); \
    yytext[yyleng] = '\0'; \
}} while (0)
#endif

/* unput now always inserts directly into main buffer - no separate pushback buffer */
"#
    )?;

    // Variable-length trailing context support - only emit if any rule uses it
    // Use per-rule array to correctly handle multiple rules with var-TC at the same DFA state
    // Track as offsets from YYTOKEN to avoid needing adjustment on buffer shifts
    if flags.has_var_tc {
        let num_rules = lexinfo.rules.len();
        writeln!(output, "/* Variable-length trailing context support */")?;
        writeln!(output, "#define YY_NUM_VAR_TC_RULES {}", num_rules)?;
        writeln!(
            output,
            "static int yy_main_end_offset[YY_NUM_VAR_TC_RULES]; /* Per-rule: offset from YYTOKEN where main pattern ends, or -1 */"
        )?;
        writeln!(output)?;
    }

    Ok(())
}

fn write_char_class_table<W: Write>(output: &mut W, dfa: &Dfa) -> io::Result<()> {
    writeln!(output, "/* Character to equivalence class mapping */")?;
    writeln!(
        output,
        "#define YY_NUM_CLASSES {}",
        dfa.char_classes.num_classes
    )?;
    write!(output, "static const unsigned char yy_ec[256] = {{")?;

    for (i, &class) in dfa.char_classes.char_to_class.iter().enumerate() {
        if i % 16 == 0 {
            write!(output, "\n    ")?;
        }
        write!(output, "{:3}", class)?;
        if i < 255 {
            write!(output, ",")?;
        }
    }

    writeln!(output, "\n}};\n")?;

    Ok(())
}

fn write_num_states<W: Write>(output: &mut W, dfa: &Dfa) -> io::Result<()> {
    writeln!(output, "#define YY_NUM_STATES {}", dfa.states.len())?;
    writeln!(output)?;
    Ok(())
}

/// Write the accepting rules list for REJECT support
fn write_accepting_list_table<W: Write>(output: &mut W, dfa: &Dfa) -> io::Result<()> {
    // Build the flattened list and index array
    let mut accept_list: Vec<i16> = Vec::new();
    let mut accept_idx: Vec<usize> = Vec::new();

    for state in &dfa.states {
        accept_idx.push(accept_list.len());
        for &rule in &state.accepting_rules {
            accept_list.push(rule as i16);
        }
    }
    accept_idx.push(accept_list.len()); // Sentinel for last state

    writeln!(output, "/* Accepting rules list for REJECT support */")?;
    writeln!(
        output,
        "/* yy_accept_list[yy_accept_idx[state]..yy_accept_idx[state+1]] gives all rules */"
    )?;

    // Write the index array
    write!(
        output,
        "static const unsigned short yy_accept_idx[YY_NUM_STATES + 1] = {{ "
    )?;
    for (i, idx) in accept_idx.iter().enumerate() {
        if i > 0 {
            write!(output, ", ")?;
        }
        write!(output, "{}", idx)?;
    }
    writeln!(output, " }};\n")?;

    // Write the rules list
    if accept_list.is_empty() {
        writeln!(output, "static const short yy_accept_list[1] = {{ -1 }};\n")?;
    } else {
        write!(
            output,
            "static const short yy_accept_list[{}] = {{ ",
            accept_list.len()
        )?;
        for (i, rule) in accept_list.iter().enumerate() {
            if i > 0 {
                write!(output, ", ")?;
            }
            write!(output, "{}", rule)?;
        }
        writeln!(output, " }};\n")?;
    }

    Ok(())
}

/// Write a table indicating which rules are active in which start conditions
fn write_rule_condition_table<W: Write>(
    output: &mut W,
    lexinfo: &LexInfo,
    config: &CodeGenConfig,
    flags: &FeatureFlags,
) -> io::Result<()> {
    // Only generate the table if we have more than just INITIAL
    if !flags.has_start_conditions {
        return Ok(());
    }

    let num_rules = lexinfo.rules.len();
    let num_conditions = config.start_conditions.len();

    writeln!(output, "/* Rule active-in-condition table */")?;
    writeln!(
        output,
        "/* yy_rule_cond[rule][condition] = 1 if rule is active in that condition */"
    )?;
    writeln!(output, "#define YY_NUM_RULES {}", num_rules)?;
    writeln!(output, "#define YY_NUM_CONDITIONS {}", num_conditions)?;
    writeln!(
        output,
        "static const unsigned char yy_rule_cond[YY_NUM_RULES][YY_NUM_CONDITIONS] = {{"
    )?;

    for (rule_idx, rule) in lexinfo.rules.iter().enumerate() {
        write!(output, "    /* rule {} */ {{ ", rule_idx)?;
        for (cond_idx, cond_name) in config.start_conditions.iter().enumerate() {
            let active = is_rule_active_in_condition(rule, cond_name, lexinfo);
            if cond_idx > 0 {
                write!(output, ", ")?;
            }
            write!(output, "{}", if active { 1 } else { 0 })?;
        }
        writeln!(output, " }},")?;
    }

    writeln!(output, "}};\n")?;

    Ok(())
}

/// Check if a rule is active in a given start condition
fn is_rule_active_in_condition(
    rule: &crate::lexfile::LexRule,
    condition: &str,
    lexinfo: &LexInfo,
) -> bool {
    // If rule has explicit start conditions, check if this condition is listed
    if !rule.start_conditions.is_empty() {
        return rule.start_conditions.contains(&condition.to_string());
    }

    // Rule has no explicit conditions
    // For INITIAL or %s (inclusive) conditions, the rule is active
    // For %x (exclusive) conditions, the rule is NOT active
    if condition == "INITIAL" || lexinfo.cond_start.contains(&condition.to_string()) {
        return true;
    }

    // This is an exclusive condition (%x), and rule has no explicit conditions
    false
}

/// Write rule metadata tables for BOL anchoring and trailing context
fn write_rule_metadata_tables<W: Write>(
    output: &mut W,
    lexinfo: &LexInfo,
    config: &CodeGenConfig,
    flags: &FeatureFlags,
) -> io::Result<()> {
    let num_rules = lexinfo.rules.len();
    if num_rules == 0 {
        return Ok(());
    }

    // Define YY_NUM_RULES if needed for BOL or trailing context checks
    if (flags.has_bol_anchors || flags.has_trailing_context) && !flags.has_start_conditions {
        writeln!(output, "#ifndef YY_NUM_RULES")?;
        writeln!(output, "#define YY_NUM_RULES {}", num_rules)?;
        writeln!(output, "#endif\n")?;
    }

    if flags.has_bol_anchors {
        // Write BOL anchor table (1 = rule requires BOL, 0 = no requirement)
        writeln!(
            output,
            "/* BOL anchor table - 1 if rule requires beginning of line */"
        )?;
        write!(output, "static const int yy_rule_bol[{}] = {{ ", num_rules)?;
        for (i, rule) in lexinfo.rules.iter().enumerate() {
            let bol = if i < config.rule_metadata.len() {
                config.rule_metadata[i].bol_anchor
            } else {
                rule.bol_anchor
            };
            write!(output, "{}", if bol { 1 } else { 0 })?;
            if i < num_rules - 1 {
                write!(output, ", ")?;
            }
        }
        writeln!(output, " }};\n")?;
    }

    if flags.has_trailing_context {
        writeln!(
            output,
            "/* Main pattern length table (for trailing context) */"
        )?;
        writeln!(
            output,
            "/* -1 = no trailing context, >= 0 = fixed main pattern length, -2 = variable */"
        )?;
        write!(
            output,
            "static const int yy_rule_main_len[{}] = {{ ",
            num_rules
        )?;
        for (i, _) in lexinfo.rules.iter().enumerate() {
            let main_len = if i < config.rule_metadata.len() {
                let meta = &config.rule_metadata[i];
                if meta.has_trailing_context {
                    match meta.main_pattern_len {
                        Some(len) => len as i32,
                        None => -2, // Variable length main pattern
                    }
                } else {
                    -1 // No trailing context
                }
            } else {
                -1
            };
            write!(output, "{}", main_len)?;
            if i < num_rules - 1 {
                write!(output, ", ")?;
            }
        }
        writeln!(output, " }};\n")?;
    }

    Ok(())
}

/// Write table mapping DFA states to main pattern end rules (for variable-length trailing context)
fn write_main_pattern_end_table<W: Write>(
    _output: &mut W,
    _dfa: &Dfa,
    _config: &CodeGenConfig,
) -> io::Result<()> {
    // Note: yy_state_main_end and yy_rule_var_tc tables were originally generated here
    // but are unused - the implementation uses yy_main_end_offset[] (per-rule offset from
    // YYTOKEN) at runtime instead of static tables. Removed to avoid unused variable warnings.
    Ok(())
}

/// Generate EOF handling code that respects start conditions
fn write_eof_dispatch<W: Write>(
    output: &mut W,
    eof_rules: &[EofRule],
    config: &CodeGenConfig,
    indent: &str,
) -> io::Result<()> {
    if eof_rules.is_empty() {
        // No EOF rules - just call yywrap
        writeln!(output, "{}/* EOF - call yywrap() */", indent)?;
        writeln!(output, "{}if (yywrap()) return 0;", indent)?;
        writeln!(output, "{}goto yy_scan;", indent)?;
    } else if eof_rules.len() == 1 && eof_rules[0].start_conditions.is_empty() {
        // Single unconditional EOF rule
        let idx = eof_rules[0].rule_idx;
        writeln!(output, "{}/* Execute <<EOF>> rule */", indent)?;
        writeln!(output, "{}yyleng = 0;", indent)?;
        writeln!(output, "{}yytext[0] = '\\0';", indent)?;
        writeln!(output, "{}goto yy_action_{};", indent, idx)?;
    } else if config.start_conditions.len() <= 1 && eof_rules.len() == 1 {
        // Single start condition (INITIAL only) with single conditional EOF rule
        // Check if the EOF rule applies to INITIAL
        let rule = &eof_rules[0];
        if rule.start_conditions.is_empty()
            || rule.start_conditions.contains(&"INITIAL".to_string())
        {
            writeln!(output, "{}/* Execute <<EOF>> rule */", indent)?;
            writeln!(output, "{}yyleng = 0;", indent)?;
            writeln!(output, "{}yytext[0] = '\\0';", indent)?;
            writeln!(output, "{}goto yy_action_{};", indent, rule.rule_idx)?;
        } else {
            writeln!(output, "{}/* EOF - call yywrap() */", indent)?;
            writeln!(output, "{}if (yywrap()) return 0;", indent)?;
            writeln!(output, "{}goto yy_scan;", indent)?;
        }
    } else {
        // Multiple start conditions - need dispatch
        writeln!(
            output,
            "{}/* Handle EOF based on start condition */",
            indent
        )?;
        writeln!(output, "{}{{", indent)?;
        writeln!(output, "{}    int yy_eof_rule = -1;", indent)?;
        writeln!(output, "{}    switch (yy_start_state) {{", indent)?;

        for (cond_idx, cond_name) in config.start_conditions.iter().enumerate() {
            // Find EOF rule for this condition
            let matching_rule = eof_rules
                .iter()
                .find(|r| r.start_conditions.is_empty() || r.start_conditions.contains(cond_name));
            if let Some(rule) = matching_rule {
                writeln!(
                    output,
                    "{}        case {}: yy_eof_rule = {}; break; /* {} */",
                    indent, cond_idx, rule.rule_idx, cond_name
                )?;
            }
        }
        writeln!(output, "{}        default: break;", indent)?;
        writeln!(output, "{}    }}", indent)?;
        writeln!(output, "{}    if (yy_eof_rule >= 0) {{", indent)?;
        writeln!(output, "{}        yyleng = 0;", indent)?;
        writeln!(output, "{}        yytext[0] = '\\0';", indent)?;
        writeln!(output, "{}        switch (yy_eof_rule) {{", indent)?;
        for rule in eof_rules {
            writeln!(
                output,
                "{}            case {}: goto yy_action_{};",
                indent, rule.rule_idx, rule.rule_idx
            )?;
        }
        writeln!(output, "{}            default: break;", indent)?;
        writeln!(output, "{}        }}", indent)?;
        writeln!(output, "{}    }}", indent)?;
        writeln!(
            output,
            "{}    /* No EOF rule for this condition - call yywrap() */",
            indent
        )?;
        writeln!(output, "{}    if (yywrap()) return 0;", indent)?;
        writeln!(output, "{}    goto yy_scan;", indent)?;
        writeln!(output, "}}")?;
    }
    Ok(())
}

fn write_helper_functions<W: Write>(output: &mut W, lexinfo: &LexInfo) -> io::Result<()> {
    // input() function - conditionally generated based on %option noinput
    if !lexinfo.options.noinput {
        writeln!(
            output,
            r#"/* input - read one character from input */
static int input(void)
{{
    /* Check main buffer - unput() now always inserts directly here */
    if (YYCURSOR < YYLIMIT) {{
        return *YYCURSOR++;
    }}
    /* Need to refill buffer */
    if (yyin == NULL) yyin = stdin;
    return getc(yyin);  /* Returns EOF (-1) on end of file */
}}
"#
        )?;
    }

    // unput() function - conditionally generated based on %option nounput
    if !lexinfo.options.nounput {
        writeln!(
            output,
            r#"/* unput - push character back to input
 * Inserts directly into main buffer (not a separate pushback buffer) so the
 * DFA sees the character immediately on the next scan.
 * Two paths: (1) if room before YYCURSOR, decrement and store there;
 *            (2) otherwise, shift buffer contents right to make room at start.
 */
static void unput(int c)
{{
    if (YYCURSOR > yy_buffer) {{
        /* Room before cursor - just back up and insert */
        *--YYCURSOR = (unsigned char)c;
    }} else {{
        /* At start of buffer - need to shift content right to make room */
        size_t yy_remain = YYLIMIT - yy_buffer;
        /* Grow buffer if full */
        if (yy_remain >= yy_buffer_size) {{
            size_t new_size = yy_buffer_size * 2;
            size_t cursor_off = YYCURSOR - yy_buffer;
            size_t limit_off = YYLIMIT - yy_buffer;
            size_t token_off = YYTOKEN - yy_buffer;
            size_t marker_off = YYMARKER - yy_buffer;
            unsigned char *new_buf = (unsigned char *)realloc(yy_buffer, new_size + 2);
            if (new_buf == NULL) {{
                fprintf(stderr, "lex: out of memory in unput()\\n");
                return;
            }}
            yy_buffer = new_buf;
            yy_buffer_size = new_size;
            YYCURSOR = yy_buffer + cursor_off;
            YYLIMIT = yy_buffer + limit_off;
            YYTOKEN = yy_buffer + token_off;
            YYMARKER = yy_buffer + marker_off;
        }}
        if (yy_remain > 0) {{
            memmove(yy_buffer + 1, yy_buffer, yy_remain);
        }}
        yy_buffer[0] = (unsigned char)c;
        YYLIMIT++;
        /* YYCURSOR stays at yy_buffer, now pointing to inserted char */
    }}
}}
"#
        )?;
    }

    Ok(())
}

/// Build a map of equivalence class -> target state for transitions from a DFA state
fn build_class_transitions(dfa: &Dfa, state: &DfaState) -> BTreeMap<usize, usize> {
    let mut class_to_target: BTreeMap<usize, usize> = BTreeMap::new();

    for (input, &target) in &state.transitions {
        let DfaInput::Char(ch) = input;
        let ch_code = *ch as u32;
        if ch_code < 256 {
            let class_idx = dfa.char_classes.char_to_class[ch_code as usize] as usize;
            class_to_target.insert(class_idx, target);
        }
    }

    class_to_target
}

/// Write a single DFA state as a labeled block with switch-based transitions
fn write_dfa_state<W: Write>(
    output: &mut W,
    dfa: &Dfa,
    state_idx: usize,
    state: &DfaState,
    config: &CodeGenConfig,
    flags: &FeatureFlags,
) -> io::Result<()> {
    writeln!(output, "yy_state_{}:", state_idx)?;

    // If this is an accepting state, save marker position and state info
    if let Some(rule) = state.accepting {
        writeln!(output, "    /* Accepting state for rule {} */", rule)?;
        writeln!(output, "    YYMARKER = YYCURSOR;")?;
        writeln!(output, "    yyaccept = {};", rule)?;
        writeln!(output, "    yy_full_match_state = {};", state_idx)?;
        // Push to REJECT history stack for shorter match fallback
        if flags.has_reject {
            writeln!(output, "    if (yy_reject_top < YY_REJECT_STACK_SIZE) {{")?;
            writeln!(
                output,
                "        yy_reject_stack[yy_reject_top].marker = YYCURSOR;"
            )?;
            writeln!(
                output,
                "        yy_reject_stack[yy_reject_top].state = {};",
                state_idx
            )?;
            writeln!(
                output,
                "        yy_reject_stack[yy_reject_top].rule_idx = 0;"
            )?;
            writeln!(output, "        yy_reject_top++;")?;
            writeln!(output, "    }}")?;
        }
    }

    // Track main pattern end for variable-length trailing context
    // Track ALL rules that have their main pattern end at this state, using offset from YYTOKEN
    if flags.has_var_tc && !state.main_pattern_end_rules.is_empty() {
        for &rule in &state.main_pattern_end_rules {
            if rule < config.rule_metadata.len()
                && config.rule_metadata[rule].has_variable_trailing_context
            {
                writeln!(output, "    /* Main pattern ends here for rule {} */", rule)?;
                writeln!(
                    output,
                    "    yy_main_end_offset[{}] = (int)(YYCURSOR - YYTOKEN);",
                    rule
                )?;
            }
        }
    }

    // Check for end of input - save state for resume after refill
    writeln!(
        output,
        "    if (YYCURSOR >= YYLIMIT) {{ yy_resume_state = {}; goto yy_fill_or_eof; }}",
        state_idx
    )?;

    // Read next character and switch on equivalence class
    writeln!(output, "    switch (yy_ec[*YYCURSOR++]) {{")?;

    // Get transitions grouped by equivalence class
    let transitions = build_class_transitions(dfa, state);

    for (class, target) in &transitions {
        writeln!(output, "        case {}: goto yy_state_{};", class, target)?;
    }

    writeln!(output, "        default: goto yy_fail;")?;
    writeln!(output, "    }}")?;
    writeln!(output)?;

    Ok(())
}

/// Write the direct-coded yylex function
fn write_yylex_direct_coded<W: Write>(
    output: &mut W,
    dfa: &Dfa,
    lexinfo: &LexInfo,
    config: &CodeGenConfig,
    flags: &FeatureFlags,
) -> io::Result<()> {
    // Use pre-computed flags
    let has_start_conditions = flags.has_start_conditions;
    let has_bol_anchors = flags.has_bol_anchors;
    let has_trailing_context = flags.has_trailing_context;
    let has_var_tc = flags.has_var_tc;
    let eof_rules = &flags.eof_rules;
    let has_reject = flags.has_reject;

    writeln!(output, "/* The main lexer function - direct coded */")?;
    writeln!(output, "int yylex(void)")?;
    writeln!(output, "{{")?;

    // Initialize yyin/yyout
    writeln!(output, "    if (yyin == NULL) yyin = stdin;")?;
    writeln!(output, "    if (yyout == NULL) yyout = stdout;")?;
    writeln!(output)?;

    // Initialize buffer on first call
    writeln!(output, "    /* Initialize buffers on first call */")?;
    writeln!(output, "    if (yy_buffer == NULL) {{")?;
    writeln!(output, "        yy_buffer_size = YY_BUF_SIZE;")?;
    writeln!(
        output,
        "        yy_buffer = (unsigned char *)malloc(yy_buffer_size + 2);"
    )?;
    writeln!(output, "        if (yy_buffer == NULL) {{")?;
    writeln!(
        output,
        "            fprintf(stderr, \"lex: out of memory for input buffer\\n\");"
    )?;
    writeln!(output, "            return -1;")?;
    writeln!(output, "        }}")?;
    writeln!(output, "        YYCURSOR = yy_buffer;")?;
    writeln!(output, "        YYLIMIT = yy_buffer;")?;
    writeln!(output, "        YYTOKEN = yy_buffer;")?;
    writeln!(output, "        YYMARKER = yy_buffer;")?;
    if config.yytext_is_pointer {
        writeln!(output, "        yy_yytext_size = YY_BUF_SIZE;")?;
        writeln!(
            output,
            "        yy_yytext_buf = (char *)malloc(yy_yytext_size + 1);"
        )?;
        writeln!(output, "        if (yy_yytext_buf == NULL) {{")?;
        writeln!(
            output,
            "            fprintf(stderr, \"lex: out of memory for yytext buffer\\n\");"
        )?;
        writeln!(output, "            free(yy_buffer);")?;
        writeln!(output, "            yy_buffer = NULL;")?;
        writeln!(output, "            return -1;")?;
        writeln!(output, "        }}")?;
        writeln!(output, "        yytext = yy_yytext_buf;")?;
    }
    writeln!(output, "    }}")?;
    writeln!(output)?;

    // Main scanning loop entry point
    writeln!(output, "yy_scan:")?;

    // User code from indented lines before first rule
    if !lexinfo.internal_defs.is_empty() {
        writeln!(
            output,
            "    /* User code from rules section (runs on each yylex call) */"
        )?;
        for line in &lexinfo.internal_defs {
            write!(output, "    {}", line)?;
        }
        writeln!(output)?;
    }

    // Buffer refill check
    writeln!(output, "    /* Check if buffer needs refill */")?;
    writeln!(output, "    if (YYCURSOR >= YYLIMIT) {{")?;
    writeln!(output, "        int yy_result;")?;
    writeln!(
        output,
        "        /* unput() inserts directly into buffer, no separate drain needed */"
    )?;
    writeln!(
        output,
        "        YY_INPUT(yy_buffer, yy_result, (int)yy_buffer_size);"
    )?;
    writeln!(output, "        YYLIMIT = yy_buffer + yy_result;")?;
    writeln!(output, "        YYCURSOR = yy_buffer;")?;
    writeln!(output, "        if (yy_result == 0) {{")?;

    // Handle EOF with start condition awareness
    write_eof_dispatch(output, eof_rules, config, "            ")?;
    writeln!(output, "        }}")?;
    writeln!(output, "    }}")?;
    writeln!(output)?;

    // Initialize for new token
    writeln!(output, "    /* Initialize for new token */")?;
    writeln!(output, "    YYTOKEN = YYCURSOR;")?;
    writeln!(output, "    yyaccept = -1;")?;
    writeln!(output, "    yy_reject_flag = 0;")?;
    writeln!(output, "    yy_full_match_state = 0;")?;
    if has_reject {
        writeln!(
            output,
            "    yy_reject_top = 0; /* Reset REJECT history stack */"
        )?;
    }
    if has_var_tc {
        // Reset per-rule main pattern end tracking for new token
        // Use memset to set all bytes to 0xFF, giving -1 for each int (two's complement)
        writeln!(
            output,
            "    memset(yy_main_end_offset, -1, sizeof(yy_main_end_offset));"
        )?;
    }
    // Save BOL status at token start (for BOL anchor validation)
    if has_bol_anchors {
        writeln!(output, "    int yy_token_started_at_bol = yy_at_bol;")?;
    }
    writeln!(output)?;

    // Start condition dispatch
    if has_start_conditions {
        writeln!(output, "    /* Dispatch based on start condition */")?;
        writeln!(output, "    switch (yy_start_state) {{")?;
        for (idx, name) in config.start_conditions.iter().enumerate() {
            // For now, all start conditions share state 0
            // A more sophisticated implementation would have separate start states
            writeln!(
                output,
                "        case {}: /* {} */ goto yy_state_0;",
                idx, name
            )?;
        }
        writeln!(output, "        default: goto yy_state_0;")?;
        writeln!(output, "    }}")?;
    } else {
        writeln!(output, "    goto yy_state_0;")?;
    }
    writeln!(output)?;

    // Generate all DFA states
    for (state_idx, state) in dfa.states.iter().enumerate() {
        write_dfa_state(output, dfa, state_idx, state, config, flags)?;
    }

    // YYFILL/EOF block
    writeln!(output, "yy_fill_or_eof:")?;
    writeln!(output, "    /* End of buffer reached during scan */")?;
    writeln!(
        output,
        "    /* Try to refill buffer - may find longer match */"
    )?;
    writeln!(output, "    {{")?;
    writeln!(output, "        int yy_result;")?;
    writeln!(
        output,
        "        /* Save current offsets relative to buffer */"
    )?;
    writeln!(output, "        int yy_token_offset = YYTOKEN - yy_buffer;")?;
    writeln!(
        output,
        "        int yy_cursor_offset = YYCURSOR - yy_buffer;"
    )?;
    writeln!(
        output,
        "        int yy_marker_offset = YYMARKER - yy_buffer;"
    )?;
    // Note: yy_main_end_offset[] tracks offsets from YYTOKEN, so no adjustment needed on buffer shifts
    writeln!(
        output,
        "        /* Shift remaining data to start of buffer */"
    )?;
    writeln!(output, "        int yy_remain = (int)(YYLIMIT - YYTOKEN);")?;
    // Dynamic buffer growth: if token approaches buffer size, grow the buffer
    writeln!(
        output,
        "        /* Grow buffer if token is getting too long */"
    )?;
    writeln!(
        output,
        "        if (YYTOKEN == yy_buffer && (size_t)yy_remain >= yy_buffer_size - 256) {{"
    )?;
    writeln!(output, "            size_t new_size = yy_buffer_size * 2;")?;
    writeln!(output, "            /* Save offsets before realloc */")?;
    writeln!(
        output,
        "            size_t cursor_off = YYCURSOR - yy_buffer;"
    )?;
    writeln!(
        output,
        "            size_t token_off = YYTOKEN - yy_buffer;"
    )?;
    writeln!(
        output,
        "            size_t marker_off = YYMARKER - yy_buffer;"
    )?;
    writeln!(
        output,
        "            size_t limit_off = YYLIMIT - yy_buffer;"
    )?;
    writeln!(
        output,
        "            unsigned char *new_buf = (unsigned char *)realloc(yy_buffer, new_size + 2);"
    )?;
    writeln!(output, "            if (new_buf == NULL) {{")?;
    writeln!(
        output,
        "                fprintf(stderr, \"lex: out of memory growing buffer to %zu bytes\\n\", new_size);"
    )?;
    writeln!(output, "                return -1;")?;
    writeln!(output, "            }}")?;
    writeln!(output, "            yy_buffer = new_buf;")?;
    writeln!(output, "            yy_buffer_size = new_size;")?;
    writeln!(output, "            YYCURSOR = yy_buffer + cursor_off;")?;
    writeln!(output, "            YYTOKEN = yy_buffer + token_off;")?;
    writeln!(output, "            YYMARKER = yy_buffer + marker_off;")?;
    writeln!(output, "            YYLIMIT = yy_buffer + limit_off;")?;
    writeln!(output, "        }}")?;
    writeln!(
        output,
        "        /* Move unscanned data to buffer start; skip if yy_remain==0 (nothing to preserve) */"
    )?;
    writeln!(
        output,
        "        if (yy_remain > 0 && YYTOKEN > yy_buffer) {{"
    )?;
    writeln!(
        output,
        "            memmove(yy_buffer, YYTOKEN, yy_remain);"
    )?;
    writeln!(output, "        }}")?;
    writeln!(
        output,
        "        YY_INPUT(yy_buffer + yy_remain, yy_result, (int)(yy_buffer_size - yy_remain));"
    )?;
    writeln!(output, "        if (yy_result == 0) {{")?;
    writeln!(
        output,
        "            /* True EOF - no more input available */"
    )?;
    writeln!(output, "            if (yy_remain == 0) {{")?;
    writeln!(
        output,
        "                /* Buffer completely empty - handle EOF */"
    )?;
    write_eof_dispatch(output, eof_rules, config, "                ")?;
    writeln!(output, "            }}")?;
    writeln!(
        output,
        "            /* Have remaining data - finalize with current match */"
    )?;
    writeln!(output, "            YYLIMIT = yy_buffer + yy_remain;")?;
    writeln!(output, "            YYTOKEN = yy_buffer;")?;
    writeln!(
        output,
        "            YYCURSOR = yy_buffer + (yy_cursor_offset - yy_token_offset);"
    )?;
    writeln!(
        output,
        "            YYMARKER = yy_buffer + (yy_marker_offset - yy_token_offset);"
    )?;
    // Note: yy_main_end_offset[] tracks offsets from YYTOKEN, no adjustment needed on shift
    writeln!(output, "            if (yyaccept >= 0) {{")?;
    writeln!(output, "                goto yy_fail;")?;
    writeln!(output, "            }}")?;
    writeln!(
        output,
        "            /* No match - default action on remaining */"
    )?;
    writeln!(output, "            yy_at_bol = (*YYTOKEN == '\\n');")?;
    writeln!(output, "            putc(*YYTOKEN++, yyout);")?;
    writeln!(output, "            YYCURSOR = YYTOKEN;")?;
    writeln!(output, "            goto yy_scan;")?;
    writeln!(output, "        }}")?;
    writeln!(
        output,
        "        /* Refill succeeded - adjust pointers and resume scanning */"
    )?;
    writeln!(
        output,
        "        YYLIMIT = yy_buffer + yy_remain + yy_result;"
    )?;
    writeln!(output, "        YYTOKEN = yy_buffer;")?;
    writeln!(
        output,
        "        YYCURSOR = yy_buffer + (yy_cursor_offset - yy_token_offset);"
    )?;
    writeln!(
        output,
        "        YYMARKER = yy_buffer + (yy_marker_offset - yy_token_offset);"
    )?;
    // Note: yy_main_end_offset[] tracks offsets from YYTOKEN, no adjustment needed on refill
    writeln!(
        output,
        "        /* Resume scanning from the DFA state that hit buffer end */"
    )?;
    writeln!(output, "        switch (yy_resume_state) {{")?;
    for state_idx in 0..dfa.states.len() {
        writeln!(
            output,
            "            case {}: goto yy_state_{};",
            state_idx, state_idx
        )?;
    }
    writeln!(output, "            default: goto yy_state_0;")?;
    writeln!(output, "        }}")?;
    writeln!(output, "    }}")?;
    writeln!(output)?;

    // YYFAIL block - handle match or default action
    writeln!(output, "yy_fail:")?;
    // Only generate yy_find_next_match label if any rule uses REJECT
    if has_reject {
        writeln!(output, "yy_find_next_match:")?;
    }
    writeln!(output, "    /* Match failed or end of automaton reached */")?;
    writeln!(output, "    if (yyaccept < 0) {{")?;
    writeln!(
        output,
        "        /* No match - default action (ECHO one char) */"
    )?;
    writeln!(output, "        if (YYTOKEN < YYLIMIT) {{")?;
    writeln!(output, "            yy_at_bol = (*YYTOKEN == '\\n');")?;
    writeln!(output, "            putc(*YYTOKEN++, yyout);")?;
    writeln!(output, "            YYCURSOR = YYTOKEN;")?;
    writeln!(output, "            goto yy_scan;")?;
    writeln!(output, "        }}")?;
    writeln!(output, "        /* EOF - consult yywrap() */")?;
    writeln!(output, "        if (yywrap()) return 0;")?;
    writeln!(output, "        goto yy_scan;")?;
    writeln!(output, "    }}")?;
    writeln!(output)?;

    // REJECT support with shorter match fallback - only generate if any rule uses REJECT
    if has_reject {
        writeln!(
            output,
            "    /* REJECT support: find next valid rule, trying shorter matches if needed */"
        )?;
        writeln!(output, "    if (yy_reject_flag) {{")?;
        writeln!(output, "        int yy_found = 0;")?;
        writeln!(
            output,
            "        /* First try alternate rules at current position */"
        )?;
        writeln!(output, "        {{")?;
        writeln!(output, "            int yy_i;")?;
        writeln!(
            output,
            "            int yy_start_idx = yy_accept_idx[yy_full_match_state];"
        )?;
        writeln!(
            output,
            "            int yy_end_idx = yy_accept_idx[yy_full_match_state + 1];"
        )?;
        writeln!(output, "            int yy_skip_until_after = yyaccept;")?;
        writeln!(output, "            int yy_skipping = 1;")?;
        writeln!(
            output,
            "            for (yy_i = yy_start_idx; yy_i < yy_end_idx; yy_i++) {{"
        )?;
        writeln!(
            output,
            "                int yy_rule = yy_accept_list[yy_i];"
        )?;
        writeln!(
            output,
            "                if (yy_skipping) {{ if (yy_rule == yy_skip_until_after) yy_skipping = 0; continue; }}"
        )?;
        if has_start_conditions {
            writeln!(
                output,
                "                if (!yy_rule_cond[yy_rule][yy_start_state]) continue;"
            )?;
        }
        if has_bol_anchors {
            writeln!(
                output,
                "                if (yy_rule_bol[yy_rule] && !yy_token_started_at_bol) continue;"
            )?;
        }
        writeln!(output, "                yyaccept = yy_rule;")?;
        writeln!(output, "                yy_found = 1;")?;
        writeln!(output, "                break;")?;
        writeln!(output, "            }}")?;
        writeln!(output, "        }}")?;
        writeln!(
            output,
            "        /* If not found, try shorter matches from history stack */"
        )?;
        writeln!(output, "        while (!yy_found && yy_reject_top > 0) {{")?;
        writeln!(output, "            yy_reject_top--;")?;
        writeln!(
            output,
            "            YYMARKER = yy_reject_stack[yy_reject_top].marker;"
        )?;
        writeln!(
            output,
            "            yy_full_match_state = yy_reject_stack[yy_reject_top].state;"
        )?;
        writeln!(output, "            {{")?;
        writeln!(output, "                int yy_i;")?;
        writeln!(
            output,
            "                int yy_start_idx = yy_accept_idx[yy_full_match_state] + yy_reject_stack[yy_reject_top].rule_idx;"
        )?;
        writeln!(
            output,
            "                int yy_end_idx = yy_accept_idx[yy_full_match_state + 1];"
        )?;
        writeln!(
            output,
            "                for (yy_i = yy_start_idx; yy_i < yy_end_idx; yy_i++) {{"
        )?;
        writeln!(
            output,
            "                    int yy_rule = yy_accept_list[yy_i];"
        )?;
        if has_start_conditions {
            writeln!(
                output,
                "                    if (!yy_rule_cond[yy_rule][yy_start_state]) continue;"
            )?;
        }
        if has_bol_anchors {
            writeln!(
                output,
                "                    if (yy_rule_bol[yy_rule] && !yy_token_started_at_bol) continue;"
            )?;
        }
        writeln!(output, "                    yyaccept = yy_rule;")?;
        writeln!(
            output,
            "                    yy_reject_stack[yy_reject_top].rule_idx = yy_i - yy_accept_idx[yy_full_match_state] + 1;"
        )?;
        writeln!(
            output,
            "                    yy_reject_top++; /* Keep this entry for next REJECT */"
        )?;
        writeln!(output, "                    yy_found = 1;")?;
        writeln!(output, "                    break;")?;
        writeln!(output, "                }}")?;
        writeln!(output, "            }}")?;
        writeln!(output, "        }}")?;
        writeln!(output, "        if (!yy_found) {{")?;
        writeln!(
            output,
            "            /* No alternate rule found - do default action (ECHO one char) */"
        )?;
        writeln!(output, "            yy_at_bol = (*YYTOKEN == '\\n');")?;
        writeln!(output, "            putc(*YYTOKEN++, yyout);")?;
        writeln!(output, "            YYCURSOR = YYTOKEN;")?;
        writeln!(output, "            goto yy_scan;")?;
        writeln!(output, "        }}")?;
        writeln!(output, "        yy_reject_flag = 0;")?;
        writeln!(output, "    }}")?;
        writeln!(output)?;
    }

    // Rollback to marker and set yytext/yyleng
    writeln!(output, "    /* Rollback to accepted position */")?;
    writeln!(output, "    YYCURSOR = YYMARKER;")?;
    writeln!(output)?;

    // Set yytext and yyleng with yymore support
    writeln!(output, "    /* Set yytext and yyleng (handle yymore) */")?;
    writeln!(output, "    if (yy_more_flag) {{")?;
    writeln!(
        output,
        "        int yy_new_len = (int)(YYCURSOR - YYTOKEN);"
    )?;
    writeln!(
        output,
        "        int yy_total_len = yy_more_len + yy_new_len;"
    )?;
    // Grow yytext buffer if needed (only for pointer mode)
    if config.yytext_is_pointer {
        writeln!(
            output,
            "        if ((size_t)yy_total_len >= yy_yytext_size) {{"
        )?;
        writeln!(output, "            size_t new_size = yy_yytext_size * 2;")?;
        writeln!(
            output,
            "            while (new_size <= (size_t)yy_total_len) new_size *= 2;"
        )?;
        writeln!(
            output,
            "            char *new_buf = (char *)realloc(yy_yytext_buf, new_size + 1);"
        )?;
        writeln!(output, "            if (new_buf == NULL) {{")?;
        writeln!(
            output,
            "                fprintf(stderr, \"lex: out of memory growing yytext to %zu bytes\\n\", new_size);"
        )?;
        writeln!(output, "                return -1;")?;
        writeln!(output, "            }}")?;
        writeln!(output, "            yy_yytext_buf = new_buf;")?;
        writeln!(output, "            yytext = new_buf;")?;
        writeln!(output, "            yy_yytext_size = new_size;")?;
        writeln!(output, "        }}")?;
    }
    writeln!(
        output,
        "        memcpy(yytext + yy_more_len, YYTOKEN, yy_new_len);"
    )?;
    writeln!(output, "        yyleng = yy_total_len;")?;
    writeln!(output, "        yytext[yyleng] = '\\0';")?;
    writeln!(output, "        yy_more_flag = 0;")?;
    writeln!(output, "    }} else {{")?;
    writeln!(output, "        yyleng = (int)(YYCURSOR - YYTOKEN);")?;
    // Grow yytext buffer if needed (only for pointer mode)
    if config.yytext_is_pointer {
        writeln!(output, "        if ((size_t)yyleng >= yy_yytext_size) {{")?;
        writeln!(output, "            size_t new_size = yy_yytext_size * 2;")?;
        writeln!(
            output,
            "            while (new_size <= (size_t)yyleng) new_size *= 2;"
        )?;
        writeln!(
            output,
            "            char *new_buf = (char *)realloc(yy_yytext_buf, new_size + 1);"
        )?;
        writeln!(output, "            if (new_buf == NULL) {{")?;
        writeln!(
            output,
            "                fprintf(stderr, \"lex: out of memory growing yytext to %zu bytes\\n\", new_size);"
        )?;
        writeln!(output, "                return -1;")?;
        writeln!(output, "            }}")?;
        writeln!(output, "            yy_yytext_buf = new_buf;")?;
        writeln!(output, "            yytext = new_buf;")?;
        writeln!(output, "            yy_yytext_size = new_size;")?;
        writeln!(output, "        }}")?;
    }
    writeln!(output, "        memcpy(yytext, YYTOKEN, yyleng);")?;
    writeln!(output, "        yytext[yyleng] = '\\0';")?;
    writeln!(output, "    }}")?;
    writeln!(output)?;

    // Note: Trailing context, yy_more_len, and BOL update moved to AFTER rule selection
    // to ensure they use the finalized yyaccept value (Bug C fix)

    // Start condition and BOL anchor validation
    if has_start_conditions || has_bol_anchors {
        writeln!(
            output,
            "    /* Validate rule against current start condition and BOL */"
        )?;
        // Note: removed yy_validate_rule: label as it was unused (no gotos to it)
        if has_start_conditions {
            writeln!(
                output,
                "    if (yyaccept >= 0 && !yy_rule_cond[yyaccept][yy_start_state]) {{"
            )?;
            writeln!(
                output,
                "        /* Rule not valid in current start condition */"
            )?;
            writeln!(output, "        goto yy_try_alternate_rule;")?;
            writeln!(output, "    }}")?;
        }
        if has_bol_anchors {
            writeln!(
                output,
                "    if (yyaccept >= 0 && yy_rule_bol[yyaccept] && !yy_token_started_at_bol) {{"
            )?;
            writeln!(
                output,
                "        /* BOL rule but not at beginning of line */"
            )?;
            writeln!(output, "        goto yy_try_alternate_rule;")?;
            writeln!(output, "    }}")?;
        }
        writeln!(output, "    goto yy_execute_action;")?;
        writeln!(output)?;

        // Try alternate rule (find next rule in accepting list)
        writeln!(output, "yy_try_alternate_rule:")?;
        writeln!(output, "    {{")?;
        writeln!(
            output,
            "        /* Find next valid rule at this position */"
        )?;
        writeln!(output, "        int yy_found = 0;")?;
        writeln!(output, "        int yy_i;")?;
        writeln!(
            output,
            "        int yy_start_idx = yy_accept_idx[yy_full_match_state];"
        )?;
        writeln!(
            output,
            "        int yy_end_idx = yy_accept_idx[yy_full_match_state + 1];"
        )?;
        writeln!(output, "        int yy_skip_until_after = yyaccept;")?;
        writeln!(output, "        int yy_skipping = 1;")?;
        writeln!(
            output,
            "        for (yy_i = yy_start_idx; yy_i < yy_end_idx; yy_i++) {{"
        )?;
        writeln!(output, "            int yy_rule = yy_accept_list[yy_i];")?;
        writeln!(
            output,
            "            if (yy_skipping) {{ if (yy_rule == yy_skip_until_after) yy_skipping = 0; continue; }}"
        )?;
        if has_start_conditions {
            writeln!(
                output,
                "            if (!yy_rule_cond[yy_rule][yy_start_state]) continue;"
            )?;
        }
        if has_bol_anchors {
            writeln!(
                output,
                "            if (yy_rule_bol[yy_rule] && !yy_token_started_at_bol) continue;"
            )?;
        }
        writeln!(output, "            yyaccept = yy_rule;")?;
        writeln!(output, "            yy_found = 1;")?;
        writeln!(output, "            break;")?;
        writeln!(output, "        }}")?;
        writeln!(output, "        if (yy_found) {{")?;
        writeln!(output, "            goto yy_execute_action;")?;
        writeln!(output, "        }}")?;
        writeln!(
            output,
            "        /* No valid rule found - do default action (ECHO one char) */"
        )?;
        writeln!(output, "        yy_at_bol = (*YYTOKEN == '\\n');")?;
        writeln!(output, "        putc(*YYTOKEN++, yyout);")?;
        writeln!(output, "        YYCURSOR = YYTOKEN;")?;
        writeln!(output, "        goto yy_scan;")?;
        writeln!(output, "    }}")?;
        writeln!(output)?;

        writeln!(output, "yy_execute_action:")?;
    }

    // Handle trailing context AFTER rule selection is finalized (Bug C fix)
    // This ensures we use the correct yyaccept value even after alternate rule selection
    if has_trailing_context {
        writeln!(output, "    /* Handle trailing context - adjust yyleng */")?;
        writeln!(
            output,
            "    if (yyaccept >= 0 && yyaccept < YY_NUM_RULES) {{"
        )?;
        writeln!(
            output,
            "        int yy_main_len = yy_rule_main_len[yyaccept];"
        )?;
        writeln!(output, "        if (yy_main_len >= 0) {{")?;
        writeln!(output, "            /* Fixed-length main pattern */")?;
        writeln!(output, "            YYCURSOR = YYTOKEN + yy_main_len;")?;
        writeln!(output, "            yyleng = yy_main_len;")?;
        writeln!(output, "            yytext[yyleng] = '\\0';")?;
        if has_var_tc {
            writeln!(output, "        }} else if (yy_main_len == -2) {{")?;
            writeln!(
                output,
                "            /* Variable-length main pattern - use per-rule offset */"
            )?;
            writeln!(
                output,
                "            if (yy_main_end_offset[yyaccept] >= 0) {{"
            )?;
            writeln!(
                output,
                "                YYCURSOR = YYTOKEN + yy_main_end_offset[yyaccept];"
            )?;
            writeln!(
                output,
                "                yyleng = yy_main_end_offset[yyaccept];"
            )?;
            writeln!(output, "                yytext[yyleng] = '\\0';")?;
            writeln!(output, "            }}")?;
        }
        writeln!(output, "        }}")?;
        writeln!(output, "    }}")?;
        writeln!(output)?;
    }

    // Save yy_more_len AFTER trailing context adjustment
    writeln!(output, "    yy_more_len = yyleng;")?;
    writeln!(output)?;

    // Update BOL status for NEXT token (based on whether current match ends with newline)
    writeln!(
        output,
        "    /* Update beginning-of-line status for next token */"
    )?;
    writeln!(output, "    if (yyleng > 0) {{")?;
    writeln!(output, "        yy_at_bol = (yytext[yyleng - 1] == '\\n');")?;
    writeln!(output, "    }}")?;
    writeln!(output)?;

    // Action dispatch via switch
    writeln!(output, "    /* Execute rule action */")?;
    writeln!(output, "    switch (yyaccept) {{")?;

    for (rule_idx, rule) in lexinfo.rules.iter().enumerate() {
        writeln!(output, "    case {}:", rule_idx)?;
        // Generate yy_action_N label for <<EOF>> rules (enables direct jump from EOF handling)
        if eof_rules.iter().any(|r| r.rule_idx == rule_idx) {
            writeln!(output, "    yy_action_{}:", rule_idx)?;
        }
        if rule.action.trim() == "|" {
            writeln!(output, "        /* fall through */")?;
        } else {
            writeln!(output, "        {}", rule.action)?;
            writeln!(output, "        break;")?;
        }
    }

    writeln!(output, "    default:")?;
    writeln!(output, "        ECHO;")?;
    writeln!(output, "        break;")?;
    writeln!(output, "    }}")?;
    writeln!(output)?;

    writeln!(output, "    goto yy_scan;")?;
    writeln!(output, "}}\n")?;

    // Generate yywrap and main if needed
    write_default_yywrap_main(output, lexinfo)?;

    Ok(())
}

fn write_default_yywrap_main<W: Write>(output: &mut W, lexinfo: &LexInfo) -> io::Result<()> {
    // Generate yywrap if not in user subroutines
    let has_yywrap = lexinfo.user_subs.iter().any(|s| s.contains("yywrap"));
    if !has_yywrap {
        writeln!(output, "/* Default yywrap */")?;
        writeln!(output, "int yywrap(void)")?;
        writeln!(output, "{{")?;
        writeln!(output, "    return 1;")?;
        writeln!(output, "}}")?;
        writeln!(output)?;
    }

    // Generate main if not in user subroutines
    let has_main = lexinfo
        .user_subs
        .iter()
        .any(|s| s.contains("int main") || s.contains("void main"));
    if !has_main {
        writeln!(output, "/* Default main */")?;
        writeln!(output, "#ifndef YY_SKIP_YYWRAP")?;
        writeln!(output, "int main(int argc, char *argv[])")?;
        writeln!(output, "{{")?;
        writeln!(output, "    (void)argc; (void)argv;")?;
        writeln!(output, "    return yylex();")?;
        writeln!(output, "}}")?;
        writeln!(output, "#endif\n")?;
    }

    Ok(())
}

fn write_user_subroutines<W: Write>(output: &mut W, lexinfo: &LexInfo) -> io::Result<()> {
    if !lexinfo.user_subs.is_empty() {
        writeln!(output, "/* User subroutines */")?;
        for line in &lexinfo.user_subs {
            write!(output, "{}", line)?;
        }
        writeln!(output)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dfa::Dfa;
    use crate::lexfile::LexInfo;
    use crate::nfa::Nfa;
    use std::collections::HashMap;

    #[test]
    fn test_contains_identifier() {
        // Positive cases - should match
        assert!(contains_identifier("REJECT;", "REJECT"));
        assert!(contains_identifier("if (x) REJECT;", "REJECT"));
        assert!(contains_identifier("REJECT", "REJECT"));
        assert!(contains_identifier("{ REJECT; }", "REJECT"));

        // Negative cases - should NOT match (part of larger identifier)
        assert!(!contains_identifier("REJECTED", "REJECT"));
        assert!(!contains_identifier("NOT_REJECT", "REJECT"));
        assert!(!contains_identifier("REJECT_ALL", "REJECT"));
        assert!(!contains_identifier("myREJECT", "REJECT"));

        // Edge cases
        assert!(contains_identifier("x=REJECT+1", "REJECT")); // operators as boundaries
        assert!(contains_identifier("(REJECT)", "REJECT")); // parens as boundaries
    }

    fn create_test_lexinfo() -> LexInfo {
        LexInfo {
            external_def: vec!["#include <stdio.h>\n".to_string()],
            subs: HashMap::new(),
            internal_defs: vec![],
            cond_start: vec![],
            cond_xstart: vec![],
            yyt_is_ptr: true,
            user_subs: vec![],
            rules: vec![],
            table_sizes: HashMap::new(),
            options: crate::lexfile::LexOptions::default(),
        }
    }

    #[test]
    fn test_generate_header() {
        let mut output = Vec::new();
        write_header(&mut output).unwrap();
        let s = String::from_utf8(output).unwrap();
        assert!(s.contains("Generated by lex-rs"));
        assert!(s.contains("Direct-coded"));
    }

    #[test]
    fn test_generate_simple_lexer() {
        let hir = regex_syntax::parse("a").unwrap();
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();
        let dfa = Dfa::from_nfa(&nfa);

        let mut lexinfo = create_test_lexinfo();
        lexinfo.rules.push(crate::lexfile::LexRule {
            ere: "a".to_string(),
            compiled_ere: "a".to_string(),
            action: "return 1;".to_string(),
            start_conditions: vec![],
            bol_anchor: false,
            trailing_context: None,
            compiled_trailing_context: None,
            is_eof: false,
        });

        let mut output = Vec::new();
        let config = CodeGenConfig::default();
        generate(&mut output, &dfa, &lexinfo, &config).unwrap();

        let s = String::from_utf8(output).unwrap();
        assert!(s.contains("int yylex(void)"));
        assert!(s.contains("yy_state_0:"));
        assert!(s.contains("yy_ec"));
        assert!(s.contains("YY_NUM_STATES"));
    }

    #[test]
    fn test_generate_with_bol_anchor() {
        let hir = regex_syntax::parse("foo").unwrap();
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();
        let dfa = Dfa::from_nfa(&nfa);

        let mut lexinfo = create_test_lexinfo();
        lexinfo.rules.push(crate::lexfile::LexRule {
            ere: "^foo".to_string(),
            compiled_ere: "foo".to_string(),
            action: "return BOL_RULE;".to_string(),
            start_conditions: vec![],
            bol_anchor: true,
            trailing_context: None,
            compiled_trailing_context: None,
            is_eof: false,
        });

        let rule_meta = vec![RuleMetadata {
            bol_anchor: true,
            main_pattern_len: None,
            has_trailing_context: false,
            has_variable_trailing_context: false,
        }];

        let mut output = Vec::new();
        let config = CodeGenConfig {
            rule_metadata: rule_meta,
            ..Default::default()
        };
        generate(&mut output, &dfa, &lexinfo, &config).unwrap();

        let s = String::from_utf8(output).unwrap();
        assert!(s.contains("return BOL_RULE"));
        assert!(s.contains("int yylex(void)"));
    }

    #[test]
    fn test_generate_with_start_conditions() {
        let hir = regex_syntax::parse("foo").unwrap();
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();
        let dfa = Dfa::from_nfa(&nfa);

        let mut lexinfo = create_test_lexinfo();
        lexinfo.cond_start.push("COMMENT".to_string());
        lexinfo.cond_xstart.push("STRING".to_string());
        lexinfo.rules.push(crate::lexfile::LexRule {
            ere: "foo".to_string(),
            compiled_ere: "foo".to_string(),
            action: "return 1;".to_string(),
            start_conditions: vec!["COMMENT".to_string()],
            bol_anchor: false,
            trailing_context: None,
            compiled_trailing_context: None,
            is_eof: false,
        });

        let mut output = Vec::new();
        let config = CodeGenConfig {
            start_conditions: vec![
                "INITIAL".to_string(),
                "COMMENT".to_string(),
                "STRING".to_string(),
            ],
            ..Default::default()
        };
        generate(&mut output, &dfa, &lexinfo, &config).unwrap();

        let s = String::from_utf8(output).unwrap();
        assert!(s.contains("#define INITIAL 0"));
        assert!(s.contains("#define COMMENT 1"));
        assert!(s.contains("#define STRING 2"));
        assert!(s.contains("BEGIN"));
    }

    #[test]
    fn test_generate_with_trailing_context() {
        let hir = regex_syntax::parse("foo").unwrap();
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();
        let dfa = Dfa::from_nfa(&nfa);

        let mut lexinfo = create_test_lexinfo();
        lexinfo.rules.push(crate::lexfile::LexRule {
            ere: "foo/bar".to_string(),
            compiled_ere: "foo".to_string(),
            action: "return TC_RULE;".to_string(),
            start_conditions: vec![],
            bol_anchor: false,
            trailing_context: Some("bar".to_string()),
            compiled_trailing_context: Some("bar".to_string()),
            is_eof: false,
        });

        let rule_meta = vec![RuleMetadata {
            bol_anchor: false,
            main_pattern_len: Some(3),
            has_trailing_context: true,
            has_variable_trailing_context: false,
        }];

        let mut output = Vec::new();
        let config = CodeGenConfig {
            rule_metadata: rule_meta,
            ..Default::default()
        };
        generate(&mut output, &dfa, &lexinfo, &config).unwrap();

        let s = String::from_utf8(output).unwrap();
        assert!(s.contains("TC_RULE"));
        assert!(s.contains("yy_rule_main_len"));
    }
}
