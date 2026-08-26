//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Direct-coded C code generation for the lexical analyzer.
//!
//! Generates POSIX-compliant C code from the DFA using direct-coded states
//! with span-compressed transitions. Each DFA state becomes a C label;
//! consecutive equivalence classes with the same target state are merged
//! into range comparisons, producing compact code.

use crate::dfa::{ClassId, Dfa, DfaState};
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
    /// Standalone DFA for each variable-length trailing context, by rule index.
    ///
    /// Used to test a candidate split: does the text after this main-pattern
    /// end match the trailing context exactly?
    pub tc_dfas: Vec<(usize, Dfa)>,
}

impl Default for CodeGenConfig {
    fn default() -> Self {
        CodeGenConfig {
            yytext_is_pointer: true,
            yytext_size: 8192,
            start_conditions: vec!["INITIAL".to_string()],
            rule_metadata: Vec::new(),
            tc_dfas: Vec::new(),
        }
    }
}

/// EOF rule with its start conditions
struct EofRule {
    rule_idx: usize,
    start_conditions: Vec<String>,
}

/// A span represents a contiguous range of equivalence classes that all
/// transition to the same target state. Merging consecutive classes into
/// spans and generating range comparisons produces more compact code than
/// individual case statements.
struct Span {
    /// Lower bound of equivalence class range (inclusive)
    lower: usize,
    /// Upper bound of equivalence class range (inclusive)
    upper: usize,
    /// Target state index
    target: usize,
}

/// Pre-computed feature flags to avoid redundant iteration over rules/metadata
struct FeatureFlags {
    has_reject: bool,
    has_start_conditions: bool,
    has_trailing_context: bool,
    has_var_tc: bool,
    /// All EOF rules with their start conditions
    eof_rules: Vec<EofRule>,
    needs_accept_lists: bool,
}

impl FeatureFlags {
    /// Compute all feature flags in a single pass over rules
    fn compute(lexinfo: &LexInfo, config: &CodeGenConfig) -> Self {
        let has_start_conditions = config.start_conditions.len() > 1;

        // Single pass over lexinfo.rules (the authoritative source)
        let mut has_trailing_context = false;
        let mut has_var_tc = false;
        let mut has_reject = false;
        let mut eof_rules = Vec::new();

        for (idx, rule) in lexinfo.rules.iter().enumerate() {
            // BOL and trailing context come directly from parsed rules
            has_trailing_context |= rule.trailing_context.is_some();

            // Variable TC requires computed info only available in metadata
            if idx < config.rule_metadata.len() {
                has_var_tc |= config.rule_metadata[idx].has_variable_trailing_context;
            }

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

        // Anchoring and start conditions are both resolved by the automaton's
        // shape now, so only REJECT still needs per-state accept lists.
        let needs_accept_lists = has_reject;

        FeatureFlags {
            has_reject,
            has_start_conditions,
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
    // No progress chatter here: POSIX makes the statistics summary conditional
    // on -v (or declared table sizes) and suppressible with -n, and run() owns
    // that decision. The state and equivalence-class counts this used to print
    // unconditionally are reported by write_stats().

    // Pre-compute all feature flags in a single pass
    let flags = FeatureFlags::compute(lexinfo, config);

    write_header(output)?;
    write_includes(output)?;
    write_external_definitions(output, lexinfo)?;
    write_macros_and_types(output, config, &flags)?;
    write_char_class_table(output, dfa)?;
    write_num_states(output, dfa)?;
    // Only generate accept lists if needed for REJECT or alternate rule finding
    if flags.needs_accept_lists {
        write_accepting_list_table(output, dfa)?;
    }
    write_trailing_context_matchers(output, dfa, config)?;
    write_rule_metadata_tables(output, lexinfo, config, &flags)?;
    write_main_pattern_end_table(output, dfa, config)?;
    write_buffer_management(output)?;
    write_helper_functions(output, lexinfo)?;
    write_yylex_direct_coded(output, dfa, lexinfo, config, &flags)?;
    write_user_subroutines(output, lexinfo)?;

    Ok(())
}

fn write_header<W: Write>(output: &mut W) -> io::Result<()> {
    writeln!(output, "/* Generated by lex-rs - POSIX compatible lex */")?;
    writeln!(output, "/* Direct-coded scanner */")?;
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
#include <limits.h>
#include <stddef.h>

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
    config: &CodeGenConfig,
    flags: &FeatureFlags,
) -> io::Result<()> {
    writeln!(
        output,
        r#"/* Lex macros and types */
#ifndef YY_BUF_SIZE
#define YY_BUF_SIZE 16384
#endif

#ifndef YY_EXIT_FAILURE
#define YY_EXIT_FAILURE 2
#endif

/* Fatal error handler - user can override to customize error handling */
#ifndef YY_FATAL_ERROR
#define YY_FATAL_ERROR(msg) do {{ fprintf(stderr, "%s\n", (msg)); exit(YY_EXIT_FAILURE); }} while (0)
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
/* REJECT history: one entry per accepting position reached while scanning the
   current token, in increasing order of position.
   Positions are offsets from YYTOKEN rather than pointers, so a refill that
   compacts or reallocates the input buffer needs no fixups here. The stack
   grows on demand; a token longer than any fixed bound is ordinary. */
typedef struct {{
    ptrdiff_t offset;  /* Distance from YYTOKEN to this accept */
    int state;         /* DFA state that accepted */
}} yy_reject_entry;
static yy_reject_entry *yy_reject_stack = NULL;
static size_t yy_reject_size = 0;
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
        writeln!(
            output,
            r#"/* Variable-length trailing context: every position at which some rule's
   main pattern could have ended, recorded in increasing order while scanning.
   Only the split whose remainder matches the trailing context is correct, so
   all candidates are kept and tested afterwards; keeping just the furthest one
   silently mis-tokenized. Offsets are from YYTOKEN, so a buffer shift needs no
   fixups here. */
typedef struct {{
    ptrdiff_t offset;  /* Distance from YYTOKEN to this main-pattern end */
    int rule;          /* Rule whose main pattern could end here */
}} yy_main_end_entry;
static yy_main_end_entry *yy_main_end_stack = NULL;
static size_t yy_main_end_size = 0;
static int yy_main_end_top = 0;
"#
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

    for b in 0u8..=255 {
        if b % 16 == 0 {
            write!(output, "\n    ")?;
        }
        write!(output, "{:3}", dfa.char_classes.class_of_byte(b).0)?;
        if b < 255 {
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
    // Use i32 to avoid truncation with many rules (i16 max is 32767)
    let mut accept_list: Vec<i32> = Vec::new();
    let mut accept_idx: Vec<usize> = Vec::new();

    for state in &dfa.states {
        accept_idx.push(accept_list.len());
        // Sort accepting rules by rule index to ensure deterministic priority order
        let mut sorted_rules = state.accepting_rules.clone();
        sorted_rules.sort();
        for &rule in &sorted_rules {
            accept_list.push(rule as i32);
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
        "static const unsigned int yy_accept_idx[YY_NUM_STATES + 1] = {{ "
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
        writeln!(output, "static const int yy_accept_list[1] = {{ -1 }};\n")?;
    } else {
        write!(
            output,
            "static const int yy_accept_list[{}] = {{ ",
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
    if flags.has_trailing_context {
        writeln!(output, "#ifndef YY_NUM_RULES")?;
        writeln!(output, "#define YY_NUM_RULES {}", num_rules)?;
        writeln!(output, "#endif\n")?;
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
                        // A length that will not survive the narrowing is
                        // reported as variable rather than silently wrapping
                        // into a bogus (possibly negative) offset.
                        Some(len) => i32::try_from(len).unwrap_or(-2),
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

/// Emit the input buffer's maintenance routines.
///
/// Every position the scanner remembers -- YYCURSOR, YYLIMIT, YYTOKEN,
/// YYMARKER, and the REJECT and trailing-context stacks -- names a byte of the
/// current token. Only these three routines move that byte, and each keeps
/// every position on the byte it named, so nothing else has to think about it.
/// The rebasing used to be written out at each growth and compaction site,
/// which is how a stale pointer got left behind.
fn write_buffer_management<W: Write>(output: &mut W) -> io::Result<()> {
    writeln!(
        output,
        r#"/* Input buffer maintenance. These are the only routines that move the
   buffer; each leaves every saved position on the byte it named. */

/* Double the buffer, keeping all four cursors on their bytes. */
static void yy_buffer_grow(void)
{{
    size_t yy_cursor_off = (size_t)(YYCURSOR - yy_buffer);
    size_t yy_limit_off = (size_t)(YYLIMIT - yy_buffer);
    size_t yy_token_off = (size_t)(YYTOKEN - yy_buffer);
    size_t yy_marker_off = (size_t)(YYMARKER - yy_buffer);
    size_t yy_new_size = yy_buffer_size ? yy_buffer_size * 2 : YY_BUF_SIZE;
    unsigned char *yy_new = (unsigned char *)realloc(yy_buffer, yy_new_size + 2);
    if (yy_new == NULL) {{
        YY_FATAL_ERROR("lex: out of memory growing input buffer");
    }}
    yy_buffer = yy_new;
    yy_buffer_size = yy_new_size;
    YYCURSOR = yy_buffer + yy_cursor_off;
    YYLIMIT = yy_buffer + yy_limit_off;
    YYTOKEN = yy_buffer + yy_token_off;
    YYMARKER = yy_buffer + yy_marker_off;
}}

/* Drop the text before the current token, moving it to the front.
   Distances from YYTOKEN are unchanged, which is why the REJECT and
   trailing-context stacks can store offsets from it and need no fixups. */
static void yy_buffer_compact(void)
{{
    size_t yy_shift = (size_t)(YYTOKEN - yy_buffer);
    size_t yy_live;
    if (yy_shift == 0) {{
        return;
    }}
    yy_live = (size_t)(YYLIMIT - YYTOKEN);
    if (yy_live > 0) {{
        memmove(yy_buffer, YYTOKEN, yy_live);
    }}
    YYCURSOR -= yy_shift;
    YYLIMIT -= yy_shift;
    YYMARKER -= yy_shift;
    YYTOKEN = yy_buffer;
}}

/* Read more input at YYLIMIT, growing first if the buffer is full.
   Returns the number of bytes read; 0 means end of input. */
static int yy_buffer_fill(void)
{{
    int yy_result;
    size_t yy_used;
    size_t yy_room;
    yy_used = (size_t)(YYLIMIT - yy_buffer);
    if (yy_used >= yy_buffer_size) {{
        yy_buffer_grow();
        yy_used = (size_t)(YYLIMIT - yy_buffer);
    }}
    yy_room = yy_buffer_size - yy_used;
    if (yy_room > (size_t)INT_MAX) {{
        yy_room = (size_t)INT_MAX;
    }}
    YY_INPUT(yy_buffer + yy_used, yy_result, (int)yy_room);
    if (yy_result > 0) {{
        YYLIMIT = yy_buffer + yy_used + (size_t)yy_result;
    }}
    return yy_result;
}}
"#
    )?;
    Ok(())
}

fn write_helper_functions<W: Write>(output: &mut W, lexinfo: &LexInfo) -> io::Result<()> {
    // input() function - conditionally generated based on %option noinput
    if !lexinfo.options.noinput {
        writeln!(
            output,
            r#"/* input - read one character from input */
/* POSIX 102013-102017 makes input() accessible to user code, so it is not
   static; that also keeps a scanner which happens not to call it building
   cleanly under -Wall -Werror (-Wunused-function fires only on statics). */
int input(void)
{{
    /* Check main buffer - unput() now always inserts directly here */
    if (YYCURSOR >= YYLIMIT) {{
        /* Refill through the same routine the scan loop uses, so a
           user-supplied YY_INPUT sees these reads too. */
        if (yy_buffer_fill() == 0) {{
            return 0;  /* POSIX: input() returns 0 on end of file */
        }}
    }}
    {{
        int yy_c = *YYCURSOR++;
        /* A newline consumed here still starts a new line for the next token. */
        yy_at_bol = (yy_c == '\n');
        return yy_c;
    }}
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
 * POSIX prototype is `int unput(int c)`; returns the pushed-back character.
 * POSIX 102018-102021 makes unput() accessible to user code, so it is not
 * static; that also keeps a scanner which happens not to call it building
 * cleanly under -Wall -Werror (-Wunused-function fires only on statics).
 */
int unput(int c)
{{
    if (YYCURSOR > yy_buffer) {{
        /* Room before cursor - just back up and insert */
        *--YYCURSOR = (unsigned char)c;
    }} else {{
        /* At start of buffer - need to shift content right to make room.
           Compaction cannot help here: the room has to be *before* YYTOKEN. */
        size_t yy_remain = YYLIMIT - yy_buffer;
        if (yy_remain >= yy_buffer_size) {{
            yy_buffer_grow();
            yy_remain = YYLIMIT - yy_buffer;
        }}
        if (yy_remain > 0) {{
            memmove(yy_buffer + 1, yy_buffer, yy_remain);
        }}
        yy_buffer[0] = (unsigned char)c;
        /* Everything already buffered moved up one byte, so every pointer
           into it moves with its data. YYCURSOR is the exception: it stays at
           yy_buffer, which is now the character just pushed back. */
        YYLIMIT++;
        YYTOKEN++;
        YYMARKER++;
    }}
    return c;
}}
"#
        )?;
    }

    Ok(())
}

/// Emit a standalone matcher for each variable-length trailing context.
///
/// `yy_tc_match_<rule>(from, to)` answers: does the text in [from, to) match
/// this rule's trailing context exactly? That is the test which decides where
/// a variable-length main pattern really ended.
///
/// Transitions are indexed by the scanner's equivalence classes. Those classes
/// come from the combined automaton, which contains these very trailing-context
/// transitions, so two characters sharing a class are interchangeable here too.
fn write_trailing_context_matchers<W: Write>(
    output: &mut W,
    dfa: &Dfa,
    config: &CodeGenConfig,
) -> io::Result<()> {
    if config.tc_dfas.is_empty() {
        return Ok(());
    }

    let num_classes = dfa.char_classes.num_classes;

    writeln!(
        output,
        "/* Trailing-context matchers for variable-length main patterns */"
    )?;
    for (rule, tc) in &config.tc_dfas {
        writeln!(
            output,
            "static int yy_tc_match_{}(const unsigned char *from, const unsigned char *to)",
            rule
        )?;
        writeln!(output, "{{")?;

        // -1 marks "no transition": the candidate split is impossible.
        writeln!(
            output,
            "    static const short yy_tc_trans[{}][{}] = {{",
            tc.states.len(),
            num_classes
        )?;
        for state in &tc.states {
            write!(output, "        {{ ")?;
            for class in 0..num_classes {
                let target = state
                    .transitions
                    .get(&ClassId(class))
                    .map(|&t| t as i32)
                    .unwrap_or(-1);
                write!(output, "{}", target)?;
                if class + 1 < num_classes {
                    write!(output, ", ")?;
                }
            }
            writeln!(output, " }},")?;
        }
        writeln!(output, "    }};")?;

        write!(
            output,
            "    static const char yy_tc_accept[{}] = {{ ",
            tc.states.len()
        )?;
        for (i, state) in tc.states.iter().enumerate() {
            write!(output, "{}", if state.accepting.is_some() { 1 } else { 0 })?;
            if i + 1 < tc.states.len() {
                write!(output, ", ")?;
            }
        }
        writeln!(output, " }};")?;

        writeln!(output, "    const unsigned char *p;")?;
        writeln!(output, "    int st = {};", tc.starts[0].plain)?;
        writeln!(output, "    for (p = from; p < to; ++p) {{")?;
        writeln!(output, "        st = yy_tc_trans[st][yy_ec[*p]];")?;
        writeln!(output, "        if (st < 0) return 0;")?;
        writeln!(output, "    }}")?;
        writeln!(output, "    return yy_tc_accept[st];")?;
        writeln!(output, "}}\n")?;
    }

    // Dispatch by rule so the match path can stay rule-agnostic.
    writeln!(
        output,
        "static int yy_tc_match(int rule, const unsigned char *from, const unsigned char *to)"
    )?;
    writeln!(output, "{{")?;
    writeln!(output, "    switch (rule) {{")?;
    for (rule, _) in &config.tc_dfas {
        writeln!(
            output,
            "        case {}: return yy_tc_match_{}(from, to);",
            rule, rule
        )?;
    }
    writeln!(output, "        default: return 0;")?;
    writeln!(output, "    }}")?;
    writeln!(output, "}}\n")?;

    Ok(())
}

/// Emit the REJECT history push for an accepting state, indented by `indent`.
///
/// Recording every accepting position on the way forward is what lets REJECT
/// fall back to a shorter match afterwards.
fn write_reject_push<W: Write>(output: &mut W, indent: &str, state_idx: usize) -> io::Result<()> {
    writeln!(
        output,
        "{}if ((size_t)yy_reject_top >= yy_reject_size) {{",
        indent
    )?;
    writeln!(
        output,
        "{}    size_t yy_new_size = yy_reject_size ? yy_reject_size * 2 : 64;",
        indent
    )?;
    writeln!(
        output,
        "{}    yy_reject_entry *yy_new = (yy_reject_entry *)realloc(yy_reject_stack, yy_new_size * sizeof(*yy_reject_stack));",
        indent
    )?;
    writeln!(
        output,
        "{}    if (yy_new == NULL) {{ YY_FATAL_ERROR(\"lex: out of memory growing REJECT stack\"); }}",
        indent
    )?;
    writeln!(output, "{}    yy_reject_stack = yy_new;", indent)?;
    writeln!(output, "{}    yy_reject_size = yy_new_size;", indent)?;
    writeln!(output, "{}}}", indent)?;
    writeln!(
        output,
        "{}yy_reject_stack[yy_reject_top].offset = YYCURSOR - YYTOKEN;",
        indent
    )?;
    writeln!(
        output,
        "{}yy_reject_stack[yy_reject_top].state = {};",
        indent, state_idx
    )?;
    writeln!(output, "{}yy_reject_top++;", indent)?;
    Ok(())
}

/// Build a map of equivalence class -> target state for transitions from a DFA state
fn build_class_transitions(state: &DfaState) -> BTreeMap<usize, usize> {
    // Transitions are already keyed by equivalence class, so this is only a
    // change of key type. It used to fold characters into classes here and
    // assert the fold was consistent; determinization now works in classes, so
    // there is nothing left to collapse.
    state
        .transitions
        .iter()
        .map(|(class, &target)| (class.0, target))
        .collect()
}

/// Build spans from class transitions by merging consecutive classes with the same target.
///
/// This is the core of span compression: instead of generating one `case` statement per
/// equivalence class, we merge consecutive classes that go to the same target state into
/// a single span, then generate range comparisons.
///
/// Example: classes 0,1,2 -> state 3 and classes 3,4 -> state 5
/// becomes: [Span{0,2,3}, Span{3,4,5}]
/// generates: if (yych <= 2) goto state_3; if (yych <= 4) goto state_5;
fn build_spans(class_transitions: &BTreeMap<usize, usize>) -> Vec<Span> {
    if class_transitions.is_empty() {
        return Vec::new();
    }

    let mut spans = Vec::new();
    let mut iter = class_transitions.iter();

    // Start first span
    let (&first_class, &first_target) = iter.next().unwrap();
    let mut current_lower = first_class;
    let mut current_upper = first_class;
    let mut current_target = first_target;

    for (&class, &target) in iter {
        if target == current_target && class == current_upper + 1 {
            // Extend current span
            current_upper = class;
        } else {
            // Save current span and start new one
            spans.push(Span {
                lower: current_lower,
                upper: current_upper,
                target: current_target,
            });
            current_lower = class;
            current_upper = class;
            current_target = target;
        }
    }

    // Don't forget the last span
    spans.push(Span {
        lower: current_lower,
        upper: current_upper,
        target: current_target,
    });

    spans
}

/// Write transitions using span-based if-chain.
///
/// Generates more compact code than individual case statements when there are
/// consecutive equivalence classes going to the same target state. Uses a local
/// variable `yych` to hold the equivalence class, then range comparisons to
/// dispatch to target states.
fn write_transitions_as_spans<W: Write>(output: &mut W, spans: &[Span]) -> io::Result<()> {
    if spans.is_empty() {
        // No transitions at all - go directly to fail
        writeln!(output, "    {{")?;
        writeln!(output, "        (void)*YYCURSOR++;")?;
        writeln!(output, "        goto yy_fail;")?;
        writeln!(output, "    }}")?;
        return Ok(());
    }

    writeln!(output, "    {{")?;
    writeln!(output, "        unsigned char yych = yy_ec[*YYCURSOR++];")?;

    // Use if-else chain: once a span matches, skip remaining checks
    let mut first_span = true;
    for span in spans {
        let prefix = if first_span { "if" } else { "else if" };
        first_span = false;

        if span.lower == span.upper {
            // Single class - use equality
            writeln!(
                output,
                "        {} (yych == {}) goto yy_state_{};",
                prefix, span.lower, span.target
            )?;
        } else if span.lower == 0 {
            // Range starting at 0 - just check upper bound
            writeln!(
                output,
                "        {} (yych <= {}) goto yy_state_{};",
                prefix, span.upper, span.target
            )?;
        } else {
            // General range - check both bounds
            writeln!(
                output,
                "        {} (yych >= {} && yych <= {}) goto yy_state_{};",
                prefix, span.lower, span.upper, span.target
            )?;
        }
    }

    writeln!(output, "        else goto yy_fail;")?;
    writeln!(output, "    }}")?;

    Ok(())
}

/// Write a single DFA state as a labeled block with span-based transitions
fn write_dfa_state<W: Write>(
    output: &mut W,
    state_idx: usize,
    state: &DfaState,
    config: &CodeGenConfig,
    flags: &FeatureFlags,
) -> io::Result<()> {
    writeln!(output, "yy_state_{}:", state_idx)?;

    // If this is an accepting state, save marker position and state info
    // When start conditions are in use, only update YYMARKER if the rule is valid
    // for the current start condition. This prevents incorrect backtracking.
    if let Some(accepting_rule) = state.accepting {
        // Every rule reachable here is active in this state's start condition
        // and allowed at this line position, so the longest match is recorded
        // unconditionally: there is no predicate left to check.
        writeln!(
            output,
            "    /* Accepting state for rule {} */",
            accepting_rule
        )?;
        writeln!(output, "    YYMARKER = YYCURSOR;")?;
        writeln!(output, "    yyaccept = {};", accepting_rule)?;
        writeln!(output, "    yy_full_match_state = {};", state_idx)?;
        // Push to REJECT history stack for shorter match fallback
        if flags.has_reject {
            write_reject_push(output, "    ", state_idx)?;
        }
    }

    // Track main pattern end for variable-length trailing context
    // Track ALL rules that have their main pattern end at this state, using offset from YYTOKEN
    if flags.has_var_tc && !state.main_pattern_end_rules.is_empty() {
        for &rule in &state.main_pattern_end_rules {
            if rule < config.rule_metadata.len()
                && config.rule_metadata[rule].has_variable_trailing_context
            {
                writeln!(
                    output,
                    "    /* Main pattern could end here for rule {} */",
                    rule
                )?;
                writeln!(
                    output,
                    "    if ((size_t)yy_main_end_top >= yy_main_end_size) {{"
                )?;
                writeln!(
                    output,
                    "        size_t yy_new_size = yy_main_end_size ? yy_main_end_size * 2 : 64;"
                )?;
                writeln!(
                    output,
                    "        yy_main_end_entry *yy_new = (yy_main_end_entry *)realloc(yy_main_end_stack, yy_new_size * sizeof(*yy_main_end_stack));"
                )?;
                writeln!(
                    output,
                    "        if (yy_new == NULL) {{ YY_FATAL_ERROR(\"lex: out of memory growing trailing-context stack\"); }}"
                )?;
                writeln!(output, "        yy_main_end_stack = yy_new;")?;
                writeln!(output, "        yy_main_end_size = yy_new_size;")?;
                writeln!(output, "    }}")?;
                writeln!(
                    output,
                    "    yy_main_end_stack[yy_main_end_top].offset = YYCURSOR - YYTOKEN;"
                )?;
                writeln!(
                    output,
                    "    yy_main_end_stack[yy_main_end_top].rule = {};",
                    rule
                )?;
                writeln!(output, "    yy_main_end_top++;")?;
            }
        }
    }

    // Check for end of input - save state for resume after refill
    // Resumption after a refill re-enters here, past the accept record and the
    // trailing-context candidate above. Re-running those would push a second,
    // identical entry for this position, and the REJECT walk-back -- which
    // discards the top entry as the one just exhausted -- would then hand the
    // same match back a second time.
    writeln!(output, "yy_resume_{}:", state_idx)?;
    writeln!(
        output,
        "    if (YYCURSOR >= YYLIMIT) {{ yy_resume_state = {}; goto yy_fill_or_eof; }}",
        state_idx
    )?;

    // Get transitions grouped by equivalence class and compress into spans
    let transitions = build_class_transitions(state);
    let spans = build_spans(&transitions);

    // Generate span-based if-chain (more compact than switch with individual cases)
    write_transitions_as_spans(output, &spans)?;
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

    // The token starts here, so compaction below can drop everything before it.
    writeln!(output, "    /* Initialize for new token */")?;
    writeln!(output, "    YYTOKEN = YYCURSOR;")?;
    writeln!(output, "    /* Check if buffer needs refill */")?;
    writeln!(output, "    if (YYCURSOR >= YYLIMIT) {{")?;
    writeln!(output, "        yy_buffer_compact();")?;
    writeln!(output, "        if (yy_buffer_fill() == 0) {{")?;

    // Handle EOF with start condition awareness
    write_eof_dispatch(output, eof_rules, config, "            ")?;
    writeln!(output, "        }}")?;
    writeln!(output, "    }}")?;
    writeln!(output)?;
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
        writeln!(
            output,
            "    yy_main_end_top = 0; /* Reset trailing-context candidates */"
        )?;
    }
    writeln!(output)?;

    // Entry state. Both the start condition and '^' are settled by which
    // automaton we enter: a rule inactive in this condition, or anchored when
    // we are not at a line start, is simply unreachable from here. Nothing is
    // left to test after a match.
    let entries: Vec<usize> = dfa.starts.iter().flat_map(|s| [s.plain, s.bol]).collect();
    let single_entry = entries.iter().all(|&e| e == entries[0]);
    if single_entry {
        // Every condition and both line positions share one root.
        writeln!(output, "    goto yy_state_{};", entries[0])?;
    } else if dfa.starts.len() == 1 {
        // Only INITIAL, so the sole choice is the line position.
        writeln!(output, "    /* '^' rules live only in the BOL automaton */")?;
        writeln!(
            output,
            "    if (yy_at_bol) goto yy_state_{};",
            dfa.starts[0].bol
        )?;
        writeln!(output, "    goto yy_state_{};", dfa.starts[0].plain)?;
    } else {
        writeln!(
            output,
            "    /* Entry automaton for (start condition, BOL) */"
        )?;
        writeln!(
            output,
            "    switch (yy_start_state * 2 + (yy_at_bol ? 1 : 0)) {{"
        )?;
        for (cond_idx, start) in dfa.starts.iter().enumerate() {
            let name = config
                .start_conditions
                .get(cond_idx)
                .map(String::as_str)
                .unwrap_or("?");
            writeln!(
                output,
                "        case {}: /* {} */ goto yy_state_{};",
                cond_idx * 2,
                name,
                start.plain
            )?;
            writeln!(
                output,
                "        case {}: /* {} at BOL */ goto yy_state_{};",
                cond_idx * 2 + 1,
                name,
                start.bol
            )?;
        }
        writeln!(
            output,
            "        default: goto yy_state_{};",
            dfa.starts[0].plain
        )?;
        writeln!(output, "    }}")?;
    }
    writeln!(output)?;

    // Generate all DFA states
    for (state_idx, state) in dfa.states.iter().enumerate() {
        write_dfa_state(output, state_idx, state, config, flags)?;
    }

    // YYFILL/EOF block
    writeln!(output, "yy_fill_or_eof:")?;
    writeln!(output, "    /* End of buffer reached during scan */")?;
    writeln!(output, "    {{")?;
    writeln!(
        output,
        "        /* Compact then refill. Both keep every saved position on its"
    )?;
    writeln!(
        output,
        "           byte, so the partial match in progress survives untouched. */"
    )?;
    writeln!(output, "        yy_buffer_compact();")?;
    writeln!(output, "        if (yy_buffer_fill() == 0) {{")?;
    writeln!(output, "            if (YYLIMIT == yy_buffer) {{")?;

    // Nothing buffered at all: this really is end of input.
    write_eof_dispatch(output, eof_rules, config, "                ")?;
    writeln!(output, "            }}")?;
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
        "        /* Resume scanning from the DFA state that hit buffer end */"
    )?;
    writeln!(output, "        switch (yy_resume_state) {{")?;
    for state_idx in 0..dfa.states.len() {
        writeln!(
            output,
            "            case {}: goto yy_resume_{};",
            state_idx, state_idx
        )?;
    }
    writeln!(output, "            default: goto yy_resume_0;")?;
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
        writeln!(output, "                yyaccept = yy_rule;")?;
        writeln!(output, "                yy_found = 1;")?;
        writeln!(output, "                break;")?;
        writeln!(output, "            }}")?;
        writeln!(output, "        }}")?;
        writeln!(
            output,
            "        /* If not found, walk back to shorter matches. The top entry is"
        )?;
        writeln!(
            output,
            "           the position just exhausted above, so drop it first; a token"
        )?;
        writeln!(
            output,
            "           that rejected every rule at its longest match must not be"
        )?;
        writeln!(output, "           offered that same match again. */")?;
        writeln!(output, "        while (!yy_found && yy_reject_top > 1) {{")?;
        writeln!(output, "            yy_reject_top--;")?;
        writeln!(output, "            {{")?;
        writeln!(
            output,
            "                yy_reject_entry *yy_e = &yy_reject_stack[yy_reject_top - 1];"
        )?;
        writeln!(output, "                int yy_i;")?;
        writeln!(
            output,
            "                int yy_start_idx = yy_accept_idx[yy_e->state];"
        )?;
        writeln!(
            output,
            "                int yy_end_idx = yy_accept_idx[yy_e->state + 1];"
        )?;
        writeln!(
            output,
            "                for (yy_i = yy_start_idx; yy_i < yy_end_idx; yy_i++) {{"
        )?;
        writeln!(
            output,
            "                    int yy_rule = yy_accept_list[yy_i];"
        )?;
        writeln!(
            output,
            "                    YYMARKER = YYTOKEN + yy_e->offset;"
        )?;
        writeln!(
            output,
            "                    yy_full_match_state = yy_e->state;"
        )?;
        writeln!(output, "                    yyaccept = yy_rule;")?;
        writeln!(output, "                    yy_found = 1;")?;
        writeln!(
            output,
            "                    /* Entry stays on top: a further REJECT resumes past"
        )?;
        writeln!(
            output,
            "                       this rule via the scan above. */"
        )?;
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
        "        size_t yy_new_len = (size_t)(YYCURSOR - YYTOKEN);"
    )?;
    writeln!(
        output,
        "        size_t yy_total_len = (size_t)yy_more_len + yy_new_len;"
    )?;
    // Check for overflow before assigning to int yyleng
    writeln!(
        output,
        "        if (yy_total_len > (size_t)INT_MAX) {{ YY_FATAL_ERROR(\"lex: token too long\"); }}"
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
    } else {
        // %array mode: yytext is a fixed char[YYLMAX] that cannot grow, while
        // the input buffer can, so the copy below needs an explicit bound.
        writeln!(
            output,
            "        if (yy_total_len >= (size_t)YYLMAX) {{ YY_FATAL_ERROR(\"lex: token too large, exceeds YYLMAX\"); }}"
        )?;
    }
    writeln!(
        output,
        "        memcpy(yytext + yy_more_len, YYTOKEN, yy_new_len);"
    )?;
    writeln!(output, "        yyleng = (int)yy_total_len;")?;
    writeln!(output, "        yytext[yyleng] = '\\0';")?;
    writeln!(output, "        yy_more_flag = 0;")?;
    writeln!(output, "    }} else {{")?;
    // Check for token length overflow before casting to int
    writeln!(output, "        ptrdiff_t yy_len = YYCURSOR - YYTOKEN;")?;
    writeln!(
        output,
        "        if (yy_len > INT_MAX) {{ YY_FATAL_ERROR(\"lex: token too long\"); }}"
    )?;
    writeln!(output, "        yyleng = (int)yy_len;")?;
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
    } else {
        // %array mode: see above -- yytext cannot grow to meet the token.
        writeln!(
            output,
            "        if ((size_t)yyleng >= (size_t)YYLMAX) {{ YY_FATAL_ERROR(\"lex: token too large, exceeds YYLMAX\"); }}"
        )?;
    }
    writeln!(output, "        memcpy(yytext, YYTOKEN, yyleng);")?;
    writeln!(output, "        yytext[yyleng] = '\\0';")?;
    writeln!(output, "    }}")?;
    writeln!(output)?;

    // Note: Trailing context, yy_more_len, and BOL update moved to AFTER rule selection
    // to ensure they use the finalized yyaccept value (Bug C fix)

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
                "            /* Variable-length main pattern: of the positions where it"
            )?;
            writeln!(
                output,
                "               could have ended, take the longest whose remainder matches"
            )?;
            writeln!(output, "               the trailing context exactly. */")?;
            writeln!(output, "            ptrdiff_t yy_head = -1;")?;
            writeln!(output, "            int yy_k;")?;
            writeln!(
                output,
                "            for (yy_k = yy_main_end_top - 1; yy_k >= 0; yy_k--) {{"
            )?;
            writeln!(
                output,
                "                if (yy_main_end_stack[yy_k].rule != yyaccept) continue;"
            )?;
            writeln!(
                output,
                "                if (yy_tc_match(yyaccept, YYTOKEN + yy_main_end_stack[yy_k].offset, YYMARKER)) {{"
            )?;
            writeln!(
                output,
                "                    yy_head = yy_main_end_stack[yy_k].offset;"
            )?;
            writeln!(output, "                    break;")?;
            writeln!(output, "                }}")?;
            writeln!(output, "            }}")?;
            writeln!(output, "            if (yy_head > 0) {{")?;
            writeln!(output, "                YYCURSOR = YYTOKEN + yy_head;")?;
            writeln!(output, "                yyleng = (int)yy_head;")?;
            writeln!(output, "                yytext[yyleng] = '\\0';")?;
            writeln!(output, "            }} else if (yy_head == 0) {{")?;
            writeln!(
                output,
                "                /* The only consistent split leaves an empty token, so this"
            )?;
            writeln!(
                output,
                "                   rule consumes nothing here. Taking it would rescan the same"
            )?;
            writeln!(
                output,
                "                   position forever, so advance one character instead. */"
            )?;
            writeln!(output, "                yy_at_bol = (*YYTOKEN == '\\n');")?;
            writeln!(output, "                putc(*YYTOKEN++, yyout);")?;
            writeln!(output, "                YYCURSOR = YYTOKEN;")?;
            writeln!(output, "                goto yy_scan;")?;
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

    // Generate yylex_destroy, yywrap and main if needed
    write_default_yywrap_main(output, lexinfo, config, flags)?;

    Ok(())
}

fn write_default_yywrap_main<W: Write>(
    output: &mut W,
    lexinfo: &LexInfo,
    config: &CodeGenConfig,
    flags: &FeatureFlags,
) -> io::Result<()> {
    // Generate yylex_destroy to free allocated buffers (enables valgrind-clean runs)
    writeln!(output, "/* Cleanup function - free allocated buffers */")?;
    writeln!(output, "void yylex_destroy(void)")?;
    writeln!(output, "{{")?;
    writeln!(output, "    if (yy_buffer != NULL) {{")?;
    writeln!(output, "        free(yy_buffer);")?;
    writeln!(output, "        yy_buffer = NULL;")?;
    writeln!(output, "    }}")?;
    if config.yytext_is_pointer {
        writeln!(output, "    if (yy_yytext_buf != NULL) {{")?;
        writeln!(output, "        free(yy_yytext_buf);")?;
        writeln!(output, "        yy_yytext_buf = NULL;")?;
        writeln!(output, "    }}")?;
        writeln!(output, "    yytext = NULL;")?;
    }
    if flags.has_var_tc {
        writeln!(output, "    if (yy_main_end_stack != NULL) {{")?;
        writeln!(output, "        free(yy_main_end_stack);")?;
        writeln!(output, "        yy_main_end_stack = NULL;")?;
        writeln!(output, "    }}")?;
        writeln!(output, "    yy_main_end_size = 0;")?;
        writeln!(output, "    yy_main_end_top = 0;")?;
    }
    if flags.has_reject {
        writeln!(output, "    if (yy_reject_stack != NULL) {{")?;
        writeln!(output, "        free(yy_reject_stack);")?;
        writeln!(output, "        yy_reject_stack = NULL;")?;
        writeln!(output, "    }}")?;
        writeln!(output, "    yy_reject_size = 0;")?;
        writeln!(output, "    yy_reject_top = 0;")?;
    }
    writeln!(output, "    YYCURSOR = NULL;")?;
    writeln!(output, "    YYLIMIT = NULL;")?;
    writeln!(output, "    YYTOKEN = NULL;")?;
    writeln!(output, "    YYMARKER = NULL;")?;
    writeln!(output, "    yy_buffer_size = 0;")?;
    if config.yytext_is_pointer {
        writeln!(output, "    yy_yytext_size = 0;")?;
    }
    writeln!(output, "}}")?;
    writeln!(output)?;

    // POSIX 102022-102031 says `yywrap()` and `main()` "shall appear only in
    // the lex library accessible through the -l l operand", so that a
    // conforming application can redefine them. We emit defaults anyway
    // (nothing in this project ships a libl, and the generated yylex() calls
    // yywrap() on every EOF path), but each is now individually suppressible
    // so an application supplying its own definition -- in this file or in a
    // separate translation unit -- can turn ours off instead of colliding at
    // link time (#L1).
    //
    // The user_subs/external_def scan is a convenience for definitions written
    // into the lex input itself; it cannot see a separate .c file, which is
    // exactly why the macros exist. Match on identifier boundaries so a call
    // to yywrap(), or a `my_yywrap`, does not suppress the definition (#L14).
    let defines = |name: &str| {
        lexinfo
            .user_subs
            .iter()
            .chain(lexinfo.external_def.iter())
            .any(|s| contains_identifier(s, name))
    };

    if !defines("yywrap") {
        writeln!(
            output,
            "/* Default yywrap; suppress with -DYY_NO_DEFAULT_YYWRAP */"
        )?;
        writeln!(output, "#ifndef YY_NO_DEFAULT_YYWRAP")?;
        writeln!(output, "int yywrap(void)")?;
        writeln!(output, "{{")?;
        writeln!(output, "    return 1;")?;
        writeln!(output, "}}")?;
        writeln!(output, "#endif")?;
        writeln!(output)?;
    }

    if !defines("main") {
        writeln!(
            output,
            "/* Default main; suppress with -DYY_NO_DEFAULT_MAIN */"
        )?;
        writeln!(output, "#ifndef YY_NO_DEFAULT_MAIN")?;
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
    use crate::nfa::NfaRule;
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
        let nfa = Nfa::from_rules(&[NfaRule::plain(hir, 0)], 1).unwrap();
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
        // Anchored, so the beginning-of-line root and the other one differ and
        // the generated scanner has to choose between them on entry.
        let nfa = Nfa::from_rules(
            &[NfaRule {
                main: hir,
                trailing: None,
                index: 0,
                bol_anchor: true,
                active_conditions: vec![0],
            }],
            1,
        )
        .unwrap();
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
        // The anchor is resolved by entering a separate automaton.
        assert!(
            s.contains("if (yy_at_bol) goto yy_state_"),
            "an anchored rule should produce a beginning-of-line entry state"
        );
    }

    #[test]
    fn test_generate_with_start_conditions() {
        let hir = regex_syntax::parse("foo").unwrap();
        let nfa = Nfa::from_rules(&[NfaRule::plain(hir, 0)], 1).unwrap();
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
        let nfa = Nfa::from_rules(&[NfaRule::plain(hir, 0)], 1).unwrap();
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
