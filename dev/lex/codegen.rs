//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! C code generation for the lexical analyzer.
//!
//! This module generates POSIX-compliant C code from the DFA and rule actions.

use crate::dfa::{CompressedTables, Dfa};
use crate::lexfile::LexInfo;
use std::fmt::Display;
use std::io::{self, Write};

/// Write an array of values inline (comma-separated, single line)
fn write_array_inline<W: Write, T: Display>(output: &mut W, values: &[T]) -> io::Result<()> {
    for (i, v) in values.iter().enumerate() {
        if i > 0 {
            write!(output, ", ")?;
        }
        write!(output, "{}", v)?;
    }
    Ok(())
}

/// Write an array of values with formatting (16 per line, right-aligned)
fn write_array_formatted<W: Write, T: Display>(output: &mut W, values: &[T]) -> io::Result<()> {
    for (i, v) in values.iter().enumerate() {
        if i % 16 == 0 {
            write!(output, "\n    ")?;
        }
        write!(output, "{:4}", v)?;
        if i < values.len() - 1 {
            write!(output, ",")?;
        }
    }
    Ok(())
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

/// Whether to use compressed or dense table format
#[derive(Clone, Copy, PartialEq, Default)]
pub enum TableFormat {
    Dense,
    Compressed,
    /// Automatically choose based on compression ratio (default)
    #[default]
    Auto,
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
    /// Table format: Dense, Compressed, or Auto (default)
    pub table_format: TableFormat,
}

impl Default for CodeGenConfig {
    fn default() -> Self {
        CodeGenConfig {
            yytext_is_pointer: true,
            yytext_size: 8192,
            start_conditions: vec!["INITIAL".to_string()],
            rule_metadata: Vec::new(),
            table_format: TableFormat::Auto,
        }
    }
}

/// Generate the complete lex.yy.c output
pub fn generate<W: Write>(
    output: &mut W,
    dfa: &Dfa,
    lexinfo: &LexInfo,
    config: &CodeGenConfig,
) -> io::Result<()> {
    // Try compression and decide which format to use
    let compressed = dfa.compress();

    // Build-time verification: exhaustively check compressed tables match original
    if let Err(e) = compressed.verify(dfa) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("Table compression verification failed: {}", e),
        ));
    }

    let stats = compressed.stats();

    // Determine table format based on config
    let table_format = match config.table_format {
        TableFormat::Auto => {
            // Use compressed tables only if they're actually smaller (ratio > 1.0)
            if stats.ratio > 1.0 {
                TableFormat::Compressed
            } else {
                TableFormat::Dense
            }
        }
        TableFormat::Dense => TableFormat::Dense,
        TableFormat::Compressed => TableFormat::Compressed,
    };

    // Log table format decision
    eprintln!(
        "lex: {} states, {} classes, dense={}B, compressed={}B (ratio: {:.2}x) -> {}",
        stats.num_states,
        stats.num_classes,
        stats.dense_size,
        stats.compressed_size,
        stats.ratio,
        if table_format == TableFormat::Compressed {
            "using compressed"
        } else {
            "using dense"
        }
    );

    write_header(output)?;
    write_includes(output)?;
    write_external_definitions(output, lexinfo)?;
    write_macros_and_types(output, config)?;

    match table_format {
        TableFormat::Compressed => write_compressed_tables(output, dfa, &compressed)?,
        TableFormat::Dense => write_dense_tables(output, dfa)?,
        TableFormat::Auto => unreachable!("Auto should have been resolved above"),
    }
    write_accepting_table(output, dfa)?;
    write_accepting_list_table(output, dfa)?;
    write_char_class_table(output, dfa)?;
    write_rule_condition_table(output, lexinfo, config)?;
    write_rule_metadata_tables(output, lexinfo, config)?;
    write_main_pattern_end_table(output, dfa, config)?;
    write_internal_definitions(output, lexinfo)?;
    write_helper_functions(output)?;
    write_yylex_function(output, dfa, lexinfo, config, table_format)?;
    write_user_subroutines(output, lexinfo)?;

    Ok(())
}

fn write_header<W: Write>(output: &mut W) -> io::Result<()> {
    writeln!(output, "/* Generated by lex-rs - POSIX compatible lex */")?;
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

fn write_macros_and_types<W: Write>(output: &mut W, config: &CodeGenConfig) -> io::Result<()> {
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

    // yytext declaration
    if config.yytext_is_pointer {
        writeln!(
            output,
            r#"/* yytext as pointer */
static char yy_yytext_buf[YY_BUF_SIZE + 1];
char *yytext = yy_yytext_buf;
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
FILE *yyout = NULL;
static int yy_start_state = INITIAL;

/* Input buffer */
static char yy_buffer[YY_BUF_SIZE + 1];
static int yy_buffer_pos = 0;
static int yy_buffer_len = 0;

/* Beginning of line tracking */
static int yy_at_bol = 1; /* Start at beginning of line */

/* REJECT support */
static int yy_reject_flag = 0;
static int yy_full_match_pos = 0;
static int yy_full_match_state = 0;
static int yy_full_match_rule_idx = 0;
static int yy_saved_buffer_pos = 0; /* Buffer pos before action */

#ifndef REJECT
#define REJECT {{ yy_reject_flag = 1; yy_buffer_pos = yy_saved_buffer_pos; goto yy_find_next_match; }}
#endif

/* yymore support */
static int yy_more_flag = 0;
static int yy_more_len = 0;

/* Variable-length trailing context support */
static int yy_main_end_pos = 0; /* Position where main pattern ended */
static int yy_main_end_rule = -1; /* Which rule's main pattern ended there */

#ifndef yymore
#define yymore() (yy_more_flag = 1)
#endif

/* yyless - return characters to input */
#ifndef yyless
#define yyless(n) do {{ \
    yy_buffer_pos = yy_buffer_pos - yyleng + (n); \
    yyleng = (n); \
    yytext[yyleng] = '\0'; \
}} while (0)
#endif

/* unput support - pushback buffer */
static char yy_unput_buf[YY_BUF_SIZE];
static int yy_unput_pos = 0;
"#
    )?;

    Ok(())
}

/// Write compressed DFA tables using row displacement encoding
fn write_compressed_tables<W: Write>(
    output: &mut W,
    dfa: &Dfa,
    compressed: &CompressedTables,
) -> io::Result<()> {
    let num_states = dfa.states.len();
    let num_classes = dfa.char_classes.num_classes;

    writeln!(
        output,
        "/* Compressed DFA transition tables (row displacement) */"
    )?;
    writeln!(output, "#define YY_NUM_STATES {}", num_states)?;
    writeln!(output, "#define YY_NUM_CLASSES {}", num_classes)?;
    writeln!(
        output,
        "#define YY_NXT_SIZE {}",
        compressed.nxt.len().max(1)
    )?;
    writeln!(output)?;

    // Write base array (offset into nxt/chk for each state)
    writeln!(
        output,
        "/* Base offset for each state into nxt/chk arrays */"
    )?;
    write!(output, "static const int yy_base[YY_NUM_STATES] = {{ ")?;
    write_array_inline(output, &compressed.base)?;
    writeln!(output, " }};\n")?;

    // Write default transition array
    writeln!(output, "/* Default transition for each state */")?;
    write!(output, "static const short yy_default[YY_NUM_STATES] = {{ ")?;
    write_array_inline(output, &compressed.default)?;
    writeln!(output, " }};\n")?;

    // Write nxt array (packed next-state transitions)
    writeln!(output, "/* Packed next-state array */")?;
    if compressed.nxt.is_empty() {
        writeln!(output, "static const short yy_nxt[1] = {{ -1 }};\n")?;
    } else {
        write!(output, "static const short yy_nxt[YY_NXT_SIZE] = {{")?;
        write_array_formatted(output, &compressed.nxt)?;
        writeln!(output, "\n}};\n")?;
    }

    // Write chk array (state ownership verification)
    writeln!(output, "/* State ownership verification array */")?;
    if compressed.chk.is_empty() {
        writeln!(output, "static const short yy_chk[1] = {{ -1 }};\n")?;
    } else {
        write!(output, "static const short yy_chk[YY_NXT_SIZE] = {{")?;
        write_array_formatted(output, &compressed.chk)?;
        writeln!(output, "\n}};\n")?;
    }

    Ok(())
}

/// Write dense (uncompressed) 2D DFA transition table
fn write_dense_tables<W: Write>(output: &mut W, dfa: &Dfa) -> io::Result<()> {
    let num_states = dfa.states.len();
    let num_classes = dfa.char_classes.num_classes;

    writeln!(output, "/* Dense DFA state transition table */")?;
    writeln!(output, "#define YY_NUM_STATES {}", num_states)?;
    writeln!(output, "#define YY_NUM_CLASSES {}", num_classes)?;
    writeln!(output, "#define YY_USE_DENSE_TABLES 1")?;
    writeln!(output)?;

    // Generate the transition table: yy_nxt[state][char_class] = next_state (-1 for no transition)
    writeln!(
        output,
        "static const short yy_nxt[YY_NUM_STATES][YY_NUM_CLASSES] = {{"
    )?;

    for (state_idx, state) in dfa.states.iter().enumerate() {
        write!(output, "    /* state {} */ {{ ", state_idx)?;

        // Build transition array for this state
        let mut class_transitions: Vec<i16> = vec![-1; num_classes];

        for (input, &target) in &state.transitions {
            let crate::dfa::DfaInput::Char(ch) = input;
            let ch_code = *ch as u32;
            if ch_code < 256 {
                let class_idx = dfa.char_classes.char_to_class[ch_code as usize] as usize;
                class_transitions[class_idx] = target as i16;
            }
        }

        for (i, trans) in class_transitions.iter().enumerate() {
            if i > 0 {
                write!(output, ", ")?;
            }
            write!(output, "{:3}", trans)?;
        }
        writeln!(output, " }},")?;
    }

    writeln!(output, "}};\n")?;

    Ok(())
}

fn write_accepting_table<W: Write>(output: &mut W, dfa: &Dfa) -> io::Result<()> {
    writeln!(output, "/* Accepting states table */")?;
    writeln!(
        output,
        "/* -1 = not accepting, >= 0 = rule number (action index) */"
    )?;
    write!(output, "static const short yy_accept[YY_NUM_STATES] = {{ ")?;

    for (i, state) in dfa.states.iter().enumerate() {
        if i > 0 {
            write!(output, ", ")?;
        }
        match state.accepting {
            Some(rule) => write!(output, "{}", rule)?,
            None => write!(output, "-1")?,
        }
    }

    writeln!(output, " }};\n")?;

    Ok(())
}

/// Write the accepting rules list for REJECT support
/// yy_accept_list contains all accepting rules per state (for trying next-best matches)
/// Format: yy_accept_list[yy_accept_idx[state]..yy_accept_idx[state+1]] gives the rules
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
    write_array_inline(output, &accept_idx)?;
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
        write_array_inline(output, &accept_list)?;
        writeln!(output, " }};\n")?;
    }

    Ok(())
}

fn write_char_class_table<W: Write>(output: &mut W, dfa: &Dfa) -> io::Result<()> {
    writeln!(output, "/* Character to equivalence class mapping */")?;
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

/// Write a table indicating which rules are active in which start conditions
/// yy_rule_cond[rule][condition] = 1 if rule is active in condition, 0 otherwise
fn write_rule_condition_table<W: Write>(
    output: &mut W,
    lexinfo: &LexInfo,
    config: &CodeGenConfig,
) -> io::Result<()> {
    let num_rules = lexinfo.rules.len();
    let num_conditions = config.start_conditions.len();

    // Only generate the table if we have more than just INITIAL
    if num_conditions <= 1 {
        return Ok(());
    }

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
) -> io::Result<()> {
    let num_rules = lexinfo.rules.len();
    if num_rules == 0 {
        return Ok(());
    }

    // Check if any rules have BOL anchoring
    let has_bol_anchors = config.rule_metadata.iter().any(|m| m.bol_anchor);

    // Check if any rules have trailing context
    let has_trailing_context = config.rule_metadata.iter().any(|m| m.has_trailing_context);

    // Define YY_NUM_RULES if needed for BOL or trailing context checks
    // (Only define if not already defined by write_rule_condition_table)
    if (has_bol_anchors || has_trailing_context) && config.start_conditions.len() <= 1 {
        writeln!(output, "#ifndef YY_NUM_RULES")?;
        writeln!(output, "#define YY_NUM_RULES {}", num_rules)?;
        writeln!(output, "#endif\n")?;
    }

    if has_bol_anchors {
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

    if has_trailing_context {
        // Write main pattern length table (for trailing context handling)
        // -1 = no trailing context (don't adjust yyleng)
        // >=0 = fixed main pattern length (set yyleng to this value)
        // -2 = variable main pattern length (complex case, not fully supported)
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
/// This allows runtime tracking of where the main pattern ends during DFA simulation
fn write_main_pattern_end_table<W: Write>(
    output: &mut W,
    dfa: &Dfa,
    config: &CodeGenConfig,
) -> io::Result<()> {
    // Check if any rules have variable-length trailing context
    let has_var_tc = config
        .rule_metadata
        .iter()
        .any(|m| m.has_variable_trailing_context);
    if !has_var_tc {
        return Ok(());
    }

    // Generate the table: yy_state_main_end[state] = rule whose main pattern ends at this state
    // -1 means no main pattern ends at this state
    // For simplicity, if multiple rules have main pattern ends at same state, we use the lowest index
    writeln!(
        output,
        "/* Main pattern end table (for variable-length trailing context) */"
    )?;
    writeln!(
        output,
        "/* yy_state_main_end[state] = rule whose main pattern ends at this state (-1 = none) */"
    )?;
    write!(
        output,
        "static const short yy_state_main_end[YY_NUM_STATES] = {{ "
    )?;

    for (state_idx, state) in dfa.states.iter().enumerate() {
        if state_idx > 0 {
            write!(output, ", ")?;
        }
        // Get the first (lowest) rule that has main pattern end at this state
        let main_end_rule = state.main_pattern_end_rules.first().copied().unwrap_or(!0);
        if main_end_rule == !0 {
            write!(output, "-1")?;
        } else {
            write!(output, "{}", main_end_rule)?;
        }
    }

    writeln!(output, " }};\n")?;

    // Also generate table marking which rules have variable-length trailing context
    writeln!(output, "/* Variable trailing context flag table */")?;
    writeln!(
        output,
        "/* yy_rule_var_tc[rule] = 1 if rule has variable-length trailing context */"
    )?;
    let num_rules = config.rule_metadata.len();
    write!(
        output,
        "static const unsigned char yy_rule_var_tc[{}] = {{ ",
        num_rules.max(1)
    )?;
    for (i, meta) in config.rule_metadata.iter().enumerate() {
        if i > 0 {
            write!(output, ", ")?;
        }
        write!(
            output,
            "{}",
            if meta.has_variable_trailing_context {
                1
            } else {
                0
            }
        )?;
    }
    if num_rules == 0 {
        write!(output, "0")?;
    }
    writeln!(output, " }};\n")?;

    Ok(())
}

fn write_internal_definitions<W: Write>(output: &mut W, lexinfo: &LexInfo) -> io::Result<()> {
    if !lexinfo.internal_defs.is_empty() {
        writeln!(output, "/* User internal definitions */")?;
        for line in &lexinfo.internal_defs {
            write!(output, "{}", line)?;
        }
        writeln!(output)?;
    }
    Ok(())
}

fn write_helper_functions<W: Write>(output: &mut W) -> io::Result<()> {
    // input() and unput() functions
    writeln!(
        output,
        r#"/* input - read one character from input */
static int input(void)
{{
    int c;
    /* First check unput buffer */
    if (yy_unput_pos > 0) {{
        return (unsigned char)yy_unput_buf[--yy_unput_pos];
    }}
    /* Then check main buffer */
    if (yy_buffer_pos < yy_buffer_len) {{
        return (unsigned char)yy_buffer[yy_buffer_pos++];
    }}
    /* Need to refill buffer */
    if (yyin == NULL) yyin = stdin;
    c = getc(yyin);
    if (c == EOF) return 0;
    return c;
}}

/* unput - push character back to input */
static void unput(int c)
{{
    if (yy_buffer_pos > 0) {{
        /* Push back into main buffer if possible */
        yy_buffer[--yy_buffer_pos] = (char)c;
    }} else {{
        /* Use pushback buffer */
        if (yy_unput_pos < YY_BUF_SIZE) {{
            yy_unput_buf[yy_unput_pos++] = (char)c;
        }}
    }}
}}
"#
    )?;

    Ok(())
}

fn write_yylex_function<W: Write>(
    output: &mut W,
    _dfa: &Dfa,
    lexinfo: &LexInfo,
    config: &CodeGenConfig,
    table_format: TableFormat,
) -> io::Result<()> {
    let has_start_conditions = config.start_conditions.len() > 1;

    writeln!(output, "/* The main lexer function */")?;
    writeln!(output, "int yylex(void)")?;
    writeln!(output, "{{")?;

    // Local variables
    writeln!(output, "    int yy_current_state;")?;
    writeln!(output, "    int yy_act;")?;
    writeln!(output, "    int yy_cp;")?;
    writeln!(output, "    int yy_last_accepting_state;")?;
    writeln!(output, "    int yy_last_accepting_cpos;")?;
    writeln!(output, "    int yy_i;")?;
    if has_start_conditions {
        writeln!(output, "    int yy_rule_valid;")?;
    }
    writeln!(output)?;

    // Initialize yyin/yyout if needed
    writeln!(output, "    if (yyin == NULL) yyin = stdin;")?;
    writeln!(output, "    if (yyout == NULL) yyout = stdout;")?;
    writeln!(output)?;

    // Main scanning loop
    writeln!(output, "    while (1) {{")?;

    // Read input if buffer is empty or depleted
    writeln!(output, "        /* Fill buffer if needed */")?;
    writeln!(output, "        if (yy_buffer_pos >= yy_buffer_len) {{")?;
    writeln!(output, "            /* First drain any unput buffer */")?;
    writeln!(output, "            if (yy_unput_pos > 0) {{")?;
    writeln!(output, "                int i;")?;
    writeln!(output, "                yy_buffer_len = yy_unput_pos;")?;
    writeln!(
        output,
        "                for (i = 0; i < yy_unput_pos; i++) {{"
    )?;
    writeln!(
        output,
        "                    yy_buffer[i] = yy_unput_buf[yy_unput_pos - 1 - i];"
    )?;
    writeln!(output, "                }}")?;
    writeln!(output, "                yy_unput_pos = 0;")?;
    writeln!(output, "                yy_buffer_pos = 0;")?;
    writeln!(output, "            }} else {{")?;
    writeln!(output, "                int result;")?;
    writeln!(
        output,
        "                YY_INPUT(yy_buffer, result, YY_BUF_SIZE);"
    )?;
    writeln!(output, "                if (result == 0) {{")?;
    writeln!(
        output,
        "                    /* EOF - call yywrap() per POSIX */"
    )?;
    writeln!(output, "                    if (yywrap()) {{")?;
    writeln!(
        output,
        "                        return 0; /* yywrap returned non-zero, stop */"
    )?;
    writeln!(output, "                    }}")?;
    writeln!(
        output,
        "                    /* yywrap returned 0, continue with new input */"
    )?;
    writeln!(output, "                    continue;")?;
    writeln!(output, "                }}")?;
    writeln!(output, "                yy_buffer_len = result;")?;
    writeln!(output, "                yy_buffer_pos = 0;")?;
    writeln!(output, "            }}")?;
    writeln!(output, "        }}")?;
    writeln!(output)?;

    // Reset REJECT tracking for new match
    writeln!(output, "        /* Reset REJECT tracking */")?;
    writeln!(output, "        yy_reject_flag = 0;")?;
    writeln!(output, "        yy_full_match_rule_idx = 0;")?;
    writeln!(output)?;

    // Reset variable-length trailing context tracking
    let has_var_tc = config
        .rule_metadata
        .iter()
        .any(|m| m.has_variable_trailing_context);
    if has_var_tc {
        writeln!(
            output,
            "        /* Reset variable-length trailing context tracking */"
        )?;
        writeln!(output, "        yy_main_end_pos = yy_buffer_pos;")?;
        writeln!(output, "        yy_main_end_rule = -1;")?;
        writeln!(output)?;
    }

    // Start from initial state (DFA state 0)
    writeln!(output, "        yy_current_state = 0;")?;
    writeln!(output, "        yy_cp = yy_buffer_pos;")?;
    writeln!(output, "        yy_last_accepting_state = -1;")?;
    writeln!(output, "        yy_last_accepting_cpos = yy_buffer_pos;")?;
    writeln!(output)?;

    // DFA simulation
    writeln!(output, "        /* Run DFA until no more transitions */")?;
    writeln!(output, "        while (yy_cp < yy_buffer_len) {{")?;
    writeln!(
        output,
        "            unsigned char yy_c = (unsigned char)yy_buffer[yy_cp];"
    )?;
    writeln!(output, "            int yy_class = yy_ec[yy_c];")?;

    // Generate lookup code based on table format
    // Note: Auto is resolved to Dense or Compressed in generate() before calling this function
    match table_format {
        TableFormat::Dense => {
            writeln!(
                output,
                "            int yy_next = yy_nxt[yy_current_state][yy_class];"
            )?;
        }
        TableFormat::Compressed | TableFormat::Auto => {
            writeln!(
                output,
                "            int yy_idx = yy_base[yy_current_state] + yy_class;"
            )?;
            writeln!(output, "            int yy_next;")?;
            writeln!(
                output,
                "            if (yy_idx >= 0 && yy_idx < YY_NXT_SIZE && yy_chk[yy_idx] == yy_current_state)"
            )?;
            writeln!(output, "                yy_next = yy_nxt[yy_idx];")?;
            writeln!(output, "            else")?;
            writeln!(
                output,
                "                yy_next = yy_default[yy_current_state];"
            )?;
        }
    }
    writeln!(output)?;
    writeln!(output, "            if (yy_next < 0) {{")?;
    writeln!(
        output,
        "                /* No transition - check if current state accepts */"
    )?;
    writeln!(
        output,
        "                if (yy_accept[yy_current_state] >= 0) {{"
    )?;
    writeln!(
        output,
        "                    yy_last_accepting_state = yy_current_state;"
    )?;
    writeln!(
        output,
        "                    yy_last_accepting_cpos = yy_cp;"
    )?;
    writeln!(output, "                }}")?;
    writeln!(output, "                break;")?;
    writeln!(output, "            }}")?;
    writeln!(output)?;
    writeln!(output, "            yy_current_state = yy_next;")?;
    writeln!(output, "            yy_cp++;")?;
    writeln!(output)?;
    writeln!(output, "            /* Track accepting states as we go */")?;
    writeln!(
        output,
        "            if (yy_accept[yy_current_state] >= 0) {{"
    )?;
    writeln!(
        output,
        "                yy_last_accepting_state = yy_current_state;"
    )?;
    writeln!(output, "                yy_last_accepting_cpos = yy_cp;")?;
    writeln!(output, "            }}")?;

    // Track main pattern end for variable-length trailing context
    if has_var_tc {
        writeln!(output)?;
        writeln!(
            output,
            "            /* Track main pattern end for variable-length trailing context */"
        )?;
        writeln!(output, "            {{")?;
        writeln!(
            output,
            "                short yy_me_rule = yy_state_main_end[yy_current_state];"
        )?;
        writeln!(
            output,
            "                if (yy_me_rule >= 0 && yy_rule_var_tc[yy_me_rule]) {{"
        )?;
        writeln!(
            output,
            "                    /* This state marks end of main pattern for a var TC rule */"
        )?;
        writeln!(output, "                    yy_main_end_pos = yy_cp;")?;
        writeln!(output, "                    yy_main_end_rule = yy_me_rule;")?;
        writeln!(output, "                }}")?;
        writeln!(output, "            }}")?;
    }

    writeln!(output, "        }}")?;
    writeln!(output)?;

    // Save full match info for REJECT
    writeln!(output, "        /* Save full match info for REJECT */")?;
    writeln!(
        output,
        "        yy_full_match_state = yy_last_accepting_state;"
    )?;
    writeln!(
        output,
        "        yy_full_match_pos = yy_last_accepting_cpos;"
    )?;
    writeln!(output)?;

    // Label for REJECT to jump to
    writeln!(output, "    yy_find_next_match:")?;

    // Check final state
    writeln!(output, "        /* Determine action */")?;
    writeln!(output, "        if (yy_last_accepting_state >= 0) {{")?;

    // Handle REJECT - find next rule at current position or fall back to shorter match
    writeln!(
        output,
        "            /* Find the rule to execute (handling REJECT) */"
    )?;
    writeln!(output, "            yy_act = -1;")?;
    writeln!(
        output,
        "            for (yy_i = yy_accept_idx[yy_last_accepting_state]; yy_i < yy_accept_idx[yy_last_accepting_state + 1]; yy_i++) {{"
    )?;
    writeln!(
        output,
        "                int yy_rule = yy_accept_list[yy_i];"
    )?;
    writeln!(
        output,
        "                if (yy_rule > yy_full_match_rule_idx || (yy_reject_flag == 0 && yy_rule >= yy_full_match_rule_idx)) {{"
    )?;

    // Check if any rules have BOL anchors
    let has_bol_anchors = config.rule_metadata.iter().any(|m| m.bol_anchor);

    if has_start_conditions || has_bol_anchors {
        writeln!(output, "                    int yy_rule_ok = 1;")?;
        if has_start_conditions {
            writeln!(
                output,
                "                    /* Check if rule is active in current start condition */"
            )?;
            writeln!(
                output,
                "                    if (yy_rule >= YY_NUM_RULES || !yy_rule_cond[yy_rule][yy_start_state]) {{"
            )?;
            writeln!(output, "                        yy_rule_ok = 0;")?;
            writeln!(output, "                    }}")?;
        }
        if has_bol_anchors {
            writeln!(
                output,
                "                    /* Check if rule requires beginning of line */"
            )?;
            writeln!(
                output,
                "                    if (yy_rule_ok && yy_rule < YY_NUM_RULES && yy_rule_bol[yy_rule] && !yy_at_bol) {{"
            )?;
            writeln!(output, "                        yy_rule_ok = 0;")?;
            writeln!(output, "                    }}")?;
        }
        writeln!(output, "                    if (yy_rule_ok) {{")?;
        writeln!(output, "                        yy_act = yy_rule;")?;
        writeln!(output, "                        break;")?;
        writeln!(output, "                    }}")?;
    } else {
        writeln!(output, "                    yy_act = yy_rule;")?;
        writeln!(output, "                    break;")?;
    }
    writeln!(output, "                }}")?;
    writeln!(output, "            }}")?;
    writeln!(output)?;

    writeln!(output, "            if (yy_act < 0) {{")?;
    writeln!(
        output,
        "                /* No more rules at this position, try shorter match */"
    )?;
    writeln!(
        output,
        "                if (yy_last_accepting_cpos > yy_buffer_pos + 1) {{"
    )?;
    writeln!(
        output,
        "                    /* Try matching a shorter string */"
    )?;
    writeln!(output, "                    yy_last_accepting_cpos--;")?;
    writeln!(output, "                    yy_reject_flag = 0;")?;
    writeln!(output, "                    yy_full_match_rule_idx = 0;")?;
    writeln!(output)?;
    writeln!(
        output,
        "                    /* Re-run DFA to find accepting state at new position */"
    )?;
    writeln!(output, "                    yy_current_state = 0;")?;
    writeln!(output, "                    yy_last_accepting_state = -1;")?;
    // Reset variable-length trailing context tracking for re-run
    if has_var_tc {
        writeln!(
            output,
            "                    yy_main_end_pos = yy_buffer_pos;"
        )?;
        writeln!(output, "                    yy_main_end_rule = -1;")?;
    }
    writeln!(
        output,
        "                    for (yy_cp = yy_buffer_pos; yy_cp < yy_last_accepting_cpos + 1 && yy_cp < yy_buffer_len; yy_cp++) {{"
    )?;
    writeln!(
        output,
        "                        unsigned char yy_c = (unsigned char)yy_buffer[yy_cp];"
    )?;
    writeln!(
        output,
        "                        int yy_class = yy_ec[yy_c];"
    )?;

    // Generate lookup code based on table format (same as main loop)
    match table_format {
        TableFormat::Dense => {
            writeln!(
                output,
                "                        int yy_next = yy_nxt[yy_current_state][yy_class];"
            )?;
        }
        TableFormat::Compressed | TableFormat::Auto => {
            writeln!(
                output,
                "                        int yy_idx = yy_base[yy_current_state] + yy_class;"
            )?;
            writeln!(output, "                        int yy_next;")?;
            writeln!(
                output,
                "                        if (yy_idx >= 0 && yy_idx < YY_NXT_SIZE && yy_chk[yy_idx] == yy_current_state)"
            )?;
            writeln!(
                output,
                "                            yy_next = yy_nxt[yy_idx];"
            )?;
            writeln!(output, "                        else")?;
            writeln!(
                output,
                "                            yy_next = yy_default[yy_current_state];"
            )?;
        }
    }
    writeln!(output, "                        if (yy_next < 0) break;")?;
    writeln!(
        output,
        "                        yy_current_state = yy_next;"
    )?;
    writeln!(
        output,
        "                        if (yy_accept[yy_current_state] >= 0) {{"
    )?;
    writeln!(
        output,
        "                            yy_last_accepting_state = yy_current_state;"
    )?;
    writeln!(output, "                        }}")?;
    // Track main pattern end in re-run loop for variable-length trailing context
    if has_var_tc {
        writeln!(output, "                        {{")?;
        writeln!(
            output,
            "                            short yy_me_rule = yy_state_main_end[yy_current_state];"
        )?;
        writeln!(
            output,
            "                            if (yy_me_rule >= 0 && yy_rule_var_tc[yy_me_rule]) {{"
        )?;
        writeln!(
            output,
            "                                yy_main_end_pos = yy_cp;"
        )?;
        writeln!(
            output,
            "                                yy_main_end_rule = yy_me_rule;"
        )?;
        writeln!(output, "                            }}")?;
        writeln!(output, "                        }}")?;
    }
    writeln!(output, "                    }}")?;
    writeln!(
        output,
        "                    yy_full_match_state = yy_last_accepting_state;"
    )?;
    writeln!(
        output,
        "                    yy_full_match_pos = yy_last_accepting_cpos;"
    )?;
    writeln!(output, "                    goto yy_find_next_match;")?;
    writeln!(output, "                }}")?;
    writeln!(
        output,
        "                /* No match found - output one character */"
    )?;
    writeln!(
        output,
        "                yy_at_bol = (yy_buffer[yy_buffer_pos] == '\\n');"
    )?;
    writeln!(
        output,
        "                putc(yy_buffer[yy_buffer_pos], yyout);"
    )?;
    writeln!(output, "                yy_buffer_pos++;")?;
    writeln!(output, "                continue;")?;
    writeln!(output, "            }}")?;

    writeln!(output, "            yy_cp = yy_last_accepting_cpos;")?;
    writeln!(output, "            yy_full_match_rule_idx = yy_act;")?;

    writeln!(output, "        }} else {{")?;
    writeln!(
        output,
        "            /* No match - output one character (default action) */"
    )?;
    writeln!(
        output,
        "            yy_at_bol = (yy_buffer[yy_buffer_pos] == '\\n');"
    )?;
    writeln!(output, "            putc(yy_buffer[yy_buffer_pos], yyout);")?;
    writeln!(output, "            yy_buffer_pos++;")?;
    writeln!(output, "            continue;")?;
    writeln!(output, "        }}")?;
    writeln!(output)?;

    // Set yytext and yyleng - copy to yytext buffer and null-terminate
    // Handle yymore() - if flag is set, append to existing yytext
    writeln!(
        output,
        "        /* Set yytext and yyleng (handle yymore) */"
    )?;
    writeln!(output, "        if (yy_more_flag) {{")?;
    writeln!(
        output,
        "            /* Append new match to existing yytext */"
    )?;
    writeln!(
        output,
        "            int yy_new_len = yy_cp - yy_buffer_pos;"
    )?;
    writeln!(
        output,
        "            memcpy(yytext + yy_more_len, &yy_buffer[yy_buffer_pos], yy_new_len);"
    )?;
    writeln!(output, "            yyleng = yy_more_len + yy_new_len;")?;
    writeln!(output, "            yytext[yyleng] = '\\0';")?;
    writeln!(output, "            yy_more_flag = 0;")?;
    writeln!(output, "        }} else {{")?;
    writeln!(output, "            yyleng = yy_cp - yy_buffer_pos;")?;
    writeln!(
        output,
        "            memcpy(yytext, &yy_buffer[yy_buffer_pos], yyleng);"
    )?;
    writeln!(output, "            yytext[yyleng] = '\\0';")?;
    writeln!(output, "        }}")?;
    writeln!(
        output,
        "        yy_more_len = yyleng; /* Save for potential yymore() */"
    )?;
    writeln!(output)?;

    // Handle trailing context - adjust yyleng and yytext, and track adjustment for buffer position
    let has_trailing_context = config.rule_metadata.iter().any(|m| m.has_trailing_context);
    if has_trailing_context {
        writeln!(
            output,
            "        /* Handle trailing context - adjust yyleng and buffer position */"
        )?;
        writeln!(output, "        int yy_tc_adjustment = 0;")?;
        writeln!(
            output,
            "        if (yy_act >= 0 && yy_act < YY_NUM_RULES) {{"
        )?;
        writeln!(
            output,
            "            int yy_main_len = yy_rule_main_len[yy_act];"
        )?;
        writeln!(output, "            if (yy_main_len >= 0) {{")?;
        writeln!(
            output,
            "                /* Fixed-length main pattern with trailing context */"
        )?;
        writeln!(
            output,
            "                yy_tc_adjustment = yyleng - yy_main_len;"
        )?;
        writeln!(output, "                yyleng = yy_main_len;")?;
        writeln!(output, "                yytext[yyleng] = '\\0';")?;
        writeln!(output, "            }} else if (yy_main_len == -2) {{")?;
        writeln!(
            output,
            "                /* Variable-length main pattern with trailing context */"
        )?;
        writeln!(
            output,
            "                /* Use tracked main pattern end position */"
        )?;
        writeln!(
            output,
            "                if (yy_main_end_rule == yy_act && yy_main_end_pos > yy_buffer_pos) {{"
        )?;
        writeln!(
            output,
            "                    int yy_actual_main_len = yy_main_end_pos - yy_buffer_pos;"
        )?;
        writeln!(
            output,
            "                    yy_tc_adjustment = yyleng - yy_actual_main_len;"
        )?;
        writeln!(output, "                    yyleng = yy_actual_main_len;")?;
        writeln!(output, "                    yytext[yyleng] = '\\0';")?;
        writeln!(output, "                }}")?;
        writeln!(output, "            }}")?;
        writeln!(output, "        }}")?;
        writeln!(output)?;
    }

    // Update yy_at_bol based on the last character of the matched text
    writeln!(output, "        /* Update beginning-of-line status */")?;
    writeln!(output, "        if (yyleng > 0) {{")?;
    writeln!(
        output,
        "            yy_at_bol = (yytext[yyleng - 1] == '\\n');"
    )?;
    writeln!(output, "        }}")?;
    writeln!(output)?;

    // Save buffer position for REJECT, then advance
    // For trailing context, only advance past the main pattern, not the trailing context
    writeln!(
        output,
        "        /* Save buffer position for REJECT, then advance past matched text */"
    )?;
    writeln!(output, "        yy_saved_buffer_pos = yy_buffer_pos;")?;
    if has_trailing_context {
        writeln!(output, "        yy_buffer_pos = yy_cp - yy_tc_adjustment;")?;
    } else {
        writeln!(output, "        yy_buffer_pos = yy_cp;")?;
    }
    writeln!(output)?;

    // Execute action based on rule
    writeln!(output, "        /* Execute rule action */")?;
    writeln!(output, "        switch (yy_act) {{")?;

    for (rule_idx, rule) in lexinfo.rules.iter().enumerate() {
        writeln!(output, "        case {}:", rule_idx)?;
        // Handle the '|' action (fall through to next rule)
        if rule.action.trim() == "|" {
            writeln!(output, "            /* fall through */")?;
        } else {
            writeln!(output, "            {}", rule.action)?;
            writeln!(output, "            break;")?;
        }
    }

    writeln!(output, "        default:")?;
    writeln!(output, "            ECHO;")?;
    writeln!(output, "            break;")?;
    writeln!(output, "        }}")?;

    writeln!(output, "    }}")?;

    writeln!(output, "}}\n")?;

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
        }
    }

    #[test]
    fn test_generate_header() {
        let mut output = Vec::new();
        write_header(&mut output).unwrap();
        let s = String::from_utf8(output).unwrap();
        assert!(s.contains("Generated by lex-rs"));
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
        });

        let mut output = Vec::new();
        let config = CodeGenConfig::default();
        generate(&mut output, &dfa, &lexinfo, &config).unwrap();

        let s = String::from_utf8(output).unwrap();
        assert!(s.contains("int yylex(void)"));
        assert!(s.contains("yy_nxt"));
        assert!(s.contains("yy_accept"));
    }

    #[test]
    fn test_generate_with_bol_anchor() {
        // Test that BOL anchor rules generate the correct code
        let hir = regex_syntax::parse("foo").unwrap();
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();
        let dfa = Dfa::from_nfa(&nfa);

        let mut lexinfo = create_test_lexinfo();
        lexinfo.rules.push(crate::lexfile::LexRule {
            ere: "^foo".to_string(),
            compiled_ere: "foo".to_string(),
            action: "return BOL_RULE;".to_string(),
            start_conditions: vec![],
            bol_anchor: true, // BOL-anchored rule
            trailing_context: None,
            compiled_trailing_context: None,
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

        // Should contain BOL checking logic in the action switch
        // The generated code checks yy_at_bol for BOL-anchored rules
        assert!(
            s.contains("return BOL_RULE"),
            "Should include the BOL rule action"
        );
        // yylex function should be generated
        assert!(
            s.contains("int yylex(void)"),
            "Should generate yylex function"
        );
    }

    #[test]
    fn test_generate_with_start_conditions() {
        // Test that start conditions generate correct code
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

        // Should contain start condition definitions
        assert!(
            s.contains("#define INITIAL 0"),
            "Should define INITIAL start condition"
        );
        assert!(
            s.contains("#define COMMENT 1"),
            "Should define COMMENT start condition"
        );
        assert!(
            s.contains("#define STRING 2"),
            "Should define STRING start condition"
        );
        assert!(s.contains("BEGIN"), "Should define BEGIN macro");
    }

    #[test]
    fn test_generate_with_trailing_context() {
        // Test that trailing context generates correct handling code
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
        });

        let rule_meta = vec![RuleMetadata {
            bol_anchor: false,
            main_pattern_len: Some(3), // "foo" is 3 chars
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

        // Should handle fixed-length trailing context by adjusting yyleng
        assert!(
            s.contains("TC_RULE"),
            "Should include the trailing context rule action"
        );
    }

    #[test]
    fn test_compressed_table_format() {
        // Test that compressed table format generates correct arrays
        let hir = regex_syntax::parse("[a-z]+").unwrap();
        let nfa = Nfa::from_rules(&[(hir, 0)]).unwrap();
        let dfa = Dfa::from_nfa(&nfa);
        let minimized = dfa.minimize();

        let mut lexinfo = create_test_lexinfo();
        lexinfo.rules.push(crate::lexfile::LexRule {
            ere: "[a-z]+".to_string(),
            compiled_ere: "[a-z]+".to_string(),
            action: "return ID;".to_string(),
            start_conditions: vec![],
            bol_anchor: false,
            trailing_context: None,
            compiled_trailing_context: None,
        });

        let mut output = Vec::new();
        let config = CodeGenConfig {
            table_format: TableFormat::Compressed,
            ..Default::default()
        };
        generate(&mut output, &minimized, &lexinfo, &config).unwrap();

        let s = String::from_utf8(output).unwrap();

        // Compressed format should have base, default, nxt, chk arrays
        assert!(
            s.contains("yy_base"),
            "Compressed format should have yy_base array"
        );
        assert!(
            s.contains("yy_def"),
            "Compressed format should have yy_def array"
        );
        assert!(
            s.contains("yy_nxt"),
            "Compressed format should have yy_nxt array"
        );
        assert!(
            s.contains("yy_chk"),
            "Compressed format should have yy_chk array"
        );
    }
}
