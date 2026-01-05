//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod codegen;
mod dfa;
pub mod diag;
mod lexfile;
mod nfa;
mod pattern_escape;
mod pattern_validate;

use clap::Parser;
use dfa::Dfa;
use gettextrs::gettext;
use nfa::Nfa;
use regex_syntax::ast::parse::ParserBuilder;
use regex_syntax::hir::{Hir, translate::TranslatorBuilder};
use std::fs;
use std::io::{self, BufRead, Read, Write};

/// lex - generate programs for lexical tasks (POSIX compatible)
#[derive(Parser, Debug)]
#[command(author, version, about = gettext("lex - generate programs for lexical tasks (POSIX compatible)"))]
struct Args {
    #[arg(short = 'n', long, help = gettext("Suppress the summary of statistics usually written with the -v option"))]
    no_stats: bool,

    #[arg(short = 't', long, help = gettext("Write the resulting program to standard output instead of lex.yy.c"))]
    stdout: bool,

    #[arg(short, long, help = gettext("Write a summary of lex statistics to the standard output"))]
    verbose: bool,

    #[arg(short, long, default_value = "lex.yy.c", help = gettext("Write output to this filename (unless superceded by -t)"))]
    outfile: String,

    #[arg(long, help = gettext("Force dense (non-compressed) DFA tables"))]
    dense: bool,

    #[arg(help = gettext("Files to read as input"))]
    files: Vec<String>,
}

/// Concatenate input files, handling special filename "-" as stdin
fn concat_input_files(files: &[String]) -> io::Result<Vec<String>> {
    let mut input = Vec::new();

    for filename in files {
        let file: Box<dyn Read> = if filename == "-" {
            Box::new(io::stdin().lock())
        } else {
            Box::new(fs::File::open(filename)?)
        };
        let mut reader = io::BufReader::new(file);

        loop {
            let mut line = String::new();
            let n_read = reader.read_line(&mut line);
            match n_read {
                Ok(0) => break,
                Ok(_) => {
                    input.push(line);
                }
                Err(e) => {
                    eprintln!("Error reading file: {}", e);
                    return Err(e);
                }
            }
        }
    }

    Ok(input)
}

/// A parsed rule with its HIR, index, and start conditions
pub struct ParsedRule {
    pub hir: Hir,
    pub index: usize,
    pub start_conditions: Vec<String>,
    pub bol_anchor: bool,
    pub trailing_context: Option<Hir>,
    /// Fixed length of the main pattern (if computable)
    /// Used to determine yyleng when trailing context is present
    pub main_pattern_len: Option<usize>,
    /// True if this rule has variable-length trailing context
    /// (i.e., trailing context exists but main pattern length is not fixed)
    pub has_variable_trailing_context: bool,
}

/// Parse a regex pattern to HIR with POSIX-compliant settings
/// In particular, '.' should NOT match newlines per POSIX spec
fn parse_regex_posix(pattern: &str) -> Result<Hir, String> {
    // Parse to AST
    let ast = ParserBuilder::new()
        .build()
        .parse(pattern)
        .map_err(|e| format!("Failed to parse regular expression '{}': {}", pattern, e))?;

    // Translate to HIR with POSIX-compliant settings:
    // - dot_matches_new_line(false): '.' should NOT match newlines per POSIX
    let hir = TranslatorBuilder::new()
        .dot_matches_new_line(false)
        .build()
        .translate(pattern, &ast)
        .map_err(|e| {
            format!(
                "Failed to translate regular expression '{}': {}",
                pattern, e
            )
        })?;

    Ok(hir)
}

/// Parse all rule patterns and return them with their indices and start conditions
fn parse_rules(lexinfo: &lexfile::LexInfo) -> Result<Vec<ParsedRule>, String> {
    let mut rules = Vec::new();

    for (idx, rule) in lexinfo.rules.iter().enumerate() {
        // Use compiled_ere which has substitutions wrapped in parens for correct quantifier handling
        let hir = parse_regex_posix(&rule.compiled_ere)
            .map_err(|e| format!("rule {}: pattern '{}': {}", idx + 1, rule.ere, e))?;

        // Parse trailing context if present (use compiled version)
        let (trailing_context, main_pattern_len, has_variable_tc) =
            if let Some(ref tc) = rule.compiled_trailing_context {
                let tc_hir = parse_regex_posix(tc).map_err(|e| {
                    format!(
                        "rule {}: trailing context '{}': {}",
                        idx + 1,
                        rule.trailing_context.as_deref().unwrap_or(""),
                        e
                    )
                })?;
                // Compute fixed length of MAIN pattern (for setting yyleng correctly)
                // If main pattern has fixed length, we can set yyleng to that value
                let main_len = compute_fixed_length(&hir);
                // Variable-length TC means main pattern doesn't have fixed length
                let has_var_tc = main_len.is_none();
                (Some(tc_hir), main_len, has_var_tc)
            } else {
                (None, None, false)
            };

        rules.push(ParsedRule {
            hir,
            index: idx,
            start_conditions: rule.start_conditions.clone(),
            bol_anchor: rule.bol_anchor,
            trailing_context,
            main_pattern_len,
            has_variable_trailing_context: has_variable_tc,
        });
    }

    Ok(rules)
}

/// Try to compute the fixed length of a regex pattern
/// Returns Some(length) if the pattern has a fixed length, None otherwise
fn compute_fixed_length(hir: &Hir) -> Option<usize> {
    use regex_syntax::hir::HirKind;

    match hir.kind() {
        HirKind::Empty => Some(0),
        HirKind::Literal(lit) => {
            // Count UTF-8 bytes in the literal
            Some(lit.0.len())
        }
        HirKind::Class(_) => Some(1), // Character class matches exactly 1 character
        HirKind::Look(_) => Some(0),  // Look-around assertions don't consume input
        HirKind::Concat(parts) => {
            let mut total = 0;
            for part in parts.iter() {
                total += compute_fixed_length(part)?;
            }
            Some(total)
        }
        HirKind::Alternation(alts) => {
            // All alternatives must have the same length
            let first_len = compute_fixed_length(&alts[0])?;
            for alt in &alts[1..] {
                if compute_fixed_length(alt)? != first_len {
                    return None;
                }
            }
            Some(first_len)
        }
        HirKind::Repetition(rep) => {
            // Only fixed repetitions have fixed length
            if rep.min == rep.max.unwrap_or(0) {
                let sub_len = compute_fixed_length(&rep.sub)?;
                Some(sub_len * rep.min as usize)
            } else {
                None
            }
        }
        HirKind::Capture(cap) => compute_fixed_length(&cap.sub),
    }
}

/// Get all start condition names (INITIAL is always included as index 0)
fn get_start_conditions(lexinfo: &lexfile::LexInfo) -> Vec<String> {
    let mut conditions = vec!["INITIAL".to_string()];
    for name in &lexinfo.cond_start {
        if !conditions.contains(name) {
            conditions.push(name.clone());
        }
    }
    for name in &lexinfo.cond_xstart {
        if !conditions.contains(name) {
            conditions.push(name.clone());
        }
    }
    conditions
}

/// Write statistics to the given output
fn write_stats<W: Write + ?Sized>(
    output: &mut W,
    dfa: &Dfa,
    nfa: &Nfa,
    lexinfo: &lexfile::LexInfo,
) -> io::Result<()> {
    writeln!(output, "lex statistics:")?;
    writeln!(output, "  {} rules", lexinfo.rules.len())?;
    writeln!(output, "  {} substitution definitions", lexinfo.subs.len())?;
    writeln!(output, "  {} NFA states", nfa.states.len())?;
    writeln!(
        output,
        "  {} DFA states (before minimization)",
        dfa.num_states()
    )?;
    writeln!(output, "  {} DFA transitions", dfa.num_transitions())?;
    writeln!(
        output,
        "  {} character equivalence classes",
        dfa.char_classes.num_classes
    )?;
    // Output declared table sizes if any were specified
    if !lexinfo.table_sizes.is_empty() {
        writeln!(output, "  declared table sizes:")?;
        for (key, value) in &lexinfo.table_sizes {
            let desc = match key {
                'p' => "positions",
                'n' => "states",
                'a' => "transitions",
                'e' => "parse tree nodes",
                'k' => "packed character classes",
                'o' => "output array size",
                _ => "unknown",
            };
            writeln!(output, "    %{} {} ({})", key, value, desc)?;
        }
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Parse command line arguments
    let mut args = Args::parse();

    // If no files, read from stdin
    if args.files.is_empty() {
        args.files.push(String::from("-"));
    }

    // Initialize diagnostics with the input filename
    let input_name = if args.files.len() == 1 && args.files[0] != "-" {
        args.files[0].clone()
    } else if args.files.len() == 1 {
        "<stdin>".to_string()
    } else {
        // Multiple files concatenated - use first non-stdin name
        args.files
            .iter()
            .find(|f| *f != "-")
            .cloned()
            .unwrap_or_else(|| "<stdin>".to_string())
    };
    diag::init(&input_name);

    // POSIX says multiple input files are concatenated
    let rawinput = concat_input_files(&args.files)?;

    // Parse input lex file into a data structure containing the rules table
    let lexinfo = lexfile::parse(&rawinput)?;

    // Check for parse errors
    if diag::has_errors() {
        std::process::exit(1);
    }

    // Parse all regular expressions
    let rules = parse_rules(&lexinfo)?;

    if rules.is_empty() {
        eprintln!("Warning: no rules defined");
    }

    // Get all start conditions
    let start_conditions = get_start_conditions(&lexinfo);

    // Build NFA/DFA for each start condition
    // For simplicity, we build a single combined NFA/DFA but track which rules are active per condition
    // The code generator will handle selecting the right rules based on the current start condition

    // Build rules in the format expected by NFA with trailing context support
    // This tracks main pattern end states for variable-length trailing context
    let nfa_rules: Vec<(Hir, Option<Hir>, usize)> = rules
        .iter()
        .map(|r| (r.hir.clone(), r.trailing_context.clone(), r.index))
        .collect();

    // Build NFA from rules using Thompson's construction
    // This version properly tracks main pattern end states for trailing context
    let nfa = Nfa::from_rules_with_trailing_context(&nfa_rules)?;

    // Convert NFA to DFA using subset construction
    let dfa = Dfa::from_nfa(&nfa);

    // Minimize the DFA
    let dfa = dfa.minimize();

    // Determine output destination
    let mut output: Box<dyn Write> = if args.stdout {
        Box::new(io::stdout())
    } else {
        Box::new(fs::File::create(&args.outfile)?)
    };

    // Build rule metadata for code generation
    let rule_metadata: Vec<codegen::RuleMetadata> = rules
        .iter()
        .map(|r| codegen::RuleMetadata {
            bol_anchor: r.bol_anchor,
            main_pattern_len: r.main_pattern_len,
            has_trailing_context: r.trailing_context.is_some(),
            has_variable_trailing_context: r.has_variable_trailing_context,
        })
        .collect();

    // Generate C code
    let config = codegen::CodeGenConfig {
        yytext_is_pointer: lexinfo.yyt_is_ptr,
        start_conditions: start_conditions.clone(),
        rule_metadata,
        table_format: if args.dense {
            codegen::TableFormat::Dense
        } else {
            codegen::TableFormat::Auto
        },
        ..Default::default()
    };
    codegen::generate(&mut output, &dfa, &lexinfo, &config)?;

    // Write statistics if requested
    if args.verbose && !args.no_stats {
        let stats_output: &mut dyn Write = if args.stdout {
            &mut io::stderr()
        } else {
            &mut io::stdout()
        };
        write_stats(stats_output, &dfa, &nfa, &lexinfo)?;
    }

    if !args.stdout {
        eprintln!("Output written to {}", args.outfile);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_lex_file() {
        let input = vec![
            "%%\n".to_string(),
            "[a-z]+    printf(\"word\\n\");\n".to_string(),
            "%%\n".to_string(),
        ];

        let lexinfo = lexfile::parse(&input).expect("Failed to parse");
        assert_eq!(lexinfo.rules.len(), 1);

        let rules = parse_rules(&lexinfo).expect("Failed to parse rules");
        assert_eq!(rules.len(), 1);

        let nfa_rules: Vec<(Hir, usize)> = rules.iter().map(|r| (r.hir.clone(), r.index)).collect();
        let nfa = Nfa::from_rules(&nfa_rules).expect("Failed to build NFA");
        assert!(!nfa.states.is_empty());

        let dfa = Dfa::from_nfa(&nfa);
        assert!(!dfa.states.is_empty());
    }

    #[test]
    fn test_multiple_rules() {
        let input = vec![
            "%%\n".to_string(),
            "if    return IF;\n".to_string(),
            "then  return THEN;\n".to_string(),
            "[a-z]+    return ID;\n".to_string(),
            "%%\n".to_string(),
        ];

        let lexinfo = lexfile::parse(&input).expect("Failed to parse");
        assert_eq!(lexinfo.rules.len(), 3);

        let rules = parse_rules(&lexinfo).expect("Failed to parse rules");
        let nfa_rules: Vec<(Hir, usize)> = rules.iter().map(|r| (r.hir.clone(), r.index)).collect();
        let nfa = Nfa::from_rules(&nfa_rules).expect("Failed to build NFA");
        let dfa = Dfa::from_nfa(&nfa);
        let minimized = dfa.minimize();

        // "if" should be matched by rule 0, not rule 2
        // We can verify this by checking the DFA's accepting states
        assert!(minimized.states.iter().any(|s| s.accepting == Some(0)));
    }

    #[test]
    fn test_codegen() {
        let input = vec![
            "%%\n".to_string(),
            "a    return 1;\n".to_string(),
            "%%\n".to_string(),
        ];

        let lexinfo = lexfile::parse(&input).expect("Failed to parse");
        let rules = parse_rules(&lexinfo).expect("Failed to parse rules");
        let nfa_rules: Vec<(Hir, usize)> = rules.iter().map(|r| (r.hir.clone(), r.index)).collect();
        let nfa = Nfa::from_rules(&nfa_rules).expect("Failed to build NFA");
        let dfa = Dfa::from_nfa(&nfa);

        let mut output = Vec::new();
        let config = codegen::CodeGenConfig::default();
        codegen::generate(&mut output, &dfa, &lexinfo, &config).expect("Failed to generate code");

        let code = String::from_utf8(output).expect("Invalid UTF-8");
        assert!(code.contains("int yylex(void)"));
        assert!(code.contains("yy_nxt"));
        assert!(code.contains("case 0:")); // Rule 0 action
    }
}
