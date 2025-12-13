//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod codegen;
mod error;
mod first_follow;
mod grammar;
mod lalr;
mod lexer;
mod lr0;
mod parser;

use std::env;
use std::fs;
use std::process;

use error::YaccError;

/// Command-line options for yacc
#[derive(Debug, Clone)]
pub struct Options {
    /// -b file_prefix: Use file_prefix instead of "y" for output files
    pub file_prefix: String,
    /// -d: Write header file
    pub write_header: bool,
    /// -l: Omit #line directives
    pub omit_line_directives: bool,
    /// -p sym_prefix: Use sym_prefix instead of "yy" for external names
    pub sym_prefix: String,
    /// -t: Enable debugging code
    pub debug_enabled: bool,
    /// -v: Write description file
    pub write_description: bool,
    /// Input grammar file
    pub grammar_file: String,
}

impl Default for Options {
    fn default() -> Self {
        Options {
            file_prefix: "y".to_string(),
            write_header: false,
            omit_line_directives: false,
            sym_prefix: "yy".to_string(),
            debug_enabled: false,
            write_description: false,
            grammar_file: String::new(),
        }
    }
}

fn parse_args() -> Result<Options, YaccError> {
    let args: Vec<String> = env::args().collect();
    let mut opts = Options::default();

    let mut i = 1;
    while i < args.len() {
        let arg = &args[i];
        if arg.starts_with('-') {
            let mut chars = arg.chars().skip(1);
            while let Some(c) = chars.next() {
                match c {
                    'b' => {
                        // -b file_prefix
                        let prefix = if let Some(remaining) = chars.next() {
                            // -bprefix (no space)
                            let mut s = String::new();
                            s.push(remaining);
                            s.extend(chars);
                            s
                        } else {
                            // -b prefix (with space)
                            i += 1;
                            if i >= args.len() {
                                return Err(YaccError::Usage(
                                    "option -b requires an argument".into(),
                                ));
                            }
                            args[i].clone()
                        };
                        opts.file_prefix = prefix;
                        break;
                    }
                    'd' => opts.write_header = true,
                    'l' => opts.omit_line_directives = true,
                    'p' => {
                        // -p sym_prefix
                        let prefix = if let Some(remaining) = chars.next() {
                            let mut s = String::new();
                            s.push(remaining);
                            s.extend(chars);
                            s
                        } else {
                            i += 1;
                            if i >= args.len() {
                                return Err(YaccError::Usage(
                                    "option -p requires an argument".into(),
                                ));
                            }
                            args[i].clone()
                        };
                        opts.sym_prefix = prefix;
                        break;
                    }
                    't' => opts.debug_enabled = true,
                    'v' => opts.write_description = true,
                    _ => {
                        return Err(YaccError::Usage(format!("unknown option: -{}", c)));
                    }
                }
            }
        } else {
            // Grammar file
            if !opts.grammar_file.is_empty() {
                return Err(YaccError::Usage("multiple grammar files specified".into()));
            }
            opts.grammar_file = arg.clone();
        }
        i += 1;
    }

    if opts.grammar_file.is_empty() {
        return Err(YaccError::Usage("no grammar file specified".into()));
    }

    Ok(opts)
}

fn print_usage() {
    eprintln!("Usage: yacc [-dltv] [-b file_prefix] [-p sym_prefix] grammar");
    eprintln!("Options:");
    eprintln!("  -b file_prefix  Use file_prefix instead of 'y' for output files");
    eprintln!("  -d              Write header file");
    eprintln!("  -l              Omit #line directives");
    eprintln!("  -p sym_prefix   Use sym_prefix instead of 'yy' for external names");
    eprintln!("  -t              Enable debugging code in generated parser");
    eprintln!("  -v              Write description file");
}

fn run(opts: &Options) -> Result<(), YaccError> {
    // Read input grammar
    let input = fs::read_to_string(&opts.grammar_file)
        .map_err(|e| YaccError::Io(format!("cannot read '{}': {}", opts.grammar_file, e)))?;

    // Lex the input
    let tokens = lexer::lex(&input)?;

    // Parse the grammar
    let parsed = parser::parse(&tokens)?;

    // Build grammar representation
    let grammar = grammar::Grammar::from_parsed(parsed)?;

    // Compute FIRST and FOLLOW sets
    let first_follow = first_follow::compute(&grammar);

    // Build LR(0) automaton
    let lr0_automaton = lr0::build(&grammar);

    // Compute LALR(1) lookaheads
    let lalr_automaton = lalr::compute(&lr0_automaton, &grammar, &first_follow);

    // Generate output files
    codegen::generate(opts, &grammar, &lalr_automaton)?;

    // Report conflicts to stderr
    let (sr_conflicts, rr_conflicts) = lalr_automaton.count_conflicts();
    if sr_conflicts > 0 || rr_conflicts > 0 {
        if sr_conflicts > 0 {
            eprintln!(
                "{}: {} shift/reduce conflict{}",
                opts.grammar_file,
                sr_conflicts,
                if sr_conflicts == 1 { "" } else { "s" }
            );
        }
        if rr_conflicts > 0 {
            eprintln!(
                "{}: {} reduce/reduce conflict{}",
                opts.grammar_file,
                rr_conflicts,
                if rr_conflicts == 1 { "" } else { "s" }
            );
        }
    }

    Ok(())
}

fn main() {
    let opts = match parse_args() {
        Ok(opts) => opts,
        Err(e) => {
            eprintln!("yacc: {}", e);
            print_usage();
            process::exit(1);
        }
    };

    if let Err(e) = run(&opts) {
        eprintln!("yacc: {}", e);
        process::exit(1);
    }
}
