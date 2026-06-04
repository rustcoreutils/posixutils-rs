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
mod verify;

use std::env;
use std::fs;
use std::process;

use error::YaccError;
use gettextrs::gettext;
use plib::diag;

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
    /// --strict: Disable optimizations that may change yylex timing
    pub strict_mode: bool,
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
            strict_mode: false,
            grammar_file: String::new(),
        }
    }
}

fn parse_args() -> Result<Options, YaccError> {
    let args: Vec<String> = env::args().collect();
    let mut opts = Options::default();

    let mut i = 1;
    let mut end_of_opts = false;
    while i < args.len() {
        let arg = &args[i];

        // XBD Section 12.2: "--" terminates the options; every remaining
        // argument is an operand, even if it begins with '-'.
        if !end_of_opts && arg == "--" {
            end_of_opts = true;
            i += 1;
            continue;
        }

        // Treat as an operand (the grammar file) once "--" has been seen or
        // when the argument is not option-shaped.
        if end_of_opts || !arg.starts_with('-') {
            if !opts.grammar_file.is_empty() {
                return Err(YaccError::Usage(gettext(
                    "multiple grammar files specified",
                )));
            }
            opts.grammar_file = arg.clone();
            i += 1;
            continue;
        }

        if arg == "--strict" {
            opts.strict_mode = true;
            i += 1;
            continue;
        }

        // Option cluster: arg begins with '-' (e.g. -dtv, -bprefix).
        {
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
                                return Err(YaccError::Usage(gettext(
                                    "option -b requires an argument",
                                )));
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
                                return Err(YaccError::Usage(gettext(
                                    "option -p requires an argument",
                                )));
                            }
                            args[i].clone()
                        };
                        opts.sym_prefix = prefix;
                        break;
                    }
                    't' => opts.debug_enabled = true,
                    'v' => opts.write_description = true,
                    _ => {
                        return Err(YaccError::Usage(format!(
                            "{}: -{}",
                            gettext("unknown option"),
                            c
                        )));
                    }
                }
            }
        }
        i += 1;
    }

    if opts.grammar_file.is_empty() {
        return Err(YaccError::Usage(gettext("no grammar file specified")));
    }

    Ok(opts)
}

fn print_usage() {
    eprintln!(
        "{}",
        gettext("Usage: yacc [-dltv] [-b file_prefix] [-p sym_prefix] [--strict] grammar")
    );
    eprintln!("{}", gettext("Options:"));
    eprintln!(
        "  -b file_prefix  {}",
        gettext("Use file_prefix instead of 'y' for output files")
    );
    eprintln!("  -d              {}", gettext("Write header file"));
    eprintln!("  -l              {}", gettext("Omit #line directives"));
    eprintln!(
        "  -p sym_prefix   {}",
        gettext("Use sym_prefix instead of 'yy' for external names")
    );
    eprintln!(
        "  -t              {}",
        gettext("Enable debugging code in generated parser")
    );
    eprintln!("  -v              {}", gettext("Write description file"));
    eprintln!(
        "  --strict        {}",
        gettext("Disable optimizations that may change yylex timing")
    );
}

fn run(opts: &Options) -> Result<(), YaccError> {
    // Set the source filename used by diag::error_at / warning_at positions.
    diag::set_source(&opts.grammar_file);

    // Read input grammar
    let input = fs::read_to_string(&opts.grammar_file).map_err(|e| {
        YaccError::Io(format!(
            "{} '{}': {}",
            gettext("cannot read"),
            opts.grammar_file,
            e
        ))
    })?;

    // POSIX CONSEQUENCES OF ERRORS (123202-4): "summary information in the
    // description file shall always be produced if the -v flag is present."
    // If a stage fails before codegen builds the full description, emit a
    // stub so a y.output still exists.
    match run_pipeline(opts, &input) {
        Ok(()) => Ok(()),
        Err(e) => {
            if opts.write_description {
                write_description_stub(opts, &e);
            }
            Err(e)
        }
    }
}

/// Write a minimal description file when -v is set but generation aborted
/// before the full y.output could be produced. Never overwrites an existing
/// file: on the success path codegen writes the complete description first.
fn write_description_stub(opts: &Options, err: &YaccError) {
    let path = format!("{}.output", opts.file_prefix);
    if std::path::Path::new(&path).exists() {
        return;
    }
    let body = format!(
        "Grammar description for {}\n\n\
         (generation aborted before parser tables were built: {})\n\n\
         Internal table limits: dynamic; no fixed limits.\n",
        opts.grammar_file, err
    );
    let _ = fs::write(&path, body);
}

fn run_pipeline(opts: &Options, input: &str) -> Result<(), YaccError> {
    // Lex the input
    let tokens = lexer::lex(input)?;

    // Parse the grammar
    let parsed = parser::parse(&tokens)?;

    // Extract expect values before consuming parsed grammar
    let expect_sr = parsed.expect_sr;
    let expect_rr = parsed.expect_rr;

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

    // Check %expect and %expect-rr directives
    let mut conflict_error = false;

    // Handle shift/reduce conflicts
    match expect_sr {
        Some(expected) if sr_conflicts != expected => {
            diag::error(&format!(
                "{}: {} {} {}{}, {} {}",
                opts.grammar_file,
                gettext("expected"),
                expected,
                gettext("shift/reduce conflict"),
                if expected == 1 { "" } else { "s" },
                gettext("found"),
                sr_conflicts
            ));
            conflict_error = true;
        }
        Some(_) => {
            // Matches expected: suppress warning
        }
        None if sr_conflicts > 0 => {
            diag::warning(&format!(
                "{}: {} {}{}",
                opts.grammar_file,
                sr_conflicts,
                gettext("shift/reduce conflict"),
                if sr_conflicts == 1 { "" } else { "s" }
            ));
        }
        None => {}
    }

    // Handle reduce/reduce conflicts
    match expect_rr {
        Some(expected) if rr_conflicts != expected => {
            diag::error(&format!(
                "{}: {} {} {}{}, {} {}",
                opts.grammar_file,
                gettext("expected"),
                expected,
                gettext("reduce/reduce conflict"),
                if expected == 1 { "" } else { "s" },
                gettext("found"),
                rr_conflicts
            ));
            conflict_error = true;
        }
        Some(_) => {
            // Matches expected: suppress warning
        }
        None if rr_conflicts > 0 => {
            diag::warning(&format!(
                "{}: {} {}{}",
                opts.grammar_file,
                rr_conflicts,
                gettext("reduce/reduce conflict"),
                if rr_conflicts == 1 { "" } else { "s" }
            ));
        }
        None => {}
    }

    if conflict_error {
        return Err(YaccError::Grammar(gettext(
            "conflict count does not match expected",
        )));
    }

    Ok(())
}

fn main() {
    diag::init_locale("yacc");
    let opts = match parse_args() {
        Ok(opts) => opts,
        Err(e) => {
            diag::error(&format!("{}", e));
            print_usage();
            process::exit(1);
        }
    };

    if let Err(e) = run(&opts) {
        diag::error(&format!("{}", e));
    }
    process::exit(diag::exit_status());
}
