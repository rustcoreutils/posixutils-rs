//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! localedef - define locale environment
//!
//! The localedef utility converts source definitions for locale
//! categories into a format usable by the functions and utilities
//! whose operational behavior is determined by locale settings.

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::PathBuf;
use std::process::exit;

/// localedef - define locale environment
#[derive(Parser)]
#[command(
    version,
    about = "localedef - define locale environment",
    disable_help_flag = true,
    disable_version_flag = true
)]
struct Args {
    /// Create permanent output even if warnings occur
    #[arg(short = 'c')]
    force: bool,

    /// Path of charmap file
    #[arg(short = 'f')]
    charmap: Option<PathBuf>,

    /// Path of source definitions
    #[arg(short = 'i')]
    input: Option<PathBuf>,

    /// Target codeset for conversion
    #[arg(short = 'u')]
    code_set: Option<String>,

    /// Verbose output
    #[arg(short = 'v')]
    verbose: bool,

    /// Print help
    #[arg(short, long, action = clap::ArgAction::HelpLong)]
    help: Option<bool>,

    /// Print version
    #[arg(short = 'V', long, action = clap::ArgAction::Version)]
    version: Option<bool>,

    /// Name of the locale to define
    name: String,
}

/// Parsed locale definition
#[derive(Debug, Default)]
struct LocaleDefinition {
    /// LC_CTYPE data
    lc_ctype: LcCtypeData,
    /// LC_COLLATE data
    lc_collate: LcCollateData,
    /// LC_MONETARY data
    lc_monetary: LcMonetaryData,
    /// LC_NUMERIC data
    lc_numeric: LcNumericData,
    /// LC_TIME data
    lc_time: LcTimeData,
    /// LC_MESSAGES data
    lc_messages: LcMessagesData,
}

#[derive(Debug, Default)]
struct LcCtypeData {
    copy_from: Option<String>,
}

#[derive(Debug, Default)]
struct LcCollateData {
    copy_from: Option<String>,
}

#[derive(Debug, Default)]
struct LcMonetaryData {
    copy_from: Option<String>,
    int_curr_symbol: Option<String>,
    currency_symbol: Option<String>,
    mon_decimal_point: Option<String>,
    mon_thousands_sep: Option<String>,
    positive_sign: Option<String>,
    negative_sign: Option<String>,
}

#[derive(Debug, Default)]
struct LcNumericData {
    copy_from: Option<String>,
    decimal_point: Option<String>,
    thousands_sep: Option<String>,
    grouping: Option<String>,
}

#[derive(Debug, Default)]
struct LcTimeData {
    copy_from: Option<String>,
    _abday: Vec<String>,
    _day: Vec<String>,
    _abmon: Vec<String>,
    _mon: Vec<String>,
    d_t_fmt: Option<String>,
    d_fmt: Option<String>,
    t_fmt: Option<String>,
    _am_pm: Vec<String>,
}

#[derive(Debug, Default)]
struct LcMessagesData {
    copy_from: Option<String>,
    yesexpr: Option<String>,
    noexpr: Option<String>,
    yesstr: Option<String>,
    nostr: Option<String>,
}

/// Warning or error from compilation
struct Diagnostic {
    line: usize,
    message: String,
    is_error: bool,
}

fn main() {
    // Set up localization
    setlocale(LocaleCategory::LcAll, "");
    if textdomain("posixutils-rs").is_err() {
        // Ignore error
    }
    let _ = bind_textdomain_codeset("posixutils-rs", "UTF-8");

    let args = Args::parse();

    // Read input
    let input = match read_input(&args.input) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("localedef: {}", e);
            exit(4);
        }
    };

    // Parse the locale definition
    let mut diagnostics = Vec::new();
    let definition = match parse_locale_definition(&input, &mut diagnostics) {
        Ok(def) => def,
        Err(e) => {
            eprintln!("localedef: {}", e);
            exit(4);
        }
    };

    // Print diagnostics
    let has_errors = diagnostics.iter().any(|d| d.is_error);
    let has_warnings = diagnostics.iter().any(|d| !d.is_error);

    for diag in &diagnostics {
        if diag.is_error || args.verbose {
            eprintln!(
                "localedef:{}: {}: {}",
                diag.line,
                if diag.is_error { "error" } else { "warning" },
                diag.message
            );
        }
    }

    // Determine exit code
    let exit_code = if has_errors {
        4 // Errors occurred
    } else if has_warnings && !args.force {
        1 // Warnings but continuing
    } else {
        0
    };

    // Write output if no errors (or if -c specified with warnings)
    if !has_errors || args.force {
        if let Err(e) = write_locale(&args.name, &definition, args.verbose) {
            eprintln!("localedef: {}", e);
            exit(4);
        }
    }

    exit(exit_code);
}

/// Read input from file or stdin
fn read_input(input_path: &Option<PathBuf>) -> Result<String, String> {
    match input_path {
        Some(path) => {
            let file = File::open(path).map_err(|e| format!("{}: {}", path.display(), e))?;
            let mut reader = BufReader::new(file);
            let mut content = String::new();
            reader
                .read_to_string(&mut content)
                .map_err(|e| format!("{}: {}", path.display(), e))?;
            Ok(content)
        }
        None => {
            let mut content = String::new();
            std::io::stdin()
                .read_to_string(&mut content)
                .map_err(|e| format!("stdin: {}", e))?;
            Ok(content)
        }
    }
}

/// Parse a locale definition file
fn parse_locale_definition(
    input: &str,
    diagnostics: &mut Vec<Diagnostic>,
) -> Result<LocaleDefinition, String> {
    let mut definition = LocaleDefinition::default();
    let mut current_category: Option<&str> = None;
    let mut line_number = 0;

    for line in input.lines() {
        line_number += 1;
        let line = line.trim();

        // Skip empty lines and comments
        if line.is_empty()
            || line.starts_with('#')
            || line.starts_with("comment_char")
            || line.starts_with("escape_char")
        {
            continue;
        }

        // Check for category start
        if line.starts_with("LC_") && !line.contains(' ') {
            current_category = Some(line);
            continue;
        }

        // Check for category end
        if line.starts_with("END ") {
            current_category = None;
            continue;
        }

        // Parse category content
        if let Some(category) = current_category {
            parse_category_line(category, line, &mut definition, line_number, diagnostics);
        }
    }

    Ok(definition)
}

/// Parse a line within a category
fn parse_category_line(
    category: &str,
    line: &str,
    definition: &mut LocaleDefinition,
    line_number: usize,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Handle copy directive
    if let Some(rest) = line.strip_prefix("copy") {
        let source = rest.trim().trim_matches('"');
        match category {
            "LC_CTYPE" => definition.lc_ctype.copy_from = Some(source.to_string()),
            "LC_COLLATE" => definition.lc_collate.copy_from = Some(source.to_string()),
            "LC_MONETARY" => definition.lc_monetary.copy_from = Some(source.to_string()),
            "LC_NUMERIC" => definition.lc_numeric.copy_from = Some(source.to_string()),
            "LC_TIME" => definition.lc_time.copy_from = Some(source.to_string()),
            "LC_MESSAGES" => definition.lc_messages.copy_from = Some(source.to_string()),
            _ => {}
        }
        return;
    }

    // Parse keyword value pairs
    let parts: Vec<&str> = line.splitn(2, char::is_whitespace).collect();
    if parts.len() < 2 {
        return;
    }

    let keyword = parts[0];
    let value = parts[1].trim().trim_matches('"');

    match category {
        "LC_NUMERIC" => match keyword {
            "decimal_point" => definition.lc_numeric.decimal_point = Some(value.to_string()),
            "thousands_sep" => definition.lc_numeric.thousands_sep = Some(value.to_string()),
            "grouping" => definition.lc_numeric.grouping = Some(value.to_string()),
            _ => {
                diagnostics.push(Diagnostic {
                    line: line_number,
                    message: format!("unknown keyword: {}", keyword),
                    is_error: false,
                });
            }
        },
        "LC_MONETARY" => match keyword {
            "int_curr_symbol" => definition.lc_monetary.int_curr_symbol = Some(value.to_string()),
            "currency_symbol" => definition.lc_monetary.currency_symbol = Some(value.to_string()),
            "mon_decimal_point" => {
                definition.lc_monetary.mon_decimal_point = Some(value.to_string())
            }
            "mon_thousands_sep" => {
                definition.lc_monetary.mon_thousands_sep = Some(value.to_string())
            }
            "positive_sign" => definition.lc_monetary.positive_sign = Some(value.to_string()),
            "negative_sign" => definition.lc_monetary.negative_sign = Some(value.to_string()),
            _ => {}
        },
        "LC_TIME" => match keyword {
            "d_t_fmt" => definition.lc_time.d_t_fmt = Some(value.to_string()),
            "d_fmt" => definition.lc_time.d_fmt = Some(value.to_string()),
            "t_fmt" => definition.lc_time.t_fmt = Some(value.to_string()),
            _ => {}
        },
        "LC_MESSAGES" => match keyword {
            "yesexpr" => definition.lc_messages.yesexpr = Some(value.to_string()),
            "noexpr" => definition.lc_messages.noexpr = Some(value.to_string()),
            "yesstr" => definition.lc_messages.yesstr = Some(value.to_string()),
            "nostr" => definition.lc_messages.nostr = Some(value.to_string()),
            _ => {}
        },
        _ => {}
    }
}

/// Write the compiled locale
fn write_locale(name: &str, _definition: &LocaleDefinition, verbose: bool) -> Result<(), String> {
    // Determine output path
    let output_path = if name.contains('/') {
        PathBuf::from(name)
    } else {
        // Write to user's locale directory or a temp location
        let mut path = std::env::temp_dir();
        path.push("locale");
        path.push(name);
        path
    };

    // Create directory
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| format!("cannot create directory: {}", e))?;
    }

    // For now, just create a marker file
    // A full implementation would write the binary locale format
    let marker_path = output_path.join("LC_IDENTIFICATION");
    std::fs::create_dir_all(&output_path)
        .map_err(|e| format!("cannot create locale directory: {}", e))?;

    std::fs::write(&marker_path, format!("locale: {}\n", name))
        .map_err(|e| format!("cannot write locale data: {}", e))?;

    if verbose {
        eprintln!("localedef: created locale at {}", output_path.display());
    }

    Ok(())
}
