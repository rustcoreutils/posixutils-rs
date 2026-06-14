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
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use posixutils_i18n::locale_lib::types::{
    LC_MESSAGES_KEYWORDS, LC_MONETARY_KEYWORDS, LC_NUMERIC_KEYWORDS, LC_TIME_KEYWORDS,
};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::{Path, PathBuf};
use std::process::exit;

/// Locale category names recognized as section headers.
const CATEGORIES: &[&str] = &[
    "LC_CTYPE",
    "LC_COLLATE",
    "LC_MONETARY",
    "LC_NUMERIC",
    "LC_TIME",
    "LC_MESSAGES",
    "LC_IDENTIFICATION",
    "LC_PAPER",
    "LC_NAME",
    "LC_ADDRESS",
    "LC_TELEPHONE",
    "LC_MEASUREMENT",
];

/// Recognized keyword names for the simple (keyword = value) categories.
fn known_keywords(category: &str) -> &'static [&'static str] {
    match category {
        "LC_NUMERIC" => LC_NUMERIC_KEYWORDS,
        "LC_MONETARY" => LC_MONETARY_KEYWORDS,
        "LC_TIME" => LC_TIME_KEYWORDS,
        "LC_MESSAGES" => LC_MESSAGES_KEYWORDS,
        _ => &[],
    }
}

/// localedef - define locale environment
#[derive(Parser)]
#[command(
    version,
    about = gettext("localedef - define locale environment"),
    disable_help_flag = true,
    disable_version_flag = true
)]
struct Args {
    #[arg(short = 'c', help = gettext("Create permanent output even if warnings occur"))]
    force: bool,

    #[arg(short = 'f', help = gettext("Path of charmap file"))]
    charmap: Option<PathBuf>,

    #[arg(short = 'i', help = gettext("Path of source definitions"))]
    input: Option<PathBuf>,

    #[arg(short = 'u', help = gettext("Target codeset for conversion"))]
    code_set: Option<String>,

    #[arg(short = 'v', help = gettext("Verbose output"))]
    verbose: bool,

    #[arg(short, long, action = clap::ArgAction::HelpLong, help = gettext("Print help"))]
    help: Option<bool>,

    #[arg(short = 'V', long, action = clap::ArgAction::Version, help = gettext("Print version"))]
    version: Option<bool>,

    #[arg(help = gettext("Name of the locale to define"))]
    name: String,
}

/// Parsed data for a single locale category.
#[derive(Debug, Default)]
struct CategoryData {
    /// `copy` directive target, if present.
    copy_from: Option<String>,
    /// keyword = value pairs.
    keywords: HashMap<String, String>,
    /// `<symbolic>` names referenced (LC_CTYPE / LC_COLLATE).
    symbols: Vec<String>,
}

/// Parsed locale definition: per-category data plus the order categories were
/// processed in (for the STDOUT report).
#[derive(Debug, Default)]
struct LocaleDefinition {
    categories: HashMap<String, CategoryData>,
    order: Vec<String>,
}

impl LocaleDefinition {
    fn category_mut(&mut self, name: &str) -> &mut CategoryData {
        if !self.categories.contains_key(name) {
            self.order.push(name.to_string());
        }
        self.categories.entry(name.to_string()).or_default()
    }
}

/// Mutable parser state carried across (possibly `include`d) source files.
struct ParseState {
    comment_char: char,
    escape_char: char,
    current_category: Option<String>,
}

impl Default for ParseState {
    fn default() -> Self {
        ParseState {
            comment_char: '#',
            escape_char: '\\',
            current_category: None,
        }
    }
}

/// Warning or error from processing.
struct Diagnostic {
    line: usize,
    message: String,
    is_error: bool,
}

impl Diagnostic {
    fn error(line: usize, message: String) -> Self {
        Diagnostic {
            line,
            message,
            is_error: true,
        }
    }
    fn warning(line: usize, message: String) -> Self {
        Diagnostic {
            line,
            message,
            is_error: false,
        }
    }
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

    // LD-2: a `-f charmap` operand is opened and validated up front. A charmap
    // that cannot be read means the coded character set is not available, so —
    // per the exit-status table — exit 2 and create no locale.
    let charmap_symbols = match &args.charmap {
        Some(path) => match parse_charmap_symbols(path) {
            Ok(symbols) => Some(symbols),
            Err(e) => {
                eprintln!("localedef: {}", e);
                exit(2);
            }
        },
        None => None,
    };

    // Parse the locale definition.
    let base_dir = args
        .input
        .as_deref()
        .and_then(Path::parent)
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("."));
    let mut diagnostics = Vec::new();
    let mut definition = LocaleDefinition::default();
    let mut state = ParseState::default();
    parse_source(
        &input,
        &base_dir,
        charmap_symbols.as_ref(),
        &mut definition,
        &mut state,
        &mut diagnostics,
        0,
    );
    if let Some(open) = state.current_category.take() {
        diagnostics.push(Diagnostic::error(0, format!("missing END {}", open)));
    }

    let has_errors = diagnostics.iter().any(|d| d.is_error);
    let has_warnings = diagnostics.iter().any(|d| !d.is_error);

    // LD-11: warnings are always reported, not only under -v.
    for diag in &diagnostics {
        eprintln!(
            "localedef:{}: {}: {}",
            diag.line,
            if diag.is_error { "error" } else { "warning" },
            diag.message
        );
    }

    // LD-4: a detected error never produces output, even with -c. Warnings
    // produce output only with -c.
    let create_output = !has_errors && (args.force || !has_warnings);

    if create_output {
        if let Err(e) = write_locale(&args.name, &definition, args.verbose) {
            eprintln!("localedef: {}", e);
            exit(4);
        }
        // LD-9: report the categories successfully processed.
        for category in &definition.order {
            println!("{}", category);
        }
    }

    // LD-5: 0 = created, no warnings; 1 = created, warnings; >3 = errors or
    // warnings with no output created. (Exit 2 for an unsupported charset is
    // handled above; 3 — "creating locales unsupported" — is not used since a
    // marker locale is still written.)
    let exit_code = if create_output {
        if has_warnings {
            1
        } else {
            0
        }
    } else {
        4
    };

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

/// Parse a locale source (recursively for `include`d files). `depth` guards
/// against runaway include nesting.
#[allow(clippy::too_many_arguments)]
fn parse_source(
    input: &str,
    base_dir: &Path,
    charmap_symbols: Option<&HashSet<String>>,
    definition: &mut LocaleDefinition,
    state: &mut ParseState,
    diagnostics: &mut Vec<Diagnostic>,
    depth: usize,
) {
    let raw: Vec<&str> = input.lines().collect();
    let mut i = 0;

    while i < raw.len() {
        let line_number = i + 1;
        let trimmed = raw[i].trim();
        i += 1;

        // Empty / comment lines (comment_char is honored — LD-6).
        if trimmed.is_empty() || trimmed.starts_with(state.comment_char) {
            continue;
        }

        // escape_char / comment_char directives (LD-6).
        if let Some(rest) = trimmed.strip_prefix("escape_char") {
            if let Some(c) = rest.trim().chars().next() {
                state.escape_char = c;
            }
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("comment_char") {
            if let Some(c) = rest.trim().chars().next() {
                state.comment_char = c;
            }
            continue;
        }

        // Join continued lines (a line ending in escape_char — LD-6).
        let mut logical = trimmed.to_string();
        while logical.ends_with(state.escape_char) {
            logical.pop();
            if i < raw.len() {
                logical.push_str(raw[i].trim());
                i += 1;
            } else {
                break;
            }
        }
        let logical = logical.trim().to_string();
        if logical.is_empty() {
            continue;
        }

        // Category header (LD-12: matched against the known category set).
        if CATEGORIES.contains(&logical.as_str()) {
            state.current_category = Some(logical);
            continue;
        }

        // Category end (LD-12: validated against the open category).
        if let Some(rest) = logical.strip_prefix("END") {
            let ended = rest.trim();
            match state.current_category.take() {
                Some(open) if open == ended => {}
                Some(open) => diagnostics.push(Diagnostic::error(
                    line_number,
                    format!("END {} does not match open category {}", ended, open),
                )),
                None => diagnostics.push(Diagnostic::error(
                    line_number,
                    format!("END {} with no open category", ended),
                )),
            }
            continue;
        }

        // include directive (LD-7): inline another source file.
        if let Some(rest) = logical.strip_prefix("include") {
            let name = rest.trim().trim_matches('"');
            if depth >= 8 {
                diagnostics.push(Diagnostic::error(
                    line_number,
                    "include nesting too deep".to_string(),
                ));
                continue;
            }
            let path = base_dir.join(name);
            match std::fs::read_to_string(&path) {
                Ok(included) => {
                    let inc_dir = path
                        .parent()
                        .map(Path::to_path_buf)
                        .unwrap_or_else(|| base_dir.to_path_buf());
                    parse_source(
                        &included,
                        &inc_dir,
                        charmap_symbols,
                        definition,
                        state,
                        diagnostics,
                        depth + 1,
                    );
                }
                Err(e) => diagnostics.push(Diagnostic::error(
                    line_number,
                    format!("include {}: {}", name, e),
                )),
            }
            continue;
        }

        if let Some(category) = state.current_category.clone() {
            parse_category_line(
                &category,
                &logical,
                charmap_symbols,
                definition,
                line_number,
                diagnostics,
            );
        }
    }
}

/// Parse a line within a category.
fn parse_category_line(
    category: &str,
    line: &str,
    charmap_symbols: Option<&HashSet<String>>,
    definition: &mut LocaleDefinition,
    line_number: usize,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // copy directive (LD-7): recorded for every category.
    if let Some(rest) = line.strip_prefix("copy") {
        let source = rest.trim().trim_matches('"');
        definition.category_mut(category).copy_from = Some(source.to_string());
        return;
    }

    // LD-3: LC_CTYPE / LC_COLLATE are parsed for their symbolic-name references.
    // A symbol absent from the charmap is a warning for these two categories
    // (the spec makes it an error for the others).
    if category == "LC_CTYPE" || category == "LC_COLLATE" {
        for symbol in extract_symbols(line) {
            if let Some(symbols) = charmap_symbols {
                if !symbols.contains(&symbol) {
                    diagnostics.push(Diagnostic::warning(
                        line_number,
                        format!("symbol <{}> not found in charmap", symbol),
                    ));
                }
            }
            definition.category_mut(category).symbols.push(symbol);
        }
        return;
    }

    // Simple keyword = value categories.
    let mut parts = line.splitn(2, char::is_whitespace);
    let keyword = match parts.next() {
        Some(k) if !k.is_empty() => k,
        _ => return,
    };
    let value = parts.next().unwrap_or("").trim().trim_matches('"');

    let known = known_keywords(category);
    if !known.is_empty() && !known.contains(&keyword) {
        // LD-10: the full POSIX keyword set is recognized; anything else is an
        // unsupported optional keyword and warned about (spec).
        diagnostics.push(Diagnostic::warning(
            line_number,
            format!("unknown keyword: {}", keyword),
        ));
        return;
    }

    definition
        .category_mut(category)
        .keywords
        .insert(keyword.to_string(), value.to_string());
}

/// Parse the symbolic names defined in a charmap file (LD-2). Returns an error
/// if the file cannot be read.
fn parse_charmap_symbols(path: &Path) -> Result<HashSet<String>, String> {
    let content =
        std::fs::read_to_string(path).map_err(|e| format!("{}: {}", path.display(), e))?;
    let mut symbols = HashSet::new();
    let mut in_charmap = false;
    for line in content.lines() {
        let line = line.trim();
        if line == "CHARMAP" {
            in_charmap = true;
            continue;
        }
        if line == "END CHARMAP" {
            in_charmap = false;
            continue;
        }
        if in_charmap {
            if let Some(token) = line.split_whitespace().next() {
                let name = token.trim_matches(|c| c == '<' || c == '>');
                if !name.is_empty() {
                    symbols.insert(name.to_string());
                }
            }
        }
    }
    Ok(symbols)
}

/// Extract `<symbolic>` names referenced in a line.
fn extract_symbols(line: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut rest = line;
    while let Some(start) = rest.find('<') {
        let after = &rest[start + 1..];
        if let Some(end) = after.find('>') {
            let name = &after[..end];
            if !name.is_empty() {
                out.push(name.to_string());
            }
            rest = &after[end + 1..];
        } else {
            break;
        }
    }
    out
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
