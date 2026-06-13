//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! msgfmt - compile message catalog to binary format
//!
//! The msgfmt utility compiles portable message object (.po) files
//! into machine object (.mo) files for use by gettext functions.

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use posixutils_i18n::gettext_lib::mo_file::MO_MAGIC_LE;
use posixutils_i18n::gettext_lib::po_file::PoFile;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufReader, Write};
use std::path::PathBuf;
use std::process::exit;

/// msgfmt - compile message catalog to binary format
#[derive(Parser)]
#[command(
    version,
    about = gettext("msgfmt - compile message catalog to binary format"),
    disable_help_flag = true,
    disable_version_flag = true
)]
struct Args {
    #[arg(short = 'c', help = gettext("Check the PO file for validity"))]
    check: bool,

    #[arg(short = 'f', help = gettext("Include fuzzy entries in the output"))]
    include_fuzzy: bool,

    #[arg(short = 'S', help = gettext("Append .mo suffix to output file names"))]
    add_suffix: bool,

    #[arg(short = 'v', help = gettext("Verbose mode - print warnings"))]
    verbose: bool,

    #[arg(short = 'D', action = clap::ArgAction::Append, help = gettext("Add directory to search path for input files"))]
    directories: Vec<PathBuf>,

    #[arg(short = 'o', long = "output-file", help = gettext("Output file name"))]
    output: Option<PathBuf>,

    #[arg(short, long, action = clap::ArgAction::HelpLong, help = gettext("Print help"))]
    help: Option<bool>,

    #[arg(short = 'V', long, action = clap::ArgAction::Version, help = gettext("Print version"))]
    version: Option<bool>,

    #[arg(help = gettext("Input .po files"))]
    files: Vec<PathBuf>,
}

/// Warning or error from processing
#[derive(Debug)]
struct Diagnostic {
    file: String,
    line: Option<usize>,
    message: String,
    is_error: bool,
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:", self.file)?;
        if let Some(line) = self.line {
            write!(f, "{}:", line)?;
        }
        write!(
            f,
            " {}: {}",
            if self.is_error { "error" } else { "warning" },
            self.message
        )
    }
}

fn main() {
    // Set up localization
    setlocale(LocaleCategory::LcAll, "");
    if textdomain("posixutils-rs").is_err() {
        // Ignore error - translation may not be available
    }
    let _ = bind_textdomain_codeset("posixutils-rs", "UTF-8");

    let args = Args::parse();

    // POSIX: at least one pathname operand is required, but report it as a
    // usage diagnostic rather than a clap argument error.
    if args.files.is_empty() {
        eprintln!("{}", gettext("msgfmt: no input file given"));
        exit(1);
    }

    // The spec's checks apply only when both -c and -v are given; with just one
    // of them the behavior is unspecified, so we run no abnormality checks.
    let run_checks = args.check && args.verbose;

    let mut exit_code = 0;
    // Messages accumulated per output domain. The default domain is "messages".
    let mut domains: HashMap<String, HashMap<String, String>> = HashMap::new();
    // Domains in first-seen order, for stable output.
    let mut domain_order: Vec<String> = Vec::new();
    let mut diagnostics: Vec<Diagnostic> = Vec::new();

    // Statistics for -v output (header entries excluded).
    let mut n_translated = 0usize;
    let mut n_untranslated = 0usize;
    let mut n_fuzzy = 0usize;

    // Process each input file
    for input_path in &args.files {
        let path = find_input_file(input_path, &args.directories);

        let file = match File::open(&path) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("msgfmt: {}: {}", path.display(), e);
                exit_code = 1;
                continue;
            }
        };

        let reader = BufReader::new(file);
        let po = match PoFile::parse_from(reader) {
            Ok(po) => po,
            Err(e) => {
                eprintln!("msgfmt: {}: {}", path.display(), e);
                exit_code = 1;
                continue;
            }
        };

        // Process entries (headers are tagged per domain and flow through here
        // as the empty-msgid entry).
        for entry in po.all_entries() {
            // Tally translation statistics (excluding the header entry).
            if !entry.is_header() {
                if entry.is_fuzzy {
                    n_fuzzy += 1;
                } else if entry.msgstr.iter().all(|s| s.is_empty()) {
                    n_untranslated += 1;
                } else {
                    n_translated += 1;
                }
            }

            // Skip fuzzy entries unless -f is specified
            if entry.is_fuzzy && !args.include_fuzzy {
                if args.verbose {
                    diagnostics.push(Diagnostic {
                        file: path.display().to_string(),
                        line: None,
                        message: format!("skipping fuzzy entry: {}", truncate(&entry.msgid, 30)),
                        is_error: false,
                    });
                }
                continue;
            }

            // Skip obsolete entries
            if entry.is_obsolete {
                continue;
            }

            // Abnormality checks (only when both -c and -v are given).
            if run_checks {
                validate_entry(&path, entry, &mut diagnostics);
            }

            let domain = entry.domain.clone().unwrap_or_else(default_domain);
            if !domains.contains_key(&domain) {
                domain_order.push(domain.clone());
            }
            let messages = domains.entry(domain).or_default();

            // Add to messages
            if entry.is_plural() {
                // Plural message: key is "msgid\0msgid_plural"
                let key = format!(
                    "{}\0{}",
                    entry.msgid,
                    entry.msgid_plural.as_ref().unwrap_or(&String::new())
                );
                // Value is null-separated plural forms
                let value = entry.msgstr.join("\0");
                messages.insert(key, value);
            } else if !entry.msgstr.is_empty() {
                messages.insert(entry.msgid.clone(), entry.msgstr[0].clone());
            }
        }
    }

    // Print diagnostics
    let has_errors = diagnostics.iter().any(|d| d.is_error);
    for diag in &diagnostics {
        if diag.is_error || args.verbose {
            eprintln!("{}", diag);
        }
    }

    if has_errors {
        exit_code = 1;
    }

    // -v: print translation statistics.
    if args.verbose {
        print_statistics(n_translated, n_fuzzy, n_untranslated);
    }

    // Generate output
    if exit_code == 0 || !args.check {
        if let Some(ref output) = args.output {
            // -o: all `domain` directives are ignored; everything is written to
            // the single named output file.
            let mut merged: HashMap<String, String> = HashMap::new();
            for domain in &domain_order {
                if let Some(messages) = domains.get(domain) {
                    for (k, v) in messages {
                        merged.insert(k.clone(), v.clone());
                    }
                }
            }
            let output_path = apply_suffix(output.clone(), args.add_suffix);
            if let Err(e) = write_mo_file(&output_path, &merged) {
                eprintln!("msgfmt: {}: {}", output_path.display(), e);
                exit_code = 1;
            }
        } else {
            // One messages object file per domain, named after the domain.
            for domain in &domain_order {
                let messages = &domains[domain];
                let output_path = domain_output_path(domain, args.add_suffix);
                if let Err(e) = write_mo_file(&output_path, messages) {
                    eprintln!("msgfmt: {}: {}", output_path.display(), e);
                    exit_code = 1;
                }
            }
        }
    }

    exit(exit_code);
}

/// The default domain name when no `domain` directive applies.
fn default_domain() -> String {
    "messages".to_string()
}

/// Output path for a domain when `-o` is not given. POSIX leaves the choice of
/// `domainname` vs `domainname.mo` implementation-defined when `-S` is absent;
/// we use the bare domain name so that `-S` has an observable effect (it appends
/// the `.mo` suffix).
fn domain_output_path(domain: &str, add_suffix: bool) -> PathBuf {
    apply_suffix(PathBuf::from(domain), add_suffix)
}

/// Append the `.mo` suffix to `path` when `-S` is set and it does not already
/// end in `.mo`.
fn apply_suffix(path: PathBuf, add_suffix: bool) -> PathBuf {
    if !add_suffix {
        return path;
    }
    let s = path.to_string_lossy();
    if s.ends_with(".mo") {
        path
    } else {
        PathBuf::from(format!("{}.mo", s))
    }
}

/// Find an input file, searching directories if needed
fn find_input_file(path: &PathBuf, directories: &[PathBuf]) -> PathBuf {
    if path.exists() {
        return path.clone();
    }

    for dir in directories {
        let full_path = dir.join(path);
        if full_path.exists() {
            return full_path;
        }
    }

    path.clone()
}

/// Does the `c-format`/`no-c-format` flag set make this entry c-format? The
/// last of the two flags to appear wins (POSIX/GNU semantics).
fn is_c_format(flags: &[String]) -> bool {
    let mut active = false;
    for flag in flags {
        match flag.as_str() {
            "c-format" => active = true,
            "no-c-format" => active = false,
            _ => {}
        }
    }
    active
}

/// True if exactly one of `a`/`b` starts with a newline, or exactly one ends
/// with a newline (the abnormality the spec describes).
fn boundary_newline_mismatch(a: &str, b: &str) -> bool {
    (a.starts_with('\n') != b.starts_with('\n')) || (a.ends_with('\n') != b.ends_with('\n'))
}

/// Validate a PO entry, recording genuine abnormalities as errors (affecting the
/// exit status) and softer findings as warnings. Only called when both -c and
/// -v are given.
fn validate_entry(
    path: &std::path::Path,
    entry: &posixutils_i18n::gettext_lib::po_file::PoEntry,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let file = path.display().to_string();
    if entry.is_header() {
        return;
    }

    let is_plural = entry.is_plural();
    let c_format = is_c_format(&entry.flags);

    for (i, msgstr) in entry.msgstr.iter().enumerate() {
        if msgstr.is_empty() {
            continue;
        }
        // The source string corresponding to this msgstr: the plural form is
        // compared against msgid_plural, the singular against msgid.
        let source = if is_plural && i > 0 {
            entry.msgid_plural.as_deref().unwrap_or(&entry.msgid)
        } else {
            &entry.msgid
        };
        let suffix = if entry.msgstr.len() > 1 {
            format!("[{}]", i)
        } else {
            String::new()
        };

        // Abnormality: boundary <newline> mismatch.
        if boundary_newline_mismatch(source, msgstr) {
            diagnostics.push(Diagnostic {
                file: file.clone(),
                line: None,
                message: format!(
                    "'msgid' and 'msgstr{}' do not both begin/end with '\\n'",
                    suffix
                ),
                is_error: true,
            });
        }

        // Abnormality: c-format conversion specifiers differ in number or type.
        if c_format {
            let src_specs = format_signatures(source);
            let dst_specs = format_signatures(msgstr);
            if src_specs != dst_specs {
                diagnostics.push(Diagnostic {
                    file: file.clone(),
                    line: None,
                    message: format!(
                        "format specifications in 'msgid' and 'msgstr{}' differ",
                        suffix
                    ),
                    is_error: true,
                });
            }
        }
    }

    // Softer finding: empty translation of a non-empty source (informational).
    if !entry.msgid.is_empty() && entry.msgstr.iter().all(|s| s.is_empty()) {
        diagnostics.push(Diagnostic {
            file,
            line: None,
            message: format!("empty msgstr for: {}", truncate(&entry.msgid, 30)),
            is_error: false,
        });
    }
}

/// Normalized signatures of the printf-style conversion specifications in `s`,
/// in order. Each signature is the length modifier plus an argument-type class,
/// so the comparison catches both a differing count and differing argument
/// types (`%d` vs `%s`), while treating equivalents like `%d`/`%i` as the same.
fn format_signatures(s: &str) -> Vec<String> {
    let mut sigs = Vec::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c != '%' {
            continue;
        }
        match chars.peek() {
            Some('%') => {
                chars.next(); // literal %%
                continue;
            }
            None => break,
            _ => {}
        }

        // Flags, field width, precision, positional ($) — ignored for typing.
        while let Some(&c) = chars.peek() {
            if "-+ #0".contains(c) || c.is_ascii_digit() || c == '.' || c == '*' || c == '$' {
                chars.next();
            } else {
                break;
            }
        }
        // Length modifiers affect the argument type, so keep them.
        let mut length = String::new();
        while let Some(&c) = chars.peek() {
            if "hlLjztq".contains(c) {
                length.push(c);
                chars.next();
            } else {
                break;
            }
        }
        // Conversion character.
        if let Some(conv) = chars.next() {
            sigs.push(format!("{}{}", length, conversion_class(conv)));
        }
    }

    sigs
}

/// Map a printf conversion character to an argument-type class.
fn conversion_class(c: char) -> char {
    match c {
        'd' | 'i' => 'i',
        'o' | 'u' | 'x' | 'X' => 'u',
        'e' | 'E' | 'f' | 'F' | 'g' | 'G' | 'a' | 'A' => 'f',
        'c' => 'c',
        's' => 's',
        'p' => 'p',
        'n' => 'n',
        other => other,
    }
}

/// Print `-v` translation statistics to standard error.
fn print_statistics(translated: usize, fuzzy: usize, untranslated: usize) {
    let mut parts = vec![format!("{} translated messages", translated)];
    if fuzzy > 0 {
        parts.push(format!("{} fuzzy translations", fuzzy));
    }
    if untranslated > 0 {
        parts.push(format!("{} untranslated messages", untranslated));
    }
    eprintln!("{}.", parts.join(", "));
}

/// Truncate a string for display, respecting character boundaries.
fn truncate(s: &str, max_len: usize) -> String {
    if s.chars().count() <= max_len {
        s.to_string()
    } else {
        let truncated: String = s.chars().take(max_len).collect();
        format!("{}...", truncated)
    }
}

/// Write the .mo file
fn write_mo_file(
    path: &PathBuf,
    messages: &HashMap<String, String>,
) -> Result<(), Box<dyn std::error::Error>> {
    // Sort messages (empty string first, then lexicographically)
    let mut entries: Vec<(&str, &str)> = messages
        .iter()
        .map(|(k, v)| (k.as_str(), v.as_str()))
        .collect();
    entries.sort_by(|a, b| a.0.cmp(b.0));

    let nstrings = entries.len() as u32;

    // Calculate offsets
    let header_size = 28u32; // 7 * 4 bytes
    let orig_tab_offset = header_size;
    let trans_tab_offset = orig_tab_offset + nstrings * 8; // 8 bytes per descriptor
    let strings_offset = trans_tab_offset + nstrings * 8;

    // Build string data and descriptors
    let mut orig_descriptors: Vec<(u32, u32)> = Vec::new(); // (length, offset)
    let mut trans_descriptors: Vec<(u32, u32)> = Vec::new();
    let mut string_data: Vec<u8> = Vec::new();

    for (msgid, msgstr) in &entries {
        // Original string
        let orig_offset = strings_offset + string_data.len() as u32;
        let orig_len = msgid.len() as u32;
        orig_descriptors.push((orig_len, orig_offset));
        string_data.extend_from_slice(msgid.as_bytes());
        string_data.push(0); // null terminator

        // Translation string
        let trans_offset = strings_offset + string_data.len() as u32;
        let trans_len = msgstr.len() as u32;
        trans_descriptors.push((trans_len, trans_offset));
        string_data.extend_from_slice(msgstr.as_bytes());
        string_data.push(0); // null terminator
    }

    // Write the file
    let mut file = File::create(path)?;

    // Header (little-endian)
    file.write_all(&MO_MAGIC_LE.to_le_bytes())?; // magic
    file.write_all(&0u32.to_le_bytes())?; // revision
    file.write_all(&nstrings.to_le_bytes())?; // nstrings
    file.write_all(&orig_tab_offset.to_le_bytes())?; // orig_tab_offset
    file.write_all(&trans_tab_offset.to_le_bytes())?; // trans_tab_offset
    file.write_all(&0u32.to_le_bytes())?; // hash_tab_size
    file.write_all(&0u32.to_le_bytes())?; // hash_tab_offset

    // Original string descriptors
    for (len, offset) in &orig_descriptors {
        file.write_all(&len.to_le_bytes())?;
        file.write_all(&offset.to_le_bytes())?;
    }

    // Translation string descriptors
    for (len, offset) in &trans_descriptors {
        file.write_all(&len.to_le_bytes())?;
        file.write_all(&offset.to_le_bytes())?;
    }

    // String data
    file.write_all(&string_data)?;

    Ok(())
}
