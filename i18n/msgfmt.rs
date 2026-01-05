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
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
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

    #[arg(required = true, help = gettext("Input .po files"))]
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

    let mut exit_code = 0;
    let mut all_messages: HashMap<String, String> = HashMap::new();
    let mut header_entry: Option<String> = None;
    let mut diagnostics: Vec<Diagnostic> = Vec::new();

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

        // Process header
        if let Some(ref hdr) = po.header {
            if !hdr.msgstr.is_empty() {
                header_entry = Some(hdr.msgstr[0].clone());
            }
        }

        // Process entries
        for entry in po.all_entries() {
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

            // Validation checks (-c)
            if args.check {
                validate_entry(&path, entry, &mut diagnostics);
            }

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
                all_messages.insert(key, value);
            } else if !entry.msgstr.is_empty() {
                all_messages.insert(entry.msgid.clone(), entry.msgstr[0].clone());
            }
        }
    }

    // Add header entry
    if let Some(header) = header_entry {
        all_messages.insert(String::new(), header);
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

    // Generate output
    if exit_code == 0 || !args.check {
        let output_path = get_output_path(&args);

        if let Err(e) = write_mo_file(&output_path, &all_messages) {
            eprintln!("msgfmt: {}: {}", output_path.display(), e);
            exit_code = 1;
        }
    }

    exit(exit_code);
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

/// Get the output file path
fn get_output_path(args: &Args) -> PathBuf {
    if let Some(ref output) = args.output {
        return output.clone();
    }

    // Derive from first input file
    let mut output = args.files[0].clone();
    output.set_extension("mo");

    if args.add_suffix && !output.to_string_lossy().ends_with(".mo") {
        let name = output.to_string_lossy().to_string() + ".mo";
        output = PathBuf::from(name);
    }

    output
}

/// Validate a PO entry
fn validate_entry(
    path: &std::path::Path,
    entry: &posixutils_i18n::gettext_lib::po_file::PoEntry,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let file = path.display().to_string();

    // Check for mismatched newlines
    let msgid_newlines = entry.msgid.matches('\n').count();
    for (i, msgstr) in entry.msgstr.iter().enumerate() {
        let msgstr_newlines = msgstr.matches('\n').count();
        if msgid_newlines != msgstr_newlines && !entry.msgid.is_empty() && !msgstr.is_empty() {
            diagnostics.push(Diagnostic {
                file: file.clone(),
                line: None,
                message: format!(
                    "msgid and msgstr{} have different newline counts ({} vs {})",
                    if entry.msgstr.len() > 1 {
                        format!("[{}]", i)
                    } else {
                        String::new()
                    },
                    msgid_newlines,
                    msgstr_newlines
                ),
                is_error: false,
            });
        }
    }

    // Check for c-format consistency
    if entry.flags.iter().any(|f| f == "c-format") {
        let msgid_formats = count_format_specs(&entry.msgid);
        for (i, msgstr) in entry.msgstr.iter().enumerate() {
            let msgstr_formats = count_format_specs(msgstr);
            if msgid_formats != msgstr_formats && !msgstr.is_empty() {
                diagnostics.push(Diagnostic {
                    file: file.clone(),
                    line: None,
                    message: format!(
                        "format specifications in msgid and msgstr{} differ",
                        if entry.msgstr.len() > 1 {
                            format!("[{}]", i)
                        } else {
                            String::new()
                        }
                    ),
                    is_error: false,
                });
            }
        }
    }

    // Check for empty translation of non-empty source
    if !entry.msgid.is_empty() && entry.msgstr.iter().all(|s| s.is_empty()) {
        diagnostics.push(Diagnostic {
            file: file.clone(),
            line: None,
            message: format!("empty msgstr for: {}", truncate(&entry.msgid, 30)),
            is_error: false,
        });
    }
}

/// Count printf-style format specifications
fn count_format_specs(s: &str) -> usize {
    let mut count = 0;
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '%' {
            match chars.peek() {
                Some('%') => {
                    chars.next(); // Skip %%
                }
                Some(_) => {
                    count += 1;
                    // Skip format specification
                    while let Some(&c) = chars.peek() {
                        if c.is_alphabetic() {
                            chars.next();
                            break;
                        }
                        chars.next();
                    }
                }
                None => {}
            }
        }
    }

    count
}

/// Truncate a string for display
fn truncate(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        s.to_string()
    } else {
        format!("{}...", &s[..max_len])
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
