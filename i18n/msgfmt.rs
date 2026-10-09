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
use gettextrs::gettext;
use posixutils_i18n::gettext_lib::mo_file::MO_MAGIC_LE;
use posixutils_i18n::gettext_lib::plural::parse_plural_forms;
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

    #[arg(long, help = gettext("Check that c-format translations use the same conversions as the original"))]
    check_format: bool,

    #[arg(long, help = gettext("Reject domain directives when an output file is named"))]
    check_domain: bool,

    #[arg(long, help = gettext("Print translation statistics to standard error"))]
    statistics: bool,

    #[arg(short = 'f', help = gettext("Include fuzzy entries in the output"))]
    include_fuzzy: bool,

    #[arg(short = 'S', help = gettext("Append .mo suffix to output file names"))]
    add_suffix: bool,

    #[arg(short = 'v', long, help = gettext("Verbose mode - print warnings"))]
    verbose: bool,

    #[arg(short = 'D', allow_hyphen_values = true, action = clap::ArgAction::Append, help = gettext("Add directory to search path for input files"))]
    directories: Vec<PathBuf>,

    #[arg(short = 'o', long = "output-file", allow_hyphen_values = true, help = gettext("Output file name"))]
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
    plib::diag::init_locale("msgfmt");

    let args = plib::optarg::parse::<Args>();

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
    let mut domains: HashMap<String, HashMap<Vec<u8>, Vec<u8>>> = HashMap::new();
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
                eprintln!(
                    "msgfmt: {}: {}",
                    path.display(),
                    plib::diag::io_error_text(&e)
                );
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

        // Which plural forms stand for many numbers, read from the header.
        let often = plural_forms_used_often(&po);
        // Domains of this file already reported under --check-domain.
        let mut ignored_domains = std::collections::HashSet::new();
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

            // Abnormality checks (all of them when both -c and -v are given).
            if run_checks || args.check_format {
                validate_entry(&path, entry, run_checks, often.as_deref(), &mut diagnostics);
            }

            // --check-domain: -o ignores every `domain` directive, so one is a
            // conflict (reported once per domain and file).
            if let (true, Some(_), Some(name)) = (args.check_domain, &args.output, &entry.domain) {
                if ignored_domains.insert(name.clone()) {
                    diagnostics.push(Diagnostic {
                        file: path.display().to_string(),
                        line: None,
                        message: format!("'domain {}' directive ignored", name),
                        is_error: true,
                    });
                }
            }

            let domain = entry.domain.clone().unwrap_or_else(default_domain);
            if !domains.contains_key(&domain) {
                domain_order.push(domain.clone());
            }
            let messages = domains.entry(domain).or_default();

            // Add to messages
            if entry.is_plural() {
                // Plural message: key is "msgid\0msgid_plural"
                let mut key = entry.msgid.clone();
                key.push(0);
                if let Some(plural) = entry.msgid_plural.as_ref() {
                    key.extend_from_slice(plural);
                }
                // Value is null-separated plural forms
                let value = entry.msgstr.join(&0u8);
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

    // -v or --statistics: print translation statistics.
    if args.verbose || args.statistics {
        print_statistics(n_translated, n_fuzzy, n_untranslated);
    }

    // Generate output
    if exit_code == 0 || !args.check {
        if let Some(ref output) = args.output {
            // -o: all `domain` directives are ignored; everything is written to
            // the single named output file.
            let mut merged: HashMap<Vec<u8>, Vec<u8>> = HashMap::new();
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
    // Pathnames are arbitrary bytes on Unix; operate on the raw OsString so a
    // non-UTF-8 path is preserved exactly rather than mangled by lossy decoding.
    use std::os::unix::ffi::OsStrExt;
    if path.as_os_str().as_bytes().ends_with(b".mo") {
        return path;
    }
    let mut os = path.into_os_string();
    os.push(".mo");
    PathBuf::from(os)
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
fn boundary_newline_mismatch(a: &[u8], b: &[u8]) -> bool {
    (a.first() == Some(&b'\n')) != (b.first() == Some(&b'\n'))
        || (a.last() == Some(&b'\n')) != (b.last() == Some(&b'\n'))
}

/// For each plural form of the file's `Plural-Forms` expression, whether it
/// stands for many numbers: five or more of n = 0..=1000, as GNU msgfmt
/// counts.  `None` when the header gives no usable expression.
fn plural_forms_used_often(po: &PoFile) -> Option<Vec<bool>> {
    let (nplurals, expr) = parse_plural_forms(&po.plural_forms()?)?;
    let mut counts = vec![0usize; nplurals];
    for n in 0..=1000 {
        if let Some(count) = usize::try_from(expr.evaluate(n))
            .ok()
            .and_then(|form| counts.get_mut(form))
        {
            *count += 1;
        }
    }
    Some(counts.into_iter().map(|c| c >= 5).collect())
}

/// Validate a PO entry, recording genuine abnormalities as errors (affecting the
/// exit status) and softer findings as warnings. With `all` (-c -v) every check
/// runs; without it (`--check-format`) only the c-format comparison.
///
/// `often` says which plural forms stand for many numbers (see
/// [`plural_forms_used_often`]).
fn validate_entry(
    path: &std::path::Path,
    entry: &posixutils_i18n::gettext_lib::po_file::PoEntry,
    all: bool,
    often: Option<&[bool]>,
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
        // The original whose boundary newlines this msgstr must share: the
        // plural forms share msgid_plural's, the singular msgid's.
        let source = if is_plural && i > 0 {
            entry.msgid_plural.as_deref().unwrap_or(&entry.msgid)
        } else {
            &entry.msgid
        };
        let suffix = if is_plural {
            format!("[{}]", i)
        } else {
            String::new()
        };
        let line = Some(entry.msgstr_line).filter(|&n| n > 0);

        // Abnormality: boundary <newline> mismatch.
        if all && boundary_newline_mismatch(source, msgstr) {
            diagnostics.push(Diagnostic {
                file: file.clone(),
                line,
                message: format!(
                    "'msgid' and 'msgstr{}' do not both begin/end with '\\n'",
                    suffix
                ),
                is_error: true,
            });
        }

        // Abnormality: c-format conversion specifiers differ in number or type.
        // As in GNU msgfmt, every plural form is checked against
        // msgid_plural, and a form that stands for few numbers (the singular
        // of most languages) may leave out trailing arguments: "one file"
        // for "%d files".
        if c_format {
            let (original, strict) = match &entry.msgid_plural {
                Some(plural) => (
                    ("msgid_plural", plural.as_slice()),
                    often.is_none_or(|o| o.get(i).copied().unwrap_or(true)),
                ),
                None => (("msgid", entry.msgid.as_slice()), true),
            };
            if let Some(problem) = format_mismatch(original, msgstr, &suffix, strict) {
                diagnostics.push(Diagnostic {
                    file: file.clone(),
                    line,
                    message: format!("{problem}: msgid \"{}\"", truncate(&entry.msgid, 60)),
                    is_error: true,
                });
            }
        }
    }

    // Softer finding: empty translation of a non-empty source (informational).
    if all && !entry.msgid.is_empty() && entry.msgstr.iter().all(|s| s.is_empty()) {
        diagnostics.push(Diagnostic {
            file,
            line: None,
            message: format!("empty msgstr for: {}", truncate(&entry.msgid, 30)),
            is_error: false,
        });
    }
}

/// How the conversion specifications of `msgstr` fail to match those of
/// `original`, named and given as its text, or `None` when they match.
///
/// POSIX has `msgfmt -c -v` compare only the number of conversions and the
/// argument types of corresponding ones. The comparison is by argument, so a
/// flag or a width does not count, and a `%n$` conversion is matched by its
/// argument number wherever it stands. Unless `strict`, `msgstr` may consume
/// fewer arguments than `original`. An `original` that is no valid format
/// string has nothing to compare against. The wording is GNU msgfmt's.
fn format_mismatch(
    (name, original): (&str, &[u8]),
    msgstr: &[u8],
    suffix: &str,
    strict: bool,
) -> Option<String> {
    let Ok(expected) = format_arguments(original) else {
        return None;
    };
    let found = match format_arguments(msgstr) {
        Ok(found) => found,
        Err(reason) => {
            return Some(format!(
                "'msgstr{suffix}' is not a valid C format string, unlike '{name}'. Reason: {reason}"
            ))
        }
    };
    if found.len() > expected.len() || (strict && found.len() < expected.len()) {
        return Some(format!(
            "number of format specifications in '{name}' and 'msgstr{suffix}' does not match"
        ));
    }
    let n = expected.iter().zip(&found).position(|(a, b)| a != b)?;
    Some(format!(
        "format specifications in '{name}' and 'msgstr{suffix}' for argument {} are not the same",
        n + 1
    ))
}

/// The argument types the conversion specifications of `s` consume, in
/// argument order, or why `s` is no valid C format string.
///
/// Each type is the length modifier plus an argument class, so `%d`/`%i`
/// agree and `%d`/`%ld` do not. A `*` width or precision consumes an `int`.
fn format_arguments(s: &[u8]) -> Result<Vec<String>, String> {
    // (argument number when given as `n$`, type), in textual order.
    let mut uses: Vec<(Option<usize>, String)> = Vec::new();
    let mut i = 0;
    while i < s.len() {
        if s[i] != b'%' {
            i += 1;
            continue;
        }
        i += 1;
        if s.get(i) == Some(&b'%') {
            i += 1;
            continue;
        }
        let number = argument_number(s, &mut i);
        while i < s.len() && b"-+ #0'I".contains(&s[i]) {
            i += 1;
        }
        // Field width, then precision: digits, or `*` with its own `n$`.
        for leading in [None, Some(b'.')] {
            if let Some(dot) = leading {
                if s.get(i) != Some(&dot) {
                    continue;
                }
                i += 1;
            }
            if s.get(i) == Some(&b'*') {
                i += 1;
                uses.push((argument_number(s, &mut i), "i".to_string()));
            } else {
                while i < s.len() && s[i].is_ascii_digit() {
                    i += 1;
                }
            }
        }
        let mut length = String::new();
        while i < s.len() && b"hlLjztq".contains(&s[i]) {
            length.push(char::from(s[i]));
            i += 1;
        }
        let Some(&conversion) = s.get(i) else {
            return Err("The string ends in the middle of a directive.".to_string());
        };
        i += 1;
        uses.push((number, format!("{length}{}", conversion_class(conversion))));
    }

    let numbered = uses.iter().filter(|(n, _)| n.is_some()).count();
    if numbered == 0 {
        return Ok(uses.into_iter().map(|(_, t)| t).collect());
    }
    if numbered != uses.len() {
        return Err(
            "The string refers to arguments both through absolute argument numbers \
                    and through unnumbered argument specifications."
                .to_string(),
        );
    }
    let max = uses.iter().filter_map(|(n, _)| *n).max().unwrap_or(0);
    let mut types: Vec<Option<String>> = vec![None; max];
    for (n, t) in uses {
        let slot = &mut types[n.unwrap_or(1) - 1];
        match slot {
            Some(seen) if *seen != t => {
                return Err(format!(
                    "The string refers to argument number {} in incompatible ways.",
                    n.unwrap_or(1)
                ))
            }
            _ => *slot = Some(t),
        }
    }
    types
        .into_iter()
        .enumerate()
        .map(|(k, t)| {
            t.ok_or_else(|| {
                format!(
                    "The string refers to argument number {max} but ignores argument number {}.",
                    k + 1
                )
            })
        })
        .collect()
}

/// Read an `n$` argument number at `s[*i..]`, stepping past it; `None`, and
/// `*i` left alone, when there is none. Argument numbers start at 1.
fn argument_number(s: &[u8], i: &mut usize) -> Option<usize> {
    let digits = s[*i..].iter().take_while(|b| b.is_ascii_digit()).count();
    if digits == 0 || s.get(*i + digits) != Some(&b'$') {
        return None;
    }
    let n: usize = std::str::from_utf8(&s[*i..*i + digits])
        .ok()?
        .parse()
        .ok()?;
    if n == 0 {
        return None;
    }
    *i += digits + 1;
    Some(n)
}

/// Map a printf conversion character to an argument-type class.
fn conversion_class(c: u8) -> char {
    match c {
        b'd' | b'i' => 'i',
        b'o' | b'u' | b'x' | b'X' => 'u',
        b'e' | b'E' | b'f' | b'F' | b'g' | b'G' | b'a' | b'A' => 'f',
        other => char::from(other),
    }
}

/// Print `-v` / `--statistics` translation statistics to standard error, in
/// GNU msgfmt's wording.
fn print_statistics(translated: usize, fuzzy: usize, untranslated: usize) {
    let mut parts = vec![count_phrase(translated, "translated message")];
    if fuzzy > 0 {
        parts.push(count_phrase(fuzzy, "fuzzy translation"));
    }
    if untranslated > 0 {
        parts.push(count_phrase(untranslated, "untranslated message"));
    }
    eprintln!("{}.", parts.join(", "));
}

/// "1 fuzzy translation", "2 fuzzy translations".
fn count_phrase(n: usize, noun: &str) -> String {
    let plural = if n == 1 { "" } else { "s" };
    format!("{} {}{}", n, noun, plural)
}

/// Truncate a string for display, respecting character boundaries.
fn truncate(s: &[u8], max_len: usize) -> String {
    // A diagnostic is text, so the message bytes are decoded lossily here even
    // though they are carried through to the `.mo` verbatim.
    let s = String::from_utf8_lossy(s);
    if s.chars().count() <= max_len {
        s.into_owned()
    } else {
        let truncated: String = s.chars().take(max_len).collect();
        format!("{}...", truncated)
    }
}

/// Write the .mo file
fn write_mo_file(
    path: &PathBuf,
    messages: &HashMap<Vec<u8>, Vec<u8>>,
) -> Result<(), Box<dyn std::error::Error>> {
    // Sort messages (empty string first, then lexicographically)
    let mut entries: Vec<(&[u8], &[u8])> = messages
        .iter()
        .map(|(k, v)| (k.as_slice(), v.as_slice()))
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
        string_data.extend_from_slice(msgid);
        string_data.push(0); // null terminator

        // Translation string
        let trans_offset = strings_offset + string_data.len() as u32;
        let trans_len = msgstr.len() as u32;
        trans_descriptors.push((trans_len, trans_offset));
        string_data.extend_from_slice(msgstr);
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
