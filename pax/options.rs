//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Format-specific options parsing and handling
//!
//! Implements the `-o options` functionality for pax.
//! Options are specified as comma-separated key=value pairs.
//!
//! Syntax: `-o keyword[[:]=value][,keyword[[:]=value],...]`
//!
//! The `:=` form is used for per-file options (pax format),
//! while `=` is used for global options.

use crate::archive::EntryType;
use crate::error::{PaxError, PaxResult};
use crate::pattern::Pattern;
use std::collections::HashMap;

/// Information about an archive entry for list formatting
#[derive(Debug, Clone)]
pub struct ListEntryInfo<'a> {
    pub path: &'a str,
    pub mode: u32,
    pub size: u64,
    pub mtime: u64,
    pub uid: u32,
    pub gid: u32,
    pub uname: Option<&'a str>,
    pub gname: Option<&'a str>,
    pub link_target: Option<&'a str>,
    pub entry_type: EntryType,
    pub devmajor: u32,
    pub devminor: u32,
}

/// Action to take when a filename contains invalid characters
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum InvalidAction {
    /// Skip files with invalid filenames (default in write mode)
    #[default]
    Bypass,
    /// Prompt interactively for new name (reuses -i mechanism)
    Rename,
    /// Write file with translated/sanitized name
    Write,
    /// Use raw UTF-8 encoding without translation
    Utf8,
    /// Generate hdrcharset=BINARY header for non-UTF-8 data
    Binary,
}

/// String to InvalidAction mapping table
const INVALID_ACTION_MAP: &[(&str, InvalidAction)] = &[
    ("bypass", InvalidAction::Bypass),
    ("rename", InvalidAction::Rename),
    ("write", InvalidAction::Write),
    ("UTF-8", InvalidAction::Utf8),
    ("binary", InvalidAction::Binary),
];

impl InvalidAction {
    /// Parse from string value
    pub fn from_str(s: &str) -> Option<Self> {
        INVALID_ACTION_MAP
            .iter()
            .find(|(name, _)| *name == s)
            .map(|(_, action)| *action)
    }
}

/// Parsed format options
#[derive(Debug, Clone, Default)]
pub struct FormatOptions {
    /// Global options (keyword=value)
    global: HashMap<String, String>,
    /// Per-file options (keyword:=value) - for future pax format support
    per_file: HashMap<String, String>,
    /// List format specification (listopt=format)
    pub list_format: Option<String>,
    /// Delete patterns (delete=pattern) - for future pax format support
    pub delete_patterns: Vec<String>,
    /// Pre-compiled delete patterns for efficient matching
    delete_patterns_compiled: Vec<Pattern>,
    /// Times option (include atime/mtime in extended headers)
    pub include_times: bool,
    /// Linkdata option (write contents for hard links)
    pub link_data: bool,
    /// Extended header name template (exthdr.name)
    /// Default: "%d/PaxHeaders.%p/%f"
    pub exthdr_name: Option<String>,
    /// Global extended header name template (globexthdr.name)
    /// Default: "$TMPDIR/GlobalHead.%p.%n"
    pub globexthdr_name: Option<String>,
    /// Action to take for invalid filenames
    pub invalid_action: InvalidAction,
}

/// Known option identifiers for table-driven parsing
#[derive(Clone, Copy)]
enum KnownOption {
    Times,
    LinkData,
    ListFormat,
    ExthdrName,
    GlobexthdrName,
    Delete,
    Invalid,
}

/// Known option keywords and their identifiers
const KNOWN_OPTIONS: &[(&str, KnownOption)] = &[
    ("listopt", KnownOption::ListFormat),
    ("delete", KnownOption::Delete),
    ("times", KnownOption::Times),
    ("linkdata", KnownOption::LinkData),
    ("exthdr.name", KnownOption::ExthdrName),
    ("globexthdr.name", KnownOption::GlobexthdrName),
    ("invalid", KnownOption::Invalid),
];

impl FormatOptions {
    /// Create new empty options
    pub fn new() -> Self {
        FormatOptions::default()
    }

    /// Parse options from a string
    ///
    /// Format: `keyword[[:]=value][,keyword[[:]=value],...]`
    #[cfg(test)]
    pub fn parse(input: &str) -> PaxResult<Self> {
        let mut options = FormatOptions::new();
        options.parse_into(input)?;
        Ok(options)
    }

    /// Parse options and merge into existing options
    ///
    /// Later options take precedence over earlier ones.
    pub fn parse_into(&mut self, input: &str) -> PaxResult<()> {
        let input = input.trim();
        if input.is_empty() {
            return Ok(());
        }

        // Per POSIX, `listopt` is the final <comma>-separated keyword: its value
        // runs to the end of the `-o` string, so commas inside the format are
        // literal. Split it off before the comma tokenizer can break it apart.
        if let Some(marker) = find_listopt_marker(input) {
            let before = input[..marker].trim_end().trim_end_matches(',');
            self.parse_comma_list(before)?;
            let raw = &input[marker + "listopt=".len()..];
            self.list_format = Some(unescape_backslashes(raw));
            return Ok(());
        }

        self.parse_comma_list(input)
    }

    /// Parse a sequence of comma-separated options (backslash escapes a comma).
    fn parse_comma_list(&mut self, input: &str) -> PaxResult<()> {
        let mut current = String::new();
        let mut escaped = false;

        for c in input.chars() {
            if escaped {
                current.push(c);
                escaped = false;
            } else if c == '\\' {
                escaped = true;
            } else if c == ',' {
                self.parse_single_option(current.trim())?;
                current.clear();
            } else {
                current.push(c);
            }
        }

        let final_opt = current.trim();
        if !final_opt.is_empty() {
            self.parse_single_option(final_opt)?;
        }

        Ok(())
    }

    /// Parse a single option (keyword[[:]=value])
    fn parse_single_option(&mut self, opt: &str) -> PaxResult<()> {
        use KnownOption::*;

        if opt.is_empty() {
            return Ok(());
        }

        // Check for := (per-file) or = (global)
        let (keyword, value, is_per_file) = if let Some(pos) = opt.find(":=") {
            let keyword = opt[..pos].trim();
            let value = opt[pos + 2..].trim();
            (keyword, Some(value), true)
        } else if let Some(pos) = opt.find('=') {
            let keyword = opt[..pos].trim();
            let value = opt[pos + 1..].trim();
            (keyword, Some(value), false)
        } else {
            // Boolean keyword with no value
            (opt.trim(), None, false)
        };

        // Look up keyword in known options table
        if let Some((_, known_opt)) = KNOWN_OPTIONS.iter().find(|(k, _)| *k == keyword) {
            match known_opt {
                Times => self.include_times = true,
                LinkData => self.link_data = true,
                ListFormat => self.list_format = value.map(|s| s.to_string()),
                ExthdrName => self.exthdr_name = value.map(|s| s.to_string()),
                GlobexthdrName => self.globexthdr_name = value.map(|s| s.to_string()),
                Delete => {
                    if let Some(pattern) = value {
                        match Pattern::new(pattern) {
                            Ok(compiled) => {
                                self.delete_patterns.push(pattern.to_string());
                                self.delete_patterns_compiled.push(compiled);
                            }
                            Err(e) => {
                                return Err(PaxError::PatternError(format!(
                                    "invalid delete pattern '{}': {}",
                                    pattern, e
                                )));
                            }
                        }
                    }
                }
                Invalid => {
                    if let Some(v) = value {
                        if let Some(action) = InvalidAction::from_str(v) {
                            self.invalid_action = action;
                        } else {
                            return Err(PaxError::InvalidFormat(format!(
                                "invalid value for 'invalid' option: {}",
                                v
                            )));
                        }
                    }
                }
            }
        } else {
            // Store unknown options for format-specific handling
            if is_per_file {
                self.per_file
                    .insert(keyword.to_string(), value.unwrap_or("").to_string());
            } else {
                self.global
                    .insert(keyword.to_string(), value.unwrap_or("").to_string());
            }
        }

        Ok(())
    }

    /// Get a per-file option value
    #[cfg(test)]
    pub fn get_per_file(&self, key: &str) -> Option<&String> {
        self.per_file.get(key)
    }

    /// Merge another options set into this one
    ///
    /// Later options (from `other`) take precedence.
    #[cfg(test)]
    pub fn merge(&mut self, other: &FormatOptions) {
        for (k, v) in &other.global {
            self.global.insert(k.clone(), v.clone());
        }
        for (k, v) in &other.per_file {
            self.per_file.insert(k.clone(), v.clone());
        }
        if other.list_format.is_some() {
            self.list_format.clone_from(&other.list_format);
        }
        self.delete_patterns
            .extend(other.delete_patterns.iter().cloned());
        self.delete_patterns_compiled
            .extend(other.delete_patterns_compiled.iter().cloned());
        if other.include_times {
            self.include_times = true;
        }
        if other.link_data {
            self.link_data = true;
        }
    }

    /// Check if a keyword should be deleted from extended headers
    ///
    /// Returns true if the keyword matches any of the pre-compiled delete patterns
    pub fn should_delete_keyword(&self, keyword: &str) -> bool {
        self.delete_patterns_compiled
            .iter()
            .any(|pattern| pattern.matches(keyword))
    }

    /// Get the global options map for extended header generation
    pub fn global_options(&self) -> &HashMap<String, String> {
        &self.global
    }

    /// Get the per-file options map for extended header generation
    pub fn per_file_options(&self) -> &HashMap<String, String> {
        &self.per_file
    }

    /// Expand the exthdr.name template for a given file path
    ///
    /// Template specifiers:
    /// - `%d` - directory name of the file
    /// - `%f` - filename of the file
    /// - `%p` - process ID of pax
    /// - `%%` - literal percent sign
    ///
    /// Default template: "%d/PaxHeaders.%p/%f"
    pub fn expand_exthdr_name(&self, path: &std::path::Path, sequence: u64) -> String {
        let template = self.exthdr_name.as_deref().unwrap_or("%d/PaxHeaders.%p/%f");

        expand_header_template(template, path, sequence)
    }

    /// Expand the globexthdr.name template for global extended headers
    ///
    /// Template specifiers:
    /// - `%n` - sequence number for global headers
    /// - `%p` - process ID of pax
    /// - `%%` - literal percent sign
    ///
    /// Default template: "$TMPDIR/GlobalHead.%p.%n", with `$TMPDIR` defaulting
    /// to `/tmp` when unset (per POSIX ENVIRONMENT VARIABLES).
    pub fn expand_globexthdr_name(&self, sequence: u64) -> String {
        let tmpdir = std::env::var("TMPDIR").ok();
        let default_template = default_globexthdr_template(tmpdir.as_deref());
        let template = self.globexthdr_name.as_deref().unwrap_or(&default_template);

        expand_global_header_template(template, sequence)
    }
}

/// Build the default `globexthdr.name` template from `$TMPDIR` (or `/tmp`).
fn default_globexthdr_template(tmpdir: Option<&str>) -> String {
    let dir = tmpdir.unwrap_or("/tmp");
    format!("{}/GlobalHead.%p.%n", dir.trim_end_matches('/'))
}

/// Context for template expansion
struct TemplateContext<'a> {
    dirname: Option<&'a str>,
    filename: Option<&'a str>,
    pid: u32,
    sequence: u64,
}

/// Type alias for template specifier handler
type TemplateHandler = fn(&TemplateContext) -> Option<String>;

/// Template specifier handlers
const TEMPLATE_SPECIFIERS: &[(char, TemplateHandler)] = &[
    ('d', |ctx| ctx.dirname.map(|s| s.to_string())),
    ('f', |ctx| ctx.filename.map(|s| s.to_string())),
    ('p', |ctx| Some(ctx.pid.to_string())),
    ('n', |ctx| Some(ctx.sequence.to_string())),
    ('%', |_| Some("%".to_string())),
];

/// Unified template expansion function
fn expand_template(template: &str, ctx: &TemplateContext) -> String {
    let mut result = String::new();
    let mut chars = template.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '%' {
            match chars.next() {
                Some(spec) => {
                    if let Some((_, handler)) =
                        TEMPLATE_SPECIFIERS.iter().find(|(ch, _)| *ch == spec)
                    {
                        if let Some(value) = handler(ctx) {
                            result.push_str(&value);
                        } else {
                            // Specifier not available in this context - pass through
                            result.push('%');
                            result.push(spec);
                        }
                    } else {
                        // Unknown specifier - include literally
                        result.push('%');
                        result.push(spec);
                    }
                }
                None => result.push('%'),
            }
        } else {
            result.push(c);
        }
    }

    result
}

/// Expand template for per-file extended header names
fn expand_header_template(template: &str, path: &std::path::Path, sequence: u64) -> String {
    // Use empty string as fallback (matching original behavior with unwrap_or_default)
    let dirname_owned = path
        .parent()
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_default();
    let filename_owned = path
        .file_name()
        .map(|f| f.to_string_lossy().into_owned())
        .unwrap_or_default();

    let ctx = TemplateContext {
        dirname: Some(dirname_owned.as_str()),
        filename: Some(filename_owned.as_str()),
        pid: std::process::id(),
        sequence,
    };

    expand_template(template, &ctx)
}

/// Expand template for global extended header names
fn expand_global_header_template(template: &str, sequence: u64) -> String {
    let ctx = TemplateContext {
        dirname: None,
        filename: None,
        pid: std::process::id(),
        sequence,
    };

    expand_template(template, &ctx)
}

// Format specifier handlers for list entry formatting
fn fmt_basename(info: &ListEntryInfo) -> String {
    std::path::Path::new(info.path)
        .file_name()
        .map(|s| s.to_string_lossy().to_string())
        .unwrap_or_else(|| info.path.to_string())
}

fn fmt_fullpath(info: &ListEntryInfo) -> String {
    info.path.to_string()
}

fn fmt_link_target(info: &ListEntryInfo) -> String {
    info.link_target.unwrap_or("").to_string()
}

fn fmt_mode_octal(info: &ListEntryInfo) -> String {
    format!("{:o}", info.mode & 0o7777)
}

fn fmt_mode_symbolic(info: &ListEntryInfo) -> String {
    format_mode_symbolic(info.mode, info.entry_type)
}

fn fmt_device(info: &ListEntryInfo) -> String {
    format!("{},{}", info.devmajor, info.devminor)
}

fn fmt_size(info: &ListEntryInfo) -> String {
    info.size.to_string()
}

fn fmt_mtime_trad(info: &ListEntryInfo) -> String {
    format_time_traditional(info.mtime)
}

fn fmt_mtime_iso(info: &ListEntryInfo) -> String {
    format_time_iso(info.mtime)
}

fn fmt_username(info: &ListEntryInfo) -> String {
    info.uname
        .map(|s| s.to_string())
        .unwrap_or_else(|| info.uid.to_string())
}

fn fmt_groupname(info: &ListEntryInfo) -> String {
    info.gname
        .map(|s| s.to_string())
        .unwrap_or_else(|| info.gid.to_string())
}

fn fmt_uid(info: &ListEntryInfo) -> String {
    info.uid.to_string()
}

fn fmt_gid(info: &ListEntryInfo) -> String {
    info.gid.to_string()
}

fn fmt_newline(_info: &ListEntryInfo) -> String {
    "\n".to_string()
}

fn fmt_percent(_info: &ListEntryInfo) -> String {
    "%".to_string()
}

/// Type alias for format specifier handler
type FormatHandler = fn(&ListEntryInfo) -> String;

/// Table of format specifiers and their handler functions
const FORMAT_SPECIFIERS: &[(char, FormatHandler)] = &[
    ('f', fmt_basename),
    ('F', fmt_fullpath),
    ('l', fmt_link_target),
    ('m', fmt_mode_octal),
    ('M', fmt_mode_symbolic),
    ('D', fmt_device),
    ('s', fmt_size),
    ('t', fmt_mtime_trad),
    ('T', fmt_mtime_iso),
    ('u', fmt_username),
    ('g', fmt_groupname),
    ('U', fmt_uid),
    ('G', fmt_gid),
    ('n', fmt_newline),
    ('%', fmt_percent),
];

/// Locate a `listopt=` keyword sitting at an option boundary (start of string
/// or just after an unescaped comma), returning the byte offset of `listopt=`.
fn find_listopt_marker(input: &str) -> Option<usize> {
    let mut boundaries = vec![0usize];
    let mut escaped = false;
    for (i, c) in input.char_indices() {
        if escaped {
            escaped = false;
        } else if c == '\\' {
            escaped = true;
        } else if c == ',' {
            boundaries.push(i + 1);
        }
    }
    for b in boundaries {
        let rest = &input[b..];
        let start = b + (rest.len() - rest.trim_start().len());
        if input[start..].starts_with("listopt=") {
            return Some(start);
        }
    }
    None
}

/// Remove backslash escapes (`\X` → `X`) from a listopt format value.
fn unescape_backslashes(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut escaped = false;
    for c in s.chars() {
        if escaped {
            out.push(c);
            escaped = false;
        } else if c == '\\' {
            escaped = true;
        } else {
            out.push(c);
        }
    }
    out
}

/// Parse list format specification and format an entry
///
/// Format specifiers (subset of POSIX listopt):
/// - `%f` - filename
/// - `%F` - filename (full path)
/// - `%l` - link name (for symlinks)
/// - `%m` - permission mode (octal)
/// - `%M` - permission mode (symbolic like ls -l)
/// - `%s` - file size in bytes
/// - `%t` - modification time
/// - `%T` - modification time (ISO format)
/// - `%u` - owner username
/// - `%g` - group name
/// - `%U` - owner uid
/// - `%G` - group gid
/// - `%n` - newline
/// - `%%` - literal %
pub fn format_list_entry(format: &str, info: &ListEntryInfo) -> String {
    let mut result = String::new();
    let mut chars = format.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '%' {
            if let Some(spec) = parse_format_specifier(&mut chars) {
                result.push_str(&format_with_spec(info, spec));
            } else {
                result.push('%');
            }
        } else {
            result.push(c);
        }
    }

    result
}

/// Maximum allowed width or precision for listopt format specifiers.
/// This prevents unbounded memory allocation from malicious format strings.
/// 4096 is chosen as a safe upper bound that accommodates legitimate use cases
/// while preventing OOM attacks via enormous padding strings.
const MAX_FORMAT_FIELD_SIZE: usize = 4096;

#[derive(Default)]
struct FormatSpec {
    left_justify: bool,
    width: Option<usize>,
    precision: Option<usize>,
    spec: char,
    /// POSIX `%(keyword)X` extended-header keyword (with an optional `=subformat`
    /// tail used by the `T` time conversion).
    keyword: Option<String>,
}

fn parse_format_specifier(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
) -> Option<FormatSpec> {
    let mut spec = FormatSpec::default();

    while let Some('-') = chars.peek().copied() {
        spec.left_justify = true;
        chars.next();
    }

    spec.width = parse_number(chars).map(|w| w.min(MAX_FORMAT_FIELD_SIZE));

    if let Some('.') = chars.peek().copied() {
        chars.next();
        spec.precision = parse_number(chars)
            .map(|p| p.min(MAX_FORMAT_FIELD_SIZE))
            .or(Some(0));
    }

    // POSIX keyword substitution: `%(keyword)s`, `%(mtime=%Y)T`, etc.
    if let Some('(') = chars.peek().copied() {
        chars.next();
        let mut keyword = String::new();
        for c in chars.by_ref() {
            if c == ')' {
                break;
            }
            keyword.push(c);
        }
        spec.keyword = Some(keyword);
    }

    spec.spec = chars.next()?;
    Some(spec)
}

fn parse_number(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Option<usize> {
    let mut value: usize = 0;
    let mut seen = false;

    while let Some(c) = chars.peek().copied() {
        if let Some(digit) = c.to_digit(10) {
            seen = true;
            value = value.saturating_mul(10).saturating_add(digit as usize);
            chars.next();
        } else {
            break;
        }
    }

    if seen {
        Some(value)
    } else {
        None
    }
}

fn format_with_spec(info: &ListEntryInfo, spec: FormatSpec) -> String {
    let mut rendered = if let Some(ref keyword) = spec.keyword {
        // POSIX `%(keyword)X` substitution. For the `T` time conversion the
        // keyword may carry an `=subformat` tail; here the keyword names the
        // time field and the (optional) subformat is currently rendered with
        // the default time layout.
        let (kw, _subformat) = keyword.split_once('=').unwrap_or((keyword.as_str(), ""));
        keyword_value(info, kw.trim(), spec.spec)
            .unwrap_or_else(|| format!("%({}){}", keyword, spec.spec))
    } else if let Some((_, handler)) = FORMAT_SPECIFIERS.iter().find(|(ch, _)| *ch == spec.spec) {
        handler(info)
    } else {
        let mut literal = String::from("%");
        literal.push(spec.spec);
        literal
    };

    if let Some(precision) = spec.precision {
        if rendered.len() > precision {
            rendered.truncate(precision);
        }
    }

    if let Some(width) = spec.width {
        if rendered.len() < width {
            let padding = width - rendered.len();
            if spec.left_justify {
                rendered.push_str(&" ".repeat(padding));
            } else {
                rendered = format!("{}{}", " ".repeat(padding), rendered);
            }
        }
    }

    rendered
}

/// Resolve a POSIX `%(keyword)X` listopt substitution to its rendered value.
///
/// `keyword` is the extended-header keyword named between the parentheses and
/// `conversion` is the trailing conversion character (`s`, `d`, `T`, `M`, ...).
/// Returns `None` for an unknown keyword so the caller can echo it literally.
fn keyword_value(info: &ListEntryInfo, keyword: &str, conversion: char) -> Option<String> {
    let value = match keyword {
        "path" | "name" => fmt_fullpath(info),
        "size" => info.size.to_string(),
        "uid" => info.uid.to_string(),
        "gid" => info.gid.to_string(),
        "uname" => info.uname.unwrap_or("").to_string(),
        "gname" => info.gname.unwrap_or("").to_string(),
        "linkpath" => info.link_target.unwrap_or("").to_string(),
        "mtime" | "atime" | "ctime" => {
            // Time keyword: the `T` conversion renders a calendar time; any other
            // conversion (s/d) yields the raw seconds.
            if conversion == 'T' {
                format_time_traditional(info.mtime)
            } else {
                info.mtime.to_string()
            }
        }
        "mode" => format!("{:o}", info.mode),
        _ => return None,
    };

    // For the mode/device/symlink special conversions, honor the conversion
    // character even when a keyword was given (e.g. `%(path)F`).
    let rendered = match conversion {
        'M' => format_mode_symbolic(info.mode, info.entry_type),
        'F' => fmt_fullpath(info),
        'L' => fmt_link_target(info),
        'D' => fmt_device(info),
        _ => value,
    };
    Some(rendered)
}

/// Entry type to file type character mapping for symbolic mode display
const ENTRY_TYPE_CHARS: &[(EntryType, char)] = &[
    (EntryType::Regular, '-'),
    (EntryType::Directory, 'd'),
    (EntryType::Symlink, 'l'),
    // A hard link is a regular file with link count > 1: ls -l shows '-'.
    (EntryType::Hardlink, '-'),
    (EntryType::BlockDevice, 'b'),
    (EntryType::CharDevice, 'c'),
    (EntryType::Fifo, 'p'),
    (EntryType::Socket, 's'),
];

/// Get file type character for an entry type
fn entry_type_char(entry_type: EntryType) -> char {
    ENTRY_TYPE_CHARS
        .iter()
        .find(|(et, _)| *et == entry_type)
        .map(|(_, c)| *c)
        .unwrap_or('-')
}

/// Permission triplet definition for table-driven mode formatting
struct PermTriplet {
    read: u32,
    write: u32,
    exec: u32,
    special: u32,
    set_char: char,   // char when special+exec (e.g., 's' for setuid/setgid)
    unset_char: char, // char when special but no exec (e.g., 'S')
}

/// Permission triplets: owner, group, other
const PERM_TRIPLETS: &[PermTriplet] = &[
    PermTriplet {
        read: 0o400,
        write: 0o200,
        exec: 0o100,
        special: 0o4000,
        set_char: 's',
        unset_char: 'S',
    },
    PermTriplet {
        read: 0o040,
        write: 0o020,
        exec: 0o010,
        special: 0o2000,
        set_char: 's',
        unset_char: 'S',
    },
    PermTriplet {
        read: 0o004,
        write: 0o002,
        exec: 0o001,
        special: 0o1000,
        set_char: 't',
        unset_char: 'T',
    },
];

/// Format mode as symbolic string (like ls -l)
fn format_mode_symbolic(mode: u32, entry_type: EntryType) -> String {
    let mut s = String::with_capacity(10);

    // File type (from entry_type, not mode bits - tar stores type separately)
    s.push(entry_type_char(entry_type));

    // Process each permission triplet (owner, group, other)
    for triplet in PERM_TRIPLETS {
        s.push(if mode & triplet.read != 0 { 'r' } else { '-' });
        s.push(if mode & triplet.write != 0 { 'w' } else { '-' });
        s.push(if mode & triplet.special != 0 {
            if mode & triplet.exec != 0 {
                triplet.set_char
            } else {
                triplet.unset_char
            }
        } else if mode & triplet.exec != 0 {
            'x'
        } else {
            '-'
        });
    }

    s
}

/// Format time in traditional ls -l style
fn format_time_traditional(mtime: u64) -> String {
    // POSIX `ls -l`-style time, formatted via libc strftime (localtime_r), so TZ
    // and LC_TIME take effect: date+time when recent, date+year otherwise.
    use std::time::{SystemTime, UNIX_EPOCH};

    let now_secs = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0);
    let age = now_secs - mtime as i64;
    let six_months: i64 = 180 * 24 * 60 * 60;
    let recent = (0..six_months).contains(&age);

    let fmt = if recent { "%b %e %H:%M" } else { "%b %e  %Y" };
    plib::locale::strftime(fmt, mtime as i64).unwrap_or_else(|_| mtime.to_string())
}

/// Format time in ISO 8601 form (`%Y-%m-%dT%H:%M:%S`), TZ-aware via strftime.
fn format_time_iso(mtime: u64) -> String {
    plib::locale::strftime("%Y-%m-%dT%H:%M:%S", mtime as i64).unwrap_or_else(|_| mtime.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_empty() {
        let opts = FormatOptions::parse("").unwrap();
        assert!(opts.list_format.is_none());
        assert!(opts.delete_patterns.is_empty());
    }

    #[test]
    fn test_parse_single_option() {
        let opts = FormatOptions::parse("times").unwrap();
        assert!(opts.include_times);
    }

    #[test]
    fn test_parse_key_value() {
        let opts = FormatOptions::parse("listopt=%F %s").unwrap();
        assert_eq!(opts.list_format, Some("%F %s".to_string()));
    }

    #[test]
    fn test_parse_multiple_options() {
        let opts = FormatOptions::parse("times,linkdata,listopt=%F").unwrap();
        assert!(opts.include_times);
        assert!(opts.link_data);
        assert_eq!(opts.list_format, Some("%F".to_string()));
    }

    #[test]
    fn test_parse_delete_patterns() {
        let opts = FormatOptions::parse("delete=*.tmp,delete=*.bak").unwrap();
        assert_eq!(opts.delete_patterns.len(), 2);
        assert!(opts.delete_patterns.contains(&"*.tmp".to_string()));
        assert!(opts.delete_patterns.contains(&"*.bak".to_string()));
    }

    #[test]
    fn test_parse_per_file_option() {
        let opts = FormatOptions::parse("gname:=mygroup").unwrap();
        assert_eq!(opts.get_per_file("gname"), Some(&"mygroup".to_string()));
    }

    #[test]
    fn test_parse_escaped_comma() {
        let opts = FormatOptions::parse(r"listopt=a\,b").unwrap();
        assert_eq!(opts.list_format, Some("a,b".to_string()));
    }

    #[test]
    fn test_globexthdr_name_uses_tmpdir() {
        // The default global-header name template derives from $TMPDIR, falling
        // back to /tmp when it is unset. Tested via the pure helper to avoid
        // mutating the process-global TMPDIR env (which would race other tests'
        // temp-dir creation).
        assert_eq!(
            default_globexthdr_template(Some("/custom/tmp")),
            "/custom/tmp/GlobalHead.%p.%n"
        );
        assert_eq!(
            default_globexthdr_template(Some("/custom/tmp/")),
            "/custom/tmp/GlobalHead.%p.%n"
        );
        assert_eq!(default_globexthdr_template(None), "/tmp/GlobalHead.%p.%n");
    }

    #[test]
    fn test_merge_options() {
        let mut opts1 = FormatOptions::parse("times").unwrap();
        let opts2 = FormatOptions::parse("linkdata,listopt=%F").unwrap();
        opts1.merge(&opts2);

        assert!(opts1.include_times);
        assert!(opts1.link_data);
        assert_eq!(opts1.list_format, Some("%F".to_string()));
    }

    #[test]
    fn test_format_list_entry_basic() {
        let info = ListEntryInfo {
            path: "path/to/file.txt",
            mode: 0o644,
            size: 1234,
            mtime: 0,
            uid: 1000,
            gid: 1000,
            uname: Some("user"),
            gname: Some("group"),
            link_target: None,
            entry_type: EntryType::Regular,
            devmajor: 0,
            devminor: 0,
        };
        let result = format_list_entry("%F", &info);
        assert_eq!(result, "path/to/file.txt");
    }

    #[test]
    fn test_format_list_entry_complex() {
        let info = ListEntryInfo {
            path: "dir/file.txt",
            mode: 0o755,
            size: 4096,
            mtime: 0,
            uid: 1000,
            gid: 1000,
            uname: Some("alice"),
            gname: Some("users"),
            link_target: None,
            entry_type: EntryType::Regular,
            devmajor: 0,
            devminor: 0,
        };
        let result = format_list_entry("%M %u %g %s %f", &info);
        assert_eq!(result, "-rwxr-xr-x alice users 4096 file.txt");
    }

    #[test]
    fn test_format_list_entry_keyword_substitution() {
        let info = ListEntryInfo {
            path: "dir/file.txt",
            mode: 0o644,
            size: 4096,
            mtime: 0,
            uid: 1000,
            gid: 1000,
            uname: Some("alice"),
            gname: Some("users"),
            link_target: None,
            entry_type: EntryType::Regular,
            devmajor: 0,
            devminor: 0,
        };

        // POSIX `%(keyword)s`/`%(keyword)d` substitution.
        assert_eq!(
            format_list_entry("%(path)s %(size)d", &info),
            "dir/file.txt 4096"
        );
        assert_eq!(
            format_list_entry("%(uname)s:%(gname)s", &info),
            "alice:users"
        );
        // An unknown keyword is echoed verbatim.
        assert_eq!(format_list_entry("%(bogus)s", &info), "%(bogus)s");
    }

    #[test]
    fn test_listopt_is_final_keyword_with_commas() {
        // A literal comma inside the listopt format must not split the option.
        let opts = FormatOptions::parse("times,listopt=%(path)s,%(size)d bytes").unwrap();
        assert!(opts.include_times);
        assert_eq!(
            opts.list_format,
            Some("%(path)s,%(size)d bytes".to_string())
        );
    }

    #[test]
    fn test_format_mode_symbolic() {
        // Regular file with various permissions
        assert_eq!(
            format_mode_symbolic(0o644, EntryType::Regular),
            "-rw-r--r--"
        );
        assert_eq!(
            format_mode_symbolic(0o755, EntryType::Regular),
            "-rwxr-xr-x"
        );
        // Directory
        assert_eq!(
            format_mode_symbolic(0o755, EntryType::Directory),
            "drwxr-xr-x"
        );
        // Symlink
        assert_eq!(
            format_mode_symbolic(0o777, EntryType::Symlink),
            "lrwxrwxrwx"
        );
        // Special bits
        assert_eq!(
            format_mode_symbolic(0o4755, EntryType::Regular),
            "-rwsr-xr-x"
        ); // setuid
        assert_eq!(
            format_mode_symbolic(0o2755, EntryType::Regular),
            "-rwxr-sr-x"
        ); // setgid
        assert_eq!(
            format_mode_symbolic(0o1755, EntryType::Regular),
            "-rwxr-xr-t"
        ); // sticky
           // Block and char devices
        assert_eq!(
            format_mode_symbolic(0o660, EntryType::BlockDevice),
            "brw-rw----"
        );
        assert_eq!(
            format_mode_symbolic(0o660, EntryType::CharDevice),
            "crw-rw----"
        );
        // Other types
        assert_eq!(format_mode_symbolic(0o644, EntryType::Fifo), "prw-r--r--");
        assert_eq!(format_mode_symbolic(0o755, EntryType::Socket), "srwxr-xr-x");
        // A hard link is a regular file (link count > 1): ls -l shows '-'.
        assert_eq!(
            format_mode_symbolic(0o644, EntryType::Hardlink),
            "-rw-r--r--"
        );
    }

    #[test]
    fn test_format_list_entry_device() {
        // Test %D format specifier for device major,minor
        let info = ListEntryInfo {
            path: "/dev/sda",
            mode: 0o660,
            size: 0,
            mtime: 0,
            uid: 0,
            gid: 0,
            uname: Some("root"),
            gname: Some("disk"),
            link_target: None,
            entry_type: EntryType::BlockDevice,
            devmajor: 8,
            devminor: 0,
        };
        let result = format_list_entry("%M %D %f", &info);
        assert_eq!(result, "brw-rw---- 8,0 sda");
    }

    #[test]
    fn test_format_list_entry_different_types() {
        // Test that %M correctly uses entry_type for file type character
        // Directory
        let info = ListEntryInfo {
            path: "mydir",
            mode: 0o755,
            size: 0,
            mtime: 0,
            uid: 0,
            gid: 0,
            uname: None,
            gname: None,
            link_target: None,
            entry_type: EntryType::Directory,
            devmajor: 0,
            devminor: 0,
        };
        let result = format_list_entry("%M", &info);
        assert_eq!(result, "drwxr-xr-x");

        // Symlink
        let info = ListEntryInfo {
            path: "mylink",
            mode: 0o777,
            size: 0,
            mtime: 0,
            uid: 0,
            gid: 0,
            uname: None,
            gname: None,
            link_target: Some("target"),
            entry_type: EntryType::Symlink,
            devmajor: 0,
            devminor: 0,
        };
        let result = format_list_entry("%M", &info);
        assert_eq!(result, "lrwxrwxrwx");
    }

    #[test]
    fn test_format_time_iso() {
        // The exact value is timezone-dependent (strftime via localtime_r), so
        // assert the ISO 8601 shape `YYYY-MM-DDTHH:MM:SS` rather than a fixed UTC
        // instant.
        let result = format_time_iso(1704067200);
        let bytes = result.as_bytes();
        assert_eq!(result.len(), 19, "unexpected ISO length: {result}");
        assert_eq!(&result[4..5], "-");
        assert_eq!(&result[7..8], "-");
        assert_eq!(&result[10..11], "T");
        assert_eq!(&result[13..14], ":");
        assert_eq!(&result[16..17], ":");
        assert!(
            bytes[..4].iter().all(|b| b.is_ascii_digit()),
            "year should be digits: {result}"
        );
    }
}
