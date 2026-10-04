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

use crate::archive::{ArchiveEntry, EntryType, SourceHeader};
use crate::error::{PaxError, PaxResult};
use crate::pattern::Pattern;
use std::collections::HashMap;

/// Information about an archive entry for list formatting
///
/// The member is borrowed whole rather than copied field by field. POSIX
/// listopt rule 7 admits every ustar and cpio header field name and every pax
/// extended-header keyword as a `%(keyword)`, so the set of fields a format
/// string can reach is the entry's own; a hand-copied subset is a list that
/// silently falls behind the one `keyword_value` is meant to resolve.
#[derive(Debug, Clone)]
pub struct ListEntryInfo<'a> {
    /// The member as the reader produced it, after the name rewrites list mode
    /// applies first (`-s`, `--strip-components`, `-o keyword:=value`), so a
    /// keyword reports what extracting this archive would use.
    pub entry: &'a ArchiveEntry,
    /// Whether the *substituted values* get escaped.
    ///
    /// Only the values, never the rendered result: the format string is
    /// operator-supplied, so escaping the output would turn a deliberate
    /// `-o listopt=$'%F\t%s'` tab into a `?`.
    pub style: crate::escape::Style,
}

/// The `-o invalid=` actions POSIX defines, and whether this implementation
/// performs them.
///
/// POSIX scopes this keyword to values in an *extended header record* in read,
/// copy and list mode -- a name "invalid in the destination hierarchy". On
/// Linux the only thing that makes a pathname invalid is an embedded NUL, and
/// such a member is already diagnosed and skipped, which is what `bypass`
/// specifies. The other four actions would each have to create or rename a
/// file, and none of them is implemented: accepting them and doing nothing is
/// the silent-acceptance failure, so they are refused by name.
const INVALID_ACTIONS: &[(&str, bool)] = &[
    ("bypass", true),
    ("rename", false),
    ("write", false),
    ("UTF-8", false),
    ("binary", false),
];

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
    /// Times option (include atime/mtime -- and, as an extension, ctime -- in
    /// extended headers)
    pub include_times: bool,
    /// Linkdata option (write contents for hard links)
    pub link_data: bool,
    /// Extended header name template (exthdr.name)
    /// Default: "%d/PaxHeaders.%p/%f"
    pub exthdr_name: Option<String>,
    /// Global extended header name template (globexthdr.name)
    /// Default: "$TMPDIR/GlobalHead.%p.%n"
    pub globexthdr_name: Option<String>,
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
                        match INVALID_ACTIONS.iter().find(|(name, _)| *name == v) {
                            Some((_, true)) => {}
                            Some((name, false)) => {
                                return Err(PaxError::InvalidFormat(format!(
                                    "invalid={name} is not supported; the only \
                                     supported action is invalid=bypass, which \
                                     skips a member whose name the destination \
                                     cannot hold"
                                )))
                            }
                            None => {
                                return Err(PaxError::InvalidFormat(format!(
                                    "invalid value for 'invalid' option: {}",
                                    v
                                )))
                            }
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
fn fmt_basename(info: &ListEntryInfo) -> Vec<u8> {
    match info.entry.path.file_name() {
        Some(name) => escaped(info, crate::rawpath::as_bytes(std::path::Path::new(name))),
        None => fmt_fullpath(info),
    }
}

fn fmt_fullpath(info: &ListEntryInfo) -> Vec<u8> {
    escaped(info, crate::rawpath::as_bytes(&info.entry.path))
}

fn fmt_link_target(info: &ListEntryInfo) -> Vec<u8> {
    match info.entry.link_target.as_deref() {
        Some(p) => escaped(info, crate::rawpath::as_bytes(p)),
        None => Vec::new(),
    }
}

/// A name, escaped for the stream this listing is going to.
///
/// Escaping happens here rather than on the finished line so that the width
/// and precision arithmetic below sees the units it will actually print -- and
/// so the operator's own format string keeps whatever characters it contains.
fn escaped(info: &ListEntryInfo, bytes: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(bytes.len());
    crate::escape::push_escaped(&mut out, bytes, info.style);
    out
}

fn fmt_mode_octal(info: &ListEntryInfo) -> Vec<u8> {
    format!("{:o}", info.entry.mode & 0o7777).into_bytes()
}

fn fmt_mode_symbolic(info: &ListEntryInfo) -> Vec<u8> {
    format_mode_symbolic(info.entry.mode, info.entry.entry_type).into_bytes()
}

fn fmt_device(info: &ListEntryInfo) -> Vec<u8> {
    format!("{},{}", info.entry.devmajor, info.entry.devminor).into_bytes()
}

/// Bare `%D`. Rule 10: with no keyword to fall back on, a non-device entry
/// renders as a single <space>.
fn fmt_device_or_space(info: &ListEntryInfo) -> Vec<u8> {
    if is_device(info) {
        fmt_device(info)
    } else {
        b" ".to_vec()
    }
}

fn fmt_size(info: &ListEntryInfo) -> Vec<u8> {
    info.entry.size.to_string().into_bytes()
}

fn fmt_mtime_trad(info: &ListEntryInfo) -> Vec<u8> {
    format_time_traditional(info.entry.mtime).into_bytes()
}

/// Bare `%T`. Rule 8: the default keyword is mtime and the default subformat is
/// `%b %e %H:%M %Y`.
fn fmt_mtime_posix(info: &ListEntryInfo) -> Vec<u8> {
    strftime_or_secs(info.entry.mtime, DEFAULT_TIME_SUBFORMAT).into_bytes()
}

fn fmt_username(info: &ListEntryInfo) -> Vec<u8> {
    info.entry
        .uname
        .clone()
        .unwrap_or_else(|| info.entry.uid.to_string())
        .into_bytes()
}

fn fmt_groupname(info: &ListEntryInfo) -> Vec<u8> {
    info.entry
        .gname
        .clone()
        .unwrap_or_else(|| info.entry.gid.to_string())
        .into_bytes()
}

fn fmt_uid(info: &ListEntryInfo) -> Vec<u8> {
    info.entry.uid.to_string().into_bytes()
}

fn fmt_gid(info: &ListEntryInfo) -> Vec<u8> {
    info.entry.gid.to_string().into_bytes()
}

fn fmt_newline(_info: &ListEntryInfo) -> Vec<u8> {
    b"\n".to_vec()
}

fn fmt_percent(_info: &ListEntryInfo) -> Vec<u8> {
    b"%".to_vec()
}

/// Type alias for format specifier handler
type FormatHandler = fn(&ListEntryInfo) -> Vec<u8>;

/// Table of format specifiers and their handler functions
const FORMAT_SPECIFIERS: &[(char, FormatHandler)] = &[
    ('f', fmt_basename),
    ('F', fmt_fullpath),
    ('l', fmt_link_target),
    ('m', fmt_mode_octal),
    ('M', fmt_mode_symbolic),
    ('D', fmt_device_or_space),
    ('s', fmt_size),
    ('t', fmt_mtime_trad),
    ('T', fmt_mtime_posix),
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
/// - `%t` - modification time, `ls -l` style (extension)
/// - `%T` - time, default keyword `mtime`, default subformat `%b %e %H:%M %Y`
/// - `%D` - device of a block/char special file
/// - `%u` - owner username
/// - `%g` - group name
/// - `%U` - owner uid
/// - `%G` - group gid
/// - `%n` - newline
/// - `%%` - literal %
pub fn format_list_entry(format: &str, info: &ListEntryInfo) -> Vec<u8> {
    let mut result: Vec<u8> = Vec::new();
    let mut chars = format.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '%' {
            if let Some(spec) = parse_format_specifier(&mut chars) {
                result.extend_from_slice(&format_with_spec(info, spec));
            } else {
                result.push(b'%');
            }
        } else {
            // The format string is operator-supplied text; its literal
            // characters go out as their UTF-8 encoding.
            result.extend_from_slice(c.encode_utf8(&mut [0u8; 4]).as_bytes());
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

fn format_with_spec(info: &ListEntryInfo, spec: FormatSpec) -> Vec<u8> {
    let mut rendered = if let Some(ref keyword) = spec.keyword {
        match keyword_value(info, keyword, spec.spec) {
            KeywordValue::Value(v) => v,
            KeywordValue::Absent => Vec::new(),
            KeywordValue::Unknown => format!("%({}){}", keyword, spec.spec).into_bytes(),
        }
    } else if let Some((_, handler)) = FORMAT_SPECIFIERS.iter().find(|(ch, _)| *ch == spec.spec) {
        handler(info)
    } else {
        let mut literal = String::from("%");
        literal.push(spec.spec);
        literal.into_bytes()
    };

    // Width and precision count display units, not bytes: a byte index would
    // split a multi-byte character and mis-align a column holding a non-ASCII
    // name. A unit is one valid UTF-8 character or one byte that cannot start
    // one, so a name that is not UTF-8 is measured rather than rejected. On
    // input that is valid UTF-8 this is exactly a character count.
    let units = crate::rawpath::unit_starts(&rendered);

    if let Some(precision) = spec.precision {
        if let Some(&byte_idx) = units.get(precision) {
            rendered.truncate(byte_idx);
        }
    }

    if let Some(width) = spec.width {
        let count = if spec.precision.is_some() {
            crate::rawpath::unit_starts(&rendered).len()
        } else {
            units.len()
        };
        if count < width {
            let padding = vec![b' '; width - count];
            if spec.left_justify {
                rendered.extend_from_slice(&padding);
            } else {
                rendered.splice(0..0, padding);
            }
        }
    }

    rendered
}

/// Outcome of resolving a `%(keyword)` listopt substitution.
enum KeywordValue {
    /// The keyword resolved to this rendered value.
    Value(Vec<u8>),
    /// A keyword we recognize, for which this archive member carried no record
    /// (an `atime` request against an archive written without `-o times`, say).
    /// POSIX rule 7 defines the result as the value from the extended header,
    /// and there is none, so it contributes nothing.
    Absent,
    /// Not a keyword we know: the caller echoes the specification literally.
    Unknown,
}

/// The seconds value of a time-valued keyword, if this entry carries one.
/// The outer `Option` distinguishes "not a time keyword" from "no record".
fn time_keyword(info: &ListEntryInfo, keyword: &str) -> Option<Option<u64>> {
    match keyword {
        "mtime" => Some(Some(info.entry.mtime)),
        "atime" => Some(info.entry.atime),
        // ctime is not a POSIX keyword (it was removed by
        // IEEE Std 1003.1-2001/Cor 2-2004 because st_ctime is not a creation
        // time), but it is written by star/GNU tar and rule 7 admits
        // implementation extensions, so archives carrying one can be listed.
        "ctime" => Some(info.entry.ctime),
        _ => None,
    }
}

/// Whether the `D` conversion's device rendering applies to this entry.
fn is_device(info: &ListEntryInfo) -> bool {
    matches!(
        info.entry.entry_type,
        EntryType::BlockDevice | EntryType::CharDevice
    )
}

/// What a keyword named, before the conversion character has its say.
enum Field {
    /// The keyword names a field, and this member carries this value.
    Value(Vec<u8>),
    /// The keyword names a field the member's format does not have. Rule 7
    /// defines the result as "the value from the applicable header field", and
    /// there is no such field, so it contributes nothing.
    Absent,
}

/// A numeric field's value. Decimal: a count, an id, a size or a time is read
/// in decimal, and the "Octal number" column of POSIX's field tables describes
/// how the header encodes the value, not how to print it -- the same `size` and
/// `uid` are spelled in decimal by the pax extended-header table.
fn fmt_decimal(value: u64) -> Field {
    Field::Value(value.to_string().into_bytes())
}

/// A bitfield's value, in the radix it is read in. Reserved for the fields
/// whose only meaning is as stored bits: a mode, a header checksum, a packed
/// device number.
fn fmt_octal(value: u64) -> Field {
    Field::Value(format!("{:o}", value).into_bytes())
}

/// A fixed-width header field's text value, escaped for the output stream.
///
/// Rule 7: "without any trailing NULs" -- and nothing else. A trailing <space>
/// is part of the value, which is why GNU tar's `magic` of "ustar " reports
/// with its space rather than being trimmed to a conforming-looking "ustar".
/// The bytes come from the archive, so they go through `escaped` like a name.
fn fmt_header_text(info: &ListEntryInfo, bytes: &[u8]) -> Field {
    Field::Value(escaped(info, crate::formats::ustar::path_field(bytes)))
}

/// The ustar `name` and `prefix` fields for this member's pathname.
///
/// Derived from the pathname rather than read back from the header. That keeps
/// rule 11's `(prefix,name)` default reconstructing exactly the name `%F`
/// prints -- after `-s`, `--strip-components` or `-o path:=` rewrote it, and
/// for a pax member whose real name lives in a `path=` record and whose stored
/// `name` field is only a truncated fallback for readers that ignore it.
fn ustar_name_prefix(path: &[u8]) -> (&[u8], &[u8]) {
    if let Some(halves) = crate::formats::ustar::split_name_prefix(path) {
        return halves;
    }
    // No `/` sits where the ustar fields could split this name, so neither
    // field can hold it. Split at the last `/` anyway: it is the only choice
    // that still satisfies rule 11's prefix + "/" + name == path.
    match path.iter().rposition(|&b| b == b'/') {
        Some(i) => (&path[i + 1..], &path[..i]),
        None => (path, b""),
    }
}

/// Resolve a keyword naming a pax extended-header record (POSIX rule 7, second
/// bullet). `None` when the name is not one of those keywords.
fn pax_keyword(info: &ListEntryInfo, keyword: &str) -> Option<Field> {
    Some(match keyword {
        "path" => Field::Value(fmt_fullpath(info)),
        "linkpath" => Field::Value(fmt_link_target(info)),
        "size" => fmt_decimal(info.entry.size),
        "uid" => fmt_decimal(info.entry.uid as u64),
        "gid" => fmt_decimal(info.entry.gid as u64),
        "uname" => Field::Value(info.entry.uname.clone().unwrap_or_default().into_bytes()),
        "gname" => Field::Value(info.entry.gname.clone().unwrap_or_default().into_bytes()),
        // Records that describe the member without affecting extraction. They
        // are keywords whether or not the archive used them: an operator runs
        // `%(hdrcharset)s` precisely to find out whether one was declared, so
        // an archive that declared nothing must report nothing rather than
        // echoing the request back as a typo -- and must not report POSIX's
        // implicit UTF-8 default either, which would make the two cases
        // indistinguishable. Rule 7 asks for "the value from the ... extended
        // header", and there is none.
        "charset" | "hdrcharset" | "comment" => ext_record(info, keyword).unwrap_or(Field::Absent),
        _ => return None,
    })
}

/// An extended-header record's value, or `None` when this member carried no
/// record under that keyword.
///
/// This also resolves a keyword naming an implementation extension (POSIX rule
/// 7, third bullet) -- a `SCHILY.*` or `GNU.*` record, say. In the resolution
/// chain it comes last, after all three required tables, so that a crafted
/// archive cannot change what a required keyword means by recording a value
/// under its name.
fn ext_record(info: &ListEntryInfo, keyword: &str) -> Option<Field> {
    Some(Field::Value(escaped(
        info,
        info.entry.ext_record(keyword)?.as_bytes(),
    )))
}

/// Resolve a keyword naming a ustar Header Block field (POSIX rule 7, first
/// bullet). `None` when the name is not in that table.
///
/// `magic`, `version`, `chksum` and `typeflag` describe the header itself and
/// cannot be derived from anything else, so they are `Absent` for a member
/// that was not read from a ustar header. The rest are properties of the file
/// the entry already holds, so they answer whatever header it came from.
fn ustar_keyword(info: &ListEntryInfo, keyword: &str) -> Option<Field> {
    let path = crate::rawpath::as_bytes(&info.entry.path);
    let ustar = match info.entry.source_header {
        Some(SourceHeader::Ustar {
            ref magic,
            ref version,
            chksum,
            typeflag,
        }) => Some((magic, version, chksum, typeflag)),
        _ => None,
    };

    Some(match keyword {
        "name" => Field::Value(escaped(info, ustar_name_prefix(path).0)),
        "prefix" => Field::Value(escaped(info, ustar_name_prefix(path).1)),
        // The ustar field and the pax `linkpath` record are two spellings of
        // one datum, and the entry holds the effective value: a `linkpath`
        // record has already overridden a truncated header field.
        "linkname" => Field::Value(fmt_link_target(info)),
        "mode" => fmt_octal((info.entry.mode & 0o7777) as u64),
        "devmajor" => fmt_decimal(info.entry.devmajor as u64),
        "devminor" => fmt_decimal(info.entry.devminor as u64),
        "magic" => return Some(header_magic(info)),
        "version" => match ustar {
            Some((_, version, _, _)) => fmt_header_text(info, version),
            None => Field::Absent,
        },
        "chksum" => match ustar {
            Some((_, _, chksum, _)) => fmt_octal(chksum),
            None => Field::Absent,
        },
        "typeflag" => match ustar {
            // A NUL typeflag is the historical spelling of a regular file. It
            // is a trailing NUL, which rule 7 excludes, so it reports as
            // nothing rather than as an embedded NUL in the listing.
            Some((_, _, _, typeflag)) => fmt_header_text(info, &[typeflag]),
            None => Field::Absent,
        },
        _ => return None,
    })
}

/// The cpio field names POSIX permits without the leading `c_`.
///
/// Only the names no other table claims. `mode`, `uid`, `gid`, `mtime`, `size`,
/// `name` and `magic` keep their ustar and pax readings, which differ from the
/// cpio ones in value as well as radix -- `mode` is the permission bits and
/// `c_mode` carries the file type over them -- so honoring an unprefixed alias
/// for those would silently change what an existing format string reports.
const CPIO_UNPREFIXED: &[&str] = &[
    "dev", "ino", "nlink", "rdev", "namesize", "filesize", "filedata",
];

/// Resolve a keyword naming an Octet-Oriented cpio Archive Entry field (POSIX
/// rule 7, first bullet). `None` when the name is not in that table.
///
/// Rule 7: "The implementation may support the cpio keywords without the
/// leading c_ in addition to the form required". Both spellings resolve, but
/// the `c_` is stripped only when what remains is a cpio field name, so
/// `%(c_bogus)s` keeps the literal echo that tells an operator about a typo.
///
/// `c_dev`, `c_ino` and `c_nlink` are `Absent` for a member that did not come
/// from a cpio header: a ustar header records none of them, and the entry's
/// zeros are placeholders rather than values read from an archive.
fn cpio_keyword(info: &ListEntryInfo, keyword: &str) -> Option<Field> {
    let field = match keyword.strip_prefix("c_") {
        Some(rest) => rest,
        None if CPIO_UNPREFIXED.contains(&keyword) => keyword,
        None => return None,
    };

    let cpio = matches!(info.entry.source_header, Some(SourceHeader::Cpio { .. }));
    let path = crate::rawpath::as_bytes(&info.entry.path);

    Some(match field {
        "magic" => return Some(header_magic(info)),
        "dev" if cpio => fmt_decimal(info.entry.dev),
        "ino" if cpio => fmt_decimal(info.entry.ino),
        "nlink" if cpio => fmt_decimal(info.entry.nlink as u64),
        "dev" | "ino" | "nlink" => Field::Absent,
        "mode" => fmt_octal(
            crate::formats::cpio::cpio_mode(info.entry.mode, info.entry.entry_type) as u64,
        ),
        "uid" => fmt_decimal(info.entry.uid as u64),
        "gid" => fmt_decimal(info.entry.gid as u64),
        "mtime" => fmt_decimal(info.entry.mtime),
        "rdev" if is_device(info) => fmt_octal(crate::formats::cpio::pack_rdev(
            info.entry.devmajor,
            info.entry.devminor,
        )),
        "rdev" => fmt_octal(0),
        // c_namesize counts the NUL cpio stores after the pathname.
        "namesize" => fmt_decimal(path.len() as u64 + 1),
        // A symbolic link's target is cpio's file data, so it is what
        // c_filesize counts; the entry's own size is zero for one, which is
        // what the pax `size` keyword reports. A hard link also carries a
        // target, but cpio has no link typeflag and stores it as a regular
        // file with its full contents, so it is not one of these.
        "filesize" => match (info.entry.entry_type, info.entry.link_target.as_deref()) {
            (EntryType::Symlink, Some(target)) => {
                fmt_decimal(crate::rawpath::as_bytes(target).len() as u64)
            }
            _ => fmt_decimal(info.entry.size),
        },
        "name" => Field::Value(fmt_fullpath(info)),
        // c_filedata is the member's contents, not a value a listing reports.
        "filedata" => Field::Absent,
        _ => return None,
    })
}

/// `magic` / `c_magic`: the identifying value of whichever header this member
/// came from, so the keyword answers for a tar and a cpio archive alike.
fn header_magic(info: &ListEntryInfo) -> Field {
    match info.entry.source_header {
        Some(SourceHeader::Ustar { ref magic, .. }) => fmt_header_text(info, magic),
        Some(SourceHeader::Cpio { format }) => Field::Value(format.magic_str().as_bytes().to_vec()),
        None => Field::Absent,
    }
}

/// Resolve a POSIX `%(keyword)X` listopt substitution to its rendered value.
///
/// `field` is the text between the parentheses -- a keyword, optionally with an
/// `=subformat` tail for the `T` conversion -- and `conversion` is the trailing
/// conversion character (`s`, `d`, `T`, `M`, ...).
fn keyword_value(info: &ListEntryInfo, field: &str, conversion: char) -> KeywordValue {
    // Rule 8: the T conversion character may be preceded by
    // `(keyword=subformat)`, where subformat is a date format.
    let (keyword, subformat) = match field.split_once('=') {
        Some((kw, sub)) => (kw.trim(), sub),
        None => (field.trim(), DEFAULT_TIME_SUBFORMAT),
    };

    if let Some(seconds) = time_keyword(info, keyword) {
        return match seconds {
            // Only `T` renders a calendar time; s/d yield the raw seconds.
            Some(secs) if conversion == 'T' => {
                KeywordValue::Value(strftime_or_secs(secs, subformat).into_bytes())
            }
            Some(secs) => KeywordValue::Value(secs.to_string().into_bytes()),
            None => KeywordValue::Absent,
        };
    }

    // Rule 7's three keyword tables, consulted in the order they are listed
    // there. The pax table comes first because it owns the required reading of
    // the names the tables share -- `size` and `uid` are decimal there and
    // octal in the ustar header -- so an existing format string keeps its
    // answer.
    let value = match pax_keyword(info, keyword)
        .or_else(|| ustar_keyword(info, keyword))
        .or_else(|| cpio_keyword(info, keyword))
        .or_else(|| ext_record(info, keyword))
    {
        Some(Field::Value(v)) => v,
        Some(Field::Absent) => return KeywordValue::Absent,
        None => return KeywordValue::Unknown,
    };

    // The mode/pathname/symlink conversions describe how to render the entry
    // rather than which field to read, so they still apply when a keyword was
    // given (e.g. `%(path)F`).
    let rendered = match conversion {
        'M' => format_mode_symbolic(info.entry.mode, info.entry.entry_type).into_bytes(),
        'F' => fmt_fullpath(info),
        'L' => fmt_link_target(info),
        // Rule 10: D names the device of a block/character special file. When
        // that does not apply and a keyword was given, it degrades to
        // `%(keyword)u` -- so `%(size)D` on a regular file prints the size.
        'D' if is_device(info) => fmt_device(info),
        'D' => value,
        _ => value,
    };
    KeywordValue::Value(rendered)
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
pub(crate) fn format_mode_symbolic(mode: u32, entry_type: EntryType) -> String {
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
pub(crate) fn format_time_traditional(mtime: u64) -> String {
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

/// The default subformat of the listopt `T` conversion (POSIX pax EXTENDED
/// DESCRIPTION, rule 8). Unlike `ls -l`, it is fixed: the year is always shown
/// and never traded against the time of day.
const DEFAULT_TIME_SUBFORMAT: &str = "%b %e %H:%M %Y";

/// Render `secs` through `subformat`, TZ- and LC_TIME-aware via strftime,
/// falling back to the raw seconds if the subformat cannot be rendered.
fn strftime_or_secs(secs: u64, subformat: &str) -> String {
    plib::locale::strftime(subformat, secs as i64).unwrap_or_else(|_| secs.to_string())
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

    /// `format_list_entry` yields bytes, because a member name is bytes. Every
    /// fixture here is ASCII, so rendering back to a `String` keeps the
    /// assertions readable; the byte-exact cases assert on the Vec directly.
    fn fmt(format: &str, info: &ListEntryInfo) -> String {
        String::from_utf8(format_list_entry(format, info)).expect("ASCII fixture")
    }

    /// The formatter's view of a member, with escaping off.
    ///
    /// `ArchiveEntry` is `Default`, so a fixture below names only the fields
    /// its assertions depend on -- which is the point of borrowing the entry
    /// rather than copying a fixed subset of it into `ListEntryInfo`.
    fn info(entry: &ArchiveEntry) -> ListEntryInfo<'_> {
        ListEntryInfo {
            entry,
            style: crate::escape::Style::RAW,
        }
    }

    #[test]
    fn test_format_list_entry_basic() {
        let e = ArchiveEntry {
            path: "path/to/file.txt".into(),
            mode: 0o644,
            size: 1234,
            uid: 1000,
            gid: 1000,
            uname: Some("user".into()),
            gname: Some("group".into()),
            ..Default::default()
        };
        assert_eq!(fmt("%F", &info(&e)), "path/to/file.txt");
    }

    /// `%.N` is a precision on the string, so it counts characters. Applying it
    /// as a byte index panicked whenever the cut landed inside a multi-byte
    /// character -- `%.1F` on any name with a non-ASCII first character.
    #[test]
    fn test_format_list_entry_precision_is_char_counted() {
        let e = ArchiveEntry {
            path: "élan.txt".into(),
            mode: 0o644,
            uname: Some("ünïcode".into()),
            ..Default::default()
        };
        let info = info(&e);

        // 'é' is two bytes; a byte-indexed truncate(1) split it and aborted.
        assert_eq!(fmt("%.1F", &info), "é");
        assert_eq!(fmt("%.3F", &info), "éla");
        assert_eq!(fmt("%.1u", &info), "ü");
        // A precision at or beyond the length leaves the value intact.
        assert_eq!(fmt("%.99F", &info), "élan.txt");

        // Width padding is also a character count, so a non-ASCII name lines up
        // with an ASCII one of the same length.
        assert_eq!(fmt("%10F", &info), "  élan.txt");
    }

    #[test]
    fn test_format_list_entry_complex() {
        let e = ArchiveEntry {
            path: "dir/file.txt".into(),
            mode: 0o755,
            size: 4096,
            uid: 1000,
            gid: 1000,
            uname: Some("alice".into()),
            gname: Some("users".into()),
            ..Default::default()
        };
        let result = fmt("%M %u %g %s %f", &info(&e));
        assert_eq!(result, "-rwxr-xr-x alice users 4096 file.txt");
    }

    #[test]
    fn test_format_list_entry_keyword_substitution() {
        let e = ArchiveEntry {
            path: "dir/file.txt".into(),
            mode: 0o644,
            size: 4096,
            uid: 1000,
            gid: 1000,
            uname: Some("alice".into()),
            gname: Some("users".into()),
            ..Default::default()
        };
        let info = info(&e);

        // POSIX `%(keyword)s`/`%(keyword)d` substitution.
        assert_eq!(fmt("%(path)s %(size)d", &info), "dir/file.txt 4096");
        assert_eq!(fmt("%(uname)s:%(gname)s", &info), "alice:users");
        // An unknown keyword is echoed verbatim.
        assert_eq!(fmt("%(bogus)s", &info), "%(bogus)s");
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
        let e = ArchiveEntry {
            path: "/dev/sda".into(),
            mode: 0o660,
            uname: Some("root".into()),
            gname: Some("disk".into()),
            entry_type: EntryType::BlockDevice,
            devmajor: 8,
            devminor: 0,
            ..Default::default()
        };
        let result = fmt("%M %D %f", &info(&e));
        assert_eq!(result, "brw-rw---- 8,0 sda");
    }

    #[test]
    fn test_format_list_entry_different_types() {
        // Test that %M correctly uses entry_type for file type character
        // Directory
        let e = ArchiveEntry {
            path: "mydir".into(),
            mode: 0o755,
            entry_type: EntryType::Directory,
            ..Default::default()
        };
        assert_eq!(fmt("%M", &info(&e)), "drwxr-xr-x");

        // Symlink
        let e = ArchiveEntry {
            path: "mylink".into(),
            mode: 0o777,
            link_target: Some("target".into()),
            entry_type: EntryType::Symlink,
            ..Default::default()
        };
        assert_eq!(fmt("%M", &info(&e)), "lrwxrwxrwx");
    }

    /// A member as a ustar reader produces it, with the header-identity
    /// fields a conforming writer puts there.
    fn ustar_entry(path: &str, typeflag: u8) -> ArchiveEntry {
        ArchiveEntry {
            path: path.into(),
            source_header: Some(SourceHeader::Ustar {
                magic: *b"ustar\0",
                version: *b"00",
                chksum: 0o6414,
                typeflag,
            }),
            ..Default::default()
        }
    }

    /// POSIX listopt rule 7 requires every Field Name entry of the ustar
    /// Header Block table as a `%(keyword)`. Eight of them resolved to nothing
    /// and were echoed back as their own specification.
    #[test]
    fn test_keyword_ustar_header_fields() {
        let e = ArchiveEntry {
            mode: 0o644,
            size: 1234,
            uid: 1000,
            gid: 100,
            uname: Some("alice".into()),
            gname: Some("users".into()),
            ..ustar_entry("dir/file.txt", b'0')
        };
        let info = info(&e);

        assert_eq!(fmt("%(magic)s", &info), "ustar");
        assert_eq!(fmt("%(version)s", &info), "00");
        assert_eq!(fmt("%(typeflag)s", &info), "0");
        assert_eq!(fmt("%(chksum)s", &info), "6414");
        assert_eq!(fmt("%(name)s", &info), "dir/file.txt");
        assert_eq!(fmt("%(prefix)s", &info), "");
        assert_eq!(fmt("%(mode)s", &info), "644");
        assert_eq!(fmt("%(uid)s/%(gid)s", &info), "1000/100");
        assert_eq!(fmt("%(size)s", &info), "1234");
        assert_eq!(fmt("%(uname)s:%(gname)s", &info), "alice:users");
        assert_eq!(fmt("%(devmajor)s,%(devminor)s", &info), "0,0");
        assert_eq!(fmt("%(linkname)s", &info), "");
    }

    /// Rule 7 strips trailing NULs from a header field value and nothing else.
    /// GNU tar writes `magic` as "ustar " and `version` as " \0", and trimming
    /// the whitespace too would report a conforming-looking header that is not
    /// what the archive holds.
    #[test]
    fn test_keyword_header_text_strips_nuls_only() {
        let e = ArchiveEntry {
            source_header: Some(SourceHeader::Ustar {
                magic: *b"ustar ",
                version: *b" \0",
                chksum: 0,
                typeflag: b'0',
            }),
            ..ustar_entry("f.txt", b'0')
        };
        assert_eq!(fmt("[%(magic)s]", &info(&e)), "[ustar ]");
        assert_eq!(fmt("[%(version)s]", &info(&e)), "[ ]");

        // A NUL typeflag is the historical spelling of a regular file, and is
        // itself a trailing NUL: it reports as nothing, not as a NUL byte.
        let areg = ustar_entry("f.txt", b'\0');
        assert_eq!(fmt("[%(typeflag)s]", &info(&areg)), "[]");
    }

    /// `name` and `prefix` are the two halves of the ustar spelling of the
    /// pathname, so concatenating them must give the name `%F` prints -- rule
    /// 11 makes `(prefix,name)` the default for `%F` when `path` is undefined.
    #[test]
    fn test_keyword_name_prefix_reconstruct_the_path() {
        // Short enough for the name field alone: prefix is empty.
        let short = ustar_entry("dir/f.txt", b'0');
        assert_eq!(fmt("%(name)s", &info(&short)), "dir/f.txt");
        assert_eq!(fmt("%(prefix)s", &info(&short)), "");

        // Long enough to need the split.
        let long_dir = "d".repeat(110);
        let split = ustar_entry(&format!("{long_dir}/f.txt"), b'0');
        assert_eq!(fmt("%(name)s", &info(&split)), "f.txt");
        assert_eq!(fmt("%(prefix)s", &info(&split)), long_dir);
        assert_eq!(
            fmt("%(prefix)s/%(name)s", &info(&split)),
            fmt("%(path)s", &info(&split))
        );

        // Over-long with no `/` the ustar fields could split at. Neither field
        // can hold this name, but the halves must still rebuild it.
        let huge = "x".repeat(120);
        let unsplittable = ustar_entry(&format!("d/{huge}"), b'0');
        assert_eq!(fmt("%(prefix)s", &info(&unsplittable)), "d");
        assert_eq!(fmt("%(name)s", &info(&unsplittable)), huge);
        assert_eq!(
            fmt("%(prefix)s/%(name)s", &info(&unsplittable)),
            fmt("%(path)s", &info(&unsplittable))
        );

        // No `/` at all: the whole name is the name field.
        let flat = ustar_entry(&"y".repeat(150), b'0');
        assert_eq!(fmt("%(prefix)s", &info(&flat)), "");
        assert_eq!(fmt("%(name)s", &info(&flat)), "y".repeat(150));
    }

    /// The four header-identity keywords describe a ustar header. A cpio
    /// member has none of them, so rule 7 leaves them with no value to report
    /// -- which is not the same as the name being unknown, and must not bring
    /// back the literal echo.
    #[test]
    fn test_keyword_ustar_identity_absent_for_a_cpio_member() {
        let e = ArchiveEntry {
            path: "f.txt".into(),
            source_header: Some(SourceHeader::Cpio {
                format: crate::formats::cpio::CpioFormat::Odc,
            }),
            ..Default::default()
        };
        let info = info(&e);

        assert_eq!(fmt("[%(version)s]", &info), "[]");
        assert_eq!(fmt("[%(chksum)s]", &info), "[]");
        assert_eq!(fmt("[%(typeflag)s]", &info), "[]");
        // `magic` is in both tables, so it answers for either header.
        assert_eq!(fmt("%(magic)s", &info), "070707");
        // And a name in no table still echoes.
        assert_eq!(fmt("%(bogus)s", &info), "%(bogus)s");
    }

    /// `%(mode)` formatted the whole mode word while `%m` masked it, so a
    /// header carrying S_IFMT bits printed `644` and `100644` on one line.
    #[test]
    fn test_keyword_mode_masks_the_file_type_bits() {
        let e = ArchiveEntry {
            mode: 0o100644,
            ..ustar_entry("f.txt", b'0')
        };
        assert_eq!(fmt("%m", &info(&e)), "644");
        assert_eq!(fmt("%(mode)s", &info(&e)), "644");
    }

    /// A member as a cpio reader produces it: the flavor recorded, the mode
    /// already stripped of its file type bits (`C_PERM_MASK`).
    fn cpio_entry(path: &str, format: crate::formats::cpio::CpioFormat) -> ArchiveEntry {
        ArchiveEntry {
            path: path.into(),
            source_header: Some(SourceHeader::Cpio { format }),
            ..Default::default()
        }
    }

    /// POSIX listopt rule 7 requires every Field Name entry of the
    /// Octet-Oriented cpio Archive Entry table as a `%(keyword)`, and permits
    /// the same names without the leading `c_`. None of them resolved.
    #[test]
    fn test_keyword_cpio_header_fields() {
        use crate::formats::cpio::CpioFormat;
        let e = ArchiveEntry {
            mode: 0o644,
            uid: 1000,
            gid: 100,
            size: 1234,
            mtime: 99,
            dev: 8,
            ino: 64,
            nlink: 9,
            ..cpio_entry("a/b.txt", CpioFormat::Odc)
        };
        let info = info(&e);

        assert_eq!(fmt("%(c_magic)s", &info), "070707");
        assert_eq!(fmt("%(c_dev)s", &info), "8");
        assert_eq!(fmt("%(c_ino)s", &info), "64");
        assert_eq!(fmt("%(c_nlink)s", &info), "9");
        assert_eq!(fmt("%(c_uid)s/%(c_gid)s", &info), "1000/100");
        assert_eq!(fmt("%(c_mtime)s", &info), "99");
        assert_eq!(fmt("%(c_filesize)s", &info), "1234");
        assert_eq!(fmt("%(c_name)s", &info), "a/b.txt");
        // c_namesize counts the NUL stored after the pathname.
        assert_eq!(fmt("%(c_namesize)s", &info), "8");
        // c_mode carries the file type over the permission bits, which is the
        // one place the full mode word stays reachable.
        assert_eq!(fmt("%(c_mode)s", &info), "100644");
        // c_filedata is the contents, not a value a listing reports.
        assert_eq!(fmt("[%(c_filedata)s]", &info), "[]");

        // The unprefixed spellings rule 7 permits, for the names no other
        // table claims.
        assert_eq!(
            fmt("%(dev)s|%(ino)s|%(nlink)s|%(namesize)s|%(filesize)s", &info),
            "8|64|9|8|1234"
        );
    }

    /// Each cpio flavor reports the `c_magic` it identifies itself with. ODC
    /// and the old binary format share "070707", which is why the entry
    /// records the flavor rather than the digits.
    #[test]
    fn test_keyword_cpio_magic_per_flavor() {
        use crate::formats::cpio::CpioFormat;
        for (format, magic) in [
            (CpioFormat::Odc, "070707"),
            (CpioFormat::Newc, "070701"),
            (CpioFormat::NewcCrc, "070702"),
            (CpioFormat::Binary, "070707"),
        ] {
            let e = cpio_entry("f", format);
            assert_eq!(fmt("%(c_magic)s", &info(&e)), magic, "{:?}", format);
            // `magic` is in both tables, so it answers for a cpio header too.
            assert_eq!(fmt("%(magic)s", &info(&e)), magic, "{:?}", format);
        }
    }

    /// `c_dev`, `c_ino` and `c_nlink` have no ustar counterpart, so a tar
    /// member has no value to report -- which must stay distinct from the name
    /// being unknown. An unprefixed cpio alias must not claim a name the ustar
    /// or pax table owns, and a `c_`-prefixed typo must still echo.
    #[test]
    fn test_keyword_cpio_fields_absent_for_a_ustar_member() {
        let e = ArchiveEntry {
            mode: 0o644,
            size: 7,
            ..ustar_entry("f.txt", b'0')
        };
        let info = info(&e);

        assert_eq!(fmt("[%(c_dev)s%(c_ino)s%(c_nlink)s]", &info), "[]");
        // Derivable from what the entry already holds, so these still answer.
        assert_eq!(fmt("%(c_mode)s", &info), "100644");
        assert_eq!(fmt("%(c_filesize)s", &info), "7");
        assert_eq!(fmt("%(c_namesize)s", &info), "6");
        // `mode` keeps its ustar reading; only `c_mode` carries the type bits.
        assert_eq!(fmt("%(mode)s", &info), "644");
        // A typo in either spelling is echoed, not silently empty.
        assert_eq!(fmt("%(c_bogus)s", &info), "%(c_bogus)s");
        assert_eq!(fmt("%(filedata)s", &info), "");
        assert_eq!(fmt("%(bogus)s", &info), "%(bogus)s");
    }

    /// cpio packs a device number into one c_rdev field, and stores a symbolic
    /// link's target as the member's data -- so c_filesize counts the target,
    /// while the pax `size` keyword reports the entry's own zero.
    #[test]
    fn test_keyword_cpio_rdev_and_symlink_filesize() {
        use crate::formats::cpio::CpioFormat;
        let dev = ArchiveEntry {
            mode: 0o660,
            entry_type: EntryType::CharDevice,
            devmajor: 8,
            devminor: 0,
            ..cpio_entry("chr", CpioFormat::Odc)
        };
        assert_eq!(fmt("%(c_rdev)s", &info(&dev)), "4000");
        assert_eq!(fmt("%(devmajor)s,%(devminor)s", &info(&dev)), "8,0");
        assert_eq!(fmt("%D", &info(&dev)), "8,0");

        // Not a device: the field is zero, as the writer records it.
        let plain = cpio_entry("f", CpioFormat::Odc);
        assert_eq!(fmt("%(c_rdev)s", &info(&plain)), "0");

        let link = ArchiveEntry {
            entry_type: EntryType::Symlink,
            link_target: Some("target".into()),
            ..cpio_entry("l", CpioFormat::Odc)
        };
        assert_eq!(fmt("%(size)s", &info(&link)), "0");
        assert_eq!(fmt("%(c_filesize)s", &info(&link)), "6");
        assert_eq!(fmt("%(linkname)s", &info(&link)), "target");
        assert_eq!(fmt("%(linkpath)s", &info(&link)), "target");

        // A hard link also carries a target, but cpio stores it as a regular
        // file with its own contents, so c_filesize is the size.
        let hard = ArchiveEntry {
            entry_type: EntryType::Hardlink,
            link_target: Some("original.txt".into()),
            size: 42,
            ..cpio_entry("h", CpioFormat::Odc)
        };
        assert_eq!(fmt("%(c_filesize)s", &info(&hard)), "42");
    }

    /// POSIX listopt rule 7 requires every pax extended-header keyword as a
    /// `%(keyword)`, and names `"%(charset)s"` as its own example. `charset`,
    /// `hdrcharset` and `comment` resolved to nothing: the reader dropped the
    /// records on the way to the entry, so the listing could not see them.
    #[test]
    fn test_keyword_pax_extended_header_records() {
        let mut e = ustar_entry("f.txt", b'0');
        e.set_ext_record("charset", "ISO-IR 10646 2000 UTF-8");
        e.set_ext_record("hdrcharset", "BINARY");
        e.set_ext_record("comment", "written by hand");
        // Rule 7's third bullet: an implementation extension is a keyword too.
        e.set_ext_record("SCHILY.fflags", "nodump");
        let info = info(&e);

        assert_eq!(fmt("%(charset)s", &info), "ISO-IR 10646 2000 UTF-8");
        assert_eq!(fmt("%(hdrcharset)s", &info), "BINARY");
        assert_eq!(fmt("%(comment)s", &info), "written by hand");
        assert_eq!(fmt("%(SCHILY.fflags)s", &info), "nodump");
    }

    /// A member that declared none of them must report nothing, not POSIX's
    /// implicit UTF-8 default: an operator runs `%(hdrcharset)s` to find out
    /// whether a record was there, and synthesizing the default would make
    /// "declared UTF-8" and "declared nothing" indistinguishable. A keyword in
    /// no table at all still echoes, so a typo stays visible.
    #[test]
    fn test_keyword_pax_records_absent_but_not_unknown() {
        let e = ustar_entry("f.txt", b'0');
        let info = info(&e);

        assert_eq!(fmt("[%(charset)s]", &info), "[]");
        assert_eq!(fmt("[%(hdrcharset)s]", &info), "[]");
        assert_eq!(fmt("[%(comment)s]", &info), "[]");
        assert_eq!(fmt("%(SCHILY.fflags)s", &info), "%(SCHILY.fflags)s");
        assert_eq!(fmt("%(bogus)s", &info), "%(bogus)s");
    }

    /// An extension record must not be able to redefine a required keyword: a
    /// crafted archive recording `mode=rwx` has to leave `%(mode)s` reporting
    /// the header field, which is why the extension lookup comes last.
    #[test]
    fn test_keyword_extension_record_cannot_shadow_a_required_name() {
        let mut e = ArchiveEntry {
            mode: 0o644,
            ..ustar_entry("f.txt", b'0')
        };
        e.set_ext_record("mode", "rwx");
        e.set_ext_record("typeflag", "9");
        let info = info(&e);

        assert_eq!(fmt("%(mode)s", &info), "644");
        assert_eq!(fmt("%(typeflag)s", &info), "0");
    }

    #[test]
    fn test_strftime_or_secs_subformat() {
        // The exact value is timezone-dependent (strftime via localtime_r), so
        // assert the shape the subformat asks for rather than a fixed UTC
        // instant. An ISO 8601 layout is now reachable as a `T` subformat.
        let result = strftime_or_secs(1704067200, "%Y-%m-%dT%H:%M:%S");
        assert_eq!(result.len(), 19, "unexpected ISO length: {result}");
        assert_eq!(&result[4..5], "-");
        assert_eq!(&result[7..8], "-");
        assert_eq!(&result[10..11], "T");
        assert!(
            result.as_bytes()[..4].iter().all(|b| b.is_ascii_digit()),
            "year should be digits: {result}"
        );

        // The POSIX default subformat always carries a four-digit year.
        let default = strftime_or_secs(1704067200, DEFAULT_TIME_SUBFORMAT);
        assert!(
            default.ends_with("2023") || default.ends_with("2024"),
            "default subformat must end in the year: {default}"
        );
    }
}
