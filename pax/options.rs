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
//!
//! An option-argument is bytes. Keywords are text, but a value can be a
//! pathname (`path:=`), a name template or a listopt literal, none of which
//! need be UTF-8, so values are kept as the bytes given.

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

/// The `hdrcharset` values POSIX defines, in the spelling it defines them in.
///
/// The spec allows more -- "additional names may be agreed between the
/// originator and the recipient" -- but agreeing on a name is not the same as
/// being able to encode to it, and pax performs no character-set conversion.
/// Accepting a third name would write an archive declaring an encoding its
/// values are not in, so a name outside this table is refused by name.
///
/// `charset` is deliberately not checked the same way. POSIX scopes it to the
/// *file data*, says it "is included in an extended header for information
/// only" and forbids pax translating the data, so the value is an annotation
/// pax neither acts on nor needs to understand; refusing an unlisted one would
/// reject a conforming archive to no end.
/// POSIX's name for an unencoded header, as a constant because the writer
/// branches on it as well as recording it.
pub const BINARY_CHARSET: &str = "BINARY";

const HDRCHARSET_VALUES: &[&str] = &["ISO-IR 10646 2000 UTF-8", BINARY_CHARSET];

/// Check a `-o hdrcharset=` value and return POSIX's spelling of it, so that
/// an archive records `BINARY` whichever case the operator typed.
fn canonical_hdrcharset(value: &str) -> PaxResult<&'static str> {
    HDRCHARSET_VALUES
        .iter()
        .copied()
        .find(|known| known.eq_ignore_ascii_case(value))
        .ok_or_else(|| {
            PaxError::InvalidFormat(format!(
                "hdrcharset={value} is not supported; pax performs no \
                 character-set conversion, so the header encoding has to be \
                 {}",
                HDRCHARSET_VALUES.join(" or ")
            ))
        })
}

/// Parsed format options
#[derive(Debug, Clone, Default)]
pub struct FormatOptions {
    /// Global options (keyword=value)
    global: HashMap<String, Vec<u8>>,
    /// Per-file options (keyword:=value) - for future pax format support
    per_file: HashMap<String, Vec<u8>>,
    /// List format specification (listopt=format)
    pub list_format: Option<Vec<u8>>,
    /// Delete patterns (delete=pattern) - for future pax format support
    pub delete_patterns: Vec<Vec<u8>>,
    /// Pre-compiled delete patterns for efficient matching
    delete_patterns_compiled: Vec<Pattern>,
    /// Times option (include atime/mtime -- and, as an extension, ctime -- in
    /// extended headers)
    pub include_times: bool,
    /// Linkdata option (write contents for hard links)
    pub link_data: bool,
    /// Extended header name template (exthdr.name)
    /// Default: "%d/PaxHeaders.%p/%f"
    pub exthdr_name: Option<Vec<u8>>,
    /// Global extended header name template (globexthdr.name)
    /// Default: "$TMPDIR/GlobalHead.%p.%n"
    pub globexthdr_name: Option<Vec<u8>>,
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
        options.parse_into(input.as_bytes())?;
        Ok(options)
    }

    /// Parse options and merge into existing options
    ///
    /// Later options take precedence over earlier ones.
    pub fn parse_into(&mut self, input: &[u8]) -> PaxResult<()> {
        // Per POSIX, `listopt` is the final <comma>-separated keyword: "all
        // characters in the remainder of the option-argument shall be
        // considered part of the format string" -- commas and trailing blanks
        // included. Split it off before the comma tokenizer can break it
        // apart; the input is not trimmed for the same reason.
        let Some(marker) = find_listopt_marker(input) else {
            return self.parse_comma_list(input);
        };
        // Only the one comma that separates the tail goes: an escaped comma
        // ending the keyword before it is part of that keyword's value.
        let before = input[..marker].trim_ascii_end();
        self.parse_comma_list(before.strip_suffix(b",").unwrap_or(before))?;
        let format = decode_printf_escapes(&input[marker + b"listopt=".len()..]);
        // "When multiple -o listopt=format options are specified, the format
        // strings shall be considered a single, concatenated string, evaluated
        // in command-line order."
        self.list_format
            .get_or_insert_with(Vec::new)
            .extend_from_slice(&format);
        Ok(())
    }

    /// Parse a sequence of comma-separated options (backslash escapes a comma).
    fn parse_comma_list(&mut self, input: &[u8]) -> PaxResult<()> {
        let mut current = Vec::new();
        let mut escaped = false;

        for &b in input {
            if escaped {
                current.push(b);
                escaped = false;
            } else if b == b'\\' {
                escaped = true;
            } else if b == b',' {
                self.parse_single_option(current.trim_ascii())?;
                current.clear();
            } else {
                current.push(b);
            }
        }

        let final_opt = current.trim_ascii();
        if !final_opt.is_empty() {
            self.parse_single_option(final_opt)?;
        }

        Ok(())
    }

    /// Parse a single option (keyword[[:]=value])
    fn parse_single_option(&mut self, opt: &[u8]) -> PaxResult<()> {
        use KnownOption::*;

        if opt.is_empty() {
            return Ok(());
        }

        let (keyword, value, is_per_file) = split_option(opt)?;

        // Checked here rather than through KNOWN_OPTIONS, because the value
        // still has to reach the `g`/`x` extended header like any other
        // keyword: intercepting it below would stop the record being written
        // at all. An empty value is POSIX's deletion form ("If the <value>
        // field is zero length, it shall delete any ... previously entered
        // extended header value"), so it is left alone.
        let value = match (keyword, value) {
            ("hdrcharset", Some(v)) if !v.is_empty() => {
                Some(canonical_hdrcharset(&String::from_utf8_lossy(v))?.as_bytes())
            }
            _ => value,
        };

        // Look up keyword in known options table
        if let Some((_, known_opt)) = KNOWN_OPTIONS.iter().find(|(k, _)| *k == keyword) {
            match known_opt {
                Times => self.include_times = true,
                LinkData => self.link_data = true,
                ListFormat => self.list_format = value.map(<[u8]>::to_vec),
                ExthdrName => self.exthdr_name = value.map(<[u8]>::to_vec),
                GlobexthdrName => self.globexthdr_name = value.map(<[u8]>::to_vec),
                Delete => {
                    if let Some(pattern) = value {
                        self.delete_patterns.push(pattern.to_vec());
                        self.delete_patterns_compiled.push(Pattern::new(pattern));
                    }
                }
                Invalid => {
                    if let Some(v) = value {
                        match INVALID_ACTIONS
                            .iter()
                            .find(|(name, _)| name.as_bytes() == v)
                        {
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
                                    String::from_utf8_lossy(v)
                                )))
                            }
                        }
                    }
                }
            }
        } else {
            // Store unknown options for format-specific handling
            let map = if is_per_file {
                &mut self.per_file
            } else {
                &mut self.global
            };
            map.insert(keyword.to_string(), value.unwrap_or_default().to_vec());
        }

        Ok(())
    }

    /// Get a per-file option value
    #[cfg(test)]
    pub fn get_per_file(&self, key: &str) -> Option<&Vec<u8>> {
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
            .any(|pattern| pattern.matches(keyword.as_bytes()))
    }

    /// The header character set the operator asked for, if any.
    ///
    /// The per-file `hdrcharset:=` form wins over the global `hdrcharset=`
    /// one, which is POSIX's keyword precedence. Both are already written as
    /// extended-header records by the writer; this is for the decisions that
    /// depend on the answer rather than for emitting it.
    pub fn hdrcharset(&self) -> Option<&str> {
        self.per_file
            .get("hdrcharset")
            .or_else(|| self.global.get("hdrcharset"))
            .and_then(|value| std::str::from_utf8(value).ok())
            .filter(|value| !value.is_empty())
    }

    /// Get the global options map for extended header generation
    pub fn global_options(&self) -> &HashMap<String, Vec<u8>> {
        &self.global
    }

    /// Get the per-file options map for extended header generation
    pub fn per_file_options(&self) -> &HashMap<String, Vec<u8>> {
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
    pub fn expand_exthdr_name(&self, path: &std::path::Path, sequence: u64) -> Vec<u8> {
        let template = self
            .exthdr_name
            .as_deref()
            .unwrap_or(b"%d/PaxHeaders.%p/%f");

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
    pub fn expand_globexthdr_name(&self, sequence: u64) -> Vec<u8> {
        let tmpdir = std::env::var_os("TMPDIR");
        let tmpdir = tmpdir
            .as_deref()
            .map(std::os::unix::ffi::OsStrExt::as_bytes);
        let default_template = default_globexthdr_template(tmpdir);
        let template = self.globexthdr_name.as_deref().unwrap_or(&default_template);

        expand_global_header_template(template, sequence)
    }
}

/// Split one `keyword[[:]=value]` option into its keyword, its value and
/// whether it is the per-file `:=` form.
///
/// The keyword ends at the first `=`; a `:` just before it makes the option
/// per-file. Any `:=` after that is part of the value, so `comment=a:=b` is
/// the global keyword `comment` with the value `a:=b`.
fn split_option(opt: &[u8]) -> PaxResult<(&str, Option<&[u8]>, bool)> {
    let (keyword, value, is_per_file) = match opt.iter().position(|&b| b == b'=') {
        Some(pos) => {
            let value = Some(opt[pos + 1..].trim_ascii());
            match opt[..pos].strip_suffix(b":") {
                Some(keyword) => (keyword, value, true),
                None => (&opt[..pos], value, false),
            }
        }
        // Boolean keyword with no value
        None => (opt, None, false),
    };
    let keyword = std::str::from_utf8(keyword.trim_ascii()).map_err(|_| {
        PaxError::InvalidFormat(format!(
            "-o keyword '{}' is not valid UTF-8",
            String::from_utf8_lossy(keyword)
        ))
    })?;
    Ok((keyword, value, is_per_file))
}

/// Build the default `globexthdr.name` template from `$TMPDIR` (or `/tmp`).
fn default_globexthdr_template(tmpdir: Option<&[u8]>) -> Vec<u8> {
    let dir = tmpdir.unwrap_or(b"/tmp");
    let mut template = trim_trailing_slashes(dir).to_vec();
    template.extend_from_slice(b"/GlobalHead.%p.%n");
    template
}

/// Context for template expansion
struct TemplateContext<'a> {
    dirname: Option<&'a [u8]>,
    filename: Option<&'a [u8]>,
    pid: u32,
    sequence: u64,
}

/// Type alias for template specifier handler
type TemplateHandler = fn(&TemplateContext) -> Option<Vec<u8>>;

/// Template specifier handlers
const TEMPLATE_SPECIFIERS: &[(u8, TemplateHandler)] = &[
    (b'd', |ctx| ctx.dirname.map(<[u8]>::to_vec)),
    (b'f', |ctx| ctx.filename.map(<[u8]>::to_vec)),
    (b'p', |ctx| Some(ctx.pid.to_string().into_bytes())),
    (b'n', |ctx| Some(ctx.sequence.to_string().into_bytes())),
    (b'%', |_| Some(b"%".to_vec())),
];

/// Unified template expansion function. A name is bytes, and so is the
/// template that builds one.
fn expand_template(template: &[u8], ctx: &TemplateContext) -> Vec<u8> {
    let mut result = Vec::new();
    let mut bytes = template.iter().copied();

    while let Some(b) = bytes.next() {
        if b != b'%' {
            result.push(b);
            continue;
        }
        let Some(spec) = bytes.next() else {
            result.push(b'%');
            break;
        };
        let value = TEMPLATE_SPECIFIERS
            .iter()
            .find(|(ch, _)| *ch == spec)
            .and_then(|(_, handler)| handler(ctx));
        match value {
            Some(value) => result.extend_from_slice(&value),
            // Unknown, or not available in this context: included literally.
            None => result.extend_from_slice(&[b'%', spec]),
        }
    }

    result
}

/// Expand template for per-file extended header names
fn expand_header_template(template: &[u8], path: &std::path::Path, sequence: u64) -> Vec<u8> {
    let path = crate::rawpath::as_bytes(path);
    let ctx = TemplateContext {
        dirname: Some(dirname(path)),
        filename: Some(basename(path)),
        pid: std::process::id(),
        sequence,
    };

    expand_template(template, &ctx)
}

/// What dirname(1) gives for `path`, which is what POSIX defines `%d` as.
///
/// In particular "." for a name with no directory part. `Path::parent` gives
/// "" there instead, which turned the default "%d/PaxHeaders.%p/%f" into an
/// absolute name for every top-level member -- one a reader that ignores
/// extended headers would extract at the root of the file system.
fn dirname(path: &[u8]) -> &[u8] {
    let trimmed = trim_trailing_slashes(path);
    match trimmed.iter().rposition(|&b| b == b'/') {
        None if trimmed.is_empty() && !path.is_empty() => b"/",
        None => b".",
        Some(i) => match trim_trailing_slashes(&trimmed[..i]) {
            b"" => b"/",
            dir => dir,
        },
    }
}

/// What basename(1) gives for `path`, which is what POSIX defines `%f` as.
fn basename(path: &[u8]) -> &[u8] {
    let trimmed = trim_trailing_slashes(path);
    match trimmed.iter().rposition(|&b| b == b'/') {
        Some(i) => &trimmed[i + 1..],
        None if trimmed.is_empty() && !path.is_empty() => b"/",
        None => trimmed,
    }
}

/// `path` without trailing slashes -- unless it is nothing but slashes, which
/// dirname and basename both treat as "/".
fn trim_trailing_slashes(path: &[u8]) -> &[u8] {
    let end = path.iter().rposition(|&b| b != b'/').map_or(0, |i| i + 1);
    &path[..end]
}

/// Expand template for global extended header names
fn expand_global_header_template(template: &[u8], sequence: u64) -> Vec<u8> {
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

/// Bare `%L`. Rule 12: a symbolic link expands to `"%s -> %s"` of a pathname
/// and the link's contents, and anything else is "the equivalent of %F". With
/// no keyword given, the pathname is rule 11's `(path)` default.
fn fmt_link_expansion(info: &ListEntryInfo) -> Vec<u8> {
    let mut out = fmt_fullpath(info);
    push_link_expansion(&mut out, info);
    out
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
    match info.entry.uname.as_deref() {
        Some(uname) => escaped(info, uname),
        None => info.entry.uid.to_string().into_bytes(),
    }
}

fn fmt_groupname(info: &ListEntryInfo) -> Vec<u8> {
    match info.entry.gname.as_deref() {
        Some(gname) => escaped(info, gname),
        None => info.entry.gid.to_string().into_bytes(),
    }
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
    ('L', fmt_link_expansion),
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
fn find_listopt_marker(input: &[u8]) -> Option<usize> {
    let mut boundaries = vec![0usize];
    let mut escaped = false;
    for (i, &b) in input.iter().enumerate() {
        if escaped {
            escaped = false;
        } else if b == b'\\' {
            escaped = true;
        } else if b == b',' {
            boundaries.push(i + 1);
        }
    }
    for b in boundaries {
        let rest = &input[b..];
        let start = b + (rest.len() - rest.trim_ascii_start().len());
        if input[start..].starts_with(b"listopt=") {
            return Some(start);
        }
    }
    None
}

/// Decode the backslash escapes of a listopt format, which POSIX makes a
/// `printf` format: `\\`, `\a`, `\b`, `\f`, `\n`, `\r`, `\t` and `\v` are the
/// characters XBD File Format Notation gives them, and `\ddd` -- one to three
/// octal digits -- is the byte with that value, as in `printf`. Before any
/// other character the backslash is dropped, so `\,` is a comma, as it is
/// elsewhere in `-o`.
fn decode_printf_escapes(s: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(s.len());
    let mut i = 0;
    while i < s.len() {
        let b = s[i];
        i += 1;
        if b != b'\\' {
            out.push(b);
            continue;
        }
        let Some(&next) = s.get(i) else {
            out.push(b);
            break;
        };
        let octal = s[i..]
            .iter()
            .take(3)
            .take_while(|d| matches!(d, b'0'..=b'7'))
            .count();
        if octal > 0 {
            // Like printf, a value past 0377 keeps its low eight bits.
            let value = s[i..i + octal]
                .iter()
                .fold(0u32, |v, d| v * 8 + u32::from(d - b'0'));
            out.push(value as u8);
            i += octal;
            continue;
        }
        i += 1;
        out.push(match next {
            b'a' => 0x07,
            b'b' => 0x08,
            b'f' => 0x0c,
            b'n' => b'\n',
            b'r' => b'\r',
            b't' => b'\t',
            b'v' => 0x0b,
            other => other,
        });
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
/// - `%F` - pathname, the non-null `(keyword[,keyword]...)` values joined by `/`
/// - `%L` - a symbolic link as `pathname -> contents`, otherwise `%F`
/// - `%u` - owner username
/// - `%g` - group name
/// - `%U` - owner uid
/// - `%G` - group gid
/// - `%n` - newline
/// - `%%` - literal %
pub fn format_list_entry(format: &[u8], info: &ListEntryInfo) -> Vec<u8> {
    let mut result: Vec<u8> = Vec::new();
    let mut bytes = format.iter().copied().peekable();

    while let Some(b) = bytes.next() {
        if b == b'%' {
            if let Some(spec) = parse_format_specifier(&mut bytes) {
                result.extend_from_slice(&format_with_spec(info, spec));
            } else {
                result.push(b'%');
            }
        } else {
            // The format string is operator-supplied; its literal bytes go
            // out as given.
            result.push(b);
        }
    }

    result
}

/// The bytes of a listopt format, as `format_list_entry` reads them.
type FormatBytes<'a> = std::iter::Peekable<std::iter::Copied<std::slice::Iter<'a, u8>>>;

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
    /// The conversion character. A byte, since a format need not be UTF-8;
    /// every conversion is ASCII.
    spec: u8,
    /// POSIX `%(keyword)X` extended-header keyword (with an optional `=subformat`
    /// tail used by the `T` time conversion).
    keyword: Option<String>,
}

fn parse_format_specifier(bytes: &mut FormatBytes) -> Option<FormatSpec> {
    let mut spec = FormatSpec::default();

    while bytes.next_if_eq(&b'-').is_some() {
        spec.left_justify = true;
    }

    spec.width = parse_number(bytes).map(|w| w.min(MAX_FORMAT_FIELD_SIZE));

    if bytes.next_if_eq(&b'.').is_some() {
        spec.precision = parse_number(bytes)
            .map(|p| p.min(MAX_FORMAT_FIELD_SIZE))
            .or(Some(0));
    }

    // POSIX keyword substitution: `%(keyword)s`, `%(mtime=%Y)T`, etc. A
    // keyword is text; only a time subformat could be anything else.
    if bytes.next_if_eq(&b'(').is_some() {
        let keyword: Vec<u8> = bytes.by_ref().take_while(|&b| b != b')').collect();
        spec.keyword = Some(String::from_utf8_lossy(&keyword).into_owned());
    }

    spec.spec = bytes.next()?;
    Some(spec)
}

fn parse_number(bytes: &mut FormatBytes) -> Option<usize> {
    let mut value: usize = 0;
    let mut seen = false;

    while let Some(b) = bytes.next_if(u8::is_ascii_digit) {
        seen = true;
        value = value
            .saturating_mul(10)
            .saturating_add(usize::from(b - b'0'));
    }

    if seen {
        Some(value)
    } else {
        None
    }
}

fn format_with_spec(info: &ListEntryInfo, spec: FormatSpec) -> Vec<u8> {
    let conversion = char::from(spec.spec);
    let mut rendered = if let Some(ref keyword) = spec.keyword {
        match keyword_value(info, keyword, conversion) {
            KeywordValue::Value(v) => v,
            KeywordValue::Absent => Vec::new(),
            KeywordValue::Unknown => [format!("%({})", keyword).as_bytes(), &[spec.spec]].concat(),
        }
    } else if let Some((_, handler)) = FORMAT_SPECIFIERS.iter().find(|(ch, _)| *ch == conversion) {
        handler(info)
    } else {
        vec![b'%', spec.spec]
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
fn time_keyword(info: &ListEntryInfo, keyword: &str) -> Option<Option<i64>> {
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

/// A bitfield's value, in the radix it is read in. Reserved for the three
/// fields whose only meaning is as stored bits: a mode, a header checksum, and
/// `c_rdev`, which packs a major and minor into one number that reads as
/// nothing else (8,0 is 4000).
///
/// Not `c_dev`, despite that field also holding a packed value on some cpio
/// flavors. POSIX pairs `c_dev` with `c_ino` as "values that uniquely identify
/// the file within the archive ... determined in an unspecified manner", so it
/// is an opaque identifier rather than a device, and printing one of a pair in
/// octal and the other in decimal would be the worse inconsistency.
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
    // field can hold it. Split at the last `/` anyway, so that rule 11's
    // `(prefix,name)` still concatenates back to the pathname.
    //
    // Not when that `/` is the leading one: the prefix would be empty, and
    // rule 11 joins only "the keywords that are non-null", so the separator
    // would be dropped along with it and an absolute name would come back
    // relative. The whole name goes in `name` instead.
    match path.iter().rposition(|&b| b == b'/') {
        Some(i) if i > 0 => (&path[i + 1..], &path[..i]),
        _ => (path, b""),
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
        "uname" => Field::Value(escaped(info, info.entry.uname.as_deref().unwrap_or(b""))),
        "gname" => Field::Value(escaped(info, info.entry.gname.as_deref().unwrap_or(b""))),
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
        // Only `c_magic` reaches here: the unprefixed `magic` is the ustar
        // table's name, claimed by `ustar_keyword`, and answers from either
        // header. `c_magic` names the cpio field specifically, so a ustar
        // member has none -- the same reading as c_dev and c_ino below.
        "magic" if cpio => return Some(header_magic(info)),
        "magic" => Field::Absent,
        "dev" if cpio => fmt_decimal(info.entry.dev),
        "ino" if cpio => fmt_decimal(info.entry.ino),
        "nlink" if cpio => fmt_decimal(info.entry.nlink as u64),
        "dev" | "ino" | "nlink" => Field::Absent,
        "mode" => fmt_octal(
            crate::formats::cpio::cpio_mode(info.entry.mode, info.entry.entry_type) as u64,
        ),
        "uid" => fmt_decimal(info.entry.uid as u64),
        "gid" => fmt_decimal(info.entry.gid as u64),
        "mtime" => Field::Value(info.entry.mtime.to_string().into_bytes()),
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

    // Rules 11 and 12: `F` names a pathname assembled from a list of keywords,
    // and `L` renders a symbolic link in terms of that pathname. Both may name
    // more than one keyword, so they resolve the field themselves.
    if matches!(conversion, 'F' | 'L') {
        let mut rendered = match path_conversion(info, field.trim()) {
            KeywordValue::Value(v) => v,
            other => return other,
        };
        if conversion == 'L' {
            push_link_expansion(&mut rendered, info);
        }
        return KeywordValue::Value(rendered);
    }

    let value = match keyword_field(info, keyword) {
        Some(Field::Value(v)) => v,
        Some(Field::Absent) => return KeywordValue::Absent,
        None => return KeywordValue::Unknown,
    };

    // The mode conversion describes how to render the entry rather than which
    // field to read, so it still applies when a keyword was given.
    let rendered = match conversion {
        'M' => format_mode_symbolic(info.entry.mode, info.entry.entry_type).into_bytes(),
        // Rule 10: D names the device of a block/character special file. When
        // that does not apply and a keyword was given, it degrades to
        // `%(keyword)u` -- so `%(size)D` on a regular file prints the size.
        'D' if is_device(info) => fmt_device(info),
        'D' => value,
        _ => value,
    };
    KeywordValue::Value(rendered)
}

/// Resolve one keyword against rule 7's keyword tables, in the order it lists
/// them.
///
/// The pax table comes first because it owns the required reading of the names
/// the tables share -- `size` and `uid` are decimal there and octal in the
/// ustar header -- so an existing format string keeps its answer. Extension
/// records come last, so a crafted archive cannot redefine a required name.
fn keyword_field(info: &ListEntryInfo, keyword: &str) -> Option<Field> {
    if let Some(seconds) = time_keyword(info, keyword) {
        return Some(match seconds {
            Some(secs) => Field::Value(secs.to_string().into_bytes()),
            None => Field::Absent,
        });
    }
    pax_keyword(info, keyword)
        .or_else(|| ustar_keyword(info, keyword))
        .or_else(|| cpio_keyword(info, keyword))
        .or_else(|| ext_record(info, keyword))
}

/// Rule 11's `F` conversion: "The values for all the keywords that are
/// non-null shall be concatenated together, each separated by a '/'."
///
/// So `%(prefix,name)F` rebuilds a pathname whose ustar spelling needed both
/// halves, and a half this member does not carry drops out rather than leaving
/// a stray separator.
fn path_conversion(info: &ListEntryInfo, field: &str) -> KeywordValue {
    let mut parts: Vec<Vec<u8>> = Vec::new();
    for keyword in field.split(',') {
        match keyword_field(info, keyword.trim()) {
            Some(Field::Value(value)) if !value.is_empty() => parts.push(value),
            Some(_) => {}
            None => return KeywordValue::Unknown,
        }
    }
    KeywordValue::Value(parts.join(&b'/'))
}

/// The ` -> target` a symbolic link's rule 12 expansion ends with, appended to
/// `out`. Nothing for anything that is not a symbolic link, which is that
/// rule's fallback to `F`.
fn push_link_expansion(out: &mut Vec<u8>, info: &ListEntryInfo) {
    if info.entry.entry_type != EntryType::Symlink {
        return;
    }
    if let Some(target) = info.entry.link_target.as_deref() {
        out.extend_from_slice(b" -> ");
        out.extend_from_slice(&escaped(info, crate::rawpath::as_bytes(target)));
    }
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
pub(crate) fn format_time_traditional(mtime: i64) -> String {
    // POSIX `ls -l`-style time, formatted via libc strftime (localtime_r), so TZ
    // and LC_TIME take effect: date+time when recent, date+year otherwise.
    use std::time::{SystemTime, UNIX_EPOCH};

    let now_secs = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0);
    let age = now_secs - mtime;
    let six_months: i64 = 180 * 24 * 60 * 60;
    let recent = (0..six_months).contains(&age);

    let fmt = if recent { "%b %e %H:%M" } else { "%b %e  %Y" };
    plib::locale::strftime(fmt, mtime).unwrap_or_else(|_| mtime.to_string())
}

/// The default subformat of the listopt `T` conversion (POSIX pax EXTENDED
/// DESCRIPTION, rule 8). Unlike `ls -l`, it is fixed: the year is always shown
/// and never traded against the time of day.
const DEFAULT_TIME_SUBFORMAT: &str = "%b %e %H:%M %Y";

/// Render `secs` through `subformat`, TZ- and LC_TIME-aware via strftime,
/// falling back to the raw seconds if the subformat cannot be rendered.
fn strftime_or_secs(secs: i64, subformat: &str) -> String {
    plib::locale::strftime(subformat, secs).unwrap_or_else(|_| secs.to_string())
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
        assert_eq!(opts.list_format.as_deref(), Some(&b"%F %s"[..]));
    }

    #[test]
    fn test_parse_multiple_options() {
        let opts = FormatOptions::parse("times,linkdata,listopt=%F").unwrap();
        assert!(opts.include_times);
        assert!(opts.link_data);
        assert_eq!(opts.list_format.as_deref(), Some(&b"%F"[..]));
    }

    #[test]
    fn test_parse_delete_patterns() {
        let opts = FormatOptions::parse("delete=*.tmp,delete=*.bak").unwrap();
        assert_eq!(opts.delete_patterns.len(), 2);
        assert!(opts.delete_patterns.contains(&b"*.tmp".to_vec()));
        assert!(opts.delete_patterns.contains(&b"*.bak".to_vec()));
    }

    /// The keyword ends at the first `=`; a later `:=` is value.
    #[test]
    fn test_split_option_at_first_equals() {
        let opts = FormatOptions::parse("comment=a:=b").unwrap();
        assert_eq!(
            opts.global.get("comment").map(Vec::as_slice),
            Some(&b"a:=b"[..])
        );
        let opts = FormatOptions::parse("comment:=a=b").unwrap();
        assert_eq!(opts.get_per_file("comment"), Some(&b"a=b".to_vec()));
    }

    #[test]
    fn test_parse_per_file_option() {
        let opts = FormatOptions::parse("gname:=mygroup").unwrap();
        assert_eq!(opts.get_per_file("gname"), Some(&b"mygroup".to_vec()));
    }

    #[test]
    fn test_parse_escaped_comma() {
        let opts = FormatOptions::parse(r"listopt=a\,b").unwrap();
        assert_eq!(opts.list_format.as_deref(), Some(&b"a,b"[..]));
    }

    #[test]
    fn test_exthdr_name_dirname_and_basename() {
        for (path, dir, base) in [
            ("top", ".", "top"),
            ("a/b", "a", "b"),
            ("a/b/c", "a/b", "c"),
            ("d/", ".", "d"),
            ("a//b//", "a", "b"),
            ("/top", "/", "top"),
            ("/", "/", "/"),
        ] {
            assert_eq!(dirname(path.as_bytes()), dir.as_bytes(), "dirname {path}");
            assert_eq!(
                basename(path.as_bytes()),
                base.as_bytes(),
                "basename {path}"
            );
        }
        assert_eq!(
            expand_header_template(b"%d/PaxHeaders.%p/%f", std::path::Path::new("top"), 1),
            format!("./PaxHeaders.{}/top", std::process::id()).into_bytes()
        );
    }

    #[test]
    fn test_globexthdr_name_uses_tmpdir() {
        // The default global-header name template derives from $TMPDIR, falling
        // back to /tmp when it is unset. Tested via the pure helper to avoid
        // mutating the process-global TMPDIR env (which would race other tests'
        // temp-dir creation).
        assert_eq!(
            default_globexthdr_template(Some(b"/custom/tmp")),
            b"/custom/tmp/GlobalHead.%p.%n"
        );
        assert_eq!(
            default_globexthdr_template(Some(b"/custom/tmp/")),
            b"/custom/tmp/GlobalHead.%p.%n"
        );
        assert_eq!(default_globexthdr_template(None), b"/tmp/GlobalHead.%p.%n");
    }

    /// `-o hdrcharset=` named the encoding of the path, linkpath, uname and
    /// gname records, and was accepted unchecked: any string at all went
    /// straight into the archive. pax has no character-set conversion, so a
    /// third name would declare an encoding the values are not in.
    #[test]
    fn test_parse_hdrcharset_value_is_checked() {
        // POSIX's two values, in any case, canonicalized to its spelling.
        for spelling in ["BINARY", "binary", "Binary"] {
            let opts = FormatOptions::parse(&format!("hdrcharset={spelling}")).unwrap();
            assert_eq!(
                opts.global_options().get("hdrcharset").map(Vec::as_slice),
                Some(&b"BINARY"[..]),
                "{spelling} must record as POSIX's spelling"
            );
        }
        let utf8 = FormatOptions::parse("hdrcharset=iso-ir 10646 2000 utf-8").unwrap();
        assert_eq!(
            utf8.global_options().get("hdrcharset").map(Vec::as_slice),
            Some(&b"ISO-IR 10646 2000 UTF-8"[..])
        );

        // A name pax cannot encode to is refused, and the message names the
        // values it can.
        let err = FormatOptions::parse("hdrcharset=ISO-8859-1").unwrap_err();
        let text = err.to_string();
        assert!(
            text.contains("BINARY") && text.contains("ISO-IR 10646 2000 UTF-8"),
            "the refusal must name the supported values (got {text:?})"
        );

        // An empty value is POSIX's form for deleting a previously entered
        // extended header value, not a charset name.
        assert!(FormatOptions::parse("hdrcharset=").is_ok());

        // The per-file form is checked the same way.
        assert!(FormatOptions::parse("hdrcharset:=BINARY").is_ok());
        assert!(FormatOptions::parse("hdrcharset:=nonesuch").is_err());
    }

    /// `charset` names the encoding of the file *data*, which POSIX says is
    /// "included in an extended header for information only" and forbids pax
    /// translating. Its value is an annotation to carry, not a name to check.
    #[test]
    fn test_parse_charset_value_is_carried_unchecked() {
        let opts = FormatOptions::parse("charset=ISO-IR 8859 1 1998").unwrap();
        assert_eq!(
            opts.global_options().get("charset").map(Vec::as_slice),
            Some(&b"ISO-IR 8859 1 1998"[..])
        );
    }

    #[test]
    fn test_merge_options() {
        let mut opts1 = FormatOptions::parse("times").unwrap();
        let opts2 = FormatOptions::parse("linkdata,listopt=%F").unwrap();
        opts1.merge(&opts2);

        assert!(opts1.include_times);
        assert!(opts1.link_data);
        assert_eq!(opts1.list_format.as_deref(), Some(&b"%F"[..]));
    }

    /// `format_list_entry` yields bytes, because a member name is bytes. Every
    /// fixture here is ASCII, so rendering back to a `String` keeps the
    /// assertions readable; the byte-exact cases assert on the Vec directly.
    fn fmt(format: &str, info: &ListEntryInfo) -> String {
        String::from_utf8(format_list_entry(format.as_bytes(), info)).expect("ASCII fixture")
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
            uname: Some("user".as_bytes().to_vec()),
            gname: Some("group".as_bytes().to_vec()),
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
            uname: Some("ünïcode".as_bytes().to_vec()),
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
            uname: Some("alice".as_bytes().to_vec()),
            gname: Some("users".as_bytes().to_vec()),
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
            uname: Some("alice".as_bytes().to_vec()),
            gname: Some("users".as_bytes().to_vec()),
            ..Default::default()
        };
        let info = info(&e);

        // POSIX `%(keyword)s`/`%(keyword)d` substitution.
        assert_eq!(fmt("%(path)s %(size)d", &info), "dir/file.txt 4096");
        assert_eq!(fmt("%(uname)s:%(gname)s", &info), "alice:users");
        // An unknown keyword is echoed verbatim.
        assert_eq!(fmt("%(bogus)s", &info), "%(bogus)s");
    }

    /// POSIX: "all characters in the remainder of the option-argument shall
    /// be considered part of the format string" -- trailing blanks included.
    #[test]
    fn test_listopt_value_is_the_whole_remainder() {
        let opts = FormatOptions::parse("listopt=[%F] ").unwrap();
        assert_eq!(opts.list_format.as_deref(), Some(&b"[%F] "[..]));
    }

    /// The format is a printf format, whose backslash escapes are decoded.
    #[test]
    fn test_listopt_decodes_printf_escapes() {
        let opts = FormatOptions::parse(r"listopt=%F\t%s\\").unwrap();
        assert_eq!(opts.list_format.as_deref(), Some(&b"%F\t%s\\"[..]));
    }

    /// POSIX: "When multiple -o listopt=format options are specified, the
    /// format strings shall be considered a single, concatenated string,
    /// evaluated in command-line order."
    #[test]
    fn test_listopt_options_concatenate() {
        let mut opts = FormatOptions::new();
        opts.parse_into(b"listopt=%F").unwrap();
        opts.parse_into(b"listopt=:%s").unwrap();
        assert_eq!(opts.list_format.as_deref(), Some(&b"%F:%s"[..]));
    }

    /// Splitting off the listopt tail removes the one comma that separates
    /// it, not an escaped comma that ends the value before it.
    #[test]
    fn test_escaped_comma_before_listopt_is_kept() {
        let opts = FormatOptions::parse(r"x=a\,,listopt=%F").unwrap();
        assert_eq!(opts.global.get("x").map(Vec::as_slice), Some(&b"a,"[..]));
        assert_eq!(opts.list_format.as_deref(), Some(&b"%F"[..]));
    }

    #[test]
    fn test_listopt_is_final_keyword_with_commas() {
        // A literal comma inside the listopt format must not split the option.
        let opts = FormatOptions::parse("times,listopt=%(path)s,%(size)d bytes").unwrap();
        assert!(opts.include_times);
        assert_eq!(
            opts.list_format.as_deref(),
            Some(&b"%(path)s,%(size)d bytes"[..])
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
            uname: Some("root".as_bytes().to_vec()),
            gname: Some("disk".as_bytes().to_vec()),
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
            uname: Some("alice".as_bytes().to_vec()),
            gname: Some("users".as_bytes().to_vec()),
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

        // An absolute name too long for the name field whose only `/` is the
        // leading one. Splitting there leaves the prefix empty, and rule 11
        // concatenates only "the keywords that are non-null" -- so the
        // separator goes with the dropped prefix and the leading `/` vanishes
        // from the reconstruction.
        let abs = format!("/{}", "x".repeat(120));
        let rooted = ustar_entry(&abs, b'0');
        assert_eq!(fmt("%(name)s", &info(&rooted)), abs);
        assert_eq!(fmt("%(prefix)s", &info(&rooted)), "");
        assert_eq!(
            fmt("%(prefix,name)F", &info(&rooted)),
            fmt("%F", &info(&rooted)),
            "rule 11's default must rebuild an absolute name too"
        );
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

        assert_eq!(
            fmt("[%(c_dev)s%(c_ino)s%(c_nlink)s%(c_magic)s]", &info),
            "[]"
        );
        // While the unprefixed `magic` is the ustar table's name, so it does
        // answer -- and answers from a cpio header too.
        assert_eq!(fmt("%(magic)s", &info), "ustar");
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

    /// Rule 11: the `F` conversion may name a <comma>-separated keyword list,
    /// whose non-null values are joined with `/`. `%(prefix,name)F` is the
    /// default POSIX gives for a member with no `path` record, and the list
    /// form was not parsed at all -- the whole specification echoed back.
    #[test]
    fn test_conversion_f_concatenates_its_keywords() {
        let long_dir = "d".repeat(110);
        let e = ustar_entry(&format!("{long_dir}/f.txt"), b'0');
        let split = info(&e);

        assert_eq!(fmt("%(prefix,name)F", &split), format!("{long_dir}/f.txt"));
        assert_eq!(fmt("%(prefix,name)F", &split), fmt("%F", &split));
        // One keyword is the degenerate list, and names that keyword's value
        // rather than the whole pathname.
        assert_eq!(fmt("%(name)F", &split), "f.txt");
        assert_eq!(fmt("%(prefix)F", &split), long_dir);

        // "all the keywords that are non-null": an empty half contributes
        // nothing, and leaves no separator behind.
        let e = ustar_entry("f.txt", b'0');
        assert_eq!(fmt("%(prefix,name)F", &info(&e)), "f.txt");

        // A name in no table still echoes, even inside a list.
        assert_eq!(fmt("%(prefix,bogus)F", &split), "%(prefix,bogus)F");
    }

    /// Rule 12: `%L` expands a symbolic link to `"%s -> %s"` of the pathname
    /// and the link's contents, and is "the equivalent of %F" for anything
    /// else. Bare `%L` had no handler and printed itself; the keyword form
    /// printed only the target, dropping the name and the arrow.
    #[test]
    fn test_conversion_l_expands_a_symbolic_link() {
        let link = ArchiveEntry {
            entry_type: EntryType::Symlink,
            link_target: Some("sub/target.txt".into()),
            ..ustar_entry("mylink", b'2')
        };
        assert_eq!(fmt("%L", &info(&link)), "mylink -> sub/target.txt");
        assert_eq!(fmt("%(path)L", &info(&link)), "mylink -> sub/target.txt");

        // Not a link: equivalent to %F.
        let plain = ustar_entry("f.txt", b'0');
        assert_eq!(fmt("%L", &info(&plain)), fmt("%F", &info(&plain)));
        assert_eq!(fmt("%(path)L", &info(&plain)), "f.txt");

        // A hard link is not a symbolic link, so it is %F too -- its target is
        // another member, not contents to follow.
        let hard = ArchiveEntry {
            entry_type: EntryType::Hardlink,
            link_target: Some("original.txt".into()),
            ..ustar_entry("h", b'1')
        };
        assert_eq!(fmt("%L", &info(&hard)), "h");
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
