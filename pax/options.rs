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

impl InvalidAction {
    /// Parse from string value
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "bypass" => Some(InvalidAction::Bypass),
            "rename" => Some(InvalidAction::Rename),
            "write" => Some(InvalidAction::Write),
            "UTF-8" => Some(InvalidAction::Utf8),
            "binary" => Some(InvalidAction::Binary),
            _ => None,
        }
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

        // Parse comma-separated options
        // Note: commas can be escaped with backslash
        let mut current = String::new();
        let chars = input.chars();
        let mut escaped = false;

        for c in chars {
            if escaped {
                current.push(c);
                escaped = false;
            } else if c == '\\' {
                escaped = true;
            } else if c == ',' {
                // End of option
                self.parse_single_option(current.trim())?;
                current.clear();
            } else {
                current.push(c);
            }
        }

        // Parse final option
        let final_opt = current.trim();
        if !final_opt.is_empty() {
            self.parse_single_option(final_opt)?;
        }

        Ok(())
    }

    /// Parse a single option (keyword[[:]=value])
    fn parse_single_option(&mut self, opt: &str) -> PaxResult<()> {
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

        // Handle known keywords
        match keyword {
            "listopt" => {
                self.list_format = value.map(|s| s.to_string());
            }
            "delete" => {
                if let Some(pattern) = value {
                    // Pre-compile pattern for efficient matching
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
            "times" => {
                self.include_times = true;
            }
            "linkdata" => {
                self.link_data = true;
            }
            // Extended header name templates
            "exthdr.name" => {
                self.exthdr_name = value.map(|s| s.to_string());
            }
            "globexthdr.name" => {
                self.globexthdr_name = value.map(|s| s.to_string());
            }
            // Invalid action handling
            "invalid" => {
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
            // Store any other options for format-specific handling
            _ => {
                if is_per_file {
                    self.per_file
                        .insert(keyword.to_string(), value.unwrap_or("").to_string());
                } else {
                    self.global
                        .insert(keyword.to_string(), value.unwrap_or("").to_string());
                }
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
    /// Default template: "/tmp/GlobalHead.%p.%n"
    pub fn expand_globexthdr_name(&self, sequence: u64) -> String {
        let template = self
            .globexthdr_name
            .as_deref()
            .unwrap_or("/tmp/GlobalHead.%p.%n");

        expand_global_header_template(template, sequence)
    }
}

/// Expand template for per-file extended header names
fn expand_header_template(template: &str, path: &std::path::Path, sequence: u64) -> String {
    let mut result = String::new();
    let mut chars = template.chars().peekable();
    let pid = std::process::id();

    // Extract directory and filename from path
    let dirname = path
        .parent()
        .map(|p| p.to_string_lossy())
        .unwrap_or_default();
    let filename = path
        .file_name()
        .map(|f| f.to_string_lossy())
        .unwrap_or_default();

    while let Some(c) = chars.next() {
        if c == '%' {
            match chars.next() {
                Some('d') => result.push_str(&dirname),
                Some('f') => result.push_str(&filename),
                Some('p') => result.push_str(&pid.to_string()),
                Some('n') => result.push_str(&sequence.to_string()),
                Some('%') => result.push('%'),
                Some(other) => {
                    result.push('%');
                    result.push(other);
                }
                None => result.push('%'),
            }
        } else {
            result.push(c);
        }
    }

    result
}

/// Expand template for global extended header names
fn expand_global_header_template(template: &str, sequence: u64) -> String {
    let mut result = String::new();
    let mut chars = template.chars().peekable();
    let pid = std::process::id();

    while let Some(c) = chars.next() {
        if c == '%' {
            match chars.next() {
                Some('n') => result.push_str(&sequence.to_string()),
                Some('p') => result.push_str(&pid.to_string()),
                Some('%') => result.push('%'),
                Some(other) => {
                    result.push('%');
                    result.push(other);
                }
                None => result.push('%'),
            }
        } else {
            result.push(c);
        }
    }

    result
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
            match chars.next() {
                Some('f') => {
                    // Filename (basename)
                    let name = std::path::Path::new(info.path)
                        .file_name()
                        .map(|s| s.to_string_lossy().to_string())
                        .unwrap_or_else(|| info.path.to_string());
                    result.push_str(&name);
                }
                Some('F') => {
                    // Full path
                    result.push_str(info.path);
                }
                Some('l') => {
                    // Link target
                    if let Some(target) = info.link_target {
                        result.push_str(target);
                    }
                }
                Some('m') => {
                    // Mode (octal)
                    result.push_str(&format!("{:o}", info.mode & 0o7777));
                }
                Some('M') => {
                    // Mode (symbolic)
                    result.push_str(&format_mode_symbolic(info.mode, info.entry_type));
                }
                Some('D') => {
                    // Device major,minor (for block/char special files)
                    result.push_str(&format!("{},{}", info.devmajor, info.devminor));
                }
                Some('s') => {
                    // Size
                    result.push_str(&info.size.to_string());
                }
                Some('t') => {
                    // Modification time (traditional format)
                    result.push_str(&format_time_traditional(info.mtime));
                }
                Some('T') => {
                    // Modification time (ISO format)
                    result.push_str(&format_time_iso(info.mtime));
                }
                Some('u') => {
                    // Username
                    result.push_str(info.uname.unwrap_or(&info.uid.to_string()));
                }
                Some('g') => {
                    // Group name
                    result.push_str(info.gname.unwrap_or(&info.gid.to_string()));
                }
                Some('U') => {
                    // UID
                    result.push_str(&info.uid.to_string());
                }
                Some('G') => {
                    // GID
                    result.push_str(&info.gid.to_string());
                }
                Some('n') => {
                    // Newline
                    result.push('\n');
                }
                Some('%') => {
                    // Literal %
                    result.push('%');
                }
                Some(other) => {
                    // Unknown specifier - include literally
                    result.push('%');
                    result.push(other);
                }
                None => {
                    result.push('%');
                }
            }
        } else {
            result.push(c);
        }
    }

    result
}

/// Format mode as symbolic string (like ls -l)
fn format_mode_symbolic(mode: u32, entry_type: EntryType) -> String {
    let mut s = String::with_capacity(10);

    // File type (from entry_type, not mode bits - tar stores type separately)
    s.push(match entry_type {
        EntryType::Regular => '-',
        EntryType::Directory => 'd',
        EntryType::Symlink => 'l',
        EntryType::Hardlink => 'h',
        EntryType::BlockDevice => 'b',
        EntryType::CharDevice => 'c',
        EntryType::Fifo => 'p',
        EntryType::Socket => 's',
    });

    // Owner permissions
    s.push(if mode & 0o400 != 0 { 'r' } else { '-' });
    s.push(if mode & 0o200 != 0 { 'w' } else { '-' });
    s.push(if mode & 0o4000 != 0 {
        if mode & 0o100 != 0 {
            's'
        } else {
            'S'
        }
    } else if mode & 0o100 != 0 {
        'x'
    } else {
        '-'
    });

    // Group permissions
    s.push(if mode & 0o040 != 0 { 'r' } else { '-' });
    s.push(if mode & 0o020 != 0 { 'w' } else { '-' });
    s.push(if mode & 0o2000 != 0 {
        if mode & 0o010 != 0 {
            's'
        } else {
            'S'
        }
    } else if mode & 0o010 != 0 {
        'x'
    } else {
        '-'
    });

    // Other permissions
    s.push(if mode & 0o004 != 0 { 'r' } else { '-' });
    s.push(if mode & 0o002 != 0 { 'w' } else { '-' });
    s.push(if mode & 0o1000 != 0 {
        if mode & 0o001 != 0 {
            't'
        } else {
            'T'
        }
    } else if mode & 0o001 != 0 {
        'x'
    } else {
        '-'
    });

    s
}

/// Format time in traditional ls -l style
fn format_time_traditional(mtime: u64) -> String {
    use std::time::{Duration, SystemTime, UNIX_EPOCH};

    let _time = UNIX_EPOCH + Duration::from_secs(mtime);
    let now = SystemTime::now();

    // Get current time for comparison
    let now_secs = now
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);

    // If within 6 months, show month/day/time; otherwise show month/day/year
    let six_months = 180 * 24 * 60 * 60;
    let use_time = now_secs.saturating_sub(mtime) < six_months;

    // Simple formatting without chrono
    let secs_per_day = 86400u64;
    let days_since_epoch = mtime / secs_per_day;

    // Approximate month/day calculation
    let year = 1970 + (days_since_epoch / 365) as i32;
    let day_of_year = days_since_epoch % 365;

    let months = [
        ("Jan", 31),
        ("Feb", 28),
        ("Mar", 31),
        ("Apr", 30),
        ("May", 31),
        ("Jun", 30),
        ("Jul", 31),
        ("Aug", 31),
        ("Sep", 30),
        ("Oct", 31),
        ("Nov", 30),
        ("Dec", 31),
    ];

    let mut remaining = day_of_year;
    let mut month_name = "Jan";
    let mut day = 1u64;

    for (name, days) in months.iter() {
        if remaining < *days as u64 {
            month_name = name;
            day = remaining + 1;
            break;
        }
        remaining -= *days as u64;
    }

    if use_time {
        let time_of_day = mtime % secs_per_day;
        let hour = time_of_day / 3600;
        let min = (time_of_day % 3600) / 60;
        format!("{} {:2} {:02}:{:02}", month_name, day, hour, min)
    } else {
        format!("{} {:2}  {:4}", month_name, day, year)
    }
}

/// Format time in ISO format
fn format_time_iso(mtime: u64) -> String {
    let secs_per_day = 86400u64;
    let days_since_epoch = mtime / secs_per_day;

    // Approximate date calculation
    let mut year = 1970i32;
    let mut remaining_days = days_since_epoch as i64;

    loop {
        let days_in_year = if is_leap_year(year) { 366 } else { 365 };
        if remaining_days < days_in_year {
            break;
        }
        remaining_days -= days_in_year;
        year += 1;
    }

    let months = if is_leap_year(year) {
        [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    } else {
        [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    };

    let mut month = 1;
    for days in months.iter() {
        if remaining_days < *days as i64 {
            break;
        }
        remaining_days -= *days as i64;
        month += 1;
    }
    let day = remaining_days + 1;

    let time_of_day = mtime % secs_per_day;
    let hour = time_of_day / 3600;
    let min = (time_of_day % 3600) / 60;
    let sec = time_of_day % 60;

    format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}",
        year, month, day, hour, min, sec
    )
}

fn is_leap_year(year: i32) -> bool {
    (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
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
        assert_eq!(
            format_mode_symbolic(0o644, EntryType::Hardlink),
            "hrw-r--r--"
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
        // 2024-01-01 00:00:00 UTC
        let timestamp = 1704067200u64;
        let result = format_time_iso(timestamp);
        assert!(result.starts_with("2024-01-01T"));
    }
}
