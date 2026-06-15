//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! List mode implementation - list archive contents

use crate::archive::{ArchiveEntry, ArchiveFormat, ArchiveReader, EntryType};
use crate::error::PaxResult;
use crate::formats::{CpioReader, PaxReader, UstarReader};
use crate::options::{format_list_entry, FormatOptions, ListEntryInfo};
use crate::pattern::{find_matching_pattern_subtree, Pattern};
use crate::subst::{apply_substitutions, SubstResult, Substitution};
use std::collections::HashSet;
use std::io::{Read, Write};
use std::path::PathBuf;

/// Options for list mode
#[derive(Default)]
pub struct ListOptions {
    /// Verbose output (ls -l style)
    pub verbose: bool,
    /// Patterns to match
    pub patterns: Vec<Pattern>,
    /// Match all except patterns
    pub exclude: bool,
    /// Format options from -o
    pub format_options: FormatOptions,
    /// Path substitutions (-s option)
    pub substitutions: Vec<Substitution>,
    /// Select only first archive member matching each pattern (-n)
    pub first_match: bool,
    /// `-d`: a directory pattern matches only the directory itself, not its
    /// subtree.
    pub dir_only: bool,
}

/// List archive contents
pub fn list_archive<R: Read, W: Write>(
    reader: R,
    writer: &mut W,
    format: ArchiveFormat,
    options: &ListOptions,
) -> PaxResult<()> {
    match format {
        ArchiveFormat::Ustar => {
            let mut archive = UstarReader::new(reader);
            list_entries(&mut archive, writer, options)
        }
        ArchiveFormat::Cpio => {
            let mut archive = CpioReader::new(reader);
            list_entries(&mut archive, writer, options)
        }
        ArchiveFormat::Pax => {
            let mut archive = PaxReader::new(reader);
            list_entries(&mut archive, writer, options)
        }
    }
}

/// List archive contents from an ArchiveReader (for multi-volume support)
pub fn list_archive_from_reader<R: ArchiveReader, W: Write>(
    archive: &mut R,
    writer: &mut W,
    options: &ListOptions,
) -> PaxResult<()> {
    list_entries(archive, writer, options)
}

/// List entries from any archive reader
fn list_entries<R: ArchiveReader, W: Write>(
    archive: &mut R,
    writer: &mut W,
    options: &ListOptions,
) -> PaxResult<()> {
    // Track which patterns have been matched (for -n first_match option)
    let mut matched_patterns: HashSet<usize> = HashSet::new();

    while let Some(mut entry) = archive.read_entry()? {
        if let Some(should_output) = should_list(&entry, options, &mut matched_patterns) {
            if !should_output {
                // Entry matched a pattern that's already been matched (first_match mode)
                archive.skip_data()?;
                continue;
            }
            // Apply substitutions
            if !options.substitutions.is_empty() {
                let path_str = entry.path.to_string_lossy();
                match apply_substitutions(&options.substitutions, &path_str) {
                    SubstResult::Unchanged => {
                        // Keep original path
                    }
                    SubstResult::Changed(new_path) => {
                        entry.path = PathBuf::from(new_path);
                    }
                    SubstResult::Empty => {
                        // Skip this entry
                        archive.skip_data()?;
                        continue;
                    }
                }
            }
            if let Err(e) = print_entry(writer, &entry, options) {
                crate::error::report_error(entry.path.display(), e);
            }
        }
        archive.skip_data()?;
    }

    // Diagnose any pattern operand that matched no archive member (non-exclude
    // mode) and set a non-zero exit status (POSIX DESCRIPTION).
    if !options.exclude {
        for (idx, pat) in options.patterns.iter().enumerate() {
            if !matched_patterns.contains(&idx) {
                crate::error::report_error(&pat.source, gettextrs::gettext("not found"));
            }
        }
    }

    Ok(())
}

/// Check if entry should be listed
/// Returns:
/// - None: entry should not be listed (doesn't match patterns or excluded)
/// - Some(true): entry should be listed
/// - Some(false): entry matches but pattern already matched (first_match mode)
fn should_list(
    entry: &ArchiveEntry,
    options: &ListOptions,
    matched_patterns: &mut HashSet<usize>,
) -> Option<bool> {
    let path = entry.path.to_string_lossy();

    // Try matching against both the full path and the path with "./" prefix stripped
    let path_stripped = path.strip_prefix("./").unwrap_or(&path);

    if options.patterns.is_empty() {
        // No patterns means match all
        if options.exclude {
            return None; // Exclude all
        }
        return Some(true); // Match all
    }

    // Find which pattern matches (if any). A pattern selecting a directory also
    // selects its whole subtree unless `-d` (dir_only) was given.
    let expand_subtree = !options.dir_only;
    let matching_pattern = find_matching_pattern_subtree(&options.patterns, &path, expand_subtree)
        .or_else(|| {
            find_matching_pattern_subtree(&options.patterns, path_stripped, expand_subtree)
        });

    match matching_pattern {
        Some(pattern_idx) => {
            if options.exclude {
                // Entry matched a pattern, so exclude it
                None
            } else if options.first_match && matched_patterns.contains(&pattern_idx) {
                // first_match (-n): this pattern has already selected a member
                Some(false)
            } else {
                // Record the match (for the unmatched-pattern sweep and -n) and
                // select the entry.
                matched_patterns.insert(pattern_idx);
                Some(true)
            }
        }
        None => {
            // No pattern matched
            if options.exclude {
                Some(true) // Exclude mode: output entries that don't match
            } else {
                None // Normal mode: skip entries that don't match
            }
        }
    }
}

/// Print an entry
fn print_entry<W: Write>(
    writer: &mut W,
    entry: &ArchiveEntry,
    options: &ListOptions,
) -> PaxResult<()> {
    // Check for custom list format (listopt)
    if let Some(ref format) = options.format_options.list_format {
        let path_str = entry.path.to_string_lossy();
        let link_target_str = entry
            .link_target
            .as_ref()
            .map(|p| p.to_string_lossy().to_string());
        let info = ListEntryInfo {
            path: &path_str,
            mode: entry.mode,
            size: entry.size,
            mtime: entry.mtime,
            uid: entry.uid,
            gid: entry.gid,
            uname: entry.uname.as_deref(),
            gname: entry.gname.as_deref(),
            link_target: link_target_str.as_deref(),
            entry_type: entry.entry_type,
            devmajor: entry.devmajor,
            devminor: entry.devminor,
        };
        let output = format_list_entry(format, &info);
        write!(writer, "{}", output)?;
        // Add newline if format doesn't end with one
        if !output.ends_with('\n') {
            writeln!(writer)?;
        }
    } else if options.verbose {
        print_verbose(writer, entry)?;
    } else {
        writeln!(writer, "{}", entry.path.display())?;
    }
    Ok(())
}

/// Print verbose ls -l style output
fn print_verbose<W: Write>(writer: &mut W, entry: &ArchiveEntry) -> PaxResult<()> {
    let mode_str = format_mode(entry);
    let nlink = entry.nlink;
    let owner = format_owner(entry);
    let group = format_group(entry);
    let size = entry.size;
    let mtime = format_mtime(entry.mtime);
    let path = &entry.path;

    let link_suffix = format_link_suffix(entry);

    writeln!(
        writer,
        "{} {:>3} {:>8} {:>8} {:>8} {} {}{}",
        mode_str,
        nlink,
        owner,
        group,
        size,
        mtime,
        path.display(),
        link_suffix
    )?;

    Ok(())
}

/// Format mode string like "drwxr-xr-x"
fn format_mode(entry: &ArchiveEntry) -> String {
    let mut s = String::with_capacity(10);

    // File type
    s.push(match entry.entry_type {
        EntryType::Directory => 'd',
        EntryType::Symlink => 'l',
        // A hard link is a regular file with a link count > 1; POSIX `ls -l`
        // (and the pax `-v` listing) shows it with the regular-file type char.
        EntryType::Hardlink => '-',
        EntryType::BlockDevice => 'b',
        EntryType::CharDevice => 'c',
        EntryType::Fifo => 'p',
        EntryType::Socket => 's',
        EntryType::Regular => '-',
    });

    // User permissions
    s.push(if entry.mode & 0o400 != 0 { 'r' } else { '-' });
    s.push(if entry.mode & 0o200 != 0 { 'w' } else { '-' });
    s.push(format_execute_bit(entry.mode, 0o100, 0o4000));

    // Group permissions
    s.push(if entry.mode & 0o040 != 0 { 'r' } else { '-' });
    s.push(if entry.mode & 0o020 != 0 { 'w' } else { '-' });
    s.push(format_execute_bit(entry.mode, 0o010, 0o2000));

    // Other permissions
    s.push(if entry.mode & 0o004 != 0 { 'r' } else { '-' });
    s.push(if entry.mode & 0o002 != 0 { 'w' } else { '-' });
    s.push(format_execute_bit(entry.mode, 0o001, 0o1000));

    s
}

/// Format execute bit with setuid/setgid/sticky handling
fn format_execute_bit(mode: u32, exec_bit: u32, special_bit: u32) -> char {
    let has_exec = mode & exec_bit != 0;
    let has_special = mode & special_bit != 0;

    match (has_exec, has_special) {
        (true, true) => {
            if special_bit == 0o1000 {
                't'
            } else {
                's'
            }
        }
        (false, true) => {
            if special_bit == 0o1000 {
                'T'
            } else {
                'S'
            }
        }
        (true, false) => 'x',
        (false, false) => '-',
    }
}

/// Format owner name or uid
fn format_owner(entry: &ArchiveEntry) -> String {
    entry.uname.clone().unwrap_or_else(|| entry.uid.to_string())
}

/// Format group name or gid
fn format_group(entry: &ArchiveEntry) -> String {
    entry.gname.clone().unwrap_or_else(|| entry.gid.to_string())
}

/// Format modification time
fn format_mtime(mtime: u64) -> String {
    // POSIX `ls -l`-style time: a date+time for recent members, a date+year for
    // members older than ~6 months (or in the future). Formatting goes through
    // libc strftime via localtime_r, so TZ and LC_TIME (month names) take effect.
    use std::time::{SystemTime, UNIX_EPOCH};

    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0);
    let age = now - mtime as i64;
    let six_months: i64 = 180 * 24 * 60 * 60;
    let recent = (0..six_months).contains(&age);

    let fmt = if recent { "%b %e %H:%M" } else { "%b %e  %Y" };
    plib::locale::strftime(fmt, mtime as i64).unwrap_or_else(|_| mtime.to_string())
}

/// Format link suffix for symlinks and hardlinks
fn format_link_suffix(entry: &ArchiveEntry) -> String {
    match (&entry.entry_type, &entry.link_target) {
        (EntryType::Symlink, Some(target)) => format!(" -> {}", target.display()),
        (EntryType::Hardlink, Some(target)) => format!(" == {}", target.display()),
        _ => String::new(),
    }
}
