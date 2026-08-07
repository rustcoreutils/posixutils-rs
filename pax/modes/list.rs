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
use crate::options::{
    format_list_entry, format_mode_symbolic, format_time_traditional, FormatOptions, ListEntryInfo,
};
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
            // Same as read mode: without the options the reader ignores
            // `-o delete=`, so a keyword suppressed on extract was still shown
            // in the listing.
            let mut archive = PaxReader::new(reader).with_options(options.format_options.clone());
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
            // `-o keyword:=value` forces a value regardless of what the archive
            // carried, and the listing must report what extraction would use.
            crate::modes::read::apply_keyword_overrides(&mut entry, &options.format_options);
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
            // Only worth a second pass when stripping actually changed
            // something; otherwise this repeats the first pass verbatim for
            // every non-matching member.
            if std::ptr::eq(path_stripped, path.as_ref() as &str) {
                None
            } else {
                find_matching_pattern_subtree(&options.patterns, path_stripped, expand_subtree)
            }
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
            atime: entry.atime,
            ctime: entry.ctime,
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
    let mode_str = format_mode_symbolic(entry.mode, entry.entry_type);
    let nlink = entry.nlink;
    let owner = format_owner(entry);
    let group = format_group(entry);
    let size = entry.size;
    let mtime = format_time_traditional(entry.mtime);
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

/// Format owner name or uid
fn format_owner(entry: &ArchiveEntry) -> String {
    entry.uname.clone().unwrap_or_else(|| entry.uid.to_string())
}

/// Format group name or gid
fn format_group(entry: &ArchiveEntry) -> String {
    entry.gname.clone().unwrap_or_else(|| entry.gid.to_string())
}

/// Format link suffix for symlinks and hardlinks
fn format_link_suffix(entry: &ArchiveEntry) -> String {
    match (&entry.entry_type, &entry.link_target) {
        (EntryType::Symlink, Some(target)) => format!(" -> {}", target.display()),
        (EntryType::Hardlink, Some(target)) => format!(" == {}", target.display()),
        _ => String::new(),
    }
}
