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
use crate::options::{format_list_entry, FormatOptions};
use crate::pattern::{matches_any, Pattern};
use crate::subst::{apply_substitutions, SubstResult, Substitution};
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
    while let Some(mut entry) = archive.read_entry()? {
        if should_list(&entry, options) {
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
            print_entry(writer, &entry, options)?;
        }
        archive.skip_data()?;
    }
    Ok(())
}

/// Check if entry should be listed
fn should_list(entry: &ArchiveEntry, options: &ListOptions) -> bool {
    let path = entry.path.to_string_lossy();
    let matches = matches_any(&options.patterns, &path);

    if options.exclude {
        !matches
    } else {
        matches
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
        let link_target_str = entry
            .link_target
            .as_ref()
            .map(|p| p.to_string_lossy().to_string());
        let output = format_list_entry(
            format,
            &entry.path.to_string_lossy(),
            entry.mode,
            entry.size,
            entry.mtime,
            entry.uid,
            entry.gid,
            entry.uname.as_deref(),
            entry.gname.as_deref(),
            link_target_str.as_deref(),
        );
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
        EntryType::Hardlink => 'h',
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
    // Simple format: just show the timestamp
    // In a full implementation, we'd format based on age
    use std::time::{Duration, SystemTime, UNIX_EPOCH};

    let time = UNIX_EPOCH + Duration::from_secs(mtime);

    // Get current time to determine format
    let now = SystemTime::now();
    let six_months_ago = now - Duration::from_secs(180 * 24 * 60 * 60);

    // Format the time (simplified)
    format_system_time(time, time < six_months_ago)
}

/// Format a SystemTime for display
fn format_system_time(time: std::time::SystemTime, show_year: bool) -> String {
    use std::time::{Duration, UNIX_EPOCH};

    let secs = time
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::ZERO)
        .as_secs();

    // Simple formatting without external crates
    let days_since_epoch = secs / 86400;
    let secs_today = secs % 86400;
    let hours = secs_today / 3600;
    let minutes = (secs_today % 3600) / 60;

    // Approximate year/month/day calculation
    let (year, month, day) = days_to_ymd(days_since_epoch);

    let month_names = [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    ];
    let month_name = month_names.get(month as usize).unwrap_or(&"???");

    if show_year {
        format!("{} {:2}  {:4}", month_name, day, year)
    } else {
        format!("{} {:2} {:02}:{:02}", month_name, day, hours, minutes)
    }
}

/// Convert days since epoch to (year, month, day)
fn days_to_ymd(days: u64) -> (u64, u32, u32) {
    // Simplified calculation - not perfectly accurate but good enough for display
    let mut y = 1970;
    let mut remaining = days as i64;

    loop {
        let days_in_year = if is_leap_year(y) { 366 } else { 365 };
        if remaining < days_in_year {
            break;
        }
        remaining -= days_in_year;
        y += 1;
    }

    let days_in_month = if is_leap_year(y) {
        [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    } else {
        [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    };

    let mut m = 0;
    for (i, &days) in days_in_month.iter().enumerate() {
        if remaining < days as i64 {
            m = i;
            break;
        }
        remaining -= days as i64;
    }

    (y, m as u32, remaining as u32 + 1)
}

/// Check if a year is a leap year
fn is_leap_year(year: u64) -> bool {
    (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
}

/// Format link suffix for symlinks and hardlinks
fn format_link_suffix(entry: &ArchiveEntry) -> String {
    match (&entry.entry_type, &entry.link_target) {
        (EntryType::Symlink, Some(target)) => format!(" -> {}", target.display()),
        (EntryType::Hardlink, Some(target)) => format!(" == {}", target.display()),
        _ => String::new(),
    }
}
