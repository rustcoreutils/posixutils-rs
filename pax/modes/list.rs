//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! List mode implementation - list archive contents

use crate::archive::{ArchiveEntry, ArchiveReader, EntryType, LinkSets};
use crate::error::{PaxError, PaxResult};
use crate::modes::select::Selector;
use crate::options::{
    format_list_entry, format_mode_symbolic, format_time_traditional, FormatOptions, ListEntryInfo,
};
use crate::pattern::Pattern;
use crate::subst::Substitution;
use std::io::Write;
use std::path::{Path, PathBuf};

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
    /// Members not to list (tar `--exclude` / `-X`)
    pub exclude_patterns: Vec<Pattern>,
    /// Leading pathname components to drop (tar `--strip-components`)
    pub strip_components: usize,
}

/// List the members of an archive
pub fn list_archive<R: ArchiveReader, W: Write>(
    archive: &mut R,
    writer: &mut W,
    options: &ListOptions,
) -> PaxResult<()> {
    let mut selector = Selector::new(
        &options.patterns,
        options.exclude,
        options.first_match,
        options.dir_only,
        &options.exclude_patterns,
    );
    let option_records =
        crate::modes::read::caller_option_records(archive, &options.format_options)?;
    // The first listed name of each cpio link set, for the later ones to show
    // as `== first`, the way extraction links them.
    let mut link_sets: LinkSets<PathBuf> = LinkSets::default();

    // Whether the loop met the end of the archive, rather than stopping
    // short of it under -n.
    let mut reached_end = true;
    while let Some(mut entry) = archive.read_entry()? {
        if let Some(ref records) = option_records {
            records.apply(&mut entry);
        }
        if let Some(selection) = selector.select(&entry) {
            selector.take(selection);
            // Rename as extraction would, hard link targets included, so the
            // listing shows the names `-r` would create (`tar -t` what `tar -x`).
            if crate::modes::read::rename_member(
                &mut entry,
                &options.substitutions,
                options.strip_components,
            ) {
                let linked_to = link_set_target(&mut link_sets, &entry);
                // A failure to write the listing is not about this member and
                // recurs for every one after it, so it ends the run.
                print_entry(writer, &entry, linked_to.as_deref(), options)
                    .map_err(listing_error)?;
            }
        }
        archive.skip_data()?;
        if selector.is_done() {
            reached_end = false;
            break;
        }
    }

    selector.report_unmatched();
    archive.finish(reached_end)
}

/// A failure to write the listing, which ends the run.
pub(crate) fn listing_error(e: std::io::Error) -> PaxError {
    PaxError::Io(std::io::Error::new(
        e.kind(),
        format!("writing the listing: {e}"),
    ))
}

/// The name a later name of a cpio link set is linked to on extraction: the
/// first listed name of the set. `None` for any other member, which starts a set
/// if it is the first name of one.
fn link_set_target(link_sets: &mut LinkSets<PathBuf>, entry: &ArchiveEntry) -> Option<PathBuf> {
    if let Some(first) = link_sets.find_mut(entry) {
        return Some(first.clone());
    }
    link_sets.insert(entry, || entry.path.clone());
    None
}

/// Print an entry. `linked_to` is the earlier name a cpio member is linked to.
fn print_entry<W: Write>(
    writer: &mut W,
    entry: &ArchiveEntry,
    linked_to: Option<&Path>,
    options: &ListOptions,
) -> std::io::Result<()> {
    // Check for custom list format (listopt)
    if let Some(ref format) = options.format_options.list_format {
        let info = ListEntryInfo {
            entry,
            style: crate::escape::stdout_style(),
        };
        writer.write_all(&format_list_entry(format, &info))?;
        // POSIX: "The pax utility shall append a <newline> to the listopt
        // output for each selected file" -- even one ending in a <newline>.
        writer.write_all(b"\n")?;
    } else if options.verbose {
        print_verbose(writer, entry, linked_to)?;
    } else {
        // The name goes out as the bytes the archive recorded. `display()`
        // would render an invalid byte as U+FFFD, so the listing would not
        // name the file extraction creates.
        crate::escape::write_name(writer, &entry.path, crate::escape::stdout_style())?;
        writer.write_all(b"\n")?;
    }
    Ok(())
}

/// Print verbose ls -l style output
fn print_verbose<W: Write>(
    writer: &mut W,
    entry: &ArchiveEntry,
    linked_to: Option<&Path>,
) -> std::io::Result<()> {
    let mode_str = format_mode_symbolic(entry.mode, entry.entry_type);
    let nlink = entry.nlink;
    let owner = format_owner(entry);
    let group = format_group(entry);
    let size = entry.size;
    let mtime = format_time_traditional(entry.mtime);
    let path = &entry.path;

    // The fixed columns are text; the name and the link target are bytes, so
    // the line is assembled rather than formatted in one go.
    write!(
        writer,
        "{} {:>3} {:>8} {:>8} {:>8} {} ",
        mode_str, nlink, owner, group, size, mtime
    )?;
    crate::escape::write_name(writer, path, crate::escape::stdout_style())?;
    write_link_suffix(writer, entry, linked_to)?;
    writer.write_all(b"\n")?;

    Ok(())
}

/// Format owner name or uid for the `-v` listing's aligned column.
///
/// A name is bytes, and this column is padded to a width, so it is rendered as
/// display text rather than written through. A name that is not UTF-8 -- which
/// `hdrcharset=BINARY` permits -- would otherwise mis-align every following
/// column. `-o listopt=%(uname)s` is the lossless way to read one. It comes
/// from the archive like the pathname, so it is escaped like one; escaping
/// keeps one unit per unit, so the column width is unchanged.
fn format_owner(entry: &ArchiveEntry) -> String {
    display_name(entry.uname.as_deref(), entry.uid)
}

/// Format group name or gid. See `format_owner`.
fn format_group(entry: &ArchiveEntry) -> String {
    display_name(entry.gname.as_deref(), entry.gid)
}

fn display_name(name: Option<&[u8]>, id: u32) -> String {
    match name {
        Some(name) => {
            let mut shown = Vec::with_capacity(name.len());
            crate::escape::push_escaped(&mut shown, name, crate::escape::stdout_style());
            String::from_utf8_lossy(&shown).into_owned()
        }
        None => id.to_string(),
    }
}

/// Write the ` -> target` / ` == target` suffix a link carries -- a hard link
/// either by its typeflag or, in cpio, as a later name of a link set.
///
/// Writes rather than returning a `String`, so the target's bytes never pass
/// through one -- which is what stops this drifting back to `display()`.
fn write_link_suffix<W: Write>(
    writer: &mut W,
    entry: &ArchiveEntry,
    linked_to: Option<&Path>,
) -> std::io::Result<()> {
    let (marker, target) = match (&entry.entry_type, &entry.link_target, linked_to) {
        (EntryType::Symlink, Some(target), _) => (b" -> ".as_slice(), target.as_path()),
        (EntryType::Hardlink, Some(target), _) => (b" == ".as_slice(), target.as_path()),
        (_, _, Some(target)) => (b" == ".as_slice(), target),
        _ => return Ok(()),
    };
    writer.write_all(marker)?;
    crate::escape::write_name(writer, target, crate::escape::stdout_style())?;
    Ok(())
}
