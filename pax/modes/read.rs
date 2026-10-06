//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Read mode implementation - extract archive contents

use crate::archive::{ArchiveEntry, ArchiveFormat, ArchiveReader, EntryType, LinkSets};
use crate::error::{PaxError, PaxResult};
use crate::formats::{CpioReader, OptionRecords, PaxReader, UstarReader};
use crate::interactive::{InteractivePrompter, RenameResult};
use crate::modes::anchored::{
    chown_result, create_replacing, link_replacing, set_attrs_fd, stat_at, AttrPolicy, Attrs,
    DirTree, MemberPath,
};
use crate::pattern::{find_matching_pattern_subtree, matches_excluded, Pattern};
use crate::subst::{substitute_link_target, substitute_name, Substitution};
use std::collections::HashSet;
use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::{Read, Write};
use std::os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd, OwnedFd};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};

/// Options for read/extract mode
pub struct ReadOptions {
    /// Patterns to match
    pub patterns: Vec<Pattern>,
    /// Match all except patterns
    pub exclude: bool,
    /// Don't overwrite existing files
    pub no_clobber: bool,
    /// Verbose output
    pub verbose: bool,
    /// Preserve permissions
    pub preserve_perms: bool,
    /// Preserve modification time
    pub preserve_mtime: bool,
    /// Preserve access time
    pub preserve_atime: bool,
    /// Preserve owner (requires privileges)
    pub preserve_owner: bool,
    /// Interactive rename mode
    pub interactive: bool,
    /// Update mode - only extract if archive member is newer
    pub update: bool,
    /// Path substitutions (-s option)
    pub substitutions: Vec<Substitution>,
    /// Select only first archive member matching each pattern (-n)
    pub first_match: bool,
    /// Process file-creation mask, applied to the mode of extracted files when
    /// the mode is not explicitly preserved (no `-p p`/`-p e`).
    pub umask: u32,
    /// `-o` extended-header options (delete=, keyword=value, keyword:=value)
    /// applied on extract.
    pub format_options: crate::options::FormatOptions,
    /// `-d`: a directory pattern matches only the directory itself, not its
    /// subtree.
    pub dir_only: bool,
    /// Members not to extract (tar `--exclude` / `-X`)
    pub exclude_patterns: Vec<Pattern>,
    /// Leading pathname components to drop (tar `--strip-components`)
    pub strip_components: usize,
    /// Write member contents to standard output instead of creating files
    /// (tar `-O`)
    pub to_stdout: bool,
}

impl Default for ReadOptions {
    fn default() -> Self {
        ReadOptions {
            patterns: Vec::new(),
            exclude: false,
            no_clobber: false,
            verbose: false,
            preserve_perms: true,
            preserve_mtime: true,
            preserve_atime: true,
            preserve_owner: false,
            interactive: false,
            update: false,
            substitutions: Vec::new(),
            first_match: false,
            umask: 0,
            format_options: crate::options::FormatOptions::default(),
            dir_only: false,
            exclude_patterns: Vec::new(),
            strip_components: 0,
            to_stdout: false,
        }
    }
}

/// Extract archive contents
pub fn extract_archive<R: Read>(
    reader: R,
    format: ArchiveFormat,
    options: &ReadOptions,
) -> PaxResult<()> {
    match format {
        ArchiveFormat::Ustar => {
            let mut archive = UstarReader::new(reader);
            extract_entries(&mut archive, options)
        }
        ArchiveFormat::Cpio => {
            let mut archive = CpioReader::new(reader);
            extract_entries(&mut archive, options)
        }
        ArchiveFormat::Pax => {
            let mut archive =
                PaxReader::new(reader).with_options(options.format_options.clone())?;
            extract_entries(&mut archive, options)
        }
    }
}

/// Extract archive contents from an ArchiveReader (for multi-volume support)
pub fn extract_archive_from_reader<R: ArchiveReader>(
    archive: &mut R,
    options: &ReadOptions,
) -> PaxResult<()> {
    extract_entries(archive, options)
}

/// Extract entries from any archive reader
fn extract_entries<R: ArchiveReader>(archive: &mut R, options: &ReadOptions) -> PaxResult<()> {
    let mut link_sets: LinkSets<CreatedSet> = LinkSets::default();
    // Extraction is anchored at an open descriptor for the working directory,
    // and every member path is resolved relative to it without following a
    // symlink.
    let tree = DirTree::open_cwd()?;
    // Directories take their archived attributes only once the whole archive
    // has been extracted; see apply_pending_dirs.
    let mut pending_dirs: Vec<(MemberPath, ArchiveEntry)> = Vec::new();

    // Track which patterns have been matched (for -n first_match option)
    let mut matched_patterns: HashSet<usize> = HashSet::new();
    let option_records = caller_option_records(archive, &options.format_options)?;

    // Create interactive prompter if needed
    let mut prompter = if options.interactive {
        Some(InteractivePrompter::new()?)
    } else {
        None
    };

    while let Some(mut entry) = archive.read_entry()? {
        if let Some(ref records) = option_records {
            records.apply(&mut entry);
        }
        if let Some(should_output) = should_extract(&entry, options, &mut matched_patterns) {
            if !should_output {
                // Entry matched a pattern that's already been matched (first_match mode)
                archive.skip_data()?;
                continue;
            }
            // -s, then --strip-components, both before the name is offered for
            // renaming (POSIX: -s applies before -i), so an interactive prompt
            // shows the name that will actually be created.
            if !rename_member(&mut entry, &options.substitutions, options.strip_components) {
                archive.skip_data()?;
                continue;
            }

            // Handle interactive rename if enabled
            if let Some(ref mut p) = prompter {
                match p.prompt(&entry.path)? {
                    RenameResult::Skip => {
                        archive.skip_data()?;
                        continue;
                    }
                    RenameResult::UseOriginal => {
                        // Keep the original path
                    }
                    RenameResult::Rename(new_path) => {
                        entry.path = new_path;
                    }
                }
            }
            // Per POSIX CONSEQUENCES OF ERRORS: diagnose a per-file failure and
            // set a non-zero exit, but continue with the next member. Skip any
            // unconsumed data of the failed entry to realign the reader.
            if let Err(e) = extract_entry(
                archive,
                &entry,
                options,
                &mut link_sets,
                &tree,
                &mut pending_dirs,
            ) {
                crate::error::report_error(&entry.path, e);
                let _ = archive.skip_data();
            }
        } else {
            archive.skip_data()?;
        }
    }

    apply_pending_dirs(&tree, &mut pending_dirs, options);

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

/// The `-o keyword=value` and `-o keyword:=value` records the caller has to
/// apply to each member itself: `None` when the reader applies them, as the
/// pax reader does among the archive's own extended headers.
///
/// They are applied before anything looks at the member, the way an extended
/// header record would be, so a `path:=` name is the one patterns match.
pub(crate) fn caller_option_records<R: ArchiveReader>(
    archive: &R,
    opts: &crate::options::FormatOptions,
) -> PaxResult<Option<OptionRecords>> {
    if archive.applies_option_records() {
        return Ok(None);
    }
    OptionRecords::new(opts).map(Some)
}

/// Rename a member the way -s and --strip-components direct. `false` when its
/// own name is dropped -- substituted to the empty string, or left with no
/// components -- and the member is to be ignored, as POSIX says of `-s`.
///
/// A hard link's target is another member's name, so it is renamed in step:
/// leaving it alone linked the renamed member to whatever file sat at the old
/// name, or to nothing. A symlink's target is its contents, not a member name;
/// it is resolved in the extracted tree, and `-s` leaves it alone (the `s`
/// flag). A target that is itself dropped becomes `None`: the member it names
/// was never extracted, and the link is diagnosed as having nothing to link to.
pub(crate) fn rename_member(
    entry: &mut ArchiveEntry,
    substitutions: &[Substitution],
    strip_components: usize,
) -> bool {
    let strip = |name: PathBuf| {
        if strip_components == 0 {
            Some(name)
        } else {
            strip_leading_components(&name, strip_components)
        }
    };
    let Some(path) = substitute_name(substitutions, &entry.path).and_then(strip) else {
        return false;
    };
    entry.path = path;
    if entry.entry_type == EntryType::Hardlink {
        entry.link_target = entry
            .link_target
            .as_deref()
            .and_then(|target| substitute_link_target(substitutions, target))
            .and_then(strip);
    }
    true
}

/// Drop the first `n` pathname components from an archive member name.
///
/// Returns `None` when the name has no more than `n` components, in which case
/// nothing is left to name a file and the member is skipped -- GNU tar's
/// behavior for `--strip-components`. Empty and `.` components are not counted,
/// so `./a/b` strips the same way `a/b` does; a trailing slash is preserved so a
/// directory member stays recognizable as one.
pub(crate) fn strip_leading_components(name: &std::path::Path, n: usize) -> Option<PathBuf> {
    let bytes = crate::rawpath::as_bytes(name);
    if n == 0 {
        return Some(name.to_path_buf());
    }

    let mut parts: Vec<&[u8]> = bytes
        .split(|&b| b == b'/')
        .filter(|part| !part.is_empty() && *part != b".")
        .collect();
    if parts.len() <= n {
        return None;
    }

    let mut stripped: Vec<u8> = Vec::new();
    for (i, part) in parts.split_off(n).iter().enumerate() {
        if i > 0 {
            stripped.push(b'/');
        }
        stripped.extend_from_slice(part);
    }
    if bytes.last() == Some(&b'/') {
        stripped.push(b'/');
    }
    Some(crate::rawpath::from_bytes(&stripped))
}

/// Check if entry should be extracted
/// Returns:
/// - None: entry should not be extracted (doesn't match patterns or excluded)
/// - Some(true): entry should be extracted
/// - Some(false): entry matches but pattern already matched (first_match mode)
fn should_extract(
    entry: &ArchiveEntry,
    options: &ReadOptions,
    matched_patterns: &mut HashSet<usize>,
) -> Option<bool> {
    let name = crate::rawpath::MatchName::of(&entry.path);
    let path = name.as_str();

    // tar's exclusion list is independent of the pattern operands and wins over
    // them, so it is applied to the stored name before anything else.
    if matches_excluded(&options.exclude_patterns, path) {
        return None;
    }

    // Try matching against both the full path and the path with "./" prefix stripped
    let path_stripped = path.strip_prefix("./").unwrap_or(path);

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
    let matching_pattern = find_matching_pattern_subtree(&options.patterns, path, expand_subtree)
        .or_else(|| {
            // Only worth a second pass when stripping actually changed
            // something; otherwise this repeats the first pass verbatim for
            // every non-matching member.
            if std::ptr::eq(path_stripped, path) {
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
                // Record the match (used for the unmatched-pattern sweep and for
                // -n first-match tracking) and select the entry.
                matched_patterns.insert(pattern_idx);
                Some(true)
            }
        }
        None => {
            // No pattern matched
            if options.exclude {
                Some(true) // Exclude mode: extract entries that don't match
            } else {
                None // Normal mode: skip entries that don't match
            }
        }
    }
}

/// Extract a single entry
fn extract_entry<R: ArchiveReader>(
    archive: &mut R,
    entry: &ArchiveEntry,
    options: &ReadOptions,
    link_sets: &mut LinkSets<CreatedSet>,
    tree: &DirTree,
    pending_dirs: &mut Vec<(MemberPath, ArchiveEntry)>,
) -> PaxResult<()> {
    // -O turns extraction into a dump: nothing is created on disk, so none of
    // the pathname resolution below applies.
    if options.to_stdout {
        return copy_member_to_stdout(archive, entry, options);
    }

    let Some(member) = MemberPath::parse(&entry.path)? else {
        // The member names nothing to create below the anchor. A `.` member
        // is ordinary -- every archive built with `pax -w .` carries one --
        // but an empty name, or one made only of `..` and root components, is
        // not, and dropping it in silence with a zero exit status makes an
        // archive that extracted nothing look like one that extracted
        // everything. GNU tar diagnoses the empty name too.
        if !MemberPath::names_current_directory(&entry.path) {
            crate::error::report_error(&entry.path, "names no file to create; skipping");
        }
        archive.skip_data()?;
        return Ok(());
    };

    // Walk to the directory that will hold the member, creating the
    // intermediate directories POSIX requires for read mode. Every descent
    // refuses to follow a symlink, so this is also what keeps the member inside
    // the extraction directory.
    let parent = tree.parent_of(&member, true)?;
    let pfd = parent.as_fd();
    let name = member.leaf.as_c_str();

    // -u is a policy check, not a security control: it reads the destination
    // and then decides. fstatat cannot be redirected by a planted symlink, and
    // the write that follows is exclusive on this same descriptor, so losing
    // this race can only produce a wrong skip decision, never an escape.
    if options.update && !is_archive_newer_at(entry, pfd, name) {
        archive.skip_data()?;
        return Ok(());
    }

    if options.verbose {
        let mut line = Vec::new();
        crate::escape::push_escaped(
            &mut line,
            crate::rawpath::as_bytes(&member.display),
            crate::escape::stderr_style(),
        );
        line.push(b'\n');
        let _ = std::io::Write::write_all(&mut std::io::stderr().lock(), &line);
    }

    match entry.entry_type {
        EntryType::Directory => {
            if extract_directory(pfd, name, entry, options)? {
                // Its attributes are applied once the subtree exists.
                pending_dirs.push((member, entry.clone()));
            }
            archive.skip_data()?;
        }
        EntryType::Symlink => {
            extract_symlink(pfd, name, entry, options)?;
            archive.skip_data()?;
        }
        EntryType::Hardlink => {
            extract_hardlink(tree, pfd, name, entry, options)?;
            archive.skip_data()?;
        }
        EntryType::Regular => {
            extract_regular(archive, tree, pfd, &member, entry, options, link_sets)?;
            archive.skip_data()?; // Skip padding to block boundary
        }
        EntryType::BlockDevice | EntryType::CharDevice => {
            extract_device(pfd, name, entry, options)?;
            archive.skip_data()?;
        }
        EntryType::Fifo => {
            extract_fifo(pfd, name, entry, options)?;
            archive.skip_data()?;
        }
        EntryType::Socket => {
            // Sockets cannot be extracted from archives
            if options.verbose {
                eprintln!(
                    "{}: skipping socket: {}",
                    crate::error::program_name(),
                    member.display.display()
                );
            }
            archive.skip_data()?;
        }
    }

    Ok(())
}

/// Write a member's contents to standard output (tar `-O`).
///
/// Only members that carry data produce any; a directory, symlink or device
/// contributes nothing, which is what makes `tar -xOf a.tar dir` a usable way to
/// concatenate everything under a directory. The name still goes to stderr
/// under `-v`, so the two streams stay separable.
fn copy_member_to_stdout<R: ArchiveReader>(
    archive: &mut R,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<()> {
    if options.verbose {
        let mut line = Vec::new();
        crate::escape::push_escaped(
            &mut line,
            crate::rawpath::as_bytes(&entry.path),
            crate::escape::stderr_style(),
        );
        line.push(b'\n');
        let _ = std::io::Write::write_all(&mut std::io::stderr().lock(), &line);
    }

    if matches!(entry.entry_type, EntryType::Regular | EntryType::Hardlink) {
        let mut stdout = std::io::stdout().lock();
        let mut buf = [0u8; 8192];
        loop {
            let n = archive.read_data(&mut buf)?;
            if n == 0 {
                break;
            }
            stdout.write_all(&buf[..n])?;
        }
        stdout.flush()?;
    }

    archive.skip_data()?;
    Ok(())
}

/// Extract a directory. Returns whether its attributes should be applied later.
fn extract_directory(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<bool> {
    // Force owner search/write permission so the directory can be populated;
    // the archived mode is applied once the subtree exists. An archived 0555
    // used to be set immediately and then rejected every child with EACCES.
    let mode = (entry.mode as libc::mode_t) | 0o700;

    let r = unsafe { libc::mkdirat(dirfd.as_raw_fd(), name.as_ptr(), mode) };
    if r != 0 {
        let err = std::io::Error::last_os_error();
        if err.raw_os_error() != Some(libc::EEXIST) {
            return Err(err.into());
        }
        // Extracting onto an existing directory is not an error (POSIX), but
        // with -k the existing one is left entirely alone.
        if options.no_clobber {
            return Ok(false);
        }
    }
    Ok(true)
}

/// Extract a symlink
fn extract_symlink(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<()> {
    let target = entry
        .link_target
        .as_ref()
        .ok_or_else(|| PaxError::InvalidHeader("symlink without target".to_string()))?;
    let target_c = CString::new(target.as_os_str().as_bytes())
        .map_err(|_| PaxError::InvalidHeader("path contains null".to_string()))?;

    let created = create_replacing(dirfd, name, options.no_clobber, || {
        let r = unsafe { libc::symlinkat(target_c.as_ptr(), dirfd.as_raw_fd(), name.as_ptr()) };
        if r != 0 {
            return Err(std::io::Error::last_os_error());
        }
        Ok(())
    })?;

    if created {
        // No chmod: a symlink's own mode is meaningless, and
        // fchmodat(AT_SYMLINK_NOFOLLOW) is not portable.
        set_owner_at(dirfd, name, entry, options)?;
        set_times_at(dirfd, name, entry, options)?;
    }
    Ok(())
}

/// Extract a hard link
fn extract_hardlink(
    tree: &DirTree,
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<()> {
    let Some(target) = entry.link_target.clone() else {
        return Err(PaxError::InvalidHeader(
            "hard link target not found".to_string(),
        ));
    };

    let Some(target_member) = MemberPath::parse(&target)? else {
        return Err(PaxError::InvalidHeader(
            "hard link target names no file".to_string(),
        ));
    };
    // Resolve the target the same way, and do not create anything on the way:
    // the target must already have been extracted.
    let target_parent = tree.parent_of(&target_member, false)?;

    // flags = 0, never AT_SYMLINK_FOLLOW. fs::hard_link resolves the whole
    // target path, so a symlink planted at the target -- possibly by an
    // earlier member of this very archive -- could link a file from outside
    // the extraction directory into it. A member linked to its own name, or
    // to a name it already shares, finds the file in place and keeps it.
    link_replacing(
        target_parent.as_raw_fd(),
        &target_member.leaf,
        dirfd,
        name,
        options.no_clobber,
    )?;

    Ok(())
}

/// What extraction remembers about a cpio link set.
struct CreatedSet {
    /// The names created for the set so far, as extracted (after -s and -i).
    names: Vec<PathBuf>,
    /// (st_dev, st_ino) of the file they share on disk -- not the archive's
    /// c_dev/c_ino.
    file: (u64, u64),
    /// Whether that file has its contents yet. newc stores them with the last
    /// name of a set only, so the earlier names are created empty.
    has_data: bool,
}

/// Extract a regular file, linking it to the file already on disk when it is a
/// later name of a cpio link set.
///
/// A set starts at the first of its names that is actually created, so a name
/// left out by a pattern, -u or -k does not stop the next one being extracted.
fn extract_regular<R: ArchiveReader>(
    archive: &mut R,
    tree: &DirTree,
    dirfd: BorrowedFd<'_>,
    member: &MemberPath,
    entry: &ArchiveEntry,
    options: &ReadOptions,
    link_sets: &mut LinkSets<CreatedSet>,
) -> PaxResult<()> {
    let name = member.leaf.as_c_str();
    let Some(key) = LinkSets::<CreatedSet>::key(entry) else {
        extract_file(archive, dirfd, name, entry, options)?;
        return Ok(());
    };

    if let Some(set) = link_sets.get_mut(key) {
        let result = join_link_set(archive, tree, dirfd, member, entry, options, set);
        link_sets.name_seen(key);
        return result;
    }

    if let Some(file) = extract_file(archive, dirfd, name, entry, options)? {
        let set = CreatedSet {
            names: vec![member.display.clone()],
            file,
            has_data: entry.size > 0,
        };
        link_sets.insert(key, entry.nlink, set);
    }
    Ok(())
}

/// Extract a later name of a link set whose file is already on disk.
///
/// When that file has its contents -- or this name brings none -- the name is
/// linked to it. Otherwise this is the name newc stores the data with: it is
/// created fresh, exclusively, and the earlier names are moved over to it.
/// Opening an earlier name to write the data through it could reach whatever
/// someone else had linked there since.
fn join_link_set<R: ArchiveReader>(
    archive: &mut R,
    tree: &DirTree,
    dirfd: BorrowedFd<'_>,
    member: &MemberPath,
    entry: &ArchiveEntry,
    options: &ReadOptions,
    set: &mut CreatedSet,
) -> PaxResult<()> {
    let name = member.leaf.as_c_str();

    if set.has_data || entry.size == 0 {
        // An earlier name may since have been replaced by another member of
        // the same name; only one still holding the file will do.
        let holder = set
            .names
            .iter()
            .find_map(|path| holding_name(tree, path, set.file));
        if let Some((src_dir, src_leaf)) = holder {
            link_replacing(
                src_dir.as_raw_fd(),
                &src_leaf,
                dirfd,
                name,
                options.no_clobber,
            )?;
            // -k leaves an existing name alone, and that name is no part of
            // the set.
            if id_at(dirfd, name) == Some(set.file) {
                set.names.push(member.display.clone());
            }
            return Ok(());
        }
    }

    let holders: Vec<_> = set
        .names
        .iter()
        .filter_map(|path| holding_name(tree, path, set.file).map(|h| (h, path)))
        .collect();
    let Some(file) = extract_file(archive, dirfd, name, entry, options)? else {
        return Ok(());
    };
    let mut names = Vec::with_capacity(holders.len() + 1);
    for ((dir, leaf), path) in holders {
        // These names were created by this extraction, so they are replaced
        // even under -k.
        link_replacing(dirfd.as_raw_fd(), name, dir.as_fd(), &leaf, false)?;
        names.push(path.clone());
    }
    names.push(member.display.clone());
    *set = CreatedSet {
        names,
        file,
        has_data: set.has_data || entry.size > 0,
    };
    Ok(())
}

/// The directory and leaf of an extracted name, if it still names `file`.
fn holding_name(tree: &DirTree, path: &Path, file: (u64, u64)) -> Option<(OwnedFd, CString)> {
    let member = MemberPath::parse(path).ok()??;
    let dir = tree.parent_of(&member, false).ok()?;
    (id_at(dir.as_fd(), &member.leaf) == Some(file)).then_some((dir, member.leaf))
}

/// (st_dev, st_ino) of a name below `dirfd`, not following a symlink.
fn id_at(dirfd: BorrowedFd<'_>, name: &CStr) -> Option<(u64, u64)> {
    stat_at(dirfd, name).map(|st| (st.st_dev as u64, st.st_ino))
}

/// (st_dev, st_ino) of an open file.
fn file_id(meta: &std::fs::Metadata) -> (u64, u64) {
    use std::os::unix::fs::MetadataExt;
    (meta.dev(), meta.ino())
}

/// Extract a block or character device (requires root privileges)
fn extract_device(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<()> {
    // makedev has different signatures on different platforms:
    // - Linux: makedev(major: u32, minor: u32) -> u64
    // - macOS: makedev(major: i32, minor: i32) -> i32
    #[cfg(target_os = "macos")]
    let dev = libc::makedev(entry.devmajor as i32, entry.devminor as i32);
    #[cfg(not(target_os = "macos"))]
    let dev = libc::makedev(entry.devmajor, entry.devminor);
    let type_bits: libc::mode_t = match entry.entry_type {
        EntryType::BlockDevice => libc::S_IFBLK,
        EntryType::CharDevice => libc::S_IFCHR,
        _ => 0,
    };
    // Created without the set-id bits; set_permissions_at applies the archived
    // mode below, once the node exists.
    let mode: libc::mode_t =
        (policy_of(options).creation_mode(&attrs_of(entry)) as libc::mode_t) | type_bits;

    let created = create_replacing(dirfd, name, options.no_clobber, || {
        let r = unsafe { libc::mknodat(dirfd.as_raw_fd(), name.as_ptr(), mode, dev) };
        if r != 0 {
            return Err(std::io::Error::last_os_error());
        }
        Ok(())
    });

    match created {
        Ok(true) => {}
        Ok(false) => return Ok(()),
        Err(PaxError::Io(err)) if err.raw_os_error() == Some(libc::EPERM) => {
            eprintln!("pax: cannot create device: Operation not permitted (requires root)");
            crate::error::note_error();
            return Ok(());
        }
        Err(e) => return Err(e),
    }

    let owner_set = set_owner_at(dirfd, name, entry, options)?;
    set_permissions_at(dirfd, name, entry, options, owner_set)?;
    set_times_at(dirfd, name, entry, options)
}

/// Extract a FIFO
fn extract_fifo(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<()> {
    let created = create_replacing(dirfd, name, options.no_clobber, || {
        let r = unsafe {
            libc::mkfifoat(
                dirfd.as_raw_fd(),
                name.as_ptr(),
                policy_of(options).creation_mode(&attrs_of(entry)) as libc::mode_t,
            )
        };
        if r != 0 {
            return Err(std::io::Error::last_os_error());
        }
        Ok(())
    });

    match created {
        Ok(true) => {}
        Ok(false) => return Ok(()),
        Err(PaxError::Io(err)) if err.raw_os_error() == Some(libc::EPERM) => {
            eprintln!("pax: cannot create FIFO: Operation not permitted");
            crate::error::note_error();
            return Ok(());
        }
        Err(e) => return Err(e),
    }

    let owner_set = set_owner_at(dirfd, name, entry, options)?;
    set_permissions_at(dirfd, name, entry, options, owner_set)?;
    set_times_at(dirfd, name, entry, options)
}

/// Extract a regular file, returning the (st_dev, st_ino) of the file created,
/// or `None` when -k left an existing one in place.
fn extract_file<R: ArchiveReader>(
    archive: &mut R,
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<Option<(u64, u64)>> {
    let flags = libc::O_WRONLY | libc::O_CREAT | libc::O_EXCL | libc::O_NOFOLLOW | libc::O_CLOEXEC;
    let mut opened: Option<File> = None;

    let created = create_replacing(dirfd, name, options.no_clobber, || {
        let fd = unsafe {
            libc::openat(
                dirfd.as_raw_fd(),
                name.as_ptr(),
                flags,
                policy_of(options).creation_mode(&attrs_of(entry)) as libc::c_uint,
            )
        };
        if fd < 0 {
            return Err(std::io::Error::last_os_error());
        }
        opened = Some(unsafe { File::from_raw_fd(fd) });
        Ok(())
    })?;

    let Some(mut file) = opened else {
        // -k: the name already exists, so the member is skipped. Its data is
        // consumed by the caller's skip_data.
        debug_assert!(!created);
        return Ok(None);
    };

    copy_file_data(archive, &mut file, entry.size)?;

    // Through the descriptor the data was just written to, not by name.
    set_attrs_fd(file.as_fd(), &attrs_of(entry), &policy_of(options))?;
    Ok(Some(file_id(&file.metadata()?)))
}

/// Copy file data from archive to file
fn copy_file_data<R: ArchiveReader>(archive: &mut R, file: &mut File, size: u64) -> PaxResult<()> {
    let mut remaining = size;
    let mut buf = [0u8; 8192];

    while remaining > 0 {
        let to_read = std::cmp::min(remaining, buf.len() as u64) as usize;
        let n = archive.read_data(&mut buf[..to_read])?;
        if n == 0 {
            break;
        }
        file.write_all(&buf[..n])?;
        remaining -= n as u64;
    }

    Ok(())
}

/// Whether the archive member is newer than what is already at `name`.
fn is_archive_newer_at(entry: &ArchiveEntry, dirfd: BorrowedFd<'_>, name: &CStr) -> bool {
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    let r = unsafe {
        libc::fstatat(
            dirfd.as_raw_fd(),
            name.as_ptr(),
            &mut st,
            libc::AT_SYMLINK_NOFOLLOW,
        )
    };
    if r != 0 {
        return true; // nothing there: extract it
    }
    entry.mtime as i64 > st.st_mtime
}

/// The ids to give an extracted file.
///
/// POSIX, ustar Interchange Format: "When the file is restored by a
/// privileged, protection-preserving version of the utility, the user and
/// group databases shall be scanned for these names. If found, the user and
/// group IDs contained within these files shall be used rather than the values
/// contained within the uid and gid fields." The pax `uname` record says it
/// more strongly: it "shall override the uid and uname fields in the following
/// header block(s), and any uid extended header record."
///
/// A name this host's database does not know leaves the numeric field in
/// force, which is the "If found" in that sentence.
///
/// The scan is not conditioned on being privileged. An unprivileged
/// extraction may still legitimately set a file's group to one the caller
/// belongs to, and where it may not, the chown fails with EPERM exactly as it
/// would have with the numeric id -- already diagnosed, in one place, by
/// `set_attrs_fd`. Testing euid here would only make the resolved id depend on
/// who ran pax.
fn owner_ids(entry: &ArchiveEntry) -> (u32, u32) {
    let uid = entry
        .uname
        .as_deref()
        .and_then(crate::userdb::uid_for_name)
        .unwrap_or(entry.uid);
    let gid = entry
        .gname
        .as_deref()
        .and_then(crate::userdb::gid_for_name)
        .unwrap_or(entry.gid);
    (uid, gid)
}

/// The archived attributes of a member, in the shared shape.
fn attrs_of(entry: &ArchiveEntry) -> Attrs {
    let (uid, gid) = owner_ids(entry);
    Attrs {
        mode: entry.mode,
        uid,
        gid,
        mtime: entry.mtime as i64,
        mtime_nsec: entry.mtime_nsec as i64,
        atime: entry.atime.map(|a| a as i64),
        atime_nsec: entry.atime_nsec as i64,
    }
}

/// What `-p` asked to keep, in the shared shape.
fn policy_of(options: &ReadOptions) -> AttrPolicy {
    AttrPolicy {
        preserve_owner: options.preserve_owner,
        preserve_perms: options.preserve_perms,
        preserve_mtime: options.preserve_mtime,
        preserve_atime: options.preserve_atime,
        umask: options.umask,
    }
}

/// Set file permissions on a name below `dirfd`.
///
/// `fchmodat` has no portable way to refuse a symbolic link -- Linux rejects
/// `AT_SYMLINK_NOFOLLOW` outright -- so the type is checked first and a link is
/// refused. Callers that hold a descriptor for the file should use
/// `set_attrs_fd` instead, which cannot be redirected at all.
fn set_permissions_at(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    entry: &ArchiveEntry,
    options: &ReadOptions,
    owner_set: bool,
) -> PaxResult<()> {
    let Some(st) = stat_at(dirfd, name) else {
        return Err(std::io::Error::last_os_error().into());
    };
    if st.st_mode & libc::S_IFMT == libc::S_IFLNK {
        // Whatever this name was when it was created, it is a symbolic link
        // now. Following it would apply the archived mode to the file it
        // points at, anywhere on the system.
        return Err(PaxError::InvalidHeader(
            "refusing to set permissions through a symbolic link".to_string(),
        ));
    }

    let mode = policy_of(options).mode(&attrs_of(entry), owner_set);
    let r = unsafe { libc::fchmodat(dirfd.as_raw_fd(), name.as_ptr(), mode as libc::mode_t, 0) };
    if r != 0 {
        return Err(std::io::Error::last_os_error().into());
    }
    Ok(())
}

/// Set file owner (uid/gid) - requires privileges. Returns whether it was set,
/// which decides whether `set_permissions_at` may apply set-id bits.
fn set_owner_at(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<bool> {
    if !options.preserve_owner {
        return Ok(false);
    }

    let (uid, gid) = owner_ids(entry);
    chown_result(unsafe {
        libc::fchownat(
            dirfd.as_raw_fd(),
            name.as_ptr(),
            uid,
            gid,
            libc::AT_SYMLINK_NOFOLLOW,
        )
    })
}

/// Set file access and modification times
fn set_times_at(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<()> {
    let Some(times) = policy_of(options).times(&attrs_of(entry)) else {
        return Ok(());
    };

    let result = unsafe {
        libc::utimensat(
            dirfd.as_raw_fd(),
            name.as_ptr(),
            times.as_ptr(),
            libc::AT_SYMLINK_NOFOLLOW,
        )
    };

    if result != 0 {
        let err = std::io::Error::last_os_error();
        // EPERM means we don't have permission - warn but continue
        if err.raw_os_error() == Some(libc::EPERM) {
            eprintln!("pax: warning: cannot set times: Operation not permitted");
        } else {
            eprintln!("pax: warning: cannot set times: {}", err);
        }
        crate::error::note_error();
    }

    Ok(())
}

/// Apply the archived attributes of every extracted directory, deepest first.
///
/// Directories cannot take their attributes at creation time: an archived mode
/// denying write or search stops its own contents being written, and the mtime
/// is invalidated by every child created afterwards. Both are applied here,
/// once the whole archive has been extracted.
fn apply_pending_dirs(
    tree: &DirTree,
    pending: &mut [(MemberPath, ArchiveEntry)],
    options: &ReadOptions,
) {
    // Deepest first, so a parent is stamped only after its children are done.
    pending.sort_by_key(|(member, _)| std::cmp::Reverse(member.depth()));

    for (member, entry) in pending.iter() {
        let parent = match tree.parent_of(member, false) {
            Ok(p) => p,
            Err(e) => {
                crate::error::report_error(&member.display, e);
                continue;
            }
        };
        let pfd = parent.as_fd();
        let name = member.leaf.as_c_str();

        // Reopen the directory itself and work through that descriptor. This
        // pass runs after the whole archive has been extracted, so the name
        // need not still be the directory that was created for this member --
        // a later member can have replaced it with a symbolic link, and
        // applying the archived mode by name would then chmod whatever the
        // link points at. `O_NOFOLLOW` refuses the link, and `O_DIRECTORY`
        // refuses anything else that took its place.
        let flags = libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC;
        let fd = unsafe { libc::openat(pfd.as_raw_fd(), name.as_ptr(), flags) };
        if fd < 0 {
            crate::error::report_error(
                &member.display,
                PaxError::from(std::io::Error::last_os_error()),
            );
            continue;
        }
        let dir = unsafe { OwnedFd::from_raw_fd(fd) };

        if let Err(e) = set_attrs_fd(dir.as_fd(), &attrs_of(entry), &policy_of(options)) {
            crate::error::report_error(&member.display, e);
        }
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use plib::tmp::TempDir;
    use std::os::unix::fs::PermissionsExt;

    /// POSIX, ustar Interchange Format: "When the file is restored by a
    /// privileged, protection-preserving version of the utility, the user and
    /// group databases shall be scanned for these names. If found, the user
    /// and group IDs contained within these files shall be used rather than
    /// the values contained within the uid and gid fields." The pax `uname`
    /// record puts it more strongly still: it "shall override the uid and
    /// uname fields in the following header block(s), and any uid extended
    /// header record."
    ///
    /// Extraction chowned by the numeric fields alone, so an archive carried
    /// between hosts restored each file to whichever account happened to hold
    /// the originating host's uid -- the problem uname exists to solve.
    #[test]
    fn test_owner_prefers_the_recorded_name_over_the_numeric_id() {
        let euid = unsafe { libc::geteuid() };
        let egid = unsafe { libc::getegid() };
        // A uid with no database entry (a sparse container) leaves nothing to
        // assert about; every real account has one.
        let (Some(user), Some(group)) =
            (plib::user::get_by_uid(euid), plib::group::get_by_gid(egid))
        else {
            return;
        };

        // The numeric fields deliberately disagree with the names, as they
        // would after an archive moved between hosts.
        let named = ArchiveEntry {
            uid: 0xffff_fff0,
            gid: 0xffff_fff0,
            uname: Some(user.name.clone().into_bytes()),
            gname: Some(group.name.clone().into_bytes()),
            ..Default::default()
        };
        let attrs = attrs_of(&named);
        assert_eq!(
            (attrs.uid, attrs.gid),
            (euid, egid),
            "a name the database knows must override the numeric field"
        );

        // "If found": a name this host does not know leaves the numeric field
        // in force rather than failing or extracting as the caller.
        let unknown = ArchiveEntry {
            uid: 4242,
            gid: 4243,
            uname: Some(b"nosuchuser.pax.test".to_vec()),
            gname: Some(b"nosuchgroup.pax.test".to_vec()),
            ..Default::default()
        };
        let attrs = attrs_of(&unknown);
        assert_eq!((attrs.uid, attrs.gid), (4242, 4243));

        // And a format that records no name at all -- cpio has no such field
        // -- is unaffected.
        let bare = ArchiveEntry {
            uid: 7,
            gid: 8,
            ..Default::default()
        };
        let attrs = attrs_of(&bare);
        assert_eq!((attrs.uid, attrs.gid), (7, 8));
    }

    #[test]
    fn test_rename_member_renames_hardlink_target_only() {
        let subs = [Substitution::parse(",^,P/,").unwrap()];
        let linked = |entry_type| {
            let mut entry = ArchiveEntry::new(PathBuf::from("b"), entry_type);
            entry.link_target = Some(PathBuf::from("a"));
            entry
        };

        // A hard link's target is a member name and follows the member.
        let mut hard = linked(EntryType::Hardlink);
        assert!(rename_member(&mut hard, &subs, 0));
        assert_eq!(hard.path, PathBuf::from("P/b"));
        assert_eq!(hard.link_target, Some(PathBuf::from("P/a")));

        // A symlink's target is its contents, and stays as archived.
        let mut soft = linked(EntryType::Symlink);
        assert!(rename_member(&mut soft, &subs, 0));
        assert_eq!(soft.link_target, Some(PathBuf::from("a")));

        // Substitution, then stripping, applies to the target as well.
        let mut stripped = linked(EntryType::Hardlink);
        assert!(rename_member(&mut stripped, &subs, 1));
        assert_eq!(stripped.path, PathBuf::from("b"));
        assert_eq!(stripped.link_target, Some(PathBuf::from("a")));

        // A target -s ignores leaves the link nothing to link to; the member's
        // own name is kept so extraction can diagnose it.
        let drop_a = [Substitution::parse(",^a$,,").unwrap()];
        let mut orphan = linked(EntryType::Hardlink);
        assert!(rename_member(&mut orphan, &drop_a, 0));
        assert_eq!(orphan.link_target, None);

        // A member whose own name is ignored is dropped.
        let drop_b = [Substitution::parse(",^b$,,").unwrap()];
        assert!(!rename_member(&mut linked(EntryType::Hardlink), &drop_b, 0));
    }

    #[test]
    fn test_strip_leading_components() {
        // The function takes and returns pathnames, which are bytes; the
        // fixtures here are ASCII, so a helper keeps the assertions readable.
        fn strip(name: &str, n: usize) -> Option<String> {
            strip_leading_components(std::path::Path::new(name), n)
                .map(|p| String::from_utf8(crate::rawpath::as_bytes(&p).to_vec()).unwrap())
        }

        assert_eq!(strip("a/b/c", 0).as_deref(), Some("a/b/c"));
        assert_eq!(strip("a/b/c", 1).as_deref(), Some("b/c"));
        assert_eq!(strip("a/b/c", 2).as_deref(), Some("c"));

        // Nothing is left to name a file, so the member is dropped.
        assert_eq!(strip("a/b/c", 3), None);
        assert_eq!(strip("a/b/c", 4), None);
        assert_eq!(strip("a", 1), None);

        // "." and empty components are noise, not components: "./a/b" strips
        // exactly the way "a/b" does.
        assert_eq!(strip("./a/b", 1).as_deref(), Some("b"));
        assert_eq!(strip("a//b/c", 1).as_deref(), Some("b/c"));

        // A directory member keeps its trailing slash.
        assert_eq!(strip("a/b/", 1).as_deref(), Some("b/"));

        // A component that is not UTF-8 is a component like any other.
        let stripped =
            strip_leading_components(&crate::rawpath::from_bytes(b"a/n\xffm/c"), 1).unwrap();
        assert_eq!(crate::rawpath::as_bytes(&stripped), b"n\xffm/c");
    }

    /// Without explicit `-p p`/`-p e` the extracted mode is the archived mode
    /// masked by the umask (normal file-creation action); with preservation the
    /// exact archived mode is restored.
    #[test]
    fn test_set_permissions_umask_vs_preserve() {
        let tmp = TempDir::new().unwrap();
        let path = tmp.path().join("member");
        std::fs::File::create(&path).unwrap();

        // Attributes are applied relative to an open parent directory now.
        let dir = std::fs::File::open(tmp.path()).unwrap();
        let name = CString::new("member").unwrap();

        let entry = ArchiveEntry {
            path: path.clone(),
            mode: 0o777,
            entry_type: EntryType::Regular,
            ..Default::default()
        };

        // Not preserved: 0o777 & ~0o022 == 0o755.
        let opts = ReadOptions {
            preserve_perms: false,
            umask: 0o022,
            ..Default::default()
        };
        set_permissions_at(dir.as_fd(), &name, &entry, &opts, false).unwrap();
        assert_eq!(
            std::fs::metadata(&path).unwrap().permissions().mode() & 0o777,
            0o755
        );

        // Preserved: exact 0o777 regardless of umask.
        let opts = ReadOptions {
            preserve_perms: true,
            umask: 0o022,
            ..Default::default()
        };
        set_permissions_at(dir.as_fd(), &name, &entry, &opts, false).unwrap();
        assert_eq!(
            std::fs::metadata(&path).unwrap().permissions().mode() & 0o777,
            0o777
        );
    }
}
