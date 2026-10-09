//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Read mode implementation - extract archive contents

use crate::archive::{ArchiveEntry, ArchiveReader, EntryType, LinkSets};
use crate::error::{PaxError, PaxResult};
use crate::formats::OptionRecords;
use crate::interactive::{InteractivePrompter, RenameResult};
use crate::modes::anchored::{
    attrs_withheld, create_replacing, link_replacing, link_replacing_with, make_dir_at,
    set_attrs_fd, set_made_node_attrs, unlink_at, AttrPolicy, Attrs, DirAttrs, DirTree, MemberPath,
    PendingDirs,
};
use crate::modes::select::Selector;
use crate::pattern::Pattern;
use crate::subst::{substitute_link_target, substitute_name, Substitution};
use plib::madefs::lstat_at;
use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::Write;
use std::os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd, OwnedFd};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::rc::Rc;

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
    /// cpio: the `update` check keeps a newer file at the name the member is
    /// extracted under, made once `-r` has renamed it -- not, as pax's `-u`,
    /// part of selecting the member by its archived name.
    pub update_final_name: bool,
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
            update_final_name: false,
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

/// Extract the members of an archive
pub fn extract_archive<R: ArchiveReader>(archive: &mut R, options: &ReadOptions) -> PaxResult<()> {
    // Extraction is anchored at an open descriptor for the working directory,
    // and every member path is resolved relative to it without following a
    // symlink.
    let tree = DirTree::open_cwd()?;
    // Directories take their archived attributes only once the whole archive
    // has been extracted -- and still do when a fatal error stops it early,
    // for the directories that were created by then.
    let mut pending_dirs = PendingDirs::default();
    let result = extract_members(archive, options, &tree, &mut pending_dirs);
    pending_dirs.apply(&tree, &policy_of(options));
    result
}

/// The member loop of `extract_archive`.
fn extract_members<R: ArchiveReader>(
    archive: &mut R,
    options: &ReadOptions,
    tree: &DirTree,
    pending_dirs: &mut PendingDirs,
) -> PaxResult<()> {
    let mut link_sets: LinkSets<CreatedSet> = LinkSets::default();
    let mut pins = PinBudget::new();
    let mut selector = Selector::new(
        &options.patterns,
        options.exclude,
        options.first_match,
        options.dir_only,
        &options.exclude_patterns,
    );
    let option_records = caller_option_records(archive, &options.format_options)?;

    // Create interactive prompter if needed
    let mut prompter = if options.interactive {
        Some(InteractivePrompter::new()?)
    } else {
        None
    };

    // Whether the loop met the end of the archive, rather than stopping
    // short of it under -n.
    let mut reached_end = true;
    while let Some(mut entry) = archive.read_entry()? {
        if let Some(ref records) = option_records {
            records.apply(&mut entry);
        }
        link_sets.count_name(&entry);
        if select_member(&mut selector, &mut entry, options, &mut prompter, tree)? {
            // Per POSIX CONSEQUENCES OF ERRORS: diagnose a per-file failure and
            // set a non-zero exit, but continue with the next member. Skip any
            // unconsumed data of the failed entry to realign the reader.
            // A failure every later member would meet too -- -O's output
            // gone, end of file on the terminal -- ends the run instead.
            let r = extract_entry(archive, &entry, options, &mut link_sets, tree, pending_dirs);
            report_unless_fatal(&entry, r)?;
        } else if let Some(set) = link_sets.find_mut(&entry) {
            let r = fill_link_set(archive, tree, &entry, options, set);
            report_unless_fatal(&entry, r)?;
        }
        archive.skip_data()?;
        // A set no data can still come for needs its file pinned no longer.
        if let Some(set) = link_sets.settled_mut(&entry) {
            set.unpin();
        }
        pins.account(&entry, &mut link_sets);
        // A newc set's data comes with its last name, which -n must still
        // read even when every pattern has been used by an earlier one.
        if selector.is_done() && link_sets.all_settled() {
            reached_end = false;
            break;
        }
    }

    selector.report_unmatched();
    archive.finish(reached_end)
}

/// Diagnose a member's failure and carry on, unless it is one that ends the
/// run (`modes::is_fatal`), which is handed back instead.
fn report_unless_fatal(entry: &ArchiveEntry, result: PaxResult<()>) -> PaxResult<()> {
    match result {
        Err(e) if crate::modes::is_fatal(&e) => Err(e),
        Err(e) => {
            crate::error::report_error(&entry.path, e);
            Ok(())
        }
        Ok(()) => Ok(()),
    }
}

/// Decide whether a member is extracted, renaming it as -s and -i direct.
///
/// In POSIX's order: the patterns as modified by -c, -n and -u select it, and
/// then -s and -i rename it. `-u` compares against the file of the member's
/// own name, before any renaming, and a member it turns away does not use up
/// a pattern under -n. cpio's refusal to replace a newer file instead looks
/// at the file the member would replace, under the name it ends up with.
fn select_member(
    selector: &mut Selector,
    entry: &mut ArchiveEntry,
    options: &ReadOptions,
    prompter: &mut Option<InteractivePrompter>,
    tree: &DirTree,
) -> PaxResult<bool> {
    let Some(selection) = selector.select(entry) else {
        return Ok(false);
    };
    if options.update && !options.update_final_name && !is_archive_newer(tree, entry) {
        return Ok(false);
    }
    selector.take(selection);

    // -s, then --strip-components, both before the name is offered for
    // renaming, so an interactive prompt shows the name that will actually be
    // created.
    if !rename_member(entry, &options.substitutions, options.strip_components) {
        return Ok(false);
    }
    if let Some(p) = prompter {
        match p.prompt(&entry.path)? {
            RenameResult::Skip => return Ok(false),
            RenameResult::UseOriginal => {}
            RenameResult::Rename(new_path) => entry.path = new_path,
        }
    }
    Ok(!(options.update && options.update_final_name && !is_archive_newer(tree, entry)))
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

/// Extract a single entry
fn extract_entry<R: ArchiveReader>(
    archive: &mut R,
    entry: &ArchiveEntry,
    options: &ReadOptions,
    link_sets: &mut LinkSets<CreatedSet>,
    tree: &DirTree,
    pending_dirs: &mut PendingDirs,
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
            let decided = extract_directory(tree, pfd, &member, entry, options)?;
            archive.skip_data()?;
            match decided {
                // Its attributes are applied once the subtree exists, if it
                // is still this directory then.
                DirAttrs::Apply(id) => pending_dirs.push(&member, id, attrs_of(entry, options)),
                DirAttrs::Keep => {}
                DirAttrs::Withheld(_) => return Err(attrs_withheld()),
            }
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
            // Nothing makes a socket out of an archive member: bind(2)
            // creates one only to listen on. libarchive refuses one the same
            // way; skipping it in silence reported an incomplete extraction
            // as a complete one.
            crate::error::report_error(&member.display, "cannot create a socket; skipped");
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
            stdout.write_all(&buf[..n]).map_err(PaxError::StdoutWrite)?;
        }
        stdout.flush().map_err(PaxError::StdoutWrite)?;
    }

    archive.skip_data()?;
    Ok(())
}

/// Extract a directory, saying what becomes of its attributes.
fn extract_directory(
    tree: &DirTree,
    dirfd: BorrowedFd<'_>,
    member: &MemberPath,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<DirAttrs> {
    make_dir_at(tree, dirfd, member, entry.mode, options.no_clobber)
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
        set_made_attrs(dirfd, name, libc::S_IFLNK, entry, options)?;
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

/// The link sets whose files are pinned (`CreatedSet::pin`), oldest first,
/// and how many may be at once.
///
/// A newc set waits for its data until c_nlink of its names have been read,
/// and an archive can start any number of sets that never finish: one name
/// each, with a c_nlink of 2 or of 0xffffffff. Pinned until the end, one
/// descriptor each, they would leave none for the members after them. Past
/// the budget the oldest set's pin is closed, and that set falls back to the
/// bare `(st_dev, st_ino)` check every set relies on once its data is in.
struct PinBudget {
    /// The keys (`LinkSets::key`) of the sets holding a pin, oldest first.
    held: std::collections::VecDeque<(u64, u64)>,
    /// How many may be held at once: a quarter of the descriptor limit, and
    /// at most 256.
    limit: usize,
}

impl PinBudget {
    fn new() -> Self {
        let mut lim = libc::rlimit {
            rlim_cur: 0,
            rlim_max: 0,
        };
        let soft = if unsafe { libc::getrlimit(libc::RLIMIT_NOFILE, &mut lim) } == 0 {
            lim.rlim_cur
        } else {
            0
        };
        PinBudget {
            held: std::collections::VecDeque::new(),
            limit: usize::try_from(soft / 4).map_or(256, |quarter| quarter.min(256)),
        }
    }

    /// Account for the set `entry` is a name of, after the member: forget it
    /// once it holds no pin, note it when it has taken one, and close the
    /// oldest pin when that puts the budget over.
    fn account(&mut self, entry: &ArchiveEntry, link_sets: &mut LinkSets<CreatedSet>) {
        let Some(key) = LinkSets::<CreatedSet>::key(entry) else {
            return;
        };
        let Some(set) = link_sets.by_key_mut(key) else {
            return;
        };
        let held_at = self.held.iter().position(|&k| k == key);
        match (set.pin.is_some(), held_at) {
            (false, Some(at)) => {
                self.held.remove(at);
            }
            (true, None) => self.held.push_back(key),
            _ => {}
        }
        while self.held.len() > self.limit {
            let Some(oldest) = self.held.pop_front() else {
                break;
            };
            if let Some(set) = link_sets.by_key_mut(oldest) {
                set.unpin();
            }
        }
    }
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
    /// The file held open while data may still come for it on a later name
    /// (`LinkSets::settled_mut`), within the budget (`PinBudget`). Its names
    /// can be replaced by other members meanwhile, and a filesystem that
    /// reuses inode numbers (ext4) then hands this file's number to the next
    /// file created, which `file` would take for this one. Open, it keeps its
    /// number.
    pin: Option<OwnedFd>,
}

impl CreatedSet {
    /// The set as created at `name` below `dirfd`: pinned when created empty,
    /// until the extract loop finds no data can still come for it (`unpin`).
    fn new(
        names: Vec<PathBuf>,
        file: (u64, u64),
        has_data: bool,
        dirfd: BorrowedFd<'_>,
        name: &CStr,
    ) -> Self {
        let pin = (!has_data).then(|| pin_file(dirfd, name, file)).flatten();
        CreatedSet {
            names,
            file,
            has_data,
            pin,
        }
    }

    /// Close the pin once no data can still come for the set.
    fn unpin(&mut self) {
        self.pin = None;
    }

    /// The names that still hold the set's file, each with the directory and
    /// leaf it was found at: an earlier name may since have been replaced by
    /// another member of the same name.
    fn holders(&self, tree: &DirTree) -> Vec<((Rc<OwnedFd>, CString), PathBuf)> {
        self.names
            .iter()
            .filter_map(|path| holding_name(tree, path, self.file).map(|h| (h, path.clone())))
            .collect()
    }
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
    if let Some(set) = link_sets.find_mut(entry) {
        return join_link_set(archive, tree, dirfd, member, entry, options, set);
    }

    if let Some(file) = extract_file(archive, dirfd, member.leaf.as_c_str(), entry, options)? {
        link_sets.insert(entry, || {
            let names = vec![member.display.clone()];
            CreatedSet::new(names, file, entry.size > 0, dirfd, &member.leaf)
        });
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
            // To the set's file, pinned: the holder was found holding it, but
            // its name can be given another file before the link is made.
            link_replacing_with(
                src_dir.as_raw_fd(),
                &src_leaf,
                false,
                Some(set.file),
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

    let holders = set.holders(tree);
    let Some(file) = extract_file(archive, dirfd, name, entry, options)? else {
        // -k kept what was there; the data is still the earlier names'.
        return fill_link_set(archive, tree, entry, options, set);
    };
    let mut names = move_names_to(holders, dirfd, name, file)?;
    names.push(member.display.clone());
    // Created empty when no earlier name survived to link to: the data is
    // then still to come, on a later name.
    *set = CreatedSet::new(names, file, entry.size > 0, dirfd, name);
    Ok(())
}

/// Bring the data of a newc set, which arrives with its last name, to the
/// earlier names when that one is not extracted -- not selected, renamed away,
/// kept by -k. Left alone, the names that were extracted stay empty.
///
/// As in `join_link_set`, the data goes into a file created fresh, here under
/// a temporary name beside the first of them, and the names are moved over to
/// it rather than the data written through one of them.
fn fill_link_set<R: ArchiveReader>(
    archive: &mut R,
    tree: &DirTree,
    entry: &ArchiveEntry,
    options: &ReadOptions,
    set: &mut CreatedSet,
) -> PaxResult<()> {
    if set.has_data || entry.size == 0 {
        return Ok(());
    }
    let holders = set.holders(tree);
    let Some(((dir, _), _)) = holders.first() else {
        return Ok(());
    };
    let dir = Rc::clone(dir);
    let (temp, file) = create_temp_file(dir.as_fd(), entry, options)?;
    let filled = write_file_data(archive, file, entry, options)
        .and_then(|id| Ok((id, move_names_to(holders, dir.as_fd(), &temp, id)?)));
    let removed = unlink_at(dir.as_fd(), &temp);
    let (file, names) = filled?;
    removed?;
    *set = CreatedSet {
        names,
        file,
        has_data: true,
        pin: None,
    };
    Ok(())
}

/// Link each of `holders` -- names of a set, with the directory and leaf they
/// were found at -- to `name` in `dirfd`, the file `file` this extraction has
/// just made, returning their paths.
fn move_names_to(
    holders: Vec<((Rc<OwnedFd>, CString), PathBuf)>,
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    file: (u64, u64),
) -> PaxResult<Vec<PathBuf>> {
    let mut names = Vec::with_capacity(holders.len() + 1);
    for ((dir, leaf), path) in holders {
        // These names were created by this extraction, so they are replaced
        // even under -k. The link is to the file made, pinned, never to
        // whatever has been put at its name since.
        let from = dirfd.as_raw_fd();
        link_replacing_with(from, name, false, Some(file), dir.as_fd(), &leaf, false)?;
        names.push(path);
    }
    Ok(names)
}

/// The directory and leaf of an extracted name, if it still names `file`.
fn holding_name(tree: &DirTree, path: &Path, file: (u64, u64)) -> Option<(Rc<OwnedFd>, CString)> {
    let member = MemberPath::parse(path).ok()??;
    let dir = tree.parent_of(&member, false).ok()?;
    (id_at(dir.as_fd(), &member.leaf) == Some(file)).then_some((dir, member.leaf))
}

/// The file at `name` below `dirfd`, opened without following a symlink,
/// blocking, or adopting a terminal as the controlling one, when it is still
/// the file `file`. A failure only leaves the set unpinned.
fn pin_file(dirfd: BorrowedFd<'_>, name: &CStr, file: (u64, u64)) -> Option<OwnedFd> {
    let flags =
        libc::O_RDONLY | libc::O_NOFOLLOW | libc::O_NONBLOCK | libc::O_NOCTTY | libc::O_CLOEXEC;
    let fd = unsafe { libc::openat(dirfd.as_raw_fd(), name.as_ptr(), flags) };
    if fd < 0 {
        return None;
    }
    let file_held = std::fs::File::from(unsafe { OwnedFd::from_raw_fd(fd) });
    (file_id(&file_held.metadata().ok()?) == file).then(|| file_held.into())
}

/// (st_dev, st_ino) of a name below `dirfd`, not following a symlink.
fn id_at(dirfd: BorrowedFd<'_>, name: &CStr) -> Option<(u64, u64)> {
    lstat_at(dirfd.as_raw_fd(), name)
        .ok()
        .map(|st| crate::modes::anchored::file_id(&st))
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
    // Created without the set-id bits; set_made_attrs applies the archived
    // mode below, once the node exists.
    let mode: libc::mode_t =
        (policy_of(options).creation_mode(&attrs_of(entry, options)) as libc::mode_t) | type_bits;

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

    set_made_attrs(dirfd, name, type_bits, entry, options)
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
                policy_of(options).creation_mode(&attrs_of(entry, options)) as libc::mode_t,
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

    set_made_attrs(dirfd, name, libc::S_IFIFO, entry, options)
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
    let mut opened: Option<File> = None;
    let created = create_replacing(dirfd, name, options.no_clobber, || {
        opened = Some(create_file(dirfd, name, entry, options)?);
        Ok(())
    })?;

    let Some(file) = opened else {
        // -k: the name already exists, so the member is skipped. Its data is
        // consumed by the caller's skip_data.
        debug_assert!(!created);
        return Ok(None);
    };
    write_file_data(archive, file, entry, options).map(Some)
}

/// Create `name` in `dirfd` exclusively, never following a symlink, with the
/// mode the member is created with.
fn create_file(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> std::io::Result<File> {
    let flags = libc::O_WRONLY | libc::O_CREAT | libc::O_EXCL | libc::O_NOFOLLOW | libc::O_CLOEXEC;
    let fd = unsafe {
        libc::openat(
            dirfd.as_raw_fd(),
            name.as_ptr(),
            flags,
            policy_of(options).creation_mode(&attrs_of(entry, options)) as libc::c_uint,
        )
    };
    if fd < 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(unsafe { File::from_raw_fd(fd) })
}

/// Create a file under a fresh temporary name in `dirfd`, for `fill_link_set`.
fn create_temp_file(
    dirfd: BorrowedFd<'_>,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<(CString, File)> {
    const TRIES: u32 = 100;
    for n in 0..TRIES {
        let name = CString::new(format!(".pax-link.{}.{}", std::process::id(), n))
            .expect("no NUL in a formatted number");
        match create_file(dirfd, &name, entry, options) {
            Ok(file) => return Ok((name, file)),
            Err(e) if e.raw_os_error() == Some(libc::EEXIST) => continue,
            Err(e) => return Err(e.into()),
        }
    }
    Err(std::io::Error::from_raw_os_error(libc::EEXIST).into())
}

/// Write a member's data into `file`, just created for it, and give it the
/// member's attributes, returning its (st_dev, st_ino).
fn write_file_data<R: ArchiveReader>(
    archive: &mut R,
    mut file: File,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<(u64, u64)> {
    copy_file_data(archive, &mut file, entry.size)?;

    // Through the descriptor the data was just written to, not by name.
    set_attrs_fd(file.as_fd(), &attrs_of(entry, options), &policy_of(options))?;
    let id = file_id(&file.metadata()?);
    // A filesystem that defers writes -- NFS, a quota checked late -- reports
    // their failure here, and the member is then not extracted after all.
    crate::blocked_io::close_file(file)?;
    Ok(id)
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

/// `-u`: whether the archive member is newer than the file already at its
/// name, or there is none. Times compare to the nanosecond; a member whose
/// format holds whole seconds has none to add.
///
/// A policy check, not a security control: it reads the destination and then
/// decides. Every component is opened without following a symlink, and the
/// file is examined with fstatat, so a planted symlink cannot redirect it;
/// losing a race here can only produce a wrong skip decision, never an escape.
fn is_archive_newer(tree: &DirTree, entry: &ArchiveEntry) -> bool {
    let Ok(Some(member)) = MemberPath::parse(&entry.path) else {
        return true;
    };
    let Ok(parent) = tree.parent_of(&member, false) else {
        return true; // no such directory, so nothing there: extract it
    };
    // A directory created here only to hold earlier members is not one.
    lstat_at(parent.as_raw_fd(), &member.leaf)
        .ok()
        .is_none_or(|st| {
            tree.is_implicit(&st, &member)
                || (entry.mtime, i64::from(entry.mtime_nsec)) > tree.mtime_before_run(&st)
        })
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
///
/// The owner is only ever applied under `-p o`, so only then are the user and
/// group databases consulted for it: a lookup per member is not free.
fn attrs_of(entry: &ArchiveEntry, options: &ReadOptions) -> Attrs {
    let (uid, gid) = if options.preserve_owner {
        owner_ids(entry)
    } else {
        (entry.uid, entry.gid)
    };
    Attrs {
        mode: entry.mode,
        uid,
        gid,
        mtime: entry.mtime,
        mtime_nsec: entry.mtime_nsec as i64,
        atime: entry.atime,
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

/// The member's owner, mode and times, for the FIFO, device or symbolic link
/// (`made_type`) just made for it at `name`: through the node itself, never
/// by name (`set_made_node_attrs`).
fn set_made_attrs(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    made_type: libc::mode_t,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<()> {
    let attrs = attrs_of(entry, options);
    set_made_node_attrs(dirfd, name, made_type, &attrs, &policy_of(options))
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use std::os::unix::ffi::OsStringExt;

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
    /// A link set's earlier names are moved over to the file just made for
    /// its data by linking that file's name again. Someone who can rename in
    /// that directory can put another file at the name first, and the set's
    /// names became links to it. The link is made to the file made, or not
    /// at all.
    #[cfg(target_os = "linux")]
    #[test]
    fn test_link_set_names_move_only_to_the_file_made() {
        use crate::modes::race_hook::{with_hook, Point};
        let dir = plib::tmp::TempDir::new().unwrap();
        std::fs::write(dir.path().join("new"), "data\n").unwrap();
        std::fs::write(dir.path().join("old"), "").unwrap();
        let tree = DirTree::open_path(dir.path()).unwrap();
        let made = id_at(tree.root(), c"new").unwrap();
        let root = Rc::new(tree.root().try_clone_to_owned().unwrap());
        let holders = vec![((Rc::clone(&root), c"old".to_owned()), PathBuf::from("old"))];

        let path = dir.path().to_path_buf();
        let swap = move |point, _: libc::c_int, name: &CStr| {
            if point == Point::Linking && name == c"new" {
                std::fs::write(path.join("planted"), "planted\n").unwrap();
                std::fs::rename(path.join("planted"), path.join("new")).unwrap();
            }
        };
        let moved = with_hook(swap, || move_names_to(holders, root.as_fd(), c"new", made));
        assert!(moved.is_err(), "linked to the file put in its place");
        assert_eq!(std::fs::read(dir.path().join("old")).unwrap(), b"");
    }

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
            uname: Some(user.name.clone().into_vec()),
            gname: Some(group.name.clone().into_vec()),
            ..Default::default()
        };
        let owner = ReadOptions {
            preserve_owner: true,
            ..Default::default()
        };
        let attrs = attrs_of(&named, &owner);
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
        let attrs = attrs_of(&unknown, &owner);
        assert_eq!((attrs.uid, attrs.gid), (4242, 4243));

        // And a format that records no name at all -- cpio has no such field
        // -- is unaffected.
        let bare = ArchiveEntry {
            uid: 7,
            gid: 8,
            ..Default::default()
        };
        let attrs = attrs_of(&bare, &owner);
        assert_eq!((attrs.uid, attrs.gid), (7, 8));

        // Without -p o the owner is never applied, and never looked up.
        let attrs = attrs_of(&named, &ReadOptions::default());
        assert_eq!((attrs.uid, attrs.gid), (0xffff_fff0, 0xffff_fff0));
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

    /// Without explicit `-p p`/`-p e` the mode a made node ends up with is
    /// the archived mode masked by the umask (normal file-creation action);
    /// with preservation the exact archived mode is restored.
    #[test]
    fn test_set_permissions_umask_vs_preserve() {
        use std::os::unix::fs::PermissionsExt;
        let tmp = plib::tmp::TempDir::new().unwrap();
        let path = tmp.path().join("member");
        let path_c = CString::new(path.as_os_str().as_bytes()).unwrap();
        assert_eq!(unsafe { libc::mkfifo(path_c.as_ptr(), 0o600) }, 0);

        // Attributes are applied relative to an open parent directory.
        let dir = std::fs::File::open(tmp.path()).unwrap();
        let name = CString::new("member").unwrap();
        let entry = ArchiveEntry {
            path: path.clone(),
            mode: 0o777,
            entry_type: EntryType::Fifo,
            ..Default::default()
        };
        let mode = || {
            std::fs::symlink_metadata(&path)
                .unwrap()
                .permissions()
                .mode()
                & 0o7777
        };

        // Not preserved: 0o777 & ~0o022 == 0o755.
        let opts = ReadOptions {
            preserve_perms: false,
            preserve_mtime: false,
            preserve_atime: false,
            umask: 0o022,
            ..Default::default()
        };
        set_made_attrs(dir.as_fd(), &name, libc::S_IFIFO, &entry, &opts).unwrap();
        assert_eq!(mode(), 0o755);

        // Preserved: exact 0o777 regardless of umask.
        let opts = ReadOptions {
            preserve_perms: true,
            preserve_mtime: false,
            preserve_atime: false,
            umask: 0o022,
            ..Default::default()
        };
        set_made_attrs(dir.as_fd(), &name, libc::S_IFIFO, &entry, &opts).unwrap();
        assert_eq!(mode(), 0o777);
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
}

#[cfg(test)]
mod race_tests;
