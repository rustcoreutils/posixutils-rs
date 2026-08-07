//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Read mode implementation - extract archive contents

use crate::archive::{ArchiveEntry, ArchiveFormat, ArchiveReader, EntryType, ExtractedLinks};
use crate::error::{PaxError, PaxResult};
use crate::formats::{CpioReader, PaxReader, UstarReader};
use crate::interactive::{InteractivePrompter, RenameResult};
use crate::pattern::{find_matching_pattern_subtree, Pattern};
use crate::subst::{apply_substitutions, SubstResult, Substitution};
use std::collections::HashSet;
use std::ffi::{CStr, CString, OsString};
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
    /// `-o` extended-header options (delete=/keyword:=value) applied on extract.
    pub format_options: crate::options::FormatOptions,
    /// `-d`: a directory pattern matches only the directory itself, not its
    /// subtree.
    pub dir_only: bool,
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
            let mut archive = PaxReader::new(reader).with_options(options.format_options.clone());
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
    let mut extracted_links = ExtractedLinks::new();
    // Extraction is anchored at an open descriptor for the working directory,
    // and every member path is resolved relative to it without following a
    // symlink.
    let tree = DirTree::open_cwd()?;
    // Directories take their archived attributes only once the whole archive
    // has been extracted; see apply_pending_dirs.
    let mut pending_dirs: Vec<(MemberPath, ArchiveEntry)> = Vec::new();

    // Track which patterns have been matched (for -n first_match option)
    let mut matched_patterns: HashSet<usize> = HashSet::new();

    // Create interactive prompter if needed
    let mut prompter = if options.interactive {
        Some(InteractivePrompter::new()?)
    } else {
        None
    };

    while let Some(mut entry) = archive.read_entry()? {
        if let Some(should_output) = should_extract(&entry, options, &mut matched_patterns) {
            if !should_output {
                // Entry matched a pattern that's already been matched (first_match mode)
                archive.skip_data()?;
                continue;
            }
            // Apply `-o keyword:=value` overrides before substitutions/rename so a
            // forced path/uid/gid/etc. takes effect on the extracted file.
            apply_keyword_overrides(&mut entry, &options.format_options);
            // Apply substitutions first (per POSIX: -s applies before -i)
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

            // Handle interactive rename if enabled
            if let Some(ref mut p) = prompter {
                let path_str = entry.path.to_string_lossy();
                match p.prompt(&path_str)? {
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
                &mut extracted_links,
                &tree,
                &mut pending_dirs,
            ) {
                crate::error::report_error(entry.path.display(), e);
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

/// Apply `-o keyword:=value` per-file overrides to an entry on extract.
///
/// The `:=` form forces the value regardless of what the archive carried, so it
/// is applied after the reader has merged any extended-header records. Only the
/// standard keywords that map onto an entry field are handled; unknown keywords
/// have no extraction effect. `delete=` is handled in the pax reader (so the
/// ustar value remains), not here.
pub(crate) fn apply_keyword_overrides(
    entry: &mut ArchiveEntry,
    opts: &crate::options::FormatOptions,
) {
    for (keyword, value) in opts.per_file_options() {
        match keyword.as_str() {
            "uid" => {
                if let Ok(v) = value.parse() {
                    entry.uid = v;
                }
            }
            "gid" => {
                if let Ok(v) = value.parse() {
                    entry.gid = v;
                }
            }
            "uname" => entry.uname = Some(value.clone()),
            "gname" => entry.gname = Some(value.clone()),
            "path" => entry.path = PathBuf::from(value),
            "linkpath" => entry.link_target = Some(PathBuf::from(value)),
            "size" => {
                if let Ok(v) = value.parse() {
                    entry.size = v;
                }
            }
            "mtime" => {
                if let Ok(t) = value.parse::<f64>() {
                    entry.mtime = t as u64;
                    entry.mtime_nsec = (t.fract() * 1_000_000_000.0) as u32;
                }
            }
            "atime" => {
                if let Ok(t) = value.parse::<f64>() {
                    entry.atime = Some(t as u64);
                    entry.atime_nsec = (t.fract() * 1_000_000_000.0) as u32;
                }
            }
            "ctime" => {
                if let Ok(t) = value.parse::<f64>() {
                    entry.ctime = Some(t as u64);
                    entry.ctime_nsec = (t.fract() * 1_000_000_000.0) as u32;
                }
            }
            _ => {}
        }
    }
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
    extracted_links: &mut ExtractedLinks,
    tree: &DirTree,
    pending_dirs: &mut Vec<(MemberPath, ArchiveEntry)>,
) -> PaxResult<()> {
    let Some(member) = MemberPath::parse(&entry.path)? else {
        // The member names nothing to create (`.`, or only `..`/root parts).
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
        eprintln!("{}", member.display.display());
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
            extract_hardlink(tree, pfd, name, entry, options, extracted_links)?;
            archive.skip_data()?;
        }
        EntryType::Regular => {
            extract_file(archive, pfd, name, entry, options)?;
            archive.skip_data()?; // Skip padding to block boundary
            extracted_links.record(entry, &member.display);
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
                eprintln!("pax: skipping socket: {}", member.display.display());
            }
            archive.skip_data()?;
        }
    }

    Ok(())
}

/// A member pathname reduced to the directory components that must be walked
/// and the final component to create.
struct MemberPath {
    dirs: Vec<CString>,
    leaf: CString,
    /// The same path as text, for diagnostics and hard-link bookkeeping.
    display: PathBuf,
}

impl MemberPath {
    /// `Ok(None)` for a member that names nothing to create -- `.`, or a name
    /// made up entirely of `.`, `..` and root components.
    ///
    /// `..` still pops and a leading `/` is still dropped, but this lexical
    /// pass is no longer the security boundary it used to be: it cannot see
    /// that `a/b` escapes when `a` is a symlink. Resolution opens each
    /// component with O_NOFOLLOW instead, which does not care how the name is
    /// spelled. What remains here is naming policy, plus the one check that
    /// must happen before any syscall: an embedded NUL.
    fn parse(path: &Path) -> PaxResult<Option<Self>> {
        use std::path::Component;

        let mut parts: Vec<OsString> = Vec::new();
        for comp in path.components() {
            match comp {
                Component::Normal(c) => parts.push(c.to_os_string()),
                Component::ParentDir => {
                    parts.pop();
                }
                Component::CurDir | Component::RootDir | Component::Prefix(_) => {}
            }
        }

        let Some(leaf_os) = parts.pop() else {
            return Ok(None);
        };

        let to_c = |s: &OsString| {
            CString::new(s.as_bytes())
                .map_err(|_| PaxError::InvalidHeader("path contains null".to_string()))
        };

        let dirs = parts.iter().map(to_c).collect::<PaxResult<Vec<_>>>()?;
        let mut display = PathBuf::new();
        for p in &parts {
            display.push(p);
        }
        display.push(&leaf_os);

        Ok(Some(MemberPath {
            dirs,
            leaf: to_c(&leaf_os)?,
            display,
        }))
    }

    /// How deep the member sits, for ordering the deferred directory pass.
    fn depth(&self) -> usize {
        self.dirs.len()
    }
}

/// Extraction anchored at an open descriptor for the working directory.
///
/// Member paths are walked one component at a time with
/// `O_RDONLY|O_DIRECTORY|O_NOFOLLOW`, so a symlink planted anywhere along the
/// path fails the descent rather than redirecting the write outside the
/// extraction directory. The previous code resolved whole paths through the
/// ordinary filesystem namespace, where `create_dir_all` on `sub/file` was
/// happy to follow `sub -> /elsewhere`.
struct DirTree {
    root: OwnedFd,
}

impl DirTree {
    fn open_cwd() -> PaxResult<Self> {
        let dot = CString::new(".").expect("no NUL in \".\"");
        let fd = unsafe {
            libc::openat(
                libc::AT_FDCWD,
                dot.as_ptr(),
                libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC,
            )
        };
        if fd < 0 {
            return Err(std::io::Error::last_os_error().into());
        }
        Ok(DirTree {
            root: unsafe { OwnedFd::from_raw_fd(fd) },
        })
    }

    /// Open the directory that will hold `member`, creating any missing
    /// intermediate components.
    fn parent_of(&self, member: &MemberPath, create_missing: bool) -> PaxResult<OwnedFd> {
        let mut cur = self.root.try_clone()?;
        for comp in &member.dirs {
            cur = open_dir_at(cur.as_fd(), comp, create_missing)?;
        }
        Ok(cur)
    }
}

/// Open one directory component below `dirfd` without following a symlink.
fn open_dir_at(dirfd: BorrowedFd<'_>, name: &CString, create_missing: bool) -> PaxResult<OwnedFd> {
    let flags = libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC;

    let fd = unsafe { libc::openat(dirfd.as_raw_fd(), name.as_ptr(), flags) };
    if fd >= 0 {
        return Ok(unsafe { OwnedFd::from_raw_fd(fd) });
    }

    let err = std::io::Error::last_os_error();
    if err.raw_os_error() != Some(libc::ENOENT) || !create_missing {
        return Err(err.into());
    }

    // Intermediate directories are created with the normal file-creation
    // action, per POSIX read/copy mode: mode 0777 modified by the umask.
    let r = unsafe { libc::mkdirat(dirfd.as_raw_fd(), name.as_ptr(), 0o777) };
    if r != 0 {
        let e = std::io::Error::last_os_error();
        if e.raw_os_error() != Some(libc::EEXIST) {
            return Err(e.into());
        }
    }

    let fd = unsafe { libc::openat(dirfd.as_raw_fd(), name.as_ptr(), flags) };
    if fd < 0 {
        return Err(std::io::Error::last_os_error().into());
    }
    Ok(unsafe { OwnedFd::from_raw_fd(fd) })
}

/// Remove whatever currently occupies `name`, so an exclusive create can win.
fn unlink_at(dirfd: BorrowedFd<'_>, name: &CStr) -> PaxResult<()> {
    let r = unsafe { libc::unlinkat(dirfd.as_raw_fd(), name.as_ptr(), 0) };
    if r != 0 {
        let err = std::io::Error::last_os_error();
        match err.raw_os_error() {
            Some(libc::ENOENT) => return Ok(()),
            // A directory in the way needs the directory flag instead.
            Some(libc::EISDIR) | Some(libc::EPERM) => {
                let r =
                    unsafe { libc::unlinkat(dirfd.as_raw_fd(), name.as_ptr(), libc::AT_REMOVEDIR) };
                if r == 0 {
                    return Ok(());
                }
                return Err(std::io::Error::last_os_error().into());
            }
            _ => return Err(err.into()),
        }
    }
    Ok(())
}

/// Create a member, retrying once after clearing whatever is in the way.
///
/// `create` reports `EEXIST` by returning `Err`; that is the whole point. With
/// -k an existing name means skip, atomically and with no window. Otherwise the
/// old entry is unlinked and the create retried, so the member is always a
/// freshly created object -- never a write *through* a symlink or a hard link
/// an attacker left behind, which `O_TRUNC` on an existing name would allow.
fn create_replacing<F>(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    no_clobber: bool,
    mut create: F,
) -> PaxResult<bool>
where
    F: FnMut() -> std::io::Result<()>,
{
    match create() {
        Ok(()) => return Ok(true),
        Err(e) if e.raw_os_error() == Some(libc::EEXIST) => {}
        Err(e) => return Err(e.into()),
    }

    if no_clobber {
        return Ok(false);
    }

    unlink_at(dirfd, name)?;
    match create() {
        Ok(()) => Ok(true),
        Err(e) => Err(e.into()),
    }
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
    extracted_links: &ExtractedLinks,
) -> PaxResult<()> {
    let target = if let Some(ref link_target) = entry.link_target {
        link_target.clone()
    } else if let Some(existing) = extracted_links.get_link_target(entry) {
        existing.clone()
    } else {
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

    create_replacing(dirfd, name, options.no_clobber, || {
        // flags = 0, never AT_SYMLINK_FOLLOW. fs::hard_link resolves the whole
        // target path, so a symlink planted at the target -- possibly by an
        // earlier member of this very archive -- could link a file from outside
        // the extraction directory into it.
        let r = unsafe {
            libc::linkat(
                target_parent.as_raw_fd(),
                target_member.leaf.as_ptr(),
                dirfd.as_raw_fd(),
                name.as_ptr(),
                0,
            )
        };
        if r != 0 {
            return Err(std::io::Error::last_os_error());
        }
        Ok(())
    })?;

    Ok(())
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
    let mode: libc::mode_t = (entry.mode as libc::mode_t) | type_bits;

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

    set_owner_at(dirfd, name, entry, options)?;
    set_permissions_at(dirfd, name, entry, options)?;
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
        let r =
            unsafe { libc::mkfifoat(dirfd.as_raw_fd(), name.as_ptr(), entry.mode as libc::mode_t) };
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

    set_owner_at(dirfd, name, entry, options)?;
    set_permissions_at(dirfd, name, entry, options)?;
    set_times_at(dirfd, name, entry, options)
}

/// Extract a regular file
fn extract_file<R: ArchiveReader>(
    archive: &mut R,
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<()> {
    let flags = libc::O_WRONLY | libc::O_CREAT | libc::O_EXCL | libc::O_NOFOLLOW | libc::O_CLOEXEC;
    let mut opened: Option<File> = None;

    let created = create_replacing(dirfd, name, options.no_clobber, || {
        let fd = unsafe {
            libc::openat(
                dirfd.as_raw_fd(),
                name.as_ptr(),
                flags,
                entry.mode as libc::c_uint,
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
        return Ok(());
    };

    copy_file_data(archive, &mut file, entry.size)?;
    drop(file);

    set_owner_at(dirfd, name, entry, options)?;
    set_permissions_at(dirfd, name, entry, options)?;
    set_times_at(dirfd, name, entry, options)
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
    entry.mtime as i64 > st.st_mtime as i64
}

/// Set file permissions
fn set_permissions_at(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<()> {
    let mut mode = entry.mode;

    // Per POSIX: If owner is not preserved, clear SUID and SGID bits
    if !options.preserve_owner {
        // Cast to u32 for cross-platform compatibility (u16 on macOS, u32 on Linux)
        #[allow(clippy::unnecessary_cast)]
        let setid_mask = !((libc::S_ISUID | libc::S_ISGID) as u32);
        mode &= setid_mask;
    }

    // When the mode is not explicitly preserved (no `-p p`/`-p e`), the file
    // is created as part of the "normal file creation action": the archived
    // mode is modified by the process file-creation mask (umask), exactly as
    // open()/mkdir() would do. With `-p p`/`-p e` the exact mode is restored.
    if !options.preserve_perms {
        mode &= !options.umask;
    }

    let r = unsafe { libc::fchmodat(dirfd.as_raw_fd(), name.as_ptr(), mode as libc::mode_t, 0) };
    if r != 0 {
        return Err(std::io::Error::last_os_error().into());
    }
    Ok(())
}

/// Set file owner (uid/gid) - requires privileges
fn set_owner_at(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<()> {
    if !options.preserve_owner {
        return Ok(());
    }

    let result = unsafe {
        libc::fchownat(
            dirfd.as_raw_fd(),
            name.as_ptr(),
            entry.uid,
            entry.gid,
            libc::AT_SYMLINK_NOFOLLOW,
        )
    };

    if result != 0 {
        let err = std::io::Error::last_os_error();
        // EPERM usually means we're not root - warn but continue
        if err.raw_os_error() == Some(libc::EPERM) {
            eprintln!("pax: cannot change owner: Operation not permitted");
            crate::error::note_error();
            return Ok(());
        }
        return Err(err.into());
    }

    Ok(())
}

/// Set file access and modification times
fn set_times_at(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<()> {
    // If neither atime nor mtime preservation is requested, skip
    if !options.preserve_mtime && !options.preserve_atime {
        return Ok(());
    }

    // Determine the atime to set, preserving nanosecond precision. UTIME_OMIT
    // leaves the one we are not preserving exactly as it is, which is both
    // simpler and more accurate than reading it back first.
    let atime = if options.preserve_atime {
        match entry.atime {
            Some(sec) => libc::timespec {
                tv_sec: sec as libc::time_t,
                tv_nsec: entry.atime_nsec as _,
            },
            None => libc::timespec {
                tv_sec: entry.mtime as libc::time_t,
                tv_nsec: entry.mtime_nsec as _,
            },
        }
    } else {
        libc::timespec {
            tv_sec: 0,
            tv_nsec: libc::UTIME_OMIT,
        }
    };

    let mtime = if options.preserve_mtime {
        libc::timespec {
            tv_sec: entry.mtime as libc::time_t,
            tv_nsec: entry.mtime_nsec as _,
        }
    } else {
        libc::timespec {
            tv_sec: 0,
            tv_nsec: libc::UTIME_OMIT,
        }
    };

    let times = [atime, mtime];
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
                crate::error::report_error(member.display.display(), e);
                continue;
            }
        };
        let pfd = parent.as_fd();
        let name = member.leaf.as_c_str();

        let outcome = set_owner_at(pfd, name, entry, options)
            .and_then(|_| set_permissions_at(pfd, name, entry, options))
            .and_then(|_| set_times_at(pfd, name, entry, options));
        if let Err(e) = outcome {
            crate::error::report_error(member.display.display(), e);
        }
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use std::os::unix::fs::PermissionsExt;
    use tempfile::TempDir;

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
        set_permissions_at(dir.as_fd(), &name, &entry, &opts).unwrap();
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
        set_permissions_at(dir.as_fd(), &name, &entry, &opts).unwrap();
        assert_eq!(
            std::fs::metadata(&path).unwrap().permissions().mode() & 0o777,
            0o777
        );
    }
}
