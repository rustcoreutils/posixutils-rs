//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Write mode implementation - create archives

use crate::archive::{ArchiveEntry, ArchiveFormat, ArchiveWriter, EntryType, HardLinkTracker};
use crate::error::{PaxError, PaxResult};
use crate::formats::{checksum_bytes, CpioFormat, CpioWriter, PaxWriter, UstarWriter};
use crate::interactive::{InteractivePrompter, RenameResult};
use crate::options::FormatOptions;
use crate::pattern::{matches_excluded, Pattern};
use crate::subst::{apply_substitutions, SubstResult, Substitution};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Seek, Write};
#[cfg(unix)]
use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};

/// Options for write/create mode
#[derive(Default)]
pub struct WriteOptions {
    /// Follow symlinks on command line
    pub cli_dereference: bool,
    /// Follow all symlinks
    pub dereference: bool,
    /// Don't descend into directories
    pub no_recurse: bool,
    /// Verbose output
    pub verbose: bool,
    /// Stay on one filesystem
    pub one_file_system: bool,
    /// Interactive rename mode
    pub interactive: bool,
    /// Reset access time after reading files
    pub reset_atime: bool,
    /// Path substitutions (-s option)
    pub substitutions: Vec<Substitution>,
    /// Format-specific options (-o option)
    pub format_options: FormatOptions,
    /// cpio header flavor to emit; only consulted for `ArchiveFormat::Cpio`
    pub cpio_format: CpioFormat,
    /// Names not to archive (tar `--exclude` / `-X`)
    pub exclude_patterns: Vec<Pattern>,
    /// `-u` when appending: the modification time already recorded for each
    /// member name in the archive being extended.
    ///
    /// The key is the *member* name -- what the file is stored as, after `-s`
    /// and any rename -- because that is what a later extraction resolves, and
    /// it is not the pathname the file was named by on the command line.
    pub update_times: Option<HashMap<PathBuf, u64>>,
}

impl WriteOptions {
    /// Whether `-u` should leave this member out because the archive already
    /// holds a copy of that name no older than the file.
    ///
    /// False whenever `-u` is not in force or the name is new to the archive,
    /// so an unfiltered run writes everything.
    fn is_up_to_date(&self, archive_path: &Path, metadata: &ftw::Metadata) -> bool {
        let Some(times) = &self.update_times else {
            return false;
        };
        // Directory members are stored with a trailing slash; archived_mtimes
        // strips it so both sides of this lookup spell the name the same way.
        let name = crate::rawpath::MatchName::of(archive_path);
        let name = name.as_str();
        let name = Path::new(name.trim_end_matches('/'));
        let Some(&member_mtime) = times.get(name) else {
            return false;
        };
        file_mtime_secs(metadata) <= member_mtime
    }
}

/// A file's modification time in whole seconds, the resolution every header
/// format records.
fn file_mtime_secs(metadata: &ftw::Metadata) -> u64 {
    #[cfg(unix)]
    {
        metadata.mtime().max(0) as u64
    }
    #[cfg(not(unix))]
    {
        metadata
            .modified()
            .ok()
            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
            .map(|d| d.as_secs())
            .unwrap_or(0)
    }
}

/// Create an archive from files
pub fn create_archive<W: Write>(
    writer: W,
    files: &[PathBuf],
    format: ArchiveFormat,
    options: &WriteOptions,
) -> PaxResult<()> {
    match format {
        ArchiveFormat::Ustar => {
            let mut archive = UstarWriter::new(writer);
            write_files(&mut archive, files, options)?;
            archive.finish()
        }
        ArchiveFormat::Cpio => {
            let mut archive = CpioWriter::with_format(writer, options.cpio_format);
            write_files(&mut archive, files, options)?;
            archive.finish()
        }
        ArchiveFormat::Pax => {
            let mut archive = PaxWriter::with_options(writer, options.format_options.clone());
            write_files(&mut archive, files, options)?;
            archive.finish()
        }
    }
}

/// Write files to any archive writer.
///
/// The walk is `ftw::traverse_directory`, the same race-free traversal `cp` and
/// `mv` use: everything below an operand is resolved one component at a time
/// from a directory descriptor, with `O_DIRECTORY|O_NOFOLLOW` on the descent
/// and a `(dev, ino)` re-check after it. What this replaced re-resolved the
/// whole pathname on every call, so a source tree another process could modify
/// was a window in which pax could be pointed at a file outside it.
///
/// One thing the operand itself is not: it is resolved once, by name, because
/// it has to be. Everything under it is not.
fn write_files<W: ArchiveWriter>(
    archive: &mut W,
    files: &[PathBuf],
    options: &WriteOptions,
) -> PaxResult<()> {
    let prompter = if options.interactive {
        Some(InteractivePrompter::new()?)
    } else {
        None
    };

    let walk = WriteWalk {
        archive: RefCell::new(archive),
        link_tracker: RefCell::new(HardLinkTracker::new()),
        prompter: RefCell::new(prompter),
        dev_stack: RefCell::new(Vec::new()),
        fatal: RefCell::new(None),
        options,
    };

    for path in files {
        let _ = ftw::traverse_directory(
            path,
            |entry| walk.visit(entry),
            |_, _| {
                walk.dev_stack.borrow_mut().pop();
                Ok(())
            },
            |entry, err| crate::error::report_error(entry.path().as_inner(), err.inner()),
            ftw::TraverseDirectoryOpts {
                follow_symlinks_on_args: options.cli_dereference,
                follow_symlinks: options.dereference,
                // Nothing here holds a descriptor open per level; the archive
                // is written as the walk goes.
                caller_fds_per_level: 0,
                ..Default::default()
            },
        );

        // A handler cannot abort the walk, so a fatal error is parked and
        // re-raised here. traverse_directory's own bool return conflates "an
        // error occurred" with "the operand was not a directory", so the exit
        // status comes from note_error() as everywhere else.
        if let Some(e) = walk.fatal.borrow_mut().take() {
            return Err(e);
        }
    }

    Ok(())
}

/// State the three traversal callbacks share.
///
/// `RefCell` throughout because `file_handler`, `postprocess_dir` and
/// `err_reporter` are three closures alive at once and each needs a different
/// part of it. Every borrow is taken and released within one callback; none is
/// held across a call that could re-enter.
struct WriteWalk<'a, W: ArchiveWriter> {
    archive: RefCell<&'a mut W>,
    link_tracker: RefCell<HardLinkTracker>,
    prompter: RefCell<Option<InteractivePrompter>>,
    /// `st_dev` of each directory descended into, for `-X`. Per parent rather
    /// than per operand: `-X` stops pax crossing *a* mount point, not just the
    /// one the operand sits on.
    dev_stack: RefCell<Vec<u64>>,
    /// Set by a failure that must stop the walk rather than skip a file.
    fatal: RefCell<Option<PaxError>>,
    options: &'a WriteOptions,
}

impl<W: ArchiveWriter> WriteWalk<'_, W> {
    /// Archive one entry. `Ok(true)` descends into a directory.
    fn visit(&self, entry: ftw::Entry<'_>) -> Result<bool, ()> {
        if self.fatal.borrow().is_some() {
            return Ok(false);
        }

        let path = entry.path();
        let path = path.as_inner();

        // Exclusion is decided on the name as traversed, before -s renaming
        // and before any stat. Declining to descend takes the whole subtree.
        if matches_excluded(
            &self.options.exclude_patterns,
            crate::rawpath::MatchName::of(path).as_str(),
        ) {
            return Ok(false);
        }

        let Some(metadata) = entry.metadata() else {
            // ftw reports its own failures through err_reporter; reaching here
            // without metadata would mean it handed us an entry it could not
            // stat, which it does not do inside the handler.
            return Ok(false);
        };

        // -L, and -H on an operand, ask for the *target*, not the link. ftw
        // falls back to the link's own metadata when the target cannot be
        // stat'ed, so a dangling link would otherwise be archived as a link --
        // silently, and as the opposite of what was asked for. The previous
        // traversal called fs::metadata here and reported its ENOENT.
        //
        // The walk is at an operand exactly when no directory has been
        // descended into yet, which is what dev_stack being empty means.
        let at_operand = self.dev_stack.borrow().is_empty();
        let asked_to_follow =
            self.options.dereference || (self.options.cli_dereference && at_operand);
        if asked_to_follow && entry.is_symlink() == Some(true) && metadata.is_symlink() {
            crate::error::report_error(path, std::io::Error::from_raw_os_error(libc::ENOENT));
            return Ok(false);
        }

        // -X: a directory on a different filesystem from its parent is not
        // descended and not archived.
        if self.options.one_file_system {
            if let Some(&parent_dev) = self.dev_stack.borrow().last() {
                if metadata.dev() != parent_dev {
                    return Ok(false);
                }
            }
        }

        match self.archive_entry(&entry, path, metadata) {
            Ok(descend) => Ok(descend),
            Err(e) if crate::modes::is_fatal(&e) => {
                *self.fatal.borrow_mut() = Some(e);
                Ok(false)
            }
            Err(e) => {
                // POSIX CONSEQUENCES OF ERRORS: diagnose, set a non-zero exit,
                // carry on with the next file.
                crate::error::report_error(path, e);
                Ok(false)
            }
        }
    }

    fn archive_entry(
        &self,
        entry: &ftw::Entry<'_>,
        path: &Path,
        metadata: &ftw::Metadata,
    ) -> PaxResult<bool> {
        // Apply substitutions first (per POSIX: -s applies before -i)
        let archive_path = if !self.options.substitutions.is_empty() {
            match apply_substitutions(&self.options.substitutions, path) {
                SubstResult::Unchanged => path.to_path_buf(),
                SubstResult::Changed(new_path) => crate::rawpath::from_substituted(&new_path),
                SubstResult::Empty => return Ok(false), // Skip this file
            }
        } else {
            path.to_path_buf()
        };

        // Handle interactive rename
        let archive_path = {
            let mut prompter = self.prompter.borrow_mut();
            if let Some(ref mut p) = *prompter {
                match p.prompt(&archive_path)? {
                    RenameResult::Skip => return Ok(false),
                    RenameResult::UseOriginal => archive_path,
                    RenameResult::Rename(new_path) => new_path,
                }
            } else {
                archive_path
            }
        };

        // -u is decided here rather than on the operands, because this is the
        // first point at which the member name exists: -s and an interactive
        // rename have been applied, so the name being looked up is the one an
        // extraction would resolve. A directory is never skipped outright --
        // the whole point of -u is to pick up a file that changed underneath
        // one that did not -- so only its own entry is suppressed.
        let up_to_date = self.options.is_up_to_date(&archive_path, metadata);
        if !metadata.is_dir() && up_to_date {
            return Ok(false);
        }

        if self.options.verbose {
            let mut line = Vec::new();
            crate::escape::push_escaped(
                &mut line,
                crate::rawpath::as_bytes(path),
                crate::escape::stderr_style(),
            );
            line.push(b'\n');
            let _ = std::io::Write::write_all(&mut std::io::stderr().lock(), &line);
        }

        let archive = &mut **self.archive.borrow_mut();

        if metadata.is_dir() {
            if !up_to_date {
                let dir_entry = build_entry(&archive_path, metadata, EntryType::Directory)?;
                archive.write_entry(&dir_entry)?;
                archive.finish_entry()?;
            }
            if self.options.no_recurse {
                return Ok(false);
            }
            self.dev_stack.borrow_mut().push(metadata.dev());
            return Ok(true);
        }

        if metadata.is_symlink() {
            // ftw has already done the readlinkat, from the descriptor of the
            // directory the link was found in.
            let target = entry
                .read_link()
                .map(|t| crate::rawpath::from_bytes(t.to_bytes()))
                .ok_or_else(|| {
                    PaxError::InvalidHeader("symbolic link with no target".to_string())
                })?;
            write_symlink(archive, &archive_path, metadata, target)?;
        } else if metadata.is_file() {
            write_file(
                archive,
                entry,
                &archive_path,
                metadata,
                &mut self.link_tracker.borrow_mut(),
                self.options,
            )?;
        } else {
            // Block and character devices, FIFOs and sockets are archived from
            // their metadata; none of them is ever opened, so a FIFO with no
            // writer cannot block the walk.
            write_special(archive, &archive_path, metadata)?;
        }

        Ok(false)
    }
}

/// Write a symlink
fn write_symlink<W: ArchiveWriter>(
    archive: &mut W,
    archive_path: &Path,
    metadata: &ftw::Metadata,
    target: PathBuf,
) -> PaxResult<()> {
    // The target is a pathname, so it goes out as its bytes. Taking the size
    // and the data from `to_string_lossy()` while `link_target` kept the real
    // bytes made the two disagree: for cpio the target *is* the member data,
    // so a target that is not UTF-8 was written corrupted and at the wrong
    // length, which desynchronises everything after it in the archive.
    let target_bytes = crate::rawpath::as_bytes(&target).to_vec();
    let mut entry = build_entry(archive_path, metadata, EntryType::Symlink)?;
    entry.link_target = Some(target.clone());
    // For cpio format, the symlink target is written as file data; the size
    // field is what makes the reader read it back.
    entry.size = target_bytes.len() as u64;
    if archive.needs_data_checksum() {
        entry.data_checksum = Some(checksum_bytes(0, &target_bytes));
    }

    archive.write_entry(&entry)?;
    // Write the symlink target as data (needed for cpio format)
    archive.write_data(&target_bytes)?;
    archive.finish_entry()?;

    Ok(())
}

/// Write a special file (block device, char device, fifo, socket)
#[cfg(unix)]
fn write_special<W: ArchiveWriter>(
    archive: &mut W,
    path: &Path,
    metadata: &ftw::Metadata,
) -> PaxResult<()> {
    use std::os::unix::fs::FileTypeExt;

    let file_type = metadata.file_type();
    let entry_type = if file_type.is_block_device() {
        EntryType::BlockDevice
    } else if file_type.is_char_device() {
        EntryType::CharDevice
    } else if file_type.is_fifo() {
        EntryType::Fifo
    } else if file_type.is_socket() {
        EntryType::Socket
    } else {
        crate::error::report_error(path, gettextrs::gettext("unsupported file type"));
        return Ok(());
    };

    let entry = build_entry(path, metadata, entry_type)?;
    archive.write_entry(&entry)?;
    archive.finish_entry()?;

    Ok(())
}

#[cfg(not(unix))]
fn write_special<W: ArchiveWriter>(
    _archive: &mut W,
    path: &Path,
    _metadata: &ftw::Metadata,
) -> PaxResult<()> {
    eprintln!(
        "pax: {}: special files not supported on this platform",
        path.display()
    );
    Ok(())
}

/// Write a regular file
fn write_file<W: ArchiveWriter>(
    archive: &mut W,
    entry_ref: &ftw::Entry<'_>,
    archive_path: &Path,
    metadata: &ftw::Metadata,
    link_tracker: &mut HardLinkTracker,
    options: &WriteOptions,
) -> PaxResult<()> {
    let src_path = entry_ref.path();
    let src_path = src_path.as_inner();
    // Save access time if we need to reset it after reading
    #[cfg(unix)]
    let original_atime = if options.reset_atime {
        Some((metadata.atime(), metadata.atime_nsec()))
    } else {
        None
    };

    let mut entry = build_entry(archive_path, metadata, EntryType::Regular)?;
    // But use src_path for hard link tracking (dev/ino)
    entry.dev = {
        #[cfg(unix)]
        {
            metadata.dev()
        }
        #[cfg(not(unix))]
        {
            0
        }
    };
    entry.ino = {
        #[cfg(unix)]
        {
            metadata.ino()
        }
        #[cfg(not(unix))]
        {
            0
        }
    };
    entry.nlink = {
        #[cfg(unix)]
        {
            metadata.nlink() as u32
        }
        #[cfg(not(unix))]
        {
            1
        }
    };

    // Check for hard link
    if let Some(original_path) = link_tracker.check(&entry) {
        // Only claim the link in formats that can express one. cpio cannot, so
        // recording the type there would degrade the member to a regular file
        // -- and combined with the size=0 below, to an empty one.
        let linkable = archive.supports_hardlinks();
        if linkable {
            entry.entry_type = EntryType::Hardlink;
            entry.link_target = Some(original_path);
        }

        // Per POSIX: -o linkdata means write file contents for each hard link.
        // By default a hard link has size=0 and no data -- but only where the
        // format records the linkage, otherwise the contents are the only copy
        // of the data this member will ever have.
        if linkable && !options.format_options.link_data {
            entry.size = 0;
            archive.write_entry(&entry)?;
            archive.finish_entry()?;
            return Ok(());
        }
        // Otherwise fall through and write the file contents.
    }

    // Opened once, from the descriptor of the directory the walk found it in,
    // and re-checked against the (dev, ino) the walk saw. What this replaced
    // resolved the whole pathname again -- twice over, for the cpio "crc"
    // format, which needs the contents summed before the header goes out.
    //
    // Whether the walk dereferenced this entry is directly observable: the
    // name is a symbolic link but the metadata is not, so -H/-L policy stays
    // in the traversal options and is not decided a second time here.
    let followed = entry_ref.is_symlink() == Some(true) && !metadata.is_symlink();
    let mut file = crate::modes::anchored::open_source_file(
        entry_ref.dir_fd(),
        entry_ref.file_name(),
        followed,
        (metadata.dev(), metadata.ino()),
    )?;

    // The cpio "crc" format records the data checksum in the header, ahead of
    // the data, so that one format costs an extra read of the file -- of the
    // descriptor, now, rather than of the name.
    if archive.needs_data_checksum() {
        entry.data_checksum = Some(file_checksum(&mut file)?);
        file.rewind()?;
    }

    // Write regular file
    archive.write_entry(&entry)?;

    // Copy file contents, bounded by the size already written in the header.
    copy_file_data(&mut file, archive, entry.size, src_path)?;
    // Held open past the copy so -t can stamp the descriptor below.

    archive.finish_entry()?;

    // Reset access time if requested
    #[cfg(unix)]
    if let Some((atime_sec, atime_nsec)) = original_atime {
        reset_atime(&file, src_path, atime_sec, atime_nsec);
    }

    Ok(())
}

/// Sum a file's bytes for the cpio "crc" format's c_check field
fn file_checksum(file: &mut File) -> PaxResult<u32> {
    let mut buf = [0u8; 8192];
    let mut sum = 0u32;
    loop {
        let n = file.read(&mut buf)?;
        if n == 0 {
            return Ok(sum);
        }
        sum = checksum_bytes(sum, &buf[..n]);
    }
}

/// Copy file data to the archive, writing exactly the `size` already recorded in
/// the member's header.
///
/// The header goes out before the data, so the size in it is a promise made from
/// a `stat` that has already happened. A file being written by someone else can
/// yield a different amount by the time it is read, and letting that through put
/// unbounded extra bytes into the archive at a 512-byte boundary -- where a
/// reader takes them for a header, so anyone able to modify a file while it is
/// archived could inject fabricated members. Reading short instead left the
/// member unterminated.
///
/// So the read is truncated if the file grew and zero-padded if it shrank, which
/// is what GNU tar does ("File shrank by N bytes; padding with zeros"), and the
/// exit status records that the archive does not match what was on disk. The
/// bound also has to come from `size` rather than from end-of-file: waiting for a
/// shrinking file to deliver bytes it no longer has is how CVE-2018-20482 turned
/// into an infinite loop.
fn copy_file_data<W: ArchiveWriter>(
    file: &mut File,
    archive: &mut W,
    size: u64,
    path: &Path,
) -> PaxResult<()> {
    let mut buf = [0u8; 8192];
    let mut remaining = size;

    while remaining > 0 {
        let want = remaining.min(buf.len() as u64) as usize;
        let n = file.read(&mut buf[..want])?;
        if n == 0 {
            break;
        }
        archive.write_data(&buf[..n])?;
        remaining -= n as u64;
    }

    if remaining > 0 {
        eprintln!(
            "pax: {}: File shrank by {} bytes; padding with zeros",
            path.display(),
            remaining
        );
        crate::error::note_error();

        let zeros = [0u8; 8192];
        while remaining > 0 {
            let n = remaining.min(zeros.len() as u64) as usize;
            archive.write_data(&zeros[..n])?;
            remaining -= n as u64;
        }
    } else if file.read(&mut buf[..1])? != 0 {
        // Still more to read than the header promised.
        crate::error::report_error(path, "file changed as we read it");
        crate::error::note_error();
    }

    Ok(())
}

/// Build an ArchiveEntry from path and metadata
fn build_entry(
    path: &Path,
    metadata: &ftw::Metadata,
    entry_type: EntryType,
) -> PaxResult<ArchiveEntry> {
    let mut entry = ArchiveEntry::new(path.to_path_buf(), entry_type);

    #[cfg(unix)]
    {
        entry.mode = metadata.mode() & 0o7777;
        entry.uid = metadata.uid();
        entry.gid = metadata.gid();
        entry.mtime = metadata.mtime() as u64;
        // Capture sub-second times so the pax interchange format can record a
        // fractional `mtime`/`atime` (other formats ignore the nsec fields).
        entry.mtime_nsec = metadata.mtime_nsec() as u32;
        entry.atime = Some(metadata.atime() as u64);
        entry.atime_nsec = metadata.atime_nsec() as u32;
        entry.ctime = Some(metadata.ctime() as u64);
        entry.ctime_nsec = metadata.ctime_nsec() as u32;
        entry.dev = metadata.dev();
        entry.ino = metadata.ino();
        entry.nlink = metadata.nlink() as u32;

        // Extract device major/minor for block/char devices
        if entry_type == EntryType::BlockDevice || entry_type == EntryType::CharDevice {
            let rdev = metadata.rdev() as libc::dev_t;
            entry.devmajor = libc::major(rdev) as u32;
            entry.devminor = libc::minor(rdev) as u32;
        }
    }

    #[cfg(not(unix))]
    {
        entry.mode = if metadata.permissions().readonly() {
            0o444
        } else {
            0o644
        };
        entry.mtime = metadata
            .modified()
            .ok()
            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
            .map(|d| d.as_secs())
            .unwrap_or(0);
    }

    if entry_type == EntryType::Regular || entry_type == EntryType::Symlink {
        entry.size = metadata.size();
    }

    // Try to get user/group names
    #[cfg(unix)]
    {
        entry.uname = cached_username(entry.uid);
        entry.gname = cached_groupname(entry.gid);
    }

    Ok(entry)
}

/// Get username from uid
#[cfg(unix)]
/// uid/gid to name lookups, memoized for the life of the process.
///
/// build_entry needs both for every member, and getpwuid/getgrgid are not
/// cached by libc: under a `files` backend each call is an open/read/close of
/// /etc/passwd or /etc/group, and under LDAP or SSSD a network round trip. A
/// file hierarchy almost always has one or two distinct owners, so a tree of
/// 100,000 files made 200,000 lookups where two would do.
fn cached_username(uid: u32) -> Option<String> {
    thread_local! {
        static USERS: std::cell::RefCell<std::collections::HashMap<u32, Option<String>>> =
            std::cell::RefCell::new(std::collections::HashMap::new());
    }
    USERS.with(|c| {
        c.borrow_mut()
            .entry(uid)
            .or_insert_with(|| get_username(uid))
            .clone()
    })
}

fn cached_groupname(gid: u32) -> Option<String> {
    thread_local! {
        static GROUPS: std::cell::RefCell<std::collections::HashMap<u32, Option<String>>> =
            std::cell::RefCell::new(std::collections::HashMap::new());
    }
    GROUPS.with(|c| {
        c.borrow_mut()
            .entry(gid)
            .or_insert_with(|| get_groupname(gid))
            .clone()
    })
}

fn get_username(uid: u32) -> Option<String> {
    unsafe {
        let pw = libc::getpwuid(uid);
        if pw.is_null() {
            return None;
        }
        let name = std::ffi::CStr::from_ptr((*pw).pw_name);
        name.to_str().ok().map(|s| s.to_string())
    }
}

/// Get group name from gid
#[cfg(unix)]
fn get_groupname(gid: u32) -> Option<String> {
    unsafe {
        let gr = libc::getgrgid(gid);
        if gr.is_null() {
            return None;
        }
        let name = std::ffi::CStr::from_ptr((*gr).gr_name);
        name.to_str().ok().map(|s| s.to_string())
    }
}

/// Write files to a pre-existing archive writer (for multi-volume support)
pub fn write_files_to_archive<W: ArchiveWriter>(
    archive: &mut W,
    files: &[PathBuf],
    _format: ArchiveFormat,
    options: &WriteOptions,
) -> PaxResult<()> {
    write_files(archive, files, options)
}

/// Read file list from stdin (one path per line)
pub fn read_file_list<R: Read>(reader: R) -> PaxResult<Vec<PathBuf>> {
    read_file_list_sep(reader, b'\n')
}

/// Read a list of pathnames separated by `sep`.
///
/// `sep` is `b'\n'` for the usual `find | pax` pipeline and `b'\0'` for the
/// `find -print0` pipeline that tar's `--null` and cpio's `-0` select, which is
/// the only way a pathname containing a newline survives the trip.
pub fn read_file_list_sep<R: Read>(reader: R, sep: u8) -> PaxResult<Vec<PathBuf>> {
    use std::io::BufRead;

    let mut reader = std::io::BufReader::new(reader);
    let mut files = Vec::new();
    let mut buf = Vec::new();

    loop {
        buf.clear();
        if reader.read_until(sep, &mut buf)? == 0 {
            return Ok(files);
        }
        if buf.last() == Some(&sep) {
            buf.pop();
        }
        // Keep the name verbatim so pathnames with leading or trailing spaces
        // survive; skip only a wholly empty entry (e.g. a trailing separator).
        if !buf.is_empty() {
            files.push(path_from_bytes(&buf));
        }
    }
}

/// Turn a pathname read from a file list into a `PathBuf`.
///
/// A pathname is bytes, not text, so on unix the bytes are kept exactly --
/// which is the point of reading the list this way rather than by lines.
#[cfg(unix)]
fn path_from_bytes(bytes: &[u8]) -> PathBuf {
    use std::os::unix::ffi::OsStrExt;
    PathBuf::from(std::ffi::OsStr::from_bytes(bytes).to_owned())
}

#[cfg(not(unix))]
fn path_from_bytes(bytes: &[u8]) -> PathBuf {
    PathBuf::from(String::from_utf8_lossy(bytes).into_owned())
}

/// Restore the access time `-t` recorded, on the file that was actually read.
///
/// Stamping goes through the descriptor the data came from rather than by name.
/// Resolving the name a second time was wrong both ways round: without
/// `AT_SYMLINK_NOFOLLOW` a name replaced by a symbolic link in between would
/// redirect the timestamp onto the link's target, and with it, `-L`/`-H` stamped
/// the link rather than the file whose access time the read had actually
/// disturbed. A descriptor has neither problem, and `UTIME_OMIT` leaves the
/// modification time alone instead of reading it back to write it again.
#[cfg(unix)]
fn reset_atime(file: &File, path: &Path, atime_sec: i64, atime_nsec: i64) {
    use std::os::fd::AsRawFd;

    let times = [
        libc::timespec {
            tv_sec: atime_sec as libc::time_t,
            tv_nsec: atime_nsec as libc::c_long,
        },
        libc::timespec {
            tv_sec: 0,
            tv_nsec: libc::UTIME_OMIT,
        },
    ];

    let result = unsafe { libc::futimens(file.as_raw_fd(), times.as_ptr()) };
    if result != 0 {
        eprintln!(
            "pax: warning: cannot reset atime on {}: {}",
            path.display(),
            std::io::Error::last_os_error()
        );
    }
}
