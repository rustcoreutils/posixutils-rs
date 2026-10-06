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
use crate::subst::{substitute_name, Substitution};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, Read, Seek, Write};
use std::os::fd::AsFd;
#[cfg(unix)]
use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};
use std::rc::Rc;

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
    /// The terminal `-i` prompts on. Opened by the caller before the archive
    /// file is created or touched: where there is no terminal, the run fails
    /// with the archive as it was, rather than after truncating it.
    pub prompter: Option<InteractivePrompter>,
    /// tar: skip a socket the format cannot hold with a warning, not an error
    pub ignore_sockets: bool,
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
    pub update_times: Option<HashMap<PathBuf, MemberTime>>,
    /// The files the archive is being written to, which are left out rather
    /// than copied into themselves.
    pub archive_files: ArchiveFiles,
}

/// `(st_dev, st_ino)`, which identifies a file however it is named.
pub fn file_id(metadata: &std::fs::Metadata) -> (u64, u64) {
    (metadata.dev(), metadata.ino())
}

/// The `(st_dev, st_ino)` of each regular file the archive is written to.
///
/// A name list read as the walk goes can name the archive once it exists --
/// `find . | pax -w -f out.tar` -- and so can a walk of the directory holding
/// it. Under -M that is every volume, each created part way through the walk,
/// so the volume writer adds each one as it creates it: a set shared with the
/// writer rather than one id fixed before the walk begins.
#[derive(Clone, Default)]
pub struct ArchiveFiles(Rc<RefCell<HashSet<(u64, u64)>>>);

impl ArchiveFiles {
    /// Record a file the archive is written to. Only a regular file counts:
    /// a device such as `/dev/null` or a tape is not one the walk could be
    /// copying into itself.
    pub fn add(&self, metadata: &std::fs::Metadata) {
        if metadata.is_file() {
            self.0.borrow_mut().insert(file_id(metadata));
        }
    }

    fn contains(&self, metadata: &ftw::Metadata) -> bool {
        self.0.borrow().contains(&(metadata.dev(), metadata.ino()))
    }
}

/// A member's modification time as `(seconds, nanoseconds)`.
pub type MemberTime = (i64, u32);

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
        let name = crate::rawpath::trim_trailing_slashes(archive_path);
        let Some(&member_mtime) = times.get(name) else {
            return false;
        };
        not_newer(file_mtime(metadata), member_mtime)
    }
}

/// Whether a file last modified at `file` is no newer than a member recording
/// `member`.
///
/// A member with a fraction of a second -- a pax `mtime` record -- is compared
/// to the nanosecond, so a file changed again within the same second is newer.
/// One without has its time to the second only: every ustar header, where
/// what was archived from a file modified at 100.5 says 100. Comparing the
/// file's 100.5 to that would append an unchanged file on every run, so
/// against it only the file's seconds count.
fn not_newer(file: MemberTime, member: MemberTime) -> bool {
    if member.1 == 0 {
        file.0 <= member.0
    } else {
        file <= member
    }
}

/// A file's modification time as `(seconds, nanoseconds)`.
fn file_mtime(metadata: &ftw::Metadata) -> MemberTime {
    (metadata.mtime(), metadata.mtime_nsec() as u32)
}

/// The pathnames write, append and copy mode act on, in order.
///
/// An iterator rather than a slice so a list read from standard input or a
/// `-T` file is consumed as the walk asks for it: collecting it first cost
/// memory linear in the list and wrote nothing until its producer had exited,
/// which is what stops `find / | pax -w | ssh ...` from streaming.
pub type FileNames<'a> = dyn Iterator<Item = PathBuf> + 'a;

/// Create an archive from files
pub fn create_archive<W: Write>(
    writer: W,
    files: &mut FileNames<'_>,
    format: ArchiveFormat,
    options: &mut WriteOptions,
) -> PaxResult<()> {
    match format {
        ArchiveFormat::Ustar => write_archive(&mut UstarWriter::new(writer), files, options),
        ArchiveFormat::Cpio => write_archive(
            &mut CpioWriter::with_format(writer, options.cpio_format),
            files,
            options,
        ),
        ArchiveFormat::Pax => write_archive(
            &mut PaxWriter::with_options(writer, options.format_options.clone()),
            files,
            options,
        ),
    }
}

/// Archive `files` and write the trailer, with every I/O failure of `archive`
/// itself marked as the archive's (see `ArchiveSink`).
///
/// End of file on `/dev/tty` under -i ends the run, but what has been
/// archived by then is still finished with a trailer: without one a cpio
/// archive cannot be read at all.
fn write_archive<A: ArchiveWriter>(
    archive: &mut A,
    files: &mut FileNames<'_>,
    options: &mut WriteOptions,
) -> PaxResult<()> {
    let mut sink = ArchiveSink(archive);
    match write_files(&mut sink, files, options) {
        Err(PaxError::TtyEof) => {
            sink.finish()?;
            Err(PaxError::TtyEof)
        }
        written => {
            written?;
            sink.finish()
        }
    }
}

/// An archive writer whose I/O errors are known to be the archive's.
///
/// While a file is archived, its own reads and the archive's writes happen side
/// by side, and both fail with an `io::Error`. Telling them apart by errno does
/// not work -- EIO, EFBIG or EAGAIN can come from either -- and getting it wrong
/// either blames every remaining source file for the archive's failure or stops
/// the run over one unreadable file. A writer's only I/O is its sink, so this
/// wrapper re-labels each such error `ArchiveWrite`, which ends the run.
struct ArchiveSink<'a, A: ArchiveWriter>(&'a mut A);

impl<A: ArchiveWriter> ArchiveSink<'_, A> {
    fn sink<T>(result: PaxResult<T>) -> PaxResult<T> {
        result.map_err(|e| match e {
            PaxError::Io(e) => PaxError::ArchiveWrite(e),
            e => e,
        })
    }
}

impl<A: ArchiveWriter> ArchiveWriter for ArchiveSink<'_, A> {
    fn write_entry(&mut self, entry: &ArchiveEntry) -> PaxResult<()> {
        Self::sink(self.0.write_entry(entry))
    }

    fn write_data(&mut self, data: &[u8]) -> PaxResult<()> {
        Self::sink(self.0.write_data(data))
    }

    fn finish_entry(&mut self) -> PaxResult<()> {
        Self::sink(self.0.finish_entry())
    }

    fn finish(&mut self) -> PaxResult<()> {
        Self::sink(self.0.finish())
    }

    fn supports_hardlinks(&self) -> bool {
        self.0.supports_hardlinks()
    }

    fn needs_data_checksum(&self) -> bool {
        self.0.needs_data_checksum()
    }

    fn hardlinks_may_carry_data(&self) -> bool {
        self.0.hardlinks_may_carry_data()
    }

    fn supports_sockets(&self) -> bool {
        self.0.supports_sockets()
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
    files: &mut FileNames<'_>,
    options: &mut WriteOptions,
) -> PaxResult<()> {
    let prompter = options.prompter.take();
    let options = &*options;

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
            &path,
            |entry| walk.visit(entry),
            |entry, exit| walk.leave_directory(&entry, exit),
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
    /// `st_dev` of each directory descended into. The first is the operand's,
    /// which is what `-X` compares against; an empty stack means the walk is
    /// at an operand.
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
            crate::rawpath::as_bytes(path),
        ) {
            return Ok(false);
        }

        let Some(metadata) = entry.metadata() else {
            // ftw reports its own failures through err_reporter; reaching here
            // without metadata would mean it handed us an entry it could not
            // stat, which it does not do inside the handler.
            return Ok(false);
        };

        if self.options.archive_files.contains(metadata) {
            crate::error::report_warning(
                path,
                gettextrs::gettext("file is the archive; not dumped"),
            );
            return Ok(false);
        }

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

    /// Undo what `descend` did, once the walk is done with a directory, and
    /// for -t put back the access time reading it disturbed.
    fn leave_directory(&self, entry: &ftw::Entry<'_>, exit: ftw::DirExit) -> Result<(), ()> {
        self.dev_stack.borrow_mut().pop();
        if self.options.reset_atime && exit == ftw::DirExit::Descended {
            crate::modes::anchored::restore_dir_atime(entry);
        }
        Ok(())
    }

    /// Whether to walk into a directory, recording its device for `-X` when so.
    ///
    /// `-X` is decided here and nowhere else: a directory on another device
    /// is archived like any other and only its contents are left out.
    fn descend(&self, metadata: &ftw::Metadata) -> bool {
        let operand_dev = self.dev_stack.borrow().first().copied();
        if self.options.no_recurse
            || !crate::modes::may_descend(self.options.one_file_system, operand_dev, metadata.dev())
        {
            return false;
        }
        self.dev_stack.borrow_mut().push(metadata.dev());
        true
    }

    fn archive_entry(
        &self,
        entry: &ftw::Entry<'_>,
        path: &Path,
        metadata: &ftw::Metadata,
    ) -> PaxResult<bool> {
        // Apply substitutions first (per POSIX: -s applies before -i). A name
        // that becomes empty is ignored -- that name only: a directory's
        // descendants are still archived, each under its own substitution.
        let Some(archive_path) = substitute_name(&self.options.substitutions, path) else {
            return Ok(metadata.is_dir() && self.descend(metadata));
        };

        // Handle interactive rename
        let archive_path = {
            let mut prompter = self.prompter.borrow_mut();
            if let Some(ref mut p) = *prompter {
                match p.prompt(&archive_path)? {
                    // POSIX: "the file ... shall be skipped" -- that name
                    // alone, as with an empty -s replacement.
                    RenameResult::Skip => return Ok(metadata.is_dir() && self.descend(metadata)),
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
                // A header the format refuses (a time before 1970 in ustar,
                // say) loses this entry, not everything below it.
                match write_dir_entry(archive, &archive_path, metadata) {
                    Err(e) if !crate::modes::is_fatal(&e) => crate::error::report_error(path, e),
                    r => r?,
                }
            }
            return Ok(self.descend(metadata));
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
            write_special(archive, &archive_path, metadata, self.options)?;
        }

        Ok(false)
    }
}

/// Write a directory's header.
fn write_dir_entry<W: ArchiveWriter>(
    archive: &mut W,
    archive_path: &Path,
    metadata: &ftw::Metadata,
) -> PaxResult<()> {
    let dir_entry = build_entry(archive_path, metadata, EntryType::Directory)?;
    archive.write_entry(&dir_entry)?;
    archive.finish_entry()
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
    options: &WriteOptions,
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
        // POSIX: "Attempts to archive a socket shall produce a diagnostic
        // message when ustar interchange format is used". The pax format has
        // no socket type either; recording one as an empty regular file
        // would extract something the file never was.
        if !archive.supports_sockets() {
            // tar's front-end follows GNU tar and bsdtar: a warning, and the
            // run still succeeds.
            if options.ignore_sockets {
                crate::error::report_warning(path, gettextrs::gettext("socket ignored"));
            } else {
                crate::error::report_error(
                    path,
                    gettextrs::gettext("socket not archived: the format has no socket type"),
                );
            }
            return Ok(());
        }
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
    _options: &WriteOptions,
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
    let original = link_tracker.lookup(entry.dev, entry.ino, entry.nlink);
    let later_name = original.is_some();
    if let Some(original_path) = original {
        // The same file met again under the very name it was first archived
        // as (`pax -w f f`, or `find tree | pax -w` reaching it from both the
        // list and the walk), however spelled: `./h/a` and `h/a` are one name.
        // "f == f" extracts by unlinking f and then failing to link it, and
        // archiving the data again would split f from any other name linked
        // to it in between. The earlier member already says everything this
        // one could.
        if crate::rawpath::same_name(&original_path, &entry.path) {
            return Ok(());
        }

        // Only claim the link in formats that can express one. cpio cannot, so
        // recording the type there would degrade the member to a regular file
        // -- and combined with the size=0 below, to an empty one.
        let linkable = archive.supports_hardlinks();
        if linkable {
            entry.entry_type = EntryType::Hardlink;
            entry.link_target = Some(original_path);
        }

        // By default a hard link has size=0 and no data -- but only where the
        // format records the linkage, otherwise the contents are the only copy
        // of the data this member will ever have. `-o linkdata` asks for the
        // contents with every link, which only the pax format can carry.
        let with_data = options.format_options.link_data && archive.hardlinks_may_carry_data();
        if linkable && !with_data {
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
    let mut file = crate::modes::anchored::open_source_file(
        entry_ref.dir_fd(),
        entry_ref.file_name(),
        crate::modes::followed_link(entry_ref, metadata),
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
    // Only now is there a member for a later name of this file to link to.
    if !later_name {
        link_tracker.record(entry.dev, entry.ino, entry.nlink, &entry.path);
    }

    if options.reset_atime {
        crate::modes::anchored::restore_atime(file.as_fd(), src_path, metadata);
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
/// exit status records that the archive does not match what was on disk. A read
/// error part-way through is the same case: it is the file's failure, reported
/// against the file, and the member is still padded out to its declared size --
/// stopping short would leave the next header inside this member's data area,
/// where no reader would find it or anything after it. The bound also has to
/// come from `size` rather than from end-of-file: waiting for a shrinking file
/// to deliver bytes it no longer has is how CVE-2018-20482 turned into an
/// infinite loop.
///
/// Only an error from `archive` is returned; one from `file` is reported here.
fn copy_file_data<R: Read, W: ArchiveWriter>(
    file: &mut R,
    archive: &mut W,
    size: u64,
    path: &Path,
) -> PaxResult<()> {
    let mut buf = [0u8; 8192];
    let mut remaining = size;

    while remaining > 0 {
        let want = remaining.min(buf.len() as u64) as usize;
        let n = match read_retrying(file, &mut buf[..want]) {
            Ok(0) => break,
            Ok(n) => n,
            Err(e) => {
                crate::error::report_error(
                    path,
                    format!("{e}; padding {remaining} bytes with zeros"),
                );
                return pad_with_zeros(archive, remaining);
            }
        };
        archive.write_data(&buf[..n])?;
        remaining -= n as u64;
    }

    if remaining > 0 {
        crate::error::report_error(
            path,
            format!("File shrank by {remaining} bytes; padding with zeros"),
        );
        pad_with_zeros(archive, remaining)?;
    } else if matches!(read_retrying(file, &mut buf[..1]), Ok(n) if n != 0) {
        // Still more to read than the header promised.
        crate::error::report_error(path, "file changed as we read it");
    }

    Ok(())
}

/// `read`, retried for as long as it is interrupted by a signal.
fn read_retrying<R: Read>(file: &mut R, buf: &mut [u8]) -> std::io::Result<usize> {
    loop {
        match file.read(buf) {
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => {}
            result => return result,
        }
    }
}

/// Write `count` zero bytes of member data.
fn pad_with_zeros<W: ArchiveWriter>(archive: &mut W, mut count: u64) -> PaxResult<()> {
    let zeros = [0u8; 8192];
    while count > 0 {
        let n = count.min(zeros.len() as u64) as usize;
        archive.write_data(&zeros[..n])?;
        count -= n as u64;
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
        entry.mtime = metadata.mtime();
        // Capture sub-second times so the pax interchange format can record a
        // fractional `mtime`/`atime` (other formats ignore the nsec fields).
        entry.mtime_nsec = metadata.mtime_nsec() as u32;
        entry.atime = Some(metadata.atime());
        entry.atime_nsec = metadata.atime_nsec() as u32;
        entry.ctime = Some(metadata.ctime());
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
            .map(|d| d.as_secs() as i64)
            .unwrap_or(0);
    }

    if entry_type == EntryType::Regular || entry_type == EntryType::Symlink {
        entry.size = metadata.size();
    }

    // Try to get user/group names
    #[cfg(unix)]
    {
        entry.uname = crate::userdb::name_for_uid(entry.uid);
        entry.gname = crate::userdb::name_for_gid(entry.gid);
    }

    Ok(entry)
}

/// Archive files into a pre-existing archive writer and write its trailer (for
/// multi-volume support)
pub fn write_files_to_archive<W: ArchiveWriter>(
    archive: &mut W,
    files: &mut FileNames<'_>,
    options: &mut WriteOptions,
) -> PaxResult<()> {
    write_archive(archive, files, options)
}

/// A list of pathnames for write, append or copy mode: standard input, or a
/// file named by tar's `-T`, separated by `sep`.
///
/// `sep` is `b'\n'` for the usual `find | pax` pipeline and `b'\0'` for the
/// `find -print0` pipeline that tar's `--null` and cpio's `-0` select, which is
/// the only way a pathname containing a newline survives the trip.
///
/// Nothing is read until [`names`](Self::names) is iterated. A `-T` file is
/// opened when the command line is parsed, though, so its name is resolved
/// from the directory the command was invoked in rather than from tar's `-C`.
#[derive(Debug)]
pub struct NameList {
    source: NameSource,
    sep: u8,
}

#[derive(Debug)]
enum NameSource {
    Stdin,
    File(File),
    Empty,
}

impl NameList {
    /// The names on standard input.
    pub fn stdin(sep: u8) -> Self {
        NameList {
            source: NameSource::Stdin,
            sep,
        }
    }

    /// The names in an open file.
    pub fn file(file: File, sep: u8) -> Self {
        NameList {
            source: NameSource::File(file),
            sep,
        }
    }

    /// No names at all: what stands in for standard input when a front-end
    /// is given nothing to archive and must not read it.
    pub fn empty() -> Self {
        NameList {
            source: NameSource::Empty,
            sep: b'\n',
        }
    }

    /// Whether the names are read from standard input.
    pub fn is_stdin(&self) -> bool {
        matches!(self.source, NameSource::Stdin)
    }

    /// The names, read one at a time as they are asked for.
    pub fn names(self) -> NameReader<Box<dyn BufRead>> {
        let reader: Box<dyn BufRead> = match self.source {
            NameSource::Stdin => Box::new(std::io::stdin().lock()),
            NameSource::File(file) => Box::new(std::io::BufReader::new(file)),
            NameSource::Empty => Box::new(std::io::empty()),
        };
        NameReader {
            reader,
            sep: self.sep,
            done: false,
        }
    }
}

/// The pathnames of a [`NameList`], one per `next`.
///
/// A name holding a NUL byte -- `find -print0` piped to a list read by lines --
/// can name no file, since the system interfaces end a pathname at the first
/// NUL. It is diagnosed here and left out, so the exit status records it and
/// the rest of the list is still processed, rather than handed on to a walk
/// that would otherwise act on the prefix before the NUL instead.
///
/// A read error ends the list: it is returned once, and nothing follows it.
pub struct NameReader<R> {
    reader: R,
    sep: u8,
    done: bool,
}

impl<R: BufRead> Iterator for NameReader<R> {
    type Item = PaxResult<PathBuf>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut buf = Vec::new();
        while !self.done {
            buf.clear();
            match self.reader.read_until(self.sep, &mut buf) {
                Ok(0) => self.done = true,
                Ok(_) => {
                    if buf.last() == Some(&self.sep) {
                        buf.pop();
                    }
                    // Keep the name verbatim so pathnames with leading or
                    // trailing spaces survive; skip only a wholly empty entry
                    // (e.g. a trailing separator).
                    if buf.contains(&0) {
                        crate::error::report_error(
                            &path_from_bytes(std::mem::take(&mut buf)),
                            gettextrs::gettext("pathname contains a NUL byte"),
                        );
                    } else if !buf.is_empty() {
                        return Some(Ok(path_from_bytes(buf)));
                    }
                }
                Err(e) => {
                    self.done = true;
                    return Some(Err(e.into()));
                }
            }
        }
        None
    }
}

/// Turn a pathname read from a file list into a `PathBuf`.
///
/// A pathname is bytes, not text, so on unix the bytes are kept exactly --
/// which is the point of reading the list this way rather than by lines.
#[cfg(unix)]
fn path_from_bytes(bytes: Vec<u8>) -> PathBuf {
    use std::os::unix::ffi::OsStringExt;
    PathBuf::from(std::ffi::OsString::from_vec(bytes))
}

#[cfg(not(unix))]
fn path_from_bytes(bytes: Vec<u8>) -> PathBuf {
    PathBuf::from(String::from_utf8_lossy(&bytes).into_owned())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Collects member data; fails every call with `fail` once that is set.
    #[derive(Default)]
    struct DataSink {
        data: Vec<u8>,
        fail: Option<std::io::ErrorKind>,
    }

    impl ArchiveWriter for DataSink {
        fn write_entry(&mut self, _entry: &ArchiveEntry) -> PaxResult<()> {
            Ok(())
        }

        fn write_data(&mut self, data: &[u8]) -> PaxResult<()> {
            if let Some(kind) = self.fail {
                return Err(PaxError::Io(kind.into()));
            }
            self.data.extend_from_slice(data);
            Ok(())
        }

        fn finish_entry(&mut self) -> PaxResult<()> {
            Ok(())
        }

        fn finish(&mut self) -> PaxResult<()> {
            Ok(())
        }
    }

    /// Yields `data`, then fails with EIO.
    struct FailingReader<'a> {
        data: &'a [u8],
    }

    impl Read for FailingReader<'_> {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            if self.data.is_empty() {
                return Err(std::io::Error::from_raw_os_error(libc::EIO));
            }
            let n = buf.len().min(self.data.len());
            buf[..n].copy_from_slice(&self.data[..n]);
            self.data = &self.data[n..];
            Ok(n)
        }
    }

    #[test]
    fn read_error_pads_member_to_declared_size() {
        let mut file = FailingReader {
            data: &[b'x'; 1000],
        };
        let mut sink = DataSink::default();
        copy_file_data(&mut file, &mut sink, 4000, Path::new("f")).unwrap();
        assert_eq!(sink.data.len(), 4000);
        assert!(sink.data[..1000].iter().all(|&b| b == b'x'));
        assert!(sink.data[1000..].iter().all(|&b| b == 0));
    }

    #[test]
    fn grown_file_is_cut_to_declared_size() {
        let mut file: &[u8] = &[b'y'; 5000];
        let mut sink = DataSink::default();
        copy_file_data(&mut file, &mut sink, 3000, Path::new("f")).unwrap();
        assert_eq!(sink.data, vec![b'y'; 3000]);
    }

    #[test]
    fn archive_errors_are_archive_write_errors() {
        // Any I/O error from the archive is fatal, whatever its kind; an error
        // reading a source file (the test above) is not -- the errno does not
        // decide, the origin does.
        let mut inner = DataSink {
            fail: Some(std::io::ErrorKind::Other),
            ..Default::default()
        };
        let mut sink = ArchiveSink(&mut inner);
        let mut file: &[u8] = b"abc";
        let err = copy_file_data(&mut file, &mut sink, 3, Path::new("f")).unwrap_err();
        assert!(matches!(err, PaxError::ArchiveWrite(_)));
        assert!(crate::modes::is_fatal(&err));
    }
}
