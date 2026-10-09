//
// Copyright (c) 2024-2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{
    collections::HashSet,
    ffi::OsString,
    fs, io,
    path::{Path, PathBuf},
};

use crate::diff_util::{
    constants::COULD_NOT_UNWRAP_FILENAME,
    diff_exit_status::DiffExitStatus,
    file_diff::{FileDiff, Source},
    functions::io_error_at,
};

use super::{common::FormatOptions, dir_data::DirData};

/// What identifies a directory for loop detection: (device, inode) on Unix.
#[cfg(unix)]
type DirId = (u64, u64);

/// Stable Rust does not expose a Windows file's volume serial number and file
/// index, so a directory is identified by its canonical path instead:
/// canonicalizing resolves symlinks and junctions to the directory they reach,
/// and Windows has no hard links to directories, so the path is unique.
#[cfg(windows)]
type DirId = PathBuf;

/// The identity of the directory `path` resolves to.
#[cfg(unix)]
fn dir_id(path: &Path) -> io::Result<DirId> {
    use std::os::unix::fs::MetadataExt;
    let md = fs::metadata(path)?;
    Ok((md.dev(), md.ino()))
}

#[cfg(windows)]
fn dir_id(path: &Path) -> io::Result<DirId> {
    fs::canonicalize(path)
}

/// What kind of special file `file_type` (neither a regular file nor a
/// directory) is, as diff names it.
#[cfg(unix)]
fn special_kind(file_type: fs::FileType) -> &'static str {
    use std::os::unix::fs::FileTypeExt;
    if file_type.is_fifo() {
        "fifo"
    } else if file_type.is_block_device() {
        "block special file"
    } else if file_type.is_char_device() {
        "character special file"
    } else {
        "special file"
    }
}

/// Windows has no FIFOs or device files in the file system.
#[cfg(windows)]
fn special_kind(_file_type: fs::FileType) -> &'static str {
    "special file"
}

/// A path as it appears in output.
fn display(path: &Path) -> String {
    path.to_str()
        .unwrap_or(COULD_NOT_UNWRAP_FILENAME)
        .to_string()
}

/// What a directory entry is, once symlinks have been followed.
#[derive(Clone, Copy, PartialEq, Eq)]
enum EntryKind {
    File,
    Directory,
    /// A FIFO, block-special or character-special file, named as GNU names it
    /// in the mismatch message. diff cannot read one as a regular file, and
    /// opening a FIFO would block.
    Special(&'static str),
}

impl EntryKind {
    fn describe(self) -> &'static str {
        match self {
            EntryKind::File => "regular file",
            EntryKind::Directory => "directory",
            EntryKind::Special(what) => what,
        }
    }
}

pub struct DirDiff<'a> {
    dir1: &'a mut DirData,
    dir2: &'a mut DirData,
    format_options: &'a FormatOptions,
    recursive: bool,
    /// The option arguments as they were given on the command line, for the
    /// per-file header POSIX specifies.
    options: &'a [String],
}

impl<'a> DirDiff<'a> {
    fn new(
        dir1: &'a mut DirData,
        dir2: &'a mut DirData,
        format_options: &'a FormatOptions,
        recursive: bool,
        options: &'a [String],
    ) -> Self {
        Self {
            dir1,
            dir2,
            format_options,
            recursive,
            options,
        }
    }

    pub fn dir_diff(
        path1: PathBuf,
        path2: PathBuf,
        format_options: &FormatOptions,
        recursive: bool,
        options: &[String],
    ) -> DiffExitStatus {
        let mut visited = HashSet::new();
        Self::dir_diff_inner(
            [path1, path2],
            [false, false],
            format_options,
            recursive,
            options,
            &mut visited,
        )
    }

    /// Recursive directory comparison with (dev, ino) tracking of directories
    /// already visited on the current path, so symlink cycles cannot cause
    /// infinite recursion.
    ///
    /// `absent` marks a side that does not exist, which `-N` compares as an
    /// empty directory.
    fn dir_diff_inner(
        [path1, path2]: [PathBuf; 2],
        absent: [bool; 2],
        format_options: &FormatOptions,
        recursive: bool,
        options: &[String],
        visited: &mut HashSet<DirId>,
    ) -> DiffExitStatus {
        // The two operands themselves go in before anything descends, so a
        // link back to either of them is caught as a loop.
        for path in [&path1, &path2] {
            if let Ok(id) = dir_id(path) {
                visited.insert(id);
            }
        }

        let load = |path: PathBuf, absent: bool| {
            if absent {
                Ok(DirData::absent(path))
            } else {
                DirData::load(path)
            }
        };
        let (mut dir1, mut dir2) = match (load(path1, absent[0]), load(path2, absent[1])) {
            (Ok(d1), Ok(d2)) => (d1, d2),
            (Err(e), _) | (_, Err(e)) => {
                Self::report(&e);
                return DiffExitStatus::Trouble;
            }
        };

        let mut dir_diff = DirDiff::new(&mut dir1, &mut dir2, format_options, recursive, options);
        dir_diff.analyze(visited)
    }

    /// Report an error and keep walking. The error names the path it happened
    /// on -- see `io_error_at` -- so this no longer has to guess, which it did
    /// by always naming the first operand.
    fn report(error: &io::Error) {
        eprintln!("diff: {}", error);
    }

    /// Recurse into a common subdirectory, refusing to re-enter a directory
    /// already on the current path.
    ///
    /// POSIX requires a diagnostic when the walk detects a loop; this used to
    /// skip in silence and exit 0. `visited` is also popped on the way back
    /// out, so it describes the current path rather than everything ever seen
    /// -- two sibling links to one directory are now both compared instead of
    /// the second silently disappearing.
    fn descend(
        &self,
        path1: &Path,
        path2: &Path,
        absent: [bool; 2],
        visited: &mut HashSet<DirId>,
    ) -> DiffExitStatus {
        let mut ids = Vec::new();
        for (path, absent) in [(path1, absent[0]), (path2, absent[1])] {
            if absent {
                continue;
            }
            let id = match dir_id(path) {
                Ok(id) => id,
                Err(e) => {
                    Self::report(&io_error_at(path, e));
                    return DiffExitStatus::Trouble;
                }
            };
            if visited.contains(&id) {
                eprintln!("diff: {}: recursive directory loop", display(path));
                return DiffExitStatus::Trouble;
            }
            ids.push(id);
        }
        visited.extend(ids.iter().cloned());

        let result = Self::dir_diff_inner(
            [path1.to_path_buf(), path2.to_path_buf()],
            absent,
            self.format_options,
            self.recursive,
            self.options,
            visited,
        );

        for id in &ids {
            visited.remove(id);
        }
        result
    }

    /// The `diff <options> <file1> <file2>` line printed before a differing
    /// pair.
    ///
    /// POSIX wants the options "as specified on the command line", so echo the
    /// ones the user actually typed rather than a canonical rendering of the
    /// parsed result -- this used to turn `-c` into `-C 3`, add a trailing
    /// space, and substitute a --label value for the pathname operand, which
    /// made the printed command something that could not be run.
    fn file_header(&self, path1: &Path, path2: &Path) -> String {
        let mut header = String::from("diff");
        for option in self.options {
            header.push(' ');
            header.push_str(option);
        }
        header.push(' ');
        header.push_str(&display(path1));
        header.push(' ');
        header.push_str(&display(path2));
        header
    }

    /// What an entry is, as far as diff cares.
    ///
    /// Classification follows symlinks. `DirEntry::file_type` does not, so a
    /// symlink to a regular file reported neither file nor special and was
    /// treated as a directory: without -r that printed "Common subdirectories",
    /// and with -r the walk called read_dir on it and the whole run died with
    /// ENOTDIR. Any source tree containing symlinks was uncomparable.
    fn classify(path: &Path) -> io::Result<EntryKind> {
        let file_type = fs::metadata(path)?.file_type();
        Ok(if file_type.is_dir() {
            EntryKind::Directory
        } else if file_type.is_file() {
            EntryKind::File
        } else {
            EntryKind::Special(special_kind(file_type))
        })
    }

    fn analyze(&mut self, visited: &mut HashSet<DirId>) -> DiffExitStatus {
        let mut exit_status = DiffExitStatus::NotDifferent;

        let mut dir1_files_name = self.dir1.files().keys().collect::<Vec<&OsString>>();
        let mut dir2_files_name = self.dir2.files().keys().collect::<Vec<&OsString>>();
        dir1_files_name.append(&mut dir2_files_name);

        let mut unique_files_name = HashSet::<&OsString>::from_iter(dir1_files_name)
            .iter()
            .cloned()
            .collect::<Vec<&OsString>>();
        unique_files_name.sort();

        for file_name in unique_files_name {
            let in_dir1 = self.dir1.files().contains_key(file_name);
            let in_dir2 = self.dir2.files().contains_key(file_name);

            let inner = if in_dir1 && in_dir2 {
                self.compare_entry(file_name, [false, false], visited)
            } else if self.format_options.new_file {
                self.compare_entry(file_name, [!in_dir1, !in_dir2], visited)
            } else {
                self.only_in(file_name, in_dir1)
            };
            if exit_status.status_code() < inner.status_code() {
                exit_status = inner;
            }
        }

        exit_status
    }

    /// Report an entry present in only one tree. That is a difference, so it
    /// has to raise the exit status: `if diff -r a b; then` was useless while
    /// this only printed.
    fn only_in(&self, file_name: &OsString, in_dir1: bool) -> DiffExitStatus {
        let dir = if in_dir1 { &self.dir1 } else { &self.dir2 };
        println!(
            "Only in {}: {}",
            dir.path_str(),
            file_name.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME)
        );
        DiffExitStatus::Different
    }

    /// What the two entries named `file_name` are. Under `-N` the side marked
    /// `absent` is taken to be the same kind as the side that exists.
    fn kinds(path1: &Path, path2: &Path, absent: [bool; 2]) -> io::Result<(EntryKind, EntryKind)> {
        match absent {
            [true, _] => Self::classify(path2).map(|k| (k, k)),
            [_, true] => Self::classify(path1).map(|k| (k, k)),
            _ => Ok((Self::classify(path1)?, Self::classify(path2)?)),
        }
    }

    /// Compare the two entries named `file_name`. `absent` marks a side that
    /// does not exist, which only `-N` compares rather than reporting it as
    /// "Only in".
    fn compare_entry(
        &self,
        file_name: &OsString,
        absent: [bool; 2],
        visited: &mut HashSet<DirId>,
    ) -> DiffExitStatus {
        let path1 = self.dir1.path().join(file_name);
        let path2 = self.dir2.path().join(file_name);

        // One unreadable entry used to end the walk: the error was propagated
        // out of analyze, so every later entry went uncompared. Report it
        // against the path it happened on and carry on, which is what GNU does.
        let (kind1, kind2) = match Self::kinds(&path1, &path2, absent) {
            Ok(kinds) => kinds,
            Err(e) => {
                Self::report(&e);
                return DiffExitStatus::Trouble;
            }
        };

        match (kind1, kind2) {
            (EntryKind::File, EntryKind::File) => {
                let header = self.file_header(&path1, &path2);
                let source = |path: PathBuf, absent: bool| {
                    if absent {
                        Ok(Source::absent(&path))
                    } else {
                        Source::from_path(path)
                    }
                };
                let result = source(path1, absent[0]).and_then(|src1| {
                    let src2 = source(path2, absent[1])?;
                    FileDiff::diff_sources(src1, src2, self.format_options, Some(header))
                });
                result.unwrap_or_else(|e| {
                    Self::report(&e);
                    DiffExitStatus::Trouble
                })
            }
            (EntryKind::Directory, EntryKind::Directory) => {
                if self.recursive {
                    self.descend(&path1, &path2, absent, visited)
                } else {
                    // Two directories left uncompared are not a difference;
                    // GNU exits 0 for this alone, and under -N prints this for
                    // a directory on one side only as well.
                    println!(
                        "Common subdirectories: {} and {}",
                        display(&path1),
                        display(&path2)
                    );
                    DiffExitStatus::NotDifferent
                }
            }
            // A special file has no contents to compare with an empty file.
            _ if absent.contains(&true) => self.only_in(file_name, !absent[0]),
            (k1, k2) => {
                // Anything else is a mismatch between the two trees, and a
                // mismatch is a difference.
                println!(
                    "File {} is a {} while file {} is a {}",
                    display(&path1),
                    k1.describe(),
                    display(&path2),
                    k2.describe()
                );
                DiffExitStatus::Different
            }
        }
    }
}
