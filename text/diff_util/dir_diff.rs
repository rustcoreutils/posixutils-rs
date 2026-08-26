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
    os::unix::fs::{FileTypeExt, MetadataExt},
    path::{Path, PathBuf},
};

use crate::diff_util::{
    constants::COULD_NOT_UNWRAP_FILENAME, diff_exit_status::DiffExitStatus, file_diff::FileDiff,
    functions::io_error_at,
};

use super::{common::FormatOptions, dir_data::DirData};

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
            path1,
            path2,
            format_options,
            recursive,
            options,
            &mut visited,
        )
    }

    /// Recursive directory comparison with (dev, ino) tracking of directories
    /// already visited on the current path, so symlink cycles cannot cause
    /// infinite recursion.
    fn dir_diff_inner(
        path1: PathBuf,
        path2: PathBuf,
        format_options: &FormatOptions,
        recursive: bool,
        options: &[String],
        visited: &mut HashSet<(u64, u64)>,
    ) -> DiffExitStatus {
        // The two operands themselves go in before anything descends, so a
        // link back to either of them is caught as a loop.
        for path in [&path1, &path2] {
            if let Ok(md) = fs::metadata(path) {
                visited.insert((md.dev(), md.ino()));
            }
        }

        let (mut dir1, mut dir2) = match (DirData::load(path1), DirData::load(path2)) {
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
        visited: &mut HashSet<(u64, u64)>,
    ) -> DiffExitStatus {
        let mut ids = Vec::new();
        for path in [path1, path2] {
            let md = match fs::metadata(path) {
                Ok(md) => md,
                Err(e) => {
                    Self::report(&io_error_at(path, e));
                    return DiffExitStatus::Trouble;
                }
            };
            let id = (md.dev(), md.ino());
            if visited.contains(&id) {
                eprintln!("diff: {}: recursive directory loop", display(path));
                return DiffExitStatus::Trouble;
            }
            ids.push(id);
        }
        for id in &ids {
            visited.insert(*id);
        }

        let result = Self::dir_diff_inner(
            path1.to_path_buf(),
            path2.to_path_buf(),
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
            EntryKind::Special(if file_type.is_fifo() {
                "fifo"
            } else if file_type.is_block_device() {
                "block special file"
            } else if file_type.is_char_device() {
                "character special file"
            } else {
                "special file"
            })
        })
    }

    fn analyze(&mut self, visited: &mut HashSet<(u64, u64)>) -> DiffExitStatus {
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

            match (in_dir1, in_dir2) {
                (true, true) => {
                    let path1 = self.dir1.path().join(file_name);
                    let path2 = self.dir2.path().join(file_name);

                    // One unreadable entry used to end the walk: the error was
                    // propagated out of analyze, so every later entry went
                    // uncompared. Report it against the path it happened on and
                    // carry on, which is what GNU does.
                    let (kind1, kind2) = match (Self::classify(&path1), Self::classify(&path2)) {
                        (Ok(k1), Ok(k2)) => (k1, k2),
                        (Err(e), _) | (_, Err(e)) => {
                            Self::report(&e);
                            exit_status = DiffExitStatus::Trouble;
                            continue;
                        }
                    };

                    match (kind1, kind2) {
                        (EntryKind::File, EntryKind::File) => {
                            let header = self.file_header(&path1, &path2);
                            match FileDiff::file_diff(
                                path1,
                                path2,
                                self.format_options,
                                Some(header),
                            ) {
                                Ok(inner) => {
                                    if exit_status.status_code() < inner.status_code() {
                                        exit_status = inner;
                                    }
                                }
                                Err(e) => {
                                    Self::report(&e);
                                    exit_status = DiffExitStatus::Trouble;
                                }
                            }
                        }
                        (EntryKind::Directory, EntryKind::Directory) => {
                            if self.recursive {
                                let inner = self.descend(&path1, &path2, visited);
                                if exit_status.status_code() < inner.status_code() {
                                    exit_status = inner;
                                }
                            } else {
                                // Two directories left uncompared are not a
                                // difference; GNU exits 0 for this alone.
                                println!(
                                    "Common subdirectories: {} and {}",
                                    display(&path1),
                                    display(&path2)
                                );
                            }
                        }
                        (k1, k2) => {
                            // Anything else is a mismatch between the two
                            // trees, and a mismatch is a difference.
                            println!(
                                "File {} is a {} while file {} is a {}",
                                display(&path1),
                                k1.describe(),
                                display(&path2),
                                k2.describe()
                            );
                            if exit_status.status_code() < DiffExitStatus::Different.status_code() {
                                exit_status = DiffExitStatus::Different;
                            }
                        }
                    }
                }
                // An entry present in only one tree is a difference, so it
                // has to raise the exit status: `if diff -r a b; then` was
                // useless while these arms only printed.
                (true, false) => {
                    println!(
                        "Only in {}: {}",
                        self.dir1.path_str(),
                        file_name.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME)
                    );
                    if exit_status.status_code() < DiffExitStatus::Different.status_code() {
                        exit_status = DiffExitStatus::Different;
                    }
                }
                (false, true) => {
                    println!(
                        "Only in {}: {}",
                        self.dir2.path_str(),
                        file_name.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME)
                    );
                    if exit_status.status_code() < DiffExitStatus::Different.status_code() {
                        exit_status = DiffExitStatus::Different;
                    }
                }
                (false, false) => {
                    eprintln!(
                        "At least one of directories should contain file \"{}\"",
                        file_name.to_str().unwrap_or(COULD_NOT_UNWRAP_FILENAME)
                    );
                    return DiffExitStatus::Trouble;
                }
            }
        }

        exit_status
    }
}
