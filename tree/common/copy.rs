//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::error_string;
use ftw::{self, traverse_directory};
use gettextrs::gettext;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    ffi::{CStr, CString, OsStr},
    fs, io,
    mem::MaybeUninit,
    os::{
        fd::{AsRawFd, FromRawFd},
        unix::{ffi::OsStrExt, fs::MetadataExt},
    },
    path::{Path, PathBuf},
};

pub type InodeMap = HashMap<(u64, u64), (ftw::FileDescriptor, CString)>;

pub struct CopyConfig {
    pub force: bool,
    pub follow_cli: bool,
    pub dereference: bool,
    pub interactive: bool,
    pub preserve: bool,
    pub recursive: bool,
}

enum CopyResult {
    CopyingDirectory,
    CopiedFile,
    Skipped,
}

// Implements the algorithm for `cp`:
//
// https://pubs.opengroup.org/onlinepubs/9699919799/utilities/cp.html
fn copy_file_impl<F>(
    cfg: &CopyConfig,
    source: &ftw::Entry,
    target: &Path,
    target_dirfd: libc::c_int,
    target_filename: *const libc::c_char,
    created_files: &mut HashSet<PathBuf>,
    prompt_fn: F,
) -> io::Result<CopyResult>
where
    F: Fn(&str) -> bool,
{
    let source_md = source.metadata().unwrap();
    let source_is_symlink = source.is_symlink().unwrap_or(false);
    let source_file_type = source_md.file_type();
    let source_is_dir = source_file_type == ftw::FileType::Directory;

    let source_is_special_file = matches!(
        source_file_type,
        ftw::FileType::BlockDevice
            | ftw::FileType::CharacterDevice
            | ftw::FileType::Fifo
            | ftw::FileType::Socket
    );
    // -R is required for step 4
    if source_is_special_file && cfg.recursive {
        copy_special_file(
            cfg,
            source_md,
            target,
            target_dirfd,
            target_filename,
            created_files,
            source_file_type == ftw::FileType::Fifo,
        )?;
        return Ok(CopyResult::CopiedFile);
    }

    let source_deref_md = unsafe { ftw::Metadata::new(source.dir_fd(), source.file_name(), true) };

    let target_symlink_md = ftw::Metadata::new(
        target_dirfd,
        unsafe { CStr::from_ptr(target_filename) },
        false,
    );
    let target_deref_md = ftw::Metadata::new(
        target_dirfd,
        unsafe { CStr::from_ptr(target_filename) },
        true,
    );
    let target_is_dangling_symlink = target_symlink_md.is_ok() && target_deref_md.is_err();

    let target_symlink_md = match target_symlink_md {
        Ok(md) => Some(md),
        Err(e) => {
            if e.kind() == io::ErrorKind::NotFound {
                None
            } else {
                let err_str =
                    gettext!("cannot access '{}': {}", target.display(), error_string(&e));
                return Err(io::Error::other(err_str));
            }
        }
    };

    let target_is_dir = match &target_symlink_md {
        Some(md) => md.file_type() == ftw::FileType::Directory,
        None => false,
    };

    let target_exists = target_symlink_md.is_some();

    // 1. If source_file references the same file as dest_file
    if let (Ok(smd), Ok(tmd)) = (&source_deref_md, &target_deref_md) {
        if smd.dev() == tmd.dev() && smd.ino() == tmd.ino() {
            let err_str = gettext!(
                "'{}' and '{}' are the same file",
                source.path(),
                target.display()
            );
            return Err(io::Error::other(err_str));
        }
    }

    // 2. If source_file is of type directory
    if source_is_dir {
        // 2.a
        if !cfg.recursive {
            let err_str = gettext!("-r not specified; omitting directory '{}'", source.path());
            return Err(io::Error::other(err_str));
        }

        // 2.b `fs::read_dir` skips `.` and `..`. Any occurence means it comes
        // from the input to `cp`.

        // 2.d
        if target_exists && !target_is_dir {
            let err_str = gettext!(
                "cannot overwrite non-directory '{}' with directory '{}'",
                target.display(),
                source.path()
            );
            return Err(io::Error::other(err_str));
        }

        // 2.e
        if !target_exists {
            if target.starts_with(PathBuf::from(format!("{}", source.path()))) {
                let err_str = gettext!(
                    "cannot copy a directory, '{}', into itself, '{}'",
                    source.path(),
                    target.display()
                );
                return Err(io::Error::other(err_str));
            }

            unsafe {
                // Creates the target directory with the same file permission bits as the source,
                // modified by the umask of the process. Copying the permission bits without the
                // umask is postponed to the `postprocess_dir` closure on the call to
                // `traverse_directory` inside `copy_file`.
                let ret = libc::mkdirat(
                    target_dirfd,
                    target_filename,
                    // OR'ed with S_IRWXU according to the spec
                    source_md.mode() as libc::mode_t | libc::S_IRWXU,
                );

                if ret != 0 {
                    let e = io::Error::last_os_error();
                    let err_str = gettext!(
                        "cannot create directory '{}': {}",
                        target.display(),
                        error_string(&e)
                    );
                    return Err(io::Error::other(err_str));
                }
            }
        }

        return Ok(CopyResult::CopyingDirectory);
    } else {
        // 3. If source_file is of type regular file

        let create_target_then_copy = || -> io::Result<()> {
            let source_fd = unsafe {
                libc::openat(source.dir_fd(), source.file_name().as_ptr(), libc::O_RDONLY)
            };
            if source_fd == -1 {
                let e = io::Error::last_os_error();
                let err_str = gettext!(
                    "cannot open '{}' for reading: {}",
                    source.path(),
                    error_string(&e)
                );
                return Err(io::Error::other(err_str));
            }
            let mut source_file = unsafe { fs::File::from_raw_fd(source_fd) };

            // 3.b
            let target_fd = unsafe {
                libc::openat(
                    target_dirfd,
                    target_filename,
                    libc::O_WRONLY | libc::O_CREAT,
                    source_md.mode(),
                )
            };
            if target_fd == -1 {
                let e = io::Error::last_os_error();

                // `ErrorKind::IsADirectory` is unstable:
                // https://github.com/rust-lang/rust/issues/86442
                let err_msg = if let Some(libc::EISDIR) = e.raw_os_error() {
                    // EISDIR -> ENOTDIR is to match the diagnostic from
                    // coreutils/tests/cp/trailing-slash.sh
                    error_string(&io::Error::from_raw_os_error(libc::ENOTDIR))
                } else {
                    error_string(&e)
                };
                let err_str = gettext!(
                    "cannot create regular file '{}': {}",
                    target.display(),
                    err_msg
                );
                return Err(io::Error::other(err_str));
            }
            let mut target_file = unsafe { fs::File::from_raw_fd(target_fd) };

            // 3.d
            io::copy(&mut source_file, &mut target_file)?;

            Ok(())
        };

        // 3.a
        if target_exists && !target_is_dangling_symlink {
            if created_files.contains(target) {
                let err_str = gettext!(
                    "will not overwrite just-created '{}' with '{}'",
                    target.display(),
                    source.path(),
                );
                return Err(io::Error::other(err_str));
            }

            // 3.a.i
            let target_is_writable = target_symlink_md
                .as_ref()
                .map(|md| md.is_writable())
                .unwrap_or(false);

            // Different prompt if the target is not writable
            if !target_is_writable && (cfg.interactive || cfg.force) {
                let mode = target_symlink_md.as_ref().unwrap().mode();

                let mut mode_str = String::new();
                let bit_loc = 0o400;
                for i in 0..9 {
                    let mask = bit_loc >> i;
                    if mode & mask != 0 {
                        match i % 3 {
                            0 => mode_str.push('r'),
                            1 => mode_str.push('w'),
                            2 => mode_str.push('x'),
                            _ => (),
                        }
                    } else {
                        mode_str.push('-');
                    }
                }

                // 4 octal digits
                // This needs to be formatted separately because `gettext!` does
                // not accept a format spec (just plain curly braces, `{}`).
                let mode_octal = format!("{:04o}", mode & 0o7777);

                if cfg.force {
                    let is_affirm = prompt_fn(&gettext!(
                        "replace '{}', overriding mode {} ({})?",
                        target.display(),
                        mode_octal,
                        mode_str
                    ));
                    if !is_affirm {
                        return Ok(CopyResult::Skipped);
                    }
                } else if cfg.interactive {
                    let is_affirm = prompt_fn(&gettext!(
                        "unwritable '{}' (mode {}, {}); try anyway?",
                        target.display(),
                        mode_octal,
                        mode_str
                    ));
                    if !is_affirm {
                        return Ok(CopyResult::Skipped);
                    }
                }
            } else if cfg.interactive {
                let is_affirm = prompt_fn(&gettext!("overwrite '{}'?", target.display()));
                if !is_affirm {
                    return Ok(CopyResult::Skipped);
                }
            }

            // 4.c
            if source_is_symlink {
                let ret = unsafe {
                    libc::unlinkat(
                        target_dirfd,
                        target_filename,
                        if target_is_dir { libc::AT_REMOVEDIR } else { 0 },
                    )
                };
                if ret != 0 {
                    return Err(io::Error::last_os_error());
                }

                let ret = unsafe {
                    libc::symlinkat(
                        source.read_link().unwrap().as_ptr(),
                        target_dirfd,
                        target_filename,
                    )
                };
                if ret != 0 {
                    return Err(io::Error::last_os_error());
                }
            } else {
                // 3.a.ii
                let target_fd = unsafe {
                    libc::openat(
                        target_dirfd,
                        target_filename,
                        libc::O_WRONLY | libc::O_TRUNC,
                    )
                };
                if target_fd != -1 {
                    let mut target_file = unsafe { fs::File::from_raw_fd(target_fd) };

                    let source_fd = unsafe {
                        libc::openat(source.dir_fd(), source.file_name().as_ptr(), libc::O_RDONLY)
                    };
                    if source_fd == -1 {
                        let e = io::Error::last_os_error();
                        let err_str = gettext!(
                            "cannot open '{}' for reading: {}",
                            source.path(),
                            error_string(&e)
                        );
                        return Err(io::Error::other(err_str));
                    }
                    let mut source_file = unsafe { fs::File::from_raw_fd(source_fd) };

                    io::copy(&mut source_file, &mut target_file)?;
                } else {
                    // 3.a.iii
                    if cfg.force {
                        let ret = unsafe {
                            libc::unlinkat(
                                target_dirfd,
                                target_filename,
                                if target_is_dir { libc::AT_REMOVEDIR } else { 0 },
                            )
                        };
                        if ret != 0 {
                            return Err(io::Error::last_os_error());
                        }

                        // 3.b
                        create_target_then_copy()?;
                    } else {
                        let e = io::Error::last_os_error();
                        let err_str = gettext!(
                            "cannot open '{}' for reading: {}",
                            target.display(),
                            error_string(&e)
                        );
                        return Err(io::Error::other(err_str));
                    }
                }
            }

        // 3.b
        } else {
            // 4.c
            if source_is_symlink {
                let ret = unsafe {
                    libc::symlinkat(
                        source.read_link().unwrap().as_ptr(),
                        target_dirfd,
                        target_filename,
                    )
                };
                if ret != 0 {
                    return Err(io::Error::last_os_error());
                }
            } else {
                create_target_then_copy()?;
            }
        }

        created_files.insert(target.to_path_buf());
    }

    Ok(CopyResult::CopiedFile)
}

pub fn copy_file<F>(
    cfg: &CopyConfig,
    source_arg: &Path,
    target_arg: &Path,
    created_files: &mut HashSet<PathBuf>,
    mut inode_map: Option<&mut InodeMap>,
    prompt_fn: F,
) -> io::Result<()>
where
    F: Copy + Fn(&str) -> bool,
{
    // `RefCell` to allow sharing these between closures
    let target_dirfd_stack = RefCell::new(vec![ftw::FileDescriptor::cwd()]);
    let target_dir_path = RefCell::new(PathBuf::new());
    let terminate = RefCell::new(false);
    let last_error = RefCell::new(None);

    let _ = traverse_directory(
        source_arg,
        |source| {
            let mut terminate_borrowed = terminate.borrow_mut();
            let mut target_dirfd_stack_borrowed = target_dirfd_stack.borrow_mut();
            let mut target_dir_path_borrowed = target_dir_path.borrow_mut();

            if *terminate_borrowed {
                return Ok(false);
            }

            let target_dirfd = target_dirfd_stack_borrowed.last().unwrap();

            let target_filename = if target_dirfd.as_raw_fd() == libc::AT_FDCWD {
                target_arg.as_os_str()
            } else {
                OsStr::from_bytes(source.file_name().to_bytes())
            };

            let target = target_dir_path_borrowed.join(target_filename);
            let target_filename_cstr = CString::new(target_filename.as_bytes()).unwrap();

            let source_md = source.metadata().unwrap();
            let identifier = (source_md.dev(), source_md.ino());

            // Hard-link preserving behavior of `mv`. `cp` does not maintain the hard-link structure
            // of the hierarchy according to the standard
            if let Some(inode_map) = inode_map.as_deref_mut() {
                // Preserve hard links like coreutils mv. Creating a copy is also
                // allowed by the standard.
                if let Some((prev_dirfd, prev_filename)) = inode_map.get(&identifier) {
                    let ret = unsafe {
                        libc::linkat(
                            prev_dirfd.as_raw_fd(),
                            prev_filename.as_ptr(),
                            target_dirfd.as_raw_fd(),
                            target_filename_cstr.as_ptr(),
                            0, // Don't dereference prev if it's a symlink
                        )
                    };
                    // If success
                    if ret == 0 {
                        // Skip since this file/directory is handled by hard-linking
                        return Ok(false);
                    }
                    // else failed
                    else {
                        *last_error.borrow_mut() = Some(io::Error::last_os_error());
                        *terminate_borrowed = true;
                        return Ok(false);
                    }
                }
            }

            let continue_processing = match copy_file_impl(
                cfg,
                &source,
                &target,
                target_dirfd.as_raw_fd(),
                target_filename_cstr.as_ptr(),
                created_files,
                prompt_fn,
            ) {
                Ok(copy_result) => {
                    // If copying succeeds, then store the hard-link data
                    if let Some(inode_map) = inode_map.as_deref_mut() {
                        // Don't include every file, just those with hard links
                        if source_md.nlink() > 1 {
                            inode_map.insert(
                                identifier,
                                (target_dirfd.clone(), target_filename_cstr.clone()),
                            );
                        }
                    }

                    match copy_result {
                        CopyResult::CopyingDirectory => {
                            // mkdir/mkdirat doesn't return a file descriptor so a new one must be
                            // opened here. Using O_CREAT | O_DIRECTORY in a call to open/openat would
                            // not allow atomically creating a directory then opening it:
                            //
                            // https://stackoverflow.com/questions/45818628/whats-the-expected-behavior-of-openname-o-creato-directory-mode/48693137#48693137
                            let new_target_dirfd = match unsafe {
                                ftw::FileDescriptor::open_at(
                                    target_dirfd,
                                    &target_filename_cstr,
                                    libc::O_RDONLY,
                                )
                            } {
                                Ok(fd) => fd,
                                Err(e) => {
                                    let err_str = gettext!(
                                        "cannot open directory '{}': {}",
                                        target.display(),
                                        error_string(&e)
                                    );
                                    *last_error.borrow_mut() = Some(io::Error::other(err_str));
                                    *terminate_borrowed = true;
                                    return Ok(false);
                                }
                            };

                            target_dirfd_stack_borrowed.push(new_target_dirfd);
                            target_dir_path_borrowed.push(target_filename);

                            true
                        }
                        CopyResult::CopiedFile => {
                            // Immediately copy the metadata if copying a file. Directories are
                            // handled on the `postprocess_dir` closure below.
                            if cfg.preserve {
                                if let Err(e) = copy_characteristics(
                                    &source,
                                    &target,
                                    target_dirfd.as_raw_fd(),
                                    target_filename_cstr.as_ptr(),
                                ) {
                                    *last_error.borrow_mut() = Some(e);
                                    *terminate_borrowed = true;
                                    false
                                } else {
                                    true
                                }
                            } else {
                                true
                            }
                        }
                        CopyResult::Skipped => false,
                    }
                }
                Err(e) => {
                    *last_error.borrow_mut() = Some(e);
                    *terminate_borrowed = true;
                    false
                }
            };

            Ok(continue_processing)
        },
        |source| {
            let mut terminate_borrowed = terminate.borrow_mut();
            let mut target_dirfd_stack_borrowed = target_dirfd_stack.borrow_mut();
            let mut target_dir_path_borrowed = target_dir_path.borrow_mut();

            target_dir_path_borrowed.pop();
            target_dirfd_stack_borrowed.pop();

            // Preserve metadata for directories. Must do this inside this closure to ensure no
            // further last access time changes to the source will be made.
            if cfg.preserve {
                let target_dirfd = target_dirfd_stack_borrowed.last().unwrap();

                let target_filename = if target_dirfd.as_raw_fd() == libc::AT_FDCWD {
                    target_arg.as_os_str()
                } else {
                    OsStr::from_bytes(source.file_name().to_bytes())
                };
                let target_filename_cstr = CString::new(target_filename.as_bytes()).unwrap();

                if let Err(e) = copy_characteristics(
                    &source,
                    &target_dir_path_borrowed,
                    target_dirfd.as_raw_fd(),
                    target_filename_cstr.as_ptr(),
                ) {
                    *last_error.borrow_mut() = Some(e);
                    *terminate_borrowed = true;
                }
            }

            Ok(())
        },
        |_entry, error| {
            *last_error.borrow_mut() = Some(error.inner());
            *terminate.borrow_mut() = true;
        },
        ftw::TraverseDirectoryOpts {
            follow_symlinks_on_args: cfg.follow_cli,
            follow_symlinks: cfg.dereference,
            ..Default::default()
        },
    );

    match last_error.into_inner() {
        Some(e) => Err(e),
        None => Ok(()),
    }
}

pub fn copy_files<F>(
    cfg: &CopyConfig,
    sources: &[PathBuf],
    target: &Path,
    mut inode_map: Option<&mut InodeMap>,
    prompt_fn: F,
) -> Option<()>
where
    F: Copy + Fn(&str) -> bool,
{
    let mut result = Some(());

    let mut created_files = HashSet::new();

    // loop through sources, moving each to target
    for source in sources {
        // This doesn't seem to be compliant with POSIX
        let ends_with_slash_dot = |p: &Path| -> bool {
            let bytes = p.as_os_str().as_bytes();
            if bytes.len() >= 2 {
                let end = &bytes[(bytes.len() - 2)..];
                return end == b"/.";
            }
            false
        };

        let new_target = if source.is_dir() && ends_with_slash_dot(source) {
            // This causes the contents of `source` to be copied instead of
            // `source` itself
            target.to_path_buf()
        } else {
            match source.file_name() {
                Some(file_name) => target.join(file_name),
                None => {
                    let err_str = gettext!("invalid filename: {}", source.display());
                    eprintln!("cp: {}", err_str);
                    result = None;
                    continue;
                }
            }
        };

        match copy_file(
            cfg,
            source,
            &new_target,
            &mut created_files,
            inode_map.as_deref_mut(),
            prompt_fn,
        ) {
            Ok(_) => (),
            Err(e) => {
                eprintln!("cp: {}", error_string(&e));
                result = None;
            }
        }
    }

    result
}

fn copy_special_file(
    cfg: &CopyConfig,
    source_md: &ftw::Metadata,

    // Should only be used for keeping track of created files and for displaying error messages
    target: &Path,

    target_dirfd: libc::c_int,
    target_filename: *const libc::c_char,
    created_files: &mut HashSet<PathBuf>,
    is_fifo: bool,
) -> io::Result<()> {
    // 4.a
    let dev = source_md.rdev();

    // 4.b
    let mode = if is_fifo {
        // Mandatory to be the same as source for FIFO
        source_md.mode()
    } else {
        // Under Rationale:
        // "In general, it is strongly suggested that the permissions,
        // owner, and group be the same as if the user had run the
        // historical mknod, ln, or other utility to create the file"
        0o644
    };

    let mut stat_buf = MaybeUninit::uninit();

    // Using `fstatat` to check for the existence of the target file
    let ret = unsafe {
        libc::fstatat(
            target_dirfd,
            target_filename,
            stat_buf.as_mut_ptr(),
            libc::AT_SYMLINK_NOFOLLOW,
        )
    };
    let target_exists = ret == 0;

    if target_exists {
        let ret = unsafe { libc::unlinkat(target_dirfd, target_filename, 0) };
        if ret != 0 {
            return Err(io::Error::last_os_error());
        }
    }

    let ret = unsafe {
        libc::mknodat(
            target_dirfd,
            target_filename,
            mode as libc::mode_t,
            dev as libc::dev_t,
        )
    };
    if ret == 0 {
        created_files.insert(target.to_path_buf());
        Ok(())
    } else {
        let e = io::Error::last_os_error();
        let err_str = gettext!(
            "cannot create regular file '{}': {}",
            target.display(),
            error_string(&e)
        );
        Err(io::Error::other(err_str))
    }
}

// Copy the metadata in `source_md` to the target.
fn copy_characteristics(
    source: &ftw::Entry,
    target: &Path,
    target_dirfd: libc::c_int,
    target_filename: *const libc::c_char,
) -> io::Result<()> {
    // Get a new metadata instead because the source's last access time is updated on reads (i.e,
    // `io::copy`).
    // Should fix sporadic errors on `test_cp_preserve_slink_time` where `dangle` has a later
    // access time than `d2`.
    let source_md = unsafe { ftw::Metadata::new(source.dir_fd(), source.file_name(), false) }?;

    // [last_access_time, last_modified_time]
    let times = [
        libc::timespec {
            tv_sec: source_md.atime(),
            tv_nsec: source_md.atime_nsec(),
        },
        libc::timespec {
            tv_sec: source_md.mtime(),
            tv_nsec: source_md.mtime_nsec(),
        },
    ];

    unsafe {
        // Copy last access and last modified times
        let ret = libc::utimensat(
            target_dirfd,
            target_filename,
            times.as_ptr(),
            libc::AT_SYMLINK_NOFOLLOW, // Update the file itself if a symlink
        );
        if ret != 0 {
            let err_str = gettext!(
                "failed to preserve times for '{}': {}",
                target.display(),
                io::Error::last_os_error()
            );
            return Err(io::Error::other(err_str));
        }

        // Copy user and group
        let ret = libc::fchownat(
            target_dirfd,
            target_filename,
            source_md.uid(),
            source_md.gid(),
            libc::AT_SYMLINK_NOFOLLOW,
        );
        if ret != 0 {
            // Ignore errors
            errno::set_errno(errno::Errno(0));
        }

        // Copy permissions
        let ret = libc::fchmodat(
            target_dirfd,
            target_filename,
            source_md.mode() as libc::mode_t,
            libc::AT_SYMLINK_NOFOLLOW,
        );
        if ret != 0 {
            let fchmodat_error = io::Error::last_os_error();

            // Symbolic link permissions are ignored on Linux
            #[cfg(target_os = "linux")]
            if let Ok(md) = ftw::Metadata::new(target_dirfd, CStr::from_ptr(target_filename), false)
            {
                if md.file_type() == ftw::FileType::SymbolicLink {
                    if let Some(errno) = fchmodat_error.raw_os_error() {
                        if errno == libc::EOPNOTSUPP {
                            return Ok(());
                        }
                    }
                }
            }

            let err_str = gettext!(
                "failed to preserve permissions for '{}': {}",
                target.display(),
                io::Error::last_os_error()
            );
            return Err(io::Error::other(err_str));
        }
    }
    Ok(())
}
