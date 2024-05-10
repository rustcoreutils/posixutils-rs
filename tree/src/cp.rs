//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate atty;
extern crate clap;
extern crate plib;

mod common;

use self::common::{copy_characteristics, error_string, is_file_writable};
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, textdomain};
use plib::PROJECT_NAME;
use std::collections::HashSet;
use std::ffi::CString;
use std::os::unix::{
    self,
    ffi::OsStrExt,
    fs::{DirBuilderExt, FileTypeExt, MetadataExt, OpenOptionsExt},
};
use std::path::{Path, PathBuf};
use std::{fs, io};

/// cp - copy files
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Do not prompt for confirmation if the destination path exists
    #[arg(short, long)]
    force: bool,

    /// Follow command line symlinks
    #[arg(
        short = 'H',
        long,
        overrides_with_all = [
            "follow_cli",
            "dereference",
            "no_dereference"
        ],
        requires = "recursive"
    )]
    follow_cli: bool,

    /// Follow symlinks in source
    #[arg(
        short = 'L',
        long,
        overrides_with_all = [
            "follow_cli",
            "dereference",
            "no_dereference"
        ],
        requires = "recursive",
        default_value_t = true,
    )]
    dereference: bool,

    /// Never follow symlinks in source
    #[arg(
        short = 'P',
        long,
        overrides_with_all = [
            "follow_cli",
            "dereference",
            "no_dereference"
        ]
    )]
    no_dereference: bool,

    /// Prompt for confirmation if the destination path exists.
    #[arg(short, long)]
    interactive: bool,

    /// Duplicate the characteristics of each source file in the corresponding destination file.
    #[arg(short, long)]
    preserve: bool,

    /// Remove file hierarchies.
    #[arg(short = 'R', visible_short_alias = 'r', long)]
    recursive: bool,

    /// Source(s) and target of move(s)
    files: Vec<PathBuf>,
}

struct Config {
    force: bool,
    follow_cli: bool,
    dereference: bool,
    interactive: bool,
    preserve: bool,
    recursive: bool,
}

impl Config {
    fn new(args: &Args) -> Self {
        // `args.no_dereference` serves only to disable `args.dereference` or
        // `follow_cli`
        Config {
            force: args.force,
            follow_cli: args.follow_cli,
            dereference: args.dereference,
            interactive: args.interactive,
            preserve: args.preserve,
            recursive: args.recursive,
        }
    }
}

fn prompt_user(prompt: &str) -> bool {
    eprint!("cp: {} ", prompt);
    let mut response = String::new();
    io::stdin().read_line(&mut response).unwrap();
    response.to_lowercase().starts_with('y')
}

fn copy_special_file(
    cfg: &Config,
    source_md: &fs::Metadata,
    target: &Path,
    created_files: &mut HashSet<PathBuf>,
    is_fifo: bool,
) -> io::Result<()> {
    let target_cstr = CString::new(target.as_os_str().as_bytes())?;

    // 4.a
    let dev = source_md.rdev() as libc::dev_t;

    // 4.b
    let mode = if is_fifo {
        // Mandatory to be the same as source for FIFO
        source_md.mode() as libc::mode_t
    } else {
        // Under Rationale:
        // "In general, it is strongly suggested that the permissions,
        // owner, and group be the same as if the user had run the
        // historical mknod, ln, or other utility to create the file"""
        0o644
    };

    if target.exists() {
        fs::remove_file(target)?;
    }

    let ret = unsafe { libc::mknod(target_cstr.as_ptr(), mode, dev) };
    if ret == 0 {
        if cfg.preserve {
            let _ = copy_characteristics(&source_md, target);
        }
        created_files.insert(target.to_path_buf());
        return Ok(());
    } else {
        let e = io::Error::last_os_error();
        let err_str = gettext!(
            "cannot create regular file '{}': {}",
            target.display(),
            error_string(&e)
        );
        return Err(io::Error::other(err_str));
    }
}

fn copy_file(
    cfg: &Config,
    source: &Path,
    target: &Path,
    created_files: &mut HashSet<PathBuf>,
    init_call: bool,
) -> io::Result<()> {
    let err_cannot_stat_source = |e: &io::Error| -> io::Error {
        let err_str = gettext!("cannot stat '{}': {}", source.display(), error_string(&e));
        io::Error::other(err_str)
    };

    let source_symlink_md = fs::symlink_metadata(source);
    // `as_ref` to allow the reuse the `err_cannot_stat_source` closure
    let source_symlink_md = source_symlink_md.as_ref().map_err(err_cannot_stat_source)?;

    let source_file_type = source_symlink_md.file_type();
    let source_is_special_file = source_file_type.is_block_device()
        || source_file_type.is_char_device()
        || source_file_type.is_fifo()
        || source_file_type.is_socket();

    // -R is required for step 4
    if source_is_special_file && cfg.recursive {
        return copy_special_file(
            cfg,
            &source_symlink_md,
            target,
            created_files,
            source_file_type.is_fifo(),
        );
    }

    let source_deref_md = fs::metadata(source);
    let source_is_dangling_symlink = source_deref_md.is_err();

    let (source_is_dir, source_md) =
        if (cfg.dereference || (cfg.follow_cli && init_call)) && !source_is_dangling_symlink {
            let md = source_deref_md.as_ref().map_err(err_cannot_stat_source)?;
            // Metadata of the file that `source` points to
            (md.is_dir(), md)
        } else {
            // Metadata of `source` itself
            (source_symlink_md.is_dir(), source_symlink_md)
        };

    let target_deref_md = fs::metadata(target);
    let target_symlink_md = fs::symlink_metadata(target);
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
        Some(md) => md.is_dir(),
        None => false,
    };

    let target_exists = target_symlink_md.is_some();

    // 1. If source_file references the same file as dest_file
    if let (Ok(smd), Ok(tmd)) = (&source_deref_md, &target_deref_md) {
        if smd.dev() == tmd.dev() && smd.ino() == tmd.ino() {
            let err_str = gettext!(
                "'{}' and '{}' are the same file",
                source.display(),
                target.display()
            );
            return Err(io::Error::other(err_str));
        }
    }

    // 2. If source_file is of type directory
    if source_is_dir {
        // 2.a
        if !cfg.recursive {
            let err_str = gettext!(
                "-r not specified; omitting directory '{}'",
                source.display()
            );
            return Err(io::Error::other(err_str));
        }

        // 2.b `fs::read_dir` skips `.` and `..`. Any occurence means it comes
        // from the input to `cp`.

        // 2.d
        if target_exists && !target_is_dir {
            let err_str = gettext!(
                "cannot overwrite non-directory '{}' with directory '{}'",
                target.display(),
                source.display()
            );
            return Err(io::Error::other(err_str));
        }

        // 2.e
        if !target_exists {
            if target.starts_with(source) {
                let err_str = gettext!(
                    "cannot copy a directory, '{}', into itself, '{}'",
                    source.display(),
                    target.display()
                );
                return Err(io::Error::other(err_str));
            }

            fs::DirBuilder::new()
                .recursive(true)
                .mode(source_md.mode())
                .create(target)?;

            if cfg.preserve {
                let _ = copy_characteristics(&source_md, target);
            }
        }

        // 2.f
        for entry in fs::read_dir(source)? {
            let entry = entry?;

            let new_source = entry.path();
            let new_target = target.join(entry.file_name());

            copy_file(cfg, &new_source, &new_target, created_files, false)?;
        }
    } else {
        // 3. If source_file is of type regular file

        let create_target_then_copy = || -> io::Result<()> {
            let mut source_file = fs::File::open(source).map_err(|e| {
                let err_str = gettext!(
                    "cannot open '{}' for reading: {}",
                    source.display(),
                    error_string(&e)
                );
                io::Error::other(err_str)
            })?;

            // 3.b
            let mut target_file = fs::OpenOptions::new()
                .write(true)
                .create(true)
                .mode(source_md.mode())
                .open(target)
                .map_err(|e| {
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
                    io::Error::other(err_str)
                })?;

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
                    source.display(),
                );
                return Err(io::Error::other(err_str));
            }

            // 3.a.i
            let target_is_writable = is_file_writable(&target_symlink_md);
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
                    let is_affirm = prompt_user(&gettext!(
                        "replace '{}', overriding mode {} ({})?",
                        target.display(),
                        mode_octal,
                        mode_str
                    ));
                    if !is_affirm {
                        return Ok(());
                    }
                } else if cfg.interactive {
                    let is_affirm = prompt_user(&gettext!(
                        "unwritable '{}' (mode {}, {}); try anyway?",
                        target.display(),
                        mode_octal,
                        mode_str
                    ));
                    if !is_affirm {
                        return Ok(());
                    }
                }
            } else {
                if cfg.interactive {
                    let is_affirm = prompt_user(&gettext!("overwrite '{}'?", target.display()));
                    if !is_affirm {
                        return Ok(());
                    }
                }
            }

            // 4.c
            if source.is_symlink() {
                fs::remove_file(target)?;
                let source_ref = fs::read_link(source)?;
                unix::fs::symlink(source_ref, target)?;
            } else {
                // 3.a.ii
                match fs::OpenOptions::new()
                    .write(true)
                    .truncate(true)
                    .open(target)
                {
                    Ok(mut target_file) => {
                        let mut source_file = fs::File::open(source).map_err(|e| {
                            let err_str = gettext!(
                                "cannot open '{}' for reading: {}",
                                source.display(),
                                error_string(&e)
                            );
                            io::Error::other(err_str)
                        })?;
                        io::copy(&mut source_file, &mut target_file)?;
                    }
                    Err(e) => {
                        // 3.a.iii
                        if cfg.force {
                            // This is equivalent to `libc::unlink`
                            fs::remove_file(target)?;

                            // 3.b
                            create_target_then_copy()?;
                        } else {
                            let err_str = gettext!(
                                "cannot open '{}' for reading: {}",
                                target.display(),
                                error_string(&e)
                            );
                            return Err(io::Error::other(err_str));
                        }
                    }
                }
            }

        // 3.b
        } else {
            // 4.c
            if source.is_symlink() {
                let source_ref = fs::read_link(source)?;
                unix::fs::symlink(source_ref, target)?;
            } else {
                create_target_then_copy()?;
            }
        }

        if cfg.preserve {
            let _ = copy_characteristics(&source_md, target);
        }
        created_files.insert(target.to_path_buf());
    }

    Ok(())
}

fn copy_files(cfg: &Config, sources: &[PathBuf], target: &Path) -> Option<()> {
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

        match copy_file(cfg, source, &new_target, &mut created_files, true) {
            Ok(_) => (),
            Err(e) => {
                eprintln!("cp: {}", error_string(&e));
                result = None;
            }
        }
    }

    result
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    // Initialize translation system
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    if args.files.len() < 2 {
        eprintln!("{}", gettext("Must supply a source and target for copy"));
        std::process::exit(1);
    }

    // split sources and target
    let sources = &args.files[0..args.files.len() - 1];
    let target = &args.files[args.files.len() - 1];

    // choose mode based on whether target is a directory
    let dir_exists = {
        match fs::metadata(target) {
            Ok(md) => md.is_dir(),
            Err(e) => {
                if e.kind() == io::ErrorKind::NotFound {
                    false
                } else {
                    let err_str =
                        gettext!("cannot stat '{}': {}", target.display(), error_string(&e));
                    eprintln!("cp: {}", err_str);
                    std::process::exit(1);
                }
            }
        }
    };

    let cfg = Config::new(&args);
    if dir_exists {
        match copy_files(&cfg, sources, target) {
            Some(_) => Ok(()),
            None => std::process::exit(1),
        }
    } else {
        let mut created_files = HashSet::new();
        match copy_file(&cfg, &sources[0], target, &mut created_files, true) {
            Ok(_) => Ok(()),
            Err(e) => {
                eprintln!("cp: {}", error_string(&e));
                std::process::exit(1);
            }
        }
    }
}
