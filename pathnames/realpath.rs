//
// Copyright (c) 2024-2026 Ian McLinden
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;
use std::io::{self, Write};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};

use clap::Parser;
use gettextrs::gettext;
use plib::diag;

/// Maximum number of symbolic links to follow before declaring a loop.
const MAX_SYMLINKS: usize = 40;

/// realpath - return resolved canonical path
#[derive(Parser)]
#[command(version, about = gettext("realpath - return resolved canonical path"))]
struct Args {
    #[arg(short = 'e', overrides_with = "_canonicalize_missing", help = gettext("Error if the path cannot be resolved"))]
    canonicalize_existing: bool,

    #[arg(short = 'E', overrides_with = "canonicalize_existing", help = gettext("Do not error if the path cannot be resolved (default)"))]
    _canonicalize_missing: bool,

    #[arg(short, long, help = gettext("Don't print errors when paths cannot be resolved"))]
    quiet: bool,

    #[arg(value_name = "PATH", default_value = ".", help = gettext("Paths to resolve"))]
    paths: Vec<PathBuf>,
}

/// Canonicalize `path`, resolving every symbolic-link component, but tolerating
/// a missing final component (the `-E` / default-mode behavior). The path
/// prefix must still resolve to an existing directory.
fn resolve_missing_ok(path: &Path) -> io::Result<PathBuf> {
    let abs = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()?.join(path)
    };
    let resolved = resolve_inner(&abs, 0)?;

    // A trailing <slash> (following a non-slash) requires the path to name a
    // directory. Linux's realpath(3) reports ENOTDIR for a non-directory with a
    // trailing slash, but macOS silently strips it and succeeds; enforce the
    // POSIX behavior consistently so `realpath -E regfile/` fails on both.
    let bytes = path.as_os_str().as_bytes();
    let trailing_slash = bytes.len() > 1 && bytes.ends_with(b"/");
    if trailing_slash && resolved.exists() && !resolved.is_dir() {
        return Err(io::Error::from_raw_os_error(libc::ENOTDIR));
    }

    Ok(resolved)
}

fn resolve_inner(abs: &Path, depth: usize) -> io::Result<PathBuf> {
    if depth > MAX_SYMLINKS {
        return Err(io::Error::from_raw_os_error(libc::ELOOP));
    }

    match fs::canonicalize(abs) {
        Ok(p) => Ok(p),
        // The final component (or the target of a final symbolic link) does not
        // exist. Any other error (e.g. ENOTDIR on a trailing slash) propagates.
        Err(e) if e.kind() == io::ErrorKind::NotFound => {
            let parent = abs.parent().ok_or_else(|| clone_err(&e))?;
            let file = abs.file_name().ok_or_else(|| clone_err(&e))?;

            // The path prefix must resolve to an existing directory.
            let cparent = fs::canonicalize(parent)?;
            let candidate = cparent.join(file);

            match fs::symlink_metadata(&candidate) {
                Ok(md) if md.file_type().is_symlink() => {
                    let target = fs::read_link(&candidate)?;
                    let target_abs = if target.is_absolute() {
                        target
                    } else {
                        cparent.join(target)
                    };
                    resolve_inner(&target_abs, depth + 1)
                }
                // Missing final component, or a non-symlink final component:
                // the canonicalized prefix plus the final component is the answer.
                _ => Ok(candidate),
            }
        }
        Err(e) => Err(e),
    }
}

fn clone_err(e: &io::Error) -> io::Error {
    match e.raw_os_error() {
        Some(code) => io::Error::from_raw_os_error(code),
        None => io::Error::new(e.kind(), e.to_string()),
    }
}

fn write_path(path: &Path) -> bool {
    let bytes = path.as_os_str().as_bytes();

    // FUTURE DIRECTIONS: treat an embedded <newline> in the output as an error.
    if bytes.contains(&b'\n') {
        diag::error(&gettext("result contains a newline character"));
        return false;
    }

    let mut out = io::stdout().lock();
    let _ = out.write_all(bytes);
    let _ = out.write_all(b"\n");
    true
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    diag::init_locale("realpath");

    let args = Args::parse();

    let mut had_error = false;

    for path in &args.paths {
        let ret = if args.canonicalize_existing {
            fs::canonicalize(path)
        } else {
            resolve_missing_ok(path)
        };

        match ret {
            Ok(p) => {
                if !write_path(&p) {
                    had_error = true;
                }
            }
            Err(e) => {
                had_error = true;
                if !args.quiet {
                    diag::error(&format!("{}: {}", path.to_string_lossy(), e));
                }
            }
        }
    }

    std::process::exit(if had_error { 1 } else { 0 });
}
