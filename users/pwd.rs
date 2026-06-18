//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::gettext;
use std::ffi::{OsStr, OsString};
use std::io::{self, Write};
use std::os::unix::ffi::OsStrExt;
use std::path::{Component, Path};
use std::process::ExitCode;

const PWD_ENV: &str = "PWD";

/// pwd - return working directory name
#[derive(Parser)]
#[command(version, about = gettext("pwd - return working directory name"))]
struct Args {
    /// -L: treat the working directory logically (honor a valid $PWD). Default.
    #[arg(short = 'L', overrides_with = "process", help = gettext("Treat the working directory logically; use $PWD if valid"))]
    env: bool,

    /// -P: resolve the working directory physically (no symbolic links).
    #[arg(short = 'P', overrides_with = "env", help = gettext("Resolve the working directory physically, without symbolic links"))]
    process: bool,
}

/// A `$PWD` value is usable under `-L` only if it is an absolute pathname with
/// no `.` or `..` components (POSIX pwd, step 2a/2b of the `-L` algorithm).
fn dirname_valid(name: &OsStr) -> bool {
    let path = Path::new(name);

    let mut first = true;
    for component in path.components() {
        if first {
            first = false;

            if component != Component::RootDir {
                return false;
            }
        } else if component == Component::CurDir || component == Component::ParentDir {
            return false;
        }
    }

    true
}

/// Resolve the working directory to print.
///
/// `-P` (or when `-P` is the last of `-L`/`-P`) always uses the physical path
/// from `getcwd(3)` (`current_dir`), which contains no symbolic-link components
/// on Linux/macOS. Logical mode (`-L`, the default) prefers a valid `$PWD`,
/// falling back to the physical path when `$PWD` is unset, syntactically
/// unusable, or longer than `{PATH_MAX}` (behavior is unspecified past that
/// bound, so we fall back rather than emit a too-long name).
fn resolve_cwd(physical: bool) -> io::Result<OsString> {
    let getcwd = std::env::current_dir()?.into_os_string();
    if physical {
        return Ok(getcwd);
    }

    if let Some(dir) = std::env::var_os(PWD_ENV) {
        if dir.as_bytes().len() < libc::PATH_MAX as usize && dirname_valid(&dir) {
            return Ok(dir);
        }
    }

    Ok(getcwd)
}

fn main() -> ExitCode {
    plib::diag::init_locale("pwd");

    let args = Args::parse();

    // `overrides_with` makes the last of -L/-P win; neither given => logical.
    let cwd = match resolve_cwd(args.process) {
        Ok(c) => c,
        Err(e) => {
            plib::diag::error(&format!(
                "{}: {}",
                gettext("cannot determine current directory"),
                e
            ));
            return ExitCode::from(1);
        }
    };

    // Emit the pathname byte-faithfully (paths are not guaranteed UTF-8).
    let mut out = io::stdout().lock();
    if out
        .write_all(cwd.as_bytes())
        .and_then(|_| out.write_all(b"\n"))
        .is_err()
    {
        return ExitCode::from(1);
    }

    ExitCode::SUCCESS
}
