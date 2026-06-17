//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::ffi::CString;
use std::io;
use std::os::unix::{ffi::OsStrExt, fs::MetadataExt};
use std::path::{Path, PathBuf};

/// ln - link files
#[derive(Parser)]
#[command(version, about = gettext("ln - link files"))]
struct Args {
    #[arg(short, long, help = gettext("Force existing destination pathnames to be removed to allow the link"))]
    force: bool,

    #[arg(short, long, help = gettext("Create symbolic links instead of hard links"))]
    symlink: bool,

    #[arg(short = 'L', overrides_with = "physical",
          help = gettext("For a symbolic-link source, hard-link the file it refers to"))]
    logical: bool,

    #[arg(short = 'P', overrides_with = "logical",
          help = gettext("For a symbolic-link source, hard-link the symbolic link itself"))]
    physical: bool,

    // `PathBuf` (not `String`) so non-UTF-8 and odd names are handled without panicking.
    #[arg(help = gettext("Source(s) and target of link(s)"))]
    files: Vec<PathBuf>,
}

// Build a NUL-terminated path for libc, rejecting a <newline> (FUTURE DIRECTIONS, #LN6).
fn path_cstring(p: &Path) -> io::Result<CString> {
    let bytes = p.as_os_str().as_bytes();
    if bytes.contains(&b'\n') {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            gettext("pathname contains a <newline> character"),
        ));
    }
    CString::new(bytes).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))
}

fn make_link(args: &Args, source: &Path, dest: &Path) -> io::Result<()> {
    // -f: remove an existing destination first, but never when it is the same file as the source —
    // that would destroy the only copy (`ln a a`, or hard links to the same file).
    if args.force {
        if let Ok(dest_md) = std::fs::symlink_metadata(dest) {
            // With -L (and not -s) the referent of a symbolic-link source is hard-linked, so compare
            // the referent — not the link itself — against the destination. Otherwise `ln -f -L`
            // could unlink the only copy of the destination before linking.
            let src_md = if !args.symlink && args.logical {
                std::fs::metadata(source)
            } else {
                std::fs::symlink_metadata(source)
            };
            if let Ok(src_md) = src_md {
                if src_md.dev() == dest_md.dev() && src_md.ino() == dest_md.ino() {
                    return Err(io::Error::other(gettext!(
                        "'{}' and '{}' are the same file",
                        source.display(),
                        dest.display()
                    )));
                }
            }
            // Remove a non-directory destination; a directory is left for the link call to reject.
            if !dest_md.is_dir() {
                let c = path_cstring(dest)?;
                unsafe { libc::unlink(c.as_ptr()) };
            }
        }
    }

    let src_c = path_cstring(source)?;
    let dest_c = path_cstring(dest)?;

    let ret = if args.symlink {
        // -L/-P are ignored with -s.
        unsafe { libc::symlink(src_c.as_ptr(), dest_c.as_ptr()) }
    } else {
        // -L follows a symbolic-link source; -P (and the default) link the link itself.
        let flag = if args.logical {
            libc::AT_SYMLINK_FOLLOW
        } else {
            0
        };
        unsafe {
            libc::linkat(
                libc::AT_FDCWD,
                src_c.as_ptr(),
                libc::AT_FDCWD,
                dest_c.as_ptr(),
                flag,
            )
        }
    };
    if ret != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

fn report(source: &Path, dest: &Path, e: &io::Error) {
    eprintln!(
        "ln: {}",
        gettext!("'{}' -> '{}': {}", dest.display(), source.display(), e)
    );
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    if args.files.len() < 2 {
        eprintln!(
            "ln: {}",
            gettext("a source and a target operand are required")
        );
        std::process::exit(1);
    }

    let (sources, target) = args.files.split_at(args.files.len() - 1);
    let target = &target[0];

    // POSIX: the target-directory form is used when the final operand names an existing directory
    // (or a symbolic link referring to one); otherwise the two-operand form.
    let target_is_dir = std::fs::metadata(target)
        .map(|m| m.is_dir())
        .unwrap_or(false);

    let mut exit_code = 0;

    if target_is_dir {
        for source in sources {
            let dest = match source.file_name() {
                Some(name) => target.join(name),
                None => {
                    eprintln!(
                        "ln: {}",
                        gettext!("invalid source operand: '{}'", source.display())
                    );
                    exit_code = 1;
                    continue;
                }
            };
            if let Err(e) = make_link(&args, source, &dest) {
                report(source, &dest, &e);
                exit_code = 1;
            }
        }
    } else if sources.len() == 1 {
        let source = &sources[0];
        if let Err(e) = make_link(&args, source, target) {
            report(source, target, &e);
            exit_code = 1;
        }
    } else {
        // More than two operands but the final one is not a directory.
        eprintln!(
            "ln: {}",
            gettext!("target '{}' is not a directory", target.display())
        );
        std::process::exit(1);
    }

    std::process::exit(exit_code)
}
