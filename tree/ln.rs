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
use std::ffi::{CString, OsString};
use std::io;
use std::os::unix::{ffi::OsStrExt, fs::MetadataExt};
use std::path::{Component, Path, PathBuf};

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

    #[arg(short, long, help = gettext("With -s, make each link's text relative to the link's directory"))]
    relative: bool,

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

/// Maximum number of symbolic links followed while resolving one path.
const MAX_SYMLINKS: usize = 40;

/// The absolute path `path` names, with every symbolic link that exists
/// resolved and `.` / `..` removed.  No component need exist: from the first
/// missing one on, the rest are taken as written (`realpath -m`).
fn canonicalize_missing(path: &Path) -> io::Result<PathBuf> {
    let abs = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()?.join(path)
    };

    // Steps still to take, the next one last.
    let mut pending: Vec<Step> = Vec::new();
    push_steps(&mut pending, &abs);

    let mut resolved = PathBuf::from("/");
    let mut links = 0;
    while let Some(step) = pending.pop() {
        match step {
            Step::Name(name) => {
                let candidate = resolved.join(name);
                match std::fs::symlink_metadata(&candidate) {
                    Ok(md) if md.file_type().is_symlink() => {
                        links += 1;
                        if links > MAX_SYMLINKS {
                            return Err(io::Error::from_raw_os_error(libc::ELOOP));
                        }
                        let target = std::fs::read_link(&candidate)?;
                        if target.is_absolute() {
                            resolved = PathBuf::from("/");
                        }
                        push_steps(&mut pending, &target);
                    }
                    // Not a link, or missing: keep the name as written.
                    _ => resolved = candidate,
                }
            }
            Step::Up => {
                resolved.pop();
            }
        }
    }
    Ok(resolved)
}

/// One step of a path walk: into a named entry, or up to the parent.
enum Step {
    Name(OsString),
    Up,
}

/// Push `path`'s steps onto `pending` so that the first is popped first.
/// The root and `.` are no steps at all.
fn push_steps(pending: &mut Vec<Step>, path: &Path) {
    let steps: Vec<Step> = path
        .components()
        .filter_map(|c| match c {
            Component::Normal(n) => Some(Step::Name(n.to_os_string())),
            Component::ParentDir => Some(Step::Up),
            _ => None,
        })
        .collect();
    pending.extend(steps.into_iter().rev());
}

/// -r: the text of a symbolic link at `dest` that names `source` (a path from
/// the current directory) relative to the directory holding `dest`.  Both are
/// resolved first, so `..` and symbolic links are taken into account.
fn relative_link_text(source: &Path, dest: &Path) -> io::Result<PathBuf> {
    let dest_dir = match dest.parent() {
        Some(p) if !p.as_os_str().is_empty() => p,
        _ => Path::new("."),
    };
    let from = canonicalize_missing(source)?;
    let base = canonicalize_missing(dest_dir)?;

    let from: Vec<Component> = from.components().collect();
    let base: Vec<Component> = base.components().collect();
    let common = from.iter().zip(&base).take_while(|(a, b)| a == b).count();

    let mut text = PathBuf::new();
    for _ in common..base.len() {
        text.push("..");
    }
    for comp in &from[common..] {
        text.push(comp);
    }
    if text.as_os_str().is_empty() {
        text.push(".");
    }
    Ok(text)
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

    // -r: the link text is the source's path relative to the link's directory.
    let link_text;
    let source = if args.relative {
        link_text = relative_link_text(source, dest)?;
        link_text.as_path()
    } else {
        source
    };

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
    plib::diag::init_locale("ln");

    let args = Args::parse();

    if args.files.is_empty() {
        eprintln!("ln: {}", gettext("a source operand is required"));
        std::process::exit(1);
    }
    if args.relative && !args.symlink {
        eprintln!("ln: {}", gettext("cannot do --relative without --symbolic"));
        std::process::exit(1);
    }

    // A single operand links into the current directory under its last
    // component, as `ln SOURCE .` would.
    let current_dir = PathBuf::from(".");
    let (sources, target) = if args.files.len() == 1 {
        (&args.files[..], &current_dir)
    } else {
        let (sources, target) = args.files.split_at(args.files.len() - 1);
        (sources, &target[0])
    };

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
