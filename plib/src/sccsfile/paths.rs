//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Names of the files that go with an s-file, and operand expansion.

use std::ffi::{OsStr, OsString};
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::{Path, PathBuf};

/// What an s-file's name is made of: the bytes after its `s.` prefix, which
/// the names of the files that go with it share. `None` if `path` does not
/// name an s-file: no final component, no prefix, or nothing after it.
///
/// A file name is bytes. These helpers took it for text and treated a name
/// that was not UTF-8 as no s-file at all, or as one called `s.unknown`.
fn stem(path: &Path) -> Option<&[u8]> {
    path.file_name()?
        .as_bytes()
        .strip_prefix(b"s.")
        .filter(|stem| !stem.is_empty())
}

/// The file name `<letter>.<stem>` for the s-file `sfile`.
///
/// Callers have checked [`is_sfile`]; for a path that is not one, the whole
/// final component stands in for the stem, so that what comes back is
/// still a name of its own and never the s-file's.
fn aux_name(sfile: &Path, letter: u8) -> OsString {
    let stem = stem(sfile)
        .or_else(|| sfile.file_name().map(OsStr::as_bytes))
        .unwrap_or_default();
    OsString::from_vec([&[letter, b'.'], stem].concat())
}

/// Get the g-file (gotten file) path from an s-file path.
///
/// The g-file "shall be created in the current directory" (get,
/// 99186-99187), and "only the real user need have write permission in the
/// current directory" (99190) — so the name is the s-file's basename minus
/// the `s.` prefix, resolved against the caller's cwd, *not* against the
/// s-file's directory.
///
/// This previously derived a directory from the s-file (the parent of
/// `SCCS/`, else the s-file's own directory). For the usual
/// `cd project && get SCCS/s.foo` that is the same answer, which is why it
/// went unnoticed; but `get ../other/SCCS/s.foo` wrote the working copy
/// into `../other` instead of here, and `sccs -d dir -p SCCS get mod`
/// aimed it at the read-only s-file's own directory and failed with
/// EACCES.
///
/// `delta` and `unget` read and remove the g-file through this same
/// helper, so all three agree on where it lives.
pub fn gfile_from_sfile(sfile: &Path) -> Option<PathBuf> {
    stem(sfile).map(|stem| PathBuf::from(OsStr::from_bytes(stem)))
}

/// Get the s-file path from g-file path
/// foo -> SCCS/s.foo (if SCCS exists) or s.foo
pub fn sfile_from_gfile(gfile: &Path) -> PathBuf {
    let name = gfile.file_name().map(OsStr::as_bytes).unwrap_or_default();
    let sname = OsString::from_vec([b"s.", name].concat());

    // Check for SCCS subdirectory
    let parent = gfile.parent().unwrap_or(Path::new("."));
    let sccs_dir = parent.join("SCCS");
    if sccs_dir.is_dir() {
        sccs_dir.join(&sname)
    } else {
        parent.join(&sname)
    }
}

/// Get the p-file (lock file) path from s-file
/// s.foo -> p.foo (in same directory as s-file)
pub fn pfile_from_sfile(sfile: &Path) -> PathBuf {
    sfile.with_file_name(aux_name(sfile, b'p'))
}

/// Get the q-file (the working copy the p-file is rewritten through) path
/// from s-file: s.foo -> q.foo, beside the s-file.
pub fn qfile_from_sfile(sfile: &Path) -> PathBuf {
    sfile.with_file_name(aux_name(sfile, b'q'))
}

/// Get the z-file (lock semaphore) path from s-file
pub fn zfile_from_sfile(sfile: &Path) -> PathBuf {
    sfile.with_file_name(aux_name(sfile, b'z'))
}

/// Get the l-file (delta summary) path from an s-file.
///
/// Like the g-file, "the l-file shall be created in the current directory
/// if the -l option is used" (get, 99192-99194) — not beside the s-file.
pub fn lfile_from_sfile(sfile: &Path) -> PathBuf {
    PathBuf::from(aux_name(sfile, b'l'))
}

/// Get the x-file (temporary) path from s-file
pub fn xfile_from_sfile(sfile: &Path) -> PathBuf {
    sfile.with_file_name(aux_name(sfile, b'x'))
}

/// Check if a path is an SCCS s-file: its final component is `s.` and then
/// a name.
pub fn is_sfile(path: &Path) -> bool {
    stem(path).is_some()
}

/// Get module name from s-file path
/// SCCS/s.foo.c -> foo.c
pub fn module_name(sfile: &Path) -> Option<&[u8]> {
    stem(sfile)
}

/// Expand SCCS file operands per POSIX, returning the flat list of paths to
/// process in order:
/// - a lone `-` operand reads pathnames from standard input, one per line;
/// - a directory operand (named directly or via stdin) expands to its
///   `s.*` members (sorted; non-SCCS files silently ignored);
/// - every other operand passes through verbatim, so the caller can still
///   diagnose an explicitly named non-SCCS file.
///
/// A file the user may not read is dropped from a directory's members and
/// from a standard-input list: "non-SCCS files and unreadable files
/// shall be silently ignored" there. One named as an operand is kept, for
/// the caller to diagnose; so is one that does not exist, which `admin -n`
/// is about to create.
pub fn expand_operands(operands: &[PathBuf]) -> Vec<PathBuf> {
    use std::io::BufRead;

    /// Whether `p` exists but may not be read by this user.
    fn unreadable(p: &Path) -> bool {
        matches!(
            std::fs::File::open(p),
            Err(e) if e.kind() == std::io::ErrorKind::PermissionDenied
        )
    }

    fn push(p: PathBuf, out: &mut Vec<PathBuf>) {
        if p.is_dir() {
            if let Ok(entries) = std::fs::read_dir(&p) {
                let mut members: Vec<PathBuf> = entries
                    .flatten()
                    .map(|e| e.path())
                    .filter(|q| is_sfile(q) && !unreadable(q))
                    .collect();
                members.sort();
                out.extend(members);
            }
        } else {
            out.push(p);
        }
    }

    let mut out = Vec::new();
    if operands.len() == 1 && operands[0].as_os_str() == "-" {
        // "Non-SCCS files and unreadable files shall be silently ignored"
        // (get 99128, delta 92266, unget 119264, admin 84123). A list piped
        // from `ls` or `find` routinely names ordinary files, and
        // diagnosing them would both spam stderr and fail the exit status.
        let stdin = std::io::stdin();
        for line in stdin.lock().lines().map_while(Result::ok) {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            let path = PathBuf::from(trimmed);
            if path.is_dir() || (is_sfile(&path) && !unreadable(&path)) {
                push(path, &mut out);
            }
        }
    } else {
        for op in operands {
            push(op.clone(), &mut out);
        }
    }
    out
}
