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

use std::path::{Path, PathBuf};

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
    let name = sfile.file_name()?.to_str()?;
    name.strip_prefix("s.").map(PathBuf::from)
}

/// Get the s-file path from g-file path
/// foo -> SCCS/s.foo (if SCCS exists) or s.foo
pub fn sfile_from_gfile(gfile: &Path) -> PathBuf {
    let name = gfile
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown");
    let sname = format!("s.{}", name);

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
    let name = sfile
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("s.unknown");
    let pname = format!("p.{}", &name[2..]);
    sfile.with_file_name(pname)
}

/// Get the q-file (the working copy the p-file is rewritten through) path
/// from s-file: s.foo -> q.foo, beside the s-file.
pub fn qfile_from_sfile(sfile: &Path) -> PathBuf {
    let name = sfile
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("s.unknown");
    let qname = format!("q.{}", &name[2..]);
    sfile.with_file_name(qname)
}

/// Get the z-file (lock semaphore) path from s-file
pub fn zfile_from_sfile(sfile: &Path) -> PathBuf {
    let name = sfile
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("s.unknown");
    let zname = format!("z.{}", &name[2..]);
    sfile.with_file_name(zname)
}

/// Get the l-file (delta summary) path from an s-file.
///
/// Like the g-file, "the l-file shall be created in the current directory
/// if the -l option is used" (get, 99192-99194) — not beside the s-file.
pub fn lfile_from_sfile(sfile: &Path) -> PathBuf {
    let name = sfile
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("s.unknown");
    PathBuf::from(format!("l.{}", &name[2..]))
}

/// Get the x-file (temporary) path from s-file
pub fn xfile_from_sfile(sfile: &Path) -> PathBuf {
    let name = sfile
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("s.unknown");
    let xname = format!("x.{}", &name[2..]);
    sfile.with_file_name(xname)
}

/// Check if a path is an SCCS s-file
pub fn is_sfile(path: &Path) -> bool {
    path.file_name()
        .and_then(|n| n.to_str())
        .map(|n| n.starts_with("s."))
        .unwrap_or(false)
}

/// Get module name from s-file path
/// SCCS/s.foo.c -> foo.c
pub fn module_name(sfile: &Path) -> Option<String> {
    let name = sfile.file_name()?.to_str()?;
    name.strip_prefix("s.").map(|s| s.to_string())
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
