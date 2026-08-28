//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Writing the files an SCCS command derives from an s-file.

use std::fs;
use std::io;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;

/// Write `data` to `path` with `mode`, replacing whatever is there.
///
/// `get` writes the g-file mode 0444, so on the next `get` of the same file
/// `File::create` reopens that read-only file for writing and fails with
/// EACCES — a second `get` of any file could never succeed, and neither could
/// a second `get -l`. Replacing rather than reopening is the fix, and a
/// `rename(2)` over the old file does it without needing write permission on
/// the file itself.
///
/// It must be a rename and not an unlink followed by a write. Unlinking first
/// destroys the old contents before the new ones exist, so a write that fails
/// part way — a full disk, a signal — leaves nothing at all. That is merely
/// inconvenient for a g-file, which `get` can regenerate, and data loss for
/// the p-file, which is the only record that an edit is outstanding. The
/// unlink also opens a window for another process to create something at the
/// path first.
pub fn write_replacing(path: &Path, data: &[u8], mode: u32) -> io::Result<()> {
    plib::io::write_atomic_mode(path, data, mode)
}

/// Write `serialized` to the x-file, apply `perms`, and atomically rename over
/// `path`. The x-file is registered for SIGINT cleanup for the duration of the
/// write+rename, and removed on error.
///
/// `admin` factored this out; `delta` and `rmdel` each kept an inline copy,
/// and the three disagreed about whether a failure to read the s-file's own
/// mode was fatal.
pub fn write_xfile_atomic(
    path: &Path,
    x_file: &Path,
    serialized: &[u8],
    perms: fs::Permissions,
) -> io::Result<()> {
    plib::sccsfile::register_cleanup(x_file);
    let res = (|| -> io::Result<()> {
        fs::write(x_file, serialized)?;
        fs::set_permissions(x_file, perms)?;
        fs::rename(x_file, path)?;
        Ok(())
    })();
    plib::sccsfile::unregister_cleanup(x_file);
    if res.is_err() {
        let _ = fs::remove_file(x_file);
    }
    res
}

/// The mode to give a rewritten s-file: whatever the original carried.
///
/// A missing or unreadable s-file mode is not a reason to abandon the write,
/// but it is a reason not to invent a permissive one, so it falls back to
/// read-only.
pub fn sfile_perms(path: &Path) -> fs::Permissions {
    fs::metadata(path)
        .map(|m| m.permissions())
        .unwrap_or_else(|_| fs::Permissions::from_mode(0o444))
}
