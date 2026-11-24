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

/// Replace `path` with `data`, through the temporary file `temp` beside it,
/// leaving it with mode `mode`.
///
/// The temporary is always a new file (`plib::io::write_atomic_named`):
/// whatever is at its name -- one left by a crash, or a symlink planted there
/// -- is removed, never opened. It is registered for SIGINT cleanup for the
/// length of the write, so an interrupt does not leave it behind. The caller
/// holds the z-file lock, which makes the name its own.
pub(crate) fn write_via(path: &Path, temp: &Path, data: &[u8], mode: u32) -> io::Result<()> {
    debug_assert_eq!(
        temp.parent(),
        path.parent(),
        "the temporary is beside the file"
    );
    let temp_name = temp
        .file_name()
        .ok_or_else(|| io::Error::from(io::ErrorKind::InvalidInput))?;
    plib::sccsfile::register_cleanup(temp);
    let res = plib::io::write_atomic_named(path, temp_name, data, mode);
    plib::sccsfile::unregister_cleanup(temp);
    res
}

/// Write `serialized` to the x-file and rename it over the s-file `path`,
/// which ends with the mode `perms`.
///
/// The x-file was opened with `fs::write`, which follows a symlink and
/// truncates what it points at, and then chmodded by name, which follows it
/// too: a symlink planted at `x.<name>` by anyone able to write the directory
/// had the next admin, delta or rmdel overwrite its target with the s-file
/// and make it read-only.
pub(crate) fn write_xfile_atomic(
    path: &Path,
    x_file: &Path,
    serialized: &[u8],
    perms: fs::Permissions,
) -> io::Result<()> {
    write_via(path, x_file, serialized, perms.mode() & 0o7777)
}
