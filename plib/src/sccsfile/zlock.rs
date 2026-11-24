//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The z-file lock held across a command that changes an s-file.

use std::io;
use std::path::Path;

use super::cleanup::SigintBlock;
use super::{paths, register_cleanup, unregister_cleanup};

/// RAII guard for the SCCS z-file lock.
///
/// SCCS commands that mutate an s-file create a transient read-only z-file
/// (e.g. `z.foo` for `s.foo`) containing the locking process's PID. It is held
/// only for the duration of a single command invocation and removed when the
/// guard is dropped, preventing two commands from modifying the same s-file
/// concurrently. (This is distinct from the long-lived p-file edit lock.)
///
/// The z-file is created with `O_EXCL` semantics
/// (`OpenOptions::create_new(true)`): if another process already holds the
/// lock, [`ZLock::acquire`] fails with [`io::ErrorKind::AlreadyExists`] and the
/// caller should diagnose the s-file as being edited / locked and skip it. The
/// z-file path is registered with the SIGINT cleanup registry so an interrupt
/// removes it.
pub struct ZLock {
    zfile: std::path::PathBuf,
}

impl ZLock {
    /// Acquire the z-file lock for `sfile`. On success the caller holds the
    /// lock until the returned guard is dropped. Fails with
    /// [`io::ErrorKind::AlreadyExists`] if another process already holds it.
    pub fn acquire(sfile: &Path) -> io::Result<ZLock> {
        use std::io::Write as _;
        use std::os::unix::fs::PermissionsExt;

        let zfile = paths::zfile_from_sfile(sfile);

        // Create and register as one unit, with SIGINT deferred across it.
        // Registering afterwards left a window -- a write, a flush and a chmod
        // wide -- in which the lock existed on disk but the handler did not
        // know to remove it. An interrupt landing there stranded z.<name>, and
        // every later `get -e` / `delta` on that file failed with "being
        // edited" until somebody deleted it by hand: exactly the state the
        // registry exists to prevent.
        let blocked = SigintBlock::new();
        let mut f = std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&zfile)?;
        register_cleanup(&zfile);
        drop(blocked);

        // The lock is registered from here on, so an interrupt removes it --
        // but a *failure* has to do the same by hand, or the file it just
        // created outlives the process that failed to fill it in.
        let filled = (|| -> io::Result<()> {
            // Record the locking PID, matching historical SCCS / CSSC.
            writeln!(f, "{}", std::process::id())?;
            f.flush()?;
            // SCCS z-files are read-only (r--r--r--). Set on the descriptor,
            // not the name, so it is the file just made that changes.
            f.set_permissions(std::fs::Permissions::from_mode(0o444))
        })();
        if let Err(e) = filled {
            unregister_cleanup(&zfile);
            let _ = std::fs::remove_file(&zfile);
            return Err(e);
        }

        Ok(ZLock { zfile })
    }
}

impl Drop for ZLock {
    fn drop(&mut self) {
        unregister_cleanup(&self.zfile);
        let _ = std::fs::remove_file(&self.zfile);
    }
}
