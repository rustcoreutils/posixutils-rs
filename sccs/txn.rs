//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A change to an s-file or its p-file, made under the z-file lock.
//!
//! Every command that changes an s-file or a p-file does the same four
//! things: take the lock, read what is there, decide, write. Each utility
//! assembled them itself, and each got part of it wrong at some point: a
//! check made before the lock and trusted after it, a p-file rewritten by a
//! path of its own, an x-file opened through whatever was at its name.
//!
//! [`Txn`] is those four things in one type. Beginning one takes the lock,
//! before the s-file is opened, as SCCS does. Reading for a change and every
//! write go through it, and the writers underneath are private to this
//! library, so a utility cannot write an s-file or a p-file it has not locked,
//! nor decide from a file it read before it held the lock.

use std::fs;
use std::io;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

use plib::sccsfile::{PfileEntry, SccsFile, ZLock};

use crate::{pfile, sfio, zlock};

/// The mode of a new s-file: "new SCCS files shall be given read-only
/// permission mode" (POSIX admin 83996-83997).
const NEW_SFILE_MODE: u32 = 0o444;

/// An s-file held under its z-file lock, for the length of one change.
/// Dropping it releases the lock.
#[derive(Debug)]
pub struct Txn {
    sfile: PathBuf,
    _lock: ZLock,
}

/// Whether an error from [`Txn::begin`] means another command holds the lock.
pub fn is_locked(e: &io::Error) -> bool {
    zlock::is_held(e)
}

impl Txn {
    /// Take the lock on `sfile`. Nothing of the file is read yet.
    pub fn begin(sfile: &Path) -> io::Result<Txn> {
        Ok(Txn {
            sfile: sfile.to_path_buf(),
            _lock: zlock::acquire(sfile)?,
        })
    }

    /// The s-file this is a change to.
    pub fn sfile(&self) -> &Path {
        &self.sfile
    }

    /// Whether anything is at the s-file's name: a file, or a symlink even
    /// if it dangles.
    pub fn sfile_exists(&self) -> bool {
        fs::symlink_metadata(&self.sfile).is_ok()
    }

    /// Read the s-file as it is now, under the lock.
    pub fn read(&self) -> io::Result<SccsFile> {
        SccsFile::from_path(&self.sfile)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))
    }

    /// Replace the s-file with `sccs`, keeping the mode it has.
    pub fn write(&self, sccs: &SccsFile) -> io::Result<()> {
        let perms = fs::metadata(&self.sfile)?.permissions();
        self.put(sccs, perms)
    }

    /// Create the s-file as `sccs`, read-only. It is an error for anything
    /// to be at its name: the check is made here, holding the lock, so no
    /// other SCCS command can take the name before the file is in place.
    pub fn create(&self, sccs: &SccsFile) -> io::Result<()> {
        if self.sfile_exists() {
            return Err(io::Error::new(
                io::ErrorKind::AlreadyExists,
                gettextrs::gettext("file already exists"),
            ));
        }
        self.put(sccs, fs::Permissions::from_mode(NEW_SFILE_MODE))
    }

    fn put(&self, sccs: &SccsFile, perms: fs::Permissions) -> io::Result<()> {
        let x_file = plib::sccsfile::paths::xfile_from_sfile(&self.sfile);
        sfio::write_xfile_atomic(&self.sfile, &x_file, &sccs.to_bytes(), perms)
    }

    /// The edits pending on the s-file: the entries of its p-file, none if
    /// it has no p-file.
    pub fn pending(&self) -> io::Result<Vec<PfileEntry>> {
        pfile::read(&self.sfile)
    }

    /// Replace the p-file with `entries`; with none, remove it.
    pub fn set_pending(&self, entries: &[PfileEntry]) -> io::Result<()> {
        pfile::write(&self.sfile, entries)
    }
}
