//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Acquiring the z-file lock.
//!
//! `ZLock::acquire` lives in `plib`; what was duplicated is the diagnosis of
//! the one error that matters. Three utilities each wrote the same
//! `AlreadyExists` match arm, `admin` wrote a fourth variant, and the message
//! differed between them. Worse, `get` acquired the lock and registered it for
//! cleanup but never installed the SIGINT handler, leaving the registry inert
//! and stranding `z.<name>` on ^C.

use std::io;
use std::path::Path;

use gettextrs::gettext;
use plib::sccsfile::ZLock;

/// Install the SIGINT cleanup handler. Every utility that acquires a lock or
/// writes an x-file must call this from `main`, or the cleanup registry those
/// paths populate is never consulted.
pub fn install_cleanup() {
    plib::sccsfile::install_sigint_cleanup();
}

/// Whether an error from [`acquire`] means another command holds the lock.
pub fn is_held(e: &io::Error) -> bool {
    e.kind() == io::ErrorKind::AlreadyExists
}

/// Take the per-command z-file lock, mapping the "already locked" case to a
/// clear diagnostic so callers report that the s-file is being edited rather
/// than a raw "File exists".
pub fn acquire(sfile: &Path) -> io::Result<ZLock> {
    ZLock::acquire(sfile).map_err(|e| {
        if e.kind() == io::ErrorKind::AlreadyExists {
            // Localized here rather than at the call sites: `admin` propagates
            // this error for `main` to print instead of matching on it, so an
            // untranslated literal would reach the user from that one path
            // while every other utility printed a translated one.
            io::Error::new(io::ErrorKind::AlreadyExists, gettext("being edited"))
        } else {
            e
        }
    })
}
