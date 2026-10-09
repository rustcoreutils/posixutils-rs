//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX mv step 7 after a move across filesystems: remove the source file hierarchy.
//!
//! What is removed is exactly what the copy duplicated, unchanged since (`CopiedSources`),
//! reached from the directory the operand was pinned in and from there only through directory
//! descriptors the walk opened and checked (`ftw::traverse_directory_at`). The operand's
//! pathname is never resolved again: a directory on the way to it that was renamed or replaced
//! by a symbolic link after the move began leads nowhere new. Anything the copy did not
//! duplicate -- an entry added since, a file written to or replaced since -- is left where it
//! is and reported, and so is every directory still holding one.

use crate::common::{error_string, quote, report_verbose, CopiedSources, InodeMap, PinnedEntry};
use gettextrs::gettext;
use std::{cell::RefCell, io, os::unix::fs::MetadataExt};

/// Bookkeeping for one removal walk.
#[derive(Default)]
struct Removal {
    /// Entries left in place so far, each already reported.
    left: usize,
    /// `left` when each directory being walked was entered.
    left_on_entry: Vec<usize>,
}

impl Removal {
    fn leave(&mut self, message: String) {
        eprintln!("mv: {message}");
        self.left += 1;
    }
}

/// Remove what the copy of `source` duplicated. Returns whether all of it was removed; every
/// entry that was not has been reported.
///
/// `inode_map` is the move's record of hard-linked files already copied. A file whose last link
/// this removes is forgotten there: its inode number is free for the system to give a new
/// file, which a later operand must not then take for this one and link to its copy.
pub fn remove_moved_source(
    source: &PinnedEntry,
    copied: &CopiedSources,
    inode_map: &mut InodeMap,
    verbose: bool,
) -> bool {
    let removal = RefCell::new(Removal::default());

    let file_handler = |entry: ftw::Entry<'_>| -> Result<bool, ()> {
        let mut removal = removal.borrow_mut();
        let Some(md) = entry.metadata().filter(|md| copied.unchanged(md)) else {
            removal.leave(gettext!(
                "not removing '{}': it changed during the move",
                entry.path()
            ));
            return Ok(false);
        };
        if md.is_dir() {
            // Emptied first; removed on the way out (`remove_emptied_dir`).
            let left = removal.left;
            removal.left_on_entry.push(left);
            return Ok(true);
        }
        if unsafe { libc::unlinkat(entry.dir_fd(), entry.file_name().as_ptr(), 0) } != 0 {
            removal.leave(cannot_remove(&entry, &io::Error::last_os_error()));
            return Ok(false);
        }
        if md.nlink() <= 1 {
            inode_map.remove(&(md.dev(), md.ino()));
        }
        if verbose {
            report_verbose(&gettext!("removed {}", quote(entry.path().as_inner())));
        }
        Ok(false)
    };
    let postprocess_dir = |entry: ftw::Entry<'_>, exit: ftw::DirExit| -> Result<(), ()> {
        let mut removal = removal.borrow_mut();
        let left_on_entry = removal.left_on_entry.pop().unwrap_or(0);
        // A directory that could not be read was reported by `err_reporter`.
        if exit == ftw::DirExit::Descended {
            let holds_reported = removal.left > left_on_entry;
            if remove_emptied_dir(&entry, holds_reported, &mut removal) && verbose {
                let shown = quote(entry.path().as_inner());
                report_verbose(&gettext!("removed directory {}", shown));
            }
        }
        Ok(())
    };
    let err_reporter = |entry: ftw::Entry<'_>, error: ftw::Error| {
        let message = cannot_remove(&entry, &error.inner());
        removal.borrow_mut().leave(message);
    };

    // The return value only says whether the operand was a directory walked without error; what
    // matters is what was left.
    let _ = ftw::traverse_directory_at(
        source.dir(),
        source.name(),
        source.display_parent(),
        file_handler,
        postprocess_dir,
        err_reporter,
        ftw::TraverseDirectoryOpts::default(),
    );
    removal.into_inner().left == 0
}

/// Remove a directory whose entries the walk has just removed, or left (`holds_reported`: those
/// were reported, and the directory has to stay for them).
///
/// The name must still be the directory the walk entered; `AT_REMOVEDIR` then removes it only if
/// it is empty -- nothing was added since the walk read it. Returns whether it was removed.
fn remove_emptied_dir(entry: &ftw::Entry<'_>, holds_reported: bool, removal: &mut Removal) -> bool {
    let entered = entry.metadata().map(|md| (md.dev(), md.ino()));
    let current = ftw::Metadata::new(entry.dir_fd(), entry.file_name(), false)
        .ok()
        .map(|md| (md.dev(), md.ino()));
    if current.is_none() || current != entered {
        removal.leave(gettext!(
            "not removing '{}': it changed during the move",
            entry.path()
        ));
        return false;
    }
    let ret = unsafe {
        libc::unlinkat(
            entry.dir_fd(),
            entry.file_name().as_ptr(),
            libc::AT_REMOVEDIR,
        )
    };
    if ret == 0 {
        return true;
    }
    let e = io::Error::last_os_error();
    let not_empty = matches!(e.raw_os_error(), Some(libc::ENOTEMPTY) | Some(libc::EEXIST));
    if !(not_empty && holds_reported) {
        removal.leave(cannot_remove(entry, &e));
    }
    false
}

fn cannot_remove(entry: &ftw::Entry<'_>, e: &io::Error) -> String {
    gettext!("cannot remove '{}': {}", entry.path(), error_string(e))
}
