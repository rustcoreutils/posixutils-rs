//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! pax operation mode implementations

pub(crate) mod anchored;
pub mod append;
pub mod copy;
pub mod list;
pub mod read;
pub mod write;

pub use append::append_to_archive;
pub use copy::copy_files;
pub use list::list_archive;
pub use read::extract_archive;
pub use write::create_archive;

/// Whether an error should stop a traversal rather than skip one file.
///
/// A failure to write the archive, or to reach the destination filesystem, is
/// not about the file being visited and will recur for every one after it --
/// without this, a full disk or a closed pipe produces one diagnostic per
/// remaining file. A failure to read a *source* file is per-file, and POSIX
/// CONSEQUENCES OF ERRORS says to diagnose it and carry on.
pub(crate) fn is_fatal(err: &crate::error::PaxError) -> bool {
    match err {
        crate::error::PaxError::Io(e) => matches!(
            e.kind(),
            std::io::ErrorKind::BrokenPipe | std::io::ErrorKind::StorageFull
        ),
        _ => false,
    }
}
