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
pub(crate) mod select;
pub mod write;

pub use append::append_to_archive;
pub use copy::copy_files;
pub use list::list_archive;
pub use read::extract_archive;
pub use write::create_archive;

/// Whether an error should stop a traversal rather than skip one file.
///
/// A failure to write the archive is not about the file being visited and will
/// recur for every one after it -- without this, a full disk, a closed pipe or
/// an exceeded file-size limit produces one diagnostic per remaining file, each
/// blaming a file that did nothing wrong. Every I/O error the archive writer
/// raises arrives as `ArchiveWrite` (see `write::ArchiveSink`), whatever its
/// errno. Copy mode has no archive; there a full or vanished destination
/// filesystem is the shared failure. A failure to read a *source* file is
/// per-file, and POSIX CONSEQUENCES OF ERRORS says to diagnose it and carry on.
/// End of file on `/dev/tty` under -i ends the run by definition.
pub(crate) fn is_fatal(err: &crate::error::PaxError) -> bool {
    use crate::error::PaxError;
    match err {
        PaxError::ArchiveWrite(_) | PaxError::TtyEof => true,
        PaxError::Io(e) => matches!(
            e.kind(),
            std::io::ErrorKind::BrokenPipe | std::io::ErrorKind::StorageFull
        ),
        _ => false,
    }
}

/// `-X`: whether the walk may go below a directory on device `dev`, given the
/// device of the operand it was reached from (`None` at the operand itself).
///
/// POSIX: "when a directory with a different device ID is encountered, pax
/// shall process (archive or copy) the directory itself but shall not process
/// any files below the directory." So this decides descent only; the mount
/// point is still archived or copied, which is why it is not a filter on
/// entries.
pub(crate) fn may_descend(one_file_system: bool, operand_dev: Option<u64>, dev: u64) -> bool {
    !one_file_system || operand_dev.is_none_or(|operand| operand == dev)
}

#[cfg(test)]
mod tests {
    use super::may_descend;

    #[test]
    fn one_file_system_stops_below_a_mount_point_only() {
        // The operand itself, and anything at all without -X.
        assert!(may_descend(true, None, 7));
        assert!(may_descend(false, Some(1), 7));
        // A directory on the operand's device is descended.
        assert!(may_descend(true, Some(1), 1));
        // A mount point is not.
        assert!(!may_descend(true, Some(1), 7));
    }
}
