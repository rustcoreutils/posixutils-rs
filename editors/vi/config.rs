//! Startup configuration helpers.
//!
//! This module contains functions for validating and loading .exrc
//! configuration files per POSIX security requirements.

use std::fs;
use std::os::unix::fs::MetadataExt;
use std::path::Path;

/// Check if a path refers to the same file as $HOME/.exrc using inode/device comparison.
pub fn is_same_file_as_home_exrc(path: &str) -> bool {
    let home = match std::env::var("HOME") {
        Ok(h) if !h.is_empty() => h,
        _ => return false,
    };

    let home_exrc = format!("{}/.exrc", home);
    let meta_home = match fs::metadata(&home_exrc) {
        Ok(m) => m,
        Err(_) => return false,
    };
    let meta_local = match fs::metadata(Path::new(path)) {
        Ok(m) => m,
        Err(_) => return false,
    };

    meta_home.dev() == meta_local.dev() && meta_home.ino() == meta_local.ino()
}

/// Check if an .exrc file is safe to source.
///
/// Per POSIX, the file must be:
/// - Owned by the real user ID (or process has appropriate privileges)
/// - Not writable by group or others
pub fn is_safe_exrc(path: &str) -> bool {
    let metadata = match fs::metadata(path) {
        Ok(m) => m,
        Err(_) => return false, // File doesn't exist - not an error
    };

    // Check ownership: must be owned by real user ID
    let real_uid = unsafe { libc::getuid() };
    if metadata.uid() != real_uid {
        return false;
    }

    // Check permissions: not writable by group or others
    let mode = metadata.mode();
    if (mode & 0o022) != 0 {
        // group or other write bit set
        return false;
    }

    true
}
