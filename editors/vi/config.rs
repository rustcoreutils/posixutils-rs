//! Startup configuration helpers.
//!
//! This module contains functions for validating and loading .exrc
//! configuration files per POSIX security requirements.

use std::fs;
use std::io::Read;
use std::os::unix::fs::{MetadataExt, OpenOptionsExt};
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

/// Open an .exrc file race-free, validate safety, and return its content.
///
/// Opens with O_NOFOLLOW to prevent symlink attacks, then validates via
/// fstat on the open handle (no TOCTOU window). Per POSIX, the file must be:
/// - A regular file (not a symlink, device, etc.)
/// - Owned by the real user ID (or process has appropriate privileges)
/// - Not writable by group or others
///
/// Returns `None` if the file doesn't exist, is unsafe, or can't be read.
pub fn read_safe_exrc(path: &str) -> Option<String> {
    let mut file = fs::OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NOFOLLOW)
        .open(path)
        .ok()?;

    let metadata = file.metadata().ok()?;

    // Must be a regular file
    if !metadata.file_type().is_file() {
        return None;
    }

    // Check ownership: must be owned by real user ID
    let real_uid = unsafe { libc::getuid() };
    if metadata.uid() != real_uid {
        return None;
    }

    // Check permissions: not writable by group or others
    let mode = metadata.mode();
    if (mode & 0o022) != 0 {
        return None;
    }

    let mut content = String::new();
    file.read_to_string(&mut content).ok()?;
    Some(content)
}
