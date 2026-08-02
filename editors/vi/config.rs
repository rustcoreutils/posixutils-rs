//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Startup configuration helpers.
//!
//! This module contains functions for validating and loading .exrc
//! configuration files per POSIX security requirements.

use std::fs;
use std::io::{self, Read};
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
/// Returns `Ok(None)` when the file is deliberately skipped -- it does not
/// exist, or it failed one of the safety checks above. Historical ex is silent
/// in both cases, and a rejected-for-safety `.exrc` should not be reported as
/// an I/O failure.
///
/// Returns `Err` only when the file exists but could not be read (#X21). The
/// two used to be conflated as `None`, so an unreadable `.exrc` was silently
/// ignored and the user got no hint that their configuration had not loaded.
pub fn read_safe_exrc(path: &str) -> io::Result<Option<String>> {
    let mut file = match fs::OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NOFOLLOW)
        .open(path)
    {
        Ok(f) => f,
        // Absent, or a symlink refused by O_NOFOLLOW: nothing to load, and
        // nothing worth reporting.
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(None),
        Err(e) if e.raw_os_error() == Some(libc::ELOOP) => return Ok(None),
        Err(e) => return Err(e),
    };

    let metadata = file.metadata()?;

    // Must be a regular file
    if !metadata.file_type().is_file() {
        return Ok(None);
    }

    // Check ownership: must be owned by real user ID
    let real_uid = unsafe { libc::getuid() };
    if metadata.uid() != real_uid {
        return Ok(None);
    }

    // Check permissions: not writable by group or others
    let mode = metadata.mode();
    if (mode & 0o022) != 0 {
        return Ok(None);
    }

    let mut content = String::new();
    file.read_to_string(&mut content)?;
    Ok(Some(content))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::fs::PermissionsExt;

    fn write_mode(path: &std::path::Path, content: &str, mode: u32) {
        fs::write(path, content).unwrap();
        fs::set_permissions(path, fs::Permissions::from_mode(mode)).unwrap();
    }

    #[test]
    fn test_read_safe_exrc_ok() {
        let dir = tempfile::tempdir().unwrap();
        let p = dir.path().join(".exrc");
        write_mode(&p, "set number\n", 0o600);
        assert_eq!(
            read_safe_exrc(p.to_str().unwrap()).unwrap(),
            Some("set number\n".to_string())
        );
    }

    #[test]
    fn test_read_safe_exrc_group_writable_rejected() {
        let dir = tempfile::tempdir().unwrap();
        let p = dir.path().join(".exrc");
        write_mode(&p, "set number\n", 0o620);
        assert_eq!(read_safe_exrc(p.to_str().unwrap()).unwrap(), None);
    }

    #[test]
    fn test_read_safe_exrc_other_writable_rejected() {
        let dir = tempfile::tempdir().unwrap();
        let p = dir.path().join(".exrc");
        write_mode(&p, "set number\n", 0o602);
        assert_eq!(read_safe_exrc(p.to_str().unwrap()).unwrap(), None);
    }

    #[test]
    fn test_read_safe_exrc_unreadable_is_err() {
        // #X21: an .exrc that exists but cannot be read must be distinguishable
        // from one that is absent or was deliberately skipped for safety --
        // otherwise a mis-permissioned config is silently ignored.
        //
        // Skipped as root, which can read a mode-000 file regardless.
        if unsafe { libc::getuid() } == 0 {
            return;
        }
        let dir = tempfile::tempdir().unwrap();
        let p = dir.path().join(".exrc");
        write_mode(&p, "set number\n", 0o000);
        let r = read_safe_exrc(p.to_str().unwrap());
        assert!(
            r.is_err(),
            "exists-but-unreadable must be an error, got {:?}",
            r
        );
    }

    #[test]
    fn test_read_safe_exrc_insecure_is_silent_skip_not_error() {
        // A file rejected by the ownership/permission checks is a security
        // decision, not an I/O failure: historical ex is silent there, so it
        // must stay Ok(None) rather than becoming Err.
        let dir = tempfile::tempdir().unwrap();
        let p = dir.path().join(".exrc");
        write_mode(&p, "set number\n", 0o622);
        assert_eq!(read_safe_exrc(p.to_str().unwrap()).unwrap(), None);
    }

    #[test]
    fn test_read_safe_exrc_missing_is_none() {
        let dir = tempfile::tempdir().unwrap();
        let p = dir.path().join("does-not-exist");
        assert_eq!(read_safe_exrc(p.to_str().unwrap()).unwrap(), None);
    }
}
