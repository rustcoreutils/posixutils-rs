//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A file's POSIX permission bits, on Unix and on Windows.
//!
//! Windows keeps only a read-only attribute, which is the owner-write bit
//! seen from POSIX: a read-only file reads as `0444`, any other as `0644`,
//! and a mode without owner write makes a file read-only.

use std::fs::Permissions;

/// The permission bits of `perm`, as POSIX writes them.
#[cfg(unix)]
pub fn mode_of(perm: &Permissions) -> u32 {
    use std::os::unix::fs::PermissionsExt;
    perm.mode() & 0o7777
}

/// The permission bits of `perm`: `0444` for a read-only file, else `0644`.
#[cfg(windows)]
pub fn mode_of(perm: &Permissions) -> u32 {
    if perm.readonly() {
        0o444
    } else {
        0o644
    }
}

/// Give `perm` the permission bits of `mode`, the set-ID and sticky bits
/// included, keeping only the file type it holds.
#[cfg(unix)]
pub fn set_mode(perm: &mut Permissions, mode: u32) {
    use std::os::unix::fs::PermissionsExt;
    perm.set_mode(((perm.mode() >> 12) << 12) | (mode & 0o7777));
}

/// Make `perm` read-only exactly when `mode` withholds owner write.
#[cfg(windows)]
pub fn set_mode(perm: &mut Permissions, mode: u32) {
    perm.set_readonly(mode & 0o200 == 0);
}

/// The mode a utility gives a file it creates (XCU 1.1.1.4): `0666` less the
/// umask. Windows has no umask, and a new file there is an ordinary writable
/// one, `0644`.
pub fn new_file_mode() -> u32 {
    #[cfg(unix)]
    {
        crate::modestr::default_create_mode()
    }
    #[cfg(windows)]
    {
        0o644
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn owner_write_round_trips() {
        let dir = crate::tmp::tempdir().unwrap();
        let path = dir.path().join("f");
        fs::write(&path, b"x").unwrap();

        for mode in [0o444, 0o644] {
            let mut perm = fs::metadata(&path).unwrap().permissions();
            set_mode(&mut perm, mode);
            fs::set_permissions(&path, perm).unwrap();
            let got = mode_of(&fs::metadata(&path).unwrap().permissions());
            assert_eq!(got & 0o200, mode & 0o200, "{mode:o} read back as {got:o}");
        }
    }

    #[cfg(unix)]
    #[test]
    fn every_permission_bit_round_trips() {
        let dir = crate::tmp::tempdir().unwrap();
        let path = dir.path().join("f");
        fs::write(&path, b"x").unwrap();

        let mut perm = fs::metadata(&path).unwrap().permissions();
        set_mode(&mut perm, 0o751);
        fs::set_permissions(&path, perm).unwrap();
        assert_eq!(mode_of(&fs::metadata(&path).unwrap().permissions()), 0o751);
    }

    /// The special bits are part of the mode being set, not something kept
    /// from the file: a set-user-ID file given `0644` stops being one.
    #[cfg(unix)]
    #[test]
    fn set_mode_replaces_the_special_bits() {
        let dir = crate::tmp::tempdir().unwrap();
        let path = dir.path().join("f");
        fs::write(&path, b"x").unwrap();

        let mut perm = fs::metadata(&path).unwrap().permissions();
        set_mode(&mut perm, 0o4755);
        fs::set_permissions(&path, perm).unwrap();
        assert_eq!(mode_of(&fs::metadata(&path).unwrap().permissions()), 0o4755);

        let mut perm = fs::metadata(&path).unwrap().permissions();
        set_mode(&mut perm, 0o644);
        fs::set_permissions(&path, perm).unwrap();
        assert_eq!(mode_of(&fs::metadata(&path).unwrap().permissions()), 0o644);
    }

    #[cfg(windows)]
    #[test]
    fn modes_are_the_read_only_attribute() {
        let dir = crate::tmp::tempdir().unwrap();
        let path = dir.path().join("f");
        fs::write(&path, b"x").unwrap();
        assert_eq!(mode_of(&fs::metadata(&path).unwrap().permissions()), 0o644);
        assert_eq!(new_file_mode(), 0o644);

        let mut perm = fs::metadata(&path).unwrap().permissions();
        set_mode(&mut perm, 0o555);
        fs::set_permissions(&path, perm.clone()).unwrap();
        assert_eq!(mode_of(&fs::metadata(&path).unwrap().permissions()), 0o444);

        // Wine will not delete a read-only file; clear it for the TempDir.
        set_mode(&mut perm, 0o644);
        fs::set_permissions(&path, perm).unwrap();
    }
}
