//
// Copyright (c) 2024 fox0
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use libc::{endmntent, getmntent, mntent, setmntent, FILE};
use std::ffi::{CStr, CString};
use std::io;
use crate::common::to_cstring;

/// The mtab (contraction of mounted file systems table) file
/// is a system information file, commonly found on Unix-like systems.
pub struct MountTable {
    inner: *mut FILE,
}

impl MountTable {
    /// File listing currently active mount points.
    const _PATH_MOUNTED: &CStr = c"/etc/mtab";

    pub fn try_new() -> Result<Self, io::Error> {
        Self::setmntent(Self::_PATH_MOUNTED)
    }

    /// Open mtab file
    #[inline(always)]
    fn setmntent(filename: &CStr) -> Result<Self, io::Error> {
        // SAFETY: always valid c-string
        let f = unsafe { setmntent(filename.as_ptr(), c"r".as_ptr()) };
        if f.is_null() {
            return Err(io::Error::last_os_error());
        }
        Ok(Self { inner: f })
    }
}

impl Iterator for MountTable {
    type Item = MountEntity;

    fn next(&mut self) -> Option<Self::Item> {
        // SAFETY: always valid
        let me = unsafe { getmntent(self.inner) };
        if me.is_null() {
            return None;
        }
        // SAFETY: is not null and valid c-string
        Some(unsafe { MountEntity::new(me) })
    }
}

impl Drop for MountTable {
    /// Close mtab file
    fn drop(&mut self) {
        // SAFETY: always valid
        let _rc = unsafe { endmntent(self.inner) };
    }
}

/// Structure describing a mount table entry.
#[derive(Debug, PartialEq)]
pub struct MountEntity {
    /// Device or server for filesystem.
    pub mnt_fsname: CString,
    /// Directory mounted on.
    pub mnt_dir: CString,
    /// Type of filesystem: ufs, nfs, etc.
    pub mnt_type: CString,
    /// Comma-separated options for fs.
    pub mnt_opts: CString,
    /// Dump frequency (in days).
    pub mnt_freq: i32,
    /// Pass number for `fsck`.
    pub mnt_passno: i32,
}

impl MountEntity {
    unsafe fn new(me: *const mntent) -> Self {
        debug_assert!(!me.is_null());
        Self {
            mnt_fsname: to_cstring((*me).mnt_fsname),
            mnt_dir: to_cstring((*me).mnt_dir),
            mnt_type: to_cstring((*me).mnt_type),
            mnt_opts: to_cstring((*me).mnt_opts),
            mnt_freq: (*me).mnt_freq,
            mnt_passno: (*me).mnt_passno,
        }
    }
}

// #[cfg(target_os = "linux")]
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_open() {
        let mtab = MountTable::setmntent(c"tests/mtab.txt");
        assert!(mtab.is_ok());
    }

    #[test]
    fn test_iterable() {
        let mtab = MountTable::setmntent(c"tests/mtab.txt").unwrap();
        let vec = Vec::from_iter(mtab);
        assert_eq!(vec.len(), 2);
        assert_eq!(
            vec[0],
            MountEntity {
                mnt_fsname: CString::new("/dev/sdb1").unwrap(),
                mnt_dir: CString::new("/").unwrap(),
                mnt_type: CString::new("ext3").unwrap(),
                mnt_opts: CString::new("rw,relatime,errors=remount-ro").unwrap(),
                mnt_freq: 0,
                mnt_passno: 0,
            }
        );
        assert_eq!(
            vec[1],
            MountEntity {
                mnt_fsname: CString::new("proc").unwrap(),
                mnt_dir: CString::new("/proc").unwrap(),
                mnt_type: CString::new("proc").unwrap(),
                mnt_opts: CString::new("rw,noexec,nosuid,nodev").unwrap(),
                mnt_freq: 0,
                mnt_passno: 0,
            }
        );
    }

    #[test]
    fn test_real() {
        let mtab = MountTable::try_new();
        assert!(mtab.is_ok());
    }

    #[test]
    fn test_real_iterable() {
        let mtab = MountTable::try_new().unwrap();
        for _mount in mtab {
            // dbg!(mount);
        }
        assert!(true);
    }
}
