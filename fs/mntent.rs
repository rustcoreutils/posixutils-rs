//
// Copyright (c) 2024 fox0
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The mtab file
//! https://www.gnu.org/software/libc/manual/html_node/mtab.html

use libc::{endmntent, getmntent, setmntent, FILE};
use std::ffi::{CStr, CString};
use std::io;
use std::sync::Mutex;

const _PATH_MOUNTED: &CStr = c"/etc/mtab";

/// The mtab (contraction of mounted file systems table) file
/// is a system information file, commonly found on Unix-like systems
pub struct MountTable {
    inner: *mut FILE,
}

/// Structure describing a mount table entry
#[derive(Debug, PartialEq)]
pub struct MountTableEntity {
    /// Device or server for filesystem
    pub fsname: CString,
    /// Directory mounted on
    pub dir: CString,
    /// Type of filesystem: ufs, nfs, etc
    pub fstype: CString,
    /// Comma-separated options for fs
    pub opts: CString,
    /// Dump frequency (in days)
    pub freq: i32,
    /// Pass number for `fsck``
    pub passno: i32,
}

impl MountTable {
    pub fn try_new() -> Result<Self, io::Error> {
        Self::open(_PATH_MOUNTED, c"r")
    }

    /// Open mtab file
    fn open(filename: &CStr, mode: &CStr) -> Result<Self, io::Error> {
        // Preliminary: | MT-Safe | AS-Unsafe heap lock | AC-Unsafe mem fd lock
        // https://www.gnu.org/software/libc/manual/html_node/POSIX-Safety-Concepts.html
        let inner = unsafe { setmntent(filename.as_ptr(), mode.as_ptr()) };
        if inner.is_null() {
            return Err(io::Error::last_os_error());
        }
        Ok(Self { inner })
    }
}

impl Iterator for MountTable {
    type Item = MountTableEntity;

    fn next(&mut self) -> Option<Self::Item> {
        static THREAD_UNSAFE_FUNCTION_MUTEX: Mutex<()> = Mutex::new(());
        let _lock = THREAD_UNSAFE_FUNCTION_MUTEX.lock().unwrap();

        // Preliminary: | MT-Unsafe race:mntentbuf locale | AS-Unsafe corrupt heap init | AC-Unsafe init corrupt lock mem
        // https://www.gnu.org/software/libc/manual/html_node/POSIX-Safety-Concepts.html
        let me = unsafe { getmntent(self.inner) };
        if me.is_null() {
            return None;
        }

        unsafe {
            Some(MountTableEntity {
                fsname: CStr::from_ptr((*me).mnt_fsname).into(),
                dir: CStr::from_ptr((*me).mnt_dir).into(),
                fstype: CStr::from_ptr((*me).mnt_type).into(),
                opts: CStr::from_ptr((*me).mnt_opts).into(),
                freq: (*me).mnt_freq,
                passno: (*me).mnt_passno,
            })
        }
    }
}

impl Drop for MountTable {
    /// Close mtab file
    fn drop(&mut self) {
        // Preliminary: | MT-Safe | AS-Unsafe heap lock | AC-Unsafe lock mem fd
        // https://www.gnu.org/software/libc/manual/html_node/POSIX-Safety-Concepts.html
        let _rc = unsafe { endmntent(self.inner) };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_open() {
        let mtab = MountTable::open(c"tests/mtab.txt", c"r");
        assert!(mtab.is_ok());
    }

    #[test]
    fn test_open_not_found() {
        let mtab = MountTable::open(c"/tmp/not_found", c"r");
        let mtab = mtab.err().unwrap();
        assert_eq!(mtab.kind(), std::io::ErrorKind::NotFound);
    }

    #[test]
    fn test_iterable() {
        let mtab = MountTable::open(c"tests/mtab.txt", c"r").unwrap();
        let vec = Vec::from_iter(mtab);
        assert_eq!(vec.len(), 2);
        assert_eq!(
            vec[0],
            MountTableEntity {
                fsname: CString::new("/dev/sdb1").unwrap(),
                dir: CString::new("/").unwrap(),
                fstype: CString::new("ext3").unwrap(),
                opts: CString::new("rw,relatime,errors=remount-ro").unwrap(),
                freq: 0,
                passno: 0,
            }
        );
        assert_eq!(
            vec[1],
            MountTableEntity {
                fsname: CString::new("proc").unwrap(),
                dir: CString::new("/proc").unwrap(),
                fstype: CString::new("proc").unwrap(),
                opts: CString::new("rw,noexec,nosuid,nodev").unwrap(),
                freq: 0,
                passno: 0,
            }
        );
    }
}
