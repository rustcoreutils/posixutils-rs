//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The user database (`/etc/passwd` and whatever else NSS consults), looked
//! up with the reentrant `getpwuid_r` and `getpwnam_r`.
//!
//! Every user lookup in the workspace goes through here. The plain
//! `getpwuid`/`getpwnam` return a pointer into a static buffer that a lookup
//! on any other thread overwrites, which made tests that ran in parallel read
//! each other's answers.
//!
//! Text fields are [`OsString`]s holding the database's bytes exactly: a user
//! name need not be UTF-8, and `pax` must write and match such names without
//! U+FFFD substituted into them.

use crate::nssbuf;
use std::ffi::{CString, OsStr, OsString};
use std::io;
use std::os::unix::ffi::OsStrExt;
use std::path::PathBuf;
use std::sync::Mutex;

/// A user account from the system user database.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct User {
    /// The login name.
    pub name: OsString,
    pub uid: libc::uid_t,
    /// The primary (login) group.
    pub gid: libc::gid_t,
    /// The comment field, conventionally the user's full name.
    pub gecos: OsString,
    /// The initial working (home) directory.
    pub dir: PathBuf,
    /// The initial user program (login shell); empty means the system default.
    pub shell: PathBuf,
}

impl User {
    /// Returns the user's UID.
    pub fn uid(&self) -> u32 {
        self.uid
    }

    /// Returns the user's primary GID.
    pub fn gid(&self) -> u32 {
        self.gid
    }

    /// Copy an entry the C library filled in.
    fn from_passwd(pw: &libc::passwd) -> Self {
        // SAFETY: a passwd entry from a successful lookup holds valid C strings
        // (or null, which `os_string` reads as empty).
        unsafe {
            User {
                name: nssbuf::os_string(pw.pw_name),
                uid: pw.pw_uid,
                gid: pw.pw_gid,
                gecos: nssbuf::os_string(pw.pw_gecos),
                dir: nssbuf::os_string(pw.pw_dir).into(),
                shell: nssbuf::os_string(pw.pw_shell).into(),
            }
        }
    }
}

/// Every user in the password database, through `setpwent`/`getpwent`/`endpwent`.
///
/// An error while reading is an error, never a shorter list: `getpwent` returns NULL both at
/// the end and on a failure, which only `errno` tells apart -- glibc leaves it 0 at the end,
/// and any value at all, ENOENT included, is taken for a failure. The enumeration is the
/// process's one, so callers here take turns.
pub fn load() -> io::Result<Vec<User>> {
    static ENUMERATING: Mutex<()> = Mutex::new(());
    let _turn = ENUMERATING.lock().unwrap_or_else(|e| e.into_inner());
    let mut users = Vec::new();
    let mut result = Ok(());
    unsafe {
        libc::setpwent();
        loop {
            errno::set_errno(errno::Errno(0));
            let passwd = libc::getpwent();
            if passwd.is_null() {
                let e = errno::errno().0;
                if e != 0 {
                    result = Err(io::Error::from_raw_os_error(e));
                }
                break;
            }
            users.push(User::from_passwd(&*passwd));
        }
        libc::endpwent();
    }
    result.map(|()| users)
}

/// Look up a user by name.
///
/// `Ok(None)` means the database has no such user. `Err` means the lookup
/// itself failed (an unreachable directory service, for one), which a caller
/// making a security decision must not mistake for "no such user".
pub fn lookup_by_name(name: impl AsRef<OsStr>) -> io::Result<Option<User>> {
    // A name holding a NUL cannot be passed to the C library, and no entry
    // could match one anyway.
    let Ok(name) = CString::new(name.as_ref().as_bytes()) else {
        return Ok(None);
    };
    nssbuf::lookup(
        libc::_SC_GETPW_R_SIZE_MAX,
        // SAFETY: every pointer is valid for the call and `buf` is writable
        // for its full length.
        |pw, buf, result| unsafe {
            libc::getpwnam_r(name.as_ptr(), pw, buf.as_mut_ptr(), buf.len(), result)
        },
        User::from_passwd,
    )
}

/// Look up a user by UID. See [`lookup_by_name`] for the result.
pub fn lookup_by_uid(uid: libc::uid_t) -> io::Result<Option<User>> {
    nssbuf::lookup(
        libc::_SC_GETPW_R_SIZE_MAX,
        // SAFETY: as in `lookup_by_name`.
        |pw, buf, result| unsafe { libc::getpwuid_r(uid, pw, buf.as_mut_ptr(), buf.len(), result) },
        User::from_passwd,
    )
}

/// Look up a user by name, reading a failed lookup as "no such user".
pub fn get_by_name(name: impl AsRef<OsStr>) -> Option<User> {
    lookup_by_name(name).ok().flatten()
}

/// Look up a user by UID, reading a failed lookup as "no such user".
pub fn get_by_uid(uid: libc::uid_t) -> Option<User> {
    lookup_by_uid(uid).ok().flatten()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn root_is_uid_zero() {
        let root = get_by_uid(0).expect("every system has a uid 0");
        assert_eq!(get_by_name(&root.name).map(|u| u.uid), Some(0));
    }

    #[test]
    fn unknown_and_unspellable_names_are_not_found() {
        assert!(matches!(lookup_by_name("nosuchuser.plib.test"), Ok(None)));
        assert!(matches!(lookup_by_name("ro\0ot"), Ok(None)));
    }

    /// The regression test for the static-buffer race. Several threads look up
    /// two different users by uid and by name, over and over, and check that
    /// every answer is the one asked for. Under the old `getpwuid`/`getpwnam`
    /// one thread's lookup overwrote the entry another was still reading, and
    /// a lookup for one user came back holding another's uid or name.
    ///
    /// Each answer is compared with one taken before the threads start by the
    /// same kind of lookup. The two kinds need not agree on every field: macOS
    /// answers a lookup by name and one by uid from different sources, and
    /// gives root a different shell from each.
    #[test]
    fn concurrent_lookups_do_not_see_each_other() {
        const THREADS: usize = 8;
        const ROUNDS: usize = 10_000;

        /// One user as each kind of lookup returns it.
        #[derive(Clone)]
        struct Expected {
            uid: libc::uid_t,
            by_uid: Option<User>,
            by_name: Option<User>,
        }

        fn expected(uid: libc::uid_t) -> Expected {
            let by_uid = lookup_by_uid(uid).unwrap();
            let by_name = by_uid.as_ref().map(|u| {
                lookup_by_name(&u.name)
                    .unwrap()
                    .expect("a user's name finds it")
            });
            if let (Some(a), Some(b)) = (&by_uid, &by_name) {
                assert_eq!((&a.name, a.uid), (&b.name, b.uid));
            }
            Expected {
                uid,
                by_uid,
                by_name,
            }
        }

        // SAFETY: geteuid never fails.
        let me = unsafe { libc::geteuid() };
        let root = expected(0);
        assert!(root.by_uid.is_some(), "every system has a uid 0");
        // A uid with no database entry (a sparse container) still races
        // against root's lookups; it just has no name to look up.
        let mine = expected(me);

        let handles: Vec<_> = (0..THREADS)
            .map(|t| {
                let root = root.clone();
                let mine = mine.clone();
                thread::spawn(move || {
                    for i in 0..ROUNDS {
                        // Threads start out of phase, so at any moment some
                        // are reading root's entry while others read mine.
                        let want = if (i + t) % 2 == 0 { &root } else { &mine };
                        assert_eq!(lookup_by_uid(want.uid).unwrap(), want.by_uid, "by uid");
                        if let Some(by_name) = &want.by_name {
                            let got = lookup_by_name(&by_name.name).unwrap();
                            assert_eq!(got.as_ref(), Some(by_name), "by name");
                        }
                    }
                })
            })
            .collect();
        for h in handles {
            h.join()
                .expect("a lookup thread saw another thread's entry");
        }
    }
}
