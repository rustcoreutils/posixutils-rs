//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The group database, looked up with the reentrant `getgrgid_r` and
//! `getgrnam_r`. See [`crate::user`] for why every lookup comes here and why
//! the text fields are byte-exact [`OsString`]s.

use crate::nssbuf;
use std::ffi::{CString, OsStr, OsString};
use std::io;
use std::os::unix::ffi::OsStrExt;
use std::ptr;
use std::sync::Mutex;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Group {
    pub name: OsString,
    pub passwd: OsString,
    pub gid: libc::gid_t,
    /// The supplementary members, by login name.
    pub members: Vec<OsString>,
}

impl Group {
    /// Returns the group's GID.
    pub fn gid(&self) -> u32 {
        self.gid
    }

    /// Copy an entry the C library filled in.
    fn from_group(gr: &libc::group) -> Self {
        // SAFETY: a group entry from a successful lookup holds valid C strings
        // (or null) and a null-terminated (or null) member array.
        unsafe {
            // read_unaligned: macOS does not guarantee the member array's
            // alignment inside the caller's buffer.
            let mut members = Vec::new();
            let mut member = gr.gr_mem;
            while !member.is_null() {
                let name = ptr::read_unaligned(member);
                if name.is_null() {
                    break;
                }
                members.push(nssbuf::os_string(name));
                member = member.add(1);
            }
            Group {
                name: nssbuf::os_string(gr.gr_name),
                passwd: nssbuf::os_string(gr.gr_passwd),
                gid: gr.gr_gid,
                members,
            }
        }
    }
}

/// Every entry in the group database.
///
/// Enumeration has no portable reentrant form (`getgrent_r` is glibc's own),
/// and its position is one stream per process however it is read, so this
/// serializes enumerations against each other. A lookup by name or GID never
/// touches the stream.
pub fn load() -> Vec<Group> {
    static STREAM: Mutex<()> = Mutex::new(());
    let _guard = STREAM
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());

    let mut groups = Vec::new();
    // SAFETY: the stream is ours while the guard is held, and each entry is
    // copied out before the next getgrent call can overwrite it.
    unsafe {
        libc::setgrent();
        loop {
            let gr = libc::getgrent();
            if gr.is_null() {
                break;
            }
            groups.push(Group::from_group(&*gr));
        }
        libc::endgrent();
    }
    groups
}

/// Look up a group by name.
///
/// `Ok(None)` means the database has no such group; `Err` means the lookup
/// itself failed, which a caller making a security decision must not mistake
/// for "no such group".
pub fn lookup_by_name(name: impl AsRef<OsStr>) -> io::Result<Option<Group>> {
    // A name holding a NUL cannot be passed to the C library, and no entry
    // could match one anyway.
    let Ok(name) = CString::new(name.as_ref().as_bytes()) else {
        return Ok(None);
    };
    nssbuf::lookup(
        libc::_SC_GETGR_R_SIZE_MAX,
        // SAFETY: every pointer is valid for the call and `buf` is writable
        // for its full length.
        |gr, buf, result| unsafe {
            libc::getgrnam_r(name.as_ptr(), gr, buf.as_mut_ptr(), buf.len(), result)
        },
        Group::from_group,
    )
}

/// Look up a group by GID. See [`lookup_by_name`] for the result.
pub fn lookup_by_gid(gid: libc::gid_t) -> io::Result<Option<Group>> {
    nssbuf::lookup(
        libc::_SC_GETGR_R_SIZE_MAX,
        // SAFETY: as in `lookup_by_name`.
        |gr, buf, result| unsafe { libc::getgrgid_r(gid, gr, buf.as_mut_ptr(), buf.len(), result) },
        Group::from_group,
    )
}

/// Look up a group by name, reading a failed lookup as "no such group".
pub fn get_by_name(name: impl AsRef<OsStr>) -> Option<Group> {
    lookup_by_name(name).ok().flatten()
}

/// Look up a group by GID, reading a failed lookup as "no such group".
pub fn get_by_gid(gid: libc::gid_t) -> Option<Group> {
    lookup_by_gid(gid).ok().flatten()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn unknown_and_unspellable_names_are_not_found() {
        assert!(matches!(lookup_by_name("nosuchgroup.plib.test"), Ok(None)));
        assert!(matches!(lookup_by_name("ro\0ot"), Ok(None)));
    }

    #[test]
    fn lookups_agree_with_enumeration() {
        // SAFETY: getegid never fails.
        let egid = unsafe { libc::getegid() };
        let Some(mine) = get_by_gid(egid) else {
            return; // a gid with no database entry (a sparse container)
        };
        assert_eq!(get_by_name(&mine.name).map(|g| g.gid), Some(egid));
        assert!(load().iter().any(|g| g.gid == egid && g.name == mine.name));
    }

    /// The group half of `user`'s static-buffer regression test: threads look
    /// up gid 0 and the caller's group, by gid and by name, and check every
    /// answer is the one asked for. As there, each answer is compared with one
    /// taken beforehand by the same kind of lookup, since the two kinds may
    /// come from different sources.
    #[test]
    fn concurrent_lookups_do_not_see_each_other() {
        const THREADS: usize = 8;
        const ROUNDS: usize = 10_000;

        /// One group as each kind of lookup returns it.
        #[derive(Clone)]
        struct Expected {
            gid: libc::gid_t,
            by_gid: Option<Group>,
            by_name: Option<Group>,
        }

        fn expected(gid: libc::gid_t) -> Expected {
            let by_gid = lookup_by_gid(gid).unwrap();
            let by_name = by_gid.as_ref().map(|g| {
                lookup_by_name(&g.name)
                    .unwrap()
                    .expect("a group's name finds it")
            });
            // Two groups may share a gid; the name is what a by-name lookup
            // must give back.
            if let (Some(a), Some(b)) = (&by_gid, &by_name) {
                assert_eq!(a.name, b.name);
            }
            Expected {
                gid,
                by_gid,
                by_name,
            }
        }

        // SAFETY: getegid never fails.
        let egid = unsafe { libc::getegid() };
        let zero = expected(0);
        let mine = expected(egid);

        let handles: Vec<_> = (0..THREADS)
            .map(|t| {
                let zero = zero.clone();
                let mine = mine.clone();
                thread::spawn(move || {
                    for i in 0..ROUNDS {
                        let want = if (i + t) % 2 == 0 { &zero } else { &mine };
                        assert_eq!(lookup_by_gid(want.gid).unwrap(), want.by_gid, "by gid");
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
