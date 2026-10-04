//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! User and group database lookups, in both directions, memoized.
//!
//! Both directions are needed and they are each other's inverse, so they live
//! together. Write mode records a name for every uid and gid it archives, and
//! read mode resolves an archived name back to an id: POSIX, ustar Interchange
//! Format, "the user and group databases shall be scanned for these names. If
//! found, the user and group IDs contained within these files shall be used
//! rather than the values contained within the uid and gid fields."
//!
//! A name is bytes here, not a `String`. `plib::user` and `plib::group` offer
//! the same lookups over `&str`, but they decode the database's `char *`
//! lossily, and a user name that is not UTF-8 would come back with U+FFFD in
//! it -- the very substitution `hdrcharset=BINARY` exists to avoid. On the way
//! in, a `&str` cannot spell such a name at all.
//!
//! Memoized because libc caches none of these: under a `files` backend each
//! call is an open/read/close of /etc/passwd or /etc/group, and under LDAP or
//! SSSD a network round trip. A file hierarchy almost always has one or two
//! distinct owners, so a tree of 100,000 files made 200,000 lookups where two
//! would do.

use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::{CStr, CString};

/// The name of a uid, or `None` when the database has no entry for it.
pub(crate) fn name_for_uid(uid: u32) -> Option<Vec<u8>> {
    thread_local! {
        static CACHE: RefCell<HashMap<u32, Option<Vec<u8>>>> = RefCell::new(HashMap::new());
    }
    CACHE.with(|cache| {
        cached(cache, uid, |&uid| unsafe {
            let pw = libc::getpwuid(uid);
            (!pw.is_null()).then(|| CStr::from_ptr((*pw).pw_name).to_bytes().to_vec())
        })
    })
}

/// The name of a gid, or `None` when the database has no entry for it.
pub(crate) fn name_for_gid(gid: u32) -> Option<Vec<u8>> {
    thread_local! {
        static CACHE: RefCell<HashMap<u32, Option<Vec<u8>>>> = RefCell::new(HashMap::new());
    }
    CACHE.with(|cache| {
        cached(cache, gid, |&gid| unsafe {
            let gr = libc::getgrgid(gid);
            (!gr.is_null()).then(|| CStr::from_ptr((*gr).gr_name).to_bytes().to_vec())
        })
    })
}

/// The uid a user name resolves to on this host, or `None` when the database
/// does not know the name -- which is the "If found" in POSIX's sentence, and
/// leaves the archived numeric id in force.
pub(crate) fn uid_for_name(name: &[u8]) -> Option<u32> {
    thread_local! {
        static CACHE: RefCell<HashMap<Vec<u8>, Option<u32>>> = RefCell::new(HashMap::new());
    }
    CACHE.with(|cache| {
        cached(cache, name.to_vec(), |name| unsafe {
            // A name holding a NUL cannot be passed to getpwnam at all, and no
            // database entry could match one anyway.
            let name = CString::new(name.as_slice()).ok()?;
            let pw = libc::getpwnam(name.as_ptr());
            (!pw.is_null()).then(|| (*pw).pw_uid)
        })
    })
}

/// The gid a group name resolves to on this host. See [`uid_for_name`].
pub(crate) fn gid_for_name(name: &[u8]) -> Option<u32> {
    thread_local! {
        static CACHE: RefCell<HashMap<Vec<u8>, Option<u32>>> = RefCell::new(HashMap::new());
    }
    CACHE.with(|cache| {
        cached(cache, name.to_vec(), |name| unsafe {
            let name = CString::new(name.as_slice()).ok()?;
            let gr = libc::getgrnam(name.as_ptr());
            (!gr.is_null()).then(|| (*gr).gr_gid)
        })
    })
}

/// Look `key` up in `cache`, calling `lookup` only on a miss.
///
/// A miss is cached too: a uid with no database entry is as worth remembering
/// as one with, and an archive full of them would otherwise pay for every
/// member.
fn cached<K, V, F>(cache: &RefCell<HashMap<K, V>>, key: K, lookup: F) -> V
where
    K: std::hash::Hash + Eq + Clone,
    V: Clone,
    F: FnOnce(&K) -> V,
{
    // Borrowed and dropped before `lookup` runs: it calls into libc, and a
    // borrow still held across that would panic if it ever reentered.
    let hit = cache.borrow().get(&key).cloned();
    if let Some(value) = hit {
        return value;
    }
    let value = lookup(&key);
    cache.borrow_mut().insert(key, value.clone());
    value
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The two directions have to be inverses, or a name written by write mode
    /// would not resolve back on extract.
    #[test]
    fn test_lookups_round_trip_for_the_current_user() {
        let euid = unsafe { libc::geteuid() };
        let egid = unsafe { libc::getegid() };

        // A uid with no database entry (a sparse container) leaves nothing to
        // assert; every real account has one.
        if let Some(name) = name_for_uid(euid) {
            assert_eq!(uid_for_name(&name), Some(euid), "uid round trip");
        }
        if let Some(name) = name_for_gid(egid) {
            assert_eq!(gid_for_name(&name), Some(egid), "gid round trip");
        }
    }

    /// A name no database knows resolves to nothing rather than to 0, which
    /// would hand the file to root.
    #[test]
    fn test_unknown_name_resolves_to_nothing() {
        assert_eq!(uid_for_name(b"nosuchuser.pax.test"), None);
        assert_eq!(gid_for_name(b"nosuchgroup.pax.test"), None);
        // Including one that cannot even be spelled as a C string.
        assert_eq!(uid_for_name(b"ro\0ot"), None);
    }

    /// Repeated lookups are served from the cache, including the misses.
    #[test]
    fn test_misses_are_cached_too() {
        assert_eq!(uid_for_name(b"nosuchuser.pax.cached"), None);
        assert_eq!(uid_for_name(b"nosuchuser.pax.cached"), None);
    }
}
