//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Trust checks for files the daemon loads and executes. Historical cron
//! daemons were a rich source of local-root holes via spool-file injection and
//! symlink/hardlink races; before parsing any crontab `crond` verifies the file
//! is a regular file with the expected owner, mode, and a single hard link, and
//! opens it `O_NOFOLLOW` so a symlinked spool entry is refused (audit #D1/#D2).

use std::fmt;
use std::fs::{File, Metadata};
use std::os::unix::fs::{MetadataExt, OpenOptionsExt};
use std::path::Path;

/// Who is allowed to own a trusted file.
#[derive(Clone, Copy)]
pub enum OwnerRule {
    /// Must be owned by root (uid 0).
    Root,
    /// Must be owned by root or by the given uid.
    RootOrUid(u32),
    /// Any owner is acceptable; the caller derives the run-as identity from it.
    ///
    /// This used to be spelled `RootOrUid(u32::MAX)` and relied on a match arm
    /// with a const pattern. It worked, but `u32::MAX` is exactly the
    /// "invalid uid" sentinel, so the encoding put a security decision one
    /// typo away from meaning its opposite (#D15).
    Any,
}

/// The checks a file must satisfy to be trusted.
pub struct TrustPolicy {
    pub require_regular: bool,
    pub require_single_link: bool,
    /// Exact permission bits required (`mode & 0o777`), if any.
    pub mode_exact: Option<u32>,
    /// Permission bits that must NOT be set (e.g. `0o022` = not group/other writable).
    pub forbid_mode_bits: u32,
    pub owner: OwnerRule,
}

impl TrustPolicy {
    /// A per-user crontab spool file: regular, mode 0600, single link, owned by
    /// root or the named user.
    pub fn crontab_spool(user_uid: u32) -> Self {
        Self {
            require_regular: true,
            require_single_link: true,
            mode_exact: Some(0o600),
            forbid_mode_bits: 0,
            owner: OwnerRule::RootOrUid(user_uid),
        }
    }

    /// The system crontab (`/etc/crontab`): regular, single link, root-owned,
    /// not group- or other-writable.
    pub fn system_crontab() -> Self {
        Self {
            require_regular: true,
            require_single_link: true,
            mode_exact: None,
            forbid_mode_bits: 0o022,
            owner: OwnerRule::Root,
        }
    }

    /// An at-spool job file: regular, single link, not group/other-writable. The
    /// owner becomes the run-as identity, so any real uid is accepted here.
    pub fn at_spool() -> Self {
        Self {
            require_regular: true,
            require_single_link: true,
            mode_exact: None,
            forbid_mode_bits: 0o022,
            owner: OwnerRule::Any,
        }
    }
}

/// Why a file failed its trust check.
#[derive(Debug, PartialEq)]
pub enum TrustReject {
    NotRegular,
    MultipleLinks(u64),
    BadMode(u32),
    BadOwner(u32),
}

impl fmt::Display for TrustReject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TrustReject::NotRegular => write!(f, "not a regular file"),
            TrustReject::MultipleLinks(n) => write!(f, "has {n} hard links (expected 1)"),
            TrustReject::BadMode(m) => write!(f, "insecure mode {:04o}", m),
            TrustReject::BadOwner(u) => write!(f, "untrusted owner uid {u}"),
        }
    }
}

/// Validate already-`fstat`'d metadata against `policy`. Pure and unit-testable.
pub fn check_metadata(meta: &Metadata, policy: &TrustPolicy) -> Result<(), TrustReject> {
    if policy.require_regular && !meta.file_type().is_file() {
        return Err(TrustReject::NotRegular);
    }
    if policy.require_single_link && meta.nlink() != 1 {
        return Err(TrustReject::MultipleLinks(meta.nlink()));
    }

    let mode = meta.mode() & 0o7777;
    if let Some(exact) = policy.mode_exact {
        if mode & 0o777 != exact {
            return Err(TrustReject::BadMode(mode));
        }
    }
    if mode & policy.forbid_mode_bits != 0 {
        return Err(TrustReject::BadMode(mode));
    }

    let owner_ok = match policy.owner {
        OwnerRule::Root => meta.uid() == 0,
        OwnerRule::Any => true,
        OwnerRule::RootOrUid(uid) => meta.uid() == 0 || meta.uid() == uid,
    };
    if !owner_ok {
        return Err(TrustReject::BadOwner(meta.uid()));
    }

    Ok(())
}

/// Open `path` `O_RDONLY|O_NOFOLLOW|O_NONBLOCK`, `fstat` the resulting handle,
/// and enforce `policy`. Returns the open file (positioned at offset 0) and its
/// metadata, or a human-readable reason on rejection. Opening with `O_NOFOLLOW`
/// means a symlinked entry fails at `open` rather than being followed.
pub fn open_trusted(path: &Path, policy: &TrustPolicy) -> Result<(File, Metadata), String> {
    let file = std::fs::OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK)
        .open(path)
        .map_err(|e| format!("cannot open: {e}"))?;

    let meta = file.metadata().map_err(|e| format!("cannot stat: {e}"))?;

    check_metadata(&meta, policy).map_err(|r| r.to_string())?;

    Ok((file, meta))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::os::unix::fs::PermissionsExt;

    fn uid() -> u32 {
        // SAFETY: getuid never fails.
        unsafe { libc::getuid() }
    }

    #[test]
    fn accepts_regular_0600_owned_by_self() {
        let dir = tempfile::tempdir().unwrap();
        let p = dir.path().join("cronfile");
        fs::write(&p, b"* * * * * echo hi\n").unwrap();
        fs::set_permissions(&p, fs::Permissions::from_mode(0o600)).unwrap();

        let (_f, _m) = open_trusted(&p, &TrustPolicy::crontab_spool(uid())).expect("trusted");
    }

    #[test]
    fn rejects_wrong_mode_for_spool() {
        let dir = tempfile::tempdir().unwrap();
        let p = dir.path().join("cronfile");
        fs::write(&p, b"x").unwrap();
        fs::set_permissions(&p, fs::Permissions::from_mode(0o644)).unwrap();

        let meta = fs::symlink_metadata(&p).unwrap();
        assert_eq!(
            check_metadata(&meta, &TrustPolicy::crontab_spool(uid())),
            Err(TrustReject::BadMode(0o644))
        );
    }

    #[test]
    fn rejects_group_writable_system_crontab() {
        let dir = tempfile::tempdir().unwrap();
        let p = dir.path().join("crontab");
        fs::write(&p, b"x").unwrap();
        fs::set_permissions(&p, fs::Permissions::from_mode(0o662)).unwrap();

        let meta = fs::symlink_metadata(&p).unwrap();
        // Neutralize the owner rule so the mode check is what this test
        // actually exercises: the file is owned by the test user, not root.
        //
        // This used to say `RootOrUid(u32::MAX)`, the old "any owner"
        // sentinel. Once that became `OwnerRule::Any` the sentinel silently
        // reverted to meaning "root or uid 4294967295" -- the test still
        // passed only because `check_metadata` evaluates the mode before the
        // owner, so `BadMode` won regardless. It would have started failing
        // the moment either the check order or the mode changed.
        let policy = TrustPolicy {
            owner: OwnerRule::Any,
            ..TrustPolicy::system_crontab()
        };
        assert_eq!(
            check_metadata(&meta, &policy),
            Err(TrustReject::BadMode(0o662))
        );
    }

    #[test]
    fn rejects_hardlinked_file() {
        let dir = tempfile::tempdir().unwrap();
        let p = dir.path().join("cronfile");
        fs::write(&p, b"x").unwrap();
        fs::set_permissions(&p, fs::Permissions::from_mode(0o600)).unwrap();
        let link = dir.path().join("hardlink");
        fs::hard_link(&p, &link).unwrap();

        let meta = fs::symlink_metadata(&p).unwrap();
        assert_eq!(
            check_metadata(&meta, &TrustPolicy::crontab_spool(uid())),
            Err(TrustReject::MultipleLinks(2))
        );
    }

    #[test]
    fn open_refuses_symlink() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("real");
        fs::write(&target, b"x").unwrap();
        fs::set_permissions(&target, fs::Permissions::from_mode(0o600)).unwrap();
        let link = dir.path().join("link");
        std::os::unix::fs::symlink(&target, &link).unwrap();

        // O_NOFOLLOW makes opening the symlink itself fail.
        assert!(open_trusted(&link, &TrustPolicy::crontab_spool(uid())).is_err());
    }

    #[test]
    fn at_spool_accepts_any_owner() {
        // #D15: the at-spool policy accepts any owner because the owner *is*
        // the run-as identity. That used to be encoded as
        // RootOrUid(u32::MAX) -- which happens to be the invalid-uid
        // sentinel -- so this pins the intent rather than the encoding.
        let policy = TrustPolicy::at_spool();
        assert!(matches!(policy.owner, OwnerRule::Any));
    }

    #[test]
    fn crontab_spool_still_restricts_the_owner() {
        // The neighbouring policies must NOT have become permissive.
        let policy = TrustPolicy::crontab_spool(1000);
        assert!(matches!(policy.owner, OwnerRule::RootOrUid(1000)));
        assert!(matches!(
            TrustPolicy::system_crontab().owner,
            OwnerRule::Root
        ));
    }
}
