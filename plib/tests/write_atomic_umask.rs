//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! `write_atomic`'s created-file mode, which is a function of the umask.
//!
//! These live in their own integration test — a separate process from the
//! `plib` unit tests — because `modestr::mutate` reads the umask by setting it
//! to 0 and putting it back, and a unit test doing that in a neighbouring
//! thread would make a mode assertion here flap. Same reasoning as
//! `tree/tests/tree-tests-umask.rs`.

use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::sync::{Mutex, PoisonError};

use plib::io::{write_atomic, write_atomic_mode};

/// Serializes the umask across the tests in this file.
///
/// Taken with `unwrap_or_else(PoisonError::into_inner)`: a panicking test has
/// already restored the umask through `RestoreUmask`, so the guarded state is
/// not corrupt and poisoning would only turn one failure into three.
static UMASK_SETTER: Mutex<()> = Mutex::new(());

struct RestoreUmask(libc::mode_t);

impl Drop for RestoreUmask {
    fn drop(&mut self) {
        unsafe { libc::umask(self.0) };
    }
}

/// Run `body` under `mask`, in a scratch directory, restoring the previous
/// umask even if `body` panics.
///
/// `setup` runs in that directory under the *ambient* umask, before `mask` is
/// applied — a fixture created under `umask 0o777` would be mode 0 and nothing
/// could be written into or beside it. Both phases run under one lock, so a
/// neighbouring test never creates a file while this one holds an exotic mask.
fn with_umask<T>(
    mask: libc::mode_t,
    setup: impl FnOnce(&Path),
    body: impl FnOnce(&Path) -> T,
) -> T {
    let _guard = UMASK_SETTER.lock().unwrap_or_else(PoisonError::into_inner);

    let dir = plib::tmp::tempdir().unwrap();
    setup(dir.path());

    let original = unsafe { libc::umask(mask) };
    // Pessimistically make sure the umask is applied before proceeding.
    unsafe { while libc::umask(mask) != mask {} }
    let _restore = RestoreUmask(original);

    body(dir.path())
}

fn mode_of(path: &Path) -> u32 {
    fs::metadata(path).unwrap().permissions().mode() & 0o7777
}

/// XCU 1.1.1.4: a file a utility creates gets `0o666 & ~umask`.
///
/// The temporary `write_atomic` writes through is created `O_EXCL|0600`, and
/// inheriting *that* is how a fresh `tags` file and a fresh `ar` archive came
/// out `-rw-------`.
#[test]
fn write_atomic_creates_with_umask_derived_mode() {
    for (mask, expected) in [
        (0o002, 0o664),
        (0o022, 0o644),
        (0o077, 0o600),
        (0o000, 0o666),
    ] {
        let (got, content) = with_umask(
            mask,
            |_| {},
            |dir| {
                let path = dir.join("created.bin");
                assert!(!path.exists());
                write_atomic(&path, b"hello").unwrap();
                (mode_of(&path), fs::read(&path).unwrap())
            },
        );

        assert_eq!(
            got, expected,
            "umask {:04o}: created file is {:04o}, want {:04o}",
            mask, got, expected
        );
        assert_eq!(content, b"hello");
    }
}

/// An existing file keeps its own mode, whatever the umask says. The umask
/// applies to *creation*, and a rewrite is not one.
#[test]
fn write_atomic_replacing_ignores_the_umask() {
    let (got, content) = with_umask(
        0o077,
        |dir| {
            let path = dir.join("existing.bin");
            fs::write(&path, b"old").unwrap();
            fs::set_permissions(&path, fs::Permissions::from_mode(0o755)).unwrap();
        },
        |dir| {
            let path = dir.join("existing.bin");
            write_atomic(&path, b"new").unwrap();
            (mode_of(&path), fs::read(&path).unwrap())
        },
    );

    assert_eq!(got, 0o755, "replacing a file must not re-derive its mode");
    assert_eq!(content, b"new");
}

/// An explicit mode is exactly that: neither the umask nor an existing mode
/// gets a say. This is the form `crontab` uses to keep the spool copy 0600.
#[test]
fn write_atomic_mode_is_not_masked() {
    // 0o777 would mask every bit away if the mask had any say here.
    let (fresh, existing) = with_umask(
        0o777,
        |dir| {
            let path = dir.join("existing.bin");
            fs::write(&path, b"old").unwrap();
            fs::set_permissions(&path, fs::Permissions::from_mode(0o644)).unwrap();
        },
        |dir| {
            let (fresh, existing) = (dir.join("fresh.bin"), dir.join("existing.bin"));
            write_atomic_mode(&fresh, b"a", 0o600).unwrap();
            write_atomic_mode(&existing, b"b", 0o600).unwrap();
            (mode_of(&fresh), mode_of(&existing))
        },
    );

    assert_eq!(fresh, 0o600, "explicit mode ignored on a created file");
    assert_eq!(existing, 0o600, "explicit mode ignored on a rewrite");
}
