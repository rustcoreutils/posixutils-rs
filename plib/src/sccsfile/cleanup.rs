//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Removing transient files (the z-file, the x-file) on SIGINT.

use std::ffi::CString;
use std::path::Path;
use std::sync::atomic::{AtomicPtr, Ordering};
use std::sync::{Mutex, OnceLock};

/// Maximum number of transient files registered for SIGINT cleanup at once. A
/// mutating SCCS command holds at most a couple (the z-file lock and one x-file
/// temporary), so this bound is generous.
const MAX_CLEANUP: usize = 32;

/// Async-signal-safe slot array of paths to unlink on SIGINT. Each non-null
/// entry points at the NUL-terminated bytes of a [`CString`] kept alive in
/// `CLEANUP_OWNED`. The signal handler reads ONLY this array (atomic loads plus
/// `unlink`/`_exit`, all async-signal-safe) — it never locks a mutex or
/// allocates.
static CLEANUP_SLOTS: [AtomicPtr<libc::c_char>; MAX_CLEANUP] =
    [const { AtomicPtr::new(std::ptr::null_mut()) }; MAX_CLEANUP];

/// Normal-execution bookkeeping that owns the registered `CString`s (so the raw
/// pointers stored in `CLEANUP_SLOTS` stay valid) and records each one's slot.
/// This mutex is locked ONLY from normal control flow (register/unregister) and
/// is NEVER touched by the signal handler.
static CLEANUP_OWNED: OnceLock<Mutex<Vec<(usize, CString)>>> = OnceLock::new();

fn cleanup_owned() -> &'static Mutex<Vec<(usize, CString)>> {
    CLEANUP_OWNED.get_or_init(|| Mutex::new(Vec::new()))
}

/// Convert a path into a NUL-terminated C string for async-signal-safe
/// `unlink`. Returns `None` if the path contains an interior NUL.
fn path_to_cstring(path: &Path) -> Option<CString> {
    use std::os::unix::ffi::OsStrExt;
    CString::new(path.as_os_str().as_bytes()).ok()
}

/// Register `path` to be unlinked on SIGINT.
pub fn register_cleanup(path: &Path) {
    let cstr = match path_to_cstring(path) {
        Some(c) => c,
        None => return,
    };
    // The pointer into the CString's heap buffer stays valid after the CString
    // is moved into the owned list (moving the `Box<[u8]>` does not relocate the
    // allocation).
    let ptr = cstr.as_ptr() as *mut libc::c_char;

    if let Ok(mut owned) = cleanup_owned().lock() {
        for (i, slot) in CLEANUP_SLOTS.iter().enumerate() {
            if slot.load(Ordering::Acquire).is_null() {
                slot.store(ptr, Ordering::Release);
                owned.push((i, cstr));
                return;
            }
        }
        // No free slot: the temp simply isn't covered by signal cleanup (it is
        // still removed by normal-flow unregister / RAII). Not expected in
        // practice.
    }
}

/// Remove `path` from the SIGINT cleanup registry (e.g. after a successful
/// rename, when the temporary no longer exists).
pub fn unregister_cleanup(path: &Path) {
    let cstr = match path_to_cstring(path) {
        Some(c) => c,
        None => return,
    };
    if let Ok(mut owned) = cleanup_owned().lock() {
        if let Some(pos) = owned.iter().position(|(_, c)| *c == cstr) {
            let slot_idx = owned[pos].0;
            // Clear the slot FIRST (Release) so the handler can no longer read
            // the pointer, THEN drop the owning CString (freeing the buffer).
            CLEANUP_SLOTS[slot_idx].store(std::ptr::null_mut(), Ordering::Release);
            owned.remove(pos);
        }
    }
}

/// SIGINT handler: unlink every registered path and exit non-zero. Performs
/// ONLY async-signal-safe operations — atomic loads from `CLEANUP_SLOTS`,
/// `unlink`, and `_exit` — with no mutex acquisition or allocation.
extern "C" fn sigint_cleanup_handler(_sig: libc::c_int) {
    for slot in CLEANUP_SLOTS.iter() {
        let p = slot.load(Ordering::Acquire);
        if !p.is_null() {
            unsafe {
                libc::unlink(p as *const libc::c_char);
            }
        }
    }
    unsafe {
        libc::_exit(130);
    }
}

/// Blocks SIGINT for the lifetime of the guard, restoring the previous mask on
/// drop.
///
/// Used to close the gap between a file existing on disk and the cleanup
/// registry knowing about it. For files written with `fs::write` the gap is
/// avoided by registering first, which is what the x-file paths do. That does
/// not work for a lock taken with `create_new`: if the create then fails
/// because another process holds the lock, an interrupt arriving in between
/// would unlink *their* file. Deferring the signal is exact, and costs one
/// syscall pair.
pub(super) struct SigintBlock {
    saved: libc::sigset_t,
}

impl SigintBlock {
    pub(super) fn new() -> Self {
        unsafe {
            let mut mask: libc::sigset_t = std::mem::zeroed();
            libc::sigemptyset(&mut mask);
            libc::sigaddset(&mut mask, libc::SIGINT);
            let mut saved: libc::sigset_t = std::mem::zeroed();
            // pthread_sigmask rather than sigprocmask: correct in a threaded
            // process too, and identical in a single-threaded one.
            libc::pthread_sigmask(libc::SIG_BLOCK, &mask, &mut saved);
            SigintBlock { saved }
        }
    }
}

impl Drop for SigintBlock {
    fn drop(&mut self) {
        unsafe {
            libc::pthread_sigmask(libc::SIG_SETMASK, &self.saved, std::ptr::null_mut());
        }
    }
}

/// Install the SIGINT handler that unlinks all registered temp/lock files and
/// exits with status 130. Idempotent enough to call once at the start of a
/// mutating command's `main`.
pub fn install_sigint_cleanup() {
    // Ensure the owned registry exists before the handler can fire.
    let _ = cleanup_owned();
    unsafe {
        libc::signal(
            libc::SIGINT,
            sigint_cleanup_handler as *const () as libc::sighandler_t,
        );
    }
}
