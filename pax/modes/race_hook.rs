//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Test-only pause points inside the windows a concurrent writer of the
//! destination could use.
//!
//! A race between creating a node and applying its attributes cannot be held
//! open from outside the process: nothing pax opens in between can be leased
//! or blocked on. So the unit tests install a hook here, run at the very point
//! a concurrent writer would act, and stage the swap there -- the same thing
//! every time, with no timing involved.

use std::cell::RefCell;
use std::ffi::CStr;

/// Where in pax the hook runs.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Point {
    /// A FIFO, device or symbolic link member has just been created at `name`.
    Made,
}

type Hook = Box<dyn FnMut(Point, libc::c_int, &CStr)>;

thread_local! {
    static HOOK: RefCell<Option<Hook>> = const { RefCell::new(None) };
}

/// pax has reached `point` for `name` below `dirfd`: run the hook, if any.
pub(crate) fn reached(point: Point, dirfd: libc::c_int, name: &CStr) {
    let hook = HOOK.with(|h| h.borrow_mut().take());
    if let Some(mut hook) = hook {
        hook(point, dirfd, name);
        HOOK.with(|h| *h.borrow_mut() = Some(hook));
    }
}

/// Run `f` with `hook` installed on this thread.
pub(crate) fn with_hook<R>(
    hook: impl FnMut(Point, libc::c_int, &CStr) + 'static,
    f: impl FnOnce() -> R,
) -> R {
    HOOK.with(|h| *h.borrow_mut() = Some(Box::new(hook)));
    let result = f();
    HOOK.with(|h| *h.borrow_mut() = None);
    result
}

/// A hook action: replace `name` below `dirfd` with a hard link to `victim`,
/// as a writer of the destination directory would.
pub(crate) fn swap_for_hard_link(dirfd: libc::c_int, name: &CStr, victim: &CStr) {
    assert_eq!(unsafe { libc::unlinkat(dirfd, name.as_ptr(), 0) }, 0);
    let r = unsafe { libc::linkat(libc::AT_FDCWD, victim.as_ptr(), dirfd, name.as_ptr(), 0) };
    assert_eq!(r, 0, "linkat: {}", std::io::Error::last_os_error());
}
