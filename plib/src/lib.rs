//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

pub mod archive;
pub mod cscan;
pub mod curuser;
pub mod diag;
pub mod exec;
pub mod group;
pub mod io;
pub mod linediff;
pub mod locale;
pub mod lzw;
pub mod modestr;
pub mod platform;
pub mod priority;
pub mod projectdir;
pub mod regex;
pub mod sccsfile;
pub mod syslog;
pub mod test_expr;
pub mod testing;
pub mod tmp;
pub mod tty;
pub mod user;
pub mod utmpx;

pub const BUFSZ: usize = 8 * 1024;

/// Serializes tests that depend on the process-global locale.
///
/// `setlocale` has no scope: a test that switches to a UTF-8 locale changes it
/// for every thread, and the test harness runs threads in parallel. Tests that
/// *mutate* the locale have always taken this lock. Tests that merely depend on
/// it being unchanged have to take it too, which is the half that was missing:
/// `regex`'s byte-oriented tests match an invalid-UTF-8 byte through `regexec`,
/// which only behaves that way in the C locale, and failed intermittently when
/// they happened to run beside a `locale` test holding the process in UTF-8.
///
/// Recovers from a poisoned lock — the guarded data is `()`, so a panic in a
/// prior holder leaves it perfectly usable.
#[cfg(test)]
pub(crate) fn locale_test_lock() -> std::sync::MutexGuard<'static, ()> {
    static LOCALE_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
    LOCALE_LOCK.lock().unwrap_or_else(|e| e.into_inner())
}
