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
#[cfg(unix)]
pub mod curuser;
pub mod diag;
#[cfg(unix)]
pub mod exec;
#[cfg(unix)]
pub mod group;
pub mod io;
pub mod linediff;
pub mod locale;
pub mod lzw;
#[cfg(unix)]
pub mod modestr;
#[cfg(unix)]
pub mod platform;
#[cfg(unix)]
pub mod priority;
#[cfg(unix)]
pub mod projectdir;
pub mod regex;
#[cfg(unix)]
pub mod sccsfile;
#[cfg(unix)]
pub mod syslog;
#[cfg(unix)]
pub mod test_expr;
pub mod testing;
#[cfg(windows)]
pub mod timefmt;
pub mod tmp;
#[cfg(unix)]
pub mod tty;
#[cfg(unix)]
pub mod user;
#[cfg(unix)]
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
