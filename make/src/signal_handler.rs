//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs::{self, remove_file};

use crate::rule::INTERRUPT_FLAG;
use gettextrs::gettext;
use libc::{raise, signal, SIGHUP, SIGINT, SIGQUIT, SIGTERM, SIG_DFL};

/// Handles incoming signals: deletes the partially built target (when
/// appropriate), then resets the signal to its default action and re-raises it
/// so the process dies from the signal and the parent observes a signal death.
///
/// NOTE: a strictly async-signal-safe handler would only set an atomic flag (or
/// write to a self-pipe) and do the cleanup/printing from normal control flow.
/// That is a larger redesign; here the handler does the cleanup directly but
/// uses `try_lock` on `INTERRUPT_FLAG` so it can never deadlock against the
/// build thread that briefly holds that lock while recording the active target.
pub fn handle_signals(signal_code: libc::c_int) {
    // `try_lock` (never `lock`): if the interrupted thread happens to hold the
    // lock, skip cleanup rather than deadlock — re-raising still terminates make.
    if let Some(info) = INTERRUPT_FLAG.try_lock().ok().and_then(|g| g.clone()) {
        eprintln!("{}", gettext("make: Interrupt"));

        // POSIX: delete the target only if it is not a `.PRECIOUS`/`.PHONY`
        // prerequisite and its modification time changed while the interrupted
        // recipe ran (i.e. the recipe had begun writing a partial file).
        let current_mtime = fs::metadata(&info.target)
            .ok()
            .and_then(|m| m.modified().ok());
        let mtime_changed = current_mtime != info.original_mtime;

        if !info.precious && !info.phony && mtime_changed {
            eprintln!(
                "{}: {} '{}'",
                gettext("make"),
                gettext("Deleting file"),
                info.target
            );
            if let Err(err) = remove_file(&info.target) {
                eprintln!("{}: {}", gettext("Error deleting file"), err);
            }
        }
    }

    // Reset to default and re-raise so the parent sees a signal death.
    unsafe {
        signal(signal_code, SIG_DFL);
        raise(signal_code);
    }
}

pub fn register_signals() {
    unsafe {
        signal(SIGINT, handle_signals as *const () as usize);
        signal(SIGQUIT, handle_signals as *const () as usize);
        signal(SIGTERM, handle_signals as *const () as usize);
        signal(SIGHUP, handle_signals as *const () as usize);
    }
}
