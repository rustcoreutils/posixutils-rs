//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shared exec-failure handling for the utility-launcher family
//! (`env`, `nice`, `newgrp`, …), which use
//! `std::os::unix::process::CommandExt::exec` to replace the process image.
//! `exec` only returns on failure, and POSIX mandates distinct exit statuses
//! for that failure:
//!
//! - **127** — the utility could not be found.
//! - **126** — the utility was found but could not be invoked.
//!
//! A plain `?`-propagation of the `io::Error` out of `main` would instead
//! print the `Debug` form and exit `1`, losing the 126/127 distinction.

use std::io;
use std::process;

use gettextrs::gettext;

use crate::diag;

/// Map a failed `Command::exec()` error to the POSIX-mandated exit status and
/// terminate the process. Emits a localized diagnostic via [`crate::diag`]
/// (so the caller must have run [`crate::diag::init_locale`] first), then:
///
/// - `ErrorKind::NotFound` → exit **127**
/// - anything else (e.g. `PermissionDenied`) → exit **126**
///
/// `name` is the utility the caller tried to exec, used in the diagnostic.
pub fn exec_error_exit(name: &str, e: io::Error) -> ! {
    match e.kind() {
        io::ErrorKind::NotFound => {
            diag::error(&format!("{}: {}", name, gettext("command not found")));
            process::exit(127);
        }
        _ => {
            diag::error(&format!("{}: {}", name, gettext("cannot execute")));
            process::exit(126);
        }
    }
}
