//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! What the C-parsing utilities -- `cflow`, `ctags`, `cxref` -- share.
//!
//! Not compiled into `c17` itself, which reports its own status; this is the
//! home for behaviour the three tools must agree on.

/// The exit status for a utility that parses C through this front end.
///
/// The front end keeps its own error counter, separate from `plib::diag`'s, so
/// a tool that consults only one of them exits 0 after reporting errors. POSIX
/// requires a non-zero status when either has fired. Each of the three carried
/// a byte-identical copy of this.
pub fn exit_code() -> std::process::ExitCode {
    if plib::diag::has_errors() || crate::diag::has_error() != 0 {
        std::process::ExitCode::from(1)
    } else {
        std::process::ExitCode::SUCCESS
    }
}
