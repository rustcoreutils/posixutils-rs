//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Uniform diagnostics.
//!
//! The same "not an SCCS file" message existed at seven sites with three
//! different prefixes. `prs` and `rmdel` emitted no utility prefix at all, so
//! a message from a `sccs` front-end subcommand could not be attributed to the
//! utility that produced it. POSIX 1.4 requires the utility name on a
//! diagnostic; a bare path is not one.

use std::path::Path;

/// Write `utility: message` to standard error.
pub fn error(utility: &str, message: &str) {
    eprintln!("{}: {}", utility, message);
}

/// Write `utility: path: message` to standard error, the shape almost every
/// SCCS diagnostic wants.
pub fn error_path(utility: &str, path: &Path, message: &str) {
    eprintln!("{}: {}: {}", utility, path.display(), message);
}

/// Write `utility: warning: path: message` to standard error.
pub fn warn_path(utility: &str, path: &Path, message: &str) {
    eprintln!("{}: warning: {}: {}", utility, path.display(), message);
}
