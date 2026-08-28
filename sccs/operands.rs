//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Operand expansion and the multi-file header.
//!
//! `paths::expand_operands` already handled the `-` and directory forms, and
//! four utilities used it. The other four hand-rolled a `read_dir` loop that
//! returned members in directory order rather than sorted, so their output
//! ordering was whatever the filesystem happened to hand back.

use std::io::Write;
use std::path::{Path, PathBuf};

use plib::sccsfile::paths;

/// Expand command-line operands into the s-files to process: `-` reads names
/// from standard input, a directory contributes its s-file members in sorted
/// order, and anything else is taken as named.
pub fn expand(files: &[PathBuf]) -> Vec<PathBuf> {
    paths::expand_operands(files)
}

/// Whether a `\n<path>:` banner should precede each file's output.
///
/// POSIX writes the banner "when there is more than one named file, or a
/// directory or standard input is named". The test is therefore mostly on the
/// *operands*, not on how many files they expanded to: a single directory
/// operand naming one s-file still gets a banner, because the user did not
/// name that file.
pub fn wants_banner(files: &[PathBuf], expanded: &[PathBuf]) -> bool {
    expanded.len() > 1 || is_stdin_form(files) || files.iter().any(|f| f.is_dir())
}

/// Whether the operands are the lone `-` that means "read pathnames from
/// standard input".
pub fn is_stdin_form(files: &[PathBuf]) -> bool {
    files.len() == 1 && files[0].as_os_str() == "-"
}

/// Write the `\n<path>:` banner.
pub fn banner(out: &mut dyn Write, path: &Path) -> std::io::Result<()> {
    writeln!(out, "\n{}:", path.display())
}
