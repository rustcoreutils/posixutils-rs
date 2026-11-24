//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shared implementation for the SCCS utilities.
//!
//! The ten SCCS utilities are separate binaries, so before this library
//! existed they could share code only through `plib::sccsfile`. Anything that
//! was a command-line concern rather than a file-format concern therefore got
//! copied from one binary to the next, and the copies drifted: two functions
//! named `delta_after_cutoff` with opposite boundary handling, two MR-list
//! splitters that disagreed on commas, three hand-rolled directory walks that
//! returned members in a different order than the shared one.
//!
//! What belongs here is the part of an SCCS utility that is *not* about
//! parsing an s-file: diagnostics, option-argument grammar, the p-file and
//! z-file protocols, and writing the derived files with the right mode.
//! Anything that interprets the s-file itself belongs in `plib::sccsfile`.

pub mod cutoff;
pub mod diag;
pub mod idkw;
pub mod mrlist;
pub mod operands;
pub mod pfile;
pub mod protect;
pub mod sfio;
pub mod zlock;

/// The login name to stamp into p-files and delta entries.
///
/// Five binaries wrapped `plib::sccsfile::real_login_name` in a private
/// one-line helper, under two different names.
pub fn username() -> String {
    plib::sccsfile::real_login_name()
}

/// A delta comment as the lines the s-file stores, one `^Ac` record each.
///
/// `admin` and `delta` each stored the `-y` text as a single record, so the
/// lines after the first went into the delta table as bare text, which every
/// reader skips and the next rewrite of the file deletes. A final newline
/// ends the last line rather than starting an empty one.
pub fn comment_lines(text: &str) -> Vec<Vec<u8>> {
    let text = text.strip_suffix('\n').unwrap_or(text);
    text.split('\n').map(|l| l.as_bytes().to_vec()).collect()
}
