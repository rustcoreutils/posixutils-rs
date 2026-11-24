//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The `-m mrlist` option-argument.
//!
//! `admin` split on whitespace and `delta` split on whitespace *and* commas,
//! so `admin -m "1,2"` recorded one MR named `1,2` while `delta -m "1,2"`
//! recorded two. CSSC 1.4.1 records one in both cases, which settles it: a
//! comma is an ordinary character in an MR number.

use std::ffi::OsStr;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::process::Command;

use gettextrs::gettext;
use plib::sccsfile::MrNumber;

/// Split an optional `-m` option-argument into its MR numbers, none when
/// it is absent. MR numbers are bytes, separated by white space.
pub fn parse(mrlist: Option<&OsStr>) -> Vec<MrNumber> {
    mrlist
        .map(|list| MrNumber::list(list.as_bytes()))
        .unwrap_or_default()
}

/// Run the `v` flag's MR validation program on `mrs`, reporting a failure as
/// `utility: sfile: ...` on standard error. Returns whether the MRs passed.
///
/// `delta` had this inline and `admin`, which must also validate the MRs of
/// the delta it creates (POSIX admin 84102-84106), did not have it at all.
/// `program` is a pathname as the s-file stores it.
pub fn validate(utility: &str, sfile: &Path, program: &[u8], mrs: &[MrNumber]) -> bool {
    let program = OsStr::from_bytes(program);
    let mrs = mrs.iter().map(|mr| OsStr::from_bytes(mr));
    match Command::new(program).args(mrs).status() {
        Ok(status) if status.success() => true,
        Ok(_) => {
            crate::diag::error_path(utility, sfile, &gettext("MR validation failed."));
            false
        }
        Err(e) => {
            crate::diag::error_path(
                utility,
                sfile,
                &format!(
                    "{}: {}",
                    program.to_string_lossy(),
                    plib::diag::io_error_text(&e)
                ),
            );
            false
        }
    }
}
