//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::OsString;
use std::io::{self, Write};
use std::os::unix::ffi::OsStrExt;

use clap::Parser;
use gettextrs::gettext;
use plib::diag;

#[derive(Parser)]
#[command(
    version,
    about = gettext("basename - return non-directory portion of a pathname")
)]
struct Args {
    #[arg(allow_hyphen_values = true)]
    pathname: OsString,

    #[arg(allow_hyphen_values = true)]
    suffix: Option<OsString>,
}

/// Compute the basename of `string`, removing `suffix` if present, following the
/// POSIX.1-2024 6-step algorithm operating on raw bytes.
fn basename_bytes(string: &[u8], suffix: Option<&[u8]>) -> Vec<u8> {
    // Step 1: a null string yields a null string (implementation choice; the
    // standard permits "." or null).
    if string.is_empty() {
        return Vec::new();
    }

    // Steps 2 & 3: if the string consists entirely of <slash> characters, the
    // result is a single <slash>. (This also covers the "//" case.)
    if string.iter().all(|&b| b == b'/') {
        return vec![b'/'];
    }

    // Step 4: remove any trailing <slash> characters.
    let mut end = string.len();
    while end > 0 && string[end - 1] == b'/' {
        end -= 1;
    }
    let trimmed = &string[..end];

    // Step 5: remove the prefix up to and including the last <slash>.
    let comp = match trimmed.iter().rposition(|&b| b == b'/') {
        Some(pos) => &trimmed[pos + 1..],
        None => trimmed,
    };

    // Step 6: remove the suffix, but only if it differs from the whole result
    // and is a proper suffix of it.
    let mut result = comp.to_vec();
    if let Some(suf) = suffix {
        if !suf.is_empty() && result != suf && result.ends_with(suf) {
            result.truncate(result.len() - suf.len());
        }
    }

    result
}

fn show_basename(args: &Args) {
    let suffix = args.suffix.as_ref().map(|s| s.as_bytes());
    let result = basename_bytes(args.pathname.as_bytes(), suffix);

    // FUTURE DIRECTIONS: treat an embedded <newline> in the output as an error.
    if result.contains(&b'\n') {
        diag::error(&gettext("result contains a newline character"));
        return;
    }

    let mut out = io::stdout().lock();
    if let Err(e) = out.write_all(&result).and_then(|_| out.write_all(b"\n")) {
        diag::error(&format!("{}: {}", gettext("write error"), e));
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    diag::init_locale("basename");

    let args = Args::parse();

    show_basename(&args);

    std::process::exit(diag::exit_status())
}
