//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Build script for the misc crate.
//!
//! POSIX.1-2024 specifies `test` under two names, `test [expression]` and
//! `[ [expression] ]`, and says an implementation may deliver one binary that
//! reads argv[0] to tell which it is.  That is what `test.rs` does, including
//! the rule that the bracket form requires `]` as its final argument, so `[`
//! is delivered here as a symlink to `test`.
//!
//! Without it the bracket handling in `test.rs` is unreachable in any build
//! output: `[` is not a portable filename character, so nothing creates it by
//! accident, and `find -exec [ ... ] ;` or `xargs [` have no utility to run --
//! the shell built-in of the same name cannot serve them.
//!
//! No `cargo:rerun-if-changed` key is emitted on purpose.  Naming one would
//! make Cargo rerun this script *only* when that path changes; with none, the
//! default applies and the symlink is recreated whenever any file in the
//! package changes.

use std::env;
use std::fs;
use std::path::PathBuf;

/// Names symlinked to the `test` binary.
#[cfg(unix)]
const ALIASES: &[&str] = &["["];

fn main() {
    #[cfg(unix)]
    create_symlinks();
}

#[cfg(unix)]
fn create_symlinks() {
    use std::os::unix::fs::symlink;

    // OUT_DIR is <target_dir>/<profile>/build/<crate>-<hash>/out; the binaries
    // land three levels up, in <target_dir>/<profile>.
    let out_dir = env::var("OUT_DIR").expect("OUT_DIR not set");
    let bin_dir = PathBuf::from(&out_dir)
        .parent()
        .and_then(|p| p.parent())
        .and_then(|p| p.parent())
        .map(|p| p.to_path_buf())
        .expect("Could not determine target directory from OUT_DIR");

    // The profile directory may not exist yet on a first build.
    let _ = fs::create_dir_all(&bin_dir);

    for link_name in ALIASES {
        let link_path = bin_dir.join(link_name);

        // symlink_metadata() rather than exists(), so a dangling symlink left
        // over from a previous build is replaced instead of tripping symlink().
        if link_path.symlink_metadata().is_ok() {
            let _ = fs::remove_file(&link_path);
        }

        // A relative target keeps the link valid if the target directory moves.
        if let Err(e) = symlink("test", &link_path) {
            // A missing alias costs the bracket form, not the build.
            println!(
                "cargo:warning=could not create {} symlink: {}",
                link_name, e
            );
        }
    }
}
