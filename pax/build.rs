//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Build script for the pax crate.
//!
//! `tar` and `cpio` are compatibility front-ends living inside the `pax`
//! binary: they parse their own historic command lines and then run the same
//! internal operations pax does.  Both are delivered as symlinks to `pax` in
//! the target directory; the binary picks the right command-line parser from
//! argv[0] (see `cli::ProgramMode::detect`).
//!
//! No `cargo:rerun-if-changed` key is emitted on purpose.  Naming one would
//! make Cargo rerun this script *only* when that path changes; with none, the
//! default applies and the symlinks are recreated whenever any file in the
//! package changes.

use std::env;
use std::fs;
use std::path::PathBuf;

/// Names symlinked to the `pax` binary.
#[cfg(unix)]
const ALIASES: &[&str] = &["tar", "cpio"];

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
        if let Err(e) = symlink("pax", &link_path) {
            // A missing alias costs the tar/cpio front-end, not the build.
            println!(
                "cargo:warning=could not create {} symlink: {}",
                link_name, e
            );
        }
    }
}
