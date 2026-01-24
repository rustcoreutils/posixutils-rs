//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Build script for xform crate.
//!
//! Creates symlinks for `uncompress` and `zcat` pointing to `compress` in
//! the target directory so that tests can run these commands. The compress
//! binary detects invocation via argv[0] and adjusts behavior accordingly.

use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    // Only run symlink creation on Unix-like systems
    #[cfg(unix)]
    create_symlinks();

    // Rerun if this script changes
    println!("cargo:rerun-if-changed=build.rs");
}

#[cfg(unix)]
fn create_symlinks() {
    use std::os::unix::fs::symlink;

    // Use OUT_DIR to determine the actual target directory
    // OUT_DIR is like: <target_dir>/<profile>/build/<crate>-<hash>/out
    // We need to go up to: <target_dir>/<profile>/
    let out_dir = env::var("OUT_DIR").expect("OUT_DIR not set");
    let bin_dir = PathBuf::from(&out_dir)
        .parent() // out -> <crate>-<hash>
        .and_then(|p| p.parent()) // <crate>-<hash> -> build
        .and_then(|p| p.parent()) // build -> <profile> (e.g., debug)
        .map(|p| p.to_path_buf())
        .expect("Could not determine target directory from OUT_DIR");

    // Create the target directory if it doesn't exist
    // (it may not exist on first build)
    let _ = fs::create_dir_all(&bin_dir);

    // Create symlinks: uncompress -> compress, zcat -> compress
    for link_name in &["uncompress", "zcat"] {
        let link_path = bin_dir.join(link_name);

        // Remove existing symlink or file if present
        if link_path.exists() || link_path.symlink_metadata().is_ok() {
            let _ = fs::remove_file(&link_path);
        }

        // Create the symlink
        match symlink("compress", &link_path) {
            Ok(()) => {
                eprintln!("Created symlink: {:?} -> compress", link_path);
            }
            Err(e) => {
                // Don't fail the build, just warn
                eprintln!("Warning: Could not create {} symlink: {}", link_name, e);
            }
        }
    }
}
