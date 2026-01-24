//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Build script for editors crate.
//!
//! Creates a symlink from `ex` to `vi` in the target directory so that
//! tests can run `ex` as a command. The vi binary detects invocation via
//! argv[0] and switches to ex mode when invoked as "ex".

use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    // Only run symlink creation on Unix-like systems
    #[cfg(unix)]
    create_ex_symlink();

    // Rerun if this script changes
    println!("cargo:rerun-if-changed=build.rs");
}

#[cfg(unix)]
fn create_ex_symlink() {
    // Use OUT_DIR to determine the actual target directory
    // OUT_DIR is like: <target_dir>/<profile>/build/<crate>-<hash>/out
    // We need to go up to: <target_dir>/<profile>/
    let out_dir = env::var("OUT_DIR").expect("OUT_DIR not set");
    let bin_dir = PathBuf::from(&out_dir)
        .parent() // out
        .and_then(|p| p.parent()) // <crate>-<hash>
        .and_then(|p| p.parent()) // build
        .and_then(|p| p.parent()) // <profile>
        .map(|p| p.to_path_buf())
        .expect("Could not determine target directory from OUT_DIR");

    // Create the target directory if it doesn't exist
    // (it may not exist on first build)
    let _ = fs::create_dir_all(&bin_dir);

    let ex_path = bin_dir.join("ex");
    let vi_target = "vi"; // Relative symlink

    // Remove existing symlink or file if present
    if ex_path.exists() || ex_path.symlink_metadata().is_ok() {
        let _ = fs::remove_file(&ex_path);
    }

    // Create the symlink
    #[cfg(unix)]
    {
        use std::os::unix::fs::symlink;
        match symlink(vi_target, &ex_path) {
            Ok(()) => {
                eprintln!("Created symlink: {:?} -> {}", ex_path, vi_target);
            }
            Err(e) => {
                // Don't fail the build, just warn
                eprintln!("Warning: Could not create ex symlink: {}", e);
            }
        }
    }
}
