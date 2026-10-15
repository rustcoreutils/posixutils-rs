//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! make remembers what it learned of the file system only until a command
//! runs: a file a command created or changed is seen as it is now, even when
//! make looked at it before the command ran. A generated source is the
//! everyday case: `gen` writes `b.in`, and `b.out` is inferred from it,
//! although make already asked whether `b.in` existed when it checked the
//! graph for cycles.
//!
//! GNU make 4.3 keeps what it learned for longer, and fails both cases
//! here: it finds no rule for `b.out`, and does not remake `x`. These pin
//! the behaviour make had before it cached `stat` results at all.

use crate::common::Dir;

const GENERATED_SOURCE: &str = "all: gen b.out\n\t@echo done\n\
gen:\n\t@echo hi > b.in\n\
.SUFFIXES: .in .out\n\
.in.out:\n\t@cp $< $@; echo made $@\n";

#[test]
fn a_source_a_command_created_is_found() {
    let dir = Dir::new("files_generated_source");
    dir.write("Makefile", GENERATED_SOURCE);
    dir.make(&[]).ok("made b.out\ndone\n");
}

#[test]
fn a_source_a_command_created_is_found_under_j() {
    let dir = Dir::new("files_generated_source_j");
    dir.write(
        "Makefile",
        &GENERATED_SOURCE.replace("all: gen b.out", "all: gen .WAIT b.out"),
    );
    dir.make(&["-j4"]).ok("made b.out\ndone\n");
}

/// Set the modification time of `name` in `dir` to `secs` after the epoch.
fn set_mtime(dir: &Dir, name: &str, secs: u64) {
    let when = std::time::SystemTime::UNIX_EPOCH + std::time::Duration::from_secs(secs);
    std::fs::File::options()
        .write(true)
        .open(dir.path(name))
        .unwrap()
        .set_modified(when)
        .unwrap();
}

/// `z` was judged older than `x` before `y`'s commands ran and touched it;
/// `x` is compared with the `z` there is now, so it is remade.
#[test]
fn a_file_a_command_changed_is_seen_as_changed() {
    let dir = Dir::new("files_changed_by_command");
    dir.write("Makefile", "x: z y\n\t@echo made x\ny: w\n\t@touch z\n");
    for (name, secs) in [("z", 1000), ("y", 2000), ("w", 3000), ("x", 4000)] {
        dir.write(name, "");
        set_mtime(&dir, name, secs);
    }
    dir.make(&["x"]).ok("made x\n");
}
