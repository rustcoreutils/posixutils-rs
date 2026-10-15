//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A long chain of prerequisites: `t0: t1`, `t1: t2`, and so on. make
//! walks it depth first, a level of recursion for each link, and a chain of
//! ten thousand overflowed the stack and aborted where GNU make builds it.

use crate::common::Dir;

const LINKS: usize = 20_000;

fn chain_makefile() -> String {
    let mut text = String::new();
    for i in 0..LINKS - 1 {
        text.push_str(&format!("t{i}: t{}\n", i + 1));
    }
    text.push_str(&format!("t{}:\n\t@echo bottom\n", LINKS - 1));
    text
}

#[test]
fn a_long_chain_is_built() {
    let dir = Dir::new("deep_chain");
    dir.write("Makefile", &chain_makefile());
    dir.make(&[]).ok("bottom\n");
}

/// Under `-j` the walk may continue on another thread, which needs as
/// much room as the first.
#[test]
fn a_long_chain_is_built_under_j() {
    let dir = Dir::new("deep_chain_j");
    let mut text = chain_makefile();
    // Two prerequisites at the top, so a second thread is started.
    text.insert_str(0, "top: t0 side\nside:\n\t@:\n");
    dir.write("Makefile", &text);
    dir.make(&["-j2"]).ok("bottom\n");
}
