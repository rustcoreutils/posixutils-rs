//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `.NOTPARALLEL` with prerequisites. POSIX says an application shall not
//! give it any, but bash's debian/rules, which a Debian build runs with this
//! make, has `.NOTPARALLEL: build`. It is accepted, and the run is serial.

use crate::common::Dir;

#[test]
fn notparallel_with_prerequisites_serialises_the_build() {
    let dir = Dir::new("notparallel_prereqs");
    dir.write(
        "Makefile",
        "build: a b\n.NOTPARALLEL: build\na:\n\t@sleep 1; echo a\nb:\n\t@echo b\n",
    );
    dir.make(&["-j2"]).ok("a\nb\n");
}
