//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU make's order-only prerequisites, `target: normal | order-only`, which
//! libzstd uses to create its build directories before compiling into them:
//! `$(ZSTD_DYNLIB_DIR)/%.o : %.c | $(ZSTD_DYNLIB_DIR)`.
//!
//! An order-only prerequisite is brought up to date before the target, but
//! being newer does not make the target out of date, and it is not among
//! `$^`, `$+`, `$?` or `$<`.

use crate::common::Dir;
use std::time::{Duration, SystemTime};

fn set_age(dir: &Dir, name: &str, secs: u64) {
    std::fs::File::options()
        .write(true)
        .open(dir.path(name))
        .unwrap()
        .set_modified(SystemTime::UNIX_EPOCH + Duration::from_secs(secs))
        .unwrap();
}

#[test]
fn an_order_only_prerequisite_is_made_first() {
    let dir = Dir::new("oo_first");
    dir.write(
        "Makefile",
        "obj/a.o: a.c | obj\n\t@echo cc $^ -o $@\n\t@touch $@\nobj:\n\t@echo mkdir $@\n\t@mkdir $@\n",
    );
    dir.write("a.c", "");
    dir.make(&[]).ok("mkdir obj\ncc a.c -o obj/a.o\n");
}

#[test]
fn a_newer_order_only_prerequisite_does_not_rebuild() {
    let dir = Dir::new("oo_newer");
    dir.write("Makefile", "out: in | stamp\n\t@echo build [$^] [$?]\n");
    dir.write("in", "");
    dir.write("out", "");
    dir.write("stamp", "");
    set_age(&dir, "in", 1_000_000);
    set_age(&dir, "out", 2_000_000);
    set_age(&dir, "stamp", 3_000_000);
    let run = dir.make(&[]);
    assert_eq!(run.code, Some(0), "{run:?}");
    assert!(!run.stdout.contains("build"), "{run:?}");
    // A missing target is still built, without the order-only name in $^.
    std::fs::remove_file(dir.path("out")).unwrap();
    dir.make(&[]).ok("build [in] [in]\n");
}

#[test]
fn a_pattern_rule_with_order_only_prerequisites() {
    let dir = Dir::new("oo_pattern");
    dir.write(
        "Makefile",
        "D = build\nall: $(D)/x.o $(D)/y.o\n\
         $(D)/%.o: %.c | $(D)\n\t@echo cc $< -o $@ [$^]\n\t@touch $@\n\
         $(D):\n\t@echo mkdir $@\n\t@mkdir -p $@\n",
    );
    dir.write("x.c", "");
    dir.write("y.c", "");
    dir.make(&[])
        .ok("mkdir build\ncc x.c -o build/x.o [x.c]\ncc y.c -o build/y.o [y.c]\n");
}

// libzstd writes the order-only part in a rule of its own.
#[test]
fn order_only_prerequisites_accumulate_across_rules() {
    let dir = Dir::new("oo_accumulate");
    dir.write(
        "Makefile",
        "lib.a: | libdir\nlib.a: a.o\n\t@echo ar $@ $^\nlibdir a.o:\n\t@echo make $@\n",
    );
    dir.make(&[]).ok("make a.o\nmake libdir\nar lib.a a.o\n");
}

#[test]
fn a_static_pattern_rule_with_order_only_prerequisites() {
    let dir = Dir::new("oo_static");
    dir.write(
        "Makefile",
        "a.o b.o: %.o: %.c | dir\n\t@echo cc $^\ndir a.c b.c:\n\t@echo make $@\n",
    );
    dir.make(&["a.o"]).ok("make a.c\nmake dir\ncc a.c\n");
}
