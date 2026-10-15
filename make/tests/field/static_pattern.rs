//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU make's static pattern rules, `targets: target-pattern:
//! prereq-patterns`, which bzip2's Makefile builds its objects with:
//! `$(OBJS) bzip2.o bzip2recover.o: %.o: %.c`.

use crate::common::Dir;

#[test]
fn each_target_gets_its_own_prerequisites() {
    let dir = Dir::new("static_basic");
    dir.write(
        "Makefile",
        "OBJS = a.o b.o\nall: $(OBJS)\n\
         $(OBJS): %.o: %.c\n\t@echo cc -o $@ -c $< [$^]\n",
    );
    dir.write("a.c", "");
    dir.write("b.c", "");
    dir.make(&[])
        .ok("cc -o a.o -c a.c [a.c]\ncc -o b.o -c b.c [b.c]\n");
}

// bzip2: the target list is itself a substitution reference.
#[test]
fn bzip2_shared_objects() {
    let dir = Dir::new("static_bzip2");
    dir.write(
        "Makefile",
        "OBJS= blocksort.o  \\\n      huffman.o\n\
         libbz2.so: $(OBJS:%.o=%.sho)\n\t@echo link $^\n\
         $(OBJS:%.o=%.sho) bzip2.sho: %.sho: %.c\n\
         \t@echo cc -fPIC -o $@ -c $<\n",
    );
    for f in ["blocksort.c", "huffman.c", "bzip2.c"] {
        dir.write(f, "");
    }
    dir.make(&[])
        .ok("cc -fPIC -o blocksort.sho -c blocksort.c\n\
         cc -fPIC -o huffman.sho -c huffman.c\n\
         link blocksort.sho huffman.sho\n");
    dir.make(&["bzip2.sho"])
        .ok("cc -fPIC -o bzip2.sho -c bzip2.c\n");
}

#[test]
fn several_prerequisite_patterns_and_plain_names() {
    let dir = Dir::new("static_several");
    dir.write(
        "Makefile",
        "x.out y.out: %.out: %.in %.h common.h\n\t@echo $@: $^\n\
         x.in y.in x.h y.h common.h:\n\t@:\n",
    );
    dir.make(&["x.out", "y.out"])
        .ok("x.out: x.in x.h common.h\ny.out: y.in y.h common.h\n");
}

#[test]
fn the_stem_is_star() {
    let dir = Dir::new("static_stem");
    dir.write(
        "Makefile",
        "lib/a.o: lib/%.o: src/%.c\n\t@echo [$*] [$<]\nsrc/%.c:\n\t@:\n",
    );
    dir.make(&[]).ok("[a] [src/a.c]\n");
}

// A static pattern rule is an ordinary target rule for each target: the
// first one is the default target, and an up-to-date one is left alone.
#[test]
fn a_static_pattern_target_is_a_target() {
    let dir = Dir::new("static_default");
    dir.write("Makefile", "a.o: %.o: %.c\n\t@echo cc $<\n\t@touch $@\n");
    dir.write("a.c", "");
    dir.make(&[]).ok("cc a.c\n");
    let run = dir.make(&[]);
    assert_eq!(run.code, Some(0), "{run:?}");
    assert!(!run.stdout.contains("cc a.c"), "{run:?}");
}
