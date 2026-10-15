//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A target with no rule that does not exist. debhelper decides whether a
//! package's makefile has a `test`, `check` or `distclean` target by running
//! `$(MAKE) -s -n --no-print-directory TARGET` and matching GNU make's
//! `*** No rule to make target 'TARGET'` in what it writes
//! (`exists_make_target` in Debian's makefile.pm). Any other wording reads
//! as "the target exists", and dh then runs a `make test` that fails.

use crate::common::Dir;

#[test]
fn a_missing_goal_is_reported_in_gnu_wording() {
    let dir = Dir::new("norule_goal");
    dir.write("Makefile", "all:\n\techo all\n");
    let run = dir.make(&["-s", "-n", "--no-print-directory", "test"]);
    assert_eq!(run.stdout, "");
    assert_eq!(
        run.stderr,
        "make: *** No rule to make target 'test'.  Stop.\n"
    );
    assert_eq!(run.code, Some(2));
}

#[test]
fn a_missing_prerequisite_names_its_dependent() {
    let dir = Dir::new("norule_prereq");
    dir.write("Makefile", "x: y\n\t@echo x\ny: z\n\t@echo y\n");
    let run = dir.make(&[]);
    assert_eq!(run.stdout, "");
    assert_eq!(
        run.stderr,
        "make: *** No rule to make target 'z', needed by 'y'.  Stop.\n"
    );
    assert_eq!(run.code, Some(2));
}

#[test]
fn under_k_make_does_not_say_it_stops() {
    let dir = Dir::new("norule_k");
    dir.write("Makefile", "all: foo bar\n\t@echo all\nbar:\n\t@echo bar\n");
    let run = dir.make(&["-k"]);
    assert_eq!(run.stdout, "bar\n");
    assert!(
        run.stderr
            .starts_with("make: *** No rule to make target 'foo', needed by 'all'.\n"),
        "{run:?}"
    );
    assert_eq!(run.code, Some(2));
}

/// POSIX 105672: a prerequisite of `.PHONY` is a target itself, so one with
/// no rule is a target with no commands, not a missing file. binutils'
/// top-level Makefile has `.PHONY: all-stage1 clean-stage1` and `do-clean:
/// clean-stage1` with no rule for `clean-stage1`.
#[test]
fn a_phony_prerequisite_needs_no_rule() {
    let dir = Dir::new("norule_phony");
    dir.write(
        "Makefile",
        ".PHONY: all-stage1 clean-stage1\nclean: do-clean\ndo-clean: clean-stage1\n\t@echo do-clean\n",
    );
    dir.make(&["clean"]).ok("do-clean\n");
    let run = dir.make(&["clean-stage1"]);
    assert_eq!((run.code, run.stderr.as_str()), (Some(0), ""), "{run:?}");
}

/// A phony target with no rule is always out of date, so what depends on it
/// is remade even when its file is newer, and `.DEFAULT`, which is for a
/// target with no rule, does not run for it (GNU make agrees).
#[test]
fn a_phony_prerequisite_with_no_rule_remakes_its_dependent() {
    let dir = Dir::new("norule_phony_remade");
    dir.write(
        "Makefile",
        ".PHONY: a\nall: a\n\t@echo all\n.DEFAULT:\n\t@echo default $@\n",
    );
    dir.write("all", "");
    dir.make(&[]).ok("all\n");
}
