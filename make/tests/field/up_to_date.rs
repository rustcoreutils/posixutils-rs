//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The message for a goal that needed nothing done. POSIX lets make write
//! one and leaves its text unspecified; it is GNU make's, written only when
//! no command was shown or run for the goal, and never under `-s`, a
//! `.SILENT` with no prerequisites, or `-q`.

use crate::common::Dir;

/// A file that is newer than its prerequisite, made by a rule with commands.
fn fresh_target(name: &str) -> Dir {
    let dir = Dir::new(name);
    dir.write(
        "Makefile",
        "ab: dep\n\techo hi > ab\ndep:\nall: ab\nnorule:\nempty: ;\nphon:\n.PHONY: phon\n",
    )
    .write("dep", "")
    .write("ab", "");
    let old = std::time::SystemTime::UNIX_EPOCH + std::time::Duration::from_secs(1 << 30);
    std::fs::File::options()
        .write(true)
        .open(dir.path("dep"))
        .unwrap()
        .set_modified(old)
        .unwrap();
    dir
}

/// A goal with commands is "up to date", quoted with two straight quotes.
#[test]
fn a_goal_with_commands_is_up_to_date() {
    let dir = fresh_target("utd_commands");
    dir.make(&["ab"]).ok("make: 'ab' is up to date.\n");
    dir.make(&["empty"]).ok("make: 'empty' is up to date.\n");
    dir.make(&["ab", "ab"])
        .ok("make: 'ab' is up to date.\nmake: 'ab' is up to date.\n");
}

/// A goal with no commands of its own, or a phony one, has "nothing to be
/// done", whether it is a file or not.
#[test]
fn a_goal_without_commands_has_nothing_to_be_done() {
    let dir = fresh_target("utd_no_commands");
    dir.make(&["all"])
        .ok("make: Nothing to be done for 'all'.\n");
    dir.make(&["norule"])
        .ok("make: Nothing to be done for 'norule'.\n");
    dir.make(&["dep"])
        .ok("make: Nothing to be done for 'dep'.\n");
    dir.make(&["phon"])
        .ok("make: Nothing to be done for 'phon'.\n");
    dir.make(&["Makefile"])
        .ok("make: Nothing to be done for 'Makefile'.\n");
}

/// Commands from a pattern rule, or from `.DEFAULT`, are the goal's own.
#[test]
fn borrowed_commands_are_the_goals_own() {
    let dir = Dir::new("utd_borrowed");
    dir.write(
        "Makefile",
        "%.o: %.c\n\tcp $< $@\n.DEFAULT:\n\techo default $@\n",
    )
    .write("x.c", "")
    .write("x.o", "")
    .write("y.c", "");
    dir.make(&["x.o"]).ok("make: 'x.o' is up to date.\n");
    dir.make(&["y.c"]).ok("make: 'y.c' is up to date.\n");
}

/// `-s`, given or inherited, and `.SILENT:` silence the message.
#[test]
fn silent_writes_no_message() {
    let dir = fresh_target("utd_silent");
    for goal in ["ab", "all", "norule", "empty", "phon", "dep"] {
        dir.make(&["-s", goal]).ok("");
    }
    dir.make_env(&["ab"], &[("MAKEFLAGS", "s")]).ok("");
    dir.write("M2", "ab: ;\n.SILENT:\n");
    dir.make(&["-f", "M2", "ab"]).ok("");
}

/// A `.SILENT` that names the goal silences its commands, not the message.
#[test]
fn silent_for_one_target_keeps_the_message() {
    let dir = Dir::new("utd_silent_one");
    dir.write("Makefile", "x: ;\n.SILENT: x\n").write("x", "");
    dir.make(&["x"]).ok("make: 'x' is up to date.\n");
}

/// `-q` prints nothing; `-n` and `-t` still say there was nothing to do.
#[test]
fn question_dry_run_and_touch() {
    let dir = fresh_target("utd_q_n_t");
    dir.make(&["-q", "ab"]).ok("");
    dir.make(&["-n", "ab"]).ok("make: 'ab' is up to date.\n");
    dir.make(&["-t", "ab"]).ok("make: 'ab' is up to date.\n");
}

/// A goal whose build showed or ran a command gets no message.
#[test]
fn a_goal_that_ran_commands_gets_no_message() {
    let dir = Dir::new("utd_ran");
    dir.write(
        "Makefile",
        "st: stale\nstale:\n\t@echo made\nphc:\n\t@:\n.PHONY: phc\n",
    )
    .write("st", "");
    dir.make(&["st"]).ok("made\n");
    dir.make(&["phc"]).ok("");
}

/// A sub-make inherits `-s` through `MAKEFLAGS`.
#[test]
fn a_sub_make_inherits_silence() {
    let dir = fresh_target("utd_sub_make");
    dir.write("M3", "sub:\n\t@$(MAKE) -f Makefile ab\n");
    dir.make(&["-s", "-f", "M3", "sub"]).ok("");
}
