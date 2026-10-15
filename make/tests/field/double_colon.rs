//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU make's double-colon rules, which perl's ExtUtils::MakeMaker output
//! (`pure_all ::`, `config ::`, `subdirs ::`) and ncurses' Makefiles
//! (`all libs ::`, `clean :: mostlyclean`) are written in.
//!
//! Each `::` rule for a target is independent: its own prerequisites, its
//! own commands, run in order, each only when that rule's prerequisites are
//! newer than the target -- or always, when it has none.

use crate::common::Dir;
use std::time::{Duration, SystemTime};

fn set_age(dir: &Dir, name: &str, secs: u64) {
    dir.write(name, "");
    std::fs::File::options()
        .write(true)
        .open(dir.path(name))
        .unwrap()
        .set_modified(SystemTime::UNIX_EPOCH + Duration::from_secs(secs))
        .unwrap();
}

#[test]
fn every_rule_runs_in_order_with_its_own_prerequisites() {
    let dir = Dir::new("dc_order");
    dir.write(
        "Makefile",
        "all:: b\n\t@echo one $^\nall:: c\n\t@echo two $^ $<\nb c:\n\t@echo making $@\n",
    );
    dir.make(&[]).ok("making b\none b\nmaking c\ntwo c c\n");
}

#[test]
fn a_rule_without_prerequisites_always_runs() {
    let dir = Dir::new("dc_always");
    dir.write("Makefile", "x::\n\t@echo always\nx::\n\t@echo again\n");
    dir.write("x", "");
    dir.make(&[]).ok("always\nagain\n");
}

#[test]
fn only_the_rule_with_a_newer_prerequisite_runs() {
    let dir = Dir::new("dc_newer");
    dir.write(
        "Makefile",
        "out:: old\n\t@echo from old\nout:: new\n\t@echo from new\n",
    );
    set_age(&dir, "old", 1_000_000);
    set_age(&dir, "out", 2_000_000);
    set_age(&dir, "new", 3_000_000);
    dir.make(&[]).ok("from new\n");
}

#[test]
fn a_rule_without_commands_still_brings_its_prerequisites_up_to_date() {
    let dir = Dir::new("dc_no_commands");
    dir.write(
        "Makefile",
        "all:: dep\nall::\n\t@echo all\ndep:\n\t@echo dep\n",
    );
    dir.make(&[]).ok("dep\nall\n");
}

// ncurses: `clean :: mostlyclean` over two `mostlyclean ::` rules.
#[test]
fn ncurses_clean_chain() {
    let dir = Dir::new("dc_ncurses");
    dir.write(
        "Makefile",
        ".PHONY: clean\nclean :: mostlyclean\n\t@echo clean\n\
         mostlyclean ::\n\t@echo mostly1\nmostlyclean ::\n\t@echo mostly2\n",
    );
    dir.make(&["clean"]).ok("mostly1\nmostly2\nclean\n");
}

#[test]
fn double_colon_rules_under_dash_n() {
    let dir = Dir::new("dc_dash_n");
    dir.write(
        "Makefile",
        "all:: b\n\techo one\nall:: c\n\techo two\nb c:\n\techo $@\n",
    );
    dir.make(&["-n"]).ok("echo b\necho one\necho c\necho two\n");
}

// The shape ExtUtils::MakeMaker writes.
#[test]
fn makemaker_shape() {
    let dir = Dir::new("dc_makemaker");
    dir.write(
        "Makefile",
        "NOECHO = @\nNOOP = $(TRUE)\nTRUE = true\n\
         all :: pure_all manifypods\n\t$(NOECHO) $(NOOP)\n\
         pure_all :: config pm_to_blib subdirs linkext\n\t$(NOECHO) $(NOOP)\n\
         subdirs :: $(MYEXTLIB)\n\t$(NOECHO) $(NOOP)\n\
         config :: Makefile blibdirs\n\t$(NOECHO) $(NOOP)\n\
         blibdirs :\n\t$(NOECHO) echo blibdirs\n\
         pm_to_blib :\n\t$(NOECHO) echo pm_to_blib\n\
         linkext :: dynamic\n\t$(NOECHO) $(NOOP)\n\
         dynamic ::\n\t$(NOECHO) echo dynamic\n\
         manifypods : pure_all\n\t$(NOECHO) echo manifypods\n\
         .PHONY: all pure_all subdirs config linkext dynamic manifypods\n",
    );
    dir.write("Makefile.PL", "");
    dir.make(&[])
        .ok("blibdirs\npm_to_blib\ndynamic\nmanifypods\n");
}

// GNU: "target file 'a' has both : and :: entries".
#[test]
fn single_and_double_colon_rules_for_one_target_are_an_error() {
    let dir = Dir::new("dc_mixed");
    dir.write(
        "Makefile",
        "a: x\n\t@echo s\na:: y\n\t@echo d\nx y:\n\t@:\n",
    );
    let run = dir.make(&["a"]);
    assert_ne!(run.code, Some(0), "{run:?}");
    assert_eq!(run.stdout, "");
    assert!(run.stderr.contains("both : and ::"), "{run:?}");
}
