//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A `$$` in a rule header is one `$` in the name it makes.
//!
//! POSIX 105799-105800: `$$` stands for a single `$`. A rule header is
//! expanded once, as it is read, so `a$$b:` names the file `a$b`, and a
//! macro whose value is `c$$$$d` gives the name `c$$d`. Every expected
//! output here is GNU make 4.3's.

use crate::common::Dir;

/// Targets, prerequisites, and the automatic macros that name them.
#[test]
fn a_header_dollar_pair_names_one_dollar() {
    let dir = Dir::new("hdr_dollar_names");
    dir.write(
        "Makefile",
        "all: a$$b c$$$$d\n\t@echo '[$^] [$<]'\n\
         a$$b c$$$$d: ; @echo '[$@]'\n",
    );
    dir.make(&[]).ok("[a$b]\n[c$$d]\n[a$b c$$d] [a$b]\n");
}

/// A macro's value is expanded once in a header, and the result is not
/// unescaped a second time.
#[test]
fn a_macro_value_in_a_header_is_expanded_once() {
    let dir = Dir::new("hdr_dollar_macro");
    dir.write(
        "Makefile",
        "X = c$$$$d\nY := p$$q\nall: $(X) $(Y)\n\t@echo '[$^]'\n\
         c$$$$d: ; @echo '[$@] four'\nc$$d: ; @echo '[$@] two'\n\
         $(Y): ; @echo '[$@]'\n",
    );
    dir.make(&[]).ok("[c$$d] four\n[p$q]\n[c$$d p$q]\n");
}

/// The name is looked up as a file: an existing `a$b` is up to date.
#[test]
fn a_header_dollar_name_is_the_file_looked_up() {
    let dir = Dir::new("hdr_dollar_file");
    dir.write("a$b", "").write(
        "Makefile",
        "all: a$$b\n\t@echo done\na$$b:\n\t@echo built\n",
    );
    dir.make(&[]).ok("done\n");
}

/// A goal named on the command line finds the target.
#[test]
fn a_header_dollar_target_is_found_as_a_goal() {
    let dir = Dir::new("hdr_dollar_goal");
    dir.write("Makefile", "all: ; @echo all\na$$b: ; @echo '[$@]'\n");
    dir.make(&["a$b"]).ok("[a$b]\n");
}

/// Special targets name the same target as the rule.
#[test]
fn special_targets_see_the_header_dollar_name() {
    let dir = Dir::new("hdr_dollar_special");
    dir.write("a$b", "").write(
        "Makefile",
        ".PHONY: a$$b\n.SILENT: a$$b\na$$b: ; echo '[$@]'\n",
    );
    dir.make(&[]).ok("[a$b]\n");
}

/// Pattern rules, both a goal matched against `%.x$` and a chain of them.
#[test]
fn pattern_rules_match_header_dollar_names() {
    let dir = Dir::new("hdr_dollar_pattern");
    dir.write(
        "Makefile",
        "all: foo.x$$\n%.x$$: %.y$$ ; @echo '[$@] [$*] [$<]'\n\
         %.y$$: ; @echo '[$@]'\n",
    );
    dir.make(&[]).ok("[foo.y$]\n[foo.x$] [foo] [foo.y$]\n");
    dir.make(&["bar.y$"]).ok("[bar.y$]\n");
}

/// Static pattern parts, order-only prerequisites, double-colon rules and
/// archive members.
#[test]
fn every_header_part_unescapes_its_dollars() {
    let dir = Dir::new("hdr_dollar_parts");
    dir.write("a$1$.c", "").write(
        "Makefile",
        "all: a$$1.o t lib$$.a(m$$.o) d$$\n\
         a$$1.o: %.o: %$$.c ; @echo '[$@] [$<] [$*]'\n\
         t: | o$$d ; @echo '[$@]'\no$$d: ; @echo '[$@]'\n\
         lib$$.a(m$$.o): ; @echo '[$@] [$%]'\n\
         d$$:: ; @echo '1 [$@]'\nd$$:: ; @echo '2 [$@]'\n",
    );
    dir.make(&[])
        .ok("[a$1.o] [a$1$.c] [a$1]\n[o$d]\n[t]\n[lib$.a] [m$.o]\n1 [d$]\n2 [d$]\n");
}

/// The target list of a target-specific assignment names the same targets.
#[test]
fn a_target_variable_list_unescapes_its_dollars() {
    let dir = Dir::new("hdr_dollar_target_var");
    dir.write(
        "Makefile",
        "X = u$$$$v\nall: t$$v u$$$$v\nt$$v: V = hi\n$(X): V = ho\n\
         t$$v u$$$$v: ; @echo '[$@] [$(V)]'\n",
    );
    dir.make(&[]).ok("[t$v] [hi]\n[u$$v] [ho]\n");
}

/// A header word is globbed with its `$` in place, and a `\*` still
/// matches only a literal `*`.
#[test]
fn a_header_dollar_word_is_globbed_as_a_name() {
    let dir = Dir::new("hdr_dollar_glob");
    dir.write("g$1.txt", "")
        .write("g$2.txt", "")
        .write("lit*.c", "")
        .write("litA.c", "")
        .write("Makefile", "all: g$$*.txt lit\\*.c\n\t@echo '[$^]'\n");
    dir.make(&[]).ok("[g$1.txt g$2.txt lit*.c]\n");
}

/// `-p` prints a target by its name, `a$b:`, as GNU make does.
#[test]
fn the_database_prints_the_header_dollar_name() {
    let dir = Dir::new("hdr_dollar_database");
    dir.write("Makefile", "all: a$$b\na$$b: ; @:\n");
    let run = dir.make(&["-p"]);
    assert_eq!(run.code, Some(0), "make failed: {run:?}");
    let lines: Vec<&str> = run.stdout.lines().collect();
    assert!(lines.contains(&"a$b:"), "{}", run.stdout);
    assert!(lines.contains(&"all: a$b"), "{}", run.stdout);
    assert!(!run.stdout.contains("a$$b:"), "{}", run.stdout);
}
