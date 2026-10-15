//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU make globs each target and prerequisite word holding `*`, `?` or
//! `[` against the files there are, after macro expansion: libzstd's
//! doc/educational_decoder/Makefile writes `HARNESS_FILES=*.c` and
//! `harness: $(HARNESS_FILES)`. The names found replace the word, sorted; a
//! word that matches nothing is kept as written. A macro's value is not
//! globbed, and neither is a command line: the shell does that. Every
//! expectation here was checked against GNU make 4.3.

use crate::common::Dir;

/// A directory holding `a.c`, `b.c`, `c.h` and `.hidden.c`.
fn sources(name: &str) -> Dir {
    let dir = Dir::new(name);
    for file in ["a.c", "b.c", "c.h", ".hidden.c"] {
        dir.write(file, "");
    }
    dir
}

// libzstd's doc/educational_decoder/Makefile.
#[test]
fn a_prerequisite_is_globbed_after_expansion() {
    let dir = sources("glob_prereq");
    dir.write(
        "Makefile",
        "F = *.c\nall: $(F)\n\t@echo '[$^]' '[$(F)]' '[$<]' '$(F)'\n",
    );
    dir.make(&[]).ok("[a.c b.c] [*.c] [a.c] *.c\n");
}

#[test]
fn question_marks_and_brackets() {
    let dir = sources("glob_class");
    dir.write(
        "Makefile",
        "all: ?.h [ab].c [!a].c [a-b].c\n\t@echo '[$^]'\n",
    );
    dir.make(&[]).ok("[c.h a.c b.c]\n");
}

#[test]
fn a_word_matching_nothing_is_kept() {
    let dir = sources("glob_nomatch");
    dir.write("Makefile", "all: none*.z\n\t@echo '[$^]'\n");
    let run = dir.make(&[]);
    assert_eq!(run.code, Some(2), "{run:?}");
    assert_eq!(
        run.stderr,
        "make: *** No rule to make target 'none*.z', needed by 'all'.  Stop.\n"
    );
}

// An escaped wildcard is matched literally, and when nothing matches, the
// word is kept with its backslash.
#[test]
fn an_escaped_wildcard_is_literal() {
    let dir = sources("glob_escaped");
    dir.write(
        "Makefile",
        "all: \\*.c x\\[a].c\n\t@echo '[$^]'\n\\*.c x\\[a].c:\n\t@echo 'making $@'\n",
    );
    dir.make(&[])
        .ok("making \\*.c\nmaking x\\[a].c\n[\\*.c x\\[a].c]\n");
    dir.write("*.c", "");
    dir.write("Makefile", "all: \\*.c\n\t@echo '[$^]'\n");
    dir.make(&[]).ok("[*.c]\n");
}

// Quotes mean nothing to make.
#[test]
fn a_quoted_wildcard_is_not_special() {
    let dir = sources("glob_quoted");
    dir.write("Makefile", "all: \"*.c\"\n\t@echo '[$^]'\n");
    let run = dir.make(&[]);
    assert_eq!(
        run.stderr,
        "make: *** No rule to make target '\"*.c\"', needed by 'all'.  Stop.\n"
    );
}

#[test]
fn in_a_directory_and_in_order_only_prerequisites() {
    let dir = sources("glob_dir");
    dir.write("sub/z.c", "")
        .write("sub/y.c", "")
        .write("sub/.x.c", "");
    dir.write(
        "Makefile",
        "all: sub/*.c | *.h\n\t@echo '[$^]'\nc.h: FORCE\n\t@echo 'made $@'\nFORCE:\n",
    );
    dir.make(&[]).ok("made c.h\n[sub/y.c sub/z.c]\n");
}

// A target is globbed too: `*.h:` is a rule for c.h.
#[test]
fn a_target_is_globbed() {
    let dir = sources("glob_target");
    dir.write("Makefile", "[ab].c *.h: FORCE ; @echo 'T=$@'\nFORCE:\n");
    dir.make(&["a.c"]).ok("T=a.c\n");
    dir.make(&[]).ok("T=a.c\n");
    dir.make(&["c.h"]).ok("T=c.h\n");
}

// `%` rules and `$(wildcard)` are untouched; a pattern rule's own
// prerequisites are globbed where it is read.
#[test]
fn pattern_rules_and_the_wildcard_function() {
    let dir = sources("glob_pattern");
    dir.write(
        "Makefile",
        "all: z.o\n\t@echo '$(wildcard [ab].c)'\n%.o: *.h %.c\n\t@echo '[$^]'\n",
    );
    dir.write("z.c", "");
    dir.make(&[]).ok("[c.h z.c]\na.c b.c\n");
}
