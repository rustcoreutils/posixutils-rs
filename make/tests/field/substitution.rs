//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX 105804-105810: `$(string1:subst1=subst2)`.

use crate::common::Dir;

// An empty `subst1` is a suffix of every word. automake's test harness
// computes its log names as `$(TESTS:=.log)` and removes them with `rm -f`;
// taking the words back unchanged deleted gzip's test scripts.
#[test]
fn an_empty_suffix_appends() {
    let dir = Dir::new("subst_empty_suffix");
    dir.write(
        "Makefile",
        "TESTS = gzip-env keep.test\nam__test_logs1 = $(TESTS:=.log)\n\
         TEST_LOGS = $(am__test_logs1:.test.log=.log)\nall:\n\t@echo $(TEST_LOGS)\n",
    );
    dir.make(&[]).ok("gzip-env.log keep.log\n");
}

// An internal macro takes a substitution like any other (POSIX 105804).
// automake's `.texi.html` rule writes `$(@:.html=.htp)` and moves it to
// `$@`; the substitution was dropped, so libffi's `make html` moved
// libffi.html onto itself after deleting it.
#[test]
fn an_internal_macro_takes_a_substitution() {
    let dir = Dir::new("subst_internal");
    dir.write("a.texi", "");
    dir.write("b.c", "");
    dir.write(
        "Makefile",
        "EXT = .x\n.SUFFIXES: .texi .html\nall: a.html d/q.o\n\
         .texi.html:\n\t@echo [$(@:.html=.htp)] [${@:.html=$(EXT)}] [$(<:.texi=.y)] \
         [$(*:a=b)] [$(@F:%.html=%.z)]\n\
         d/q.o: b.c\n\t@echo [$(@D:d=e)] [$(^:.c=.h)]\n",
    );
    dir.make(&[])
        .ok("[a.htp] [a.x] [a.y] [b] [a.z]\n[e] [b.h]\n");
}
