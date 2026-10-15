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
