//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `s///g` searches on from each match with the whole line as context, so a
//! word boundary right where the last match ended is decided by the
//! character before it.  Expectations are GNU sed 4.9's output.

use super::sed_test;

const LINE: &str = "xx yxx ab_c\n";

fn ok(script: &str, expected: &str) {
    sed_test(&[script], LINE, expected, "", 0);
}

#[test]
fn test_s_global_word_boundary_after_a_match() {
    ok("s/\\>/|/g", "xx| yxx| ab_c|\n");
    // macOS's regexec cannot be shown the character before where a search
    // starts (see `plib::regex::Regex::find_bytes_at`), so these hold
    // everywhere but there.
    if cfg!(not(target_vendor = "apple")) {
        ok("s/\\bx/Z/g", "Zx yxx ab_c\n");
        ok("s/\\<x/Z/g", "Zx yxx ab_c\n");
        ok("s/\\Bx/Z/g", "xZ yZZ ab_c\n");
        ok("s/\\b/|/g", "|xx| |yxx| |ab_c|\n");
        ok("s/\\B/|/g", "x|x y|x|x a|b|_|c\n");
    }
}
