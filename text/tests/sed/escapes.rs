//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `\t` and the other control escapes in an RE, outside a bracket
//! expression, where POSIX leaves `\c` unspecified: util-linux's ipcs test
//! cuts at a tab with `sed -n '/^bytes/s/\t.*//p'`.

use super::sed_test;

#[test]
fn sed_tab_escape_in_an_re() {
    sed_test(
        &["-n", r"/^bytes/s/\t.*//p"],
        "bytes=1\tx\nother\tz\n",
        "bytes=1\n",
        "",
        0,
    );
    // In an address too.
    sed_test(&["-n", r"/a\tb/p"], "atb\na\tb\n", "a\tb\n", "", 0);
}

#[test]
fn sed_control_escapes_in_an_re() {
    sed_test(&[r"s/\r$//"], "dos\r\n", "dos\n", "", 0);
    sed_test(
        &[r"s/\f/F/;s/\v/V/;s/\a/A/"],
        "1\x0c2\x0b3\x074\n",
        "1F2V3A4\n",
        "",
        0,
    );
}

/// Inside a bracket expression a backslash is an ordinary character, as
/// POSIX requires: `[\t]` matches a backslash or a `t`, not a tab.
#[test]
fn sed_tab_escape_in_a_bracket_is_literal() {
    sed_test(&[r"s/[\t]/X/g"], "a\tt\\\n", "a\tXX\n", "", 0);
}
