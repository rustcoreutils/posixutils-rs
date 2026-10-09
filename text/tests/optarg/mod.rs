//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

// XBD 12.1: an option-argument attached to its option letter is everything
// after the letter, so `-X=` is the argument "=" and `-X=x` the argument
// "=x", exactly as `-X =` and `-X =x`. clap alone read `-X=VALUE` as `-X`
// with VALUE, dropping the '='.
#[test]
fn attached_option_argument_may_begin_with_equals() {
    let input = b"a=b=c\nd=e=f\n";
    for (cmd, opt, rest) in [
        ("cut", "-d", &["-f2"][..]),
        ("cut", "-f", &[][..]),
        ("paste", "-d", &["-", "-"][..]),
        ("sort", "-t", &["-k2"][..]),
        ("sort", "-k", &[][..]),
        ("join", "-t", &["-", "/dev/null"][..]),
        ("nl", "-s", &[][..]),
        ("nl", "-d", &[][..]),
        ("expand", "-t", &[][..]),
        ("unexpand", "-t", &[][..]),
        ("fold", "-w", &[][..]),
        ("head", "-n", &[][..]),
        ("tail", "-n", &[][..]),
        ("uniq", "-f", &[][..]),
        ("grep", "-e", &[][..]),
        ("sed", "-e", &[][..]),
    ] {
        plib::testing::assert_equals_option_argument(cmd, opt, rest, input);
    }
}
