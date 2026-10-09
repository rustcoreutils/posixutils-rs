//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! yacc's command line.

// An argument that is not valid UTF-8 is reported, not a panic.
#[cfg(unix)]
#[test]
fn yacc_non_utf8_argument_is_reported() {
    plib::testing::assert_non_utf8_argument_rejected("yacc", &[]);
}
