//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The `mode` attribute's named modes: `byte` and `unwind_word` were left
// unimplemented, warned, and kept the declared type's size -- 4 bytes where
// gcc gives 1 and 8.
//

use crate::common::compile_expect_ok;

#[test]
fn mode_byte_and_unwind_word() {
    compile_expect_ok(
        "mode_byte_unwind_word",
        "typedef int b __attribute__((mode(byte)));\n\
         typedef unsigned ub __attribute__((__mode__(__byte__)));\n\
         typedef int uw __attribute__((mode(unwind_word)));\n\
         typedef unsigned uuw __attribute__((__mode__(__unwind_word__)));\n\
         _Static_assert(sizeof(b) == 1 && sizeof(ub) == 1, \"byte\");\n\
         _Static_assert(sizeof(uw) == 8 && sizeof(uuw) == 8, \"unwind_word\");\n\
         _Static_assert((b)-1 < 0 && (ub)-1 == 255 && (uuw)-1 > 0, \"signedness\");\n",
    );
}
