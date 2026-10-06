//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's `-Woverflow` for signed arithmetic on constants (C17 6.5p5), and
// the array size such arithmetic makes variable.
//

use crate::common::{compile_and_run_everywhere, compile_expect_error, compile_object_run};

/// The warning, once per overflow and at the operator, and the wrapped
/// value it names is the value the program computes.
#[test]
fn signed_constant_overflow_warns() {
    let run = compile_object_run(
        "signed_overflow",
        "int x = 2147483647 + 1;\nint y = -(-2147483647 - 1);\nunsigned u = 4294967295u + 1;\nint z = (2147483647 + 1) - 1;\n",
        &[],
    );
    assert!(run.success, "{}", run.stderr);
    for want in [
        ":1:20: warning: integer overflow in expression of type 'int' results in '-2147483648'",
        ":2:9: warning: integer overflow in expression '-2147483648' of type 'int' results in '-2147483648'",
        ":4:21: warning: integer overflow in expression of type 'int' results in '-2147483648'",
    ] {
        assert!(run.stderr.contains(want), "{want}:\n{}", run.stderr);
    }
    assert_eq!(run.stderr.matches("warning:").count(), 3, "{}", run.stderr);
    let quiet = compile_object_run(
        "signed_overflow_quiet",
        "int x = 2147483647 + 1;\n",
        &["-Wno-overflow"],
    );
    assert!(quiet.success && quiet.stderr.is_empty(), "{}", quiet.stderr);

    let src = "int x = 2147483647 + 1;\n\
               int m = (-2147483647 - 1) % -1;\n\
               enum { E = 65536 * 65536 };\n\
               int main(void) { return x == -2147483647 - 1 && m == 0 && E == 0 ? 0 : 1; }\n";
    compile_and_run_everywhere("signed_overflow_values", src);
}

/// gcc does not take an array size whose computation overflows as a
/// constant: at file scope the array is refused, in a block it is a VLA.
#[test]
fn overflowed_array_size_is_variable() {
    compile_expect_error(
        "overflowed_array_size",
        "int a[2147483647 + 2 > 0 ? 1 : 2];\n",
        "file scope",
    );
    let src = "int main(void) { int n = 0; int a[2147483647 + 2 > 0 ? 1 : 2]; n = sizeof a; return n == 8 ? 0 : 1; }\n";
    compile_and_run_everywhere("overflowed_array_size_vla", src);
}
