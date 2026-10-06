//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's `-Woverflow` for signed arithmetic on constants whose result the
// type cannot hold (C17 6.5p5). Every expected line is gcc 13's.
//

use crate::test_compile::{compile, compile_warnings};

const X86_64: &str = "--target=x86_64-unknown-linux-gnu";

/// Each operator that can overflow, at each width, in every context: a
/// static initializer, an enumerator, a `case` label, an array size, an
/// expression in code. The operand that overflowed is not reported again by
/// the operation that consumes it.
#[test]
fn diagnostics_signed_constant_overflow_warns() {
    let src = "int x = 2147483647 + 1;\n\
               int y = 65536 * 65536;\n\
               int z = -2147483647 - 2;\n\
               int w = -(-2147483647 - 1);\n\
               long l = 9223372036854775807L + 1;\n\
               enum { E1 = 2147483647 + 1 };\n\
               int f(int v) {\n\
               switch (v) { case 2147483647 + 1: return 1; }\n\
               int a[2147483647 + 2 > 0 ? 1 : 2];\n\
               return v + (2147483647 * 2) + sizeof a;\n\
               }\n\
               int d = (-2147483647 - 1) / -1;\n\
               int m = (-2147483647 - 1) % -1;\n\
               int once = (2147483647 + 1) - 1;\n\
               int neg_once = -(2147483647 + 1);\n\
               long long ll = 3037000500LL * 3037000500LL;\n\
               int el = 0 ?: 2147483647 + 1;\n";
    assert_eq!(
        compile_warnings("sco_warns", src, &[X86_64]),
        [
            "1:20: integer overflow in expression of type 'int' results in '-2147483648'",
            "2:15: integer overflow in expression of type 'int' results in '0'",
            "3:21: integer overflow in expression of type 'int' results in '2147483647'",
            "4:9: integer overflow in expression '-2147483648' of type 'int' results in '-2147483648'",
            "5:31: integer overflow in expression of type 'long int' results in '-9223372036854775808'",
            "6:24: integer overflow in expression of type 'int' results in '-2147483648'",
            "8:30: integer overflow in expression of type 'int' results in '-2147483648'",
            "9:18: integer overflow in expression of type 'int' results in '-2147483647'",
            "10:24: integer overflow in expression of type 'int' results in '-2'",
            "12:27: integer overflow in expression of type 'int' results in '-2147483648'",
            "13:27: integer overflow in expression of type 'int' results in '0'",
            "14:24: integer overflow in expression of type 'int' results in '-2147483648'",
            "15:29: integer overflow in expression of type 'int' results in '-2147483648'",
            "16:29: integer overflow in expression of type 'long long int' results in '-9223372036709301616'",
            "17:26: integer overflow in expression of type 'int' results in '-2147483648'",
        ]
    );
}

/// What gcc does not warn about: unsigned arithmetic, which wraps by
/// definition; an operand promoted out of overflow; a shift; and an operand
/// that is never evaluated -- of `sizeof`, `_Alignof` or `typeof`, the
/// controlling expression of `_Generic`, or one behind a constant `?:`,
/// `&&` or `||` condition.
#[test]
fn diagnostics_constant_arithmetic_that_does_not_overflow_is_silent() {
    let src = "unsigned u = 4294967295u + 1;\n\
               int s = (short)32767 + 1;\n\
               int sh = 1 << 31;\n\
               int g = 0 ? 2147483647 + 1 : 3;\n\
               int h = 1 || (2147483647 + 1);\n\
               int k = 0 && (2147483647 * 2);\n\
               int sz = sizeof(2147483647 + 1);\n\
               int al = _Alignof(char[2147483647 + 1 > 0]);\n\
               int max = 2147483646 + 1;\n\
               long min = -9223372036854775807L - 1;\n\
               __typeof__(2147483647 + 1) ty;\n\
               int ge = _Generic(2147483647 + 1, int: 1, default: 0);\n\
               int el = 1 ?: 2147483647 + 1;\n\
               int ne = 1 ? 0 : -(-2147483647 - 1);\n";
    let got = compile_warnings("sco_silent", src, &[X86_64]);
    assert!(!got.iter().any(|w| w.contains("overflow")), "{got:?}");
    let quiet = compile_warnings("sco_wno", "int x = 2147483647 + 1;\n", &["-Wno-overflow"]);
    assert!(quiet.is_empty(), "{quiet:?}");
}

/// An array size whose evaluation overflows is not an integer constant
/// expression to gcc, so the array is variable: refused at file scope and
/// for a static array, and a VLA in a block.
#[test]
fn diagnostics_overflowed_array_size_is_not_constant() {
    for (name, src) in [
        ("sco_vla_file", "int a[2147483647 + 2 > 0 ? 1 : 2];\n"),
        (
            "sco_vla_static",
            "int f(void) { static int a[2147483647 + 2 > 0 ? 1 : 2]; return a[0]; }\n",
        ),
    ] {
        let c = compile(name, src, &[]);
        assert!(!c.success, "{name}: {}", c.stderr);
    }
    let got = compile_warnings(
        "sco_vla_block",
        "int f(void) { int a[2147483647 + 2 > 0 ? 1 : 2]; return sizeof a; }\n",
        &[],
    );
    assert_eq!(got.len(), 1, "{got:?}");
}
