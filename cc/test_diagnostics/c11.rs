//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compile-only cases of the c11 suite (tests/c11), in process.
//

use crate::test_compile::{
    compile_expect_error, compile_expect_no_diagnostic, compile_expect_ok, compile_expect_warning,
};

// ============================================================================
// tests/c11/generic.rs:
// C11 `_Generic` type-generic selection (C17 6.5.1.1), audit #X3.
// ============================================================================

/// A pointer to an enum and a pointer to its integer type point at compatible
/// types (C17 6.7.6.1p2), so assigning one to the other is silent in gcc.
#[test]
fn c11_enum_pointer_to_its_integer_type_is_silent() {
    compile_expect_no_diagnostic(
        "enum_ptr_same_int",
        r#"
enum E { A, B };
enum S { M = -1 };
int main(void) {
    enum E e = A; enum S s = M;
    unsigned *pu = &e;
    enum E *pe = pu;
    int *ps = &s;
    enum S *pes = ps;
    return *pe + *pes;
}
"#,
        "pointer",
    );
}

/// The signedness has to agree: `enum E` is `unsigned int`, so an `int *`
/// does not point at a compatible type, and gcc warns.
#[test]
fn c11_enum_pointer_to_the_other_signedness_warns() {
    compile_expect_warning(
        "enum_ptr_other_int",
        "enum E { A, B };\nint main(void) { enum E e = A; int *p = &e; return *p; }\n",
        "incompatible pointer type",
    );
    compile_expect_warning(
        "enum_ptr_other_uint",
        "enum S { M = -1 };\nint main(void) { enum S s = M; unsigned *p = &s; return *p; }\n",
        "incompatible pointer type",
    );
}

/// Compatible types make a compatible redeclaration (C17 6.2.7), which gcc
/// accepts -- warning only under `-Wenum-int-mismatch`, part of `-Wall`.
#[test]
fn c11_enum_redeclared_as_its_integer_type() {
    compile_expect_no_diagnostic(
        "enum_redecl_int",
        r#"
enum E { E0, E1 };
enum S { S0 = -1 };
enum E f(void);
unsigned f(void) { return E1; }
int g(enum S);
int g(int x) { return x; }
extern enum E v;
unsigned v = E1;
int main(void) { return f() + g(S0) + (int)v - 1 != 0; }
"#,
        "conflicting",
    );
}

/// A typedef may be redefined only as the *same* type (C17 6.7p3), and
/// neither an enum and its integer type nor `int` and `const int` are that.
#[test]
fn c11_typedef_redefinition_needs_the_same_type() {
    compile_expect_error(
        "typedef_enum_vs_int",
        "enum E { A };\ntypedef enum E T;\ntypedef unsigned T;\n",
        "typedef 'T' redefined with a different type ('enum E' then 'unsigned int')",
    );
    compile_expect_error(
        "typedef_const_int",
        "typedef int T;\ntypedef const int T;\n",
        "typedef 'T' redefined with a different type ('int' then 'const int')",
    );
    compile_expect_ok(
        "typedef_enum_repeated",
        "enum E { A };\ntypedef enum E T;\ntypedef enum E T;\ntypedef const int C;\ntypedef const int C;\n",
    );
}

/// An enum and its integer type are compatible, so they cannot both be
/// `_Generic` associations (C17 6.5.1.1p2).
#[test]
fn c11_generic_rejects_an_enum_beside_its_integer_type() {
    compile_expect_error(
        "generic_enum_dup",
        "enum E { A };\nint main(void) { return _Generic(0u, enum E: 0, unsigned: 1, default: 2); }\n",
        "two associations with compatible type",
    );
}

// ============================================================================
// tests/c11/literals.rs:
// C11 encoding-prefixed literals (6.4.4.4, 6.4.5) and adjacent-literal
// concatenation, including the mixed-prefix rules.
// ============================================================================

// ============================================================================
// #L4 — adjacent literals of different encodings concatenate
// ============================================================================

/// Two *different* prefixes in one run is a constraint violation (6.4.5p2).
#[test]
fn c11_conflicting_prefix_concatenation_is_rejected() {
    compile_expect_error(
        "c11_conflicting_concat",
        "const void *p = L\"a\" u\"b\";\n",
        "different encoding prefixes",
    );
}

// ============================================================================
// tests/c11/member_lists.rs:
// Valid code c17 once rejected, each found in a Linux UAPI header: member
// lists, a flexible array member after an anonymous union, a concatenated
// `_Static_assert` message, and an `&&` / `||` whose right operand is not
// evaluated.
// ============================================================================

/// Translation phase 6 makes one literal of adjacent ones, so a
/// `_Static_assert` message may be written as several (`BUILD_BUG_ON_ZERO`
/// pastes `#e " is true"`), with any encoding prefix.
#[test]
fn static_assert_message_is_a_concatenated_literal() {
    compile_expect_ok(
        "sa_concat_ok",
        "_Static_assert(1, \"a\" \"b\");\n\
         _Static_assert(1, L\"a\" L\"b\");\n\
         struct S { int x; _Static_assert(sizeof(int) >= 2, \"int\" \" too small\"); };\n\
         void f(void) { _Static_assert(1, \"in\" \" a block\"); }\n",
    );
    compile_expect_error(
        "sa_concat_fail",
        "_Static_assert(0, \"first \" \"second\");\n",
        "static assertion failed: first second",
    );
}

/// glibc headers that include only <stdint.h> and expect it to bring in
/// <sys/cdefs.h>, as glibc's own does: the bundled one hands a hosted glibc
/// build to the C library's.
#[cfg(target_os = "linux")]
#[test]
fn glibc_headers_that_lean_on_stdint() {
    compile_expect_ok(
        "glibc_stdint_users",
        "#include <sys/eventfd.h>\n#include <sys/inotify.h>\n\
         #include <sys/signalfd.h>\n#include <sys/fanotify.h>\n\
         #include <stdint.h>\n#include <inttypes.h>\n\
         _Static_assert(INT64_MAX == 0x7fffffffffffffff, \"int64\");\n\
         _Static_assert(sizeof(intptr_t) == sizeof(void *), \"intptr\");\n\
         int64_t v = INT64_C(5);\nint main(void) { return 0; }\n",
    );
}
