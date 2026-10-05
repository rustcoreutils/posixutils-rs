//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compile-only cases of the c89 suite (tests/c89), in process.
//

use crate::test_compile::{
    compile_expect_error, compile_expect_warning, compile_expect_warning_with, compile_rejected,
};

// ============================================================================
// tests/c89/declarations.rs:
// C89/C99 Declarations Mega-Test
//
// Covers: declaration syntax, variable decls, function decls (incl. K&R),
// structs (forward decl, flexible array, zero-width bitfield), unions,
// enums, typedefs, initializers
// ============================================================================

// ============================================================================
// Where a GNU attribute may be written
// ============================================================================

/// `sizeof` of an expression of incomplete structure type is a constraint
/// violation (C17 6.5.3.4p1), whichever tag is incomplete: one never
/// defined, one hidden by `struct S;`, or one whose name an inner scope has
/// given a complete definition. The last was accepted because the pointee's
/// tag was looked up by name and found the inner definition.
#[test]
fn c89_sizeof_an_incomplete_struct_expression_is_rejected() {
    let what = "invalid application of 'sizeof' to incomplete type 'struct S'";
    compile_expect_error(
        "sizeof_incomplete_never_defined",
        "struct S; int f(struct S *p) { return sizeof *p; }\n",
        what,
    );
    compile_expect_error(
        "sizeof_incomplete_hidden",
        "struct S { int a; };\nint f(void) { struct S; struct S *p = 0; return sizeof *p; }\n",
        what,
    );
    compile_expect_error(
        "sizeof_incomplete_under_inner_tag",
        "struct S; struct S *gp;\nint f(void) { struct S { char c; }; return sizeof(*gp); }\n",
        what,
    );
}

// ============================================================================
// tests/c89/functions.rs:
// C89 Functions Mega-Test
//
// Consolidates: function_pointers, calls, recursion tests from features/
// ============================================================================

// ============================================================================
// Calls without a prototype, and identifier-list definitions
// ============================================================================

/// Dropping a qualifier on the way to or from `void *` is diagnosed as it is
/// between compatible pointees (C17 6.5.16.1p1), in a call, an
/// initialization and the other direction alike; gcc warns on each.
#[test]
fn c89_functions_void_pointer_conversion_keeps_qualifiers() {
    for (name, code) in [
        (
            "voidptr_init_const",
            "const int *p; void f(void) { void *q = p; (void)q; }\n",
        ),
        (
            "voidptr_arg_volatile",
            "void h(const void *); volatile int *p; void f(void) { h(p); }\n",
        ),
        (
            "voidptr_from_const_void",
            "const void *p; void f(void) { int *q = p; (void)q; }\n",
        ),
    ] {
        compile_expect_warning(
            name,
            code,
            "discards a qualifier from the pointer target type",
        );
    }
}

// ============================================================================
// tests/c89/operators.rs:
// C89 Operators Mega-Test
//
// Consolidates: bitfield, mixed_cmp, ops_struct, short_circuit tests
// ============================================================================

// ============================================================================
// Mega-test: Operator coverage gaps — compound assignments, bitwise NOT,
// comma operator, precedence (15 levels), associativity
// ============================================================================

/// An arithmetic operator on a structure is a constraint violation (C17
/// 6.5.6p2), diagnosed where the operator is. c17 let `s + 1` through and
/// reported only the assignment of its result, which names the wrong
/// mistake -- and says nothing at all where the result is discarded.
#[test]
fn c89_arithmetic_on_a_struct_is_rejected_at_the_operator() {
    compile_expect_error(
        "struct_plus",
        "struct S { int a; } s; int f(void) { int i; i = s + 1; return i; }\n",
        "invalid operands to binary +",
    );
    compile_expect_error(
        "struct_plus_discarded",
        "struct S { int a; } s; void f(void) { (void)(s + 1); }\n",
        "invalid operands to binary +",
    );
}

/// Declarations every operand-constraint case below draws on.
const OPERAND_DECLS: &str = "struct S { int a; } s; union U { int a; } u; struct S as[2];\n\
    void fv(void); int fn(int); _Complex double z; _Complex int zi;\n\
    int *p, *q; long *lp; void *vp; const int *cp;\n\
    int i; double d; _Bool b; enum E { E0 } e;\n";

fn operand_case(body: &str) -> String {
    format!("{OPERAND_DECLS}void f(void) {{ {body} }}\n")
}

/// Every operator class against an operand its constraint excludes, with the
/// error gcc 13 gives (C17 6.5.3.3p1, 6.5.5p2-6.5.15p2, 6.5.16.2p1-2,
/// 6.8.4.1p1, 6.8.5p2). gcc spells a complex type `complex double` where c17
/// spells it `double _Complex`, so those cases match the operator alone.
#[test]
fn c89_operand_constraints_are_diagnosed_as_gcc_does() {
    let cases = [
        // additive (6.5.6p2-3)
        (
            "(void)(1 + s);",
            "invalid operands to binary + (have 'int' and 'struct S')",
        ),
        (
            "(void)(u + 1);",
            "invalid operands to binary + (have 'union U' and 'int')",
        ),
        (
            "(void)(as[0] + 1);",
            "invalid operands to binary + (have 'struct S' and 'int')",
        ),
        (
            "(void)(s - 1);",
            "invalid operands to binary - (have 'struct S' and 'int')",
        ),
        (
            "(void)(p + p);",
            "invalid operands to binary + (have 'int *' and 'int *')",
        ),
        (
            "(void)(1 - p);",
            "invalid operands to binary - (have 'int' and 'int *')",
        ),
        (
            "(void)(p - vp);",
            "invalid operands to binary - (have 'int *' and 'void *')",
        ),
        // subscript (6.5.2.1p1): the pointer must be to a complete object
        // type, and gcc's arithmetic on function pointers does not reach it
        (
            "(void)((&fn)[0]);",
            "subscripted value is pointer to function",
        ),
        (
            "(void)(0[&fn]);",
            "subscripted value is pointer to function",
        ),
        (
            "(void)(p - as);",
            "invalid operands to binary - (have 'int *' and 'struct S *')",
        ),
        (
            "(void)(p + d);",
            "invalid operands to binary + (have 'int *' and 'double')",
        ),
        (
            "(void)(p + z);",
            "invalid operands to binary + (have 'int *' and",
        ),
        (
            "(void)(e + s);",
            "invalid operands to binary + (have 'unsigned int' and 'struct S')",
        ),
        (
            "(void)((void)0 + 1);",
            "void value not ignored as it ought to be",
        ),
        // multiplicative (6.5.5p2)
        (
            "(void)(s * 2);",
            "invalid operands to binary * (have 'struct S' and 'int')",
        ),
        (
            "(void)(p * 2);",
            "invalid operands to binary * (have 'int *' and 'int')",
        ),
        (
            "(void)(fn * 2);",
            "invalid operands to binary * (have 'int (*)(int)' and 'int')",
        ),
        (
            "(void)(s / 2);",
            "invalid operands to binary / (have 'struct S' and 'int')",
        ),
        (
            "(void)(d % 2);",
            "invalid operands to binary % (have 'double' and 'int')",
        ),
        (
            "(void)(b % d);",
            "invalid operands to binary % (have 'int' and 'double')",
        ),
        ("(void)(z % 2);", "invalid operands to binary %"),
        ("(void)(zi % 2);", "invalid operands to binary %"),
        // shifts (6.5.7p2)
        (
            "(void)(s << 1);",
            "invalid operands to binary << (have 'struct S' and 'int')",
        ),
        (
            "(void)(d << 1);",
            "invalid operands to binary << (have 'double' and 'int')",
        ),
        (
            "(void)(p << 1);",
            "invalid operands to binary << (have 'int *' and 'int')",
        ),
        (
            "(void)(1 << 2.0);",
            "invalid operands to binary << (have 'int' and 'double')",
        ),
        ("(void)(z << 1);", "invalid operands to binary <<"),
        // relational (6.5.8p2)
        (
            "(void)(s < 1);",
            "invalid operands to binary < (have 'struct S' and 'int')",
        ),
        (
            "(void)(u > u);",
            "invalid operands to binary > (have 'union U' and 'union U')",
        ),
        (
            "(void)(p < d);",
            "invalid operands to binary < (have 'int *' and 'double')",
        ),
        ("(void)(z < 1);", "invalid operands to binary <"),
        // equality (6.5.9p2)
        (
            "(void)(s == s);",
            "invalid operands to binary == (have 'struct S' and 'struct S')",
        ),
        (
            "(void)(p == 1.0);",
            "invalid operands to binary == (have 'int *' and 'double')",
        ),
        // bitwise (6.5.10-6.5.12)
        (
            "(void)(s & 1);",
            "invalid operands to binary & (have 'struct S' and 'int')",
        ),
        (
            "(void)(d & 1);",
            "invalid operands to binary & (have 'double' and 'int')",
        ),
        (
            "(void)(p ^ 1);",
            "invalid operands to binary ^ (have 'int *' and 'int')",
        ),
        ("(void)(zi | 1);", "invalid operands to binary |"),
        // logical (6.5.13p2, 6.5.14p2): the left operand is a truth value
        (
            "(void)(s && 1);",
            "used struct type value where scalar is required",
        ),
        (
            "(void)(u || u);",
            "used union type value where scalar is required",
        ),
        (
            "(void)(1 || u);",
            "invalid operands to binary || (have 'int' and 'union U')",
        ),
        (
            "(void)(d && s);",
            "invalid operands to binary && (have 'int' and 'struct S')",
        ),
        // unary (6.5.3.3p1)
        ("(void)(~s);", "wrong type argument to bit-complement"),
        ("(void)(~p);", "wrong type argument to bit-complement"),
        ("(void)(~d);", "wrong type argument to bit-complement"),
        (
            "(void)(!s);",
            "wrong type argument to unary exclamation mark",
        ),
        (
            "(void)(!u);",
            "wrong type argument to unary exclamation mark",
        ),
        ("(void)(-s);", "wrong type argument to unary minus"),
        ("(void)(-fn);", "wrong type argument to unary minus"),
        ("(void)(+s);", "wrong type argument to unary plus"),
        ("(void)(!(void)0);", "invalid use of void expression"),
        ("(void)(-(void)0);", "invalid use of void expression"),
        // increment and decrement (6.5.2.4p1, 6.5.3.1p1)
        ("s++;", "wrong type argument to increment"),
        ("++s;", "wrong type argument to increment"),
        ("s--;", "wrong type argument to decrement"),
        // conditional (6.5.15p2) and controlling expressions (6.8.4.1p1, 6.8.5p2)
        (
            "(void)(s ? 1 : 2);",
            "used struct type value where scalar is required",
        ),
        (
            "(void)(u ? 1 : 2);",
            "used union type value where scalar is required",
        ),
        (
            "(void)(s ?: 2);",
            "used struct type value where scalar is required",
        ),
        (
            "(void)((void)0 ? 1 : 2);",
            "void value not ignored as it ought to be",
        ),
        (
            "if (s) {}",
            "used struct type value where scalar is required",
        ),
        (
            "while (u) {}",
            "used union type value where scalar is required",
        ),
        (
            "for (; s;) {}",
            "used struct type value where scalar is required",
        ),
        (
            "do {} while (s);",
            "used struct type value where scalar is required",
        ),
        // compound assignment (6.5.16.2p1-2)
        (
            "s += 1;",
            "invalid operands to binary + (have 'struct S' and 'int')",
        ),
        (
            "s <<= 1;",
            "invalid operands to binary << (have 'struct S' and 'int')",
        ),
        (
            "i *= s;",
            "invalid operands to binary * (have 'int' and 'struct S')",
        ),
        (
            "b += s;",
            "invalid operands to binary + (have 'int' and 'struct S')",
        ),
        (
            "i %= d;",
            "invalid operands to binary % (have 'int' and 'double')",
        ),
        (
            "d &= 1;",
            "invalid operands to binary & (have 'double' and 'int')",
        ),
        (
            "i -= p;",
            "invalid operands to binary - (have 'int' and 'int *')",
        ),
        (
            "p += p;",
            "invalid operands to binary + (have 'int *' and 'int *')",
        ),
        (
            "p *= 2;",
            "invalid operands to binary * (have 'int *' and 'int')",
        ),
        ("z %= 2;", "invalid operands to binary %"),
        ("i += (void)0;", "void value not ignored as it ought to be"),
    ];
    for (n, (body, expected)) in cases.into_iter().enumerate() {
        compile_expect_error(
            &format!("operand_constraint_{n}"),
            &operand_case(body),
            expected,
        );
    }
}

/// The comparisons gcc accepts with a warning, and a compound assignment
/// whose result converts to the target only with one.
#[test]
fn c89_operand_constraints_gcc_warns_about() {
    let cases = [
        ("(void)(p < 1);", "comparison between pointer and integer"),
        ("(void)(p == 1);", "comparison between pointer and integer"),
        ("(void)(2 != p);", "comparison between pointer and integer"),
        (
            "(void)(p == lp);",
            "comparison of distinct pointer types lacks a cast",
        ),
        (
            "(void)(p < vp);",
            "comparison of distinct pointer types lacks a cast",
        ),
        (
            "(void)(fn == p);",
            "comparison of distinct pointer types lacks a cast",
        ),
        (
            "i += p;",
            "assignment to 'int' from 'int *' makes integer from pointer",
        ),
        ("p -= q;", "makes pointer from integer without a cast"),
    ];
    for (n, (body, expected)) in cases.into_iter().enumerate() {
        compile_expect_warning(
            &format!("operand_warning_{n}"),
            &operand_case(body),
            expected,
        );
    }
}

/// An operator's error is the only one: the bad result is left untyped, so
/// the assignment or operator around it does not report the same mistake as
/// incompatible types. gcc reports each of these once.
#[test]
fn c89_operand_error_does_not_cascade() {
    for (n, body) in [
        "int i2 = 0; i2 = s + 1; (void)i2;",
        "i = -s;",
        "i = ~s;",
        "i = !s;",
        "(void)((s + 1) * 2);",
        "(void)(-(s + 1));",
        "i = (s < 1) + 1;",
    ]
    .into_iter()
    .enumerate()
    {
        let stderr = compile_rejected(&format!("operand_no_cascade_{n}"), &operand_case(body));
        assert_eq!(
            stderr.matches("error:").count(),
            1,
            "{body}: expected one error\n{stderr}"
        );
    }
}

/// Operands that satisfy their operators, unusual as some are: complex
/// arithmetic and conjugation, pointer arithmetic and comparison, the GNU
/// arithmetic on `void *` and function pointers, null pointer constants, and
/// truth tests of pointers and complex values. gcc compiles each silently.
#[test]
fn c89_valid_unusual_operands_compile_silently() {
    for (n, body) in [
        "(void)(z + 1); (void)(z * 2); (void)(d + z); z += 1; z /= d;",
        "(void)(~z); (void)(~zi); (void)(-z); (void)(+zi); z++; --zi;",
        "(void)(z == 1); (void)(z != d); (void)(!z); (void)(z && d); (void)(z ? 1 : 2);",
        "(void)(p + 1); (void)(1 + p); (void)(p - 1); (void)(p - q); p += 1; p -= 1; p++;",
        "(void)(as + 1); (void)(as - as); (void)(\"abc\" + 1); (void)(&s + 1);",
        "(void)(p < q); (void)(cp == p); (void)(cp < p); (void)(cp - p); (void)(p == vp);",
        "(void)(p == 0); (void)(0 == p); (void)(p == 0L); (void)(p > 0); (void)(p != (void *)0);",
        "(void)(vp + 1); (void)(vp - vp); (void)(fn + 1); (void)(fn - fn); (void)(fn == 0);",
        "(void)(!p); (void)(p && q); (void)(p || 0); (void)(!fn); (void)(as && 1);",
        "(void)(b + e); (void)(e & b); (void)(b % 2); b += 1; e += 1; (void)(s.a + 1);",
        "if (p) {} while (z) {} for (; d;) {} do {} while (fn);",
        "(void)(p ? 1 : 2); (void)(as[0].a << 1);",
        "long l = fn - fn; int (*g)(int) = fn + 1; g = 1 + fn; g = fn - 1; l = g - fn;",
        "int (*g)(int) = &fn + 1; g += 2; g -= 1; g++; --g; (void)(g < fn); (void)sizeof fn;",
    ]
    .into_iter()
    .enumerate()
    {
        let stderr =
            compile_expect_warning_with(&format!("operand_valid_{n}"), &operand_case(body), &[]);
        assert!(stderr.is_empty(), "{body}: expected silence\n{stderr}");
    }
}

// ============================================================================
// tests/c89/storage.rs:
// C89 Storage Classes Mega-Test
//
// Consolidates: storage.rs + static_local.rs tests
// ============================================================================

// ============================================================================
// Mega-test: C89 storage classes (auto, static, register, extern)
// ============================================================================

/// An automatic object has no address a static initializer can name, and its
/// value is not a constant: a same-named file-scope object must not be used
/// in its place, and nothing may be relocated against its bare name.
#[test]
fn c89_static_initializer_cannot_name_an_automatic() {
    compile_expect_error(
        "c89_static_init_auto_address",
        "int a; int f(void) { int a = 1; static int *p = &a; return *p; }\n",
        "not a constant expression",
    );
    compile_expect_error(
        "c89_static_init_auto_array",
        "int a[2]; int f(void) { int a[2] = {1, 2}; static int *p = a; return *p; }\n",
        "not a constant expression",
    );
    compile_expect_error(
        "c89_static_init_auto_const",
        "const int c = 5; int f(void) { const int c = 7; static int w = c; return w; }\n",
        "not a constant expression",
    );
}
