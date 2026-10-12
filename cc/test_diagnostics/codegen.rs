//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compile-only cases of the codegen suite (tests/codegen), in process.
//

use crate::test_compile::{
    compile_expect_error, compile_expect_warning, compile_rejected_with, compile_warnings,
};

// ============================================================================
// tests/codegen/ms_abi.rs:
// `__attribute__((ms_abi))`: the Microsoft x64 calling convention on x86-64
// ============================================================================

/// The convention is part of the function type: a pointer to an `ms_abi`
/// function and one to an ordinary function are incompatible, as gcc warns,
/// and a redeclaration that changes the convention conflicts.
#[test]
fn codegen_ms_abi_function_types_are_distinct() {
    if !cfg!(target_arch = "x86_64") {
        return;
    }
    compile_expect_warning(
        "ms_abi_ptr",
        "long s(long);\n\
         typedef __attribute__((ms_abi)) long (*msfp)(long);\n\
         msfp p = s;\n",
        "incompatible pointer type",
    );
    compile_expect_warning(
        "ms_abi_ptr_rev",
        "__attribute__((ms_abi)) long m(long);\n\
         long (*p)(long) = m;\n",
        "incompatible pointer type",
    );
    compile_expect_error(
        "ms_abi_redecl",
        "__attribute__((ms_abi)) long m(long);\n\
         long m(long x) { return x; }\n",
        "conflicting types",
    );
    compile_expect_error(
        "ms_abi_both",
        "__attribute__((ms_abi, sysv_abi)) long m(long);\n",
        "'ms_abi' and 'sysv_abi' attributes are not compatible",
    );
    compile_expect_warning(
        "ms_abi_object",
        "__attribute__((ms_abi)) int x;\n",
        "'ms_abi' attribute only applies to function types",
    );
    compile_expect_error(
        "ms_abi_va_start",
        "__attribute__((ms_abi)) int f(int n, ...) {\n\
         __builtin_va_list ap; __builtin_va_start(ap, n); return 0; }\n",
        "'va_start' used in Win64 ABI function",
    );
}

// ============================================================================
// `__attribute__((nocf_check))` under `-fcf-protection=branch`
// ============================================================================

/// Under branch protection `nocf_check` is part of the function type, as in
/// gcc: assigning between a pointer to such a function and a plain one warns
/// either way, spelled as gcc spells the type; a redeclaration that adds or
/// drops it conflicts; and on anything but a function type it warns and is
/// ignored.
#[test]
fn nocf_check_function_types_are_distinct() {
    let x86 = [
        "--target=x86_64-unknown-linux-gnu",
        "-fcf-protection=branch",
    ];
    let warnings = |name: &str, src: &str| compile_warnings(name, src, &x86).join("\n");
    let w = warnings(
        "nocf_ptr",
        "__attribute__((nocf_check)) void f(void) {}\nvoid g(void) {}\n\
         typedef void (*np)(void) __attribute__((nocf_check));\n\
         void (*a)(void) = f;\nnp b = g;\nnp c = f;\n",
    );
    assert!(
        w.contains(
            "4:19: initialization of 'void (*)(void)' from incompatible pointer type \
             'void (__attribute__((nocf_check)) *)(void)'"
        ),
        "{w}"
    );
    // gcc names the typedef too ("'np' {aka '...'}"), which c17 does not
    // keep for any type in this message.
    assert!(
        w.contains(
            "5:8: initialization of 'void (__attribute__((nocf_check)) *)(void)' \
             from incompatible pointer type 'void (*)(void)'"
        ),
        "{w}"
    );
    assert!(!w.contains("6:"), "{w}");
    for (name, src) in [
        (
            "nocf_redecl",
            "__attribute__((nocf_check)) void f(void);\nvoid f(void) {}\n",
        ),
        (
            "nocf_redecl_add",
            "void g(void);\n__attribute__((nocf_check)) void g(void) {}\n",
        ),
    ] {
        let stderr = compile_rejected_with(name, src, &x86);
        assert!(stderr.contains("conflicting types"), "{name}:\n{stderr}");
    }
    let w = warnings("nocf_object", "int x __attribute__((nocf_check));\n");
    assert!(
        w.contains("'nocf_check' attribute only applies to function types"),
        "{w}"
    );
}

/// Without branch protection -- no `-fcf-protection`, `return` only, or a
/// target with no CET -- gcc ignores `nocf_check` and says why, and the types
/// stay compatible.
#[test]
fn nocf_check_ignored_without_branch_protection() {
    let src = "__attribute__((nocf_check)) void f(void) {}\nvoid (*a)(void) = f;\n";
    for flags in [
        &["--target=x86_64-unknown-linux-gnu"][..],
        &[
            "--target=x86_64-unknown-linux-gnu",
            "-fcf-protection=return",
        ],
        &["--target=aarch64-unknown-linux-gnu"],
    ] {
        let w = compile_warnings("nocf_off", src, flags);
        assert_eq!(
            w,
            [
                "1:1: 'nocf_check' attribute ignored. Use '-fcf-protection' option to enable it \
              [-Wattributes]"
            ],
            "{flags:?}"
        );
    }
}

// ============================================================================
// tests/codegen/vectors.rs:
// GNU `vector_size` values: arithmetic, comparisons, splats, casts,
// assignment and initialization, lowered lane by lane. The expected values
// are gcc's.
// ============================================================================

const PRELUDE: &str = r#"
typedef int v4si __attribute__((vector_size(16)));
typedef unsigned v4su __attribute__((vector_size(16)));
typedef float v4sf __attribute__((vector_size(16)));
typedef double v2df __attribute__((vector_size(16)));
typedef long v2dl __attribute__((vector_size(16)));
typedef short v8hi __attribute__((vector_size(16)));
typedef unsigned char v16qu __attribute__((vector_size(16)));
typedef int v2si __attribute__((vector_size(8)));
typedef double v4df __attribute__((vector_size(32)));
/* Return a distinct code from the line of the first lane that differs. */
#define C4(v, a, b, c, d) do { __typeof__(v) t_ = (v); \
    if (t_[0] != (a) || t_[1] != (b) || t_[2] != (c) || t_[3] != (d)) \
        return __LINE__ % 200 + 1; } while (0)
#define C2(v, a, b) do { __typeof__(v) t_ = (v); \
    if (t_[0] != (a) || t_[1] != (b)) return __LINE__ % 200 + 1; } while (0)
"#;

#[test]
fn vector_operand_constraints() {
    for (name, expr, expected) in [
        ("vec_mixed_lanes", "a + f", "invalid operands to binary +"),
        (
            "vec_float_splat",
            "a + 1.5",
            "cannot convert value to a vector",
        ),
        (
            "vec_truncating_splat",
            "a + l",
            "conversion of scalar 'long' to vector '__vector(4) int' involves truncation",
        ),
        ("vec_float_mod", "f % f", "invalid operands to binary %"),
        (
            "vec_float_complement",
            "~f",
            "wrong type argument to bit-complement",
        ),
        (
            "vec_not",
            "!a",
            "wrong type argument to unary exclamation mark",
        ),
        ("vec_deref", "*a", "invalid type argument of unary '*'"),
        (
            "vec_logical",
            "a && a",
            "used vector type where scalar is required",
        ),
        (
            "vec_cond_mismatch",
            "l ? a : u",
            "type mismatch in conditional expression",
        ),
        (
            "vec_assign_signedness",
            "a = u",
            "incompatible types when assigning",
        ),
        ("vec_cast_size", "(v2si)l2[0]", "which has different size"),
        (
            "vec_cast_float",
            "(double)l2",
            "aggregate value used where a floating-point was expected",
        ),
    ] {
        compile_expect_error(
            name,
            &format!(
                "{PRELUDE}void g(long l) {{ v4si a = {{0}}; v4su u = {{0}}; v4sf f = {{0}}; \
                 v2si l2 = {{0}}; (void)({expr}); }}\n"
            ),
            expected,
        );
    }
}

/// A cast between vectors past 512 MiB of different sizes is refused as any
/// other: their widths saturated at the same `u32::MAX` bits and compared
/// equal, so `(W)v` between a 1 GiB and a 512 MiB vector was accepted.
#[test]
fn vector_cast_between_large_vectors_of_different_sizes() {
    compile_expect_error(
        "vec_cast_large",
        "typedef char V __attribute__((vector_size(1 << 30)));\n\
         typedef char W __attribute__((vector_size(1 << 29)));\n\
         extern V v; W *x;\n\
         void f(void) { *x = (W)v; }\n",
        "which has different size",
    );
}

#[test]
fn vector_builtin_constraints() {
    for (name, expr, expected) in [
        ("shuf_float_mask", "__builtin_shuffle(a, f)", "'__builtin_shuffle' last argument must be an integer vector"),
        ("shuf_count", "__builtin_shuffle(a, l2)", "number of elements of the argument vector(s) and the mask vector should be the same"),
        ("shuf_types", "__builtin_shuffle(a, f, a)", "'__builtin_shuffle' argument vectors must be of the same type"),
        ("shuf_scalar", "__builtin_shuffle(l, a)", "'__builtin_shuffle' arguments must be vectors"),
        ("sv_index", "__builtin_shufflevector(a, a, 0, 8)", "invalid element index '8' to '__builtin_shufflevector'"),
        ("sv_pow2", "__builtin_shufflevector(a, a, 0, 1, 2)", "must specify a result with a power of two number of elements"),
        ("sv_lane", "__builtin_shufflevector(a, f, 0, 1)", "argument vectors must have the same element type"),
        ("cv_count", "__builtin_convertvector(a, v2si)", "number of elements of the first argument vector and the second argument vector type should be the same"),
        ("cv_scalar", "__builtin_convertvector(a, int)", "second argument must be an integer or floating vector type"),
    ] {
        compile_expect_error(
            name,
            &format!(
                "{PRELUDE}void g(long l) {{ v4si a = {{0}}; v4sf f = {{0}}; v2si l2 = {{0}}; (void)({expr}); }}\n"
            ),
            expected,
        );
    }
}
