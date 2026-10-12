//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's aarch64 builtin types -- the AdvSIMD vectors (`__Float32x4_t`) and
// the SVE vectors (`__SVFloat32_t`) -- and `__attribute__((aarch64_vector_pcs))`,
// compile-only. glibc's <math.h> declares its vector math functions with all
// three on aarch64. The runtime cases are in tests/codegen/aarch64_builtin_types.rs.
// Each expectation was measured against aarch64-linux-gnu-gcc 13.
//

use crate::test_compile::{compile_accepted, compile_rejected_with, compile_warnings};

const LINUX: &[&str] = &["--target=aarch64-unknown-linux-gnu"];
const DARWIN: &[&str] = &["--target=aarch64-apple-darwin"];
const X86: &[&str] = &["--target=x86_64-unknown-linux-gnu"];

/// `__Float32x4_t` is laid out and operated on as the GNU vector of four
/// `float`s, and converts to and from one as a value, but is a type of its
/// own: `__builtin_types_compatible_p` and `_Generic` tell them apart. Its
/// lanes are the standard types -- an `__Int64x2_t` lane is a `long`.
#[test]
fn advsimd_types_are_gcc_s() {
    let src = "typedef float v4sf __attribute__((vector_size(16)));\n\
        typedef double v2df __attribute__((vector_size(16)));\n\
        _Static_assert(sizeof(__Float32x4_t) == 16 && _Alignof(__Float32x4_t) == 16, \"f\");\n\
        _Static_assert(sizeof(__Float64x2_t) == 16 && _Alignof(__Float64x2_t) == 16, \"d\");\n\
        _Static_assert(sizeof(__Int8x8_t) == 8 && sizeof(__Uint64x1_t) == 8, \"8\");\n\
        _Static_assert(!__builtin_types_compatible_p(__Float32x4_t, v4sf), \"distinct\");\n\
        _Static_assert(!__builtin_types_compatible_p(__Float64x2_t, v2df), \"distinct\");\n\
        _Static_assert(__builtin_types_compatible_p(__Float32x4_t, const __Float32x4_t), \"q\");\n\
        _Static_assert(_Generic((__Float32x4_t){0}, v4sf: 0, __Float32x4_t: 1), \"g\");\n\
        _Static_assert(_Generic(((__Float32x4_t){0})[0], float: 1, default: 0), \"f\");\n\
        _Static_assert(_Generic(((__Int64x2_t){0})[0], long: 1, default: 0), \"l\");\n\
        _Static_assert(_Generic(((__Uint8x16_t){0})[0], unsigned char: 1, default: 0), \"u\");\n\
        _Static_assert(_Generic(((__Int8x8_t){0})[0], signed char: 1, default: 0), \"s\");\n\
        __Float32x4_t add(__Float32x4_t a, v4sf b) { return a + b; }\n\
        v4sf add2(v4sf a, __Float32x4_t b) { return add(a, b) * 2.0f; }\n\
        float lane(__Float32x4_t a) { return a[2]; }\n";
    for target in [LINUX, DARWIN] {
        let stderr = compile_accepted("advsimd_types", src, target);
        assert!(stderr.is_empty(), "{target:?}:\n{stderr}");
    }
}

/// A pointer to one does not convert to a pointer to the GNU vector, nor
/// does a vector of other lanes convert, and a redeclaration that swaps
/// them conflicts -- with the type named as gcc names it.
#[test]
fn advsimd_types_are_distinct_in_gcc_s_words() {
    let w = compile_warnings(
        "advsimd_ptr",
        "typedef float v4sf __attribute__((vector_size(16)));\n\
         v4sf *pp(__Float32x4_t *p) { return p; }\n",
        LINUX,
    )
    .join("\n");
    assert!(
        w.contains(
            "2:37: returning '__Float32x4_t *' from a function with incompatible return \
             type '__vector(4) float *'"
        ),
        "{w}"
    );
    let stderr = compile_rejected_with(
        "advsimd_lanes",
        "__Float32x4_t f(__Float64x2_t a) { return a; }\n",
        LINUX,
    );
    assert!(
        stderr.contains(
            "incompatible types when returning type '__Float64x2_t' but '__Float32x4_t' was expected"
        ),
        "{stderr}"
    );
    let stderr = compile_rejected_with(
        "advsimd_redecl",
        "typedef float v4sf __attribute__((vector_size(16)));\n\
         __Float32x4_t g(__Float32x4_t);\nv4sf g(v4sf);\n",
        LINUX,
    );
    assert!(stderr.contains("conflicting types for 'g'"), "{stderr}");
    let stderr = compile_rejected_with(
        "advsimd_cond",
        "typedef float v4sf __attribute__((vector_size(16)));\n\
         v4sf c(int x, __Float32x4_t a, v4sf b) { return x ? a : b; }\n",
        LINUX,
    );
    assert!(
        stderr.contains("type mismatch in conditional expression"),
        "{stderr}"
    );
}

/// gcc defines none of the names on x86-64, where they are ordinary
/// identifiers -- a program may use them.
#[test]
fn aarch64_builtin_types_are_absent_on_x86_64() {
    compile_rejected_with("advsimd_x86", "__Float32x4_t v;\n", X86);
    compile_rejected_with("sve_x86", "__SVBool_t *p;\n", X86);
    let stderr = compile_accepted(
        "advsimd_x86_names",
        "int __Float32x4_t, __SVBool_t;\n\
         typedef float __Float64x2_t __attribute__((vector_size(16)));\n\
         __Float64x2_t v;\n",
        X86,
    );
    assert!(stderr.is_empty(), "{stderr}");
}

/// An SVE type can be named in a typedef, a pointer and a function
/// declaration -- what glibc's <math.h> does with three of them -- and is a
/// type of its own.
#[test]
fn sve_types_declare() {
    let src = "typedef __SVFloat32_t __sv_f32_t;\n\
        typedef __SVFloat64_t __sv_f64_t;\n\
        typedef __SVBool_t __sv_bool_t;\n\
        __sv_f32_t _ZGVsMxv_sinf(__sv_f32_t, __sv_bool_t);\n\
        __sv_f64_t _ZGVsMxvv_atan2(__sv_f64_t, __sv_f64_t, __sv_bool_t);\n\
        __SVInt8_t *p8; __SVUint64_t *pu; __SVFloat16_t *ph; __SVBfloat16_t *pb;\n\
        __sv_f32_t (*fp)(__sv_f32_t, __sv_bool_t) = _ZGVsMxv_sinf;\n\
        _Static_assert(!__builtin_types_compatible_p(__SVFloat32_t, __SVInt32_t), \"d\");\n\
        _Static_assert(__builtin_types_compatible_p(__SVFloat32_t, __sv_f32_t), \"s\");\n\
        void use(__sv_f32_t *p) { fp = _ZGVsMxv_sinf; (void)p; }\n";
    for target in [LINUX, DARWIN] {
        let stderr = compile_accepted("sve_decls", src, target);
        assert!(stderr.is_empty(), "{target:?}:\n{stderr}");
    }
}

/// Anything that needs an SVE type's size, or SVE code, is refused, in
/// gcc's words where gcc has them.
#[test]
fn sve_types_refuse_what_needs_a_size() {
    let pre = "typedef __SVFloat32_t sv;\nsv f(sv, __SVBool_t);\n";
    let cases = [
        (
            "sve_object",
            "sv v;\n",
            "SVE type '__SVFloat32_t' does not have a fixed size",
        ),
        (
            "sve_extern",
            "extern sv v;\n",
            "SVE type '__SVFloat32_t' does not have a fixed size",
        ),
        (
            "sve_sizeof",
            "int n = sizeof(sv);\n",
            "SVE type '__SVFloat32_t' does not have a fixed size",
        ),
        (
            "sve_alignof",
            "int n = _Alignof(sv);\n",
            "SVE type '__SVFloat32_t' does not have a defined alignment",
        ),
        (
            "sve_field",
            "struct S { sv m; };\n",
            "fields cannot have SVE type '__SVFloat32_t'",
        ),
        (
            "sve_array",
            "sv a[2];\n",
            "array elements cannot have SVE type '__SVFloat32_t'",
        ),
        (
            "sve_arith",
            "sv *g(sv *p) { return p + 1; }\n",
            "arithmetic on pointer to SVE type '__SVFloat32_t'",
        ),
        (
            "sve_call",
            "void g(sv *p, __SVBool_t *b) { f(*p, *b); }\n",
            "this operation requires the SVE ISA extension",
        ),
        (
            "sve_copy",
            "void g(sv *p, sv *q) { *p = *q; }\n",
            "this operation requires the SVE ISA extension",
        ),
        (
            "sve_local_read",
            "void g(sv *p) { sv v; *p = v; }\n",
            "this operation requires the SVE ISA extension",
        ),
        (
            "sve_local_address",
            "void h(void *); void g(void) { sv v; h(&v); }\n",
            "this operation requires the SVE ISA extension",
        ),
        (
            "sve_local_init",
            "void g(sv *p) { sv v = *p; (void)v; }\n",
            "this operation requires the SVE ISA extension",
        ),
        (
            "sve_def",
            "sv d(sv a) { return a; }\n",
            "'d' requires the SVE ISA extension",
        ),
    ];
    for (name, body, expected) in cases {
        let src = format!("{pre}{body}");
        let stderr = compile_rejected_with(name, &src, LINUX);
        assert!(stderr.contains(expected), "{name}:\n{stderr}");
        assert!(!stderr.contains("incomplete"), "{name}:\n{stderr}");
    }
}

/// What needs no SVE code is accepted, as gcc accepts it without SVE: an
/// automatic SVE object nothing reads, writes or locates -- it gets no
/// storage -- and an SVE object designated with its value discarded, which
/// reads nothing. gcc 13 at every level.
#[test]
fn sve_types_accept_what_needs_no_sve_code() {
    let pre = "typedef __SVFloat32_t sv;\n";
    for body in [
        "void g(void) { sv v; }\n",
        "void g(void) { register sv v; __SVBool_t a, b; }\n",
        "void g(sv *p) { sv v; (void)v; }\n",
        "void g(sv *p) { __typeof__(*p) v; }\n",
        "void g(sv *p) { (void)*p; }\n",
        "void g(sv *p) { *p; }\n",
        "int g(sv *p) { int x = (*p, 1); return x; }\n",
        "void g(sv *p, int c) { c ? (void)*p : (void)0; }\n",
    ] {
        let src = format!("{pre}{body}");
        for opt in ["-O0", "-O2"] {
            let stderr = compile_accepted("sve_no_code", &src, &[LINUX[0], opt]);
            assert!(stderr.is_empty(), "{body} {opt}:\n{stderr}");
        }
    }
}

/// `aarch64_vector_pcs` is part of the function type, as in gcc: a pointer
/// to such a function and a plain one are incompatible, spelled as gcc
/// spells them; a redeclaration that adds or drops it conflicts; it wants a
/// function type and no argument; and `__has_attribute` answers 1.
#[test]
fn vector_pcs_is_part_of_the_function_type() {
    let decl = "typedef float v4sf __attribute__((vector_size(16)));\n\
        __attribute__((aarch64_vector_pcs)) v4sf vf(v4sf);\n\
        __attribute__((__aarch64_vector_pcs__)) v4sf vf2(v4sf);\n";
    let w = compile_warnings(
        "vpcs_ptr",
        &format!(
            "{decl}v4sf (*p1)(v4sf) = vf;\n\
             v4sf (__attribute__((aarch64_vector_pcs)) *p2)(v4sf) = vf2;\n\
             typedef __attribute__((aarch64_vector_pcs)) v4sf FT(v4sf);\nFT vf;\n\
             _Static_assert(__has_attribute(aarch64_vector_pcs) == 1, \"has\");\n\
             _Static_assert(__has_attribute(__aarch64_vector_pcs__) == 1, \"has\");\n"
        ),
        LINUX,
    );
    assert_eq!(
        w,
        [
            "4:20: initialization of '__vector(4) float (*)(__vector(4) float)' from \
          incompatible pointer type '__vector(4) float (__attribute__((aarch64_vector_pcs)) \
          *)(__vector(4) float)' [-Wincompatible-pointer-types]"
        ],
        "{w:?}"
    );
    for (name, src) in [
        ("vpcs_redecl", format!("{decl}v4sf vf(v4sf);\n")),
        (
            "vpcs_redecl_after",
            format!("{decl}v4sf vf3(v4sf) __attribute__((aarch64_vector_pcs));\nv4sf vf3(v4sf);\n"),
        ),
    ] {
        let stderr = compile_rejected_with(name, &src, LINUX);
        assert!(stderr.contains("conflicting types"), "{name}:\n{stderr}");
    }
    let w = compile_warnings(
        "vpcs_object",
        "__attribute__((aarch64_vector_pcs)) int x;\n",
        LINUX,
    );
    assert!(
        w.iter()
            .any(|l| l.contains("'aarch64_vector_pcs' attribute only applies to function types")),
        "{w:?}"
    );
    let stderr = compile_rejected_with(
        "vpcs_args",
        "__attribute__((aarch64_vector_pcs(1))) void g(void);\n",
        LINUX,
    );
    assert!(
        stderr.contains("wrong number of arguments specified for 'aarch64_vector_pcs' attribute"),
        "{stderr}"
    );
}

/// A definition is compiled, as gcc compiles one, with nothing to say:
/// its frame preserves all of q8-q23 (`test_asm/codegen_vector_pcs.rs`).
#[test]
fn vector_pcs_definition_is_accepted() {
    let stderr = compile_accepted(
        "vpcs_def",
        "__attribute__((aarch64_vector_pcs)) int f(int x) { return x; }\n\
         static __attribute__((aarch64_vector_pcs)) int g(int x) { return f(x); }\n\
         int (__attribute__((aarch64_vector_pcs)) *p)(int) = g;\n",
        LINUX,
    );
    assert!(stderr.is_empty(), "{stderr}");
}

/// On x86-64 gcc knows no such attribute: it warns that the directive is
/// ignored, `__has_attribute` answers 0, and the type is the plain one.
#[test]
fn vector_pcs_is_ignored_on_x86_64() {
    let w = compile_warnings(
        "vpcs_x86",
        "__attribute__((aarch64_vector_pcs)) int f(int);\n\
         int (*p)(int) = f;\n\
         int f(int x) { return x; }\n\
         _Static_assert(__has_attribute(aarch64_vector_pcs) == 0, \"has\");\n",
        X86,
    );
    assert_eq!(
        w,
        ["1:16: 'aarch64_vector_pcs' attribute directive ignored [-Wattributes]"],
        "{w:?}"
    );
}
