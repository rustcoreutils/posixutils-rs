//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compile-only cases of the builtins suite (tests/builtins), in process.
//

use crate::test_compile::{
    compile_expect_error, compile_expect_no_diagnostic, compile_expect_warning,
};

// ============================================================================
// tests/builtins/argument_checks.rs:
// Builtin argument checking: what gcc rejects, c17 must reject
// ============================================================================

/// Each of these is an error in gcc, in the words checked here, and c17
/// compiled every one without a diagnostic -- generating code for a call
/// whose arguments meant nothing: a classification of an `int`'s bits, an
/// overflow result written through a `double *`, a fence order read from a
/// variable. None needs a header: every name is a builtin.
#[test]
fn builtins_invalid_arguments_are_rejected() {
    for (name, src, expected) in [
        (
            "isnan_int",
            "int f(int i){return __builtin_isnan(i);}",
            "non-floating-point argument in call to function",
        ),
        (
            "isinf_int",
            "int f(int i){return __builtin_isinf(i);}",
            "non-floating-point argument in call to function",
        ),
        (
            "isnormal_int",
            "int f(int i){return __builtin_isnormal(i);}",
            "non-floating-point argument in call to function",
        ),
        (
            "isinf_sign_int",
            "int f(int i){return __builtin_isinf_sign(i);}",
            "non-floating-point argument in call to function",
        ),
        (
            "fpclassify_int",
            "int f(int i){return __builtin_fpclassify(0,1,2,3,4,i);}",
            "non-floating-point argument in call to function",
        ),
        (
            "complex_int",
            "void f(void){_Complex double z = __builtin_complex(1, 2);(void)z;}",
            "operand not of real binary floating-point type",
        ),
        (
            "addov_double",
            "double d; int f(void){return __builtin_add_overflow(1, 2, &d);}",
            "does not have pointer to integral type",
        ),
        (
            "addov_notptr",
            "int r; int f(void){return __builtin_add_overflow(1, 2, r);}",
            "does not have pointer to integral type",
        ),
        (
            "addov_float_op",
            "int r; int f(void){return __builtin_add_overflow(1.0, 2, &r);}",
            "does not have integral type",
        ),
        (
            "objsize_var",
            "int f(char *p, int i){return __builtin_object_size(p, i);}",
            "is not integer constant between 0 and 3",
        ),
        (
            "objsize_range",
            "int f(char *p){return __builtin_object_size(p, 5);}",
            "is not integer constant between 0 and 3",
        ),
        (
            "prefetch_var",
            "void f(char *p, int i){__builtin_prefetch(p, i);}",
            "must be a constant",
        ),
        (
            "atomic_double",
            "double d; void f(void){__atomic_fetch_add(&d, 1, 0);}",
            "is incompatible with argument 1",
        ),
        (
            "atomic_notptr",
            "int i; int f(void){return __atomic_load_n(i, 0);}",
            "is incompatible with argument 1",
        ),
        (
            "lockfree_var",
            "int f(int i){return __atomic_always_lock_free(i, 0);}",
            "non-constant argument 1",
        ),
        (
            "vastart_nonvar",
            "void f(int a){__builtin_va_list ap; __builtin_va_start(ap, a); __builtin_va_end(ap);}",
            "used in function with fixed arguments",
        ),
        (
            "pow_arity",
            "double f(void){return __builtin_pow(1.0);}",
            "too few arguments to function",
        ),
        (
            "sin_ptr",
            "double f(char *p){return __builtin_sin(p);}",
            "incompatible type for argument 1",
        ),
        (
            "ffs_arity",
            "int f(void){return __builtin_ffs(1, 2);}",
            "too many arguments to function",
        ),
        (
            "memcpy_chk_arity",
            "void f(char *p){__builtin___memcpy_chk(p, p);}",
            "too few arguments to function",
        ),
    ] {
        compile_expect_error(&format!("argchk_{name}"), &format!("{src}\n"), expected);
    }
}

/// The rest of each family gcc checks the same way, and the rules past the
/// first: the other members, a `_Bool`, enumerated or `const` result, the
/// count, and a library builtin with no header for every kind of fault.
#[test]
fn builtins_invalid_arguments_are_rejected_family_wide() {
    let decls = "char *p; int i; double d; _Bool b; enum E { A } e; const int ci = 0;\
                 struct S { int a; } s; _Complex double z;";
    for (name, body, expected) in [
        (
            "isfinite_ptr",
            "return __builtin_isfinite(p);",
            "non-floating-point argument in call to function '__builtin_isfinite'",
        ),
        (
            "signbit_complex",
            "return __builtin_signbit(z);",
            "non-floating-point argument in call to function '__builtin_signbit'",
        ),
        (
            "isnan_two",
            "return __builtin_isnan(d, d);",
            "too many arguments to function '__builtin_isnan'",
        ),
        (
            "isnanf_struct",
            "return __builtin_isnanf(s);",
            "incompatible type for argument 1 of '__builtin_isnanf'",
        ),
        (
            "fpclassify_class",
            "return __builtin_fpclassify(i, 1, 2, 3, 4, d);",
            "non-const integer argument 1 in call to function '__builtin_fpclassify'",
        ),
        (
            "fpclassify_five",
            "return __builtin_fpclassify(1, 2, 3, 4, d);",
            "too few arguments to function '__builtin_fpclassify'",
        ),
        (
            "complex_mixed",
            "return __real__ __builtin_complex(1.0f, 2.0);",
            "'__builtin_complex' operands of different types",
        ),
        (
            "complex_of_complex",
            "return __real__ __builtin_complex(z, z);",
            "operand not of real binary floating-point type",
        ),
        (
            "subov_bool",
            "return __builtin_sub_overflow(1, 2, &b);",
            "argument 3 in call to function '__builtin_sub_overflow' has pointer to boolean type",
        ),
        (
            "mulov_enum",
            "return __builtin_mul_overflow(1, 2, &e);",
            "has pointer to enumerated type",
        ),
        (
            "addov_const",
            "return __builtin_add_overflow(1, 2, &ci);",
            "has pointer to 'const' type ('const int *')",
        ),
        (
            "addov_ptr_op",
            "return __builtin_add_overflow(p, 2, &i);",
            "argument 1 in call to function '__builtin_add_overflow' does not have integral type",
        ),
        (
            "subov_p_double",
            "return __builtin_sub_overflow_p(1, 2, d);",
            "argument 3 in call to function '__builtin_sub_overflow_p' does not have integral type",
        ),
        (
            "mulov_p_bool",
            "return __builtin_mul_overflow_p(1, 2, b);",
            "has boolean type",
        ),
        (
            "addov_p_enum",
            "return __builtin_add_overflow_p(1, 2, e);",
            "has enumerated type",
        ),
        (
            "addov_two",
            "return __builtin_add_overflow(1, 2);",
            "too few arguments to function '__builtin_add_overflow'",
        ),
        (
            "sadd_four",
            "return __builtin_sadd_overflow(1, 2, &i, 4);",
            "too many arguments to function '__builtin_sadd_overflow'",
        ),
        (
            "objsize_struct",
            "return __builtin_object_size(s, 0);",
            "incompatible type for argument 1 of '__builtin_object_size'",
        ),
        (
            "objsize_double",
            "return __builtin_object_size(p, d);",
            "is not integer constant between 0 and 3",
        ),
        (
            "objsize_const_obj",
            "return __builtin_object_size(p, ci);",
            "is not integer constant between 0 and 3",
        ),
        (
            "prefetch_float",
            "__builtin_prefetch(p, 1.0); return 0;",
            "second argument to '__builtin_prefetch' must be a constant",
        ),
        (
            "prefetch_locality",
            "__builtin_prefetch(p, 1, i); return 0;",
            "third argument to '__builtin_prefetch' must be a constant",
        ),
        (
            "prefetch_none",
            "__builtin_prefetch(); return 0;",
            "too few arguments to function '__builtin_prefetch'",
        ),
        (
            "sync_bool",
            "return __sync_fetch_and_add(&b, 1);",
            "operand type '_Bool *' is incompatible with argument 1 of '__sync_fetch_and_add'",
        ),
        (
            "atomic_exchange_struct",
            "return __atomic_exchange_n(&s, s, 0).a;",
            "is incompatible with argument 1 of '__atomic_exchange_n'",
        ),
        (
            "atomic_cas_double",
            "return __atomic_compare_exchange_n(&d, &d, 1.0, 0, 5, 5);",
            "operand type 'double *' is incompatible with argument 1",
        ),
        (
            "sync_release_int",
            "__sync_lock_release(i); return 0;",
            "operand type 'int' is incompatible with argument 1 of '__sync_lock_release'",
        ),
        (
            "is_lock_free_one",
            "return __atomic_is_lock_free(4);",
            "too few arguments to function '__atomic_is_lock_free'",
        ),
        (
            "alloca_two",
            "return *(char *)__builtin_alloca(1, 2);",
            "too many arguments to function '__builtin_alloca'",
        ),
        (
            "trap_arg",
            "__builtin_trap(1); return 0;",
            "too many arguments to function '__builtin_trap'",
        ),
        (
            "malloc_none",
            "return __builtin_malloc() != 0;",
            "too few arguments to function '__builtin_malloc'",
        ),
        (
            "strspn_one",
            "return __builtin_strspn(p);",
            "too few arguments to function '__builtin_strspn'",
        ),
        (
            "sprintf_chk_three",
            "return __builtin___sprintf_chk(p, 0, 9);",
            "too few arguments to function '__builtin___sprintf_chk'",
        ),
        (
            "powl_struct",
            "return __builtin_powl(s, d);",
            "incompatible type for argument 1 of '__builtin_powl'",
        ),
    ] {
        let src = format!("{decls}\nint f(void) {{ {body} }}\n");
        compile_expect_error(&format!("argchk_family_{name}"), &src, expected);
    }
}

/// What gcc only warns about, c17 only warns about: a prefetch hint out of
/// range (zero is used), a `va_start` naming a parameter that is not the
/// last, and an argument a builtin's prototype converts with a warning.
#[test]
fn builtins_questionable_arguments_are_warned_about() {
    let decls = "char *p; int i;";
    for (name, body, expected) in [
        (
            "prefetch_rw",
            "__builtin_prefetch(p, 2);",
            "invalid second argument to '__builtin_prefetch'; using zero",
        ),
        (
            "prefetch_locality",
            "__builtin_prefetch(p, 0, -1);",
            "invalid third argument to '__builtin_prefetch'; using zero",
        ),
        (
            "prefetch_int",
            "__builtin_prefetch(i);",
            "passing argument 1 of '__builtin_prefetch'",
        ),
        (
            "objsize_int",
            "(void)__builtin_object_size(i, 0);",
            "passing argument 1 of '__builtin_object_size'",
        ),
        (
            "test_and_set_int",
            "(void)__atomic_test_and_set(i, 0);",
            "passing argument 1 of '__atomic_test_and_set'",
        ),
        (
            "malloc_ptr",
            "(void)__builtin_malloc(p);",
            "passing argument 1 of '__builtin_malloc'",
        ),
        (
            "memcpy_chk_int",
            "(void)__builtin___memcpy_chk(i, p, 1, 2);",
            "passing argument 1 of '__builtin___memcpy_chk'",
        ),
    ] {
        let src = format!("{decls}\nvoid f(void) {{ {body} }}\n");
        compile_expect_warning(&format!("argchk_warn_{name}"), &src, expected);
    }
    compile_expect_warning(
        "argchk_warn_va_start_not_last",
        "void f(int a, int b, ...) { __builtin_va_list ap; __builtin_va_start(ap, a); __builtin_va_end(ap); }\n",
        "second parameter of 'va_start' not last named argument",
    );
}

/// Every call here is one gcc accepts without a word, with the unusual
/// arguments each family must still take: an enumeration or `_Bool` where an
/// integer goes, `long double` and `_Float16` where a floating value goes, a
/// pointer to `const` or `volatile`, an array that decays, a constant spelled
/// as an enumerator or a cast. No header is included: every name is a
/// builtin.
#[test]
fn builtins_unusual_valid_arguments_are_accepted() {
    let decls = "char *p; const char *cp; int i; double d; float f; long double ld; _Bool b;\
                 enum E { A, B } e; struct S { int x; } s; int arr[4]; _Float16 h;\
                 unsigned u; long l; long long ll; const double cd = 1; __int128 w;\
                 volatile int vi; void *vp; const int ci = 0;";
    let calls = [
        "__builtin_isnan(f)",
        "__builtin_isnan(ld)",
        "__builtin_isnan(h)",
        "__builtin_isnan(cd)",
        "__builtin_isinf_sign(ld)",
        "__builtin_isnormal(h)",
        "__builtin_fpclassify(A, B, 2, 3, (char)4, f)",
        "__builtin_fpclassify(0.5, 1, 2, 3, 4, ld)",
        "__builtin_isnanf(i)",
        "__builtin_isinfl(e)",
        "__builtin_isnanl(b)",
        "__builtin_signbitf(i)",
        "__builtin_signbit(h)",
        "__builtin_complex(1.0L, 2.0L)",
        "__builtin_complex(f, f)",
        "__builtin_complex(h, h)",
        "__builtin_complex(cd, d)",
        "__builtin_add_overflow(e, b, &u)",
        "__builtin_sub_overflow(A, 'c', &ll)",
        "__builtin_mul_overflow(i, l, arr)",
        "__builtin_add_overflow(w, 1, &w)",
        "__builtin_add_overflow(1, 2, &vi)",
        "__builtin_add_overflow_p(1, 2, (short)0)",
        "__builtin_mul_overflow_p(e, b, 0UL)",
        "__builtin_sadd_overflow(1.5, 2, &i)",
        "__builtin_sadd_overflow(e, b, &i)",
        "__builtin_object_size(arr, A)",
        "__builtin_object_size(cp, (int)1.0)",
        "__builtin_object_size(p, sizeof(int) - 3)",
        "__builtin_object_size(p, 1.0)",
        "__builtin_object_size(\"str\", 0)",
        "__builtin_object_size(&s, B)",
        "__builtin_object_size(vp, 3)",
        "__builtin_prefetch(cp)",
        "__builtin_prefetch(arr)",
        "__builtin_prefetch(p, A, B)",
        "__builtin_prefetch(p, 0, 3)",
        "__builtin_prefetch(p, (int)1.0)",
        "__builtin_prefetch(p, 1, 3, i)",
        "__atomic_load_n(&e, 0)",
        "__atomic_load_n(&b, 0)",
        "__atomic_load_n(&p, 0)",
        "__atomic_load_n(&ci, 0)",
        "__atomic_fetch_add(&p, 1, 0)",
        "__atomic_fetch_add(arr, 1, 0)",
        "__atomic_fetch_add(&e, 1, 0)",
        "__atomic_fetch_or(&w, 1, 0)",
        "__atomic_store_n(&vi, 1, 0)",
        "__atomic_exchange_n(&b, 1, 0)",
        "__atomic_compare_exchange_n(&p, &p, p, 0, 5, 5)",
        "__sync_lock_test_and_set(&b, 1)",
        "__sync_val_compare_and_swap(&p, p, p)",
        "__sync_fetch_and_add(&ll, 1)",
        "__atomic_always_lock_free(sizeof(int), 0)",
        "__atomic_always_lock_free(A, &i)",
        "__atomic_always_lock_free(8, &vi)",
        "__atomic_is_lock_free(i, 0)",
        "__atomic_test_and_set(&d, 0)",
        "__atomic_test_and_set(&vi, 5)",
        "__atomic_clear(&b, 0)",
        "__builtin_pow(1, 2)",
        "__builtin_sinl(e)",
        "__builtin_sin(b)",
        "__builtin_frexpl(ld, &i)",
        "__builtin_modff(f, &f)",
        "__builtin_ldexp(d, e)",
        "__builtin_nanf16(\"\")",
        "__builtin_nanl(cp)",
        "__builtin___memcpy_chk(arr, cp, 4, 16)",
        "__builtin___memset_chk(p, A, 4, 16)",
        "__builtin___sprintf_chk(p, 0, 9, \"%d\", 1)",
        "__builtin___snprintf_chk(p, 9, 0, 9, \"%s\", cp)",
        "__builtin_strdup(p)",
        "__builtin_malloc(e)",
        "__builtin_calloc(b, 1)",
        "__builtin_alloca(e)",
        "__builtin_snprintf(p, 4, \"%d\", 1)",
        "__builtin_bcmp(arr, cp, 2)",
        "__builtin_ffs(e)",
        "__builtin_ffsll(b)",
        "__builtin_assume_aligned(cp, 16, e)",
    ];
    let body: String = calls
        .iter()
        .map(|call| format!("    (void)({call});\n"))
        .collect();
    let src = format!(
        "{decls}\nvoid calls(void) {{\n{body}}}\n\
         void va(int a, double x, ...) {{ __builtin_va_list ap; __builtin_va_start(ap, x); \
         __builtin_va_end(ap); (void)a; }}\n\
         void ends(void) {{ if (i) __builtin_exit(2); if (i) __builtin_abort(); if (i) __builtin_trap(); }}\n"
    );
    compile_expect_no_diagnostic("argchk_unusual_valid", &src, "warning");
}

/// A header declaring a function after a `__builtin_` call has declared it
/// agrees with the declaration the call made: the table's prototype is the
/// library's own.
#[test]
fn builtins_library_declaration_after_a_call_agrees() {
    compile_expect_no_diagnostic(
        "argchk_declared_after",
        "void g(char *p) { (void)__builtin___memcpy_chk(p, p, 1, 2); }\n\
         extern void *__memcpy_chk(void *restrict, const void *restrict, unsigned long, unsigned long);\n\
         extern double pow(double, double);\n\
         double h(void) { return __builtin_pow(2, 3) + pow(1, 2); }\n",
        "warning",
    );
}

/// `va_arg`'s type must be a complete object type (C17 7.16.1.1p2), and gcc
/// rejects `void` and an incomplete structure outright; c17 accepted both.
#[test]
fn builtins_va_arg_of_an_incomplete_type_is_rejected() {
    compile_expect_error(
        "va_arg_void",
        "void f(__builtin_va_list ap) { __builtin_va_arg(ap, void); }\n",
        "second argument to 'va_arg' is of incomplete type 'void'",
    );
    compile_expect_error(
        "va_arg_incomplete",
        "struct I; void f(__builtin_va_list ap) { __builtin_va_arg(ap, struct I); }\n",
        "second argument to 'va_arg' is of incomplete type 'struct I'",
    );
    compile_expect_error(
        "va_arg_incomplete_enum",
        "enum E; void f(__builtin_va_list ap) { __builtin_va_arg(ap, enum E); }\n",
        "second argument to 'va_arg' is of incomplete type 'enum E'",
    );
    compile_expect_error(
        "va_arg_incomplete_array",
        "void f(__builtin_va_list ap) { __builtin_va_arg(ap, int[]); }\n",
        "second argument to 'va_arg' is of incomplete type 'int[]'",
    );
    // gcc names the type unqualified.
    compile_expect_error(
        "va_arg_const_void",
        "void f(__builtin_va_list ap) { __builtin_va_arg(ap, const void); }\n",
        "second argument to 'va_arg' is of incomplete type 'void'",
    );
    compile_expect_error(
        "va_arg_function",
        "void f(__builtin_va_list ap) { __builtin_va_arg(ap, int(void)); }\n",
        "second argument to 'va_arg' is a function type 'int(void)'",
    );
}

/// A type the default argument promotions change is never what a caller
/// passed through `...`, and gcc warns; a complete array, a pointer to a
/// variably modified array, an enumeration and a complex `float` are taken
/// without a word.
#[test]
fn builtins_va_arg_of_a_promoted_type_warns() {
    compile_expect_warning(
        "va_arg_char",
        "void f(__builtin_va_list ap) { __builtin_va_arg(ap, const char); }\n",
        "'char' is promoted to 'int' when passed through '...'",
    );
    compile_expect_warning(
        "va_arg_float",
        "void f(__builtin_va_list ap) { __builtin_va_arg(ap, float); }\n",
        "'float' is promoted to 'double' when passed through '...'",
    );
    compile_expect_warning(
        "va_arg_bool",
        "void f(__builtin_va_list ap) { __builtin_va_arg(ap, _Bool); }\n",
        "'_Bool' is promoted to 'int' when passed through '...'",
    );
    compile_expect_no_diagnostic(
        "va_arg_unpromoted",
        "enum G { Z };\n\
         void f(int n, __builtin_va_list ap) {\n\
             __builtin_va_arg(ap, int[3]); __builtin_va_arg(ap, int(*)[n]);\n\
             __builtin_va_arg(ap, enum G); __builtin_va_arg(ap, float _Complex);\n\
         }\n",
        "promoted",
    );
}

// ============================================================================
// tests/builtins/bit_ops.rs:
// Bit Operations Builtins Mega-Test
//
// Consolidates: clz, ctz, popcount, bswap tests
// ============================================================================

// ============================================================================
// popcount and parity on the x86-64 baseline
// ============================================================================

/// An argument the prototype cannot convert is diagnosed as in any call, in
/// gcc's words: a structure is an error, a pointer the integer-from-pointer
/// warning an ordinary call draws.
#[test]
fn builtins_bit_ops_check_their_argument() {
    for (name, call, param) in [
        ("ctz", "__builtin_ctz(s)", "unsigned int"),
        ("parityl", "__builtin_parityl(s)", "unsigned long"),
        ("bswap16", "__builtin_bswap16(s)", "unsigned short"),
        ("clrsbll", "__builtin_clrsbll(s)", "long long"),
        ("ffs", "__builtin_ffs(s)", "int"),
    ] {
        let builtin = call.split('(').next().unwrap();
        compile_expect_error(
            &format!("bit_ops_struct_{name}"),
            &format!("struct S {{ int a; }};\nint f(struct S s) {{ return {call}; }}\n"),
            &format!(
                "incompatible type for argument 1 of '{builtin}': \
                 expected '{param}', got 'struct S'"
            ),
        );
    }
    compile_expect_warning(
        "bit_ops_pointer",
        "int f(unsigned *p) { return __builtin_popcount(p); }\n",
        "passing argument 1 of '__builtin_popcount' as 'unsigned int' from 'unsigned int *' \
         makes integer from pointer without a cast",
    );
}

// ============================================================================
// tests/builtins/frame_address.rs:
// __builtin_return_address / __builtin_frame_address at a level above zero
// ============================================================================

/// gcc requires the level to be an integer constant ("invalid argument to
/// '__builtin_return_address'"): walking a run-time number of frames is not
/// something either builtin does.
#[test]
fn builtins_frame_level_must_be_constant() {
    for builtin in ["__builtin_return_address", "__builtin_frame_address"] {
        let src = format!("void *f(int n) {{ return {builtin}(n); }}\n");
        compile_expect_error("frame_level_nonconst", &src, builtin);
    }
}

// ============================================================================
// tests/builtins/gnu_batch.rs:
// gcc builtins real code uses that c17 lacked: the `_FloatN` and `q`
// spellings of fabs, copysign, inf, nan and friends (glibc's <math.h>
// reaches for them), `sqrtf128`/`fmaf128`, the position builtins
// `__builtin_FILE`/`LINE`/`FUNCTION`, `__builtin_expect_with_probability`,
// `__builtin_dynamic_object_size`, and x86's CPU detection.
// ============================================================================

/// `#line` moves diagnostics, as gcc's does, not only `__LINE__`.
#[test]
fn builtins_line_directive_moves_diagnostics() {
    compile_expect_error(
        "line_moves_diagnostics",
        "int a;\n#line 77 \"renamed.c\"\nint b = undeclared_x;\n",
        "renamed.c:77:",
    );
}

// ============================================================================
// tests/builtins/intrinsics.rs:
// Intrinsic Builtins Mega-Test
//
// Consolidates: types_compatible, constant_p, unreachable, expect tests
// ============================================================================

// ============================================================================
// Integer magnitude: abs, labs, llabs, imaxabs
// ============================================================================

/// A `__builtin_` library alias the translation unit never declared is
/// declared by c17 from what it knows of the entry point, with parameter
/// types that are placeholders. A later call finds that declaration in
/// scope, and must not be checked against it: `strlen` does not take an
/// `unsigned long`.
#[test]
fn builtins_undeclared_library_alias_is_not_checked_against_placeholders() {
    let code = "int f(void) {\n\
                    return (int)__builtin_strlen(\"a\") + (int)__builtin_strlen(\"bc\")\n\
                        + __builtin_strcmp(\"a\", \"b\") + __builtin_strcmp(\"c\", \"d\");\n\
                }\n";
    compile_expect_no_diagnostic("undeclared_library_alias", code, "argument");
}

/// What gcc rejects in a call to `__builtin_assume_aligned`, in its words
/// where c17's call checks share them: too many arguments, a misalignment
/// that is not an integer, and a first argument no conversion makes a
/// pointer.
#[test]
fn builtins_assume_aligned_rejects_bad_arguments() {
    for (name, call, expected) in [
        (
            "assume_aligned_too_many",
            "__builtin_assume_aligned(p, 16, 0, 1)",
            "too many arguments to function '__builtin_assume_aligned'",
        ),
        (
            "assume_aligned_too_few",
            "__builtin_assume_aligned(p)",
            "too few arguments to function '__builtin_assume_aligned'",
        ),
        (
            "assume_aligned_float_misalign",
            "__builtin_assume_aligned(p, 16, 1.5)",
            "non-integer argument 3 in call to function '__builtin_assume_aligned'",
        ),
        (
            "assume_aligned_ptr_misalign",
            "__builtin_assume_aligned(p, 16, p)",
            "non-integer argument 3 in call to function '__builtin_assume_aligned'",
        ),
        (
            "assume_aligned_struct",
            "__builtin_assume_aligned(s, 16)",
            "incompatible type for argument 1 of '__builtin_assume_aligned'",
        ),
    ] {
        let code = format!(
            "struct S {{ int a; }} s; char *p;\n\
             void *f(void) {{ return {call}; }}\n"
        );
        compile_expect_error(name, &code, expected);
    }
}

/// gcc warns, as for any call through `const void *`: an integer made a
/// pointer, a pointer made a `size_t`, and a `volatile` the parameter drops.
/// An alignment that is a variable, or not a power of two, is accepted
/// without a word, and a `const` pointee is not one dropped.
#[test]
fn builtins_assume_aligned_warns_as_a_call_does() {
    for (name, call, expected) in [
        (
            "assume_aligned_int_ptr",
            "__builtin_assume_aligned(n, 16)",
            "makes pointer from integer without a cast",
        ),
        (
            "assume_aligned_ptr_align",
            "__builtin_assume_aligned(c, c)",
            "makes integer from pointer without a cast",
        ),
        (
            "assume_aligned_volatile",
            "__builtin_assume_aligned(v, 16)",
            "discards a qualifier from the pointer target type",
        ),
    ] {
        let code = format!(
            "int n; char *c; volatile int *v;\n\
             void *f(void) {{ return {call}; }}\n"
        );
        compile_expect_warning(name, &code, expected);
    }
    compile_expect_no_diagnostic(
        "assume_aligned_accepted",
        "const char *p; int n;\n\
         char *f(void) { return __builtin_assume_aligned(p, n, n); }\n\
         char *g(void) { return __builtin_assume_aligned(p, 3); }\n",
        "warning",
    );
}

// ============================================================================
// tests/builtins/va_arg_pack.rs:
// __builtin_va_arg_pack / __builtin_va_arg_pack_len
// ============================================================================

/// Both builtins are meaningless outside an `always_inline` variadic
/// function -- there is no caller whose arguments they could name. GCC
/// rejects the program; so must c17, rather than silently producing nothing.
#[test]
fn builtins_va_arg_pack_outside_a_forwarding_function_is_rejected() {
    compile_expect_error(
        "va_arg_pack_not_variadic",
        "__attribute__((always_inline)) static inline int f(int x) {\n\
         return x + __builtin_va_arg_pack_len(); }\n\
         int main(void) { return f(1); }\n",
        "va_arg_pack",
    );
    compile_expect_error(
        "va_arg_pack_not_always_inline",
        "static int f(const char *t, ...) {\n\
         (void)t; return __builtin_va_arg_pack_len(); }\n\
         int main(void) { return f(\"x\", 1); }\n",
        "va_arg_pack",
    );
    compile_expect_error(
        "va_arg_pack_at_file_scope",
        "int x = __builtin_va_arg_pack_len();\nint main(void) { return x; }\n",
        "va_arg_pack",
    );
}

/// gcc's `__builtin_longjmp` takes only the constant 1, and both builtins
/// are prototyped, so a wrong count is reported as for any call -- each in
/// gcc's words. `0 + 1` is the constant 1 and is accepted.
#[test]
fn builtins_setjmp_longjmp_arguments_are_checked() {
    for (name, src, expected) in [
        (
            "longjmp_two",
            "void *b[5]; void f(void){ __builtin_longjmp(b, 2); }",
            "'__builtin_longjmp' second argument must be 1",
        ),
        (
            "longjmp_variable",
            "void *b[5]; void f(int v){ __builtin_longjmp(b, v); }",
            "'__builtin_longjmp' second argument must be 1",
        ),
        (
            "longjmp_one_arg",
            "void *b[5]; void f(void){ __builtin_longjmp(b); }",
            "too few arguments to function '__builtin_longjmp'",
        ),
        (
            "setjmp_no_arg",
            "int f(void){ return __builtin_setjmp(); }",
            "too few arguments to function '__builtin_setjmp'",
        ),
        (
            "setjmp_two_args",
            "void *b[5]; int f(void){ return __builtin_setjmp(b, b); }",
            "too many arguments to function '__builtin_setjmp'",
        ),
    ] {
        compile_expect_error(name, src, expected);
    }
    compile_expect_no_diagnostic(
        "longjmp_constant_expression",
        "void *b[5]; void f(void){ __builtin_longjmp(b, 0 + 1); }",
        "second argument",
    );
}

// ============================================================================
// tests/builtins/gcc_lowering.rs:
// The builtins gcc makes for its own lowering, misused
// ============================================================================

/// `__builtin_clear_padding` takes one pointer to a complete, non-`const`
/// object type with well-defined padding, and returns nothing; the others
/// are checked through gcc's prototypes. Each in gcc 13's words.
#[test]
fn builtins_gcc_lowering_misuse_is_rejected() {
    for (name, src, expected) in [
        (
            "padding_not_pointer",
            "struct S { int a; char b; }; void f(struct S s){__builtin_clear_padding(s);}",
            "argument 1 in call to function '__builtin_clear_padding' does not have pointer type",
        ),
        (
            "padding_null_constant",
            "void f(void){__builtin_clear_padding(0);}",
            "does not have pointer type",
        ),
        (
            "padding_void",
            "void f(void *p){__builtin_clear_padding(p);}",
            "argument 1 in call to function '__builtin_clear_padding' points to incomplete type",
        ),
        (
            "padding_incomplete_struct",
            "struct I; void f(struct I *p){__builtin_clear_padding(p);}",
            "points to incomplete type",
        ),
        (
            "padding_incomplete_array",
            "void f(int (*p)[]){__builtin_clear_padding(p);}",
            "points to incomplete type",
        ),
        (
            "padding_const",
            "struct S { int a; char b; }; void f(const struct S *p){__builtin_clear_padding(p);}",
            "argument 1 in call to function '__builtin_clear_padding' has pointer to 'const' type ('const struct S *')",
        ),
        (
            "padding_flexible",
            "struct F { int n; char c[]; }; void f(struct F *p){__builtin_clear_padding(p);}",
            "flexible array member 'c' does not have well defined padding bits for '__builtin_clear_padding'",
        ),
        (
            "padding_too_many",
            "struct S { int a; }; void f(struct S *p){__builtin_clear_padding(p, 1);}",
            "too many arguments to function '__builtin_clear_padding'",
        ),
        (
            "padding_too_few",
            "void f(void){__builtin_clear_padding();}",
            "too few arguments to function '__builtin_clear_padding'",
        ),
        (
            "padding_void_value",
            "struct S { int a; }; int f(struct S *p){return __builtin_clear_padding(p);}",
            "void value not ignored as it ought to be",
        ),
        (
            "stack_save_args",
            "void f(void){__builtin_stack_save(1);}",
            "too many arguments to function '__builtin_stack_save'",
        ),
        (
            "stack_restore_double",
            "void f(void){__builtin_stack_restore(1.0);}",
            "incompatible type for argument 1 of '__builtin_stack_restore'",
        ),
        (
            "stack_restore_void_value",
            "int f(void){return __builtin_stack_restore(0);}",
            "void value not ignored as it ought to be",
        ),
        (
            "cexpi_pointer",
            "void f(int *p){__builtin_cexpi(p);}",
            "incompatible type for argument 1 of '__builtin_cexpi'",
        ),
        (
            "cpow_arity",
            "void f(void){__builtin_cpow(1.0);}",
            "too few arguments to function '__builtin_cpow'",
        ),
    ] {
        compile_expect_error(name, src, expected);
    }
}

/// What gcc accepts: a pointer to a variable length array, an array that
/// decays to a pointer, a `volatile` object and a pointer to a function.
#[test]
fn builtins_clear_padding_accepts_what_gcc_does() {
    compile_expect_no_diagnostic(
        "padding_accepted",
        "struct S { char a; long b; };\n\
         void f(int n, volatile struct S *v, void (*fn)(void)) {\n\
             struct S a[n][2], b[3];\n\
             __builtin_clear_padding(a);\n\
             __builtin_clear_padding(&a);\n\
             __builtin_clear_padding(b);\n\
             __builtin_clear_padding(v);\n\
             __builtin_clear_padding(fn);\n\
         }\n",
        "__builtin_clear_padding",
    );
}
