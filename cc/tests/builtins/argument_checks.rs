//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Builtin argument checking: what gcc rejects, c17 must reject
//

use crate::common::{
    compile_and_run_everywhere, compile_expect_error, compile_expect_no_diagnostic,
    compile_expect_warning,
};

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

/// The valid calls compute what gcc computes: a prototyped builtin converts
/// its argument, a type-generic one keeps it, and the flag operations touch
/// one byte.
#[test]
fn builtins_unusual_valid_arguments_compute_as_gcc() {
    let src = r#"enum E { A, B, C };
int main(void) {
    int i = 1;
    if (__builtin_isnanf(i)) return 1;
    if (!__builtin_signbitf(-i)) return 2;
    double d = 0.5;
    if (__builtin_fpclassify(0.5, 10, 20, 30, 40, d) != 20) return 3;
    const double cd = 3.0;
    _Complex double z = __builtin_complex(cd, d);
    if (__real__ z != 3.0 || __imag__ z != 0.5) return 4;
    enum E e = C; _Bool b = 1; unsigned u;
    if (__builtin_sub_overflow(b, e, &u) != 1 || u != 0xffffffffu) return 5;
    int r;
    if (__builtin_sadd_overflow(1.5, 2.9, &r) || r != 3) return 6;
    char arr[10];
    if (__builtin_object_size(arr, B) != 10) return 7;
    if (__builtin_object_size(&arr[4], (int)1.0) != 6) return 8;
    unsigned flag = 0x100;
    if (__atomic_test_and_set(&flag, 5) || flag != 0x101) return 9;
    unsigned other = 0x1ff;
    __atomic_clear(&other, 5);
    if (other != 0x100) return 10;
    char *p = __builtin_alloca(e);
    p[0] = 1;
    if (__builtin_add_overflow_p(100, 100, (signed char)0) != 1) return 11;
    if (!__atomic_always_lock_free(sizeof(int), 0)) return 12;
    return 0;
}
"#;
    compile_and_run_everywhere("argchk_semantics", src);
}
