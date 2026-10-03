//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Declaration constraints: linkage (C17 6.2.2), redeclaration in one scope
// (6.7p3), one definition (6.9p3, p5), tags (6.7.2.3), storage-class and
// function specifiers (6.7.1, 6.7.4, 6.9p2), and the types a declarator may
// derive (6.7.3p2, 6.7.6.3p1). Every rejection here once compiled, most of
// them silently: the duplicate definitions were caught only by the assembler,
// and a block-scope redeclaration simply bound nothing.
//

use crate::common::{
    compile_and_run, compile_expect_error, compile_expect_no_diagnostic, compile_expect_ok,
    compile_expect_warning,
};

/// Each case must be rejected with a diagnostic naming the given text.
fn rejects(cases: &[(&str, &str, &str)]) {
    for (name, src, expected) in cases {
        compile_expect_error(name, src, expected);
    }
}

#[test]
fn redefinitions_are_rejected() {
    rejects(&[
        (
            "rd_block",
            "int f(void) { int x = 1; int x = 2; return x; }\n",
            "redefinition of 'x'",
        ),
        (
            "rd_block_static",
            "void f(void) { static int s; static int s; }\n",
            "redefinition of 's'",
        ),
        (
            "rd_param",
            "void f(int a) { int a; }\n",
            "redefinition of 'a'",
        ),
        (
            "rd_object",
            "int i = 1;\nint i = 2;\n",
            "redefinition of 'i'",
        ),
        (
            "rd_function",
            "void f(void) {}\nvoid f(void) {}\n",
            "redefinition of 'f'",
        ),
        (
            "rd_extern_then_two",
            "extern int k;\nint k = 1;\nint k = 2;\n",
            "redefinition of 'k'",
        ),
    ]);
}

#[test]
fn linkage_conflicts_are_rejected() {
    rejects(&[
        (
            "lk_static_after_plain",
            "int i;\nstatic int i;\n",
            "static declaration of 'i' follows non-static declaration",
        ),
        (
            "lk_static_after_extern",
            "extern int i;\nstatic int i;\n",
            "static declaration of 'i' follows non-static declaration",
        ),
        (
            "lk_static_fn_after",
            "void f(void);\nstatic void f(void) {}\n",
            "static declaration of 'f' follows non-static declaration",
        ),
        (
            "lk_plain_after_static",
            "static int j;\nint j;\n",
            "non-static declaration of 'j' follows static declaration",
        ),
        (
            "lk_extern_after_local",
            "void f(void) { int y; extern int y; }\n",
            "extern declaration of 'y' follows declaration with no linkage",
        ),
        (
            "lk_local_after_extern",
            "void f(void) { extern int y; int y; }\n",
            "declaration of 'y' with no linkage follows extern declaration",
        ),
        (
            "lk_block_static_fn",
            "void f(void) { static void g(void); }\n",
            "invalid storage class for function 'g'",
        ),
    ]);
}

/// Every declaration of one entity has a compatible type (6.2.7p2), in
/// whatever scope it is written.
#[test]
fn types_agree_across_scopes() {
    rejects(&[
        (
            "ty_block_vs_file",
            "int x;\nvoid f(void) { extern long x; }\n",
            "conflicting types for 'x'",
        ),
        (
            "ty_block_vs_block",
            "void f(void) { extern int x; }\nvoid g(void) { extern long x; }\n",
            "conflicting types for 'x'",
        ),
    ]);
}

/// What these rules must keep accepting.
#[test]
fn declarations_that_stay_legal() {
    let src = r#"
int i;
int i;
int i = 3;
extern int i;
static int s;
extern int s;
static int g(void);
int g(void);
static int g(void) { return s; }
int h(void);
extern int h(void);
int h(void) { return 4; }
extern int late;
int late = 5;
void blocks(void) {
    extern int i;
    extern int i;
    { int i = 7; (void)i; }
    extern int h(void);
}
struct S;
struct S { int a; };
struct S *ps;
enum E;
enum E { A };
static inline int inl(void) { return 1; }
int main(void) { blocks(); return (i + g() + h() + late + inl() == 13) ? 0 : 1; }
"#;
    assert_eq!(compile_and_run("decl_legal", src, &[]), 0);

    // A GNU inline-only body emits nothing, so the real one may join it.
    compile_expect_ok(
        "decl_gnu_inline_then_real",
        "extern inline __attribute__((gnu_inline)) int f(void) { return 1; }\n\
         int f(void) { return 2; }\n",
    );
}

#[test]
fn tags_are_declared_once_and_of_one_kind() {
    rejects(&[
        (
            "tag_struct_redef",
            "struct S { int x; };\nstruct S { long y; };\n",
            "redefinition of 'struct S'",
        ),
        (
            "tag_enum_redef",
            "enum E { A };\nenum E { B };\n",
            "redefinition of 'enum E'",
        ),
        (
            "tag_kind",
            "struct S;\nunion S *p;\n",
            "'S' defined as wrong kind of tag",
        ),
        (
            "tag_kind_def",
            "struct S { int a; };\nunion S { int a; };\n",
            "'S' defined as wrong kind of tag",
        ),
        (
            "tag_kind_enum",
            "struct T { int a; };\nenum T e;\n",
            "'T' defined as wrong kind of tag",
        ),
    ]);
    // An inner scope's tag is a new one.
    compile_expect_ok(
        "tag_shadow",
        "struct S { int x; };\nvoid f(void) { struct S { long y; } s; union U { int a; } u; (void)s; (void)u; }\n",
    );
}

#[test]
fn storage_classes_where_they_belong() {
    rejects(&[
        (
            "sc_tl_function",
            "_Thread_local void f(void);\n",
            "invalid storage class for function 'f'",
        ),
        (
            "sc_tl_typedef",
            "typedef _Thread_local int T;\n",
            "_Thread_local cannot be combined with typedef",
        ),
        (
            "sc_tl_block",
            "void f(void) { _Thread_local int d; (void)d; }\n",
            "function-scope 'd' implicitly auto and declared '_Thread_local'",
        ),
        (
            "sc_register_file",
            "register int x;\n",
            "register name not specified for 'x'",
        ),
        (
            "sc_auto_file",
            "auto int x;\n",
            "file-scope declaration of 'x' specifies 'auto'",
        ),
        (
            "sc_for_typedef",
            "void f(void) { for (typedef int T;;) break; }\n",
            "declaration of non-variable 'T' in 'for' loop initial declaration",
        ),
        (
            "sc_for_struct",
            "void f(void) { for (struct S { int a; } s = {0};;) break; }\n",
            "'struct S' declared in 'for' loop initial declaration",
        ),
        (
            "vm_typedef_redef",
            "void f(int n) { typedef int T[n]; typedef int T[n]; }\n",
            "redefinition of typedef 'T' with variably modified type",
        ),
    ]);
    compile_expect_ok(
        "sc_ok",
        "struct P { int q; };\n\
         void f(int n) { static _Thread_local int a; extern _Thread_local int b; \
         for (struct P *p = 0; p; ) break; (void)a; (void)n; }\n",
    );
    // gcc's global register variable names its register, and is accepted.
    if cfg!(target_arch = "x86_64") {
        compile_expect_ok(
            "sc_global_register",
            "register unsigned int reg __asm(\"r14\");\nunsigned f(void) { return reg; }\n",
        );
    }
    for (name, src, expected) in [
        (
            "sc_inline_object",
            "inline int x;\n",
            "variable 'x' declared 'inline'",
        ),
        (
            "sc_noreturn_object",
            "_Noreturn int x;\n",
            "variable 'x' declared '_Noreturn'",
        ),
        (
            "sc_inline_main",
            "inline int main(void) { return 0; }\n",
            "cannot inline function 'main'",
        ),
        (
            "sc_static_incomplete",
            "static int a[];\n",
            "array 'a' assumed to have one element",
        ),
        (
            "sc_unnamed_struct",
            "struct { int a; };\n",
            "unnamed struct/union that defines no instances",
        ),
    ] {
        compile_expect_warning(name, src, expected);
    }
}

#[test]
fn derived_types_c_forbids() {
    rejects(&[
        (
            "dt_returns_array",
            "typedef int A[3];\nA f(void);\n",
            "'f' declared as function returning an array",
        ),
        (
            "dt_returns_function",
            "typedef int F(void);\nF f(void);\n",
            "'f' declared as function returning a function",
        ),
        (
            "dt_restrict_int",
            "restrict int x;\n",
            "invalid use of 'restrict'",
        ),
        (
            "dt_restrict_fnptr",
            "void (* restrict fp)(void);\n",
            "invalid use of 'restrict'",
        ),
        (
            "dt_restrict_typedef",
            "typedef int T;\nrestrict T y;\n",
            "invalid use of 'restrict'",
        ),
        (
            "dt_restrict_param",
            "void f(restrict int x);\n",
            "invalid use of 'restrict'",
        ),
    ]);
    compile_expect_ok(
        "dt_ok",
        "int * restrict p;\nvoid f(int * restrict q, const char * restrict s);\n\
         typedef int *IP;\nrestrict IP r;\nint (*fp(void))[3];\nint (*gp(void))(void);\n",
    );
}

/// C17 6.9.2p2: a file-scope array declared without an extent is judged at
/// the end of the unit. Completed by a later declaration it is that size,
/// silently; still incomplete, it has one element and gcc's warning, once.
/// The completed array's storage was the incomplete declaration's eight
/// bytes, so `a[2]` overlapped the next object.
#[test]
fn tentative_array_completed_later_in_the_unit() {
    compile_expect_no_diagnostic(
        "tentative_array_completed",
        "static int a[];\nstatic int a[3] = {1, 2, 3};\nint *g(void) { return a; }\n",
        "assumed to have one element",
    );
    compile_expect_warning(
        "tentative_array_static",
        "static int a[];\nint *g(void) { return a; }\n",
        "array 'a' assumed to have one element",
    );
    compile_expect_warning(
        "tentative_array_external",
        "int a[];\nint *g(void) { return a; }\n",
        "array 'a' assumed to have one element",
    );
    let src = "static int a[];\nstatic int a[3];\nstatic int b[3];\n\
               int c[];\nint c[3];\nint d[3];\nint e[3];\nint e[];\n\
               int main(void) {\n\
                   for (int i = 0; i < 3; i++) {\n\
                       a[i] = i + 1; b[i] = 10 + i; c[i] = 20 + i;\n\
                       d[i] = 30 + i; e[i] = 40 + i;\n\
                   }\n\
                   for (int i = 0; i < 3; i++)\n\
                       if (a[i] != i + 1 || b[i] != 10 + i || c[i] != 20 + i\n\
                           || d[i] != 30 + i || e[i] != 40 + i)\n\
                           return 1;\n\
                   return sizeof a == 12 && sizeof c == 12 && sizeof e == 12 ? 0 : 2;\n\
               }\n";
    assert_eq!(compile_and_run("tentative_array_storage", src, &[]), 0);
    // A block-scope `extern` names the same object: written before the
    // definition, its extent is the object's; after it, too late.
    let src = "int *g(void) { extern int a[5]; return a; }\nint a[];\nint b[5];\n\
               int main(void) {\n\
                   int *p = g();\n\
                   for (int i = 0; i < 5; i++) { p[i] = i + 1; b[i] = 10 + i; }\n\
                   for (int i = 0; i < 5; i++)\n\
                       if (p[i] != i + 1 || b[i] != 10 + i) return 1;\n\
                   return 0;\n\
               }\n";
    assert_eq!(compile_and_run("tentative_array_block_extern", src, &[]), 0);
    compile_expect_error(
        "tentative_array_block_extern_late",
        "int a[];\nint *g(void) { extern int a[5]; return a; }\n",
        "type of array 'a' completed incompatibly with implicit initialization",
    );
}

/// A tag first declared in a parameter list has prototype scope, so nothing
/// outside can name the same type. gcc warns.
#[test]
fn tag_declared_in_a_parameter_list_warns() {
    for (name, src, expected) in [
        (
            "tag_param_struct",
            "void f(struct S *p);\n",
            "'struct S' declared inside parameter list will not be visible",
        ),
        (
            "tag_param_anon",
            "void q(struct { int a; } *p);\n",
            "anonymous struct declared inside parameter list",
        ),
        (
            "tag_param_enum",
            "void r(enum E { A } e);\n",
            "'enum E' declared inside parameter list",
        ),
    ] {
        compile_expect_warning(name, src, expected);
    }
    // Declared first, it is the file's tag, and there is nothing to say.
    compile_expect_ok("tag_param_visible", "struct T;\nvoid m(struct T *p);\n");
}
