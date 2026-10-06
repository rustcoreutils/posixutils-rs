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

use crate::common::{compile_and_run, compile_expect_error, compile_object_run};

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
}

/// `static` and qualifiers in `[ ]` qualify the parameter's own array type,
/// however its name is parenthesized; through a pointer they do not.
#[test]
fn static_array_parameter_with_parenthesized_name() {
    let src = "static int sum(int (a)[static 3], int ((b))[const 2]) {\n\
                   return a[0] + a[1] + a[2] + b[1];\n\
               }\n\
               int main(void) { int x[3] = {1, 2, 3}, y[2] = {0, 4};\n\
                   return sum(x, y) == 10 ? 0 : 1; }\n";
    assert_eq!(compile_and_run("static_param_paren", src, &[]), 0);
}

/// C17 6.9.2p2: a file-scope array declared without an extent is judged at
/// the end of the unit. Completed by a later declaration it is that size,
/// silently; still incomplete, it has one element and gcc's warning, once.
/// The completed array's storage was the incomplete declaration's eight
/// bytes, so `a[2]` overlapped the next object.
#[test]
fn tentative_array_completed_later_in_the_unit() {
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
}

/// A tag declaration with a specifier that applies to nothing (C17 6.7p2):
/// gcc's warning naming the useless specifier, its pedwarn for a reference
/// that redeclares nothing, and its error for a specifier it refuses. The
/// tag is still declared, and usable after.
#[test]
fn tag_declaration_with_useless_specifier() {
    let src = "static struct S1 { int a; };\n\
               const struct S2 { int a; };\n\
               _Alignas(8) struct S3 { int a; };\n\
               static struct S1;\n\
               int main(void) { struct S1 x = { 1 }; struct S2 y = { 2 }; struct S3 z = { 3 }; return x.a + y.a + z.a == 6 ? 0 : 1; }\n";
    let run = compile_object_run("tag_useless", src, &[]);
    assert!(run.success, "{}", run.stderr);
    for want in [
        ":1:1: warning: useless storage class specifier in empty declaration",
        ":2:1: warning: useless type qualifier in empty declaration",
        ":3:1: warning: useless '_Alignas' in empty declaration",
        ":4:1: warning: empty declaration with storage class specifier does not redeclare tag",
    ] {
        assert!(run.stderr.contains(want), "{want}:\n{}", run.stderr);
    }
    assert_eq!(compile_and_run("tag_useless_run", src, &[]), 0);
    compile_expect_error(
        "tag_inline",
        "inline struct S { int a; };\n",
        "'inline' in empty declaration",
    );
}
