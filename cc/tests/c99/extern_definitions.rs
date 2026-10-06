//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A file-scope `extern` declaration with an initializer is an external
// definition (C17 6.9.2p1; 6.9.2p4's `extern int i3 = 3;`). c17 skipped
// every `extern` declarator, so the object was never defined: an undefined
// reference at link, or -- after a prior `static` -- a variable reading 0.
//

use crate::common::compile_and_run_everywhere;

#[test]
fn c99_file_scope_extern_with_initializer_is_a_definition() {
    compile_and_run_everywhere(
        "extern_definition",
        r#"
/* C17 6.9.2p1: a file-scope declaration with an initializer is an
   external definition, `extern` or not (6.9.2p4 example: extern int i3 = 3). */
extern int y = 5;
int *p = &y;
static int x;
extern int x = 7;          /* internal linkage, from the prior static */
extern const char s[] = "hi";
extern struct { int a, b; } pt = {1, 2};
int main(void)
{
    if (y != 5 || *p != 5) return 1;
    if (x != 7) return 2;
    if (s[1] != 0x69) return 3;
    if (pt.b != 2) return 4;
    return 0;
}
"#,
    );
}

/// The definition is the one object every other file-scope declaration of
/// the name refers to: a tentative definition or a plain `extern` after it,
/// a tentative definition before it, and a thread-local one.
#[test]
fn c99_extern_definition_is_the_one_object() {
    compile_and_run_everywhere(
        "extern_definition_one_object",
        r#"
extern int y = 5;
int y;                     /* tentative: refers to the definition */
extern int y;
int u;
extern int u = 3;          /* the definition the tentative one awaited */
extern _Thread_local int t = 1;
int *py = &y, *pu = &u;
int main(void)
{
    extern int y;
    if (y != 5 || *py != 5) return 1;
    y = 9;
    if (*py != 9) return 2;
    if (u != 3 || *pu != 3) return 3;
    if (t != 1) return 4;
    t = 2;
    if (t != 2) return 5;
    return 0;
}
"#,
    );
}

/// The definition after a declaration that spelled another storage class is
/// still one object of one type: an array, a pointer, a qualified object,
/// and an array or pointee the definition completes.
#[test]
fn c99_extern_definition_after_another_storage_class() {
    compile_and_run_everywhere(
        "extern_definition_storage_class",
        r#"
static const int c;
extern const int c = 4;
static int a[3];
extern int a[3] = {1, 2, 3};
static int e[];
extern int e[3] = {4, 5, 6};
int b[3];
int (*q)[];
int (*q)[3] = &b;
static int *ptr;
extern int *ptr = &a[1];
int main(void)
{
    static int n;
    if (c != 4) return 1;
    if (a[2] != 3 || sizeof a != 3 * sizeof(int)) return 2;
    if (e[2] != 6 || sizeof e != 3 * sizeof(int)) return 3;
    if (*q != b) return 4;
    if (*ptr != 2) return 5;
    return n;
}
"#,
    );
}

/// A later declaration gives the object the composite type (C17 6.2.7p3-4),
/// wherever in the type the incomplete array it completes sits: `int (*q)[];
/// int (*q)[3];` makes `sizeof *q` 12. c17 merged only a top-level array,
/// so `sizeof *q` was 0.
#[test]
fn c99_redeclaration_completes_a_nested_array() {
    compile_and_run_everywhere(
        "composite_nested_array",
        r#"
/* C17 6.2.7p3-4: a later declaration of the same object gives it the
   composite type, so a nested incomplete array completed by the second
   declaration is complete afterwards, wherever it sits in the type. */
int b[3];
int (*q)[];
int (*q)[3] = &b;
extern int m[][4];
int m[2][4];
struct s { int (*p)[]; };
int f(int (*a)[]);
int f(int (*a)[5]) { return sizeof *a; }
extern int (*fp(void))[];
int (*fp(void))[6] { return 0; }
int main(void)
{
    if (sizeof *q != 3 * sizeof(int)) return 1;
    if (sizeof m != 8 * sizeof(int)) return 2;
    if (f(0) != 5 * sizeof(int)) return 3;
    if (sizeof *fp() != 6 * sizeof(int)) return 4;
    return 0;
}
"#,
    );
}
