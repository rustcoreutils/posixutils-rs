//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's `note:` lines: what a diagnostic points back at -- the declaration a
// redeclaration conflicts with, the parameter an argument does not suit, the
// first of two case labels -- each right after its diagnostic, and only when
// that diagnostic was shown. Checked against gcc 13.
//

use crate::test_compile::compile;

/// Every line of `src` compiled as `<name>.c` with `flags`, the driver's
/// closing failure line left out.
fn lines(name: &str, src: &str, flags: &[&str]) -> Vec<String> {
    compile(name, src, flags)
        .stderr
        .lines()
        .filter(|l| !l.ends_with("compilation failed"))
        .map(str::to_string)
        .collect()
}

#[track_caller]
fn expect(name: &str, src: &str, flags: &[&str], want: &[&str]) {
    let want: Vec<String> = want.iter().map(|l| format!("{name}.c:{l}")).collect();
    assert_eq!(lines(name, src, flags), want);
}

/// A conflicting redeclaration names the new type, and its note the
/// declaration before it with that one's type -- the latest declaration,
/// or the definition once there is one.
#[test]
fn conflicting_types_note_the_previous_declaration() {
    expect(
        "note_conflict",
        "int x;\nextern int x;\nlong x;\n",
        &[],
        &[
            "3:6: error: conflicting types for 'x'; have 'long'",
            "2:12: note: previous declaration of 'x' with type 'int'",
        ],
    );
    expect(
        "note_conflict_def",
        "int f(int);\nint f(int a) { return a; }\nint f(int);\nlong f(int);\n",
        &[],
        &[
            "4:6: error: conflicting types for 'f'; have 'long(int)'",
            "2:5: note: previous definition of 'f' with type 'int(int)'",
        ],
    );
    // Across scopes: a block-scope `extern` against the file's.
    expect(
        "note_conflict_block",
        "int z;\nvoid g(void) { extern long z; }\n",
        &[],
        &[
            "2:28: error: conflicting types for 'z'; have 'long'",
            "1:5: note: previous declaration of 'z' with type 'int'",
        ],
    );
}

/// A second definition points at the first; a linkage conflict at the
/// declaration that gave the other linkage.
#[test]
fn redefinition_and_linkage_conflicts_note_the_first() {
    expect(
        "note_redef",
        "int f(void) { return 0; }\nint f(void) { return 1; }\nint x = 1;\nint x = 2;\n",
        &[],
        &[
            "2:5: error: redefinition of 'f'",
            "1:5: note: previous definition of 'f' with type 'int(void)'",
            "4:5: error: redefinition of 'x'",
            "3:5: note: previous definition of 'x' with type 'int'",
        ],
    );
    expect(
        "note_linkage",
        "int x;\nstatic int x;\nstatic int y;\nint y;\n",
        &[],
        &[
            "2:12: error: static declaration of 'x' follows non-static declaration",
            "1:5: note: previous declaration of 'x' with type 'int'",
            "4:5: error: non-static declaration of 'y' follows static declaration",
            "3:12: note: previous declaration of 'y' with type 'int'",
        ],
    );
}

/// In a block, gcc calls a repeat a redefinition only when both have an
/// initializer, and says which of them came first.
#[test]
fn no_linkage_repeats_in_gcc_words() {
    expect(
        "note_no_linkage",
        "void a(void) { int x = 1; int x = 2; }\n\
         void b(void) { int x = 1; int x; }\n\
         void c(void) { int x; int x = 1; }\n\
         void d(void) { int x; extern int x; }\n",
        &[],
        &[
            "1:31: error: redefinition of 'x'",
            "1:20: note: previous definition of 'x' with type 'int'",
            "2:31: error: redeclaration of 'x' with no linkage",
            "2:20: note: previous definition of 'x' with type 'int'",
            "3:27: error: redeclaration of 'x' with no linkage",
            "3:20: note: previous declaration of 'x' with type 'int'",
            "4:34: error: extern declaration of 'x' follows declaration with no linkage",
            "4:20: note: previous declaration of 'x' with type 'int'",
        ],
    );
}

/// A name that changes kind, an enumerator declared twice, a parameter
/// named twice and a typedef that changes type: each points back, a
/// typedef's type spelled with what it stands for.
#[test]
fn other_redeclarations_note_the_first() {
    expect(
        "note_kind",
        "int x;\nvoid x(void);\nenum E { A };\nenum F { A };\nint A;\n",
        &[],
        &[
            "2:6: error: 'x' redeclared as different kind of symbol",
            "1:5: note: previous declaration of 'x' with type 'int'",
            "4:10: error: redeclaration of enumerator 'A'",
            "3:10: note: previous definition of 'A' with type 'int'",
            "5:5: error: 'A' redeclared as different kind of symbol",
            "3:10: note: previous definition of 'A' with type 'int'",
        ],
    );
    expect(
        "note_typedef",
        "typedef int T;\ntypedef long T;\n",
        &[],
        &[
            "2:14: error: conflicting types for 'T'; have 'long'",
            "1:13: note: previous declaration of 'T' with type 'T' {aka 'int'}",
        ],
    );
    expect(
        "note_param",
        "void g(int a, int a) {}\n",
        &[],
        &[
            "1:19: error: redefinition of parameter 'a'",
            "1:12: note: previous definition of 'a' with type 'int'",
        ],
    );
}

/// A second definition of a tag points at the first; gcc calls a second
/// enumerator list a redeclaration.
#[test]
fn tag_redefinition_notes_the_original() {
    expect(
        "note_tag",
        "struct S { int a; };\nstruct S { int b; };\nenum E { A };\nenum E { B };\n",
        &[],
        &[
            "2:8: error: redefinition of 'struct S'",
            "1:8: note: originally defined here",
            "4:6: error: redeclaration of 'enum E'",
            "3:6: note: originally defined here",
        ],
    );
}

/// An argument that does not suit its parameter is reported in gcc's
/// words, and the note gives both types at the parameter -- its name, or
/// an unnamed one's specifiers -- in the callee's first declaration.
#[test]
fn argument_notes_point_at_the_parameter() {
    expect(
        "note_arg",
        "void h(long *);\nvoid h(long *q);\nvoid k(int a, long *q);\n\
         void f(int *p) { h(p); k(0, p); }\n",
        &[],
        &[
            "4:20: warning: passing argument 1 of 'h' from incompatible pointer type \
             [-Wincompatible-pointer-types]",
            "1:8: note: expected 'long *' but argument is of type 'int *'",
            "4:29: warning: passing argument 2 of 'k' from incompatible pointer type \
             [-Wincompatible-pointer-types]",
            "3:21: note: expected 'long *' but argument is of type 'int *'",
        ],
    );
    expect(
        "note_arg_kinds",
        "void h(int *);\nvoid g(int);\nvoid u(unsigned *);\nstruct S { int a; };\n\
         void f(const int *p, int *q, struct S s) { h(p); h(5); g(q); u(q); g(s); }\n",
        &["-Wall"],
        &[
            "5:46: warning: passing argument 1 of 'h' discards 'const' qualifier from pointer \
             target type [-Wdiscarded-qualifiers]",
            "1:8: note: expected 'int *' but argument is of type 'const int *'",
            "5:52: warning: passing argument 1 of 'h' makes pointer from integer without a cast \
             [-Wint-conversion]",
            "1:8: note: expected 'int *' but argument is of type 'int'",
            "5:58: warning: passing argument 1 of 'g' makes integer from pointer without a cast \
             [-Wint-conversion]",
            "2:8: note: expected 'int' but argument is of type 'int *'",
            "5:64: warning: pointer targets in passing argument 1 of 'u' differ in signedness \
             [-Wpointer-sign]",
            "3:8: note: expected 'unsigned int *' but argument is of type 'int *'",
            "5:70: error: incompatible type for argument 1 of 'g'",
            "2:8: note: expected 'int' but argument is of type 'struct S'",
        ],
    );
    // Through a pointer, the note is at the argument.
    expect(
        "note_arg_indirect",
        "void (*fp)(long *);\nvoid f(int *p) { fp(p); }\n",
        &[],
        &[
            "2:21: warning: passing argument 1 of 'fp' from incompatible pointer type \
             [-Wincompatible-pointer-types]",
            "2:21: note: expected 'long *' but argument is of type 'int *'",
        ],
    );
}

/// Assignment, initialization and `return` name the types as gcc does,
/// and a discarded qualifier by name.
#[test]
fn conversion_faults_in_gcc_words() {
    expect(
        "conv_words",
        "void z(const int *p) { long *q; q = p; int *r = p; (void)q; (void)r; }\n\
         int *w(volatile const int *p) { return p; }\n\
         long *v(int *p) { return p; }\n",
        &[],
        &[
            "1:35: warning: assignment to 'long *' from incompatible pointer type 'const int *' \
             [-Wincompatible-pointer-types]",
            "1:49: warning: initialization discards 'const' qualifier from pointer target type \
             [-Wdiscarded-qualifiers]",
            "2:40: warning: return discards 'const volatile' qualifier from pointer target type \
             [-Wdiscarded-qualifiers]",
            "3:26: warning: returning 'int *' from a function with incompatible return type \
             'long *' [-Wincompatible-pointer-types]",
        ],
    );
}

/// A call with the wrong number of arguments points at the callee.
#[test]
fn argument_count_notes_the_declaration() {
    expect(
        "note_count",
        "int g(int, int);\nint f(void) { return g(1) + g(1, 2, 3); }\n",
        &[],
        &[
            "2:23: error: too few arguments to function 'g'",
            "1:5: note: declared here",
            "2:30: error: too many arguments to function 'g'",
            "1:5: note: declared here",
        ],
    );
}

/// A repeated case value or `default` points at the first; gcc's words for
/// a range differ.
#[test]
fn switch_label_notes() {
    expect(
        "note_case",
        "int f(int x) {\n  switch (x) {\n  case 1: case 2: return 0;\n  case 1: return 1;\n  \
         case 0 ... 2: return 2;\n  default: default: return 3;\n  }\n}\n",
        &[],
        &[
            "4:8: error: duplicate case value",
            "3:8: note: previously used here",
            "5:8: error: duplicate (or overlapping) case value",
            "3:8: note: this is the first entry overlapping that value",
            "6:12: error: multiple default labels in one switch",
            "6:3: note: this is the first default label",
        ],
    );
}

/// A duplicate label, and a jump into the scope of a variably modified
/// object, point at what they collide with.
#[test]
fn label_and_jump_notes() {
    expect(
        "note_label",
        "void f(void) { l: ; l: ; }\nvoid g(int n) { goto m; { int a[n]; m: (void)a; } }\n",
        &[],
        &[
            "1:21: error: duplicate label 'l'",
            "1:16: note: previous definition of 'l' with type 'void'",
            "2:17: error: jump into scope of identifier with variably modified type",
            "2:37: note: label 'm' defined here",
            "2:31: note: 'a' declared here",
        ],
    );
}

/// An undeclared identifier is reported once per function, with gcc's note
/// once per unit; outside functions once too, with no note.
#[test]
fn undeclared_identifier_in_gcc_words() {
    expect(
        "note_undeclared",
        "int f(void) { return y + y + z; }\nint g(void) { return y; }\n\
         int a[sizeof q];\nint r = q;\n",
        &[],
        &[
            "1:22: error: 'y' undeclared (first use in this function)",
            "1:22: note: each undeclared identifier is reported only once for each function \
             it appears in",
            "1:30: error: 'z' undeclared (first use in this function)",
            "2:22: error: 'y' undeclared (first use in this function)",
            "3:14: error: 'q' undeclared here (not in a function)",
        ],
    );
}

/// A macro redefined differently points at the definition it replaces.
#[test]
fn macro_redefinition_notes_the_previous_definition() {
    expect(
        "note_macro",
        "#define A 1\n#define A 2\n",
        &[],
        &[
            "2:9: warning: 'A' redefined: the replacement lists differ",
            "1:9: note: this is the location of the previous definition",
        ],
    );
}

/// `va_arg` of a type that is promoted says which type to ask for.
#[test]
fn va_arg_promotion_note() {
    let all = lines(
        "note_va_arg",
        "void f(int a, ...) { __builtin_va_list ap; __builtin_va_start(ap, a); \
         char c = __builtin_va_arg(ap, char); (void)c; __builtin_va_end(ap); }\n",
        &[],
    );
    assert_eq!(all.len(), 2, "{all:?}");
    assert!(
        all[0].ends_with("warning: 'char' is promoted to 'int' when passed through '...'"),
        "{all:?}"
    );
    assert!(
        all[1].ends_with("note: (so you should pass 'int' not 'char' to 'va_arg')"),
        "{all:?}"
    );
}

/// A `return` that does not suit the function points at it.
#[test]
fn return_mismatch_notes_the_function() {
    expect(
        "note_return",
        "int f(void) { return; }\nvoid g(void) { return 1; }\n",
        &["-fpermissive"],
        &[
            "1:15: warning: 'return' with no value, in function returning non-void",
            "1:5: note: declared here",
            "2:23: warning: 'return' with a value, in function returning void",
            "2:6: note: declared here",
        ],
    );
}

/// A section asked to hold incompatible definitions points at the other.
#[cfg(target_os = "linux")]
#[test]
fn section_conflict_notes_the_other_definition() {
    expect(
        "note_section",
        "const int a __attribute__((section(\"s1\"))) = 1;\n\
         int b __attribute__((section(\"s1\"))) = 2;\n",
        &[],
        &[
            "1:11: error: 'a' causes a section type conflict with 'b'",
            "2:5: note: 'b' was declared here",
        ],
    );
}

/// A note follows its diagnostic: none when the warning is not shown, by
/// `-w`, by `-Wno-<name>` or in a system header; and an error's note under
/// `-w` is still given.
#[test]
fn notes_follow_their_diagnostic() {
    let src = "void h(long *);\nvoid f(int *p) { h(p); }\n";
    for flags in [&["-w"][..], &["-Wno-incompatible-pointer-types"]] {
        let c = compile("note_hidden", src, flags);
        assert!(c.stderr.is_empty(), "{flags:?}:\n{}", c.stderr);
    }
    let system = "# 1 \"sys.h\" 1 3\nvoid h(long *);\nvoid f(int *p) { h(p); }\n";
    let c = compile("note_system", system, &[]);
    assert!(c.stderr.is_empty(), "{}", c.stderr);

    let promoted = lines("note_werror", src, &["-Werror"]);
    assert_eq!(
        promoted[1],
        "note_werror.c:1:8: note: expected 'long *' but argument is of type 'int *'"
    );

    let error = lines("note_error_w", "int x;\nlong x;\n", &["-w"]);
    assert_eq!(
        error,
        [
            "note_error_w.c:2:6: error: conflicting types for 'x'; have 'long'",
            "note_error_w.c:1:5: note: previous declaration of 'x' with type 'int'",
        ]
    );
}
