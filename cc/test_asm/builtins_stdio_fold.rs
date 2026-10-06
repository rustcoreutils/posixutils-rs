//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly and compile-only cases of tests/builtins/stdio_fold.rs, in process.
//

use crate::test_compile::asm_for;

/// What `<stdio.h>` declares, spelled out, so that the assembly tests can
/// name either target without its headers.
const PROTOTYPES: &str = "typedef struct F FILE;\n\
    typedef unsigned long size_t;\n\
    int printf(const char *, ...);\n\
    int fprintf(FILE *, const char *, ...);\n\
    int fputs(const char *, FILE *);\n\
    int puts(const char *);\n\
    typedef __builtin_va_list va_list;\n\
    int vprintf(const char *, va_list);\n\
    int vfprintf(FILE *, const char *, va_list);\n";

/// The functions the assembly calls or jumps to, by name, less any `_`
/// prefix.
///
/// A branch to one of the function's own blocks is not a call. Its label is
/// spelled `.L…` on ELF; on Mach-O it is `L…` and a C name is the one with
/// the `_`.
fn called(asm: &str) -> Vec<String> {
    let macho = asm.contains("__TEXT,");
    let mut names: Vec<String> = asm
        .lines()
        .filter_map(|l| {
            let mut words = l.split_whitespace();
            let op = words.next()?;
            if !["call", "jmp", "bl", "b"].contains(&op) {
                return None;
            }
            let name = words.next()?.split('@').next()?;
            // A compiler-made label is private: `L...` on Mach-O, `.L...` on
            // ELF. Anything else is a callee, including an `asm` label,
            // which Mach-O spells verbatim, without the `_`.
            let is_c_name = if macho {
                !name.trim_start_matches('"').starts_with('L')
            } else {
                !name.starts_with('.')
            };
            is_c_name.then(|| name.trim_start_matches('_').to_string())
        })
        .collect();
    names.sort();
    names
}

/// Compile `body` at `opts` for the host and for aarch64, and hand what
/// it calls to `check`.
fn for_each_target(body: &str, opts: &[&str], check: impl Fn(&[String], &str)) {
    for_each_target_src(&format!("{PROTOTYPES}{body}\n"), opts, check);
}

/// The same, for a test that must spell its own prototypes -- one that binds
/// a name [`PROTOTYPES`] declares as a function to something else.
fn for_each_target_src(src: &str, opts: &[&str], check: impl Fn(&[String], &str)) {
    for target in [None, Some("aarch64-unknown-linux-gnu")] {
        let mut args = opts.to_vec();
        if target.is_some() {
            args.push("--target=aarch64-unknown-linux-gnu");
        }
        let asm = asm_for("stdio_fold", src, &args);
        check(&called(&asm), target.unwrap_or("host"));
    }
}

/// Each rewrite, as the call it leaves.
#[test]
fn builtins_stdio_fold_makes_the_calls() {
    let cases: &[(&str, &[&str])] = &[
        // An empty write whose result is unused is deleted, as gcc deletes
        // it, although C17 7.21.2p4 has it orient the stream.
        ("void f(void) { printf(\"\"); }", &[]),
        ("void f(FILE *fp) { fprintf(fp, \"\"); }", &[]),
        ("void f(void) { printf(\"%s\", \"\"); }", &[]),
        ("void f(FILE *fp) { fprintf(fp, \"%s\", \"\"); }", &[]),
        ("void f(va_list ap) { vprintf(\"\", ap); }", &[]),
        (
            "void f(FILE *fp, va_list ap) { vfprintf(fp, \"\", ap); }",
            &[],
        ),
        ("void f(void) { printf(\"a\"); }", &["putchar"]),
        ("void f(void) { printf(\"hi\\n\"); }", &["puts"]),
        ("void f(const char *s) { printf(\"%s\\n\", s); }", &["puts"]),
        ("void f(int c) { printf(\"%c\", c); }", &["putchar"]),
        ("void f(FILE *fp) { fprintf(fp, \"hello\"); }", &["fwrite"]),
        (
            "void f(FILE *fp, const char *s) { fprintf(fp, \"%s\", s); }",
            &["fputs"],
        ),
        (
            "void f(FILE *fp, int c) { fprintf(fp, \"%c\", c); }",
            &["fputc"],
        ),
        ("void f(FILE *fp) { fputs(\"\", fp); }", &[]),
        ("void f(FILE *fp) { fputs(\"x\", fp); }", &["fputc"]),
        (
            "void f(FILE *fp, int i) { fputs(i ? \"ab\" : \"cd\", fp); }",
            &["fwrite"],
        ),
    ];
    for (body, want) in cases {
        for_each_target(body, &["-O2"], |calls, what| {
            assert_eq!(calls, *want, "{what}: {body}");
        });
    }
}

/// A used result, a directive other than `%s` and `%c`, text that is
/// neither empty, one character nor a line, an unknown string, `-O0` and
/// `-fno-builtin-printf` all keep the call.
#[test]
fn builtins_stdio_fold_keeps_the_call_otherwise() {
    let cases: &[(&str, &str)] = &[
        ("int f(void) { return printf(\"hi\\n\"); }", "printf"),
        ("void f(int x) { printf(\"%d\\n\", x); }", "printf"),
        ("void f(void) { printf(\"hi\"); }", "printf"),
        ("void f(const char *s) { printf(\"%s\", s); }", "printf"),
        ("void f(long c) { printf(\"%c\", c); }", "printf"),
        ("int f(FILE *fp) { return fputs(\"hi\", fp); }", "fputs"),
        // An empty write whose result is used stays, as in gcc.
        ("int f(void) { return printf(\"\"); }", "printf"),
        ("int f(FILE *fp) { return fprintf(fp, \"\"); }", "fprintf"),
        ("int f(FILE *fp) { return fputs(\"\", fp); }", "fputs"),
        // puts("") writes a newline.
        ("void f(void) { puts(\"\"); }", "puts"),
        ("void f(FILE *fp, const char *s) { fputs(s, fp); }", "fputs"),
    ];
    for (body, name) in cases {
        for_each_target(body, &["-O2"], |calls, what| {
            assert_eq!(calls, [*name], "{what}: {body}");
        });
    }
    let body = "void f(void) { printf(\"hi\\n\"); }";
    for opts in [&["-O0"][..], &["-O2", "-fno-builtin-printf"]] {
        for_each_target(body, opts, |calls, what| {
            assert_eq!(calls, ["printf"], "{what} {opts:?}");
        });
    }
}

/// The call a rewrite makes goes to the program's own name for the
/// function.
#[test]
fn builtins_stdio_fold_follows_asm_labels() {
    let body = "int puts(const char *) __asm__(\"my_puts\");\n\
                void f(void) { printf(\"hi\\n\"); }";
    for_each_target(body, &["-O2"], |calls, what| {
        assert_eq!(calls, ["my_puts"], "{what}");
    });
}

/// A library function a fold would call has to *be* that function.
///
/// `int puts;` binds a name C17 7.1.3 reserves to an object, so the program
/// is undefined -- but gcc and clang degrade gracefully and keep the call,
/// where c17 folded `printf("hi\n")` to `puts` and emitted `call puts`
/// against the four-byte `.bss` object it had just defined. That runs:
///
/// ```text
///     int puts;  int main(void) { printf("ok\n"); }   ->   bus error
/// ```
#[test]
fn builtins_stdio_fold_declines_a_callee_that_is_not_a_function() {
    // Each case binds the name a fold would reach for to an object, and
    // names the call that has to survive instead.
    // (the shadowed name, the object binding it, the body, the calls left)
    let cases: &[(&str, &str, &str, &[&str])] = &[
        (
            "puts",
            "int puts;",
            "void f(void) { printf(\"hi\\n\"); }",
            &["printf"],
        ),
        (
            "putchar",
            "int putchar;",
            "void f(void) { printf(\"a\"); }",
            &["printf"],
        ),
        (
            "fputc",
            "int fputc;",
            "void f(FILE *fp, int c) { fprintf(fp, \"%c\", c); }",
            &["fprintf"],
        ),
        // This one rewrites in two steps -- `fprintf` to `fputs`, then
        // `fputs` of a known length to `fwrite`. Only the second step is
        // barred, so it stops at `fputs`, which writes the same bytes and
        // is a real function. Degrading to the next legitimate call is the
        // point; what must not happen is a call to the object.
        (
            "fwrite",
            "int fwrite;",
            "void f(FILE *fp) { fprintf(fp, \"hello\"); }",
            &["fputs"],
        ),
    ];
    for (shadowed, object, body, want) in cases {
        let src = format!(
            "typedef struct F FILE;\n\
             typedef unsigned long size_t;\n\
             int printf(const char *, ...);\n\
             int fprintf(FILE *, const char *, ...);\n\
             {object}\n{body}\n"
        );
        for_each_target_src(&src, &["-O2"], |calls, what| {
            // The invariant: nothing may call the object.
            assert!(
                !calls.iter().any(|c| c == shadowed),
                "{what}: with `{object}` in scope, `{body}` compiled to \
                 {calls:?} -- `{shadowed}` is that object's own storage, so \
                 calling it jumps into .bss"
            );
            assert_eq!(
                calls, *want,
                "{what}: with `{object}` in scope, `{body}` should leave \
                 {want:?}, not {calls:?}"
            );
        });
    }
}
