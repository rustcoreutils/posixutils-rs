//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The <stdio.h> output calls the optimizer rewrites
//
// `printf("hi\n")` whose result is unused becomes `puts("hi")`, `fputs` of
// a known string becomes `fputc` or `fwrite`, and a call that prints nothing
// goes. What matters is that the program writes the same bytes: each run
// here checks its whole output, which gcc gives byte for byte.
//

use crate::common::{asm_for_at, compile_and_capture_aarch64, run_c17};
use std::process::{Command, Output};

/// A program making every rewrite, and some that must not be made, whose
/// output is [`EXPECTED`].
const PROGRAM: &str = r#"
#include <stdarg.h>
#include <stdio.h>

static int effects;
static FILE *out(void) { effects++; return stdout; }

static void v(const char *unused, ...) {
    va_list ap;
    va_start(ap, unused);
    vprintf("v line\n", ap);
    vfprintf(stdout, "vf\n", ap);
    vprintf("", ap);
    va_end(ap);
}

int main(void) {
    const char *const hello = "hello";
    const char *const s2[] = { hello, 0 };
    const char *const *s3 = s2;
    volatile int one = 1;
    int i = 0;

    printf("");
    printf("A");
    printf("line\n");
    printf("no newline;");
    printf("%s\n", *s3++);
    printf("%c", 'B');
    printf("%s", "C\n");
    printf("%s", "");
    printf("%d\n", 42);
    printf("%%\n");
    printf("%s", "100%\n");
    printf("\xe9\n");
    fprintf(stdout, "");
    fprintf(out(), "fp text\n");
    fprintf(out(), "%s", "fp s\n");
    fprintf(out(), "%c", 'D');
    fprintf(out(), "%s", "");
    fputs("", out());
    fputs("E", out());
    fputs("fputs line\n", out());
    fputs(one ? "F" : "G", out());
    fputs(i++ ? "x" : "y", stdout);
    fputs(--i ? "\n" : "\n", stdout);
    v("");
    if (printf("used\n") != 5) return 1;
    if (fputs("", stdout) < 0) return 2;
    if (effects != 8) return 3;
    if (i != 0 || s3 != s2 + 1) return 4;
    return 0;
}
"#;

/// What [`PROGRAM`] writes, as gcc builds it at every level.
const EXPECTED: &[u8] = b"Aline\nno newline;hello\nBC\n42\n%\n100%\n\xe9\n\
    fp text\nfp s\nDEfputs line\nFy\nv line\nvf\nused\n";

/// Build `src` for the host with c17 at `opt`, and run it.
fn host_run(name: &str, src: &str, opt: &str) -> Output {
    let dir = plib::tmp::Builder::new()
        .prefix(&format!("c17_{name}_"))
        .tempdir()
        .expect("tempdir");
    let c = dir.path().join("t.c");
    let exe = dir.path().join("t");
    std::fs::write(&c, src).expect("write source");
    let built = run_c17(&[opt, "-o", exe.to_str().unwrap(), c.to_str().unwrap()]);
    assert!(built.success, "{name} {opt}: {}", built.stderr);
    Command::new(&exe).output().expect("run the program")
}

fn check(what: &str, run: &Output) {
    assert_eq!(run.status.code(), Some(0), "{what}: exit status");
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(EXPECTED),
        "{what}: output"
    );
    assert_eq!(run.stdout, EXPECTED, "{what}: output bytes");
}

#[test]
fn builtins_stdio_fold_output() {
    for opt in ["-O0", "-O1", "-O2"] {
        check(
            &format!("host {opt}"),
            &host_run("stdio_fold", PROGRAM, opt),
        );
    }
}

#[test]
fn builtins_stdio_fold_output_aarch64() {
    // The target's own <stdio.h>, where the cross toolchain puts it.
    let headers = ["-isystem", "/usr/aarch64-linux-gnu/include"];
    for opt in ["-O0", "-O2"] {
        let opts = [&[opt][..], &headers].concat();
        if let Some(run) = compile_and_capture_aarch64("stdio_fold", PROGRAM, &opts, &[]) {
            check(&format!("aarch64 {opt}"), &run);
        }
    }
}

/// What `<stdio.h>` declares, spelled out, so that the assembly tests can
/// name either target without its headers.
const PROTOTYPES: &str = "typedef struct F FILE;\n\
    typedef unsigned long size_t;\n\
    int printf(const char *, ...);\n\
    int fprintf(FILE *, const char *, ...);\n\
    int fputs(const char *, FILE *);\n\
    int puts(const char *);\n";

/// The functions the assembly calls or jumps to, by name, less any `_`
/// prefix.
fn called(asm: &str) -> Vec<String> {
    let mut names: Vec<String> = asm
        .lines()
        .filter_map(|l| {
            let mut words = l.split_whitespace();
            let op = words.next()?;
            if !["call", "jmp", "bl", "b"].contains(&op) {
                return None;
            }
            let name = words.next()?.split('@').next()?;
            (!name.starts_with('.')).then(|| name.trim_start_matches('_').to_string())
        })
        .collect();
    names.sort();
    names
}

/// Compile `body` at `opts` for the host and for aarch64, and hand what
/// it calls to `check`.
fn for_each_target(body: &str, opts: &[&str], check: impl Fn(&[String], &str)) {
    let src = format!("{PROTOTYPES}{body}\n");
    for target in [None, Some("aarch64-unknown-linux-gnu")] {
        let mut args = opts.to_vec();
        if let Some(t) = target {
            args.extend(["--target", t]);
        }
        let asm = asm_for_at("stdio_fold", &src, &args);
        check(&called(&asm), target.unwrap_or("host"));
    }
}

/// Each rewrite, as the call it leaves.
#[test]
fn builtins_stdio_fold_makes_the_calls() {
    let cases: &[(&str, &[&str])] = &[
        ("void f(void) { printf(\"\"); }", &[]),
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
