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
        if let Some(t) = target {
            args.extend(["--target", t]);
        }
        let asm = asm_for_at("stdio_fold", src, &args);
        check(&called(&asm), target.unwrap_or("host"));
    }
}

/// Each rewrite, as the call it leaves.
#[test]
fn builtins_stdio_fold_makes_the_calls() {
    let cases: &[(&str, &[&str])] = &[
        // An empty write orients the stream (C17 7.21.2p4), so the call
        // stays; see `Print::text`.
        ("void f(void) { printf(\"\"); }", &["printf"]),
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
        // Writes nothing, but still orients the stream (C17 7.21.2p4), so
        // the call stays.
        ("void f(FILE *fp) { fputs(\"\", fp); }", &["fputs"]),
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

/// `fputs("", fp)` writes nothing, but it still orients the stream.
///
/// C17 7.21.2p4: a stream has no orientation until an input or output
/// function is applied to it, and the first one sets it -- whether or not it
/// transfers any bytes. Folding the call away took the orientation with it,
/// so `fwide(fp, 0)` answered 0 at -O2 and a byte orientation at -O0:
///
/// ```text
///     fputs("", fp);  fwide(fp, 0)   ->   -1 at -O0,  0 at -O2
/// ```
///
/// gcc and clang keep this call for the same reason. They drop
/// `fprintf(fp, "")` and `fprintf(fp, "%s", "")` and lose the orientation
/// with them; every empty write is treated alike here, so those are
/// asserted too.
#[test]
fn stdio_fold_empty_fputs_still_orients_the_stream() {
    let src = r#"
#include <stdio.h>
#include <wchar.h>
int main(void) {
    FILE *f = fopen("/dev/null", "w");
    if (!f) return 1;
    /* Nothing is written, but the stream becomes byte-oriented. The result
       is deliberately unused: that is the shape the fold applies to. */
    fputs("", f);
    if (fwide(f, 0) >= 0) return 3;
    fclose(f);

    /* The same through a variable the optimizer can see is empty. */
    FILE *g = fopen("/dev/null", "w");
    if (!g) return 4;
    const char *empty = "";
    fputs(empty, g);
    if (fwide(g, 0) >= 0) return 6;
    fclose(g);

    /* Every other empty write orients the stream too. */
    FILE *h = fopen("/dev/null", "w");
    if (!h) return 7;
    fprintf(h, "");
    if (fwide(h, 0) >= 0) return 8;
    fclose(h);

    FILE *i = fopen("/dev/null", "w");
    if (!i) return 9;
    fprintf(i, "%s", "");
    if (fwide(i, 0) >= 0) return 10;
    fclose(i);
    return 0;
}
"#;
    for opt in ["-O0", "-O1", "-O2"] {
        let dir = plib::tmp::Builder::new()
            .prefix("c17_fputs_orient_")
            .tempdir()
            .expect("tempdir");
        let c = dir.path().join("t.c");
        let exe = dir.path().join("t");
        std::fs::write(&c, src).expect("write source");
        let built = run_c17(&[opt, "-o", exe.to_str().unwrap(), c.to_str().unwrap()]);
        assert!(built.success, "{opt}: {}{}", built.stdout, built.stderr);
        let code = Command::new(&exe)
            .status()
            .expect("run")
            .code()
            .unwrap_or(-1);
        assert_eq!(code, 0, "{opt}");
    }
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
