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

// The assembly cases are unit tests in cc/test_asm/builtins_stdio_fold.rs.

use crate::common::{compile_and_capture_aarch64, run_c17};
use std::process::{Command, Output};

/// A program making every rewrite, and some that must not be made, whose
/// output is [`EXPECTED`].
const PROGRAM: &str = r#"
#include <stdarg.h>
#include <stdio.h>

static int effects;
static FILE *out(void) { effects++; return stdout; }
static const char *nothing(void) { effects++; return ""; }

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
    printf("%s", nothing());
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
    if (effects != 9) return 3;
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

/// A write of nothing whose result is unused is deleted from -O1 up, as gcc
/// deletes it, and the stream is left unoriented.
///
/// C17 7.21.2p4: a stream has no orientation until an input or output
/// function is applied to it, and the first one sets it -- whether or not it
/// transfers any bytes. gcc deletes `printf("")`, `fprintf(fp, "")`,
/// `fprintf(fp, "%s", "")` and `fputs("", fp)` all the same, and c17 does
/// as gcc does:
///
/// ```text
///     fputs("", fp);  fwide(fp, 0)   ->   -1 at -O0,  0 at -O1 and -O2
/// ```
///
/// gcc deletes them at -O0 too; c17 folds no stdio call at -O0.
#[test]
fn stdio_fold_empty_write_is_deleted_as_in_gcc() {
    let src = r#"
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
int main(int argc, char **argv) {
    /* -1 when the call stayed and oriented the stream, 0 when it went. */
    int want = atoi(argv[1]);
    FILE *f = fopen("/dev/null", "w");
    if (!f) return 1;
    /* The result is deliberately unused: that is the shape the fold
       applies to. */
    fputs("", f);
    if (fwide(f, 0) != want) return 3;
    fclose(f);

    /* The same through a variable the optimizer can see is empty. */
    FILE *g = fopen("/dev/null", "w");
    if (!g) return 4;
    const char *empty = "";
    fputs(empty, g);
    if (fwide(g, 0) != want) return 6;
    fclose(g);

    FILE *h = fopen("/dev/null", "w");
    if (!h) return 7;
    fprintf(h, "");
    if (fwide(h, 0) != want) return 8;
    fclose(h);

    FILE *i = fopen("/dev/null", "w");
    if (!i) return 9;
    fprintf(i, "%s", "");
    if (fwide(i, 0) != want) return 10;
    fclose(i);

    /* A used result keeps the call, which orients the stream. */
    FILE *j = fopen("/dev/null", "w");
    if (!j) return 11;
    if (fputs("", j) < 0) return 12;
    if (fwide(j, 0) >= 0) return 13;
    fclose(j);
    return 0;
}
"#;
    for (opt, want) in [("-O0", "-1"), ("-O1", "0"), ("-O2", "0")] {
        let dir = plib::tmp::Builder::new()
            .prefix("c17_empty_write_")
            .tempdir()
            .expect("tempdir");
        let c = dir.path().join("t.c");
        let exe = dir.path().join("t");
        std::fs::write(&c, src).expect("write source");
        let built = run_c17(&[opt, "-o", exe.to_str().unwrap(), c.to_str().unwrap()]);
        assert!(built.success, "{opt}: {}{}", built.stdout, built.stderr);
        let code = Command::new(&exe)
            .arg(want)
            .status()
            .expect("run")
            .code()
            .unwrap_or(-1);
        assert_eq!(code, 0, "{opt}");
    }
}
