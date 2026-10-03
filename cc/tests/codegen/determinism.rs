//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Determinism: the same source and options give the same assembly every run.
//
// Each run is a separate c17 process, so each gets its own hash seed: any
// HashMap or HashSet whose iteration order reaches the output -- the order
// literals are added, pseudos numbered, labels made -- shows up as two runs
// that differ.
//

use super::asm_probe::{AARCH64_DARWIN, X86_64_LINUX};
use crate::common::run_c17;

/// How many separate compilations of each program must agree.
const RUNS: usize = 10;

/// `printf` calls with no conversions fold to `puts`, in many blocks of one
/// function, each minting a literal without the newline. The literals were
/// added in the order the folds were applied, block by block, and the blocks
/// were visited in a HashMap's order.
const VARARGS_AND_FOLDED_PRINTF: &str = r#"
#include <stdarg.h>
typedef struct { float a, b; }          F2;
typedef struct { float a, b, c; }       F3;
typedef struct { double a, b; }         D2;
typedef struct { long a, b; }           L2;
typedef struct { char a, b, c; }        C3;

int printf(const char *, ...);
int fflush(void *);

double g_f2(int n, ...) { va_list ap; va_start(ap, n);
    F2 v = va_arg(ap, F2); va_end(ap); return (double)(v.a * 10 + v.b); }
double g_f3(int n, ...) { va_list ap; va_start(ap, n);
    F3 v = va_arg(ap, F3); va_end(ap); return (double)(v.a * 100 + v.b * 10 + v.c); }
double g_d2(int n, ...) { va_list ap; va_start(ap, n);
    D2 v = va_arg(ap, D2); va_end(ap); return v.a * 10 + v.b; }
long   g_l2(int n, ...) { va_list ap; va_start(ap, n);
    L2 v = va_arg(ap, L2); va_end(ap); return v.a * 10 + v.b; }
int    g_c3(int n, ...) { va_list ap; va_start(ap, n);
    C3 v = va_arg(ap, C3); va_end(ap); return v.a * 100 + v.b * 10 + v.c; }

int main(void) {
    F2 f2 = {1, 2};
    F3 f3 = {1, 2, 3};
    D2 d2 = {1, 2};
    L2 l2 = {1, 2};
    C3 c3 = {1, 2, 3};
#define T(n) (printf("try " n "\n"), fflush(0))
#define G(n, g) (printf("got " n " %.1f\n", (double)(g)), fflush(0))
    T("f2"); { double g = g_f2(0, f2); G("f2", g); if (g != 12) return 1; }
    T("f3"); { double g = g_f3(0, f3); G("f3", g); if (g != 123) return 2; }
    T("d2"); { double g = g_d2(0, d2); G("d2", g); if (g != 12) return 3; }
    T("l2"); { long g = g_l2(0, l2); G("l2", g); if (g != 12) return 4; }
    T("c3"); { int g = g_c3(0, c3); G("c3", g); if (g != 123) return 5; }
    T("end");
    return 0;
}
"#;

/// String-library folds that make new calls and new literals, spread over
/// blocks, inside functions the inliner copies into several callers.
const STRING_FOLDS_AND_INLINING: &str = r#"
typedef unsigned long size_t;
int printf(const char *, ...);
int fprintf(void *, const char *, ...);
int puts(const char *);
char *strcpy(char *, const char *);
char *strcat(char *, const char *);
size_t strlen(const char *);
char *strchr(const char *, int);
char *strstr(const char *, const char *);
extern void *stderr;

static inline void say(int k) {
    if (k == 1) printf("one\n");
    else if (k == 2) printf("two\n");
    else if (k == 3) printf("three\n");
    else printf("many\n");
}

static inline int probe(const char *s, int k) {
    if (k & 1) return strstr(s, "x") != 0;
    if (k & 2) return strchr("abcdef", 'd') - "abcdef";
    return (int)strlen("seventeen chars!!");
}

int build(char *buf, int k) {
    strcpy(buf, "alpha");
    if (k > 3) strcat(buf, "-beta");
    if (k > 5) { strcpy(buf, "gamma"); fprintf(stderr, "gamma\n"); }
    if (k > 7) printf("%s\n", "delta");
    return (int)strlen(buf);
}

int main(int argc, char **argv) {
    char buf[64];
    int r = 0;
    for (int i = 0; i < argc + 4; i++) {
        say(i);
        r += probe(argv[0], i);
        if (i == 2) printf("%s", "two again\n");
        if (i == 3) puts("three again");
        r += build(buf, i);
    }
    say(r);
    return r & 1;
}
"#;

/// Compile `src` `RUNS` times for `triple` at `-O2` and require every run to
/// produce the first run's assembly.
///
/// Every run compiles the same file: the assembly names its source, so a
/// fresh temporary path per run would differ for no fault of the compiler.
fn assert_deterministic(name: &str, triple: &str, src: &str) {
    let dir = plib::tmp::Builder::new()
        .prefix(&format!("c17_det_{name}_"))
        .tempdir()
        .expect("failed to create work dir");
    let c = dir.path().join("t.c");
    std::fs::write(&c, src).expect("failed to write source");
    let s = dir.path().join("t.s");
    let (c, s) = (c.to_str().unwrap(), s.to_str().unwrap());
    let compile = || {
        let r = run_c17(&["--target", triple, "-O2", "-S", c, "-o", s]);
        assert!(
            r.success,
            "c17 --target {triple} failed for {name}:\n{}",
            r.stderr
        );
        std::fs::read_to_string(s).expect("no assembly produced")
    };
    let first = compile();
    for run in 2..=RUNS {
        let again = compile();
        if again != first {
            let line = first
                .lines()
                .zip(again.lines())
                .position(|(a, b)| a != b)
                .map_or(0, |i| i + 1);
            panic!("{name} for {triple}: run {run} differs from run 1 at line {line}");
        }
    }
}

#[test]
fn codegen_deterministic_folded_printf_literals() {
    for triple in [X86_64_LINUX, AARCH64_DARWIN] {
        assert_deterministic("det_printf", triple, VARARGS_AND_FOLDED_PRINTF);
    }
}

#[test]
fn codegen_deterministic_string_folds_and_inlining() {
    for triple in [X86_64_LINUX, AARCH64_DARWIN] {
        assert_deterministic("det_strings", triple, STRING_FOLDS_AND_INLINING);
    }
}
