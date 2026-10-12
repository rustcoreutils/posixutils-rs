//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__builtin_dynamic_object_size`: the size of the object a pointer points
// into, computed at run time when it is a run-time value -- `malloc(n)`,
// `calloc(n, m)`, an `alloc_size` function's argument, a VLA, `alloca`,
// `realloc`, `strdup`, a run-time offset into any of them. glibc's
// `_FORTIFY_SOURCE=3` checks every fortified call against it. Every
// expected value was read off gcc 13 at -O2.
//

use crate::common::compile_and_run_everywhere;

/// Each answer gcc 13 gives at -O2, where its object-size pass runs; at -O0
/// gcc answers "unknown" for every run-time size, so there an answer may be
/// either -- but a fixed array at a run-time offset is exact at every level,
/// as in gcc.
#[test]
fn builtins_dynamic_object_size_is_computed_at_run_time() {
    let src = r#"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#define S(p, t) __builtin_dynamic_object_size(p, t)
void *my_alloc(size_t) __attribute__((alloc_size(1), malloc));
void *my_alloc2(size_t, size_t) __attribute__((alloc_size(1, 2), malloc));
__attribute__((noinline)) void *my_alloc(size_t n) { return malloc(n); }
__attribute__((noinline)) void *my_alloc2(size_t n, size_t m) { return calloc(n, m); }
__attribute__((noinline, access(read_only, 1, 2)))
size_t acc(const char *p, size_t n) { (void)n; return S(p, 0); }
struct s { int a; char b[10]; int c; };
volatile size_t vn = 7, vm = 3;
volatile int pick = 1;
static int fails;
/* Optimizing, exactly gcc's answer; at -O0, it or "unknown". */
static void check(int line, int type, size_t got, size_t want) {
    size_t unknown = type < 2 ? SIZE_MAX : 0;
#ifdef __OPTIMIZE__
    int ok = got == want;
#else
    int ok = got == want || got == unknown;
#endif
    if (!ok && !fails) fails = line;
}
#define CHECK(p, t, want) check(__LINE__, t, S(p, t), want)
int main(void) {
    size_t n = vn, m = vm;
    char *p = malloc(n);
    CHECK(p, 0, 7); CHECK(p, 1, 7); CHECK(p, 2, 7); CHECK(p, 3, 7);
    CHECK(p + 3, 0, 4); CHECK(p + 3, 2, 4);
    CHECK(p + m, 0, 4); CHECK(p + m, 2, 4);
    int *q = calloc(n, m);
    CHECK(q, 0, 21);
    CHECK(q + 1, 0, 17);
    char *r = my_alloc(n * 2);
    CHECK(r, 0, 14);
    char *r2 = my_alloc2(n, m);
    CHECK(r2, 0, 21);
    char vla[n];
    CHECK(vla, 0, 7); CHECK(&vla[m], 0, 4);
    int vla2[n][m];
    CHECK(vla2, 0, 84);
    char *c = pick ? p : vla;
    CHECK(c, 0, 7); CHECK(c, 2, 7);
    struct s *ps = malloc(sizeof *ps * n);
    CHECK(ps, 0, 140);
    CHECK(ps->b, 1, 10);
    char *a = __builtin_alloca(n + 1);
    CHECK(a, 0, 8);
    char *dup = strdup("hello");
    CHECK(dup, 0, 6);
#ifdef __OPTIMIZE__
    if (acc(p, n) != 7 && !fails) fails = __LINE__;
#endif
    char *rr = realloc(p, n + 5);
    CHECK(rr, 0, 12);
    /* A fixed array at a run-time offset: exact at every level. */
    char fixed[17];
    if (S(fixed + m, 0) != 14 && !fails) fails = __LINE__;
    if (S(fixed + m, 2) != 14 && !fails) fails = __LINE__;
    /* Never larger than the object, whatever the level. */
    if (S(p + m, 0) != SIZE_MAX && S(p + m, 0) > 4 && !fails) fails = __LINE__;
    free(rr); free(q); free(r); free(r2); free(ps); free(dup);
    return fails;
}
"#;
    compile_and_run_everywhere("dynamic_object_size", src);
}

/// A fortified call checked against the dynamic size, as glibc's
/// `_FORTIFY_SOURCE=3` writes `memcpy` -- `__builtin___memcpy_chk(d, s, n,
/// __builtin_dynamic_object_size(d, 0))` -- catches a copy of 12 bytes into
/// `malloc(7)` and aborts, as with gcc. c17 answered "unknown", and the
/// overflow ran. (glibc grants level 3 itself only to gcc 12 and later.)
#[cfg(target_os = "linux")]
#[test]
fn builtins_dynamic_size_check_catches_a_heap_overflow() {
    let dir = crate::common::work_dir("dynamic_size_check");
    let src = dir.path().join("f.c");
    let exe = dir.path().join("f");
    std::fs::write(
        &src,
        "#include <stdlib.h>\n\
         #include <string.h>\n\
         volatile size_t vn = 7, vm = 12;\n\
         int main(void) {\n\
         \x20   size_t n = vn, m = vm;\n\
         \x20   char *p = malloc(n);\n\
         \x20   char src[32] = \"0123456789abcdef0123456789abcde\";\n\
         \x20   __builtin___memcpy_chk(p, src, m, __builtin_dynamic_object_size(p, 0));\n\
         \x20   return p[0] == '0' ? 0 : 1;\n\
         }\n",
    )
    .unwrap();
    let r = crate::common::run_c17(&["-O2", "-o", exe.to_str().unwrap(), src.to_str().unwrap()]);
    assert!(r.success, "{}", r.stderr);
    let out = std::process::Command::new(&exe).output().unwrap();
    assert!(!out.status.success(), "the overflow ran: {:?}", out.status);
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("buffer overflow detected"),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// glibc's `_FORTIFY_SOURCE=3` checks a fortified call against the dynamic
/// size: a `memcpy` of 12 bytes into `malloc(7)` is caught and the program
/// aborted, as with gcc. glibc grants level 3 only to gcc 12 and later, so
/// under c17's gcc 7.5 claim it fell back to level 2, whose static size of a
/// `malloc` result is unknown, and the overflow ran.
#[cfg(all(target_os = "linux", target_env = "gnu"))]
#[test]
fn builtins_fortify_3_catches_a_heap_overflow() {
    let dir = crate::common::work_dir("fortify3");
    let src = dir.path().join("f.c");
    let exe = dir.path().join("f");
    std::fs::write(
        &src,
        "#include <stdlib.h>\n\
         #include <string.h>\n\
         #if __USE_FORTIFY_LEVEL != 3\n\
         #error \"glibc did not grant level 3\"\n\
         #endif\n\
         volatile size_t vn = 7, vm = 12;\n\
         int main(void) {\n\
         \x20   size_t n = vn, m = vm;\n\
         \x20   char *p = malloc(n);\n\
         \x20   char src[32] = \"0123456789abcdef0123456789abcde\";\n\
         \x20   memcpy(p, src, m);\n\
         \x20   return p[0] == '0' ? 0 : 1;\n\
         }\n",
    )
    .unwrap();
    let r = crate::common::run_c17(&[
        "-O2",
        "-D_FORTIFY_SOURCE=3",
        "-o",
        exe.to_str().unwrap(),
        src.to_str().unwrap(),
    ]);
    assert!(r.success, "{}", r.stderr);
    let out = std::process::Command::new(&exe).output().unwrap();
    assert!(!out.status.success(), "the overflow ran: {:?}", out.status);
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("buffer overflow detected"),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
}
