//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// __builtin_va_arg_pack / __builtin_va_arg_pack_len
//

// The compile-only case is a unit test in cc/test_asm/builtins_va_arg_pack.rs.

use crate::common::{compile_and_run, compile_and_run_optimized};

/// GCC's forwarding builtins: inside an `always_inline` variadic function,
/// `__builtin_va_arg_pack()` stands for the *caller's* variadic arguments and
/// `__builtin_va_arg_pack_len()` for how many there are. Both are resolved
/// when the function is inlined, so neither can be folded before that.
///
/// glibc forwards `sprintf`/`printf` into the `__*_chk` family exactly this
/// way in `bits/stdio2.h`, which is why `__OPTIMIZE__` cannot be predefined
/// without them.
const FORWARDING: &str = r#"
#include <stdarg.h>

static int target(int n, ...) {
    va_list ap;
    va_start(ap, n);
    long t = 0;
    for (int i = 0; i < n; i++) t += va_arg(ap, int);
    va_end(ap);
    return (int)t;
}

__attribute__((always_inline))
static inline int wrap(const char *tag, ...) {
    (void)tag;
    return target(__builtin_va_arg_pack_len(), __builtin_va_arg_pack());
}

int main(void) {
    if (wrap("none") != 0) return 1;
    if (wrap("one", 42) != 42) return 2;
    if (wrap("three", 1, 2, 3) != 6) return 3;
    if (wrap("many", 1, 2, 3, 4, 5, 6, 7, 8) != 36) return 4;
    return 0;
}
"#;

#[test]
fn builtins_va_arg_pack_forwards_the_callers_arguments() {
    assert_eq!(compile_and_run("builtins_va_arg_pack", FORWARDING, &[]), 0);
    assert_eq!(
        compile_and_run("builtins_va_arg_pack_o0", FORWARDING, &["-O0".to_string()]),
        0
    );
}

/// The splice happens in the inliner, and `always_inline` fires at every
/// level, so the result must not depend on `-O`.
#[test]
fn builtins_va_arg_pack_forwards_when_optimized() {
    assert_eq!(
        compile_and_run_optimized("builtins_va_arg_pack_opt", FORWARDING),
        0
    );
}

/// The arguments keep their types across the splice: the ABI classification
/// of each one is carried over from the outer call, since the inliner has no
/// type table to recompute it from.
#[test]
fn builtins_va_arg_pack_keeps_argument_types() {
    let code = r#"
#include <stdarg.h>
#include <string.h>

static int target(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int i = va_arg(ap, int);
    double d = va_arg(ap, double);
    const char *s = va_arg(ap, const char *);
    long long l = va_arg(ap, long long);
    va_end(ap);
    return (i == 7 && d == 2.5 && strcmp(s, "str") == 0 && l == 123456789012345LL)
        ? 0 : 1;
}

__attribute__((always_inline))
static inline int wrap(const char *fmt, ...) {
    return target(fmt, __builtin_va_arg_pack());
}

int main(void) { return wrap("%d", 7, 2.5, "str", 123456789012345LL); }
"#;
    assert_eq!(compile_and_run("builtins_va_arg_pack_types", code, &[]), 0);
}

/// `_len()` counts the caller's variadic arguments, not the callee's
/// parameters, and is a constant expression at the call site.
#[test]
fn builtins_va_arg_pack_len_counts_the_call_site() {
    let code = r#"
__attribute__((always_inline))
static inline int count(const char *tag, ...) {
    (void)tag;
    return __builtin_va_arg_pack_len();
}

int main(void) {
    if (count("a") != 0) return 1;
    if (count("a", 1) != 1) return 2;
    if (count("a", 1, 2, 3, 4, 5) != 5) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_va_arg_pack_len", code, &[]), 0);
    assert_eq!(
        compile_and_run("builtins_va_arg_pack_len_o0", code, &["-O0".to_string()]),
        0
    );
}

/// At `-O0` nothing removes a static forwarder once every call is spliced,
/// and an extern `gnu_inline` one -- glibc's `open` in `bits/fcntl2.h` -- is
/// never removed at any level. Either way its body is still in the module
/// with its `__builtin_va_arg_pack_len()` unresolved, which is right: it has
/// no out-of-line form and is never emitted. Lowering used to reject it as a
/// placeholder reaching codegen and stop the compiler.
#[test]
fn builtins_va_arg_pack_unemitted_forwarder_is_not_codegen() {
    let forwarder = |linkage: &str| {
        format!(
            r#"
{linkage} __attribute__((always_inline)) int count(const char *tag, ...) {{
    (void)tag;
    return __builtin_va_arg_pack_len();
}}

int main(void) {{
    if (count("a") != 0) return 1;
    if (count("a", 1, 2, 3, 4, 5) != 5) return 2;
    return 0;
}}
"#
        )
    };
    for (tag, linkage) in [
        ("static", "static inline"),
        ("gnu_inline", "extern inline __attribute__((gnu_inline))"),
    ] {
        let code = forwarder(linkage);
        for opt in ["-O0", "-O2"] {
            let name = format!("va_pack_unemitted_{tag}{opt}");
            assert_eq!(
                compile_and_run(&name, &code, &[opt.to_string()]),
                0,
                "{tag} at {opt}"
            );
        }
    }
}

/// A forwarder that also uses `alloca` now inlines like any other.
///
/// `alloca` used to disqualify a callee before `always_inline` was even
/// consulted, which for a `__builtin_va_arg_pack` forwarder meant the body was
/// suppressed and nothing was left to call. gcc compiles this.
#[test]
fn builtins_va_arg_pack_forwarder_may_use_alloca() {
    let code = r#"
#include <stdio.h>
extern int snprintf(char *, unsigned long, const char *, ...);

__attribute__((always_inline)) static inline int wrap(const char *fmt, ...) {
    char *buf = __builtin_alloca(64);
    int n = snprintf(buf, 64, fmt, __builtin_va_arg_pack());
    return n + (buf[0] == '4' ? 100 : 0);
}

int main(void) { return wrap("%d-%d", 42, 7) == 104 ? 0 : 1; }
"#;
    assert_eq!(compile_and_run("va_pack_alloca", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("va_pack_alloca_opt", code), 0);
}

/// A forwarder whose body calls the library through a second declaration
/// with the same assembler name: glibc's `open` in `bits/fcntl2.h` calls
/// `__open_alias`, and `error` in `bits/error.h` calls `__error_alias`, each
/// `__REDIRECT`ed to the very symbol the wrapper itself is labelled with.
/// The alias is a different function -- the library's -- so the call is not
/// recursion, and gcc inlines the wrapper.
#[test]
fn builtins_va_arg_pack_forwarder_calls_its_own_assembler_name() {
    let code = r#"
#define STR2(x) #x
#define STR(x) STR2(x)
#define ASMNAME(cname) __asm__(STR(__USER_LABEL_PREFIX__) cname)
typedef __SIZE_TYPE__ size_t;

extern int fmt(char *, size_t, const char *, ...) ASMNAME("snprintf");
extern int fmt_alias(char *, size_t, const char *, ...) ASMNAME("snprintf");

extern __inline __attribute__((__always_inline__, __gnu_inline__, __artificial__)) int
fmt(char *d, size_t n, const char *f, ...)
{
    if (__builtin_va_arg_pack_len() > 2)
        return -1;
    return fmt_alias(d, n, f, __builtin_va_arg_pack());
}

int main(void)
{
    char b[32];
    if (fmt(b, sizeof b, "%d-%s", 42, "x") != 4)
        return 1;
    if (b[0] != '4' || b[3] != 'x')
        return 2;
    if (fmt(b, sizeof b, "%d%d%d", 1, 2, 3) != -1)
        return 3;
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run("va_pack_own_asm_name", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// The forwarding wrapper and the program around it, with `body` -- straight
/// line code of whatever size a test needs -- in `caller`, which also calls
/// the wrapper once. `seen` adds up the first argument each forwarded call
/// delivers.
fn forwarding_program(caller: &str) -> String {
    format!(
        r#"
#include <stdarg.h>
static int seen;
static int target(const char *f, ...) {{
    va_list ap;
    va_start(ap, f);
    seen += va_arg(ap, int);
    va_end(ap);
    return 0;
}}

__attribute__((always_inline)) static inline int wrap(const char *f, ...) {{
    return target(f, __builtin_va_arg_pack());
}}

volatile int vals[8] = {{1, 2, 3, 4, 5, 6, 7, 8}};
{caller}
"#
    )
}

/// `count` statements of straight-line code, each a few instructions.
fn straight_line(count: usize) -> String {
    (0..count)
        .map(|i| format!("    acc += vals[{}] * {i};\n", i % 8))
        .collect()
}

/// A recursive caller of any size still inlines a forwarder, which has no
/// out-of-line form to call instead: c17's stack-depth cap on inlining into a
/// recursive function does not apply. jansson's `do_dump` calls `snprintf`,
/// isl's `print_help` calls `printf`, and gcc inlines both.
#[test]
fn builtins_va_arg_pack_forwarder_in_large_recursive_caller() {
    let code = forwarding_program(&format!(
        r#"
static long walk(int n) {{
    long acc = 0;
{}
    wrap("%d", n);
    if (n > 0)
        acc += walk(n - 1);
    return acc;
}}

int main(void) {{
    walk(3);
    return seen == 6 ? 0 : 1;
}}
"#,
        straight_line(64)
    ));
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run("va_pack_recursive_caller", &code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// A caller past c17's own size cap on inlining -- bzip2's `sendMTFValues`,
/// mpfr's test `main`s -- still inlines a forwarder. gcc has no such cap.
#[test]
fn builtins_va_arg_pack_forwarder_in_huge_caller() {
    let code = forwarding_program(&format!(
        r#"
int main(void) {{
    long acc = 0;
{}
    wrap("%d", 5);
    return seen == 5 && acc != 0 ? 0 : 1;
}}
"#,
        straight_line(1600)
    ));
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run("va_pack_huge_caller", &code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}
