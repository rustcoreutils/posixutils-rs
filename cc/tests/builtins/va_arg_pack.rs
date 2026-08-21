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

use crate::common::{compile_and_run, compile_and_run_optimized, compile_expect_error};

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
}

/// Both builtins are meaningless outside an `always_inline` variadic
/// function -- there is no caller whose arguments they could name. GCC
/// rejects the program; so must c17, rather than silently producing nothing.
#[test]
fn builtins_va_arg_pack_outside_a_forwarding_function_is_rejected() {
    compile_expect_error(
        "va_arg_pack_not_variadic",
        "__attribute__((always_inline)) static inline int f(int x) {\n\
         return x + __builtin_va_arg_pack_len(); }\n\
         int main(void) { return f(1); }\n",
        "va_arg_pack",
    );
    compile_expect_error(
        "va_arg_pack_not_always_inline",
        "static int f(const char *t, ...) {\n\
         (void)t; return __builtin_va_arg_pack_len(); }\n\
         int main(void) { return f(\"x\", 1); }\n",
        "va_arg_pack",
    );
    compile_expect_error(
        "va_arg_pack_at_file_scope",
        "int x = __builtin_va_arg_pack_len();\nint main(void) { return x; }\n",
        "va_arg_pack",
    );
}
