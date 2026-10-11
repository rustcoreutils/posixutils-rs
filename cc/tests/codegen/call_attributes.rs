//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Which calls a function declaration's `noreturn`, `returns_twice` and
// `nothrow` reach. gcc keeps them on the declaration: a call naming the
// function sees them, and so -- for `noreturn` alone, which gcc writes into
// the *pointer* type as a `volatile` function -- does a call through the
// function's address. `__typeof__` of the function names its bare type, and
// a typedef of a function type cannot carry them at all, so a function or a
// pointer declared from either is an ordinary one, free to return. And the
// `error` attribute, which refuses only a call that survives optimization.
//

use crate::common::{compile_and_run, compile_and_run_aarch64, compile_and_run_everywhere};

/// Each call below reaches a function that returns. c17 put `noreturn` on
/// the function's type, so every declaration spelled through `__typeof__` or
/// the typedef believed the call never came back and trapped after it.
#[test]
fn noreturn_does_not_travel_through_typeof_or_a_typedef() {
    let src = r#"
extern void exit(int);
void stop(int) __attribute__((noreturn));
void stop(int c) { exit(c); }
typedef void vfn(int) __attribute__((noreturn));

static int hits;
void back(int c) { hits += c; }

__typeof__(stop) *p1 = back;
vfn *p2 = back;
__typeof__(stop) again;
void again(int c) { hits += c; }
vfn too;
void too(int c) { hits += c; }

int main(void)
{
    p1(1);
    p2(2);
    again(4);
    too(8);
    return hits == 15 ? 0 : 1;
}
"#;
    compile_and_run_everywhere("noreturn_typeof_typedef", src);
}

/// The Linux kernel's `BUILD_BUG_ON`: a call to a block-scope `__error__`
/// function in a branch that folds away once `check` is inlined with a
/// constant. Optimized, the program builds and runs on both targets.
#[test]
fn kernel_build_bug_on_builds_when_optimized() {
    let src = r#"
#define __compiletime_assert(condition, msg, prefix, suffix)            \
    do {                                                                \
        extern void prefix ## suffix(void) __attribute__((__error__(msg))); \
        if (!(condition))                                               \
            prefix ## suffix();                                         \
    } while (0)
#define _compiletime_assert(condition, msg, prefix, suffix) \
    __compiletime_assert(condition, msg, prefix, suffix)
#define BUILD_BUG_ON(cond) \
    _compiletime_assert(!(cond), "BUILD_BUG_ON failed: " #cond, __compiletime_assert_, __COUNTER__)
static inline int check(int n) { BUILD_BUG_ON(n > 8); return n * 2; }
int main(void) { BUILD_BUG_ON(sizeof(int) != 4); return check(4) - 8; }
"#;
    for opt in ["-O1", "-O2"] {
        assert_eq!(
            compile_and_run("build_bug_on", src, &[opt.to_string()]),
            0,
            "{opt}"
        );
        if let Some(code) = compile_and_run_aarch64("build_bug_on", src, opt) {
            assert_eq!(code, 0, "aarch64 {opt}");
        }
    }
}
