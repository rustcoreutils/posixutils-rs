//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Calls of functions that return twice: the library's `setjmp` family,
// gcc's list of such names, and anything declared
// `__attribute__((returns_twice))`. Each is a call of the function the
// program names, and a value live across it is kept out of every register.
//

use super::asm_probe::{
    asm_for, asm_for_with, assert_body_contains, body_of, AARCH64_DARWIN, AARCH64_LINUX,
    X86_64_LINUX,
};

/// The functions `func` calls directly, as the source spells them: the
/// `call`/`bl` operand without its PLT suffix or Mach-O underscore.
fn callees(asm: &str, func: &str, triple: &str) -> Vec<String> {
    body_of(asm, func)
        .lines()
        .filter_map(|l| {
            let l = l.trim();
            l.strip_prefix("call ").or_else(|| l.strip_prefix("bl "))
        })
        .map(|t| {
            let t = t.trim_end_matches("@PLT");
            let t = if triple == AARCH64_DARWIN {
                t.strip_prefix('_').unwrap_or(t)
            } else {
                t
            };
            t.to_string()
        })
        .collect()
}

/// gcc calls exactly what the source names: glibc's `setjmp` macro expands
/// to `_setjmp`, which saves no signal mask, while the function `setjmp`
/// saves it -- and on Darwin it is the other way round. c17 lowered
/// `_setjmp` to a call of `setjmp` and `_longjmp` to one of `longjmp`.
#[test]
fn returns_twice_calls_the_function_it_names() {
    let src = r#"
typedef long jmp_buf[64];
extern int setjmp(jmp_buf);
extern int _setjmp(jmp_buf);
extern int __sigsetjmp(jmp_buf, int);
extern int sigsetjmp(jmp_buf, int);
extern void longjmp(jmp_buf, int) __attribute__((noreturn));
extern void _longjmp(jmp_buf, int) __attribute__((noreturn));
extern void siglongjmp(jmp_buf, int) __attribute__((noreturn));
jmp_buf jb;
int s0(void) { return setjmp(jb); }
int s1(void) { return _setjmp(jb); }
int s2(void) { return __sigsetjmp(jb, 1); }
int s3(void) { return sigsetjmp(jb, 0); }
void l0(void) { longjmp(jb, 1); }
void l1(void) { _longjmp(jb, 1); }
void l2(void) { siglongjmp(jb, 1); }
"#;
    let expect = [
        ("s0", "setjmp"),
        ("s1", "_setjmp"),
        ("s2", "__sigsetjmp"),
        ("s3", "sigsetjmp"),
        ("l0", "longjmp"),
        ("l1", "_longjmp"),
        ("l2", "siglongjmp"),
    ];
    for triple in [X86_64_LINUX, AARCH64_LINUX, AARCH64_DARWIN] {
        for opt in ["-O0", "-O2"] {
            let asm = asm_for_with("rt_names", triple, src, &[opt]);
            for (func, callee) in expect {
                assert_eq!(
                    callees(&asm, func, triple),
                    [callee],
                    "{triple} {opt} {func}:\n{asm}"
                );
            }
        }
    }
}

const ATTR: &str = r#"
extern int rt(void *) __attribute__((returns_twice));
extern void g(long);
void *buf[3];
long f(long a, long b) {
    long p = a * 7, q = b * 13;
    if (rt(buf)) return p + q;
    g(p);
    return 0;
}
int (*fp)(void *) = rt;
int through(void) { if (fp(buf)) return 1; g(0); return 0; }
"#;

/// A function declared `returns_twice` comes back by an indirect jump, so
/// under branch protection the call is followed by `endbr64`, as gcc's list
/// of names is. A call through a pointer is not: gcc ignores the attribute
/// on a pointer, and the pointer's type does not carry it.
#[test]
fn returns_twice_attribute_lands_with_endbr64() {
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with(
            "rt_endbr",
            X86_64_LINUX,
            ATTR,
            &[opt, "-fcf-protection=branch"],
        );
        assert_body_contains(&asm, "f", "call rt@PLT\n    endbr64\n", opt);
        let body = body_of(&asm, "through");
        assert_eq!(
            body.matches("endbr64").count(),
            1,
            "{opt}: entry only\n{body}"
        );
    }
}

/// A value read after a `returns_twice` call lives in memory across it, as
/// gcc keeps it: the second return restores no register the program can
/// count on but the frame and stack pointers.
#[test]
fn returns_twice_attribute_keeps_no_value_in_a_register_across_it() {
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for("rt_spill", triple, ATTR);
        let body = body_of(&asm, "f");
        let call = body
            .find("call rt@PLT")
            .or_else(|| body.find("bl rt"))
            .unwrap_or_else(|| panic!("{triple}: no call of rt:\n{body}"));
        let frame_reads = body[call..]
            .lines()
            .filter(|l| l.contains("(%rbp)") || l.contains("[x29"))
            .count();
        assert!(
            frame_reads >= 2,
            "{triple}: p and q must be reloaded from the frame after the call:\n{body}"
        );
    }
}

/// gcc never inlines a function that calls one returning twice ("can never
/// be inlined because it uses setjmp"): the second return would land in the
/// caller's frame, which the callee's values were never laid out for.
#[test]
fn returns_twice_caller_is_never_inlined() {
    let src = r#"
extern int rt(void *) __attribute__((returns_twice));
extern int _setjmp(long *);
long jb[64];
static int a(void) { return rt(jb); }
static int b(void) { return _setjmp(jb); }
int f(void) { return a() + b(); }
"#;
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("rt_inline", triple, src, &["-O2"]);
        assert_eq!(callees(&asm, "f", triple), ["a", "b"], "{triple}:\n{asm}");
    }
}
