//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Plain `char` signedness, run and linked against the platform compiler.
//
// C17 6.2.5p15 leaves it to the implementation; the platform ABI decides it
// per architecture *and* OS. x86-64 is signed everywhere, AAPCS64 (Linux,
// FreeBSD) is unsigned, and Apple arm64 overrides AAPCS64 to signed. The
// assembly shapes are pinned in `cross_abi.rs`; these tests run the code.
//

#[cfg(target_os = "linux")]
use crate::common::{aarch64_cross_available, interop_aarch64};
use crate::common::{compile_and_run, compile_and_run_aarch64, interop_host};

/// The platform rule for the host this test binary runs on, stated here
/// independently of c17 so a test cannot pass by c17 agreeing with itself.
const HOST_CHAR_SIGNED: bool = cfg!(target_arch = "x86_64") || cfg!(target_os = "macos");

/// A program that exits 0 exactly when plain `char` has the signedness
/// `signed` says, as a value, as a character constant, and as `<limits.h>`
/// describes it.
fn signedness_probe(signed: bool) -> String {
    format!(
        r#"
#include <limits.h>
#define WANT_SIGNED {}

int widen(char c) {{ return c; }}

int main(void) {{
    volatile char c = (char)0x80;
    if ((c < 0) != WANT_SIGNED) return 1;
    if (('\x80' < 0) != WANT_SIGNED) return 2;
    if (widen((char)0xC3) != (WANT_SIGNED ? -61 : 195)) return 3;
    if (CHAR_MIN != (WANT_SIGNED ? SCHAR_MIN : 0)) return 4;
    if (CHAR_MAX != (WANT_SIGNED ? SCHAR_MAX : UCHAR_MAX)) return 5;
#ifdef __CHAR_UNSIGNED__
    if (WANT_SIGNED) return 6;
#else
    if (!WANT_SIGNED) return 6;
#endif
    /* Plain `char` converts like the type it behaves as. */
    if ((int)c != (WANT_SIGNED ? (int)(signed char)0x80 : (int)(unsigned char)0x80)) return 7;
    return 0;
}}
"#,
        i32::from(signed)
    )
}

/// On the host, plain `char` has the host platform's signedness -- signed on
/// x86-64 and on Apple arm64, unsigned on aarch64 Linux.
#[test]
fn plain_char_signedness_matches_the_host_platform() {
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(
                "plain_char_host",
                &signedness_probe(HOST_CHAR_SIGNED),
                &[opt.to_string()]
            ),
            0,
            "{opt}"
        );
    }
}

/// On aarch64 Linux under qemu, plain `char` is unsigned (AAPCS64), whatever
/// the host is.
#[test]
fn plain_char_is_unsigned_on_aarch64_linux() {
    for opt in ["-O0", "-O2"] {
        if let Some(rc) = compile_and_run_aarch64("plain_char_a64", &signedness_probe(false), opt) {
            assert_eq!(rc, 0, "{opt}");
        }
    }
}

/// Every declaration the interop units share. Each side answers for its own
/// compiler, and the caller checks that the two agree, so the pair fails the
/// moment c17 and the platform compiler disagree about plain `char`.
const INTEROP_DECLS: &str = r#"
#include <limits.h>
#include <stdarg.h>
struct pair { char a; char b; };
int callee_is_signed(void);
int callee_char_min(void);
int callee_widen(char c);
char callee_make(void);
struct pair callee_pair(void);
int callee_widen_pair(struct pair p);
int callee_vararg(int n, ...);
int callee_widen_sc(signed char c);
int callee_widen_sh(short s);
"#;

const INTEROP_CALLEE: &str = r#"
int callee_is_signed(void) { volatile char c = (char)0x80; return c < 0; }
int callee_char_min(void) { return CHAR_MIN; }
int callee_widen(char c) { return c; }
char callee_make(void) { return (char)0xC3; }
struct pair callee_pair(void) { struct pair p = { (char)0x80, (char)0xFF }; return p; }
int callee_widen_pair(struct pair p) { return p.a + p.b; }
int callee_widen_sc(signed char c) { return c; }
int callee_widen_sh(short s) { return s; }
int callee_vararg(int n, ...) {
    va_list ap;
    va_start(ap, n);
    int v = va_arg(ap, int);
    va_end(ap);
    return v;
}
"#;

fn interop_caller(signed: bool) -> String {
    format!(
        r#"
#define WANT_SIGNED {}
static int widen(char c) {{ return c; }}
int main(void) {{
    volatile char c80 = (char)0x80, cc3 = (char)0xC3, cff = (char)0xFF;
    if ((c80 < 0) != WANT_SIGNED) return 1;
    if (callee_is_signed() != WANT_SIGNED) return 2;
    if (callee_char_min() != CHAR_MIN) return 3;
    /* A `char` argument, widened by the callee. */
    if (callee_widen(cc3) != widen(cc3)) return 4;
    if (callee_widen(c80) != (WANT_SIGNED ? -128 : 128)) return 5;
    /* A `char` return value, widened by the caller. */
    if (callee_make() != widen(cc3)) return 6;
    if ((int)callee_make() != (WANT_SIGNED ? -61 : 195)) return 7;
    /* `char` members crossing in a struct, both ways. */
    struct pair p = callee_pair();
    if (p.a != widen(c80) || p.b != widen(cff)) return 8;
    if (callee_widen_pair(p) != widen(c80) + widen(cff)) return 9;
    /* The default argument promotion is the caller's. */
    if (callee_vararg(1, cff) != (WANT_SIGNED ? -1 : 255)) return 10;
    /* A narrowing conversion *at the call site*, which is a different path
       from every check above: loading a `char` object already extends it by
       the type's signedness, so those never reach the conversion. Only a
       value narrowed here does, and the callee is entitled to assume the
       caller extended it -- an optimized gcc or clang compiles
       `int f(signed char)` to a bare register move. */
    volatile int wide = 0xC3, wide16 = 0xFFC3;
    if (callee_widen((char)wide) != (WANT_SIGNED ? -61 : 195)) return 11;
    if (callee_widen_sc((signed char)wide) != -61) return 12;
    if (callee_widen_sh((short)wide16) != -61) return 13;
    return 0;
}}
"#,
        i32::from(signed)
    )
}

fn interop_sources(signed: bool) -> (String, String) {
    (
        format!("{INTEROP_DECLS}\n{INTEROP_CALLEE}"),
        format!("{INTEROP_DECLS}\n{}", interop_caller(signed)),
    )
}

/// c17 and Apple clang on either side of calls carrying plain `char` values
/// of 0x80 and above. Apple arm64 makes `char` signed, so a c17 that follows
/// AAPCS64 here zero-extends what clang sign-extends.
#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
#[test]
fn plain_char_interoperates_with_apple_clang() {
    let (callee, caller) = interop_sources(true);
    interop_host("plain_char_apple", &callee, &caller);
}

/// The same calls between c17 and the host compiler everywhere else.
#[cfg(not(all(target_os = "macos", target_arch = "aarch64")))]
#[test]
fn plain_char_interoperates_with_host_cc() {
    let (callee, caller) = interop_sources(HOST_CHAR_SIGNED);
    interop_host("plain_char_host", &callee, &caller);
}

/// And with aarch64 gcc under qemu, where `char` is unsigned.
#[cfg(target_os = "linux")]
#[test]
fn plain_char_interoperates_with_gcc_aarch64() {
    if !aarch64_cross_available() {
        eprintln!("SKIP: no aarch64 cross toolchain");
        return;
    }
    let (callee, caller) = interop_sources(false);
    interop_aarch64("plain_char_a64", &callee, &caller);
}
