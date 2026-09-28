//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Named arguments that overflow their registers, on both aarch64 platforms.
//
// AAPCS64 (§6.4.2 C.12-C.16) gives every stacked argument whole eight-byte
// granules. Apple's arm64 convention ("Writing ARM64 code for Apple
// platforms") packs them instead: a scalar at its natural size and
// alignment, so a `char` takes one byte and a following `short` starts two
// bytes in; an HFA at its element's alignment; a composite that is not an
// HFA still in granules. c17 stacked Apple arguments by the AAPCS64 rule on
// both sides, so it agreed with itself and with no Apple-compiled code.
//
// The assembly tests run everywhere; the interop tests pair c17 with the
// platform's own compiler where one can run the result.
//

use super::asm_probe::{asm_for_with, body_of, AARCH64_DARWIN, AARCH64_LINUX};
#[cfg(any(
    all(target_os = "macos", target_arch = "aarch64"),
    all(target_os = "linux", target_arch = "x86_64")
))]
use crate::common::interop_host;
#[cfg(target_os = "linux")]
use crate::common::{aarch64_cross_available, interop_aarch64};

/// Eight ints and eight doubles use up x0-x7 and v0-v7, so the six
/// arguments after them are stacked.
const MIXED: &str = r#"
int take(int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7,
         double d0, double d1, double d2, double d3,
         double d4, double d5, double d6, double d7,
         char c, short s, char c2, int i, double d, char c3)
{ return c + s + c2 + i + (int)d + c3; }

/* Declared only, so the optimizer cannot fold the call away. */
int sink(int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7,
         double d0, double d1, double d2, double d3,
         double d4, double d5, double d6, double d7,
         char c, short s, char c2, int i, double d, char c3);

int give(void)
{ return sink(0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7,
              'a', 300, 'b', 70000, 2.5, 'c'); }
"#;

/// The SP offset of every store in `body` that writes to the outgoing
/// argument area, with its mnemonic, in program order.
fn sp_stores(body: &str) -> Vec<(String, i32)> {
    body.lines()
        .filter_map(|line| {
            let line = line.trim();
            let (op, rest) = line.split_once(' ')?;
            if !op.starts_with("str") || op.starts_with("stp") {
                return None;
            }
            let addr = rest.split_once("[sp")?.1;
            let off = match addr.strip_prefix(", #") {
                Some(tail) => tail.trim_end_matches(']').parse().ok()?,
                None if addr.starts_with(']') => 0,
                None => return None,
            };
            Some((op.to_string(), off))
        })
        .collect()
}

/// The caller writes each stacked argument at the platform's offset, and no
/// wider than its slot: on Apple the next argument begins at the very next
/// byte, so a `char` written as a doubleword would be overwritten -- or,
/// last in the area, would overwrite the caller's own frame.
#[test]
fn codegen_aarch64_stacked_named_arguments_caller() {
    for opt in ["-O0", "-O2"] {
        let darwin = asm_for_with("stacked_mixed_darwin", AARCH64_DARWIN, MIXED, &[opt]);
        assert_eq!(
            sp_stores(body_of(&darwin, "give")),
            [
                ("strb".to_string(), 0),
                ("strh".to_string(), 2),
                ("strb".to_string(), 4),
                ("str".to_string(), 8),
                ("str".to_string(), 16),
                ("strb".to_string(), 24),
            ],
            "Apple packs stacked arguments at their natural size and alignment ({opt}):\n{darwin}"
        );

        let linux = asm_for_with("stacked_mixed_linux", AARCH64_LINUX, MIXED, &[opt]);
        let offsets: Vec<i32> = sp_stores(body_of(&linux, "give"))
            .into_iter()
            .map(|(_, off)| off)
            .collect();
        assert_eq!(
            offsets,
            [0, 8, 16, 24, 32, 40],
            "AAPCS64 gives each stacked argument an eight-byte granule ({opt}):\n{linux}"
        );
    }
}

/// The callee reads each stacked parameter back where its caller put it:
/// the incoming area starts at x29+16, past the saved frame record.
#[test]
fn codegen_aarch64_stacked_named_arguments_callee() {
    for (triple, offsets) in [
        (AARCH64_DARWIN, [16, 18, 20, 24, 32, 40]),
        (AARCH64_LINUX, [16, 24, 32, 40, 48, 56]),
    ] {
        let asm = asm_for_with("stacked_mixed_callee", triple, MIXED, &["-O0"]);
        let body = body_of(&asm, "take");
        for off in offsets {
            assert!(
                body.contains(&format!("[x29, #{off}]")),
                "{triple}: a stacked parameter is read at x29+{off}:\n{body}"
            );
        }
    }
    // And nothing between Apple's packed slots: +17 would be the char read
    // as if the short that follows it began there.
    let asm = asm_for_with("stacked_mixed_callee", AARCH64_DARWIN, MIXED, &["-O0"]);
    assert!(!body_of(&asm, "take").contains("[x29, #17]"));
}

/// A Darwin variadic call stacks its named overflow packed and the variadic
/// arguments after it, each on an eight-byte granule; the callee's
/// `va_start` has to point past the named ones. It pointed at the start of
/// the area, so the first `va_arg` read the named `char`.
#[test]
fn codegen_darwin_variadics_follow_packed_named_arguments() {
    let src = r#"
int vf(int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7, char c, ...);
int call(void) { return vf(0, 1, 2, 3, 4, 5, 6, 7, 'c', 5, 6L); }
"#;
    let asm = asm_for_with("darwin_va_after_named", AARCH64_DARWIN, src, &["-O0"]);
    assert_eq!(
        sp_stores(body_of(&asm, "call")),
        [
            ("strb".to_string(), 0),
            ("str".to_string(), 8),
            ("str".to_string(), 16)
        ],
        "one packed byte, then each variadic argument on a granule:\n{asm}"
    );
}

/// A Darwin variadic call that also returns a struct in memory keeps its
/// named arguments in registers.
///
/// `variadic_arg_start` counts the call's *own* arguments, but `insn.src`
/// carries the hidden sret pointer ahead of them, so the two are one apart
/// for a call that returns in memory. `setup_darwin_variadic_args` compared
/// them directly -- `.max(args_start)` raises the index but never shifts it
/// -- so the named range came out empty and every argument, `n` included,
/// was stacked as though it were variadic:
///
/// ```text
///     str x9, [sp]        ; 7, the named argument, belongs in w0
///     str x9, [sp, #8]    ; 11
///     str x9, [sp, #16]   ; 22
/// ```
///
/// Apple clang emits `mov w0, #7` and stacks only the two variadic ones, so
/// the callee read garbage for `n`. The non-sret call beside it is the
/// control: it was always right, and `args_start` is zero there, so the fix
/// must not move it.
#[test]
fn codegen_darwin_sret_variadic_keeps_its_named_argument_in_a_register() {
    let src = r#"
struct Big { long a, b, c, d; };
struct Big f(int n, ...);
struct Big sret_call(void) { return f(7, 11, 22); }

int g(int n, ...);
int plain_call(void) { return g(7, 11, 22); }
"#;
    let asm = asm_for_with("darwin_sret_va", AARCH64_DARWIN, src, &["-O0"]);

    // The named argument is in w0, and only the two variadic ones are stacked.
    let body = body_of(&asm, "sret_call");
    assert!(
        body.contains("movz w0, #7") || body.contains("mov w0, #7"),
        "the named argument of an sret variadic call goes in w0:\n{body}"
    );
    assert_eq!(
        sp_stores(body).len(),
        2,
        "only the two variadic arguments are stacked:\n{body}"
    );

    // The control: without sret the same call was already correct.
    let body = body_of(&asm, "plain_call");
    assert!(
        body.contains("movz w0, #7") || body.contains("mov w0, #7"),
        "a non-sret variadic call still passes its named argument in w0:\n{body}"
    );
    assert_eq!(
        sp_stores(body).len(),
        2,
        "and still stacks only the variadic ones:\n{body}"
    );
}

/// The interop sources below are built only where a second compiler can run
/// the result: Apple clang on an arm64 Mac, or gcc on Linux. An x86-64 Mac
/// has neither, so they are not compiled there.
#[cfg(any(all(target_os = "macos", target_arch = "aarch64"), target_os = "linux"))]
const INTEROP_DECLS: &str = r#"
#include <stdarg.h>
typedef struct { char a, b, c; } S3;
typedef struct { char a, b, c, d, e; } S5;
typedef struct { short a, b, c; } S6;
typedef struct { int a, b, c; } S12;
typedef struct { float a, b, c; } F3;
typedef struct { double a, b; } D2;
typedef struct { _Float16 a, b, c; } H3;
typedef struct { float a; } F1;
typedef struct { long a, b, c; } Big;

int many(int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7,
         double d0, double d1, double d2, double d3,
         double d4, double d5, double d6, double d7,
         char c1, short s1, char c2, int n1, double x1, char c3,
         _Bool b1, unsigned char uc, float f1, unsigned short us,
         _Float16 h1, long l1, signed char sc,
         S3 s3, char c4, S5 s5, short s2, S6 s6, F3 f3, char c5, D2 dd,
         H3 h3, char c6, F1 f1s, char c7, Big big, char c8, void *p,
         S12 s12, long double ld, float _Complex fc, char c9,
         double _Complex dc, char c10);
int named_then_va(int i0, int i1, int i2, int i3, int i4, int i5, int i6,
                  int i7, char c, ...);
int packed_then_va(int i0, int i1, int i2, int i3, int i4, int i5, int i6,
                   int i7, char c, short s, char c2, ...);
"#;

#[cfg(any(all(target_os = "macos", target_arch = "aarch64"), target_os = "linux"))]
const INTEROP_CALLEE: &str = r#"
#define CK(n, cond) do { if (!(cond)) return n; } while (0)
int many(int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7,
         double d0, double d1, double d2, double d3,
         double d4, double d5, double d6, double d7,
         char c1, short s1, char c2, int n1, double x1, char c3,
         _Bool b1, unsigned char uc, float f1, unsigned short us,
         _Float16 h1, long l1, signed char sc,
         S3 s3, char c4, S5 s5, short s2, S6 s6, F3 f3, char c5, D2 dd,
         H3 h3, char c6, F1 f1s, char c7, Big big, char c8, void *p,
         S12 s12, long double ld, float _Complex fc, char c9,
         double _Complex dc, char c10)
{
    CK(1, i0 == 10 && i7 == 17 && d0 == 0.5 && d7 == 7.5);
    CK(2, c1 == 'a' && s1 == -300 && c2 == 'b' && n1 == 70000);
    CK(3, x1 == 2.25 && c3 == 'c');
    CK(4, b1 == 1 && uc == 200 && f1 == 1.5f && us == 60000);
    CK(5, h1 == (_Float16)0.75f && l1 == -1234567890123L && sc == -5);
    CK(6, s3.a == 1 && s3.b == 2 && s3.c == 3 && c4 == 'd');
    CK(7, s5.a == 4 && s5.e == 8 && s2 == 1234);
    CK(8, s6.a == -1 && s6.b == -2 && s6.c == -3);
    CK(9, f3.a == 1.25f && f3.b == 2.25f && f3.c == 3.25f && c5 == 'e');
    CK(10, dd.a == 4.5 && dd.b == 5.5);
    CK(11, h3.a == (_Float16)1.5f && h3.b == (_Float16)2.5f && h3.c == (_Float16)3.5f);
    CK(12, c6 == 'f' && f1s.a == 6.25f && c7 == 'g');
    CK(13, big.a == 100 && big.b == 200 && big.c == 300 && c8 == 'h');
    CK(14, p == (void *)&many);
    CK(15, s12.a == 7 && s12.b == 8 && s12.c == 9);
    CK(16, ld == 9.75L);
    CK(17, __real__ fc == 1.5f && __imag__ fc == -2.5f && c9 == 'i');
    CK(18, __real__ dc == 3.5 && __imag__ dc == -4.5 && c10 == 'j');
    return 0;
}

int named_then_va(int i0, int i1, int i2, int i3, int i4, int i5, int i6,
                  int i7, char c, ...)
{
    va_list ap;
    va_start(ap, c);
    int a = va_arg(ap, int);
    double b = va_arg(ap, double);
    long l = va_arg(ap, long);
    va_end(ap);
    CK(31, c == 'x');
    CK(32, a == 41 && b == 42.5 && l == 43);
    return 0;
}

int packed_then_va(int i0, int i1, int i2, int i3, int i4, int i5, int i6,
                   int i7, char c, short s, char c2, ...)
{
    va_list ap;
    va_start(ap, c2);
    int a = va_arg(ap, int);
    char *str = va_arg(ap, char *);
    va_end(ap);
    CK(41, c == 'y' && s == -7 && c2 == 'z');
    CK(42, a == 51 && str[0] == 'o' && str[1] == 'k');
    return 0;
}
"#;

#[cfg(any(all(target_os = "macos", target_arch = "aarch64"), target_os = "linux"))]
const INTEROP_CALLER: &str = r#"
int main(void)
{
    S3 s3 = {1, 2, 3};
    S5 s5 = {4, 5, 6, 7, 8};
    S6 s6 = {-1, -2, -3};
    S12 s12 = {7, 8, 9};
    F3 f3 = {1.25f, 2.25f, 3.25f};
    D2 dd = {4.5, 5.5};
    H3 h3 = {1.5f, 2.5f, 3.5f};
    F1 f1s = {6.25f};
    Big big = {100, 200, 300};
    float _Complex fc = __builtin_complex(1.5f, -2.5f);
    double _Complex dc = __builtin_complex(3.5, -4.5);
    int r = many(10, 11, 12, 13, 14, 15, 16, 17,
                 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5,
                 'a', -300, 'b', 70000, 2.25, 'c',
                 1, 200, 1.5f, 60000, (_Float16)0.75f, -1234567890123L, -5,
                 s3, 'd', s5, 1234, s6, f3, 'e', dd,
                 h3, 'f', f1s, 'g', big, 'h', (void *)&many,
                 s12, 9.75L, fc, 'i', dc, 'j');
    if (r)
        return r;
    r = named_then_va(0, 1, 2, 3, 4, 5, 6, 7, 'x', 41, 42.5, 43L);
    if (r)
        return r;
    return packed_then_va(0, 1, 2, 3, 4, 5, 6, 7, 'y', -7, 'z', 51, "ok");
}
"#;

#[cfg(any(all(target_os = "macos", target_arch = "aarch64"), target_os = "linux"))]
fn interop_sources() -> (String, String) {
    (
        format!("{INTEROP_DECLS}\n{INTEROP_CALLEE}"),
        format!("{INTEROP_DECLS}\n{INTEROP_CALLER}"),
    )
}

/// c17 and Apple clang on either side of calls whose named arguments
/// overflow onto the stack. Only an Apple-compiled translation unit shows
/// that c17 packs them where Apple's rule does; c17 against itself agreed
/// under the old, wrong layout as well.
#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
#[test]
fn stacked_named_arguments_interoperate_with_apple_clang() {
    let (callee, caller) = interop_sources();
    interop_host("stacked_apple", &callee, &caller);
}

/// The same calls between c17 and aarch64 gcc under qemu, where the
/// AAPCS64 layout must not have moved.
#[cfg(target_os = "linux")]
#[test]
fn stacked_named_arguments_interoperate_with_gcc_aarch64() {
    if !aarch64_cross_available() {
        eprintln!("SKIP: no aarch64 cross toolchain");
        return;
    }
    let (callee, caller) = interop_sources();
    interop_aarch64("stacked_a64", &callee, &caller);
}

/// And on the x86-64 host, which shares the front end's argument typing.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn stacked_named_arguments_interoperate_with_gcc_host() {
    let (callee, caller) = interop_sources();
    interop_host("stacked_x86", &callee, &caller);
}
