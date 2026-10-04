//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Cross-target ABI checks for the types each target treats its own way
// -- long double, binary128, _Float16, complex, __int128 and plain char --
// that need the driver: c17 against gcc, and IR dumps. The assembly-shape
// assertions compile in process, in `cc/test_asm/codegen_cross_abi_types.rs`.
//

use super::asm_probe::{body_of, AARCH64_LINUX, X86_64_LINUX};
use super::cross_abi::post_opt_ir_inlined;
use crate::common::{
    aarch64_cross_available, compile_with_host_cc, create_c_file, cross_link_and_run, run_c17,
};

const PAIRING_DECLS: &str = r#"
#include <stdarg.h>
typedef struct { __int128 x; } N16;                                    /* natural 16 */
typedef struct { long long a, b; } N8;                                 /* 8 */
typedef struct __attribute__((aligned(16))) { long long a, b; } A16;   /* own attribute */
typedef struct { long long a __attribute__((aligned(16))); long long b; } M16;
typedef struct __attribute__((packed)) { __int128 x; } P16;
int t_n16(long, long, long, long, long, N16, long, long);
int t_n8(long, long, long, long, long, N8, long, long);
int t_a16(long, long, long, long, long, A16, long, long);
int t_m16(long, long, long, long, long, M16, long, long);
int t_p16(long, long, long, long, long, P16, long, long);
int t_i128(long, long, long, long, long, __int128, long, long);
int t_ovf(long, long, long, long, long, long, long, N8, long);
int t_va(int, ...);
#ifdef HAVE_CI
typedef _Complex __int128 CI;
int t_arg(long a, CI z, long b, long c);
int t_stk(long a0, long a1, long a2, long a3, long a4, long a5, long a6,
          long a7, CI z, long tail);
#endif
"#;

const PAIRING_CALLEE: &str = r#"
#define CHK(cond) return (cond) ? 0 : __LINE__
int t_n16(long a, long b, long c, long d, long e, N16 s, long f, long g)
{ (void)b;(void)c;(void)d; CHK(a==1 && e==5 && (long)s.x==77 && f==8 && g==9); }
int t_n8(long a, long b, long c, long d, long e, N8 s, long f, long g)
{ (void)b;(void)c;(void)d; CHK(a==1 && e==5 && s.a==77 && s.b==78 && f==8 && g==9); }
int t_a16(long a, long b, long c, long d, long e, A16 s, long f, long g)
{ (void)b;(void)c;(void)d; CHK(a==1 && e==5 && s.a==77 && s.b==78 && f==8 && g==9); }
int t_m16(long a, long b, long c, long d, long e, M16 s, long f, long g)
{ (void)b;(void)c;(void)d; CHK(a==1 && e==5 && s.a==77 && s.b==78 && f==8 && g==9); }
int t_p16(long a, long b, long c, long d, long e, P16 s, long f, long g)
{ (void)b;(void)c;(void)d; CHK(a==1 && e==5 && (long)s.x==77 && f==8 && g==9); }
int t_i128(long a, long b, long c, long d, long e, __int128 s, long f, long g)
{ (void)b;(void)c;(void)d; CHK(a==1 && e==5 && (long)s==77 && f==8 && g==9); }

/* Stage C.11: the pair does not fit, so `h` is on the stack as well. */
int t_ovf(long a, long b, long c, long d, long e, long f, long g, N8 s, long h)
{ (void)b;(void)c;(void)d;(void)e;(void)f; CHK(a==1 && g==7 && s.a==77 && s.b==78 && h==9); }

/* `va_arg` walks the same stage-C state, and rounds `__gr_offs` to 16 for a
   16-aligned argument exactly as gcc does. */
int t_va(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    for (int i = 0; i < 5; i++)
        if (va_arg(ap, long) != i + 1) { va_end(ap); return __LINE__; }
    N16 s = va_arg(ap, N16);
    long f = va_arg(ap, long);
    va_end(ap);
    return ((long)s.x == 77 && f == 8) ? 0 : __LINE__;
}

#ifdef HAVE_CI
int t_arg(long a, CI z, long b, long c)
{ CHK(a == 1 && (long)__real__ z == 77 && (long)__imag__ z == 78 && b == 2 && c == 3); }
int t_stk(long a0, long a1, long a2, long a3, long a4, long a5, long a6,
          long a7, CI z, long tail)
{ (void)a1;(void)a2;(void)a3;(void)a4;(void)a5;(void)a6;
  CHK(a0 == 0 && a7 == 7 && (long)__real__ z == 77 && (long)__imag__ z == 78
      && tail == 4242); }
#endif
"#;

const PAIRING_CALLER: &str = r#"
int main(void)
{
    N16 n16 = { 77 };
    N8 n8 = { 77, 78 };
    A16 a16 = { 77, 78 };
    M16 m16 = { 77, 78 };
    P16 p16 = { 77 };
    __int128 i128 = 77;
    if (t_n16(1, 2, 3, 4, 5, n16, 8, 9)) return 1;
    if (t_n8(1, 2, 3, 4, 5, n8, 8, 9)) return 2;
    if (t_a16(1, 2, 3, 4, 5, a16, 8, 9)) return 3;
    if (t_m16(1, 2, 3, 4, 5, m16, 8, 9)) return 4;
    if (t_p16(1, 2, 3, 4, 5, p16, 8, 9)) return 5;
    if (t_i128(1, 2, 3, 4, 5, i128, 8, 9)) return 6;
    if (t_ovf(1, 2, 3, 4, 5, 6, 7, n8, 9)) return 7;
    if (t_va(0, 1L, 2L, 3L, 4L, 5L, n16, 8L)) return 8;
#ifdef HAVE_CI
    CI z;
    __real__ z = 77;
    __imag__ z = 78;
    if (t_arg(1, z, 2, 3)) return 21;
    if (t_stk(0, 1, 2, 3, 4, 5, 6, 7, z, 4242)) return 22;
#endif
    return 0;
}
"#;

/// c17 and aarch64 gcc on either side of calls whose arguments the
/// register-pairing rules place, under qemu: the gcc/gcc reference, then
/// c17 as callee, as caller and as both, at -O0 and -O2. Consolidates two
/// tests, in one callee unit and one caller unit; the caller exits 1..=8
/// for the second and 21..=22 for the first, whose shapes are built only
/// when this gcc accepts `_Complex __int128` (`HAVE_CI`).
///
/// `codegen_aarch64_agrees_with_gcc_on_complex_int128`:
///
/// c17 and aarch64 gcc on either side of a call carrying `_Complex __int128`.
///
/// The asm-shape tests (`cc/test_asm/codegen_cross_abi_types.rs`) say the register assignment is right; this says
/// gcc agrees, which is the only way to be sure -- caller and callee were
/// wrong in the same direction, so c17 on both sides of the call could not
/// tell. Runs only where the cross toolchain is installed.
///
/// `t_stk` is the stacked half: `z` overflows the registers and travels as a
/// pointer in an eight-byte slot, with `tail` in the next. Writing sixteen
/// bytes there is what took `tail` with it.
///
/// `codegen_aarch64_agrees_with_gcc_on_even_register_pairing`:
///
/// AAPCS64 stage C.10: an argument whose alignment is 16 starts at an **even**
/// NGRN, so an odd one skips a general register and leaves it unused; and
/// stage C.11: one that does not fit sets NGRN to 8, so everything after it is
/// on the stack too.
///
/// c17 applied C.10 only where the type was a scalar `__int128`, and C.11 not
/// at all for a general-register composite pair. Both mistakes were made by
/// the caller and the callee alike, so every c17-only program agreed with
/// itself -- this links against gcc in both directions, which is the only
/// shape that can see it.
///
/// The five shapes are the discriminating ones, and it is the *alignment*
/// that decides, not the type: `struct { __int128 x; }` and a struct whose
/// first member carries `aligned(16)` round, while the same struct carrying
/// `aligned(16)` on *itself* does not, and neither does a packed one. Five
/// leading `long`s make NGRN odd so the rounding is observable at all.
#[test]
fn codegen_aarch64_agrees_with_gcc_on_register_pairing() {
    if !aarch64_cross_available() {
        eprintln!(
            "SKIP codegen_aarch64_agrees_with_gcc_on_register_pairing: \
             no aarch64 cross toolchain"
        );
        return;
    }

    // gcc's own support for `_Complex __int128` is what that half rests on,
    // so ask before assuming it: a skip that says why beats a failure that
    // looks like the compiler's.
    let probe = create_c_file(
        "a64_ci_probe",
        "typedef _Complex __int128 CI;\nCI probe(CI z) { return z; }\n",
    );
    let probe_ok = std::process::Command::new("aarch64-linux-gnu-gcc")
        .args(["-c", "-o", "/dev/null"])
        .arg(probe.path())
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    let decls = if probe_ok {
        format!("#define HAVE_CI 1\n{PAIRING_DECLS}")
    } else {
        eprintln!(
            "SKIP the _Complex __int128 half of \
             codegen_aarch64_agrees_with_gcc_on_register_pairing: \
             this gcc does not accept _Complex __int128"
        );
        PAIRING_DECLS.to_string()
    };

    let callee_c = create_c_file("a64_pair_callee", &format!("{decls}{PAIRING_CALLEE}"));
    let caller_c = create_c_file("a64_pair_caller", &format!("{decls}{PAIRING_CALLER}"));
    let callee_path = callee_c.path().to_string_lossy().to_string();
    let caller_path = caller_c.path().to_string_lossy().to_string();

    assert_eq!(
        cross_link_and_run("a64_pair_ref", &[&caller_path, &callee_path]),
        0,
        "the gcc/gcc reference must pass, or this probe is not testing the ABI"
    );
    for opt in ["-O0", "-O2"] {
        let mut asm_paths = Vec::new();
        for (tag, src_path) in [("callee", &callee_path), ("caller", &caller_path)] {
            let out = plib::tmp::Builder::new()
                .prefix(&format!("c17_a64_pair_{tag}_"))
                .suffix(".s")
                .tempfile()
                .expect("failed to create temp file");
            let out_path = out.path().to_string_lossy().to_string();
            let run = run_c17(&[
                "--target",
                "aarch64-unknown-linux-gnu",
                opt,
                "-S",
                "-o",
                &out_path,
                src_path,
            ]);
            assert!(
                run.success,
                "c17 failed on the {tag} at {opt}:\n{}",
                run.stderr
            );
            asm_paths.push((out, out_path));
        }
        let callee_asm = asm_paths[0].1.clone();
        let caller_asm = asm_paths[1].1.clone();

        assert_eq!(
            cross_link_and_run("a64_pair_c17_callee", &[&caller_path, &callee_asm]),
            0,
            "{opt}: a gcc caller must reach a c17 callee -- c17 read the \
             16-aligned aggregate from the odd register gcc skipped, or the \
             thirty-two byte complex from a register pair gcc passes a \
             pointer in"
        );
        assert_eq!(
            cross_link_and_run("a64_pair_c17_caller", &[&caller_asm, &callee_path]),
            0,
            "{opt}: a c17 caller must reach a gcc callee -- c17 wrote the \
             16-aligned aggregate to the odd register gcc does not read, or \
             spent three registers on an argument gcc reads from one"
        );
        assert_eq!(
            cross_link_and_run("a64_pair_c17_both", &[&caller_asm, &callee_asm]),
            0,
            "{opt}: c17 must also agree with itself"
        );
    }
}

// `_Float16 _Complex` across a call, against gcc. System V classifies it as
// one SSE eightbyte (both halves packed in the low 32 bits of %xmm0); AAPCS64
// and Apple arm64 make it a two-member HFA in h0/h1. The callee takes them
// first, between other scalars, and past the eight argument registers.
const HALF_COMPLEX_CALLEE: &str = r#"
typedef _Float16 _Complex hc;
hc id(hc a) { return a; }
hc swap(hc a) { return __builtin_complex(__imag__ a, __real__ a); }
hc mix(int n, hc a, double d, hc b) {
    return __builtin_complex((_Float16)(__imag__ b + (_Float16)n),
                             (_Float16)(__real__ a + (_Float16)d));
}
hc many(hc a, hc b, hc c, hc d, hc e, hc f, hc g, hc h, hc i, hc j) {
    (void)c; (void)d; (void)e; (void)f; (void)g;
    return __builtin_complex((_Float16)(__real__ a + __real__ j),
                             (_Float16)(__imag__ i - __imag__ b + __real__ h));
}
"#;

const HALF_COMPLEX_CALLER: &str = r#"
typedef _Float16 _Complex hc;
hc id(hc a);
hc swap(hc a);
hc mix(int n, hc a, double d, hc b);
hc many(hc a, hc b, hc c, hc d, hc e, hc f, hc g, hc h, hc i, hc j);
static hc mk(double r, double i) { return __builtin_complex((_Float16)r, (_Float16)i); }
int main(void) {
    hc r = id(mk(1.5, -2.25));
    if (__real__ r != 1.5f16 || __imag__ r != -2.25f16) return 1;
    r = swap(mk(3, 4));
    if (__real__ r != 4 || __imag__ r != 3) return 2;
    r = mix(3, mk(1, 2), 0.5, mk(4, 5));
    if (__real__ r != 8 || __imag__ r != 1.5f16) return 3;
    r = many(mk(1, 2), mk(3, 4), mk(5, 6), mk(7, 8), mk(9, 10), mk(11, 12), mk(13, 14),
             mk(15, 16), mk(17, 18), mk(19, 20));
    if (__real__ r != 20 || __imag__ r != 29) return 4;
    return 0;
}
"#;

/// `_Float16 _Complex` arguments and returns agree with gcc on x86-64: a c17
/// callee under a gcc caller, and the reverse.
#[test]
fn cross_abi_float16_complex_matches_gcc_on_the_host() {
    if !cfg!(all(target_os = "linux", target_arch = "x86_64")) {
        return;
    }
    for (name, c17_unit, host_unit) in [
        (
            "half_complex_c17_callee",
            HALF_COMPLEX_CALLEE,
            HALF_COMPLEX_CALLER,
        ),
        (
            "half_complex_c17_caller",
            HALF_COMPLEX_CALLER,
            HALF_COMPLEX_CALLEE,
        ),
    ] {
        if let Some(rc) = compile_with_host_cc(name, c17_unit, host_unit) {
            assert_eq!(rc, 0, "{name}");
        }
    }
}

/// The same pairings on aarch64 under qemu, with the gcc/gcc pair as the
/// reference; and on Apple arm64, whose non-variadic HFA rule is AAPCS64's,
/// the callee reads both halves from h0 and h1.
#[test]
fn cross_abi_float16_complex_matches_gcc_on_aarch64() {
    let asm = super::asm_probe::asm_for(
        "half_complex_darwin",
        "aarch64-apple-darwin",
        HALF_COMPLEX_CALLEE,
    );
    let body = body_of(&asm, "swap");
    assert!(
        body.contains("h0") && body.contains("h1"),
        "Apple arm64 passes a _Float16 _Complex in h0/h1:\n{body}"
    );

    if !aarch64_cross_available() {
        eprintln!("SKIP: no aarch64 cross toolchain");
        return;
    }
    let callee_c = create_c_file("half_complex_callee", HALF_COMPLEX_CALLEE);
    let caller_c = create_c_file("half_complex_caller", HALF_COMPLEX_CALLER);
    let callee_src = callee_c.path().to_string_lossy().into_owned();
    let caller_src = caller_c.path().to_string_lossy().into_owned();
    assert_eq!(
        cross_link_and_run("half_complex_ref", &[&caller_src, &callee_src]),
        0,
        "the gcc/gcc reference must pass, or this probe is not testing the ABI"
    );
    for opt in ["-O0", "-O2"] {
        let asm = |src: &str, tag: &str| {
            let out = plib::tmp::Builder::new()
                .prefix(&format!("c17_half_complex_{tag}_"))
                .suffix(".s")
                .tempfile()
                .expect("failed to create temp file");
            let path = out.path().to_string_lossy().into_owned();
            let run = run_c17(&["--target", AARCH64_LINUX, opt, "-S", "-o", &path, src]);
            assert!(run.success, "c17 failed on the {tag}:\n{}", run.stderr);
            (out, path)
        };
        let (_callee_tmp, callee_s) = asm(&callee_src, "callee");
        let (_caller_tmp, caller_s) = asm(&caller_src, "caller");
        assert_eq!(
            cross_link_and_run("half_complex_c17_callee", &[&caller_src, &callee_s]),
            0,
            "gcc caller, c17 callee, {opt}"
        );
        assert_eq!(
            cross_link_and_run("half_complex_c17_caller", &[&caller_s, &callee_src]),
            0,
            "c17 caller, gcc callee, {opt}"
        );
    }
}

/// An aggregate returned in registers is spliced into its caller as its value,
/// not as the address of the callee's copy.
///
/// The inliner replaces a `Ret` with a phi of the returned value. For a
/// register-returned aggregate it has to read that value out of the callee's
/// result local first. It did for the two-register case and for a one-register
/// aggregate of eight bytes, but a *sixteen*-byte aggregate returned in one SSE
/// register -- `struct { __float128 a; }` -- had its `symaddr` fed straight
/// into the phi, so the caller received the address where the value belonged:
///
///     leaq -96(%rbp), %rax     ; the callee's result local
///     movq %r10, -64(%rbp)     ; stored into eight bytes of a sixteen-byte slot
///     movq -56(%rbp), %rax     ; the other eight read uninitialized
///
/// Inlining therefore changed the answer. Compiling for an explicit target is
/// what makes this testable at all: `__float128` is rejected on Darwin, so the
/// shape cannot be built for the host, and no test covered it.
#[test]
fn codegen_an_inlined_register_aggregate_return_is_a_value() {
    let src = "\
struct Q { __float128 a; };
static struct Q mk(__float128 x) { struct Q r = {x}; return r; }
__float128 probe(__float128 x) { struct Q v = mk(x); return v.a; }
";
    let ir = post_opt_ir_inlined("inl_sse_ret", src, X86_64_LINUX, "probe");

    // Every pseudo that holds an address rather than a value.
    let addresses: Vec<&str> = ir
        .lines()
        .filter_map(|l| {
            let t = l.trim();
            let (target, rest) = t.split_once(" = ")?;
            rest.starts_with("symaddr").then_some(target)
        })
        .collect();

    // A phi source carries the returned value, so none of them may be one.
    for line in ir.lines().map(str::trim).filter(|l| l.contains("phisrc")) {
        for addr in &addresses {
            assert!(
                !line
                    .split_whitespace()
                    .any(|w| w.trim_end_matches(',') == *addr),
                "the inlined return hands the caller {addr}, which is an address, \
                 where the aggregate's value belongs:\n  {line}\n\nfull IR:\n{ir}"
            );
        }
    }
}
