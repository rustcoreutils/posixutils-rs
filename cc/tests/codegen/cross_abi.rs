//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Cross-target ABI checks that need the driver: c17 linked against gcc
// under qemu, programs run on the host, and IR dumps. The assembly-shape
// assertions compile in process, in `cc/test_asm/codegen_cross_abi.rs`.
//

use super::asm_probe::{asm_for_with, assert_body_lacks, AARCH64_LINUX, X86_64_LINUX};
use crate::common::{
    aarch64_cross_available, compile_and_run, compile_and_run_aarch64, create_c_file,
    cross_link_and_run, run_c17,
};

/// c17's aarch64 code must agree with **gcc** about the ABI, not merely with
/// itself.
///
/// This links a c17-compiled translation unit against a gcc-compiled one and
/// runs the result under qemu, in both directions. It is the only shape of
/// test in this suite that can see a divergence where c17's caller and callee
/// are wrong in the same way: a zero-sized argument was charged a register by
/// both, so every c17-only program agreed with itself and every mixed program
/// read its arguments one register out. Assembly assertions could not settle
/// it either, because the wrong register is still a plausible one.
#[test]
fn codegen_aarch64_agrees_with_gcc_on_zero_sized_arguments() {
    if !aarch64_cross_available() {
        eprintln!(
            "SKIP codegen_aarch64_agrees_with_gcc_on_zero_sized_arguments: \
             no aarch64 cross toolchain"
        );
        return;
    }

    // Deliberately in separate translation units, so neither compiler can see
    // the other's idea of the calling convention.
    let callee_src = r#"
#include <stdarg.h>
struct Z { char x[0]; };

int named(int a, struct Z z, int b, int c)
{
    (void)z;
    return a * 100 + b * 10 + c;
}

int variadic(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    int a = va_arg(ap, int);
    (void)va_arg(ap, struct Z);
    int b = va_arg(ap, int);
    va_end(ap);
    (void)n;
    return a * 10 + b;
}
"#;
    let caller_src = r#"
struct Z { char x[0]; };
int named(int a, struct Z z, int b, int c);
int variadic(int n, ...);

int main(void)
{
    struct Z z;
    if (named(1, z, 2, 3) != 123) return 1;
    if (variadic(0, 4, z, 5) != 45) return 2;
    return 0;
}
"#;

    let callee_c = create_c_file("a64_abi_callee", callee_src);
    let caller_c = create_c_file("a64_abi_caller", caller_src);
    let callee_path = callee_c.path().to_string_lossy().to_string();
    let caller_path = caller_c.path().to_string_lossy().to_string();

    // Compile each side with c17 for aarch64, keeping the .c files so gcc can
    // compile the same source for the other side of each pair.
    let mut asm_paths = Vec::new();
    for (tag, src_path) in [("callee", &callee_path), ("caller", &caller_path)] {
        let out = plib::tmp::Builder::new()
            .prefix(&format!("c17_a64_abi_{tag}_"))
            .suffix(".s")
            .tempfile()
            .expect("failed to create temp file");
        let out_path = out.path().to_string_lossy().to_string();
        let run = run_c17(&[
            "--target",
            "aarch64-unknown-linux-gnu",
            "-O0",
            "-S",
            "-o",
            &out_path,
            src_path,
        ]);
        assert!(run.success, "c17 failed on the {tag}:\n{}", run.stderr);
        asm_paths.push((out, out_path));
    }
    let callee_asm = asm_paths[0].1.clone();
    let caller_asm = asm_paths[1].1.clone();

    // gcc on both sides: the reference. If this fails the probe itself is
    // wrong, and nothing below means anything.
    assert_eq!(
        cross_link_and_run("a64_abi_ref", &[&caller_path, &callee_path]),
        0,
        "the gcc/gcc reference must pass, or this probe is not testing the ABI"
    );

    assert_eq!(
        cross_link_and_run("a64_abi_c17_callee", &[&caller_path, &callee_asm]),
        0,
        "a gcc caller must be able to call a c17 callee: c17 charged the \
         zero-sized argument a register that gcc does not pass"
    );
    assert_eq!(
        cross_link_and_run("a64_abi_c17_caller", &[&caller_asm, &callee_path]),
        0,
        "a c17 caller must be able to call a gcc callee: c17 passed the \
         zero-sized argument in a register gcc does not read"
    );
    assert_eq!(
        cross_link_and_run("a64_abi_c17_both", &[&caller_asm, &callee_asm]),
        0,
        "c17 must also agree with itself"
    );
}

/// A function returning an aggregate by *address* is inlined, and the caller
/// gets the value.
///
/// AAPCS64 returns a homogeneous floating-point aggregate in `d0`-`d3` at any
/// size -- four `double`s is thirty-two bytes -- and x86-64 an x87 one in
/// st(0); in both the callee's `Ret` hands back the address of the value. The
/// inliner copies the bytes it names into the call's result local, which it
/// can do only from the `Ret`'s ABI classification. The callee's side attached
/// one only up to sixteen bytes while the caller's side had no bound, so a
/// three- or four-`double` HFA's `Ret` carried none, inlining it phi-ed the
/// address as though it were the aggregate, and every HFA and x87 return was
/// refused inlining. Both sides now ask one `returns_reg_aggregate`.
#[test]
fn codegen_address_returned_aggregates_are_inlined() {
    let src = r#"
struct H2 { double v[2]; };
struct H3 { double v[3]; };
struct H4 { double v[4]; };
struct X { long double x; };
static struct H2 mk2(double s){ struct H2 r; r.v[0]=s; r.v[1]=s+1; return r; }
static struct H3 mk3(double s){ struct H3 r; for (int i=0;i<3;i++) r.v[i]=s+i; return r; }
static struct H4 mk4(double s){ struct H4 r; for (int i=0;i<4;i++) r.v[i]=s+i; return r; }
static struct X mkx(double s){ struct X r = { s * 2 }; return r; }
__attribute__((noinline)) double use2(double s){ struct H2 b = mk2(s); return b.v[0]+b.v[1]; }
__attribute__((noinline)) double use3(double s){ struct H3 b = mk3(s); return b.v[0]+b.v[2]; }
__attribute__((noinline)) double use4(double s){ struct H4 b = mk4(s); return b.v[0]+b.v[3]; }
__attribute__((noinline)) double usex(double s){ struct X b = mkx(s); return (double)b.x; }
int main(void) {
    if (use2(1) != 3) return 1;
    if (use3(1) != 4) return 2;
    if (use4(1) != 5) return 3;
    if (usex(1.5) != 3) return 4;
    return 0;
}
"#;
    let asm = asm_for_with("hfa_inline", AARCH64_LINUX, src, &["-O2"]);
    for (caller, callee) in [("use2", "mk2"), ("use3", "mk3"), ("use4", "mk4")] {
        assert_body_lacks(
            &asm,
            caller,
            &format!("bl {callee}"),
            &format!("{caller} should inline {callee}"),
        );
    }
    let asm = asm_for_with("x87_inline", X86_64_LINUX, src, &["-O2"]);
    assert_body_lacks(&asm, "usex", "call mkx", "usex should inline mkx");

    for level in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(
                &format!("addr_ret_inline{level}"),
                src,
                &[level.to_string()]
            ),
            0,
            "{level}"
        );
        if let Some(rc) = compile_and_run_aarch64("addr_ret_inline_a64", src, level) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}

/// AAPCS64 B.4: a composite over sixteen bytes is passed as a pointer to a
/// copy the *caller* made, and the callee owns that memory.
///
/// c17 passed the address of the original object. A c17 callee copies out of
/// the pointer before touching its parameter, so c17-to-c17 never showed it;
/// a gcc callee that assigns to its parameter wrote straight into the caller's
/// global, static or local. System V is unaffected -- its MEMORY class puts
/// the bytes themselves on the stack -- which `Abi::indirect_param_is_reference`
/// now says.
#[test]
fn codegen_aarch64_large_composite_argument_is_a_copy() {
    if !aarch64_cross_available() {
        eprintln!("SKIP: no aarch64 cross toolchain");
        return;
    }
    let callee_src = r#"
struct big { long a, b, c; };
long mutate(struct big s) { s.a = 99; s.b = 99; s.c = 99; return s.a; }
"#;
    let caller_src = r#"
struct big { long a, b, c; };
struct big g = { 1, 2, 3 };
long mutate(struct big s);
__attribute__((noinline)) void via_ptr(struct big *p) { mutate(*p); }
int main(void)
{
    struct big x = { 1, 2, 3 };
    static struct big st = { 1, 2, 3 };
    if (mutate(g) != 99) return 1;
    via_ptr(&x);
    mutate(st);
    if (g.a != 1 || g.b != 2 || g.c != 3) return 2;
    if (x.a != 1 || x.c != 3) return 3;
    if (st.a != 1 || st.c != 3) return 4;
    return 0;
}
"#;
    let callee_c = create_c_file("a64_b4_callee", callee_src);
    let caller_c = create_c_file("a64_b4_caller", caller_src);
    let callee_path = callee_c.path().to_string_lossy().to_string();
    let caller_path = caller_c.path().to_string_lossy().to_string();
    assert_eq!(
        cross_link_and_run("a64_b4_ref", &[&caller_path, &callee_path]),
        0,
        "the gcc/gcc reference must pass"
    );
    for opt in ["-O0", "-O2"] {
        let out = plib::tmp::Builder::new()
            .prefix("c17_a64_b4_caller_")
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
            &caller_path,
        ]);
        assert!(run.success, "c17 failed at {opt}:\n{}", run.stderr);
        assert_eq!(
            cross_link_and_run("a64_b4_c17_caller", &[&out_path, &callee_path]),
            0,
            "{opt}: a gcc callee wrote through into the c17 caller's object"
        );
    }
}

/// The values survive the paths above, with guards either side.
///
/// Every size here is one the register-pair and all-SSE prologues classify
/// into two eightbytes whose second is short, and every object is fenced, so a
/// store that is wider than its object shows up as a clobbered guard.
#[test]
fn codegen_register_composite_parameters_keep_their_values() {
    let code = r#"
struct B13 { unsigned char c[13]; };
struct MX { int a, b; float c; };
struct __attribute__((packed)) P6 { float a, b, c; _Float16 d; };
struct __attribute__((packed)) G13 { long x; unsigned char c[5]; };

__attribute__((noinline)) int take_b13(struct B13 v)
{ int s = 0; for (int i = 0; i < 13; i++) s += v.c[i]; return s; }
__attribute__((noinline)) float take_mx(struct MX p) { return (float)(p.a + p.b) + p.c; }
__attribute__((noinline)) float take_p6(struct P6 p) { return p.a + p.b + p.c + (float)p.d; }
__attribute__((noinline)) long take_g13(struct G13 p) { return p.x + p.c[0] + p.c[4]; }

int main(void)
{
    volatile unsigned char lo = 0xA5;
    struct B13 b13;
    struct MX mx = {1, 2, 4.f};
    struct P6 p6 = {1.f, 2.f, 4.f, (_Float16)8.f};
    struct G13 g13 = {7, {1, 2, 3, 4, 5}};
    volatile unsigned char hi = 0x5A;

    for (int i = 0; i < 13; i++) b13.c[i] = (unsigned char)(i + 1);

    if (take_b13(b13) != 91) return 1;
    if (take_mx(mx) != 7.f) return 2;
    if (take_p6(p6) != 15.f) return 3;
    if (take_g13(g13) != 13) return 4;
    if (lo != 0xA5 || hi != 0x5A) return 5;
    return 0;
}
"#;
    assert_eq!(compile_and_run("register_composite_params", code, &[]), 0);
}

/// The optimized IR of `src` for `target`, with inlining left on.
pub(super) fn post_opt_ir_inlined(prefix: &str, src: &str, target: &str, func: &str) -> String {
    let dir = plib::tmp::Builder::new()
        .prefix(prefix)
        .tempdir()
        .expect("tempdir");
    let c = dir.path().join("t.c");
    std::fs::write(&c, src).expect("write source");
    let r = run_c17(&[
        "--target",
        target,
        "-O2",
        "--dump-ir",
        "post-opt",
        "--dump-ir-func",
        func,
        "-S",
        "-o",
        "/dev/null",
        c.to_str().unwrap(),
    ]);
    assert!(r.success, "compile failed: {}", r.stderr);
    format!("{}{}", r.stdout, r.stderr)
}

/// The control: the shapes that already worked must keep working, so the check
/// above cannot pass by the inliner declining to inline.
#[test]
fn codegen_inlined_aggregate_returns_still_inline() {
    let src = "\
struct F2 { float a, b; };
struct F4 { float a, b, c, d; };
static struct F2 mk2(float x) { struct F2 r = {x, x + 1}; return r; }
static struct F4 mk4(float x) { struct F4 r = {x, x+1, x+2, x+3}; return r; }
float probe2(float x) { struct F2 v = mk2(x); return v.a + v.b; }
float probe4(float x) { struct F4 v = mk4(x); return v.a + v.d; }
";
    for func in ["probe2", "probe4"] {
        let ir = post_opt_ir_inlined("inl_agg_ok", src, X86_64_LINUX, func);
        assert!(
            ir.contains("_inline"),
            "{func}'s callee must still be inlined, or the check above is vacuous:\n{ir}"
        );
        assert!(
            ir.lines().any(|l| l.contains("load")),
            "{func} must read the returned aggregate's value:\n{ir}"
        );
    }
}
