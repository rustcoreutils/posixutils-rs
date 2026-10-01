//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Cross-target ABI assertions, made against generated assembly.
//
// Every other codegen test compiles and *runs* a program, so it can only ever
// check the host's architecture. That left the AArch64 calling convention
// covered by nothing but macOS CI — and two defects lived there through a full
// audit: a complex parameter took a general-purpose register and left the FP
// argument index untouched, so the next floating-point parameter was read from
// the register holding the complex value's real part; and a complex parameter
// was loaded from an incoming *pointer* that no longer existed once the value
// arrived in registers, overwriting what the prologue had just stored.
//
// `--target` lets any host emit for either architecture, so these run
// everywhere.
//

use super::asm_probe::{
    asm_for, asm_for_with, assert_body_lacks, body_of, AARCH64_DARWIN, AARCH64_LINUX, X86_64_LINUX,
};
use crate::common::{
    aarch64_cross_available, compile_and_run, compile_and_run_aarch64, create_c_file,
    cross_link_and_run, run_c17,
};

#[test]
fn x0_dereference_probe_still_catches_the_original_defect() {
    // The shape the guard exists for: X0 dereferenced with nothing having
    // written it, which is the incoming-pointer reload.
    assert!(dereferences_x0_before_defining_it(
        "_re_f:\n    str s0, [x29, #24]\n    ldr s0, [x0]\n    ret\n"
    ));
    // X0 computed from the frame first, then used: correct, and must pass.
    assert!(!dereferences_x0_before_defining_it(
        "_re_f:\n    str s0, [x29, #24]\n    add x0, x29, #24\n    ldr s0, [x0]\n    ret\n"
    ));
    // A load through X0 that also defines it is still a first-use read.
    assert!(dereferences_x0_before_defining_it(
        "_re_f:\n    ldr x0, [x0]\n    ret\n"
    ));
}

/// Does this body read through X0 before anything has put a value in it?
///
/// The defect being guarded against is dereferencing an incoming pointer that
/// was never passed. Asserting on the mere presence of `[x0]` cannot say that:
/// X0 is an ordinary scratch register here, so once the body has computed an
/// address into it -- `add x0, x29, #24` -- loading through it is exactly
/// right. What must never happen is the dereference coming *first*.
pub(super) fn dereferences_x0_before_defining_it(body: &str) -> bool {
    for line in body.lines() {
        let insn = line.trim();
        if insn.is_empty() || insn.starts_with('.') || insn.ends_with(':') {
            continue;
        }
        let operands = insn.split_once(char::is_whitespace).map(|(_, o)| o);
        if insn.contains("[x0]") || insn.contains("[x0,") {
            return true;
        }
        // Destination is the first operand on every aarch64 instruction that
        // writes a register.
        if let Some(first) = operands.and_then(|o| o.split(',').next()) {
            if first.trim() == "x0" {
                return false;
            }
        }
    }
    false
}

/// A MEMORY-class aggregate is passed by value on the stack, not as a pointer.
///
/// System V classifies `struct R { long double v; }` MEMORY as an *argument*:
/// gcc leaves sixteen bytes on the stack and the callee reads them with
/// `fldt 8(%rsp)`. c17 decided by raw size, and 128 bits is not greater than
/// 128, so it took the medium-struct path and passed a pointer in RDI. Both
/// sides agreed within one translation unit, which is why running a program
/// could never catch it -- and disagreed with every gcc-compiled peer.
///
/// The two shapes that reach MEMORY at this size are an aggregate whose sole
/// content is a `long double`, and one that merges a `long double` with
/// something else in an eightbyte.
#[test]
fn codegen_memory_class_struct_arrives_by_value() {
    let src = r#"
struct R { long double v; };
union  M { long double v; double d; };
struct P { double a, b; };
struct I { long a, b; };

long double take_r(struct R a) { return a.v; }
long double take_m(union  M a) { return a.v; }
double      take_p(struct P a) { return a.a + a.b; }
long        take_i(struct I a) { return a.a + a.b; }
"#;
    let asm = asm_for("memory_class_arg", X86_64_LINUX, src);

    for name in ["take_r", "take_m"] {
        let body = body_of(&asm, name);
        assert!(
            body.contains("16(%rbp)"),
            "{name} must read its argument from the incoming argument area:\n{body}"
        );
        assert!(
            !body.contains("movq (%rdi)"),
            "{name} must not dereference a pointer argument:\n{body}"
        );
    }

    // Controls: an all-SSE pair still travels in XMM registers, and an
    // integer pair still takes today's pointer path. Neither may move.
    let p = body_of(&asm, "take_p");
    assert!(
        p.contains("%xmm0") && p.contains("%xmm1"),
        "a two-double struct still arrives in two XMM registers:\n{p}"
    );
    // An integer pair arrives in two general registers by value, which the
    // prologue writes into the parameter's local. It used to arrive as a
    // pointer, which no gcc-compiled caller ever sends.
    let i = body_of(&asm, "take_i");
    assert!(
        i.contains("movq %rdi,") && i.contains("movq %rsi,"),
        "an integer pair arrives in RDI and RSI by value:\n{i}"
    );
    assert!(
        !i.contains("movq (%rdi)"),
        "and must not be dereferenced as a pointer:\n{i}"
    );
}

/// A union is an HFA of its largest member, not of all of them at once.
///
/// A union's members overlap, so `union { double v; double d; }` is eight
/// bytes and one V register. The HFA walk summed member counts, making it a
/// *two*-element HFA: the callee read sixteen bytes out of an eight-byte
/// object, and the caller wrote sixteen back into an eight-byte slot -- over
/// whatever followed it on the frame.
///
/// On Apple arm64 `long double` is `double`, so `union { long double v;
/// double d; }` is exactly that shape, and returning one corrupted the
/// caller's frame badly enough to kill the process. On aarch64 Linux the same
/// union is sixteen bytes with two different bases, so it is not an HFA at all
/// and never showed the fault.
#[test]
fn codegen_aarch64_union_hfa_counts_overlapping_members_once() {
    let src = r#"
union  U { double v; double d; };
struct S { double a, b; };

union  U mku(void) { union U r; r.v = 3.25; return r; }
struct S mks(void) { struct S r; r.a = 1.0; r.b = 2.0; return r; }
double   useu(void) { union U r = mku(); return r.v; }
"#;
    let asm = asm_for("aarch64_union_hfa", AARCH64_LINUX, src);

    // `d17` contains "d1", so the register has to be matched, not the text.
    let uses_d1 = |body: &str| body.contains("d1,") || body.contains("d1]");

    let mk = body_of(&asm, "mku");
    assert!(
        !uses_d1(mk),
        "an eight-byte union is one register, not two:\n{mk}"
    );
    let use_ = body_of(&asm, "useu");
    assert!(
        !use_.contains("str d1,"),
        "the caller must not write past an eight-byte union's slot:\n{use_}"
    );
    // A struct of two doubles genuinely is two elements and must not change.
    let st = body_of(&asm, "mks");
    assert!(
        st.contains("d0,") && uses_d1(st),
        "a struct of two doubles is still a two-element HFA:\n{st}"
    );
}

/// A nine-to-sixteen-byte integer or mixed struct is handed over in two
/// registers, not as a pointer.
///
/// System V classifies each eightbyte on its own: `struct { long a, b; }` is
/// two general registers, `struct { double a; int b; }` is one SSE register and
/// one general one. c17 passed a pointer in a single general register. Caller
/// and callee agreed inside a c17 translation unit -- which is why *running* a
/// program cannot catch this, and why the assertion has to be about the
/// register file.
#[test]
fn codegen_medium_struct_uses_two_registers() {
    let src = r#"
struct LL { long a, b; };
struct DI { double a; int b; };
struct DD { double a, b; };
struct I1 { int v; };

extern long   sink_ll(struct LL);
extern double sink_di(struct DI);
extern double sink_dd(struct DD);
extern int    sink_i1(struct I1);

long   c_ll(struct LL s) { return sink_ll(s); }
double c_di(struct DI s) { return sink_di(s); }
double c_dd(struct DD s) { return sink_dd(s); }
int    c_i1(struct I1 s) { return sink_i1(s); }
"#;
    let asm = asm_for("medium_struct_regs", X86_64_LINUX, src);

    // Two general registers: the second argument register is the tell, since
    // the pointer convention only ever used the first.
    let ll = body_of(&asm, "c_ll");
    assert!(
        ll.contains("%rsi"),
        "an integer pair occupies RDI and RSI:\n{ll}"
    );
    // One SSE register and one general register.
    let di = body_of(&asm, "c_di");
    assert!(
        di.contains("%xmm0") && di.contains("%rdi"),
        "a double-then-int pair occupies XMM0 and RDI:\n{di}"
    );

    // Controls: an all-SSE pair still takes two XMMs, and a single eightbyte
    // still takes one general register.
    let dd = body_of(&asm, "c_dd");
    assert!(
        dd.contains("%xmm0") && dd.contains("%xmm1"),
        "a two-double struct still takes XMM0 and XMM1:\n{dd}"
    );
    let i1 = body_of(&asm, "c_i1");
    assert!(
        !i1.contains("%rsi"),
        "a single-eightbyte struct still takes one register:\n{i1}"
    );
}

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

/// An HFA's members are counted through every level of nesting, arrays of
/// aggregates included.
///
/// AAPCS64 5.9.5 defines a homogeneous floating-point aggregate by the
/// floating-point members a composite has when it is flattened, so
/// `struct { struct { float x, y; } p[2]; }` is four floats and goes in
/// s0-s3. `try_classify_hfa` recursed into a nested *struct* member but its
/// array arm asked only whether the element was a scalar floating type, so an
/// array of structs answered "not an HFA" and the whole aggregate went in
/// general registers:
///
/// ```text
///     c17    stp x0, x1, [x29, #16]     ; the parameter, in x0 and x1
///     clang  fadd s0, s0, s3            ; s0-s3
/// ```
///
/// Both sides are asserted, and a `struct { float x, y; }[2]` that is *too
/// long* to be an HFA -- five floats -- is the control, since flattening
/// must still respect the four-element bound.
#[test]
fn codegen_aarch64_hfa_counts_through_an_array_of_aggregates() {
    let src = r#"
struct Pair { float x, y; };
struct NEST { struct Pair p[2]; };
struct BIG  { struct Pair p[3]; };

float take(struct NEST n);
float sum(struct NEST n) { return n.p[0].x + n.p[1].y; }
float call(void) { struct NEST n; n.p[0].x = 1; n.p[0].y = 2;
                   n.p[1].x = 3; n.p[1].y = 4; return take(n) + 1.0f; }

float take_big(struct BIG b);
float big(void) { struct BIG b; b.p[0].x = 1; return take_big(b) + 1.0f; }
"#;
    for triple in [AARCH64_LINUX, AARCH64_DARWIN] {
        let a = asm_for_with("hfa_nested", triple, src, &["-O1"]);

        // The callee receives four floats, so the fourth is in s3.
        let body = body_of(&a, "sum");
        assert!(
            body.contains("s3"),
            "{triple}: a four-float HFA arrives in s0-s3:\n{body}"
        );
        assert!(
            !body.contains("stp x0, x1"),
            "{triple}: and not in general registers:\n{body}"
        );

        // The caller puts it there too.
        let body = body_of(&a, "call");
        assert!(
            body.contains("s3"),
            "{triple}: the caller passes a four-float HFA in s0-s3:\n{body}"
        );

        // The control: six floats is past the four-element bound, so it is
        // not an HFA and must go the ordinary way.
        let body = body_of(&a, "big");
        assert!(
            !body.contains("s5"),
            "{triple}: six floats exceed the HFA bound:\n{body}"
        );
    }
}

/// A zero-width bit-field allocates nothing, so it must not change how the
/// aggregate around it is passed (#C103).
///
/// Both ABIs were reading a bit-field member's *declared type* width. On
/// System V that let `int :0` at offset 4 claim bits 32..64 of the first
/// eightbyte and merge INTEGER over the SSE class the `float` had put there,
/// so `struct { float f; int :0; }` travelled in a general-purpose register
/// where gcc uses `%xmm0`. On AAPCS64 the zero-width member was simply a
/// non-floating field, which disqualified the struct from being an HFA, so the
/// same type went in `x0` where gcc uses `s0`. Both are silent ABI breaks
/// against any gcc-compiled object: a caller's `1.5f` arrived as `1.4e-45`.
///
/// The claim is that the bit-field changes *nothing*, so the assertion is that
/// each function's body is byte-identical to the same struct without it. A
/// positive check on the reference keeps the comparison honest -- two functions
/// that were both wrong in the same way would otherwise agree. And a bit-field
/// of non-zero width is a real integer member that still disqualifies the
/// aggregate, which is what stops a fix from ignoring every bit-field.
#[test]
fn cross_abi_zero_width_bitfield_does_not_change_argument_class() {
    let src = r#"
        struct PlainF { float f; };
        struct ZwF    { float f; int :0; };
        struct ZwFR   { int :0; float f; };
        struct PlainD { double f; };
        struct ZwD    { double f; int :0; };
        struct Mixed  { float f; int b:3; };

        float take_plain_f(struct PlainF v) { return v.f; }
        float take_zw_f(struct ZwF v)       { return v.f; }
        float take_zw_f_r(struct ZwFR v)    { return v.f; }
        double take_plain_d(struct PlainD v){ return v.f; }
        double take_zw_d(struct ZwD v)      { return v.f; }
        float take_mixed(struct Mixed v)    { return v.f; }
    "#;

    for (triple, fp_arg) in [(AARCH64_LINUX, "s0"), (X86_64_LINUX, "%xmm0")] {
        let asm = asm_for("zero_width_bitfield_class", triple, src);

        // The reference really must take its argument in a floating-point
        // register, or the comparisons below prove nothing.
        let plain_f = body_of(&asm, "take_plain_f");
        assert!(
            plain_f.contains(fp_arg),
            "{triple}: a lone float should arrive in {fp_arg}:\n{plain_f}"
        );

        for name in ["take_zw_f", "take_zw_f_r"] {
            assert_eq!(
                body_of(&asm, name).replace(name, "REF"),
                plain_f.replace("take_plain_f", "REF"),
                "{triple}: {name} differs from the same struct without the \
                 zero-width bit-field"
            );
        }
        let plain_d = body_of(&asm, "take_plain_d");
        assert_eq!(
            body_of(&asm, "take_zw_d").replace("take_zw_d", "REF"),
            plain_d.replace("take_plain_d", "REF"),
            "{triple}: a zero-width bit-field changed how a double is passed"
        );

        // A bit-field with bits in it is an ordinary integer member, so this
        // aggregate is *not* homogeneous and must not look like the reference.
        let mixed = body_of(&asm, "take_mixed");
        assert_ne!(
            mixed.replace("take_mixed", "REF"),
            plain_f.replace("take_plain_f", "REF"),
            "{triple}: a non-zero-width bit-field must still disqualify the \
             aggregate:\n{mixed}"
        );
    }
}

/// `__attribute__((transparent_union))` passes the union exactly as its
/// **first member** would be passed, not as the class its members merge to.
///
/// The merge is what makes this observable: SysV's `RegClass::merge` rule (d)
/// says one INTEGER makes the whole eightbyte INTEGER, so
/// `union { float f; int i; }` classifies as INTEGER, and AAPCS64 reaches the
/// same answer through its own overlap rule. gcc, under the attribute, hands
/// it over in an SSE/V register because `float` is the first member. Reversing
/// the two members must reverse the answer -- that is the whole rule, and a
/// test on one order alone would pass against a compiler that simply ignored
/// the attribute.
#[test]
fn codegen_transparent_union_is_passed_as_its_first_member() {
    let src = r#"
        union FirstFloat { float f; int i; } __attribute__((transparent_union));
        union FirstInt   { int i; float f; } __attribute__((transparent_union));
        union Plain      { float f; int i; };
        extern void sink_ff(union FirstFloat);
        extern void sink_fi(union FirstInt);
        extern void sink_pl(union Plain);
        void fwd_ff(union FirstFloat u) { sink_ff(u); }
        void fwd_fi(union FirstInt u)   { sink_fi(u); }
        void fwd_pl(union Plain u)      { sink_pl(u); }
    "#;

    for (triple, fp, gp) in [(X86_64_LINUX, "xmm0", "%edi"), (AARCH64_LINUX, "s0", "w0")] {
        let asm = asm_for("transparent_union_first_member", triple, src);

        let ff = body_of(&asm, "fwd_ff");
        assert!(
            ff.contains(fp),
            "{triple}: a transparent union whose first member is `float` must \
             travel in {fp}:\n{ff}"
        );

        // The reverse order, and the same union without the attribute, both
        // go in a general-purpose register. If either mentioned the FP one,
        // the substitution is firing on the wrong types.
        for name in ["fwd_fi", "fwd_pl"] {
            let body = body_of(&asm, name);
            assert!(
                body.contains(gp),
                "{triple}: {name} must travel in {gp}:\n{body}"
            );
            assert!(
                !body.contains(fp),
                "{triple}: {name} must not reach {fp} -- only a transparent \
                 union whose *first* member is floating point does:\n{body}"
            );
        }
    }
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

/// The bytes a prologue stores into the frame, summed by the width of each
/// store's mnemonic.
///
/// Only the prologue: the region before the first `.L` block label, which is
/// where the incoming arguments are written to their locals. A store is a move
/// whose *destination* is the frame, so the line ends with `(%rbp)`; a load
/// from the same place has it in the middle.
pub(super) fn prologue_frame_store_bytes(body: &str) -> i64 {
    body.lines()
        .take_while(|l| !l.trim().starts_with(".L"))
        .filter_map(|l| {
            let t = l.trim();
            let (mnemonic, operands) = t.split_once(' ')?;
            if !operands.ends_with("(%rbp)") {
                return None;
            }
            match mnemonic {
                "movq" | "movsd" => Some(8),
                "movl" | "movss" => Some(4),
                "movw" => Some(2),
                "movb" => Some(1),
                _ => None,
            }
        })
        .sum()
}

/// A composite parameter that arrives in registers is stored no wider than it
/// is.
///
/// Each eightbyte travels in a whole register, but the last eightbyte of a
/// composite that is not a multiple of eight holds fewer bytes than the
/// register does -- five, for a thirteen-byte one. `grow_frame` rounds a slot
/// up only to the type's own alignment, which is *one* for `unsigned char[13]`,
/// so storing the register's eight wrote three bytes past the local.
#[test]
fn codegen_a_register_pair_parameter_stores_only_its_own_bytes() {
    let src = "\
struct B13 { unsigned char c[13]; };
int probe(struct B13 v) { return v.c[0] + v.c[12]; }
";
    let asm = asm_for_with("reg_pair_tail", X86_64_LINUX, src, &["-O0"]);
    let body = body_of(&asm, "probe");
    assert_eq!(
        prologue_frame_store_bytes(body),
        13,
        "a thirteen-byte parameter is 8 + 4 + 1, and nothing more:\n{body}"
    );
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
