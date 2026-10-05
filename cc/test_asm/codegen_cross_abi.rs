//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Cross-target ABI assertions, made against generated assembly in process.
//
// A program run on the host can only ever check the host's architecture.
// That left the AArch64 calling convention covered by nothing but macOS CI
// -- and two defects lived there through a full audit: a complex parameter
// took a general-purpose register and left the FP argument index untouched,
// so the next floating-point parameter was read from the register holding
// the complex value's real part; and a complex parameter was loaded from an
// incoming *pointer* that no longer existed once the value arrived in
// registers, overwriting what the prologue had just stored.
//
// `--target` lets any host emit for either architecture, so these run
// everywhere. Moved from `tests/codegen/cross_abi.rs`.
//

use super::asm_probe::{
    asm_for, asm_for_with, body_of, AARCH64_DARWIN, AARCH64_LINUX, X86_64_LINUX,
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
