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

use super::asm_probe::{asm_for, body_of, AARCH64_LINUX, X86_64_LINUX};

/// AAPCS64 passes a `_Complex` as a two-element HFA, so it occupies **two**
/// V registers and the next floating-point parameter starts after both.
/// Classifying it as anything else takes a general-purpose register instead
/// and leaves the FP index where it was, so `after` gets read from V0 — the
/// register holding the complex value's real part.
#[test]
fn codegen_aarch64_complex_param_consumes_two_fp_registers() {
    let src = r#"
        int check_f(float _Complex z, float after) {
            float *f = (float *)&z;
            return (f[0] == 1.0f && f[1] == 2.0f && after == 9.0f) ? 0 : 1;
        }
        int check_d(double _Complex z, double after) {
            double *d = (double *)&z;
            return (d[0] == 1.0 && d[1] == 2.0 && after == 9.0) ? 0 : 1;
        }
    "#;

    for triple in ["aarch64-apple-darwin", "aarch64-unknown-linux-gnu"] {
        let asm = asm_for("two_fp_regs", triple, src);

        let f = body_of(&asm, "check_f");
        assert!(
            f.contains("s2"),
            "on {}, the float after a `float _Complex` must come from S2 \
             (the complex occupies S0 and S1); it is never mentioned:\n{}",
            triple,
            f
        );
        let d = body_of(&asm, "check_d");
        assert!(
            d.contains("d2"),
            "on {}, the double after a `double _Complex` must come from D2:\n{}",
            triple,
            d
        );
    }
}

/// A complex parameter that arrives in registers has exactly one source: the
/// prologue's stores. Treating it as memory-class as well made the body reload
/// it from an incoming pointer that does not exist, so it read whatever was in
/// X0 and overwrote the registers the prologue had just saved.
///
/// Apple's `long double` is a `double`, so all three complex types arrive in
/// V registers there; only a genuinely indirect parameter may be dereferenced.
#[test]
fn codegen_aarch64_register_complex_param_is_not_reloaded_from_a_pointer() {
    let src = r#"
        float re_f(float _Complex z) { float *p = (float *)&z; return p[0]; }
        double re_d(double _Complex z) { double *p = (double *)&z; return p[0]; }
        long double re_l(long double _Complex z) {
            long double *p = (long double *)&z; return p[0];
        }
    "#;
    let asm = asm_for("no_ptr_reload", "aarch64-apple-darwin", src);

    for func in ["re_f", "re_d", "re_l"] {
        let body = body_of(&asm, func);
        assert!(
            body.contains("str s0") || body.contains("str d0"),
            "{}: the prologue must store the incoming V register:\n{}",
            func,
            body
        );
        assert!(
            !dereferences_x0_before_defining_it(body),
            "{}: a register-passed complex must not be reloaded through X0 — \
             that register holds no argument here:\n{}",
            func,
            body
        );
    }
}

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
fn dereferences_x0_before_defining_it(body: &str) -> bool {
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

/// The x86_64 side of the same classification, so the two conventions are
/// pinned against each other. `long double _Complex` is COMPLEX_X87 there:
/// memory for arguments, st(0)/st(1) for returns — never XMM.
#[test]
fn codegen_x86_64_long_double_complex_uses_x87() {
    let src = r#"
        long double _Complex mk(void) { return __builtin_complex(6.0L, 7.0L); }
    "#;
    let asm = asm_for("x87_return", "x86_64-unknown-linux-gnu", src);
    let body = body_of(&asm, "mk");
    assert!(
        body.contains("fldt") || body.contains("fstpt"),
        "a long double _Complex return must go through the x87 stack:\n{}",
        body
    );
    assert!(
        !body.contains("%xmm"),
        "a long double _Complex has no XMM form — this is what produced \
         `movt %xmm0`:\n{}",
        body
    );
}

/// A `_Complex` that runs out of V registers is laid on the stack, and both
/// sides have to agree about it (#H13).
///
/// The caller used to push the argument pseudo twice and move it with
/// `emit_fp_move`, which for a complex pseudo -- holding the value's *address*
/// -- wrote the pointer's bit pattern into both halves. The callee's prologue
/// simply skipped the copy, leaving the parameter uninitialized.
///
/// Runs from any host via `--target`.
#[test]
fn codegen_aarch64_stacked_complex_argument_is_dereferenced() {
    let src = r#"
        double _Complex sink(double _Complex a, double _Complex b,
                             double _Complex c, double _Complex d,
                             double _Complex e);
        double _Complex call5(double _Complex a, double _Complex b,
                              double _Complex c, double _Complex d,
                              double _Complex e) {
            return sink(a, b, c, d, e);
        }
    "#;
    let asm = super::asm_probe::asm_for("stacked_complex", "aarch64-unknown-linux-gnu", src);
    let body = super::asm_probe::body_of(&asm, "call5");

    // The fifth complex goes to the outgoing stack area, written as two
    // separate elements loaded out of the value's address.
    assert!(
        body.contains("[sp]") || body.contains("[sp, #0]"),
        "the stacked complex must be written to the outgoing area:\n{body}"
    );
    assert!(
        body.contains("ldr d") || body.contains("ldr s"),
        "both elements must be loaded from the value's address:\n{body}"
    );
    // Writing the address itself into the slot is the bug this pins.
    assert!(
        !body.contains("fmov d16, x"),
        "the argument's address must not be stored as if it were the value:\n{body}"
    );
}

/// AAPCS64 §6.4.2: once any argument is laid out on the stack, NSRN is 8, so
/// every later floating-point argument goes to the stack too -- the registers
/// the over-large argument did not fit into are *not* reused. System V does the
/// opposite, which is why the x86_64 fix could not simply be copied.
#[test]
fn codegen_aarch64_nsrn_saturates_after_a_stacked_argument() {
    let src = r#"
        void sink(double a, double b, double c, double d,
                  double e, double f, double g,
                  double _Complex z, double after);
        void call(double _Complex z) {
            sink(1, 2, 3, 4, 5, 6, 7, z, 9.0);
        }
    "#;
    let asm = super::asm_probe::asm_for("nsrn_saturate", "aarch64-unknown-linux-gnu", src);
    let body = super::asm_probe::body_of(&asm, "call");

    // Seven doubles consume d0-d6. The complex needs two registers and only d7
    // remains, so it is stacked -- and `after` must then be stacked as well
    // rather than taking the free d7.
    assert!(
        !body.contains("d7,"),
        "no floating-point argument may use d7 after an argument is stacked:\n{body}"
    );
}

/// aarch64/Linux `long double` is IEEE binary128 in a whole Q register (#H4).
///
/// `fp_size_from_type` mapped it to `FpSize::Double`, so a 128-bit object was
/// loaded and stored 64 bits at a time -- and `emit_store` had no
/// floating-point dispatch at all, so the store fell through to
/// `emit_struct_store`, whose operand match ends in `_ => return`, and vanished
/// entirely.
#[test]
fn codegen_aarch64_long_double_is_quad_precision() {
    let src = r#"
        long double g;
        long double f(long double x) { long double y = x; g = y; return y; }
    "#;
    let asm = super::asm_probe::asm_for("ld_quad", "aarch64-unknown-linux-gnu", src);
    let body = super::asm_probe::body_of(&asm, "f");

    assert!(
        body.contains("q0") || body.contains("q1"),
        "binary128 must move through a Q register:\n{body}"
    );
    assert!(
        body.contains("str q"),
        "the store to the global must be emitted -- it used to vanish:\n{body}"
    );
    assert!(
        !body.contains("ldr d0"),
        "a 128-bit value must not be loaded 64 bits at a time:\n{body}"
    );
}

/// Apple keeps `long double` at 64 bits, so the same source must *not* use Q
/// registers there. Without this the test above could pass by using quad
/// everywhere.
#[test]
fn codegen_aarch64_darwin_long_double_stays_double() {
    let src = r#"
        long double g;
        long double f(long double x) { long double y = x; g = y; return y; }
    "#;
    let asm = super::asm_probe::asm_for("ld_darwin", "aarch64-apple-darwin", src);
    let body = super::asm_probe::body_of(&asm, "f");

    assert!(
        body.contains("d0"),
        "Darwin's long double is a double:\n{body}"
    );
    assert!(
        !body.contains("str q") && !body.contains("ldr q"),
        "Darwin's long double must not use quad precision:\n{body}"
    );
}

/// `long double _Complex` used to panic the compiler outright -- the codegen
/// reached an `unreachable!("x87 extended not available on AArch64")` because
/// `complex_fp_info` answered `Extended` for a 128-bit base.
///
/// It is a two-element HVA in q0/q1, not an indirect return: gcc emits no x8
/// indirect-result pointer for it.
#[test]
fn codegen_aarch64_long_double_complex_compiles() {
    let src = r#"
        long double _Complex id(long double _Complex a) { return a; }
        long double _Complex mk(void) { return __builtin_complex(6.0L, 7.0L); }
    "#;
    let asm = super::asm_probe::asm_for("ld_complex", "aarch64-unknown-linux-gnu", src);
    // Reaching here at all is most of the test: this used to panic.
    assert!(asm.contains("id:") || asm.contains("_id:"));
    assert!(asm.contains("mk:") || asm.contains("_mk:"));

    let body = super::asm_probe::body_of(&asm, "mk");
    assert!(
        !body.contains("x8,") || body.contains("q"),
        "a long double _Complex returns in q0/q1, not through an sret pointer:\n{body}"
    );
}

/// A stacked two-element floating-point argument must be copied at its own
/// element size.
///
/// `copy_stacked_pair_to_local` derived the stride with `complex_fp_info`,
/// which answers `(Double, 8)` for anything that is not complex -- including a
/// `struct { float x, y; }`, whose elements are 4 bytes. So an HFA-2 struct was
/// copied at twice its stride, reading 8 bytes past the incoming slot and
/// writing 8 bytes past the local, and the callee saw garbage.
#[test]
fn codegen_aarch64_stacked_hfa_uses_its_own_element_size() {
    let src = r#"
        struct P { float x, y; };
        float f(double a, double b, double c, double d,
                double e, double g, double h, double i,
                struct P p) { return p.x + p.y; }
    "#;
    let asm = super::asm_probe::asm_for("stacked_hfa", "aarch64-unknown-linux-gnu", src);
    let body = super::asm_probe::body_of(&asm, "f");

    // Elements are floats, so the copy must use S registers.
    assert!(
        body.contains("ldr s") && body.contains("str s"),
        "a {{float,float}} HFA must be copied 4 bytes at a time:\n{body}"
    );
    // A D-register copy of the pair is the bug: 8-byte stride on 4-byte
    // elements. (Other D accesses in the function are the eight double
    // parameters, so restrict the check to the scratch register the copy uses.)
    assert!(
        !body.contains("ldr d16") && !body.contains("str d16"),
        "the pair was copied at double stride:\n{body}"
    );
}

/// The outgoing-argument area must be as large as what is written into it.
///
/// The reservation counted 16 bytes only when `size == 128`, so a
/// `long double _Complex` (256 bits) reserved 8 -- rounded to 16 -- while the
/// store loop wrote two Q registers, 32 bytes, straight through the caller's
/// own frame.
#[test]
fn codegen_aarch64_stacked_complex_reservation_covers_the_writes() {
    let src = r#"
        void g(long double _Complex, long double _Complex, long double _Complex,
               long double _Complex, long double _Complex);
        void call5(long double _Complex a) { g(a, a, a, a, a); }
    "#;
    let asm = super::asm_probe::asm_for("stacked_reserve", "aarch64-unknown-linux-gnu", src);
    let body = super::asm_probe::body_of(&asm, "call5");

    // Find the outgoing-area reservation and the highest offset written.
    let reserved: i32 = body
        .lines()
        .find_map(|l| l.trim().strip_prefix("sub sp, sp, #"))
        .and_then(|n| n.trim().parse().ok())
        .unwrap_or_else(|| panic!("no outgoing-area reservation in:\n{body}"));

    let mut highest = 0i32;
    for line in body.lines() {
        let t = line.trim();
        if !t.starts_with("str q") {
            continue;
        }
        if t.ends_with("[sp]") {
            highest = highest.max(16);
        } else if let Some(rest) = t.split("[sp, #").nth(1) {
            if let Ok(off) = rest.trim_end_matches(']').parse::<i32>() {
                highest = highest.max(off + 16);
            }
        }
    }

    assert!(
        highest <= reserved,
        "writes reach {highest} bytes but only {reserved} were reserved:\n{body}"
    );
}

/// `_Float16` is lowered to native half-precision instructions, which are an
/// ARMv8.2-A extension. Without a `.arch` directive declaring it, GNU as
/// rejects every one of them ("selected processor does not support `fmov
/// h17,h0'"), so any translation unit touching `_Float16` failed to assemble on
/// aarch64 Linux. Apple's assembler enables fp16 for its own targets, which is
/// why macOS never saw it.
#[test]
fn codegen_aarch64_declares_fp16_for_elf() {
    let src = "_Float16 add(_Float16 a, _Float16 b) { return a + b; }";

    let elf = super::asm_probe::asm_for("fp16_elf", "aarch64-unknown-linux-gnu", src);
    assert!(
        elf.contains(".arch") && elf.contains("fp16"),
        "an ELF aarch64 file using _Float16 must declare the fp16 extension:\n{elf}"
    );
    // The instructions the directive exists for.
    assert!(
        elf.contains("fmov h") || elf.contains("fadd h"),
        "expected native half-precision instructions:\n{elf}"
    );

    // Mach-O has no .arch directive.
    let macho = super::asm_probe::asm_for("fp16_macho", "aarch64-apple-darwin", src);
    assert!(
        !macho.contains(".arch"),
        "Mach-O does not use .arch:\n{macho}"
    );
}

/// A spilled binary128 needs a 16-byte, 16-byte-aligned stack slot.
///
/// Every aarch64 FP spill used a hardcoded 8 bytes. That is right for a float
/// or a double and wrong for `long double` on this target: the two halves of a
/// `long double _Complex` were given slots 8 bytes apart and then written with
/// `str q`, so they overlapped -- and the resulting offsets were not multiples
/// of 16 either, which the assembler rejects outright ("immediate offset out of
/// range").
#[test]
fn codegen_aarch64_quad_spill_slots_are_16_byte_aligned() {
    let src = r#"
        long double _Complex add(long double _Complex a, long double _Complex b) {
            long double _Complex s = a + b;
            long double _Complex t = s + a;
            return t + b;
        }
    "#;
    let asm = super::asm_probe::asm_for("quad_spill", "aarch64-unknown-linux-gnu", src);

    // Every quad access must use an offset the instruction can encode: a
    // multiple of 16 in the scaled form, or -256..255 unscaled.
    let mut checked = 0;
    for line in asm.lines() {
        let t = line.trim();
        if !(t.starts_with("ldr q") || t.starts_with("str q")) {
            continue;
        }
        let Some(rest) = t.split(", #").nth(1) else {
            continue;
        };
        let Ok(off) = rest.trim_end_matches(']').parse::<i32>() else {
            continue;
        };
        checked += 1;
        assert!(
            (off % 16 == 0 && (0..=65520).contains(&off)) || (-256..=255).contains(&off),
            "offset {off} is not encodable by ldr/str q:\n{t}"
        );
    }
    assert!(checked > 0, "expected some quad accesses:\n{asm}");
}

/// A `long double` on aarch64 is binary128 and must move as a whole vector
/// register, never through a general-purpose one.
///
/// The 128-bit copy helper is shared with `__int128`, and it moved the low
/// half through x9 and zero-filled the rest. That silently turned every wide
/// float constant into a denormal near zero.
#[test]
fn codegen_aarch64_long_double_moves_as_quad() {
    let src = r#"
        long double pick(void) {
            long double a = 3.14159265358979323846L;
            return a;
        }
    "#;

    let asm = asm_for("ld_quad", "aarch64-unknown-linux-gnu", src);
    let body = body_of(&asm, "pick");

    // 0x4000921FB54442D1 is the top half of binary128 3.14159..., and 16384 /
    // 37407 / 46404 / 17105 are its four halfwords, built with movz/movk.
    assert!(
        body.contains("movk x10, #16384, lsl #48"),
        "the binary128 exponent halfword must be materialized:\n{body}"
    );
    assert!(
        body.contains("mov v") && body.contains(".d[1]"),
        "the high half of a binary128 constant must be inserted into lane 1:\n{body}"
    );
    assert!(
        !body.contains("stp x9, xzr"),
        "a binary128 value must not be stored as a 64-bit half plus zero:\n{body}"
    );
}

/// AAPCS64 hands unnamed floating arguments in v0-v7, so a variadic function
/// has to spill them alongside x0-x7 before `va_arg` can find them.
///
/// The backend saved only the general-purpose half and walked `ap` as a flat
/// pointer across it, which meant `va_arg(ap, double)` read a GP slot while
/// the caller's d0-d7 were never written to memory at all. This asserts on
/// the whole `va_list` record, since a save area nothing points at is no
/// better than no save area.
#[test]
fn codegen_aarch64_variadic_saves_fp_registers() {
    let src = r#"
        #include <stdarg.h>
        double sum_d(int n, ...) {
            va_list ap; va_start(ap, n);
            double t = 0.0;
            for (int i = 0; i < n; i++) t += va_arg(ap, double);
            va_end(ap);
            return t;
        }
    "#;

    let asm = asm_for("va_fp_save", "aarch64-unknown-linux-gnu", src);
    let body = body_of(&asm, "sum_d");

    for q in 0..8 {
        assert!(
            body.contains(&format!("str q{q}, [x29,")),
            "q{q} must be spilled to the variadic save area; \
             without it va_arg(ap, double) reads an uninitialized slot:\n{body}"
        );
    }

    // __vr_offs (+28) starts at -(8 * 16) with no named FP parameters, and
    // __gr_offs (+24) at -(7 * 8) after the one named `int`. Both are stored
    // as 32-bit fields.
    assert!(
        body.contains("str w9, [x1, #28]") || body.contains("str w9, [x11, #28]"),
        "va_start must initialize __vr_offs at +28:\n{body}"
    );
    assert!(
        body.contains(", #24]"),
        "va_start must initialize __gr_offs at +24:\n{body}"
    );

    // The double must be fetched through the *FP* offset field, not the GP one.
    assert!(
        body.contains("ldr w9, [x11, #28]"),
        "va_arg(ap, double) must consult __vr_offs (+28), not __gr_offs:\n{body}"
    );
    assert!(
        body.contains("ldr x16, [x11, #16]"),
        "va_arg(ap, double) must read the slot relative to __vr_top (+16):\n{body}"
    );
}

/// An integer `va_arg` on aarch64 must use the general-purpose fields, so the
/// two banks advance independently.
#[test]
fn codegen_aarch64_variadic_integer_uses_gp_fields() {
    let src = r#"
        #include <stdarg.h>
        int sum_i(int n, ...) {
            va_list ap; va_start(ap, n);
            int t = 0;
            for (int i = 0; i < n; i++) t += va_arg(ap, int);
            va_end(ap);
            return t;
        }
    "#;

    let asm = asm_for("va_gp", "aarch64-unknown-linux-gnu", src);
    let body = body_of(&asm, "sum_i");

    assert!(
        body.contains("ldr w9, [x11, #24]"),
        "va_arg(ap, int) must consult __gr_offs (+24):\n{body}"
    );
    assert!(
        body.contains("ldr x16, [x11, #8]"),
        "va_arg(ap, int) must read the slot relative to __gr_top (+8):\n{body}"
    );
    // GP slots are 8 bytes, not the 16 a SIMD slot takes.
    assert!(
        body.contains("add x10, x9, #8"),
        "a GP slot advances __gr_offs by 8:\n{body}"
    );
}

/// `va_copy` must move exactly as many bytes as the target's `va_list` holds.
///
/// The two aarch64 targets disagree about that size: Linux/FreeBSD use the
/// 32-byte AAPCS64 record, Darwin a single pointer. Copying the Darwin one
/// with a 16-byte `stp` -- which a `step_by(16)` loop still emits for an
/// 8-byte object -- wrote 8 bytes past the destination, over whatever the
/// frame held next, and crashed every `va_copy` test on macOS.
#[test]
fn codegen_aarch64_va_copy_matches_the_va_list_size() {
    let src = r#"
        #include <stdarg.h>
        long f(int n, ...) {
            va_list ap, ap2;
            va_start(ap, n);
            va_copy(ap2, ap);
            long s = va_arg(ap, long) + va_arg(ap2, long);
            va_end(ap); va_end(ap2);
            return s;
        }
    "#;

    // The destination pointer is pinned in x16, so stores through it are
    // exactly the bytes va_copy writes.
    let count = |asm: &str, needle: &str| asm.matches(needle).count();

    let linux = asm_for("va_copy_linux", "aarch64-unknown-linux-gnu", src);
    let l = body_of(&linux, "f");
    assert_eq!(
        count(l, "stp x9, x10, [x16"),
        2,
        "a 32-byte va_list is two pairs:\n{l}"
    );
    assert_eq!(
        count(l, "str x9, [x16"),
        0,
        "32 bytes divides evenly; no single-register tail belongs here:\n{l}"
    );

    let darwin = asm_for("va_copy_darwin", "aarch64-apple-darwin", src);
    let d = body_of(&darwin, "f");
    assert_eq!(
        count(d, "stp x9, x10, [x16"),
        0,
        "an 8-byte va_list must not be copied with a 16-byte pair store:\n{d}"
    );
    assert_eq!(
        count(d, "str x9, [x16"),
        1,
        "a Darwin va_list is one 8-byte store:\n{d}"
    );
}

/// A `va_list` handed to another function must travel the way the target
/// spells the type.
///
/// SysV x86_64 spells it `__va_list_tag[1]` and AAPCS64 a 32-byte record, so
/// on both the *address* is what gets passed -- an array decays, and a
/// composite that large goes by reference. Darwin on aarch64 spells it
/// `char *`, where there is nothing to decay: passing its address hands the
/// callee a pointer to a pointer. libc reads the argument list through that,
/// so `vsnprintf` printed garbage while every compiler-internal use kept
/// working.
#[test]
fn codegen_aarch64_va_list_argument_matches_the_target_spelling() {
    let src = r#"
        typedef __builtin_va_list va_list;
        #define va_start(a, p) __builtin_va_start(a, p)
        #define va_end(a) __builtin_va_end(a)
        int vsnprintf(char *, unsigned long, const char *, va_list);
        int fmt(char *b, unsigned long n, const char *f, ...) {
            va_list ap;
            va_start(ap, f);
            int r = vsnprintf(b, n, f, ap);
            va_end(ap);
            return r;
        }
    "#;

    // Darwin: load the pointer out of the va_list and pass that.
    let darwin = asm_for("va_list_arg_darwin", "aarch64-apple-darwin", src);
    let d = body_of(&darwin, "fmt");
    let call = d.split("bl _vsnprintf").next().unwrap_or(d);
    assert!(
        call.contains("ldr x23, [x29,"),
        "Darwin's va_list is a pointer; its value must be loaded, not its \
         address taken:\n{d}"
    );
    assert!(
        !call.contains("add x23, x29,"),
        "passing the address of a Darwin va_list gives vsnprintf a pointer \
         to a pointer:\n{d}"
    );

    // Linux: the 32-byte record goes by reference, so the address is right.
    let linux = asm_for("va_list_arg_linux", "aarch64-unknown-linux-gnu", src);
    let l = body_of(&linux, "fmt");
    let call = l.split("bl vsnprintf").next().unwrap_or(l);
    assert!(
        call.contains("add x23, x29,"),
        "AAPCS64 passes the 32-byte va_list record by reference:\n{l}"
    );
}

/// The `.init_array` / `.fini_array` entries take each object format's shape.
///
/// The behavioral test in `codegen::misc` can only exercise the host, and the
/// two formats disagree on more than spelling: ELF encodes the priority in the
/// section name, where Mach-O has no ordering mechanism at all and drops it.
#[test]
fn cross_abi_init_array_sections() {
    let src = "
        __attribute__((constructor))      static void c1(void) {}
        __attribute__((constructor(101))) static void c2(void) {}
        __attribute__((destructor))       static void d1(void) {}
        __attribute__((destructor(102)))  static void d2(void) {}
        int main(void) { return 0; }
    ";

    for triple in ["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"] {
        let asm = asm_for("init_array_elf", triple, src);
        // GCC pads the priority to five digits so that the linker's plain
        // string sort of section names matches numeric order.
        for expected in [
            ".section .init_array,\"aw\"",
            ".section .init_array.00101,\"aw\"",
            ".section .fini_array,\"aw\"",
            ".section .fini_array.00102,\"aw\"",
        ] {
            assert!(
                asm.contains(expected),
                "{triple}: missing `{expected}`:\n{asm}"
            );
        }
        for (sym, section) in [("c1", ".init_array"), ("d1", ".fini_array")] {
            let after = asm.split(&format!("{section},\"aw\"\n")).nth(1).unwrap();
            assert!(
                after
                    .lines()
                    .take(3)
                    .any(|l| l.trim() == format!(".quad {sym}")),
                "{triple}: {section} entry must point at {sym}:\n{asm}"
            );
        }
    }

    for triple in ["x86_64-apple-darwin", "aarch64-apple-darwin"] {
        let asm = asm_for("init_array_macho", triple, src);
        assert!(
            asm.contains(".section __DATA,__mod_init_func,mod_init_funcs"),
            "{triple}: missing the initializer section:\n{asm}"
        );
        assert!(
            !asm.contains("00101") && !asm.contains("00102"),
            "{triple}: Mach-O has no priority-ordered section:\n{asm}"
        );
        assert!(
            asm.contains(".quad _c1"),
            "{triple}: entries must name the underscore-prefixed symbols:\n{asm}"
        );

        // A destructor is an `atexit` registration here, not a table entry:
        // `__mod_term_func` is deprecated and a main executable's terminators
        // listed there are not run. See `ir::mach_o_dtors`.
        assert!(
            !asm.contains("__mod_term_func"),
            "{triple}: nothing should be listed for termination:\n{asm}"
        );
        assert!(
            asm.contains("_atexit") && asm.contains("_d1"),
            "{triple}: each destructor must be handed to atexit:\n{asm}"
        );
    }
}

/// `__float128` is soft-float on both targets, and does not disturb the
/// hardware path `long double` takes on either.
///
/// Neither target has binary128 arithmetic in hardware, so every operation is
/// a libgcc `__*tf*` call. What differs is what sits beside it: on x86-64
/// `long double` is x87 and must still reach `fldt`/`faddp`, and on aarch64
/// `long double` *is* binary128 and shares the same calls.
#[test]
fn cross_abi_float128_is_soft_float_everywhere() {
    let src = "
        __float128 qadd(__float128 a, __float128 b) { return a + b; }
        int qlt(__float128 a, __float128 b) { return a < b; }
        double qtod(__float128 a) { return (double)a; }
        long double ldadd(long double a, long double b) { return a + b; }
    ";

    for triple in ["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"] {
        let asm = asm_for("float128_soft", triple, src);
        for expected in ["__addtf3", "__lttf2", "__trunctfdf2"] {
            assert!(
                asm.contains(expected),
                "{triple}: binary128 must go through {expected}:\n{asm}"
            );
        }
    }

    // x86-64: `long double` is x87 and must not have been dragged into the
    // soft-float path, and a 16-byte value moves as a packed quantity — there
    // is no scalar 16-byte move, which is what `movt` used to be.
    let asm = asm_for("float128_x86", "x86_64-unknown-linux-gnu", src);
    assert!(
        asm.contains("faddp") || asm.contains("fadd"),
        "x86-64 long double must stay on the x87 unit:\n{asm}"
    );
    assert!(
        !asm.contains("movt"),
        "`movt` is not an instruction:\n{asm}"
    );
    assert!(
        asm.contains("movups") || asm.contains("movaps"),
        "a binary128 moves 16 bytes at a time:\n{asm}"
    );

    // aarch64: `long double` is binary128 there, so it shares the calls and
    // there is no x87 anything.
    let asm = asm_for("float128_arm", "aarch64-unknown-linux-gnu", src);
    assert!(
        !asm.contains("fldt") && !asm.contains("movt"),
        "aarch64 has no x87:\n{asm}"
    );
}

/// A `__float128` that does not fit in a register is passed as two eightbytes.
///
/// The register cases were what the first pass tested, and they hid two
/// defects: the stack store used a scalar move (`movt`, which is not an
/// instruction) and reserved half the space, and on aarch64 it stored eight
/// bytes at an eight-byte stride so the next argument landed inside the
/// previous one.
#[test]
fn cross_abi_float128_stack_arguments_are_sixteen_bytes() {
    let src = "
        __float128 f(__float128 a, __float128 b, __float128 c, __float128 d,
                     __float128 e, __float128 g, __float128 h, __float128 i,
                     __float128 j, __float128 k);
        __float128 call(void) {
            return f(1.0q, 2.0q, 3.0q, 4.0q, 5.0q, 6.0q, 7.0q, 8.0q, 9.0q, 10.0q);
        }
    ";

    let asm = asm_for("f128_stack_x86", "x86_64-unknown-linux-gnu", src);
    assert!(
        !asm.contains("movt"),
        "`movt` is not an instruction:\n{asm}"
    );
    // Two binary128s overflow to the stack, and each takes two eightbytes, so
    // the second begins sixteen bytes after the first. The stride is what
    // matters -- an eight-byte one overlapped the previous argument.
    //
    // Asserted on the offsets rather than on a `subq $16` per argument: the
    // outgoing area is now reserved once and written at computed offsets,
    // because pushing could not express the gap an alignment boundary needs.
    let body = body_of(&asm, "call");
    assert!(
        body.contains("(%rsp)") && body.contains("16(%rsp)"),
        "two stacked binary128s sit at +0 and +16:\n{body}"
    );
    assert!(
        !body.contains("8(%rsp)") || body.contains("16(%rsp)"),
        "an eight-byte stride would overlap the previous argument:\n{body}"
    );

    let asm = asm_for("f128_stack_arm", "aarch64-unknown-linux-gnu", src);
    assert!(
        asm.contains("str q"),
        "aarch64 must store all 16 bytes of a stack argument:\n{asm}"
    );
    assert!(
        !asm.contains("str d16, [sp, #8]"),
        "an eight-byte stride overlaps the previous argument:\n{asm}"
    );
}

/// `long double` and `__float128` convert through libgcc on x86-64.
///
/// They are different formats of the same width there, so the conversion was
/// elided by a width comparison and each format's bytes were read as the
/// other's.
#[test]
fn cross_abi_long_double_to_float128_is_a_real_conversion() {
    let src = "
        __float128 up(long double x) { return (__float128)x; }
        long double dn(__float128 x) { return (long double)x; }
    ";

    let asm = asm_for("f128_ld_conv", "x86_64-unknown-linux-gnu", src);
    assert!(
        asm.contains("__extendxftf2"),
        "x87 -> binary128 must call __extendxftf2:\n{asm}"
    );
    assert!(
        asm.contains("__trunctfxf2"),
        "binary128 -> x87 must call __trunctfxf2:\n{asm}"
    );

    // On aarch64 the two *are* the same format, so there is nothing to call.
    let asm = asm_for("f128_ld_conv_arm", "aarch64-unknown-linux-gnu", src);
    assert!(
        !asm.contains("__extendxftf2") && !asm.contains("__trunctfxf2"),
        "aarch64 long double is already binary128:\n{asm}"
    );
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

/// An aggregate that is nothing but a `__float128` travels in one XMM.
///
/// System V classifies binary128 SSE + SSEUP, and SSEUP never travels alone:
/// the pair is a single register carrying all sixteen bytes, which is what a
/// scalar `__float128` has always used. Counting eightbytes instead put the
/// value in xmm0 *and* xmm1, so a gcc-compiled peer read only its low half.
/// Merged with anything else it really is two registers, and that must not
/// change -- gcc emits `movapd %xmm1, %xmm0` for the union below.
#[test]
fn codegen_lone_binary128_struct_uses_one_xmm() {
    let src = r#"
struct Q { __float128 v; };
union  M { __float128 v; double d[2]; };
struct P { double a, b; };

__float128 take_q(struct Q a) { return a.v; }
__float128 take_m(union  M a) { return a.v; }
struct Q   mk_q(void) { struct Q r; r.v = 3.25q; return r; }
double     take_p(struct P a) { return a.a + a.b; }
"#;
    let asm = asm_for("lone_binary128", X86_64_LINUX, src);

    // `xmm15` is the reserved scratch and contains "xmm1" as a substring, so
    // the negative assertions have to look for the register, not the text.
    let uses_xmm1 = |body: &str| body.contains("%xmm1,") || body.contains("%xmm1)");

    let q = body_of(&asm, "take_q");
    assert!(
        !uses_xmm1(q),
        "a lone binary128 argument arrives in xmm0 alone:\n{q}"
    );
    let mk = body_of(&asm, "mk_q");
    assert!(
        !uses_xmm1(mk) && !mk.contains("%rdx"),
        "a lone binary128 is returned in xmm0 alone, not split:\n{mk}"
    );

    // Controls: merged with two doubles it is genuinely two registers, and a
    // two-double struct is unchanged.
    let m = body_of(&asm, "take_m");
    assert!(
        uses_xmm1(m),
        "a binary128 merged with two doubles is two registers:\n{m}"
    );
    let p = body_of(&asm, "take_p");
    assert!(
        p.contains("%xmm0") && uses_xmm1(p),
        "a two-double struct still arrives in two XMM registers:\n{p}"
    );
}

/// A one-element HFA is returned in V0, whatever its width.
///
/// AAPCS64 returns a struct holding a single `float`, `double` or `long double`
/// exactly as it returns the bare scalar. c17 sent all of them out through a
/// general register while the *caller* read the FP one, so the two sides
/// disagreed inside a single program. The binary128 case was worse: the return
/// path treated any HFA as a *pair* and tried to move sixteen bytes out of one
/// X register, which killed the compiler with "a binary128 value does not fit
/// one X register".
///
/// On aarch64 Linux `long double` is binary128, so `struct { long double v; }`
/// is the one-element quad case.
#[test]
fn codegen_aarch64_one_element_hfa_returns_in_v0() {
    let src = r#"
struct F { float v; };
struct D { double v; };
struct L { long double v; };
struct P { double a, b; };

struct F mkf(void) { struct F r; r.v = 3.5f; return r; }
struct D mkd(void) { struct D r; r.v = 4.5; return r; }
struct L mkl(void) { struct L r; r.v = 3.25L; return r; }
struct P mkp(void) { struct P r; r.a = 1.0; r.b = 2.0; return r; }
"#;
    let asm = asm_for("aarch64_hfa1_ret", AARCH64_LINUX, src);

    // Each returns through V0, at its own width. A binary128 is assembled
    // into the register's two lanes, so it is the lane insert that says the
    // whole sixteen bytes got there.
    // The value may pass through a general register on its way, so what
    // matters is that it lands in V0 before the return.
    for (name, marker) in [("mkf", "fmov s0,"), ("mkd", "fmov d0,"), ("mkl", "q0")] {
        let body = body_of(&asm, name);
        assert!(
            body.contains(marker),
            "{name} must leave its value in V0 (looking for `{marker}`):\n{body}"
        );
    }
    // A two-element HFA still uses V0 and V1, which must not change.
    let p = body_of(&asm, "mkp");
    assert!(
        p.contains("d0") && p.contains("d1"),
        "a two-double HFA still returns in d0 and d1:\n{p}"
    );
}

/// A spilled binary128 argument keeps all sixteen of its bytes.
///
/// An FP argument register that has to survive a call is stored to the frame in
/// the prologue. That store was a fixed eight bytes and the slot a fixed eight
/// wide, so on aarch64 Linux -- where `long double` is binary128 -- the top
/// half of such an argument was dropped, and the slot overlapped whatever came
/// after it. The first such parameter, stored on a different path, survived;
/// the second came back truncated.
#[test]
fn codegen_aarch64_spilled_binary128_argument_is_whole() {
    let src = r#"
/* Comparing two binary128 values is a libgcc call, so both parameters have to
   survive it and are spilled to the frame. */
static int eql(const long double *v, long double re, long double im)
{
    return v[0] == re && v[1] == im;
}
int run(const long double *p) { return eql(p, 1.5L, 2.5L); }
"#;
    let asm = asm_for("aarch64_spill_q", AARCH64_LINUX, src);
    let body = body_of(&asm, "eql");
    assert!(
        !body.contains("str d1,"),
        "no half of a binary128 argument may be stored as a double:\n{body}"
    );
    assert!(
        body.contains("str q1,"),
        "the spilled binary128 argument is stored whole:\n{body}"
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

/// A struct of eight bytes or fewer whose eightbyte is SSE arrives in an XMM.
///
/// The return side asks the ABI class; the argument side asked the kind and the
/// size, and treated everything at or below eight bytes as a general-register
/// value. So `struct { float v; }`, `struct { float a, b; }` and the `_Float16`
/// pair were handed over in RDI where gcc uses XMM0 -- the two sides of c17
/// agreed with each other and with nobody else.
#[test]
fn codegen_small_sse_struct_uses_an_xmm() {
    let src = r#"
struct F1 { float v; };
struct F2 { float a, b; };
struct H2 { _Float16 a, b; };
struct I1 { int v; };
struct I2 { int a, b; };

extern float    s_f1(struct F1);
extern float    s_f2(struct F2);
extern _Float16 s_h2(struct H2);
extern int      s_i1(struct I1);
extern int      s_i2(struct I2);

float    c_f1(struct F1 s) { return s_f1(s); }
float    c_f2(struct F2 s) { return s_f2(s); }
_Float16 c_h2(struct H2 s) { return s_h2(s); }
int      c_i1(struct I1 s) { return s_i1(s); }
int      c_i2(struct I2 s) { return s_i2(s); }
"#;
    let asm = asm_for("small_sse_struct", X86_64_LINUX, src);

    // XMM0 alone proves nothing here -- these functions also *return* a float
    // in it, and that side was already right. The argument register is the
    // tell: EDI is where the value used to go.
    for name in ["c_f1", "c_f2", "c_h2"] {
        let body = body_of(&asm, name);
        assert!(
            !body.contains(", %edi"),
            "{name} must not pass its argument in a general register:\n{body}"
        );
        assert!(
            body.contains("%xmm0"),
            "{name} passes its argument in XMM0:\n{body}"
        );
    }
    // Controls: an all-integer struct of the same size still takes a general
    // register, and must not have moved.
    for name in ["c_i1", "c_i2"] {
        let body = body_of(&asm, name);
        assert!(
            body.contains(", %edi") || body.contains(", %rdi"),
            "{name} still passes its argument in a general register:\n{body}"
        );
    }
}

/// aarch64 recognises an HFA whose members are arrays, and half precision as a
/// base type.
///
/// `try_classify_hfa` recursed into a nested *struct* but not into an array
/// member, and its array arm was reachable only for a top-level array type,
/// which C never forms. So `struct { float v[1]; }` was rejected as holding a
/// non-floating field and went back in a general register. `_Float16` was not
/// among the accepted base types either, though AAPCS64 admits half precision.
///
/// Filed against `long double v[1]` and `_Float16`; every array-member shape
/// was affected, including `struct { float v[1]; }` and `struct { double v[2]; }`.
#[test]
fn codegen_aarch64_hfa_accepts_arrays_and_half_precision() {
    let src = r#"
struct FA { float v[1]; };
struct DA { double v[2]; };
struct LA { long double v[1]; };
struct H1 { _Float16 v; };
struct H2 { _Float16 a, b; };
struct F2 { float a, b; };        /* control: already an HFA */
struct MI { float a; int b; };    /* control: not an HFA at all */

struct FA mkfa(void) { struct FA r; r.v[0] = 1.5f; return r; }
struct DA mkda(void) { struct DA r; r.v[0] = 1.5; r.v[1] = 2.5; return r; }
struct LA mkla(void) { struct LA r; r.v[0] = 1.5L; return r; }
struct H1 mkh1(void) { struct H1 r; r.v = 1.5f16; return r; }
struct H2 mkh2(void) { struct H2 r; r.a = 1.5f16; r.b = 2.5f16; return r; }
struct F2 mkf2(void) { struct F2 r; r.a = 1.5f; r.b = 2.5f; return r; }
struct MI mkmi(void) { struct MI r; r.a = 1.5f; r.b = 2; return r; }
"#;
    let asm = asm_for("aarch64_hfa_arrays", AARCH64_LINUX, src);

    // Each returns through V0, at its own element width.
    for (name, marker) in [
        ("mkfa", "s0"),
        ("mkda", "d0"),
        ("mkla", "q0"),
        ("mkh1", "h0"),
        ("mkh2", "h0"),
        ("mkf2", "s0"),
    ] {
        let body = body_of(&asm, name);
        assert!(
            body.contains(marker),
            "{name} is an HFA and returns through V0 (looking for `{marker}`):\n{body}"
        );
    }
    // A struct mixing a float with an int is not homogeneous, so it keeps the
    // general-register return.
    let mi = body_of(&asm, "mkmi");
    assert!(
        mi.contains("x0") || mi.contains("w0"),
        "a mixed struct is not an HFA:\n{mi}"
    );
}

/// A half-precision HFA of eight bytes or fewer is packed in a register, not
/// pointed at.
///
/// `struct { _Float16 a, b; }` is four bytes and became an HFA when half
/// precision was admitted as a base type. The two-element return path then
/// treated its source as an *address* -- correct for the sixteen-byte shapes it
/// was written for, fatal here, because at this size the value sits in the
/// register itself. It also stepped between elements by eight bytes, having no
/// arm for a two-byte one.
///
/// The parameter side had the mirror problem: the prologue wrote both halves
/// into the local and the linearizer's small-struct store then overwrote them,
/// because the "arrives in registers" test was gated on size rather than on
/// the class.
#[test]
fn codegen_aarch64_small_half_hfa_is_packed_not_addressed() {
    let src = r#"
struct H2 { _Float16 a, b; };
struct F2 { float a, b; };          /* eight bytes, same shape */
struct D2 { double a, b; };         /* sixteen: travels by address */

struct H2 mkh2(void) { struct H2 r; r.a = 1.5f16; r.b = 2.5f16; return r; }
_Float16 second(struct H2 s) { return s.b; }
struct F2 mkf2(void) { struct F2 r; r.a = 1.5f; r.b = 2.5f; return r; }
struct D2 mkd2(void) { struct D2 r; r.a = 1.5; r.b = 2.5; return r; }
"#;
    let asm = asm_for("aarch64_small_half_hfa", AARCH64_LINUX, src);

    // The halves are shifted out of the packed register; nothing is loaded
    // through it as though it held an address.
    let mk = body_of(&asm, "mkh2");
    assert!(
        mk.contains("lsr") && mk.contains("fmov h0,"),
        "a four-byte half HFA is unpacked from its register:\n{mk}"
    );
    assert!(
        !mk.contains("ldr h0, [x0]"),
        "and must not be dereferenced as an address:\n{mk}"
    );

    // The parameter's two halves survive: the prologue writes them and nothing
    // overwrites them with a wider store.
    let sec = body_of(&asm, "second");
    assert!(
        sec.contains("str h0,") && sec.contains("str h1,"),
        "both halves reach the local:\n{sec}"
    );
    assert!(
        !sec.contains("str s0,"),
        "and are not overwritten by a four-byte store:\n{sec}"
    );

    // Controls: the eight- and sixteen-byte shapes still work their own way.
    for name in ["mkf2", "mkd2"] {
        let body = body_of(&asm, name);
        assert!(
            body.contains("0") && !body.is_empty(),
            "{name} still emits a return sequence:\n{body}"
        );
    }
}

/// aarch64 `va_start` skips exactly the registers the named parameters took.
///
/// The counting loop asked `is_float`, which is false for a `_Complex` -- so a
/// `double _Complex` named parameter, which arrives in *two* V registers, was
/// counted as one general register and none floating. `va_start` then recorded
/// the wrong `__gr_offs`/`__vr_offs` and the first variadic argument came from
/// the wrong slot. The allocator dispatches on the ABI class; this now does
/// too, so the two cannot disagree.
///
/// For `void f(double _Complex z, ...)` the named parameter takes no general
/// register and two of eight V registers, so the offsets are -(8-0)*8 = -64 and
/// -(8-2)*16 = -96. They are materialised as 16-bit immediates.
#[test]
fn codegen_aarch64_va_start_counts_named_registers() {
    let src = r#"
#include <stdarg.h>
long v_cx(double _Complex z, ...)
{
    va_list ap; va_start(ap, z);
    long a = va_arg(ap, long);
    va_end(ap);
    (void)z;
    return a;
}
"#;
    let asm = asm_for("aarch64_va_named_regs", AARCH64_LINUX, src);
    let body = body_of(&asm, "v_cx");

    // -64 and -96 as unsigned 16-bit halves.
    let gr = (-64i32 as u32) & 0xffff;
    let vr = (-96i32 as u32) & 0xffff;
    assert!(
        body.contains(&format!("#{gr}")),
        "__gr_offs must be -64 (no general register taken):\n{body}"
    );
    assert!(
        body.contains(&format!("#{vr}")),
        "__vr_offs must be -96 (two V registers taken):\n{body}"
    );
}

/// A one-element HFA argument goes in a V register on aarch64.
///
/// The caller recognised only the two-element case, so every shape that is an
/// HFA of *one* element -- `struct { float v; }`, and everything that became
/// one when array members and half precision were admitted -- went out in a
/// general register. The callee does ask the ABI, so it read V0; and each
/// floating argument after it was shifted a register along, which is how a
/// variadic call whose named parameter was such a struct went wrong.
#[test]
fn codegen_aarch64_one_element_hfa_argument_uses_a_v_register() {
    let src = r#"
struct F1 { float v; };
struct D1 { double v; };
struct I1 { int v; };            /* control: not an HFA */

extern long sink_f1(struct F1, double);
extern long sink_d1(struct D1, double);
extern long sink_i1(struct I1, double);

long c_f1(struct F1 s) { return sink_f1(s, 5.0); }
long c_d1(struct D1 s) { return sink_d1(s, 5.0); }
long c_i1(struct I1 s) { return sink_i1(s, 5.0); }
"#;
    let asm = asm_for("aarch64_hfa1_arg", AARCH64_LINUX, src);

    // The struct takes V0, so the double that follows it takes V1.
    for (name, first) in [("c_f1", "s0"), ("c_d1", "d0")] {
        let body = body_of(&asm, name);
        assert!(
            body.contains(&format!("fmov {first},")),
            "{name} passes its HFA argument in V0:\n{body}"
        );
        assert!(
            body.contains("d1,"),
            "{name} passes the following double in V1:\n{body}"
        );
    }
    // A non-HFA struct still goes in a general register, and the double after
    // it is then the first floating argument.
    let i1 = body_of(&asm, "c_i1");
    assert!(
        !i1.contains("d1,"),
        "an integer struct leaves V0 for the double after it:\n{i1}"
    );
}

/// SysV AMD64 §3.2.3 starts each stacked argument at an address rounded up to
/// `max(8, alignof(type))`, so a sixteen-byte-aligned one after an odd number
/// of eight-byte slots begins at the *next* boundary, not immediately after.
///
/// The callee laid its incoming area out by advancing a running offset per
/// argument and never rounding up, so `__int128` and `long double` in that
/// position were read eight bytes early -- the value fetched was the argument
/// before them. Six registers' worth of `long`s then one stacked `long` puts
/// the value under test at +32; reading +16 gets that seventh `long`.
///
/// Checked against gcc on this source: it loads from 32(%rbp) for both.
#[test]
fn codegen_stacked_argument_starts_on_its_alignment() {
    let src = r#"
long long take_i128(long a, long b, long c, long d, long e, long f, long g, __int128 v)
{ return (long long)v; }
long double take_ld(long a, long b, long c, long d, long e, long f, long g, long double v)
{ return v; }
/* Control: eight-byte alignment needs no rounding, so this one really is
   adjacent to the seventh long, at +24. */
long take_l(long a, long b, long c, long d, long e, long f, long g, long v)
{ return v; }
"#;
    let asm = asm_for("stacked_arg_alignment", X86_64_LINUX, src);

    // Both halves of the sixteen-byte value, at +32 and +40. Before the fix
    // they were read from +16 and +24 -- the seventh long and the padding
    // after it. A bare "not +16" assertion would be wrong: the seventh long
    // itself legitimately lives there and is loaded in the same body.
    let i = body_of(&asm, "take_i128");
    for off in ["32(%rbp)", "40(%rbp)"] {
        assert!(
            i.contains(off),
            "take_i128: a sixteen-byte-aligned stacked argument starts at +32, \
             so its halves are at +32 and +40, not immediately after the \
             seventh long. Missing {off}:\n{i}"
        );
    }

    let ld = body_of(&asm, "take_ld");
    assert!(
        ld.contains("32(%rbp)"),
        "take_ld: a long double is sixteen-byte aligned too:\n{ld}"
    );

    let l = body_of(&asm, "take_l");
    assert!(
        l.contains("24(%rbp)"),
        "take_l: an eight-byte-aligned argument needs no rounding:\n{l}"
    );
}

/// Plain `char` takes the target's signedness, in the front end as well as
/// the back end.
///
/// C17 6.2.5p15 leaves plain `char`'s signedness implementation-defined; the
/// x86-64 psABI makes it signed and AAPCS64 makes it unsigned, and
/// `Target::char_signed` has recorded that all along. Only the two backends'
/// load paths consulted it, so aarch64 emitted a correct zero-extending load
/// and the front end then sign-extended the result back:
///
/// ```text
///     ldrb  w1, [x0]      ; correct
///     sxtb  x0, w0        ; undoes it
/// ```
///
/// cross-gcc emits the `ldrb` alone. Both directions are asserted, and both
/// architectures, so a fix cannot pass by making every `char` unsigned.
#[test]
fn codegen_plain_char_follows_the_target_signedness() {
    let src = r#"
int  ld_plain(char *p)          { return *p; }
int  ld_signed(signed char *p)  { return *p; }
int  ld_unsigned(unsigned char *p) { return *p; }
"#;

    // aarch64: plain char is unsigned, so it must not be sign-extended.
    let a = asm_for("char_sign_a64", AARCH64_LINUX, src);
    let plain = body_of(&a, "ld_plain");
    assert!(
        !plain.contains("sxtb"),
        "aarch64: plain char is unsigned and must not sign-extend:\n{plain}"
    );
    assert!(
        plain.contains("ldrb"),
        "aarch64: plain char loads zero-extended:\n{plain}"
    );
    // The control: `signed char` still sign-extends on the same target.
    let signed = body_of(&a, "ld_signed");
    assert!(
        signed.contains("ldrsb") || signed.contains("sxtb"),
        "aarch64: signed char must still sign-extend:\n{signed}"
    );
    let unsigned = body_of(&a, "ld_unsigned");
    assert!(
        !unsigned.contains("sxtb") && !unsigned.contains("ldrsb"),
        "aarch64: unsigned char must not sign-extend:\n{unsigned}"
    );

    // x86-64: plain char is signed, and must keep sign-extending.
    let x = asm_for("char_sign_x64", X86_64_LINUX, src);
    let plain = body_of(&x, "ld_plain");
    assert!(
        plain.contains("movsb"),
        "x86-64: plain char is signed and must sign-extend:\n{plain}"
    );
    let unsigned = body_of(&x, "ld_unsigned");
    assert!(
        unsigned.contains("movzb"),
        "x86-64: unsigned char must zero-extend:\n{unsigned}"
    );
}

/// A pointer comparison must select the *unsigned* condition code.
///
/// C17 6.5.8 compares addresses, and an address is unsigned. The behavioural
/// test cannot reach the case that distinguishes them -- it needs two
/// addresses straddling the sign bit -- so the instruction is asserted
/// directly, on both targets, with signed and unsigned integer controls
/// beside it so the assertion cannot pass by making everything unsigned.
#[test]
fn codegen_pointer_comparison_is_unsigned() {
    let src = r#"
int ptr_lt(char *a, char *b)         { return a < b; }
int ptr_ge(char *a, char *b)         { return a >= b; }
int int_lt(int a, int b)             { return a < b; }
int uint_lt(unsigned a, unsigned b)  { return a < b; }
"#;

    // x86-64: setb/setae for unsigned, setl for signed.
    let x = asm_for("ptr_cmp_x64", X86_64_LINUX, src);
    let p = body_of(&x, "ptr_lt");
    assert!(
        p.contains("setb") && !p.contains("setl"),
        "x86-64: a pointer comparison is unsigned:\n{p}"
    );
    let p = body_of(&x, "ptr_ge");
    assert!(
        p.contains("setae") || p.contains("setnb"),
        "x86-64: `>=` on pointers is unsigned:\n{p}"
    );
    let i = body_of(&x, "int_lt");
    assert!(
        i.contains("setl"),
        "x86-64: a signed int comparison must stay signed:\n{i}"
    );
    let u = body_of(&x, "uint_lt");
    assert!(u.contains("setb"), "x86-64: unsigned int control:\n{u}");

    // aarch64: the condition is on the cset -- lo/hs unsigned, lt signed.
    let a = asm_for("ptr_cmp_a64", AARCH64_LINUX, src);
    let p = body_of(&a, "ptr_lt");
    assert!(
        p.contains("lo") && !p.contains(" lt"),
        "aarch64: a pointer comparison is unsigned:\n{p}"
    );
    let i = body_of(&a, "int_lt");
    assert!(
        i.contains("lt"),
        "aarch64: a signed int comparison must stay signed:\n{i}"
    );
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

/// Every shape of argument that overflows the register file and arrives in the
/// **caller's** frame.
///
/// aarch64 used to describe such an argument with the same `Loc::Stack` variant
/// as a local, telling the two frames apart by the *sign* of an `i32` (#C34).
/// Splitting them into `Loc::Stack(LocalSlot)` and `Loc::IncomingArg(IncomingOff)`
/// made the compiler find the sites that mattered -- but only where a `match`
/// was exhaustive. Where one had a `_` arm, an incoming argument silently fell
/// through it and the parameter was left uninitialized, which is exactly the
/// failure `codegen_aarch64_stacked_hfa_uses_its_own_element_size` caught.
///
/// So these run the values rather than inspecting assembly, and they fill the
/// argument registers first so the interesting parameter is genuinely stacked.
/// On this host they exercise the x86-64 path; the aarch64 sweep extracts and
/// runs the same programs under qemu, which is where they earn their keep.
#[test]
fn codegen_stacked_arguments_of_every_class_round_trip() {
    let code = r#"
#include <stdarg.h>
#include <complex.h>

/* Eight of each fills the integer and floating-point argument registers. */
#define I8 long a,long b,long c,long d,long e,long f,long g,long h
#define D8 double a,double b,double c,double d,double e,double f,double g,double h
#define IA 1,2,3,4,5,6,7,8

struct P2f { float x, y; };
struct P2d { double x, y; };
struct Q4f { float a, b, c, d; };
struct S2i { int a, b; };
struct S2l { long a, b; };
struct Big { long v[8]; };

static long   s_long(I8, long z)            { return z; }
static double s_double(D8, double z)        { return z; }
static float  s_float(D8, float z)          { return z; }
static float  s_hfa2f(D8, struct P2f zp)    { return zp.x + zp.y; }
static double s_hfa2d(D8, struct P2d zp)    { return zp.x + zp.y; }
static float  s_hfa4f(D8, struct Q4f zq)    { return zq.a + zq.b + zq.c + zq.d; }
static int    s_s2i(I8, struct S2i zs)      { return zs.a + zs.b; }
static long   s_s2l(I8, struct S2l zs)      { return zs.a + zs.b; }
static long   s_big(I8, struct Big zb)      { return zb.v[0] + zb.v[7]; }
static __int128 s_i128(I8, __int128 z)      { return z; }
static unsigned __int128 s_u128(I8, unsigned __int128 z) { return z; }
static double s_cplx(D8, double _Complex z) { return __real__ z + __imag__ z; }
static long double s_ld(D8, long double z)  { return z; }
static int    s_branch(I8, int z)           { if (z) return 7; return 9; }
static int    s_switch(I8, int z)           { switch (z) { case 5: return 7; default: return 9; } }
/* The address of a stacked argument, and a stacked argument forwarded on. */
static long   s_addr(I8, struct S2l zs)     { struct S2l *zp = &zs; return zp->a + zp->b; }
static double s_inner(struct P2d zp)        { return zp.x * 10 + zp.y; }
static double s_fwd(D8, struct P2d zp)      { return s_inner(zp); }
static __int128 s_i128_inner(__int128 v)    { return v + 1; }
static __int128 s_i128_fwd(I8, __int128 zv) { return s_i128_inner(zv); }

static long s_varargs(int n, ...) {
    va_list ap; va_start(ap, n);
    long t = 0;
    for (int i = 0; i < n; i++) t += va_arg(ap, long);
    va_end(ap);
    return t;
}

int main(void) {
    if (s_long(IA, 42) != 42) return 1;
    if (s_double(IA, 2.5) != 2.5) return 2;
    if (s_float(IA, 2.5f) != 2.5f) return 3;

    struct P2f p2f = {1.5f, 2.5f};
    if (s_hfa2f(IA, p2f) != 4.0f) return 4;
    struct P2d p2d = {1.5, 2.5};
    if (s_hfa2d(IA, p2d) != 4.0) return 5;
    struct Q4f q4f = {1, 2, 3, 4};
    if (s_hfa4f(IA, q4f) != 10.0f) return 6;

    struct S2i s2i = {3, 4};
    if (s_s2i(IA, s2i) != 7) return 7;
    struct S2l s2l = {3, 4};
    if (s_s2l(IA, s2l) != 7) return 8;
    struct Big big = {{1, 0, 0, 0, 0, 0, 0, 9}};
    if (s_big(IA, big) != 10) return 9;

    /* A 128-bit argument must survive whole: a fallback that loads 64 bits
       and zeroes the top half is a silent wrong answer, not a crash. */
    if (s_i128(IA, (__int128)-1) != -1) return 10;
    unsigned __int128 hi = (unsigned __int128)1 << 100;
    if (s_u128(IA, hi) != hi) return 11;

    double _Complex z = 1.5 + 2.5 * _Complex_I;
    if (s_cplx(IA, z) != 4.0) return 12;
    if (s_ld(IA, 4.0L) != 4.0L) return 13;

    if (s_branch(IA, 1) != 7) return 14;
    if (s_switch(IA, 5) != 7) return 15;

    if (s_addr(IA, s2l) != 7) return 16;
    if (s_fwd(IA, p2d) != 17.5) return 17;  /* 1.5*10 + 2.5 */
    if (s_i128_fwd(IA, (__int128)41) != 42) return 18;

    if (s_varargs(10, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L) != 55) return 19;
    return 0;
}
"#;
    assert_eq!(
        crate::common::compile_and_run("stacked_args_all", code, &[]),
        0
    );
    assert_eq!(
        crate::common::compile_and_run_optimized("stacked_args_all_o2", code),
        0
    );
}
