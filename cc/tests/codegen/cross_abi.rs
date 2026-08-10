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

use super::asm_probe::{asm_for, body_of};

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
            !body.contains("[x0]") && !body.contains("[x0,"),
            "{}: a register-passed complex must not be reloaded through X0 — \
             that register holds no argument here:\n{}",
            func,
            body
        );
    }
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
