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

use super::asm_probe::{asm_for, body_of, X86_64_LINUX};

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
    assert!(
        asm.contains("subq $16, %rsp"),
        "a stack-passed binary128 needs two eightbytes:\n{asm}"
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
    let i = body_of(&asm, "take_i");
    assert!(
        i.contains("movq (%rdi)"),
        "an integer pair still arrives as a pointer:\n{i}"
    );
}
