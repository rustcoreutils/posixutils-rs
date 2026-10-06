//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Cross-target ABI assertions for the types each target treats its own
// way -- long double, binary128, _Float16, complex, __int128 and plain
// char -- made against generated assembly in process; see
// `codegen_cross_abi.rs`. Moved from `tests/codegen/cross_abi_types.rs`.
//

use super::asm_probe::{
    asm_for, asm_for_with, body_of, AARCH64_DARWIN, AARCH64_LINUX, X86_64_LINUX,
};
use super::codegen_cross_abi::dereferences_x0_before_defining_it;
use super::codegen_cross_abi::prologue_frame_store_bytes;

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

/// A conversion between `long double` and `__int128` is a libgcc call named
/// by the floating format, and Darwin's `long double` is binary64: the call
/// is `__fixdfti`/`__floattidf`, never the binary128 `tf` routine, which
/// Apple's runtime does not have (the link failed on macOS). aarch64 Linux,
/// whose `long double` is binary128, keeps `tf`.
#[test]
fn codegen_darwin_long_double_int128_conversions_use_double_routines() {
    let src = r#"
        __int128 s(long double x) { return x; }
        unsigned __int128 u(long double x) { return x; }
        long double fs(__int128 i) { return i; }
        long double fu(unsigned __int128 i) { return i; }
    "#;
    let cases = [
        ("s", "fixdfti", "fixtfti"),
        ("u", "fixunsdfti", "fixunstfti"),
        ("fs", "floattidf", "floattitf"),
        ("fu", "floatuntidf", "floatuntitf"),
    ];
    let darwin = asm_for("ld_i128_darwin", AARCH64_DARWIN, src);
    let linux = asm_for("ld_i128_linux", AARCH64_LINUX, src);
    for (f, df, tf) in cases {
        let body = body_of(&darwin, f);
        assert!(
            body.contains(df) && !body.contains(tf),
            "Darwin {f} must call __{df}:\n{body}"
        );
        let body = body_of(&linux, f);
        assert!(body.contains(tf), "Linux {f} must call __{tf}:\n{body}");
    }
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
   survive it and are spilled to the frame. Kept out of line, since the
   spill is what is under test. */
static __attribute__((noinline)) int eql(const long double *v, long double re, long double im)
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

/// Plain `char` takes the target's signedness, in the front end as well as
/// the back end.
///
/// C17 6.2.5p15 leaves plain `char`'s signedness implementation-defined; the
/// x86-64 psABI makes it signed and AAPCS64 makes it unsigned, and
/// `Target::plain_char` has recorded that all along. Only the two backends'
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
///
/// The rule is per OS as well: Apple arm64 departs from AAPCS64 and makes
/// plain `char` signed, as Apple clang does, so the same source must
/// sign-extend under `aarch64-apple-darwin` -- both for a load and for a
/// `char` parameter widened to `int`.
#[test]
fn codegen_plain_char_follows_the_target_signedness() {
    let src = r#"
int  ld_plain(char *p)          { return *p; }
int  ld_signed(signed char *p)  { return *p; }
int  ld_unsigned(unsigned char *p) { return *p; }
int  arg_plain(char c)          { return c; }
"#;

    // aarch64 Linux: plain char is unsigned, so it must not be sign-extended.
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
    let arg = body_of(&a, "arg_plain");
    assert!(
        !arg.contains("sxtb") && arg.contains("uxtb"),
        "aarch64: a plain char parameter widens by zero extension:\n{arg}"
    );

    // Apple arm64: plain char is signed, so the same load sign-extends.
    let d = asm_for("char_sign_darwin", AARCH64_DARWIN, src);
    let plain = body_of(&d, "ld_plain");
    assert!(
        plain.contains("ldrsb") && !plain.contains("ldrb") && !plain.contains("uxtb"),
        "Apple arm64: plain char is signed and must sign-extend:\n{plain}"
    );
    let arg = body_of(&d, "arg_plain");
    assert!(
        arg.contains("sxtb") && !arg.contains("uxtb"),
        "Apple arm64: a plain char parameter widens by sign extension:\n{arg}"
    );
    // The control: `unsigned char` still zero-extends there.
    let unsigned = body_of(&d, "ld_unsigned");
    assert!(
        !unsigned.contains("sxtb") && !unsigned.contains("ldrsb"),
        "Apple arm64: unsigned char must not sign-extend:\n{unsigned}"
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

/// `-funsigned-char` / `-fsigned-char` override the target's default, the
/// last one winning, and leave the explicit `signed char` alone.
#[test]
fn codegen_plain_char_follows_the_signedness_flag() {
    let src = r#"
int  ld_plain(char *p)          { return *p; }
int  ld_signed(signed char *p)  { return *p; }
"#;

    // x86-64 defaults to signed; -funsigned-char makes plain char zero-extend.
    let x = asm_for_with(
        "char_flag_x64",
        X86_64_LINUX,
        src,
        &["-O", "-funsigned-char"],
    );
    let plain = body_of(&x, "ld_plain");
    assert!(
        plain.contains("movzb") && !plain.contains("movsb"),
        "x86-64 -funsigned-char: plain char must zero-extend:\n{plain}"
    );
    let signed = body_of(&x, "ld_signed");
    assert!(
        signed.contains("movsb"),
        "x86-64 -funsigned-char: signed char must still sign-extend:\n{signed}"
    );

    // aarch64 Linux defaults to unsigned; the last flag (-fno-unsigned-char)
    // makes plain char signed.
    let a = asm_for_with(
        "char_flag_a64",
        AARCH64_LINUX,
        src,
        &["-O", "-funsigned-char", "-fno-unsigned-char"],
    );
    let plain = body_of(&a, "ld_plain");
    assert!(
        plain.contains("ldrsb") || plain.contains("sxtb"),
        "aarch64 -fno-unsigned-char: plain char must sign-extend:\n{plain}"
    );
}

/// Apple arm64 makes narrowing to a small integer type *the ABI's* business,
/// so the narrowed value must be extended by its own signedness.
///
/// AAPCS64 §6.8.2 leaves the bits above a return value narrower than 32 bits
/// unspecified, and the caller re-extends; Apple's ABI does not. There the
/// callee extends a narrow return to 32 bits, the caller extends a narrow
/// argument, and each side is entitled to assume the other did. c17 narrowed
/// with a zero-extending mask whatever the type's signedness:
///
/// ```text
///     movz x0, #195
///     and  w0, w0, #255       ; 195, where Apple clang reads -61
/// ```
///
/// c17 could not catch this against itself, because it re-extends after every
/// call it makes -- `bl _f; sxtb x0, w0` -- so both sides agreed on a value
/// the ABI says is already wrong. It takes Apple clang on the other side, and
/// `plain_char_interoperates_with_apple_clang` is what found it.
///
/// Asserted at `-O0` and `-O2`: constant folding hid the defect at `-O2` for a
/// constant return, but not for a computed one. `unsigned char` and
/// `unsigned short` are the controls, so a fix cannot pass by sign-extending
/// everything, and aarch64 Linux keeps plain `char` unsigned, so a fix cannot
/// pass by making every `char` signed either.
#[test]
fn codegen_darwin_narrowing_extends_by_the_types_signedness() {
    let src = r#"
signed char    ret_sc(int x)   { return (signed char)x; }
unsigned char  ret_uc(int x)   { return (unsigned char)x; }
short          ret_sh(int x)   { return (short)x; }
unsigned short ret_ush(int x)  { return (unsigned short)x; }
char           ret_plain(int x){ return (char)x; }
char           ret_const(void) { return (char)0xC3; }

int take_sc(signed char c);
int pass_sc(int x) { return take_sc((signed char)x); }
"#;

    for opt in ["-O0", "-O2"] {
        let d = asm_for_with("narrow_ext_darwin", AARCH64_DARWIN, src, &[opt]);

        // Every signed narrowing leaves a sign-extended value behind.
        for (func, insn, mask) in [
            ("ret_sc", "sxtb", "#255"),
            ("ret_sh", "sxth", "#65535"),
            ("ret_plain", "sxtb", "#255"),
            ("ret_const", "sxtb", "#255"),
        ] {
            let b = body_of(&d, func);
            assert!(
                b.contains(insn) || !b.contains(mask),
                "Apple arm64 {opt}: {func} must extend its return by sign, \
                 not mask it with {mask}:\n{b}"
            );
            assert!(
                !b.contains(mask),
                "Apple arm64 {opt}: {func} still zero-extends a signed \
                 narrowing:\n{b}"
            );
        }

        // The caller extends a narrow argument for the same reason.
        let b = body_of(&d, "pass_sc");
        assert!(
            b.contains("sxtb") && !b.contains("#255"),
            "Apple arm64 {opt}: a signed char argument is sign-extended by the \
             caller:\n{b}"
        );

        // Controls: the unsigned types must not acquire a sign extension.
        for (func, insn) in [("ret_uc", "sxtb"), ("ret_ush", "sxth")] {
            let b = body_of(&d, func);
            assert!(
                !b.contains(insn),
                "Apple arm64 {opt}: {func} is unsigned and must not \
                 sign-extend:\n{b}"
            );
        }

        // Control: plain `char` is unsigned on aarch64 Linux, so the same
        // source must *not* sign-extend there.
        let a = asm_for_with("narrow_ext_a64", AARCH64_LINUX, src, &[opt]);
        let b = body_of(&a, "ret_plain");
        assert!(
            !b.contains("sxtb"),
            "aarch64 Linux {opt}: plain char is unsigned and must not \
             sign-extend:\n{b}"
        );
    }
}

/// `_Complex __int128` travels by reference on aarch64, in one register.
///
/// It is thirty-two bytes, and AAPCS64 §5.4.2 stage C.12 replaces a composite
/// larger than sixteen with a pointer to a copy -- which `classify_param`
/// already answers, `classify_complex_integer` naming this very case. The
/// back end overrode it: `kind()` reports a complex type's *base* kind, so
/// `kind(t) == TypeKind::Int128` was true here too and took the arm meant for
/// a bare `__int128`, two consecutive even-aligned X registers.
///
/// ```text
///     int g(long a, _Complex __int128 z, long b, long c)
///     was  a->x0  z->x2,x3  b->x4  c->x5
///     is   a->x0  z->x1     b->x2  c->x3
/// ```
///
/// Caller and callee were wrong in the same direction, so no program c17
/// compiles on both sides can see it; the shapes are asserted instead. The
/// thirty-two byte struct beside each case is the control -- it classifies
/// `Indirect` too, and was always right, so the fix cannot pass by moving
/// both.
#[test]
fn codegen_aarch64_complex_int128_argument_travels_by_reference() {
    let src = r#"
struct Big { long a, b, c, d; };
int g_struct(long a, struct Big z, long b, long c);
int g_cplx(long a, _Complex __int128 z, long b, long c);
int c_struct(struct Big *p) { return g_struct(1111, *p, 2222, 3333); }
int c_cplx(_Complex __int128 *p) { return g_cplx(1111, *p, 2222, 3333); }
long k_struct(long a, struct Big z, long b, long c) { return a + b + c; }
long k_cplx(long a, _Complex __int128 z, long b, long c) { return a + b + c; }
"#;
    for triple in [AARCH64_LINUX, AARCH64_DARWIN] {
        let asm = asm_for_with("cplx_i128_arg", triple, src, &["-O1"]);

        // Caller: the last argument lands in x3, because the one before it
        // took a single register. The immediate is materialized either
        // straight into its argument register or into a scratch first, in
        // which case follow that one move.
        let last_arg_register = |func: &str| -> String {
            let body = body_of(&asm, func);
            let scratch = body
                .lines()
                .map(str::trim)
                .find_map(|l| l.strip_prefix("movz ")?.split_once(", #3333"))
                .map(|(r, _)| r.to_string())
                .unwrap_or_else(|| panic!("{func}: nothing materializes 3333:\n{body}"));
            let is_arg_reg = |r: &str| {
                r.strip_prefix('x')
                    .and_then(|n| n.parse::<u32>().ok())
                    .is_some_and(|n| n < 8)
            };
            if is_arg_reg(&scratch) {
                return scratch;
            }
            body.lines()
                .map(str::trim)
                .find_map(|l| {
                    let (dst, src) = l.strip_prefix("mov ")?.split_once(", ")?;
                    (src == scratch).then(|| dst.to_string())
                })
                .unwrap_or_else(|| panic!("{func}: {scratch} never reaches an argument:\n{body}"))
        };
        assert_eq!(
            last_arg_register("c_struct"),
            "x3",
            "{triple}: the control's last argument is x3"
        );
        assert_eq!(
            last_arg_register("c_cplx"),
            "x3",
            "{triple}: a complex __int128 takes one register, so the last \
             argument is x3 and not x5:\n{}",
            body_of(&asm, "c_cplx")
        );

        // Callee: a parameter passed by reference is one register, so nothing
        // spills a consecutive pair for it.
        for func in ["k_struct", "k_cplx"] {
            let body = body_of(&asm, func);
            assert!(
                !body.contains("stp x2, x3"),
                "{triple}: {func} receives a pointer in one register, so no \
                 pair is spilled for it:\n{body}"
            );
        }
    }
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

/// The same, for an all-SSE composite whose last eightbyte is a width no
/// floating-point store has.
///
/// `struct { float a, b, c; _Float16 d; }` is fourteen bytes when packed, and
/// both of its eightbytes are SSE class -- the second holds six bytes. There
/// is no six-byte SSE store, so the register has to go through a general one;
/// rounding the width up instead wrote two bytes past the object.
#[test]
fn codegen_a_packed_two_sse_parameter_stores_only_its_own_bytes() {
    let src = "\
struct __attribute__((packed)) P6 { float a, b, c; _Float16 d; };
float probe(struct P6 p) { return p.a + p.b + p.c; }
";
    let asm = asm_for_with("packed_two_sse", X86_64_LINUX, src, &["-O0"]);
    let body = body_of(&asm, "probe");
    assert_eq!(
        prologue_frame_store_bytes(body),
        14,
        "a fourteen-byte two-SSE parameter is 8 + 4 + 2, and nothing more:\n{body}"
    );
}
