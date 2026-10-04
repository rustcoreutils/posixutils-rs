//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Cross-target ABI assertions for variadic calls and stack-passed
// arguments, made against generated assembly in process; see
// `codegen_cross_abi.rs`. Moved from `tests/codegen/cross_abi_varargs.rs`.
//

use super::asm_probe::{
    asm_for, asm_for_with, body_of, AARCH64_DARWIN, AARCH64_LINUX, X86_64_LINUX,
};

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
    // Whichever register carries the fourth argument into `x3`, it must hold
    // what was loaded out of the frame, not a frame address.
    let carrier = call
        .lines()
        .map(str::trim)
        .filter_map(|l| l.strip_prefix("mov x3, "))
        .next_back()
        .unwrap_or("x3");
    assert!(
        call.contains(&format!("ldr {carrier}, [x29,")),
        "Darwin's va_list is a pointer; its value must be loaded, not its \
         address taken:\n{d}"
    );
    assert!(
        !call.contains(&format!("add {carrier}, x29,")),
        "passing the address of a Darwin va_list gives vsnprintf a pointer \
         to a pointer:\n{d}"
    );

    // Linux: the 32-byte record goes by reference, so the address is right.
    let linux = asm_for("va_list_arg_linux", "aarch64-unknown-linux-gnu", src);
    let l = body_of(&linux, "fmt");
    let call = l.split("bl vsnprintf").next().unwrap_or(l);
    let carrier = call
        .lines()
        .map(str::trim)
        .filter_map(|l| l.strip_prefix("mov x3, "))
        .next_back()
        .unwrap_or("x3");
    assert!(
        call.contains(&format!("add {carrier}, x29,")),
        "AAPCS64 passes the 32-byte va_list record by reference:\n{l}"
    );
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

/// `va_arg` of an `__int128` copies **both** eightbytes on Darwin too.
///
/// Darwin has its own `va_arg` emitter -- every variadic argument is already
/// on the stack, so there is no register save area to gather from -- and the
/// fix that gave AAPCS64 a 128-bit arm did not reach it. Its scalar path sizes
/// the move with `OperandSize::from_bits`, which saturates at 64, so the low
/// half was copied and the high half was left as whatever the destination slot
/// held. The *advance* was already 16, which is why the next `va_arg` was
/// correct and only the value was wrong.
///
/// Asserted on assembly because it cannot be executed here: there is no macOS
/// runner and qemu cannot run Mach-O. Two loads from the argument pointer, at
/// 0 and at 8, are the property; the linux-aarch64 half of the same rule is
/// covered behaviourally by `codegen_va_arg_int128_reads_both_eightbytes`.
#[test]
fn codegen_darwin_va_arg_int128_copies_both_eightbytes() {
    let src = r#"
#include <stdarg.h>
__int128 wide(int x, ...)
{
    __int128 r;
    va_list ap;
    va_start(ap, x);
    while (x--) va_arg(ap, int);
    r = va_arg(ap, __int128);
    va_end(ap);
    return r;
}
"#;
    let asm = asm_for("darwin_va_int128", "aarch64-apple-darwin", src);
    let body = body_of(&asm, "wide");

    // The pointer is loaded into some register, then read at +0 and +8. Find
    // the register the second load uses and require a matching first load, so
    // this cannot pass on a single load plus unrelated traffic.
    let hi = body
        .lines()
        .map(str::trim)
        .find(|l| l.starts_with("ldr x") && l.ends_with(", #8]"))
        .unwrap_or_else(|| panic!("no high-half load of the __int128:\n{body}"));
    let base = hi
        .rsplit_once(", [")
        .and_then(|(_, rest)| rest.split(',').next())
        .unwrap_or_else(|| panic!("unparsable load `{hi}`:\n{body}"));
    assert!(
        body.lines()
            .map(str::trim)
            .any(|l| l.starts_with("ldr x") && l.ends_with(&format!(", [{base}]"))),
        "the low half must be loaded from {base} as well as the high half \
         (`{hi}`):\n{body}"
    );
}

/// A zero-sized argument takes no register on aarch64, in the caller.
///
/// AAPCS64 gives it no class, and `va_arg` skips it. The caller did not, in
/// *both* of its argument loops, so it charged a general register the callee
/// never reads and everything after shifted by one. The two errors cancelled
/// exactly, so c17 agreed with itself and disagreed with gcc -- a divergence
/// only visible across a translation-unit boundary, which no behavioural test
/// in this suite can reach. Hence an assembly assertion: the argument after
/// the zero-sized one must be in `w1`, the register it would have been pushed
/// out of.
#[test]
fn codegen_aarch64_zero_sized_argument_takes_no_register() {
    let src = r#"
struct Z { char x[0]; };
int callee(int n, int a, int b);
int caller(void)
{
    struct Z z;
    (void)z;
    return callee(1, 1234, 5678);
}
int variadic(int n, ...);
int caller_va(void)
{
    struct Z z;
    return variadic(1, z, 1234);
}
int callee_side(int a, struct Z z, int b)
{
    (void)z;
    return a * 10 + b;
}
"#;
    let asm = asm_for("aarch64_zero_sized_arg", AARCH64_LINUX, src);

    // The control: with no zero-sized argument, 1234 is the second argument
    // and so lives in w1.
    let plain = body_of(&asm, "caller");
    assert!(
        plain.contains("#1234"),
        "the control call must materialise 1234:\n{plain}"
    );

    // With the zero-sized argument first, 1234 is still the *first* argument
    // that is actually passed after `n`, so it must still be w1 -- not w2.
    let va = body_of(&asm, "caller_va");
    let w1 = va.contains("w1, #1234") || va.contains("w1, #1234\n");
    assert!(
        w1,
        "1234 follows a zero-sized argument, which is passed in no register \
         at all, so it must go in w1:\n{va}"
    );
    assert!(
        !va.contains("w2, #1234"),
        "the zero-sized argument must not push 1234 into w2:\n{va}"
    );

    // The callee side of the same rule. `b` follows a zero-sized parameter, so
    // it arrives in w1; the allocator's catch-all arm charged the zero-sized
    // one a register and read `b` out of w2 instead. Asserting on which
    // register a value is *read from* is what distinguishes the two, since w2
    // is also used as scratch either way.
    let callee = body_of(&asm, "callee_side");
    assert!(
        !callee.contains("mov w1, w2"),
        "a zero-sized parameter takes no register, so `b` arrives in w1 and \
         must not be read out of w2:\n{callee}"
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

/// The same argument, once it has run out of registers: its stack slot is
/// eight bytes, not sixteen.
///
/// `StackedArgs::slot` gives an `Indirect` argument an eight-byte slot,
/// because what travels is the pointer. The stacked-store path asked
/// `kind(t) == TypeKind::Int128` as well and wrote both halves of a
/// sixteen-byte value into it, over whatever the next argument had been put
/// in.
#[test]
fn codegen_aarch64_stacked_complex_int128_writes_one_slot() {
    let src = r#"
int g(long a0, long a1, long a2, long a3, long a4, long a5, long a6, long a7,
      _Complex __int128 z, long tail);
int call(_Complex __int128 *p) {
    return g(0, 1, 2, 3, 4, 5, 6, 7, *p, 4242);
}
"#;
    for triple in [AARCH64_LINUX, AARCH64_DARWIN] {
        let asm = asm_for_with("cplx_i128_stacked", triple, src, &["-O1"]);
        let body = body_of(&asm, "call");
        // The outgoing area is written with `str`, one slot at a time. A
        // `stp` into it is the sixteen-byte write that overruns the slot.
        // The frame record's own `stp x29, x30, [sp, #-N]!` is pre-indexed
        // and is not a store into that area.
        assert!(
            !body.lines().any(|l| {
                let l = l.trim();
                l.starts_with("stp ") && l.contains("[sp") && !l.contains("]!")
            }),
            "{triple}: a by-reference argument fills one eight-byte slot, so \
             no pair is stored into the outgoing area:\n{body}"
        );
    }
}

/// A stacked argument is placed from the argument area's base, on both targets.
///
/// The behavioural test in `codegen::misc` runs on the host only, so it cannot
/// see the other architecture's layout -- and both implementations of
/// `IncomingOff::take` had the same defect: the rounding was applied to the
/// frame displacement, which already carries the saved frame pointer and
/// return address, rather than to the offset within the argument area. An
/// over-aligned argument arriving first went one whole alignment unit past
/// where the caller had written it.
///
/// The two targets need different sources to reach the case at all. On x86-64
/// a 32-byte composite is MEMORY class and lands on the stack once the six
/// integer registers are spent. On aarch64 that same struct is a homogeneous
/// floating-point aggregate and travels in `d0`-`d3`, so it is only stacked
/// once the eight FP registers are spent -- a signature of six ints stacks
/// nothing there, and asserting against it matched the frame-base register
/// spill at `[x29, #16]` and passed while the bug was live.
#[test]
fn codegen_stacked_arg_starts_at_the_argument_area_base() {
    let src = r#"
        struct A32 { _Alignas(32) double v[4]; };
        /* x86-64: MEMORY class, stacked after the six GP registers. */
        double after_ints(int a, int b, int c, int d, int e, int f,
                          struct A32 x) {
            return x.v[0];
        }
        /* aarch64: an HFA, stacked only after the eight FP registers. */
        double after_fps(double a, double b, double c, double d,
                         double e, double f, double g, double h,
                         struct A32 x) {
            return x.v[0];
        }
    "#;

    // x86-64 addresses incoming arguments from %rbp, and the first sits just
    // above the saved %rbp and the return address.
    let full = asm_for("stacked_arg_base", X86_64_LINUX, src);
    let asm = body_of(&full, "after_ints");
    assert!(
        asm.contains("16(%rbp)"),
        "x86-64: the first stacked argument must be read from 16(%rbp), not \
         padded past it:\n{asm}"
    );

    // aarch64 has no return address on the stack, so the argument area begins
    // at the top of the callee's own frame. Asserted as agreement with the
    // frame the prologue actually allocates rather than as a literal offset,
    // which would only re-encode whatever the allocator happened to pick.
    let full = asm_for("stacked_arg_base", AARCH64_LINUX, src);
    let asm = body_of(&full, "after_fps");
    let frame = asm
        .lines()
        .find_map(|l| {
            l.split("[sp, #-")
                .nth(1)
                .and_then(|r| r.split(']').next())
                .and_then(|n| n.parse::<i32>().ok())
        })
        .unwrap_or_else(|| panic!("no frame allocation found:\n{asm}"));
    let base = format!("[x29, #{frame}]");
    assert!(
        asm.contains(&base),
        "aarch64: the first stacked argument must be read from the argument \
         area base {base} (the frame's own size), not padded past it:\n{asm}"
    );
}

/// A Darwin variadic `__int128` argument occupies **sixteen** bytes of the
/// caller's outgoing area, on a sixteen-byte boundary.
///
/// The reading side got its second eightbyte first; the writing side kept
/// giving every non-aggregate scalar exactly one eight-byte granule and never
/// aligning a slot, so the caller wrote half the value and the next argument
/// landed on top of the other half. Both sides were wrong the same way, so no
/// c17-only program could see it -- only a clang-compiled callee, which is how
/// it reached macOS CI. LLVM's Darwin vararg convention stacks an `i128` as
/// sixteen bytes, sixteen-aligned, and clang's `va_arg` rounds the cursor to
/// match.
///
/// Asserted on assembly because it cannot be executed here: there is no macOS
/// runner and qemu cannot run Mach-O.
#[test]
fn codegen_darwin_variadic_int128_takes_two_granules() {
    let src = r#"
__int128 wide(int n, ...);

/* One leading `int` leaves the cursor at 8, so a 16-aligned slot has to skip
   to 16 -- with no leading argument at all, 0 is already aligned and the test
   would pass without any rounding. */
__int128 call_odd(__int128 u) { return wide(1, 0, u); }
__int128 call_even(__int128 u) { return wide(0, u); }
"#;
    let asm = asm_for("darwin_variadic_int128", "aarch64-apple-darwin", src);

    for (func, want) in [("call_odd", "[sp, #16]"), ("call_even", "[sp]")] {
        let body = body_of(&asm, func);
        let stores: Vec<&str> = body
            .lines()
            .map(str::trim)
            .filter(|l| l.starts_with("stp x") && l.contains("[sp"))
            .collect();
        assert!(
            stores.iter().any(|l| l.ends_with(want)),
            "{func}: the __int128 must be stored as a pair at {want}, so that \
             both eightbytes reach the callee and the slot is 16-aligned; \
             found {stores:?}\n{body}"
        );
    }

    // The area has to be big enough for the sixteen-byte slot plus the
    // rounding, not one granule per argument.
    let odd = body_of(&asm, "call_odd");
    let sub = odd
        .lines()
        .map(str::trim)
        .find(|l| l.starts_with("sub sp, sp, #"))
        .unwrap_or_else(|| panic!("call_odd reserves no outgoing area:\n{odd}"));
    let bytes: i32 = sub
        .rsplit('#')
        .next()
        .and_then(|n| n.trim().parse().ok())
        .unwrap_or_else(|| panic!("unparsable reservation `{sub}`"));
    assert!(
        bytes >= 32,
        "call_odd reserved {bytes} bytes: an `int` then a 16-aligned \
         `__int128` needs 8 + 8 of padding + 16\n{odd}"
    );
}

/// A Darwin variadic argument that wants more than sixteen bytes of alignment
/// makes the caller realign its outgoing area.
///
/// clang's `va_arg` rounds the cursor up to the type's own alignment, so the
/// argument has to be at an address that is actually that aligned -- and
/// `%sp` is guaranteed only to sixteen, which makes rounding a static offset
/// meaningless on its own. The caller therefore rounds `%sp` down and stashes
/// the old value above the arguments, because the amount the rounding
/// consumed is not known until it runs.
///
/// clang's own caller does *not* do this: it stacks an over-aligned aggregate
/// a legalized element at a time in eight-byte granules, disagreeing with its
/// own `va_arg`. c17 follows `va_arg`. See the divergence recorded in
/// `cc/DECISIONS.md`.
///
/// Asserted on assembly because it cannot be executed here: there is no macOS
/// runner and qemu cannot run Mach-O.
#[test]
fn codegen_darwin_variadic_over_aligned_realigns_the_outgoing_area() {
    let src = r#"
struct __attribute__((aligned (32))) A32 { double a, b, c, d; };
int variadic(int n, ...);

/* Nine leading doubles leave the cursor at 72, which is neither 16- nor
   32-aligned, so the rounding is observable. */
int call(struct A32 s)
{
    return variadic(9, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, s, 9);
}
"#;
    let asm = asm_for("darwin_va_overalign", "aarch64-apple-darwin", src);
    let body = body_of(&asm, "call");
    let lines: Vec<&str> = body.lines().map(str::trim).collect();

    assert!(
        lines.iter().any(|l| l.starts_with("and x17, x17, x")),
        "the outgoing area's base must be rounded down to the argument's \
         alignment; %sp only guarantees 16:\n{body}"
    );
    // The old %sp is saved and restored, because the rounding consumed an
    // amount no `add` can undo.
    let save = lines
        .iter()
        .find(|l| l.starts_with("str x16, [sp, #"))
        .unwrap_or_else(|| panic!("the pre-call %sp is never saved:\n{body}"));
    let slot = save
        .rsplit_once("#")
        .and_then(|(_, rest)| rest.trim_end_matches(']').parse::<i32>().ok())
        .unwrap_or_else(|| panic!("unparsable save `{save}`"));
    assert!(
        lines
            .iter()
            .any(|l| *l == format!("ldr x16, [sp, #{slot}]")),
        "the saved %sp at {slot} is never read back:\n{body}"
    );
    assert!(
        lines.contains(&"add sp, x16, #0"),
        "%sp is never restored from the saved value:\n{body}"
    );

    // The struct itself has to land on a 32-byte boundary of that base: nine
    // eight-byte slots end at 72, so the next multiple of 32 is 96.
    assert!(
        lines.iter().any(|l| l.contains("[sp, #96]")),
        "the 32-aligned argument must start at 96, not at the next granule:\n{body}"
    );
}

/// Darwin's `va_arg` reads a whole argument and advances by its whole slot.
///
/// Darwin has its own emitters on both sides of a variadic call, and three
/// separate fixes in this area reached the AAPCS64 one and not this one --
/// each time caller and reader stayed wrong together, so every c17-only
/// program agreed with itself and only macOS CI disagreed. There is no macOS
/// runner here and qemu cannot run Mach-O, so this is the check that runs
/// locally.
///
/// Two numbers per type, and both have been wrong: the bytes read from the
/// cursor (an `__int128` had one eightbyte copied and the other left as
/// whatever the destination held) and the advance, which is the slot
/// `darwin_va_slot` hands the caller.
///
/// `BIG24` is the row that is not its own size either way: a non-homogeneous
/// aggregate past sixteen bytes travels as a pointer, so one eightbyte is
/// read from the cursor and the object comes through it.
#[test]
fn codegen_darwin_va_arg_reads_a_whole_argument() {
    let src = r#"
#include <stdarg.h>
typedef struct { long long a, b; } G16;
typedef struct { double a, b, c, d; } H32;
typedef struct { long long a, b, c; } BIG24;
#define MK(name, T) T name(int n, ...) \
    { va_list ap; va_start(ap, n); T v = va_arg(ap, T); va_end(ap); return v; }
MK(f_long, long)
MK(f_dbl, double)
MK(f_i128, __int128)
MK(f_g16, G16)
MK(f_h32, H32)
MK(f_big, BIG24)
"#;
    let asm = asm_for("darwin_va_slots", "aarch64-apple-darwin", src);

    for (func, read, advance, why) in [
        ("f_long", 8, 8, "a long is one granule"),
        ("f_dbl", 8, 8, "a double is one granule"),
        (
            "f_i128",
            16,
            16,
            "an __int128 is two eightbytes and two granules, not one of each",
        ),
        ("f_g16", 16, 16, "a sixteen-byte composite is its own size"),
        ("f_h32", 32, 32, "a thirty-two-byte HFA is its own size"),
        (
            "f_big",
            8,
            8,
            "a non-HFA past sixteen bytes travels as a pointer",
        ),
    ] {
        let body = body_of(&asm, func);
        let lines: Vec<&str> = body.lines().map(str::trim).collect();

        // The cursor is the register that advances into itself; everything
        // else here computes a frame address into a different one.
        let mut cursor = None;
        let mut advances = Vec::new();
        for l in &lines {
            let Some(rest) = l.strip_prefix("add ") else {
                continue;
            };
            let Some((dst, rest)) = rest.split_once(", ") else {
                continue;
            };
            let Some((src, imm)) = rest.split_once(", #") else {
                continue;
            };
            if dst == src {
                if let Ok(n) = imm.parse::<i64>() {
                    cursor = Some(dst.to_string());
                    advances.push(n);
                }
            }
        }
        let cursor =
            cursor.unwrap_or_else(|| panic!("{func}: va_arg never advances a cursor:\n{body}"));
        assert!(
            advances.contains(&advance),
            "{func}: {why}, so the cursor must advance by {advance}; \
             found {advances:?}\n{body}"
        );

        // How far past the cursor the argument is read. `emit_va_arg_bytes`
        // walks it in descending chunk sizes, so the last offset plus its
        // width is the whole of what was copied.
        let mut read_end = 0i64;
        for l in &lines {
            let (op, rest) = match (l.strip_prefix("ldr "), l.strip_prefix("ldp ")) {
                (Some(r), _) => ("ldr", r),
                (_, Some(r)) => ("ldp", r),
                _ => continue,
            };
            let Some((regs, addr)) = rest.rsplit_once(", [") else {
                continue;
            };
            let addr = addr.trim_end_matches(']');
            let (base, off) = match addr.split_once(", #") {
                Some((b, o)) => (b, o.parse::<i64>().unwrap_or(0)),
                None => (addr, 0),
            };
            if base != cursor {
                continue;
            }
            // A `w` destination moves four bytes, an `x` one eight, and `ldp`
            // moves two of them.
            let width = if regs.trim_start().starts_with('w') {
                4
            } else {
                8
            };
            let width = if op == "ldp" { width * 2 } else { width };
            read_end = read_end.max(off + width);
        }
        assert_eq!(
            read_end, read,
            "{func}: {why}, so {read} bytes must be read from the cursor \
             {cursor}, not {read_end}\n{body}"
        );
    }
}

/// The cursor advance of each Darwin `va_arg` in `body`, in order: every
/// `add R, R, #N` that is stored straight back through the `va_list`.
fn darwin_va_advances(body: &str) -> Vec<i64> {
    let lines: Vec<&str> = body.lines().map(str::trim).collect();
    lines
        .windows(2)
        .filter_map(|w| {
            let (dst, rest) = w[0].strip_prefix("add ")?.split_once(", ")?;
            let (src, imm) = rest.split_once(", #")?;
            (dst == src && w[1].starts_with(&format!("str {dst}, [")))
                .then(|| imm.parse().ok())
                .flatten()
        })
        .collect()
}

/// Darwin `va_arg` of a complex type reads the value as the object it is on
/// the stack -- both halves, contiguous at the cursor -- and steps over its
/// own size rounded to eight.
///
/// It took each complex type for a scalar of its base: one half was read,
/// the cursor moved by one slot, and the result was written into a register
/// that every consumer then dereferenced as the address of the two halves.
/// Asserted on assembly because there is no macOS runner and qemu cannot run
/// Mach-O; the behaviour is pinned on the other targets by
/// `c99_complex_va_arg_interoperates_with_gcc_aarch64`.
#[test]
fn codegen_darwin_va_arg_complex_reads_the_object() {
    let src = r#"
typedef __builtin_va_list va_list;
double re_d(int n, ...) {
    va_list ap; __builtin_va_start(ap, n);
    double _Complex z = __builtin_va_arg(ap, double _Complex);
    int after = __builtin_va_arg(ap, int);
    __builtin_va_end(ap);
    return __imag__ z + after;
}
float re_f(int n, ...) {
    va_list ap; __builtin_va_start(ap, n);
    float _Complex z = __builtin_va_arg(ap, float _Complex);
    int after = __builtin_va_arg(ap, int);
    __builtin_va_end(ap);
    return __imag__ z + after;
}
_Float16 re_h(int n, ...) {
    va_list ap; __builtin_va_start(ap, n);
    _Float16 _Complex z = __builtin_va_arg(ap, _Float16 _Complex);
    int after = __builtin_va_arg(ap, int);
    __builtin_va_end(ap);
    return __imag__ z + after;
}
"#;
    let asm = asm_for("darwin_va_complex", "aarch64-apple-darwin", src);
    // Apple's `long double` is `double`, so there is no wider case to check.
    for (func, first) in [("re_d", 16), ("re_f", 8), ("re_h", 8)] {
        let body = body_of(&asm, func);
        assert_eq!(
            darwin_va_advances(body),
            [first, 8],
            "{func}: the complex value takes its own size rounded to eight, \
             then the `int` one slot:\n{body}"
        );
    }
    // Both halves of the double pair are read, from +0 and +8 of the cursor.
    let body = body_of(&asm, "re_d");
    assert!(
        body.lines()
            .map(str::trim)
            .any(|l| l.starts_with("ldr x") && l.ends_with(", #8]")),
        "re_d: the imaginary half at +8 is never read:\n{body}"
    );
}

/// A Darwin variadic call stacks a complex argument's *value*, and passes a
/// named complex argument in `d0`/`d1` as any other call does.
///
/// Its argument walk was its own and knew scalars and wide structs only. A
/// variadic `_Complex` -- whose pseudo is an address at every size -- was
/// stored as that pointer, zero-extended into a sixteen-byte pair, and a
/// named complex or HFA argument went to `x0` as the address.
#[test]
fn codegen_darwin_variadic_call_passes_complex_values() {
    let src = r#"
struct P { double a, b; };
int check(int n, ...);
int nf(double _Complex z, ...);
int ns(struct P p, ...);
int var_d(double _Complex x) { return check(1, x); }
int var_f(float _Complex x) { return check(1, x); }
int named_d(double _Complex x) { return nf(x, 1); }
int named_s(struct P p) { return ns(p, 1); }
"#;
    let asm = asm_for("darwin_va_complex_call", "aarch64-apple-darwin", src);
    let lines = |func: &str| -> Vec<String> {
        body_of(&asm, func)
            .lines()
            .map(|l| l.trim().to_string())
            .collect()
    };

    let d = lines("var_d");
    for at in ["[sp]", "[sp, #8]"] {
        assert!(
            d.iter().any(|l| l.starts_with("str d") && l.ends_with(at)),
            "var_d: a double half must be stored at {at}:\n{}",
            d.join("\n")
        );
    }
    let f = lines("var_f");
    for at in ["[sp]", "[sp, #4]"] {
        assert!(
            f.iter().any(|l| l.starts_with("str s") && l.ends_with(at)),
            "var_f: a float half must be stored at {at}:\n{}",
            f.join("\n")
        );
    }

    for func in ["named_d", "named_s"] {
        let body = lines(func);
        for v in ["d0", "d1"] {
            assert!(
                body.iter().any(|l| l.starts_with(&format!("ldr {v}, ["))),
                "{func}: the named pair must be loaded into {v}:\n{}",
                body.join("\n")
            );
        }
        assert!(
            !body.iter().any(|l| l.starts_with("mov x0, x")),
            "{func}: the pair's address must not be passed in x0:\n{}",
            body.join("\n")
        );
    }
}

/// The bulk `va_arg` copy still moves the ragged tail.
///
/// A bound is only half of the rule: `rep movsq` and the counted loop both
/// move whole eightbytes (sixteen bytes, for the loop), and 4093 bytes is
/// neither. A copy that stopped at the last whole unit would leave the last
/// bytes of the aggregate unwritten, which no instruction count can show.
#[test]
fn codegen_a_bulk_va_arg_copy_moves_the_ragged_tail() {
    let src = "\
#include <stdarg.h>
struct Odd { unsigned char c[4093]; };
void sink(struct Odd *);
void probe(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    struct Odd o = va_arg(ap, struct Odd);
    sink(&o);
    va_end(ap);
}
";
    for (triple, byte_move) in [(X86_64_LINUX, "movb"), (AARCH64_LINUX, "ldrb")] {
        let asm = asm_for_with("va_arg_odd", triple, src, &["-O2"]);
        let body = body_of(&asm, "probe");
        assert!(
            body.contains(byte_move),
            "4093 bytes ends on an odd byte, so the tail needs a byte move on \
             {triple}:\n{body}"
        );
    }
}
