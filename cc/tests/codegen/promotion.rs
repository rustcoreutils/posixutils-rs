//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Promotion of locals out of memory, and what it must not break.
//
// `ir/ssa.rs` used to decline any local whose uses all sat in one basic
// block, which is the common case and the cheap one. Every scalar in
// straight-line code therefore kept a stack slot and round-tripped through
// memory, and constant folding could not cross a statement boundary because
// the value went through a `Load`.
//
// Frame size is invisible to a program's exit status -- a promoted and an
// unpromoted local compute the same answer -- so the size assertions here go
// through `asm_probe::frame_size` rather than through a return code. The
// behavioral tests alongside them cover the cases where promotion would
// change the answer, which is to say the cases where it would be a bug.

use crate::codegen::asm_probe::{
    asm_for_with, body_of, count_in_body, frame_size, AARCH64_LINUX, X86_64_LINUX,
};
use crate::common::{compile_and_run, compile_and_run_aarch64};

/// Ten address-free int locals in straight-line code.
///
/// Each unpromoted local costs 8 bytes of frame (slots are `size.max(8)` and
/// never reused), so before promotion this reserved 112 bytes.
#[test]
fn codegen_straight_line_locals_leave_memory() {
    let src = r#"
int straight(int x) {
    int a = x + 1;  int b = a * 2;  int c = b - 3;
    int d = c + 4;  int e = d * 5;  int f = e - 6;
    int g = f + 7;  int h = g * 8;  int i = h - 9;
    int j = i + 10;
    return j;
}
"#;
    let asm = asm_for_with("straight_line_locals", X86_64_LINUX, src, &["-O2"]);
    let frame = frame_size(&asm, "straight").unwrap_or(0);
    assert!(
        frame <= 16,
        "ten straight-line int locals must not each keep a stack slot, \
         got a {frame}-byte frame:\n{asm}"
    );
}

/// A function with no locals at all still framed its incoming parameter,
/// because the parameter's own spill slot was itself a single-block local.
#[test]
fn codegen_parameter_only_function_needs_no_frame() {
    let src = "int nolocal(int x) { return x + 1; }\n";
    let asm = asm_for_with("parameter_only", X86_64_LINUX, src, &["-O2"]);
    let frame = frame_size(&asm, "nolocal").unwrap_or(0);
    assert!(
        frame == 0,
        "a function whose only value is its parameter needs no frame, \
         got {frame} bytes:\n{asm}"
    );
}

/// Constant folding has to survive the hop from one statement to the next.
///
/// Promotion alone is not enough: it turns the `Load` into a `Copy`, and
/// `instcombine` reads constants off the pseudo's kind, which a `Copy`
/// target does not have. Both halves are needed for this to reach `21`.
#[test]
fn codegen_constants_fold_across_statements() {
    let src = "int trivial(void) { int a = 2 + 3; int b = a * 4; return b + 1; }\n";
    let asm = asm_for_with("fold_across_statements", X86_64_LINUX, src, &["-O2"]);
    assert_eq!(
        count_in_body(&asm, "trivial", "imul"),
        0,
        "a chain of integer constants must fold, not multiply at run time:\n{asm}"
    );
    assert!(
        asm.contains("$21"),
        "2+3 then *4 then +1 is 21, and it should appear as an immediate:\n{asm}"
    );
}

/// `_Complex` is a scalar by `is_scalar`, but its halves are stored
/// separately at offsets 0 and 8 and read back by a single 128-bit load at
/// offset 0. Promoting it on the strength of "scalar, address not taken"
/// forwards the *last* store -- the imaginary half -- into that load, and
/// `if (z)` becomes permanently false.
///
/// This is the shape that makes the width guard in `analyze_variable` load
/// bearing rather than defensive.
#[test]
fn codegen_complex_local_is_not_forwarded_by_half() {
    // `if (z)` on a complex value currently tests only the real part, so the
    // nonzero-imaginary case is not asserted here -- that is a separate,
    // pre-existing defect and folding it into this test would hide it.
    let src = r#"
int nonzero_real(void) { double _Complex z = 3.0; if (z) return 1; return 0; }
int zero_both(void)    { double _Complex z = 0.0; if (z) return 1; return 0; }
int sum_halves(void) {
    double _Complex z = 3.0;
    /* Reads both halves back out of the same slot the two stores wrote. */
    return (int)(__real__ z) * 10 + (int)(__imag__ z);
}
int main(void) {
    if (nonzero_real() != 1) return 1;
    if (zero_both()   != 0) return 2;
    if (sum_halves()  != 30) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("complex_local_halves", src, &[]), 0);
}

/// Folding through a `Copy` chain must not widen the existing looseness
/// about constants not being truncated to their operand width.
///
/// `0x40000000 * 4` overflows `int`. The product is held as a full-width
/// `i128`, so a chained fold of `y / 2` would answer `INT_MIN` where the
/// truncated operand gives `0`. gcc gives 0.
#[test]
fn codegen_folded_constant_is_not_reused_beyond_its_width() {
    let src = r#"
int width(void) { int y = 0x40000000 * 4; int z = y / 2; return z; }
int main(void) { return width() == 0 ? 0 : 1; }
"#;
    assert_eq!(compile_and_run("fold_width_guard", src, &[]), 0);
}

/// A local declared inside a loop body has all its uses in one block, but
/// reading it before writing it reads the previous iteration -- so it needs
/// a phi at the loop header, not a linear forward.
#[test]
fn codegen_loop_body_local_reads_previous_iteration() {
    let src = r#"
int carry(int n) {
    int acc = 0;
    int seed = 7;
    for (int i = 0; i < n; i++) {
        int t;
        if (i > 0) acc += t;   /* reads the value stored last iteration */
        t = i + seed;
    }
    return acc;
}
int straight_body(int n) {
    int acc = 0;
    for (int i = 0; i < n; i++) { int t = i * 2; acc += t; }
    return acc;
}
int main(void) {
    /* t = i + 7, read on the next iteration: 7 + 8 + 9 = 24 for n = 4 */
    if (carry(4) != 24) return 1;
    if (straight_body(5) != 20) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("loop_body_local", src, &[]), 0);
}

/// A parameter lives in `func.locals` under its bare name; a global reached
/// through a block-scoped `extern` gets a fresh pseudo carrying the *same*
/// name. Renaming keyed on the name alone would kill the global's store and
/// hand its value to the parameter, so the parameter would read 2.
///
/// Only the parameter's value is asserted. The global's own value is wrong
/// today for an unrelated reason -- `arch/*/regalloc.rs` resolves a `Sym` by
/// looking its *name* up in `func.locals`, so the global's pseudo finds the
/// parameter's slot -- and asserting it here would turn a pre-existing defect
/// into a failure of this change.
#[test]
fn codegen_shadowed_global_is_not_confused_with_parameter() {
    let src = r#"
int v = 5;
int sink;
int f(int v) {
    v = 1;
    { extern int v; v = 2; }   /* writes the GLOBAL, not the parameter */
    sink = v;                  /* must be the parameter: 1 */
    return sink;
}
int main(void) { return f(99) == 1 ? 0 : 1; }
"#;
    assert_eq!(compile_and_run("shadowed_global", src, &[]), 0);
}

/// Hidden compiler-generated locals that are pointer-typed, and therefore
/// scalar, and therefore newly promotable: the VLA base pointer and a
/// `va_list` parameter. Neither goes through `SymAddr`, so neither was
/// covered before.
#[test]
fn codegen_hidden_pointer_locals_survive_promotion() {
    let src = r#"
#include <stdarg.h>
int vla_sum(int n) {
    int a[n];
    for (int i = 0; i < n; i++) a[i] = i * 3;
    int s = 0;
    for (int i = 0; i < n; i++) s += a[i];
    return s;
}
static int va_sum(int count, va_list ap) {
    int s = 0;
    for (int i = 0; i < count; i++) s += va_arg(ap, int);
    return s;
}
static int trampoline(int count, ...) {
    va_list ap; va_start(ap, count);
    int s = va_sum(count, ap);
    va_end(ap);
    return s;
}
int main(void) {
    if (vla_sum(5) != 30) return 1;          /* 0+3+6+9+12 */
    if (trampoline(4, 1, 2, 3, 4) != 10) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("hidden_pointer_locals", src, &[]), 0);
}

/// Values wider than a general register, single-block. `long double` is
/// stored and loaded as one 128-bit unit, so it is promotable; `__int128`
/// likewise. Both cross the 8-byte boundary where aggregate handling
/// historically confuses a value with its address.
#[test]
fn codegen_wide_single_block_locals_promote_correctly() {
    let src = r#"
long double ld(long double x) { long double t = x + 1.0L; return t * 2.0L; }
__int128 i128(__int128 x) { __int128 t = x + 1; return t * 2; }
int main(void) {
    if (ld(10.0L) != 22.0L) return 1;
    __int128 r = i128((__int128)10);
    if (r != (__int128)22) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("wide_single_block", src, &[]), 0);
}

/// Reading an uninitialized local is undefined, but it must not crash the
/// compiler: the load has no reaching definition and becomes a `Copy` of an
/// undef pseudo, which has to survive to codegen.
#[test]
fn codegen_uninitialized_local_still_compiles() {
    let src = r#"
int uninit(int c) { int t; int a = t; if (c) t = 1; else t = 2; return a + t; }
int single_block_uninit(void) { int t; return t; }
int main(void) { return 0; }
"#;
    assert_eq!(compile_and_run("uninitialized_local", src, &[]), 0);
}

/// A frame holds only what the function uses.
///
/// Two reservations were made whatever the function did: the x87 scratch,
/// sixteen bytes at the bottom of every x86-64 frame though only an x87
/// conversion or an x87 `asm` operand stages a value through it, and every
/// local the linearizer created, though the optimizer may forward and delete
/// every access to one -- `folded` reads back the element it just stored.
/// Now neither costs a function that does not use it, and a function that
/// does still gets the scratch.
#[test]
fn codegen_a_frame_holds_only_what_the_function_uses() {
    let src = "\
int plus1(int x) { return x + 1; }
int folded(void) { int a[4] = {1, 2, 3, 4}; return a[2]; }
int sum(const int *p, int n) { int s = 0; for (int i = 0; i < n; i++) s += p[i]; return s; }
long double widen(int x) { return x; }
float root(float x) { __asm__(\"fsqrt\" : \"+t\"(x)); return x; }
";
    let asm = asm_for_with("frame_uses", X86_64_LINUX, src, &["-O2"]);
    for f in ["plus1", "folded", "sum"] {
        let body = body_of(&asm, f);
        assert!(
            frame_size(&asm, f).is_none() && !body.contains("subq"),
            "{f} needs no frame:\n{body}"
        );
    }
    for f in ["widen", "root"] {
        let frame = frame_size(&asm, f).unwrap_or(0);
        assert!(
            frame >= 16,
            "{f} stages a value through the x87 scratch:\n{asm}"
        );
    }
    let a64 = asm_for_with(
        "frame_uses_a64",
        AARCH64_LINUX,
        "int folded(void) { int a[4] = {1, 2, 3, 4}; return a[2]; }\n",
        &["-O2"],
    );
    assert!(frame_size(&a64, "folded").is_none(), "aarch64:\n{a64}");

    let run = format!(
        "{src}int main(void) {{ int a[3] = {{1, 2, 3}}; \
         return plus1(1) == 2 && folded() == 3 && sum(a, 3) == 6 \
         && widen(7) == 7.0L && root(16.0f) == 4.0f ? 0 : 1; }}\n"
    );
    assert_eq!(
        compile_and_run("frame_uses_run", &run, &["-O2".to_string()]),
        0
    );
    let portable = run.replace(
        "float root(float x) { __asm__(\"fsqrt\" : \"+t\"(x)); return x; }\n",
        "float root(float x) { return x == 16.0f ? 4.0f : 0; }\n",
    );
    if let Some(rc) = compile_and_run_aarch64("frame_uses_a64_run", &portable, "-O2") {
        assert_eq!(rc, 0, "aarch64");
    }
}

/// Locals whose lifetimes do not overlap share a frame slot.
///
/// Every local held its own slot for the whole function: a `Sym` has no
/// defining instruction, so liveness carried each one to the entry and they
/// all overlapped. The interval of a local is now its lifetime -- from its
/// first mention to the `LifetimeEnd` the linearizer puts where control falls
/// out of its block -- so `scopes`' three arrays take one slot. The rest of
/// the program is the shapes that must still come out right: a nested scope
/// that overlaps its parent, a loop body's array beside one live across the
/// loop, a `switch` jumping past a declaration into its scope, a backward
/// `goto`, an address that escapes, and a parameter the prologue stores.
#[test]
fn codegen_locals_with_disjoint_lifetimes_share_a_slot() {
    let src = r#"
__attribute__((noinline)) void fill(int *p, int n, int v) { for (int i = 0; i < n; i++) p[i] = v + i; }
__attribute__((noinline)) int sum(const int *p, int n) { int s = 0; for (int i = 0; i < n; i++) s += p[i]; return s; }
int *escaped;
__attribute__((noinline)) int scopes(int k) {
    int r = 0;
    { int a[16]; fill(a, 16, k); r += sum(a, 16); }
    { int b[16]; fill(b, 16, 2 * k); r += sum(b, 16); }
    { int c[16]; fill(c, 16, 3 * k); r += sum(c, 16); }
    return r;
}
__attribute__((noinline)) int nested(int k) {
    int a[16]; fill(a, 16, k);
    { int b[16]; fill(b, 16, 100); if (sum(b, 16) != 1720) return -1; }
    return sum(a, 16);
}
__attribute__((noinline)) int loop(int n) {
    int keep[8]; fill(keep, 8, 7);
    int r = 0;
    for (int i = 0; i < n; i++) { int t[8]; fill(t, 8, i); r += sum(t, 8); }
    return r + sum(keep, 8);
}
__attribute__((noinline)) int sw(int x) {
    int other[4]; fill(other, 4, 50);
    switch (x) {
        int y[4];
    case 1: fill(y, 4, 1); return sum(y, 4) + sum(other, 4);
    case 2: { int z[4]; fill(z, 4, 9); y[0] = 0; return sum(z, 4); }
    }
    return 0;
}
__attribute__((noinline)) int back(int n) {
    int r = 0, i = 0;
again:
    { int v[4]; fill(v, 4, i); r += sum(v, 4); }
    { int w[4]; fill(w, 4, 1000); r += w[3] - 1003; }
    if (++i < n) goto again;
    return r;
}
__attribute__((noinline)) int esc(void) {
    int r;
    { int a[4]; escaped = a; fill(escaped, 4, 3); r = sum(a, 4); }
    { int b[4]; fill(b, 4, 0); r += sum(b, 4); }
    return r;
}
__attribute__((noinline)) int param(int p) {
    int *q = &p;
    { int a[8]; fill(a, 8, 1); if (sum(a, 8) != 36) return -1; }
    return *q;
}
int main(void) {
    int bad = 0;
    if (scopes(1) != (16*1+120) + (16*2+120) + (16*3+120)) bad |= 1;
    if (nested(2) != 16*2+120) bad |= 2;
    if (loop(3) != (0+28)+(8+28)+(16+28) + (56+28)) bad |= 4;
    if (sw(1) != 4+6 + 200+6) bad |= 8;
    if (sw(2) != 36+6) bad |= 16;
    if (back(3) != (0+6)+(4+6)+(8+6)) bad |= 32;
    if (esc() != 12+6 + 6) bad |= 64;
    if (param(77) != 77) bad |= 128;
    return bad;
}
"#;
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run("disjoint_lifetimes", src, &[opt.to_string()]),
            0,
            "{opt}"
        );
        if let Some(rc) = compile_and_run_aarch64("disjoint_lifetimes", src, opt) {
            assert_eq!(rc, 0, "aarch64 at {opt}");
        }
    }
    // Three 64-byte arrays, one at a time: one slot's worth, not three.
    let asm = asm_for_with("disjoint_lifetimes_frame", X86_64_LINUX, src, &["-O2"]);
    let frame = frame_size(&asm, "scopes").unwrap_or(0);
    assert!(
        frame < 128,
        "three disjoint arrays share a slot, got {frame} bytes:\n{asm}"
    );
}
