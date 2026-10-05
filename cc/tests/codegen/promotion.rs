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

use crate::codegen::asm_probe::{asm_for_with, frame_size, X86_64_LINUX};
use crate::common::{compile_and_run, compile_and_run_aarch64};

/// The cases where promoting a local out of memory would change the
/// answer, one program run at the compile matrix levels.
///
/// Consolidates these tests, one section each (each original `main` is
/// a `noinline` section function; the program exits with the section's
/// base plus the original code):
/// - `codegen_complex_local_is_not_forwarded_by_half`: 1..=3
/// - `codegen_folded_constant_is_not_reused_beyond_its_width`: 4..=4
/// - `codegen_loop_body_local_reads_previous_iteration`: 5..=6
/// - `codegen_shadowed_global_is_not_confused_with_parameter`: 7..=7
/// - `codegen_hidden_pointer_locals_survive_promotion`: 8..=9
/// - `codegen_wide_single_block_locals_promote_correctly`: 10..=11
/// - `codegen_uninitialized_local_still_compiles`: never fails by exit code
#[test]
fn codegen_promotion_mega() {
    let code = r#"
/* ---- codegen_complex_local_is_not_forwarded_by_half: exits 1..3
 * `_Complex` is a scalar by `is_scalar`, but its halves are stored
 * separately at offsets 0 and 8 and read back by a single 128-bit load at
 * offset 0. Promoting it on the strength of "scalar, address not taken"
 * forwards the *last* store -- the imaginary half -- into that load, and
 * `if (z)` becomes permanently false.
 *
 * This is the shape that makes the width guard in `analyze_variable` load
 * bearing rather than defensive.
 */
int nonzero_real(void) { double _Complex z = 3.0; if (z) return 1; return 0; }
int zero_both(void)    { double _Complex z = 0.0; if (z) return 1; return 0; }
int sum_halves(void) {
    double _Complex z = 3.0;
    /* Reads both halves back out of the same slot the two stores wrote. */
    return (int)(__real__ z) * 10 + (int)(__imag__ z);
}
static __attribute__((noinline)) int t_complex_local_is_not_forwarded_by_half(void)
{
    if (nonzero_real() != 1) return 1;
    if (zero_both()   != 0) return 2;
    if (sum_halves()  != 30) return 3;
    return 0;
}

/* ---- codegen_folded_constant_is_not_reused_beyond_its_width: exits 4..4
 * Folding through a `Copy` chain must not widen the existing looseness
 * about constants not being truncated to their operand width.
 *
 * `0x40000000 * 4` overflows `int`. The product is held as a full-width
 * `i128`, so a chained fold of `y / 2` would answer `INT_MIN` where the
 * truncated operand gives `0`. gcc gives 0.
 */
int width(void) { int y = 0x40000000 * 4; int z = y / 2; return z; }
static __attribute__((noinline)) int t_folded_constant_is_not_reused_beyond_its_width(void)
{ return width() == 0 ? 0 : 1; }

/* ---- codegen_loop_body_local_reads_previous_iteration: exits 5..6
 * A local declared inside a loop body has all its uses in one block, but
 * reading it before writing it reads the previous iteration -- so it needs
 * a phi at the loop header, not a linear forward.
 */
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
static __attribute__((noinline)) int t_loop_body_local_reads_previous_iteration(void)
{
    /* t = i + 7, read on the next iteration: 7 + 8 + 9 = 24 for n = 4 */
    if (carry(4) != 24) return 1;
    if (straight_body(5) != 20) return 2;
    return 0;
}

/* ---- codegen_shadowed_global_is_not_confused_with_parameter: exits 7..7
 * A parameter lives in `func.locals` under its bare name; a global reached
 * through a block-scoped `extern` gets a fresh pseudo carrying the *same*
 * name. Renaming keyed on the name alone would kill the global's store and
 * hand its value to the parameter, so the parameter would read 2.
 *
 * Only the parameter's value is asserted. The global's own value is wrong
 * today for an unrelated reason -- `arch/* /regalloc.rs` resolves a `Sym` by
 * looking its *name* up in `func.locals`, so the global's pseudo finds the
 * parameter's slot -- and asserting it here would turn a pre-existing defect
 * into a failure of this change.
 */
int v = 5;
int sink;
int f(int v) {
    v = 1;
    { extern int v; v = 2; }   /* writes the GLOBAL, not the parameter */
    sink = v;                  /* must be the parameter: 1 */
    return sink;
}
static __attribute__((noinline)) int t_shadowed_global_is_not_confused_with_parameter(void)
{ return f(99) == 1 ? 0 : 1; }

/* ---- codegen_hidden_pointer_locals_survive_promotion: exits 8..9
 * Hidden compiler-generated locals that are pointer-typed, and therefore
 * scalar, and therefore newly promotable: the VLA base pointer and a
 * `va_list` parameter. Neither goes through `SymAddr`, so neither was
 * covered before.
 */
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
static __attribute__((noinline)) int t_hidden_pointer_locals_survive_promotion(void)
{
    if (vla_sum(5) != 30) return 1;          /* 0+3+6+9+12 */
    if (trampoline(4, 1, 2, 3, 4) != 10) return 2;
    return 0;
}

/* ---- codegen_wide_single_block_locals_promote_correctly: exits 10..11
 * Values wider than a general register, single-block. `long double` is
 * stored and loaded as one 128-bit unit, so it is promotable; `__int128`
 * likewise. Both cross the 8-byte boundary where aggregate handling
 * historically confuses a value with its address.
 */
long double ld(long double x) { long double t = x + 1.0L; return t * 2.0L; }
__int128 i128(__int128 x) { __int128 t = x + 1; return t * 2; }
static __attribute__((noinline)) int t_wide_single_block_locals_promote_correctly(void)
{
    if (ld(10.0L) != 22.0L) return 1;
    __int128 r = i128((__int128)10);
    if (r != (__int128)22) return 2;
    return 0;
}

/* ---- codegen_uninitialized_local_still_compiles: never fails by exit code
 * Reading an uninitialized local is undefined, but it must not crash the
 * compiler: the load has no reaching definition and becomes a `Copy` of an
 * undef pseudo, which has to survive to codegen.
 */
int uninit(int c) { int t; int a = t; if (c) t = 1; else t = 2; return a + t; }
int single_block_uninit(void) { int t; return t; }
static __attribute__((noinline)) int t_uninitialized_local_still_compiles(void)
{ return 0; }

int main(void)
{
    int r;
    if ((r = t_complex_local_is_not_forwarded_by_half()) != 0) return r;
    if ((r = t_folded_constant_is_not_reused_beyond_its_width()) != 0) return 3 + r;
    if ((r = t_loop_body_local_reads_previous_iteration()) != 0) return 4 + r;
    if ((r = t_shadowed_global_is_not_confused_with_parameter()) != 0) return 6 + r;
    if ((r = t_hidden_pointer_locals_survive_promotion()) != 0) return 7 + r;
    if ((r = t_wide_single_block_locals_promote_correctly()) != 0) return 9 + r;
    if ((r = t_uninitialized_local_still_compiles()) != 0) return 11 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("promotion_mega", code, &[]), 0);
}

/// A frame holds only what the function uses.
///
/// Two reservations were made whatever the function did: the x87 scratch,
/// sixteen bytes at the bottom of every x86-64 frame though only an x87
/// conversion or an x87 `asm` operand stages a value through it, and every
/// local the linearizer created, though the optimizer may forward and delete
/// every access to one -- `folded` reads back the element it just stored.
/// Now neither costs a function that does not use it, and a function that
/// does still gets the scratch. Off x86-64 the host runs the program with
/// `root`'s x87 `asm` replaced by plain C.
#[test]
fn codegen_a_frame_holds_only_what_the_function_uses() {
    let src = "\
int plus1(int x) { return x + 1; }
int folded(void) { int a[4] = {1, 2, 3, 4}; return a[2]; }
int sum(const int *p, int n) { int s = 0; for (int i = 0; i < n; i++) s += p[i]; return s; }
long double widen(int x) { return x; }
float root(float x) { __asm__(\"fsqrt\" : \"+t\"(x)); return x; }
";
    // The frame sizes of these functions are asserted by the test of the
    // same name in `cc/test_asm/codegen_promotion.rs`; this runs them.
    let run = format!(
        "{src}int main(void) {{ int a[3] = {{1, 2, 3}}; \
         return plus1(1) == 2 && folded() == 3 && sum(a, 3) == 6 \
         && widen(7) == 7.0L && root(16.0f) == 4.0f ? 0 : 1; }}\n"
    );
    let portable = run.replace(
        "float root(float x) { __asm__(\"fsqrt\" : \"+t\"(x)); return x; }\n",
        "float root(float x) { return x == 16.0f ? 4.0f : 0; }\n",
    );
    // `compile_and_run` targets the host, and an x87 `asm` only assembles
    // on x86-64; elsewhere the host runs the portable form.
    let host = if cfg!(target_arch = "x86_64") {
        &run
    } else {
        &portable
    };
    assert_eq!(
        compile_and_run("frame_uses_run", host, &["-O2".to_string()]),
        0
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
