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

use crate::codegen::asm_probe::{asm_for_with, count_in_body, frame_size, X86_64_LINUX};
use crate::common::compile_and_run;

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
        frame <= 16,
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
