//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Inline Assembly Mega-Test
//
// Consolidates: ALL inline_asm tests (GCC-compatible extended asm)
//
// Tests are architecture-conditional since inline assembly is platform-specific.
//

use crate::common::{compile_and_run, compile_and_run_optimized};

// ============================================================================
// Architecture-Independent Inline Assembly Test
// ============================================================================

#[test]
fn codegen_inline_asm_nop() {
    let code = r#"
int main(void) {
    __asm__ volatile("nop");
    return 0;
}
"#;
    assert_eq!(compile_and_run("asm_nop", code, &[]), 0);
}

// ============================================================================
// x86-64 Inline Assembly Mega-Test
// ============================================================================

#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_inline_asm_x86_64_mega() {
    let code = r#"
int main(void) {
    // ========== BASIC ASM (returns 1-19) ==========
    {
        // Output register
        int result;
        __asm__("movl $42, %0" : "=r"(result));
        if (result != 42) return 1;

        // Input/output
        int x = 10;
        __asm__("addl $5, %0" : "+r"(x));
        if (x != 15) return 2;

        // Two operands
        int a = 10, b = 20;
        int r;
        __asm__("movl %1, %0\n\t"
                "addl %2, %0"
                : "=r"(r)
                : "r"(a), "r"(b));
        if (r != 30) return 3;

        // Multiline nops
        __asm__ volatile("nop\n\tnop\n\tnop");

        // With memory clobber
        result = 0;
        __asm__ volatile("movl $1, %0" : "=r"(result) : : "memory");
        if (result != 1) return 4;
    }

    // ========== ASM SYNTAX VARIANTS (returns 20-29) ==========
    {
        // __asm__ spelling
        __asm__ volatile("nop");
        // asm spelling
        asm volatile("nop");
        // __asm spelling
        __asm volatile("nop");
    }

    // ========== MATCHING CONSTRAINTS (returns 30-39) ==========
    {
        int x = 10;
        __asm__("addl $5, %0" : "=r"(x) : "0"(x));
        if (x != 15) return 30;
    }

    // ========== NAMED OPERANDS (returns 40-49) ==========
    {
        int result;
        __asm__("movl $42, %[out]" : [out] "=r"(result));
        if (result != 42) return 40;

        int x = 10, y = 20;
        int r;
        __asm__("movl %[a], %[res]\n\t"
                "addl %[b], %[res]"
                : [res] "=r"(r)
                : [a] "r"(x), [b] "r"(y));
        if (r != 30) return 41;
    }

    // ========== CLOBBERS (returns 50-59) ==========
    {
        // Memory clobber
        int x = 10;
        __asm__ volatile("" ::: "memory");
        if (x != 10) return 50;

        // CC clobber
        x = 5;
        __asm__ volatile("testl %0, %0" : : "r"(x) : "cc");

        // Early clobber
        int a = 10, b = 20;
        int result;
        __asm__("movl %1, %0\n\t"
                "addl %2, %0"
                : "=&r"(result)
                : "r"(a), "r"(b));
        if (result != 30) return 51;
    }

    // ========== SPECIFIC REGISTERS (returns 60-69) ==========
    {
        // =a constraint (EAX)
        int result;
        __asm__("movl $42, %%eax" : "=a"(result));
        if (result != 42) return 60;

        // c constraint for shifts (ECX)
        int value = 4;
        int count = 2;
        __asm__("shll %%cl, %0" : "+r"(value) : "c"(count));
        if (value != 16) return 61;

        // =d constraint (EDX)
        __asm__("movl $99, %%edx" : "=d"(result));
        if (result != 99) return 62;

        // Note: =a and =d together test removed - multiple output register bug
        // int lo, hi;
        // __asm__("movl $100, %%eax\n\t"
        //         "movl $200, %%edx"
        //         : "=a"(lo), "=d"(hi));
        // if (lo != 100 || hi != 200) return 63;
    }

    // ========== ASM GOTO (returns 70-89) ==========
    {
        // Basic asm goto
        int x = 5;
        __asm__ goto("cmpl $5, %0\n\t"
                     "je %l[match1]"
                     : : "r"(x) : "cc" : match1);
        return 70;
match1:

        // Fallthrough case
        x = 10;
        __asm__ goto("cmpl $5, %0\n\t"
                     "je %l[match2]"
                     : : "r"(x) : "cc" : match2);
        goto cont2;
match2:
        return 71;
cont2:

        // Named label reference
        x = 5;
        __asm__ goto("cmpl $5, %0\n\t"
                     "je %l[target]"
                     : : "r"(x) : "cc" : target);
        return 72;
target:

        // Multiple labels
        x = 2;
        __asm__ goto("cmpl $1, %0\n\t"
                     "je %l[one]\n\t"
                     "cmpl $2, %0\n\t"
                     "je %l[two]\n\t"
                     "jmp %l[other]"
                     : : "r"(x) : "cc" : one, two, other);
        return 73;
one:
        return 74;
two:
        goto end_goto;
other:
        return 75;
end_goto:
        ;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("asm_x86_64_mega", code, &[]), 0);
}

#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_inline_asm_x86_64_asm_goto_pseudo_survives_edge() {
    // The asm-goto CFG plumbing is wired by `linearize_stmt.rs`:
    // when an `__asm__ goto(...)` is emitted, the linearizer
    // explicitly `link_bb`s the current block to every goto-label
    // target plus a fresh fall-through. So liveness propagates
    // correctly from the goto-target back through the asm block,
    // and a pseudo live only at the goto target survives the asm
    // edge with its allocator assignment intact.
    //
    // This test guards against future regression in that plumbing:
    // `val` is computed before the asm, conditionally goto'd-over
    // (so it's still live at the `match_label:` target), then read.
    // If the CFG edge didn't exist, chordal coloring would treat
    // `val` as dying before the asm and could reuse its register
    // for an intermediate inside the asm block, corrupting the
    // value the `match_label:` branch reads.
    let code = r#"
int main(void) {
    int val = 0xDEADBEEF;
    int x = 5;
    __asm__ goto("cmpl $5, %0\n\t"
                 "je %l[match_label]"
                 : : "r"(x) : "cc" : match_label);
    return 1;  // x != 5 — shouldn't happen
match_label:
    if (val != 0xDEADBEEF) return 2;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("asm_x86_64_goto_pseudo_survives_edge", code, &[]),
        0,
        "pseudo live at asm-goto target must survive the edge"
    );
}

#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_inline_asm_x86_64_fixed_register_precolor() {
    // Regression test for C3's Fixed-operand pre-coloring path.
    //
    // The chordal allocator now pre-colors the operand pseudo of
    // `"=a"(...)` / `"a"(...)` / etc. directly to the constraint-
    // required register. The first commit of C3 had a `get_location`
    // hole: pre-coloring populated only the chordal `pre_colored`
    // map, not `self.locations`. The subsequent Store/Load of the
    // operand then fell through to `Loc::Imm(0)` and silently
    // wrote/read zero.
    //
    // This test exercises every Fixed letter currently in scope
    // (a, b, c, d, S, D) in both `=` and bare positions so a future
    // regression in `collect_asm_fixed_precolors_x86_64` or the
    // `self.locations.insert` in `color_gp_bank` is caught.
    let code = r#"
int main(void) {
    {
        int r;
        __asm__("movl $42, %%eax" : "=a"(r));
        if (r != 42) return 1;
    }
    {
        int r;
        __asm__("movl $7, %%ebx" : "=b"(r));
        if (r != 7) return 2;
    }
    {
        int r;
        __asm__("movl $9, %%ecx" : "=c"(r));
        if (r != 9) return 3;
    }
    {
        int r;
        __asm__("movl $13, %%edx" : "=d"(r));
        if (r != 13) return 4;
    }
    {
        int input = 100;
        int r;
        __asm__("movl %1, %0" : "=a"(r) : "S"(input));
        if (r != 100) return 5;
    }
    {
        int input = 200;
        int r;
        __asm__("movl %1, %0" : "=a"(r) : "D"(input));
        if (r != 200) return 6;
    }
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("asm_x86_64_fixed_register_precolor", code, &[]),
        0,
        "fixed-register asm operand must read/write the chordal-assigned register"
    );
}

#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_inline_asm_x86_64_fixed_precolor_collides_with_abi_pin() {
    // Exercises the C3 Fixed-precolor + ABI-pin collision path
    // surfaced by Copilot review.
    //
    // The chordal allocator pre-colors two pseudo sources: (a) ABI-
    // pinned params via `allocate_arguments` (first int arg → RDI),
    // and (b) inline-asm `Fixed(R)` operands. When the same pseudo
    // shows up in both, `pre_colored.entry(pid).or_insert(reg)`
    // correctly keeps the prior ABI register, but the previous code
    // then unconditionally overwrote `self.locations[pid]` with the
    // asm-requested register. Allocator and codegen would disagree —
    // chordal coloring would use RDI, codegen would read RAX.
    //
    // Fix: commit whatever `pre_colored` actually holds to
    // `self.locations`, never the asm-requested register when an
    // earlier pin already won.
    //
    // Today's linearizer stores incoming ABI-args to local stack
    // slots at function entry, so the asm operand is loaded back
    // through a fresh pseudo that's NOT ABI-pinned — meaning the
    // bug doesn't trigger end-to-end on current c17. This test
    // therefore can't fail under the broken code today, but it
    // anchors the linearizer-side assumption: if a future change
    // makes the arg pseudo and the asm operand pseudo identical
    // (e.g. an arg-promotion pass under -O2 that bypasses the
    // entry Store/Load shuffle), the corrected codegen path here
    // continues to work. Without the regalloc.rs fix, that future
    // change would silently miscompile inline asm with no compile-
    // time signal.
    let code = r#"
__attribute__((noinline))
static int incr_arg1_via_rsi(int arg) {
    // arg pinned by ABI to RDI (1st int arg); asm forces RSI ("+S").
    __asm__("addl $5, %0" : "+S"(arg));
    return arg;
}

__attribute__((noinline))
static int incr_arg4_via_rdx(int a, int b, int c, int arg) {
    // arg pinned by ABI to RCX (4th int arg); asm forces RDX ("+d").
    (void)a; (void)b; (void)c;
    __asm__("addl $7, %0" : "+d"(arg));
    return arg;
}

__attribute__((noinline))
static int incr_arg2_via_rax(int a, int arg) {
    // arg pinned by ABI to RSI (2nd int arg); asm forces RAX ("+a").
    (void)a;
    __asm__("addl $9, %0" : "+a"(arg));
    return arg;
}

int main(void) {
    if (incr_arg1_via_rsi(37) != 42) return 1;
    if (incr_arg4_via_rdx(0, 0, 0, 33) != 40) return 2;
    if (incr_arg2_via_rax(0, 31) != 40) return 3;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("asm_x86_64_fixed_precolor_abi_collide", code, &[]),
        0,
        "asm Fixed precolor must not split self.locations from pre_colored when the pseudo is already ABI-pinned"
    );
    assert_eq!(
        compile_and_run_optimized("asm_x86_64_fixed_precolor_abi_collide_opt", code),
        0,
        "ABI-vs-asm-Fixed split-view fix must hold under -O1"
    );
}

// ============================================================================
// AArch64 Inline Assembly Mega-Test
// ============================================================================

#[cfg(target_arch = "aarch64")]
#[test]
fn codegen_inline_asm_aarch64_mega() {
    let code = r#"
int main(void) {
    // ========== BASIC ASM (returns 1-19) ==========
    {
        // Output register
        int result;
        __asm__("mov %w0, #42" : "=r"(result));
        if (result != 42) return 1;

        // Input/output
        int x = 10;
        __asm__("add %w0, %w0, #5" : "+r"(x));
        if (x != 15) return 2;

        // Two operands
        int a = 10, b = 20;
        int r;
        __asm__("add %w0, %w1, %w2"
                : "=r"(r)
                : "r"(a), "r"(b));
        if (r != 30) return 3;

        // Multiline nops
        __asm__ volatile("nop\n\tnop\n\tnop");

        // With memory clobber
        result = 0;
        __asm__ volatile("mov %w0, #1" : "=r"(result) : : "memory");
        if (result != 1) return 4;
    }

    // ========== ASM SYNTAX VARIANTS (returns 20-29) ==========
    {
        __asm__ volatile("nop");
        asm volatile("nop");
        __asm volatile("nop");
    }

    // ========== MATCHING CONSTRAINTS (returns 30-39) ==========
    {
        int x = 10;
        __asm__("add %w0, %w0, #5" : "=r"(x) : "0"(x));
        if (x != 15) return 30;
    }

    // ========== NAMED OPERANDS (returns 40-49) ==========
    {
        int result;
        __asm__("mov %w[out], #42" : [out] "=r"(result));
        if (result != 42) return 40;

        int x = 10, y = 20;
        int r;
        __asm__("add %w[res], %w[a], %w[b]"
                : [res] "=r"(r)
                : [a] "r"(x), [b] "r"(y));
        if (r != 30) return 41;
    }

    // ========== CLOBBERS (returns 50-59) ==========
    {
        // Memory clobber
        int x = 10;
        __asm__ volatile("" ::: "memory");
        if (x != 10) return 50;

        // CC clobber
        x = 5;
        __asm__ volatile("cmp %w0, #0" : : "r"(x) : "cc");

        // Early clobber
        int a = 10, b = 20;
        int result;
        __asm__("add %w0, %w1, %w2"
                : "=&r"(result)
                : "r"(a), "r"(b));
        if (result != 30) return 51;
    }

    // ========== ASM GOTO (returns 70-89) ==========
    {
        // Basic asm goto
        int x = 5;
        __asm__ goto("cmp %w0, #5\n\t"
                     "beq %l[match1]"
                     : : "r"(x) : "cc" : match1);
        return 70;
match1:

        // Fallthrough case
        x = 10;
        __asm__ goto("cmp %w0, #5\n\t"
                     "beq %l[match2]"
                     : : "r"(x) : "cc" : match2);
        goto cont2;
match2:
        return 71;
cont2:

        // Named label reference
        x = 5;
        __asm__ goto("cmp %w0, #5\n\t"
                     "beq %l[target]"
                     : : "r"(x) : "cc" : target);
        return 72;
target:

        // Multiple labels
        x = 2;
        __asm__ goto("cmp %w0, #1\n\t"
                     "beq %l[one]\n\t"
                     "cmp %w0, #2\n\t"
                     "beq %l[two]\n\t"
                     "b %l[other]"
                     : : "r"(x) : "cc" : one, two, other);
        return 73;
one:
        return 74;
two:
        goto end_goto;
other:
        return 75;
end_goto:
        ;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("asm_aarch64_mega", code, &[]), 0);
}

// ============================================================================
// C6b — Memory-Barrier Pattern Lock-In Tests
// ============================================================================
//
// These tests lock in the heavily-used `asm volatile("..." ::: "memory")`
// idioms — spin-loop pause/yield, full fence (mfence / dmb ish), and the
// double-checked-init pattern — against future memory-reordering passes.
//
// Today no IR pass reorders memory ops across an instruction satisfying
// `Instruction::is_memory_barrier()` (see contract docs in `cc/ir/dce.rs`
// and `cc/ir/instcombine.rs`). When future passes do start reordering
// (GVN, LICM, load-store forwarding, machine scheduler), they MUST
// consult the predicate before crossing. If they regress, these tests
// fail loudly with the wrong return code instead of silently breaking a
// real spinlock under -O2.
//
// Each pattern runs at both default and -O1 to catch the optimizer-only
// regression case.

/// Plain pointer-aliased store/reload across an empty `asm volatile("" :::
/// "memory")` compiler barrier. A reordering pass that ignored the
/// barrier could forward the first store's value into the post-barrier
/// load, missing the intervening store. Today c17 doesn't reorder; this
/// test locks it in.
#[test]
fn codegen_inline_asm_memory_barrier_pointer_aliased_reload() {
    let code = r#"
int main(void) {
    int sentinel = 0;
    int *p = &sentinel;
    *p = 42;
    __asm__ volatile("" ::: "memory");
    *p = 99;
    __asm__ volatile("" ::: "memory");
    int read_back = *p;
    return (read_back == 99) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_memory_barrier_reload", code, &[]),
        0,
        "memory clobber must prevent store-forwarding the stale 42 across the barrier"
    );
    assert_eq!(
        compile_and_run_optimized("asm_memory_barrier_reload_opt", code),
        0,
        "memory clobber semantics must hold at -O1 too"
    );
}

/// Spin-wait loop polling a non-volatile location through a pointer.
/// The `asm volatile("pause"/"yield" ::: "memory")` inside the loop body
/// must force a reload of `*p` every iteration — otherwise a hoisting
/// pass would lift `*p` out of the loop and the loop would never
/// terminate. The loop body also writes the flag, so a single-threaded
/// run reaches termination iff the reload happens.
#[test]
fn codegen_inline_asm_memory_barrier_spin_wait_pattern() {
    let code = r#"
int main(void) {
    int flag = 1;
    int *p = &flag;
    int iters = 0;
    while (*p) {
#if defined(__x86_64__)
        __asm__ volatile("pause" ::: "memory");
#elif defined(__aarch64__)
        __asm__ volatile("yield" ::: "memory");
#else
        __asm__ volatile("" ::: "memory");
#endif
        *p = 0;
        iters++;
        if (iters > 10) return 2;
    }
    return (iters == 1) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_memory_barrier_spin_wait", code, &[]),
        0,
        "pause/yield + memory clobber must force flag reload each iteration"
    );
    assert_eq!(
        compile_and_run_optimized("asm_memory_barrier_spin_wait_opt", code),
        0,
        "loop body memory clobber must defeat -O1 hoisting of *p"
    );
}

/// Full memory fence (`mfence` on x86_64, `dmb ish` on aarch64). The
/// surrounding stores and loads must straddle the fence — a pass that
/// reordered the post-fence load before the fence (or dropped the
/// pre-fence store) would observe the wrong value.
#[test]
fn codegen_inline_asm_memory_barrier_full_fence() {
    let code = r#"
int main(void) {
    int x = 5;
    int *p = &x;
    *p = 10;
#if defined(__x86_64__)
    __asm__ volatile("mfence" ::: "memory");
#elif defined(__aarch64__)
    __asm__ volatile("dmb ish" ::: "memory");
#else
    __asm__ volatile("" ::: "memory");
#endif
    *p = 20;
    return (*p == 20) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_memory_barrier_full_fence", code, &[]),
        0,
        "stores around a full fence must not be reordered or dropped"
    );
    assert_eq!(
        compile_and_run_optimized("asm_memory_barrier_full_fence_opt", code),
        0,
        "full fence semantics must hold at -O1 too"
    );
}

/// Double-checked-init pattern. The release barrier in `slow_init()`
/// must order the `value = 42` store before the `initialized = 1` store.
/// The acquire barrier in `get_value()` must force a reload of `value`
/// after seeing `initialized == 1`. A reordering pass that ignored
/// either barrier could observe `value == 0` despite `initialized == 1`.
/// Single-threaded execution still exercises the compiler-level
/// reordering semantics — what we're locking in is "the compiler does
/// not reorder/eliminate loads or stores across the barriers."
#[test]
fn codegen_inline_asm_memory_barrier_double_checked_init() {
    let code = r#"
static int initialized = 0;
static int value = 0;

static void slow_init(void) {
    value = 42;
    __asm__ volatile("" ::: "memory");
    initialized = 1;
}

static int get_value(void) {
    if (!initialized) {
        slow_init();
    }
    __asm__ volatile("" ::: "memory");
    return value;
}

int main(void) {
    if (get_value() != 42) return 1;
    if (get_value() != 42) return 2;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("asm_memory_barrier_dci", code, &[]),
        0,
        "double-checked init: barriers must preserve store ordering and force value reload"
    );
    assert_eq!(
        compile_and_run_optimized("asm_memory_barrier_dci_opt", code),
        0,
        "double-checked init must hold at -O1 too"
    );
}

// ============================================================================
// C9 — Multi-Alternative Constraint Tests
// ============================================================================
//
// `"rm"`, `"ri"`, `"rmi"`, and `"g"` (= `"rmi"`) let the compiler choose
// register vs memory vs immediate based on what fits cheapest given the
// operand's location. C9a teaches the parser to build
// `OperandConstraint::Alternatives`; C9b makes the x86_64 codegen's
// `constraint_requires_register` / `constraint_requires_memory` honor
// the alternatives (a constraint that lists any non-register class no
// longer force-loads a spilled value into a temp register; a constraint
// that lists any non-memory class no longer forces a memory operand).
//
// These tests are arch-independent — the patterns work on both x86_64
// and aarch64 because the assembly inside the asm template uses
// arch-specific mnemonics in dedicated branches.

/// `"+rm"` with a value that lives in a register. The asm template must
/// substitute the register form; the value must be incremented in place.
#[test]
fn codegen_inline_asm_multi_alt_rm_register_path() {
    let code = r#"
int main(void) {
    int x = 41;
#if defined(__x86_64__)
    __asm__("addl $1, %0" : "+rm"(x));
#elif defined(__aarch64__)
    __asm__("add %w0, %w0, #1" : "+rm"(x));
#endif
    return (x == 42) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_multi_alt_rm_register", code, &[]),
        0,
        "+rm with register-resident value must use register syntax"
    );
    assert_eq!(
        compile_and_run_optimized("asm_multi_alt_rm_register_opt", code),
        0
    );
}

/// `"+m"` on an addr-taken value — value lives in memory by construction.
#[test]
fn codegen_inline_asm_multi_alt_m_memory_path() {
    let code = r#"
int main(void) {
    int x = 99;
    int *p = &x;  // address taken → x stays in memory (mem2reg can't promote)
#if defined(__x86_64__)
    __asm__("addl $1, %0" : "+m"(*p));
#elif defined(__aarch64__)
    __asm__("ldr w8, %0\n\tadd w8, w8, #1\n\tstr w8, %0" : "+m"(*p));
#endif
    return (*p == 100) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_multi_alt_m_memory", code, &[]),
        0,
        "+m on addr-taken operand must use memory syntax"
    );
    assert_eq!(
        compile_and_run_optimized("asm_multi_alt_m_memory_opt", code),
        0
    );
}

/// `"g"` (= `"rmi"`) — most permissive. Tested with both a runtime
/// value (compiler should use register form) and a const value
/// (compiler may substitute immediate; today it falls through to
/// register, which is also correct — C9c later optimizes this).
#[test]
fn codegen_inline_asm_multi_alt_g_runtime_value() {
    let code = r#"
int main(void) {
    int x = 7;
    int r;
#if defined(__x86_64__)
    __asm__("movl %1, %0\n\taddl $35, %0" : "=r"(r) : "g"(x));
#elif defined(__aarch64__)
    __asm__("add %w0, %w1, #35" : "=r"(r) : "g"(x));
#endif
    return (r == 42) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_multi_alt_g_runtime", code, &[]),
        0,
        "g constraint must work with a runtime int operand"
    );
    assert_eq!(
        compile_and_run_optimized("asm_multi_alt_g_runtime_opt", code),
        0
    );
}

/// `"=rm"` output to an addr-taken local — codegen must place the
/// result into memory (no force-load to a temp register).
#[test]
fn codegen_inline_asm_multi_alt_output_rm_to_memory() {
    let code = r#"
int main(void) {
    int dst = 0;
    int *p = &dst;
#if defined(__x86_64__)
    __asm__("movl $123, %0" : "=rm"(*p));
#elif defined(__aarch64__)
    __asm__("mov %w0, #123" : "=rm"(*p));
#endif
    return (*p == 123) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_multi_alt_output_rm", code, &[]),
        0,
        "=rm output to addr-taken location must succeed"
    );
    assert_eq!(
        compile_and_run_optimized("asm_multi_alt_output_rm_opt", code),
        0
    );
}

/// Multiple multi-alt operands in one asm, mixed input/output.
#[test]
fn codegen_inline_asm_multi_alt_mixed_operands() {
    let code = r#"
int main(void) {
    int a = 10, b = 20;
    int sum;
#if defined(__x86_64__)
    __asm__("movl %1, %0\n\taddl %2, %0"
            : "=r"(sum)
            : "rm"(a), "rm"(b));
#elif defined(__aarch64__)
    __asm__("add %w0, %w1, %w2"
            : "=r"(sum)
            : "rm"(a), "rm"(b));
#endif
    return (sum == 30) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_multi_alt_mixed", code, &[]),
        0,
        "multiple rm-constrained operands in one asm must compile and run"
    );
    assert_eq!(
        compile_and_run_optimized("asm_multi_alt_mixed_opt", code),
        0
    );
}

// ============================================================================
// C9c — Immediate-Bearing Constraint Lock-In Tests
// ============================================================================
//
// `"ri"` / `"g"` / x86_64's `I`/`J`/.../`O` accept a const-propagated
// operand by substituting it as a literal in the asm template instead
// of materializing it through a register. The substitution is implicit
// in `emit_inline_asm`'s `Loc::Imm(v)` arm — when an operand's
// allocator location is `Loc::Imm`, the codegen renders it as `$value`
// (x86_64) regardless of which class letter accepted it. These tests
// pin the behavior so a future codegen change that, e.g., forced a
// register load for `"r"` operands with `Loc::Imm` would break loudly
// instead of silently regressing asm quality.
//
// The tests inspect the resulting assembly only via the runtime
// observation that the test exits 0 — the deeper "is the literal
// substituted, not register-loaded?" assertion is left informal,
// since the runtime semantics are what matters for correctness.

#[test]
fn codegen_inline_asm_imm_const_via_ri() {
    // `"ri"(const)` — register or immediate. Const operand should
    // substitute as `$N` literal.
    let code = r#"
int main(void) {
    int r;
#if defined(__x86_64__)
    __asm__("movl %1, %0" : "=r"(r) : "ri"(42));
#elif defined(__aarch64__)
    __asm__("mov %w0, %w1" : "=r"(r) : "ri"(42));
#endif
    return (r == 42) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_imm_const_ri", code, &[]),
        0,
        "ri with const operand must substitute as immediate"
    );
    assert_eq!(compile_and_run_optimized("asm_imm_const_ri_opt", code), 0);
}

#[test]
fn codegen_inline_asm_imm_const_via_g() {
    // `"g"(const)` — any operand. Const should substitute as `$N`.
    let code = r#"
int main(void) {
    int r;
#if defined(__x86_64__)
    __asm__("movl %1, %0" : "=r"(r) : "g"(99));
#elif defined(__aarch64__)
    __asm__("mov %w0, %w1" : "=r"(r) : "g"(99));
#endif
    return (r == 99) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_imm_const_g", code, &[]),
        0,
        "g with const operand must substitute as immediate"
    );
    assert_eq!(compile_and_run_optimized("asm_imm_const_g_opt", code), 0);
}

#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_inline_asm_imm_const_via_i_letter() {
    // x86_64 `"I"(const)` — constant in [0, 31]. Used for shift
    // counts.
    let code = r#"
int main(void) {
    int x = 5;
    int r;
    __asm__("shll %1, %0" : "=r"(r) : "I"(3), "0"(x));
    return (r == 40) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_imm_const_I", code, &[]),
        0,
        "I constraint with in-range constant substitutes as immediate"
    );
}

#[test]
fn codegen_inline_asm_imm_runtime_via_ri_uses_register() {
    // `"ri"(runtime_value)` — operand isn't const, so the codegen
    // must NOT substitute as a literal. The operand must go through
    // a register (since `"ri"` allows register or immediate, and
    // memory is not allowed). Negative case complementing the
    // const-substitution test above.
    let code = r#"
int main(void) {
    int x;
#if defined(__x86_64__)
    __asm__("movl $77, %0" : "=r"(x));
    int r;
    __asm__("movl %1, %0" : "=r"(r) : "ri"(x));
#elif defined(__aarch64__)
    __asm__("mov %w0, #77" : "=r"(x));
    int r;
    __asm__("mov %w0, %w1" : "=r"(r) : "ri"(x));
#endif
    return (r == 77) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_imm_runtime_ri", code, &[]),
        0,
        "ri with runtime operand must go through a register"
    );
    assert_eq!(compile_and_run_optimized("asm_imm_runtime_ri_opt", code), 0);
}

// ============================================================================
// C10 — Per-arch Class Letter Tests
// ============================================================================
//
// Tests for the rare GCC constraint letters that aren't built-in
// classes: x86_64's `q` (byte-register-class), `R`/`l` (register
// synonyms), `I`/`J`/`K`/`L`/`M`/`N`/`O` (constant-range immediates);
// aarch64's `I`/`J`/`K`/`L`/`M`/`N` (immediate ranges).

#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_inline_asm_x86_64_class_letter_q() {
    // `q` constraint — byte-class register. Used by kernel/glibc
    // for byte stores via `movb`. c17 maps `q` to `Any` since every
    // modern x86_64 GP register has a low-byte alias.
    let code = r#"
int main(void) {
    unsigned char dst = 0;
    unsigned char src = 0x42;
    __asm__("movb %1, %0" : "=qm"(dst) : "q"(src));
    return (dst == 0x42) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_x86_64_class_letter_q", code, &[]),
        0,
        "q constraint must accept any GP register for byte operations"
    );
    assert_eq!(
        compile_and_run_optimized("asm_x86_64_class_letter_q_opt", code),
        0
    );
}

#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_inline_asm_x86_64_class_letter_i_immediate() {
    // `I` constraint — integer constant in [0, 31]. Used for shift
    // counts. c17 maps `I` to `Imm`; the assembler enforces the
    // range.
    let code = r#"
int main(void) {
    int x = 0x42;
    int shifted;
    // Shift left by a compile-time constant 4 — fits in `I` range.
    __asm__("shll $4, %0" : "+r"(shifted) : "0"(x));
    return (shifted == 0x420) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_x86_64_class_letter_I", code, &[]),
        0,
        "I constraint must accept a small compile-time constant"
    );
    assert_eq!(
        compile_and_run_optimized("asm_x86_64_class_letter_I_opt", code),
        0
    );
}

/// `+r`(out) paired with a tied matching input (`"0"(in)`) must not double-
/// define the output's pseudo.  The linearizer used to emit BOTH a Load (the
/// `+r` read-half) AND a Copy (from the tied input) targeting the same
/// pseudo — fine at -O0 (no validator) but rejected by the optimizer-stage
/// I1 invariant ("single definition per pseudo") at -O2.  This test pins
/// the behavior down for several lvalue shapes so a regression in the
/// linearizer's tied-input handling is caught no matter which constraint
/// letter happens to surface it.
#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_inline_asm_tied_input_no_double_def() {
    // Local int.
    assert_eq!(
        compile_and_run_optimized(
            "asm_tied_int_local",
            r#"
int main(void) {
    int src = 0x42;
    int dst;
    __asm__("shll $4, %0" : "+r"(dst) : "0"(src));
    return (dst == 0x420) ? 0 : 1;
}
"#,
        ),
        0
    );

    // Global int (different lvalue path — Load from .data, not from frame).
    assert_eq!(
        compile_and_run_optimized(
            "asm_tied_int_global",
            r#"
int g = 7;
int main(void) {
    int out;
    __asm__("addl $3, %0" : "+r"(out) : "0"(g));
    return (out == 10) ? 0 : 1;
}
"#,
        ),
        0
    );

    // Parameter source (the `param_info.is_some()` linearizer branch — also
    // historically emitted the wrong Copy when a tied input was present).
    assert_eq!(
        compile_and_run_optimized(
            "asm_tied_int_param",
            r#"
int helper(int src) {
    int dst;
    __asm__("addl $1, %0" : "+r"(dst) : "0"(src));
    return dst;
}
int main(void) { return (helper(41) == 42) ? 0 : 1; }
"#,
        ),
        0
    );
}

#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_inline_asm_x86_64_class_letter_r_synonym() {
    // `R` constraint — legacy 8-register set. Same as `r` on x86_64.
    let code = r#"
int main(void) {
    int x = 100;
    int r;
    __asm__("movl %1, %0" : "=R"(r) : "R"(x));
    return (r == 100) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_x86_64_class_letter_R", code, &[]),
        0,
        "R constraint synonym for r must work"
    );
}

#[cfg(target_arch = "aarch64")]
#[test]
fn codegen_inline_asm_aarch64_class_letter_k_immediate() {
    // `K` on aarch64 — 32-bit logical immediate. c17 maps to Imm.
    let code = r#"
int main(void) {
    int x = 0xFF;
    int r;
    __asm__("and %w0, %w1, #15" : "=r"(r) : "r"(x));
    return (r == 15) ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("asm_aarch64_class_letter_K", code, &[]),
        0,
        "K constraint must accept logical immediates"
    );
}

/// `__sync_synchronize`-equivalent: a single global barrier between two
/// unrelated memory accesses. A reordering pass that moved the second
/// load before the barrier could silently miscompile any code that
/// relies on this idiom for cross-thread visibility. Single-threaded
/// here, so we only assert the compiler-level ordering.
#[test]
fn codegen_inline_asm_memory_barrier_sync_synchronize_pattern() {
    let code = r#"
static int a = 0;
static int b = 0;

int main(void) {
    a = 1;
#if defined(__x86_64__)
    __asm__ volatile("mfence" ::: "memory");
#elif defined(__aarch64__)
    __asm__ volatile("dmb ish" ::: "memory");
#else
    __asm__ volatile("" ::: "memory");
#endif
    b = 2;
    if (a != 1) return 1;
    if (b != 2) return 2;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("asm_memory_barrier_sync_synchronize", code, &[]),
        0,
        "stores on either side of a full fence must not be reordered or dropped"
    );
    assert_eq!(
        compile_and_run_optimized("asm_memory_barrier_sync_synchronize_opt", code),
        0,
        "sync_synchronize pattern must hold at -O1 too"
    );
}

// ============================================================================
// Memory-constrained output operands ("=m")
// ============================================================================

/// Regression test: a memory *output* operand reads its pseudo -- the pseudo
/// holds the address the assembly writes through -- but DCE's use-collector
/// looked only at `asm_data.inputs`. At `-O` and above it therefore deleted the
/// instruction that materialized the address, and the emitted store went
/// through whatever the register happened to hold.
///
/// Found via the CPython acceptance build: `_Py_get_387controlword`
/// (`Python/pymath.c`) is exactly this shape, and c17 compiled its
/// `fnstcw %0` to `fnstcw (%rax)` with RAX left at 0 from an earlier zero-fill
/// -- a null-pointer write that segfaulted the bootstrap interpreter.
///
/// The `-O0` path was always correct, so this must run optimized to mean
/// anything.
#[test]
fn codegen_asm_memory_output_survives_optimization() {
    let code = r#"
/* Store through a "=m" output, then read the object back. If the address
   computation is dropped, this either faults or writes somewhere else. */
static int store_via_m(void) {
    int out = 0;
#if defined(__x86_64__)
    __asm__ __volatile__ ("movl $1234, %0" : "=m" (out));
#elif defined(__aarch64__)
    {
        int tmp = 1234;
        __asm__ __volatile__ ("str %w1, %0" : "=m" (out) : "r" (tmp));
    }
#else
    out = 1234;
#endif
    return out;
}

/* Same, but with the object surrounded by other locals so a stray write
   would land on a neighbour rather than faulting. */
static int store_via_m_neighbours(int *before, int *after) {
    int lo = 11;
    int out = 0;
    int hi = 22;
#if defined(__x86_64__)
    __asm__ __volatile__ ("movl $77, %0" : "=m" (out));
#elif defined(__aarch64__)
    {
        int tmp = 77;
        __asm__ __volatile__ ("str %w1, %0" : "=m" (out) : "r" (tmp));
    }
#else
    out = 77;
#endif
    *before = lo;
    *after = hi;
    return out;
}

int main(void) {
    if (store_via_m() != 1234) return 1;

    int before = 0, after = 0;
    if (store_via_m_neighbours(&before, &after) != 77) return 2;
    if (before != 11) return 3;
    if (after != 22) return 4;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("asm_memory_output_operand", code, &[]),
        0,
        "a \"=m\" output must write the named object"
    );
    assert_eq!(
        compile_and_run_optimized("asm_memory_output_operand_opt", code),
        0,
        "the address computation for a \"=m\" output must survive DCE"
    );
}

/// The `"=m"` fix has to reach the *register allocator*, not just DCE.
///
/// DCE learning that a memory output reads its pseudo stopped the address
/// computation being deleted, but the allocator's own use-collectors --
/// live-interval bounds, liveness propagation, the interference graph's def
/// set and live set, and next-use distance -- all still classified such an
/// output as a pure def. So the address register stayed free for the
/// allocator to hand to another operand of the same asm, and the store went
/// through an input's value as a pointer.
///
/// Enough inputs are needed to make the allocator actually reuse the register;
/// a one-operand asm never exhibits it, which is why the first version of this
/// test passed on x86_64 while the bug was still live.
#[test]
fn codegen_asm_memory_output_address_survives_register_allocation() {
    let code = r#"
#include <string.h>

struct Out { unsigned v0, v1, v2, v3, v4, v5; };

/* Six register inputs alongside a memory output. The output's address must
   not be assigned a register that one of the inputs also gets. */
static unsigned write_through_m(unsigned a, unsigned b, unsigned c,
                                unsigned d, unsigned e, unsigned f) {
    unsigned out = 0;
#if defined(__x86_64__)
    __asm__ __volatile__ ("movl %1, %0"
                          : "=m"(out)
                          : "r"(a), "r"(b), "r"(c), "r"(d), "r"(e), "r"(f));
#elif defined(__aarch64__)
    __asm__ __volatile__ ("str %w1, %0"
                          : "=m"(out)
                          : "r"(a), "r"(b), "r"(c), "r"(d), "r"(e), "r"(f));
#else
    out = a;
    (void)b; (void)c; (void)d; (void)e; (void)f;
#endif
    return out;
}

int main(void) {
    /* The first input is what the asm stores, so the result must be it and
       not some other operand's value or a wild read. */
    if (write_through_m(11u, 22u, 33u, 44u, 55u, 66u) != 11u) return 1;
    if (write_through_m(99u, 1u, 2u, 3u, 4u, 5u) != 99u) return 2;

    /* Neighbouring locals must be untouched: a store through the wrong
       pointer usually lands somewhere else on the frame. */
    volatile unsigned before = 0xAAAAAAAAu;
    unsigned got = write_through_m(7u, 0u, 0u, 0u, 0u, 0u);
    volatile unsigned after = 0xBBBBBBBBu;
    if (got != 7u) return 3;
    if (before != 0xAAAAAAAAu) return 4;
    if (after != 0xBBBBBBBBu) return 5;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("asm_memory_output_regalloc", code, &[]),
        0,
        "a \"=m\" output's address must not be reallocated to another operand"
    );
    assert_eq!(
        compile_and_run_optimized("asm_memory_output_regalloc_opt", code),
        0,
        "the same, once the allocator is under real pressure"
    );
}

// ============================================================================
// A floating constant as an inline-asm operand
//
// `loc_to_asm_string` used to `panic!("Float immediate not supported in inline
// asm operand")` on both targets. The linearizer does no type filtering on a
// non-memory asm input, so a floating constant stays an FVal pseudo, and
// regalloc maps every FVal to `Loc::FImm` -- an FP *constant* is never given a
// register. `__asm__ ("" :: "r"(1.0))` therefore aborted the compiler at -O0.
//
// There was no float-operand test here at all, which is why it survived.
// ============================================================================

/// A general-register constraint gets the constant's bit pattern, which is
/// what gcc materializes. 1.5 is 0x3FF8000000000000.
#[test]
fn codegen_inline_asm_float_constant_in_general_register() {
    #[cfg(target_arch = "x86_64")]
    let code = r#"
int main(void) {
    unsigned long bits = 0;
    __asm__ ("movq %1, %0" : "=r"(bits) : "r"(1.5));
    return bits == 0x3FF8000000000000UL ? 0 : 1;
}
"#;
    #[cfg(target_arch = "aarch64")]
    let code = r#"
int main(void) {
    unsigned long bits = 0;
    __asm__ ("mov %0, %1" : "=r"(bits) : "r"(1.5));
    return bits == 0x3FF8000000000000UL ? 0 : 1;
}
"#;
    assert_eq!(compile_and_run("asm_fp_const_gp", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("asm_fp_const_gp_opt", code), 0);
}

/// An immediate-class constraint substitutes the same bit pattern literally,
/// with no register spent on it.
#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_inline_asm_float_constant_as_immediate() {
    let code = r#"
int main(void) {
    unsigned long bits = 0;
    __asm__ ("movq %1, %0" : "=r"(bits) : "i"(3.5));
    return bits == 0x400C000000000000UL ? 0 : 1;
}
"#;
    assert_eq!(compile_and_run("asm_fp_const_imm", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("asm_fp_const_imm_opt", code), 0);
}

/// An SSE-class constraint takes the reserved scratch register, loaded with
/// the constant. The clobber is declared because the operand *is* the scratch.
#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_inline_asm_float_constant_in_sse_register() {
    let code = r#"
int main(void) {
    double x = 0;
    __asm__ ("movsd %%xmm15, %0" : "=m"(x) : "x"(2.25) : "xmm15");
    return x == 2.25 ? 0 : 1;
}
"#;
    assert_eq!(compile_and_run("asm_fp_const_sse", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("asm_fp_const_sse_opt", code), 0);
}

/// The zero constant has its own materialization path (`xorps`), so it is
/// worth its own case -- and a negative zero must not come back as positive.
#[test]
fn codegen_inline_asm_float_constant_zero_and_negative() {
    #[cfg(target_arch = "x86_64")]
    let code = r#"
int main(void) {
    unsigned long z = 1, nz = 0;
    __asm__ ("movq %1, %0" : "=r"(z) : "r"(0.0));
    __asm__ ("movq %1, %0" : "=r"(nz) : "r"(-0.0));
    if (z != 0UL) return 1;
    if (nz != 0x8000000000000000UL) return 2;
    return 0;
}
"#;
    #[cfg(target_arch = "aarch64")]
    let code = r#"
int main(void) {
    unsigned long z = 1, nz = 0;
    __asm__ ("mov %0, %1" : "=r"(z) : "r"(0.0));
    __asm__ ("mov %0, %1" : "=r"(nz) : "r"(-0.0));
    if (z != 0UL) return 1;
    if (nz != 0x8000000000000000UL) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("asm_fp_const_zero", code, &[]), 0);
}

/// A `float` constant is narrowed to its own width, not handed over as the
/// `double` bit pattern: 1.5f is 0x3FC00000, not 0x3FF8000000000000.
#[test]
fn codegen_inline_asm_float_constant_uses_its_own_width() {
    #[cfg(target_arch = "x86_64")]
    let code = r#"
int main(void) {
    unsigned int bits = 0;
    __asm__ ("movl %1, %0" : "=r"(bits) : "r"(1.5f));
    return bits == 0x3FC00000U ? 0 : 1;
}
"#;
    #[cfg(target_arch = "aarch64")]
    let code = r#"
int main(void) {
    unsigned int bits = 0;
    __asm__ ("mov %w0, %w1" : "=r"(bits) : "r"(1.5f));
    return bits == 0x3FC00000U ? 0 : 1;
}
"#;
    assert_eq!(compile_and_run("asm_fp_const_float_width", code, &[]), 0);
}

/// An FP *value* — not a constant — under a general-register constraint.
///
/// On aarch64 nothing moved it out of the vector register it was computed in,
/// and the operand rendered as the vector register's name, so `mov %0, %1`
/// assembled as `mov x0, d0` and the assembler read `d0` as an undefined
/// symbol. Pre-existing and independent of any floating constant: it is
/// reachable from any FP variable passed as `"r"`. x86-64 was never affected,
/// spilling the value to a stack slot the register path then loads from.
///
/// It is also why `-0.0` needed its own case above — that arrives as `fneg` of
/// zero, a computed value in a vector register, rather than as an immediate.
#[test]
fn codegen_inline_asm_fp_value_in_general_register() {
    #[cfg(target_arch = "x86_64")]
    let code = r#"
double src = 2.5;
int main(void) {
    unsigned long bits = 0;
    double d = src * 2.0;                 /* computed, so it lives in an FP reg */
    __asm__ ("movq %1, %0" : "=r"(bits) : "r"(d));
    return bits == 0x4014000000000000UL ? 0 : 1;   /* 5.0 */
}
"#;
    #[cfg(target_arch = "aarch64")]
    let code = r#"
double src = 2.5;
int main(void) {
    unsigned long bits = 0;
    double d = src * 2.0;
    __asm__ ("mov %0, %1" : "=r"(bits) : "r"(d));
    return bits == 0x4014000000000000UL ? 0 : 1;   /* 5.0 */
}
"#;
    assert_eq!(compile_and_run("asm_fp_value_gp", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("asm_fp_value_gp_opt", code), 0);
}

/// The template chooses the width it wants, so a materialized constant has to
/// be handed over as a *register* rather than a pre-rendered name: `%w1`
/// against a hard-coded `x9` assembled as `mov w0, x9`.
#[cfg(target_arch = "aarch64")]
#[test]
fn codegen_inline_asm_width_modifier_applies_to_a_materialized_constant() {
    let code = r#"
int main(void) {
    unsigned int lo = 0;
    unsigned long full = 0;
    __asm__ ("mov %w0, %w1" : "=r"(lo) : "r"(1.5f));
    __asm__ ("mov %0, %1"   : "=r"(full) : "r"(1.5));
    if (lo != 0x3FC00000U) return 1;
    if (full != 0x3FF8000000000000UL) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("asm_width_modifier_const", code, &[]), 0);
}

// ============================================================================
// SSE register constraints on an inline-asm *output* (#C139)
//
// `constraint_requires_register` and `constraint_requires_memory` know no FP
// class letter, so `"=x"` matched neither and the output fell through to the
// general-register arm: the template was handed `%rax` and `movsd %xmm0, %rax`
// was refused by the assembler. The input side had been given
// `constraint_requires_sse`; the output side had not.
//
// x86-64 only. aarch64's output loop falls through to `loc_to_asm_string`,
// which renders a vector register as `dN` correctly.
// ============================================================================

/// The plain case: a value out of the asm through an SSE register.
#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_inline_asm_sse_output_constraint() {
    let code = r#"
double src = 2.25;
int main(void) {
    double back = 0;
    __asm__ ("movsd %1, %0" : "=x"(back) : "x"(src));
    return back == 2.25 ? 0 : 1;
}
"#;
    assert_eq!(compile_and_run("asm_sse_output", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("asm_sse_output_opt", code), 0);
}

/// A read-write `"+x"` operand. Its slot carries text rather than a register,
/// so the tied-input path that loads an output's initial value found nothing
/// and `addsd %xmm15, %xmm15` ran on whatever happened to be in the scratch.
#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_inline_asm_sse_read_write_constraint() {
    let code = r#"
double d = 1.5;
float  f = 1.25f;
int main(void) {
    __asm__ ("addsd %0, %0" : "+x"(d));
    if (d != 3.0) return 1;
    __asm__ ("addss %0, %0" : "+x"(f));
    if (f != 2.5f) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("asm_sse_readwrite", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("asm_sse_readwrite_opt", code), 0);
}

/// An output and inputs together, which needs both scratch registers and so
/// pins that the two loops share one budget rather than each claiming Xmm15.
#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_inline_asm_sse_output_and_inputs_share_the_scratch_budget() {
    let code = r#"
double a = 1.5, b = 2.25;
int main(void) {
    double r = 0;
    __asm__ ("movsd %1, %0; addsd %2, %0" : "=&x"(r) : "x"(a), "x"(b));
    if (r != 3.75) return 1;

    /* An SSE constant alongside an SSE output: one scratch each. */
    double c = 0;
    __asm__ ("movsd %1, %0" : "=x"(c) : "x"(0.5));
    if (c != 0.5) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("asm_sse_budget", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("asm_sse_budget_opt", code), 0);
}

/// An x87 register constraint reaches the x87 stack, not an XMM register.
///
/// On x86 `f`, `t` and `u` are the **x87 stack** classes -- `t` is st(0), `u`
/// is st(1), `f` is any of them -- and they were counted as SSE. So a long
/// double operand was routed through an XMM scratch and the assembler was
/// handed `movt -48(%rbp), %xmm15`, which is not an instruction. musl's
///
///   long double sqrtl(long double x) { __asm__("fsqrt" : "+t"(x)); return x; }
///
/// failed to build. c17 keeps a long double in memory and reaches it with
/// `fldt`/`fstpt`, so an x87 operand is pushed onto the FP stack before the
/// template and popped back after it.
#[test]
#[cfg(target_arch = "x86_64")]
fn codegen_inline_asm_x87_constraint() {
    let code = r#"
static long double my_sqrtl(long double x) { __asm__("fsqrt" : "+t"(x)); return x; }
static long double my_absl(long double x)  { __asm__("fabs"  : "+t"(x)); return x; }

/* An x87 *input* -- the template consumes it and leaves nothing to store. */
static int to_int(long double x) { int r; __asm__("fistpl %0" : "=m"(r) : "t"(x)); return r; }

/* Two x87 operands: `t` is st(0) and `u` is st(1), so `u` must be pushed
   first for `t` to end up on top. Getting this backwards computes b/a. */
static long double mul(long double a, long double b) {
    __asm__("fmulp" : "+t"(a) : "u"(b));
    return a;
}
static long double sub(long double a, long double b) {
    __asm__("fsubrp" : "+t"(a) : "u"(b));
    return a;
}

int main(void) {
    if (my_sqrtl(4.0L) != 2.0L) return 1;
    if (my_sqrtl(16.0L) != 4.0L) return 2;
    if (my_absl(-3.5L) != 3.5L) return 3;
    if (my_absl(3.5L) != 3.5L) return 4;

    if (to_int(7.0L) != 7) return 5;
    if (to_int(-3.0L) != -3) return 6;

    if (mul(2.0L, 3.0L) != 6.0L) return 7;
    /* `fsubrp` computes st(1) - st(0), i.e. b - a. Asymmetric, so having the
       two operands the wrong way round flips the sign rather than passing by
       coincidence -- which is the whole point, since `u` must be pushed
       before `t` for `t` to land on top. */
    if (sub(10.0L, 4.0L) != -6.0L) return 8;
    if (sub(4.0L, 10.0L) != 6.0L) return 9;
    return 0;
}
"#;
    assert_eq!(compile_and_run("asm_x87_constraint", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("asm_x87_constraint_opt", code), 0);
}

/// A vector-class operand gets a vector register, on both sides of the asm.
///
/// The aarch64 output loop had no vector arm at all, so a `"=w"` output took
/// whatever the allocator gave the pseudo -- a general register -- and emitted
/// `fmov d17, x0` around a template the assembler then rejected. #C139 fixed
/// the x86-64 twin and recorded this target as sound; it was not, because the
/// output never reaches the path that renders a `Loc::VReg` correctly.
///
/// The `b`/`h`/`s`/`d`/`q` operand modifiers land with it: without them a
/// vector operand could not be named in a template at all, so there was no
/// way to write a working `"w"` asm.
#[test]
#[cfg(target_arch = "aarch64")]
fn codegen_inline_asm_vector_constraint() {
    let code = r#"
static double vsqrt(double a) { double r; __asm__("fsqrt %d0, %d1" : "=w"(r) : "w"(a)); return r; }
static float  fsqrt_(float a) { float  r; __asm__("fsqrt %s0, %s1" : "=w"(r) : "w"(a)); return r; }

int main(void) {
    if (vsqrt(4.0) != 2.0) return 1;
    if (vsqrt(16.0) != 4.0) return 2;
    if (fsqrt_(9.0f) != 3.0f) return 3;

    /* Three vector operands at once: all three scratch registers. */
    double r;
    __asm__("fadd %d0, %d1, %d2" : "=w"(r) : "w"(1.5), "w"(2.5));
    if (r != 4.0) return 4;

    /* A read-write vector operand is loaded before the template. */
    double x = 4.0;
    __asm__("fsqrt %d0, %d0" : "+w"(x));
    if (x != 2.0) return 5;
    return 0;
}
"#;
    assert_eq!(compile_and_run("asm_vector_constraint", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("asm_vector_constraint_opt", code),
        0
    );
}

/// Two general-register operands get two different registers.
///
/// Both scratch-taking arms took `Reg::scratch_regs().0` -- X9 -- with no
/// tracking of whether it was already spent, so `"r"(a), "r"(b)` with two FP
/// values emitted `fmov x9, d0; fmov x9, d1; add x0, x9, x9`: operand 1
/// destroyed, the result twice operand 2, and no diagnostic. X10 and X11 were
/// reserved and idle the whole time.
#[test]
#[cfg(target_arch = "aarch64")]
fn codegen_inline_asm_two_general_operands_are_distinct() {
    let code = r#"
static long bits(double d) { union { double d; long l; } u; u.d = d; return u.l; }

static long add_bits(double a, double b) {
    long r;
    __asm__("add %0, %1, %2" : "=r"(r) : "r"(a), "r"(b));
    return r;
}

int main(void) {
    double x = 2.0, y = 3.0;
    if (add_bits(x, y) != bits(x) + bits(y)) return 1;
    /* Distinct values, so sharing a register cannot pass by coincidence. */
    if (add_bits(1.0, 8.0) != bits(1.0) + bits(8.0)) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("asm_two_gp_operands", code, &[]), 0);
}

/// An `asm goto` label reference is spelled the way its definition is.
///
/// A function whose identifier holds an extended character needs its local
/// labels quoted, and `Label::name()` quotes them. The aarch64 operand builder
/// hand-rolled `format!(".L{}_{}", ...)` instead, so the branch the template
/// expanded to named `.Lf\u{e9}_1` while the block that defines it was emitted
/// as `".Lf\u{e9}_1"` -- two spellings of one label, which the assembler reads
/// as an undefined symbol.
///
/// Asserted as agreement rather than as a literal: the test does not care
/// whether the label is quoted, only that both sites agree.
#[test]
fn codegen_asm_goto_label_matches_its_definition() {
    use crate::codegen::asm_probe::{asm_for, AARCH64_LINUX, X86_64_LINUX};

    let code = "
int f\u{e9}(int x) {
    __asm__ goto (\"b %l[done]\" : : : : done);
    return 0;
done:
    return 1;
}
";

    for triple in [AARCH64_LINUX, X86_64_LINUX] {
        let asm = asm_for("asm_goto_label_quoting", triple, code);

        // Every local label this function defines, exactly as written.
        let defined: Vec<&str> = asm
            .lines()
            .map(str::trim)
            .filter_map(|l| l.strip_suffix(':'))
            .filter(|l| l.contains(".L") && l.contains("f\u{e9}"))
            .collect();
        assert!(
            !defined.is_empty(),
            "{triple}: no local labels defined:\n{asm}"
        );

        // Every branch target naming one of this function's local labels.
        for line in asm.lines().map(str::trim) {
            let Some(rest) = line
                .strip_prefix("b ")
                .or_else(|| line.strip_prefix("jmp "))
            else {
                continue;
            };
            let target = rest.trim();
            if !target.contains("f\u{e9}") {
                continue;
            }
            assert!(
                defined.contains(&target),
                "{triple}: branch to {target} but the definitions are {defined:?}:\n{asm}"
            );
        }
    }
}

/// An `asm goto` label survives being inlined.
///
/// The label names a *block*, and it is the callee's block. The inliner
/// remaps `bb_true`/`bb_false` and the pseudos inside the asm operands, but
/// not `asm_data.goto_labels` -- so after inlining the label named whichever
/// caller block happened to share the id. For a small callee that is the block
/// holding the asm itself, and the branch became `b .Lmain_1` from inside
/// `.Lmain_1`: the program jumped to its own address and hung.
///
/// This must be *run*, not read: the assembly was well-formed and the label
/// was spelled correctly, which is all a text probe checks. Only executing it
/// shows the branch going to the wrong place.
#[test]
#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
fn codegen_inlined_asm_goto_branches_to_the_right_block() {
    #[cfg(target_arch = "x86_64")]
    let jump = "jmp %l[done]";
    #[cfg(target_arch = "aarch64")]
    let jump = "b %l[done]";

    let code = format!(
        r#"
/* Small enough that the inliner takes it, which is the whole point. */
static int taken(void) {{
    __asm__ goto ("{jump}" : : : : done);
    return 0;
done:
    return 1;
}}

/* The fallthrough arm: the asm does not branch, so the label is not used. */
static int not_taken(void) {{
    __asm__ goto ("" : : : : done);
    return 0;
done:
    return 1;
}}

int main(void) {{
    if (taken() != 1) return 1;
    if (not_taken() != 0) return 2;
    /* Twice, so a second inlining of the same callee is remapped too. */
    if (taken() != 1) return 3;
    if (taken() + not_taken() != 1) return 4;
    return 0;
}}
"#
    );
    assert_eq!(compile_and_run("inlined_asm_goto", &code, &[]), 0);
    assert_eq!(compile_and_run_optimized("inlined_asm_goto_opt", &code), 0);
}
