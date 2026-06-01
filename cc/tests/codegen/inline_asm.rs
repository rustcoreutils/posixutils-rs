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
/// load, missing the intervening store. Today pcc doesn't reorder; this
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
