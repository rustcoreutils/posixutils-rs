//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly and compile-only cases of tests/builtins/gnu_atomics.rs, in process.
//

use crate::test_compile::{asm_for, compile_expect_no_diagnostic, compile_expect_warning};

/// The c17 options that compile for linux-aarch64. (The integration suite's
/// copy adds an `-isystem` for the target's headers, which none of these
/// cases includes.)
const AARCH64_TARGET_ARGS: [&str; 1] = ["--target=aarch64-unknown-linux-gnu"];

/// `int f(int *p) { return __atomic_load_n(p, ORDER); }` for one `ORDER`,
/// compiled at -O2 for `target`, with its instruction lines only.
fn atomic_asm(target: &[&str], body: &str) -> String {
    let mut args = vec!["-O2"];
    args.extend_from_slice(target);
    asm_for("atomic_order", body, &args)
        .lines()
        .filter(|l| !l.trim_start().starts_with('.'))
        .collect::<Vec<_>>()
        .join("\n")
}

/// An `__atomic_*` operation is only as ordered as it is asked to be. On
/// aarch64, c17 emitted the acquire and release forms -- `ldar`, `stlr`,
/// `ldaxr`/`stlxr` -- whatever the order, so a relaxed counter paid for two
/// barriers per update. gcc emits `ldr`, `str` and `ldxr`/`stxr` for relaxed,
/// and the acquire or release half alone where only that is asked for.
#[test]
fn builtins_aarch64_atomics_honour_their_memory_order() {
    let a64 = &AARCH64_TARGET_ARGS[..];
    for (body, present, absent) in [
        (
            "int f(int *p) { return __atomic_load_n(p, __ATOMIC_RELAXED); }",
            "ldr",
            &["ldar"][..],
        ),
        (
            "int f(int *p) { return __atomic_load_n(p, __ATOMIC_ACQUIRE); }",
            "ldar",
            &[][..],
        ),
        (
            "void f(int *p, int v) { __atomic_store_n(p, v, __ATOMIC_RELAXED); }",
            "str",
            &["stlr"][..],
        ),
        (
            "void f(int *p, int v) { __atomic_store_n(p, v, __ATOMIC_RELEASE); }",
            "stlr",
            &[][..],
        ),
        (
            "int f(int *p, int v) { return __atomic_exchange_n(p, v, __ATOMIC_RELAXED); }",
            "ldxr",
            &["ldaxr", "stlxr"][..],
        ),
        (
            "int f(int *p, int v) { return __atomic_fetch_add(p, v, __ATOMIC_RELAXED); }",
            "ldxr",
            &["ldaxr", "stlxr"][..],
        ),
        (
            "int f(int *p, int v) { return __atomic_fetch_add(p, v, __ATOMIC_ACQUIRE); }",
            "ldaxr",
            &["stlxr"][..],
        ),
        (
            "int f(int *p, int v) { return __atomic_fetch_add(p, v, __ATOMIC_SEQ_CST); }",
            "stlxr",
            &[][..],
        ),
    ] {
        let asm = atomic_asm(a64, body);
        let has = |m: &str| asm.lines().any(|l| l.split_whitespace().next() == Some(m));
        assert!(has(present), "{body}: expected `{present}`:\n{asm}");
        for m in absent {
            assert!(!has(m), "{body}: `{m}` is stronger than asked:\n{asm}");
        }
    }
}

/// A compare-exchange runs one instruction sequence for both outcomes, so
/// its two orders combine into one, as gcc combines them: a failure order
/// stronger than the success order makes it seq-cst, and a release that
/// acquires on failure is acq-rel. The orders are gcc's choices, read off
/// `aarch64-linux-gnu-gcc -O2 -mno-outline-atomics`.
#[test]
fn builtins_aarch64_compare_exchange_combines_its_orders() {
    let a64 = &AARCH64_TARGET_ARGS[..];
    for (success, failure, load, store) in [
        ("RELAXED", "RELAXED", "ldxr", "stxr"),
        ("CONSUME", "RELAXED", "ldaxr", "stxr"),
        ("ACQUIRE", "ACQUIRE", "ldaxr", "stxr"),
        ("RELEASE", "RELAXED", "ldxr", "stlxr"),
        ("RELEASE", "ACQUIRE", "ldaxr", "stlxr"),
        ("RELAXED", "ACQUIRE", "ldaxr", "stlxr"),
        ("ACQUIRE", "SEQ_CST", "ldaxr", "stlxr"),
        ("ACQ_REL", "RELAXED", "ldaxr", "stlxr"),
        ("SEQ_CST", "RELAXED", "ldaxr", "stlxr"),
    ] {
        let body = format!(
            "int f(int *p, int *e, int d) {{ return __atomic_compare_exchange_n(\
             p, e, d, 0, __ATOMIC_{success}, __ATOMIC_{failure}); }}"
        );
        let asm = atomic_asm(a64, &body);
        let ops: Vec<_> = asm
            .lines()
            .filter_map(|l| l.split_whitespace().next())
            .filter(|m| m.contains("xr"))
            .collect();
        assert_eq!(ops, [load, store], "{success}/{failure}:\n{asm}");
    }
}

/// A fence emits what gcc emits for its order. On aarch64 a release fence is
/// a full `dmb ish`: it must keep earlier *loads* before later stores too,
/// which the `dmb ishst` c17 emitted (stores only) does not. On x86-64 only
/// seq-cst costs an instruction; c17 emitted `lfence`/`sfence`, which order
/// nothing an acquire or release fence needs.
#[test]
fn builtins_thread_fences_follow_their_order() {
    let host: &[&str] = &[];
    let a64 = &AARCH64_TARGET_ARGS[..];
    let fence = |target: &[&str], order: &str| {
        let body = format!("void f(void) {{ __atomic_thread_fence(__ATOMIC_{order}); }}");
        atomic_asm(target, &body)
            .lines()
            .map(str::trim)
            .filter(|l| l.contains("fence") || l.starts_with("dmb"))
            .map(|l| l.split_whitespace().collect::<Vec<_>>().join(" "))
            .collect::<Vec<_>>()
    };
    for (order, barrier) in [
        ("RELAXED", None),
        ("CONSUME", Some("dmb ishld")),
        ("ACQUIRE", Some("dmb ishld")),
        ("RELEASE", Some("dmb ish")),
        ("ACQ_REL", Some("dmb ish")),
        ("SEQ_CST", Some("dmb ish")),
    ] {
        assert_eq!(
            fence(a64, order),
            Vec::from_iter(barrier),
            "aarch64 {order}"
        );
    }
    if cfg!(target_arch = "x86_64") {
        for order in ["RELAXED", "CONSUME", "ACQUIRE", "RELEASE", "ACQ_REL"] {
            assert!(fence(host, order).is_empty(), "x86-64 {order}");
        }
        assert_eq!(fence(host, "SEQ_CST"), ["mfence"]);
    }
}

/// An order the operation cannot have -- a release load, an acquire store --
/// is answered with seq-cst and gcc's `-Winvalid-memory-model` warning, as
/// gcc answers it; so are a failure order that is a release or stronger than
/// the success order.
#[test]
fn builtins_atomics_answer_an_invalid_order_with_seq_cst() {
    let a64 = &AARCH64_TARGET_ARGS[..];
    let load = atomic_asm(
        a64,
        "int f(int *p) { return __atomic_load_n(p, __ATOMIC_RELEASE); }",
    );
    assert!(load.contains("ldar"), "{load}");
    let store = atomic_asm(
        a64,
        "void f(int *p) { __atomic_store_n(p, 1, __ATOMIC_ACQUIRE); }",
    );
    assert!(store.contains("stlr"), "{store}");
    if cfg!(target_arch = "x86_64") {
        let host: &[&str] = &[];
        let store = atomic_asm(
            host,
            "void f(char *p) { __atomic_clear(p, __ATOMIC_ACQ_REL); }",
        );
        assert!(store.contains("xchg"), "{store}");
    }

    for (body, warning) in [
        (
            "int f(int *p) { return __atomic_load_n(p, __ATOMIC_RELEASE); }",
            "invalid memory model 'memory_order_release' for an atomic load",
        ),
        (
            "void f(int *p) { __atomic_store_n(p, 1, __ATOMIC_CONSUME); }",
            "invalid memory model 'memory_order_consume' for an atomic store",
        ),
        (
            "int f(int *p) { return __atomic_load_n(p, 9); }",
            "invalid memory model 9 for an atomic load",
        ),
        (
            "int f(int *p, int *e) { return __atomic_compare_exchange_n(p, e, 1, 0, \
             __ATOMIC_SEQ_CST, __ATOMIC_RELEASE); }",
            "invalid failure memory model 'memory_order_release'",
        ),
        (
            "int f(int *p, int *e) { return __atomic_compare_exchange_n(p, e, 1, 0, \
             __ATOMIC_RELAXED, __ATOMIC_ACQUIRE); }",
            "failure memory model 'memory_order_acquire' cannot be stronger than \
             success memory model 'memory_order_relaxed'",
        ),
    ] {
        compile_expect_warning("atomic_invalid_order", body, warning);
    }
    compile_expect_no_diagnostic(
        "atomic_valid_order",
        "int f(int *p, int *e, int o) { return __atomic_load_n(p, o) \
         + __atomic_compare_exchange_n(p, e, 1, 0, __ATOMIC_RELEASE, __ATOMIC_ACQUIRE); }",
        "memory model",
    );
}

/// `__atomic_signal_fence` orders only against a signal handler on the same
/// thread, which needs the compiler not to move memory accesses across it and
/// no instruction at all; gcc emits nothing. c17 emitted a full hardware
/// fence (`mfence`, `dmb ish`). The thread fence keeps its instruction.
#[test]
fn builtins_signal_fence_emits_no_instruction() {
    let host: &[&str] = &[];
    for target in [host, &AARCH64_TARGET_ARGS[..]] {
        let sig = atomic_asm(
            target,
            "void f(void) { __atomic_signal_fence(__ATOMIC_SEQ_CST); }",
        );
        assert!(
            !sig.contains("mfence") && !sig.contains("dmb"),
            "signal fence emitted a hardware fence:\n{sig}"
        );
        let thr = atomic_asm(
            target,
            "void f(void) { __atomic_thread_fence(__ATOMIC_SEQ_CST); }",
        );
        assert!(
            thr.contains("mfence") || thr.contains("dmb"),
            "thread fence lost its instruction:\n{thr}"
        );
    }
}
