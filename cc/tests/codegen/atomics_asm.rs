//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Atomicity is invisible to a program's exit status.
//
// `_Atomic int x; x = 100;` reads back 100 whether or not the store was
// atomic, which is why cc/tests/c11/atomics.rs passed for years against plain
// `movl`. These tests look at the instructions instead, on both architectures
// from whichever host is running.
//

use super::asm_probe::{
    asm_for, assert_body_contains, assert_body_lacks, count_in_body, AARCH64_LINUX, X86_64_LINUX,
};

/// Source exercising each operand width through the RMW path.
const NARROW_SRC: &str = r#"
#include <stdatomic.h>
_Atomic unsigned char b;
_Atomic unsigned short h;
_Atomic unsigned int w;
_Atomic unsigned long q;
unsigned char add_b(void) { return __c11_atomic_fetch_add(&b, 1, __ATOMIC_SEQ_CST); }
unsigned short add_h(void) { return __c11_atomic_fetch_add(&h, 1, __ATOMIC_SEQ_CST); }
unsigned int add_w(void) { return __c11_atomic_fetch_add(&w, 1, __ATOMIC_SEQ_CST); }
unsigned long add_q(void) { return __c11_atomic_fetch_add(&q, 1, __ATOMIC_SEQ_CST); }
"#;

/// The memory operand must match the object's width. Widening a byte RMW to
/// 32 bits made it read and write the three adjacent bytes.
#[test]
fn codegen_atomic_rmw_operand_width_matches_the_object() {
    let asm = asm_for("atomic_width_x86", X86_64_LINUX, NARROW_SRC);
    assert_body_contains(&asm, "add_b", "xaddb", "an 8-bit atomic add needs xaddb");
    assert_body_lacks(
        &asm,
        "add_b",
        "xaddl",
        "a 32-bit xadd on a byte object rewrites its neighbours",
    );
    assert_body_contains(&asm, "add_h", "xaddw", "a 16-bit atomic add needs xaddw");
    assert_body_lacks(&asm, "add_h", "xaddl", "same for 16 bits");
    assert_body_contains(&asm, "add_w", "xaddl", "32-bit stays 32-bit");
    assert_body_contains(&asm, "add_q", "xaddq", "64-bit stays 64-bit");

    // aarch64 was already correct; pin it so it stays that way.
    let asm = asm_for("atomic_width_arm", AARCH64_LINUX, NARROW_SRC);
    assert_body_contains(&asm, "add_b", "ldaxrb", "8-bit load-exclusive");
    assert_body_contains(&asm, "add_b", "stlxrb", "8-bit store-exclusive");
    assert_body_contains(&asm, "add_h", "ldaxrh", "16-bit load-exclusive");
    assert_body_contains(&asm, "add_q", "ldaxr", "64-bit load-exclusive");
}

/// Every atomic operation must carry a real hardware guarantee, and no
/// non-atomic operation should.
#[test]
fn codegen_atomic_operations_emit_locking_instructions() {
    let src = r#"
#include <stdatomic.h>
atomic_int g;
int fetch_add(void) { return __c11_atomic_fetch_add(&g, 1, __ATOMIC_SEQ_CST); }
int fetch_and(void) { return __c11_atomic_fetch_and(&g, 3, __ATOMIC_SEQ_CST); }
int exchange(void)  { return __c11_atomic_exchange(&g, 5, __ATOMIC_SEQ_CST); }
void store(void)    { __c11_atomic_store(&g, 5, __ATOMIC_SEQ_CST); }
int load(void)      { return __c11_atomic_load(&g, __ATOMIC_SEQ_CST); }

/* Control: the same shape on a plain int must stay unsynchronized. */
int plain;
int plain_add(void) { int old = plain; plain = old + 1; return old; }
"#;

    let asm = asm_for("atomic_ops_x86", X86_64_LINUX, src);
    assert_body_contains(&asm, "fetch_add", "lock xadd", "RMW must be locked");
    assert_body_contains(
        &asm,
        "fetch_and",
        "lock cmpxchg",
        "a bit op with no native form uses a locked CAS loop",
    );
    // The CAS loop must actually loop.
    assert_body_contains(&asm, "fetch_and", "jne", "the CAS loop needs a back edge");
    assert_body_contains(&asm, "exchange", "xchg", "xchg is implicitly locked on x86");
    assert_body_contains(
        &asm,
        "store",
        "xchg",
        "a seq_cst store needs xchg, not a plain mov",
    );
    // A plain aligned load is already seq_cst on x86's memory model.
    assert_body_contains(&asm, "load", "mov", "a seq_cst load is a plain mov on x86");
    assert_body_lacks(
        &asm,
        "plain_add",
        "lock",
        "a non-atomic increment must not be locked",
    );

    let asm = asm_for("atomic_ops_arm", AARCH64_LINUX, src);
    assert_body_contains(&asm, "load", "ldar", "a seq_cst load needs ldar on aarch64");
    assert_body_contains(&asm, "store", "stlr", "a seq_cst store needs stlr");
    assert_body_contains(&asm, "fetch_add", "ldaxr", "RMW is an LL/SC loop");
    assert_body_contains(&asm, "fetch_add", "stlxr", "RMW is an LL/SC loop");
    assert_body_contains(
        &asm,
        "fetch_add",
        "cbnz",
        "the LL/SC loop needs a back edge",
    );
    assert_body_lacks(
        &asm,
        "plain_add",
        "ldaxr",
        "a non-atomic increment must not be exclusive",
    );
    assert_body_lacks(&asm, "plain_add", "ldar", "nor acquire-ordered");
}

/// An LL/SC loop must contain exactly one load-exclusive / store-exclusive
/// pair. More than one means a nested or duplicated loop, which cannot make
/// forward progress reliably.
#[test]
fn codegen_atomic_rmw_has_one_exclusive_pair() {
    let src = r#"
#include <stdatomic.h>
atomic_int g;
int f(void) { return __c11_atomic_fetch_or(&g, 8, __ATOMIC_SEQ_CST); }
"#;
    let asm = asm_for("atomic_pair", AARCH64_LINUX, src);
    assert_eq!(
        count_in_body(&asm, "f", "ldaxr"),
        1,
        "expected exactly one load-exclusive in:\n{}",
        super::asm_probe::body_of(&asm, "f")
    );
    assert_eq!(
        count_in_body(&asm, "f", "stlxr"),
        1,
        "expected exactly one store-exclusive in:\n{}",
        super::asm_probe::body_of(&asm, "f")
    );
}

/// The audit item this whole series exists for: `_Atomic` accessed through
/// *ordinary operators* must produce the same hardware guarantees as the
/// `<stdatomic.h>` builtins. It used to emit a plain load and a plain store.
#[test]
fn codegen_atomic_operators_are_atomic() {
    let src = r#"
#include <stdatomic.h>
atomic_int g;
void assign(void)   { g = 5; }
void add(void)      { g += 3; }
void sub(void)      { g -= 3; }
void and_(void)     { g &= 7; }
void mul(void)      { g *= 3; }
void shl(void)      { g <<= 2; }
void preinc(void)   { ++g; }
void postinc(void)  { g++; }
void predec(void)   { --g; }
int  read(void)     { return g; }

/* Control: identical shapes on a plain int. */
int plain;
void plain_assign(void) { plain = 5; }
void plain_add(void)    { plain += 3; }
void plain_inc(void)    { plain++; }
int  plain_read(void)   { return plain; }
"#;

    let asm = asm_for("atomic_ops_operators_x86", X86_64_LINUX, src);

    // A sequentially consistent store is an exchange, not a mov.
    assert_body_contains(&asm, "assign", "xchg", "`g = 5` must be a seq_cst store");
    // Native read-modify-write forms.
    for f in ["add", "sub", "preinc", "postinc", "predec"] {
        assert_body_contains(&asm, f, "lock xadd", "must be one locked RMW");
    }
    // Operators with no native atomic form use a locked CAS loop.
    for f in ["and_", "mul", "shl"] {
        assert_body_contains(&asm, f, "lock cmpxchg", "must use a locked CAS loop");
    }

    // The controls must stay unsynchronized, or the assertions above prove
    // nothing about `_Atomic` in particular.
    for f in ["plain_assign", "plain_add", "plain_inc"] {
        assert_body_lacks(&asm, f, "lock", "a plain int must not be locked");
        assert_body_lacks(&asm, f, "xchg", "nor exchanged");
    }

    let asm = asm_for("atomic_ops_operators_arm", AARCH64_LINUX, src);
    assert_body_contains(&asm, "assign", "stlr", "seq_cst store on aarch64");
    assert_body_contains(&asm, "read", "ldar", "seq_cst load on aarch64");
    for f in ["add", "sub", "preinc", "postinc", "mul", "and_"] {
        assert_body_contains(&asm, f, "ldaxr", "RMW is an LL/SC loop");
        assert_body_contains(&asm, f, "stlxr", "RMW is an LL/SC loop");
    }
    assert_body_lacks(
        &asm,
        "plain_add",
        "ldaxr",
        "a plain int must not be exclusive",
    );
    assert_body_lacks(&asm, "plain_read", "ldar", "nor acquire-ordered");
}

/// A floating-point atomic has no native RMW anywhere, so it must go through
/// the CAS loop -- and the value has to survive the trip between the FP and
/// general-purpose register files, which is where it used to become zero.
#[test]
fn codegen_atomic_float_operators_use_a_cas_loop() {
    let src = r#"
#include <stdatomic.h>
_Atomic double d;
_Atomic float f;
void add_d(void) { d += 1.5; }
void add_f(void) { f += 1.5f; }
"#;
    let asm = asm_for("atomic_float_x86", X86_64_LINUX, src);
    assert_body_contains(&asm, "add_d", "lock cmpxchg", "FP RMW needs a locked CAS");
    assert_body_contains(&asm, "add_f", "lock cmpxchg", "FP RMW needs a locked CAS");
    // The bit pattern must move between the register files rather than being
    // replaced with a zero immediate.
    assert_body_contains(
        &asm,
        "add_d",
        "movq",
        "the double's bits must cross to a GP register",
    );

    let asm = asm_for("atomic_float_arm", AARCH64_LINUX, src);
    assert_body_contains(&asm, "add_d", "ldaxr", "FP RMW is an LL/SC loop");
    assert_body_contains(&asm, "add_d", "stlxr", "FP RMW is an LL/SC loop");
}
