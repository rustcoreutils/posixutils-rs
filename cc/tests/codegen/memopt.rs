//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// End-to-end tests for the memory analyses: escape, aliasing, and
// store-to-load forwarding.
//
// These are almost all *negative*. Forwarding a value that was still correct
// changes nothing observable, so a test that only checks the optimized answer
// proves very little; what proves something is a program whose answer changes
// if the pass forwards one byte it should not have.
//

use crate::common::compile_and_run;

fn at_o2(name: &str, code: &str) -> i32 {
    compile_and_run(name, code, &["-O2".to_string()])
}

fn at_o2_no_inline(name: &str, code: &str) -> i32 {
    compile_and_run(name, code, &["-O2".to_string(), "-fno-inline".to_string()])
}

/// A callee cannot write a local whose address never left the function --
/// the rule that lets a store be forwarded across a call to an entirely
/// unknown function, with no purity attribute anywhere.
#[test]
fn memopt_a_call_cannot_write_a_local_it_was_never_given() {
    let code = r#"
extern int opaque(int);
int main(void) {
    int a[4];
    a[0] = 11;
    a[1] = 22;
    opaque(0);
    if (a[0] != 11 || a[1] != 22) return 1;
    return 0;
}
int opaque(int x) { return x; }
"#;
    assert_eq!(at_o2("memopt_call_vs_local", code), 0);
    assert_eq!(at_o2_no_inline("memopt_call_vs_local_ni", code), 0);
}

/// The same shape once the address *has* left: the callee writes through it,
/// and the value read afterwards is the callee's.
#[test]
fn memopt_a_call_given_the_address_does_write_it() {
    let code = r#"
extern void fill(int *);
int main(void) {
    int a[4];
    a[0] = 11;
    fill(a);
    if (a[0] != 99) return 1;
    return 0;
}
void fill(int *p) { p[0] = 99; }
"#;
    assert_eq!(at_o2("memopt_escaped_local", code), 0);
    assert_eq!(at_o2_no_inline("memopt_escaped_local_ni", code), 0);
}

/// A global is reachable by any externally-linked callee, whatever this
/// function does with it.
#[test]
fn memopt_a_call_may_write_a_global() {
    let code = r#"
int g = 1;
extern void bump(void);
int main(void) {
    g = 5;
    bump();
    if (g != 6) return 1;
    return 0;
}
void bump(void) { g++; }
"#;
    assert_eq!(at_o2("memopt_global_vs_call", code), 0);
    assert_eq!(at_o2_no_inline("memopt_global_vs_call_ni", code), 0);
}

/// The back-edge clobber: the store dominates the load, and the write that
/// invalidates it sits *after* the load, on the latch.
#[test]
fn memopt_a_clobber_on_the_back_edge_is_not_dominated_away() {
    let code = r#"
int main(void) {
    int a[1];
    int sum = 0;
    a[0] = 1;
    for (int i = 0; i < 5; i++) {
        sum += a[0];
        a[0] = a[0] + 1;
    }
    /* 1 + 2 + 3 + 4 + 5 */
    return sum == 15 ? 0 : 1;
}
"#;
    assert_eq!(at_o2("memopt_back_edge", code), 0);
}

/// The dominator chain is not every path: one arm of a diamond writes the
/// bytes the pre-`if` store put there.
#[test]
fn memopt_a_clobber_on_one_arm_of_a_diamond() {
    let code = r#"
extern int pick(void);
int main(void) {
    int a[1];
    a[0] = 1;
    if (pick()) a[0] = 2;
    return a[0] == 2 ? 0 : 1;
}
int pick(void) { return 1; }
"#;
    assert_eq!(at_o2("memopt_diamond_arm", code), 0);
    assert_eq!(at_o2_no_inline("memopt_diamond_arm_ni", code), 0);
}

/// A `volatile` object is read as many times as the program says, and the
/// property is on the object rather than on the instruction -- which is why
/// both ends of an access have to be checked.
#[test]
fn memopt_a_volatile_object_is_read_every_time() {
    let code = r#"
static volatile int counter;
static volatile int local_seen;
int main(void) {
    volatile int v = 1;
    counter = 1;
    int a = counter;
    counter = 2;
    int b = counter;
    v = 3;
    local_seen = v;
    v = 4;
    return (a == 1 && b == 2 && local_seen == 3 && v == 4) ? 0 : 1;
}
"#;
    assert_eq!(at_o2("memopt_volatile", code), 0);
    assert_eq!(at_o2_no_inline("memopt_volatile_ni", code), 0);
}

/// An `asm` with a memory clobber can name a frame slot without naming an
/// operand, so nothing may be carried across it.
#[test]
fn memopt_inline_asm_may_write_anything() {
    let code = r#"
int main(void) {
    int a[2];
    int *p = a;
    a[0] = 1;
#if defined(__x86_64__) || defined(__aarch64__)
    __asm__ volatile("" : : "r"(p) : "memory");
#endif
    return a[0] == 1 ? 0 : 1;
}
"#;
    assert_eq!(at_o2("memopt_asm_clobber", code), 0);
}

/// A bit-field is a partial write of its storage unit, and the value handed
/// back is only as wide as the unit it came out of. `k = -1` in an eight-bit
/// field is `255`, not `0xFFFF`.
#[test]
fn memopt_a_bitfield_read_is_the_width_its_type_names() {
    let code = r#"
struct __attribute__((packed)) S { unsigned short i:6, j:2, k:8; unsigned long long l; };
struct S s;
int main(void) {
    s.k = -1;
    unsigned int mask = s.k;
    if (mask != 255u) return 1;
    s.i = -1;
    if ((unsigned int)s.i != 63u) return 2;
    s.j = -1;
    if ((unsigned int)s.j != 3u) return 3;
    return 0;
}
"#;
    assert_eq!(at_o2("memopt_bitfield_width", code), 0);
    assert_eq!(at_o2_no_inline("memopt_bitfield_width_ni", code), 0);
}

/// A read-modify-write of one bit-field must not disturb its neighbours in
/// the same storage unit, and the pass must not mistake the unit-wide store
/// for the field it wants.
#[test]
fn memopt_a_bitfield_rmw_leaves_its_neighbours_alone() {
    let code = r#"
struct __attribute__((packed)) S { unsigned short i:6, j:2, k:8; unsigned long long l; };
struct S s;
extern unsigned int add(unsigned int);
int main(void) {
    s.i = 5; s.j = 2; s.k = 7; s.l = 0x1122334455667788ULL;
    s.k += add(3);
    if (s.i != 5 || s.j != 2 || s.k != 10) return 1;
    if (s.l != 0x1122334455667788ULL) return 2;
    return 0;
}
unsigned int add(unsigned int x) { return x; }
"#;
    assert_eq!(at_o2("memopt_bitfield_rmw", code), 0);
    assert_eq!(at_o2_no_inline("memopt_bitfield_rmw_ni", code), 0);
}

/// Two locals are two objects, and a write to one must not be taken for a
/// write to the other -- nor a write at one offset for a write at another.
#[test]
fn memopt_distinct_objects_and_offsets() {
    let code = r#"
extern int opaque(int);
int main(void) {
    int a[4], b[4];
    a[0] = 1; a[1] = 2;
    b[0] = 3; b[1] = 4;
    opaque(0);
    b[0] = 30;
    if (a[0] != 1 || a[1] != 2 || b[0] != 30 || b[1] != 4) return 1;
    return 0;
}
int opaque(int x) { return x; }
"#;
    assert_eq!(at_o2("memopt_two_objects", code), 0);
    assert_eq!(at_o2_no_inline("memopt_two_objects_ni", code), 0);
}

/// A narrow store followed by a wide read of the same bytes: the value the
/// store wrote is only the low bits of what the register held.
#[test]
fn memopt_a_narrow_store_does_not_supply_a_wide_read() {
    let code = r#"
extern int opaque(int);
int main(void) {
    union { unsigned int u; unsigned char b[4]; } v;
    v.u = 0;
    v.b[0] = (unsigned char)opaque(0x1234);
    if (v.b[0] != 0x34) return 1;
    if (v.u != 0x34u) return 2;
    return 0;
}
int opaque(int x) { return x; }
"#;
    assert_eq!(at_o2("memopt_narrow_store", code), 0);
    assert_eq!(at_o2_no_inline("memopt_narrow_store_ni", code), 0);
}

/// `setjmp` resumes at a point the CFG does not model, so every ordering
/// claim a memory pass makes is void and it declines the function outright.
#[test]
fn memopt_setjmp_declines_the_function() {
    let code = r#"
#include <setjmp.h>
static jmp_buf jb;
volatile int n;
extern void jump(void);
int main(void) {
    n = 0;
    if (setjmp(jb) == 0) {
        n = 1;
        jump();
    }
    return n == 1 ? 0 : 1;
}
void jump(void) { longjmp(jb, 1); }
"#;
    assert_eq!(at_o2("memopt_setjmp", code), 0);
    assert_eq!(at_o2_no_inline("memopt_setjmp_ni", code), 0);
}

/// A computed `goto` reaches a block by a route with no CFG edge.
#[test]
fn memopt_a_computed_goto_is_an_edge_the_cfg_does_not_have() {
    let code = r#"
extern int opaque(int);
int main(void) {
    int a[1];
    void *t = &&again;
    int n = 0;
    a[0] = 0;
again:
    a[0] = a[0] + 1;
    n++;
    if (n < 3) goto *t;
    return a[0] == 3 ? 0 : 1;
}
int opaque(int x) { return x; }
"#;
    assert_eq!(at_o2("memopt_computed_goto", code), 0);
}

/// A struct-returning call writes its receiving local with no `Store`
/// anywhere: the `Sym` it targets *is* the storage.
#[test]
fn memopt_a_struct_return_writes_its_receiving_local() {
    let code = r#"
typedef struct { unsigned long lo, hi; } P;
static P make(unsigned long a, unsigned long b) { P r; r.lo = a; r.hi = b; return r; }
int main(void) {
    P p = make(1, 2);
    if (p.lo != 1 || p.hi != 2) return 1;
    p = make(3, 4);
    if (p.lo != 3 || p.hi != 4) return 2;
    /* The halves arrive swapped relative to where they are wanted. */
    P q = make(p.hi, p.lo);
    if (q.lo != 4 || q.hi != 3) return 3;
    return 0;
}
"#;
    assert_eq!(at_o2("memopt_struct_return", code), 0);
    assert_eq!(at_o2_no_inline("memopt_struct_return_ni", code), 0);
}

/// An inline-asm output is a second definition of its pseudo. A tied operand
/// writes that pseudo with a `Copy` *before* the asm, and following it past
/// the asm's own write answers with the input where the question was about
/// the result.
#[test]
fn memopt_an_asm_output_is_not_its_tied_input() {
    let code = r#"
int main(void) {
    int x = 5;
    int r;
#if defined(__x86_64__)
    __asm__("shll %1, %0" : "=r"(r) : "I"(3), "0"(x));
#elif defined(__aarch64__)
    __asm__("lsl %w0, %w0, #3" : "=r"(r) : "0"(x));
#else
    r = x << 3;
#endif
    return r == 40 ? 0 : 1;
}
"#;
    assert_eq!(at_o2("memopt_asm_tied_output", code), 0);
    assert_eq!(at_o2_no_inline("memopt_asm_tied_output_ni", code), 0);
}

/// Both halves of a two-register struct return are live at once, and a
/// return can want them the other way round: the first's source sits in the
/// second's destination *and* the second's in the first's. No order of two
/// moves preserves both, so the values have to be exchanged.
///
/// This is unreachable while both halves are loaded out of memory on their
/// way to the return, which is why it stayed hidden until forwarding could
/// leave them in registers -- so it needs `-O2` and a callee that is not
/// inlined away.
#[test]
fn memopt_a_two_register_return_may_need_a_swap() {
    let code = r#"
typedef struct { unsigned long low, high; } u128_t;
static u128_t make(unsigned long lo, unsigned long hi) {
    u128_t r;
    r.low = lo;
    r.high = hi;
    return r;
}
static u128_t add(u128_t a, u128_t b) {
    u128_t r;
    r.low = a.low + b.low;
    r.high = a.high + b.high + (r.low < a.low ? 1UL : 0UL);
    return r;
}
int main(void) {
    u128_t a = make(100, 0);
    if (a.low != 100 || a.high != 0) return 1;
    u128_t c = add(a, make(200, 0));
    if (c.low != 300 || c.high != 0) return 2;
    u128_t d = add(make(0xFFFFFFFFFFFFFFFFUL, 0), make(1, 0));
    if (d.low != 0 || d.high != 1) return 3;
    return 0;
}
"#;
    assert_eq!(at_o2_no_inline("memopt_two_reg_return_swap", code), 0);
    assert_eq!(at_o2("memopt_two_reg_return_swap_inl", code), 0);
}
