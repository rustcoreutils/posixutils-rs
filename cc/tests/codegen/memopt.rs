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

use crate::codegen::asm_probe::{
    asm_for_with, assert_body_contains, assert_body_lacks, body_of, count_in_body, AARCH64_LINUX,
    X86_64_LINUX,
};
use crate::common::{
    compile_and_run, compile_and_run_aarch64, compile_and_run_optimized, compile_and_run_two_units,
    run_c17,
};

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

/// `__attribute__((pure))` and `((const))` are promises about memory, and
/// they buy what escape analysis cannot: a *global* survives across a call
/// to a function that writes nothing.
#[test]
fn memopt_a_pure_callee_does_not_disturb_a_global() {
    let code = r#"
extern void abort(void);
int g;
__attribute__((pure)) extern int peek(int);
__attribute__((const)) extern int square(int);
extern int poke(int);

int main(void) {
    g = 5;
    if (peek(0) != 5) abort();
    if (g != 5) abort();
    if (square(3) != 9) abort();
    if (g != 5) abort();
    if (poke(0) != 0) abort();
    if (g != 6) abort();
    return 0;
}

int peek(int x) { return x + g; }
int square(int x) { return x * x; }
int poke(int x) { g++; return x; }
"#;
    assert_eq!(at_o2("memopt_pure_callee", code), 0);
    assert_eq!(at_o2_no_inline("memopt_pure_callee_ni", code), 0);
}

/// The same promise inferred rather than written. A `static` function that
/// only reads is clean, and one that writes is not -- and the difference has
/// to survive the stack traffic every body has before promotion.
#[test]
fn memopt_an_inferred_clean_callee_does_not_disturb_a_global() {
    let code = r#"
extern void abort(void);
int g;

/* Reads a global and its own arguments: clean, despite the frame slots. */
static int reads(int a, int b) { int t = a + b; return t + g; }

/* Writes one: not. */
static int writes(int a) { g += a; return g; }

int main(void) {
    g = 5;
    if (reads(1, 2) != 8) abort();
    if (g != 5) abort();
    if (writes(3) != 8) abort();
    if (g != 8) abort();
    return 0;
}
"#;
    assert_eq!(at_o2("memopt_inferred_clean", code), 0);
    assert_eq!(at_o2_no_inline("memopt_inferred_clean_ni", code), 0);
}

/// A callee that writes a global through a *third* function must not come
/// out clean: the effect has to travel the call graph.
#[test]
fn memopt_an_effect_travels_the_call_graph() {
    let code = r#"
extern void abort(void);
int g;
static int inner(int a) { g += a; return g; }
static int middle(int a) { return inner(a) + 1; }
static int outer(int a) { return middle(a) + 1; }

int main(void) {
    g = 1;
    if (outer(2) != 5) abort();
    if (g != 3) abort();
    return 0;
}
"#;
    assert_eq!(at_o2("memopt_effect_transitive", code), 0);
    assert_eq!(at_o2_no_inline("memopt_effect_transitive_ni", code), 0);
}

/// A definition another object can replace is not evidence about what will
/// run, so a `weak` one is never trusted however clean its body looks.
#[test]
fn memopt_a_weak_definition_is_not_trusted() {
    let code = r#"
extern void abort(void);
int g;
__attribute__((weak)) int maybe_replaced(int a);
__attribute__((weak)) int maybe_replaced(int a) { return a; }

int main(void) {
    g = 4;
    if (maybe_replaced(1) != 1) abort();
    if (g != 4) abort();
    return 0;
}
"#;
    assert_eq!(at_o2("memopt_weak_callee", code), 0);
    assert_eq!(at_o2_no_inline("memopt_weak_callee_ni", code), 0);
}

/// An indirect call names no callee, so nothing may be assumed about it.
#[test]
fn memopt_an_indirect_call_is_opaque() {
    let code = r#"
extern void abort(void);
int g;
static int bump(int a) { g += a; return g; }

int main(void) {
    int (*fp)(int) = bump;
    g = 1;
    if (fp(2) != 3) abort();
    if (g != 3) abort();
    return 0;
}
"#;
    assert_eq!(at_o2("memopt_indirect_call", code), 0);
    assert_eq!(at_o2_no_inline("memopt_indirect_call_ni", code), 0);
}

/// A `pure` callee still writes nothing, but it may *read* -- so a store
/// before it must still have happened by the time it runs.
#[test]
fn memopt_a_pure_callee_still_sees_earlier_stores() {
    let code = r#"
extern void abort(void);
int g;
__attribute__((pure)) extern int peek(void);

int main(void) {
    g = 1;
    if (peek() != 1) abort();
    g = 2;
    if (peek() != 2) abort();
    return 0;
}

int peek(void) { return g; }
"#;
    assert_eq!(at_o2("memopt_pure_reads", code), 0);
    assert_eq!(at_o2_no_inline("memopt_pure_reads_ni", code), 0);
}

/// A store nothing can observe is deleted -- and the value of the pass is
/// entirely in what it *keeps*.
#[test]
fn memopt_a_dead_store_is_deleted_and_a_live_one_is_not() {
    let code = r#"
extern void abort(void);
extern int opaque(int);

int main(void) {
    int a[4];
    a[0] = 1;
    a[0] = 2;               /* the first is overwritten before any read */
    if (a[0] != 2) abort();

    /* A read between two stores keeps the first. */
    a[1] = 10;
    int seen = a[1];
    a[1] = 20;
    if (seen != 10 || a[1] != 20) abort();

    /* A partial overwrite leaves the rest live. */
    unsigned int u = 0x11223344u;
    unsigned char *p = (unsigned char *)&u;
    p[0] = 0xFF;
    if (u != 0x112233FFu && u != 0xFF223344u) abort();

    return opaque(0);
}
int opaque(int x) { return x; }
"#;
    assert_eq!(at_o2("memopt_dse_basic", code), 0);
    assert_eq!(at_o2_no_inline("memopt_dse_basic_ni", code), 0);
}

/// A read-modify-write is a load of the storage unit, a mask, and a store
/// back. Killing the *first* store of such a pair because the second covers
/// it would lose every field it did not name.
#[test]
fn memopt_dse_does_not_break_a_read_modify_write() {
    let code = r#"
extern void abort(void);
struct S { unsigned int a : 5, b : 11, c : 16; };

int main(void) {
    struct S s;
    s.a = 1; s.b = 2; s.c = 3;
    s.b = 7;                      /* rewrites the unit; a and c must survive */
    if (s.a != 1 || s.b != 7 || s.c != 3) abort();
    s.a += 2;
    if (s.a != 3 || s.b != 7 || s.c != 3) abort();
    return 0;
}
"#;
    assert_eq!(at_o2("memopt_dse_rmw", code), 0);
    assert_eq!(at_o2_no_inline("memopt_dse_rmw_ni", code), 0);
}

/// A store live only on one path out, or only through a back edge, is not
/// dead at exit.
#[test]
fn memopt_dse_respects_every_path_to_the_exit() {
    let code = r#"
extern void abort(void);
extern int pick(void);

int main(void) {
    int a[1];
    int total = 0;

    /* Read on one arm of a diamond only. */
    a[0] = 5;
    if (pick()) total += a[0];
    if (total != 5) abort();

    /* Read at the top of a loop body, written at the bottom: the store is
       observed through the back edge. */
    a[0] = 1;
    for (int i = 0; i < 4; i++) {
        total += a[0];
        a[0] = a[0] + 1;
    }
    /* 5 + (1+2+3+4) */
    if (total != 15) abort();
    return 0;
}
int pick(void) { return 1; }
"#;
    assert_eq!(at_o2("memopt_dse_paths", code), 0);
    assert_eq!(at_o2_no_inline("memopt_dse_paths_ni", code), 0);
}

/// A global outlives the frame and a `volatile` object is written as many
/// times as the program says, so neither is ever dead at exit.
#[test]
fn memopt_dse_keeps_globals_and_volatiles() {
    let code = r#"
extern void abort(void);
int g;
static int s;
volatile int v;

static int report(void) { return g + s; }

int main(void) {
    g = 1;
    s = 2;
    v = 3;      /* observable even though nothing reads it back */
    v = 4;
    if (report() != 3) abort();
    if (v != 4) abort();
    return 0;
}
"#;
    assert_eq!(at_o2("memopt_dse_globals", code), 0);
    assert_eq!(at_o2_no_inline("memopt_dse_globals_ni", code), 0);
}

/// A local whose address left the function is observable after the frame
/// goes, so a store to it is never dead at exit.
#[test]
fn memopt_dse_keeps_a_store_to_an_escaped_local() {
    let code = r#"
extern void abort(void);
extern void keep(int *);
static int *saved;

int main(void) {
    int a = 1;
    keep(&a);
    a = 42;         /* `keep` stashed the address; this is observable */
    if (*saved != 42) abort();
    return 0;
}
void keep(int *p) { saved = p; }
"#;
    assert_eq!(at_o2("memopt_dse_escaped", code), 0);
    assert_eq!(at_o2_no_inline("memopt_dse_escaped_ni", code), 0);
}

/// `setjmp` resumes at a point the CFG does not model, so nothing may be
/// called dead at exit in a frame it can return into.
#[test]
fn memopt_dse_declines_a_frame_setjmp_can_reenter() {
    let code = r#"
#include <setjmp.h>
extern void abort(void);
static jmp_buf jb;
static int *watch;
extern void jump(void);

int main(void) {
    volatile int n = 0;
    int a = 1;
    watch = &a;
    if (setjmp(jb) == 0) {
        a = 7;
        n = 1;
        jump();
    }
    if (n != 1 || *watch != 7) abort();
    return 0;
}
void jump(void) { longjmp(jb, 1); }
"#;
    assert_eq!(at_o2("memopt_dse_setjmp", code), 0);
    assert_eq!(at_o2_no_inline("memopt_dse_setjmp_ni", code), 0);
}

/// An attribute belongs to the declarator it is written on. `extern int
/// p(void) __attribute__((pure)), q(void);` promises nothing about `q`, and
/// a callee wrongly believed to write nothing is a miscompile at the *call
/// site* rather than anywhere near the declaration.
///
/// Two units, because that is the only shape where the promise is
/// load-bearing: a function *defined* in the same unit is judged by its
/// body, so the leak is overwritten before anything can act on it.
#[test]
fn memopt_a_purity_attribute_does_not_leak_to_the_next_declarator() {
    let caller = r#"
int g;
extern int pure_fn(void) __attribute__((pure)), dirty_fn(void);
extern int a_fn(void), b_fn(void) __attribute__((pure));

int probe_dirty(void) { g = 1; dirty_fn(); return g; }
int probe_a(void)     { g = 1; a_fn();     return g; }
int probe_pure(void)  { g = 7; pure_fn();  return g; }
int probe_b(void)     { g = 9; b_fn();     return g; }
"#;
    let callee = r#"
extern void abort(void);
extern int g;
extern int probe_dirty(void), probe_a(void), probe_pure(void), probe_b(void);

int pure_fn(void)  { return g; }
int b_fn(void)     { return g; }
int dirty_fn(void) { g = 42; return 0; }
int a_fn(void)     { g = 43; return 0; }

int main(void) {
    if (probe_dirty() != 42) abort();   /* `pure` was on pure_fn, not on this */
    if (probe_a() != 43) abort();       /* nor on a_fn, which precedes it */
    if (probe_pure() != 7) abort();
    if (probe_b() != 9) abort();
    return 0;
}
"#;
    for opt in ["-O2", "-O1"] {
        assert_eq!(
            compile_and_run_two_units("memopt_attr_leak", caller, callee, &[opt.to_string()],),
            0,
            "at {opt}"
        );
    }
}

/// The call-graph fixed point starts every inferable function optimistically
/// at `Const`, so stopping before it converges *keeps the optimistic seed* --
/// a function that transitively writes a global comes out clean and a store
/// across a call to it is forwarded. A capped sweep count propagated
/// dirtiness one caller per pass, so the cap was exactly the chain depth at
/// which the answer went wrong.
#[test]
fn memopt_a_deep_call_chain_still_reaches_its_callee() {
    // Declared first, defined caller-before-callee: the order in which a
    // sweep makes the least progress per pass.
    let mut code = String::from("extern void abort(void);\nint g;\n");
    const DEPTH: usize = 40;
    for i in 1..=DEPTH {
        code.push_str(&format!(
            "__attribute__((noinline)) static int f{i}(void);\n"
        ));
    }
    for i in 1..=DEPTH {
        if i == DEPTH {
            code.push_str(&format!(
                "__attribute__((noinline)) static int f{i}(void) {{ g = 42; return 0; }}\n"
            ));
        } else {
            let n = i + 1;
            code.push_str(&format!(
                "__attribute__((noinline)) static int f{i}(void) {{ return f{n}(); }}\n"
            ));
        }
    }
    code.push_str("int main(void) { g = 1; f1(); if (g != 42) abort(); return 0; }\n");

    assert_eq!(at_o2("memopt_deep_chain", &code), 0);
    assert_eq!(at_o2_no_inline("memopt_deep_chain_ni", &code), 0);
}

/// The optimized IR of `src` for `target`, as `--dump-ir post-opt` prints it.
fn post_opt_ir(prefix: &str, src: &str, target: &str) -> String {
    let dir = plib::tmp::Builder::new()
        .prefix(prefix)
        .tempdir()
        .expect("tempdir");
    let c = dir.path().join("t.c");
    std::fs::write(&c, src).expect("write source");
    let r = run_c17(&[
        "--target",
        target,
        "-O2",
        "-fno-inline",
        "--dump-ir",
        "post-opt",
        "-o",
        "/dev/null",
        &c.to_string_lossy(),
    ]);
    assert!(r.success, "compile failed: {}", r.stderr);
    format!("{}{}", r.stdout, r.stderr)
}

/// A parameter's stack slot is a local like any other: the value stored into
/// it on entry is the incoming argument, and a load of it through a pointer
/// is that argument. The store's value is an `Arg`, which no instruction in
/// the function defines, and asking only the defining instruction for its
/// width refused every parameter while forwarding the same shape for a local.
#[test]
fn memopt_a_parameter_slot_forwards_like_a_local() {
    // No headers: the aarch64 target's are not on the default search path.
    let src = r#"
void *memcpy(void *, const void *, unsigned long);
unsigned through_ptr(unsigned x) { unsigned *p = &x; return *p + 1; }
unsigned through_memcpy(unsigned x) { unsigned y; memcpy(&y, &x, 4); return y; }
double a_double(double d) { double *p = &d; return *p * 2.0; }
char *a_pointer(char *s) { char **p = &s; return *p; }
int a_narrow(signed char c) { signed char *p = &c; return *p; }
"#;
    for target in ["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"] {
        let ir = post_opt_ir("c17_param_fwd_", src, target);
        assert!(
            !ir.contains("load"),
            "{target}: every parameter read should be forwarded:\n{ir}"
        );
    }
}

/// What forwarding a parameter's slot must still decline: an address that
/// reaches a callee, a pointer that writes the slot, a `volatile` parameter,
/// the named parameter of a variadic function and a `va_list` parameter,
/// narrow and reinterpreted reads, promoted identifier-list parameters.
#[test]
fn memopt_a_parameter_slot_is_still_an_object() {
    let code = r#"
#include <stdarg.h>
#include <string.h>
extern void abort(void);

__attribute__((noinline)) void bump(int *p) { *p += 5; }
int *gp;
__attribute__((noinline)) void through_global(void) { *gp = 99; }

__attribute__((noinline)) int escapes(int x) { int *p = &x; *p = 3; bump(&x); return *p; }
__attribute__((noinline)) int via_global(int x) { gp = &x; x = 1; through_global(); return x; }
__attribute__((noinline)) int written(int x) { int *p = &x; *p = x * 2; return x + *p; }
__attribute__((noinline)) int is_volatile(volatile int x) {
    volatile int *p = &x; int a = *p; *p = a + 1; return *p + x;
}
__attribute__((noinline)) int variadic(int n, ...) {
    int *p = &n; va_list ap; va_start(ap, n);
    int s = va_arg(ap, int); va_end(ap);
    return *p + s;
}
__attribute__((noinline)) int takes_va_list(int k, va_list ap) {
    int *q = &k; int r = va_arg(ap, int); return *q + r;
}
__attribute__((noinline)) int pass_va_list(int k, ...) {
    va_list ap; va_start(ap, k); int r = takes_va_list(k, ap); va_end(ap); return r;
}
__attribute__((noinline)) int reinterpret(signed char c) { return *(unsigned char *)&c; }
__attribute__((noinline)) int sign(signed char c) { signed char *p = &c; return *p; }
__attribute__((noinline)) int wide_short(short s) { short *p = &s; return *p; }
__attribute__((noinline)) unsigned bytes(unsigned x) {
    unsigned char b[4]; unsigned y;
    memcpy(b, &x, 4); b[1] ^= 0xff; memcpy(&y, b, 4);
    return y;
}
__attribute__((noinline)) unsigned partial(unsigned x) {
    unsigned char *p = (unsigned char *)&x; p[1] = 0; return x;
}
__attribute__((noinline)) double dbl(double d) { double *p = &d; *p += 0.5; return d; }
__attribute__((noinline)) long double ldbl(long double d) { long double *p = &d; return *p * 2; }
__attribute__((noinline)) int kr(c, f) signed char c; float f; { signed char *p = &c; float *q = &f; return *p + (int)*q; }
__attribute__((noinline)) int *ptr(int *q) { int **r = &q; return *r; }

int main(void) {
    int z = 7;
    union { unsigned u; unsigned char b[4]; } e1, e2;
    e1.u = e2.u = 0x12345678u;
    e1.b[1] ^= 0xff;
    e2.b[1] = 0;
    if (escapes(1) != 8) abort();
    if (via_global(5) != 99) abort();
    if (written(4) != 16) abort();
    if (is_volatile(10) != 22) abort();
    if (variadic(3, 4) != 7) abort();
    if (pass_va_list(6, 9) != 15) abort();
    if (reinterpret(-1) != 255) abort();
    if (sign(-3) != -3) abort();
    if (wide_short(-2) != -2) abort();
    if (bytes(0x12345678u) != e1.u) abort();
    if (partial(0x12345678u) != e2.u) abort();
    if (dbl(1.0) != 1.5) abort();
    if (ldbl(1.25L) != 2.5L) abort();
    if (kr(-5, 2.5f) != -3) abort();
    if (ptr(&z) != &z) abort();
    return 0;
}
"#;
    for opt in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run("memopt_param_object", code, &[opt.to_string()]),
            0,
            "host at {opt}"
        );
        if let Some(rc) = compile_and_run_aarch64("memopt_param_object_a64", code, opt) {
            assert_eq!(rc, 0, "aarch64 at {opt}");
        }
    }
}

/// An aggregate with a `volatile` member is re-read for each copy of it.
///
/// C17 6.7.3p7: an object with volatile-qualified type may change in ways
/// the implementation cannot see, so every access to it happens as written.
/// A qualifier on a *member* makes that member's storage volatile, but the
/// struct holding it is not itself volatile-qualified -- and the struct's own
/// modifiers, plus the access type, were all `forwardable` asked. Once the
/// copy is expanded into loads and stores those accesses are plain integers,
/// so `t = s; u = s;` loaded `s` once and fed both copies from it.
///
/// The unqualified struct beside it is the control: forwarding *is* right
/// there, and clang does it -- 4 loads of the volatile object against 2 of
/// the plain one. The assertion is relative for that reason, rather than
/// pinning an instruction count that codegen may fairly change.
#[test]
fn codegen_volatile_member_is_reread_for_each_aggregate_copy() {
    let src = r#"
struct V { volatile int v; int pad[7]; };
struct P { int v; int pad[7]; };
struct V vs, vt, vu;
struct P ps, pt, pu;
void fv(void) { vt = vs; vu = vs; }
void fp(void) { pt = ps; pu = ps; }
"#;
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("vol_member_copy", triple, src, &["-O2"]);
        // A load is an instruction whose *source* operand is memory.
        let loads = |func: &str| -> usize {
            body_of(&asm, func)
                .lines()
                .map(str::trim)
                .filter(|l| {
                    if triple == X86_64_LINUX {
                        l.starts_with("mov")
                            && l.split_once(char::is_whitespace).is_some_and(|(_, ops)| {
                                ops.split(',').next().is_some_and(|src| src.contains('('))
                            })
                    } else {
                        l.starts_with("ldr ") || l.starts_with("ldp ")
                    }
                })
                .count()
        };
        let (volatile, plain) = (loads("fv"), loads("fp"));
        assert!(
            volatile > plain,
            "{triple}: the volatile member must be re-read for the second \
             copy -- volatile {volatile} loads, plain {plain}:\n{}",
            body_of(&asm, "fv")
        );
    }
}

/// A store into an aggregate with a `volatile` member is not deleted by a
/// later store that covers it.
///
/// The mirror of the load case: `dse::deletable` asked the same three
/// questions `loadfwd::forwardable` did -- the access type, which an expanded
/// aggregate copy makes a plain integer, and the object's own modifiers, which
/// a `struct` holding a volatile member does not carry -- so `s = x; s = y;`
/// let the first copy's stores go, dropping a write to the volatile member
/// that C17 6.7.3p7 says must happen.
///
/// The unqualified struct beside it is the control: deleting the dead store
/// *is* right there, so the fix cannot pass by giving up on every aggregate.
#[test]
fn codegen_volatile_member_keeps_a_store_a_later_one_covers() {
    let src = r#"
struct V { volatile int v; int pad[7]; };
struct P { int v; int pad[7]; };
struct V vs, vx, vy;
struct P ps, px, py;
void fv(void) { vs = vx; vs = vy; }
void fp(void) { ps = px; ps = py; }
"#;
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("vol_member_dse", triple, src, &["-O2"]);
        let stores = |func: &str| -> usize {
            body_of(&asm, func)
                .lines()
                .map(str::trim)
                .filter(|l| {
                    if triple == X86_64_LINUX {
                        // `movq %rdx, 8(%rax)` -- a memory *destination*.
                        l.starts_with("mov")
                            && l.split_once(char::is_whitespace).is_some_and(|(_, ops)| {
                                ops.rsplit(',').next().is_some_and(|d| d.contains('('))
                            })
                    } else {
                        l.starts_with("str ") || l.starts_with("stp ")
                    }
                })
                .count()
        };
        let (volatile, plain) = (stores("fv"), stores("fp"));
        assert!(
            volatile > plain,
            "{triple}: the store to a volatile member survives a later copy \
             (volatile {volatile} stores, plain {plain}):\n{}",
            body_of(&asm, "fv")
        );
    }
}

/// A composite argument is read at its own size, not rounded up to a
/// register.
///
/// c17 loaded every composite argument with a fixed eight- or sixteen-byte
/// access, whatever the object held: `_Complex char` (two bytes) and a
/// five-byte struct were both read as eight, and a twelve-byte struct as
/// sixteen. AAPCS64 and System V both leave the *register's* upper bits
/// unspecified, so the value handed over was right either way -- what was
/// wrong was the memory read, which runs past the end of the object:
///
/// ```text
///     _Complex char        2 bytes, read 8   -- 6 past
///     struct { char[5]; }  5 bytes, read 8   -- 3 past
///     struct { int[3]; }  12 bytes, read 16  -- 4 past
/// ```
///
/// Ordinarily invisible, because the bytes past a small object are usually
/// its own padding. Here each object is placed flush against a guard page,
/// so reading even one byte too far is a fault rather than a guess -- the
/// program returns the value it was given, or it dies. The callee is a
/// separate translation unit, or it inlines and no argument is passed at
/// all.
///
/// One-byte and two-byte structs were always read at their own width, and a
/// sixteen-byte struct fills its two registers exactly; both are here as
/// controls, so the fix cannot pass by refusing to use registers.
///
/// A composite of three, five, six or seven bytes is *not* here. It over-reads
/// too, but for a different reason and in a different place: its dereference
/// reaches the back end as a correct `load.40`, and it is load lowering that
/// widens it, `OperandSize::from_bits` having no encoding for those widths.
/// That one is its own defect and its own fix.
#[test]
fn codegen_composite_argument_is_read_at_its_own_size() {
    // Each case: a type, and an initializer for its first member.
    let cases: &[(&str, &str)] = &[
        ("_Complex char", "c"),
        ("_Complex short", "c"),
        ("struct S1 { char a[1]; }", "s"),
        ("struct S2 { char a[2]; }", "s"),
        ("struct S3 { char a[3]; }", "s"),
        ("struct S5 { char a[5]; }", "s"),
        ("struct S6 { char a[6]; }", "s"),
        ("struct S7 { char a[7]; }", "s"),
        ("struct S9 { char a[9]; }", "s"),
        ("struct S12 { int a[3]; }", "i"),
        ("struct S15 { char a[15]; }", "s"),
        ("struct S16 { long a[2]; }", "l"),
    ];
    for (n, (ty, kind)) in cases.iter().enumerate() {
        // `T` is the argument type; `first(x)` reads its first byte or word,
        // which is all the callee needs to prove it received the value.
        let (decl, read) = match *kind {
            "c" => (format!("typedef {ty} T;"), "(int)(__real__ v)"),
            "i" => (format!("{ty}; typedef struct S12 T;"), "v.a[0]"),
            "l" => (format!("{ty}; typedef struct S16 T;"), "(int)v.a[0]"),
            _ => {
                let tag = ty.split_whitespace().nth(1).unwrap();
                (format!("{ty}; typedef struct {tag} T;"), "(int)v.a[0]")
            }
        };
        let callee = format!("{decl}\nint take(T v) {{ return {read}; }}\n");
        let caller = format!(
            r#"
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>
{decl}
int take(T v);
int main(void) {{
    long ps = sysconf(_SC_PAGESIZE);
    char *p = mmap(0, 2 * ps, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (p == (char *)-1) return 77;                 /* cannot test here */
    if (mprotect(p + ps, ps, PROT_NONE) != 0) return 77;
    /* Flush against the guard page: one byte too far is a fault. */
    T *obj = (T *)(p + ps - sizeof(T));
    char *raw = (char *)obj;
    for (unsigned i = 0; i < sizeof(T); i++) raw[i] = 0;
    raw[0] = 42;
    return take(*obj) == 42 ? 0 : 1;
}}
"#
        );
        for opt in ["-O0", "-O2"] {
            let got = compile_and_run_two_units(
                &format!("argread_{n}"),
                &caller,
                &callee,
                &[opt.to_string()],
            );
            // 77 means this host has no usable mmap/mprotect; skip loudly
            // rather than pass quietly.
            if got == 77 {
                eprintln!("SKIP codegen_composite_argument_is_read_at_its_own_size: no guard page");
                return;
            }
            assert_eq!(
                got, 0,
                "{opt}: `{ty}` argument -- a non-zero status here is the read \
                 running past the object into the guard page",
            );
        }
    }
}

/// A composite whose size is not a natural access width -- 3, 5, 6 or 7 bytes
/// -- is assembled from two overlapping reads, so the guard-page test above
/// proves only that nothing was read too far. This one proves the bytes that
/// *were* read all arrive, in order: the halves overlap, and getting the
/// shift or the OR wrong drops or doubles a middle byte silently.
#[test]
fn codegen_ragged_composite_keeps_every_byte() {
    for n in [3usize, 5, 6, 7] {
        let decl = format!("struct S {{ unsigned char a[{n}]; }};");
        // The callee rebuilds the value it was handed; the caller compares.
        let callee = format!(
            "{decl}\n\
             unsigned long take(struct S v) {{\n\
             \x20   unsigned long r = 0;\n\
             \x20   for (unsigned i = 0; i < {n}; i++) r |= (unsigned long)v.a[i] << (i * 8);\n\
             \x20   return r;\n\
             }}\n"
        );
        let caller = format!(
            r#"
#include <stdio.h>
{decl}
unsigned long take(struct S v);
int main(void) {{
    struct S s;
    unsigned long want = 0;
    for (unsigned i = 0; i < {n}; i++) {{
        s.a[i] = (unsigned char)(0x11 * (i + 1));
        want |= (unsigned long)s.a[i] << (i * 8);
    }}
    struct S *p = &s;                 /* read through a pointer, as the ABI path does */
    unsigned long got = take(*p);
    if (got != want) {{
        printf("%zu bytes: got %lx want %lx\n", (size_t){n}, got, want);
        return 1;
    }}
    return 0;
}}
"#
        );
        for opt in ["-O0", "-O2"] {
            let got = compile_and_run_two_units(
                &format!("ragged_{n}"),
                &caller,
                &callee,
                &[opt.to_string()],
            );
            assert_eq!(
                got, 0,
                "{opt}: a {n}-byte struct passed by value came back with the \
                 wrong bytes -- the two overlapping halves were not recombined \
                 correctly",
            );
        }
    }
}

/// The widest load in `body` that reads through a pointer, in bytes, or 0.
///
/// Frame-relative accesses are skipped: a spill or a stack temporary is the
/// compiler's own storage, and its width says nothing about the object.
fn widest_object_load(body: &str, aarch64: bool) -> (u32, String) {
    let mut widest = 0;
    let mut at = String::new();
    for line in body.lines() {
        let text = line.trim();
        let (mnemonic, operands) = match text.split_once(char::is_whitespace) {
            Some(pair) => pair,
            None => continue,
        };
        let width = if aarch64 {
            if operands.contains("[sp") || operands.contains("[x29") {
                continue;
            }
            match mnemonic {
                "ldrb" | "ldrsb" => 1,
                "ldrh" | "ldrsh" => 2,
                "ldr" | "ldrsw" if operands.starts_with('w') => 4,
                "ldr" if operands.starts_with('x') => 8,
                "ldp" if operands.starts_with('x') => 16,
                "ldp" if operands.starts_with('w') => 8,
                _ => continue,
            }
        } else {
            // The source is the first operand; it must be a memory reference
            // through something other than the frame.
            let src = operands.split(',').next().unwrap_or("").trim();
            if !src.contains("(%r") || src.contains("%rbp)") || src.contains("%rsp)") {
                continue;
            }
            match mnemonic {
                "movb" | "movzbl" | "movsbl" | "movzbq" | "movsbq" => 1,
                "movw" | "movzwl" | "movswl" | "movzwq" | "movswq" => 2,
                "movl" | "movslq" => 4,
                "movq" => 8,
                _ => continue,
            }
        };
        if width > widest {
            widest = width;
            at = text.to_string();
        }
    }
    (widest, at)
}

/// No composite is read wider than it is, on either target.
///
/// The runtime tests above execute, so they only ever cover the host --
/// x86-64 here. This one compiles for both triples and reads the widths out
/// of the assembly, which is what covers the aarch64 lowering on a machine
/// that cannot run it. A ragged size is read as two overlapping halves, so
/// the widest access is the half, never the object rounded up.
#[test]
fn codegen_no_composite_is_read_wider_than_itself() {
    // (bytes in the struct, the widest load its value may be read with)
    let cases: &[(usize, u32)] = &[
        (1, 1),
        (2, 2),
        // The ragged sizes: 3 is two halves of 2, and 5, 6 and 7 are two of 4.
        (3, 2),
        (5, 4),
        (6, 4),
        (7, 4),
        // The controls, each already one natural access.
        (4, 4),
        (8, 8),
    ];
    for &(bytes, want) in cases {
        let src = format!(
            "struct S {{ char a[{bytes}]; }};\n\
             int g(struct S s);\n\
             int f(struct S *p) {{ return g(*p); }}\n"
        );
        for (triple, is_a64) in [(X86_64_LINUX, false), (AARCH64_LINUX, true)] {
            let asm = asm_for_with(&format!("widest_{bytes}"), triple, &src, &["-O1"]);
            let body = body_of(&asm, "f");
            let (got, line) = widest_object_load(body, is_a64);
            assert_eq!(
                got, want,
                "{triple}: a {bytes}-byte struct is read with a {got}-byte \
                 access (`{line}`), not {want} -- anything wider reaches past \
                 the object:\n{body}"
            );
        }
    }
}

/// Reading a `volatile` object is an observable side effect, so the access
/// survives every optimization level -- including a read whose value is
/// discarded, which no data-flow fact keeps alive (C17 5.1.2.3).
///
/// The property is on the access, not on the result, so a discarded read has
/// nothing an exit status can see. The check is on the emitted instruction,
/// against both targets, because the rule is architecture-independent.
#[test]
fn memopt_a_discarded_volatile_read_is_still_performed() {
    // The object names are deliberately unmistakable. A single letter is not a
    // sound needle here: every x86-64 body contains `pushq`/`popq` and every
    // aarch64 body contains `stp`/`sp`, and `.cfi_startproc` is inside the
    // range `body_of` returns -- so searching for "p" passes against a body
    // that was emptied, which is exactly the defect. (No empty body on either
    // target contains a "g", which is why the other cases were sound.)
    let cases = [
        (
            "assign",
            "volatile int volobj;\nvoid probe(void) { int a = volobj; (void)a; }\n",
            "volobj",
        ),
        (
            "discard",
            "volatile int volobj;\nvoid probe(void) { volobj; }\n",
            "volobj",
        ),
        (
            "via_ptr",
            "volatile int *volptr;\nvoid probe(void) { *volptr; }\n",
            "volptr",
        ),
        (
            "cast_void",
            "volatile int volobj;\nvoid probe(void) { (void)volobj; }\n",
            "volobj",
        ),
    ];

    for (tag, src, object) in cases {
        for level in ["-O0", "-O1", "-O2", "-Os"] {
            for triple in [X86_64_LINUX, AARCH64_LINUX] {
                let asm = asm_for_with(&format!("vol_{tag}"), triple, src, &[level]);
                assert_body_contains(
                    &asm,
                    "probe",
                    object,
                    &format!(
                        "a volatile read is observable: `{tag}` at {level} on {triple} \
                         must still access `{object}`"
                    ),
                );

                // Naming the pointer is not the same as dereferencing it, and
                // the qualifier here is on the pointee, so the load through it
                // is the access under test.
                if tag == "via_ptr" {
                    let indirect = if triple == X86_64_LINUX { "(%r" } else { "[x" };
                    assert_body_contains(
                        &asm,
                        "probe",
                        indirect,
                        &format!(
                            "the volatile pointee is read, not just the pointer: \
                             {level} on {triple}"
                        ),
                    );
                }
            }
        }
    }
}

/// The counterpart that keeps the fix above honest: an *ordinary* discarded
/// read is still dead code, and DCE still deletes it.
///
/// Without this, marking every load a root would pass the volatile test.
#[test]
fn memopt_a_discarded_plain_read_is_still_removed() {
    // Object names chosen to occur in no mnemonic, register or label the
    // body can otherwise contain -- `popq` alone contains both `p` and `pq`,
    // and the body a negative assertion searches includes the function's own
    // label and prologue.
    let cases = [
        (
            "assign",
            "int objx;\nvoid probe(void) { int a = objx; (void)a; }\n",
            "objx",
        ),
        ("discard", "int objx;\nvoid probe(void) { objx; }\n", "objx"),
        (
            "via_ptr",
            "int *ptrx;\nvoid probe(void) { *ptrx; }\n",
            "ptrx",
        ),
    ];

    for (tag, src, object) in cases {
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with(&format!("plain_{tag}"), triple, src, &["-O2"]);
            assert_body_lacks(
                &asm,
                "probe",
                object,
                &format!(
                    "reading a non-volatile object has no effect: `{tag}` on {triple} \
                     must not access `{object}`"
                ),
            );
        }
    }
}

/// Each read of a `volatile` object is its own observable event, so two of
/// them are two accesses -- neither load-forwarding nor DCE may fold the pair
/// into one.
#[test]
fn memopt_two_volatile_reads_are_both_performed() {
    // A named object only: two reads through one `volatile int *p` show up as
    // a *single* reference to `p` -- reading the pointer itself is not
    // volatile and is rightly done once -- so the count says nothing there.
    // The through-pointer case is pinned at the IR level instead, by
    // `test_volatile_accesses_carry_the_marker` and the `dce` unit tests.
    //
    // The name occurs in no mnemonic, register or label the body can
    // otherwise contain: `popq` alone contains both `p` and `pq`.
    let cases = [(
        "named",
        "volatile int objx;\nint sink(int, int);\n\
         int probe(void) { int a = objx; int b = objx; return sink(a, b); }\n",
        "objx",
    )];

    for (tag, src, object) in cases {
        for level in ["-O1", "-O2", "-Os"] {
            for triple in [X86_64_LINUX, AARCH64_LINUX] {
                let asm = asm_for_with(&format!("vol_two_reads_{tag}"), triple, src, &[level]);
                let n = count_in_body(&asm, "probe", object);
                assert!(
                    n >= 2,
                    "both volatile reads are observable: `{tag}` at {level} on {triple} \
                     kept {n} reference(s) to `{object}`:\n{}",
                    body_of(&asm, "probe")
                );
            }
        }
    }
}

/// An `_Atomic` read is observable for the same reason, and reaches DCE by a
/// different route: `AtomicLoad` is a side-effecting opcode outright, so this
/// cross-checks that the two spellings of "this read must happen" agree.
#[test]
fn memopt_a_discarded_atomic_read_is_still_performed() {
    let cases = [
        ("discard", "_Atomic int g;\nvoid probe(void) { g; }\n", "g"),
        (
            "assign",
            "_Atomic int g;\nvoid probe(void) { int a = g; (void)a; }\n",
            "g",
        ),
    ];

    for (tag, src, object) in cases {
        for level in ["-O0", "-O2"] {
            for triple in [X86_64_LINUX, AARCH64_LINUX] {
                let asm = asm_for_with(&format!("atomic_{tag}"), triple, src, &[level]);
                assert_body_contains(
                    &asm,
                    "probe",
                    object,
                    &format!(
                        "an atomic read is observable: `{tag}` at {level} on {triple} \
                         must still access `{object}`"
                    ),
                );
            }
        }
    }
}

/// A `volatile` read inside a loop happens once per iteration: the value is
/// not a loop invariant, whatever the compiler can see written to the object.
///
/// Nothing in c17 hoists memory out of a loop today (see the ordering contract
/// in `cc/ir/dce.rs`), so this passes by construction -- it exists to fail the
/// day something does, because the exit status is where that would show up.
#[test]
fn memopt_a_volatile_read_in_a_loop_is_repeated() {
    // `g` changes between iterations through a pointer the loop writes, so a
    // read hoisted to the top would sum 7 three times instead of 7 + 10 + 11.
    let code = r#"
volatile int g;
static int *alias(void) { return (int *)&g; }
int main(void) {
    int sum = 0;
    *alias() = 7;
    for (int i = 0; i < 3; i++) {
        sum += g;
        *alias() = 10 + i;
    }
    return sum == 28 ? 0 : 1;
}
"#;
    assert_eq!(at_o2("memopt_volatile_in_loop", code), 0);
    if let Some(rc) = compile_and_run_aarch64("memopt_volatile_in_loop_a64", code, "-O2") {
        assert_eq!(rc, 0, "aarch64 at -O2");
    }
}

/// A `volatile` store is observable for the same reason, and DSE must not drop
/// the earlier of two writes to one.
///
/// The companion to the read case above: a test that only checked loads would
/// pass against an `has_side_effects` that named `Store` and not `Load`.
#[test]
fn memopt_two_volatile_stores_are_both_performed() {
    let src = "volatile int g;\nvoid probe(void) { g = 1; g = 2; }\n";
    for level in ["-O1", "-O2", "-Os"] {
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with("vol_two_stores", triple, src, &[level]);
            let n = count_in_body(&asm, "probe", "g");
            assert!(
                n >= 2,
                "both volatile stores are observable: {level} on {triple} kept {n} \
                 reference(s) to `g`:\n{}",
                body_of(&asm, "probe")
            );
        }
    }
}

/// A member of a `volatile` object is itself volatile, so reading it is an
/// observable event that survives every optimization level.
///
/// C17 6.5.2.3p3/p4: the result of `s.m` has the *so-qualified* version of the
/// member's type — it inherits the qualifiers of the object. c17 took the
/// member's declared type unchanged, so a member of a `volatile` struct read as
/// an ordinary `int` and DCE deleted it from `-O1` up. The reverse direction
/// (`struct T { volatile int a; }`) always worked, because there the member's
/// own type carries the qualifier; that case is the control below.
#[test]
fn memopt_a_member_of_a_volatile_object_is_volatile() {
    // Distinctive names: a single letter matches `pushq`/`stp`/`.cfi_startproc`
    // inside the body range and would pass against an emptied function.
    let src = "\
struct S { int a; int b; };
volatile struct S vqobj;
volatile struct S *vqptr;
void probe_direct(void) { vqobj.a; }
void probe_arrow(void) { vqptr->a; }
void probe_assign(void) { int t = vqobj.a; (void)t; }
void probe_second(void) { vqobj.b; }
";
    for level in ["-O0", "-O1", "-O2", "-Os"] {
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with("vol_member", triple, src, &[level]);
            for (func, object) in [
                ("probe_direct", "vqobj"),
                ("probe_arrow", "vqptr"),
                ("probe_assign", "vqobj"),
                ("probe_second", "vqobj"),
            ] {
                assert_body_contains(
                    &asm,
                    func,
                    object,
                    &format!(
                        "a member of a volatile object is volatile (C17 6.5.2.3p3): \
                         {func} at {level} on {triple} must still access `{object}`"
                    ),
                );
            }
        }
    }
}

/// A copy out of a `volatile` aggregate reads it, even when nothing uses the
/// copy (C17 5.1.2.3p6).
///
/// A struct too wide for one register is copied in integer chunks, and the
/// chunks carried no volatile marker, so `struct S t = vstructobj;` with `t`
/// unused lost every read from `-O1` up on both targets. The copy is of a
/// named global so that the object's name in the body is the access itself.
#[test]
fn memopt_a_copy_out_of_a_volatile_aggregate_is_performed() {
    let src = "\
struct S { int a, b, c; };
volatile struct S vstructobj;
struct { volatile struct { int a; }; int b; } vanonobj;
void probe_copy(void) { struct S t = vstructobj; (void)t; }
void probe_anon(void) { vanonobj.a; }
";
    for level in ["-O0", "-O1", "-O2"] {
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with("vol_copy", triple, src, &[level]);
            for (func, object) in [("probe_copy", "vstructobj"), ("probe_anon", "vanonobj")] {
                assert_body_contains(
                    &asm,
                    func,
                    object,
                    &format!("{func} at {level} on {triple} must still read `{object}`"),
                );
            }
        }
    }
}

/// The control for the test above: an ordinary aggregate's member read is still
/// deleted, so that test cannot pass by marking every member access volatile.
#[test]
fn memopt_a_member_of_a_plain_object_is_still_removed() {
    let src = "\
struct S { int a; };
struct S pqobj;
void probe(void) { pqobj.a; }
";
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("plain_member", triple, src, &["-O2"]);
        assert_body_lacks(
            &asm,
            "probe",
            "pqobj",
            "reading an ordinary member has no effect and is dead code",
        );
    }
}

/// A `volatile` member is not speculatable, so a conditional must not read the
/// arm it did not take.
///
/// C17 6.5.15p4 evaluates only one of the second and third operands, and
/// 5.1.2.3 makes each volatile read an observable event. `is_pure_expr`'s
/// `Member` arm asked only whether the *base* was pure, so the read was
/// hoisted and both members were loaded unconditionally into a branchless
/// select — at `-O0` too.
#[test]
fn memopt_a_volatile_member_is_not_speculated_by_a_conditional() {
    let src = "\
struct S { volatile unsigned status; unsigned other; };
struct S sqobj;
unsigned probe(int c) { return c ? sqobj.status : sqobj.other; }
";
    for level in ["-O0", "-O2"] {
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with("vol_member_select", triple, src, &[level]);
            let select = if triple == X86_64_LINUX {
                "cmov"
            } else {
                "csel"
            };
            assert_body_lacks(
                &asm,
                "probe",
                select,
                &format!(
                    "a volatile member read cannot be speculated, so the arms may not \
                     collapse into a conditional move: {level} on {triple}"
                ),
            );
        }
    }
}

/// The control for the test above: with no volatile member, the branchless
/// select is still allowed, so that test is asserting the qualifier and not
/// merely that c17 stopped emitting conditional moves.
#[test]
fn memopt_a_plain_member_may_still_be_speculated() {
    let src = "\
struct S { unsigned one; unsigned other; };
struct S pqsel;
unsigned probe(int c) { return c ? pqsel.one : pqsel.other; }
";
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("plain_member_select", triple, src, &["-O2"]);
        let select = if triple == X86_64_LINUX {
            "cmov"
        } else {
            "csel"
        };
        assert_body_contains(
            &asm,
            "probe",
            select,
            "two ordinary member reads are pure and may still collapse to a select",
        );
    }
}

/// A `volatile` bit-field read is observable, even though the access is of the
/// carrier and the carrier can never carry the qualifier.
///
/// The bit-field emitters build their load and store at
/// `bitfield_storage_type`, which is the unqualified storage unit, so nothing
/// derived the marker from the access type. `mark_volatile_access` anticipates
/// exactly this ("a bit-field reads a storage unit whose type is the carrier")
/// and preserves a marker the site sets itself — neither emitter set one, and
/// the read was deleted outright from `-O1` up. Both spellings are covered: the
/// field declared `volatile`, and an ordinary field of a `volatile` object.
#[test]
fn memopt_a_volatile_bitfield_read_is_performed() {
    let src = "\
struct B { volatile unsigned f : 3; unsigned g : 5; };
struct B bfqobj;
volatile struct B vbfqobj;
void probe_field(void) { bfqobj.f; }
void probe_object(void) { vbfqobj.g; }
";
    for level in ["-O0", "-O1", "-O2", "-Os"] {
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with("vol_bitfield", triple, src, &[level]);
            for (func, object) in [("probe_field", "bfqobj"), ("probe_object", "vbfqobj")] {
                assert_body_contains(
                    &asm,
                    func,
                    object,
                    &format!(
                        "a volatile bit-field read is observable: {func} at {level} \
                         on {triple} must still access `{object}`"
                    ),
                );
            }
        }
    }
}

/// The control: an ordinary bit-field read is still dead code, so the test
/// above cannot pass by marking every bit-field access volatile.
#[test]
fn memopt_a_plain_bitfield_read_is_still_removed() {
    let src = "\
struct B { unsigned f : 3; };
struct B pbfqobj;
void probe(void) { pbfqobj.f; }
";
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("plain_bitfield", triple, src, &["-O2"]);
        assert_body_lacks(
            &asm,
            "probe",
            "pbfqobj",
            "reading an ordinary bit-field has no effect and is dead code",
        );
    }
}

/// No copy survives optimization when every one of them is a no-op.
///
/// Promotion out of memory gives every read of a local its own `Copy`, and
/// nothing removed them: five ordinary functions came out of `-O2` with more
/// than a quarter of their IR as copies and close to half their instructions
/// as register-to-register moves, and aarch64 swapped `a + b`'s operands
/// through a temporary. Copy propagation forwards each use to the source.
#[test]
fn memopt_no_op_copies_are_propagated_away() {
    let src = "\
int add2(int a, int b) { return a + b; }
long sum(const long *p, int n) { long s = 0; for (int i = 0; i < n; i++) s += p[i]; return s; }
int maxi(int a, int b, int c) { int m = a; if (b > m) m = b; if (c > m) m = c; return m; }
unsigned hash(const char *s) { unsigned h = 5381; while (*s) h = h * 33 + (unsigned char)*s++; return h; }
";
    for target in [X86_64_LINUX, AARCH64_LINUX] {
        let ir = post_opt_ir("copyprop", src, target);
        let copies: Vec<&str> = ir.lines().filter(|l| l.contains("= copy.")).collect();
        assert!(
            copies.is_empty(),
            "{target}: copies left:\n{}",
            copies.join("\n")
        );
    }
    // And the program still computes what it did.
    let run = format!(
        "{src}int main(void) {{ long a[] = {{1, 2, 3}}; \
         return add2(2, 3) == 5 && sum(a, 3) == 6 && maxi(1, 7, 3) == 7 \
         && hash(\"\") == 5381 ? 0 : 1; }}\n"
    );
    assert_eq!(compile_and_run_optimized("copyprop_run", &run), 0);
}

/// Each `strlen` below folds only once the `printf` guarding the one before
/// it is proved dead and deleted -- the call sees the array, so it stands
/// between the two -- which makes a chain one fold per round of the
/// optimizer's loop, longer than its iteration cap. A function cut off there
/// is less optimized, never wrong; and one that does reach its fixed point is
/// not reported under `--dump-ir`.
#[test]
fn memopt_a_function_cut_off_by_the_iteration_cap_is_still_correct() {
    let check = "{ const char *s = (E); unsigned n = __builtin_strlen(s); \
                 if (n != N) { __builtin_printf(\"%s\\n\", s); ++fails; } }";
    let mut body = String::new();
    for k in 0..16 {
        let step = check
            .replace('E', &format!("&a[{}]", k % 4))
            .replace('N', &(4 - k % 4).to_string());
        body.push_str(&step);
        body.push('\n');
    }
    let src = format!(
        "unsigned fails;\n\
         static void chain(void) {{\n\
         const char a[] = \"1234\";\n\
         {body}}}\n\
         int main(void) {{ chain(); return fails != 0; }}\n"
    );
    assert_eq!(at_o2("itercap", &src), 0);
    if let Some(rc) = compile_and_run_aarch64("itercap", &src, "-O2") {
        assert_eq!(rc, 0, "aarch64");
    }

    let ir = post_opt_ir(
        "itercap_note",
        "int f(int x) { return x + 1; }\n",
        X86_64_LINUX,
    );
    assert!(
        !ir.contains("did not reach a fixed point"),
        "a converged function is not reported:\n{ir}"
    );
}
