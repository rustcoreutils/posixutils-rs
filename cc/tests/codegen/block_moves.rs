//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Blocks of bytes the back ends move, and the two rules they obey.
//
// An object's bytes are moved in descending power-of-two chunks, and the moves
// are bounded: past a threshold the copy becomes a bulk primitive instead of an
// unrolled run. `memexpand::block_chunks` and `BlockOp::limit` state both rules
// for the IR, and `codegen_struct_copy_across_the_inline_threshold` records the
// incident that put them there.
//
// The back ends cannot call `memcpy` -- they are past the point where a call
// can be synthesized -- so their bulk primitive is `rep movsq` or a counted
// loop. But the chunk rule is the same, and each site that wrote its own copy
// of it either rounded the size *up* or dropped the bound:
//
//   * a two-SSE struct parameter stored both halves at eight bytes, so the
//     four-byte high half of a 12-byte struct wrote four bytes past it;
//   * the spilled-parameter prologue stepped eight regardless of width;
//   * `va_arg` of a large aggregate unrolled with no bound at all;
//   * the stacked-argument copy rounded the *source* read up, which is not the
//     same as the destination: argument slots really are eightbyte-granular, so
//     writing eight is correct where reading eight is not.
//
// Sizes here are deliberately not multiples of eight, and guards sit either
// side of the objects, so a rounded-up move shows as a wrong byte rather than
// landing in padding and passing by luck.
//

use crate::codegen::asm_probe::{asm_for_with, body_of, AARCH64_LINUX, X86_64_LINUX};
use crate::common::{compile_and_run, compile_and_run_everywhere};

/// How many instructions the body of `func` has.
fn body_insns(asm: &str, func: &str) -> usize {
    body_of(asm, func)
        .lines()
        .filter(|l| {
            let t = l.trim();
            !t.is_empty() && !t.starts_with('.') && !t.starts_with('#') && !t.ends_with(':')
        })
        .count()
}

/// A two-SSE struct parameter's high half is as wide as the half, not as wide
/// as a register.
///
/// `struct P { float x, y, z; }` is classified into two SSE eightbytes, but the
/// second holds only four bytes. The prologue used one `FpSize` for both, so it
/// stored eight and wrote four bytes past the object.
#[test]
fn codegen_a_two_sse_struct_parameter_stores_only_its_own_bytes() {
    let src = "\
struct P { float x, y, z; };
__attribute__((noinline)) float probe(struct P p) { float g = 99.f; return p.x + p.y + p.z + g; }
";
    let asm = asm_for_with("two_sse_param", X86_64_LINUX, src, &["-O0"]);
    let body = body_of(&asm, "probe");
    let wide = body.matches("movsd").count();
    assert!(
        wide <= 1,
        "the high half of a 12-byte two-SSE struct is 4 bytes, so at most one \
         8-byte fp store belongs in the prologue; found {wide}:\n{body}"
    );
}

/// The spilled-parameter prologue moves no more than the parameter.
///
/// `copy_incoming_arg_to_local` stepped eight bytes at a time regardless of
/// width, so a 12-byte struct read eight bytes at the incoming area's offset 8
/// and wrote eight at the local's -- four past a local that is exactly twelve
/// bytes, because a slot is only rounded up to its type's own alignment.
#[test]
fn codegen_a_spilled_struct_parameter_is_copied_no_wider_than_itself() {
    let src = "\
struct P { int a, b, c; };
__attribute__((noinline)) int probe(long a, long b, long c, long d, long e, long f,
                                    struct P p)
{ return p.a + p.b + p.c; }
";
    let asm = asm_for_with("spilled_param", X86_64_LINUX, src, &["-O0"]);
    let body = body_of(&asm, "probe");

    // The incoming argument area is at a *positive* displacement from %rbp --
    // the saved frame pointer and return address are below it -- so reads of
    // the spilled parameter are the moves from a positive offset. Everything
    // the function writes is at a negative one. Matching on the substring
    // "8(%rbp)" is not enough: "-88(%rbp)" ends with it.
    let incoming_reads = |mnemonic: &str| {
        body.lines()
            .filter_map(|l| {
                let t = l.trim();
                let rest = t.strip_prefix(mnemonic)?.trim_start();
                let (disp, _) = rest.split_once("(%rbp)")?;
                disp.parse::<i64>().ok().filter(|d| *d > 0)
            })
            .count()
    };

    // A 12-byte object is 8 + 4: exactly one eight-byte read, and the tail read
    // with a four-byte one.
    assert_eq!(
        incoming_reads("movq"),
        1,
        "a 12-byte spilled parameter has one 8-byte chunk, so one 8-byte read \
         of the incoming area; a second means the 4-byte tail was read as 8:\n{body}"
    );
    assert_eq!(
        incoming_reads("movl"),
        1,
        "and its 4-byte tail is read with a 4-byte move:\n{body}"
    );
}

/// `va_arg` of a large aggregate is bounded, like every other block move.
///
/// The `va_arg` byte copy had no limit, so fetching a 4 KB aggregate emitted one
/// load/store pair per chunk -- about 1100 instructions on each target, and
/// linear in the object, so a 256 KB aggregate would be the compile-time
/// explosion `emit_aggregate_zero` used to be.
#[test]
fn codegen_va_arg_of_a_large_aggregate_is_bounded() {
    let src = "\
#include <stdarg.h>
struct Big { char c[4096]; };
void sink(struct Big *);
void probe(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    struct Big b = va_arg(ap, struct Big);
    sink(&b);
    va_end(ap);
}
";
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("va_arg_big", triple, src, &["-O2"]);
        let n = body_insns(&asm, "probe");
        assert!(
            n < 200,
            "fetching a 4096-byte aggregate through va_arg must use a bulk copy, \
             not one pair per chunk: {n} instructions on {triple}"
        );
    }
}

/// The stacked-argument copy reads only the object's own bytes.
///
/// The destination is the outgoing argument area, which is allocated in whole
/// eightbytes -- so rounding the *write* up is correct and deliberate. The read
/// is from the object, which is not, and a 12-byte struct read eight bytes at
/// offset 8. Four of them belong to whatever follows it, and the read faults if
/// the object ends a page.
#[test]
fn codegen_a_stacked_argument_reads_only_its_object() {
    let src = "\
struct P { int a, b, c; };
void g(long, long, long, long, long, long, struct P);
void probe(struct P p) { g(1, 2, 3, 4, 5, 6, p); }
";
    let asm = asm_for_with("stacked_arg_src", X86_64_LINUX, src, &["-O0"]);
    let body = body_of(&asm, "probe");

    // Only the *read* is constrained. In AT&T order the memory operand of a
    // load comes first, which is what distinguishes `movq 8(%r11), %rax` --
    // reading four bytes past a 12-byte object -- from `movq %rax, 8(%rsp)`,
    // a write into the outgoing argument area. That area is allocated in whole
    // eightbytes and its padding is unspecified, so the store's width is the
    // back end's choice and this test does not pin it.
    let wide_source_reads = body
        .lines()
        .filter(|l| {
            let t = l.trim();
            let Some(operands) = t.strip_prefix("movq ") else {
                return false;
            };
            let Some((src_operand, _)) = operands.split_once(',') else {
                return false;
            };
            let src_operand = src_operand.trim();
            src_operand.starts_with("8(%r") && !src_operand.contains("%rbp")
        })
        .count();
    assert_eq!(
        wide_source_reads, 0,
        "a 12-byte object has 4 bytes at offset 8, so reading 8 there is 4 past it:\n{body}"
    );
}

/// The answers, with guards, across the sizes these paths classify differently.
///
/// The assembly checks above pin the widths; this pins that the values survive.
/// None of the sizes is a multiple of eight and every object is fenced, so an
/// over-copy shows up as a clobbered guard rather than as padding nobody reads.
#[test]
fn codegen_block_moved_parameters_keep_their_values() {
    let code = r#"
#include <stdarg.h>

struct P3 { int a, b, c; };            /* 12 bytes, spilled/stacked */
struct F3 { float x, y, z; };          /* 12 bytes, two SSE */
struct B7 { unsigned char c[7]; };     /* 7 bytes */
struct B13 { unsigned char c[13]; };   /* 13 bytes */

__attribute__((noinline)) int take_p3(long a, long b, long c, long d, long e,
                                      long f, struct P3 p)
{ return p.a + p.b + p.c; }

__attribute__((noinline)) float take_f3(struct F3 p) { return p.x + p.y + p.z; }

__attribute__((noinline)) int take_b7(struct B7 v)
{
    int s = 0;
    for (int i = 0; i < 7; i++) s += v.c[i];
    return s;
}

__attribute__((noinline)) int take_b13(long a, long b, long c, long d, long e,
                                       long f, struct B13 v)
{
    int s = 0;
    for (int i = 0; i < 13; i++) s += v.c[i];
    return s;
}

__attribute__((noinline)) int va_b13(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    struct B13 v = va_arg(ap, struct B13);
    va_end(ap);
    int s = 0;
    for (int i = 0; i < 13; i++) s += v.c[i];
    return s;
}

int main(void)
{
    unsigned char lo = 0xA5;
    struct P3 p = {1, 2, 3};
    struct F3 f = {1.f, 2.f, 4.f};
    struct B7 b7;
    struct B13 b13;
    unsigned char hi = 0x5A;

    for (int i = 0; i < 7; i++) b7.c[i] = (unsigned char)(i + 1);
    for (int i = 0; i < 13; i++) b13.c[i] = (unsigned char)(i + 1);

    if (take_p3(1, 2, 3, 4, 5, 6, p) != 6) return 1;
    if (take_f3(f) != 7.f) return 2;
    if (take_b7(b7) != 28) return 3;
    if (take_b13(1, 2, 3, 4, 5, 6, b13) != 91) return 4;
    if (va_b13(0, b13) != 91) return 5;
    if (lo != 0xA5 || hi != 0x5A) return 6;
    return 0;
}
"#;
    assert_eq!(compile_and_run("block_moved_params", code, &[]), 0);
}

/// A zero-sized aggregate -- GNU's `struct {}`, or `struct { int a[0]; }` --
/// moves no bytes however it is read: by name, through `*p`, `s.m`, `a[i]`,
/// a compound literal, a conditional, a statement expression, a call's
/// result, or as an argument.
///
/// Read as an rvalue, it was "loaded" at zero bits wherever the reading site
/// spelled the by-address rule as `size > 64`, and the store of that value
/// was lowered as one byte -- over the member that follows it. The members
/// around it here are the guards.
#[test]
fn codegen_a_zero_sized_aggregate_moves_no_bytes() {
    let code = r#"
#include <stdarg.h>

struct E {};
struct Z { int a[0]; };
struct W { char lo; struct E e; char mid; struct Z z; char hi; };

struct E ge1, ge2, garr[4];
static struct E gs;

__attribute__((noinline)) struct E ret_e(void) { struct E e; return e; }
__attribute__((noinline)) struct Z ret_z(void) { struct Z z; return z; }
__attribute__((noinline)) struct E pass_e(struct E e) { return e; }
__attribute__((noinline)) int take(int a, struct E e, int b, struct Z z, int c)
{ (void)e; (void)z; return a * 100 + b * 10 + c; }
__attribute__((noinline)) int va_take(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    int a = va_arg(ap, int);
    struct E e = va_arg(ap, struct E);
    int b = va_arg(ap, int);
    va_end(ap);
    (void)e;
    return n * 100 + a * 10 + b;
}

static int intact(const struct W *w)
{ return w->lo == 0x11 && w->mid == 0x22 && w->hi == 0x33; }

int main(int argc, char **argv)
{
    (void)argv;
    int c = argc > 0, i = argc;
    struct E a, b, arr[3];
    struct Z za, zarr[3];
    struct W w = {0x11, {}, 0x22, {}, 0x33};
    struct W *pw = &w;
    struct E *p = &a;
    static struct E sl;

    w.e = a;                              if (!intact(&w)) return 1;
    w.e = *p;                             if (!intact(&w)) return 2;
    w.e = arr[i];                         if (!intact(&w)) return 3;
    w.e = c ? a : b;                      if (!intact(&w)) return 4;
    w.e = (struct E){};                   if (!intact(&w)) return 5;
    w.e = ({ struct E t; t; });           if (!intact(&w)) return 6;
    w.e = ret_e();                        if (!intact(&w)) return 7;
    w.e = pass_e(w.e);                    if (!intact(&w)) return 8;
    w.e = ge1;                            if (!intact(&w)) return 9;
    w.e = gs;                             if (!intact(&w)) return 10;
    w.e = garr[i];                        if (!intact(&w)) return 11;
    w.e = sl;                             if (!intact(&w)) return 12;
    pw->e = pw->e;                        if (!intact(&w)) return 13;
    w.z = za;                             if (!intact(&w)) return 14;
    w.z = zarr[i];                        if (!intact(&w)) return 15;
    w.z = ret_z();                        if (!intact(&w)) return 16;
    w.z = *&pw->z;                        if (!intact(&w)) return 17;

    struct E x = w.e, y = *p, z = arr[i];
    (void)x; (void)y; (void)z;
    arr[i] = w.e; *p = pw->e; ge1 = ge2; gs = garr[i]; sl = a;
    if (!intact(&w)) return 18;

    if (take(1, w.e, 2, w.z, 3) != 123) return 19;
    if (take(4, ret_e(), 5, ret_z(), 6) != 456) return 20;
    if (take(7, c ? a : b, 8, zarr[i], 9) != 789) return 21;
    if (va_take(1, 2, w.e, 3) != 123) return 22;
    if (!intact(&w)) return 23;
    return 0;
}
"#;
    compile_and_run_everywhere("zero_sized_aggregate", code);
}

/// A zero-sized parameter arrives in nothing, so the prologue stores nothing
/// for it -- it stored a byte of whatever the register held.
#[test]
fn codegen_a_zero_sized_parameter_stores_nothing() {
    let src = "\
struct E {};
__attribute__((noinline)) int probe(struct E a, struct E b) { (void)a; (void)b; return 7; }
";
    let asm = asm_for_with("zero_sized_param", X86_64_LINUX, src, &["-O0"]);
    let body = body_of(&asm, "probe");
    assert!(
        !body.contains("movb"),
        "a zero-sized parameter has no byte to store:\n{body}"
    );
}
