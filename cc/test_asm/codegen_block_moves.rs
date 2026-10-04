//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly cases of tests/codegen/block_moves.rs, in process: blocks of bytes
// the back ends move are moved in descending power-of-two chunks, never wider
// than the object, and the moves are bounded.
//

use crate::test_asm::asm_probe::{asm_for_with, body_of, AARCH64_LINUX, X86_64_LINUX};

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
