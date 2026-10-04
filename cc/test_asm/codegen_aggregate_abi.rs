//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Aggregates in the calling convention: the assembly half, compiled in
// process. The programs that run are `tests/codegen/aggregate_abi.rs`.
//

use super::asm_probe::{asm_for_with, body_of, AARCH64_LINUX, X86_64_LINUX};

/// A register-returned aggregate must be stored through the frame's own base
/// register, not through `%rbp`.
///
/// When a local's alignment exceeds the stack's, the prologue realigns `%rsp`
/// and keeps the frame's base in a second register; `stack_mem` then addresses
/// every local relative to that. Six sites in the call path spelled
/// `-(slot + callee_saved_offset)(%rbp)` by hand instead, so under such a
/// frame the return value was written to an address nothing reads back --
/// `movq %rax, -112(%rbp)` followed by `movq 32(%rbx), %rax`.
///
/// The behavioural half, swept over the optimization levels, is
/// `codegen_aggregate_return_into_an_over_aligned_frame` in
/// `tests/codegen/aggregate_abi.rs`.
#[test]
fn codegen_aggregate_return_into_an_over_aligned_frame_names_no_rbp() {
    // The behavioural check only fails when the address written happens
    // to matter, so pin the property itself: in a function whose frame is
    // realigned, no aggregate-return store may name `%rbp`.
    let probe = r#"
struct TwoInt { long long a, b; };
struct TwoInt make(void);
void use(char *);
long realigned(void)
{
    _Alignas(32) char pad[64];   /* escapes, so it keeps its slot */
    struct TwoInt r = make();
    use(pad);
    return r.a + r.b + pad[0];
}
"#;
    let asm = asm_for_with("agg_ret_base_reg", X86_64_LINUX, probe, &["-O1"]);
    let body = body_of(&asm, "realigned");
    assert!(
        body.contains("andq $-32, %rsp"),
        "the frame is realigned:\n{body}"
    );
    for reg in ["%rax", "%rdx"] {
        for line in body.lines() {
            let line = line.trim();
            if line.starts_with(&format!("movq {reg}, ")) && line.contains("(%rbp)") {
                panic!(
                    "the aggregate-return store must go through the realigned \
                     frame base, not %rbp: `{line}`\n{body}"
                );
            }
        }
    }
}

// ============================================================================
// Regression: pointer scaling by a type past the old 512 MB bound
// ============================================================================

/// Indexing scales by the element's **byte size**, at every size the compiler
/// accepts.
///
/// `size_bits` answers a *value* width in a `u32` and saturates for an
/// aggregate past `u32::MAX` bits. While the object-size bound was that same
/// number the saturation was unreachable -- the parser refused any type that
/// could reach it. Raising the bound made it reachable, and every site still
/// deriving a byte count as `size_bits / 8` began answering 536870911 for any
/// larger type: `&a[1][0] - &a[0][0]` on a `char[3][600000000]`, the stride of
/// an array of a 600 MB struct, and `p + 1` on a pointer to one. `sizeof` was
/// right throughout, so the sizes agreed with gcc while the addresses did not.
///
/// Asserted on the assembly rather than by running: the scale factor is what
/// was wrong, and no test should ask its machine for gigabytes to see it. The
/// objects are `extern` for the same reason -- nothing is defined, allocated
/// or dereferenced.
#[test]
fn codegen_pointer_scaling_past_the_old_object_bound() {
    const SRC: &str = r#"
extern char rows[3][600000000L];
struct Big { char x[600000000L]; };
extern struct Big bigs[2];

char *row(int i) { return rows[i]; }
struct Big *elem(int i) { return &bigs[i]; }
long stride(struct Big *p, int i) { return (char *)&p[i] - (char *)p; }
"#;

    // One target is enough, and x86-64 is the one that materialises the scale
    // as a literal: the element size is computed in `ir/linearize.rs`, before
    // any backend runs, so the defect was target-independent. aarch64 builds
    // the same constant with `movz`/`movk`, which would make this assertion
    // about instruction encoding rather than about the size.
    let asm = asm_for_with("pointer_scale", X86_64_LINUX, SRC, &["-O2"]);
    for func in ["row", "elem", "stride"] {
        let body = body_of(&asm, func);
        assert!(
            body.contains("600000000"),
            "{func} does not scale by the element size:\n{body}"
        );
        assert!(
            !body.contains("536870911"),
            "{func} scales by the saturated size_bits:\n{body}"
        );
    }
}

/// An aggregate is copied by its **byte size**, at every size the compiler
/// accepts.
///
/// The companion to `codegen_pointer_scaling_past_the_old_object_bound`, and
/// the same root cause: `size_bits` saturates at `u32::MAX` bits, and raising
/// the object-size bound made the saturation reachable. The sites that survived
/// that commit's audit were the ones that launder the bit count through a local
/// variable -- two of them spell it `let target_size_bytes = target_size / 8;`,
/// which no grep for `size_bits(..) / 8` can find -- through a `u32` field
/// (`struct_return_size`), or through `ArgClass::Indirect`'s payload.
///
/// Every shape below copied 536870911 bytes of a 600000000-byte object, on both
/// targets, at every optimization level. `a = b` is the one that matters most:
/// it is the plainest aggregate copy in the language.
///
/// Asserted on x86-64 alone, and the source is shaped to stay cheap. Both are
/// load-bearing, not stylistic -- each one avoids a different pre-existing
/// backend blowup that this test walked straight into and that took both Linux
/// CI runners down with SIGTERM:
///
/// - **x86-64 only.** Nothing to do with coverage: the length is computed in
///   `ir/` before any backend runs, so one target proves it, and x86-64 is the
///   one that materialises the constant as a literal rather than as
///   `movz`/`movk`. An `AARCH64_LINUX` assertion once cost **12.5 seconds and
///   16 GB** resident, because `initialize`'s 600 MB local went through an
///   unrolled zeroing of the frame. Nothing zeroes the frame now; nobody has
///   re-measured the rest of the aarch64 path at this size, so keep the list
///   as it is.
/// - **`by_value_param` does not pass its argument on.** The prologue copy is
///   the site under test. Sending a 600 MB aggregate used to cost 16 seconds
///   and 21.9 GB of compiler memory, one load/store pair per eightbyte; the
///   outgoing copy is a `rep movsq` now, but it is not what this test is about.
///
/// Everything here is `extern`; nothing is defined or run.
#[test]
fn codegen_aggregate_copy_length_past_the_old_object_bound() {
    const SRC: &str = r#"
struct Big { char x[600000000L]; };
extern struct Big src, dst;
void sink(struct Big *);

void assign(void) { dst = src; }
void initialize(void) { struct Big loc = src; sink(&loc); }
void by_value_param(struct Big p) { sink(&p); }
struct Big returns_it(void) { return src; }
unsigned long extent(void) { return __builtin_object_size(src.x, 0); }
"#;

    let asm = asm_for_with("aggregate_copy_length", X86_64_LINUX, SRC, &["-O2"]);
    for func in [
        "assign",
        "initialize",
        "by_value_param",
        "returns_it",
        "extent",
    ] {
        let body = body_of(&asm, func);
        assert!(
            body.contains("600000000"),
            "{func} does not use the aggregate's byte size:\n{body}"
        );
        assert!(
            !body.contains("536870911"),
            "{func} uses the saturated size_bits:\n{body}"
        );
    }
}

/// `noreturn` belongs to the function *type*, since that is what a call site
/// reads. Only the first plain file-scope declarator put it there, so a
/// grouped, later or block-scope declarator produced a function the caller
/// believed could return -- visible at -O2 as the code after the call
/// surviving.
#[test]
fn codegen_noreturn_reaches_the_type_from_every_declarator() {
    let shapes = [
        ("first", "void f(void) __attribute__((noreturn));\n", ""),
        ("grouped", "void (f)(void) __attribute__((noreturn));\n", ""),
        ("later", "int x, f(void) __attribute__((noreturn));\n", ""),
        ("block", "", "void f(void) __attribute__((noreturn));"),
        ("block_keyword", "", "_Noreturn void f(void);"),
        // Written among the specifiers, the attribute belongs to every
        // declarator of the list, as gcc has it.
        (
            "specifier",
            "__attribute__((noreturn)) void e(void), f(void);\n",
            "",
        ),
        // And a prototype's attribute carries to a later redeclaration.
        (
            "redeclared",
            "void f(void) __attribute__((noreturn));\nvoid f(void);\n",
            "",
        ),
    ];
    for (name, file_decl, block_decl) in shapes {
        let src = format!("{file_decl}int g(void) {{ {block_decl} f(); return 12345; }}\n");
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with(&format!("noreturn_{name}"), triple, &src, &["-O2"]);
            assert!(
                !body_of(&asm, "g").contains("12345"),
                "{name} on {triple}: the code after a noreturn call must be dead:\n{asm}"
            );
        }
    }
    // The controls: without the attribute the return survives, so the probe
    // above can fail -- and an attribute on a *parameter* is the parameter's,
    // not the function's.
    for (name, decl, call) in [
        ("noreturn_control", "void f(void);", "f()"),
        (
            "noreturn_param",
            "void f(void (*cb)(void) __attribute__((noreturn)));",
            "f(0)",
        ),
    ] {
        let src = format!("{decl}\nint g(void) {{ {call}; return 12345; }}\n");
        let asm = asm_for_with(name, X86_64_LINUX, &src, &["-O2"]);
        assert!(body_of(&asm, "g").contains("12345"), "{name}:\n{asm}");
    }
}

/// The bound itself, not just the answer: above the threshold the zero-fill is
/// a `memset` call, below it is still stores.
///
/// The behavioural test above passes either way — a million unrolled stores
/// produce a correctly zeroed object, just not in a time anyone will wait for.
/// This is the test that the *bound* exists, and the negative half keeps it
/// from being satisfied by calling `memset` for every size, which would cost
/// more than the stores it replaced for a small object.
#[test]
fn codegen_a_large_aggregate_zero_is_a_memset_call() {
    let src = |n: usize| {
        format!("void sink(char *);\nvoid probe(void) {{ char buf[{n}] = {{0}}; sink(buf); }}\n")
    };

    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        // Comfortably over `INLINE_LIMIT_BYTES` (128).
        let big = asm_for_with("aggzero_big", triple, &src(4096), &["-O2"]);
        assert!(
            big.contains("memset"),
            "a 4096-byte zero-fill belongs in a memset call, not 512 stores, on {triple}:\n{big}"
        );

        // And the unrolled form is still used where it is cheaper than a call.
        let small = asm_for_with("aggzero_small", triple, &src(16), &["-O2"]);
        assert!(
            !small.contains("memset"),
            "a 16-byte zero-fill is cheaper unrolled than called, on {triple}:\n{small}"
        );
    }
}
