//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 immediates that do not encode: frame offsets, member offsets, and
// add/sub immediates past what one instruction can hold.
//

use crate::common::compile_and_run_aarch64;

/// Every class of access the aarch64 backend emitted with an immediate no
/// instruction encodes. A load or store reaches 4095 elements of its own
/// size (4 KB for a byte, 32 KB for a doubleword) and an unaligned one only
/// -256..255; an `add`/`sub` immediate is twelve bits, optionally shifted by
/// twelve, so 16 MiB at most. c17 printed whatever offset it had, so the
/// assembler rejected the output -- more than a thousand errors for this
/// program -- where gcc builds and runs it.
///
/// Covered: scalars of each width placed far from the frame pointer, values
/// spilled in a big frame, stacked incoming arguments and the variadic save
/// area above the locals, a packed member at offset 4001, 4 KiB- and
/// 64 KiB-aligned locals, more than 4 KB of outgoing arguments, a 40 KB
/// struct copy and zero, and a 20 MiB frame (run on a thread with the stack
/// for it). Headers are declared by hand so it cross-compiles without a
/// sysroot.
fn far_offsets_source() -> String {
    const N: usize = 600;
    let params: Vec<String> = (0..N).map(|i| format!("long a{i}")).collect();
    let sum: Vec<String> = (0..N).map(|i| format!("a{i}")).collect();
    let args: Vec<String> = (0..N).map(|i| i.to_string()).collect();
    FAR_OFFSETS
        .replace("@PARAMS@", &params.join(", "))
        .replace("@SUM@", &sum.join(" + "))
        .replace("@ARGS@", &args.join(", "))
        .replace("@EXPECT@", &(N * (N - 1) / 2).to_string())
}

const FAR_OFFSETS: &str = r#"
#include <stdarg.h>

/* No system headers, so that this cross-compiles without a sysroot:
   glibc's aarch64 pthread_attr_t is 64 bytes, pthread_t a word. */
typedef union { char size[64]; long align; } pthread_attr_t;
typedef unsigned long pthread_t;
int pthread_attr_init(pthread_attr_t *);
int pthread_attr_setstacksize(pthread_attr_t *, unsigned long);
int pthread_create(pthread_t *, const pthread_attr_t *, void *(*)(void *), void *);
int pthread_join(pthread_t, void **);

#define NI __attribute__((noinline))
volatile long sink;
NI void touch(void *p) { sink += *(volatile char *)p; }

/* Every access width, sitting between two arrays so that some of them are
   far from the frame pointer whatever order the frame is laid out in. */
NI long scalars_far_away(long seed)
{
    volatile char lo[40000];
    char c = (char)seed;
    short s = (short)(seed * 2);
    int i = (int)(seed * 3);
    long l = seed * 4;
    float f = (float)(seed * 5);
    double d = (double)(seed * 6);
    long double ld = (long double)(seed * 7);
    double _Complex z;
    ((double *)&z)[0] = seed * 8.0;   /* C17 6.2.5p13: real, then imaginary */
    ((double *)&z)[1] = seed * 9.0;
    __int128 q = (__int128)seed << 70;
    volatile char hi[40000];
    lo[0] = 1; hi[39999] = 2;
    touch((void *)lo); touch((void *)hi);
    touch(&c); touch(&s); touch(&i); touch(&l); touch(&f); touch(&d);
    touch(&ld); touch(&z); touch(&q);
    return c + s + i + l + (long)f + (long)d + (long)ld + (long)((double *)&z)[0]
         + (long)((double *)&z)[1] + (long)(q >> 70) + lo[0] + hi[39999];
}

/* Values live across calls in a big frame: at -O2 they are spilled. */
NI long spills_in_a_big_frame(long x)
{
    volatile char big[70000];
    long a = x + 1, b = x + 2, c = x + 3, d = x + 4, e = x + 5, f = x + 6;
    double g = x + 7.5, h = x + 8.5;
    touch((void *)big);
    long r = a * b + c * d + e * f;
    touch((void *)big);
    return r + (long)(g * h) + a + b + c + d + e + f;
}

/* Stacked incoming arguments above a big frame. */
NI long incoming_stack_args(long a0, long a1, long a2, long a3, long a4,
                            long a5, long a6, long a7, long a8, long a9,
                            double d0, double d1, double d2, double d3,
                            double d4, double d5, double d6, double d7,
                            double d8, double d9)
{
    volatile char big[40000];
    touch((void *)big);
    return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9
         + (long)(d0 + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9);
}

/* The variadic register save area sits above the locals. */
NI long variadic_big_frame(int n, ...)
{
    volatile char big[40000];
    touch((void *)big);
    va_list ap;
    va_start(ap, n);
    long total = 0;
    for (int k = 0; k < n; k++)
        total += va_arg(ap, long);
    double f = va_arg(ap, double);
    va_end(ap);
    return total + (long)f;
}

/* A member no scaled offset reaches, and one that is not aligned at all. */
struct __attribute__((packed)) packed_far {
    char pad[4001];
    int y;
    long z;
    double w;
    short s;
};
NI long packed_member(struct packed_far *p)
{
    p->y += 1; p->z += 2; p->w += 3.0; p->s += 4;
    return p->y + p->z + (long)p->w + p->s;
}

/* Over-aligned locals: the aligned frame base is past the add range. */
NI int over_aligned(void)
{
    _Alignas(4096) char page[64];
    _Alignas(65536) char block[64];
    touch(page); touch(block);
    return ((unsigned long)page % 4096 == 0) && ((unsigned long)block % 65536 == 0);
}

/* More than 4 KB of stacked outgoing arguments. */
NI long many_args(@PARAMS@) { return @SUM@; }

/* Struct copy and zero past the scaled offset range. */
struct big_s { long v[5000]; };
NI long struct_copy_and_zero(struct big_s *src)
{
    struct big_s a = *src;
    struct big_s b = {0};
    touch(&a); touch(&b);
    return a.v[0] + a.v[4999] + b.v[0] + b.v[4999];
}

/* A frame past 16 MiB, run on a thread with room for it. */
NI long huge_frame(long seed)
{
    volatile char big[20 * 1024 * 1024];
    long x = seed;
    big[0] = 1; big[sizeof big - 1] = 2;
    touch((void *)big);
    char *p = (char *)&big[sizeof big - 1];
    return x + big[0] + *p;
}
static long huge_result;
static void *huge_thread(void *arg) { huge_result = huge_frame((long)arg); return 0; }

int main(void)
{
    if (scalars_far_away(1) != 1+2+3+4+5+6+7+8+9+1+1+2) return 1;
    if (spills_in_a_big_frame(1) != 2*3 + 4*5 + 6*7 + (long)(8.5*9.5) + 2+3+4+5+6+7) return 2;
    if (incoming_stack_args(0,1,2,3,4,5,6,7,8,9, 0,1,2,3,4,5,6,7,8,9) != 45 + 45) return 3;
    if (variadic_big_frame(10, 1L,2L,3L,4L,5L,6L,7L,8L,9L,10L, 5.0) != 60) return 4;
    static struct packed_far pf;
    pf.y = 10; pf.z = 20; pf.w = 30.0; pf.s = 40;
    if (packed_member(&pf) != 11 + 22 + 33 + 44) return 5;
    if (!over_aligned()) return 6;
    if (many_args(@ARGS@) != @EXPECT@L) return 7;
    static struct big_s s;
    s.v[0] = 3; s.v[4999] = 4;
    if (struct_copy_and_zero(&s) != 7) return 8;
    pthread_attr_t attr;
    pthread_t t;
    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr, 64 * 1024 * 1024);
    if (pthread_create(&t, &attr, huge_thread, (void *)5L)) return 9;
    pthread_join(t, 0);
    if (huge_result != 5 + 1 + 2) return 10;
    return 0;
}
"#;

#[test]
fn aarch64_far_offsets_assemble_and_run() {
    let src = far_offsets_source();
    for opt in ["-O0", "-O2"] {
        if let Some(code) = compile_and_run_aarch64("a64_far_offsets", &src, opt) {
            assert_eq!(code, 0, "at {opt}");
        }
    }
}

/// One asm statement with more memory operands than there are allocatable
/// registers, written two ways.
///
/// Over the locals themselves (`through_pointers` false), each operand is a
/// named object at a constant offset and is addressed where it lives,
/// `[x29, #N]`, with no register spent -- as gcc does. That is what lets the
/// statement have more memory operands than the six scratch registers c17
/// has for addresses; routed through an address each, it was an error.
///
/// Through opaque pointers (`through_pointers` true), each operand's address
/// is a run-time value, and with this many some are spilled. c17 once
/// rendered a spilled one as its spill slot, `[x29, #N]`, which is the saved
/// pointer rather than the object: the template read and wrote the wrong
/// memory with nothing to say so. Those addresses are loaded into X16, X17,
/// the unspent codegen scratches and X15, in that order, before the template.
///
/// Every kind is present -- `"+m"`, `"=m"`, `"m"`, and a `char` accessed by
/// `ldrb`/`strb` -- and x9/x10 are the template's clobbered temporaries,
/// which also keeps them out of the address pool. The frame is past the
/// scaled load range, so a spill slot far from `x29` is legalized as well.
/// gcc builds and runs it, within its 30-operand limit; before the fix c17
/// returned 100 (the sum of the `"m"` inputs read pointers).
fn spilled_memory_operands_source(through_pointers: bool, extra_inputs: usize) -> String {
    let mut plan = vec!["rw", "ch"];
    plan.extend(std::iter::repeat_n("wo", 10));
    plan.extend(std::iter::repeat_n("ro", 10));
    plan.extend(["rw", "ch"]);
    plan.extend(std::iter::repeat_n("ro", extra_inputs));

    let (mut decls, mut outs, mut ins, mut checks) = (vec![], vec![], vec![], vec![]);
    outs.push(r#"[sum] "=m"(sum)"#.to_string());
    let mut body = String::from(r"str xzr, %[sum]\n\t");
    let mut sum = 0;
    let mut pointers = vec![];
    for (i, kind) in plan.iter().enumerate() {
        // The operand expression: the object, or the object through a
        // pointer the compiler cannot see through.
        let e = if through_pointers {
            let t = if *kind == "ch" { "char" } else { "long" };
            pointers.push(format!("    {t} *p{i} = opaque(&m{i});"));
            format!("*p{i}")
        } else {
            format!("m{i}")
        };
        match *kind {
            "ch" => {
                decls.push(format!("    char m{i} = {i};"));
                outs.push(format!(r#"[m{i}] "+m"({e})"#));
                body.push_str(&format!(
                    r"ldrb w9, %[m{i}]\n\tadd w9, w9, #1\n\tstrb w9, %[m{i}]\n\t"
                ));
                checks.push(format!("    if (m{i} != {i} + 1) return {};", i + 1));
            }
            "rw" => {
                decls.push(format!("    long m{i} = {i} * 1000;"));
                outs.push(format!(r#"[m{i}] "+m"({e})"#));
                body.push_str(&format!(
                    r"ldr x9, %[m{i}]\n\tadd x9, x9, #1\n\tstr x9, %[m{i}]\n\t"
                ));
                checks.push(format!("    if (m{i} != {i} * 1000 + 1) return {};", i + 1));
            }
            "wo" => {
                decls.push(format!("    long m{i} = {i} * 1000;"));
                outs.push(format!(r#"[m{i}] "=m"({e})"#));
                body.push_str(&format!(r"mov x9, #{}\n\tstr x9, %[m{i}]\n\t", i + 7));
                checks.push(format!("    if (m{i} != {}) return {};", i + 7, i + 1));
            }
            _ => {
                decls.push(format!("    long m{i} = {i} * 1000;"));
                ins.push(format!(r#"[m{i}] "m"({e})"#));
                body.push_str(&format!(
                    r"ldr x9, %[m{i}]\n\tldr x10, %[sum]\n\tadd x10, x10, x9\n\tstr x10, %[sum]\n\t"
                ));
                sum += i * 1000;
            }
        }
    }
    format!(
        r#"#define NI __attribute__((noinline))
volatile long sink;
NI void touch(void *p) {{ sink += *(volatile char *)p; }}
NI void *opaque(void *p) {{ return p; }}

NI int many_memory_operands(void)
{{
    volatile char lo[40000];
{decls}
{pointers}
    long sum;
    touch((void *)lo);
    __asm__ volatile(
        "{body}"
        : {outs}
        : {ins}
        : "x9", "x10", "memory");
{checks}
    if (sum != {sum}) return 100;
    return 0;
}}

int main(void) {{ return many_memory_operands(); }}
"#,
        decls = decls.join("\n"),
        pointers = pointers.join("\n"),
        outs = outs.join(", "),
        ins = ins.join(", "),
        checks = checks.join("\n"),
    )
}

#[test]
fn aarch64_spilled_inline_asm_memory_operands() {
    for (through_pointers, extra) in [(false, 0), (true, 0), (true, 1)] {
        let src = spilled_memory_operands_source(through_pointers, extra);
        for opt in ["-O0", "-O2"] {
            if let Some(code) = compile_and_run_aarch64("a64_asm_spilled_mem", &src, opt) {
                assert_eq!(
                    code, 0,
                    "at {opt}, through pointers: {through_pointers}+{extra}"
                );
            }
        }
    }
}

/// Over the locals: every operand is addressed in place and no register is
/// loaded with an address for the template.
#[test]
fn aarch64_local_inline_asm_memory_operands_are_addressed_in_place() {
    let asm = crate::common::asm_for_at(
        "a64_asm_mem_in_place",
        &spilled_memory_operands_source(false, 0),
        &["--target", "aarch64-unknown-linux-gnu"],
    );
    let start = asm.find("many_memory_operands:").expect("the function");
    let body = &asm[start
        ..asm[start..]
            .find(".cfi_endproc")
            .map_or(asm.len(), |e| start + e)];
    assert!(body.contains("ldrb w9, [x29, #"), "{body}");
    assert!(body.contains("str xzr, [x29, #"), "{body}");
    // No operand is named through an address register. The zeroing loop's
    // post-indexed `[x16], #16` is not an operand, hence the line end.
    for reg in ["x16", "x17", "x11", "x15"] {
        assert!(!body.contains(&format!(", [{reg}]\n")), "{reg}:\n{body}");
    }
}

/// Through pointers: a spilled operand's address is loaded before the
/// template and the operand names the register, X15 last; an operand whose
/// address is already in a register still names that register. Each address
/// is a register demand of the statement and is colored first, so it takes
/// one more address than the allocator has registers -- 25 against 21, with
/// x9/x10 clobbered -- to reach all four scratch registers. That is gcc's
/// 30-operand limit exactly.
#[test]
fn aarch64_spilled_inline_asm_memory_operand_shape() {
    let asm = crate::common::asm_for_at(
        "a64_asm_mem_shape",
        &spilled_memory_operands_source(true, 1),
        &["--target", "aarch64-unknown-linux-gnu"],
    );
    for reg in ["x16", "x17", "x11", "x15"] {
        assert!(
            asm.contains(&format!("    ldr {reg}, [x29, #")),
            "{reg}:\n{asm}"
        );
        assert!(asm.contains(&format!(", [{reg}]")), "{reg}:\n{asm}");
    }
    // The legalizer writes X15 while expanding anything, so its load is the
    // last instruction before the template.
    let x15 = asm.find("    ldr x15, [x29, #").unwrap();
    assert!(
        asm[x15..].lines().nth(1).unwrap().contains("str xzr, ["),
        "{asm}"
    );
    // `sum` is a local, so it is addressed in place even here; the operands
    // through pointers never are.
    assert!(asm.contains("str xzr, [x29, #"), "{asm}");
    assert!(!asm.contains("ldrb w9, [x29"), "{asm}");
    assert!(asm.contains("ldrb w9, [x"), "{asm}");
}
