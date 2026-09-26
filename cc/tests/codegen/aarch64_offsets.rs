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

/// An inline-asm memory operand far from the frame pointer. The operand is
/// substituted into the template as text, so it has to be an address the
/// template's own `ldr`/`str` can encode; c17 rendered `[x29, #40016]`.
const ASM_FAR_MEMORY: &str = r#"
#define NI __attribute__((noinline))
volatile long sink;
NI void touch(void *p) { sink += *(volatile char *)p; }

/* A memory operand far from the frame pointer: the template's ldr/str
   must be handed an address it can encode. */
NI long asm_far_memory(long seed)
{
    volatile char lo[40000];
    long x = seed;
    volatile char hi[40000];
    long out;
    touch((void *)lo); touch((void *)hi); touch(&x);
    __asm__ volatile("ldr %0, %1" : "=r"(out) : "m"(x));
    __asm__ volatile("str %1, %0" : "=m"(x) : "r"(out + 1));
    return x + out;
}

int main(void) { return asm_far_memory(20) == 41 ? 0 : 1; }
"#;

#[test]
fn aarch64_far_inline_asm_memory_operand() {
    for opt in ["-O0", "-O2"] {
        if let Some(code) = compile_and_run_aarch64("a64_far_asm_mem", ASM_FAR_MEMORY, opt) {
            assert_eq!(code, 0, "at {opt}");
        }
    }
}
