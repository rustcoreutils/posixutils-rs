//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__attribute__((ifunc("resolver")))` (GNU, ELF only): the symbol is a
// GNU indirect function, bound at load time to whatever its resolver
// returns -- glibc's string functions and runtime CPU dispatch use it.
// Mach-O has no indirect functions, so the runtime test is Linux-only.
//

#[cfg(target_os = "linux")]
use crate::common::{compile_and_run, compile_and_run_everywhere};

#[cfg(target_os = "linux")]
const IFUNC: &str = r#"
/* `ifunc("resolver")`: the dynamic linker (or the static startup code)
   calls the resolver once and binds the symbol to the function it returns,
   as glibc's string functions and zlib-ng's dispatch do. */
static int impl_b(int x) { return x + 2; }

typedef int (*fn_t)(int);

/* A resolver runs before main and before relocation of everything else is
   complete, so it only reads constants here. */
static fn_t pick(void) { return impl_b; }

int add(int x) __attribute__((ifunc("pick")));

/* A static ifunc, declared before its resolver is defined. */
static int twice(int x) __attribute__((ifunc("pick_twice")));
static int twice_impl(int x) { return 2 * x; }
static void *pick_twice(void) { return (void *)twice_impl; }

/* Referenced through a pointer as well as called directly. */
static int (*volatile padd)(int) = add;

int main(void)
{
    if (add(1) != 3) return 1;
    if (padd(5) != 7) return 2;
    if (twice(21) != 42) return 3;
    return 0;
}
"#;

/// Called directly and through a pointer, global and static, dynamic on the
/// host and static (IRELATIVE) on aarch64.
#[cfg(target_os = "linux")]
#[test]
fn codegen_ifunc_binds_to_the_resolvers_choice() {
    compile_and_run_everywhere("ifunc", IFUNC);
}

/// An executable that is not PIE binds an indirect function through an IPLT
/// entry and an IRELATIVE relocation the startup code applies; PIC code and
/// a PIE through the PLT and GOT the dynamic linker fills. Each must bind.
#[cfg(target_os = "linux")]
#[test]
fn codegen_ifunc_binds_without_and_with_pic() {
    for flags in [&["-no-pie"][..], &["-fPIC"][..], &["-O2", "-no-pie"][..]] {
        let flags: Vec<String> = flags.iter().map(|s| s.to_string()).collect();
        assert_eq!(compile_and_run("ifunc_pic", IFUNC, &flags), 0, "{flags:?}");
    }
}

/// Runtime CPU dispatch as real code writes it: a `target("sse4.1")`
/// function using SSE4.1 intrinsics at the SSE2 baseline, called when the
/// CPU has it, and a `target_clones` function dispatched by its resolver.
/// x86-64 only: the intrinsics are x86's.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn codegen_target_attributes_dispatch_at_run_time() {
    let src = r#"
/* Runtime CPU dispatch the way zlib-ng, xxhash and pixman write it: the
   build targets the SSE2 baseline, one function is compiled for a higher
   ISA with `target(...)` and uses that ISA's intrinsics, and it is called
   only when the CPU has it. `target_clones` makes the compiler build the
   copies and the dispatcher itself. */
#include <smmintrin.h>
#include <string.h>

__attribute__((target("sse4.1"))) static void mul_min_sse41(const unsigned *a, const unsigned *b,
                                                          unsigned *prod, unsigned *mn)
{
    __m128i x = _mm_loadu_si128((const __m128i *)a);
    __m128i y = _mm_loadu_si128((const __m128i *)b);
    _mm_storeu_si128((__m128i *)prod, _mm_mullo_epi32(x, y));
    _mm_storeu_si128((__m128i *)mn, _mm_min_epu32(x, y));
}

static void mul_min_scalar(const unsigned *a, const unsigned *b, unsigned *prod, unsigned *mn)
{
    for (int i = 0; i < 4; i++) {
        prod[i] = a[i] * b[i];
        mn[i] = a[i] < b[i] ? a[i] : b[i];
    }
}

/* A target("...") written on the declaration and the definition, with an
   arch= spelling and a no- feature. */
__attribute__((target("arch=x86-64-v2"))) int popc(unsigned x);
int popc(unsigned x) { return __builtin_popcount(x); }
__attribute__((target("no-sse4.2"))) static int plain(int x) { return x + 1; }

/* target_clones: one body, compiled per ISA, dispatched at load time. */
__attribute__((target_clones("sse4.2", "default"))) int sum(const int *p, int n)
{
    int s = 0;
    for (int i = 0; i < n; i++)
        s += p[i];
    return s;
}

int main(void)
{
    const unsigned a[4] = {1, 0xffffffffu, 0x80000000u, 7};
    const unsigned b[4] = {3, 2, 0x7fffffffu, 0x10000u};
    unsigned p1[4], m1[4], p2[4], m2[4];
    mul_min_scalar(a, b, p1, m1);
    __builtin_cpu_init();
    if (__builtin_cpu_supports("sse4.1")) {
        mul_min_sse41(a, b, p2, m2);
        if (memcmp(p1, p2, sizeof p1) || memcmp(m1, m2, sizeof m1)) return 1;
    }
    if (popc(0xf0f0u) != 8) return 2;
    if (plain(1) != 2) return 3;
    int v[5] = {1, 2, 3, 4, 5};
    if (sum(v, 5) != 15) return 4;
    int (*volatile ps)(const int *, int) = sum;
    if (ps(v, 4) != 10) return 5;
    return 0;
}
"#;
    for level in ["-O0", "-O2"] {
        assert_eq!(
            crate::common::compile_and_run("target_dispatch", src, &[level.to_string()]),
            0,
            "{level}"
        );
    }
}

/// The versions of a `target_clones` function are one function to the
/// program: a static local is shared whichever version runs, a `static`
/// one dispatches as an external one does, and its address is the same
/// from every caller. With the inliner on, a `target` function stays out
/// of a baseline caller and its result is unchanged.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
#[test]
fn codegen_target_clones_are_one_function() {
    let src = r#"
__attribute__((target_clones("sse4.2", "popcnt", "default")))
int counted(int x) { static int calls; return x + ++calls; }

__attribute__((target_clones("sse4.1", "default")))
static int twice(int x) { return 2 * x; }

__attribute__((target("sse4.1"))) static int hi(int x) { return x * 5 + 2; }
static int lo(int x) { return x * 3 + 1; }
__attribute__((target("sse4.1"))) int call_lo(int x) { return lo(x); }
int call_hi(int x) { __builtin_cpu_init(); return __builtin_cpu_supports("sse4.1") ? hi(x) : 17; }

int main(void)
{
    int (*p)(int) = counted;
    if (counted(10) != 11 || p(10) != 12 || counted(10) != 13) return 1;
    int (*q)(int) = twice;
    if (twice(4) != 8 || q(5) != 10 || q != twice) return 2;
    if (call_lo(2) != 7 || call_hi(3) != 17) return 3;
    return 0;
}
"#;
    for flags in [&["-O0"][..], &["-O2"], &["-O2", "-fPIC"]] {
        let flags: Vec<String> = flags.iter().map(|f| f.to_string()).collect();
        assert_eq!(
            crate::common::compile_and_run("target_clones_one", src, &flags),
            0,
            "{flags:?}"
        );
    }
}
