//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Programs built with the x86-64 options a kernel build passes -- the code
// model, no SSE or x87, retpolines and return thunks, patchable entries,
// zeroed registers, initialized automatic variables, the stack guard's
// placement -- linked and run as user programs, where the option allows
// it. The assembly-only checks are `test_asm/codegen_kernel.rs`.
//

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use crate::common::compile_and_run;

fn opts(flags: &[&str]) -> Vec<String> {
    flags.iter().map(|f| f.to_string()).collect()
}

/// A position-dependent executable is linked below 2 GB, where the kernel
/// code model's sign-extended addresses are the same as zero-extended
/// ones, so one built with `-mcmodel=kernel` runs: every address it takes
/// as an immediate (`R_X86_64_32S`) must still be the object's.
#[test]
fn codegen_kernel_code_model_runs_below_2gb() {
    let src = r#"
int g[4] = {1, 2, 3, 4};
extern int puts(const char *);
static int s = 7;
int *addr(void) { return &g[1]; }
int (*fp(void))(const char *) { return puts; }
int *saddr(void) { return &s; }
int *tab[] = { &g[0], &s };
int main(void) {
    if (*addr() != 2 || addr() != &g[1]) return 1;
    if (fp() != puts) return 2;
    if (*saddr() != 7 || tab[1] != saddr() || tab[0] != g) return 3;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run(
            "kernel_model",
            src,
            &opts(&["-mcmodel=kernel", "-fno-pie", "-no-pie"])
        ),
        0
    );
}

/// The kernel's floating-point options, which leave integer code its
/// general registers only.
const NO_FP: &[&str] = &[
    "-mno-sse",
    "-mno-mmx",
    "-mno-sse2",
    "-mno-3dnow",
    "-mno-avx",
    "-mno-80387",
    "-mno-fp-ret-in-387",
    "-mskip-rax-setup",
];

/// An integer program built without SSE or the x87 runs: its variadic
/// functions read their arguments from the general-register save area and
/// the stack, `va_copy` and block copies move their eightbytes through a
/// general register, and its variadic calls leave `%al` unset.
#[test]
fn codegen_kernel_integer_program_without_fp_registers() {
    let src = r#"
#include <stdarg.h>
#include <string.h>
struct big { long a[9]; };
static long sum(int n, ...) {
    va_list ap, aq;
    va_start(ap, n);
    va_copy(aq, ap);
    long s = 0, t = 0;
    for (int i = 0; i < n; i++) s += va_arg(ap, long);
    for (int i = 0; i < n; i++) t += va_arg(aq, long);
    va_end(aq);
    va_end(ap);
    return s == t ? s : -1;
}
static struct big twice(struct big b) { for (int i = 0; i < 9; i++) b.a[i] *= 2; return b; }
static int fmt(char *out, const char *f, ...) {
    va_list ap; va_start(ap, f);
    int n = 0;
    for (; *f; f++) {
        if (*f == '%') { long v = va_arg(ap, long); out[n++] = (char)('0' + v); f++; }
        else out[n++] = *f;
    }
    out[n] = 0; va_end(ap); return n;
}
int main(void) {
    if (sum(10, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L) != 55) return 1;
    struct big b, c;
    for (int i = 0; i < 9; i++) b.a[i] = i;
    c = twice(b);
    struct big d = c;
    for (int i = 0; i < 9; i++) if (d.a[i] != 2 * i || b.a[i] != i) return 2;
    char buf[32];
    if (fmt(buf, "a%d b%d", 3L, 7L) != 5 || strcmp(buf, "a3 b7") != 0) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("kernel_no_fp", src, &opts(NO_FP)), 0);
    let mut general = opts(&["-mgeneral-regs-only", "-mskip-rax-setup"]);
    general.push("-fno-pie".to_string());
    assert_eq!(compile_and_run("kernel_general_regs", src, &general), 0);
}

/// Under `-mpreferred-stack-boundary=3` a function may be entered with
/// `%rsp` 8 bytes off a 16-byte boundary, as a kernel's callers leave it,
/// and still lays a 16-byte aligned local on its alignment: called here
/// through a thunk that skips the usual adjustment, which a function
/// compiled for the 16-byte boundary gets wrong.
#[test]
fn codegen_kernel_eight_byte_boundary_realigns() {
    let src = r#"
#include <stdint.h>
int check(void);
int misaligned_call(void);
__asm__(".text\n.globl misaligned_call\nmisaligned_call:\n\tcall check\n\tret\n");
__attribute__((noinline)) void touch(volatile char *p) { p[0] = 1; }
int check(void) {
    __attribute__((aligned(16))) char b[16];
    touch(b);
    return ((uintptr_t)b & 15) != 0;
}
int main(void) { return misaligned_call() | check(); }
"#;
    let flags = opts(&[
        "-mno-sse",
        "-mpreferred-stack-boundary=3",
        "-fno-pie",
        "-no-pie",
    ]);
    assert_eq!(compile_and_run("stack_boundary_8", src, &flags), 0);
}

/// Indirect calls, a computed goto and returns built with the retpoline
/// mitigations run. With `thunk-extern` the program supplies the thunks, as
/// a kernel does -- here ones that count their calls, so the run shows
/// every indirect branch and return went through them; with `thunk` the
/// unit defines its own, and with `thunk-inline` writes them in place.
/// `-mharden-sls=all`'s `int3`s are never reached.
#[test]
fn codegen_kernel_retpoline_programs_run() {
    let body = r#"
static int add1(int x) { return x + 1; }
static int twice(int x) { return 2 * x; }
int (*volatile ops[2])(int) = { add1, twice };
static int dispatch(int i) {
    static void *labels[] = { &&one, &&two };
    goto *labels[i];
one: return 10;
two: return 20;
}
static int work(void) {
    int r = 0;
    for (int i = 0; i < 4; i++) r += ops[i & 1](i);
    return r + dispatch(1) + dispatch(0);
}
"#;
    let counted = format!(
        r#"{body}
long thunk_calls, ret_calls;
__asm__(".text\n"
    ".irp reg,rax,rbx,rcx,rdx,rsi,rdi,rbp,r8,r9,r10,r11,r12,r13,r14,r15\n"
    ".globl __x86_indirect_thunk_\\reg\n"
    "__x86_indirect_thunk_\\reg:\n"
    "\tincq thunk_calls(%rip)\n"
    "\tjmp *%\\reg\n"
    ".endr\n"
    ".globl __x86_return_thunk\n"
    "__x86_return_thunk:\n"
    "\tincq ret_calls(%rip)\n"
    "\tret\n");
int main(void) {{
    if (work() != 1 + 2 + 3 + 6 + 20 + 10) return 1;
    if (thunk_calls < 6) return 2;
    if (ret_calls < 6) return 3;
    return 0;
}}
"#
    );
    let plain =
        format!("{body}\nint main(void) {{ return work() == 1 + 2 + 3 + 6 + 20 + 10 ? 0 : 1; }}\n");
    let extern_flags = opts(&[
        "-mindirect-branch=thunk-extern",
        "-mfunction-return=thunk-extern",
        "-mindirect-branch-cs-prefix",
        "-mharden-sls=all",
    ]);
    assert_eq!(
        compile_and_run("retpoline_extern", &counted, &extern_flags),
        0
    );
    for flags in [
        &["-mindirect-branch=thunk", "-mfunction-return=thunk"][..],
        &[
            "-mindirect-branch=thunk-inline",
            "-mfunction-return=thunk-inline",
        ],
        &["-mharden-sls=all"],
    ] {
        assert_eq!(
            compile_and_run("retpoline", &plain, &opts(flags)),
            0,
            "{flags:?}"
        );
    }
}

/// A program whose functions clear the registers they used, or all of
/// them, before each return runs: the cleared ones are call-clobbered, and
/// the one holding the result is left alone -- an integer, a `double` and
/// a structure returned in two registers.
#[test]
fn codegen_kernel_zero_call_used_regs_programs_run() {
    let src = r#"
struct pair { long a, b; };
__attribute__((noinline)) long mix(long a, long b, long c) { return a * b + c; }
__attribute__((noinline)) double scale(double x, double y) { return x * y + 1.0; }
__attribute__((noinline)) struct pair both(long a) { struct pair p = { a, a + 1 }; return p; }
int main(void) {
    if (mix(3, 4, 5) != 17) return 1;
    if (scale(2.0, 3.0) != 7.0) return 2;
    struct pair p = both(41);
    if (p.a != 41 || p.b != 42) return 3;
    return 0;
}
"#;
    for mode in [
        "used-gpr",
        "all-gpr",
        "used",
        "all",
        "all-arg",
        "used-gpr-arg",
    ] {
        let flag = format!("-fzero-call-used-regs={mode}");
        assert_eq!(
            compile_and_run("zero_regs", src, &opts(&[&flag])),
            0,
            "{mode}"
        );
    }
}

/// A program built with `-fpatchable-function-entry=N,M` runs, and its
/// `__patchable_function_entries` holds one record per function, each the
/// address of that function's first nop: `M` bytes before its entry.
#[test]
fn codegen_kernel_patchable_entries_record_the_nops() {
    let src = r#"
extern const unsigned long __start___patchable_function_entries[];
extern const unsigned long __stop___patchable_function_entries[];
__attribute__((noinline)) int f(int x) { return x + 1; }
int main(void) {
    long n = __stop___patchable_function_entries - __start___patchable_function_entries;
    if (n != 2) return 1;
    const unsigned char *fp = (const unsigned char *)f, *mp = (const unsigned char *)main;
    int seen_f = 0, seen_main = 0;
    for (long i = 0; i < n; i++) {
        const unsigned char *p = (const unsigned char *)__start___patchable_function_entries[i];
        if (p == fp - 2) seen_f = 1;
        if (p == mp - 2) seen_main = 1;
        for (int k = 0; k < 2; k++) if (p[k] != 0x90) return 2;
    }
    if (!seen_f || !seen_main) return 3;
    return f(41) == 42 ? 0 : 4;
}
"#;
    assert_eq!(
        compile_and_run("patchable", src, &opts(&["-fpatchable-function-entry=5,2"])),
        0
    );
}

/// A program whose stack guard is somewhere other than glibc's `%fs:40`
/// runs: the global `__stack_chk_guard` it defines itself, or another word
/// of the thread control block -- each read the same way at entry and at
/// exit.
#[test]
fn codegen_kernel_stack_guard_placements_run() {
    let src = r#"
unsigned long __stack_chk_guard = 0x5a5aa5a5deadbeefUL;
__attribute__((noinline)) int fill(char *b, int n) { for (int i = 0; i < n; i++) b[i] = (char)i; return b[n - 1]; }
__attribute__((noinline)) int f(int n) { char b[64]; return fill(b, n); }
int main(void) { return f(64) == 63 ? 0 : 1; }
"#;
    for flags in [
        &["-fstack-protector-all", "-mstack-protector-guard=global"][..],
        &["-fstack-protector-all", "-mstack-protector-guard-offset=48"],
    ] {
        assert_eq!(
            compile_and_run("guard_place", src, &opts(flags)),
            0,
            "{flags:?}"
        );
    }
}

/// `patchable_function_entry` and `zero_call_used_regs` give one function
/// what their options give every function: a record and nops for `f`
/// alone, `(0)` taking `g` out of the option, and cleared registers
/// around a result that survives.
#[test]
fn codegen_kernel_per_function_attributes_run() {
    let src = r#"
extern const unsigned long __start___patchable_function_entries[];
extern const unsigned long __stop___patchable_function_entries[];
__attribute__((noinline, patchable_function_entry(3, 1))) int f(int x) { return x + 1; }
__attribute__((noinline, patchable_function_entry(0))) int g(int x) { return x + 2; }
__attribute__((noinline, zero_call_used_regs("all-gpr"))) long mix(long a, long b) { return a * b; }
int main(void) {
    long n = __stop___patchable_function_entries - __start___patchable_function_entries;
    const unsigned char *fp = (const unsigned char *)f;
    if (n < 1 || (const unsigned char *)__start___patchable_function_entries[0] != fp - 1)
        return 1;
    for (int k = 0; k < 3; k++)
        if (fp[k - 1] != 0x90) return 2;
    if (f(41) != 42 || g(40) != 42 || mix(6, 7) != 42) return 3;
    return 0;
}
"#;
    for flags in [
        &["-O0"][..],
        &["-O2"],
        &["-O2", "-fpatchable-function-entry=2"],
    ] {
        assert_eq!(
            compile_and_run("fn_shape_attrs", src, &opts(flags)),
            0,
            "{flags:?}"
        );
    }
}

/// A `no_caller_saved_registers` function, built with no SSE as gcc 13
/// requires, returns with every general register as it found it, the
/// call-clobbered ones included, though it calls an ordinary function that
/// changes them: inline assembly loads each with a value, calls it, and
/// sums them.
#[test]
fn codegen_kernel_no_caller_saved_registers_preserves_registers() {
    let src = r#"
int g;
__attribute__((noinline)) void ext(int x) { g = g * 7 + x; }
__attribute__((no_caller_saved_registers)) void keep(void) { ext(1); g = g * 3 / 2 + 5; }
int main(void) {
    long sum;
    __asm__ volatile(
        "movq $11, %%rcx\n\tmovq $12, %%rdx\n\tmovq $13, %%rsi\n\tmovq $14, %%rdi\n\t"
        "movq $15, %%r8\n\tmovq $16, %%r9\n\tmovq $17, %%r10\n\tmovq $18, %%r11\n\t"
        "movq $19, %%rax\n\t"
        "subq $128, %%rsp\n\tcall keep\n\taddq $128, %%rsp\n\t"
        "addq %%rcx, %%rax\n\taddq %%rdx, %%rax\n\taddq %%rsi, %%rax\n\taddq %%rdi, %%rax\n\t"
        "addq %%r8, %%rax\n\taddq %%r9, %%rax\n\taddq %%r10, %%rax\n\taddq %%r11, %%rax\n\t"
        "movq %%rax, %0"
        : "=m"(sum)
        :
        : "rax", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11", "memory", "cc");
    if (sum != 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19) return 1;
    return g == 6 ? 0 : 2;
}
"#;
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run("ncsr_run", src, &opts(&[opt, "-mgeneral-regs-only"])),
            0,
            "{opt}"
        );
    }
}
