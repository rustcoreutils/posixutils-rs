//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// An inline asm that writes a callee-saved register -- by clobber, or by an
// operand pinned there -- obliges the function around it to preserve that
// register for its caller.
//
// Each program's caller is hand-written assembly at file scope: it loads
// known values into every callee-saved register, calls the C function, and
// reports whether they all survived. C cannot make c17 keep a value in a
// particular register across a call, so only assembly tests the contract.
//

#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
use crate::common::compile_and_run;
use crate::common::compile_and_run_aarch64;

/// `check(fn)` -- x86-64: returns 0 when %rbx and %r12-%r15 survive `fn()`.
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
const X86_CHECK: &str = r#"
__asm__(".text\n.globl check\ncheck:\n"
        "\tpush %rbx\n\tpush %r12\n\tpush %r13\n\tpush %r14\n\tpush %r15\n"
        "\tmovq $11, %rbx\n\tmovq $12, %r12\n\tmovq $13, %r13\n"
        "\tmovq $14, %r14\n\tmovq $15, %r15\n"
        "\tcall *%rdi\n"
        "\tmovl $1, %eax\n"
        "\tcmpq $11, %rbx\n\tjne 1f\n\tcmpq $12, %r12\n\tjne 1f\n"
        "\tcmpq $13, %r13\n\tjne 1f\n\tcmpq $14, %r14\n\tjne 1f\n"
        "\tcmpq $15, %r15\n\tjne 1f\n"
        "\txorl %eax, %eax\n"
        "1:\tpop %r15\n\tpop %r14\n\tpop %r13\n\tpop %r12\n\tpop %rbx\n\tret\n");
int check(void (*fn)(void));
"#;

/// A clobber list naming callee-saved registers, and the `"=b"` output
/// `cpuid` needs -- sljit's and libgc's shape. Before, c17 saved only the
/// callee-saved registers its own allocator had handed out, so these
/// returned to a caller whose %rbx and %r12-%r15 were gone.
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
#[test]
fn asm_callee_saved_clobbers_and_pins_are_preserved_x86_64() {
    let src = format!(
        r#"{X86_CHECK}
__attribute__((noinline)) void clobbers(void) {{
    __asm__ volatile("xorl %%ebx, %%ebx\n\txorl %%r12d, %%r12d\n\txorl %%r13d, %%r13d\n\t"
                     "xorl %%r14d, %%r14d\n\txorl %%r15d, %%r15d"
                     ::: "rbx", "r12", "r13", "r14", "r15");
}}
unsigned sink;
__attribute__((noinline)) void pinned(void) {{
    unsigned a = 0, b, c = 0, d;
    __asm__ volatile("cpuid" : "+a"(a), "=b"(b), "+c"(c), "=d"(d));
    sink = b;
    __asm__ volatile("" : : "b"(7u));
}}
int main(void) {{
    if (check(clobbers)) return 1;
    if (check(pinned)) return 2;
    return 0;
}}
"#
    );
    assert_eq!(compile_and_run("asm_callee_saved", &src, &[]), 0);
    assert_eq!(
        compile_and_run("asm_callee_saved_o2", &src, &["-O2".to_string()]),
        0
    );
}

/// aarch64: x19-x28 named in a clobber list.
#[test]
fn asm_callee_saved_clobbers_are_preserved_aarch64() {
    let src = r#"
__asm__(".text\n.globl check\ncheck:\n"
        "\tstp x29, x30, [sp, #-96]!\n\tmov x29, sp\n"
        "\tstp x19, x20, [sp, #16]\n\tstp x21, x22, [sp, #32]\n"
        "\tstp x23, x24, [sp, #48]\n\tstp x25, x26, [sp, #64]\n"
        "\tstp x27, x28, [sp, #80]\n"
        "\tmov x19, #19\n\tmov x20, #20\n\tmov x21, #21\n\tmov x22, #22\n"
        "\tmov x23, #23\n\tmov x24, #24\n\tmov x25, #25\n\tmov x26, #26\n"
        "\tmov x27, #27\n\tmov x28, #28\n"
        "\tblr x0\n"
        "\tmov w0, #1\n"
        "\tcmp x19, #19\n\tb.ne 1f\n\tcmp x20, #20\n\tb.ne 1f\n"
        "\tcmp x21, #21\n\tb.ne 1f\n\tcmp x22, #22\n\tb.ne 1f\n"
        "\tcmp x23, #23\n\tb.ne 1f\n\tcmp x24, #24\n\tb.ne 1f\n"
        "\tcmp x25, #25\n\tb.ne 1f\n\tcmp x26, #26\n\tb.ne 1f\n"
        "\tcmp x27, #27\n\tb.ne 1f\n\tcmp x28, #28\n\tb.ne 1f\n"
        "\tmov w0, #0\n"
        "1:\tldp x19, x20, [sp, #16]\n\tldp x21, x22, [sp, #32]\n"
        "\tldp x23, x24, [sp, #48]\n\tldp x25, x26, [sp, #64]\n"
        "\tldp x27, x28, [sp, #80]\n\tldp x29, x30, [sp], #96\n\tret\n");
int check(void (*fn)(void));
__attribute__((noinline)) void clobbers(void) {
    __asm__ volatile("mov x19, xzr\n\tmov x20, xzr\n\tmov x21, xzr\n\tmov x22, xzr\n\t"
                     "mov x23, xzr\n\tmov x24, xzr\n\tmov x25, xzr\n\tmov x26, xzr\n\t"
                     "mov x27, xzr\n\tmov x28, xzr"
                     ::: "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26",
                         "x27", "x28");
}
int main(void) { return check(clobbers); }
"#;
    for opt in ["-O0", "-O2"] {
        if let Some(rc) = compile_and_run_aarch64("asm_callee_saved_a64", src, opt) {
            assert_eq!(rc, 0, "{opt}");
        }
    }
}
