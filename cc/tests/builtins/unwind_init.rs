//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__builtin_unwind_init()`: every callee-saved register is saved in the
// calling function's frame, where a conservative collector scanning the
// stack finds a pointer the caller held only in a register.
//
// libgc's mach_dep.c uses it to push the registers before marking: its
// gcconfig.h selects it for gcc 2.8 through 10 by `__GNUC__` alone, with no
// probe, and c17 claims gcc 7.
//
// Each caller is hand-written assembly: it loads a marker into every
// callee-saved register and calls `scan`, which counts the markers it finds
// on the stack above its own local and checks they survive.
//

#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
use crate::common::compile_and_run;
use crate::common::compile_and_run_aarch64;

/// The C half, libgc's shape: `scan` flushes the registers and calls `count`,
/// which counts the words on the stack from its own local upward -- across
/// all of `scan`'s frame -- that hold a marker `0x5a5a0000000000NN`. `count`
/// may save a marker again itself, so the test asks for at least one each.
const SCAN: &str = r#"
int found;
__attribute__((noinline)) void count(void) {
    volatile long here = 0;
    int n = 0;
    for (volatile long *p = &here; p < &here + 96; p++)
        if ((*p & ~0xffL) == 0x5a5a000000000000L)
            n++;
    found = n;
}
__attribute__((noinline)) void scan(void) {
    __builtin_unwind_init();
    count();
}
int check(void);
"#;

#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
#[test]
fn unwind_init_spills_every_callee_saved_register_x86_64() {
    let src = format!(
        r#"
__asm__(".text\n.globl check\ncheck:\n"
        "\tpush %rbx\n\tpush %r12\n\tpush %r13\n\tpush %r14\n\tpush %r15\n"
        "\tmovabsq $0x5a5a000000000011, %rbx\n\tmovabsq $0x5a5a000000000012, %r12\n"
        "\tmovabsq $0x5a5a000000000013, %r13\n\tmovabsq $0x5a5a000000000014, %r14\n"
        "\tmovabsq $0x5a5a000000000015, %r15\n"
        "\tcall scan\n"
        "\tmovl $1, %eax\n"
        "\tmovabsq $0x5a5a000000000011, %rcx\n\tcmpq %rcx, %rbx\n\tjne 1f\n"
        "\tmovabsq $0x5a5a000000000015, %rcx\n\tcmpq %rcx, %r15\n\tjne 1f\n"
        "\txorl %eax, %eax\n"
        "1:\tpop %r15\n\tpop %r14\n\tpop %r13\n\tpop %r12\n\tpop %rbx\n\tret\n");
{SCAN}
int main(void) {{
    if (check()) return 1;
    return found >= 5 ? 0 : 10 + found;
}}
"#
    );
    assert_eq!(compile_and_run("unwind_init", &src, &[]), 0);
    assert_eq!(
        compile_and_run("unwind_init_o2", &src, &["-O2".to_string()]),
        0
    );
}

#[test]
fn unwind_init_spills_every_callee_saved_register_aarch64() {
    let src = format!(
        r#"
__asm__(".text\n.globl check\ncheck:\n"
        "\tstp x29, x30, [sp, #-96]!\n\tmov x29, sp\n"
        "\tstp x19, x20, [sp, #16]\n\tstp x21, x22, [sp, #32]\n"
        "\tstp x23, x24, [sp, #48]\n\tstp x25, x26, [sp, #64]\n"
        "\tstp x27, x28, [sp, #80]\n"
        "\tmov x9, #0x5a5a000000000000\n"
        "\tadd x19, x9, #19\n\tadd x20, x9, #20\n\tadd x21, x9, #21\n"
        "\tadd x22, x9, #22\n\tadd x23, x9, #23\n\tadd x24, x9, #24\n"
        "\tadd x25, x9, #25\n\tadd x26, x9, #26\n\tadd x27, x9, #27\n"
        "\tadd x28, x9, #28\n"
        "\tbl scan\n"
        "\tmov x9, #0x5a5a000000000000\n"
        "\tmov w0, #1\n"
        "\tadd x10, x9, #19\n\tcmp x19, x10\n\tb.ne 1f\n"
        "\tadd x10, x9, #28\n\tcmp x28, x10\n\tb.ne 1f\n"
        "\tmov w0, #0\n"
        "1:\tldp x19, x20, [sp, #16]\n\tldp x21, x22, [sp, #32]\n"
        "\tldp x23, x24, [sp, #48]\n\tldp x25, x26, [sp, #64]\n"
        "\tldp x27, x28, [sp, #80]\n\tldp x29, x30, [sp], #96\n\tret\n");
{SCAN}
int main(void) {{
    if (check()) return 1;
    return found >= 10 ? 0 : 10 + found;
}}
"#
    );
    for opt in ["-O0", "-O2"] {
        if let Some(rc) = compile_and_run_aarch64("unwind_init_a64", &src, opt) {
            assert_eq!(rc, 0, "{opt}");
        }
    }
}
