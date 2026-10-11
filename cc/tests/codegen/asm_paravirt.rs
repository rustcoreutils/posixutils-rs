//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The Linux kernel's paravirt calls (arch/x86/include/asm/paravirt_types.h,
// `____PVOP_CALL`): one asm whose outputs are pinned to %rdi, %rsi, %rdx,
// %rcx and %rax, which reads the stack pointer as a `"+r"` register
// variable, and which clobbers the rest of the caller-saved registers. What
// is left for its other register operands is the callee-saved registers, and
// gcc spends them only on those operands.
//

#[cfg(target_arch = "x86_64")]
use crate::common::{compile_and_run, compile_rejected_with};

/// `____PVOP_CALL` and its wrappers, as the kernel spells them.
#[cfg(target_arch = "x86_64")]
const PVOP_MACROS: &str = r#"
register unsigned long current_stack_pointer asm("rsp");
#define ASM_CALL_CONSTRAINT "+r"(current_stack_pointer)
#define PVOP_CALL_ARGS \
    unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx, __eax = __eax;
#define PVOP_CALL_ARG1(x) "D"((unsigned long)(x))
#define PVOP_CALL_ARG2(x) "S"((unsigned long)(x))
#define PVOP_CALL_ARG3(x) "d"((unsigned long)(x))
#define PVOP_CALL_ARG4(x) "c"((unsigned long)(x))
#define PVOP_VCALL_CLOBBERS "=D"(__edi), "=S"(__esi), "=d"(__edx), "=c"(__ecx)
#define PVOP_CALL_CLOBBERS PVOP_VCALL_CLOBBERS, "=a"(__eax)
#define VEXTRA_CLOBBERS , "rax", "r8", "r9", "r10", "r11"
#define EXTRA_CLOBBERS , "r8", "r9", "r10", "r11"
#define paravirt_ptr(op) [paravirt_opptr] "m"(pv_ops.op)
#define ____PVOP_CALL(ret, op, call_clbr, extra_clbr, ...)           \
    ({                                                               \
        PVOP_CALL_ARGS;                                              \
        asm volatile("call *%[paravirt_opptr];"                      \
                     : call_clbr, ASM_CALL_CONSTRAINT                \
                     : paravirt_ptr(op), ##__VA_ARGS__               \
                     : "memory", "cc" extra_clbr);                   \
        ret;                                                         \
    })
#define __PVOP_CALL(rettype, op, ...) \
    ____PVOP_CALL((rettype)__eax, op, PVOP_CALL_CLOBBERS, EXTRA_CLOBBERS, ##__VA_ARGS__)
#define __PVOP_VCALL(op, ...) \
    (void)____PVOP_CALL(, op, PVOP_VCALL_CLOBBERS, VEXTRA_CLOBBERS, ##__VA_ARGS__)
#define PVOP_CALL0(rettype, op) __PVOP_CALL(rettype, op)
#define PVOP_CALL2(rettype, op, a1, a2) \
    __PVOP_CALL(rettype, op, PVOP_CALL_ARG1(a1), PVOP_CALL_ARG2(a2))
#define PVOP_CALL4(rettype, op, a1, a2, a3, a4)                       \
    __PVOP_CALL(rettype, op, PVOP_CALL_ARG1(a1), PVOP_CALL_ARG2(a2), \
                PVOP_CALL_ARG3(a3), PVOP_CALL_ARG4(a4))
#define PVOP_VCALL2(op, a1, a2) __PVOP_VCALL(op, PVOP_CALL_ARG1(a1), PVOP_CALL_ARG2(a2))
"#;

/// The paravirt calls themselves, with values live across them that must be
/// kept out of every register the call writes.
#[cfg(target_arch = "x86_64")]
#[test]
fn asm_paravirt_calls() {
    let src = format!(
        "{PVOP_MACROS}{}",
        r#"
struct pv_ops_s {
    unsigned long (*op0)(void);
    unsigned long (*op2)(unsigned long, unsigned long);
    unsigned long (*op4)(unsigned long, unsigned long, unsigned long, unsigned long);
    void (*vop2)(unsigned long, unsigned long);
};
static unsigned long seen;
static unsigned long f0(void) { return 42; }
static unsigned long f2(unsigned long a, unsigned long b) { return a - b; }
static unsigned long f4(unsigned long a, unsigned long b, unsigned long c, unsigned long d) {
    return a * 1000 + b * 100 + c * 10 + d;
}
static void v2(unsigned long a, unsigned long b) { seen = a + b; }
struct pv_ops_s pv_ops = { f0, f2, f4, v2 };

__attribute__((noinline)) unsigned long call0(void) { return PVOP_CALL0(unsigned long, op0); }
__attribute__((noinline)) unsigned long call2(unsigned long a, unsigned long b) {
    return PVOP_CALL2(unsigned long, op2, a, b);
}
__attribute__((noinline)) void vcall2(unsigned long a, unsigned long b) { PVOP_VCALL2(vop2, a, b); }
__attribute__((noinline)) unsigned long live_across(unsigned long a, unsigned long b,
                                                    unsigned long c) {
    unsigned long x = a * 7, y = b * 11, z = c * 13;
    unsigned long r = PVOP_CALL4(unsigned long, op4, a, b, c, 4);
    return r + x + y + z;
}
int main(void) {
    if (call0() != 42) return 1;
    if (call2(10, 3) != 7) return 2;
    vcall2(20, 22);
    if (seen != 42) return 3;
    if (live_across(1, 2, 3) != 1234 + 7 + 22 + 39) return 4;
    return 0;
}
"#
    );
    assert_eq!(compile_and_run("asm_pvop_calls", &src, &[]), 0);
    for opt in ["-O0", "-O2"] {
        let level = vec![opt.to_string()];
        assert_eq!(compile_and_run("asm_pvop_calls", &src, &level), 0, "{opt}");
    }
}

/// Every caller-saved register pinned by an operand or clobbered, four more
/// `"r"` operands and a memory operand addressed through a run-time pointer:
/// five registers wanted, and %rbx and %r12-%r15 are five. gcc fits them. The
/// operands pinned to %rdi, %rdx and %rax -- whose values were still wanted
/// in those registers by something else, so the allocator homed them
/// elsewhere -- took four of the five first, though their own register is
/// where the asm wants them and a move puts them there; c17 then had no
/// register for the rest.
#[cfg(target_arch = "x86_64")]
#[test]
fn asm_pinned_operands_leave_the_free_registers_to_the_rest() {
    let src = r#"
register unsigned long current_stack_pointer asm("rsp");
__attribute__((noinline)) unsigned long sum(unsigned long a, unsigned long b, unsigned long c,
                                           unsigned long d, unsigned long e, unsigned long *p) {
    unsigned long __edi = a, __esi = b, __edx = c, __ecx = d, __eax = e;
    unsigned long x = a + 1, y = b + 2, z = c + 3, w = d + 4;
    asm volatile("add %[x], %%rax\n\tadd %[y], %%rax\n\tadd %[z], %%rax\n\t"
                 "add %[w], %%rax\n\tadd %[m], %%rax\n\tadd %%rdi, %%rax\n\t"
                 "add %%rsi, %%rax\n\tadd %%rdx, %%rax\n\tadd %%rcx, %%rax\n\t"
                 "xor %%r8, %%r8\n\txor %%r9, %%r9\n\txor %%r10, %%r10\n\txor %%r11, %%r11"
                 : "+D"(__edi), "+S"(__esi), "+d"(__edx), "+c"(__ecx), "+a"(__eax),
                   "+r"(current_stack_pointer)
                 : [x] "r"(x), [y] "r"(y), [z] "r"(z), [w] "r"(w), [m] "m"(p[a])
                 : "memory", "cc", "r8", "r9", "r10", "r11");
    return __eax + a + c;
}
int main(void) {
    unsigned long arr[4] = { 100, 200, 300, 400 };
    if (sum(1, 2, 3, 4, 5, arr) != 5 + 20 + 200 + 10 + 1 + 3) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("asm_pinned_leave_free", src, &[]), 0);
    for opt in ["-O0", "-O2"] {
        let level = vec![opt.to_string()];
        assert_eq!(
            compile_and_run("asm_pinned_leave_free", src, &level),
            0,
            "{opt}"
        );
    }
}

/// One register more than the statement can have: five `"r"` operands and a
/// memory operand through a pointer, with every caller-saved register taken.
/// gcc refuses it, in these words, at every level.
#[cfg(target_arch = "x86_64")]
#[test]
fn asm_more_register_operands_than_registers() {
    let src = "__attribute__((noinline)) unsigned long f(unsigned long *p, unsigned long i) {\n\
               unsigned long r = 0;\n\
               unsigned long x = i + 1, y = i + 2, z = i + 3, w = i + 4, v = i + 5;\n\
               asm volatile(\"add %[x], %%rax\\n\\tadd %[y], %%rax\\n\\tadd %[z], %%rax\\n\\t\"\n\
                            \"add %[w], %%rax\\n\\tadd %[v], %%rax\\n\\tadd %[m], %%rax\"\n\
                            : \"+a\"(r)\n\
                            : [x] \"r\"(x), [y] \"r\"(y), [z] \"r\"(z), [w] \"r\"(w), [v] \"r\"(v),\n\
                              [m] \"m\"(p[i])\n\
                            : \"memory\", \"cc\", \"rdi\", \"rsi\", \"rdx\", \"rcx\", \"r8\", \"r9\",\n\
                              \"r10\", \"r11\");\n\
               return r;\n\
               }\n";
    for opt in ["-O0", "-O2"] {
        let stderr = compile_rejected_with("asm_too_many_regs", src, &[opt]);
        assert!(
            stderr.contains("'asm' operand has impossible constraints"),
            "{opt}: {stderr}"
        );
        assert_eq!(
            stderr.matches("error:").count(),
            1,
            "{opt}: one report per statement, as gcc gives: {stderr}"
        );
    }
}
