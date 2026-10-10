//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU register variables: `register T x asm("reg")`. At block scope the one
// documented guarantee is that `x`, as an inline-asm register operand, is in
// that register; at file scope `x` has no storage and *is* the register.
//

use crate::common::{
    compile_and_run_everywhere, compile_and_run_two_units, compile_expect_error, compile_object_run,
};

/// Register operands named by local register variables: the template reads
/// and writes the registers by name, never through `%N`. An input, an output,
/// a `"+r"` operand, one tied by `"0"`, several in one statement, one reached
/// through an inlined helper, three arguments each wanted in the register of
/// another, and one value in two registers. At -O2 the variables are promoted out of memory and nothing
/// but the binding puts them in their registers.
#[test]
fn local_register_vars_pin_asm_operands() {
    let src = r#"
#if defined(__x86_64__)
static inline long add2(long x, long y)
{
	register long a asm("r10") = x;
	register long b asm("%r8") = y;
	long out;
	asm("lea (%%r10,%%r8),%0" : "=r"(out) : "r"(a), "r"(b));
	return out;
}
__attribute__((noinline)) long swap_sub(long x, long y)
{
	/* Arrive in %rdi and %rsi, wanted the other way round. */
	register long p asm("rsi") = x;
	register long q asm("rdi") = y;
	long out;
	asm("mov %%rsi, %0\n\tsub %%rdi, %0" : "=&r"(out) : "r"(p), "r"(q));
	return out;
}
__attribute__((noinline)) unsigned long rdx_out(void)
{
	register unsigned long v asm("%rdx");
	asm volatile("movq $77, %%rdx" : "=r"(v));
	return v;
}
__attribute__((noinline)) int r11_inout(int x)
{
	register int t asm("r11") = x;
	asm("addl %%r11d, %%r11d" : "+r"(t));
	return t;
}
__attribute__((noinline)) long tied(long x)
{
	register long r asm("rbx");
	asm("imul $3, %%rbx, %%rbx" : "=r"(r) : "0"(x));
	return r;
}
__attribute__((noinline)) long callee_saved_in(long x)
{
	register long r asm("r12") = x;
	long out;
	asm("lea 1(%%r12),%0" : "=r"(out) : "r"(r));
	return out;
}
__attribute__((noinline)) long rotate(long a, long b, long c)
{
	register long ra asm("rsi") = a;
	register long rb asm("rdx") = b;
	register long rc asm("rdi") = c;
	long out;
	asm("lea (%%rsi,%%rdx,2), %0\n\tlea (%0,%%rdi,4), %0" : "=&r"(out) : "r"(ra), "r"(rb), "r"(rc));
	return out;
}
__attribute__((noinline)) long same_value(long x)
{
	register long p asm("r8") = x;
	register long q asm("r9") = x;
	long out;
	asm("lea (%%r8,%%r9), %0" : "=r"(out) : "r"(p), "r"(q));
	return out;
}
#elif defined(__aarch64__)
static inline long add2(long x, long y)
{
	register long a asm("x10") = x;
	register long b asm("%x11") = y;
	long out;
	asm("add %0, x10, x11" : "=r"(out) : "r"(a), "r"(b));
	return out;
}
__attribute__((noinline)) long swap_sub(long x, long y)
{
	register long p asm("x1") = x;
	register long q asm("x0") = y;
	long out;
	asm("sub %0, x1, x0" : "=&r"(out) : "r"(p), "r"(q));
	return out;
}
__attribute__((noinline)) unsigned long rdx_out(void)
{
	register unsigned long v asm("x2");
	asm volatile("mov x2, #77" : "=r"(v));
	return v;
}
__attribute__((noinline)) int r11_inout(int x)
{
	register int t asm("w16") = x;
	asm("add w16, w16, w16" : "+r"(t));
	return t;
}
__attribute__((noinline)) long tied(long x)
{
	register long r asm("x19");
	asm("add x19, x19, x19, lsl #1" : "=r"(r) : "0"(x));
	return r;
}
__attribute__((noinline)) long callee_saved_in(long x)
{
	register long r asm("x20") = x;
	long out;
	asm("add %0, x20, #1" : "=r"(out) : "r"(r));
	return out;
}
__attribute__((noinline)) long rotate(long a, long b, long c)
{
	register long ra asm("x1") = a;
	register long rb asm("x2") = b;
	register long rc asm("x0") = c;
	long out;
	asm("add %0, x1, x2, lsl #1\n\tadd %0, %0, x0, lsl #2" : "=&r"(out) : "r"(ra), "r"(rb), "r"(rc));
	return out;
}
__attribute__((noinline)) long same_value(long x)
{
	register long p asm("x12") = x;
	register long q asm("x13") = x;
	long out;
	asm("add %0, x12, x13" : "=r"(out) : "r"(p), "r"(q));
	return out;
}
#endif
int main(void)
{
#if defined(__x86_64__) || defined(__aarch64__)
	volatile long three = 3, four = 4;
	if (add2(three, four) != 7) return 1;
	if (swap_sub(10, 3) != 7) return 2;
	if (rdx_out() != 77) return 3;
	if (r11_inout(21) != 42) return 4;
	if (tied(5) != 15) return 5;
	if (callee_saved_in(41) != 42) return 6;
	if (rotate(1, 2, 3) != 17) return 7;
	if (same_value(4) != 8) return 8;
#endif
	return 0;
}
"#;
    compile_and_run_everywhere("asm_regvar_local", src);
}

/// glibc's `INTERNAL_SYSCALL` shape: six arguments in local register
/// variables, the number tied to the result, and the kernel reading the
/// arguments out of exactly those registers. `rt_sigprocmask` writes the old
/// mask through the third argument, so a value left in the wrong register is
/// a wrong answer, not just a wrong return code.
#[test]
fn local_register_vars_make_a_linux_syscall() {
    let src = r#"
#include <signal.h>
#include <string.h>
#include <unistd.h>
#if defined(__linux__) && (defined(__x86_64__) || defined(__aarch64__))
#include <sys/syscall.h>
__attribute__((noinline)) long sys6(long n, long a1, long a2, long a3, long a4, long a5, long a6)
{
#if defined(__x86_64__)
	long ret;
	register long _a6 asm("r9") = a6;
	register long _a5 asm("r8") = a5;
	register long _a4 asm("r10") = a4;
	register long _a3 asm("rdx") = a3;
	register long _a2 asm("rsi") = a2;
	register long _a1 asm("rdi") = a1;
	asm volatile("syscall"
		     : "=a"(ret)
		     : "0"(n), "r"(_a1), "r"(_a2), "r"(_a3), "r"(_a4), "r"(_a5), "r"(_a6)
		     : "memory", "cc", "r11", "rcx");
	return ret;
#else
	register long _n asm("x8") = n;
	register long _a1 asm("x0") = a1;
	register long _a2 asm("x1") = a2;
	register long _a3 asm("x2") = a3;
	register long _a4 asm("x3") = a4;
	register long _a5 asm("x4") = a5;
	register long _a6 asm("x5") = a6;
	asm volatile("svc 0"
		     : "=r"(_a1)
		     : "r"(_n), "0"(_a1), "r"(_a2), "r"(_a3), "r"(_a4), "r"(_a5), "r"(_a6)
		     : "memory", "cc");
	return _a1;
#endif
}
#endif
int main(void)
{
#if defined(__linux__) && (defined(__x86_64__) || defined(__aarch64__))
	sigset_t want, got;
	memset(&want, 0, sizeof want);
	memset(&got, 0xff, sizeof got);
	sigprocmask(SIG_SETMASK, 0, &want);
	if (sys6(SYS_rt_sigprocmask, SIG_SETMASK, 0, (long)&got, 8, 0, 0) != 0) return 1;
	if (memcmp(&want, &got, 8) != 0) return 2;
	if (sys6(SYS_getpid, 0, 0, 0, 0, 0, 0) != getpid()) return 3;
#endif
	return 0;
}
"#;
    compile_and_run_everywhere("asm_regvar_syscall", src);
}

/// Linux's `current_stack_pointer`: a file-scope register variable read as a
/// value and used as a `"+r"` operand. It has no storage -- reading it reads
/// the stack pointer.
#[test]
fn global_register_var_is_the_stack_pointer() {
    let src = r#"
#if defined(__x86_64__)
register unsigned long current_stack_pointer asm("rsp");
#elif defined(__aarch64__)
register unsigned long current_stack_pointer asm("sp");
#endif
__attribute__((noinline)) int leaf(int x) { return x + 1; }
int main(void)
{
#if defined(__x86_64__) || defined(__aarch64__)
	unsigned long local;
	unsigned long sp = current_stack_pointer;
	if ((unsigned long)&local - sp >= 4096) return 1;
	asm volatile("" : "+r"(current_stack_pointer));
	if (leaf(1) != 2) return 2;
	if (current_stack_pointer != sp) return 3;
	if (current_stack_pointer % 16 != 0) return 4;
#endif
	return 0;
}
"#;
    compile_and_run_everywhere("asm_regvar_global_sp", src);
}

/// File-scope register variables in callee-saved registers: the register
/// is the variable for the whole unit. It keeps its value across calls --
/// to this unit's functions, which never allocate it, and to libc, which
/// preserves it -- is read, written and updated in place, and a 32-bit one
/// wraps at 32 bits. Every function here is under register pressure, so an
/// allocator that handed the register out would overwrite the variable.
///
/// `main` leaves both registers as it found them: its caller in libc is
/// owed them.
#[test]
fn global_register_vars_in_callee_saved_registers() {
    let src = r#"
#include <string.h>
#if defined(__x86_64__)
register unsigned long g asm("r14");
register unsigned int h asm("r15");
#define SAVE(v, r) asm volatile("mov %%" r ", %0" : "=r"(v))
#define RESTORE(v, r) asm volatile("mov %0, %%" r :: "r"(v))
#define R1 "r14"
#define R2 "r15"
#elif defined(__aarch64__)
register unsigned long g asm("x20");
register unsigned int h asm("w28");
#define SAVE(v, r) asm volatile("mov %0, " r : "=r"(v))
#define RESTORE(v, r) asm volatile("mov " r ", %0" :: "r"(v))
#define R1 "x20"
#define R2 "x28"
#endif
#if defined(__x86_64__) || defined(__aarch64__)
__attribute__((noinline)) long sink(long x) { return x * 3 + 1; }
__attribute__((noinline)) long pressure(long x)
{
	long a = sink(x), b = sink(a), c = sink(b), d = sink(c), e = sink(d);
	long f = sink(e), i = sink(f), j = sink(i), k = sink(j), l = sink(k);
	sink(0);
	return a + b + c + d + e + f + i + j + k + l;
}
__attribute__((noinline)) void bump(void)
{
	g += 3;
	h = h * 2 + 1;
}
__attribute__((noinline)) unsigned long in_asm(void)
{
	unsigned long out;
	asm("mov %0, %1" : "=r"(out) : "r"(g));
	return out;
}
#endif
int main(void)
{
#if defined(__x86_64__) || defined(__aarch64__)
	unsigned long s1, s2;
	int rc = 0;
	SAVE(s1, R1);
	SAVE(s2, R2);
	g = 5;
	h = 0x80000000u;
	long want = pressure(2);
	bump();
	if (g != 8) rc = 1;
	else if (h != 1) rc = 2;
	else if (pressure(2) != want) rc = 3;
	else if (strlen("abc") != 3 || g != 8 || h != 1) rc = 4;
	else if (in_asm() != 8) rc = 5;
	else {
		g |= 0x100;
		h -= 2;
		if (g != 0x108 || h != 0xffffffffu) rc = 6;
	}
	RESTORE(s1, R1);
	RESTORE(s2, R2);
	return rc;
#else
	return 0;
#endif
}
"#;
    compile_and_run_everywhere("asm_regvar_global_callee_saved", src);
}

/// A unit with a global register variable compiles as gcc's torture test
/// for PR rtl-optimization/17825 has it: a 32-bit variable in a 64-bit
/// register, read, divided and merged back.
#[test]
fn global_register_var_in_a_division_merge() {
    let src = r#"
#if defined(__x86_64__)
register unsigned int reg __asm("r14");
#elif defined(__aarch64__)
register unsigned int reg __asm("x21");
#else
unsigned int reg;
#endif
struct S { unsigned int h[8]; } *b;
unsigned int c;
__attribute__((noinline)) void foo(int x) { (void)x; }
__attribute__((noinline)) void bar(void)
{
	unsigned int j, k, l, m;
	j = (reg & 0xffff) | ((b->h[2] & 0xffff) << 16);
	k = c & 0xffff;
	if (k == 0)
		foo(0);
	l = (j / k) & 0xffff;
	m = (j % k) & 0xffff;
	reg = (reg & 0xffff0000) | l;
	b->h[2] = (b->h[2] & 0xffff0000) | m;
}
int main(void)
{
	struct S s = { { 0, 0, 0x12340003u } };
	unsigned int saved = reg;
	int rc = 0;
	b = &s;
	c = 7;
	reg = 0xabcd0064u;
	bar();
	/* j = 0x00030064 = 196708: / 7 = 28101 = 0x6dc5, % 7 = 1 */
	if (reg != 0xabcd6dc5u) rc = 1;
	else if (s.h[2] != 0x12340001u) rc = 2;
	reg = saved;
	return rc;
}
"#;
    compile_and_run_everywhere("asm_regvar_global_divmerge", src);
}

/// Every object file of the Linux kernel declares `current_stack_pointer`;
/// had it storage, the second would be a duplicate definition.
#[test]
fn global_register_var_defines_no_symbol() {
    let decl = r#"
#if defined(__x86_64__)
register unsigned long current_stack_pointer asm("rsp");
#elif defined(__aarch64__)
register unsigned long current_stack_pointer asm("sp");
#endif
"#;
    let a = format!("{decl}\nint other(void) {{ return 0; }}\n");
    let b = format!("{decl}\nint other(void);\nint main(void) {{ return other(); }}\n");
    assert_eq!(compile_and_run_two_units("asm_regvar_two", &a, &b, &[]), 0);
}

#[test]
fn register_var_diagnostics() {
    #[cfg(target_arch = "x86_64")]
    let (sp, other, caller_saved) = ("rsp", "r12", "r11");
    #[cfg(not(target_arch = "x86_64"))]
    let (sp, other, caller_saved) = ("sp", "x20", "x9");
    compile_expect_error(
        "regvar_addr",
        &format!(
            "register unsigned long s asm(\"{sp}\");\nunsigned long *f(void) {{ return &s; }}\n"
        ),
        "address of global register variable 's' requested",
    );
    compile_expect_error(
        "regvar_init",
        &format!("register unsigned long s asm(\"{sp}\") = 4;\n"),
        "global register variable has initial value",
    );
    compile_expect_error(
        "regvar_bad_name",
        "void f(void) { register long x asm(\"bogus\") = 1; asm(\"\" :: \"r\"(x)); }\n",
        "invalid register name for 'x'",
    );
    compile_expect_error(
        "regvar_bad_global_name",
        "register long g asm(\"bogus\");\n",
        "invalid register name for 'g'",
    );
    compile_expect_error(
        "regvar_clobber",
        &format!(
            "void f(void) {{ register long x asm(\"{other}\") = 1; asm(\"\" :: \"r\"(x) : \"{other}\"); }}\n"
        ),
        "'asm' specifier for variable 'x' conflicts with 'asm' clobber list",
    );
    compile_expect_error(
        "regvar_global_memory",
        &format!(
            "register unsigned long s asm(\"{sp}\");\nvoid f(void) {{ asm(\"\" :: \"m\"(s)); }}\n"
        ),
        "address of global register variable 's' requested",
    );
    // Without `register` the label names nothing; gcc says so and goes on.
    let run = compile_object_run(
        "regvar_plain_label",
        &format!("long f(long a) {{ long x asm(\"{other}\") = a; return x; }}\n"),
        &[],
    );
    assert!(run.success, "{}", run.stderr);
    assert!(
        run.stderr
            .contains("ignoring 'asm' specifier for non-static local variable 'x'"),
        "{}",
        run.stderr
    );
    compile_expect_error(
        "regvar_global_other",
        &format!("register long g asm(\"{caller_saved}\");\n"),
        "global register variable 'g'",
    );
}
