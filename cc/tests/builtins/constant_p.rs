//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__builtin_constant_p`, answered as gcc answers it.
//

use crate::common::compile_and_run_everywhere;

/// gcc.c-torture execute/bcp-1, reduced, plus the shapes its rules imply.
/// Every expectation was checked against gcc 13 at -O0, -O1, -O2 and -Os on
/// x86-64 and aarch64.
///
/// - `bad*` is 0 at every level: an unknown value, an operand with side
///   effects (which is never evaluated), a parameter of a function not
///   inlined, and any pointer or aggregate other than a string literal's
///   address -- even one whose value is known.
/// - `good*` is 1 at every level: a constant, and the address of a string
///   literal's first character, however it is spelled.
/// - `opt*` is 1 once optimized and 0 at -O0 (a GNU vector of constants
///   included -- it is a value, not an aggregate): an inline function's
///   parameter after inlining a constant argument, a byte of a string
///   literal, and computations -- a division, a subscript, a floating
///   multiply -- that only propagation proves constant. The answer to the
///   last three is deferred with their operand computed, which `dce`
///   deletes unrun once the answer is in.
#[test]
fn builtins_constant_p_gcc_rules() {
    compile_and_run_everywhere("builtins_constant_p_gcc_rules", CONSTANT_P);
}

const CONSTANT_P: &str = r#"
int global;
int func(void) { return 3; }

/* Answered 0 at every level (gcc.c-torture bcp-1 "must fail"). */
int bad0(void) { return __builtin_constant_p(global); }
int bad1(void) { return __builtin_constant_p(global++); }
static inline int bad2(int x) { return __builtin_constant_p(x++); }
static inline int bad3(int x) { return __builtin_constant_p(x); }
static inline int bad4(const char *x) { return __builtin_constant_p(x); }
int bad5(void) { return bad2(1); }
static inline int bad6(int x) { return __builtin_constant_p(x + 1); }
int bad7(void) { return __builtin_constant_p(func()); }
int bad8(void) { char buf[10]; return __builtin_constant_p(buf); }
int bad9(const char *x) { return __builtin_constant_p(x[123456]); }
int bad10(void) { return __builtin_constant_p(&global); }
/* Pointer and aggregate operands are 0 even when their value is known. */
int bad11(void) { char *p = (char *)16; return __builtin_constant_p(p); }
int bad12(void) { struct { int a; } s = { 1 }; return __builtin_constant_p(s); }
int bad13(void) { return bad4("hi"); }

/* Answered 1 at every level. */
int good0(void) { return __builtin_constant_p(1); }
int good1(void) { return __builtin_constant_p("hi"); }
int good2(void) { return __builtin_constant_p((1234 + 45) & ~7); }
int good3(void) { return __builtin_constant_p(&"hi"[0]); }
int good4(void) { return __builtin_constant_p(L"wide"); }
int good5(void) { return __builtin_constant_p((const void *)"hi"); }
int good6(void) { return __builtin_constant_p("hi" + 0); }
int at_file_scope = __builtin_constant_p("hi");

/* 1 once optimized: inlining and propagation make them constant. */
int opt0(void) { return bad3(1); }
int opt1(void) { return bad6(1); }
int opt2(void) { return __builtin_constant_p("hi"[0]); }
int opt3(void) { int x = 6, y = 2; return __builtin_constant_p(x / y); }
int opt4(void) { int a[2] = { 1, 2 }; return __builtin_constant_p(a[1]); }
int opt5(void) { double d = 2.0; return __builtin_constant_p(d * 3.0); }
typedef int v4si_cp __attribute__((vector_size(16)));
int opt6(void) { v4si_cp v = {1, 2, 3, 4}; return __builtin_constant_p(v); }
typedef double v2df_cp __attribute__((vector_size(16)));
int opt7(void) { v2df_cp d = {1.0, 2.0}; return __builtin_constant_p(d); }
/* One lane unknown makes the vector unknown. */
int bad14(int x) { v4si_cp v = {1, x, 3, 4}; return __builtin_constant_p(v); }

typedef int (*fn0)(void);
static fn0 volatile zero[] = { bad0, bad1, bad5, bad7, bad8, bad10, bad11, bad12, bad13 };
static int (*volatile zero_int[])(int) = { bad2, bad3, bad6, bad14 };
static int (*volatile zero_str[])(const char *) = { bad4, bad9 };
static fn0 volatile one[] = { good0, good1, good2, good3, good4, good5, good6 };
static fn0 volatile opt[] = { opt0, opt1, opt2, opt3, opt4, opt5, opt6, opt7 };

#define N(a) (int)(sizeof(a) / sizeof *(a))

int main(void)
{
    int i;
    for (i = 0; i < N(zero); i++)
        if (zero[i]()) return 10 + i;
    for (i = 0; i < N(zero_int); i++)
        if (zero_int[i](1)) return 20 + i;
    for (i = 0; i < N(zero_str); i++)
        if (zero_str[i]("hi")) return 30 + i;
    for (i = 0; i < N(one); i++)
        if (!one[i]()) return 40 + i;
    if (!at_file_scope) return 50;
    for (i = 0; i < N(opt); i++) {
#ifdef __OPTIMIZE__
        if (!opt[i]()) return 60 + i;
#else
        if (opt[i]()) return 70 + i;
#endif
    }
    return 0;
}
"#;

/// glibc's fortified `open` (`bits/fcntl2.h`), over a flag that is a
/// constant on a loop's first trip and not after: diffutils' `stdopen`
/// passes `fd == STDIN_FILENO ? O_WRONLY : O_RDONLY`. SCCP saw the operand
/// as a constant first and answered 1, then 0 once it was not, and the two
/// answers met to "unknown": the branch to `__open_missing_mode` -- declared
/// with `__attribute__((error))` and defined nowhere -- was never deleted,
/// and the program failed to link. gcc links it at every level, as must c17.
#[test]
fn builtins_constant_p_over_a_value_constant_only_at_first() {
    let src = r#"
extern void missing_mode(void);

int lib(const char *p, int f, ...) { return f + (p[0] == '/'); }
int lib2(const char *p, int f) { return f + (p[0] == '/'); }
int fails(int fd) { return fd != 1; }

extern __inline __attribute__((__always_inline__, __gnu_inline__)) int
wrap(const char *p, int f, ...)
{
    if (__builtin_constant_p(f)) {
        if ((f & 64) && __builtin_va_arg_pack_len() < 1) {
            missing_mode();
            return lib2(p, f);
        }
        return lib(p, f, __builtin_va_arg_pack());
    }
    if (__builtin_va_arg_pack_len() < 1)
        return lib2(p, f);
    return lib(p, f, __builtin_va_arg_pack());
}

int total;

int main(void)
{
    for (int fd = 0; fd <= 2; fd++) {
        if (fails(fd)) {
            int mode = fd == 0 ? 1 : 0;
            total += wrap("/dev/null", mode);
        }
    }
    return total == 3 ? 0 : 1;
}
"#;
    compile_and_run_everywhere("builtins_constant_p_first_trip", src);
}
