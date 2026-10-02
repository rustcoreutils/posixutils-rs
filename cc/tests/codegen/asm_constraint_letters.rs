//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Inline-asm constraint letters: what each letter means on each target.
//
// A letter is classified once, for the target being compiled, and every
// consumer -- the linearizer, liveness, the register allocator and the
// backend that substitutes the template -- reads that one classification.
// Each test here is a program on which two of those consumers used to
// disagree.
//

use crate::common::{compile_and_run, compile_and_run_aarch64, compile_and_run_everywhere};

/// Run `src` on the host at the matrix levels and at -O0 and -O2.
#[cfg(target_arch = "x86_64")]
fn run_host_levels(name: &str, src: &str) {
    assert_eq!(compile_and_run(name, src, &[]), 0, "{name} at the matrix");
    for opt in ["-O0", "-O2"] {
        let level = vec![opt.to_string()];
        assert_eq!(compile_and_run(name, src, &level), 0, "{name} at {opt}");
    }
}

/// Run `src` on aarch64 under qemu at -O0 and -O2, when the cross toolchain
/// is present.
fn run_aarch64_levels(name: &str, src: &str) {
    for opt in ["-O0", "-O2"] {
        if let Some(rc) = compile_and_run_aarch64(name, src, opt) {
            assert_eq!(rc, 0, "{name} on aarch64 at {opt}");
        }
    }
}

/// x86-64 `Q` is a register class -- `a`, `b`, `c` or `d`, the registers
/// with an addressable high byte -- not memory. Taken for memory, the
/// operand was handed over as an address and `%h1` named no register.
#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_asm_x86_64_q_is_a_high_byte_register() {
    let src = r#"
int main(void) {
    unsigned long x = 0x1234, y;
    unsigned char r;
    __asm__("movb %h1, %0" : "=r"(r) : "Q"(x));
    if (r != 0x12)
        return 1;
    __asm__("xorl %k0, %k0\n\tmovb $0x56, %h0" : "=Q"(y));
    if (y != 0x5600)
        return 2;
    unsigned int z = 0x10ff;
    __asm__("incb %h0" : "+Q"(z));
    if (z != 0x11ff)
        return 3;
    return 0;
}
"#;
    run_host_levels("asm_x86_q_high_byte", src);
}

/// An early-clobber output pinned to one register (`"=&d"`) claims that
/// register at the asm. The allocator read `=&d` as an unknown letter `&`
/// and dropped the operand, so it never learned %rdx was written there and
/// left a live argument in it.
#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_asm_x86_64_early_clobber_pinned_output_claims_its_register() {
    let src = r#"
__attribute__((noinline)) long f(long a, long b, long c, long d, long e, long g) {
    long r;
    __asm__("movq $77, %0" : "=&d"(r) : "r"(a));
    return a + b + c + d + e + g + r;
}
__attribute__((noinline)) long h(long a, long b, long c, long d, long e, long g) {
    long r;
    __asm__("movq $77, %0" : "=&c"(r) : "r"(a));
    return a + b + c + d + e + g + r;
}
int main(void) {
    if (f(1, 2, 3, 4, 5, 6) != 98)
        return 1;
    if (h(1, 2, 3, 4, 5, 6) != 98)
        return 2;
    return 0;
}
"#;
    run_host_levels("asm_x86_early_clobber_pinned", src);
}

/// A matching constraint names an operand number, which may have two digits.
/// Read one digit at a time, `"10"` tied the input to output 0, which was
/// already tied, and the IR validator rejected the second definition.
#[test]
fn codegen_asm_matching_constraint_has_two_digits() {
    let src = r#"
int main(void) {
    int a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0, i = 0, j = 0, k = 0, l = 0;
    __asm__(""
            : "=r"(a), "=r"(b), "=r"(c), "=r"(d), "=r"(e), "=r"(f),
              "=r"(g), "=r"(h), "=r"(i), "=r"(j), "=r"(k), "=r"(l)
            : "0"(1), "1"(2), "2"(3), "3"(4), "4"(5), "5"(6),
              "6"(7), "7"(8), "8"(9), "9"(10), "10"(11), "11"(12));
    int got[12] = {a, b, c, d, e, f, g, h, i, j, k, l};
    for (int n = 0; n < 12; n++)
        if (got[n] != n + 1)
            return n + 1;
    return 0;
}
"#;
    compile_and_run_everywhere("asm_matching_two_digits", src);
}

/// aarch64 `w` is a register class, so `"+wm"` may be a register. The IR
/// read it as memory-only -- `w` was not one of its letters -- and passed the
/// address, while the backend chose the vector register: the template
/// doubled the pointer's bits and `*p` never changed. gcc -O0 also chooses
/// the register for this operand.
#[test]
fn codegen_asm_aarch64_w_or_memory_is_a_register_operand() {
    let src = r#"
__attribute__((noinline)) double twice(double *p) {
    __asm__("fadd %d0, %d0, %d0" : "+wm"(*p));
    return *p;
}
int main(void) {
    double v = 1.25;
    if (twice(&v) != 2.5)
        return 1;
    if (v != 2.5)
        return 2;
    return 0;
}
"#;
    run_aarch64_levels("asm_a64_wm_register", src);
}

/// aarch64 `Q` is memory addressed by a base register alone: `ldxr` and the
/// other exclusives encode no offset. A local or an array element addressed
/// in place as `[x29, #N]` did not assemble.
#[test]
fn codegen_asm_aarch64_q_is_addressed_by_a_bare_base_register() {
    let src = r#"
long g = 9;
int main(void) {
    long x = 41, r, z;
    long y[4] = {1, 2, 3, 77};
    __asm__ volatile("ldxr %0, %1\n\tclrex" : "=r"(r) : "Q"(x));
    if (r != 41)
        return 1;
    __asm__ volatile("ldxr %0, %1\n\tclrex" : "=r"(z) : "Q"(y[3]));
    if (z != 77)
        return 2;
    __asm__ volatile("ldxr %0, %1\n\tclrex" : "=r"(r) : "Q"(g));
    if (r != 9)
        return 3;
    return 0;
}
"#;
    run_aarch64_levels("asm_a64_q_base_only", src);
}

/// An immediate-only operand is written into the template as the constant
/// it is: a global's address with its offset (`$g`, `$x+8`), `sizeof`, an
/// enumeration constant -- and, once a `static inline` helper is inlined at
/// -O1 and above, the literal its caller passed to `"i"(param)`, the Linux
/// kernel's idiom. c17 used to substitute whatever register held the value,
/// so `"i"(&g)` gave `%rax`. `-no-pie`, because an absolute address is not
/// position-independent: gcc writes the same `$g`, and a PIE link refuses it.
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
#[test]
fn codegen_asm_x86_64_immediates_are_written_as_constants() {
    let src = r#"
long g = 42;
long x[2] = {5, 7};
enum { E = 5 };
int main(void) {
    long *p, *q, v;
    __asm__("movq %1, %0" : "=r"(p) : "i"(&g));
    if (*p != 42)
        return 1;
    __asm__("movq %1, %0" : "=r"(q) : "i"(&x[1]));
    if (*q != 7)
        return 2;
    __asm__("movq %1, %0" : "=r"(v) : "n"(sizeof(long) * E));
    if (v != 40)
        return 3;
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        let opts = vec![opt.to_string(), "-no-pie".to_string()];
        assert_eq!(compile_and_run("asm_x86_imm", src, &opts), 0, "at {opt}");
    }
    let inlined = r#"
static inline long add_const(long a, int k) {
    __asm__("addq %1, %0" : "+r"(a) : "i"(k));
    return a;
}
int main(void) {
    return add_const(1, 9) == 10 && add_const(5, -5) == 0 ? 0 : 1;
}
"#;
    for opt in ["-O1", "-O2"] {
        let opts = vec![opt.to_string()];
        assert_eq!(
            compile_and_run("asm_x86_imm_inl", inlined, &opts),
            0,
            "at {opt}"
        );
    }
}

/// aarch64 `S` writes a global's address with its offset into the template
/// (`x+8`), as an `adrp`/`:lo12:` pair needs; the inlined-literal idiom
/// works through `"i"` at -O2.
#[test]
fn codegen_asm_aarch64_immediates_are_written_as_constants() {
    let src = r#"
long g = 42;
long x[2] = {5, 7};
static inline long add_const(long a, int k) {
    __asm__("add %0, %0, %1" : "+r"(a) : "i"(k));
    return a;
}
int main(void) {
    long *p, *q, v;
    __asm__("adrp %0, %1\n\tadd %0, %0, :lo12:%1" : "=r"(p) : "S"(&g));
    if (*p != 42)
        return 1;
    __asm__("adrp %0, %1\n\tadd %0, %0, :lo12:%1" : "=r"(q) : "S"(&x[1]));
    if (*q != 7)
        return 2;
    __asm__("mov %0, %1" : "=r"(v) : "n"(sizeof(long) * 5));
    if (v != 40)
        return 3;
#ifdef __OPTIMIZE__
    if (add_const(1, 9) != 10)
        return 4;
#endif
    return 0;
}
"#;
    run_aarch64_levels("asm_a64_imm", src);
}
