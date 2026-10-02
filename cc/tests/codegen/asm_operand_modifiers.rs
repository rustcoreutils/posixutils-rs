//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Inline-asm operand modifiers and immediates, run.
//
// Each program uses the modifiers real code uses -- x86-64 `%c`, `%P`, `%n`,
// `%a`, `%V`; aarch64 `%c`, `%n`, `%a`, `%w`/`%x` of a constant zero, the
// vector widths -- and constants in data directives, where an immediate
// prefix does not assemble. gcc runs each identically. Before, aarch64 wrote
// every constant as `#4`, so `.word %0` failed to assemble, and x86-64
// passed `%c0` through as text.
//

#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
use crate::common::compile_and_run;
use crate::common::compile_and_run_aarch64;

/// x86-64: constants without `$` in data directives (`%c`, `%P`, and the
/// value sign-extended from its type, as gcc writes it), `%n`, `%a` of a
/// register and of a symbol, `%V`, `%P` of a function; a constant no
/// immediate alternative takes goes in a register (`"rI"(100)`, and
/// `"rm"` under `bsr`, which has no immediate form).
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
#[test]
fn codegen_asm_x86_64_operand_modifiers() {
    let src = r#"
long g = 42;
static long helper(void) { return 7; }
int main(void) {
    const long *t;
    __asm__(".pushsection .data\n\t.balign 8\n"
            "1:\t.quad %c1, %P2, %c3, %c4\n\t.long %c5\n\t.byte %c6\n\t.popsection\n\t"
            "leaq 1b(%%rip), %0"
            : "=r"(t)
            : "i"(7), "i"(-3), "i"(&g), "i"(&g + 1), "i"(0xffffffffu), "n"(sizeof(int) * 3));
    if (t[0] != 7 || t[1] != -3 || t[2] != (long)&g || t[3] != (long)(&g + 1))
        return 1;
    if (((const int *)t)[8] != -1 || ((const unsigned char *)t)[36] != 12)
        return 2;
    long v;
    __asm__("movq $%n1, %0" : "=r"(v) : "i"(5));
    if (v != -5)
        return 3;
    long *p = &g, w;
    __asm__("movq %a1, %0" : "=r"(w) : "r"(p));
    if (w != 42)
        return 4;
    __asm__("movq %a1, %0" : "=r"(w) : "i"(&g));
    if (w != 42)
        return 5;
    __asm__("movq %%%V1, %0" : "=r"(w) : "r"(p));
    if (w != (long)&g)
        return 6;
    long (*fp)(void);
    __asm__("leaq %P1(%%rip), %0" : "=r"(fp) : "i"(helper));
    if (fp != helper || fp() != 7)
        return 7;
    /* A constant no immediate alternative takes goes in a register. */
    long a = 1;
    __asm__("addq %1, %0" : "+r"(a) : "rI"(100L));
    if (a != 101)
        return 8;
    __asm__("bsrq %1, %0" : "=r"(w) : "rm"(0x100L));
    if (w != 8)
        return 9;
    __asm__("shlq %1, %0" : "+r"(a) : "J"(2));
    if (a != 404)
        return 10;
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        let opts = vec![opt.to_string()];
        assert_eq!(compile_and_run("asm_x86_mods", src, &opts), 0, "at {opt}");
    }
}

/// aarch64: constants written bare, so data directives assemble; `%c`,
/// `%n`, `%a`; `%w`/`%x` of a constant zero name the zero register; a
/// constant `I` cannot encode goes in a register; a floating immediate
/// `fmov` encodes; vector widths renamed by modifier.
#[test]
fn codegen_asm_aarch64_operand_modifiers() {
    let src = r#"
long g = 42;
int main(void) {
    const long *t;
    __asm__(".pushsection .data\n\t.balign 8\n"
            "1:\t.quad %1, %c2, %3\n\t.word %4\n\t.byte %c5\n\t.popsection\n\t"
            "adrp %0, 1b\n\tadd %0, %0, :lo12:1b"
            : "=r"(t)
            : "i"(7), "i"(-3), "S"(&g), "i"(0xffffffffu), "n"(sizeof(int) * 3));
    if (t[0] != 7 || t[1] != -3 || t[2] != (long)&g)
        return 1;
    if (((const int *)t)[6] != -1 || ((const unsigned char *)t)[28] != 12)
        return 2;
    long v;
    __asm__("mov %0, %n1" : "=r"(v) : "i"(5));
    if (v != -5)
        return 3;
    long *p = &g;
    __asm__("ldr %0, %a1" : "=r"(v) : "r"(p));
    if (v != 42)
        return 4;
    unsigned w = 7, x = 7;
    __asm__("mov %w0, %w1" : "=r"(w) : "rZ"(0));
    __asm__("mov %x0, %x1" : "=r"(v) : "rZ"(0L));
    if (w != 0 || v != 0)
        return 5;
    __asm__("mov %w0, %w1" : "=r"(x) : "rZ"(9));
    if (x != 9)
        return 6;
    long a = 1;
    __asm__("add %0, %0, %1" : "+r"(a) : "rI"(5000L));
    __asm__("add %0, %0, %1" : "+r"(a) : "rI"(100L));
    if (a != 5101)
        return 7;
    double d;
    float f = 2.5f, r;
    __asm__("fmov %d0, %1" : "=w"(d) : "i"(1.5));
    __asm__("fmov %s0, %s1" : "=w"(r) : "w"(f));
    if (d != 1.5 || r != 2.5f)
        return 8;
    __asm__("fmov %d0, %d1" : "=w"(d) : "w"(f));
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        if let Some(rc) = compile_and_run_aarch64("asm_a64_mods", src, opt) {
            assert_eq!(rc, 0, "aarch64 at {opt}");
        }
    }
}
