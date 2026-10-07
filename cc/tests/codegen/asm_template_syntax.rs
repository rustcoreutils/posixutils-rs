//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Extended-asm template syntax beyond operands: `%=`, the number unique to
// each emitted asm.
//

#[cfg(target_arch = "x86_64")]
use crate::common::compile_and_run;
use crate::common::compile_and_run_aarch64;

/// gmp's `ASM_L(name)` is `".Lasm_%=_" #name`: a label private to one asm.
/// The asm below sits in an inline function called twice, so once inlined it
/// is emitted twice in one function, and each copy needs its own number or
/// the labels are defined twice. gcc's number is per emitted instance.
const X86_LOOP: &str = r#"
static inline long count_up(long x, long lim) {
    __asm__("jmp .Lchk%=\n"
            ".Ltop%=:\n\tincq %0\n"
            ".Lchk%=:\n\tcmpq %1, %0\n\tjl .Ltop%="
            : "+r"(x) : "r"(lim));
    return x;
}
int main(void) {
    long a = count_up(1, 5);
    long b = count_up(10, 12);
    return !(a == 5 && b == 12);
}
"#;

#[cfg(target_arch = "x86_64")]
#[test]
fn asm_template_percent_equals_is_unique_per_instance_x86_64() {
    assert_eq!(compile_and_run("asm_pct_eq", X86_LOOP, &[]), 0);
    assert_eq!(
        compile_and_run("asm_pct_eq_o2", X86_LOOP, &["-O2".to_string()]),
        0
    );
}

#[test]
fn asm_template_percent_equals_is_unique_per_instance_aarch64() {
    let src = r#"
static inline long count_up(long x, long lim) {
    __asm__("b .Lchk%=\n"
            ".Ltop%=:\n\tadd %0, %0, #1\n"
            ".Lchk%=:\n\tcmp %0, %1\n\tb.lt .Ltop%="
            : "+r"(x) : "r"(lim) : "cc");
    return x;
}
int main(void) {
    long a = count_up(1, 5);
    long b = count_up(10, 12);
    return !(a == 5 && b == 12);
}
"#;
    for opt in ["-O0", "-O2"] {
        if let Some(rc) = compile_and_run_aarch64("asm_pct_eq_a64", src, opt) {
            assert_eq!(rc, 0, "{opt}");
        }
    }
}
