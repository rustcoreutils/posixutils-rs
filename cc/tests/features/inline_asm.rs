//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for inline assembly support (GCC-compatible extended asm)
//
// Tests are architecture-conditional since inline assembly is platform-specific.
//

use crate::common::compile_and_run;

// ============================================================================
// Architecture-Independent Inline Assembly Tests
// ============================================================================

/// Test basic nop instruction - works on all architectures since every
/// assembler recognizes "nop" as a valid no-operation instruction.
#[test]
fn asm_basic_nop() {
    let code = r#"
int main(void) {
    __asm__ volatile("nop");
    return 0;
}
"#;
    assert_eq!(compile_and_run("asm_nop", code), 0);
}

// ============================================================================
// x86-64 Inline Assembly Tests
// ============================================================================

#[cfg(target_arch = "x86_64")]
mod x86_64_tests {
    use super::*;

    #[test]
    fn asm_output_register() {
        let code = r#"
int main(void) {
    int result;
    __asm__("movl $42, %0" : "=r"(result));
    return result == 42 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_output_x86", code), 0);
    }

    #[test]
    fn asm_input_output() {
        let code = r#"
int main(void) {
    int x = 10;
    __asm__("addl $5, %0" : "+r"(x));
    return x == 15 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_inout_x86", code), 0);
    }

    #[test]
    fn asm_two_operands() {
        let code = r#"
int main(void) {
    int a = 10, b = 20;
    int result;
    __asm__("movl %1, %0\n\t"
            "addl %2, %0"
            : "=r"(result)
            : "r"(a), "r"(b));
    return result == 30 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_two_ops_x86", code), 0);
    }

    #[test]
    fn asm_multiline() {
        let code = r#"
int main(void) {
    int x = 5;
    __asm__ volatile(
        "nop\n\t"
        "nop\n\t"
        "nop"
    );
    return 0;
}
"#;
        assert_eq!(compile_and_run("asm_multiline_x86", code), 0);
    }

    #[test]
    fn asm_with_clobbers() {
        let code = r#"
int main(void) {
    int result = 0;
    __asm__ volatile("movl $1, %0" : "=r"(result) : : "memory");
    return result == 1 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_clobbers_x86", code), 0);
    }

    #[test]
    fn asm_syntax_variants() {
        // Test __asm__ spelling
        let code1 = r#"
int main(void) {
    __asm__ volatile("nop");
    return 0;
}
"#;
        assert_eq!(compile_and_run("asm_spelling1_x86", code1), 0);

        // Test asm spelling
        let code2 = r#"
int main(void) {
    asm volatile("nop");
    return 0;
}
"#;
        assert_eq!(compile_and_run("asm_spelling2_x86", code2), 0);

        // Test __asm spelling
        let code3 = r#"
int main(void) {
    __asm volatile("nop");
    return 0;
}
"#;
        assert_eq!(compile_and_run("asm_spelling3_x86", code3), 0);
    }

    // ========================================================================
    // Phase 2 Tests - Matching Constraints, Named Operands, Clobbers
    // ========================================================================

    #[test]
    fn asm_matching_constraint() {
        // Test matching constraint "0" - use same register as output %0
        let code = r#"
int main(void) {
    int x = 10;
    __asm__("addl $5, %0" : "=r"(x) : "0"(x));
    return x == 15 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_match_x86", code), 0);
    }

    #[test]
    fn asm_named_operand() {
        // Test named operand syntax [name]
        let code = r#"
int main(void) {
    int result;
    __asm__("movl $42, %[out]" : [out] "=r"(result));
    return result == 42 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_named_x86", code), 0);
    }

    #[test]
    fn asm_named_input_output() {
        // Test named operands for both input and output
        let code = r#"
int main(void) {
    int x = 10, y = 20;
    int result;
    __asm__("movl %[a], %[res]\n\t"
            "addl %[b], %[res]"
            : [res] "=r"(result)
            : [a] "r"(x), [b] "r"(y));
    return result == 30 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_named_inout_x86", code), 0);
    }

    #[test]
    fn asm_memory_clobber() {
        // Test memory clobber - prevents compiler from caching values
        let code = r#"
int main(void) {
    int x = 10;
    __asm__ volatile("" ::: "memory");
    return x == 10 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_memclob_x86", code), 0);
    }

    #[test]
    fn asm_cc_clobber() {
        // Test cc (condition codes) clobber
        let code = r#"
int main(void) {
    int x = 5;
    __asm__ volatile("testl %0, %0" : : "r"(x) : "cc");
    return 0;
}
"#;
        assert_eq!(compile_and_run("asm_ccclob_x86", code), 0);
    }

    #[test]
    fn asm_early_clobber() {
        // Test early clobber (&) - output is written before inputs are read
        // This means output cannot share register with inputs
        let code = r#"
int main(void) {
    int x = 10, y = 20;
    int result;
    __asm__("movl %1, %0\n\t"
            "addl %2, %0"
            : "=&r"(result)
            : "r"(x), "r"(y));
    return result == 30 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_earlyclob_x86", code), 0);
    }

    // ========================================================================
    // Phase 3 Tests - x86-specific register constraints
    // ========================================================================

    #[test]
    fn asm_specific_register_a() {
        // Test "=a" constraint - output must go to EAX
        let code = r#"
int main(void) {
    int result;
    __asm__("movl $42, %%eax" : "=a"(result));
    return result == 42 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_reg_a", code), 0);
    }

    #[test]
    fn asm_specific_register_c_input() {
        // Test "c" constraint - input must go to ECX (for shifts)
        let code = r#"
int main(void) {
    int value = 4;
    int count = 2;
    __asm__("shll %%cl, %0" : "+r"(value) : "c"(count));
    return value == 16 ? 0 : 1;  // 4 << 2 = 16
}
"#;
        assert_eq!(compile_and_run("asm_reg_c", code), 0);
    }

    #[test]
    fn asm_specific_register_d() {
        // Test "=d" constraint - output must go to EDX
        let code = r#"
int main(void) {
    int result;
    __asm__("movl $99, %%edx" : "=d"(result));
    return result == 99 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_reg_d", code), 0);
    }

    #[test]
    fn asm_specific_register_ad() {
        // Test "=a" and "=d" together (common for div/mul results)
        let code = r#"
int main(void) {
    int lo, hi;
    __asm__("movl $100, %%eax\n\t"
            "movl $200, %%edx"
            : "=a"(lo), "=d"(hi));
    return (lo == 100 && hi == 200) ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_reg_ad", code), 0);
    }

    // ========================================================================
    // Phase 3 Tests - asm goto (jump to C labels from inline asm)
    // ========================================================================

    #[test]
    fn asm_goto_basic() {
        // Test basic asm goto - jump to a label if condition is met
        let code = r#"
int main(void) {
    int x = 5;
    __asm__ goto("cmpl $5, %0\n\t"
                 "je %l[match]"
                 : /* no outputs */
                 : "r"(x)
                 : "cc"
                 : match);
    return 1;  // Should not reach here
match:
    return 0;  // Should jump here
}
"#;
        assert_eq!(compile_and_run("asm_goto_basic", code), 0);
    }

    #[test]
    fn asm_goto_fallthrough() {
        // Test asm goto that doesn't jump (fallthrough case)
        let code = r#"
int main(void) {
    int x = 10;
    __asm__ goto("cmpl $5, %0\n\t"
                 "je %l[match]"
                 : /* no outputs */
                 : "r"(x)
                 : "cc"
                 : match);
    return 0;  // Should reach here (x != 5)
match:
    return 1;  // Should not jump here
}
"#;
        assert_eq!(compile_and_run("asm_goto_fallthrough", code), 0);
    }

    #[test]
    fn asm_goto_named_label() {
        // Test asm goto with named label reference %l[name]
        let code = r#"
int main(void) {
    int x = 5;
    __asm__ goto("cmpl $5, %0\n\t"
                 "je %l[target]"
                 : /* no outputs */
                 : "r"(x)
                 : "cc"
                 : target);
    return 1;
target:
    return 0;
}
"#;
        assert_eq!(compile_and_run("asm_goto_named", code), 0);
    }

    #[test]
    fn asm_goto_multiple_labels() {
        // Test asm goto with multiple possible targets
        let code = r#"
int main(void) {
    int x = 2;
    __asm__ goto("cmpl $1, %0\n\t"
                 "je %l[one]\n\t"
                 "cmpl $2, %0\n\t"
                 "je %l[two]\n\t"
                 "jmp %l[other]"
                 : /* no outputs */
                 : "r"(x)
                 : "cc"
                 : one, two, other);
    return 99;  // Should not reach here
one:
    return 1;
two:
    return 0;  // x==2, should land here
other:
    return 3;
}
"#;
        assert_eq!(compile_and_run("asm_goto_multi", code), 0);
    }
}

// ============================================================================
// AArch64 Inline Assembly Tests
// ============================================================================

#[cfg(target_arch = "aarch64")]
mod aarch64_tests {
    use super::*;

    #[test]
    fn asm_output_register() {
        let code = r#"
int main(void) {
    int result;
    __asm__("mov %w0, #42" : "=r"(result));
    return result == 42 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_output_aarch64", code), 0);
    }

    #[test]
    fn asm_input_output() {
        let code = r#"
int main(void) {
    int x = 10;
    __asm__("add %w0, %w0, #5" : "+r"(x));
    return x == 15 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_inout_aarch64", code), 0);
    }

    #[test]
    fn asm_two_operands() {
        let code = r#"
int main(void) {
    int a = 10, b = 20;
    int result;
    __asm__("add %w0, %w1, %w2"
            : "=r"(result)
            : "r"(a), "r"(b));
    return result == 30 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_two_ops_aarch64", code), 0);
    }

    #[test]
    fn asm_multiline() {
        let code = r#"
int main(void) {
    __asm__ volatile(
        "nop\n\t"
        "nop\n\t"
        "nop"
    );
    return 0;
}
"#;
        assert_eq!(compile_and_run("asm_multiline_aarch64", code), 0);
    }

    #[test]
    fn asm_with_clobbers() {
        let code = r#"
int main(void) {
    int result = 0;
    __asm__ volatile("mov %w0, #1" : "=r"(result) : : "memory");
    return result == 1 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_clobbers_aarch64", code), 0);
    }

    #[test]
    fn asm_syntax_variants() {
        // Test __asm__ spelling
        let code1 = r#"
int main(void) {
    __asm__ volatile("nop");
    return 0;
}
"#;
        assert_eq!(compile_and_run("asm_spelling1_aarch64", code1), 0);

        // Test asm spelling
        let code2 = r#"
int main(void) {
    asm volatile("nop");
    return 0;
}
"#;
        assert_eq!(compile_and_run("asm_spelling2_aarch64", code2), 0);

        // Test __asm spelling
        let code3 = r#"
int main(void) {
    __asm volatile("nop");
    return 0;
}
"#;
        assert_eq!(compile_and_run("asm_spelling3_aarch64", code3), 0);
    }

    // ========================================================================
    // Phase 2 Tests - Matching Constraints, Named Operands, Clobbers
    // ========================================================================

    #[test]
    fn asm_matching_constraint() {
        // Test matching constraint "0" - use same register as output %0
        let code = r#"
int main(void) {
    int x = 10;
    __asm__("add %w0, %w0, #5" : "=r"(x) : "0"(x));
    return x == 15 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_match_aarch64", code), 0);
    }

    #[test]
    fn asm_named_operand() {
        // Test named operand syntax [name]
        let code = r#"
int main(void) {
    int result;
    __asm__("mov %w[out], #42" : [out] "=r"(result));
    return result == 42 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_named_aarch64", code), 0);
    }

    #[test]
    fn asm_named_input_output() {
        // Test named operands for both input and output
        let code = r#"
int main(void) {
    int x = 10, y = 20;
    int result;
    __asm__("add %w[res], %w[a], %w[b]"
            : [res] "=r"(result)
            : [a] "r"(x), [b] "r"(y));
    return result == 30 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_named_inout_aarch64", code), 0);
    }

    #[test]
    fn asm_memory_clobber() {
        // Test memory clobber - prevents compiler from caching values
        let code = r#"
int main(void) {
    int x = 10;
    __asm__ volatile("" ::: "memory");
    return x == 10 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_memclob_aarch64", code), 0);
    }

    #[test]
    fn asm_cc_clobber() {
        // Test cc (condition codes) clobber
        let code = r#"
int main(void) {
    int x = 5;
    __asm__ volatile("cmp %w0, #0" : : "r"(x) : "cc");
    return 0;
}
"#;
        assert_eq!(compile_and_run("asm_ccclob_aarch64", code), 0);
    }

    #[test]
    fn asm_early_clobber() {
        // Test early clobber (&) - output is written before inputs are read
        // This means output cannot share register with inputs
        let code = r#"
int main(void) {
    int x = 10, y = 20;
    int result;
    __asm__("add %w0, %w1, %w2"
            : "=&r"(result)
            : "r"(x), "r"(y));
    return result == 30 ? 0 : 1;
}
"#;
        assert_eq!(compile_and_run("asm_earlyclob_aarch64", code), 0);
    }

    // ========================================================================
    // Phase 3 Tests - asm goto (jump to C labels from inline asm)
    // ========================================================================

    #[test]
    fn asm_goto_basic() {
        // Test basic asm goto - jump to a label if condition is met
        let code = r#"
int main(void) {
    int x = 5;
    __asm__ goto("cmp %w0, #5\n\t"
                 "beq %l[match]"
                 : /* no outputs */
                 : "r"(x)
                 : "cc"
                 : match);
    return 1;  // Should not reach here
match:
    return 0;  // Should jump here
}
"#;
        assert_eq!(compile_and_run("asm_goto_basic_aarch64", code), 0);
    }

    #[test]
    fn asm_goto_fallthrough() {
        // Test asm goto that doesn't jump (fallthrough case)
        let code = r#"
int main(void) {
    int x = 10;
    __asm__ goto("cmp %w0, #5\n\t"
                 "beq %l[match]"
                 : /* no outputs */
                 : "r"(x)
                 : "cc"
                 : match);
    return 0;  // Should reach here (x != 5)
match:
    return 1;  // Should not jump here
}
"#;
        assert_eq!(compile_and_run("asm_goto_fallthrough_aarch64", code), 0);
    }

    #[test]
    fn asm_goto_named_label() {
        // Test asm goto with named label reference %l[name]
        let code = r#"
int main(void) {
    int x = 5;
    __asm__ goto("cmp %w0, #5\n\t"
                 "beq %l[target]"
                 : /* no outputs */
                 : "r"(x)
                 : "cc"
                 : target);
    return 1;
target:
    return 0;
}
"#;
        assert_eq!(compile_and_run("asm_goto_named_aarch64", code), 0);
    }

    #[test]
    fn asm_goto_multiple_labels() {
        // Test asm goto with multiple possible targets
        let code = r#"
int main(void) {
    int x = 2;
    __asm__ goto("cmp %w0, #1\n\t"
                 "beq %l[one]\n\t"
                 "cmp %w0, #2\n\t"
                 "beq %l[two]\n\t"
                 "b %l[other]"
                 : /* no outputs */
                 : "r"(x)
                 : "cc"
                 : one, two, other);
    return 99;  // Should not reach here
one:
    return 1;
two:
    return 0;  // x==2, should land here
other:
    return 3;
}
"#;
        assert_eq!(compile_and_run("asm_goto_multi_aarch64", code), 0);
    }
}
