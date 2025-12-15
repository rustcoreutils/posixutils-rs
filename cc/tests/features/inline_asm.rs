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
// x86-64 Inline Assembly Tests
// ============================================================================

#[cfg(target_arch = "x86_64")]
mod x86_64_tests {
    use super::*;

    #[test]
    fn asm_basic_nop() {
        let code = r#"
int main(void) {
    __asm__ volatile("nop");
    return 0;
}
"#;
        assert_eq!(compile_and_run("asm_nop_x86", code), 0);
    }

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
}

// ============================================================================
// AArch64 Inline Assembly Tests
// ============================================================================

#[cfg(target_arch = "aarch64")]
mod aarch64_tests {
    use super::*;

    #[test]
    fn asm_basic_nop() {
        let code = r#"
int main(void) {
    __asm__ volatile("nop");
    return 0;
}
"#;
        assert_eq!(compile_and_run("asm_nop_aarch64", code), 0);
    }

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
}
