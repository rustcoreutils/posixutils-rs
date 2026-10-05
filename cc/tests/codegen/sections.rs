//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Section-classification tests
//
// Verifies that c17 routes global variables into the appropriate object-file
// sections:
//
//   * `.rodata`        — const data without relocations
//   * `.data.rel.ro`   — const data containing symbol addresses
//                       (on Mach-O the dynamic linker handles this inside
//                       `__DATA,__const`, so we accept either form)
//   * `.bss` (via `.comm` / `.local`+`.comm` / `.zerofill`) — zero-initialized
//   * `.data`          — everything else
//
// The behavioral test compiles AND runs a C program, so correctness of the
// routing is verified end-to-end on whichever host runs the suite. The
// assembly-text checks are in `cc/test_asm/codegen_sections.rs`. The
// directive-shape assertions are conditioned on `cfg!(target_os)` so that
// the same test source works on Linux/x86_64 and macOS/aarch64 (our two
// supported tier-1 hosts).
//

use crate::common::compile_and_run;

// ----------------------------------------------------------------------------
// Behavioral correctness — programs must run and produce expected output
// regardless of how the globals are classified.
// ----------------------------------------------------------------------------

#[test]
fn sections_runtime_behavior_mega() {
    // One self-checking program that exercises every routing path. Each block
    // returns a distinct non-zero code if it observes the wrong value, so a
    // regression in any routing path surfaces as a specific exit code.
    //
    // Also consolidates sections_tentative_extern_is_zero (code 14).
    let code = r#"
        const int ro_scalar = 0xCAFE;
        const int ro_array[4] = { 10, 20, 30, 40 };
        static const long ro_static_long = 0x1122334455667788L;

        static int bss_static_scalar;            /* tentative => zero, BSS */
        static int bss_static_explicit_zero = 0; /* explicit zero, BSS */
        static int bss_static_array[64];         /* large zero array, BSS */
        int bss_extern_scalar;                   /* tentative external, .comm */

        int data_writable_scalar = 7;
        int data_writable_array[3] = { 100, 200, 300 };

        static const char hello[] = "hi";
        static const char goodbye[] = "bye";
        /* Pointer-to-char + const initializer with reloc => .data.rel.ro */
        static const char * const greetings[] = { hello, goodbye };

        /* Was sections_tentative_extern_is_zero: tentative external
           definitions (`int x;` at file scope, no initializer) must produce a
           runnable program whose global reads back as zero. The assembler
           resolves the tentative definition to BSS-class storage. */
        int tentative_extern;

        int main(void) {
            if (ro_scalar != 0xCAFE) return 1;
            if (ro_array[0] != 10 || ro_array[3] != 40) return 2;
            if (ro_static_long != 0x1122334455667788L) return 3;

            if (bss_static_scalar != 0) return 4;
            if (bss_static_explicit_zero != 0) return 5;
            for (int i = 0; i < 64; ++i) {
                if (bss_static_array[i] != 0) return 6;
            }
            if (bss_extern_scalar != 0) return 7;

            if (data_writable_scalar != 7) return 8;
            data_writable_scalar = 9;
            if (data_writable_scalar != 9) return 9;
            if (data_writable_array[1] != 200) return 10;

            if (greetings[0][0] != 'h') return 11;
            if (greetings[1][0] != 'b') return 12;

            /* Mutate a BSS slot and confirm. */
            bss_static_array[7] = 0x55;
            if (bss_static_array[7] != 0x55) return 13;

            /* Tentative external (was sections_tentative_extern_is_zero). */
            if (tentative_extern != 0) return 14;

            return 0;
        }
    "#;
    assert_eq!(
        compile_and_run("sections_runtime_behavior_mega", code, &[]),
        0
    );
}
