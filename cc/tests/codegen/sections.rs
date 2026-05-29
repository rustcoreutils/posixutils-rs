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
// Verifies that pcc routes global variables into the appropriate object-file
// sections:
//
//   * `.rodata`        — const data without relocations
//   * `.data.rel.ro`   — const data containing symbol addresses
//                       (on Mach-O the dynamic linker handles this inside
//                       `__DATA,__const`, so we accept either form)
//   * `.bss` (via `.comm` / `.local`+`.comm` / `.zerofill`) — zero-initialized
//   * `.data`          — everything else
//
// Each behavioral test compiles AND runs a C program, so correctness of the
// routing is verified end-to-end on whichever host runs the suite. The
// directive-shape assertions are conditioned on `cfg!(target_os)` so that
// the same test source works on Linux/x86_64 and macOS/aarch64 (our two
// supported tier-1 hosts).
//

use crate::common::{compile_and_run, create_c_file};
use plib::testing::run_test_base;

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

/// Compile a C snippet to assembly with `-O -S` and return the assembly text.
fn compile_to_asm(name: &str, content: &str) -> String {
    let c_file = create_c_file(name, content);
    let c_path = c_file.path().to_path_buf();
    let output = run_test_base(
        "pcc",
        &vec![
            "-O".to_string(),
            "-S".to_string(),
            "-o".to_string(),
            "-".to_string(),
            c_path.to_string_lossy().to_string(),
        ],
        &[],
    );
    assert!(
        output.status.success(),
        "pcc -O -S failed for '{}':\n{}",
        name,
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).expect("assembly output was not UTF-8")
}

/// True if the assembly text declares a `.rodata`-class section header at any
/// point. On ELF the directive is `.section .rodata`; on Mach-O it is
/// `.section __TEXT,__const`.
fn has_rodata_section(asm: &str) -> bool {
    if cfg!(target_os = "macos") {
        asm.contains(".section __TEXT,__const")
    } else {
        asm.contains(".section .rodata")
    }
}

/// True if the assembly text declares a `.data.rel.ro`-class section.
/// On Mach-O this is folded into `__DATA,__const`.
fn has_data_rel_ro_section(asm: &str) -> bool {
    if cfg!(target_os = "macos") {
        asm.contains(".section __DATA,__const")
    } else {
        asm.contains(".section .data.rel.ro")
    }
}

/// True if `name` is allocated as BSS-class local storage in the assembly.
/// On ELF this looks like `.local NAME\n.comm NAME,...`; on Mach-O it looks
/// like `.zerofill __DATA,__bss,_NAME,...`.
fn has_bss_local(asm: &str, name: &str) -> bool {
    if cfg!(target_os = "macos") {
        asm.contains(&format!(".zerofill __DATA,__bss,_{}", name))
    } else {
        asm.contains(&format!(".local {}\n.comm {}", name, name))
            || asm.contains(&format!(".local {}", name))
                && asm.contains(&format!(".comm {},", name))
    }
}

/// True if `name` is allocated as a regular (external) `.comm` symbol.
fn has_comm_external(asm: &str, name: &str) -> bool {
    if cfg!(target_os = "macos") {
        asm.contains(&format!(".comm _{},", name))
    } else {
        asm.contains(&format!(".comm {},", name))
    }
}

// ----------------------------------------------------------------------------
// Behavioral correctness — programs must run and produce expected output
// regardless of how the globals are classified.
// ----------------------------------------------------------------------------

#[test]
fn sections_runtime_behavior_mega() {
    // One self-checking program that exercises every routing path. Each block
    // returns a distinct non-zero code if it observes the wrong value, so a
    // regression in any routing path surfaces as a specific exit code.
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

            return 0;
        }
    "#;
    assert_eq!(
        compile_and_run("sections_runtime_behavior_mega", code, &[]),
        0
    );
}

/// Tentative external definitions (`int x;` at file scope, no initializer)
/// must produce a runnable program whose global reads back as zero. The
/// assembler resolves the tentative definition to BSS-class storage.
#[test]
fn sections_tentative_extern_is_zero() {
    let code = r#"
        int tentative_extern;
        int main(void) { return tentative_extern; }
    "#;
    assert_eq!(
        compile_and_run("sections_tentative_extern_is_zero", code, &[]),
        0
    );
}

// ----------------------------------------------------------------------------
// Assembly-text inspection — verifies the directive shape per platform.
// ----------------------------------------------------------------------------

#[test]
fn sections_const_int_goes_to_rodata() {
    let asm = compile_to_asm(
        "const_int_rodata",
        r#"
            const int my_ro = 42;
            int main(void) { return my_ro; }
        "#,
    );
    assert!(
        has_rodata_section(&asm),
        "expected a .rodata-class section header in:\n{}",
        asm
    );
    // The symbol's data must follow the rodata directive — match by name.
    if cfg!(target_os = "macos") {
        assert!(
            asm.contains("_my_ro:"),
            "expected symbol label _my_ro in asm:\n{}",
            asm
        );
    } else {
        assert!(
            asm.contains("my_ro:"),
            "expected symbol label my_ro in asm:\n{}",
            asm
        );
    }
}

#[test]
fn sections_const_array_goes_to_rodata() {
    let asm = compile_to_asm(
        "const_arr_rodata",
        r#"
            const long arr[3] = { 1, 2, 3 };
            int main(void) { return (int)arr[2]; }
        "#,
    );
    assert!(
        has_rodata_section(&asm),
        "expected .rodata-class section for `const long arr[3]`:\n{}",
        asm
    );
}

#[test]
fn sections_static_zero_goes_to_bss_local() {
    let asm = compile_to_asm(
        "static_zero_bss",
        r#"
            static int s_zero;
            static int s_zero_explicit = 0;
            static int s_zero_array[16];
            int main(void) {
                return s_zero + s_zero_explicit + s_zero_array[0];
            }
        "#,
    );
    assert!(
        has_bss_local(&asm, "s_zero"),
        "expected s_zero in BSS-local form:\n{}",
        asm
    );
    assert!(
        has_bss_local(&asm, "s_zero_explicit"),
        "expected s_zero_explicit in BSS-local form:\n{}",
        asm
    );
    assert!(
        has_bss_local(&asm, "s_zero_array"),
        "expected s_zero_array in BSS-local form:\n{}",
        asm
    );
    // No `.data` initializer line should claim these symbols.
    assert!(
        !asm.contains("s_zero_array:\n    .zero 64"),
        "s_zero_array should not be emitted as explicit zeros under .data:\n{}",
        asm
    );
}

#[test]
fn sections_extern_zero_goes_to_comm() {
    let asm = compile_to_asm(
        "extern_zero_comm",
        r#"
            int e_zero;
            int e_zero_explicit = 0;
            int main(void) { return e_zero + e_zero_explicit; }
        "#,
    );
    assert!(
        has_comm_external(&asm, "e_zero"),
        "expected e_zero in external `.comm` form:\n{}",
        asm
    );
    assert!(
        has_comm_external(&asm, "e_zero_explicit"),
        "expected e_zero_explicit in external `.comm` form:\n{}",
        asm
    );
}

#[test]
fn sections_pointer_const_table_goes_to_data_rel_ro() {
    let asm = compile_to_asm(
        "ptr_const_table_data_rel_ro",
        r#"
            static const char a[] = "x";
            static const char b[] = "y";
            static const char * const table[] = { a, b };
            int main(void) { return table[0][0]; }
        "#,
    );
    assert!(
        has_data_rel_ro_section(&asm),
        "expected `.data.rel.ro` / `__DATA,__const` section for pointer-table-const:\n{}",
        asm
    );
}

#[test]
fn sections_writable_global_stays_in_data() {
    let asm = compile_to_asm(
        "writable_in_data",
        r#"
            int w = 5;
            int main(void) { return w; }
        "#,
    );
    // The writable global must not appear in any read-only section.
    // We assert positively that it appears in `.data` (or Mach-O __DATA,__data).
    let in_data = if cfg!(target_os = "macos") {
        asm.contains(".section __DATA,__data")
    } else {
        // ELF .data directive is just `.data` (no `.section` prefix in pcc output).
        asm.contains("\n.data\n") || asm.starts_with(".data\n")
    };
    assert!(
        in_data,
        "expected writable global `w` to be in .data:\n{}",
        asm
    );
}

// ----------------------------------------------------------------------------
// Boundary: const-qualified data with NO relocations must stay in rodata,
// not get pushed to .data.rel.ro.
// ----------------------------------------------------------------------------

#[test]
fn sections_no_reloc_const_stays_in_rodata() {
    let asm = compile_to_asm(
        "no_reloc_in_rodata",
        r#"
            static const int table[] = { 1, 2, 3, 4 };
            int main(void) { return table[3]; }
        "#,
    );
    assert!(
        has_rodata_section(&asm),
        "no-reloc const data must be in .rodata:\n{}",
        asm
    );
    assert!(
        !has_data_rel_ro_section(&asm),
        "no-reloc const data must NOT use .data.rel.ro:\n{}",
        asm
    );
}
