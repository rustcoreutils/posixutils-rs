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
// The behavioral test, in `tests/codegen/sections.rs`, compiles AND runs a C
// program, so correctness of the routing is verified end-to-end on whichever
// host runs the suite. These are the assembly-text checks. The
// directive-shape assertions are conditioned on `cfg!(target_os)` so that
// the same test source works on Linux/x86_64 and macOS/aarch64 (our two
// supported tier-1 hosts).
//

use super::asm_probe::{asm_for_at, asm_symbol};

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

/// Compile a C snippet to assembly with `-O -S` and return the assembly text.
#[track_caller]
fn compile_to_asm(name: &str, content: &str) -> String {
    asm_for_at(name, content, &["-O"])
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
        asm.contains(&format!(".zerofill __DATA,__bss,{}", asm_symbol(name)))
    } else {
        asm.contains(&format!(".local {}\n.comm {}", name, name))
            || asm.contains(&format!(".local {}", name))
                && asm.contains(&format!(".comm {},", name))
    }
}

/// True if `name` is allocated as a regular (external) `.comm` symbol.
/// Is `name` an exported object with no initialized bytes?
///
/// A *definition*, not a common symbol: `.comm` merges across translation
/// units, so two definitions of one object would link silently where C17
/// 6.9p5 allows one. gcc has emitted none since it defaulted to
/// `-fno-common`.
fn has_bss_external(asm: &str, name: &str) -> bool {
    if cfg!(target_os = "macos") {
        asm.contains(&format!(".zerofill __DATA,__bss,{},", asm_symbol(name)))
    } else {
        asm.contains(&format!("\n{name}:\n.zero "))
    }
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
fn sections_extern_zero_goes_to_bss() {
    let asm = compile_to_asm(
        "extern_zero_bss",
        r#"
            int e_zero;
            int e_zero_explicit = 0;
            int main(void) { return e_zero + e_zero_explicit; }
        "#,
    );
    for name in ["e_zero", "e_zero_explicit"] {
        assert!(
            has_bss_external(&asm, name),
            "expected {name} as an external BSS definition:\n{asm}"
        );
        assert!(
            !asm.contains(&format!(".comm {name},")),
            "{name} must not be a common symbol:\n{asm}"
        );
    }
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
        // ELF .data directive is just `.data` (no `.section` prefix in c17 output).
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
