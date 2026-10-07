//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU basic asm at file scope: its text reaches the assembly verbatim, in
// source order with the functions and objects around it, on every target;
// and whatever section it leaves the assembler in, the definitions after it
// are still placed in their own.
//

use super::asm_probe::{asm_for_with, AARCH64_DARWIN, AARCH64_LINUX, X86_64_LINUX};
use crate::test_compile::compile_warnings;

const X86_64_DARWIN: &str = "x86_64-apple-darwin";
const TRIPLES: [&str; 4] = [X86_64_LINUX, AARCH64_LINUX, X86_64_DARWIN, AARCH64_DARWIN];

/// The symbol prefix the target puts on a C identifier.
fn prefix(triple: &str) -> &'static str {
    if triple.contains("apple") {
        "_"
    } else {
        ""
    }
}

/// The index of the first line of `asm` that is exactly `line`, trimmed.
fn line_index(asm: &str, line: &str) -> usize {
    asm.lines()
        .position(|l| l.trim() == line)
        .unwrap_or_else(|| panic!("no line {line:?} in:\n{asm}"))
}

/// The section directive in force at line `at` of `asm`: the last one before
/// it, as written.
fn section_at(asm: &str, at: usize) -> String {
    asm.lines()
        .take(at)
        .map(str::trim)
        .filter(|l| {
            matches!(*l, ".text" | ".data" | ".bss")
                || l.starts_with(".section ")
                || l.starts_with(".pushsection")
                || l.starts_with(".popsection")
        })
        .last()
        .unwrap_or_default()
        .to_string()
}

fn is_text(section: &str) -> bool {
    section == ".text" || section == ".section __TEXT,__text"
}

fn is_data(section: &str) -> bool {
    section == ".data" || section == ".section __DATA,__data"
}

const PLACEMENT: &str = r#"
int before(void) { return 1; }
__asm__(".section .rodata\n" "fsasm_mark1: .byte 1");
int data1 = 5;
int after(void) { return data1; }
__asm__("fsasm_mark2: .byte 2 /* %eax %% */");
"#;

/// The asm text sits where the source put it -- after `before`, ahead of
/// `data1` and `after` -- and the second after them; each is written as it
/// was, with nothing substituted for `%`.
#[test]
fn file_scope_asm_lands_in_source_order() {
    for triple in TRIPLES {
        for opt in ["-O0", "-O2"] {
            let asm = asm_for_with("fsasm_order", triple, PLACEMENT, &[opt]);
            let p = prefix(triple);
            let before = line_index(&asm, &format!("{p}before:"));
            let mark1 = line_index(&asm, "fsasm_mark1: .byte 1");
            let data1 = line_index(&asm, &format!("{p}data1:"));
            let after = line_index(&asm, &format!("{p}after:"));
            let mark2 = line_index(&asm, "fsasm_mark2: .byte 2 /* %eax %% */");
            assert!(
                before < mark1 && mark1 < data1 && data1 < after && after < mark2,
                "{triple} {opt}: out of order:\n{asm}"
            );
            assert!(
                asm.contains(".section .rodata\nfsasm_mark1: .byte 1\n"),
                "{triple} {opt}: not verbatim:\n{asm}"
            );
        }
    }
}

/// The first asm leaves the assembler in `.rodata`; `data1` still goes to
/// the data section and `after` to the text section.
#[test]
fn file_scope_asm_section_switch_does_not_move_what_follows() {
    for triple in TRIPLES {
        let asm = asm_for_with("fsasm_sections", triple, PLACEMENT, &["-O0"]);
        let p = prefix(triple);
        let data1 = line_index(&asm, &format!("{p}data1:"));
        let after = line_index(&asm, &format!("{p}after:"));
        assert!(
            is_data(&section_at(&asm, data1)),
            "{triple}: data1 in {:?}:\n{asm}",
            section_at(&asm, data1)
        );
        assert!(
            is_text(&section_at(&asm, after)),
            "{triple}: after in {:?}:\n{asm}",
            section_at(&asm, after)
        );
    }
}

/// The DWARF end-of-text label follows the last function in the text
/// section, even when an asm after that function switched away from it.
#[test]
fn file_scope_asm_leaves_debug_text_end_in_text() {
    let src = "int f(void) { return 0; }\n__asm__(\".data\\n.byte 7\");\n";
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("fsasm_debug", triple, src, &["-g"]);
        let end = line_index(&asm, ".Ltext_end:");
        assert!(
            is_text(&section_at(&asm, end)),
            "{triple}: .Ltext_end in {:?}:\n{asm}",
            section_at(&asm, end)
        );
    }
}

/// A file-scope asm declares nothing, so it is no "empty declaration"; and
/// gcc has no pedantic warning for `__asm__` or `asm` there.
#[test]
fn file_scope_asm_draws_no_warning() {
    let src = "asm(\"nop\");\n__asm(\"nop\");\n__asm__(\".symver a, b@V\");\nint a;\n";
    for flags in [&[][..], &["-pedantic"][..], &["-Wall", "-Wextra"][..]] {
        let warnings = compile_warnings("fsasm_quiet", src, flags);
        assert!(warnings.is_empty(), "{flags:?}: {warnings:?}");
    }
}

/// A unit holding only an asm still emits it.
#[test]
fn file_scope_asm_alone_is_emitted() {
    for triple in TRIPLES {
        let asm = asm_for_with("fsasm_alone", triple, "__asm__(\"# only\");\n", &[]);
        assert!(asm.contains("\n# only\n"), "{triple}:\n{asm}");
    }
}
