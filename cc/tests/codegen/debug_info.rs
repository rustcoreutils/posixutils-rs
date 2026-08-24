//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// What `-g` has to put in the assembly for a debugger to be able to use it.
//
// These are assembly assertions rather than a debugger session: gdb is not a
// build dependency, and the properties below are exactly the ones whose absence
// left a c17-built binary showing symbol names and no source lines.
//

use super::asm_probe::{asm_for_with, body_of, AARCH64_LINUX, X86_64_LINUX};

const DARWIN: &str = "aarch64-apple-darwin";

/// A function's prologue carries a `.loc`, so its entry address has a line.
///
/// `.loc` is emitted per instruction as the blocks are walked, so the first one
/// of a function used to land *after* the prologue -- leaving the function's
/// entry address with no row in the line table at all. `break f` asks for
/// exactly that address, so gdb answered "No line number information available"
/// and gave up on the whole function, though every later address in it was
/// covered. gcc emits the opening line before `.cfi_startproc` for this reason.
#[test]
fn codegen_debug_prologue_has_a_line() {
    let src = "int add(int a, int b)\n{\n\tint c = a + b;\n\treturn c;\n}\n";

    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("debug_prologue_loc", triple, src, &["-g", "-O0"]);
        let body = body_of(&asm, "add");
        let first = body
            .lines()
            .map(str::trim)
            .find(|l| l.starts_with(".loc ") || l.starts_with(".cfi_startproc"))
            .unwrap_or_else(|| panic!("{triple}: neither .loc nor .cfi_startproc in:\n{body}"));
        assert!(
            first.starts_with(".loc "),
            "{triple}: the first thing in the function must be a .loc, so the \
             entry address has a line; found {first:?} in:\n{body}"
        );
    }
}

/// Each function records its size, so a debugger can tell which one owns an
/// address. Without `.size` the symbol carries `st_size = 0`.
#[test]
fn codegen_debug_functions_have_a_size() {
    let src = "int add(int a, int b) { return a + b; }\nint main(void) { return add(1, 2) - 3; }\n";

    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("debug_size", triple, src, &["-g", "-O0"]);
        for f in ["add", "main"] {
            assert!(
                asm.contains(&format!(".size {f}, .-{f}")),
                "{triple}: {f} needs `.size {f}, .-{f}`:\n{asm}"
            );
        }
    }

    // Mach-O has no `.size`; emitting one is an assembler error there.
    let asm = asm_for_with("debug_size_macho", DARWIN, src, &["-g", "-O0"]);
    assert!(
        !asm.contains(".size"),
        "Mach-O has no .size directive:\n{asm}"
    );
}

/// `DW_AT_stmt_list` names *this* unit's line program, not offset zero.
///
/// The assembler builds one line program per object from the `.loc` directives
/// and the linker packs them into `.debug_line` one after another, so only the
/// first object's program sits at offset 0. Emitting a literal 0 made every
/// unit share the first one's line table: linking sparse's 56 units gave source
/// lines for exactly one file and none for the other 55, which reads like
/// missing debug info rather than a misdirected offset.
///
/// A single-unit program cannot show this -- its line program really is at 0 --
/// which is why it survived every small test.
#[test]
fn codegen_debug_stmt_list_is_relocatable() {
    let src = "int f(void) { return 1; }\n";

    for triple in [X86_64_LINUX, AARCH64_LINUX, DARWIN] {
        let asm = asm_for_with("debug_stmt_list", triple, src, &["-g", "-O0"]);
        assert!(
            asm.contains(".long .Ldebug_line0"),
            "{triple}: DW_AT_stmt_list must reference this unit's line program:\n{asm}"
        );
        assert!(
            asm.contains(".Ldebug_line0:"),
            "{triple}: the label DW_AT_stmt_list names must be defined, at the \
             start of this object's .debug_line contribution:\n{asm}"
        );
    }
}

/// `-g` is what turns all of this on; without it none of it appears.
#[test]
fn codegen_debug_directives_need_dash_g() {
    let src = "int f(void) { return 1; }\n";
    let asm = asm_for_with("debug_off", X86_64_LINUX, src, &["-O0"]);
    assert!(!asm.contains(".loc "), "no .loc without -g:\n{asm}");
    assert!(
        !asm.contains(".Ldebug_line0"),
        "no line-program label without -g:\n{asm}"
    );
}
