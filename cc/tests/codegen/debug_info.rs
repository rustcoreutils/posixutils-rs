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

/// Functions get a `DW_TAG_subprogram` DIE, with the things inside them as
/// children.
///
/// The abbreviation table used to hold exactly one shape -- a compile unit with
/// no children -- so a c17 binary described its files and nothing inside them.
/// gdb fell back to the ELF symbol table for function names, which is why
/// backtraces named functions while `info args`, `info locals` and `ptype` all
/// came back empty.
#[test]
fn codegen_debug_emits_subprogram_dies() {
    // `sink` takes an address, so `c` and `arr` keep a stack home instead of
    // being promoted into registers -- a variable with no memory to point at
    // gets no location, which is the honest answer but not what this checks.
    let src = r#"
        int sink(int *p);
        int f(int a, int b) {
            int c = a + b;
            int arr[4];
            arr[0] = c;
            sink(&c);
            sink(arr);
            return c;
        }
    "#;

    for (triple, fp) in [(X86_64_LINUX, "rbp"), (AARCH64_LINUX, "x29")] {
        let asm = asm_for_with("debug_subprogram", triple, src, &["-g", "-O0"]);

        // The subprogram tag, and a base type for `int` to point at.
        assert!(
            asm.contains(".uleb128 0x2e") || asm.contains(".uleb128 46"),
            "{triple}: no DW_TAG_subprogram in the abbreviation table:\n{asm}"
        );
        // A function's extent needs an end label to measure against.
        assert!(
            asm.contains(".Lfunc_end_f"),
            "{triple}: DW_AT_high_pc needs a label at the end of the function:\n{asm}"
        );
        // The variables that kept a stack slot are described, by name.
        for name in ["c", "arr"] {
            assert!(
                asm.contains(&format!("\"{name}\"")) || asm.contains(&format!(".asciz \"{name}\"")),
                "{triple}: {name} should appear as a DIE name:\n{asm}"
            );
        }
        let _ = fp;
    }
}

/// A variable's location names the register the code generator actually used.
///
/// The location is built from the same address computation the loads and stores
/// go through, rather than re-derived from the frame layout, so it stays true
/// for a frame whose locals are not addressed from the frame pointer at all.
#[test]
fn codegen_debug_variable_locations_use_the_real_base() {
    let src = r#"
        int sink(int *p);
        int f(void) { int c = 7; sink(&c); return c; }
    "#;

    // The DWARF register number of each target's frame pointer: 6 for %rbp,
    // 29 for x29. DW_OP_breg0 is 0x70, so the opcode byte is 0x70 + N.
    for (triple, op) in [(X86_64_LINUX, 0x70 + 6), (AARCH64_LINUX, 0x70 + 29)] {
        let asm = asm_for_with("debug_var_loc", triple, src, &["-g", "-O0"]);
        assert!(
            asm.contains(&format!(".byte {op}")),
            "{triple}: a local's location should be DW_OP_breg{} ({op}); \
             aarch64 omits x18 from its register enum, so a number taken from \
             the discriminant lands one low and gdb reads the wrong \
             register:\n{asm}",
            op - 0x70
        );
    }
}
