//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembler-private labels on Mach-O.
//
// ELF assemblers keep `.L` names out of the symbol table; Mach-O's keeps only
// `L` names out. A `.L` block label on Mach-O is an ordinary symbol, which
// starts a new atom in `__text`, so a CFI advance across it is no longer a
// constant and Apple's assembler rejects the file ("invalid CFI advance_loc
// expression") as soon as an epilogue's CFI follows a block label.
//

use super::asm_probe::{asm_for_with, AARCH64_DARWIN, AARCH64_LINUX, X86_64_LINUX};

const X86_64_DARWIN: &str = "x86_64-apple-darwin";

/// Every place c17 makes up a label: block labels behind epilogues and
/// several returns, a computed goto through a static label table, one through
/// a function whose name needs quoting, a switch, string literals and a
/// compound literal referenced from data, wide strings, TLS, a constant pool,
/// `asm goto`, a frame too large for one immediate, a VLA, constructors and
/// destructors. Shared with `tests/codegen/macho_labels.rs`, which assembles
/// it.
const PROGRAM: &str = include_str!("../tests/codegen/macho_labels.c");

const OPTION_SETS: [&[&str]; 4] = [&["-O0"], &["-O2"], &["-O0", "-g"], &["-O2", "-g"]];

/// Lines of `asm` that define or reference a name spelled `.L…`.
fn dot_l_mentions(asm: &str) -> Vec<&str> {
    asm.lines()
        .filter(|line| {
            line.match_indices(".L").any(|(at, _)| {
                at == 0
                    || !matches!(line.as_bytes()[at - 1],
                        b'_' | b'.' | b'$' | b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z')
            })
        })
        .collect()
}

/// No `.L` name reaches Mach-O output: each one is spelled `L…`, which is
/// what that assembler treats as private. ELF output keeps `.L`.
#[test]
fn codegen_macho_private_labels_use_the_l_prefix() {
    for triple in [AARCH64_DARWIN, X86_64_DARWIN] {
        for opts in OPTION_SETS {
            let asm = asm_for_with("macho_labels_text", triple, PROGRAM, opts);
            let leaked = dot_l_mentions(&asm);
            assert!(
                leaked.is_empty(),
                "{triple} {opts:?}: `.L` names are ordinary symbols on Mach-O:\n{}",
                leaked.join("\n")
            );
            for label in ["\nLepilogue_", "\nLC0:", "\nLdispatch_", "\nL.sjlj_resume."] {
                assert!(
                    asm.contains(label),
                    "{triple} {opts:?}: expected a private label `{}`:\n{asm}",
                    label.trim()
                );
            }
        }
    }
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("elf_labels_text", triple, PROGRAM, &["-O2", "-g"]);
        for label in [
            "\n.Lepilogue_",
            "\n.LC0:",
            "\n.Ldispatch_",
            "\n.Ldebug_line0:",
            "\n.L.sjlj_resume.",
        ] {
            assert!(
                asm.contains(label),
                "{triple}: expected an ELF private label `{}`:\n{asm}",
                label.trim()
            );
        }
    }
}
