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

use super::asm_probe::{asm_for_with, AARCH64_DARWIN};
use std::process::{Command, Output};

const X86_64_DARWIN: &str = "x86_64-apple-darwin";

/// Every place c17 makes up a label: block labels behind epilogues and
/// several returns, a computed goto through a static label table, one through
/// a function whose name needs quoting, a switch, string literals and a
/// compound literal referenced from data, wide strings, TLS, a constant pool,
/// `asm goto`, a frame too large for one immediate, a VLA, constructors and
/// destructors. The private-label spellings are checked in process by
/// `cc/test_asm/codegen_macho_labels.rs`; this file assembles it.
const PROGRAM: &str = include_str!("macho_labels.c");

const OPTION_SETS: [&[&str]; 4] = [&["-O0"], &["-O2"], &["-O0", "-g"], &["-O2", "-g"]];

/// Assemble `input` as Mach-O for `arch` (`arm64` or `x86_64`): with
/// `llvm-mc` wherever it is installed, else with the system assembler on a
/// macOS host. `None` when neither exists.
fn assemble_macho(arch: &str, input: &str, output: &str) -> Option<Output> {
    let have_llvm_mc = Command::new("llvm-mc")
        .arg("--version")
        .output()
        .is_ok_and(|o| o.status.success());
    let mut cmd = if have_llvm_mc {
        let mut cmd = Command::new("llvm-mc");
        cmd.arg(format!("-triple={arch}-apple-macos"));
        // What Apple's assembler assumes for arm64; llvm-mc's default CPU
        // lacks the half-precision instructions `_Float16` uses.
        if arch == "arm64" {
            cmd.arg("-mcpu=apple-m1");
        }
        cmd.args(["-filetype=obj", "-o", output, input]);
        cmd
    } else if cfg!(target_os = "macos") {
        let mut cmd = Command::new("as");
        cmd.args(["-arch", arch, "-o", output, input]);
        cmd
    } else {
        return None;
    };
    Some(cmd.output().expect("failed to run the assembler"))
}

/// The Mach-O assembler accepts c17's output for both Darwin targets, at
/// every optimization level, with and without `-g`.
#[test]
fn codegen_macho_output_assembles() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_macho_assemble_")
        .tempdir()
        .expect("failed to create work dir");
    let s = dir.path().join("t.s");
    let o = dir.path().join("t.o");
    let (s_path, o_path) = (s.to_string_lossy(), o.to_string_lossy());
    for (triple, arch) in [(AARCH64_DARWIN, "arm64"), (X86_64_DARWIN, "x86_64")] {
        for opts in OPTION_SETS {
            let asm = asm_for_with("macho_assemble", triple, PROGRAM, opts);
            std::fs::write(&s, &asm).expect("failed to write assembly");
            let Some(out) = assemble_macho(arch, &s_path, &o_path) else {
                eprintln!("SKIP codegen_macho_output_assembles: no llvm-mc and not a macOS host");
                return;
            };
            assert!(
                out.status.success(),
                "{triple} {opts:?}: the Mach-O assembler rejected c17's output:\n{}\n{asm}",
                String::from_utf8_lossy(&out.stderr)
            );
        }
    }
}
