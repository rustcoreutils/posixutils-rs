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
use std::process::{Command, Output};

const X86_64_DARWIN: &str = "x86_64-apple-darwin";

/// Every place c17 makes up a label: block labels behind epilogues and
/// several returns, a computed goto through a static label table, one through
/// a function whose name needs quoting, a switch, string literals and a
/// compound literal referenced from data, wide strings, TLS, a constant pool,
/// `asm goto`, a frame too large for one immediate, a VLA, constructors and
/// destructors.
const PROGRAM: &str = r#"
int g(int);
static const char *names[] = { "zero", "one", "two" };
const char *greeting = "hello";
int *file_cl = (int[]){ 4, 5, 6 };
const int *wide = (const int *)L"wide";
_Thread_local int tls_counter;
double scale(double x) { return x * 1.25 + 0.5; }
int epilogue(int x) { if (x) return g(x) + 1; return 3; }
int many_returns(int x) {
    for (int i = 0; i < x; i++) {
        if (g(i) == 7) return i;
        if (g(i) < 0) return -i;
    }
    return x > 3 ? 1 : 2;
}
int dispatch(int op) {
    static void *table[] = { &&add, &&sub, &&done };
    int acc = 0;
    goto *table[op % 3];
add: acc += 2; goto done;
sub: acc -= 2;
done: return acc;
}
int müller(int x) {
    void *targets[] = { &&lab0, &&lab1 };
    goto *targets[x & 1];
lab0: return 1;
lab1: return 2;
}
int sw(int v) {
    switch (v) {
    case 0: return g(10); case 1: return g(11); case 2: return g(12);
    case 3: return g(13); case 4: return g(14); case 5: return g(15);
    case 6: return g(16); case 7: return g(17); default: return -1;
    }
}
int jumped(int x) {
#if defined(__aarch64__)
    __asm__ goto ("cbz %w0, %l[out]" : : "r"(x) : : out);
#else
    __asm__ goto ("testl %0, %0; jz %l[out]" : : "r"(x) : : out);
#endif
    return 1;
out:
    return 0;
}
int big_frame(int i) {
    volatile char buf[70000];
    buf[i] = (char)i;
    return buf[69999 - i] + g(i);
}
int vla(int n) {
    int a[n];
    for (int i = 0; i < n; i++) a[i] = g(i);
    return n ? a[n - 1] : 0;
}
int bump(void) { return ++tls_counter; }
const char *name_of(int i) { return names[i % 3]; }
__attribute__((constructor)) static void init(void) { tls_counter = 1; }
__attribute__((destructor)) static void fini(void) { tls_counter = 0; }
long double ld(long double x) { return x * 3.0L; }
"#;

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
            for label in ["\nLepilogue_", "\nLC0:", "\nLdispatch_"] {
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
        ] {
            assert!(
                asm.contains(label),
                "{triple}: expected an ELF private label `{}`:\n{asm}",
                label.trim()
            );
        }
    }
}

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
