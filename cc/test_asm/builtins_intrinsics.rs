//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly cases of tests/builtins/intrinsics.rs, in process.
//

use super::asm_probe::asm_symbol;
use crate::test_compile::asm_for;

/// Does `asm` call `name`? Matches `call abs@PLT`, `call _abs` and `bl abs`.
fn calls(asm: &str, name: &str) -> bool {
    let sym = asm_symbol(name);
    asm.lines().any(|l| {
        let mut words = l.split_whitespace();
        matches!(words.next(), Some("call" | "bl" | "jmp" | "b"))
            && words.next().map(|t| t.trim_end_matches("@PLT")) == Some(sym.as_str())
    })
}

/// `abs` and its siblings are computed in place at every level, bare or
/// reserved, as gcc does; a call is what `-fno-builtin[-NAME]` asks for.
/// (The run halves are in tests/builtins/intrinsics.rs.)
#[test]
fn builtins_int_abs_is_expanded_inline() {
    let src = "#include <stdlib.h>\n\
               #include <inttypes.h>\n\
               long f(int a, long b, long long c, intmax_t d) {\n\
                   return abs(a) + labs(b) + llabs(c) + imaxabs(d)\n\
                        + __builtin_abs(a) + __builtin_labs(b);\n\
               }\n";
    for opt in ["-O0", "-O2"] {
        let asm = asm_for("int_abs_asm", src, &[opt]);
        for name in ["abs", "labs", "llabs", "imaxabs"] {
            assert!(!calls(&asm, name), "{opt}: {name} was called:\n{asm}");
        }
    }

    // `-fno-builtin-abs` turns off the bare spelling it names and nothing
    // else; `-fno-builtin` turns off every bare spelling. The reserved
    // spelling is never displaced.
    let asm = asm_for("int_abs_nb_abs", src, &["-fno-builtin-abs"]);
    assert!(
        calls(&asm, "abs"),
        "-fno-builtin-abs kept abs inline:\n{asm}"
    );
    assert!(
        !calls(&asm, "labs"),
        "-fno-builtin-abs displaced labs:\n{asm}"
    );
    let asm = asm_for("int_abs_nb", src, &["-fno-builtin"]);
    for name in ["abs", "labs", "llabs", "imaxabs"] {
        assert!(calls(&asm, name), "-fno-builtin kept {name} inline:\n{asm}");
    }
}

/// A compatible redeclaration of a bare library name -- the one `<stdlib.h>`
/// writes -- keeps the builtin. (The incompatible half, where the builtin
/// must stand aside, runs in tests/builtins/intrinsics.rs.)
#[test]
fn builtins_incompatible_declaration_displaces_the_bare_builtin() {
    let src =
        "int abs(int);\nlong labs(long);\nlong f(int a, long b) { return abs(a) + labs(b); }\n";
    let asm = asm_for("compatible_bare_builtin", src, &[]);
    assert!(!calls(&asm, "abs") && !calls(&asm, "labs"), "{asm}");
}

/// `__builtin_trap` is the target's trap instruction, as gcc emits it --
/// `ud2` on x86-64, `brk #1000` on aarch64 -- never a call to `abort`: glibc
/// uses it on paths that must not depend on libc, and its configure asks
/// for exactly that. It does not return, and unlike `__builtin_unreachable`
/// it is no promise about the program: the branch to it stays.
#[test]
fn builtins_trap_is_a_trap_instruction() {
    use super::asm_probe::{asm_for_with, body_of, AARCH64_LINUX, X86_64_LINUX};
    let src = "int f(int x) { if (x) __builtin_trap(); return 1; }\n\
               void g(void) { __builtin_trap(); }\n";
    for (triple, trap) in [(X86_64_LINUX, "ud2"), (AARCH64_LINUX, "brk #1000")] {
        for opt in ["-O0", "-O2"] {
            let asm = asm_for_with("builtin_trap", triple, src, &[opt]);
            for func in ["f", "g"] {
                let body = body_of(&asm, func);
                assert!(
                    body.lines().any(|l| l.trim() == trap),
                    "{triple} {opt} {func}: no {trap}:\n{body}"
                );
            }
            assert!(!calls(&asm, "abort"), "{triple} {opt}:\n{asm}");
            // `f` still returns 1 when `x` is 0, and `g` never returns.
            let f = body_of(&asm, "f");
            assert!(f.contains("ret"), "{triple} {opt}:\n{f}");
            let g = body_of(&asm, "g");
            assert!(!g.contains("ret"), "{triple} {opt}:\n{g}");
        }
    }
}
