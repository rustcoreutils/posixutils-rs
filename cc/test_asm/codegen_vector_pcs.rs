//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Definitions of `aarch64_vector_pcs` functions, in the assembly: the
// symbol marked `.variant_pcs`, as gcc 13 marks it, and q8-q23 saved whole
// wherever the body may change them. The run-time half, against a caller
// that checks every bit of those registers, is `tests/codegen/vector_pcs.rs`.
//

use super::asm_probe::{asm_for_with, body_of, AARCH64_DARWIN, AARCH64_LINUX, X86_64_LINUX};

const SRC: &str = r#"
typedef float v4sf __attribute__((vector_size(16)));
v4sf base_fn(v4sf);
__attribute__((aarch64_vector_pcs)) v4sf leaf(v4sf a, v4sf b) { return a * b + a; }
__attribute__((aarch64_vector_pcs)) v4sf calls(v4sf a, v4sf b) {
    v4sf x = base_fn(a);
    return x * b + a;
}
__attribute__((aarch64_vector_pcs)) double clob(double a) {
    __asm__ volatile("" ::: "v8", "v9", "v20", "v23", "v24");
    return a;
}
static __attribute__((aarch64_vector_pcs)) v4sf local_vpcs(v4sf a) { return a + a; }
typedef __attribute__((aarch64_vector_pcs)) v4sf vfn(v4sf);
vfn *take(void) { return local_vpcs; }
double base_clob(double a) {
    __asm__ volatile("" ::: "v8", "v9", "v20", "v23", "v24");
    return a;
}
"#;

/// The q registers past the argument ones (q8 up) function `func` stores
/// whole in its prologue -- up to its first block's label -- from its
/// `stp qN, qM` and `str qN`.
fn q_saves(asm: &str, func: &str) -> Vec<u32> {
    let mut regs = Vec::new();
    let prologue = body_of(asm, func)
        .lines()
        .map(str::trim)
        .skip_while(|l| *l != format!("{func}:"))
        .skip(1)
        .take_while(|l| !l.ends_with(':'));
    for line in prologue {
        let Some(rest) = line
            .strip_prefix("stp ")
            .or_else(|| line.strip_prefix("str "))
        else {
            continue;
        };
        for operand in rest.split(", ") {
            match operand.strip_prefix('q').and_then(|n| n.parse().ok()) {
                Some(n) if n >= 8 => regs.push(n),
                _ => {}
            }
        }
    }
    regs
}

/// Each vector-PCS definition, local ones too, is marked `.variant_pcs` on
/// ELF, which tells the linker and the dynamic loader that a call to it may
/// not go through anything that keeps only the base standard's registers;
/// a base function is not marked.
#[test]
fn vector_pcs_definition_is_marked_variant_pcs() {
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with("vpcs_mark", AARCH64_LINUX, SRC, &[opt]);
        for func in ["leaf", "calls", "clob", "local_vpcs"] {
            assert!(
                asm.lines()
                    .any(|l| l.trim() == format!(".variant_pcs {func}")),
                "{opt}: {func} is not marked:\n{asm}"
            );
        }
        for func in ["take", "base_clob"] {
            assert!(
                !asm.lines()
                    .any(|l| l.trim() == format!(".variant_pcs {func}")),
                "{opt}: base function {func} is marked:\n{asm}"
            );
        }
    }
    // Mach-O has no such directive, and Apple's assembler none by that name.
    let asm = asm_for_with("vpcs_mark_darwin", AARCH64_DARWIN, SRC, &["-O2"]);
    assert!(!asm.contains(".variant_pcs"), "{asm}");
}

/// A function that calls a base-PCS one saves all of q8-q23 whole, as
/// gcc's does: the callee may change v16-v23 and the top half of v8-v15.
/// One that changes some of them in an asm statement saves those, whole;
/// a base function with the same statement saves only d8 and d9, and
/// nothing of v20, v23 or v24.
#[test]
fn vector_pcs_definition_saves_q8_to_q23_whole() {
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with("vpcs_saves", AARCH64_LINUX, SRC, &[opt]);
        let calls = q_saves(&asm, "calls");
        assert_eq!(
            calls,
            (8..=23).collect::<Vec<_>>(),
            "{opt}: calls\n{}",
            body_of(&asm, "calls")
        );
        let clob = q_saves(&asm, "clob");
        for n in [8, 9, 20, 23] {
            assert!(
                clob.contains(&n),
                "{opt}: clob does not save q{n}:\n{}",
                body_of(&asm, "clob")
            );
        }
        assert!(
            !clob.contains(&24) && clob.iter().all(|n| (8..=23).contains(n)),
            "{opt}: clob saves {clob:?}"
        );
        // Every saved register is restored whole too, and described to the
        // unwinder.
        let body = body_of(&asm, "clob");
        for n in [8, 9, 20, 23] {
            assert!(
                body.lines().any(|l| {
                    let l = l.trim();
                    (l.starts_with("ldp ") || l.starts_with("ldr "))
                        && l.split(", ")
                            .any(|o| o == format!("q{n}") || o.ends_with(&format!(" q{n}")))
                }),
                "{opt}: clob does not restore q{n}:\n{body}"
            );
            assert!(
                body.contains(&format!(".cfi_offset d{n},")),
                "{opt}: no CFI for v{n}:\n{body}"
            );
        }
        let base = body_of(&asm, "base_clob");
        assert!(q_saves(&asm, "base_clob").is_empty(), "{opt}: {base}");
        assert!(base.contains("stp d8, d9"), "{opt}: {base}");
        assert!(
            !base.contains("d20") && !base.contains("d24"),
            "{opt}: {base}"
        );
    }
}

/// On x86-64 the attribute is ignored with gcc's warning, so a definition
/// with it is an ordinary function.
#[test]
fn vector_pcs_definition_on_x86_64_is_ordinary() {
    let asm = asm_for_with(
        "vpcs_x86",
        X86_64_LINUX,
        "__attribute__((aarch64_vector_pcs)) double f(double a) { return a + 1; }\n",
        &["-O2"],
    );
    assert!(!asm.contains(".variant_pcs"), "{asm}");
    assert!(body_of(&asm, "f").contains("ret"), "{asm}");
}
