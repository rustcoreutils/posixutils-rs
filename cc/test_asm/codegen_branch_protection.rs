//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-mbranch-protection` on aarch64: `bti` wherever gcc puts one, the return
// address signed and authenticated around every frame that saves it, and
// the GNU property note that tells the linker and loader what the object
// was built for. Every shape is aarch64-linux-gnu-gcc 13's.
//

use super::asm_probe::{
    asm_for_with, assert_body_contains, assert_body_lacks, body_of, count_in_body, AARCH64_LINUX,
};

const PROGRAM: &str = r#"
extern int g(int);
__attribute__((noinline)) static int direct(int x) { return g(x) - 1; }
static int taken(int x) { return g(x) * 3; }
int (*fp)(int) = taken;
int h(int x) { return direct(x) + direct(x + 1); }
int leaf(int x) { return x + 1; }
long jb[40];
extern int _setjmp(long *);
int sj(void) { if (_setjmp(jb)) return 1; g(0); return 0; }
int cg(int i) {
    static void *t[] = { &&a, &&b };
    goto *t[i];
a:  return g(1);
b:  return g(2);
}
void *bsjbuf[5];
int bsj(void) { if (__builtin_setjmp(bsjbuf)) return 1; g(0); return 0; }
void *ra(void) { return __builtin_return_address(0); }
"#;

/// Every function the program defines that has external linkage or whose
/// address is taken.
const INDIRECT: [&str; 7] = ["taken", "h", "leaf", "sj", "cg", "bsj", "ra"];

/// The first instruction of `func`, past its label and directives.
fn first_insn<'a>(asm: &'a str, func: &str) -> &'a str {
    body_of(asm, func)
        .lines()
        .map(str::trim)
        .find(|l| !l.is_empty() && !l.starts_with('.') && !l.ends_with(':'))
        .unwrap_or("")
}

const BTI_C: &str = "hint 34 // bti c";
const BTI_J: &str = "hint 36 // bti j";

/// `bti`: every function an indirect call may reach starts with `bti c` --
/// a static one only called directly has none once optimizing, as in gcc --
/// and an indirect jump lands on `bti j`: the return of a call that returns
/// twice, each label a computed goto reaches, and `__builtin_setjmp`'s
/// resume point. No return address is signed.
#[test]
fn bti_marks_every_landing_pad() {
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with(
            "bp_bti",
            AARCH64_LINUX,
            PROGRAM,
            &[opt, "-mbranch-protection=bti"],
        );
        for func in INDIRECT {
            assert_eq!(first_insn(&asm, func), BTI_C, "{opt} {func}:\n{asm}");
        }
        if opt == "-O0" {
            assert_eq!(first_insn(&asm, "direct"), BTI_C, "-O0:\n{asm}");
        } else {
            assert_body_lacks(&asm, "direct", "bti c", "only called directly");
        }
        assert_eq!(count_in_body(&asm, "sj", BTI_J), 1, "{opt}:\n{asm}");
        assert_eq!(count_in_body(&asm, "cg", BTI_J), 2, "{opt}:\n{asm}");
        assert_eq!(count_in_body(&asm, "bsj", BTI_J), 1, "{opt}:\n{asm}");
        assert_eq!(count_in_body(&asm, "h", "bti"), 1, "{opt}:\n{asm}");
        assert!(!asm.contains("paci"), "{opt}:\n{asm}");
        assert!(!asm.contains("xpaclri"), "{opt}:\n{asm}");
    }
}

/// `standard`: `bti` and `pac-ret` together. Every c17 frame saves the link
/// register, so every function signs it -- gcc's rule, "sign when x30 is
/// saved" -- with `paciasp` first, which is a landing pad for an indirect
/// call too, so no entry needs `bti c`. Each return authenticates it just
/// before the `ret`, and the CFI toggles the return-address state after
/// both, as gcc's `.cfi_window_save` does.
#[test]
fn standard_signs_every_frame() {
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with(
            "bp_std",
            AARCH64_LINUX,
            PROGRAM,
            &[opt, "-mbranch-protection=standard"],
        );
        for func in INDIRECT.iter().chain(&["direct"]) {
            assert_eq!(
                first_insn(&asm, func),
                "hint 25 // paciasp",
                "{opt} {func}:\n{asm}"
            );
            assert_body_contains(
                &asm,
                func,
                "    hint 25 // paciasp\n    .cfi_window_save\n",
                opt,
            );
            assert_body_contains(
                &asm,
                func,
                "    hint 29 // autiasp\n    .cfi_window_save\n    ret\n",
                opt,
            );
            assert_eq!(
                count_in_body(&asm, func, "autiasp"),
                count_in_body(&asm, func, "\n    ret\n"),
                "{opt} {func}: one authentication per return:\n{asm}"
            );
        }
        assert!(!asm.contains("bti c"), "{opt}:\n{asm}");
        assert_eq!(count_in_body(&asm, "sj", BTI_J), 1, "{opt}:\n{asm}");
        assert_eq!(count_in_body(&asm, "cg", BTI_J), 2, "{opt}:\n{asm}");
        // The saved link register is signed, so the address the builtin
        // reads back is stripped of its signature.
        assert_body_contains(&asm, "ra", "hint 7 // xpaclri", opt);
    }
}

/// `pac-ret` alone signs and authenticates, and marks no landing pad;
/// `+b-key` uses the B key and says so to the unwinder first; `+leaf`
/// changes nothing c17 emits, since no c17 frame leaves the link register
/// unsaved.
#[test]
fn pac_ret_variants() {
    let asm = asm_for_with(
        "bp_pac",
        AARCH64_LINUX,
        PROGRAM,
        &["-O2", "-mbranch-protection=pac-ret"],
    );
    assert!(!asm.contains("bti"), "{asm}");
    assert_eq!(first_insn(&asm, "leaf"), "hint 25 // paciasp", "{asm}");
    let leaf = asm_for_with(
        "bp_pac",
        AARCH64_LINUX,
        PROGRAM,
        &["-O2", "-mbranch-protection=pac-ret+leaf"],
    );
    assert_eq!(asm, leaf);

    let asm = asm_for_with(
        "bp_bkey",
        AARCH64_LINUX,
        PROGRAM,
        &["-O2", "-mbranch-protection=pac-ret+b-key+bti"],
    );
    for func in INDIRECT {
        assert_body_contains(
            &asm,
            func,
            "    .cfi_b_key_frame\n    hint 27 // pacibsp\n    .cfi_window_save\n",
            func,
        );
        assert_body_contains(
            &asm,
            func,
            "    hint 31 // autibsp\n    .cfi_window_save\n    ret\n",
            func,
        );
    }
    assert!(
        !asm.contains("paciasp") && !asm.contains("autiasp"),
        "{asm}"
    );
    assert_eq!(count_in_body(&asm, "cg", BTI_J), 2, "{asm}");
}

/// Exception landing pads are reached by the unwinder's indirect jump.
#[test]
fn bti_marks_exception_landing_pads() {
    let src = "extern void g(void);\n\
               static void cl(int *p) { (void)p; g(); }\n\
               int f(void) { int x __attribute__((cleanup(cl))) = 0; g(); return x; }\n";
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with(
            "bp_eh",
            AARCH64_LINUX,
            src,
            &[opt, "-fexceptions", "-mbranch-protection=standard"],
        );
        assert_eq!(count_in_body(&asm, "f", BTI_J), 1, "{opt}:\n{asm}");
    }
}

/// `.note.gnu.property`, with the `GNU_PROPERTY_AARCH64_FEATURE_1_AND` bits
/// of what was asked: BTI is 1, PAC is 2, whichever key. None for `none` or
/// no option, and no instruction of either kind either.
#[test]
fn branch_protection_property_note() {
    let note = |bits: &str| {
        format!(
            "    .section .note.gnu.property,\"a\"\n    .p2align 3\n    .long 1f - 0f\n    \
             .long 4f - 1f\n    .long 5\n0:\n    .string \"GNU\"\n1:\n    .p2align 3\n    \
             .long 0xc0000000\n    .long 3f - 2f\n2:\n    .long {bits}\n3:\n    .p2align 3\n4:\n"
        )
    };
    for (value, bits) in [
        ("bti", "0x1"),
        ("pac-ret", "0x2"),
        ("pac-ret+b-key", "0x2"),
        ("standard", "0x3"),
    ] {
        let flag = format!("-mbranch-protection={value}");
        let asm = asm_for_with("bp_note", AARCH64_LINUX, "int x;\n", &["-O2", &flag]);
        assert!(asm.contains(&note(bits)), "{flag}:\n{asm}");
        assert_eq!(asm.matches(".note.gnu.property").count(), 1, "{flag}");
    }
    for flags in [&["-O2", "-mbranch-protection=none"][..], &["-O2"]] {
        let asm = asm_for_with("bp_no_note", AARCH64_LINUX, PROGRAM, flags);
        assert!(!asm.contains(".note.gnu.property"), "{flags:?}:\n{asm}");
        assert!(!asm.contains("hint"), "{flags:?}:\n{asm}");
        assert!(!asm.contains(".cfi_window_save"), "{flags:?}:\n{asm}");
    }
}
