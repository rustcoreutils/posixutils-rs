//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-fcf-protection` on x86-64: `endbr64` wherever gcc puts one, and the
// GNU property note that tells the linker and loader what the object was
// built for.
//

use super::asm_probe::{
    asm_for_with, assert_body_contains, assert_body_lacks, body_of, count_in_body, X86_64_LINUX,
};

const PROGRAM: &str = r#"
extern int g(int);
__attribute__((noinline)) static int direct(int x) { return g(x) - 1; }
static int taken(int x) { return g(x) * 3; }
static int returned(int x) { return g(x) * 5; }
int (*fp)(int) = taken;
int (*getp(void))(int) { return returned; }
int h(int x) { return direct(x) + direct(x + 1); }
__attribute__((constructor)) static void ctor(void) { g(9); }
__attribute__((used)) static void kept(void) { g(10); }
static int aliased(int x) { return g(x) + 3; }
int alias_of(int) __attribute__((alias("aliased")));
__attribute__((visibility("hidden"))) int hid(int x) { return g(x) + 2; }
long jb[25];
extern int _setjmp(long *);
int sj(void) { if (_setjmp(jb)) return 1; g(0); return 0; }
extern int vfork(void);
int vf(void) { return vfork() + g(0); }
int cg(int i) {
    static void *t[] = { &&a, &&b };
    goto *t[i];
a:  return g(1);
b:  return g(2);
}
int main(void) { return h(1); }
"#;

/// Whether `func` starts with `endbr64`: the first instruction after its
/// label and `.cfi_startproc`.
fn starts_with_endbr(asm: &str, func: &str) -> bool {
    body_of(asm, func)
        .lines()
        .map(str::trim)
        .find(|l| !l.is_empty() && !l.starts_with('.') && !l.ends_with(':'))
        == Some("endbr64")
}

/// Under branch protection every function that can be reached indirectly
/// starts with `endbr64`: every one with external linkage, and every static
/// one whose address is taken, that runs from `.init_array`, that `used`
/// keeps, or that an alias names. A static function only ever called directly
/// has none -- when optimizing. At -O0 gcc gives every function one, and so
/// does c17.
#[test]
fn cf_branch_marks_every_indirect_entry() {
    let indirect = [
        "taken", "returned", "getp", "h", "ctor", "kept", "aliased", "hid", "sj", "vf", "cg",
        "main",
    ];
    for level in ["branch", "full"] {
        let flag = format!("-fcf-protection={level}");
        for opt in ["-O1", "-O2"] {
            let asm = asm_for_with("cf_entry", X86_64_LINUX, PROGRAM, &[opt, &flag]);
            for func in indirect {
                assert!(starts_with_endbr(&asm, func), "{flag} {opt} {func}:\n{asm}");
            }
            assert_body_lacks(&asm, "direct", "endbr64", "only called directly");
        }
        let asm = asm_for_with("cf_entry_o0", X86_64_LINUX, PROGRAM, &["-O0", &flag]);
        for func in indirect.iter().chain(&["direct"]) {
            assert!(starts_with_endbr(&asm, func), "{flag} -O0 {func}:\n{asm}");
        }
    }
}

/// A function that returns twice comes back by an indirect jump, so the call
/// is followed by `endbr64`: `setjmp` (through glibc's `_setjmp`) and the rest
/// of gcc's list of such names. A label whose address is taken is the target
/// of a computed goto and starts with one too.
#[test]
fn cf_branch_marks_returns_twice_and_label_targets() {
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with(
            "cf_twice",
            X86_64_LINUX,
            PROGRAM,
            &[opt, "-fcf-protection=branch"],
        );
        assert_body_contains(&asm, "sj", "setjmp@PLT\n    endbr64\n", opt);
        assert_body_contains(&asm, "vf", "call vfork@PLT\n    endbr64\n", opt);
        // The entry and each of the two labels.
        assert_eq!(count_in_body(&asm, "cg", "endbr64"), 3, "{opt}:\n{asm}");
        // An ordinary call is not a landing pad.
        assert_eq!(count_in_body(&asm, "h", "endbr64"), 1, "{opt}:\n{asm}");
    }
}

/// Return protection alone changes no code: the shadow stack needs nothing
/// of the program but the note.
#[test]
fn cf_return_adds_no_landing_pads() {
    let asm = asm_for_with(
        "cf_return",
        X86_64_LINUX,
        PROGRAM,
        &["-O2", "-fcf-protection=return"],
    );
    assert!(!asm.contains("endbr64"), "{asm}");
}

/// `.note.gnu.property`, in gcc's exact text, with the
/// `GNU_PROPERTY_X86_FEATURE_1_AND` bits of the level: IBT is 1, SHSTK is 2.
/// None at all for `none`, `check` or no option, and the instructions are
/// gone with it.
#[test]
fn cf_property_note() {
    let note = |bits: &str| {
        format!(
            "    .section .note.gnu.property,\"a\"\n    .align 8\n    .long 1f - 0f\n    \
             .long 4f - 1f\n    .long 5\n0:\n    .string \"GNU\"\n1:\n    .align 8\n    \
             .long 0xc0000002\n    .long 3f - 2f\n2:\n    .long {bits}\n3:\n    .align 8\n4:\n"
        )
    };
    for (level, bits) in [("full", "0x3"), ("branch", "0x1"), ("return", "0x2")] {
        let flag = format!("-fcf-protection={level}");
        let asm = asm_for_with("cf_note", X86_64_LINUX, "int x;\n", &["-O2", &flag]);
        assert!(asm.contains(&note(bits)), "{flag}:\n{asm}");
        assert_eq!(asm.matches(".note.gnu.property").count(), 1, "{flag}");
    }
    for flags in [
        &["-O2", "-fcf-protection=none"][..],
        &["-O2", "-fcf-protection=check"],
        &["-O2"],
    ] {
        let asm = asm_for_with("cf_no_note", X86_64_LINUX, PROGRAM, flags);
        assert!(!asm.contains(".note.gnu.property"), "{flags:?}:\n{asm}");
        assert!(!asm.contains("endbr64"), "{flags:?}:\n{asm}");
    }
}

/// With `-pg` the `endbr64` still comes first, ahead of the prologue and the
/// profiling call, as in gcc; `-mfentry`'s call follows it directly.
#[test]
fn cf_branch_endbr_precedes_the_profiling_call() {
    let src = "extern int g(int);\nint f(int x) { return g(x) + x; }\n";
    let asm = asm_for_with(
        "cf_pg",
        X86_64_LINUX,
        src,
        &["-O2", "-fno-pic", "-pg", "-fcf-protection=branch"],
    );
    assert!(starts_with_endbr(&asm, "f"), "{asm}");
    let asm = asm_for_with(
        "cf_fentry",
        X86_64_LINUX,
        src,
        &["-O2", "-fno-pic", "-pg", "-mfentry", "-fcf-protection=full"],
    );
    assert_body_contains(
        &asm,
        "f",
        "    endbr64\n1:\n    call __fentry__\n",
        "-mfentry",
    );
}
