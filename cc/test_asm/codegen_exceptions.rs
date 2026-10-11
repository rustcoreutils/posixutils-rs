//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-fexceptions`: the personality routine, the LSDA and the landing pads gcc
// gives a function whose calls lie in the scope of a `cleanup` variable, and
// nothing for a function that needs none.
//

use super::asm_probe::{
    asm_for_with, assert_body_contains, assert_body_lacks, body_of, AARCH64_DARWIN, AARCH64_LINUX,
    X86_64_LINUX,
};

const PROGRAM: &str = r#"
extern void g(int);
extern void h(int) __attribute__((nothrow));
static void cl(int *p) { g(*p); }
int f(int x) {
    int a __attribute__((cleanup(cl))) = x;
    g(1);
    {
        int b __attribute__((cleanup(cl))) = x + 1;
        g(2);
        h(3);
    }
    g(4);
    return a;
}
int nothrow_only(int x) {
    int a __attribute__((cleanup(cl))) = x;
    h(1);
    return 0;
}
int plain(int x) { g(x); return 1; }
"#;

/// The lines of the LSDA function `name` names with `.cfi_lsda`, up to the
/// next label.
fn lsda_of(asm: &str, name: &str) -> Vec<String> {
    let body = body_of(asm, name);
    let label = body
        .lines()
        .find_map(|l| l.trim().strip_prefix(".cfi_lsda "))
        .and_then(|l| l.split_once(','))
        .map(|(_, label)| format!("{label}:"))
        .unwrap_or_else(|| panic!("{name} names no LSDA:\n{body}"));
    asm.lines()
        .map(str::trim)
        .skip_while(|l| *l != label)
        .skip(1)
        .take_while(|l| !l.ends_with(':') || l.contains("CSB"))
        .map(str::to_string)
        .collect()
}

/// A function with throwing calls in a cleanup's scope names gcc's
/// personality routine through the hidden `DW.ref` pointer and its LSDA, and
/// its landing pads end in `_Unwind_Resume`. The LSDA, in
/// `.gcc_except_table`, has gcc's header -- no landing-pad base, no type
/// table, uleb128 call sites -- and one entry per throwing call in a
/// cleanup's scope: four `.uleb128`s, the last the cleanup action 0. A
/// function whose cleanup scopes hold only `nothrow` calls, or that has no
/// cleanup at all, gets neither.
#[test]
fn fexceptions_lsda_and_personality_shape() {
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        for level in ["-O0", "-O2"] {
            let asm = asm_for_with("eh_shape", triple, PROGRAM, &[level, "-fexceptions"]);
            let why = format!("{triple} {level}");
            assert_body_contains(
                &asm,
                "f",
                ".cfi_personality 0x9b,DW.ref.__gcc_personality_v0",
                &why,
            );
            assert_body_contains(&asm, "f", ".cfi_lsda 0x1b,", &why);
            assert_body_contains(&asm, "f", "_Unwind_Resume", &why);
            assert!(
                asm.contains(".section .gcc_except_table,\"a\",@progbits"),
                "{why}: no exception table section:\n{asm}"
            );
            let lsda = lsda_of(&asm, "f");
            assert_eq!(lsda[..3], [".byte 255", ".byte 255", ".byte 1"], "{why}");
            let entries: Vec<&String> = lsda.iter().skip(5).collect();
            assert!(
                !entries.is_empty() && entries.len().is_multiple_of(4),
                "{why}: call sites:\n{lsda:#?}"
            );
            for entry in entries.chunks(4) {
                assert!(
                    entry.iter().all(|l| l.starts_with(".uleb128 ")),
                    "{why}: {entry:?}"
                );
                assert_eq!(entry[3], ".uleb128 0", "{why}: {entry:?}");
            }
            for quiet in ["nothrow_only", "plain"] {
                assert_body_lacks(&asm, quiet, ".cfi_personality", &why);
                assert_body_lacks(&asm, quiet, ".cfi_lsda", &why);
                assert_body_contains(&asm, quiet, ".cfi_startproc", &why);
            }
            assert_eq!(asm.matches(".cfi_lsda").count(), 1, "{why}");
            for line in [
                ".hidden DW.ref.__gcc_personality_v0",
                ".weak DW.ref.__gcc_personality_v0",
                ".section .data.rel.local.DW.ref.__gcc_personality_v0,\"awG\",@progbits,DW.ref.__gcc_personality_v0,comdat",
                "DW.ref.__gcc_personality_v0:",
                ".quad __gcc_personality_v0",
            ] {
                assert!(asm.contains(line), "{why}: no `{line}` in:\n{asm}");
            }
        }
    }
}

/// Non-PIC code names the personality routine and the LSDA directly, as
/// gcc's `-fno-pic` does, and needs no `DW.ref` pointer.
#[test]
fn fexceptions_without_pic_names_the_personality_directly() {
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("eh_nopic", triple, PROGRAM, &["-fexceptions", "-fno-pic"]);
        assert_body_contains(
            &asm,
            "f",
            ".cfi_personality 0x3,__gcc_personality_v0",
            triple,
        );
        assert_body_contains(&asm, "f", ".cfi_lsda 0x3,", triple);
        assert!(!asm.contains("DW.ref"), "{triple}:\n{asm}");
    }
}

/// Mach-O names `___gcc_personality_v0` itself, the assembler making the
/// GOT indirection, and the LSDA PC-relative, as clang does; the table goes
/// in `__TEXT,__gcc_except_tab`.
#[test]
fn fexceptions_on_darwin_names_the_personality_as_clang_does() {
    for triple in [AARCH64_DARWIN, "x86_64-apple-darwin"] {
        let asm = asm_for_with("eh_darwin", triple, PROGRAM, &["-fexceptions"]);
        assert_body_contains(
            &asm,
            "f",
            ".cfi_personality 0x9b,___gcc_personality_v0",
            triple,
        );
        assert_body_contains(&asm, "f", ".cfi_lsda 0x10,", triple);
        assert_body_contains(&asm, "f", "__Unwind_Resume", triple);
        assert!(
            asm.contains(".section __TEXT,__gcc_except_tab"),
            "{triple}:\n{asm}"
        );
        assert_eq!(
            lsda_of(&asm, "f")[..3],
            [".byte 255", ".byte 255", ".byte 1"]
        );
        assert!(!asm.contains("DW.ref"), "{triple}:\n{asm}");
    }
}

/// Without `-fexceptions` -- C's default -- nothing changes: no personality,
/// no LSDA, no landing pad.
#[test]
fn no_fexceptions_emits_no_eh_tables() {
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        for extra in [
            &[][..],
            &["-fno-exceptions"][..],
            &["-fexceptions", "-fno-exceptions"][..],
        ] {
            let asm = asm_for_with("eh_off", triple, PROGRAM, extra);
            for needle in [
                ".cfi_personality",
                ".cfi_lsda",
                ".gcc_except_table",
                "_Unwind_Resume",
            ] {
                assert!(
                    !asm.contains(needle),
                    "{triple} {extra:?}: `{needle}` in:\n{asm}"
                );
            }
        }
    }
}

/// A call that returns twice and may throw, in a cleanup's scope, under
/// branch protection: the `endbr64` its second return lands on follows the
/// call and, as in gcc, lies inside the call's call-site entry.
#[test]
fn returns_twice_call_with_landing_pad_keeps_endbr64_in_its_entry() {
    let src = "extern int sj(void *) __attribute__((returns_twice));\n\
               extern void g(int);\n\
               static void cl(int *p) { g(*p); }\n\
               void *b[8];\n\
               int f(int x) {\n\
                   int a __attribute__((cleanup(cl))) = x;\n\
                   if (sj(b)) return 1;\n\
                   g(2);\n\
                   return 0;\n\
               }\n";
    for level in ["-O0", "-O2"] {
        let asm = asm_for_with(
            "eh_rt",
            X86_64_LINUX,
            src,
            &[level, "-fexceptions", "-fcf-protection=full"],
        );
        let body: Vec<&str> = body_of(&asm, "f").lines().map(str::trim).collect();
        let call = body
            .iter()
            .position(|l| l.starts_with("call sj"))
            .unwrap_or_else(|| panic!("{level}: no call to sj:\n{asm}"));
        assert!(body[call - 1].starts_with(".L.LEHB."), "{level}: {body:?}");
        assert_eq!(body[call + 1], "endbr64", "{level}: {body:?}");
        assert!(body[call + 2].starts_with(".L.LEHE."), "{level}: {body:?}");
    }
}

/// `nothrow` is the declaration's, as gcc has it: a call naming the function
/// -- through `&` or `*` too -- needs no landing pad, and a call of a
/// function declared through `__typeof__` of it, or through a pointer to it,
/// does. gcc ignores the attribute on a pointer declarator, with a warning.
#[test]
fn nothrow_is_the_declarations() {
    let decls = r#"
static void cl(int *p) { (void)p; }
extern int nt(void) __attribute__((nothrow));
__typeof__(nt) nt2;
__typeof__(&nt) pnt;
int (*p7)(void) __attribute__((nothrow));
"#;
    for (call, nothrow) in [
        ("nt()", true),
        ("(&nt)()", true),
        ("(*nt)()", true),
        ("nt2()", false),
        ("pnt()", false),
        ("p7()", false),
        ("({ __auto_type q = nt; q(); })", false),
    ] {
        let src = format!(
            "{decls}int g(void) {{ int a __attribute__((cleanup(cl))) = 0; return {call}; }}\n"
        );
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with("eh_nothrow_decl", triple, &src, &["-O0", "-fexceptions"]);
            let why = format!("{call} on {triple}");
            if nothrow {
                assert_body_lacks(&asm, "g", ".cfi_lsda", &why);
            } else {
                assert_body_contains(&asm, "g", ".cfi_lsda", &why);
            }
        }
    }
}

/// An arm64 Darwin variadic call whose argument area must be aligned past
/// sixteen realigns `sp` by an amount only known at run time, so no
/// `.cfi_GNU_args_size` can give it back: the unwinder would hand the pad a
/// stack pointer somewhere inside the realigned area. The call says 0, and
/// its call-site entry lands on a detour that reloads `sp` from where the
/// call saved it -- as the call's own return path does -- and then jumps to
/// the pad. A call with an ordinary area keeps its constant.
#[test]
fn fexceptions_realigned_darwin_variadic_call_lands_on_a_detour() {
    let src = r#"
typedef long al32 __attribute__((aligned(32)));
extern void g(int, ...);
extern void h(int);
static void cl(int *p) { h(*p); }
int f(int x) {
    int a __attribute__((cleanup(cl))) = x;
    al32 v = x;
    g(1, v);
    g(2, 3L, 4L, 5L);
    return a;
}
"#;
    for level in ["-O0", "-O2"] {
        let asm = asm_for_with(
            "eh_darwin_realign",
            AARCH64_DARWIN,
            src,
            &[level, "-fexceptions"],
        );
        let why = format!("{level}:\n{asm}");
        let body = body_of(&asm, "f");
        let lines: Vec<&str> = body.lines().map(str::trim).collect();
        // The realigning call saves the old `sp` and reads it back after.
        let saved = lines
            .iter()
            .find_map(|l| l.strip_prefix("str x16, [sp, #"))
            .and_then(|l| l.strip_suffix(']'))
            .unwrap_or_else(|| panic!("no saved sp: {why}"));
        let detour = lines
            .iter()
            .position(|l| l.starts_with("L.LEHD.") && l.ends_with(':'))
            .unwrap_or_else(|| panic!("no detour: {why}"));
        assert_eq!(
            lines[detour + 1],
            format!("ldr x16, [sp, #{saved}]"),
            "{why}"
        );
        assert_eq!(lines[detour + 2], "add sp, x16, #0", "{why}");
        assert!(lines[detour + 3].starts_with("b Lf_"), "{why}");
        // The LSDA names the detour as that call's pad.
        let label = lines[detour].trim_end_matches(':');
        assert!(
            lsda_of(&asm, "f")
                .iter()
                .any(|l| l == &format!(".uleb128 {label}-L.LFB.0")),
            "{why}"
        );
        // The realigning call, first, leaves the size at 0; the other's
        // three stacked longs keep their constant, 32 bytes.
        let first_call = lines
            .iter()
            .position(|l| l.starts_with("L.LEHB."))
            .unwrap_or_else(|| panic!("no call site: {why}"));
        assert!(
            !lines[..first_call]
                .iter()
                .any(|l| l.starts_with(".cfi_GNU_args_size")),
            "{why}"
        );
        assert!(lines.contains(&".cfi_GNU_args_size 32"), "{why}");
    }
}
