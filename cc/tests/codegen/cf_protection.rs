//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-fcf-protection` objects: what the linker and loader read of them, and
// that the program still runs. CET is not necessarily enforced on the host,
// so the landing pads are checked in the disassembly.
//

use crate::common::{run_c17, work_dir};
use std::process::Command;

/// Indirect calls, a computed goto and a setjmp that comes back: each lands
/// where branch protection needs an `endbr64`.
const PROGRAM: &str = r#"
#include <setjmp.h>
static int twice(int x) { return 2 * x; }
int (*volatile fp)(int) = twice;
static jmp_buf jb;
__attribute__((noinline)) static void jump(void) { longjmp(jb, 7); }
__attribute__((noinline)) int pick(int i)
{
    static void *t[] = { &&a, &&b };
    goto *t[i];
a:  return 10;
b:  return 20;
}
int main(void)
{
    if (fp(21) != 42) return 1;
    if (pick(0) + pick(1) != 30) return 2;
    int r = setjmp(jb);
    if (r == 0) jump();
    return r == 7 ? 0 : 3;
}
"#;

fn tool(name: &str, args: &[&str]) -> String {
    let out = Command::new(name)
        .args(args)
        .output()
        .unwrap_or_else(|e| panic!("run {name}: {e}"));
    assert!(out.status.success(), "{name} {args:?} failed");
    String::from_utf8_lossy(&out.stdout).into_owned()
}

/// Each level's object carries gcc's property note, which readelf decodes
/// to the protections asked for, and runs; `none` has no note. Under branch
/// protection `main`, the indirectly called `twice` and both labels of the
/// computed goto start with `endbr64`, and so does the return from `setjmp`.
#[test]
fn cf_protection_object_note_and_landing_pads() {
    let dir = work_dir("cet");
    let src = dir.path().join("cet.c");
    std::fs::write(&src, PROGRAM).unwrap();
    for (level, feature) in [
        ("full", Some("IBT, SHSTK")),
        ("branch", Some("IBT")),
        ("return", Some("SHSTK")),
        ("none", None),
    ] {
        for opt in ["-O0", "-O2"] {
            let flag = format!("-fcf-protection={level}");
            let obj = dir.path().join("cet.o");
            let exe = dir.path().join("cet");
            let (o, s, e) = (
                obj.to_str().unwrap(),
                src.to_str().unwrap(),
                exe.to_str().unwrap(),
            );
            let run = run_c17(&[opt, &flag, "-c", "-o", o, s]);
            assert!(run.success, "{flag} {opt}: {}", run.stderr);
            let notes = tool("readelf", &["-n", o]);
            match feature {
                Some(f) => assert!(
                    notes.contains(&format!("x86 feature: {f}\n")),
                    "{flag}:\n{notes}"
                ),
                None => assert!(!notes.contains("x86 feature"), "{flag}:\n{notes}"),
            }

            let dis = tool("objdump", &["-d", "--no-show-raw-insn", o]);
            let endbr_after = |label: &str| {
                let at = dis
                    .find(label)
                    .unwrap_or_else(|| panic!("no {label}:\n{dis}"));
                dis[at..].lines().nth(1).unwrap_or("").contains("endbr64")
            };
            let branch = level == "full" || level == "branch";
            for func in ["<main>:", "<twice>:"] {
                assert_eq!(endbr_after(func), branch, "{flag} {opt} {func}:\n{dis}");
            }
            let pads = dis.matches("endbr64").count();
            // main, twice, pick (external), jump and the two labels at -O0;
            // `jump` is only called directly, so above -O0 it has none. And
            // the instruction after the call of setjmp.
            let expected = match (branch, opt) {
                (false, _) => 0,
                (true, "-O0") => 7,
                (true, _) => 6,
            };
            assert_eq!(pads, expected, "{flag} {opt}:\n{dis}");

            let run = run_c17(&[opt, &flag, "-o", e, s]);
            assert!(run.success, "{flag} {opt}: {}", run.stderr);
            let status = Command::new(&exe).status().expect("run");
            assert_eq!(status.code(), Some(0), "{flag} {opt}");
        }
    }
}

/// gcc's `<cet.h>`: in assembly, `_CET_ENDBR` is `endbr64` under branch
/// protection and nothing otherwise, and including it gives the object the
/// property note for the `__CET__` level; in C it defines nothing. libffi's
/// and guile's sources include it once `__CET__` is defined.
#[test]
fn cf_protection_cet_h_for_c_and_assembly() {
    let dir = work_dir("cet_h");
    let c = dir.path().join("c.c");
    std::fs::write(
        &c,
        "#include <cet.h>\n#ifdef _CET_ENDBR\n#error not in C\n#endif\n\
         int main(void) { return __CET__ == 3 ? 0 : 1; }\n",
    )
    .unwrap();
    let exe = dir.path().join("c");
    let (e, cs) = (exe.to_str().unwrap(), c.to_str().unwrap());
    let run = run_c17(&["-fcf-protection", "-o", e, cs]);
    assert!(run.success, "{}", run.stderr);
    assert_eq!(Command::new(&exe).status().expect("run").code(), Some(0));

    let s = dir.path().join("a.S");
    std::fs::write(
        &s,
        "#include <cet.h>\n\t.text\n\t.globl f\nf:\n\t_CET_ENDBR\n\tret\n\
         \t.section .note.GNU-stack,\"\",@progbits\n",
    )
    .unwrap();
    for (level, feature) in [
        ("full", Some("IBT, SHSTK")),
        ("branch", Some("IBT")),
        ("return", Some("SHSTK")),
        ("none", None),
    ] {
        let flag = format!("-fcf-protection={level}");
        let obj = dir.path().join("a.o");
        let (o, src) = (obj.to_str().unwrap(), s.to_str().unwrap());
        let run = run_c17(&[&flag, "-c", "-o", o, src]);
        assert!(run.success, "{flag}: {}", run.stderr);
        let notes = tool("readelf", &["-n", o]);
        match feature {
            Some(f) => assert!(
                notes.contains(&format!("x86 feature: {f}\n")),
                "{flag}:\n{notes}"
            ),
            None => assert!(!notes.contains("x86 feature"), "{flag}:\n{notes}"),
        }
        let dis = tool("objdump", &["-d", o]);
        let branch = level == "full" || level == "branch";
        assert_eq!(dis.contains("endbr64"), branch, "{flag}:\n{dis}");
    }
}

/// `-mshstk` and the shadow-stack intrinsics of `<immintrin.h>`, as pcre2
/// uses them once `__CET__` says the shadow stack may be on: `_get_ssp` is
/// 0 where it is not enabled (RDSSP is a no-op there; INCSSP would fault).
#[test]
fn cf_protection_shstk_intrinsics() {
    let dir = work_dir("shstk");
    let c = dir.path().join("s.c");
    std::fs::write(
        &c,
        "#include <immintrin.h>\n#ifndef __SHSTK__\n#error no __SHSTK__\n#endif\n\
         int main(void) {\n\
             unsigned long long ssp = _get_ssp();\n\
             if (ssp != 0) _inc_ssp(0);\n\
             (void)&_saveprevssp; (void)&_rstorssp; (void)&_wrssd; (void)&_wrssq;\n\
             (void)&_wrussd; (void)&_wrussq; (void)&_setssbsy; (void)&_clrssbsy;\n\
             return 0;\n}\n",
    )
    .unwrap();
    let cs = c.to_str().unwrap();
    for opt in ["-O0", "-O2"] {
        let exe = dir.path().join("s");
        let e = exe.to_str().unwrap();
        let run = run_c17(&[opt, "-mshstk", "-fcf-protection", "-o", e, cs]);
        assert!(run.success, "{opt}: {}", run.stderr);
        let status = Command::new(&exe).status().expect("run");
        assert_eq!(status.code(), Some(0), "{opt}");
        let obj = dir.path().join("s.o");
        let o = obj.to_str().unwrap();
        let run = run_c17(&[opt, "-mshstk", "-c", "-o", o, cs]);
        assert!(run.success, "{opt}: {}", run.stderr);
        let dis = tool("objdump", &["-d", o]);
        assert!(
            dis.contains("rdsspq") && dis.contains("incsspq"),
            "{opt}:\n{dis}"
        );
    }
}

/// `nocf_check`, as the Linux kernel's `__nocf_check` uses it: the function
/// has no `endbr64`, the call through a pointer of its type is `notrack`, and
/// the program runs -- at every level, the call reaching the function whether
/// or not the processor tracks branches.
#[test]
fn cf_protection_nocf_check_object_and_run() {
    let dir = work_dir("nocf");
    let src = dir.path().join("nocf.c");
    std::fs::write(
        &src,
        "__attribute__((nocf_check)) int untracked(int x) { return x * 3; }\n\
         int tracked(int x) { return x + 4; }\n\
         typedef int (*nocf_fn)(int) __attribute__((nocf_check));\n\
         nocf_fn volatile up = untracked;\n\
         int (*volatile tp)(int) = tracked;\n\
         int main(void) { return up(5) == 15 && tp(5) == 9 ? 0 : 1; }\n",
    )
    .unwrap();
    for opt in ["-O0", "-O2"] {
        let obj = dir.path().join("nocf.o");
        let exe = dir.path().join("nocf");
        let (o, s, e) = (
            obj.to_str().unwrap(),
            src.to_str().unwrap(),
            exe.to_str().unwrap(),
        );
        let run = run_c17(&[opt, "-fcf-protection=branch", "-c", "-o", o, s]);
        assert!(run.success, "{opt}: {}", run.stderr);
        let dis = tool("objdump", &["-d", "--no-show-raw-insn", o]);
        let first_after = |label: &str| {
            let at = dis
                .find(label)
                .unwrap_or_else(|| panic!("no {label}:\n{dis}"));
            dis[at..].lines().nth(1).unwrap_or("").to_string()
        };
        assert!(
            !first_after("<untracked>:").contains("endbr64"),
            "{opt}:\n{dis}"
        );
        assert!(
            first_after("<tracked>:").contains("endbr64"),
            "{opt}:\n{dis}"
        );
        assert_eq!(dis.matches("notrack call").count(), 1, "{opt}:\n{dis}");

        let run = run_c17(&[opt, "-fcf-protection=branch", "-o", e, s]);
        assert!(run.success, "{opt}: {}", run.stderr);
        let status = Command::new(&exe).status().expect("run");
        assert_eq!(status.code(), Some(0), "{opt}");
    }
}
