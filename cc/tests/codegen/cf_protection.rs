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
