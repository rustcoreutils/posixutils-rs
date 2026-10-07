//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The `-M` family on a `.S` operand: it is preprocessed, so it has a rule.
//

use crate::common::run_c17;
use std::path::PathBuf;

/// A scratch directory holding `u.S`, which includes `regs.h`.
fn scratch() -> (plib::tmp::TempDir, PathBuf) {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_asm_deps_")
        .tempdir()
        .expect("tempdir");
    std::fs::write(dir.path().join("regs.h"), "#define RET ret\n").unwrap();
    let src = dir.path().join("u.S");
    std::fs::write(
        &src,
        "#include \"regs.h\"\n\t.text\n\t.globl f\nf:\n\tRET\n",
    )
    .unwrap();
    (dir, src)
}

/// `-MD -MP -MF` on a `.S` writes the rule beside the object. libffi's
/// automake rule compiles `unix64.S` with `-MD -MP -MF .deps/unix64.Tpo` and
/// then `mv`s that file; c17 wrote no rule for an assembler operand, so the
/// `mv` failed and with it the build.
#[test]
fn asm_dependencies_md_writes_the_rule_for_a_dot_s_capital() {
    let (dir, src) = scratch();
    let obj = dir.path().join("u.o");
    let dep = dir.path().join("u.Tpo");
    let r = run_c17(&[
        "-MT",
        "u.lo",
        "-MD",
        "-MP",
        "-MF",
        dep.to_str().unwrap(),
        "-c",
        src.to_str().unwrap(),
        "-o",
        obj.to_str().unwrap(),
    ]);
    assert!(r.success, "-MD on a .S failed: {}", r.stderr);
    assert!(obj.exists(), "-MD must still assemble");
    let rule = std::fs::read_to_string(&dep).expect("-MF names the rule's file");
    assert!(rule.starts_with("u.lo:"), "{rule:?}");
    assert!(rule.contains("u.S"), "{rule:?}");
    assert!(rule.contains("regs.h"), "{rule:?}");
    assert!(
        rule.lines().any(|l| l.ends_with("regs.h:")),
        "-MP adds a bare rule for the header: {rule:?}"
    );
}

/// `-M` on a `.S` prints the rule instead of anything else: no preprocessed
/// text and no object.
#[test]
fn asm_dependencies_dash_m_prints_only_the_rule() {
    let (dir, src) = scratch();
    let r = run_c17(&["-M", src.to_str().unwrap()]);
    assert!(r.success, "-M on a .S failed: {}", r.stderr);
    assert!(r.stdout.starts_with("u.o:"), "{:?}", r.stdout);
    assert!(r.stdout.contains("regs.h"), "{:?}", r.stdout);
    assert!(!r.stdout.contains(".globl"), "{:?}", r.stdout);
    assert!(!dir.path().join("u.o").exists());
}
