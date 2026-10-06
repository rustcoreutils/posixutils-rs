//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__attribute__((target("...")))` and `target_clones(...)`: one function
// compiled for an ISA above the translation unit's, the way runtime CPU
// dispatch is written (zlib-ng, xxhash, pixman). c17's ceiling is SSE4.2;
// anything above it is warned about and compiled at the unit's ISA.
//

use super::asm_probe::{asm_for, asm_for_with, assert_body_contains, assert_body_lacks};
use super::asm_probe::{AARCH64_LINUX, X86_64_LINUX};
use crate::test_compile::compile;

const VECTORS: &str = "typedef int v4si __attribute__((vector_size(16)));\n\
                       typedef unsigned v4su __attribute__((vector_size(16)));\n";

/// A `target("sse4.1")` function gets SSE4.1's one-instruction forms at the
/// SSE2 baseline; its neighbour keeps the SSE2 sequences.
#[test]
fn target_attr_raises_one_functions_isa() {
    let src = format!(
        "{VECTORS}\
         __attribute__((target(\"sse4.1\"))) v4si mul41(v4si a, v4si b) {{ return a * b; }}\n\
         __attribute__((target(\"sse4.1\"))) v4si ugt41(v4su a, v4su b) {{ return a > b; }}\n\
         v4si mul2(v4si a, v4si b) {{ return a * b; }}\n\
         v4si ugt2(v4su a, v4su b) {{ return a > b; }}\n"
    );
    let asm = asm_for("target_sse41", X86_64_LINUX, &src);
    assert_body_contains(
        &asm,
        "mul41",
        "pmulld",
        "target(\"sse4.1\") multiplies dwords",
    );
    assert_body_contains(
        &asm,
        "ugt41",
        "pminud",
        "target(\"sse4.1\") orders unsigned dwords",
    );
    assert_body_lacks(&asm, "mul2", "pmulld", "the baseline function stays SSE2");
    assert_body_contains(&asm, "mul2", "pmuludq", "the baseline function stays SSE2");
    assert_body_lacks(&asm, "ugt2", "pminud", "the baseline function stays SSE2");
}

/// The spellings real code uses are accepted in silence: a feature, a
/// `no-` feature, `arch=`, a comma list, `__target__`, and the attribute on a
/// declaration before the definition.
#[test]
fn target_attr_spellings_are_recognised() {
    let src = "__attribute__((target(\"sse4.2\"))) int a(int x) { return x; }\n\
               __attribute__((target(\"no-sse4.2\"))) int b(int x) { return x; }\n\
               __attribute__((target(\"arch=x86-64-v2\"))) int c(int x) { return x; }\n\
               __attribute__((__target__(\"sse4.1,popcnt\"))) int d(int x) { return x; }\n\
               __attribute__((target(\"ssse3\"))) int e(int x);\n\
               int e(int x) { return x; }\n";
    let c = compile(
        "target_spellings",
        src,
        &["--target=x86_64-unknown-linux-gnu"],
    );
    assert!(c.success, "{}", c.stderr);
    assert!(!c.stderr.contains("ignored"), "{}", c.stderr);
    // aarch64's spellings are accepted too; nothing in them changes code.
    let src = "__attribute__((target(\"arch=armv8-a+crc\"))) int a(int x) { return x; }\n\
               __attribute__((target(\"+crc\"))) int b(int x) { return x; }\n";
    let c = compile(
        "target_spellings_a64",
        src,
        &["--target=aarch64-unknown-linux-gnu"],
    );
    assert!(c.success, "{}", c.stderr);
    assert!(!c.stderr.contains("ignored"), "{}", c.stderr);
}

/// An ISA above c17's SSE4.2 ceiling is named in a warning, and the function
/// is compiled at the unit's ISA rather than refused.
#[test]
fn target_attr_beyond_the_ceiling_warns_and_compiles() {
    let src = "__attribute__((target(\"avx2\"))) int f(int x) { return x + 1; }\n";
    let c = compile("target_avx2", src, &["--target=x86_64-unknown-linux-gnu"]);
    assert!(c.success, "{}", c.stderr);
    assert!(c.stderr.contains("avx2"), "{}", c.stderr);
    // Recognised and judged, not skipped as an unknown attribute.
    assert!(!c.stderr.contains("directive ignored"), "{}", c.stderr);
    assert!(c.asm.is_some());
}

/// `target_clones` builds one copy per supported ISA plus `default`, named
/// as gcc names them, and makes the function itself a GNU indirect function
/// bound by a resolver. A clone above the ceiling is dropped.
#[test]
fn target_clones_builds_copies_and_a_resolver() {
    let src = "__attribute__((target_clones(\"avx2\", \"sse4.2\", \"default\")))\n\
               int sum(const int *p, int n) { int s = 0; for (int i = 0; i < n; i++) s += p[i]; return s; }\n\
               int use(const int *p) { return sum(p, 4); }\n";
    let asm = asm_for_with("target_clones", X86_64_LINUX, src, &["-O2"]);
    for label in ["sum.default:", "sum.sse4_2:", "sum.resolver:"] {
        assert!(asm.contains(label), "missing {label}:\n{asm}");
    }
    assert!(
        asm.contains("sum, @gnu_indirect_function"),
        "sum is not an indirect function:\n{asm}"
    );
    assert!(
        !asm.contains("sum.avx2"),
        "a clone above the ceiling:\n{asm}"
    );
}

/// aarch64 as gcc 13 has it: no function version dispatcher, so a list of
/// versions is refused -- an x86 name for what it is, a valid aarch64 one
/// for the missing dispatcher -- and `default` alone is the function, with
/// gcc's warning.
#[test]
fn target_clones_on_aarch64_matches_gcc() {
    let a64 = ["--target=aarch64-unknown-linux-gnu"];
    let c = compile(
        "target_clones_a64_x86",
        "__attribute__((target_clones(\"sse4.2\", \"default\")))\n\
         int sum(int a, int b) { return a + b; }\n",
        &a64,
    );
    assert!(!c.success, "{}", c.stderr);
    assert!(
        c.stderr
            .contains("error: pragma or attribute 'target(\"sse4.2\")' is not valid"),
        "{}",
        c.stderr
    );
    let c = compile(
        "target_clones_a64_crc",
        "__attribute__((target_clones(\"+crc\", \"default\")))\n\
         int sum(int a, int b) { return a + b; }\n",
        &a64,
    );
    assert!(!c.success, "{}", c.stderr);
    assert!(
        c.stderr
            .contains("error: target does not support function version dispatcher"),
        "{}",
        c.stderr
    );
    let src = "__attribute__((target_clones(\"default\")))\n\
               int sum(int a, int b) { return a + b; }\n";
    let c = compile("target_clones_a64_default", src, &a64);
    assert!(c.success, "{}", c.stderr);
    assert!(
        c.stderr
            .contains("warning: single 'target_clones' attribute is ignored"),
        "{}",
        c.stderr
    );
    let asm = asm_for("target_clones_a64_default_asm", AARCH64_LINUX, src);
    assert!(
        asm.contains("sum:") && !asm.contains("sum.default"),
        "{asm}"
    );
}

/// x86-64 too: one version is nothing to dispatch between, so gcc warns and
/// compiles the plain function.
#[test]
fn target_clones_single_version_is_the_plain_function() {
    let src = "__attribute__((target_clones(\"default\")))\n\
               int sum(int a, int b) { return a + b; }\n";
    let c = compile(
        "target_clones_single",
        src,
        &["--target=x86_64-unknown-linux-gnu"],
    );
    assert!(c.success, "{}", c.stderr);
    assert!(
        c.stderr
            .contains("warning: single 'target_clones' attribute is ignored"),
        "{}",
        c.stderr
    );
    let asm = c.asm.unwrap();
    assert!(
        asm.contains("sum:") && !asm.contains("gnu_indirect_function"),
        "{asm}"
    );
}

/// The resolver as gcc 13 emits it for a function with external linkage:
/// weak, in a comdat group of its own, the versions local; for a `static`
/// function everything is local. Versions above the ceiling are dropped,
/// and the resolver tests the rest best first.
#[test]
fn target_clones_symbols_match_gcc() {
    let src = "__attribute__((target_clones(\"sse3\", \"popcnt\", \"sse4.2\", \"default\")))\n\
               int sum(int a) { return a + 1; }\n\
               __attribute__((target_clones(\"sse4.1\", \"default\")))\n\
               static int loc(int a) { return a + 2; }\n\
               int (*get(void))(int) { return loc; }\n";
    let asm = asm_for("target_clones_symbols", X86_64_LINUX, src);
    for want in [
        ".section .text.sum.resolver,\"axG\",@progbits,sum.resolver,comdat",
        ".weak sum.resolver",
        ".globl sum",
        ".type sum, @gnu_indirect_function",
        ".set sum, sum.resolver",
        ".type loc, @gnu_indirect_function",
        ".set loc, loc.resolver",
        "call __cpu_indicator_init@PLT",
    ] {
        assert!(asm.contains(want), "missing {want:?}:\n{asm}");
    }
    for local in ["sum.default", "sum.sse4_2", "loc.resolver", "loc.sse4_1"] {
        assert!(
            !asm.contains(&format!(".globl {local}\n")),
            "{local} is global:\n{asm}"
        );
    }
    // popcnt (bit 2), then sse4.2 (bit 8), then sse3 (bit 5): the chain of
    // selects is built worst first, so the masks appear in that order
    // reversed.
    let body = asm
        .split("sum.resolver:")
        .nth(1)
        .and_then(|b| b.split(".size").next())
        .unwrap();
    let at = |m: &str| body.find(m).unwrap_or_else(|| panic!("no {m}:\n{body}"));
    assert!(
        at("$32,") < at("$256,") && at("$256,") < at("$4,"),
        "{body}"
    );
}
