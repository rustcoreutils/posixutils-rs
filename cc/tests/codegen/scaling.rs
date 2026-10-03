//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Functions whose size once made c17 quadratic in time or memory
//

use crate::common::{compile_and_run, compile_and_run_aarch64, create_c_file};
use plib::testing::run_test_base_with_env;

/// `if (x > i) v = i;` repeated `n` times: one constant per statement, and a
/// dominator tree `n` levels deep.
///
/// Every constant had no def, so liveness carried it from each use back to
/// the entry block -- n constants live across up to n blocks: 7 GB and
/// 155 s at n = 20000. The walks over the dominator tree recursed once per
/// level.
fn sequential_ifs(n: usize) -> String {
    let body: String = (0..n)
        .map(|i| format!("    if (x > {i}) v = {i};\n"))
        .collect();
    format!(
        "volatile int v;\n\
         int deep(int x)\n{{\n{body}    return v;\n}}\n\
         int main(void) {{ return deep(3) == 2 ? 0 : 1; }}\n"
    )
}

/// The same with a local accumulator, so every statement is a diamond with
/// a phi: phi elimination looked each source up by scanning every pseudo,
/// and if-conversion rescanned the function after every collapse.
fn accumulating_ifs(n: usize) -> String {
    let body: String = (0..n)
        .map(|i| format!("    if (x > {i}) s += {};\n", i % 7))
        .collect();
    format!(
        "int deep(int x)\n{{\n    int s = 0;\n{body}    return s;\n}}\n\
         int main(void) {{ return deep(3) == 0 + 1 + 2 ? 0 : 1; }}\n"
    )
}

/// `n` locals live at once: spilling looked each one's interval up by
/// scanning every interval.
fn many_locals(n: usize) -> String {
    let decls: String = (0..n)
        .map(|i| format!("    volatile int a{i} = x + {i};\n"))
        .collect();
    let sum: Vec<String> = (0..n).step_by(n / 20).map(|i| format!("a{i}")).collect();
    let expect: usize = (0..n).step_by(n / 20).sum();
    format!(
        "int deep(int x)\n{{\n{decls}    return {};\n}}\n\
         int main(void) {{ return deep(0) == {expect} ? 0 : 1; }}\n",
        sum.join(" + ")
    )
}

/// `n` labels, each with a division and a `goto` to a label far away, so the
/// merged live range of every value spans most of the function and crosses
/// every division, which clobbers fixed registers on x86-64: the forbidden
/// registers were collected by visiting every clobbering point inside every
/// range, 27 s at n = 10000. The duplicate-label and jump-scope checks also
/// compared every label with every other and with every `goto`.
fn many_gotos(n: usize) -> String {
    let body: String = (0..n)
        .map(|i| {
            format!(
                " l{i}: acc += x / d + {i};\n    x = (x * 3 + {i}) & 1023;\n    \
                 if (++steps > 5000) return acc;\n    if (x % 5 == 0) goto l{};\n",
                (i * 31 + 7) % n
            )
        })
        .collect();
    // The same walk, to know the answer.
    let (mut i, mut x, mut acc, mut steps) = (0, 5usize, 0usize, 0);
    while i < n {
        acc += x / 3 + i;
        x = (x * 3 + i) & 1023;
        steps += 1;
        if steps > 5000 {
            break;
        }
        i = if x % 5 == 0 { (i * 31 + 7) % n } else { i + 1 };
    }
    format!(
        "int deep(int x, int d)\n{{\n    int acc = 0, steps = 0;\n{body}    return acc;\n}}\n\
         int main(void) {{ return deep(5, 3) == {acc} ? 0 : 1; }}\n"
    )
}

#[test]
fn codegen_large_functions_compile_and_run() {
    for (name, src) in [
        ("sequential_ifs", sequential_ifs(20000)),
        ("accumulating_ifs", accumulating_ifs(5000)),
        ("many_locals", many_locals(5000)),
        ("many_gotos", many_gotos(3000)),
    ] {
        assert_eq!(compile_and_run(name, &src, &[]), 0, "{name} -O0");
        let opts = vec!["-O2".to_string()];
        assert_eq!(compile_and_run(name, &src, &opts), 0, "{name} -O2");
        for opt in ["-O0", "-O2"] {
            if let Some(code) = compile_and_run_aarch64(name, &src, opt) {
                assert_eq!(code, 0, "{name} aarch64 {opt}");
            }
        }
    }
}

/// No pass may recurse once per dominator-tree level: a function this long
/// compiles in an 8 MB compiler stack, far below the one c17 runs itself on.
#[test]
fn codegen_deep_dominator_tree_needs_no_deep_stack() {
    let c = create_c_file("deep_domtree", &sequential_ifs(100000));
    for opt in ["-O0", "-O2"] {
        let args: Vec<String> = [opt, "-S", "-o", "/dev/null", &c.path().to_string_lossy()]
            .iter()
            .map(|s| s.to_string())
            .collect();
        let out = run_test_base_with_env("c17", &args, &[], &[("RUST_MIN_STACK", "8388608")]);
        assert!(
            out.status.success(),
            "{opt}: {}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}
