//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Determinism regression tests.
//
// pcc must produce bit-identical .s/.o output on identical input
// across runs. This is a property of the *compiler*, not the
// generated code: it ensures reproducible builds, makes binary
// diffs across compiler changes meaningful, and prevents subtle
// "works on my machine" bugs where HashMap iteration order
// happens to produce one valid ordering on dev and a different
// valid (but mismatched) ordering on CI.
//
// Each test below compiles a small representative C program twice
// with the same flags and asserts the assembly output is
// byte-identical. The programs are chosen to exercise specific
// containers known to have historically produced non-deterministic
// output:
//
//   - `phi_pseudo_ids`     :  ssa::ssa_convert iterating
//                              func.locals.keys() (HashMap of
//                              local names → LocalVar)
//   - `copy_temp_ids`      :  ir::lower::eliminate_phi_nodes
//                              iterating copies_to_insert
//                              (HashMap of bb_id → CopyInfo[])
//                              and sequenced_copies
//   - `fp_constants`       :  x86_64 codegen iterating
//                              double_constants (HashMap of u64
//                              → f64) when emitting .rodata
//   - `ld_constants`       :  x86_64 codegen iterating
//                              ld_constants (HashMap of u64 →
//                              [u8; 16]) for long double constants
//
// If a new HashMap is introduced into the compiler whose iteration
// affects output, the test that touches the same code path will
// catch the regression on the next run.

use plib::testing::run_test_base;
use std::io::Write;
use tempfile::NamedTempFile;

fn create_c_file(name: &str, content: &str) -> NamedTempFile {
    let mut file = tempfile::Builder::new()
        .prefix(&format!("pcc_det_{}_", name))
        .suffix(".c")
        .tempfile()
        .expect("failed to create temp file");
    file.write_all(content.as_bytes())
        .expect("failed to write test file");
    file
}

/// Compile `content` with the given extra args twice and return both
/// assembly outputs as Strings. Each invocation is a fresh pcc
/// process, so each gets its own HashMap RandomState seed.
fn compile_twice(name: &str, content: &str, extra_args: &[&str]) -> (String, String) {
    let c_file = create_c_file(name, content);
    let c_path = c_file.path().to_string_lossy().to_string();

    let mut outputs = Vec::with_capacity(2);
    for i in 0..2 {
        let s_path = std::env::temp_dir().join(format!("pcc_det_{}_{}.s", name, i));
        let s_path_str = s_path.to_string_lossy().to_string();

        let mut args: Vec<String> = vec!["-S".to_string(), "-o".to_string(), s_path_str.clone()];
        for a in extra_args {
            args.push((*a).to_string());
        }
        args.push(c_path.clone());

        let result = run_test_base("pcc", &args, &[]);
        assert!(
            result.status.success(),
            "pcc compilation failed for {name} (run {i}):\n{}",
            String::from_utf8_lossy(&result.stderr)
        );

        let asm = std::fs::read_to_string(&s_path)
            .unwrap_or_else(|e| panic!("failed to read {s_path_str}: {e}"));
        let _ = std::fs::remove_file(&s_path);
        outputs.push(asm);
    }

    let b = outputs.pop().unwrap();
    let a = outputs.pop().unwrap();
    (a, b)
}

/// Assert that two compilations of the same source produce byte-
/// identical assembly. On failure, print a unified diff so the
/// specific divergence is visible (much more useful than "outputs
/// differ").
fn assert_deterministic(name: &str, content: &str, extra_args: &[&str]) {
    let (a, b) = compile_twice(name, content, extra_args);
    if a == b {
        return;
    }
    // Emit a short diff to help locate the non-determinism.
    let mut diff_lines = Vec::new();
    let a_lines: Vec<&str> = a.lines().collect();
    let b_lines: Vec<&str> = b.lines().collect();
    let max = a_lines.len().max(b_lines.len());
    let mut shown = 0;
    for i in 0..max {
        let la = a_lines.get(i).copied().unwrap_or("<EOF>");
        let lb = b_lines.get(i).copied().unwrap_or("<EOF>");
        if la != lb {
            diff_lines.push(format!("  line {i:5}: - {la}"));
            diff_lines.push(format!("           + {lb}"));
            shown += 1;
            if shown >= 10 {
                diff_lines.push(format!("  ... ({} more differing lines)", max - i - 1));
                break;
            }
        }
    }
    panic!(
        "non-deterministic compilation for '{name}':\n{}",
        diff_lines.join("\n")
    );
}

#[test]
fn determinism_phi_pseudo_ids_no_opt() {
    // Same as phi_pseudo_ids but without -O. Helps isolate whether
    // any non-determinism comes from the optimization pipeline vs.
    // the regalloc / codegen.
    let src = r#"
int f(int n) {
    int sum = 0;
    int prod = 1;
    for (int i = 0; i < n; i++) {
        sum += i;
        prod *= (i + 1);
    }
    return sum + prod;
}
"#;
    assert_deterministic("phi_pseudo_ids_no_opt", src, &[]);
}

#[test]
fn determinism_phi_pseudo_ids() {
    // Exercises ssa::ssa_convert's iteration over `func.locals`. Two
    // locals that both get φ-promoted; their order of promotion
    // determines which one gets the lower phi pseudo ID. Pre-fix this
    // varied between runs; post-fix it's stable (sorted by name).
    let src = r#"
int f(int n) {
    int sum = 0;
    int prod = 1;
    for (int i = 0; i < n; i++) {
        sum += i;
        prod *= (i + 1);
    }
    return sum + prod;
}
"#;
    assert_deterministic("phi_pseudo_ids", src, &["-O"]);
}

#[test]
fn determinism_copy_temp_ids() {
    // Exercises ir::lower::eliminate_phi_nodes's iteration over
    // copies_to_insert. Multiple blocks each carry phi sources;
    // sequentialize_copies allocates temp pseudos for cycles. Pre-fix
    // the per-block order of temp pseudo IDs varied; post-fix it's
    // stable (sorted by bb_id).
    let src = r#"
int f(int x, int y, int z) {
    int a = x;
    int b = y;
    int c = z;
    for (int i = 0; i < 10; i++) {
        int t = a;
        a = b;
        b = c;
        c = t;
    }
    return a + b + c;
}
"#;
    assert_deterministic("copy_temp_ids", src, &["-O"]);
}

#[test]
fn determinism_double_constants() {
    // Exercises x86_64 codegen's iteration over `double_constants`
    // when emitting the .rodata section for floating-point literals
    // that need to round-trip through memory (e.g., int → double
    // conversions with values that don't fit in 32-bit). Pre-fix the
    // emission order varied; post-fix it's stable (sorted by bit
    // pattern).
    let src = r#"
double f(int a, int b, int c, int d) {
    double da = (double)a;
    double db = (double)b;
    double dc = (double)c;
    double dd = (double)d;
    return da * 1.5 + db * 2.5 + dc * 3.5 + dd * 4.5;
}
"#;
    assert_deterministic("double_constants", src, &["-O"]);
}

#[test]
fn determinism_ld_constants() {
    // Exercises x86_64 codegen's iteration over `ld_constants` when
    // emitting long-double literals. Multiple distinct long doubles
    // populate the table; pre-fix the .rodata layout varied with
    // HashMap order, post-fix it's stable (sorted by bit pattern).
    let src = r#"
long double f(int x) {
    long double a = 1.5L;
    long double b = 2.25L;
    long double c = 3.125L;
    long double d = 4.0625L;
    return (x > 0) ? a + b + c + d : a - b - c - d;
}
"#;
    assert_deterministic("ld_constants", src, &["-O"]);
}

#[test]
fn determinism_complex_function() {
    // A general-purpose stress test: a single function exercising
    // many compiler passes — SSA promotion, phi elimination, FP
    // constants, struct/array indexing, loops, calls. If any pass
    // introduces new non-determinism, this is the most likely test
    // to catch it.
    let src = r#"
#include <math.h>
struct Vec3 { double x; double y; double z; };

static double dot(struct Vec3 a, struct Vec3 b) {
    return a.x*b.x + a.y*b.y + a.z*b.z;
}

double f(struct Vec3 *v, int n) {
    double sum = 0.0;
    double max = -1e308;
    for (int i = 0; i < n; i++) {
        struct Vec3 a = v[i];
        struct Vec3 b = v[(i + 1) % n];
        double d = dot(a, b);
        sum += d;
        if (d > max) max = d;
    }
    return sum + max * 0.5 - sqrt(sum);
}
"#;
    assert_deterministic("complex_function", src, &["-O"]);
}
