//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Frame slots, seen through `-fstack-usage`: how big a frame the allocator
// builds, which a program's exit status cannot show.
//

use super::asm_probe::{stack_use, AARCH64_LINUX, X86_64_LINUX};

/// A `switch` of `n` cases, each with a floating-point value defined at the
/// case's head and used across an `if` in its body -- so the value lives in
/// a frame slot.
fn switch_of(n: usize) -> String {
    let mut src = String::from("long sw(long k, double x) {\n  switch (k) {\n");
    for i in 0..n {
        src.push_str(&format!(
            "  case {i}: {{ double v = x * {i}.5; if (v > {i}) v = v * x; \
             return (long)(v + {i}); }}\n"
        ));
    }
    src.push_str("  }\n  return 0;\n}\n");
    src
}

/// Only one case runs, so the cases' spilled values are never live
/// together, and one case's slot serves every case: the frame does not grow
/// with the number of cases.
///
/// Block order scatters a case's blocks among the others', so the hull of a
/// value's positions spans cases it is dead in. Slots shared only between
/// values whose hulls were disjoint gave each case its own, and the frame of
/// an N-case switch grew by sixteen bytes a case. Slots are shared by exact
/// liveness instead.
#[test]
fn a_switch_frame_does_not_grow_with_its_cases() {
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let small = stack_use("switch_frame_4", triple, &switch_of(4), &["-O2"]);
        let large = stack_use("switch_frame_16", triple, &switch_of(16), &["-O2"]);
        assert_eq!(
            small["sw"], large["sw"],
            "{triple}: 4 cases use {} bytes, 16 cases {}",
            small["sw"], large["sw"]
        );
    }
}
