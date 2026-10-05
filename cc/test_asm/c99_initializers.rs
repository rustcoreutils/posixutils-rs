//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compile-only cases of tests/c99/initializers*.rs, in process.
//

use crate::test_compile::compile;

/// The same path's rejection: a condition that really is not constant is an
/// error, reported at the expression rather than at line 0, and naming the
/// mistake rather than dumping the AST.
#[test]
fn c99_global_initializer_rejects_non_constant_condition() {
    let out = compile("nc", "int obj;\nstatic int bad = obj ?: 1;\n", &[]);
    let stderr = out.stderr;

    assert!(!out.success, "expected a rejection: {}", stderr);
    assert!(
        stderr.contains("is not a constant expression"),
        "expected the shared wording: {}",
        stderr
    );
    assert!(
        stderr.contains("nc.c:2:"),
        "expected the expression's own line, not line 0: {}",
        stderr
    );
    assert!(
        !stderr.contains("ExprKind") && !stderr.contains("Ident("),
        "expected no AST dump: {}",
        stderr
    );
}
