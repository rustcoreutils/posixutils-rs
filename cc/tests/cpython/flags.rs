//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// CPython flag compatibility tests
//

use crate::common::compile_and_run;

/// Test that c17 accepts CPython-style compiler flags without crashing
#[test]
fn cpython_flags_ignored_flags() {
    let code = r#"
int main(void) {
    return 0;
}
"#;
    // Simulate flags that CPython's configure probes
    let extra = vec![
        "-fvisibility=hidden".to_string(),
        "-fno-semantic-interposition".to_string(),
        "-fstack-protector-strong".to_string(),
        "-fno-plt".to_string(),
        "-pipe".to_string(),
        "-fno-common".to_string(),
    ];
    assert_eq!(compile_and_run("cpython_flags_ignored", code, &extra), 0);
}

/// Test that -pthread defines _REENTRANT and links correctly
#[test]
fn cpython_flags_pthread() {
    let code = r#"
#ifndef _REENTRANT
#error "_REENTRANT not defined"
#endif
int main(void) {
    return 0;
}
"#;
    let extra = vec!["-pthread".to_string()];
    assert_eq!(compile_and_run("cpython_flags_pthread", code, &extra), 0);
}

/// The instruction-set flags CPython's configure probes for its HACL*
/// SIMD builds are x86-64 options: accepted there, with the macros that
/// say so, and unrecognized on aarch64, as gcc has them.
#[test]
fn cpython_flags_m_flags() {
    let code = r#"
int main(void) {
#if defined(__x86_64__) && !(defined(__SSE4_2__) && defined(__AVX2__))
    return 1;
#endif
    return 0;
}
"#;
    let extra: Vec<String> = [
        "-msse", "-msse2", "-msse3", "-msse4.1", "-msse4.2", "-mavx2",
    ]
    .iter()
    .map(|f| f.to_string())
    .collect();
    let code_of = compile_and_run("cpython_flags_mflags", code, &extra);
    if cfg!(target_arch = "x86_64") {
        assert_eq!(code_of, 0);
    } else {
        assert_ne!(code_of, 0);
    }
}
