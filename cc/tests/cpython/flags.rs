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

/// Test that pcc accepts CPython-style compiler flags without crashing
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

#[test]
fn cpython_flags_m_flags_unsupported() {
    let code = r#"
int main(void) {
    return 0;
}
"#;
    let extra = vec![
        "-msse".to_string(),
        "-msse2".to_string(),
        "-mavx2".to_string(),
        "-march=x86-64".to_string(),
    ];
    assert_ne!(
        compile_and_run("cpython_flags_mflags_unsupported", code, &extra),
        0
    );
}
