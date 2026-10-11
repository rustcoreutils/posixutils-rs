//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-ffunction-sections`, `-fdata-sections`, `-fcommon` and `-fno-plt`:
// programs built with them link and run, and the linker sees what they ask
// for. What the assembly says is in `test_asm/codegen_object_layout.rs`.
//

use crate::common::{compile_and_run, compile_and_run_aarch64_with};

const PROGRAM: &str = r#"
#include <stdio.h>
#include <string.h>
int counter;
static int hidden_total = 4;
int unused_object[1000] = {1};
__thread int per_thread = 6;
const char *const greeting = "hi";
int unused_function(int x) { return x * 7 + unused_object[x]; }
static int helper(int x) { return x + hidden_total; }
int main(void)
{
    char buf[8];
    counter++;
    snprintf(buf, sizeof buf, "%s%d", greeting, helper(counter) + per_thread);
    return strcmp(buf, "hi11") != 0;
}
"#;

/// Each function and object in its own section still makes the same
/// program, at every level and on both targets, and `-fno-plt`'s calls
/// through the GOT reach the C library.
#[test]
fn codegen_object_layout_options_run() {
    for flags in [
        &["-ffunction-sections", "-fdata-sections"][..],
        &["-fno-plt"],
        &["-fno-plt", "-fPIC"],
        &["-fno-plt", "-fno-pie"],
        &["-fcommon"],
    ] {
        for opt in ["-O0", "-O2"] {
            let mut all: Vec<String> = flags.iter().map(|f| f.to_string()).collect();
            all.push(opt.to_string());
            assert_eq!(
                compile_and_run("object_layout", PROGRAM, &all),
                0,
                "{flags:?} {opt}"
            );
            let mut a64 = flags.to_vec();
            a64.push(opt);
            if let Some(code) = compile_and_run_aarch64_with("object_layout", PROGRAM, &a64, &[]) {
                assert_eq!(code, 0, "aarch64 {flags:?} {opt}");
            }
        }
    }
}

/// `ld --gc-sections` drops what nothing reaches once each function and
/// object has a section of its own, as with gcc: the unused function and
/// array are gone from the executable, and what `main` uses is kept.
#[cfg(target_os = "linux")]
#[test]
fn codegen_function_sections_let_the_linker_collect_garbage() {
    let dir = crate::common::work_dir("gc_sections");
    let src = dir.path().join("prog.c");
    std::fs::write(&src, PROGRAM).unwrap();
    let exe = dir.path().join("prog").to_string_lossy().into_owned();
    let r = crate::common::run_c17(&[
        "-O2",
        "-ffunction-sections",
        "-fdata-sections",
        "-Wl,--gc-sections",
        "-o",
        &exe,
        &src.to_string_lossy(),
    ]);
    assert!(r.success, "{}", r.stderr);
    let status = std::process::Command::new(&exe).status().unwrap();
    assert_eq!(status.code(), Some(0));
    let nm = std::process::Command::new("nm").arg(&exe).output().unwrap();
    let symbols = String::from_utf8_lossy(&nm.stdout);
    for gone in ["unused_function", "unused_object"] {
        assert!(!symbols.contains(gone), "{gone} kept:\n{symbols}");
    }
    assert!(symbols.contains(" counter"), "{symbols}");
}

/// Under `-fcommon` two units may each have a tentative definition of one
/// object, and the linker merges them into one, as gcc's `-fcommon`; with
/// the default `-fno-common` the same pair is a multiple definition.
#[cfg(target_os = "linux")]
#[test]
fn codegen_fcommon_merges_tentative_definitions() {
    let unit_a = "int shared_count;\nint bump(void);\nint main(void) { shared_count = 41; return bump() != 42; }\n";
    let unit_b = "int shared_count;\nint bump(void) { return ++shared_count; }\n";
    let common = vec!["-fcommon".to_string()];
    assert_eq!(
        crate::common::compile_and_run_two_units("fcommon_merge", unit_a, unit_b, &common),
        0
    );
    assert_eq!(
        crate::common::compile_and_run_two_units("fno_common_merge", unit_a, unit_b, &[]),
        -1,
        "two definitions linked under -fno-common"
    );
}
