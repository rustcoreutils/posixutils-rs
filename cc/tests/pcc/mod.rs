//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Integration tests for pcc - end-to-end compilation tests
//

use crate::common::{cleanup_exe, compile, compile_test_file, create_c_file, run, run_with_output};

// ============================================================================
// Basic Tests
// ============================================================================

#[test]
fn test_return_constant() {
    let c_file = create_c_file(
        "return_constant",
        r#"
int main(void) {
    return 42;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 42, "expected exit code 42");

    cleanup_exe(&exe);
}

#[test]
fn test_arithmetic() {
    let c_file = create_c_file(
        "arithmetic",
        r#"
int main(void) {
    int a = 10;
    int b = 5;
    int c = a + b * 2;
    return c;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    // 10 + 5 * 2 = 10 + 10 = 20
    assert_eq!(exit_code, 20, "expected exit code 20");

    cleanup_exe(&exe);
}

#[test]
fn test_if_else() {
    let c_file = create_c_file(
        "if_else",
        r#"
int main(void) {
    int x = 10;
    if (x > 5) {
        return 1;
    } else {
        return 0;
    }
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    // 10 > 5 is true, so should return 1
    assert_eq!(exit_code, 1, "expected exit code 1");

    cleanup_exe(&exe);
}

#[test]
fn test_subtraction() {
    let c_file = create_c_file(
        "subtraction",
        r#"
int main(void) {
    int a = 100;
    int b = 58;
    return a - b;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    // 100 - 58 = 42
    assert_eq!(exit_code, 42, "expected exit code 42");

    cleanup_exe(&exe);
}

#[test]
fn test_division() {
    let c_file = create_c_file(
        "division",
        r#"
int main(void) {
    int a = 84;
    int b = 2;
    return a / b;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    // 84 / 2 = 42
    assert_eq!(exit_code, 42, "expected exit code 42");

    cleanup_exe(&exe);
}

#[test]
fn test_modulo() {
    let c_file = create_c_file(
        "modulo",
        r#"
int main(void) {
    int a = 47;
    int b = 10;
    return a % b;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    // 47 % 10 = 7
    assert_eq!(exit_code, 7, "expected exit code 7");

    cleanup_exe(&exe);
}

#[test]
fn test_nested_if() {
    let c_file = create_c_file(
        "nested_if",
        r#"
int main(void) {
    int a = 10;
    int b = 20;
    if (a < b) {
        if (b > 15) {
            return 3;
        } else {
            return 2;
        }
    } else {
        return 1;
    }
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    // a < b is true (10 < 20), b > 15 is true (20 > 15), so return 3
    assert_eq!(exit_code, 3, "expected exit code 3");

    cleanup_exe(&exe);
}

#[test]
fn test_bitwise() {
    let c_file = create_c_file(
        "bitwise",
        r#"
int main(void) {
    int a = 0x0F;
    int b = 0xF0;
    int c = a | b;
    return c & 0xFF;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    // 0x0F | 0xF0 = 0xFF, 0xFF & 0xFF = 0xFF = 255
    assert_eq!(exit_code, 255, "expected exit code 255");

    cleanup_exe(&exe);
}

// ============================================================================
// Function Call Tests
// ============================================================================

/// Test: Internal function calls
#[test]
fn test_function_call_internal() {
    let exe = compile_test_file("pcc", "func_internal.c");
    assert!(exe.is_some(), "compilation should succeed");

    let exit_code = run(exe.as_ref().unwrap());
    // sum(3, 4) = 7, double_it(7) = 14
    assert_eq!(exit_code, 14, "expected exit code 14");

    cleanup_exe(&exe);
}

/// Test: External libc function call (puts)
#[test]
fn test_function_call_libc() {
    let exe = compile_test_file("pcc", "func_libc.c");
    assert!(exe.is_some(), "compilation should succeed");

    let (exit_code, stdout) = run_with_output(exe.as_ref().unwrap());
    assert_eq!(exit_code, 0, "expected exit code 0");
    assert!(
        stdout.contains("Hello from libc!"),
        "expected output containing 'Hello from libc!'"
    );

    cleanup_exe(&exe);
}

/// Test: Combined internal and external function calls
#[test]
fn test_function_call_combined() {
    let exe = compile_test_file("pcc", "func_combined.c");
    assert!(exe.is_some(), "compilation should succeed");

    let (exit_code, stdout) = run_with_output(exe.as_ref().unwrap());
    // sum(3, 4) = 7, double_it(7) = 14
    assert_eq!(exit_code, 14, "expected exit code 14");
    assert!(
        stdout.contains("Function calls work!"),
        "expected output containing 'Function calls work!'"
    );

    cleanup_exe(&exe);
}

// ============================================================================
// Loop Tests
// ============================================================================

/// Test: While loop with break
#[test]
fn test_while_loop() {
    let c_file = create_c_file(
        "while_loop",
        r#"
int main(void) {
    int sum = 0;
    int i = 1;

    // Basic while loop: sum 1..5 = 15
    while (i <= 5) {
        sum = sum + i;
        i = i + 1;
    }
    if (sum != 15) return 1;

    // While with break: count to 5 using infinite loop + break
    i = 0;
    while (1) {
        i = i + 1;
        if (i == 5) break;
    }
    if (i != 5) return 2;

    // While with continue: sum odd numbers 1-9 = 25
    sum = 0;
    i = 0;
    while (i < 10) {
        i = i + 1;
        if ((i % 2) == 0) continue;  // Skip even
        sum = sum + i;
    }
    if (sum != 25) return 3;

    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(
        exit_code, 0,
        "while loop with break/continue: all tests should pass"
    );
    cleanup_exe(&exe);
}

/// Test: Do-while loop with break/continue
#[test]
fn test_do_while_loop() {
    let c_file = create_c_file(
        "do_while_loop",
        r#"
int main(void) {
    int sum = 0;
    int i = 1;

    // Basic do-while: sum 1..5 = 15
    do {
        sum = sum + i;
        i = i + 1;
    } while (i <= 5);
    if (sum != 15) return 1;

    // Do-while with break: count to 5
    i = 0;
    do {
        i = i + 1;
        if (i == 5) break;
    } while (1);
    if (i != 5) return 2;

    // Do-while with continue: sum odd 1-9 = 25
    sum = 0;
    i = 0;
    do {
        i = i + 1;
        if ((i % 2) == 0) continue;
        sum = sum + i;
    } while (i < 10);
    if (sum != 25) return 3;

    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(
        exit_code, 0,
        "do-while with break/continue: all tests should pass"
    );
    cleanup_exe(&exe);
}

/// Test: For loop with break/continue
#[test]
fn test_for_loop() {
    let c_file = create_c_file(
        "for_loop",
        r#"
int main(void) {
    int sum = 0;
    int i;

    // Basic for loop: sum 1..5 = 15
    for (i = 1; i <= 5; i++) {
        sum = sum + i;
    }
    if (sum != 15) return 1;

    // For with break: find first multiple of 7
    for (i = 1; i <= 100; i++) {
        if ((i % 7) == 0) break;
    }
    if (i != 7) return 2;

    // For with continue: sum odd numbers 1-9 = 25
    sum = 0;
    for (i = 1; i <= 9; i++) {
        if ((i % 2) == 0) continue;
        sum = sum + i;
    }
    if (sum != 25) return 3;

    // For with early break: sum until >= 10 (1+2+3+4 = 10)
    sum = 0;
    for (i = 1; i <= 100; i++) {
        sum = sum + i;
        if (sum >= 10) break;
    }
    if (sum != 10) return 4;

    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(
        exit_code, 0,
        "for loop with break/continue: all tests should pass"
    );
    cleanup_exe(&exe);
}

/// Test: 2-level nested for loops
#[test]
fn test_nested_for_2_levels() {
    let c_file = create_c_file(
        "nested_for_2",
        r#"
int main(void) {
    int sum = 0;
    int i, j;
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 3; j++) {
            sum++;
        }
    }
    return sum;  // 3*3 = 9
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 9, "nested for (2 levels): expected 9");
    cleanup_exe(&exe);
}

/// Test: 3-level nested for loops
#[test]
fn test_nested_for_3_levels() {
    let c_file = create_c_file(
        "nested_for_3",
        r#"
int main(void) {
    int sum = 0;
    int i, j, k;
    for (i = 0; i < 2; i++) {
        for (j = 0; j < 3; j++) {
            for (k = 0; k < 4; k++) {
                sum++;
            }
        }
    }
    return sum;  // 2*3*4 = 24
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 24, "nested for (3 levels): expected 24");
    cleanup_exe(&exe);
}

/// Test: Nested while loops
#[test]
fn test_nested_while() {
    let c_file = create_c_file(
        "nested_while",
        r#"
int main(void) {
    int sum = 0;
    int i = 0;
    int j;
    while (i < 3) {
        j = 0;
        while (j < 3) {
            sum++;
            j++;
        }
        i++;
    }
    return sum;  // 3*3 = 9
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 9, "nested while: expected 9");
    cleanup_exe(&exe);
}

/// Test: Nested do-while loops
#[test]
fn test_nested_do_while() {
    let c_file = create_c_file(
        "nested_do_while",
        r#"
int main(void) {
    int sum = 0;
    int i = 0;
    int j;
    do {
        j = 0;
        do {
            sum++;
            j++;
        } while (j < 2);
        i++;
    } while (i < 3);
    return sum;  // 3*2 = 6
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 6, "nested do-while: expected 6");
    cleanup_exe(&exe);
}

// ============================================================================
// Switch Statement Tests
// ============================================================================

/// Test: Basic switch statement with cases and default
#[test]
fn test_switch_basic() {
    let c_file = create_c_file(
        "switch_basic",
        r#"
int main(void) {
    int x = 2;
    int result = 0;
    switch (x) {
        case 1:
            result = 10;
            break;
        case 2:
            result = 20;
            break;
        case 3:
            result = 30;
            break;
        default:
            result = 99;
            break;
    }
    if (result != 20) return 1;
    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "switch_basic: compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 0, "switch_basic: case 2 should set result to 20");
    cleanup_exe(&exe);
}

/// Test: Switch with fallthrough between cases
#[test]
fn test_switch_fallthrough() {
    let c_file = create_c_file(
        "switch_fallthrough",
        r#"
int main(void) {
    int x = 1;
    int result = 0;
    switch (x) {
        case 1:
            result = result + 1;
        case 2:
            result = result + 2;
            break;
        case 3:
            result = 30;
            break;
    }
    if (result != 3) return 1;  // 1 + 2 from fallthrough
    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(
        exe.is_some(),
        "switch_fallthrough: compilation should succeed"
    );
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(
        exit_code, 0,
        "switch_fallthrough: case 1 should fall through to case 2"
    );
    cleanup_exe(&exe);
}

/// Test: Switch that uses default case
#[test]
fn test_switch_default() {
    let c_file = create_c_file(
        "switch_default",
        r#"
int main(void) {
    int x = 99;
    int result = 0;
    switch (x) {
        case 1:
            result = 10;
            break;
        case 2:
            result = 20;
            break;
        default:
            result = 42;
            break;
    }
    if (result != 42) return 1;
    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "switch_default: compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(
        exit_code, 0,
        "switch_default: unmatched value should use default"
    );
    cleanup_exe(&exe);
}

/// Test: Switch with no default and no matching case
#[test]
fn test_switch_no_default() {
    let c_file = create_c_file(
        "switch_no_default",
        r#"
int main(void) {
    int x = 99;
    int result = 5;
    switch (x) {
        case 1:
            result = 10;
            break;
        case 2:
            result = 20;
            break;
    }
    if (result != 5) return 1;  // No case matched, result unchanged
    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(
        exe.is_some(),
        "switch_no_default: compilation should succeed"
    );
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(
        exit_code, 0,
        "switch_no_default: unmatched value should skip switch body"
    );
    cleanup_exe(&exe);
}

/// Test: Switch with negative case values
#[test]
fn test_switch_negative_cases() {
    let c_file = create_c_file(
        "switch_negative",
        r#"
int main(void) {
    int x = -1;
    int result = 0;
    switch (x) {
        case -2:
            result = 1;
            break;
        case -1:
            result = 2;
            break;
        case 0:
            result = 3;
            break;
        case 1:
            result = 4;
            break;
    }
    if (result != 2) return 1;
    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "switch_negative: compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 0, "switch_negative: case -1 should match");
    cleanup_exe(&exe);
}

/// Test: Switch with zero case value
#[test]
fn test_switch_zero_case() {
    let c_file = create_c_file(
        "switch_zero",
        r#"
int main(void) {
    int x = 0;
    int result = 99;
    switch (x) {
        case 0:
            result = 0;
            break;
        case 1:
            result = 1;
            break;
    }
    if (result != 0) return 1;
    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "switch_zero: compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 0, "switch_zero: case 0 should match");
    cleanup_exe(&exe);
}

// ============================================================================
// Enum Tests
// ============================================================================

/// Test: Basic enum with implicit values
#[test]
fn test_enum_basic() {
    let c_file = create_c_file(
        "enum_basic",
        r#"
enum Color { RED, GREEN, BLUE };
int main(void) {
    enum Color c = GREEN;
    if (c != 1) return 1;
    if (RED != 0) return 2;
    if (BLUE != 2) return 3;
    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "enum_basic: compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(
        exit_code, 0,
        "enum_basic: enum values should match expected"
    );
    cleanup_exe(&exe);
}

/// Test: Enum with explicit values
#[test]
fn test_enum_explicit_values() {
    let c_file = create_c_file(
        "enum_explicit",
        r#"
enum Status { OK = 0, ERROR = 10, PENDING = 100 };
int main(void) {
    if (OK != 0) return 1;
    if (ERROR != 10) return 2;
    if (PENDING != 100) return 3;
    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "enum_explicit: compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 0, "enum_explicit: explicit values should work");
    cleanup_exe(&exe);
}

/// Test: Enum with negative values
#[test]
fn test_enum_negative_values() {
    let c_file = create_c_file(
        "enum_negative",
        r#"
enum Level { LOW = -1, MEDIUM = 0, HIGH = 1 };
int main(void) {
    if (LOW != -1) return 1;
    if (MEDIUM != 0) return 2;
    if (HIGH != 1) return 3;
    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "enum_negative: compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 0, "enum_negative: negative values should work");
    cleanup_exe(&exe);
}

/// Test: Enum in switch statement
#[test]
fn test_enum_switch() {
    let c_file = create_c_file(
        "enum_switch",
        r#"
enum Day { MON, TUE, WED };
int main(void) {
    enum Day d = TUE;
    int result = 0;
    switch (d) {
        case MON: result = 1; break;
        case TUE: result = 2; break;
        case WED: result = 3; break;
    }
    if (result != 2) return 1;
    return 0;
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "enum_switch: compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 0, "enum_switch: switch on enum should work");
    cleanup_exe(&exe);
}

// ============================================================================
// Goto Statement Tests
// ============================================================================

/// Test: Basic forward goto
#[test]
fn test_goto_forward() {
    let c_file = create_c_file(
        "goto_forward",
        r#"
int main(void) {
    int x = 1;
    goto skip;
    x = 99;  // Should be skipped
skip:
    return x;  // Should return 1
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "goto_forward: compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(
        exit_code, 1,
        "goto_forward: should skip assignment and return 1"
    );
    cleanup_exe(&exe);
}

/// Test: Backward goto (loop-like pattern)
#[test]
fn test_goto_backward() {
    let c_file = create_c_file(
        "goto_backward",
        r#"
int main(void) {
    int sum = 0;
    int i = 1;
loop:
    sum = sum + i;
    i = i + 1;
    if (i <= 5)
        goto loop;
    return sum;  // Should return 1+2+3+4+5 = 15
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "goto_backward: compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(exit_code, 15, "goto_backward: loop via goto should sum 1-5");
    cleanup_exe(&exe);
}

/// Test: Multiple labels and gotos
#[test]
fn test_goto_multiple_labels() {
    let c_file = create_c_file(
        "goto_multi",
        r#"
int main(void) {
    int x = 0;
    goto first;
second:
    x = x + 10;
    goto done;
first:
    x = x + 1;
    goto second;
done:
    return x;  // Should return 11
}
"#,
    );

    let exe = compile(&c_file.path().to_path_buf());
    assert!(exe.is_some(), "goto_multi: compilation should succeed");
    let exit_code = run(exe.as_ref().unwrap());
    assert_eq!(
        exit_code, 11,
        "goto_multi: should follow first->second->done path"
    );
    cleanup_exe(&exe);
}
