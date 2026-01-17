//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for C11 features: _Alignof, _Static_assert, _Thread_local,
// and GCC builtins: __builtin_frame_address, __builtin_return_address
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// _Alignof tests (C11 6.5.3.4)
// ============================================================================

#[test]
fn alignof_basic_types() {
    let code = r#"
int main(void) {
    // Test 1-4: Basic type alignments
    if (_Alignof(char) != 1) return 1;
    if (_Alignof(short) != 2) return 2;
    if (_Alignof(int) != 4) return 3;
    if (_Alignof(long) != 8) return 4;

    // Test 5-6: Pointer and double alignment
    if (_Alignof(void*) != 8) return 5;
    if (_Alignof(double) != 8) return 6;

    // Test 7-9: Alternative spellings
    if (alignof(int) != 4) return 7;
    if (__alignof__(int) != 4) return 8;
    if (__alignof(int) != 4) return 9;

    // Test 10: _Alignof on expression
    int x;
    if (_Alignof(x) != 4) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("alignof_basic", code, &[]), 0);
}

#[test]
fn alignof_struct_and_array() {
    let code = r#"
struct S {
    char c;
    int i;
    double d;
};

int main(void) {
    // Test 1: Struct alignment is max of member alignments
    if (_Alignof(struct S) != 8) return 1;  // double has alignment 8

    // Test 2: Array alignment equals element alignment
    int arr[10];
    if (_Alignof(arr) != 4) return 2;

    // Test 3: Alignment of struct with only chars
    struct CharOnly { char a; char b; };
    if (_Alignof(struct CharOnly) != 1) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("alignof_struct", code, &[]), 0);
}

// ============================================================================
// _Static_assert tests (C11 6.7.10)
// ============================================================================

#[test]
fn static_assert_basic() {
    let code = r#"
// File-scope static assertions
_Static_assert(1, "always true");
_Static_assert(sizeof(int) >= 4, "int at least 4 bytes");
_Static_assert(sizeof(char) == 1, "char is 1 byte");

// C23 style without message
static_assert(1);
static_assert(sizeof(long) == 8);

int main(void) {
    // Block-scope static assertion
    _Static_assert(sizeof(void*) == 8, "64-bit pointers");
    static_assert(1 + 1 == 2);

    return 0;
}
"#;
    assert_eq!(compile_and_run("static_assert_basic", code, &[]), 0);
}

#[test]
fn static_assert_in_struct() {
    let code = r#"
// C11 allows _Static_assert in struct definitions (6.7.2.1p1)
struct WithAssert {
    int x;
    _Static_assert(sizeof(int) == 4, "int size check");
    double y;
    _Static_assert(1, "always passes");
};

int main(void) {
    struct WithAssert s;
    s.x = 42;
    s.y = 3.14;
    if (s.x != 42) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("static_assert_struct", code, &[]), 0);
}

// ============================================================================
// _Thread_local tests (C11 6.7.1)
// ============================================================================

#[test]
fn thread_local_basic() {
    let code = r#"
// File-scope thread-local variables
_Thread_local int tls_var = 42;
__thread int tls_var2 = 100;
static _Thread_local int tls_static = 200;

int main(void) {
    // Test 1-2: Basic TLS read
    if (tls_var != 42) return 1;
    if (tls_var2 != 100) return 2;

    // Test 3: Static TLS
    if (tls_static != 200) return 3;

    // Test 4-5: TLS write
    tls_var = 10;
    if (tls_var != 10) return 4;
    tls_var2 = 20;
    if (tls_var2 != 20) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("thread_local_basic", code, &[]), 0);
}

#[test]
fn thread_local_uninitialized() {
    let code = r#"
// Uninitialized TLS should be zero-initialized
_Thread_local int tls_uninit;
__thread long tls_uninit_long;

int main(void) {
    if (tls_uninit != 0) return 1;
    if (tls_uninit_long != 0) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("thread_local_uninit", code, &[]), 0);
}

// ============================================================================
// __builtin_frame_address and __builtin_return_address tests
// ============================================================================

#[test]
fn frame_return_address_basic() {
    let code = r#"
int main(void) {
    // Test 1: Frame pointer should be non-null
    void *fp = __builtin_frame_address(0);
    if (fp == 0) return 1;

    // Test 2: Return address should be non-null
    void *ra = __builtin_return_address(0);
    if (ra == 0) return 2;

    // Test 3: Frame pointer and return address should be different
    if (fp == ra) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("frame_return_addr", code, &[]), 0);
}

#[test]
fn frame_address_in_function() {
    let code = r#"
void *get_frame(void) {
    return __builtin_frame_address(0);
}

void *get_return_addr(void) {
    return __builtin_return_address(0);
}

int main(void) {
    // Test 1: Frame address from helper function
    void *fp = get_frame();
    if (fp == 0) return 1;

    // Test 2: Return address from helper function
    void *ra = get_return_addr();
    if (ra == 0) return 2;

    // Test 3: Main's frame should be different from helper's
    void *main_fp = __builtin_frame_address(0);
    // Can't directly compare since helper's frame is gone,
    // but main's frame should be valid
    if (main_fp == 0) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("frame_addr_func", code, &[]), 0);
}
