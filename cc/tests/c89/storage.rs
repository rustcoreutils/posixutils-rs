//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C89 Storage Classes Mega-Test
//
// Consolidates: storage.rs + static_local.rs tests
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: C89 storage classes (auto, static, register, extern)
// ============================================================================

#[test]
fn c89_storage_mega() {
    let code = r#"
// Extern declaration
extern int external_var;
int external_var = 42;

// Static file-scope
static int file_static = 100;

// Static function counter
int call_counter(void) {
    static int count = 0;
    count++;
    return count;
}

// Static with initialization
int get_static_array_sum(void) {
    static int arr[3] = {10, 20, 12};
    return arr[0] + arr[1] + arr[2];
}

// Multiple static locals in same function
int multi_static(int op) {
    static int a = 10;
    static int b = 0;

    if (op == 0) {
        a += 1;
        return a;
    } else {
        b += 5;
        return b;
    }
}

// Static struct
int get_static_point_sum(void) {
    static struct { int x; int y; } point = {10, 32};
    return point.x + point.y;
}

// Const storage
const int const_global = 42;

// Register hint (may be ignored by compiler)
int sum_with_register(int n) {
    register int sum = 0;
    register int i;
    for (i = 1; i <= n; i++) {
        sum += i;
    }
    return sum;
}

int main(void) {
    // ========== AUTO (DEFAULT) STORAGE (returns 1-19) ==========
    {
        // auto is the default for local variables
        auto int a = 10;
        if (a != 10) return 1;

        // Multiple auto declarations
        auto int x = 1, y = 2, z = 3;
        if (x + y + z != 6) return 2;

        // Auto is function-scoped
        {
            auto int inner = 42;
            if (inner != 42) return 3;
        }
        // inner not accessible here

        // Auto array
        auto int arr[3] = {1, 2, 3};
        if (arr[0] + arr[1] + arr[2] != 6) return 4;
    }

    // ========== STATIC LOCAL STORAGE (returns 20-49) ==========
    {
        // Static local persists across calls
        int c1 = call_counter();
        int c2 = call_counter();
        int c3 = call_counter();
        if (c1 != 1) return 20;
        if (c2 != 2) return 21;
        if (c3 != 3) return 22;

        // Static array initialization
        if (get_static_array_sum() != 42) return 23;

        // Multiple static locals
        int m1 = multi_static(0);  // a = 11
        int m2 = multi_static(1);  // b = 5
        int m3 = multi_static(0);  // a = 12
        int m4 = multi_static(1);  // b = 10
        if (m1 != 11) return 24;
        if (m2 != 5) return 25;
        if (m3 != 12) return 26;
        if (m4 != 10) return 27;

        // Static struct
        if (get_static_point_sum() != 42) return 28;

        // Static in block scope
        {
            static int block_static = 50;
            if (block_static != 50) return 29;
            block_static = 60;
        }
        // block_static persists but is not accessible here
    }

    // ========== STATIC FILE SCOPE (returns 50-59) ==========
    {
        // Access file-scope static
        if (file_static != 100) return 50;

        // Modify file-scope static
        file_static = 200;
        if (file_static != 200) return 51;
        file_static = 100;  // Reset
    }

    // ========== EXTERN STORAGE (returns 60-69) ==========
    {
        // Access extern variable
        if (external_var != 42) return 60;

        // Modify extern variable
        external_var = 100;
        if (external_var != 100) return 61;
        external_var = 42;  // Reset
    }

    // ========== REGISTER STORAGE (returns 70-79) ==========
    {
        // Register hint (compiler may ignore)
        register int r = 42;
        if (r != 42) return 70;

        // Register in loop
        if (sum_with_register(5) != 15) return 71;  // 1+2+3+4+5

        // Note: cannot take address of register variable
        // &r would be invalid
    }

    // ========== CONST STORAGE (returns 80-89) ==========
    {
        // Const global
        if (const_global != 42) return 80;

        // Const local
        const int local_const = 100;
        if (local_const != 100) return 81;

        // Const pointer to non-const
        int x = 10;
        int * const cp = &x;
        *cp = 20;
        if (x != 20) return 82;

        // Pointer to const
        const int ci = 30;
        const int *pci = &ci;
        if (*pci != 30) return 83;

        // Const array
        const int carr[3] = {1, 2, 3};
        if (carr[0] + carr[1] + carr[2] != 6) return 84;
    }

    // ========== VOLATILE STORAGE (returns 90-99) ==========
    {
        // Volatile prevents optimization
        volatile int v = 42;
        if (v != 42) return 90;

        v = 100;
        if (v != 100) return 91;

        // Volatile pointer
        volatile int *vp = &v;
        *vp = 200;
        if (v != 200) return 92;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_storage_mega", code, &[]), 0);
}

/// A declaration inside a function body with `extern` declares no object: it
/// refers to one with external linkage (C17 6.2.2p4). c17 used to fall through
/// to the ordinary automatic-storage path and give it a stack slot, so reads
/// returned whatever the frame held and writes never reached the real object.
/// No shadowing is required to see it.
#[test]
fn c89_block_scope_extern_refers_to_the_global() {
    let code = r#"
int g = 5;
long long wide = 1234567890123LL;
double dg = 2.5;
int arr[3] = {7, 8, 9};

int read_in_function_body(void) { extern int g; return g; }
int read_in_nested_block(void)  { { extern int g; return g; } }
int read_without_extern(void)   { return g; }

int write_through_extern(void)  { { extern int g; g = 42; } return 0; }

long long read_wide(void) { extern long long wide; return wide; }
double read_double(void)  { extern double dg; return dg; }
int read_array(void)      { extern int arr[3]; return arr[0] + arr[1] + arr[2]; }

/* The address must be the global's, not a slot's. */
int *addr_of_extern(void) { extern int g; return &g; }

/* A block-scope extern whose name is shadowed by a parameter still names the
   global; the parameter is untouched. */
int shadowed(int g) {
    g = 1;
    { extern int g; g = 77; }
    return g;                 /* the parameter */
}

/* The same, with the parameter's address taken so it cannot be promoted out
   of memory. It then keeps a stack slot registered under its bare name, which
   is what a same-named global's symbol collides with in the register
   allocator -- a distinct failure from the one above. */
int *keep;
int shadowed_addr_taken(int g) {
    keep = &g;
    *keep = 1;
    { extern int g; g = 88; }
    return *keep;             /* the parameter */
}

int main(void) {
    if (read_in_function_body() != 5) return 1;
    if (read_in_nested_block()  != 5) return 2;
    if (read_without_extern()   != 5) return 3;

    write_through_extern();
    if (g != 42) return 4;
    g = 5;

    if (read_wide() != 1234567890123LL) return 5;
    if (read_double() != 2.5) return 6;
    if (read_array() != 24) return 7;

    if (addr_of_extern() != &g) return 8;

    if (shadowed(3) != 1) return 9;
    if (g != 77) return 10;

    g = 5;
    if (shadowed_addr_taken(3) != 1) return 11;
    if (g != 88) return 12;

    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_block_scope_extern", code, &[]), 0);
}

/// A function declared inside a function body has external linkage whether or
/// not `extern` is spelled, so it must not get a stack slot either. Calling it
/// worked regardless -- calls resolve by name -- but using it as a value took
/// the address of the slot.
#[test]
fn c89_block_scope_function_declaration_is_not_an_object() {
    let code = r#"
int target(void) { return 77; }

int call_after_extern_decl(void) { extern int target(void); return target(); }
int call_after_plain_decl(void)  { int target(void); return target(); }

int via_pointer(void) {
    extern int target(void);
    int (*p)(void) = target;
    return p();
}
int via_address(void) {
    int target(void);
    int (*p)(void) = &target;
    return p();
}
int pointer_identity(void) {
    extern int target(void);
    int (*a)(void) = target;
    int (*b)(void) = &target;
    return a == b && a == target;
}

int main(void) {
    if (call_after_extern_decl() != 77) return 1;
    if (call_after_plain_decl()  != 77) return 2;
    if (via_pointer()            != 77) return 3;
    if (via_address()            != 77) return 4;
    if (!pointer_identity())          return 5;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c89_block_scope_fn_decl", code, &[]), 0);
}
