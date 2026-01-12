//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for C99 pointer types
//
// C99 pointer semantics:
// - Pointers hold memory addresses of objects
// - Pointer arithmetic scales by the size of the pointed-to type
// - void* is a generic pointer that can point to any object
// - Null pointers compare equal to 0
// - Arrays decay to pointers in most contexts
// - Pointers can be compared with relational operators
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// Basic Pointer Operations: address-of, dereference, modify, reassign
// ============================================================================

#[test]
fn pointer_basic_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Basic int pointer
    int x = 42;
    int *p = &x;
    if (*p != 42) return 1;

    // Test 2: Basic char pointer
    char c = 'A';
    char *cp = &c;
    if (*cp != 'A') return 2;

    // Test 3: Basic long pointer
    long lx = 123456789L;
    long *lp = &lx;
    if (*lp != 123456789L) return 3;

    // Test 4: Modify through pointer
    int y = 10;
    int *py = &y;
    *py = 20;
    if (y != 20) return 4;

    // Test 5: Pointer reassignment
    int a = 10;
    int b = 20;
    int *pa = &a;
    if (*pa != 10) return 5;
    pa = &b;
    if (*pa != 20) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ptr_basic_comprehensive", code, &[]), 0);
}

// ============================================================================
// Pointer Arithmetic: add, sub, increment, decrement, compound, difference
// ============================================================================

#[test]
fn pointer_arithmetic_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Addition
    int arr[5] = {10, 20, 30, 40, 50};
    int *p = arr;
    if (*(p + 0) != 10) return 1;
    if (*(p + 1) != 20) return 2;
    if (*(p + 2) != 30) return 3;
    if (*(p + 3) != 40) return 4;
    if (*(p + 4) != 50) return 5;

    // Test 2: Subtraction
    int *p2 = &arr[4];
    if (*(p2 - 0) != 50) return 6;
    if (*(p2 - 1) != 40) return 7;
    if (*(p2 - 2) != 30) return 8;
    if (*(p2 - 3) != 20) return 9;
    if (*(p2 - 4) != 10) return 10;

    // Test 3: Increment
    int arr2[3] = {1, 2, 3};
    int *p3 = arr2;
    if (*p3 != 1) return 11;
    p3++;
    if (*p3 != 2) return 12;
    p3++;
    if (*p3 != 3) return 13;

    // Test 4: Decrement
    int *p4 = &arr2[2];
    if (*p4 != 3) return 14;
    p4--;
    if (*p4 != 2) return 15;
    p4--;
    if (*p4 != 1) return 16;

    // Test 5: Compound add
    int *p5 = arr;
    p5 += 2;
    if (*p5 != 30) return 17;
    p5 += 2;
    if (*p5 != 50) return 18;

    // Test 6: Compound sub
    int *p6 = &arr[4];
    p6 -= 2;
    if (*p6 != 30) return 19;
    p6 -= 2;
    if (*p6 != 10) return 20;

    // Test 7: Pointer difference
    int *p7 = &arr[0];
    int *p8 = &arr[4];
    long diff = p8 - p7;
    if (diff != 4) return 21;
    diff = p7 - p8;
    if (diff != -4) return 22;

    // Test 8: Char arithmetic
    char carr[4] = {'a', 'b', 'c', 'd'};
    char *cp = carr;
    if (*(cp + 0) != 'a') return 23;
    if (*(cp + 1) != 'b') return 24;
    if (*(cp + 2) != 'c') return 25;
    if (*(cp + 3) != 'd') return 26;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ptr_arith_comprehensive", code, &[]), 0);
}

// ============================================================================
// Pointer Comparisons and NULL Pointers
// ============================================================================

#[test]
fn pointer_comparison_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Equal pointers
    int x = 42;
    int *p1 = &x;
    int *p2 = &x;
    if (p1 != p2) return 1;
    if (!(p1 == p2)) return 2;

    // Test 2: Not equal pointers
    int y = 2;
    int *p3 = &x;
    int *p4 = &y;
    if (p3 == p4) return 3;
    if (!(p3 != p4)) return 4;

    // Test 3: Relational comparisons
    int arr[3] = {1, 2, 3};
    int *pa = &arr[0];
    int *pb = &arr[2];
    if (!(pa < pb)) return 5;
    if (!(pa <= pb)) return 6;
    if (!(pb > pa)) return 7;
    if (!(pb >= pa)) return 8;

    // Equal pointers with <= and >=
    int *pc = &arr[1];
    int *pd = &arr[1];
    if (!(pc <= pd)) return 9;
    if (!(pc >= pd)) return 10;

    // Test 4: NULL pointer init
    int *pnull = 0;
    if (pnull != 0) return 11;

    // Test 5: NULL pointer comparison
    int *pn = 0;
    if (pn) return 12;  // Should be false
    if (pn != 0) return 13;
    if (!(pn == 0)) return 14;

    int z = 42;
    pn = &z;
    if (!pn) return 15;  // Should be true now
    if (pn == 0) return 16;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ptr_cmp_comprehensive", code, &[]), 0);
}

// ============================================================================
// Void Pointers
// ============================================================================

#[test]
fn void_pointer_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Basic void pointer
    int x = 42;
    void *vp = &x;
    int *ip = vp;
    if (*ip != 42) return 1;

    // Test 2: Multiple types
    int i = 10;
    char c = 'X';
    long l = 12345L;

    vp = &i;
    int *ip2 = vp;
    if (*ip2 != 10) return 2;

    vp = &c;
    char *cp = vp;
    if (*cp != 'X') return 3;

    vp = &l;
    long *lp = vp;
    if (*lp != 12345L) return 4;

    // Test 3: NULL void pointer
    void *vnull = 0;
    if (vnull != 0) return 5;

    int y = 1;
    vnull = &y;
    if (vnull == 0) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("void_ptr_comprehensive", code, &[]), 0);
}

// ============================================================================
// Double/Nested Pointers
// ============================================================================

#[test]
fn pointer_double_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Basic double pointer
    int x = 42;
    int *p = &x;
    int **pp = &p;
    if (**pp != 42) return 1;

    // Test 2: Modify through double pointer
    int y = 10;
    int *py = &y;
    int **ppy = &py;
    **ppy = 20;
    if (y != 20) return 2;
    if (*py != 20) return 3;

    // Test 3: Reassign through double pointer
    int a = 10;
    int b = 20;
    int *p1 = &a;
    int *p2 = &b;
    int **pp2 = &p1;
    if (**pp2 != 10) return 4;
    *pp2 = p2;  // Now pp2 points to p2
    if (**pp2 != 20) return 5;

    // Test 4: Triple pointer
    int z = 42;
    int *pz = &z;
    int **ppz = &pz;
    int ***pppz = &ppz;
    if (***pppz != 42) return 6;
    ***pppz = 100;
    if (z != 100) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ptr_double_comprehensive", code, &[]), 0);
}

// ============================================================================
// Char Pointers and String Literals
// ============================================================================

#[test]
fn char_pointer_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Char pointer to array
    char arr[5] = {'H', 'e', 'l', 'l', 'o'};
    char *p = arr;
    if (p[0] != 'H') return 1;
    if (p[1] != 'e') return 2;
    if (p[2] != 'l') return 3;
    if (p[3] != 'l') return 4;
    if (p[4] != 'o') return 5;

    // Test 2: String literal
    char *s = "Hello";
    if (s[0] != 'H') return 6;
    if (s[1] != 'e') return 7;
    if (s[2] != 'l') return 8;
    if (s[3] != 'l') return 9;
    if (s[4] != 'o') return 10;
    if (s[5] != 0) return 11;  // Null terminator

    // Test 3: Walk string
    char *s2 = "abc";
    int count = 0;
    while (*s2 != 0) {
        count++;
        s2++;
    }
    if (count != 3) return 12;

    // Test 4: Multiple strings
    char *s3 = "ABC";
    char *s4 = "XYZ";
    if (s3[0] != 'A') return 13;
    if (s4[0] != 'X') return 14;
    if (s3[2] != 'C') return 15;
    if (s4[2] != 'Z') return 16;

    return 0;
}
"#;
    assert_eq!(compile_and_run("char_ptr_comprehensive", code, &[]), 0);
}

// ============================================================================
// Arrays of Pointers and Pointer to Array
// ============================================================================

#[test]
fn pointer_array_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Array of int pointers
    int a = 1, b = 2, c = 3;
    int *arr[3];
    arr[0] = &a;
    arr[1] = &b;
    arr[2] = &c;
    if (*arr[0] != 1) return 1;
    if (*arr[1] != 2) return 2;
    if (*arr[2] != 3) return 3;

    // Test 2: Array of char pointers
    char *carr[3];
    carr[0] = "one";
    carr[1] = "two";
    carr[2] = "three";
    if (carr[0][0] != 'o') return 4;
    if (carr[1][0] != 't') return 5;
    if (carr[2][0] != 't') return 6;
    if (carr[2][2] != 'r') return 7;

    // Test 3: Array of pointers with initializer
    int d = 10, e = 20, f = 30;
    int *iarr[3] = {&d, &e, &f};
    if (*iarr[0] != 10) return 8;
    if (*iarr[1] != 20) return 9;
    if (*iarr[2] != 30) return 10;

    // Test 4: Array of string pointers
    char *strs[3] = {"apple", "banana", "cherry"};
    if (strs[0][0] != 'a') return 11;
    if (strs[1][0] != 'b') return 12;
    if (strs[2][0] != 'c') return 13;
    if (strs[0][1] != 'p') return 14;
    if (strs[1][1] != 'a') return 15;
    if (strs[2][1] != 'h') return 16;

    // Test 5: Pointer to array
    int parr[3] = {1, 2, 3};
    int (*p)[3] = &parr;
    if ((*p)[0] != 1) return 17;
    if ((*p)[1] != 2) return 18;
    if ((*p)[2] != 3) return 19;

    // Test 6: Pointer to 2D array
    int arr2d[2][3] = {{1, 2, 3}, {4, 5, 6}};
    int (*p2d)[3] = arr2d;
    if ((*p2d)[0] != 1) return 20;
    if ((*p2d)[1] != 2) return 21;
    if ((*p2d)[2] != 3) return 22;
    p2d++;
    if ((*p2d)[0] != 4) return 23;
    if ((*p2d)[1] != 5) return 24;
    if ((*p2d)[2] != 6) return 25;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ptr_array_comprehensive", code, &[]), 0);
}

// ============================================================================
// Pointer Parameters and Return Values
// ============================================================================

#[test]
fn pointer_param_return_comprehensive() {
    let code = r#"
int read_ptr(int *p) {
    return *p;
}

void write_ptr(int *p, int val) {
    *p = val;
}

void swap(int *a, int *b) {
    int tmp = *a;
    *a = *b;
    *b = tmp;
}

void set_ptr(int **pp, int *new_val) {
    *pp = new_val;
}

int global_val = 42;

int* get_global_ptr(void) {
    return &global_val;
}

int* identity(int *p) {
    return p;
}

int* select_ptr(int *a, int *b, int use_first) {
    if (use_first) {
        return a;
    }
    return b;
}

int main(void) {
    // Test 1: Read through pointer param
    int x = 42;
    if (read_ptr(&x) != 42) return 1;

    // Test 2: Write through pointer param
    int y = 0;
    write_ptr(&y, 42);
    if (y != 42) return 2;

    // Test 3: Swap
    int a = 10;
    int b = 20;
    swap(&a, &b);
    if (a != 20) return 3;
    if (b != 10) return 4;

    // Test 4: Double pointer param
    int c = 10;
    int d = 20;
    int *p = &c;
    if (*p != 10) return 5;
    set_ptr(&p, &d);
    if (*p != 20) return 6;

    // Test 5: Return global pointer
    int *gp = get_global_ptr();
    if (*gp != 42) return 7;
    *gp = 100;
    if (global_val != 100) return 8;

    // Test 6: Return param pointer
    int e = 42;
    int *pe = identity(&e);
    if (*pe != 42) return 9;

    // Test 7: Conditional return
    int f = 10;
    int g = 20;
    int *ps = select_ptr(&f, &g, 1);
    if (*ps != 10) return 10;
    ps = select_ptr(&f, &g, 0);
    if (*ps != 20) return 11;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ptr_param_ret_comprehensive", code, &[]), 0);
}

// ============================================================================
// Pointer Casts and Structs
// ============================================================================

#[test]
fn pointer_cast_struct_comprehensive() {
    let code = r#"
struct Point {
    int x;
    int y;
};

struct Container {
    int *ptr;
    int val;
};

struct Item {
    int value;
};

int main(void) {
    // Test 1: Cast int* to char*
    int x = 0x44434241;  // "ABCD" in little-endian
    char *cp = (char*)&x;
    if (cp[0] != 'A') return 1;
    if (cp[1] != 'B') return 2;
    if (cp[2] != 'C') return 3;
    if (cp[3] != 'D') return 4;

    // Test 2: Cast to void* and back
    int y = 42;
    void *vp = (void*)&y;
    int *ip = (int*)vp;
    if (*ip != 42) return 5;

    // Test 3: Pointer to struct
    struct Point p = {10, 20};
    struct Point *ptr = &p;
    if (ptr->x != 10) return 6;
    if (ptr->y != 20) return 7;
    ptr->x = 30;
    ptr->y = 40;
    if (p.x != 30) return 8;
    if (p.y != 40) return 9;

    // Test 4: Pointer in struct
    int z = 42;
    struct Container c;
    c.ptr = &z;
    c.val = 10;
    if (*c.ptr != 42) return 10;
    if (c.val != 10) return 11;
    *c.ptr = 100;
    if (z != 100) return 12;

    // Test 5: Pointer to struct array
    struct Item items[3] = {{10}, {20}, {30}};
    struct Item *pi = items;
    if (pi->value != 10) return 13;
    pi++;
    if (pi->value != 20) return 14;
    pi++;
    if (pi->value != 30) return 15;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("ptr_cast_struct_comprehensive", code, &[]),
        0
    );
}

// ============================================================================
// Sizeof, Expressions, and Edge Cases
// ============================================================================

#[test]
fn pointer_expr_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Sizeof pointers (64-bit)
    if (sizeof(int*) != 8) return 1;
    if (sizeof(char*) != 8) return 2;
    if (sizeof(long*) != 8) return 3;
    if (sizeof(void*) != 8) return 4;
    int *p;
    if (sizeof(p) != 8) return 5;

    // Test 2: Address of dereference
    int x = 42;
    int *px = &x;
    if (&*px != px) return 6;
    if (*&x != 42) return 7;

    // Test 3: Complex expressions
    int arr[3] = {10, 20, 30};
    int *pa = arr;
    if (*(pa + 1) != pa[1]) return 8;
    if (*(1 + pa) != pa[1]) return 9;  // commutative
    if (*pa + 1 != 11) return 10;

    // Test 4: Negative index
    int arr2[5] = {1, 2, 3, 4, 5};
    int *pm = &arr2[2];  // Points to arr2[2] = 3
    if (pm[-2] != 1) return 11;
    if (pm[-1] != 2) return 12;
    if (pm[0] != 3) return 13;
    if (pm[1] != 4) return 14;
    if (pm[2] != 5) return 15;

    // Test 5: Self reference
    int y = 42;
    int *py = &y;
    int **ppy = &py;
    if (*ppy != py) return 16;
    if (**ppy != y) return 17;

    // Test 6: Post-increment dereference (*p++)
    int arr3[3] = {10, 20, 30};
    int *pi = arr3;
    int val = *pi++;
    if (val != 10) return 18;
    if (*pi != 20) return 19;
    val = *pi++;
    if (val != 20) return 20;
    if (*pi != 30) return 21;

    // Test 7: Pre-increment dereference (*++p)
    int arr4[3] = {10, 20, 30};
    int *pj = arr4;
    val = *++pj;
    if (val != 20) return 22;
    val = *++pj;
    if (val != 30) return 23;

    // Test 8: Dereference then increment ((*p)++)
    int arr5[3] = {10, 20, 30};
    int *pk = arr5;
    val = (*pk)++;
    if (val != 10) return 24;
    if (arr5[0] != 11) return 25;
    if (*pk != 11) return 26;  // pk still points to arr5[0]

    return 0;
}
"#;
    assert_eq!(compile_and_run("ptr_expr_comprehensive", code, &[]), 0);
}

// ============================================================================
// Pointer Aliasing and Const Pointers
// ============================================================================

#[test]
fn pointer_alias_const_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Aliasing
    int x = 10;
    int *p1 = &x;
    int *p2 = &x;

    *p1 = 20;
    if (*p2 != 20) return 1;

    *p2 = 30;
    if (*p1 != 30) return 2;
    if (x != 30) return 3;

    // Test 2: Const pointer (int * const p)
    int y = 10;
    int * const cp = &y;  // Constant pointer to int
    *cp = 15;
    if (y != 15) return 4;
    // Cannot reassign cp (would be compile error)

    // Test 3: Pointer to const (const int *p)
    int a = 10;
    int b = 20;
    const int *pc = &a;  // Pointer to constant int
    pc = &b;  // Can reassign
    if (*pc != 20) return 5;
    // Cannot modify through pc (would be compile error)

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("ptr_alias_const_comprehensive", code, &[]),
        0
    );
}
