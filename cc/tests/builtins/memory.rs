//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Memory Builtins Mega-Test
//
// Consolidates: alloca, offsetof tests
//

use crate::common::{compile_and_run, compile_and_run_everywhere, compile_expect_error};

// ============================================================================
// Mega-test: Memory builtins (alloca, offsetof)
// ============================================================================

/// The memory builtins, and every other program of this file built at the matrix
/// levels with no options, as one program; each section keeps its original test
/// name and doc comment, and the exit-code table is at the top.
///
/// Consolidates: builtins_memory_mega, builtins_object_size_of_known_objects,
/// builtins_fortified_chk_entry_points, and the no-option runs of
/// builtins_bare_alloca and builtins_fno_builtin_disables_bare_spellings_only
/// (both programs of the latter).
#[test]
fn builtins_memory_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1- 58  builtins_memory_mega
 *    61- 76  builtins_object_size_of_known_objects
 *    81- 88  builtins_fortified_chk_entry_points
 *    91- 96  builtins_bare_alloca
 *   101-103  builtins_fno_builtin_disables_bare_spellings_only
 *   111-111  builtins_fno_builtin_disables_bare_spellings_only
 */

/* ---- builtins_memory_mega (exit codes 1-58) ----
 */
#include <stddef.h>

struct TestStruct {
    char a;     // offset 0
    int b;      // offset 4 (padded)
    char c;     // offset 8
    double d;   // offset 16 (padded)
    short e;    // offset 24
};

struct Packed {
    char a;
    char b;
    char c;
};

struct Nested {
    int x;
    struct {
        int y;
        int z;
    } inner;
    int w;
};

int test_alloca_size(int n) {
    int *arr = __builtin_alloca(n * sizeof(int));
    for (int i = 0; i < n; i++) {
        arr[i] = i * 10;
    }
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
    return sum;
}

static int t_builtins_memory_mega(void) {
    // ========== ALLOCA SECTION (returns 1-39) ==========
    {
        // Basic allocation
        char *buf = __builtin_alloca(64);
        buf[0] = 'A';
        buf[1] = 'B';
        buf[2] = 'C';
        buf[3] = '\0';
        if (buf[0] != 'A') return 1;
        if (buf[1] != 'B') return 2;
        if (buf[2] != 'C') return 3;

        // Multiple allocations don't interfere
        int *arr1 = __builtin_alloca(sizeof(int) * 4);
        int *arr2 = __builtin_alloca(sizeof(int) * 4);
        arr1[0] = 100; arr1[1] = 200; arr1[2] = 300; arr1[3] = 400;
        arr2[0] = 1000; arr2[1] = 2000; arr2[2] = 3000; arr2[3] = 4000;
        if (arr1[0] != 100) return 4;
        if (arr1[3] != 400) return 5;
        if (arr2[0] != 1000) return 6;
        if (arr2[3] != 4000) return 7;

        // Alignment (16-byte)
        void *p1 = __builtin_alloca(1);
        void *p2 = __builtin_alloca(7);
        void *p3 = __builtin_alloca(16);
        void *p4 = __builtin_alloca(17);
        if (((long)p1 & 0xF) != 0) return 8;
        if (((long)p2 & 0xF) != 0) return 9;
        if (((long)p3 & 0xF) != 0) return 10;
        if (((long)p4 & 0xF) != 0) return 11;

        // Computed size
        if (test_alloca_size(5) != 100) return 12;   // 0+10+20+30+40
        if (test_alloca_size(10) != 450) return 13;  // 0+10+...+90

        // Allocation in loop
        int total = 0;
        for (int i = 0; i < 5; i++) {
            int *p = __builtin_alloca(sizeof(int) * 4);
            p[0] = i;
            p[1] = i * 2;
            p[2] = i * 3;
            p[3] = i * 4;
            total += p[0] + p[1] + p[2] + p[3];
        }
        if (total != 100) return 14;
    }

    // ========== OFFSETOF SECTION (returns 40-69) ==========
    {
        // Basic offsetof
        if (offsetof(struct TestStruct, a) != 0) return 40;
        if (offsetof(struct TestStruct, b) != 4) return 41;
        if (offsetof(struct TestStruct, c) != 8) return 42;
        if (offsetof(struct TestStruct, d) != 16) return 43;
        if (offsetof(struct TestStruct, e) != 24) return 44;

        // Packed struct (no padding between chars)
        if (offsetof(struct Packed, a) != 0) return 45;
        if (offsetof(struct Packed, b) != 1) return 46;
        if (offsetof(struct Packed, c) != 2) return 47;

        // Nested struct
        if (offsetof(struct Nested, x) != 0) return 48;
        if (offsetof(struct Nested, inner) != 4) return 49;
        if (offsetof(struct Nested, inner.y) != 4) return 50;
        if (offsetof(struct Nested, inner.z) != 8) return 51;
        if (offsetof(struct Nested, w) != 12) return 52;

        // __builtin_offsetof (GCC extension)
        if (__builtin_offsetof(struct TestStruct, a) != 0) return 53;
        if (__builtin_offsetof(struct TestStruct, b) != 4) return 54;

        // sizeof struct members via offsetof pattern
        long size_b = offsetof(struct TestStruct, c) - offsetof(struct TestStruct, b);
        if (size_b != 4) return 55;

        // Array in struct
        struct WithArray { int x; int arr[5]; int y; };
        if (offsetof(struct WithArray, arr) != 4) return 56;
        if (offsetof(struct WithArray, y) != 24) return 57;  // 4 + 5*4

        // Compile-time constant usage
        char buf[offsetof(struct TestStruct, d)];  // char buf[16]
        if (sizeof(buf) != 16) return 58;
    }

    return 0;
}


/* ---- builtins_object_size_of_known_objects (exit codes 61-76) ----
 *
 *  `__builtin_object_size` reports the real size of a statically known object.
 *
 *  Every expectation was taken from gcc on the same source. Before this was
 *  implemented the builtin answered `(size_t)-1` -- "unknown" -- for all of
 *  them, which is what `_FORTIFY_SOURCE` reads as "nothing to check".
 *
 *  The type argument matters twice over: bit 0 selects the closest surrounding
 *  subobject over the whole object (`s.a` is 10 bytes of its own but 36 bytes
 *  to the end of `s`), and bit 1 asks for a minimum instead of a maximum,
 *  which only changes the answer when the object is *not* known -- 0 rather
 *  than `(size_t)-1`.
 */
struct os_S { char a[10]; int b; char c[20]; };   /* sizeof == 36 */
static char os_g_arr[64];
static struct os_S os_g_s;
static char *os_opaque(char *p) { return p; }

static int t_builtins_object_size_of_known_objects(void)
{
    char local[32];
    struct os_S ls;

    /* type 0: to the end of the whole object */
    if (__builtin_object_size(os_g_arr, 0) != 64) return 1;
    if (__builtin_object_size(os_g_arr + 8, 0) != 56) return 2;
    if (__builtin_object_size(local, 0) != 32) return 3;
    if (__builtin_object_size(os_g_s.a, 0) != 36) return 4;
    if (__builtin_object_size(os_g_s.c, 0) != 20) return 5;
    if (__builtin_object_size(&ls.a[2], 0) != 34) return 6;
    if (__builtin_object_size(&os_g_s, 0) != 36) return 7;

    /* type 1: to the end of the closest surrounding subobject */
    if (__builtin_object_size(os_g_s.a, 1) != 10) return 8;
    if (__builtin_object_size(os_g_arr, 1) != 64) return 9;

    /* a known object has the same minimum and maximum size */
    if (__builtin_object_size(os_g_arr, 2) != 64) return 10;
    if (__builtin_object_size(os_g_s.a, 3) != 10) return 11;

    /* unknown: -1 for a maximum, 0 for a minimum */
    if (__builtin_object_size(os_opaque(local), 0) != (unsigned long)-1) return 12;
    if (__builtin_object_size(os_opaque(local), 1) != (unsigned long)-1) return 13;
    if (__builtin_object_size(os_opaque(local), 2) != 0) return 14;
    if (__builtin_object_size(os_opaque(local), 3) != 0) return 15;

    /* a string literal is its bytes plus the terminator */
    if (__builtin_object_size("hello", 0) != 6) return 16;

    return 0;
}


/* ---- builtins_fortified_chk_entry_points (exit codes 81-88) ----
 *
 *  The fortified `__builtin___*_chk` builtins work without a declaration.
 *
 *  glibc never declares `__memcpy_chk` and friends -- `bits/string_fortified.h`
 *  calls `__builtin___memcpy_chk` and relies on the compiler knowing the
 *  entry point intrinsically. c17 rewrote the name and then demanded a
 *  declaration that does not exist, so every one of them was an error.
 *
 *  The return type is the part that has to be right rather than merely
 *  present: most of these return a pointer, and declaring one `int` would
 *  truncate the returned address to 32 bits. The assertions below compare the
 *  returned pointer against the expected address for exactly that reason.
 *
 *  Checked against gcc on the same source, which returns 0.
 */
#include <string.h>
#include <stdio.h>

static int t_builtins_fortified_chk_entry_points(void)
{
    char buf[32];

    char *p = __builtin___strcpy_chk(buf, "hello", sizeof buf);
    if (p != buf) return 1;
    if (strcmp(buf, "hello") != 0) return 2;

    void *q = __builtin___memcpy_chk(buf + 6, "world", 6, sizeof buf - 6);
    if (q != buf + 6) return 3;
    if (strcmp(buf + 6, "world") != 0) return 4;

    void *r = __builtin___memset_chk(buf, 'x', 4, sizeof buf);
    if (r != buf) return 5;
    if (buf[0] != 'x' || buf[3] != 'x' || buf[4] != 'o') return 6;

    int n = __builtin___snprintf_chk(buf, sizeof buf, 0, sizeof buf, "%d", 12345);
    if (n != 5) return 7;
    if (strcmp(buf, "12345") != 0) return 8;

    return 0;
}


/* ---- builtins_bare_alloca (exit codes 91-96) ----
 *
 *  gcc predefines bare `alloca` as well as `__builtin_alloca`, and real code
 *  calls it without including `<alloca.h>`.
 *
 *  It is not reserved the way a `__builtin_*` spelling is, so unlike those it
 *  must yield to a user declaration that is not a function -- the same rule
 *  `setjmp`, `longjmp` and `offsetof` already follow in `builtin_is_shadowed`.
 *  A declaration from `<alloca.h>` *is* a function and must not displace it,
 *  which is why the predicate asks what kind of declaration it found rather
 *  than merely whether one exists.
 */
int ba_use_alloca(int n) {
    int *p = (int *)alloca(n * sizeof(int));
    for (int i = 0; i < n; i++) p[i] = i * 2;
    int s = 0;
    for (int i = 0; i < n; i++) s += p[i];
    return s;
}

/* A local object named `alloca` is an ordinary variable. */
int ba_shadowed_by_a_variable(void) {
    int alloca = 7;
    return alloca;
}

static int t_builtins_bare_alloca(void) {
    if (ba_use_alloca(5) != 0 + 2 + 4 + 6 + 8) return 1;
    if (ba_use_alloca(1) != 0) return 2;
    if (ba_shadowed_by_a_variable() != 7) return 3;

    /* The reserved spelling keeps working alongside the bare one, and both
       give storage that survives to the end of the enclosing function. */
    char *q = (char *)__builtin_alloca(16);
    q[0] = 'a';
    q[15] = 'z';
    if (q[0] != 'a' || q[15] != 'z') return 4;

    char *r = (char *)alloca(16);
    r[0] = 'b';
    if (r[0] != 'b' || q[0] != 'a') return 5;
    if (r == q) return 6;
    return 0;
}


/* ---- builtins_fno_builtin_disables_bare_spellings_only (exit codes 101-103) ----
 *
 *  `-fno-builtin` and `-fno-builtin-NAME`.
 *
 *  gcc's rule, which this follows exactly: the flag disables builtins **whose
 *  name does not begin with `__builtin_`**. The reserved spellings keep
 *  working and `__has_builtin` keeps answering 1 for them — verified against
 *  gcc, which compiles `__builtin_strcpy` under `-fno-builtin` and fails to
 *  link a bare `alloca`.
 *
 *  So the flag lands on exactly the bare names c17 answers to: `alloca`,
 *  `offsetof`, `setjmp`, `longjmp`. Those are also the only names a user
 *  declaration may displace, which is the same boundary for the same reason —
 *  they are not reserved to the implementation.
 *
 *  Both driver fields were parsed into variables nothing read, so the flag was
 *  accepted and did nothing, which is indistinguishable from it working.
 */
static int t_builtins_nb_reserved(void) {
    char b[8];
    __builtin_strcpy(b, "hi");
    if (!__has_builtin(__builtin_strcpy)) return 1;
    if (b[0] != 'h' || b[1] != 'i') return 2;
    char *p = (char *)__builtin_alloca(16);
    p[0] = 'z';
    if (p[0] != 'z') return 3;
    return 0;
}


/* ---- builtins_fno_builtin_disables_bare_spellings_only (exit codes 111-111) ----
 */
static int t_builtins_nb_bare_on(void) {
    char *p = (char *)alloca(16);
    p[0] = 'q';
    return p[0] != 'q';
}

int main(void)
{
    int r;
    if ((r = t_builtins_memory_mega()) != 0)
        return 0 + r;
    if ((r = t_builtins_object_size_of_known_objects()) != 0)
        return 60 + r;
    if ((r = t_builtins_fortified_chk_entry_points()) != 0)
        return 80 + r;
    if ((r = t_builtins_bare_alloca()) != 0)
        return 90 + r;
    if ((r = t_builtins_nb_reserved()) != 0)
        return 100 + r;
    if ((r = t_builtins_nb_bare_on()) != 0)
        return 110 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_memory_mega", code, &[]), 0);
}

/// gcc predefines bare `alloca` as well as `__builtin_alloca`, and real code
/// calls it without including `<alloca.h>`.
///
/// It is not reserved the way a `__builtin_*` spelling is, so unlike those it
/// must yield to a user declaration that is not a function -- the same rule
/// `setjmp`, `longjmp` and `offsetof` already follow in `builtin_is_shadowed`.
/// A declaration from `<alloca.h>` *is* a function and must not displace it,
/// which is why the predicate asks what kind of declaration it found rather
/// than merely whether one exists.
#[test]
fn builtins_bare_alloca() {
    let code = r#"
int use_alloca(int n) {
    int *p = (int *)alloca(n * sizeof(int));
    for (int i = 0; i < n; i++) p[i] = i * 2;
    int s = 0;
    for (int i = 0; i < n; i++) s += p[i];
    return s;
}

/* A local object named `alloca` is an ordinary variable. */
int shadowed_by_a_variable(void) {
    int alloca = 7;
    return alloca;
}

int main(void) {
    if (use_alloca(5) != 0 + 2 + 4 + 6 + 8) return 1;
    if (use_alloca(1) != 0) return 2;
    if (shadowed_by_a_variable() != 7) return 3;

    /* The reserved spelling keeps working alongside the bare one, and both
       give storage that survives to the end of the enclosing function. */
    char *q = (char *)__builtin_alloca(16);
    q[0] = 'a';
    q[15] = 'z';
    if (q[0] != 'a' || q[15] != 'z') return 4;

    char *r = (char *)alloca(16);
    r[0] = 'b';
    if (r[0] != 'b' || q[0] != 'a') return 5;
    if (r == q) return 6;
    return 0;
}
"#;
    // The no-option run is a section of builtins_memory_mega.
    assert_eq!(
        compile_and_run("builtins_bare_alloca_o2", code, &["-O2".to_string()]),
        0
    );
}

/// `-fno-builtin` and `-fno-builtin-NAME`.
///
/// gcc's rule, which this follows exactly: the flag disables builtins **whose
/// name does not begin with `__builtin_`**. The reserved spellings keep
/// working and `__has_builtin` keeps answering 1 for them — verified against
/// gcc, which compiles `__builtin_strcpy` under `-fno-builtin` and fails to
/// link a bare `alloca`.
///
/// So the flag lands on exactly the bare names c17 answers to: `alloca`,
/// `offsetof`, `setjmp`, `longjmp`. Those are also the only names a user
/// declaration may displace, which is the same boundary for the same reason —
/// they are not reserved to the implementation.
///
/// Both driver fields were parsed into variables nothing read, so the flag was
/// accepted and did nothing, which is indistinguishable from it working.
#[test]
fn builtins_fno_builtin_disables_bare_spellings_only() {
    // The reserved spelling is unaffected, with the flag and without.
    let reserved = r#"
int main(void) {
    char b[8];
    __builtin_strcpy(b, "hi");
    if (!__has_builtin(__builtin_strcpy)) return 1;
    if (b[0] != 'h' || b[1] != 'i') return 2;
    char *p = (char *)__builtin_alloca(16);
    p[0] = 'z';
    if (p[0] != 'z') return 3;
    return 0;
}
"#;
    // Its no-option run is a section of builtins_memory_mega.
    assert_eq!(
        compile_and_run(
            "builtins_nb_reserved_off",
            reserved,
            &["-fno-builtin".to_string()]
        ),
        0
    );

    // The bare spelling still works when the flag is absent: that program
    // is a section of builtins_memory_mega.
}

/// The other half: under the flag, a bare spelling is an ordinary call, so it
/// needs a declaration and a definition like any other function.
#[test]
fn builtins_fno_builtin_makes_a_bare_name_an_ordinary_call() {
    // `offsetof` is the clearest case — with the builtin off, the name is the
    // caller's to define, and the program must use that definition.
    let code = r#"
struct S { int a; long b; };
static unsigned long offsetof(int which) { return which == 0 ? 111 : 222; }

int main(void) {
    if (offsetof(0) != 111) return 1;
    if (offsetof(1) != 222) return 2;
    /* The reserved spelling still reaches the real builtin. */
    if (__builtin_offsetof(struct S, b) != sizeof(long)) return 3;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run(
            "builtins_nb_bare_is_ordinary",
            code,
            &["-fno-builtin".to_string()]
        ),
        0
    );

    // And -fno-builtin-NAME reaches just the one name.
    assert_eq!(
        compile_and_run(
            "builtins_nb_named",
            code,
            &["-fno-builtin-offsetof".to_string()]
        ),
        0
    );
}

/// `__builtin_memcpy`, `__builtin_memset` and `__builtin_memmove` actually
/// doing something.
///
/// x86-64 lowered all three in its own `features.rs`. aarch64 lowered none of
/// them: the opcode reached codegen, fell into the arm that skips no-ops, and
/// produced nothing — so `__builtin_memcpy(b, a, 32)` left `b` untouched,
/// silently, on every aarch64 build. A comment claimed they reached the
/// ordinary call path instead; nothing performed that conversion.
///
/// This test runs on the host, so on x86-64 it guards against a regression
/// rather than proving the fix. The fix was verified by building for
/// aarch64 and running under qemu against cross-gcc, which is the only way to
/// see it from here.
#[test]
fn builtins_memory_ops_actually_copy() {
    let code = r#"
int main(void) {
    char a[40], b[40], c[13];
    for (int i = 0; i < 40; i++) a[i] = (char)(i + 1);

    /* Aligned, a multiple of eight. */
    __builtin_memcpy(b, a, 32);
    for (int i = 0; i < 32; i++) if (b[i] != (char)(i + 1)) return 1;

    /* A size that is not a multiple of eight, so the tail matters. */
    __builtin_memset(c, 9, 13);
    for (int i = 0; i < 13; i++) if (c[i] != 9) return 2;

    { char d[13]; __builtin_memcpy(d, c, 13);
      for (int i = 0; i < 13; i++) if (d[i] != 9) return 3; }

    /* Overlapping, which is the whole reason memmove exists. */
    for (int i = 0; i < 40; i++) a[i] = (char)i;
    __builtin_memmove(a + 3, a, 20);
    for (int i = 0; i < 20; i++) if (a[i + 3] != (char)i) return 4;

    /* Each returns the destination pointer. */
    if (__builtin_memcpy(b, a, 4) != (void *)b) return 5;
    if (__builtin_memset(b, 0, 4) != (void *)b) return 6;
    if (__builtin_memmove(b, a, 4) != (void *)b) return 7;

    /* Zero length must touch nothing. */
    b[0] = 42;
    __builtin_memcpy(b, a, 0);
    if (b[0] != 42) return 8;
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(
                &format!("builtins_mem_ops{}", opt.replace('-', "_")),
                code,
                &[opt.to_string()]
            ),
            0,
            "memory builtins failed at {opt}"
        );
    }
}

/// `offsetof` with an array index that is not a constant, a GNU extension
/// (C17 7.19p3 wants an address constant): the offset of the indexed
/// element, computed at run time. util-linux's lsns.c needs it through
/// `list_entry(p, struct lsns_process, ns_siblings[ns->type])`, and
/// libblkid's atari.c and bcache.c through `offsetof(T, part[i])`.
#[test]
fn builtins_offsetof_with_a_variable_index() {
    let code = r#"
#include <stddef.h>

struct list_head { struct list_head *next, *prev; };
struct proc { int pid; struct list_head siblings[3]; char tail; };
struct S { char c; struct { int x; short y[3]; } a[4]; long z; };

#define container_of(ptr, type, member) \
    ((type *)((char *)(ptr) - offsetof(type, member)))

__attribute__((noinline)) static size_t at(int i, int j)
{
    return offsetof(struct S, a[i].y[j]);
}

__attribute__((noinline)) static size_t elem(long i)
{
    return __builtin_offsetof(struct S, a[i]);
}

int main(void)
{
    struct proc p;
    for (int t = 0; t < 3; t++)
        if (container_of(&p.siblings[t], struct proc, siblings[t]) != &p)
            return 1 + t;
    for (int i = 0; i < 4; i++) {
        if (elem(i) != offsetof(struct S, a[0]) + i * sizeof(((struct S *)0)->a[0]))
            return 10 + i;
        for (int j = 0; j < 3; j++)
            if (at(i, j) != (size_t)((char *)&((struct S *)0)->a[i].y[j] - (char *)0))
                return 20 + 3 * i + j;
    }
    /* A negative index, as in the extension. */
    if (elem(-1) != offsetof(struct S, a[0]) - sizeof(((struct S *)0)->a[0]))
        return 40;
    /* The index is evaluated once. */
    int n = 1;
    if (offsetof(struct S, a[n++]) != offsetof(struct S, a[1]) || n != 2)
        return 41;
    /* A constant index is still an integer constant expression. */
    _Static_assert(offsetof(struct S, a[2].y[1]) == offsetof(struct S, a[0]) + 2 * 12 + 4 + 2,
                   "constant index");
    static const size_t k = offsetof(struct S, a[3]);
    if (k != elem(3))
        return 42;
    return 0;
}
"#;
    compile_and_run_everywhere("builtins_offsetof_with_a_variable_index", code);
}

/// A variable index makes `offsetof` no constant, which a static
/// initializer needs.
#[test]
fn builtins_offsetof_with_a_variable_index_is_not_a_static_initializer() {
    compile_expect_error(
        "offsetof_variable_index_static",
        r#"
struct S { int a[4]; };
unsigned long f(int n)
{
    static unsigned long v = __builtin_offsetof(struct S, a[n]);
    return v;
}
"#,
        "not constant",
    );
}
