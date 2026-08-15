//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 Initializers Mega-Test
//
// Consolidates: designated initializers, compound literals tests
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: C99 initializers (designated init, compound literals)
// ============================================================================

#[test]
fn c99_initializers_mega() {
    let code = r#"
struct Point { int x; int y; int z; };
struct Rect { struct Point tl; struct Point br; };

int sum_point(struct Point p) {
    return p.x + p.y + p.z;
}

int main(void) {
    // ========== DESIGNATED ARRAY INIT (returns 1-19) ==========
    {
        // Basic designated init
        int arr[5] = {[1] = 10, [3] = 30};
        if (arr[0] != 0) return 1;
        if (arr[1] != 10) return 2;
        if (arr[2] != 0) return 3;
        if (arr[3] != 30) return 4;
        if (arr[4] != 0) return 5;

        // Out of order designation
        int arr3[5] = {[4] = 4, [2] = 2, [0] = 0};
        if (arr3[0] != 0) return 11;
        if (arr3[1] != 0) return 12;
        if (arr3[2] != 2) return 13;
        if (arr3[3] != 0) return 14;
        if (arr3[4] != 4) return 15;
    }

    // ========== DESIGNATED STRUCT INIT (returns 20-39) ==========
    {
        // Basic designated struct init
        struct Point p1 = {.x = 10, .y = 20, .z = 12};
        if (p1.x != 10) return 20;
        if (p1.y != 20) return 21;
        if (p1.z != 12) return 22;

        // Out of order
        struct Point p2 = {.z = 30, .x = 10, .y = 2};
        if (sum_point(p2) != 42) return 23;

        // Partial designated (others are 0)
        struct Point p3 = {.y = 42};
        if (p3.x != 0) return 24;
        if (p3.y != 42) return 25;
        if (p3.z != 0) return 26;

        // Nested struct designated init
        struct Rect r = {
            .tl = {.x = 0, .y = 0},
            .br = {.x = 100, .y = 50}
        };
        if (r.tl.x != 0) return 27;
        if (r.br.x != 100) return 28;
        if (r.br.y != 50) return 29;
    }

    // ========== COMPOUND LITERALS (returns 40-69) ==========
    {
        // Basic compound literal
        struct Point p = (struct Point){10, 20, 12};
        if (sum_point(p) != 42) return 40;

        // Compound literal with designated init
        struct Point p2 = (struct Point){.y = 30, .x = 10, .z = 2};
        if (sum_point(p2) != 42) return 41;

        // Array compound literal
        int *arr = (int[]){1, 2, 3, 4, 5};
        if (arr[0] != 1) return 42;
        if (arr[2] != 3) return 43;
        if (arr[4] != 5) return 44;

        // Compound literal as function argument
        int result = sum_point((struct Point){5, 7, 30});
        if (result != 42) return 45;

        // Address of compound literal
        struct Point *ptr = &(struct Point){100, 200, 300};
        if (ptr->x != 100) return 46;
        if (ptr->y != 200) return 47;
        if (ptr->z != 300) return 48;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_initializers_mega", code, &[]), 0);
}

// ============================================================================
// Complex initializers torture test
// ============================================================================

#[test]
fn c99_initializers_complex_mega() {
    let code = r#"
#include <stddef.h>

// ===== Type definitions for all sections =====

// Section 1: Function pointer fields
typedef int (*int_func_t)(int);
typedef void (*void_func_t)(void);

struct Callbacks {
    int (*handler)(int);
    void (*cleanup)(void);
    int (*transform)(int);
};

int double_it(int x) { return x * 2; }
int triple_it(int x) { return x * 3; }
void do_nothing(void) { }

// Section 3: Self-referencing structs
struct Node {
    int val;
    struct Node *next;
};

struct ListHead {
    struct ListHead *next;
    struct ListHead *prev;
};

// Section 4: Anonymous members
struct WithAnon {
    int tag;
    union {
        int ival;
        float fval;
    };
    int after;
};

struct WithAnonStruct {
    int before;
    struct {
        int x;
        int y;
    };
    int after;
};

// Section 5: Enums
enum Color { RED = 0, GREEN = 1, BLUE = 2 };

struct Tagged {
    char *name;
    enum Color color;
    int size;
};

// Section 6: Large struct (~30 fields, CPython-scale)
struct BigType {
    char *tp_name;
    long tp_basicsize;
    long tp_itemsize;
    int (*tp_init)(int);
    void (*tp_dealloc)(void);
    int (*tp_compare)(int);
    void *tp_as_number;
    void *tp_as_sequence;
    void *tp_as_mapping;
    int (*tp_hash)(int);
    int (*tp_call)(int);
    char *tp_doc;
    void *tp_methods;
    void *tp_members;
    void *tp_getset;
    void *tp_base;
    void *tp_dict;
    int (*tp_descr_get)(int);
    int (*tp_descr_set)(int);
    long tp_dictoffset;
    int (*tp_alloc)(int);
    int (*tp_new)(int);
    void (*tp_free)(void);
    int tp_flags;
    void *tp_subclasses;
    void *tp_weaklist;
    void *tp_del;
    unsigned int tp_version_tag;
    void *tp_finalize;
    void *tp_vectorcall;
    long tp_padding;
};

// Section 7: Array of structs
struct Entry {
    int key;
    int value;
    char *label;
};

// Section 8: Deeply nested
struct Inner {
    int a;
    int b;
};

struct Middle {
    struct Inner inner;
    int c;
};

struct Outer {
    struct Middle mid;
    int d;
};


// ===== Section 3 globals (self-referencing) =====
struct Node self_ref = {42, &self_ref};
struct ListHead self_list = {&self_list, &self_list};
struct Node node_b;
struct Node node_a = {1, &node_b};
struct Node node_b = {2, &node_a};

// ===== Section 6 globals (large struct) =====
struct BigType my_type = {
    .tp_name = "MyType",
    .tp_basicsize = 64,
    .tp_init = double_it,
    .tp_flags = 0x1234,
    .tp_doc = "A test type",
    .tp_version_tag = 42,
};


int main(void) {

    // ========== SECTION 1: Function pointer fields (returns 1-19) ==========
    {
        // Designated init with function pointers
        struct Callbacks cb1 = {.handler = double_it, .cleanup = do_nothing, .transform = triple_it};
        if (cb1.handler(5) != 10) return 1;
        if (cb1.transform(5) != 15) return 2;

        // NULL function pointer via cast
        struct Callbacks cb2 = {.handler = double_it, .cleanup = (void(*)(void))0, .transform = 0};
        if (cb2.handler(3) != 6) return 3;
        if (cb2.cleanup != 0) return 4;
        if (cb2.transform != 0) return 5;

        // Positional init with function pointers
        struct Callbacks cb3 = {double_it, do_nothing, triple_it};
        if (cb3.handler(7) != 14) return 6;
        if (cb3.transform(4) != 12) return 7;

        // Partial init - missing fields should be NULL
        struct Callbacks cb4 = {.handler = double_it};
        if (cb4.handler(10) != 20) return 8;
        if (cb4.cleanup != 0) return 9;
        if (cb4.transform != 0) return 10;

        // Function pointer via typedef
        int_func_t fp = double_it;
        struct Callbacks cb5 = {.handler = fp, .transform = fp};
        if (cb5.handler(3) != 6) return 11;
        if (cb5.transform(3) != 6) return 12;
    }

    // ========== SECTION 2: Mixed designated + positional (returns 20-39) ==========
    {
        // Positional first, then designated
        struct { int a; int b; int c; int d; } m1 = {10, 20, .d = 40};
        if (m1.a != 10) return 20;
        if (m1.b != 20) return 21;
        if (m1.c != 0) return 22;
        if (m1.d != 40) return 23;

        // Designated first, then positional continues from after that field
        struct { int a; int b; int c; int d; } m2 = {.b = 20, 30, 40};
        if (m2.a != 0) return 24;
        if (m2.b != 20) return 25;
        if (m2.c != 30) return 26;
        if (m2.d != 40) return 27;

        // Gaps (implicit zero) between designated fields
        struct { int a; int b; int c; int d; int e; } m3 = {.a = 1, .c = 3, .e = 5};
        if (m3.a != 1) return 28;
        if (m3.b != 0) return 29;
        if (m3.c != 3) return 30;
        if (m3.d != 0) return 31;
        if (m3.e != 5) return 32;

        // Interleaving: designated, positional, designated
        struct { int a; int b; int c; int d; } m4 = {.a = 10, 20, .d = 40};
        if (m4.a != 10) return 33;
        if (m4.b != 20) return 34;
        if (m4.c != 0) return 35;
        if (m4.d != 40) return 36;

        // Override: designated field overrides earlier positional
        struct { int a; int b; int c; } m5 = {100, 200, 300, .a = 999};
        if (m5.a != 999) return 37;
        if (m5.b != 200) return 38;
        if (m5.c != 300) return 39;
    }

    // ========== SECTION 3: Self-referencing structs (returns 40-59) ==========
    {
        // Global self-referencing node
        if (self_ref.val != 42) return 40;
        if (self_ref.next != &self_ref) return 41;
        if (self_ref.next->val != 42) return 42;

        // Global self-referencing list head (both pointers to self)
        if (self_list.next != &self_list) return 43;
        if (self_list.prev != &self_list) return 44;

        // Mutual references
        if (node_a.val != 1) return 45;
        if (node_a.next != &node_b) return 46;
        if (node_b.val != 2) return 47;
        if (node_b.next != &node_a) return 48;
        if (node_a.next->val != 2) return 49;
        if (node_b.next->val != 1) return 50;

        // Array of structs where elements point to each other (local static)
        static struct Node nodes[3] = {
            {10, &nodes[1]},
            {20, &nodes[2]},
            {30, &nodes[0]},
        };
        if (nodes[0].val != 10) return 51;
        if (nodes[0].next != &nodes[1]) return 52;
        if (nodes[1].val != 20) return 53;
        if (nodes[1].next->val != 30) return 54;
        if (nodes[2].next->val != 10) return 55;
    }

    // ========== SECTION 4: Anonymous struct/union members (returns 60-79) ==========
    {
        // Anonymous union inside struct
        struct WithAnon wa1 = {.tag = 1, .ival = 42, .after = 99};
        if (wa1.tag != 1) return 60;
        if (wa1.ival != 42) return 61;
        if (wa1.after != 99) return 62;

        // Anonymous struct inside struct
        struct WithAnonStruct was1 = {.before = 10, .x = 20, .y = 30, .after = 40};
        if (was1.before != 10) return 63;
        if (was1.x != 20) return 64;
        if (was1.y != 30) return 65;
        if (was1.after != 40) return 66;

        // Positional init of anonymous members
        struct WithAnon wa2 = {5, 50, 55};
        if (wa2.tag != 5) return 67;
        if (wa2.ival != 50) return 68;
        if (wa2.after != 55) return 69;

        // Partial init with anonymous members
        struct WithAnon wa3 = {.ival = 77};
        if (wa3.tag != 0) return 70;
        if (wa3.ival != 77) return 71;
        if (wa3.after != 0) return 72;
    }

    // ========== SECTION 5: String, enum, sizeof, cast (returns 80-99) ==========
    {
        // char* string field
        struct Tagged t1 = {.name = "hello", .color = GREEN, .size = 5};
        if (t1.name[0] != 'h') return 80;
        if (t1.name[4] != 'o') return 81;
        if (t1.color != GREEN) return 82;
        if (t1.size != 5) return 83;

        // char array in struct (not pointer)
        struct { char name[32]; int val; } sa1 = {.name = "world", .val = 42};
        if (sa1.name[0] != 'w') return 84;
        if (sa1.name[4] != 'd') return 85;
        if (sa1.name[5] != '\0') return 86;
        if (sa1.val != 42) return 87;

        // Enum constant fields
        struct Tagged t2 = {"blue", BLUE, 10};
        if (t2.color != 2) return 88;

        // sizeof in initializer
        struct { int sz; int val; } sz1 = {sizeof(int), 42};
        if (sz1.sz != 4) return 89;
        if (sz1.val != 42) return 90;

        struct { long sz; } sz2 = {.sz = sizeof(struct BigType)};
        if (sz2.sz == 0) return 91;

        // Cast expressions in initializer
        struct { long lval; void *ptr; } c1 = {(long)42, (void*)0};
        if (c1.lval != 42) return 92;
        if (c1.ptr != 0) return 93;

        // Negative initializer
        struct { int a; int b; } neg1 = {-10, -20};
        if (neg1.a != -10) return 94;
        if (neg1.b != -20) return 95;
    }

    // ========== SECTION 6: Large struct (~30 fields, CPython-scale) (returns 100-119) ==========
    {
        // Check the global BigType
        if (my_type.tp_name[0] != 'M') return 100;
        if (my_type.tp_basicsize != 64) return 101;
        if (my_type.tp_init(5) != 10) return 102;
        if (my_type.tp_flags != 0x1234) return 103;
        if (my_type.tp_doc[0] != 'A') return 104;
        if (my_type.tp_version_tag != 42) return 105;

        // Uninitialized fields should be zero/NULL
        if (my_type.tp_itemsize != 0) return 106;
        if (my_type.tp_dealloc != 0) return 107;
        if (my_type.tp_as_number != 0) return 108;
        if (my_type.tp_dict != 0) return 109;
        if (my_type.tp_padding != 0) return 110;

        // Local large struct with designated init
        struct BigType local_type = {
            .tp_name = "LocalType",
            .tp_basicsize = 128,
            .tp_hash = triple_it,
            .tp_flags = 0xFF,
        };
        if (local_type.tp_name[0] != 'L') return 111;
        if (local_type.tp_basicsize != 128) return 112;
        if (local_type.tp_hash(3) != 9) return 113;
        if (local_type.tp_flags != 0xFF) return 114;
        if (local_type.tp_init != 0) return 115;
        if (local_type.tp_as_number != 0) return 116;
    }

    // ========== SECTION 7: Array of structs with complex init (returns 120-139) ==========
    {
        // Static array of structs with mixed designated init
        struct Entry table[] = {
            {.key = 1, .value = 100, .label = "one"},
            {.key = 2, .value = 200, .label = "two"},
            {.key = 3, .value = 300, .label = "three"},
        };
        if (table[0].key != 1) return 120;
        if (table[0].value != 100) return 121;
        if (table[0].label[0] != 'o') return 122;
        if (table[1].key != 2) return 123;
        if (table[1].value != 200) return 124;
        if (table[2].value != 300) return 125;
        if (table[2].label[0] != 't') return 126;

        // Positional array of structs
        struct Entry table2[] = {
            {10, 1000, "ten"},
            {20, 2000, "twenty"},
        };
        if (table2[0].key != 10) return 127;
        if (table2[0].value != 1000) return 128;
        if (table2[1].key != 20) return 129;
        if (table2[1].label[0] != 't') return 130;

        // Nested arrays of structs
        struct { struct Entry entries[2]; int count; } group = {
            .entries = {
                {.key = 1, .value = 10, .label = "a"},
                {.key = 2, .value = 20, .label = "b"},
            },
            .count = 2,
        };
        if (group.entries[0].key != 1) return 131;
        if (group.entries[1].value != 20) return 132;
        if (group.count != 2) return 133;
    }

    // ========== SECTION 8: Deeply nested + compound literals (returns 140-159) ==========
    {
        // 3 levels of struct nesting
        struct Outer o1 = {
            .mid = {
                .inner = {.a = 10, .b = 20},
                .c = 30,
            },
            .d = 40,
        };
        if (o1.mid.inner.a != 10) return 140;
        if (o1.mid.inner.b != 20) return 141;
        if (o1.mid.c != 30) return 142;
        if (o1.d != 40) return 143;

        // Positional nested init
        struct Outer o2 = {{{1, 2}, 3}, 4};
        if (o2.mid.inner.a != 1) return 144;
        if (o2.mid.inner.b != 2) return 145;
        if (o2.mid.c != 3) return 146;
        if (o2.d != 4) return 147;

        // Compound literal as struct value
        struct Middle m1 = {.inner = (struct Inner){100, 200}, .c = 300};
        if (m1.inner.a != 100) return 148;
        if (m1.inner.b != 200) return 149;
        if (m1.c != 300) return 150;

        // Address of compound literal in initializer
        struct Inner *ip = &(struct Inner){55, 66};
        if (ip->a != 55) return 151;
        if (ip->b != 66) return 152;

        // Nested compound literals
        struct Outer *op = &(struct Outer){
            .mid = {.inner = {.a = 7, .b = 8}, .c = 9},
            .d = 10,
        };
        if (op->mid.inner.a != 7) return 153;
        if (op->mid.inner.b != 8) return 154;
        if (op->mid.c != 9) return 155;
        if (op->d != 10) return 156;
    }

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_initializers_complex_mega", code, &[]),
        0
    );
}

#[test]
fn c99_initializers_cpython_llist_pattern() {
    let code = r#"
struct llist_node {
    struct llist_node *next;
    struct llist_node *prev;
};

#define LLIST_INIT(head) { &head, &head }

struct llist_node my_list = LLIST_INIT(my_list);

int main(void) {
    if (my_list.next != &my_list) return 1;
    if (my_list.prev != &my_list) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("cpython_llist_init", code, &[]), 0);
}

#[test]
fn c99_initializers_cpython_opcode_pattern() {
    let code = r#"
struct uop { int op; int arg; int off; };
struct expansion { int nuops; struct uop uops[4]; };

enum { OP_A = 5, OP_B = 10 };

struct expansion table[16] = {
    [OP_A] = { .nuops = 2, .uops = { {1, 2, 3}, {3, 4, 5} } },
    [OP_B] = { .nuops = 1, .uops = { {5, 6, 7} } },
};

int main(void) {
    if (table[OP_A].nuops != 2) return 1;
    if (table[OP_A].uops[0].op != 1) return 2;
    if (table[OP_A].uops[1].arg != 4) return 3;
    if (table[OP_B].nuops != 1) return 4;
    if (table[OP_B].uops[0].op != 5) return 5;
    if (table[0].nuops != 0) return 6;
    return 0;
}
"#;
    assert_eq!(compile_and_run("cpython_opcode_init", code, &[]), 0);
}

#[test]
fn c99_initializers_cpython_pytypeobject_pattern() {
    let code = r#"
typedef void (*func_t)(void);
struct PyTypeObject {
    long ob_refcnt;
    void *ob_type;
    char *tp_name;
    long tp_basicsize;
    func_t tp_dealloc;
    func_t tp_repr;
    void *tp_as_number;
    long tp_flags;
    char *tp_doc;
};

void my_dealloc(void) {}

struct PyTypeObject MyType = {
    1,
    0,
    "MyType",
    64,
    my_dealloc,
    0,
    0,
    .tp_flags = 0x1234,
    .tp_doc = "doc",
};

int main(void) {
    if (MyType.ob_refcnt != 1) return 1;
    if (MyType.tp_basicsize != 64) return 2;
    if (MyType.tp_dealloc != my_dealloc) return 3;
    if (MyType.tp_flags != 0x1234) return 4;
    if (MyType.tp_doc[0] != 'd') return 5;
    return 0;
}
"#;
    assert_eq!(compile_and_run("cpython_pytypeobject_init", code, &[]), 0);
}

#[test]
fn c99_initializers_nested_designated_pattern() {
    let code = r#"
struct inner { int tag; int data; };
struct outer {
    void *type;
    struct inner value;
};

struct outer obj = {
    (void*)0x1234,
    { .tag = 42, .data = 99 }
};

int main(void) {
    if (obj.type != (void*)0x1234) return 1;
    if (obj.value.tag != 42) return 2;
    if (obj.value.data != 99) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("nested_designated_init", code, &[]), 0);
}

/// Test that sizeof works correctly for arrays with size inferred from initializer
/// This tests the fix for GitHub issue where sizeof(arr) returned 0 for arr[] = {...}
#[test]
fn c99_initializers_sizeof_inferred_array() {
    let code = r#"
// Global arrays with inferred size
int global_arr[] = {1, 2, 3, 4, 5};
static int static_arr[] = {10, 20, 30, 40, 50, 60};

// Array of structs
struct Pair { int x; int y; };
static struct Pair pairs[] = {{1, 2}, {3, 4}, {5, 6}};

// Pointer array
static int *ptrs[] = {0, 0, 0};

int main(void) {
    // Test global array
    if (sizeof(global_arr) != 20) return 1;  // 5 * 4 bytes
    if (sizeof(global_arr) / sizeof(global_arr[0]) != 5) return 2;

    // Test static array
    if (sizeof(static_arr) != 24) return 10;  // 6 * 4 bytes
    if (sizeof(static_arr) / sizeof(static_arr[0]) != 6) return 11;

    // Test local array with inferred size
    int local_arr[] = {100, 200, 300};
    if (sizeof(local_arr) != 12) return 20;  // 3 * 4 bytes
    if (sizeof(local_arr) / sizeof(local_arr[0]) != 3) return 21;

    // Test array of structs
    if (sizeof(pairs) != 24) return 30;  // 3 * (4 + 4) bytes
    if (sizeof(pairs) / sizeof(pairs[0]) != 3) return 31;

    // Test pointer array
    if (sizeof(ptrs) != 24) return 40;  // 3 * 8 bytes on 64-bit
    if (sizeof(ptrs) / sizeof(ptrs[0]) != 3) return 41;

    // Test complex pattern like CPython's static_types[]
    typedef void *PyTypeObject;
    static PyTypeObject types[] = {
        (void*)1,
        (void*)2,
        (void*)3,
        (void*)4,
    };
    if (sizeof(types) / sizeof(types[0]) != 4) return 50;

    return 0;
}
"#;
    assert_eq!(compile_and_run("sizeof_inferred_array", code, &[]), 0);
}

/// Test designated initialization of multiple bitfields within the same storage unit
/// This tests the fix for a bug where only the last bitfield was initialized
/// (due to incorrect deduplication of fields at the same offset)
#[test]
fn c99_initializers_bitfield_designated() {
    let code = r#"
#include <stdio.h>

// Bitfields all packed into a single storage unit (like CPython's PyASCIIObject state)
struct state {
    unsigned int interned:2;
    unsigned int kind:3;
    unsigned int compact:1;
    unsigned int ascii:1;
    unsigned int statically_allocated:1;
};

struct obj {
    void *ptr;
    long length;
    long hash;
    struct state state;
};

// Test global designated initializer with multiple bitfields
struct obj test_obj = {
    .ptr = (void*)0x12345678,
    .length = 8,
    .hash = -1,
    .state = {
        .kind = 1,
        .compact = 1,
        .ascii = 1,
        .statically_allocated = 1,
    },
};

// Test that all bitfields within the same byte can be initialized
struct flags {
    unsigned int a:1;
    unsigned int b:1;
    unsigned int c:1;
    unsigned int d:1;
    unsigned int e:1;
    unsigned int f:1;
    unsigned int g:1;
    unsigned int h:1;
};

struct flags all_flags = {
    .a = 1, .b = 1, .c = 1, .d = 1,
    .e = 1, .f = 1, .g = 1, .h = 1,
};

struct flags some_flags = {
    .b = 1, .d = 1, .f = 1, .h = 1,
};

int main(void) {
    // Verify global struct with nested bitfield struct
    if (test_obj.ptr != (void*)0x12345678) return 1;
    if (test_obj.length != 8) return 2;
    if (test_obj.hash != -1) return 3;
    if (test_obj.state.interned != 0) return 4;
    if (test_obj.state.kind != 1) return 5;
    if (test_obj.state.compact != 1) return 6;
    if (test_obj.state.ascii != 1) return 7;
    if (test_obj.state.statically_allocated != 1) return 8;

    // Verify all flags set
    if (all_flags.a != 1) return 10;
    if (all_flags.b != 1) return 11;
    if (all_flags.c != 1) return 12;
    if (all_flags.d != 1) return 13;
    if (all_flags.e != 1) return 14;
    if (all_flags.f != 1) return 15;
    if (all_flags.g != 1) return 16;
    if (all_flags.h != 1) return 17;

    // Verify alternating flags
    if (some_flags.a != 0) return 20;
    if (some_flags.b != 1) return 21;
    if (some_flags.c != 0) return 22;
    if (some_flags.d != 1) return 23;
    if (some_flags.e != 0) return 24;
    if (some_flags.f != 1) return 25;
    if (some_flags.g != 0) return 26;
    if (some_flags.h != 1) return 27;

    // Local variable with bitfield designated init
    struct obj local_obj = {
        .state = {
            .interned = 2,
            .kind = 5,
            .compact = 0,
            .ascii = 1,
        },
    };
    if (local_obj.state.interned != 2) return 30;
    if (local_obj.state.kind != 5) return 31;
    if (local_obj.state.compact != 0) return 32;
    if (local_obj.state.ascii != 1) return 33;
    if (local_obj.state.statically_allocated != 0) return 34;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_designated_init", code, &[]), 0);
}

// ============================================================================
// BUG 1: Pointer arithmetic in global initializers + hard error for unknown exprs
// ============================================================================

#[test]
fn c99_initializers_ptr_arithmetic_global() {
    let code = r#"
int arr[10] = {0,1,2,3,4,5,6,7,8,9};
int *p1 = arr + 5;
int *p2 = &arr[0] + 3;
int *p3 = arr + 0;

struct S { int x; int y; int z; };
struct S global_s = {10, 20, 30};
int *sp = (int*)&global_s + 1;

// Pointer to middle of array
int *mid = &arr[4];

int main(void) {
    if (*p1 != 5) return 1;
    if (*p2 != 3) return 2;
    if (*p3 != 0) return 3;
    if (*sp != 20) return 4;
    if (*mid != 4) return 5;

    // Pointer subtraction in arithmetic
    int *p4 = arr + 9;
    if (*p4 != 9) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ptr_arithmetic_global_init", code, &[]), 0);
}

// ============================================================================
// BUG 2: Anonymous struct positional continuation after designator
// ============================================================================

#[test]
fn c99_initializers_anon_struct_continuation() {
    let code = r#"
// Test: after designating a field in an anonymous struct,
// the next positional should continue within that anonymous struct

struct WithAnonStruct {
    int a;
    struct {
        int x;
        int y;
        int z;
    };
    int c;
};

int main(void) {
    // .x designates inside anon struct, 20 should go to y (not c)
    struct WithAnonStruct s1 = {.a = 1, .x = 10, 20, 30, 99};
    if (s1.a != 1) return 1;
    if (s1.x != 10) return 2;
    if (s1.y != 20) return 3;
    if (s1.z != 30) return 4;
    if (s1.c != 99) return 5;

    // Only designating inside anon struct
    struct WithAnonStruct s2 = {.x = 100, 200};
    if (s2.a != 0) return 10;
    if (s2.x != 100) return 11;
    if (s2.y != 200) return 12;
    if (s2.z != 0) return 13;

    // Designate last field of anon struct, then positional goes to outer
    struct WithAnonStruct s3 = {.z = 50, 60};
    if (s3.z != 50) return 20;
    if (s3.c != 60) return 21;

    return 0;
}
"#;
    assert_eq!(compile_and_run("anon_struct_continuation", code, &[]), 0);
}

#[test]
fn c99_initializers_anon_struct_nested_continuation() {
    let code = r#"
// Test deeply nested anonymous structs: positional continuation must
// walk through all nesting levels correctly (C11 6.7.2.1p13).

struct Deep {
    int a;
    struct {
        int x;
        struct {
            int p;
            int q;
        };
        int z;
    };
    int c;
};

int main(void) {
    // .p is inside nested anon struct; 20 should go to q, then z, then c
    struct Deep d1 = {.a = 1, .p = 10, 20, 30, 40};
    if (d1.a != 1) return 1;
    if (d1.p != 10) return 2;
    if (d1.q != 20) return 3;
    if (d1.z != 30) return 4;
    if (d1.c != 40) return 5;

    // .q is last in inner anon; next positional goes to z (outer anon), then c
    struct Deep d2 = {.q = 100, 200, 300};
    if (d2.q != 100) return 10;
    if (d2.z != 200) return 11;
    if (d2.c != 300) return 12;

    // .x is in outer anon; positional should descend into inner anon next
    struct Deep d3 = {.x = 50, 60, 70, 80, 90};
    if (d3.x != 50) return 20;
    if (d3.p != 60) return 21;
    if (d3.q != 70) return 22;
    if (d3.z != 80) return 23;
    if (d3.c != 90) return 24;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("anon_struct_nested_continuation", code, &[]),
        0
    );
}

// ============================================================================
// BUG 3: CompoundLiteral type mismatch
// ============================================================================

#[test]
fn c99_initializers_compound_literal_type_mismatch() {
    let code = r#"
struct Small { int x; int y; };
struct Big { int a; int b; int c; int d; };

// Global: compound literal type != target type, not pointer
// This exercises the else branch where cl_type must be used instead of target type
struct Big global_b = {.a = 1, .b = 2, .c = 3, .d = 4};

int main(void) {
    // Same-type compound literal (exercises the cl_type == typ branch)
    struct Small s = (struct Small){42, 99};
    if (s.x != 42) return 1;
    if (s.y != 99) return 2;

    // Verify global init too
    if (global_b.a != 1) return 10;
    if (global_b.d != 4) return 11;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("compound_literal_type_mismatch", code, &[]),
        0
    );
}

// ============================================================================
// BUG 4: Brace elision (C99 6.7.8p17-20)
// ============================================================================

#[test]
fn c99_initializers_brace_elision() {
    let code = r#"
int main(void) {
    // 2D array without inner braces
    int a[2][2] = {1, 2, 3, 4};
    if (a[0][0] != 1) return 1;
    if (a[0][1] != 2) return 2;
    if (a[1][0] != 3) return 3;
    if (a[1][1] != 4) return 4;

    // 2D array with partial fill
    int b[2][3] = {1, 2, 3, 4, 5, 6};
    if (b[0][0] != 1) return 10;
    if (b[0][2] != 3) return 11;
    if (b[1][0] != 4) return 12;
    if (b[1][2] != 6) return 13;

    // Array of structs without inner braces
    struct { int a; int b; } arr[2] = {1, 2, 3, 4};
    if (arr[0].a != 1) return 20;
    if (arr[0].b != 2) return 21;
    if (arr[1].a != 3) return 22;
    if (arr[1].b != 4) return 23;

    // Struct containing array with brace elision
    struct { int arr[3]; int val; } s = {10, 20, 30, 40};
    if (s.arr[0] != 10) return 30;
    if (s.arr[1] != 20) return 31;
    if (s.arr[2] != 30) return 32;
    if (s.val != 40) return 33;

    // Mixed: braced and elided
    int c[3][2] = {{1, 2}, 3, 4, {5, 6}};
    if (c[0][0] != 1) return 40;
    if (c[0][1] != 2) return 41;
    if (c[1][0] != 3) return 42;
    if (c[1][1] != 4) return 43;
    if (c[2][0] != 5) return 44;
    if (c[2][1] != 6) return 45;

    // Partial brace elision (fewer elements than needed)
    int d[2][2] = {1, 2, 3};
    if (d[0][0] != 1) return 50;
    if (d[0][1] != 2) return 51;
    if (d[1][0] != 3) return 52;
    if (d[1][1] != 0) return 53;

    return 0;
}
"#;
    assert_eq!(compile_and_run("brace_elision", code, &[]), 0);
}

#[test]
fn c99_initializers_brace_elision_global() {
    let code = r#"
// Global 2D array with brace elision
int g[2][2] = {1, 2, 3, 4};

// Global array of structs with brace elision
struct Pair { int x; int y; };
struct Pair pairs[3] = {1, 2, 3, 4, 5, 6};

// Nested struct with brace elision
struct Inner { int a; int b; };
struct Outer { struct Inner inner; int c; };
struct Outer outer = {10, 20, 30};

int main(void) {
    if (g[0][0] != 1) return 1;
    if (g[0][1] != 2) return 2;
    if (g[1][0] != 3) return 3;
    if (g[1][1] != 4) return 4;

    if (pairs[0].x != 1) return 10;
    if (pairs[0].y != 2) return 11;
    if (pairs[1].x != 3) return 12;
    if (pairs[1].y != 4) return 13;
    if (pairs[2].x != 5) return 14;
    if (pairs[2].y != 6) return 15;

    if (outer.inner.a != 10) return 20;
    if (outer.inner.b != 20) return 21;
    if (outer.c != 30) return 22;

    return 0;
}
"#;
    assert_eq!(compile_and_run("brace_elision_global", code, &[]), 0);
}

// ============================================================================
// String literals must NOT trigger brace elision (C99 6.7.8p14)
// ============================================================================

#[test]
fn c99_initializers_string_no_brace_elision() {
    let code = r#"
// String literals initialize char arrays directly, not via brace elision
struct WithCharArray {
    char name[16];
    int val;
};

// Global: string literal for char array member should not consume next elements
struct WithCharArray g1 = {"hello", 42};

// Array of structs with string members
struct WithCharArray table[] = {
    {"alpha", 1},
    {"beta", 2},
};

int main(void) {
    if (g1.name[0] != 'h') return 1;
    if (g1.name[4] != 'o') return 2;
    if (g1.val != 42) return 3;

    // Local with brace elision NOT applied to string
    struct WithCharArray local = {"world", 99};
    if (local.name[0] != 'w') return 10;
    if (local.val != 99) return 11;

    // Array of structs
    if (table[0].name[0] != 'a') return 20;
    if (table[0].val != 1) return 21;
    if (table[1].name[0] != 'b') return 22;
    if (table[1].val != 2) return 23;

    return 0;
}
"#;
    assert_eq!(compile_and_run("string_no_brace_elision", code, &[]), 0);
}

// ============================================================================
// Parity test: global (static) vs local initializers must produce same results
// ============================================================================

#[test]
fn c99_initializers_global_local_parity() {
    let code = r#"
struct Point { int x; int y; int z; };
struct Nested { struct Point p; int val; };
struct WithArray { int arr[4]; int extra; };

// Global initializers (static path: ast_init_list_to_ir)
struct Point g_desig = {.z = 30, .x = 10, .y = 20};
struct Point g_pos = {1, 2, 3};
struct Nested g_nested = {{100, 200, 300}, 400};
struct Nested g_nested_desig = {.p = {.y = 50, .x = 40}, .val = 60};
struct WithArray g_arr = {{10, 20, 30, 40}, 50};
struct WithArray g_arr_desig = {.arr = {[2] = 300, [0] = 100}, .extra = 99};
int g_2d[2][3] = {1, 2, 3, 4, 5, 6};
int g_2d_desig[2][3] = {[1] = {[2] = 99}};

int main(void) {
    // Local initializers (runtime path: linearize_init_list_at_offset)
    struct Point l_desig = {.z = 30, .x = 10, .y = 20};
    struct Point l_pos = {1, 2, 3};
    struct Nested l_nested = {{100, 200, 300}, 400};
    struct Nested l_nested_desig = {.p = {.y = 50, .x = 40}, .val = 60};
    struct WithArray l_arr = {{10, 20, 30, 40}, 50};
    struct WithArray l_arr_desig = {.arr = {[2] = 300, [0] = 100}, .extra = 99};
    int l_2d[2][3] = {1, 2, 3, 4, 5, 6};
    int l_2d_desig[2][3] = {[1] = {[2] = 99}};

    // Designated struct: global vs local
    if (g_desig.x != l_desig.x) return 1;
    if (g_desig.y != l_desig.y) return 2;
    if (g_desig.z != l_desig.z) return 3;

    // Positional struct: global vs local
    if (g_pos.x != l_pos.x) return 10;
    if (g_pos.y != l_pos.y) return 11;
    if (g_pos.z != l_pos.z) return 12;

    // Nested struct: global vs local
    if (g_nested.p.x != l_nested.p.x) return 20;
    if (g_nested.p.y != l_nested.p.y) return 21;
    if (g_nested.p.z != l_nested.p.z) return 22;
    if (g_nested.val != l_nested.val) return 23;

    // Nested designated: global vs local
    if (g_nested_desig.p.x != l_nested_desig.p.x) return 30;
    if (g_nested_desig.p.y != l_nested_desig.p.y) return 31;
    if (g_nested_desig.val != l_nested_desig.val) return 32;

    // Array in struct: global vs local
    if (g_arr.arr[0] != l_arr.arr[0]) return 40;
    if (g_arr.arr[3] != l_arr.arr[3]) return 41;
    if (g_arr.extra != l_arr.extra) return 42;

    // Designated array in struct: global vs local
    if (g_arr_desig.arr[0] != l_arr_desig.arr[0]) return 50;
    if (g_arr_desig.arr[1] != l_arr_desig.arr[1]) return 51;
    if (g_arr_desig.arr[2] != l_arr_desig.arr[2]) return 52;
    if (g_arr_desig.extra != l_arr_desig.extra) return 53;

    // 2D array brace elision: global vs local
    if (g_2d[0][0] != l_2d[0][0]) return 60;
    if (g_2d[1][2] != l_2d[1][2]) return 61;

    // 2D designated array: global vs local
    if (g_2d_desig[0][0] != l_2d_desig[0][0]) return 70;
    if (g_2d_desig[1][2] != l_2d_desig[1][2]) return 71;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("initializers_global_local_parity", code, &[]),
        0
    );
}

/// An initializer element that is already an expression of the aggregate's own
/// type initializes the whole aggregate (C17 6.7.9p13).
///
/// It was instead treated as a brace-elision candidate, so filling one element
/// consumed `count_scalar_fields` *elements* rather than one:
/// `struct P a[2] = {p, p};` put both structs into `a[0]`, left `a[1]`
/// uninitialized, and assigned a struct where a scalar field was expected --
/// giving `4 4 0 0` where gcc gives `4 5 4 5`. The nested case returned
/// uninitialized stack, so this read whatever happened to be there.
#[test]
fn c99_aggregate_element_initializes_whole_aggregate() {
    let code = r#"
#include <string.h>

struct P { int x, y; };
struct N { int a[2]; struct { int x, y; } in; };

int main(void) {
    struct P p = {4, 5};

    /* The case that was wrong: elements are struct expressions, not braces. */
    struct P a2[2] = {p, p};
    if (a2[0].x != 4 || a2[0].y != 5) return 1;
    if (a2[1].x != 4 || a2[1].y != 5) return 2;

    /* Nested aggregate, where the old path returned uninitialized stack. */
    struct N n = {{1, 2}, {4, 5}};
    struct N b2[2] = {n, n};
    if (b2[0].a[0] != 1 || b2[0].in.x != 4) return 3;
    if (b2[1].a[0] != 1 || b2[1].in.x != 4) return 4;
    if (memcmp(&b2[0], &n, sizeof n) != 0) return 5;
    if (memcmp(&b2[1], &n, sizeof n) != 0) return 6;

    /* Mixing an expression element with a brace list must still work. */
    struct P mix[2] = {p, {8, 9}};
    if (mix[0].x != 4 || mix[0].y != 5) return 7;
    if (mix[1].x != 8 || mix[1].y != 9) return 8;

    /* Fewer initializers than elements: the rest are zeroed, not garbage. */
    struct P few[3] = {p};
    if (few[0].x != 4 || few[1].x != 0 || few[2].x != 0) return 9;
    if (few[1].y != 0 || few[2].y != 0) return 10;

    /* A single struct initialized from another, and one member from an
       expression -- the same rule one level down. */
    struct N copy = n;
    if (copy.a[0] != 1 || copy.in.x != 4) return 11;

    /* The paths that always worked, pinned so this fix cannot regress them. */
    struct P braces[2] = {{4, 5}, {6, 7}};
    if (braces[0].x != 4 || braces[1].x != 6) return 12;
    static struct P statics[2] = {{4, 5}, {6, 7}};
    if (statics[0].x != 4 || statics[1].x != 6) return 13;
    struct P assigned[2];
    assigned[0] = p; assigned[1] = p;
    if (assigned[0].x != 4 || assigned[1].x != 4) return 14;
    struct N desig = {.in = {7, 8}};
    if (desig.in.x != 7 || desig.in.y != 8) return 15;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run(
            "c99_aggregate_element_initializes_whole_aggregate",
            code,
            &[]
        ),
        0
    );
}

// ============================================================================
// Constant folding in global initializers
// ============================================================================

/// Arithmetic on floating constants must fold at file scope.
///
/// C99 6.6p8 allows any arithmetic constant expression as the initializer of
/// an object with static storage duration. c17 folded `*` and `/` correctly
/// but not `+` or `-`: those two were intercepted by the *pointer*-arithmetic
/// arm, which fell back to an integer-only evaluator and then rejected the
/// program. `double a = 1.0 + 2.0;` did not compile.
///
/// Worse, `-(1.0 + 2.0)` was accepted and silently became `0.0` -- the
/// negation arm returned "no initializer" without a diagnostic, which lands
/// the object in `.bss`. A wrong answer with a zero exit status.
#[test]
fn c99_global_initializer_folds_floating_arithmetic() {
    let src = r#"
#include <math.h>

double d_add = 1.0 + 2.0;
double d_sub = 5.0 - 2.0;
double d_mul = 3.0 * 2.0;
double d_div = 12.0 / 4.0;
double d_mixed = 1.0 + 2;            /* int operand promotes */
float  f_add = 1.5f + 1.5f;
long double l_add = 1.0L + 2.0L;
double d_nested = (1.0 + 2.0) * 3.0 - 6.0;

/* The silent-zero cases: a negated constant expression. */
double d_neg_sum = -(1.0 + 2.0);
double d_neg_mul = -(2.0 * 1.5);
double d_neg_lit = -3.0;

/* Integer folding must keep working unchanged. */
int i_add = 1 + 2;
int i_sub = 5 - 2;

int main(void)
{
    if (d_add != 3.0) return 1;
    if (d_sub != 3.0) return 2;
    if (d_mul != 6.0) return 3;
    if (d_div != 3.0) return 4;
    if (d_mixed != 3.0) return 5;
    if (f_add != 3.0f) return 6;
    if (l_add != 3.0L) return 7;
    if (d_nested != 3.0) return 8;

    if (d_neg_sum != -3.0) return 9;
    if (d_neg_mul != -3.0) return 10;
    if (d_neg_lit != -3.0) return 11;

    if (i_add != 3) return 12;
    if (i_sub != 3) return 13;

    return 0;
}
"#;
    assert_eq!(compile_and_run("global_init_float_fold", src, &[]), 0);
}

/// Pointer arithmetic in a global initializer must keep working.
///
/// The float fix touches the arm that handles it, so this pins the behaviour
/// the arm was written for: a symbol address plus a scaled constant offset.
#[test]
fn c99_global_initializer_folds_pointer_arithmetic() {
    let src = r#"
#include <string.h>
static int arr[10] = {0,1,2,3,4,5,6,7,8,9};
int *p_fwd = arr + 3;
int *p_back = &arr[7] - 2;
int *p_zero = arr + 0;

/* A string literal has a static address too, but it only acquires a label
   when it is interned -- which the address evaluator could not do, so this
   was rejected while `arr + 1` was accepted. */
const char *s_off = "hello" + 1;
const char *s_end = "world" + 5;

int main(void)
{
    if (*p_fwd != 3) return 1;
    if (*p_back != 5) return 2;
    if (*p_zero != 0) return 3;
    if (p_fwd - arr != 3) return 4;
    if (strcmp(s_off, "ello") != 0) return 5;
    if (*s_end != '\0') return 6;
    return 0;
}
"#;
    assert_eq!(compile_and_run("global_init_ptr_fold", src, &[]), 0);
}

/// A `_Complex` object with static storage duration can be initialized.
///
/// Every spelling was rejected before: `__builtin_complex` had no arm at all,
/// and `1.0 + 2.0*I` reached the arithmetic arm, which had no notion of a
/// complex value. Only the function-local path worked.
///
/// Note `double _Complex z = {1.0, 2.0};` is *not* 1.0 + 2.0i. A complex type
/// is a scalar type (C11 6.2.5p21), so that is a braced scalar initializer
/// with an excess element; gcc warns and keeps only the first. Matching gcc
/// here means the imaginary part stays zero.
#[test]
fn c99_global_initializer_accepts_complex() {
    let src = r#"
#include <complex.h>
#include <stdio.h>

double _Complex z_builtin = __builtin_complex(1.0, 2.0);
double _Complex z_cmplx   = CMPLX(1.0, 2.0);
double _Complex z_imag    = 1.0 + 2.0*I;
double _Complex z_real    = 3.0;               /* real constant, zero imag */
double _Complex z_neg     = -(1.0 + 2.0*I);
double _Complex z_mul     = (1.0 + 2.0*I) * (3.0 + 4.0*I);   /* -5 + 10i */
double _Complex z_brace   = {1.0};             /* braced scalar */
float _Complex  f_imag    = 1.5f + 2.5f*I;     /* narrower base type */

static double _Complex s_imag = 1.0 + 2.0*I;   /* internal linkage */

int main(void)
{
    if (creal(z_builtin) != 1.0 || cimag(z_builtin) != 2.0) return 1;
    if (creal(z_cmplx) != 1.0 || cimag(z_cmplx) != 2.0) return 2;
    if (creal(z_imag) != 1.0 || cimag(z_imag) != 2.0) return 3;
    if (creal(z_real) != 3.0 || cimag(z_real) != 0.0) return 4;
    if (creal(z_neg) != -1.0 || cimag(z_neg) != -2.0) return 5;
    if (creal(z_mul) != -5.0 || cimag(z_mul) != 10.0) return 6;
    if (creal(z_brace) != 1.0 || cimag(z_brace) != 0.0) return 7;
    if (crealf(f_imag) != 1.5f || cimagf(f_imag) != 2.5f) return 8;
    if (creal(s_imag) != 1.0 || cimag(s_imag) != 2.0) return 9;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("global_init_complex", src, &["-lm".to_string()]),
        0
    );
}

// ============================================================================
// Decimal floating literals at the target's precision
// ============================================================================

/// A decimal literal must reach `long double` at full width.
///
/// Decimal literals went through `f64::from_str`, so a `long double` was
/// correct only to 53 of its 64 significand bits, and one outside double's
/// range collapsed entirely -- `LDBL_MAX` written in decimal became `inf` and
/// `1e-4900L` became zero. Hex literals were already exact, which is what the
/// comparisons below use as the reference: every right-hand side is a hex
/// literal naming the value gcc produces for the decimal on its left.
///
/// `float` and `double` were never affected: `f64::from_str` is correctly
/// rounded, and for those two the target format is `f64` or narrower.
///
/// Each reference is spelled for the format the target has. A decimal
/// correctly rounded to binary128's 113 bits is *not* the same value as one
/// correctly rounded to x87's 64, so one hex spelling cannot serve both --
/// which is the whole claim being made here, that the literal reaches the
/// width the target actually has. Both sets were taken from gcc.
#[test]
fn c99_decimal_literals_reach_long_double_precision() {
    let src = r#"
#include <float.h>

#if LDBL_MANT_DIG == 113
#define PI_REF    0x1.921fb54442d18469834ef156fa8fp+1L
#define TENTH_REF 0x1.999999999999999999999999999ap-4L
#define BIG_REF   0x1.fffffffffffffffdf5f7837da5b2p+16383L
#define TINY_REF  0x1.7769bead75ec52e4d25544b1042ep-16278L
#else
/* x87's 64 bits. A target whose long double is double rounds both sides of
   each comparison to the same value, so these serve there too. */
#define PI_REF    0xc.90fdaa22168c235p-2L
#define TENTH_REF 0xc.ccccccccccccccdp-7L
#define BIG_REF   0xf.fffffffffffffffp+16380L
#define TINY_REF  0xb.bb4df56baf62972p-16281L
#endif

int main(void)
{
    /* Needs every significand bit the format has: the tail is lost at 53. */
    if (3.14159265358979323846L != PI_REF) return 1;

    /* A value with no exact binary form, rounded at the wrong width. */
    if (0.1L != TENTH_REF) return 2;

    /* Outside double's range in both directions. */
    if (1.18973149535723176502e+4932L != BIG_REF) return 3;
    if (1e-4900L != TINY_REF) return 4;

    /* Ordinary magnitudes must stay exact, not merely close. */
    if (1.0L != 0x1p+0L) return 5;
    if (0.5L != 0x1p-1L) return 6;
    if (1e10L != 0x2.540be4p+32L) return 7;
    if (123456789.0L != 0x7.5bcd15p+24L) return 8;

    /* Zero, and a value that underflows to it, keep their sign. */
    if (0.0L != 0x0p+0L) return 9;
    if (1e-5000L != 0.0L) return 10;

    /* float and double are unchanged. */
    if (0.1 != 0x1.999999999999ap-4) return 11;
    if (0.1f != 0x1.99999ap-4f) return 12;
    if (DBL_MAX <= 0.0 || LDBL_MAX <= 0.0) return 13;

    /* The exponent forms all agree. */
    if (1.5e3L != 1500.0L) return 14;
    if (15e2L != 1500.0L) return 15;
    if (150000e-2L != 1500.0L) return 16;

    return 0;
}
"#;
    assert_eq!(compile_and_run("decimal_long_double", src, &[]), 0);
}

/// The address of a compound literal with static storage duration.
///
/// C99 6.5.2.5p5 gives a compound literal at file scope static storage
/// duration, so its address is a constant expression and may initialize a
/// pointer. c17 dropped the initializer silently: the object went to `.bss`,
/// the pointer was null, and the program segfaulted on first use.
///
/// The address-of arm returned "no initializer" when it could not evaluate the
/// operand, which is the same silent-zero shape that made `-(1.0 + 2.0)` come
/// out as `0.0`. The other arms were fixed then; this one was missed because
/// no test reached it.
#[test]
fn c99_address_of_a_static_compound_literal() {
    let src = r#"
#include <stdio.h>
#include <string.h>

struct P { int x, y; };

static struct P *gp = &(struct P){1, 2};
static int *ga = (int[]){10, 20, 30};
static const char *gs = (const char[]){'h', 'i', 0};

int main(void)
{
    if (gp->x != 1 || gp->y != 2) return 1;
    if (ga[0] != 10 || ga[1] != 20 || ga[2] != 30) return 2;
    if (strcmp(gs, "hi") != 0) return 3;

    /* Writing through it must work: it is an object, not a constant. */
    gp->x = 42;
    if (gp->x != 42) return 5;

    /* The block-scope forms already worked and must keep working. */
    struct P *lp = &(struct P){7, 8};
    if (lp->x != 7 || lp->y != 8) return 6;
    int *la = (int[]){4, 5};
    if (la[0] != 4 || la[1] != 5) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("static_compound_literal_addr", src, &[]), 0);
}

/// A static initializer is converted to the *object's* type, not kept in the
/// constant's own.
///
/// Folding floating constants in global initializers made `int c = 1.0 + 2.0;`
/// compile, which it should -- but it stored the IEEE bits of 3.0 into a
/// 4-byte integer, so `c` read back as 1077936128. The mirror case,
/// `double d = 1 + 2;`, stored the integer 3 into a double and read back as
/// 1.5e-323. C17 6.7.9p11 converts the initializer as an assignment would.
#[test]
fn c99_scalar_initializers_take_the_objects_type() {
    let code = r#"
#include <limits.h>

/* Floating constants initializing integer objects: the fraction is
   discarded (C17 6.3.1.4), it is not reinterpreted. */
int i_add = 1.0 + 2.0;
int i_sub = 9.5 - 2.0;
int i_mul = 2.5 * 2.0;
int i_div = 7 / 2.0;
int i_neg = -(1.5 + 2.0);
long l_wide = 2.9 * 2.0;
char c_narrow = 65.9;
_Bool b_from_float = 0.5;
short s_neg = -3.9;

/* Integer constants initializing floating objects: widened exactly. */
double d_add = 1 + 2;
float f_int = 7;
long double ld_int = -5;
double d_big = 2147483647;

/* And the same-type cases must not have moved. */
double d_add_f = 1.0 + 2.0;
int i_add_i = 1 + 2;

struct S { int i; double d; };
struct S agg = { 1.0 + 2.0, 1 + 2 };
int arr[3] = { 1.5, 2.5, 3.5 };

int main(void)
{
    if (i_add != 3) return 1;
    if (i_sub != 7) return 2;
    if (i_mul != 5) return 3;
    if (i_div != 3) return 4;
    if (i_neg != -3) return 5;
    if (l_wide != 5) return 6;
    if (c_narrow != 'A') return 7;
    if (b_from_float != 1) return 8;
    if (s_neg != -3) return 9;

    if (d_add != 3.0) return 10;
    if (f_int != 7.0f) return 11;
    if (ld_int != -5.0L) return 12;
    if (d_big != 2147483647.0) return 13;

    if (d_add_f != 3.0) return 14;
    if (i_add_i != 3) return 15;

    if (agg.i != 3 || agg.d != 3.0) return 16;
    if (arr[0] != 1 || arr[1] != 2 || arr[2] != 3) return 17;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_scalar_initializers_take_the_objects_type", code, &[]),
        0
    );
}

/// The pre-<stddef.h> spelling of offsetof is an integer constant expression.
///
/// `(size_t)&((struct S *)0)->member` dereferences nothing -- it is arithmetic
/// on a null pointer -- so there is no symbol to relocate against and the
/// whole thing folds to an integer. It used to fold to nothing: the static
/// initializer silently became zero, and after global initializers started
/// rejecting what they could not fold, it became a hard error instead.
#[test]
fn c99_offsetof_by_null_pointer_is_a_constant() {
    let code = r#"
#include <stddef.h>

struct S { int a; double b; char c[8]; struct { int x, y; } n; };

static const size_t off_a = (size_t)&((struct S *)0)->a;
static const size_t off_b = (size_t)&((struct S *)0)->b;
static const size_t off_c = (size_t)&((struct S *)0)->c;
static const size_t off_c2 = (size_t)&((struct S *)0)->c[2];
static const size_t off_ny = (size_t)&((struct S *)0)->n.y;

/* An integer constant expression is usable as an array bound and a case
   label, not only as an initializer. */
static char sized[(size_t)&((struct S *)0)->b];

int pick(int k)
{
    switch (k) {
    case (int)(size_t)&((struct S *)0)->b: return 1;
    default: return 0;
    }
}

int main(void)
{
    if (off_a != offsetof(struct S, a)) return 1;
    if (off_b != offsetof(struct S, b)) return 2;
    if (off_c != offsetof(struct S, c)) return 3;
    if (off_c2 != offsetof(struct S, c) + 2) return 4;
    if (off_ny != offsetof(struct S, n.y)) return 5;
    if (sizeof(sized) != offsetof(struct S, b)) return 6;
    if (pick((int)offsetof(struct S, b)) != 1) return 7;

    /* The address of a real object is still a relocation, not an integer. */
    static int obj[4];
    static int *p = &obj[2];
    if (p != &obj[2]) return 8;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_offsetof_by_null_pointer_is_a_constant", code, &[]),
        0
    );
}

/// C17 6.7.9p14: the string literal initializing a character array may be
/// enclosed in braces, and it still initializes that array -- `char b[] =
/// {"hi"}` is `char[3]` holding `hi`, not an array of one element.
///
/// It was read as an ordinary initializer list, so the element count came out
/// as 1 and the characters were never copied in: `sizeof b` was 1 and printing
/// it gave garbage. `(char[]){"hi"}` was empty for the same reason, and an
/// explicitly-sized `char c[6] = {"hi"}` had the right size with the wrong
/// contents. Both the size deduction and the store path had the look-through
/// one level down, for `char names[3][4] = {"Sun", "Mon"}`, and neither had it
/// at the outermost level.
///
/// Every expectation here came from gcc on this source.
#[test]
fn c99_string_literal_initializer_may_be_braced() {
    let code = r#"
#include <wchar.h>

char sb[] = {"hi"};
char sc[6] = {"hi"};
wchar_t sw[] = {L"hi"};
const char *sp[] = {"aa", "bbb"};
char nested[3][4] = {"Sun", "Mon", "Tue"};
struct S { char tag[4]; int n; };
struct S ss = { {"ab"}, 7 };

int main(void) {
    char b[] = {"hi"};
    char c[6] = {"hi"};
    wchar_t w[] = {L"hi"};
    char *cl = (char[]){"hi"};
    struct S as = { {"cd"}, 9 };

    if (sizeof sb != 3 || sb[0] != 'h' || sb[1] != 'i' || sb[2] != 0) return 1;
    if (sizeof sc != 6 || sc[1] != 'i' || sc[2] != 0 || sc[5] != 0) return 2;
    if (sizeof sw / sizeof sw[0] != 3 || sw[1] != L'i' || sw[2] != 0) return 3;

    /* An array of pointers is the same shape one level up, and must not be
       swallowed by the look-through. */
    if (sizeof sp / sizeof sp[0] != 2 || sp[1][2] != 'b') return 4;
    if (nested[2][0] != 'T' || nested[0][3] != 0) return 5;
    if (ss.tag[1] != 'b' || ss.tag[2] != 0 || ss.n != 7) return 6;

    if (sizeof b != 3 || b[0] != 'h' || b[1] != 'i' || b[2] != 0) return 7;
    if (sizeof c != 6 || c[1] != 'i' || c[2] != 0 || c[5] != 0) return 8;
    if (sizeof w / sizeof w[0] != 3 || w[1] != L'i' || w[2] != 0) return 9;
    if (cl[0] != 'h' || cl[1] != 'i' || cl[2] != 0) return 10;
    if (as.tag[1] != 'd' || as.tag[2] != 0 || as.n != 9) return 11;

    if (sizeof((char[]){"abcd"}) != 5) return 12;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_string_literal_initializer_may_be_braced", code, &[]),
        0
    );
}
