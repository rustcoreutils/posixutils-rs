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
