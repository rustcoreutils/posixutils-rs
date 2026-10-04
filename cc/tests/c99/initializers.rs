//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 Initializers Mega-Tests
//
// Consolidates: designated initializers, compound literals, and the
// designated/struct initializer patterns real code (CPython) depends on.
//
// Split by topic: brace elision and string initializers are in
// initializers_strings.rs, static-initializer constant folding in
// initializers_static.rs, and designated overrides in
// initializers_overrides.rs.
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: C99 initializers (designated init, compound literals)
// ============================================================================

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

// ============================================================================
// Mega-test: designated and struct initializer patterns
// ============================================================================

// Original test documentation, in section order:
//
// ---- c99_initializers_cpython_llist_pattern ----
// ---- c99_initializers_cpython_opcode_pattern ----
// ---- c99_initializers_cpython_pytypeobject_pattern ----
// ---- c99_initializers_nested_designated_pattern ----
// ---- c99_initializers_sizeof_inferred_array ----
// Test that sizeof works correctly for arrays with size inferred from initializer
// This tests the fix for GitHub issue where sizeof(arr) returned 0 for arr[] = {...}
// ---- c99_initializers_bitfield_designated ----
// Test designated initialization of multiple bitfields within the same storage unit
// This tests the fix for a bug where only the last bitfield was initialized
// (due to incorrect deduplication of fields at the same offset)
// ---- c99_initializers_anon_struct_continuation ----
// ============================================================================
// BUG 2: Anonymous struct positional continuation after designator
// ============================================================================

// ---- c99_initializers_anon_struct_nested_continuation ----
// ---- c99_initializers_compound_literal_type_mismatch ----
// ============================================================================
// BUG 3: CompoundLiteral type mismatch
// ============================================================================

// ---- c99_aggregate_element_initializes_whole_aggregate ----
// An initializer element that is already an expression of the aggregate's own
// type initializes the whole aggregate (C17 6.7.9p13).
//
// It was instead treated as a brace-elision candidate, so filling one element
// consumed one *element* per scalar member rather than one:
// `struct P a[2] = {p, p};` put both structs into `a[0]`, left `a[1]`
// uninitialized, and assigned a struct where a scalar field was expected --
// giving `4 4 0 0` where gcc gives `4 5 4 5`. The nested case returned
// uninitialized stack, so this read whatever happened to be there.
// ---- c99_an_anonymous_first_union_member_counts_for_brace_elision ----
// A union whose first member is an anonymous structure takes that
// structure's scalars when its braces are elided (C17 6.7.9p17, 6.7.2.1p13).
//
// The count brace elision runs on asked for the union's first *named*
// member, which skips the anonymous one and found `q`, while the initializer
// walk filled `a` and `b`: `union U u[] = {1, 2, 3, 4}` came out as four
// elements, each holding one value.
// ---- c99_an_override_inside_a_string_initializer_keeps_the_string ----
// A designator naming one element of an array a string literal initialized
// replaces that element and keeps the rest (C17 6.7.9p19).
//
// A literal is one initializer for the whole array, so a static object had
// nothing to replace the element in and dropped the literal whole: `sc.s`
// came out as `"\0z"`. The literal is taken apart into its elements first.
//
/// Designated, positional and anonymous-member struct initializers in the
/// shapes real code uses, one C section per original test.
///
/// Consolidates (one C section each, a `t_<name>` function):
/// - `c99_initializers_cpython_llist_pattern`
/// - `c99_initializers_cpython_opcode_pattern`
/// - `c99_initializers_cpython_pytypeobject_pattern`
/// - `c99_initializers_nested_designated_pattern`
/// - `c99_initializers_sizeof_inferred_array`
/// - `c99_initializers_bitfield_designated`
/// - `c99_initializers_anon_struct_continuation`
/// - `c99_initializers_anon_struct_nested_continuation`
/// - `c99_initializers_compound_literal_type_mismatch`
/// - `c99_aggregate_element_initializes_whole_aggregate`
/// - `c99_an_anonymous_first_union_member_counts_for_brace_elision`
/// - `c99_an_override_inside_a_string_initializer_keeps_the_string`
///
/// Exit codes: see the map at the top of the program.
#[test]
fn c99_initializers_patterns_mega() {
    let code = r#"
/* Exit-code map: each section returns its original code, offset by
   the base listed in its banner.
       1..  2  c99_initializers_cpython_llist_pattern
       3..  8  c99_initializers_cpython_opcode_pattern
       9.. 13  c99_initializers_cpython_pytypeobject_pattern
      14.. 16  c99_initializers_nested_designated_pattern
      17.. 66  c99_initializers_sizeof_inferred_array
      67..100  c99_initializers_bitfield_designated
     101..121  c99_initializers_anon_struct_continuation
     122..145  c99_initializers_anon_struct_nested_continuation
     146..156  c99_initializers_compound_literal_type_mismatch
     157..171  c99_aggregate_element_initializes_whole_aggregate
     172..173  c99_an_anonymous_first_union_member_counts_for_brace_elision
     174..188  c99_an_override_inside_a_string_initializer_keeps_the_string
*/

/* ==== c99_initializers_cpython_llist_pattern: exit codes 1..2 (original code + 0) ==== */
struct ll_llist_node {
    struct ll_llist_node *next;
    struct ll_llist_node *prev;
};

#define ll_LLIST_INIT(head) { &head, &head }

struct ll_llist_node ll_my_list = ll_LLIST_INIT(ll_my_list);

static __attribute__((noinline)) int t_c99_initializers_cpython_llist_pattern(void) {
    if (ll_my_list.next != &ll_my_list) return 1;
    if (ll_my_list.prev != &ll_my_list) return 2;
    return 0;
}

/* ==== c99_initializers_cpython_opcode_pattern: exit codes 3..8 (original code + 2) ==== */
struct op_uop { int op; int arg; int off; };
struct op_expansion { int nuops; struct op_uop uops[4]; };

enum { op_OP_A = 5, op_OP_B = 10 };

struct op_expansion op_table[16] = {
    [op_OP_A] = { .nuops = 2, .uops = { {1, 2, 3}, {3, 4, 5} } },
    [op_OP_B] = { .nuops = 1, .uops = { {5, 6, 7} } },
};

static __attribute__((noinline)) int t_c99_initializers_cpython_opcode_pattern(void) {
    if (op_table[op_OP_A].nuops != 2) return 1;
    if (op_table[op_OP_A].uops[0].op != 1) return 2;
    if (op_table[op_OP_A].uops[1].arg != 4) return 3;
    if (op_table[op_OP_B].nuops != 1) return 4;
    if (op_table[op_OP_B].uops[0].op != 5) return 5;
    if (op_table[0].nuops != 0) return 6;
    return 0;
}

/* ==== c99_initializers_cpython_pytypeobject_pattern: exit codes 9..13 (original code + 8) ==== */
typedef void (*pt_func_t)(void);
struct pt_PyTypeObject {
    long ob_refcnt;
    void *ob_type;
    char *tp_name;
    long tp_basicsize;
    pt_func_t tp_dealloc;
    pt_func_t tp_repr;
    void *tp_as_number;
    long tp_flags;
    char *tp_doc;
};

void pt_my_dealloc(void) {}

struct pt_PyTypeObject pt_MyType = {
    1,
    0,
    "MyType",
    64,
    pt_my_dealloc,
    0,
    0,
    .tp_flags = 0x1234,
    .tp_doc = "doc",
};

static __attribute__((noinline)) int t_c99_initializers_cpython_pytypeobject_pattern(void) {
    if (pt_MyType.ob_refcnt != 1) return 1;
    if (pt_MyType.tp_basicsize != 64) return 2;
    if (pt_MyType.tp_dealloc != pt_my_dealloc) return 3;
    if (pt_MyType.tp_flags != 0x1234) return 4;
    if (pt_MyType.tp_doc[0] != 'd') return 5;
    return 0;
}

/* ==== c99_initializers_nested_designated_pattern: exit codes 14..16 (original code + 13) ==== */
struct nd_inner { int tag; int data; };
struct nd_outer {
    void *type;
    struct nd_inner value;
};

struct nd_outer nd_obj = {
    (void*)0x1234,
    { .tag = 42, .data = 99 }
};

static __attribute__((noinline)) int t_c99_initializers_nested_designated_pattern(void) {
    if (nd_obj.type != (void*)0x1234) return 1;
    if (nd_obj.value.tag != 42) return 2;
    if (nd_obj.value.data != 99) return 3;
    return 0;
}

/* ==== c99_initializers_sizeof_inferred_array: exit codes 17..66 (original code + 16) ==== */
// Global arrays with inferred size
int si_global_arr[] = {1, 2, 3, 4, 5};
static int si_static_arr[] = {10, 20, 30, 40, 50, 60};

// Array of structs
struct si_Pair { int x; int y; };
static struct si_Pair si_pairs[] = {{1, 2}, {3, 4}, {5, 6}};

// Pointer array
static int *si_ptrs[] = {0, 0, 0};

static __attribute__((noinline)) int t_c99_initializers_sizeof_inferred_array(void) {
    // Test global array
    if (sizeof(si_global_arr) != 20) return 1;  // 5 * 4 bytes
    if (sizeof(si_global_arr) / sizeof(si_global_arr[0]) != 5) return 2;

    // Test static array
    if (sizeof(si_static_arr) != 24) return 10;  // 6 * 4 bytes
    if (sizeof(si_static_arr) / sizeof(si_static_arr[0]) != 6) return 11;

    // Test local array with inferred size
    int local_arr[] = {100, 200, 300};
    if (sizeof(local_arr) != 12) return 20;  // 3 * 4 bytes
    if (sizeof(local_arr) / sizeof(local_arr[0]) != 3) return 21;

    // Test array of structs
    if (sizeof(si_pairs) != 24) return 30;  // 3 * (4 + 4) bytes
    if (sizeof(si_pairs) / sizeof(si_pairs[0]) != 3) return 31;

    // Test pointer array
    if (sizeof(si_ptrs) != 24) return 40;  // 3 * 8 bytes on 64-bit
    if (sizeof(si_ptrs) / sizeof(si_ptrs[0]) != 3) return 41;

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

/* ==== c99_initializers_bitfield_designated: exit codes 67..100 (original code + 66) ==== */
#include <stdio.h>

// Bitfields all packed into a single storage unit (like CPython's PyASCIIObject state)
struct bd_state {
    unsigned int interned:2;
    unsigned int kind:3;
    unsigned int compact:1;
    unsigned int ascii:1;
    unsigned int statically_allocated:1;
};

struct bd_obj {
    void *ptr;
    long length;
    long hash;
    struct bd_state bd_state;
};

// Test global designated initializer with multiple bitfields
struct bd_obj bd_test_obj = {
    .ptr = (void*)0x12345678,
    .length = 8,
    .hash = -1,
    .bd_state = {
        .kind = 1,
        .compact = 1,
        .ascii = 1,
        .statically_allocated = 1,
    },
};

// Test that all bitfields within the same byte can be initialized
struct bd_flags {
    unsigned int a:1;
    unsigned int b:1;
    unsigned int c:1;
    unsigned int d:1;
    unsigned int e:1;
    unsigned int f:1;
    unsigned int g:1;
    unsigned int h:1;
};

struct bd_flags bd_all_flags = {
    .a = 1, .b = 1, .c = 1, .d = 1,
    .e = 1, .f = 1, .g = 1, .h = 1,
};

struct bd_flags bd_some_flags = {
    .b = 1, .d = 1, .f = 1, .h = 1,
};

static __attribute__((noinline)) int t_c99_initializers_bitfield_designated(void) {
    // Verify global struct with nested bitfield struct
    if (bd_test_obj.ptr != (void*)0x12345678) return 1;
    if (bd_test_obj.length != 8) return 2;
    if (bd_test_obj.hash != -1) return 3;
    if (bd_test_obj.bd_state.interned != 0) return 4;
    if (bd_test_obj.bd_state.kind != 1) return 5;
    if (bd_test_obj.bd_state.compact != 1) return 6;
    if (bd_test_obj.bd_state.ascii != 1) return 7;
    if (bd_test_obj.bd_state.statically_allocated != 1) return 8;

    // Verify all flags set
    if (bd_all_flags.a != 1) return 10;
    if (bd_all_flags.b != 1) return 11;
    if (bd_all_flags.c != 1) return 12;
    if (bd_all_flags.d != 1) return 13;
    if (bd_all_flags.e != 1) return 14;
    if (bd_all_flags.f != 1) return 15;
    if (bd_all_flags.g != 1) return 16;
    if (bd_all_flags.h != 1) return 17;

    // Verify alternating flags
    if (bd_some_flags.a != 0) return 20;
    if (bd_some_flags.b != 1) return 21;
    if (bd_some_flags.c != 0) return 22;
    if (bd_some_flags.d != 1) return 23;
    if (bd_some_flags.e != 0) return 24;
    if (bd_some_flags.f != 1) return 25;
    if (bd_some_flags.g != 0) return 26;
    if (bd_some_flags.h != 1) return 27;

    // Local variable with bitfield designated init
    struct bd_obj local_obj = {
        .bd_state = {
            .interned = 2,
            .kind = 5,
            .compact = 0,
            .ascii = 1,
        },
    };
    if (local_obj.bd_state.interned != 2) return 30;
    if (local_obj.bd_state.kind != 5) return 31;
    if (local_obj.bd_state.compact != 0) return 32;
    if (local_obj.bd_state.ascii != 1) return 33;
    if (local_obj.bd_state.statically_allocated != 0) return 34;

    return 0;
}

/* ==== c99_initializers_anon_struct_continuation: exit codes 101..121 (original code + 100) ==== */
// Test: after designating a field in an anonymous struct,
// the next positional should continue within that anonymous struct

struct ac_WithAnonStruct {
    int a;
    struct {
        int x;
        int y;
        int z;
    };
    int c;
};

static __attribute__((noinline)) int t_c99_initializers_anon_struct_continuation(void) {
    // .x designates inside anon struct, 20 should go to y (not c)
    struct ac_WithAnonStruct s1 = {.a = 1, .x = 10, 20, 30, 99};
    if (s1.a != 1) return 1;
    if (s1.x != 10) return 2;
    if (s1.y != 20) return 3;
    if (s1.z != 30) return 4;
    if (s1.c != 99) return 5;

    // Only designating inside anon struct
    struct ac_WithAnonStruct s2 = {.x = 100, 200};
    if (s2.a != 0) return 10;
    if (s2.x != 100) return 11;
    if (s2.y != 200) return 12;
    if (s2.z != 0) return 13;

    // Designate last field of anon struct, then positional goes to outer
    struct ac_WithAnonStruct s3 = {.z = 50, 60};
    if (s3.z != 50) return 20;
    if (s3.c != 60) return 21;

    return 0;
}

/* ==== c99_initializers_anon_struct_nested_continuation: exit codes 122..145 (original code + 121) ==== */
// Test deeply nested anonymous structs: positional continuation must
// walk through all nesting levels correctly (C11 6.7.2.1p13).

struct an_Deep {
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

static __attribute__((noinline)) int t_c99_initializers_anon_struct_nested_continuation(void) {
    // .p is inside nested anon struct; 20 should go to q, then z, then c
    struct an_Deep d1 = {.a = 1, .p = 10, 20, 30, 40};
    if (d1.a != 1) return 1;
    if (d1.p != 10) return 2;
    if (d1.q != 20) return 3;
    if (d1.z != 30) return 4;
    if (d1.c != 40) return 5;

    // .q is last in inner anon; next positional goes to z (outer anon), then c
    struct an_Deep d2 = {.q = 100, 200, 300};
    if (d2.q != 100) return 10;
    if (d2.z != 200) return 11;
    if (d2.c != 300) return 12;

    // .x is in outer anon; positional should descend into inner anon next
    struct an_Deep d3 = {.x = 50, 60, 70, 80, 90};
    if (d3.x != 50) return 20;
    if (d3.p != 60) return 21;
    if (d3.q != 70) return 22;
    if (d3.z != 80) return 23;
    if (d3.c != 90) return 24;

    return 0;
}

/* ==== c99_initializers_compound_literal_type_mismatch: exit codes 146..156 (original code + 145) ==== */
struct cm_Small { int x; int y; };
struct cm_Big { int a; int b; int c; int d; };

// Global: compound literal type != target type, not pointer
// This exercises the else branch where cl_type must be used instead of target type
struct cm_Big cm_global_b = {.a = 1, .b = 2, .c = 3, .d = 4};

static __attribute__((noinline)) int t_c99_initializers_compound_literal_type_mismatch(void) {
    // Same-type compound literal (exercises the cl_type == typ branch)
    struct cm_Small s = (struct cm_Small){42, 99};
    if (s.x != 42) return 1;
    if (s.y != 99) return 2;

    // Verify global init too
    if (cm_global_b.a != 1) return 10;
    if (cm_global_b.d != 4) return 11;

    return 0;
}

/* ==== c99_aggregate_element_initializes_whole_aggregate: exit codes 157..171 (original code + 156) ==== */
#include <string.h>

struct ae_P { int x, y; };
struct ae_N { int a[2]; struct { int x, y; } in; };

static __attribute__((noinline)) int t_c99_aggregate_element_initializes_whole_aggregate(void) {
    struct ae_P p = {4, 5};

    /* The case that was wrong: elements are struct expressions, not braces. */
    struct ae_P a2[2] = {p, p};
    if (a2[0].x != 4 || a2[0].y != 5) return 1;
    if (a2[1].x != 4 || a2[1].y != 5) return 2;

    /* Nested aggregate, where the old path returned uninitialized stack. */
    struct ae_N n = {{1, 2}, {4, 5}};
    struct ae_N b2[2] = {n, n};
    if (b2[0].a[0] != 1 || b2[0].in.x != 4) return 3;
    if (b2[1].a[0] != 1 || b2[1].in.x != 4) return 4;
    if (memcmp(&b2[0], &n, sizeof n) != 0) return 5;
    if (memcmp(&b2[1], &n, sizeof n) != 0) return 6;

    /* Mixing an expression element with a brace list must still work. */
    struct ae_P mix[2] = {p, {8, 9}};
    if (mix[0].x != 4 || mix[0].y != 5) return 7;
    if (mix[1].x != 8 || mix[1].y != 9) return 8;

    /* Fewer initializers than elements: the rest are zeroed, not garbage. */
    struct ae_P few[3] = {p};
    if (few[0].x != 4 || few[1].x != 0 || few[2].x != 0) return 9;
    if (few[1].y != 0 || few[2].y != 0) return 10;

    /* A single struct initialized from another, and one member from an
       expression -- the same rule one level down. */
    struct ae_N copy = n;
    if (copy.a[0] != 1 || copy.in.x != 4) return 11;

    /* The paths that always worked, pinned so this fix cannot regress them. */
    struct ae_P braces[2] = {{4, 5}, {6, 7}};
    if (braces[0].x != 4 || braces[1].x != 6) return 12;
    static struct ae_P statics[2] = {{4, 5}, {6, 7}};
    if (statics[0].x != 4 || statics[1].x != 6) return 13;
    struct ae_P assigned[2];
    assigned[0] = p; assigned[1] = p;
    if (assigned[0].x != 4 || assigned[1].x != 4) return 14;
    struct ae_N desig = {.in = {7, 8}};
    if (desig.in.x != 7 || desig.in.y != 8) return 15;

    return 0;
}

/* ==== c99_an_anonymous_first_union_member_counts_for_brace_elision: exit codes 172..173 (original code + 171) ==== */
union au_U { struct { int a, b; }; long q; };
union au_U au_u[] = { 1, 2, 3, 4 };
static __attribute__((noinline)) int t_c99_an_anonymous_first_union_member_counts_for_brace_elision(void) {
    if (sizeof au_u / sizeof au_u[0] != 2) return 1;
    if (au_u[0].a != 1 || au_u[0].b != 2 || au_u[1].a != 3 || au_u[1].b != 4) return 2;
    return 0;
}

/* ==== c99_an_override_inside_a_string_initializer_keeps_the_string: exit codes 174..188 (original code + 173) ==== */
struct so_C { char s[6]; int z; };
static struct so_C so_sc = { .s = "abcd", .s[1] = 'z' };
static __attribute__((noinline)) int t_c99_an_override_inside_a_string_initializer_keeps_the_string(void) {
    struct so_C lc = { .s = "abcd", .s[1] = 'z' };
    const char *want = "azcd";
    for (int i = 0; i < 6; i++) {
        char e = i < 4 ? want[i] : 0;
        if (so_sc.s[i] != e) return 1 + i;
        if (lc.s[i] != e) return 10 + i;
    }
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_c99_initializers_cpython_llist_pattern()) != 0) return 0 + r;
    if ((r = t_c99_initializers_cpython_opcode_pattern()) != 0) return 2 + r;
    if ((r = t_c99_initializers_cpython_pytypeobject_pattern()) != 0) return 8 + r;
    if ((r = t_c99_initializers_nested_designated_pattern()) != 0) return 13 + r;
    if ((r = t_c99_initializers_sizeof_inferred_array()) != 0) return 16 + r;
    if ((r = t_c99_initializers_bitfield_designated()) != 0) return 66 + r;
    if ((r = t_c99_initializers_anon_struct_continuation()) != 0) return 100 + r;
    if ((r = t_c99_initializers_anon_struct_nested_continuation()) != 0) return 121 + r;
    if ((r = t_c99_initializers_compound_literal_type_mismatch()) != 0) return 145 + r;
    if ((r = t_c99_aggregate_element_initializes_whole_aggregate()) != 0) return 156 + r;
    if ((r = t_c99_an_anonymous_first_union_member_counts_for_brace_elision()) != 0) return 171 + r;
    if ((r = t_c99_an_override_inside_a_string_initializer_keeps_the_string()) != 0) return 173 + r;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_initializers_patterns_mega", code, &[]),
        0
    );
}
