//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for __builtin_offsetof builtin
//

use crate::common::compile_and_run;

#[test]
fn offsetof_basic() {
    let code = r#"
struct S {
    int a;
    int b;
    char c;
};

int main(void) {
    // offsetof(S, a) should be 0
    if (__builtin_offsetof(struct S, a) != 0) return 1;

    // offsetof(S, b) should be sizeof(int) = 4
    if (__builtin_offsetof(struct S, b) != sizeof(int)) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("offsetof_basic", code, &[]), 0);
}

#[test]
fn offsetof_nested() {
    let code = r#"
struct Inner { int x; int y; };
struct Outer { int a; struct Inner inner; };

int main(void) {
    // offsetof nested member
    unsigned long off = __builtin_offsetof(struct Outer, inner.y);
    // a=4 bytes, inner.x=4 bytes, so inner.y at offset 8
    if (off != sizeof(int) + sizeof(int)) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("offsetof_nested", code, &[]), 0);
}

#[test]
fn offsetof_array() {
    let code = r#"
struct S { int arr[10]; };

int main(void) {
    // offsetof with array index
    unsigned long off = __builtin_offsetof(struct S, arr[5]);
    if (off != 5 * sizeof(int)) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("offsetof_array", code, &[]), 0);
}

#[test]
fn offsetof_macro_style() {
    let code = r#"
struct S { int x; int y; };

int main(void) {
    // Using 'offsetof' spelling (macro-compatible)
    unsigned long off = offsetof(struct S, y);
    if (off != sizeof(int)) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("offsetof_macro_style", code, &[]), 0);
}

#[test]
fn offsetof_static_initializer() {
    // Test offsetof in static array initializers (was producing all zeros)
    let code = r#"
struct S {
    int a;
    int b;
    long c;
};

// Static array with offsetof values - must be computed at compile time
static const unsigned long offsets[] = {
    offsetof(struct S, a),
    offsetof(struct S, b),
    offsetof(struct S, c),
};

int main(void) {
    // a is at offset 0
    if (offsets[0] != 0) return 1;
    // b is at offset 4 (after int a)
    if (offsets[1] != sizeof(int)) return 2;
    // c is at offset 8 (after int a, int b, with padding)
    if (offsets[2] != 2 * sizeof(int)) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("offsetof_static_initializer", code, &[]), 0);
}

#[test]
fn offsetof_static_struct_initializer() {
    // Test offsetof in static struct initializers
    let code = r#"
struct Config {
    int a;
    int b;
    long c;
};

struct Spec {
    const char *name;
    unsigned long offset;
};

#define SPEC(member) { #member, offsetof(struct Config, member) }

static const struct Spec specs[] = {
    SPEC(a),
    SPEC(b),
    SPEC(c),
    { 0, 0 }
};

int main(void) {
    // Verify offsets are correct
    if (specs[0].offset != 0) return 1;
    if (specs[1].offset != sizeof(int)) return 2;
    if (specs[2].offset != 2 * sizeof(int)) return 3;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("offsetof_static_struct_initializer", code, &[]),
        0
    );
}

#[test]
fn static_addr_of_member() {
    // Test address-of struct member in static initializer (was producing NULL)
    let code = r#"
struct Inner {
    int x;
    int y;
};

struct Outer {
    int a;
    struct Inner inner;
};

// Self-referential initializer: pointer to own member
static struct Outer obj = {
    .a = 42,
    .inner = { .x = 1, .y = 2 },
};

static int *ptr_to_x = &obj.inner.x;
static int *ptr_to_y = &obj.inner.y;

int main(void) {
    // Verify pointers point to correct members
    if (*ptr_to_x != 1) return 1;
    if (*ptr_to_y != 2) return 2;

    // Verify pointer addresses are correct
    if (ptr_to_x != &obj.inner.x) return 3;
    if (ptr_to_y != &obj.inner.y) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("static_addr_of_member", code, &[]), 0);
}

#[test]
fn static_self_referential_init() {
    // Test self-referential struct initialization (common pattern in CPython, etc.)
    let code = r#"
struct Node {
    struct Node *prev;
    struct Node *next;
};

// LLIST_INIT pattern: circular self-reference
#define LLIST_INIT(head) { &head, &head }

struct Queue {
    struct Node head;
};

#define QUEUE_INIT(q) { .head = LLIST_INIT(q.head) }

static struct Queue queue = QUEUE_INIT(queue);

int main(void) {
    // Verify circular self-reference
    if (queue.head.prev != &queue.head) return 1;
    if (queue.head.next != &queue.head) return 2;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("static_self_referential_init", code, &[]),
        0
    );
}

#[test]
fn static_nested_self_ref() {
    // Test nested self-referential initialization
    let code = r#"
struct Node {
    struct Node *next;
};

struct Container {
    int id;
    struct Node node;
};

struct Runtime {
    int flags;
    struct Container cont;
};

// Nested macro expansion with self-reference
#define NODE_INIT(n) { .next = &n }
#define CONTAINER_INIT(c) { .id = 1, .node = NODE_INIT(c.node) }
#define RUNTIME_INIT(r) { .flags = 0, .cont = CONTAINER_INIT(r.cont) }

static struct Runtime runtime = RUNTIME_INIT(runtime);

int main(void) {
    // Verify self-reference through nested structs
    if (runtime.cont.node.next != &runtime.cont.node) return 1;
    if (runtime.cont.id != 1) return 2;
    if (runtime.flags != 0) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("static_nested_self_ref", code, &[]), 0);
}
