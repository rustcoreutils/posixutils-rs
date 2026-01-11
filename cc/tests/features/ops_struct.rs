//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for kernel-style operations structs with function pointer members
// Pattern: ptr->func_member(args...) where struct members are function pointers
// Common in Linux kernel (file_operations, inode_operations, etc.)
//

use crate::common::compile_and_run;

/// Test basic operations struct pattern: pointer to struct with function pointer members
#[test]
fn test_ops_struct_basic() {
    let code = r#"
// Simulated file operations struct (kernel pattern)
struct file_ops {
    int (*open)(const char *path, int flags, int mode);
    int (*read)(int fd, char *buf, int count);
    int (*write)(int fd, const char *buf, int count);
    int (*close)(int fd);
};

// Implementation functions
int my_open(const char *path, int flags, int mode) {
    // Verify args passed correctly: path[0]='/', flags=1, mode=0444=292
    if (path[0] != '/') return -1;
    if (flags != 1) return -2;
    if (mode != 292) return -3;
    return 42;  // Return fake fd
}

int my_read(int fd, char *buf, int count) {
    if (fd != 42) return -10;
    if (count != 100) return -11;
    buf[0] = 'H';
    return 1;  // Read 1 byte
}

int my_write(int fd, const char *buf, int count) {
    if (fd != 42) return -20;
    if (buf[0] != 'W') return -21;
    if (count != 5) return -22;
    return count;
}

int my_close(int fd) {
    if (fd != 42) return -30;
    return 0;
}

int main(void) {
    // Create ops struct and pointer (kernel pattern)
    struct file_ops ops;
    struct file_ops *fops = &ops;

    // Initialize function pointers
    fops->open = my_open;
    fops->read = my_read;
    fops->write = my_write;
    fops->close = my_close;

    // Test: call through pointer-to-struct->function_pointer(args)
    int fd = fops->open("/tmp/test", 1, 0444);
    if (fd != 42) return 1;

    char buf[100];
    int n = fops->read(fd, buf, 100);
    if (n != 1) return 2;
    if (buf[0] != 'H') return 3;

    n = fops->write(fd, "Write", 5);
    if (n != 5) return 4;

    int ret = fops->close(fd);
    if (ret != 0) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ops_struct_basic", code, &[]), 0);
}

/// Test ops struct with mixed int/float/pointer arguments
#[test]
fn test_ops_struct_mixed_args() {
    let code = r#"
// Operations struct with mixed argument types
struct math_ops {
    double (*compute)(int a, double b, int c, double d, void *ctx);
    int (*validate)(double threshold, int count, const char *name);
};

double my_compute(int a, double b, int c, double d, void *ctx) {
    // a=10, b=2.5, c=20, d=3.5, ctx should be non-null
    if (ctx == (void*)0) return -1.0;
    return (double)a * b + (double)c * d;  // 10*2.5 + 20*3.5 = 25 + 70 = 95
}

int my_validate(double threshold, int count, const char *name) {
    // threshold=50.0, count=3, name="test"
    if (threshold < 49.9 || threshold > 50.1) return -1;
    if (count != 3) return -2;
    if (name[0] != 't') return -3;
    return 1;  // Valid
}

int main(void) {
    struct math_ops ops;
    struct math_ops *mops = &ops;

    mops->compute = my_compute;
    mops->validate = my_validate;

    int dummy_ctx = 123;
    double result = mops->compute(10, 2.5, 20, 3.5, &dummy_ctx);
    // Expected: 10*2.5 + 20*3.5 = 25 + 70 = 95
    if (result < 94.9 || result > 95.1) return 1;

    int valid = mops->validate(50.0, 3, "test");
    if (valid != 1) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ops_struct_mixed", code, &[]), 0);
}

/// Test nested ops struct pattern (ops->sub_ops->func)
#[test]
fn test_ops_struct_nested() {
    let code = r#"
struct inner_ops {
    int (*process)(int x, int y);
};

struct outer_ops {
    struct inner_ops *inner;
    int (*init)(void);
};

int my_process(int x, int y) {
    return x + y;
}

int my_init(void) {
    return 42;
}

int main(void) {
    struct inner_ops inner;
    inner.process = my_process;

    struct outer_ops outer;
    outer.inner = &inner;
    outer.init = my_init;

    struct outer_ops *ops = &outer;

    // Test nested: ops->inner->process(args)
    int result = ops->inner->process(30, 12);
    if (result != 42) return 1;

    // Test direct: ops->init()
    int init_val = ops->init();
    if (init_val != 42) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ops_struct_nested", code, &[]), 0);
}

/// Test ops struct with many arguments (forcing stack usage)
#[test]
fn test_ops_struct_many_args() {
    let code = r#"
struct io_ops {
    // Function with many args: 7 ints + 3 doubles = forces stack usage
    double (*transfer)(int a, int b, int c, int d, int e, int f, int g,
                       double rate1, double rate2, double rate3);
};

double my_transfer(int a, int b, int c, int d, int e, int f, int g,
                   double rate1, double rate2, double rate3) {
    // Sum ints: 1+2+3+4+5+6+7 = 28
    int int_sum = a + b + c + d + e + f + g;
    // Sum doubles: 0.5+1.5+2.5 = 4.5
    double fp_sum = rate1 + rate2 + rate3;
    return (double)int_sum + fp_sum;  // 28 + 4.5 = 32.5
}

int main(void) {
    struct io_ops ops;
    struct io_ops *iops = &ops;

    iops->transfer = my_transfer;

    double result = iops->transfer(1, 2, 3, 4, 5, 6, 7, 0.5, 1.5, 2.5);
    // Expected: 28 + 4.5 = 32.5
    if (result < 32.4 || result > 32.6) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ops_struct_many_args", code, &[]), 0);
}

/// Test ops struct with function pointers returning pointers
#[test]
fn test_ops_struct_return_pointer() {
    let code = r#"
struct node {
    int value;
    struct node *next;
};

struct list_ops {
    struct node *(*create)(int value);
    struct node *(*find)(struct node *head, int value);
    const char *(*describe)(struct node *n);
};

struct node nodes[10];
int node_count = 0;

struct node *my_create(int value) {
    struct node *n = &nodes[node_count++];
    n->value = value;
    n->next = (struct node *)0;
    return n;
}

struct node *my_find(struct node *head, int value) {
    while (head != (struct node *)0) {
        if (head->value == value) return head;
        head = head->next;
    }
    return (struct node *)0;
}

const char *my_describe(struct node *n) {
    if (n == (struct node *)0) return "null";
    if (n->value == 42) return "answer";
    return "other";
}

int main(void) {
    struct list_ops ops;
    struct list_ops *lops = &ops;

    lops->create = my_create;
    lops->find = my_find;
    lops->describe = my_describe;

    // Test returning pointer
    struct node *n1 = lops->create(42);
    if (n1 == (struct node *)0) return 1;
    if (n1->value != 42) return 2;

    struct node *n2 = lops->create(100);
    n1->next = n2;

    // Test find returning pointer
    struct node *found = lops->find(n1, 100);
    if (found == (struct node *)0) return 3;
    if (found->value != 100) return 4;

    // Test returning const char *
    const char *desc = lops->describe(n1);
    if (desc[0] != 'a') return 5;  // "answer"

    return 0;
}
"#;
    assert_eq!(compile_and_run("ops_struct_ret_ptr", code, &[]), 0);
}

/// Test ops struct with function pointers returning small structs (fits in registers)
#[test]
fn test_ops_struct_return_small_struct() {
    let code = r#"
// Small struct - fits in 2 registers (16 bytes)
struct point {
    int x;
    int y;
};

struct pair {
    long a;
    long b;
};

struct geom_ops {
    struct point (*make_point)(int x, int y);
    struct pair (*make_pair)(long a, long b);
    struct point (*add_points)(struct point p1, struct point p2);
};

struct point my_make_point(int x, int y) {
    struct point p;
    p.x = x;
    p.y = y;
    return p;
}

struct pair my_make_pair(long a, long b) {
    struct pair p;
    p.a = a;
    p.b = b;
    return p;
}

struct point my_add_points(struct point p1, struct point p2) {
    struct point result;
    result.x = p1.x + p2.x;
    result.y = p1.y + p2.y;
    return result;
}

int main(void) {
    struct geom_ops ops;
    struct geom_ops *gops = &ops;

    gops->make_point = my_make_point;
    gops->make_pair = my_make_pair;
    gops->add_points = my_add_points;

    // Test returning small struct
    struct point p1 = gops->make_point(10, 20);
    if (p1.x != 10) return 1;
    if (p1.y != 20) return 2;

    struct pair pr = gops->make_pair(100, 200);
    if (pr.a != 100) return 3;
    if (pr.b != 200) return 4;

    // Test struct args and struct return
    struct point p2 = gops->make_point(5, 7);
    struct point sum = gops->add_points(p1, p2);
    if (sum.x != 15) return 5;
    if (sum.y != 27) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ops_struct_ret_small", code, &[]), 0);
}

/// Test ops struct with function pointers returning large structs (sret ABI)
#[test]
fn test_ops_struct_return_large_struct() {
    let code = r#"
// Large struct - requires sret (hidden pointer parameter)
struct big_data {
    long a;
    long b;
    long c;
    long d;
    long e;
};

// Even larger - 10 longs = 80 bytes
struct huge_data {
    long values[10];
};

struct data_ops {
    struct big_data (*create_big)(long a, long b, long c, long d, long e);
    struct huge_data (*create_huge)(long base);
    struct big_data (*transform)(struct big_data input, long multiplier);
};

struct big_data my_create_big(long a, long b, long c, long d, long e) {
    struct big_data result;
    result.a = a;
    result.b = b;
    result.c = c;
    result.d = d;
    result.e = e;
    return result;
}

struct huge_data my_create_huge(long base) {
    struct huge_data result;
    for (int i = 0; i < 10; i++) {
        result.values[i] = base + i;
    }
    return result;
}

struct big_data my_transform(struct big_data input, long multiplier) {
    struct big_data result;
    result.a = input.a * multiplier;
    result.b = input.b * multiplier;
    result.c = input.c * multiplier;
    result.d = input.d * multiplier;
    result.e = input.e * multiplier;
    return result;
}

int main(void) {
    struct data_ops ops;
    struct data_ops *dops = &ops;

    dops->create_big = my_create_big;
    dops->create_huge = my_create_huge;
    dops->transform = my_transform;

    // Test large struct return (5 longs = 40 bytes, needs sret)
    struct big_data bd = dops->create_big(1, 2, 3, 4, 5);
    if (bd.a != 1) return 1;
    if (bd.b != 2) return 2;
    if (bd.c != 3) return 3;
    if (bd.d != 4) return 4;
    if (bd.e != 5) return 5;

    // Test huge struct return (80 bytes)
    struct huge_data hd = dops->create_huge(100);
    if (hd.values[0] != 100) return 6;
    if (hd.values[9] != 109) return 7;

    // Test large struct arg + large struct return
    struct big_data transformed = dops->transform(bd, 10);
    if (transformed.a != 10) return 8;
    if (transformed.b != 20) return 9;
    if (transformed.e != 50) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ops_struct_ret_large", code, &[]), 0);
}

/// Test ops struct with mixed return types in same struct
#[test]
fn test_ops_struct_mixed_returns() {
    let code = r#"
struct result_int {
    int value;
    int error;
};

struct result_fp {
    double value;
    int error;
};

struct mixed_ops {
    int (*get_int)(void);
    double (*get_double)(void);
    void *(*get_ptr)(int id);
    struct result_int (*safe_div)(int a, int b);
    struct result_fp (*safe_sqrt)(double x);
};

int my_get_int(void) { return 42; }
double my_get_double(void) { return 3.14159; }

int storage[5] = {10, 20, 30, 40, 50};
void *my_get_ptr(int id) {
    if (id < 0 || id >= 5) return (void *)0;
    return &storage[id];
}

struct result_int my_safe_div(int a, int b) {
    struct result_int r;
    if (b == 0) {
        r.value = 0;
        r.error = -1;
    } else {
        r.value = a / b;
        r.error = 0;
    }
    return r;
}

struct result_fp my_safe_sqrt(double x) {
    struct result_fp r;
    if (x < 0) {
        r.value = 0.0;
        r.error = -1;
    } else {
        // Approximate sqrt using Newton's method (3 iterations)
        double guess = x / 2.0;
        guess = (guess + x / guess) / 2.0;
        guess = (guess + x / guess) / 2.0;
        guess = (guess + x / guess) / 2.0;
        r.value = guess;
        r.error = 0;
    }
    return r;
}

int main(void) {
    struct mixed_ops ops;
    struct mixed_ops *mops = &ops;

    mops->get_int = my_get_int;
    mops->get_double = my_get_double;
    mops->get_ptr = my_get_ptr;
    mops->safe_div = my_safe_div;
    mops->safe_sqrt = my_safe_sqrt;

    // Test int return
    if (mops->get_int() != 42) return 1;

    // Test double return
    double pi = mops->get_double();
    if (pi < 3.14 || pi > 3.15) return 2;

    // Test pointer return
    int *p = (int *)mops->get_ptr(2);
    if (p == (int *)0) return 3;
    if (*p != 30) return 4;

    // Test struct with int return
    struct result_int ri = mops->safe_div(100, 4);
    if (ri.error != 0) return 5;
    if (ri.value != 25) return 6;

    // Test division by zero handling
    ri = mops->safe_div(100, 0);
    if (ri.error != -1) return 7;

    // Test struct with fp return (sqrt of 16 should be ~4)
    struct result_fp rf = mops->safe_sqrt(16.0);
    if (rf.error != 0) return 8;
    if (rf.value < 3.99 || rf.value > 4.01) return 9;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ops_struct_mixed_ret", code, &[]), 0);
}
