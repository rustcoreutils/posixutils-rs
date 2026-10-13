//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A call through a pointer holding one known function is made a direct call
// of it when optimizing (`ir::devirt`), as gcc does from -O1. The calls must
// still reach the same functions with the same arguments, a weak one and one
// variadic included, on both targets at every level.
//

use crate::common::compile_and_run_everywhere;

#[test]
fn devirtualized_calls_reach_their_functions() {
    let src = r#"
#include <stdarg.h>
static int calls;
__attribute__((noinline)) int add(int a, int b) { calls++; return a + b; }
__attribute__((noinline, weak)) int weak_twice(int a) { calls++; return 2 * a; }
__attribute__((noinline)) int sum(int n, ...) {
    va_list ap; va_start(ap, n); int s = 0;
    for (int i = 0; i < n; i++) s += va_arg(ap, int);
    va_end(ap); calls++; return s;
}
__attribute__((noinline)) double half(double x) { calls++; return x / 2; }
static int (*const twice_ptr)(int) = weak_twice;
struct ops { const char *name; int (*bin)(int, int); double (*fp)(double); };
static const struct ops table[2] = { { "a", add, half }, { "b", add, 0 } };

int main(void) {
    int (*p)(int, int) = add;
    if (p(2, 3) != 5) return 1;
    int (*v)(int, ...) = sum;
    if (v(3, 1, 2, 3) != 6) return 2;
    if (twice_ptr(21) != 42) return 3;
    if (table[0].bin(4, 5) != 9) return 4;
    if (table[0].fp(3.0) != 1.5) return 5;
    if (table[1].bin(1, 1) != 2) return 6;
    double (*h)(double) = half;
    if (h(1.0) != 0.5) return 7;
    if (calls != 7) return 8;
    return 0;
}
"#;
    compile_and_run_everywhere("devirt_calls", src);
}
