//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Operands referred to by name: `%[name]` in the template, and `"[name]"` as
// the matching constraint for the output of that name.
//

use crate::common::compile_and_run_everywhere;

/// A named matching constraint ties its input to the output it names, by
/// name alone or inside a longer constraint, and a name works beside the
/// numbers of unnamed operands.
#[test]
fn asm_named_operands_and_named_matching_constraints() {
    let src = r#"
#if defined(__x86_64__)
#define ADD(d, s) "add %[" #s "], %[" #d "]"
#elif defined(__aarch64__)
#define ADD(d, s) "add %[" #d "], %[" #d "], %[" #s "]"
#endif
__attribute__((noinline)) long by_name(long a, long b) {
    long sum;
    __asm__(ADD(sum, b) : [sum] "=r"(sum) : "[sum]"(a), [b] "r"(b));
    return sum;
}
__attribute__((noinline)) long inside(long a, long b) {
    long sum;
    __asm__(ADD(sum, b) : [sum] "=r"(sum) : "r[sum]"(a), [b] "r"(b));
    return sum;
}
__attribute__((noinline)) long read_write(long a, long b) {
    __asm__(ADD(aa, bb) : [aa] "+r"(a) : [bb] "r"(b));
    return a;
}
/* Two outputs, each tied by name to an input in the other order. */
__attribute__((noinline)) long crossed(long a, long b) {
    long s, t;
    __asm__(ADD(s, t) : [s] "=r"(s), [t] "=r"(t) : "[t]"(a), "[s]"(b));
    return s * 100 + t;
}
int main(void) {
    if (by_name(3, 4) != 7) return 1;
    if (inside(5, 6) != 11) return 2;
    if (read_write(30, 4) != 34) return 3;
    if (crossed(1, 2) != 300 + 1) return 4;
    return 0;
}
"#;
    compile_and_run_everywhere("asm_named_operands", src);
}

/// Two inputs tied to one output that read the same local variable are one
/// value in the output's register, as gcc has it: `%2` is `%0`.
#[test]
fn asm_two_ties_reading_one_variable() {
    let src = r#"
#if defined(__x86_64__)
#define DOUBLE "add %2, %0"
#elif defined(__aarch64__)
#define DOUBLE "add %0, %0, %2"
#endif
__attribute__((noinline)) long twice(long a) {
    long sum;
    __asm__(DOUBLE : "=r"(sum) : "0"(a), "0"(a));
    return sum;
}
int main(void) { return twice(21) == 42 ? 0 : 1; }
"#;
    compile_and_run_everywhere("asm_two_ties_one_variable", src);
}
