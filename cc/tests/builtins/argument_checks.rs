//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Builtin argument checking: what gcc rejects, c17 must reject
//

use crate::common::compile_and_run_everywhere;

/// The valid calls compute what gcc computes: a prototyped builtin converts
/// its argument, a type-generic one keeps it, and the flag operations touch
/// one byte.
#[test]
fn builtins_unusual_valid_arguments_compute_as_gcc() {
    let src = r#"enum E { A, B, C };
int main(void) {
    int i = 1;
    if (__builtin_isnanf(i)) return 1;
    if (!__builtin_signbitf(-i)) return 2;
    double d = 0.5;
    if (__builtin_fpclassify(0.5, 10, 20, 30, 40, d) != 20) return 3;
    const double cd = 3.0;
    _Complex double z = __builtin_complex(cd, d);
    if (__real__ z != 3.0 || __imag__ z != 0.5) return 4;
    enum E e = C; _Bool b = 1; unsigned u;
    if (__builtin_sub_overflow(b, e, &u) != 1 || u != 0xffffffffu) return 5;
    int r;
    if (__builtin_sadd_overflow(1.5, 2.9, &r) || r != 3) return 6;
    char arr[10];
    if (__builtin_object_size(arr, B) != 10) return 7;
    if (__builtin_object_size(&arr[4], (int)1.0) != 6) return 8;
    unsigned flag = 0x100;
    if (__atomic_test_and_set(&flag, 5) || flag != 0x101) return 9;
    unsigned other = 0x1ff;
    __atomic_clear(&other, 5);
    if (other != 0x100) return 10;
    char *p = __builtin_alloca(e);
    p[0] = 1;
    if (__builtin_add_overflow_p(100, 100, (signed char)0) != 1) return 11;
    if (!__atomic_always_lock_free(sizeof(int), 0)) return 12;
    return 0;
}
"#;
    compile_and_run_everywhere("argchk_semantics", src);
}
