//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU label differences in static initializers: `&&a - &&b`, optionally plus
// or minus an integer constant, stored to an integer of any width. glibc's
// vfprintf jump tables are built this way and dispatched with
// `goto *(&&base + table[i])`.
//

use crate::common::compile_and_run_everywhere;

/// A table of offsets from a base label at `int`, `short` and `long` widths,
/// one with an addend, dispatched by a computed goto from the base label.
#[test]
fn codegen_label_difference_jump_table() {
    const SRC: &str = r#"/* The difference of two label addresses as a constant (GNU): a static
   jump table of offsets from a base label, the shape glibc's vfprintf
   uses, dispatched with `goto *(&&base + table[i])`. */
static int dispatch(int op, int x)
{
    static const int table[] = {
        &&op_add - &&op_add,
        &&op_sub - &&op_add,
        &&op_mul - &&op_add,
        &&op_neg - &&op_add,
    };
    /* Narrower and wider entry types, and an offset added to the
       difference, are initializers gcc accepts too. */
    static const short narrow[] = {&&op_mul - &&op_add, &&op_neg - &&op_add};
    static const long wide[] = {&&op_sub - &&op_add + 0};
    if (narrow[0] != table[2] || narrow[1] != table[3] || wide[0] != table[1])
        return -1000;
    goto *(&&op_add + table[op]);
op_add:
    return x + 1;
op_sub:
    return x - 1;
op_mul:
    return x * 2;
op_neg:
    return -x;
}

int main(void)
{
    if (dispatch(0, 5) != 6) return 1;
    if (dispatch(1, 5) != 4) return 2;
    if (dispatch(2, 5) != 10) return 3;
    if (dispatch(3, 5) != -5) return 4;
    return 0;
}
"#;
    compile_and_run_everywhere("label_diff", SRC);
}

/// Addends in every position, folded in bytes, agree with the runtime
/// difference; labels reached only through the table survive; and a static
/// function that is never called takes its table with it (the table names
/// labels that no longer exist, which the assembler rejects). gcc 13 negates
/// the addend in `(&&b + 4) - &&a`, so this is checked against the runtime
/// difference rather than against gcc.
#[test]
fn codegen_label_difference_addends_and_dead_tables() {
    const SRC: &str = r#"
static int unused(int i)
{
    static const int t[] = {&&a - &&b, &&b - &&a + 1};
    static const int *const pt = t;
    goto *(&&a + pt[i]);
a:
    return 1;
b:
    return 2;
}

int c;

/* pr70460's shape: the targets are reached only through the table. */
__attribute__((noinline)) void step(int x)
{
    static int b[] = { &&lab1 - &&lab0, &&lab2 - &&lab0 };
    goto *(&&lab0 + b[x]);
lab1:
    c += 2;
lab2:
    c++;
lab0:
    ;
}

__attribute__((noinline)) long offsets(int i)
{
    static const long w[] = {
        (&&b + 4) - &&a, -(&&a - &&b), 3 + (&&b - &&a) - 1,
        (char *)&&b - (char *)&&a - 7, (long)&&b - (long)&&a,
    };
    static const short s[] = {&&b - &&a + 2};
    static const signed char k[] = {&&a - &&a + 5};
    volatile long d = &&b - &&a;
    if (i < 0)
        goto *(&&a + w[0]);
    if (w[0] != d + 4 || w[1] != d || w[2] != d + 2 || w[3] != d - 7 || w[4] != d)
        return 1;
    if (s[0] != (short)(d + 2) || k[0] != 5)
        return 2;
    return 0;
a:
    return 10;
b:
    return 20;
}

int main(void)
{
    step(0);
    if (c != 3) return 1;
    step(1);
    if (c != 4) return 2;
    return (int)offsets(0) * 10;
}
"#;
    compile_and_run_everywhere("label_diff_addends", SRC);
}
