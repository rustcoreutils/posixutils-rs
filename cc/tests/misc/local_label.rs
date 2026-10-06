//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__label__` (GNU): block-scoped labels, so a statement-expression macro
// that jumps to its own label can be expanded twice in one function.
//

use crate::common::compile_and_run_everywhere;

#[test]
fn local_labels_are_scoped_to_their_block() {
    compile_and_run_everywhere(
        "local_label",
        r#"
/* `__label__` (GNU): a label declared at the head of a block is local to
   it, so a macro that jumps to its own label can be expanded more than
   once in one function -- the reason it exists. */

/* Search a 2-D array, leaving both loops with one goto. */
#define FIND(arr, rows, cols, want)                                            \
    ({                                                                         \
        __label__ found;                                                       \
        int r_ = -1;                                                           \
        for (int i_ = 0; i_ < (rows); i_++)                                    \
            for (int j_ = 0; j_ < (cols); j_++)                                \
                if ((arr)[i_][j_] == (want)) {                                 \
                    r_ = i_ * (cols) + j_;                                     \
                    goto found;                                                \
                }                                                              \
    found:                                                                     \
        r_;                                                                    \
    })

/* A local label shadows the function's label of the same name. */
static int shadow(int x)
{
    {
        __label__ out;
        if (x)
            goto out;
        x += 10;
    out:
        x += 1;
    }
    goto out;
    x += 100;
out:
    return x;
}

/* Two labels in one declaration, and a computed goto to a local label. */
static int two(int x)
{
    __label__ a, b;
    static const int pick[] = {0, 1};
    void *t[] = {&&a, &&b};
    goto *t[pick[x & 1]];
a:
    return 1;
b:
    return 2;
}

/* Nested blocks each declaring the same local label. */
static int nested(void)
{
    int n = 0;
    {
        __label__ l;
        goto l;
        n += 100;
    l:
        n += 1;
        {
            __label__ l;
            goto l;
            n += 100;
        l:
            n += 2;
        }
    }
    return n;
}

int main(void)
{
    int g[2][3] = {{1, 2, 3}, {4, 5, 6}};
    /* Two expansions in one function: two `found` labels. */
    if (FIND(g, 2, 3, 5) != 4) return 1;
    if (FIND(g, 2, 3, 9) != -1) return 2;
    if (shadow(1) != 2 || shadow(0) != 11) return 3;
    if (two(0) != 1 || two(1) != 2) return 4;
    if (nested() != 3) return 5;
    return 0;
}
"#,
    );
}

#[test]
fn local_labels_keep_cleanup_and_vla_jump_rules() {
    compile_and_run_everywhere(
        "local_label_scopes",
        r#"
static int log_[16], n_;
static void done(int *p) { log_[n_++] = *p; }

/* A goto to a local label leaving a cleanup scope runs the cleanup; the
   same-named local label of a later block, inside a cleanup scope, is
   reached without running it. */
static int f(int k)
{
    int r = 0;
    {
        __label__ out;
        {
            __attribute__((cleanup(done))) int a = 1;
            if (k)
                goto out;
            r += 10;
        }
    out:
        r += 1;
    }
    {
        __label__ out;
        __attribute__((cleanup(done))) int b = 2;
        if (k)
            goto out;
        r += 100;
    out:
        r += 1000;
    }
    return r;
}

/* A backward goto to a local label before a VLA releases the array each
   time round, or 100000 KB-sized arrays overflow the stack. */
static int g(int n)
{
    int bad = 0, i = 0;
    {
        __label__ again;
    again:
        {
            char v[n];
            v[0] = (char)i;
            bad += v[0] != (char)i;
            if (++i < 100000)
                goto again;
        }
    }
    return bad + i;
}

int main(void)
{
    if (f(1) != 1001) return 1;
    if (n_ != 2 || log_[0] != 1 || log_[1] != 2) return 2;
    if (f(0) != 1111) return 3;
    if (n_ != 4 || log_[2] != 1 || log_[3] != 2) return 4;
    if (g(1000) != 100000) return 5;
    return 0;
}
"#,
    );
}
