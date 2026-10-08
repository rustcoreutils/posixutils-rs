//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A label in front of a declaration (C23; gcc and clang take it in C17).
//

use crate::common::compile_and_run_everywhere;

/// grep 3.11's `dfa.c` (Debian's stray-backslash patch) puts `char const *`
/// right after a `stray_backslash:` label that a `goto` reaches, and C23
/// code puts a declaration right after `case`. Each runs as `L: ; decl`: the
/// jump lands before the declaration, whose initializer then runs.
#[test]
fn labeled_declarations_run_as_an_empty_statement_then_the_declaration() {
    compile_and_run_everywhere(
        "labeled_declaration",
        r#"
static int classify(int c)
{
    int r = 0;
    switch (c) {
    case 1:
        int a = 10;
        r = a;
        break;
    default:
        if (c > 5)
            goto stray;
        r = 2;
        break;
    stray:
        char const *p;
        p = "xyz";
        r = (p[1] - 'x') + 40;
        break;
    }
    return r;
}

static int count(int n)
{
    int total = 0;
again:
    int step = n--;
    total += step;
    if (n > 0)
        goto again;
    return total;
}

int main(void)
{
    if (classify(1) != 10) return 1;
    if (classify(2) != 2) return 2;
    if (classify(7) != 41) return 3;
    if (count(4) != 10) return 4;
    if (({ l: int x = 3; x; }) != 3) return 5;
    return 0;
}
"#,
    );
}

/// GNU label attributes: binutils' gas/read.c writes `just_record_alignment:
/// ATTRIBUTE_UNUSED_LABEL` (`__attribute__ ((__unused__))`) straight before
/// an `if`. The label still labels that statement.
#[test]
fn label_attributes_before_a_statement() {
    compile_and_run_everywhere(
        "label_attributes",
        r#"
static int f(int n)
{
    int r = 0;
    if (n > 5)
        goto done;
    r = 1;
done: __attribute__ ((__unused__))
    if (n > 2)
        r += 10;
unused: __attribute__((unused)) __attribute__((cold))
    r += 100;
    return r;
}

int main(void)
{
    if (f(9) != 110) return 1;
    if (f(3) != 111) return 2;
    if (f(1) != 101) return 3;
    return 0;
}
"#,
    );
}
