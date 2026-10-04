//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Valid C for which the linearizer holds no current basic block.
//
// `Linearizer::current_bb` is legitimately `None` in two situations the
// language allows: after a `goto`, and inside a `switch` body before the first
// `case` label. Code in either place is unreachable but well-formed, and the
// standard requires it to be translated, not rejected -- and certainly not to
// crash the compiler.
//
// Every lowering that ends a block has to read `current_bb` back afterwards.
// The ones that reached for `.unwrap()` instead turned each of these programs
// into an internal compiler error. `current_or_unreachable_bb()` is the
// accessor that starts a fresh unreachable block rather than panicking.
//
// These are compile-only where the construct is genuinely unreachable, because
// there is no answer to assert; the two that are reachable check the answer as
// well.
//

use crate::common::compile_and_run;

/// Unreachable code before the first `case` is discarded, and the reachable
/// part of the same function still gives the right answer.
#[test]
fn misc_an_unreachable_statement_does_not_change_the_answer() {
    let code = r#"
int calls;
int g(void) { calls++; return 3; }

int f(int x)
{
    switch (x) {
        g() ? g() : g();          /* unreachable: never selected */
        (void)(g() && g());
    case 1:
        return 1;
    default:
        return 2;
    }
}

int main(void)
{
    if (f(1) != 1) return 1;
    if (f(0) != 2) return 2;
    /* Nothing before the first case may run. */
    if (calls != 0) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("nocur_answer", code, &[]), 0);
}
