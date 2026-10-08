//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's `optimize` function attribute and `#pragma GCC optimize`
//

use crate::common::compile_and_run;

/// `optimize(...)` only changes how gcc optimizes one function, so ignoring
/// it cannot change what a correct program computes: c17 accepts it in both
/// spellings, with any argument gcc takes, without a word -- under `-Werror`
/// too. libzstd's `DONT_VECTORIZE` puts `optimize("no-tree-vectorize")` on
/// its decoder loops and builds with `-Werror`, which "'optimize' attribute
/// directive ignored" failed. `#pragma GCC optimize`, its pragma form
/// (xxhash.h), is ignored as every pragma c17 does not know.
#[test]
fn misc_optimize_attribute_is_ignored_silently() {
    let code = r#"
#pragma GCC push_options
#pragma GCC optimize("-O2")
#pragma GCC optimize("no-tree-vectorize", "O3")
__attribute__((optimize("no-tree-vectorize"))) static int f(int x) { return x + 1; }
__attribute__((__optimize__(2))) int g(int x);
__attribute__((optimize("O3", "unroll-loops"), noinline)) int g(int x) { return f(x) * 2; }
_Pragma("GCC optimize(\"no-tree-vectorize\")")
#pragma GCC pop_options
int main(void) {
#if !__has_attribute(optimize) || !__has_attribute(__optimize__)
    return 99;
#endif
    return g(20) == 42 ? 0 : 1;
}
"#;
    let flags = ["-Wall".to_string(), "-Werror".to_string()];
    assert_eq!(compile_and_run("optimize_attr", code, &flags), 0);
}
