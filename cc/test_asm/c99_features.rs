//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 features: the compile-only halves of `tests/c99/features.rs` cases,
// whose run-time halves run there as sections of a mega program.
//

use crate::test_compile::compile_expect_error;

/// A flexible array member may be initialized at the top level of a static
/// object, as a GNU extension gcc accepts, but not inside an element of an
/// array of such structures: each element would be a different size, which
/// no array can hold. gcc rejects that with "initialization of flexible array
/// member in a nested context", once per element; c17 accepted it and laid
/// the elements out as if the member were empty.
///
/// The accepted top-level form runs as the `fam_top_level_init` section of
/// `c99_features_everywhere_mega` in `tests/c99/features.rs`.
#[test]
fn c99_flexible_array_member_initializer_in_an_array_is_rejected() {
    compile_expect_error(
        "fam_nested_init",
        "struct V { int n; const char s[]; };\n\
         static const struct V arr[] = { { 1, \"x\" }, { 2, \"yy\" } };\n",
        "initialization of flexible array member in a nested context",
    );
    compile_expect_error(
        "fam_nested_init_braced",
        "struct E { int a; };\n\
         struct W { int n; struct E e[]; };\n\
         static struct W arr[] = { { 1, 4 }, { 2, 5 } };\n",
        "initialization of flexible array member in a nested context",
    );
}
