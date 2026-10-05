//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compile-only case of tests/preprocessor/std_dialect.rs, in process.
//

use crate::test_compile::compile_expect_ok;

/// The `-std=` spellings that name C17 itself. Exhaustive.
const C17_SPELLINGS: &[&str] = &[
    "c17",
    "c18",
    "gnu17",
    "gnu18",
    "iso9899:2017",
    "iso9899:2018",
];

/// The `-std=` spellings naming an older revision. Exhaustive.
///
/// Together with `C17_SPELLINGS` this is every spelling `classify_std`
/// accepts, so "every accepted spelling behaves identically" is a claim these
/// tests actually check rather than sample.
const OLDER_SPELLINGS: &[&str] = &[
    "c89",
    "c90",
    "c9x",
    "c99",
    "c1x",
    "c11",
    "gnu89",
    "gnu90",
    "gnu9x",
    "gnu99",
    "gnu1x",
    "gnu11",
    "iso9899:1990",
    "iso9899:199409",
    "iso9899:199x",
    "iso9899:1999",
    "iso9899:2011",
];

/// Every `-std=` spelling c17 accepts.
fn accepted() -> Vec<&'static str> {
    C17_SPELLINGS
        .iter()
        .chain(OLDER_SPELLINGS)
        .copied()
        .collect()
}

/// The negative test (`c17_rejects_an_unknown_std`) cannot pass vacuously:
/// every accepted spelling must work. This is the compile half of
/// `c17_accepts_every_documented_std_spelling`; its driver half, which passes
/// each `-std=` to `c17`, stays in tests/preprocessor/std_dialect.rs.
#[test]
fn c17_accepts_every_documented_std_spelling() {
    for spec in accepted() {
        compile_expect_ok(
            &format!("std_ok_{}", spec.replace(':', "_")),
            "int main(void) { return 0; }\n",
        );
    }
}
