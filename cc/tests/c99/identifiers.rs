//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Identifiers containing characters outside the basic source character set.
//

use crate::common::{compile_and_run, run_c17};

/// C17 6.4.2.1 spells an extended character in an identifier as a universal
/// character name, and Annex D says which characters those may be. Writing the
/// character directly is what C23 settled on and what gcc and clang have long
/// accepted -- in every mode, `-std=c17 -pedantic-errors` included.
///
/// The two spellings name the same identifier, so a declaration written one
/// way and a use written the other have to agree.
#[test]
fn c99_extended_identifiers_raw_and_ucn_agree() {
    let code = r#"
/* Declared raw, used through the UCN spelling and back. */
static int café = 7;
static int été = 11;

static int Ω(int x) { return x * 2; }
static int 中文(int x) { return x + 1; }

struct Σ { int α; int β; };

int main(void) {
    if (été != 11) return 1;
    if (été != 11) return 2;
    if (café != 7) return 3;
    if (café != 7) return 4;

    /* An extended character anywhere but first. */
    int xé = 1, xéy = 2;
    if (xé + xéy != 3) return 5;

    /* Outside the BMP: Annex D admits 10000-EFFFD. */
    int 𝒜 = 4;
    if (𝒜 != 4) return 6;

    /* A combining mark may continue an identifier but not start one. */
    int á = 5;
    if (á != 5) return 7;

    struct Σ s = { .α = 8, .β = 9 };
    if (s.α + s.β != 17) return 8;

    if (Ω(3) != 6) return 9;
    if (中文(3) != 4) return 10;

    /* Distinct identifiers stay distinct. */
    int e = 20;
    if (e == été) return 11;

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_extended_identifiers", code, &[]), 0);
}

/// Phase 2 runs before phase 3, so a line splice inside a multi-byte
/// character is deleted and the two halves join to form it.
///
/// The source has to be written as bytes: a splice between the two bytes of
/// `é` is not valid UTF-8, so it cannot be spelled in a Rust `&str` at all.
#[test]
fn c99_extended_identifier_across_a_line_splice() {
    let dir = tempfile::Builder::new()
        .prefix("c17_ident_splice_")
        .tempdir()
        .expect("failed to create work dir");
    let src = dir.path().join("splice.c");

    // `café` twice: once with the splice between the two bytes of `é`, once
    // with it between `caf` and the `é`.
    let mut bytes: Vec<u8> = Vec::new();
    bytes.extend_from_slice(
        b"static int caf\xc3\
\xa9 = 3;
",
    );
    bytes.extend_from_slice(
        b"static int ab\
\xc3\xa9 = 4;
",
    );
    bytes.extend_from_slice(
        b"int main(void) { return (caf\xc3\xa9 == 3 && ab\xc3\xa9 == 4) ? 0 : 1; }
",
    );
    std::fs::write(&src, &bytes).unwrap();

    let exe = dir.path().join("prog");
    let r = run_c17(&[&src.to_string_lossy(), "-o", &exe.to_string_lossy()]);
    assert!(
        r.success,
        "compiling a spliced identifier failed:\n{}",
        r.stderr
    );
    let status = std::process::Command::new(&exe).status().unwrap();
    assert_eq!(
        status.code(),
        Some(0),
        "the spliced identifiers did not join"
    );
}
