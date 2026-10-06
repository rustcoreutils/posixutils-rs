//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU label differences in static initializers, as assembler symbol
// differences at the entry's width.
//

use super::asm_probe::{asm_for_with, AARCH64_DARWIN, AARCH64_LINUX, X86_64_LINUX};

const PROGRAM: &str = r#"
int g(int);
int f(int op)
{
    static const signed char b[] = { &&l1 - &&l0 };
    static const short s[] = { &&l2 - &&l0 + 3 };
    static const int w[] = { &&l0 - &&l0, &&l1 - &&l0, &&l2 - &&l0 };
    static const long q[] = { &&l2 - &&l1 - 5 };
    goto *(&&l0 + w[op] + b[op] + s[op] + q[op]);
l0: return g(0);
l1: return g(1);
l2: return g(2);
}
/* Never called: above -O0 it goes, and its table must go with it. */
static int unused(int op)
{
    static const int t[] = { &&u1 - &&u0 };
    goto *(&&u0 + t[op]);
u0: return 1;
u1: return 2;
}
"#;

/// The label a `.byte`/`.short`/... line names first and second, with the
/// rest of the line: `.long .Lf_2 - .Lf_1 + 3` is `(".Lf_2", ".Lf_1",
/// "+ 3")`.
fn difference(line: &str) -> (&str, &str, &str) {
    let (head, tail) = line
        .split_once(" - ")
        .unwrap_or_else(|| panic!("not a difference: {line}"));
    let end = head.split_whitespace().nth(1).unwrap_or_default();
    let (start, rest) = tail.split_once(' ').unwrap_or((tail, ""));
    (end, start, rest.trim())
}

/// Each width gets its own directive, the constant rides along as an addend,
/// `&&l0 - &&l0` is the constant 0 as gcc folds it, and the labels are the
/// target's private spelling: `.L` on ELF, `L` on Mach-O.
#[test]
fn codegen_label_difference_directives() {
    for triple in [X86_64_LINUX, AARCH64_LINUX, AARCH64_DARWIN] {
        let prefix = if triple == AARCH64_DARWIN {
            "Lf_"
        } else {
            ".Lf_"
        };
        for opt in ["-O0", "-O2"] {
            let asm = asm_for_with("label_diff", triple, PROGRAM, &[opt]);
            let line = |directive: &str| -> Vec<&str> {
                asm.lines()
                    .map(str::trim)
                    .filter(|l| l.starts_with(directive) && l.contains(prefix))
                    .collect()
            };
            let (bytes, shorts, longs, quads) =
                (line(".byte"), line(".short"), line(".long"), line(".quad"));
            assert_eq!(
                (bytes.len(), shorts.len(), longs.len(), quads.len()),
                (1, 1, 2, 1),
                "{triple} {opt}: one difference per entry, at its width:\n{asm}"
            );
            let (l1, l0, k) = difference(bytes[0]);
            assert!(
                l1.starts_with(prefix) && l0.starts_with(prefix) && l1 != l0,
                "{triple} {opt}: {}",
                bytes[0]
            );
            assert_eq!(k, "", "{triple} {opt}");
            let (l2, base, k) = difference(shorts[0]);
            assert_eq!((base, k), (l0, "+ 3"), "{triple} {opt}");
            assert_eq!(difference(longs[0]), (l1, l0, ""), "{triple} {opt}");
            assert_eq!(difference(longs[1]), (l2, l0, ""), "{triple} {opt}");
            assert_eq!(difference(quads[0]), (l2, l1, "- 5"), "{triple} {opt}");
            assert!(
                asm.lines().any(|l| l.trim() == ".long 0"),
                "{triple} {opt}: &&l0 - &&l0 is 0:\n{asm}"
            );
            // Kept at -O0, as gcc keeps an unreferenced plain static there.
            assert_eq!(
                asm.contains("unused_"),
                opt == "-O0",
                "{triple} {opt}: the table of a removed function stays behind:\n{asm}"
            );
        }
    }
}
