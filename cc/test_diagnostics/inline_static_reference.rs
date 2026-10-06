//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 6.7.4p3: an inline definition of a function with external linkage
// shall not contain a reference to an identifier with internal linkage. gcc
// only warns, and so does c17: an error only under `-pedantic-errors`.
//
// The constraint is about the identifier a name *resolves to*, not its
// spelling: a parameter or a block-scope object that shadows a file-scope
// static is an object of the function's own, and referring to it is fine.
//

use crate::test_compile::{compile_expect_no_diagnostic, compile_expect_warning};

const MESSAGE: &str = "'counter' is static but used in inline function 'next' which is not static";

/// Every read-modify-write form, applied to a name that shadows the static.
const SHADOWED: &[(&str, &str)] = &[
    (
        "param_postfix",
        "inline int next(int counter) { return counter++; }\n",
    ),
    (
        "param_prefix",
        "inline int next(int counter) { return ++counter; }\n",
    ),
    (
        "param_postfix_decrement",
        "inline int next(int counter) { return counter--; }\n",
    ),
    (
        "param_compound",
        "inline int next(int counter) { counter += 2; return counter; }\n",
    ),
    (
        "param_asm_read_write",
        "inline int next(int counter) { __asm__(\"\" : \"+r\"(counter)); return counter; }\n",
    ),
    (
        "local_postfix",
        "inline int next(void) { int counter = 1; counter++; return counter; }\n",
    ),
    (
        "local_compound",
        "inline int next(void) { int counter = 1; counter *= 3; return counter; }\n",
    ),
    (
        "nested_local_prefix",
        "inline int next(int n) { { int counter = n; return ++counter; } }\n",
    ),
    (
        "nested_local_asm_read_write",
        "inline int next(int n) { { int counter = n; __asm__(\"\" : \"+r\"(counter)); return counter; } }\n",
    ),
];

/// A parameter or local named like a file-scope static is not that static.
#[test]
fn inline_definition_may_update_a_name_that_shadows_a_static() {
    for (name, body) in SHADOWED {
        let src = format!("static int counter;\n{body}int main(void) {{ return counter; }}\n");
        compile_expect_no_diagnostic(&format!("inline_shadow_{name}"), &src, MESSAGE);
    }
}

/// The static itself, under each read-modify-write form, is still reported.
#[test]
fn inline_definition_may_not_update_a_file_scope_static() {
    const BODIES: &[(&str, &str)] = &[
        ("postfix", "inline int next(void) { return counter++; }\n"),
        ("prefix", "inline int next(void) { return ++counter; }\n"),
        (
            "compound",
            "inline int next(void) { counter += 2; return 0; }\n",
        ),
        (
            "asm_read_write",
            "inline int next(void) { __asm__(\"\" : \"+r\"(counter)); return 0; }\n",
        ),
        (
            "after_shadow_ends",
            "inline int next(void) { { int counter = 0; counter++; } return counter++; }\n",
        ),
    ];
    for (name, body) in BODIES {
        let src = format!("static int counter;\n{body}int main(void) {{ return counter; }}\n");
        compile_expect_warning(&format!("inline_static_{name}"), &src, MESSAGE);
    }
}

/// gcc gives these as default warnings (pedwarns): an error only under
/// `-pedantic-errors`. A static used in, or a modifiable static declared in,
/// an external inline definition draws gcc's wording, naming both.
#[test]
fn inline_static_reference_is_gccs_default_warning() {
    let src = "static int counter;\n\
               inline int next(void) { return counter++; }\n\
               inline int nx2(void) { static int local; return local; }\n\
               int main(void) { return counter; }\n";
    let warned = crate::test_compile::compile_accepted("inline_static_warns", src, &[]);
    for want in [
        "'counter' is static but used in inline function 'next' which is not static",
        "'local' is static but declared in inline function 'nx2' which is not static",
    ] {
        assert!(warned.contains(want), "missing {want:?}:\n{warned}");
    }
    let c = crate::test_compile::compile("inline_static_errors", src, &["-pedantic-errors"]);
    assert!(!c.success, "-pedantic-errors must reject it:\n{}", c.stderr);
}
