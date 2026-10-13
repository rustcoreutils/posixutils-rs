//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Named asm operands in a constraint: `"[sum]"` is the matching constraint
// for the operand named `sum`, rewritten to its number as the statement is
// parsed, as gcc does.
//

use super::asm::{resolve_operand_names, template_name_errors, OperandNameError};
use super::ast::{BlockItem, ExternalDecl, Stmt};
use super::test_parser::parse_tu;

/// The names of an asm statement's operands -- outputs, inputs, labels --
/// in gcc's numbering.
const NAMES: [Option<&str>; 4] = [Some("sum"), None, Some("b"), Some("out")];

#[test]
fn a_bracketed_name_becomes_its_operand_number() {
    let r = |c| resolve_operand_names(c, &NAMES);
    assert_eq!(r("[sum]").unwrap(), "0");
    assert_eq!(r("[b]").unwrap(), "2");
    assert_eq!(r("[out]").unwrap(), "3");
    // Inside a longer constraint, and more than once.
    assert_eq!(r("r[sum]").unwrap(), "r0");
    assert_eq!(r("[b],[sum]").unwrap(), "2,0");
    // A constraint with no name is left as it is.
    assert_eq!(r("=r").unwrap(), "=r");
}

#[test]
fn an_unknown_or_unterminated_name_is_an_error() {
    let r = |c| resolve_operand_names(c, &NAMES);
    assert_eq!(
        r("[nope]").unwrap_err(),
        OperandNameError::Undefined("nope".to_string())
    );
    assert_eq!(
        r("[]").unwrap_err(),
        OperandNameError::Undefined(String::new())
    );
    assert_eq!(r("[sum").unwrap_err(), OperandNameError::MissingCloseBrace);
    assert_eq!(
        OperandNameError::Undefined("nope".to_string()).to_string(),
        "undefined named operand 'nope'"
    );
    assert_eq!(
        OperandNameError::MissingCloseBrace.to_string(),
        "missing close brace for named operand"
    );
}

/// The parser stores the rewritten constraint: everything after it sees a
/// plain matching constraint.
#[test]
fn the_parsed_statement_carries_the_operand_number() {
    let (tu, ..) = parse_tu(
        "long f(long a, long b) {\n\
         long sum;\n\
         __asm__(\"add %[b], %[sum]\" : [sum] \"=r\"(sum) : \"[sum]\"(a), [b] \"r\"(b));\n\
         return sum;\n\
         }\n",
    )
    .expect("parse");
    let body = tu
        .items
        .iter()
        .find_map(|item| match item {
            ExternalDecl::FunctionDef(f) => Some(&f.body),
            _ => None,
        })
        .expect("a function");
    let Stmt::Block(items) = body else {
        panic!("a block body: {body:?}");
    };
    let inputs = items
        .iter()
        .find_map(|item| match item {
            BlockItem::Statement(stmt) => match &**stmt {
                Stmt::Asm { inputs, .. } => Some(inputs),
                _ => None,
            },
            _ => None,
        })
        .expect("an asm statement");
    let constraints: Vec<&str> = inputs.iter().map(|op| op.constraint.as_str()).collect();
    assert_eq!(constraints, ["0", "r"]);
}

/// The template's `%[name]` and `%l[name]`, as gcc scans them: each one
/// naming nothing is reported, in order; `%%[` is no reference, and a name
/// with no `]` ends the scan.
#[test]
fn template_names_are_checked_as_gcc_scans_them() {
    let e = |t| template_name_errors(t, &NAMES);
    assert!(e("add %[b], %[sum]; jmp %l[out]").is_empty());
    assert!(e("%%[nope] %x").is_empty());
    assert_eq!(
        e("%[nope] %l[bar] %w[b]"),
        [
            OperandNameError::Undefined("nope".to_string()),
            OperandNameError::Undefined("bar".to_string()),
        ]
    );
    assert_eq!(e("%[sum] %[b"), [OperandNameError::MissingCloseBrace]);
}

/// An unused `static inline` function's bad names are reported as it is
/// parsed, as gcc reports them, though no code is written for it.
#[test]
fn template_names_are_checked_where_no_code_is_written() {
    let src = "static inline void h(int x) {\n\
               __asm__(\"# %[foo] %l[bar]\" : : \"r\"(x));\n\
               }\n";
    let before = crate::diag::error_count();
    let _ = parse_tu(src);
    assert_eq!(crate::diag::error_count(), before + 2);
}
