//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser tests for gcc's `scalar_storage_order` attribute: which members a
// struct's order reaches, and which expressions carry it.
//

use super::test_parser::{parse_tu_for, with_statement_expr_for};
use crate::strings::StringTable;
use crate::symbol::{Namespace, SymbolTable};
use crate::target::{Arch, Os, Target};
use crate::types::TypeTable;

fn x86_64_linux() -> Target {
    Target::new(Arch::X86_64, Os::Linux)
}

/// Whether each member of `struct <tag>` in `src` is stored in reverse
/// order, by name.
fn reversed_members(src: &str, tag: &str) -> Vec<(String, bool)> {
    let (_, types, strings, symbols) = parse_tu_for(src, &x86_64_linux()).unwrap();
    members_of(&types, &strings, &symbols, tag)
}

fn members_of(
    types: &TypeTable,
    strings: &StringTable,
    symbols: &SymbolTable,
    tag: &str,
) -> Vec<(String, bool)> {
    let tag = strings.lookup(tag).expect("tag interned");
    let typ = symbols
        .lookup(tag, Namespace::Tag)
        .expect("tag declared")
        .typ;
    let composite = types.get(typ).composite.as_ref().expect("a composite");
    composite
        .members
        .iter()
        .map(|m| {
            let at = types.innermost_element(m.typ);
            (strings.get(m.name).to_string(), types.reverses_storage(at))
        })
        .collect()
}

/// gcc's rule: integers, enums, floating and complex members, bit-fields
/// and the elements of arrays of them take a big-endian struct's order on a
/// little-endian target; pointers, vectors and nested structs do not.
#[test]
fn test_a_big_endian_struct_reverses_its_scalars() {
    let src = "enum E { A };\n\
               struct __attribute__((scalar_storage_order(\"big-endian\"))) B {\n\
               char c; int i; unsigned bf : 3; double d; _Complex float z;\n\
               enum E e; long arr[2][2]; void *p; struct { int n; } nested;\n\
               int v __attribute__((vector_size(16)));\n\
               };";
    let want = [
        ("c", true),
        ("i", true),
        ("bf", true),
        ("d", true),
        ("z", true),
        ("e", true),
        ("arr", true),
        ("p", false),
        ("nested", false),
        ("v", false),
    ];
    let got = reversed_members(src, "B");
    let want: Vec<(String, bool)> = want.iter().map(|(n, r)| (n.to_string(), *r)).collect();
    assert_eq!(got, want);
}

/// The target's own order changes nothing, and the attribute is read in all
/// three places gcc reads it: before the tag, between the tag and the
/// brace, and after the closing brace -- on a union as on a struct.
#[test]
fn test_the_attribute_positions_and_the_native_order() {
    let src = "struct __attribute__((scalar_storage_order(\"little-endian\"))) L { int i; };\n\
               struct M __attribute__((scalar_storage_order(\"big-endian\"))) { int i; };\n\
               struct T { int i; } __attribute__((scalar_storage_order(\"big-endian\")));\n\
               union __attribute__((scalar_storage_order(\"big-endian\"))) U { short h; };";
    let (_, types, strings, symbols) = parse_tu_for(src, &x86_64_linux()).unwrap();
    let one = |tag: &str| members_of(&types, &strings, &symbols, tag)[0].1;
    assert!(!one("L"));
    assert!(one("M"));
    assert!(one("T"));
    assert!(one("U"));
}

/// A member access carries the order -- through `.` and `->`, a subscript
/// and `__real__` -- and the value it yields does not.
#[test]
fn test_an_access_carries_the_order_and_its_value_does_not() {
    let decls = "struct __attribute__((scalar_storage_order(\"big-endian\"))) B {\n\
                 int i; int arr[2]; _Complex double z; } g, *p;";
    let target = x86_64_linux();
    for (stmt, reversed) in [
        ("g.i", true),
        ("p->i", true),
        ("g.arr[1]", true),
        ("*(g.arr + 1)", true),
        ("__real__ g.z", true),
        ("g.i + 1", false),
        ("-g.i", false),
        ("g.i = 1", false),
        ("g.i++", false),
        ("(int)g.i", false),
    ] {
        let got = with_statement_expr_for(&target, decls, stmt, |parser, expr| {
            parser.types.reverses_storage(expr.typ.expect("typed"))
        });
        assert_eq!(got, reversed, "{stmt}");
    }
}

/// `typeof` of a member declares an ordinary object: the order is the
/// struct's, not the member type's.
#[test]
fn test_typeof_a_member_is_native() {
    let src = "struct __attribute__((scalar_storage_order(\"big-endian\"))) B { int i; short a[2]; } g;\n\
               struct H { __typeof__(g.i) x; __typeof__(g.a) y; };";
    let got = reversed_members(src, "H");
    assert_eq!(got, [("x".to_string(), false), ("y".to_string(), false)]);
}

/// A reference to an existing struct with the attribute names gcc's variant
/// of it in that order: the same members, reversed, and a type of its own.
/// The target's own order is the struct itself.
#[test]
fn test_a_typedef_names_a_variant_in_the_written_order() {
    let src = "struct S { int i; char *p; };\n\
               typedef struct S __attribute__((scalar_storage_order(\"big-endian\"))) BE;\n\
               typedef struct S __attribute__((scalar_storage_order(\"little-endian\"))) LE;";
    let (_, types, strings, symbols) = parse_tu_for(src, &x86_64_linux()).unwrap();
    let tag = symbols
        .lookup(strings.lookup("S").unwrap(), Namespace::Tag)
        .unwrap()
        .typ;
    let typedef = |name: &str| {
        symbols
            .lookup_typedef(strings.lookup(name).unwrap())
            .expect("typedef declared")
    };
    let (be, le) = (typedef("BE"), typedef("LE"));
    let members = |t| {
        let c = types.get(t).composite.as_ref().expect("a composite");
        c.members
            .iter()
            .map(|m| types.reverses_storage(m.typ))
            .collect::<Vec<_>>()
    };
    assert_eq!(members(be), [true, false]);
    assert_eq!(members(le), [false, false]);
    assert!(!types.types_compatible(tag, be));
    assert!(types.types_compatible(tag, le));
    assert_eq!(types.size_bytes(be), types.size_bytes(tag));
}

/// The aggregate records its own order, which a struct of pointers alone --
/// none of whose members the order reaches -- has no other trace of. A
/// typedef variant in the other order is a type with the other answer.
#[test]
fn test_a_reversed_aggregate_records_its_order() {
    let src = "struct __attribute__((scalar_storage_order(\"big-endian\"))) B { int *p; };\n\
               union __attribute__((scalar_storage_order(\"big-endian\"))) U { int *p; };\n\
               struct __attribute__((scalar_storage_order(\"little-endian\"))) L { int *p; };\n\
               struct N { int *p; };\n\
               typedef struct N __attribute__((scalar_storage_order(\"big-endian\"))) NB;\n\
               NB nb;\n";
    let (_, types, strings, symbols) = parse_tu_for(src, &x86_64_linux()).unwrap();
    let reversed = |typ| {
        types
            .get(typ)
            .composite
            .as_ref()
            .expect("a composite")
            .reverse_order
    };
    let tag = |name: &str| {
        let id = strings.lookup(name).expect("tag interned");
        symbols
            .lookup(id, Namespace::Tag)
            .expect("tag declared")
            .typ
    };
    assert!(reversed(tag("B")));
    assert!(reversed(tag("U")));
    assert!(!reversed(tag("L")));
    assert!(!reversed(tag("N")));
    let nb = strings.lookup("nb").expect("interned");
    let nb = symbols
        .lookup(nb, Namespace::Ordinary)
        .expect("declared")
        .typ;
    assert!(reversed(nb));
}
