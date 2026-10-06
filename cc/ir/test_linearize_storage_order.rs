//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linearizer tests for gcc's `scalar_storage_order`: an access to a scalar
// stored in reverse order is an access in the target's order and an explicit
// byte swap, and a static image holds the reversed bytes.
//

use super::test_linearize::linearize_source_with_types;
use super::*;
use crate::target::{Arch, Os, Target};
use crate::types::TypeTable;

const BE: &str = "struct __attribute__((scalar_storage_order(\"big-endian\"))) B {\n\
                  int i; unsigned char c; float f; unsigned short bf : 12; long w; };\n\
                  struct L { int i; };\n";

fn linearize(body: &str) -> (Module, TypeTable) {
    let target = Target::new(Arch::X86_64, Os::Linux);
    linearize_source_with_types(&format!("{BE}{body}"), &target)
}

/// The opcodes of function `name`, in order, without the bookkeeping ones.
fn ops(module: &Module, name: &str) -> Vec<Opcode> {
    module
        .functions
        .iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("function {name}"))
        .blocks
        .iter()
        .flat_map(|bb| bb.insns.iter())
        .map(|i| i.op)
        .filter(|op| {
            !matches!(
                op,
                Opcode::Entry | Opcode::Ret | Opcode::SymAddr | Opcode::Br | Opcode::Nop
            )
        })
        .collect()
}

/// No load or store anywhere in the module is made at a reversed type: every
/// one was rewritten where it was emitted, so no later pass can meet one.
fn assert_no_reversed_access(module: &Module, types: &TypeTable) {
    for func in &module.functions {
        for insn in func.blocks.iter().flat_map(|bb| bb.insns.iter()) {
            if let (Opcode::Load | Opcode::Store, Some(t)) = (insn.op, insn.typ) {
                assert!(
                    !types.reverses_storage(t),
                    "{}: {insn:?} accesses at a reversed type",
                    func.name
                );
            }
        }
    }
}

/// An integer member is read as a load and a swap, and written as a swap
/// and a store, at its own width.
#[test]
fn test_a_reversed_integer_is_swapped_in_a_register() {
    let (module, types) = linearize(
        "int get(struct B *p) { return p->i; }\n\
         void set(struct B *p, int v) { p->i = v; }\n\
         long getw(struct B *p) { return p->w; }\n",
    );
    let get = ops(&module, "get");
    let load = get.iter().position(|op| *op == Opcode::Load).unwrap();
    assert_eq!(get[load + 1], Opcode::Bswap32, "{get:?}");
    let set = ops(&module, "set");
    let swap = set.iter().position(|op| *op == Opcode::Bswap32).unwrap();
    assert!(set[swap..].contains(&Opcode::Store), "{set:?}");
    assert!(ops(&module, "getw").contains(&Opcode::Bswap64));
    assert_no_reversed_access(&module, &types);
}

/// A one-byte member, and a member of a struct in the target's order, have
/// nothing to swap.
#[test]
fn test_nothing_is_swapped_without_a_reversed_multibyte_scalar() {
    let (module, types) = linearize(
        "int byte(struct B *p) { return p->c; }\n\
         int native(struct L *p) { return p->i; }\n",
    );
    for name in ["byte", "native"] {
        let got = ops(&module, name);
        assert!(
            !got.iter()
                .any(|op| matches!(op, Opcode::Bswap16 | Opcode::Bswap32 | Opcode::Bswap64)),
            "{name}: {got:?}"
        );
    }
    assert_no_reversed_access(&module, &types);
}

/// A floating member has no swap of its own: its bytes are swapped as an
/// integer and moved through a frame temporary.
#[test]
fn test_a_reversed_float_moves_through_an_integer() {
    let (module, types) = linearize("float get(struct B *p) { return p->f; }\n");
    let got = ops(&module, "get");
    let swap = got.iter().position(|op| *op == Opcode::Bswap32).unwrap();
    assert_eq!(
        &got[swap + 1..swap + 3],
        &[Opcode::Store, Opcode::Load],
        "{got:?}"
    );
    assert_no_reversed_access(&module, &types);
}

/// A bit-field's carrier is swapped before the field is extracted, and
/// swapped back after it is inserted.
#[test]
fn test_a_reversed_bitfield_swaps_its_carrier() {
    let (module, types) = linearize(
        "int get(struct B *p) { return p->bf; }\n\
         void set(struct B *p) { p->bf = 5; }\n",
    );
    assert!(ops(&module, "get").contains(&Opcode::Bswap16));
    let set = ops(&module, "set");
    assert_eq!(
        set.iter().filter(|op| **op == Opcode::Bswap16).count(),
        2,
        "{set:?}"
    );
    assert_no_reversed_access(&module, &types);
}

/// A static image holds each reversed scalar's bytes reversed, and a
/// bit-field counted from the most significant bit.
#[test]
fn test_a_static_image_is_in_the_struct_order() {
    let (module, _) = linearize("struct B g = { 0x01020304, 7, 1.0f, 0x123 };\n");
    let global = module
        .globals
        .iter()
        .find(|g| g.name == "g")
        .expect("g emitted");
    let Initializer::Struct { fields, .. } = &global.init else {
        panic!("{:?}", global.init);
    };
    let at = |offset: usize| {
        fields
            .iter()
            .find(|(o, _, _)| *o == offset)
            .map(|(_, _, init)| init.clone())
    };
    assert_eq!(at(0), Some(Initializer::Int(0x04030201)));
    assert_eq!(at(4), Some(Initializer::Int(7)));
    // 1.0f is 0x3f800000.
    assert_eq!(at(8), Some(Initializer::Int(0x0000803f)));
    // 0x123 in the top twelve bits of a big-endian 16-bit unit: 12 30.
    assert_eq!(at(12), Some(Initializer::Int(0x12)));
    assert_eq!(at(13), Some(Initializer::Int(0x30)));
}
