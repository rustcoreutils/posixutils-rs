//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__attribute__((target_clones(...)))`: one function body compiled once per
// ISA, and the function itself a GNU indirect function bound at load time to
// the best version the CPU runs. As gcc 13 builds it:
//
//   sum.default, sum.sse4_2   the versions, local symbols
//   sum.resolver              tests the CPU, best version first, and returns
//                             one -- weak in a comdat group when `sum` has
//                             external linkage, local otherwise
//   sum                       `@gnu_indirect_function`, set to the resolver
//
// The resolver reads the runtime library's CPU model as
// `__builtin_cpu_supports` does (`parse::cpu_feature_location`).
//

use super::linearize::{FnVersion, Linearizer};
use super::{BasicBlock, BasicBlockId, Function, Instruction, Opcode, Pseudo, PseudoId};
use crate::abi::CallingConv;
use crate::parse::ast::{AliasAttr, AliasForm, FunctionAttrs, FunctionDef, SymbolAttrs};
use crate::target_attr::TargetClones;
use crate::types::{TypeId, TypeTable};
use std::collections::HashMap;

/// A version the resolver can return: its symbol, and where the runtime
/// keeps the feature bit that selects it.
struct Choice {
    symbol: String,
    word: (&'static str, i64),
    mask: u32,
}

/// The share of a `target_clones` function's emission attributes one version
/// carries. See [`Division`].
#[derive(Clone, Default)]
pub(crate) struct VersionAttrs {
    pub(crate) symbol: SymbolAttrs,
    pub(crate) constructor: Option<Option<u16>>,
    pub(crate) destructor: Option<Option<u16>>,
}

/// How a `target_clones` function's attributes divide among the symbols that
/// stand for it -- the one place that decides it, as gcc 13 does:
///
/// | attribute                  | goes to                                  |
/// |----------------------------|------------------------------------------|
/// | `constructor`/`destructor` | `name.default`, with any priority: gcc   |
/// |                            | registers the default version, not `name`|
/// | `section`                  | every version but `name.default`         |
/// | `used`                     | every version and the resolver           |
/// | `visibility`               | nothing: `name` is exported, as in gcc   |
/// | `weak`                     | nothing: `name` stays global, as in gcc  |
/// | `alias`/`ifunc`            | nothing: `name` is the resolver's ifunc  |
///
/// What describes the body rather than a symbol -- `aligned`, `noinline`,
/// `always_inline`, `pure`/`const`, `noreturn` -- every compilation of the
/// body reads off the definition itself, so every version has it.
///
/// `section` and `visibility` follow gcc 13 as measured, odd as both look:
/// the default version stays in `.text`, and a `hidden` function's indirect
/// symbol is exported anyway. Matching it keeps a library's exported symbols
/// the same as a gcc build's.
struct Division {
    default: VersionAttrs,
    others: VersionAttrs,
    resolver_used: bool,
    name: SymbolAttrs,
}

impl Division {
    fn of(attrs: &FunctionAttrs) -> Division {
        let version = SymbolAttrs {
            section: attrs.symbol.section.clone(),
            used: attrs.symbol.used,
            ..Default::default()
        };
        Division {
            default: VersionAttrs {
                symbol: SymbolAttrs {
                    section: None,
                    ..version.clone()
                },
                constructor: attrs.constructor,
                destructor: attrs.destructor,
            },
            others: VersionAttrs {
                symbol: version,
                ..Default::default()
            },
            resolver_used: attrs.symbol.used,
            name: SymbolAttrs::default(),
        }
    }
}

impl Linearizer<'_> {
    /// Linearize a `target_clones` function: each version of the body, the
    /// resolver, and the indirect function that names them.
    pub(crate) fn linearize_target_clones(&mut self, func: &FunctionDef, clones: &TargetClones) {
        // Every version lowers the same source, so a diagnostic about it is
        // given once, not once per version.
        crate::diag::each_once(|| self.linearize_clones(func, clones));
    }

    fn linearize_clones(&mut self, func: &FunctionDef, clones: &TargetClones) {
        let name = self.emitted_name(func.name);
        let division = Division::of(&func.attrs);
        let unit = self.function_isa(None);
        self.clone_statics = Some(HashMap::new());
        let default = format!("{name}.default");
        self.linearize_function_as(
            func,
            Some(FnVersion {
                name: default.clone(),
                isa: unit,
                attrs: division.default,
            }),
        );
        let mut choices = Vec::with_capacity(clones.versions.len());
        for version in &clones.versions {
            let symbol = format!("{name}.{}", version.suffix);
            self.linearize_function_as(
                func,
                Some(FnVersion {
                    name: symbol.clone(),
                    isa: self.function_isa(Some(&version.request)),
                    attrs: division.others.clone(),
                }),
            );
            let (object, offset, mask) = crate::parse::cpu_feature_location(version.feature)
                .expect("every clone feature is one __builtin_cpu_supports knows");
            choices.push(Choice {
                symbol,
                word: (object, offset),
                mask,
            });
        }
        self.clone_statics = None;

        let resolver_name = format!("{name}.resolver");
        let mut resolver =
            build_resolver(&resolver_name, &default, &choices, self.types, self.target);
        resolver.is_static = func.is_static;
        resolver.symbol_attrs.used = division.resolver_used;
        if !func.is_static {
            // gcc's: one resolver per program, however many units define the
            // function, in a group of its own.
            resolver.symbol_attrs.weak = true;
            resolver.symbol_attrs.section = Some(format!(
                ".text.{resolver_name},\"axG\",@progbits,{resolver_name},comdat"
            ));
        }
        // The runtime's: reached through the PLT and the GOT.
        self.module
            .extern_symbols
            .insert("__cpu_indicator_init".to_string());
        for choice in &choices {
            self.module.extern_symbols.insert(choice.word.0.to_string());
        }
        self.module.add_function(resolver);

        let typ = self
            .symbols
            .lookup(func.name, crate::symbol::Namespace::Ordinary)
            .map_or(func.return_type, |s| s.typ);
        let attrs = SymbolAttrs {
            alias: Some(AliasAttr {
                target: resolver_name,
                form: AliasForm::Ifunc,
            }),
            ..division.name
        };
        self.declare_alias(&name, &attrs, func.is_static, typ, func.pos);
    }
}

/// A fresh register pseudo of `func`.
fn reg(func: &mut Function) -> PseudoId {
    let id = PseudoId(func.next_pseudo);
    func.next_pseudo += 1;
    func.add_pseudo(Pseudo::reg(id, id.0));
    id
}

/// A constant pseudo of `func`.
fn value(func: &mut Function, v: i128) -> PseudoId {
    let id = PseudoId(func.next_pseudo);
    func.next_pseudo += 1;
    func.add_pseudo(Pseudo::val(id, v));
    id
}

/// The address of `symbol`, computed at the end of `block`.
fn address_of(func: &mut Function, block: &mut BasicBlock, symbol: &str, typ: TypeId) -> PseudoId {
    let sym = PseudoId(func.next_pseudo);
    func.next_pseudo += 1;
    func.add_pseudo(Pseudo::sym(sym, symbol.to_string()));
    let addr = reg(func);
    block.add_insn(Instruction::sym_addr(addr, sym, typ));
    addr
}

/// The resolver `name`: initialize the runtime's CPU model, then return the
/// first of `choices` whose feature the CPU has, or `default`. Built as a
/// chain of selects from the worst choice up, so the best one decides last.
fn build_resolver(
    name: &str,
    default: &str,
    choices: &[Choice],
    types: &TypeTable,
    target: &crate::target::Target,
) -> Function {
    let void_ptr = types.void_ptr_id;
    let uint = types.uint_id;
    let mut func = Function::new(name, void_ptr);
    let mut block = BasicBlock::new(BasicBlockId(0));
    block.add_insn(Instruction::call_with_abi(
        None,
        "__cpu_indicator_init",
        Vec::new(),
        Vec::new(),
        types.void_id,
        CallingConv::C,
        types,
        target,
    ));
    let mut chosen = address_of(&mut func, &mut block, default, void_ptr);
    for choice in choices.iter().rev() {
        let base = address_of(&mut func, &mut block, choice.word.0, void_ptr);
        let word = reg(&mut func);
        block.add_insn(Instruction::load(word, base, choice.word.1, uint, 32));
        let mask = value(&mut func, i128::from(choice.mask));
        let bits = reg(&mut func);
        block.add_insn(Instruction::binop(Opcode::And, bits, word, mask, uint, 32));
        let zero = value(&mut func, 0);
        let has = reg(&mut func);
        block.add_insn(Instruction::compare(
            Opcode::SetNe,
            has,
            (bits, zero),
            (uint, 32),
            (types.int_id, 32),
        ));
        let version = address_of(&mut func, &mut block, &choice.symbol, void_ptr);
        let picked = reg(&mut func);
        block.add_insn(Instruction::select(
            picked, has, version, chosen, void_ptr, 64,
        ));
        chosen = picked;
    }
    block.add_insn(Instruction::ret_typed(Some(chosen), void_ptr, 64));
    func.entry = BasicBlockId(0);
    func.add_block(block);
    func
}
