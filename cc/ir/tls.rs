//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT

//! Thread-local access expansion for the dynamic TLS model.
//!
//! Under Local Exec and Initial Exec a thread-local's address is a link-time
//! or load-time constant, so the backend folds the access into one instruction
//! -- `movl %fs:tv@TPOFF, %eax`. Under the dynamic model it is computed by a
//! call, which clobbers a register, and the register allocator has to know
//! that. The allocator runs over the IR and finishes before any machine
//! instruction exists, so the computation has to be visible *here* rather than
//! appearing later inside a backend `emit_*` helper.
//!
//! This pass makes it visible: every reference to a thread-local becomes an
//! explicit [`Opcode::TlsAddr`] producing an ordinary pointer, followed by an
//! ordinary access through that pointer. Afterwards a thread-local never
//! reaches the backend as a global operand at all, which is also why the
//! backend's thread-local special cases need no dynamic-model arm.
//!
//! It runs only for the dynamic model. Expanding unconditionally would turn
//! the one-instruction Local Exec access into three.

use super::{Function, Instruction, Module, Opcode, Pseudo, PseudoKind};
use std::collections::HashSet;

/// Rewrite every thread-local access in `module` into an explicit address
/// computation followed by an ordinary access.
///
/// `dynamic` selects whether the pass does anything: it is the same condition
/// the backend uses to choose the dynamic model, and the two must agree, since
/// the backend's own thread-local paths are what handle the other models.
pub fn expand_dynamic_tls(module: &mut Module, dynamic: bool) {
    if !dynamic {
        return;
    }

    let tls: HashSet<String> = module
        .globals
        .iter()
        .filter(|g| g.is_thread_local)
        .map(|g| g.name.clone())
        .chain(module.extern_tls_symbols.iter().cloned())
        .collect();

    if tls.is_empty() {
        return;
    }

    for func in &mut module.functions {
        expand_function(func, &tls);
    }
}

/// Whether `pseudo` names a thread-local symbol.
fn names_tls(func: &Function, id: super::PseudoId, tls: &HashSet<String>) -> bool {
    match func.get_pseudo(id).map(|p| &p.kind) {
        // A `Sym` pseudo that also appears in `locals` is a stack slot whose
        // name merely collides with a global's; only the global is thread-local.
        Some(PseudoKind::Sym(name)) => tls.contains(name) && !func.locals.contains_key(name),
        _ => false,
    }
}

fn expand_function(func: &mut Function, tls: &HashSet<String>) {
    for block_idx in 0..func.blocks.len() {
        let old = std::mem::take(&mut func.blocks[block_idx].insns);
        let mut new = Vec::with_capacity(old.len());

        for insn in old {
            // Address-of is the computation itself, so it converts in place
            // rather than growing a second instruction.
            if insn.op == Opcode::SymAddr
                && insn.src.first().is_some_and(|&s| names_tls(func, s, tls))
            {
                let mut converted = insn.clone();
                converted.op = Opcode::TlsAddr;
                new.push(converted);
                continue;
            }

            // Any other use of a thread-local -- a load, a store, an operand --
            // gets the address materialised first and then works through it.
            let mut rewritten = insn.clone();
            for slot in 0..rewritten.src.len() {
                let sym = rewritten.src[slot];
                if !names_tls(func, sym, tls) {
                    continue;
                }
                let addr = func.alloc_pseudo();
                func.add_pseudo(Pseudo::reg(addr, addr.0));
                // The address is a pointer whatever the accessed type is;
                // reuse the instruction's own type id, which every access
                // carries, rather than inventing a pointer type here.
                let typ = match rewritten.typ {
                    Some(t) => t,
                    None => continue,
                };
                new.push(Instruction::tls_addr(addr, sym, typ));
                rewritten.src[slot] = addr;
            }
            new.push(rewritten);
        }

        func.blocks[block_idx].insns = new;
    }
}
