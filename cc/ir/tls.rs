//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT

//! Thread-local access expansion for the call-based TLS models.
//!
//! Under ELF Local Exec and Initial Exec a thread-local's address is a
//! link-time or load-time constant, so the backend folds the access into one
//! instruction -- `movl %fs:tv@TPOFF, %eax`. Under the ELF descriptor model,
//! and under Mach-O's thread-local variable descriptors on every Darwin
//! access, it is computed by a call, which clobbers registers, and the
//! register allocator has to know that. The allocator runs over the IR and
//! finishes before any machine instruction exists, so the computation has to
//! be visible *here* rather than appearing later inside a backend `emit_*`
//! helper. Which targets call is [`crate::target::Target::tls_access`].
//!
//! This pass makes it visible: every reference to a thread-local becomes an
//! explicit [`Opcode::TlsAddr`] producing an ordinary pointer, followed by an
//! ordinary access through that pointer. Afterwards a thread-local never
//! reaches the backend as a global operand at all, which is also why the
//! backend's thread-local special cases need no dynamic-model arm.
//!
//! It runs only for a call-based model. Expanding unconditionally would turn
//! the one-instruction Local Exec access into three.
//!
//! Every place a pseudo can name an object is rewritten: instruction sources,
//! and the operands of inline assembly, which live in `asm_data` rather than
//! `src`. A thread-local memory operand left as a `Sym` reached the backend
//! as a global, where the descriptor call was emitted invisibly to the
//! allocator on Linux, and a plain non-TLS reference was printed on Darwin.

use super::{Function, Instruction, Module, Opcode, Pseudo, PseudoId};
use crate::types::{TypeId, TypeTable};
use std::collections::{BTreeMap, HashSet};

/// Rewrite every thread-local access in `module` into an explicit address
/// computation followed by an ordinary access.
///
/// `dynamic` selects whether the pass does anything: it is the same condition
/// the backend uses to choose the dynamic model, and the two must agree, since
/// the backend's own thread-local paths are what handle the other models.
///
/// The addresses this pass introduces are `void *`s. What is accessed
/// through them is irrelevant to how they are held.
pub fn expand_dynamic_tls(module: &mut Module, dynamic: bool, types: &TypeTable) {
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

    let ptr_type = types.void_ptr_id;
    let ptr = (ptr_type, types.size_bits(ptr_type));
    for func in &mut module.functions {
        expand_function(func, &tls, ptr);
    }
}

/// The thread-local symbol `pseudo` names, if it names one.
///
/// Returns the name rather than a yes/no because two references to the same
/// thread-local are two *different* `Sym` pseudos carrying the same name --
/// which is what the address cache has to key on.
fn tls_name(func: &Function, id: super::PseudoId, tls: &HashSet<String>) -> Option<String> {
    // A `Sym` pseudo that *is* a local is a stack slot whose name merely
    // collides with a global's; only the global is thread-local. Asked by
    // identity, since a parameter and a block-scope `extern` can share a name.
    func.global_sym_name(id)
        .filter(|name| tls.contains(*name))
        .map(str::to_string)
}

/// `ptr` is the type and width of the addresses it introduces.
fn expand_function(func: &mut Function, tls: &HashSet<String>, ptr: (TypeId, u32)) {
    let (ptr_type, ptr_bits) = ptr;
    for block_idx in 0..func.blocks.len() {
        let old = std::mem::take(&mut func.blocks[block_idx].insns);
        let mut new = Vec::with_capacity(old.len());
        // One address per thread-local per block. A thread-local's address is
        // fixed for the thread's lifetime, so the second reference in a block
        // can reuse the first's -- and each computation is a call under this
        // model, so re-deriving it per reference is a call per reference.
        //
        // Per block rather than per function because a later block is not
        // necessarily dominated by an earlier one.
        let mut addr_of: BTreeMap<String, super::PseudoId> = BTreeMap::new();

        for insn in old {
            // Address-of is the computation itself, so it converts in place
            // rather than growing a second instruction.
            if insn.op == Opcode::SymAddr
                && insn
                    .src
                    .first()
                    .is_some_and(|&s| tls_name(func, s, tls).is_some())
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
                let Some(name) = tls_name(func, sym, tls) else {
                    continue;
                };
                rewritten.src[slot] = address_of(func, &mut new, &mut addr_of, name, sym, ptr_type);
            }
            // An inline-asm operand names its object in `asm_data`. A memory
            // operand that is the thread-local itself becomes an address
            // value, with the constraint's byte offset folded into it, since
            // an address value carries no offset of its own.
            if let Some(asm) = rewritten.extra_mut().asm_data.as_mut() {
                for c in asm.inputs.iter_mut().chain(asm.outputs.iter_mut()) {
                    let Some(name) = tls_name(func, c.pseudo, tls) else {
                        continue;
                    };
                    let base = address_of(func, &mut new, &mut addr_of, name, c.pseudo, ptr_type);
                    c.pseudo = if c.offset == 0 {
                        base
                    } else {
                        let delta = func.alloc_pseudo();
                        func.add_pseudo(Pseudo::val(delta, i128::from(c.offset)));
                        let addr = func.alloc_pseudo();
                        func.add_pseudo(Pseudo::reg(addr, addr.0));
                        new.push(Instruction::binop(
                            Opcode::Add,
                            addr,
                            base,
                            delta,
                            ptr_type,
                            ptr_bits,
                        ));
                        addr
                    };
                    c.offset = 0;
                }
            }
            new.push(rewritten);
        }

        func.blocks[block_idx].insns = new;
    }
}

/// The pseudo holding the address of the thread-local `name`, emitting the
/// `TlsAddr` that computes it into `new` the first time a block needs it.
fn address_of(
    func: &mut Function,
    new: &mut Vec<Instruction>,
    addr_of: &mut BTreeMap<String, PseudoId>,
    name: String,
    sym: PseudoId,
    ptr_type: TypeId,
) -> PseudoId {
    if let Some(&addr) = addr_of.get(&name) {
        return addr;
    }
    let addr = func.alloc_pseudo();
    func.add_pseudo(Pseudo::reg(addr, addr.0));
    // The result is an address, so it is typed as one. Typing it as whatever
    // is accessed through it -- which is what the instruction itself carries
    // -- would hand a pointer to the register allocator as, say, a double,
    // and get an SSE register for it.
    new.push(Instruction::tls_addr(addr, sym, ptr_type));
    addr_of.insert(name, addr);
    addr
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{
        AsmConstraint, AsmData, BasicBlock, BasicBlockId, GlobalDef, Initializer, PseudoKind,
    };
    use crate::target::Target;
    use crate::types::TypeTable;

    /// A thread-local named by an inline-asm memory operand lives in
    /// `asm_data`, not `src`, and used to reach the backend as a global. The
    /// pass gives it an address like any other reference, folding the
    /// operand's byte offset in, since an address value carries none.
    #[test]
    fn asm_memory_operand_on_a_thread_local_gets_its_address() {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("f", types.int_id);
        let sym = PseudoId(0);
        func.add_pseudo(Pseudo::sym(sym, "tarr".to_string()));
        let out = PseudoId(1);
        func.add_pseudo(Pseudo::reg(out, 1));
        func.next_pseudo = 2;
        let operand = |pseudo, constraint: &str, offset| AsmConstraint {
            offset,
            ..AsmConstraint::new(pseudo, constraint, Target::host().arch, 64)
        };
        let mut entry = BasicBlock::new(BasicBlockId(0));
        entry.add_insn(Instruction::new(Opcode::Entry));
        entry.add_insn(Instruction::asm(AsmData {
            template: String::new(),
            outputs: vec![operand(out, "=r", 0), operand(sym, "=m", 0)],
            inputs: vec![operand(sym, "m", 16)],
            clobbers: Vec::new(),
            goto_labels: Vec::new(),
        }));
        entry.add_insn(Instruction::ret(None));
        func.entry = BasicBlockId(0);
        func.blocks = vec![entry];
        func.rebuild_block_idx();

        let mut global = GlobalDef::new("tarr", types.long_id, Initializer::None);
        global.is_thread_local = true;
        let mut module = Module::default();
        module.globals.push(global);
        module.add_function(func);
        expand_dynamic_tls(&mut module, true, &types);

        let insns = &module.functions[0].blocks[0].insns;
        let tls: Vec<_> = insns.iter().filter(|i| i.op == Opcode::TlsAddr).collect();
        assert_eq!(tls.len(), 1, "one address per block: {insns:?}");
        let addr = tls[0].target.unwrap();
        let asm = insns
            .iter()
            .find(|i| i.op == Opcode::Asm)
            .and_then(|i| i.extra().asm_data.as_ref())
            .unwrap();
        // The output names the object at offset zero: the address itself.
        assert_eq!(asm.outputs[1].pseudo, addr);
        assert_eq!(asm.outputs[1].offset, 0);
        // The input's 16 bytes are an `Add` onto that address.
        let input = &asm.inputs[0];
        assert_eq!(input.offset, 0);
        let add = insns
            .iter()
            .find(|i| i.op == Opcode::Add && i.target == Some(input.pseudo))
            .expect("offset folded into an add");
        assert_eq!(add.src[0], addr);
        let delta = module.functions[0].get_pseudo(add.src[1]).unwrap();
        assert!(matches!(delta.kind, PseudoKind::Val(16)));
        // The register operand is untouched.
        assert_eq!(asm.outputs[0].pseudo, out);
    }

    /// A parameter is registered in `locals` under its bare name, and a
    /// thread-local reached through a block-scope `extern` of the same name
    /// is a different `Sym` carrying that name. Only the parameter's own
    /// pseudo is the stack slot; the global's store, load and address-of are
    /// all thread-local accesses.
    #[test]
    fn thread_local_shadowed_by_a_parameter_is_still_expanded() {
        let types = TypeTable::new(&Target::host());
        let int = types.int_id;
        let mut func = Function::new("f", int);
        let param = PseudoId(0);
        func.add_pseudo(Pseudo::sym(param, "x".to_string()));
        func.add_local("x", param, int, None, None);
        let global = PseudoId(1);
        func.add_pseudo(Pseudo::sym(global, "x".to_string()));
        let one = PseudoId(2);
        func.add_pseudo(Pseudo::val(one, 1));
        let loaded = PseudoId(3);
        func.add_pseudo(Pseudo::reg(loaded, 3));
        let addr = PseudoId(4);
        func.add_pseudo(Pseudo::reg(addr, 4));
        let from_param = PseudoId(5);
        func.add_pseudo(Pseudo::reg(from_param, 5));
        func.next_pseudo = 6;
        let mut entry = BasicBlock::new(BasicBlockId(0));
        entry.add_insn(Instruction::new(Opcode::Entry));
        entry.add_insn(Instruction::store(one, global, 0, int, 32));
        entry.add_insn(Instruction::load(loaded, global, 0, int, 32));
        entry.add_insn(Instruction::sym_addr(addr, global, types.void_ptr_id));
        entry.add_insn(Instruction::load(from_param, param, 0, int, 32));
        entry.add_insn(Instruction::ret(None));
        func.entry = BasicBlockId(0);
        func.blocks = vec![entry];
        func.rebuild_block_idx();

        let mut module = Module::default();
        module.extern_tls_symbols.insert("x".to_string());
        module.add_function(func);
        expand_dynamic_tls(&mut module, true, &types);

        let insns = &module.functions[0].blocks[0].insns;
        let tls: Vec<_> = insns.iter().filter(|i| i.op == Opcode::TlsAddr).collect();
        // One computed for the store and reused by the load, and the
        // address-of converted in place.
        assert_eq!(tls.len(), 2, "{insns:?}");
        assert!(tls.iter().all(|i| i.src == [global]), "{insns:?}");
        let computed = tls[0].target.unwrap();
        let store = insns.iter().find(|i| i.op == Opcode::Store).unwrap();
        assert_eq!(store.src[0], computed);
        let load = insns
            .iter()
            .find(|i| i.op == Opcode::Load && i.target == Some(loaded))
            .unwrap();
        assert_eq!(load.src[0], computed);
        assert!(tls.iter().any(|i| i.target == Some(addr)), "{insns:?}");
        // The parameter's own slot is untouched.
        let param_load = insns
            .iter()
            .find(|i| i.op == Opcode::Load && i.target == Some(from_param))
            .unwrap();
        assert_eq!(param_load.src[0], param);
    }
}
