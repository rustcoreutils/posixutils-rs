//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT

//! `destructor` attributes for Mach-O, registered rather than listed.
//!
//! ELF runs `.fini_array` at exit and that is the end of it. Mach-O has
//! `__DATA,__mod_term_func`, which looks like the same thing and is not: it is
//! deprecated, and a terminator listed there is not run for a main executable
//! -- so a `destructor` function simply never ran, while the constructors
//! beside it did.
//!
//! What does run is anything handed to `atexit`. So each destructor becomes an
//! `atexit` call made from a synthesized *constructor*, which is a mechanism
//! Mach-O does support.
//!
//! Order is preserved by construction. `atexit` runs its handlers in reverse
//! of registration and ELF runs `.fini_array` in reverse of the array, so
//! registering in the order the entries would have been listed in gives the
//! same sequence -- see `CodeGenBase::emit_init_arrays`, which sorts them.

use super::{BasicBlock, BasicBlockId, Function, Instruction, Module, Pseudo, PseudoId};
use crate::abi::{get_abi_for_conv, CallingConv};
use crate::ir::CallAbiInfo;
use crate::target::Target;
use crate::types::TypeTable;

/// Rewrite every `destructor` in `module` into an `atexit` registration.
///
/// `enabled` is whether the target is Mach-O; ELF wants the array it already
/// builds, and gets it by this pass leaving the attribute alone.
pub fn register_destructors_with_atexit(
    module: &mut Module,
    types: &TypeTable,
    target: &Target,
    enabled: bool,
) {
    if !enabled {
        return;
    }

    // Taking the attribute is what stops the backend also listing it in
    // `__mod_term_func`, which would either do nothing or run it twice.
    let mut destructors = Vec::new();
    for func in &mut module.functions {
        if !func.emit {
            continue;
        }
        if let Some(priority) = func.destructor.take() {
            destructors.push((func.name.clone(), priority));
        }
    }
    if destructors.is_empty() {
        return;
    }

    let void = types.void_id;
    let void_ptr = types.void_ptr_id;
    let abi = get_abi_for_conv(CallingConv::C, target);
    let param_class = abi.classify_param(void_ptr, types);
    let ret_class = abi.classify_return(void, types);

    for (index, (name, priority)) in destructors.into_iter().enumerate() {
        // Two pseudos: the destructor's symbol, and the address of it that
        // becomes the argument.
        let sym = PseudoId(0);
        let addr = PseudoId(1);

        let mut registrar = Function::new(format!("__c17_atexit.{index}"), void);
        registrar.is_static = true;
        registrar.constructor = Some(priority);
        registrar.add_pseudo(Pseudo::sym(sym, name.clone()));
        registrar.add_pseudo(Pseudo::reg(addr, addr.0));
        registrar.next_pseudo = 2;

        let mut call = Instruction::call(None, "atexit", vec![addr], vec![void_ptr], void, 0);
        call.abi_info = Some(Box::new(CallAbiInfo::new(
            vec![param_class.clone()],
            ret_class.clone(),
        )));

        let mut block = BasicBlock::new(BasicBlockId(0));
        block.add_insn(Instruction::sym_addr(addr, sym, void_ptr));
        block.add_insn(call);
        block.add_insn(Instruction::ret(None));
        registrar.entry = BasicBlockId(0);
        registrar.add_block(block);

        module.functions.push(registrar);
    }

    module.extern_symbols.insert("atexit".to_string());
}
