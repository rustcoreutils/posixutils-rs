//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Architecture-independent code generation interface
//

use crate::ir::Module;
use crate::target::Target;

/// Trait for architecture-specific code generators
pub trait CodeGenerator {
    /// Generate assembly code for the given IR module
    fn generate(&mut self, module: &Module) -> String;

    /// Set whether to emit basic unwind tables (cfi_startproc/cfi_endproc)
    fn set_emit_unwind_tables(&mut self, emit: bool);
}

/// Create a code generator for the given target with options
pub fn create_codegen_with_options(
    target: Target,
    emit_unwind_tables: bool,
) -> Box<dyn CodeGenerator> {
    use crate::target::Arch;

    let mut codegen: Box<dyn CodeGenerator> = match target.arch {
        Arch::X86_64 => Box::new(super::x86_64::codegen::X86_64CodeGen::new(target)),
        Arch::Aarch64 => Box::new(super::aarch64::codegen::Aarch64CodeGen::new(target)),
    };
    codegen.set_emit_unwind_tables(emit_unwind_tables);
    codegen
}
