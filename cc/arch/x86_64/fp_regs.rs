//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Code for an ISA without SSE2 or without the x87: `-mno-sse`,
// `-mno-80387`, `-mgeneral-regs-only` -- a kernel, whose entry saves no
// floating-point state, so a function that touches an XMM register or the
// x87 stack corrupts the interrupted task's.
//
// c17 keeps `float`, `double`, `_Float16`, binary128 and every vector in
// SSE2's registers and `long double` on the x87; it has no other code for
// them. A function compiled without the one its values need is refused,
// in gcc's words where gcc refuses it too: a value returned in a register
// the ISA lacks, and a variadic argument read from one. Everywhere else gcc
// would compute without the registers (the x87 for SSE, libgcc's software
// routines) and c17 says it cannot. Checked after optimization, as gcc
// checks: code the inliner or the folder leaves out needs nothing.
//
// Integer code needs none of it, and gets none: the variadic prologue
// saves no XMM register, a variadic call counts none in %al (and with
// `-mskip-rax-setup` sets nothing), and block copies use general registers.
//

use crate::abi::{get_abi, ArgClass, RegClass};
use crate::diag;
use crate::ir::{Function, Module, Opcode};
use crate::target::Target;
use crate::types::{TypeId, TypeKind, TypeTable};

/// The register file a value of a type lives in, where it is not a general
/// register.
#[derive(Clone, Copy, PartialEq, Eq)]
enum FpFile {
    Sse,
    X87,
}

/// The register file c17 keeps a value of `t` in, if not a general one.
fn file_of(t: TypeId, types: &TypeTable) -> Option<FpFile> {
    if types.is_vector(t) {
        return Some(FpFile::Sse);
    }
    let base = if types.is_complex(t) {
        types.complex_base(t)
    } else {
        t
    };
    match types.kind(base) {
        TypeKind::LongDouble => Some(FpFile::X87),
        TypeKind::Float | TypeKind::Double | TypeKind::Float16 | TypeKind::Float128 => {
            Some(FpFile::Sse)
        }
        _ => None,
    }
}

/// What the function's ISA lacks.
struct Missing {
    sse: bool,
    x87: bool,
    /// `-mno-fp-ret-in-387`: no `long double` comes back in `%st(0)`.
    x87_return: bool,
}

impl Missing {
    fn lacks(&self, file: FpFile) -> bool {
        match file {
            FpFile::Sse => self.sse,
            FpFile::X87 => self.x87,
        }
    }
}

/// Refuse what each emitted function of `module` needs of a register file
/// its ISA does not have.
pub(crate) fn check(module: &Module, types: &TypeTable, target: &Target) {
    for func in module.functions.iter().filter(|f| f.emit) {
        let missing = Missing {
            sse: !func.isa.has_sse2(),
            x87: !func.isa.has_x87(),
            x87_return: !func.isa.has_x87() || target.x86.no_fp_ret_in_387,
        };
        if missing.sse || missing.x87 || missing.x87_return {
            check_function(func, types, target, &missing);
        }
    }
}

/// gcc's word for a return in `file` the ISA lacks, if it lacks it.
fn return_error(class: &ArgClass, missing: &Missing) -> Option<&'static str> {
    match class {
        ArgClass::X87 { .. } if missing.x87_return => Some("x87 register return with x87 disabled"),
        ArgClass::Direct { classes, .. } if missing.sse && classes.contains(&RegClass::Sse) => {
            Some("SSE register return with SSE disabled")
        }
        _ => None,
    }
}

/// Whether an argument of `class` travels in an SSE register.
fn passes_in_sse(class: &ArgClass) -> bool {
    matches!(class, ArgClass::Direct { classes, .. } if classes.contains(&RegClass::Sse))
}

fn check_function(func: &Function, types: &TypeTable, target: &Target, missing: &Missing) {
    let abi = get_abi(target);
    let fn_pos = crate::arch::func_pos(func);
    if let Some(msg) = return_error(&abi.classify_return(func.return_type, types), missing) {
        diag::error(fn_pos, msg);
        return;
    }
    if missing.sse
        && func
            .params
            .iter()
            .any(|(_, t)| passes_in_sse(&abi.classify_param(*t, types)))
    {
        diag::error(fn_pos, "SSE register argument with SSE disabled");
        return;
    }
    for insn in func.blocks.iter().flat_map(|b| b.insns.iter()) {
        let pos = insn.pos.unwrap_or(fn_pos);
        match insn.op {
            Opcode::Call => {
                if let Some(msg) = insn
                    .typ
                    .and_then(|t| return_error(&abi.classify_return(t, types), missing))
                {
                    diag::error(pos, msg);
                    return;
                }
                let args = &insn.extra().arg_types;
                if missing.sse
                    && args
                        .iter()
                        .any(|t| passes_in_sse(&abi.classify_param(*t, types)))
                {
                    diag::error(pos, "SSE register argument with SSE disabled");
                    return;
                }
            }
            Opcode::VaArg => {
                if insn
                    .typ
                    .is_some_and(|t| missing.sse && passes_in_sse(&abi.classify_param(t, types)))
                {
                    diag::error(pos, "SSE register argument with SSE disabled");
                    return;
                }
            }
            _ => {
                let lacking = [insn.typ, insn.src_typ]
                    .into_iter()
                    .flatten()
                    .filter_map(|t| file_of(t, types))
                    .find(|&f| missing.lacks(f));
                if let Some(file) = lacking {
                    diag::error(
                        pos,
                        match file {
                            FpFile::Sse => "floating-point code with SSE disabled is not supported",
                            FpFile::X87 => "x87 code with x87 disabled is not supported",
                        },
                    );
                    return;
                }
            }
        }
    }
}
