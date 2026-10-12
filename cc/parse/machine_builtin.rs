//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Builtins that are one machine instruction (`ast::MachineOp`): their
// arguments checked through gcc 13's prototype for each, as a call's are.
// The linearizer emits the instruction.
//

use super::ast::{Expr, ExprKind, FpSysReg, MachineOp};
use super::builtin_args::BuiltinArgs;
use super::parser::{ParseResult, Parser};
use crate::kw;
use crate::strings::StringId;
use crate::token::lexer::Position;
use crate::types::{Type, TypeId};

impl Parser<'_> {
    /// `__builtin_ia32_rdtsc`, `__builtin_ia32_rdtscp`,
    /// `__builtin_ia32_cvtpd2ps`, `__builtin_thread_pointer` and aarch64's
    /// `__builtin_aarch64_{get,set}_{fpcr,fpsr}{,64}`.
    pub(super) fn parse_machine_builtin(
        &mut self,
        name_id: StringId,
        pos: Position,
    ) -> Option<ParseResult<Expr>> {
        let op = match name_id {
            kw::BUILTIN_IA32_RDTSC => MachineOp::Rdtsc,
            kw::BUILTIN_IA32_RDTSCP => MachineOp::Rdtscp,
            kw::BUILTIN_IA32_CVTPD2PS => MachineOp::Cvtpd2ps,
            kw::BUILTIN_THREAD_POINTER => MachineOp::ThreadPointer,
            kw::BUILTIN_AARCH64_GET_FPCR => fp_read(FpSysReg::Fpcr, false),
            kw::BUILTIN_AARCH64_GET_FPSR => fp_read(FpSysReg::Fpsr, false),
            kw::BUILTIN_AARCH64_GET_FPCR64 => fp_read(FpSysReg::Fpcr, true),
            kw::BUILTIN_AARCH64_GET_FPSR64 => fp_read(FpSysReg::Fpsr, true),
            kw::BUILTIN_AARCH64_SET_FPCR => fp_write(FpSysReg::Fpcr, false),
            kw::BUILTIN_AARCH64_SET_FPSR => fp_write(FpSysReg::Fpsr, false),
            kw::BUILTIN_AARCH64_SET_FPCR64 => fp_write(FpSysReg::Fpcr, true),
            kw::BUILTIN_AARCH64_SET_FPSR64 => fp_write(FpSysReg::Fpsr, true),
            _ => return None,
        };
        Some((|| {
            let (ret, params) = self.machine_prototype(op);
            let func_type = self.types.intern(Type::function(ret, params, false, false));
            let BuiltinArgs { args, call_pos } = self.parse_builtin_call_args()?;
            Ok(
                match self.check_prototyped_builtin(name_id, func_type, args, call_pos) {
                    Some(args) => Self::typed_expr(ExprKind::MachineBuiltin { op, args }, ret, pos),
                    None => self.diagnosed_call(ret, pos),
                },
            )
        })())
    }

    /// gcc's prototype for `op`: its return type and its parameters'.
    fn machine_prototype(&mut self, op: MachineOp) -> (TypeId, Vec<TypeId>) {
        let t = &mut *self.types;
        match op {
            MachineOp::Rdtsc => (t.ulonglong_id, vec![]),
            MachineOp::Rdtscp => (t.ulonglong_id, vec![t.pointer_to(t.uint_id)]),
            MachineOp::Cvtpd2ps => {
                let v2df = t.vector_of(t.double_id, 2, None);
                (t.vector_of(t.float_id, 4, None), vec![v2df])
            }
            MachineOp::ThreadPointer => (t.void_ptr_id, vec![]),
            // gcc 13: `unsigned int` for the plain forms, `unsigned long
            // long` for the `64` ones; a write returns nothing.
            MachineOp::FpRegRead { wide, .. } => {
                (if wide { t.ulonglong_id } else { t.uint_id }, vec![])
            }
            MachineOp::FpRegWrite { wide, .. } => {
                let value = if wide { t.ulonglong_id } else { t.uint_id };
                (t.void_id, vec![value])
            }
        }
    }
}

fn fp_read(reg: FpSysReg, wide: bool) -> MachineOp {
    MachineOp::FpRegRead { reg, wide }
}

fn fp_write(reg: FpSysReg, wide: bool) -> MachineOp {
    MachineOp::FpRegWrite { reg, wide }
}
