//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 Call Code Generation
//

use super::codegen::Aarch64CodeGen;
use super::lir::{Aarch64Inst, GpOperand, MemAddr};
use super::regalloc::{Loc, Reg, VReg};
use crate::abi::{ArgClass, HfaBase, RegClass};
use crate::arch::lir::{complex_fp_info, CallTarget, FpSize, OperandSize, Symbol};
use crate::ir::{Instruction, PseudoId};
use crate::target::Target;
use crate::types::{TypeId, TypeKind, TypeTable};

impl Aarch64CodeGen {
    /// Handle sret (hidden struct return pointer) argument
    pub(super) fn setup_sret_arg(&mut self, insn: &Instruction) -> usize {
        if insn.returns_via_sret() && !insn.src.is_empty() {
            // First argument is sret pointer - move to X8
            self.emit_move(insn.src[0], Reg::X8, 64);
            1 // Skip first arg in main loop
        } else {
            0
        }
    }

    /// Handle Darwin variadic call arguments (all variadic args go on stack)
    pub(super) fn setup_darwin_variadic_args(
        &mut self,
        insn: &Instruction,
        args_start: usize,
        types: &TypeTable,
    ) -> i32 {
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = VReg::arg_regs();
        let variadic_start = insn.variadic_arg_start.unwrap_or(usize::MAX);
        let mut int_arg_idx = 0;
        let mut fp_arg_idx = 0;
        let mut stack_args = 0;

        // Collect variadic args for stack. The `Option<i32>` is an
        // aggregate's byte count: it occupies its own size rounded up to
        // eight, not one slot holding a pointer to it.
        let mut variadic_args: Vec<(PseudoId, bool, u32, Option<i32>)> = Vec::new();

        for (i, &arg) in insn.src.iter().enumerate().skip(args_start) {
            let arg_type = insn.arg_types.get(i).copied();
            let is_fp = if let Some(typ) = arg_type {
                types.is_float(typ)
            } else {
                let arg_loc = self.get_location(arg);
                matches!(arg_loc, Loc::VReg(_) | Loc::FImm(..))
            };

            let arg_size = if let Some(typ) = arg_type {
                types.size_bits(typ).max(32)
            } else {
                64
            };

            // A composite of at most sixteen bytes travels in two X registers
            // here as it does everywhere else (AAPCS64 §5.4.2 C.10). Darwin
            // takes its own path for a variadic call, and that path still
            // moved one register's worth -- so the composite that the rest of
            // the compiler now passes as a pair arrived half formed.
            // Which aggregates are copied into the stack slot as objects.
            //
            // Not one of eight bytes or fewer: that pseudo holds the aggregate
            // *value*, exactly as a scalar does, so it goes through the
            // ordinary move below. Copying it meant dereferencing the value as
            // though it were an address -- `struct { float a, b; }` faulted on
            // whatever address its two floats spelled.
            //
            // And not one too large to pass directly: stage B.4 replaces that
            // with a pointer to the caller's copy, and `va_arg` reads it that
            // way, so the pointer travels and the object stays put.
            let agg_bytes = arg_type.and_then(|t| {
                let abi =
                    crate::abi::get_abi_for_conv(crate::abi::CallingConv::C, &self.base.target);
                (matches!(
                    types.kind(t),
                    TypeKind::Struct | TypeKind::Union | TypeKind::Array
                ) && types.size_bits(t) > 64
                    && !matches!(abi.classify_param(t, types), ArgClass::Indirect { .. }))
                .then(|| (types.size_bits(t) / 8).max(1) as i32)
            });
            let gp_pair = agg_bytes.is_some_and(|_| {
                let abi =
                    crate::abi::get_abi_for_conv(crate::abi::CallingConv::C, &self.base.target);
                arg_type.is_some_and(|t| {
                    matches!(
                        abi.classify_param(t, types),
                        ArgClass::Direct { ref classes, .. }
                            if classes.len() == 2
                                && classes.iter().all(|c| *c == crate::abi::RegClass::Integer)
                    )
                })
            });

            if i >= variadic_start {
                variadic_args.push((arg, is_fp, arg_size, agg_bytes));
            } else if gp_pair {
                if int_arg_idx + 1 < int_arg_regs.len() {
                    let mem = match self.get_location(arg) {
                        ref l @ (Loc::Stack(_) | Loc::IncomingArg(_)) => self.loc_mem(l).unwrap(),
                        Loc::Reg(r) => MemAddr::BaseOffset { base: r, offset: 0 },
                        _ => MemAddr::BaseOffset {
                            base: Reg::X9,
                            offset: 0,
                        },
                    };
                    self.emit_ldp_legalized(
                        OperandSize::B64,
                        mem,
                        int_arg_regs[int_arg_idx],
                        int_arg_regs[int_arg_idx + 1],
                    );
                    int_arg_idx += 2;
                }
            } else {
                // Fixed arg - use registers
                if is_fp {
                    let fp_size = if let Some(typ) = arg_type {
                        types.size_bits(typ)
                    } else {
                        64
                    };
                    if fp_arg_idx < fp_arg_regs.len() {
                        self.emit_fp_move(arg, fp_arg_regs[fp_arg_idx], arg_type, fp_size, types);
                        fp_arg_idx += 1;
                    }
                } else if int_arg_idx < int_arg_regs.len() {
                    self.emit_move(arg, int_arg_regs[int_arg_idx], arg_size);
                    int_arg_idx += 1;
                }
            }
        }

        // Store variadic args on stack
        let num_variadic = variadic_args.len();
        if num_variadic > 0 {
            let variadic_bytes: i32 = variadic_args
                .iter()
                .map(|(_, _, _, agg)| agg.map_or(8, |b| (b + 7) & !7))
                .sum();
            let aligned_bytes = (variadic_bytes + 15) & !15;

            self.push_lir(Aarch64Inst::Sub {
                size: OperandSize::B64,
                src1: Reg::sp(),
                src2: GpOperand::Imm(aligned_bytes as i64),
                dst: Reg::sp(),
            });

            let mut offset = 0i32;
            for (arg, is_fp, arg_size, agg_bytes) in variadic_args.into_iter() {
                if let Some(bytes) = agg_bytes {
                    // The pseudo locates the aggregate; its bytes go in the
                    // slot. Moving it as a scalar wrote the pointer instead.
                    let src = match self.get_location(arg) {
                        Loc::Reg(r) => r,
                        ref loc @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                            // Both frames reach here: an aggregate argument
                            // may be a local or may itself have arrived on the
                            // stack. `_ => continue` below would skip it.
                            let (base, disp) = self.loc_addr_parts(loc).unwrap();
                            self.push_lir(Aarch64Inst::Add {
                                size: OperandSize::B64,
                                src1: base,
                                src2: GpOperand::Imm(disp as i64),
                                dst: Reg::X9,
                            });
                            Reg::X9
                        }
                        _ => continue,
                    };
                    let mut done = 0;
                    while done < bytes {
                        let chunk = [8, 4, 2, 1]
                            .into_iter()
                            .find(|c| *c <= bytes - done)
                            .unwrap_or(1);
                        let size = OperandSize::from_bits(chunk as u32 * 8);
                        self.push_lir(Aarch64Inst::Ldr {
                            size,
                            addr: MemAddr::BaseOffset {
                                base: src,
                                offset: done,
                            },
                            dst: Reg::X16,
                        });
                        self.push_lir(Aarch64Inst::Str {
                            size,
                            src: Reg::X16,
                            addr: MemAddr::BaseOffset {
                                base: Reg::SP,
                                offset: offset + done,
                            },
                        });
                        done += chunk;
                    }
                    offset += (bytes + 7) & !7;
                    continue;
                }
                if is_fp {
                    // Variadic FP args don't have precise type info, use size-based detection
                    self.emit_fp_move(arg, VReg::V16, None, arg_size, types);
                    self.push_lir(Aarch64Inst::StrFp {
                        size: FpSize::Double,
                        src: VReg::V16,
                        addr: MemAddr::BaseOffset {
                            base: Reg::SP,
                            offset,
                        },
                    });
                } else {
                    self.emit_move(arg, Reg::X9, arg_size);
                    self.push_lir(Aarch64Inst::Str {
                        size: OperandSize::B64,
                        src: Reg::X9,
                        addr: MemAddr::BaseOffset {
                            base: Reg::SP,
                            offset,
                        },
                    });
                }
                offset += 8;
            }

            stack_args = (aligned_bytes + 15) / 16;
        }

        stack_args
    }

    /// Set up register arguments for standard AAPCS64 calls
    ///
    /// AAPCS64 requires stack arguments to be placed in parameter order at
    /// consecutive 8-byte slots starting from SP. Unlike x86-64, we don't use
    /// push instructions - instead we pre-allocate space and store directly.
    pub(super) fn setup_register_args(
        &mut self,
        insn: &Instruction,
        args_start: usize,
        types: &TypeTable,
    ) -> i32 {
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = VReg::arg_regs();

        // First pass: identify which args go to registers vs stack
        // Collect stack args with their info for the second pass
        /// What a stacked argument is made of, which decides both how many
        /// bytes it reserves and how its bytes are produced.
        ///
        /// This was a `complex_pair: bool`. Every multi-element argument that
        /// did not fit in the V registers took the `_Complex` path -- a fixed
        /// two-element loop at the complex element stride -- so a stacked
        /// three- or four-element HFA wrote the wrong number of elements at
        /// the wrong stride, and reserved the wrong slot besides.
        #[derive(Clone, Copy)]
        enum StackKind {
            /// An ordinary value; the pseudo holds it.
            Scalar,
            /// A `_Complex`: the pseudo holds the value's *address*, and both
            /// elements must be dereferenced out of it into one two-element
            /// slot. Pushing the pseudo twice as if it were two scalars wrote
            /// the pointer's bit pattern into both halves.
            Complex,
            /// An HFA: `count` elements of `base`, at that base's stride.
            Hfa { base: HfaBase, count: u8 },
            /// A composite laid on the stack by value. The pseudo carries its
            /// address; `bytes` of it are copied into the outgoing slot.
            /// Pushing it as a scalar wrote the *pointer* there instead.
            Composite { bytes: i32 },
        }

        struct StackArg {
            pseudo: PseudoId,
            is_fp: bool,
            size: u32,
            typ: Option<TypeId>,
            kind: StackKind,
        }

        impl StackArg {
            /// Where this argument starts, relative to the outgoing area.
            ///
            /// AAPCS64 §6.4.2 stage C rounds the next stacked-argument address
            /// up to `max(8, alignof(type))` *before* placing it. Advancing by
            /// the rounded size alone put a sixteen-byte-aligned argument eight
            /// bytes low whenever an odd number of eight-byte slots came first,
            /// and the callee made the same mistake, so it showed only against
            /// another compiler.
            fn slot_start(&self, at: i32, types: &TypeTable) -> i32 {
                let align = self.typ.map_or(8, |t| types.alignment(t) as i32).max(8);
                (at + align - 1) & !(align - 1)
            }

            fn slot_bytes(&self, types: &TypeTable, target: &Target) -> i32 {
                match self.kind {
                    StackKind::Complex => {
                        let elem = self
                            .typ
                            .map(|t| complex_fp_info(types, target, t).1)
                            .unwrap_or(8);
                        ((2 * elem) + 7) & !7
                    }
                    StackKind::Hfa { base, count } => {
                        ((count as i32 * HfaElem::of(base).bytes) + 7) & !7
                    }
                    StackKind::Composite { bytes } => (bytes + 7) & !7,
                    StackKind::Scalar => {
                        if self.size == 128 {
                            16
                        } else {
                            8
                        }
                    }
                }
            }
        }
        let mut stack_args_info: Vec<StackArg> = Vec::new();
        let mut int_arg_idx = 0;
        let mut fp_arg_idx = 0;

        for (i, &arg) in insn.src.iter().enumerate().skip(args_start) {
            let arg_type = insn.arg_types.get(i).copied();
            let is_complex = arg_type.is_some_and(|t| types.is_complex(t));
            // Every HFA, one element through four, goes out in V registers.
            // The exception is a single element small enough to sit in one
            // register: that arrives as an ordinary floating-point value and
            // goes out through `emit_fp_move` like any other scalar.
            let hfa_regs: Option<(HfaBase, usize)> = if is_complex {
                None
            } else {
                arg_type.and_then(|t| {
                    let abi =
                        crate::abi::get_abi_for_conv(crate::abi::CallingConv::C, &self.base.target);
                    match abi.classify_param(t, types) {
                        ArgClass::Hfa { base, count } => {
                            let count = count as usize;
                            if count == 1 && types.size_bits(t) <= 64 {
                                None
                            } else {
                                Some((base, count))
                            }
                        }
                        _ => None,
                    }
                })
            };
            // A composite of at most sixteen bytes that is not an HFA goes in
            // two consecutive X registers (AAPCS64 §5.4.2 C.10). The pseudo
            // carries its address, so the pair is loaded out of it. This used
            // to hand the address over instead, which the callee then read as
            // if it were the value.
            let gp_pair =
                !is_complex && hfa_regs.is_none() && arg_type.is_some_and(|t| {
                    matches!(
                        types.kind(t),
                        TypeKind::Struct | TypeKind::Union | TypeKind::Array
                    ) && {
                        let abi = crate::abi::get_abi_for_conv(
                            crate::abi::CallingConv::C,
                            &self.base.target,
                        );
                        matches!(
                            abi.classify_param(t, types),
                            ArgClass::Direct { ref classes, .. }
                                if classes.len() == 2
                                    && classes.iter().all(|c| *c == crate::abi::RegClass::Integer)
                        )
                    }
                });
            // A one-element HFA -- `struct { float v; }`, and every shape that
            // became one when arrays and half precision were admitted -- goes
            // in a single V register, exactly as the bare scalar does.
            let is_hfa_one = arg_type.is_some_and(|t| {
                let abi =
                    crate::abi::get_abi_for_conv(crate::abi::CallingConv::C, &self.base.target);
                types.size_bits(t) <= 64
                    && matches!(abi.classify_param(t, types), ArgClass::Hfa { count: 1, .. })
            });
            let is_fp = if is_hfa_one {
                true
            } else if let Some(typ) = arg_type {
                types.is_float(typ)
            } else {
                let arg_loc = self.get_location(arg);
                matches!(arg_loc, Loc::VReg(_) | Loc::FImm(..))
            };

            let arg_size = if let Some(typ) = arg_type {
                types.size_bits(typ).max(32)
            } else {
                64
            };

            if let Some((hfa_base, count)) = hfa_regs {
                if fp_arg_idx + count <= fp_arg_regs.len() {
                    let regs: Vec<VReg> = fp_arg_regs[fp_arg_idx..fp_arg_idx + count].to_vec();
                    self.setup_hfa_arg(arg, arg_type, &regs, types);
                    fp_arg_idx += count;
                } else {
                    stack_args_info.push(StackArg {
                        pseudo: arg,
                        is_fp: true,
                        size: arg_size,
                        typ: arg_type,
                        kind: StackKind::Hfa {
                            base: hfa_base,
                            count: count as u8,
                        },
                    });
                    // AAPCS64 §6.4.2: once anything is laid out on the stack,
                    // NSRN becomes 8 and every later floating-point argument
                    // follows it there. Unlike System V, the registers this
                    // argument did not fit into are *not* left available.
                    fp_arg_idx = fp_arg_regs.len();
                }
                continue;
            }
            if gp_pair {
                if int_arg_idx + 1 < int_arg_regs.len() {
                    let mem = match self.get_location(arg) {
                        // The slot holds the aggregate's own bytes.
                        ref l @ (Loc::Stack(_) | Loc::IncomingArg(_)) => self.loc_mem(l).unwrap(),
                        // The register holds its address.
                        Loc::Reg(r) => MemAddr::BaseOffset { base: r, offset: 0 },
                        _ => MemAddr::BaseOffset {
                            base: Reg::X9,
                            offset: 0,
                        },
                    };
                    self.emit_ldp_legalized(
                        OperandSize::B64,
                        mem,
                        int_arg_regs[int_arg_idx],
                        int_arg_regs[int_arg_idx + 1],
                    );
                    int_arg_idx += 2;
                } else {
                    stack_args_info.push(StackArg {
                        pseudo: arg,
                        is_fp: false,
                        size: arg_size,
                        typ: arg_type,
                        kind: StackKind::Composite {
                            bytes: arg_type.map_or(16, |t| (types.size_bits(t) / 8) as i32),
                        },
                    });
                }
                continue;
            }
            if is_complex {
                if fp_arg_idx + 1 < fp_arg_regs.len() {
                    self.setup_complex_arg(
                        arg,
                        arg_type,
                        fp_arg_regs[fp_arg_idx],
                        fp_arg_regs[fp_arg_idx + 1],
                        types,
                    );
                    fp_arg_idx += 2;
                } else {
                    stack_args_info.push(StackArg {
                        pseudo: arg,
                        is_fp: true,
                        size: arg_size,
                        typ: arg_type,
                        kind: StackKind::Complex,
                    });
                    fp_arg_idx = fp_arg_regs.len();
                }
            } else if is_fp {
                if fp_arg_idx < fp_arg_regs.len() {
                    let fp_size = if let Some(typ) = arg_type {
                        types.size_bits(typ)
                    } else {
                        64
                    };
                    self.emit_fp_move(arg, fp_arg_regs[fp_arg_idx], arg_type, fp_size, types);
                    fp_arg_idx += 1;
                } else {
                    stack_args_info.push(StackArg {
                        pseudo: arg,
                        is_fp: true,
                        size: arg_size,
                        typ: arg_type,
                        kind: StackKind::Scalar,
                    });
                }
            } else if arg_type.is_some_and(|t| types.kind(t) == crate::types::TypeKind::Int128) {
                // __int128 uses two consecutive *even-aligned* GP registers.
                if let Some(start) =
                    crate::arch::aarch64::int128_pair_start(int_arg_idx, int_arg_regs.len())
                {
                    int_arg_idx = start;
                    let loc = self.get_location(arg);
                    match loc {
                        ref l @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                            // Load lo/hi from the int128 slot into two
                            // consecutive regs. Either frame: a `__int128`
                            // parameter being forwarded may have arrived on
                            // the stack itself.
                            let mem = self.loc_mem(l).unwrap();
                            self.emit_ldp_legalized(
                                OperandSize::B64,
                                mem,
                                int_arg_regs[int_arg_idx],
                                int_arg_regs[int_arg_idx + 1],
                            );
                        }
                        Loc::Imm(v) => {
                            let lo = v as u64 as i64;
                            let hi = (v >> 64) as u64 as i64;
                            self.emit_mov_imm(int_arg_regs[int_arg_idx], lo, 64);
                            self.emit_mov_imm(int_arg_regs[int_arg_idx + 1], hi, 64);
                        }
                        _ => {
                            self.emit_move(arg, int_arg_regs[int_arg_idx], 64);
                            self.push_lir(Aarch64Inst::Mov {
                                size: OperandSize::B64,
                                src: GpOperand::Reg(Reg::Xzr),
                                dst: int_arg_regs[int_arg_idx + 1],
                            });
                        }
                    }
                    int_arg_idx += 2;
                } else {
                    // Int128 on stack needs 16 bytes
                    stack_args_info.push(StackArg {
                        pseudo: arg,
                        is_fp: false,
                        size: 128,
                        typ: arg_type,
                        kind: StackKind::Scalar,
                    });
                    // Stage C.11: an argument that does not fit sets NGRN to 8,
                    // so every later argument is on the stack as well. Leaving
                    // the index where it was handed the *next* argument a
                    // register the callee was not reading.
                    int_arg_idx = int_arg_regs.len();
                }
            } else if int_arg_idx < int_arg_regs.len() {
                self.emit_move(arg, int_arg_regs[int_arg_idx], arg_size);
                int_arg_idx += 1;
            } else {
                stack_args_info.push(StackArg {
                    pseudo: arg,
                    is_fp: false,
                    size: arg_size,
                    typ: arg_type,
                    kind: StackKind::Scalar,
                });
            }
        }

        // If no stack args, we're done
        if stack_args_info.is_empty() {
            return 0;
        }

        // Pre-allocate stack space for all stack args, 16-byte aligned.
        // Walked, not summed: alignment padding between arguments is part of
        // the area, and summing the sizes alone under-reserved it.
        let stack_bytes: i32 = stack_args_info.iter().fold(0, |at, a| {
            a.slot_start(at, types) + a.slot_bytes(types, &self.base.target)
        });
        let aligned_bytes = (stack_bytes + 15) & !15;

        self.push_lir(Aarch64Inst::Sub {
            size: OperandSize::B64,
            src1: Reg::sp(),
            src2: GpOperand::Imm(aligned_bytes as i64),
            dst: Reg::sp(),
        });

        // Store each stack arg at its proper offset from SP (in parameter order)
        let mut offset: i32 = 0;
        for stack_arg in stack_args_info.into_iter() {
            offset = stack_arg.slot_start(offset, types);
            if stack_arg
                .typ
                .is_some_and(|t| types.kind(t) == TypeKind::Int128)
            {
                // Int128: store both 64-bit halves
                let loc = self.get_location(stack_arg.pseudo);
                match loc {
                    ref l @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                        let mem = self.loc_mem(l).unwrap();
                        self.emit_ldp_legalized(OperandSize::B64, mem, Reg::X9, Reg::X10);
                    }
                    Loc::Imm(v) => {
                        let lo = v as u64 as i64;
                        let hi = (v >> 64) as u64 as i64;
                        self.emit_mov_imm(Reg::X9, lo, 64);
                        self.emit_mov_imm(Reg::X10, hi, 64);
                    }
                    _ => {
                        self.emit_move(stack_arg.pseudo, Reg::X9, 64);
                        self.push_lir(Aarch64Inst::Mov {
                            size: OperandSize::B64,
                            src: GpOperand::Reg(Reg::Xzr),
                            dst: Reg::X10,
                        });
                    }
                }
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::X9,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset,
                    },
                });
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::X10,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset: offset + 8,
                    },
                });
                offset += 16;
                continue;
            }
            if matches!(stack_arg.kind, StackKind::Complex) {
                // The pseudo holds the *address* of the complex value, so both
                // elements are loaded out of it -- the same dereference
                // setup_complex_arg performs for the register-passed case.
                let typ = stack_arg.typ.expect("complex arg without a type");
                let (fp_size, imag_offset) = complex_fp_info(types, &self.base.target, typ);
                let addr = self.load_complex_arg_address(stack_arg.pseudo);

                for (step, elem_off) in [(0i32, 0i32), (1, imag_offset)].into_iter() {
                    self.push_lir(Aarch64Inst::LdrFp {
                        size: fp_size,
                        dst: VReg::V16,
                        addr: MemAddr::BaseOffset {
                            base: addr,
                            offset: elem_off,
                        },
                    });
                    self.push_lir(Aarch64Inst::StrFp {
                        size: fp_size,
                        src: VReg::V16,
                        addr: MemAddr::BaseOffset {
                            base: Reg::SP,
                            offset: offset + step * imag_offset,
                        },
                    });
                }
                // AAPCS64 rounds each stacked argument up to 8 bytes; the
                // callee's allocator uses the same rule, so the two agree.
                offset += stack_arg.slot_bytes(types, &self.base.target);
                continue;
            }
            if let StackKind::Composite { bytes } = stack_arg.kind {
                // The pseudo locates the aggregate; its bytes go into the slot.
                let src = match self.get_location(stack_arg.pseudo) {
                    Loc::Reg(r) => r,
                    ref loc @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                        // As above: the aggregate may live in either frame,
                        // and `_ => continue` would silently drop it.
                        let (base, disp) = self.loc_addr_parts(loc).unwrap();
                        self.push_lir(Aarch64Inst::Add {
                            size: OperandSize::B64,
                            src1: base,
                            src2: GpOperand::Imm(disp as i64),
                            dst: Reg::X9,
                        });
                        Reg::X9
                    }
                    _ => continue,
                };
                let mut done = 0;
                while done < bytes {
                    let chunk = [8, 4, 2, 1]
                        .into_iter()
                        .find(|c| *c <= bytes - done)
                        .unwrap_or(1);
                    let size = OperandSize::from_bits(chunk as u32 * 8);
                    self.push_lir(Aarch64Inst::Ldr {
                        size,
                        addr: MemAddr::BaseOffset {
                            base: src,
                            offset: done,
                        },
                        dst: Reg::X16,
                    });
                    self.push_lir(Aarch64Inst::Str {
                        size,
                        src: Reg::X16,
                        addr: MemAddr::BaseOffset {
                            base: Reg::SP,
                            offset: offset + done,
                        },
                    });
                    done += chunk;
                }
                offset += stack_arg.slot_bytes(types, &self.base.target);
                continue;
            }
            if let StackKind::Hfa { base, count } = stack_arg.kind {
                // One element at a time through V16, at the base's own
                // stride. The `_Complex` path above writes exactly two
                // elements at the complex stride, which is right for a
                // `_Complex` and for nothing else.
                let typ = stack_arg.typ.expect("HFA arg without a type");
                let elem = HfaElem::of(base);
                for i in 0..count as usize {
                    self.load_hfa_elem(stack_arg.pseudo, typ, elem, i, VReg::V16, types);
                    self.push_lir(Aarch64Inst::StrFp {
                        size: elem.size,
                        src: VReg::V16,
                        addr: MemAddr::BaseOffset {
                            base: Reg::SP,
                            offset: offset + i as i32 * elem.bytes,
                        },
                    });
                }
                offset += stack_arg.slot_bytes(types, &self.base.target);
                continue;
            }
            if stack_arg.is_fp {
                // Use type info for proper FP size determination
                self.emit_fp_move(
                    stack_arg.pseudo,
                    VReg::V16,
                    stack_arg.typ,
                    stack_arg.size,
                    types,
                );
                // Width comes from the type, so a binary128 stack argument
                // keeps its top half rather than taking the 8-byte stride.
                let fp_sz = self.fp_size_from_type(stack_arg.typ, stack_arg.size, types);
                self.push_lir(Aarch64Inst::StrFp {
                    size: fp_sz,
                    src: VReg::V16,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset,
                    },
                });
            } else {
                self.emit_move(stack_arg.pseudo, Reg::X9, stack_arg.size);
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::X9,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset,
                    },
                });
            }
            offset += stack_arg.slot_bytes(types, &self.base.target);
        }

        // Return number of 16-byte units allocated (for cleanup)
        (aligned_bytes + 15) / 16
    }

    /// Set up a complex number argument (real + imaginary in two V registers)
    /// Load the address a complex-argument pseudo holds into a scratch
    /// register, and return that register.
    ///
    /// `Linearizer::complex_operand_addr` makes the pseudo an address, so
    /// every consumer has to dereference it; reading the slot as though it
    /// were the value only appeared to work while the pointer stayed in a
    /// register.
    fn load_complex_arg_address(&mut self, arg: PseudoId) -> Reg {
        match self.get_location(arg) {
            Loc::Reg(r) => r,
            ref l @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    dst: Reg::X9,
                    addr: self.loc_mem(l).unwrap(),
                });
                Reg::X9
            }
            _ => {
                self.emit_move(arg, Reg::X9, 64);
                Reg::X9
            }
        }
    }

    fn setup_complex_arg(
        &mut self,
        arg: PseudoId,
        arg_type: Option<crate::types::TypeId>,
        real_reg: VReg,
        imag_reg: VReg,
        types: &TypeTable,
    ) {
        let arg_loc = self.get_location(arg);
        let (fp_size, imag_offset) = complex_fp_info(types, &self.base.target, arg_type.unwrap());

        match arg_loc {
            ref l @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                // The argument pseudo holds the *address* of the complex value
                // (`Linearizer::complex_operand_addr`), so the slot has to be
                // loaded and then dereferenced. Reading the slot as though it
                // were the value worked only while the pointer happened to
                // stay in a register, and produced garbage the moment it was
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    dst: Reg::X9,
                    addr: self.loc_mem(l).unwrap(),
                });
                self.push_lir(Aarch64Inst::LdrFp {
                    size: fp_size,
                    dst: real_reg,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X9,
                        offset: 0,
                    },
                });
                self.push_lir(Aarch64Inst::LdrFp {
                    size: fp_size,
                    dst: imag_reg,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X9,
                        offset: imag_offset,
                    },
                });
            }
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::LdrFp {
                    size: fp_size,
                    dst: real_reg,
                    addr: MemAddr::BaseOffset { base: r, offset: 0 },
                });
                self.push_lir(Aarch64Inst::LdrFp {
                    size: fp_size,
                    dst: imag_reg,
                    addr: MemAddr::BaseOffset {
                        base: r,
                        offset: imag_offset,
                    },
                });
            }
            _ => {}
        }
    }

    /// Load element `index` of an HFA argument into `dst`.
    ///
    /// The three shapes an HFA argument arrives in. Above a register's worth
    /// the pseudo holds the aggregate's *address*; at or below it the pseudo
    /// holds the packed value, and a stack slot holding that value is already
    /// the aggregate's own bytes while a general register has to be shifted
    /// down an element at a time.
    fn load_hfa_elem(
        &mut self,
        arg: PseudoId,
        typ: TypeId,
        elem: HfaElem,
        index: usize,
        dst: VReg,
        types: &TypeTable,
    ) {
        let arg_loc = self.get_location(arg);
        let fp_size = elem.size;
        let delta = index as i32 * elem.bytes;
        let holds_value = types.size_bits(typ) <= 64;

        match arg_loc {
            ref l @ (Loc::Stack(_) | Loc::IncomingArg(_)) => self.push_lir(Aarch64Inst::LdrFp {
                size: fp_size,
                dst,
                addr: self.loc_mem_plus(l, delta).unwrap(),
            }),
            Loc::Reg(r) if !holds_value => self.push_lir(Aarch64Inst::LdrFp {
                size: fp_size,
                dst,
                addr: MemAddr::BaseOffset {
                    base: r,
                    offset: delta,
                },
            }),
            Loc::Reg(r) => {
                let src = if index == 0 {
                    r
                } else {
                    self.push_lir(Aarch64Inst::Lsr {
                        size: OperandSize::B64,
                        src: r,
                        amount: GpOperand::Imm(delta as i64 * 8),
                        dst: Reg::X9,
                    });
                    Reg::X9
                };
                self.push_lir(Aarch64Inst::FmovFromGp {
                    size: fp_size,
                    src,
                    dst,
                });
            }
            _ => {}
        }
    }

    fn setup_hfa_arg(
        &mut self,
        arg: PseudoId,
        arg_type: Option<crate::types::TypeId>,
        regs: &[VReg],
        types: &TypeTable,
    ) {
        let typ = arg_type.unwrap();
        let abi = crate::abi::get_abi_for_conv(crate::abi::CallingConv::C, &self.base.target);
        let elem = match abi.classify_param(typ, types) {
            ArgClass::Hfa { base, .. } => HfaElem::of(base),
            _ => HfaElem {
                size: FpSize::Double,
                bytes: 8,
            },
        };
        for (i, &dst) in regs.iter().enumerate() {
            self.load_hfa_elem(arg, typ, elem, i, dst, types);
        }
    }

    /// Emit the actual call instruction (direct or indirect)
    pub(super) fn emit_call_instruction(&mut self, insn: &Instruction, func_name: &str) {
        if insn.indirect_target.is_some() {
            self.push_lir(Aarch64Inst::Bl {
                target: CallTarget::Indirect(Reg::X16),
            });
        } else {
            self.push_lir(Aarch64Inst::Bl {
                target: CallTarget::Direct(Symbol::global(func_name)),
            });
        }
    }

    /// Clean up stack after call
    pub(super) fn cleanup_call_stack(&mut self, stack_args: i32) {
        if stack_args > 0 {
            self.push_lir(Aarch64Inst::Add {
                size: OperandSize::B64,
                src1: Reg::sp(),
                src2: GpOperand::Imm((stack_args * 16) as i64),
                dst: Reg::sp(),
            });
        }
    }

    /// Handle call return value using ABI classification.
    pub(super) fn handle_call_return_value(&mut self, insn: &Instruction, types: &TypeTable) {
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        let ret_size = insn
            .typ
            .map(|t| types.size_bits(t).max(32))
            .unwrap_or(insn.size.max(32));

        let abi_info = insn
            .abi_info
            .as_ref()
            .expect("abi_info must be populated for Call instructions");

        match &abi_info.ret {
            ArgClass::Direct { classes, size_bits } => {
                // Two-register return (9-16 bytes)
                if *size_bits > 64 && classes.len() == 2 {
                    if classes.iter().all(|c| *c == RegClass::Integer) {
                        self.handle_two_reg_return(&dst_loc);
                        return;
                    }
                    // Two SSE registers (could be HFA with 2 doubles)
                    if classes.iter().all(|c| *c == RegClass::Sse) {
                        let is_complex_result = insn.typ.is_some_and(|t| types.is_complex(t));
                        if is_complex_result {
                            self.handle_complex_return(insn, &dst_loc, types);
                        } else {
                            self.handle_two_fp_return(&dst_loc);
                        }
                        return;
                    }
                }
                // Single SSE return
                if classes.first() == Some(&RegClass::Sse) {
                    self.emit_fp_move_to_loc(VReg::V0, &dst_loc, insn.typ, ret_size, types);
                    return;
                }
                // Integer return
                self.emit_move_to_loc(Reg::X0, &dst_loc, ret_size);
            }
            ArgClass::Indirect { .. } => {
                // sret: return value already written to memory via X8
            }
            ArgClass::Hfa { count, base } => {
                // HFA: values in V0-V3
                let is_complex_result = insn.typ.is_some_and(|t| types.is_complex(t));
                if *count == 2 && is_complex_result {
                    self.handle_complex_return(insn, &dst_loc, types);
                    return;
                }
                // Handle HFA with 1-4 elements
                self.handle_hfa_return(&dst_loc, *count, *base);
            }
            ArgClass::Extend { .. } => {
                self.emit_move_to_loc(Reg::X0, &dst_loc, ret_size);
            }
            ArgClass::X87 { .. } => {
                unreachable!("x87 FPU returns not available on AArch64");
            }
            ArgClass::Ignore => {
                // Void return, nothing to do
            }
        }
    }

    /// Handle two-register struct return (X0 + X1)
    fn handle_two_reg_return(&mut self, dst_loc: &Loc) {
        match dst_loc {
            Loc::Stack(offset) => {
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::X0,
                    addr: self.stack_mem(*offset),
                });
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::X1,
                    addr: self.stack_mem_plus(*offset, 8),
                });
            }
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::X0,
                    addr: MemAddr::BaseOffset {
                        base: *r,
                        offset: 0,
                    },
                });
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::X1,
                    addr: MemAddr::BaseOffset {
                        base: *r,
                        offset: 8,
                    },
                });
            }
            _ => {}
        }
    }

    /// Handle complex return value (V0 + V1)
    fn handle_complex_return(&mut self, insn: &Instruction, dst_loc: &Loc, types: &TypeTable) {
        let (fp_size, imag_offset) = complex_fp_info(types, &self.base.target, insn.typ.unwrap());
        match dst_loc {
            Loc::Stack(offset) => {
                self.push_lir(Aarch64Inst::StrFp {
                    size: fp_size,
                    src: VReg::V0,
                    addr: self.stack_mem(*offset),
                });
                self.push_lir(Aarch64Inst::StrFp {
                    size: fp_size,
                    src: VReg::V1,
                    addr: self.stack_mem_plus(*offset, imag_offset),
                });
            }
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::StrFp {
                    size: fp_size,
                    src: VReg::V0,
                    addr: MemAddr::BaseOffset {
                        base: *r,
                        offset: 0,
                    },
                });
                self.push_lir(Aarch64Inst::StrFp {
                    size: fp_size,
                    src: VReg::V1,
                    addr: MemAddr::BaseOffset {
                        base: *r,
                        offset: imag_offset,
                    },
                });
            }
            _ => {}
        }
    }

    /// Handle two FP register return (V0 + V1) for non-complex structs
    fn handle_two_fp_return(&mut self, dst_loc: &Loc) {
        match dst_loc {
            Loc::Stack(offset) => {
                self.push_lir(Aarch64Inst::StrFp {
                    size: FpSize::Double,
                    src: VReg::V0,
                    addr: self.stack_mem(*offset),
                });
                self.push_lir(Aarch64Inst::StrFp {
                    size: FpSize::Double,
                    src: VReg::V1,
                    addr: self.stack_mem_plus(*offset, 8),
                });
            }
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::StrFp {
                    size: FpSize::Double,
                    src: VReg::V0,
                    addr: MemAddr::BaseOffset {
                        base: *r,
                        offset: 0,
                    },
                });
                self.push_lir(Aarch64Inst::StrFp {
                    size: FpSize::Double,
                    src: VReg::V1,
                    addr: MemAddr::BaseOffset {
                        base: *r,
                        offset: 8,
                    },
                });
            }
            _ => {}
        }
    }

    /// Handle HFA (Homogeneous Floating-Point Aggregate) return (V0-V3)
    /// Store an HFA return value from V0-V3 into the destination.
    fn handle_hfa_return(&mut self, dst_loc: &Loc, count: u8, base: HfaBase) {
        let (fp_size, elem_size) = match base {
            HfaBase::Float16 => (FpSize::Half, 2),
            HfaBase::Float32 => (FpSize::Single, 4),
            HfaBase::Float64 => (FpSize::Double, 8),
            HfaBase::Float128 => (FpSize::Quad, 16),
        };

        let vregs = [VReg::V0, VReg::V1, VReg::V2, VReg::V3];

        match dst_loc {
            Loc::Stack(offset) => {
                for i in 0..count.min(4) {
                    self.push_lir(Aarch64Inst::StrFp {
                        size: fp_size,
                        src: vregs[i as usize],
                        addr: self.stack_mem_plus(*offset, i as i32 * elem_size),
                    });
                }
            }
            Loc::Reg(r) => {
                for i in 0..count.min(4) {
                    self.push_lir(Aarch64Inst::StrFp {
                        size: fp_size,
                        src: vregs[i as usize],
                        addr: MemAddr::BaseOffset {
                            base: *r,
                            offset: i as i32 * elem_size,
                        },
                    });
                }
            }
            _ => {}
        }
    }
}

/// How one element of an HFA is loaded and how far apart two of them sit.
///
/// The two always travel together -- every HFA site needs both, and spelling
/// them out at each one is how a stacked HFA came to be written at the
/// `_Complex` stride.
#[derive(Clone, Copy)]
pub(super) struct HfaElem {
    pub(super) size: FpSize,
    pub(super) bytes: i32,
}

impl HfaElem {
    pub(super) fn of(base: HfaBase) -> HfaElem {
        let (size, bytes) = match base {
            HfaBase::Float16 => (FpSize::Half, 2),
            HfaBase::Float32 => (FpSize::Single, 4),
            HfaBase::Float64 => (FpSize::Double, 8),
            HfaBase::Float128 => (FpSize::Quad, 16),
        };
        HfaElem { size, bytes }
    }
}
