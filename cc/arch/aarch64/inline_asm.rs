//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 inline assembly: operand substitution, constraint classification
// and the moves that place operands where a constraint demands
//

use crate::arch::aarch64::codegen::Aarch64CodeGen;
use crate::arch::aarch64::legalize::{single_offset_fits, LEGALIZE_REG};
use crate::arch::aarch64::lir::{Aarch64Inst, GpOperand, MemAddr};
use crate::arch::aarch64::regalloc::{parse_gp_clobber_name, Loc, Reg, VReg};
use crate::arch::asm_constraints::{
    canonical_int, AsmAccess, AsmMemClass, AsmOperandClass, AsmRegClass,
};
use crate::arch::codegen::{impossible_constant, AsmOperandValue};
use crate::arch::lir::{Directive, FpSize, OperandSize, Symbol};
use crate::ir::{Instruction, PseudoId};

/// What goes in a scratch register before the template: a memory operand's
/// address, or the value of a register operand the allocator gave no register.
enum OperandSetup {
    /// `reg = base + offset`: the operand *is* this stack object.
    Object { base: Reg, offset: i32 },
    /// `reg = [slot]`: the operand's address was spilled there.
    Spilled(MemAddr),
    /// `reg = &name + offset`.
    Global { name: String, offset: i64 },
    /// `reg = value of pseudo`, wherever it lives.
    Value { pseudo: PseudoId, size: u32 },
    /// `reg = constant`, for a register-only operand whose value is one.
    Imm(i64),
}

/// The scratch registers an asm statement can hand an operand that needs one,
/// and what goes in each.
///
/// X16 and X17 first -- AAPCS64 intra-procedure scratch -- then whichever of
/// the codegen scratches X9-X11 the statement has not already spent, then
/// X15, the legalization register. None is ever allocated, so none holds a
/// live value or another operand. A register the statement names as a
/// clobber is not used: the template would destroy what it holds.
///
/// X15 is lent here under the one rule `legalize.rs` states for it: it is held
/// only from its own setup, emitted last, to the template, and only by an
/// operand whose setup never needs X15 as a temporary of its own. The
/// legalizer's expansions all accept X15 as their destination, so an object's
/// address, a spill-slot load, a plain global's address or a constant is
/// fine. An Initial Exec thread-local's address is not -- it is `mrs x15,
/// tpidr_el0; add dst, x15, dst`, which with the operand in X15 became `add
/// x15, x15, x15` -- so such an operand is never offered X15
/// (`setup_borrows_legalize_reg`). When a statement has one, X15 is offered
/// *first*, to the first operand that may take it, so the one that may not
/// still finds a register among the others; the setup stays last either way.
/// An output never gets it, since its store back after the template may
/// itself need legalizing.
struct OperandRegs {
    clobbered: Vec<Reg>,
    ip: Vec<Reg>,
    legalize_reg_free: bool,
    /// Offer X15 before the others: the statement has an operand that may
    /// not take it, which must be left one that it may.
    legalize_reg_first: bool,
    given: Vec<((PseudoId, i32), Reg)>,
    setups: Vec<(Reg, OperandSetup)>,
}

impl OperandRegs {
    fn new(clobbers: &[String], legalize_reg_first: bool) -> Self {
        let clobbered: Vec<Reg> = clobbers
            .iter()
            .filter_map(|c| parse_gp_clobber_name(c))
            .collect();
        let ip = [Reg::X17, Reg::X16]
            .into_iter()
            .filter(|r| !clobbered.contains(r))
            .collect();
        let legalize_reg_free = !clobbered.contains(&LEGALIZE_REG);
        Self {
            clobbered,
            ip,
            legalize_reg_free,
            legalize_reg_first,
            given: Vec::new(),
            setups: Vec::new(),
        }
    }

    /// The next free scratch register, spending `gp_scratch` -- the
    /// statement's shared codegen-scratch budget -- only after X16/X17, and
    /// X15 last and only when `legalize_reg_ok`.
    fn take(&mut self, gp_scratch: &mut Vec<Reg>, legalize_reg_ok: bool) -> Option<Reg> {
        if legalize_reg_ok && self.legalize_reg_first && self.legalize_reg_free {
            self.legalize_reg_free = false;
            return Some(LEGALIZE_REG);
        }
        if let Some(r) = self.ip.pop() {
            return Some(r);
        }
        while let Some(r) = gp_scratch.pop() {
            if !self.clobbered.contains(&r) {
                return Some(r);
            }
        }
        if legalize_reg_ok {
            return std::mem::take(&mut self.legalize_reg_free).then_some(LEGALIZE_REG);
        }
        None
    }
}

// Inline Assembly Helper Functions

/// Get the 64-bit register name for inline asm
pub(super) fn asm_reg_name_64(reg: Reg) -> &'static str {
    match reg {
        Reg::X0 => "x0",
        Reg::X1 => "x1",
        Reg::X2 => "x2",
        Reg::X3 => "x3",
        Reg::X4 => "x4",
        Reg::X5 => "x5",
        Reg::X6 => "x6",
        Reg::X7 => "x7",
        Reg::X8 => "x8",
        Reg::X9 => "x9",
        Reg::X10 => "x10",
        Reg::X11 => "x11",
        Reg::X12 => "x12",
        Reg::X13 => "x13",
        Reg::X14 => "x14",
        Reg::X15 => "x15",
        Reg::X16 => "x16",
        Reg::X17 => "x17",
        Reg::X19 => "x19",
        Reg::X20 => "x20",
        Reg::X21 => "x21",
        Reg::X22 => "x22",
        Reg::X23 => "x23",
        Reg::X24 => "x24",
        Reg::X25 => "x25",
        Reg::X26 => "x26",
        Reg::X27 => "x27",
        Reg::X28 => "x28",
        Reg::X29 => "x29",
        Reg::X30 => "x30",
        Reg::SP => "sp",
        Reg::Xzr => "xzr",
    }
}

/// Get the 32-bit register name for inline asm
pub(super) fn asm_reg_name_32(reg: Reg) -> &'static str {
    match reg {
        Reg::X0 => "w0",
        Reg::X1 => "w1",
        Reg::X2 => "w2",
        Reg::X3 => "w3",
        Reg::X4 => "w4",
        Reg::X5 => "w5",
        Reg::X6 => "w6",
        Reg::X7 => "w7",
        Reg::X8 => "w8",
        Reg::X9 => "w9",
        Reg::X10 => "w10",
        Reg::X11 => "w11",
        Reg::X12 => "w12",
        Reg::X13 => "w13",
        Reg::X14 => "w14",
        Reg::X15 => "w15",
        Reg::X16 => "w16",
        Reg::X17 => "w17",
        Reg::X19 => "w19",
        Reg::X20 => "w20",
        Reg::X21 => "w21",
        Reg::X22 => "w22",
        Reg::X23 => "w23",
        Reg::X24 => "w24",
        Reg::X25 => "w25",
        Reg::X26 => "w26",
        Reg::X27 => "w27",
        Reg::X28 => "w28",
        Reg::X29 => "w29",
        Reg::X30 => "w30",
        Reg::SP => "wsp",
        Reg::Xzr => "wzr",
    }
}

/// Whether the class asks for a vector (SIMD/FP) register.
fn requires_vector(class: &AsmOperandClass) -> bool {
    class.reg == Some(AsmRegClass::Vector)
}

// AsmOperandFormatter trait implementation

impl Aarch64CodeGen {
    /// Emit inline assembly instruction
    pub(super) fn emit_inline_asm(&mut self, insn: &Instruction) {
        let asm_data = match &insn.extra().asm_data {
            Some(data) => data,
            None => return,
        };

        // Build operand slots for asm substitution. Each `AsmOperandSlot`
        // bundles (reg, mem, size, name) so per-operand pushes can't go
        // out of sync.
        let operand_count = asm_data.outputs.len() + asm_data.inputs.len();
        let mut slots: Vec<crate::arch::AsmOperandSlot<Reg>> = Vec::with_capacity(operand_count);

        // Scratch budgets, shared by the output and input loops so two
        // operands can never be handed the same register. `Reg::scratch_regs`
        // reserves X9/X10/X11 and `VReg::allocatable` reserves V16/V17/V18.
        // Popped from the end so X9 and V16 go first.
        let mut gp_scratch: Vec<Reg> = vec![Reg::X11, Reg::X10, Reg::X9];
        let mut v_scratch: Vec<VReg> = vec![VReg::V18, VReg::V17, VReg::V16];
        // Vector operands copied back out of their scratch after the template.
        let mut vec_output_moves: Vec<(VReg, Loc, u32)> = Vec::new();
        // Vector operands copied into their scratch before it.
        let mut vec_input_moves: Vec<(VReg, Loc, u32)> = Vec::new();
        // Operands that have to be put in a scratch register first.
        let mut addr_regs = OperandRegs::new(
            &asm_data.clobbers,
            asm_data
                .outputs
                .iter()
                .chain(&asm_data.inputs)
                .any(|c| self.names_ie_thread_local(c.pseudo)),
        );
        // Register outputs the allocator gave no register: written by the
        // template into a scratch, stored back after it.
        let mut gp_output_moves: Vec<(Reg, Loc, u32)> = Vec::new();

        // Process output operands (they go first: %0, %1, etc.)
        for output in &asm_data.outputs {
            let loc = self.get_location(output.pseudo);
            let requires_mem = output.is_memory();
            let read_write = output.class.access == AsmAccess::ReadWrite;
            let op_size = output.size;
            let op_name = output.name.clone();
            let mk = |value: AsmOperandValue<Reg>| crate::arch::AsmOperandSlot {
                value,
                size: op_size,
                name: op_name.clone(),
            };
            match loc {
                Loc::Reg(r) if requires_mem => {
                    // Memory-class constraint with the address in a
                    // register: render `[xN]` so `ldr`/`str` see a
                    // valid AAPCS64 memory operand. Without this the
                    // asm template substitutes `wN`/`xN` and the
                    // assembler rejects (`ldr w8, w0` → "expected
                    // label or encodable integer pc offset").
                    slots.push(mk(AsmOperandValue::Mem(format!(
                        "[{}]",
                        asm_reg_name_64(r)
                    ))));
                }
                // A vector-class output. Without this arm a `"=w"` output
                // takes whatever the allocator gave the pseudo -- a general
                // register -- and emits `fmov d17, x0` around a template the
                // assembler rejects.
                _ if requires_vector(&output.class) => match v_scratch.pop() {
                    Some(v) => {
                        slots.push(mk(AsmOperandValue::RegName(
                            Self::vreg_name(v, op_size).to_string(),
                        )));
                        vec_output_moves.push((v, loc.clone(), op_size));
                        if read_write {
                            vec_input_moves.push((v, loc.clone(), op_size));
                        }
                    }
                    None => {
                        crate::diag::error(
                            insn.pos.unwrap_or_default(),
                            "too many vector register constraints in one asm \
                             statement; c17 has three scratch registers to give",
                        );
                        slots.push(mk(AsmOperandValue::RegName(VReg::V16.name_d().to_string())));
                    }
                },
                Loc::Reg(r) => {
                    slots.push(mk(AsmOperandValue::Reg(r)));
                }
                // A register output the allocator gave no register: the
                // template writes a scratch, stored back once it has run.
                // Rendering the slot instead named memory where the template
                // wants a register (`mov [x29, #104], #0`).
                Loc::Stack(_) | Loc::IncomingArg(_) | Loc::Global(_)
                    if !requires_mem && output.class.reg.is_some() =>
                {
                    let Some(reg) = addr_regs.take(&mut gp_scratch, false) else {
                        crate::diag::error(
                            insn.pos.unwrap_or_default(),
                            "too many register operands in one asm statement; \
                             c17 has no register left to give one",
                        );
                        slots.push(mk(AsmOperandValue::Reg(Reg::X9)));
                        continue;
                    };
                    slots.push(mk(AsmOperandValue::Reg(reg)));
                    gp_output_moves.push((reg, loc.clone(), op_size));
                    if read_write {
                        addr_regs.setups.push((
                            reg,
                            OperandSetup::Value {
                                pseudo: output.pseudo,
                                size: op_size,
                            },
                        ));
                    }
                }
                _ if requires_mem => {
                    let mem_str = self.memory_operand(
                        output.pseudo,
                        output.offset,
                        output.class.mem,
                        &loc,
                        op_size,
                        &mut addr_regs,
                        &mut gp_scratch,
                        insn,
                    );
                    slots.push(mk(mem_str));
                }
                _ => {
                    // Memory or other location - emit as memory operand
                    slots.push(mk(self.loc_to_asm_operand(&loc, op_size)));
                }
            }
        }

        let num_outputs = asm_data.outputs.len();

        // Whether V16 has already been spent materializing a floating
        // constant for a vector-class constraint. There is only the one.

        // The hidden inputs of `"+"` outputs, numbered after every explicit
        // input -- see `AsmConstraint::is_hidden_readwrite_input`. Numbering
        // one in place made `%1` in `"add %0, %0, %1" : "+r"(t) : "r"(b)`
        // name `t` again rather than `b`. It needs no setup of its own: its
        // output's slot already holds the value, the address, or the vector
        // move.
        let mut hidden: Vec<usize> = Vec::new();

        // Process input operands
        for input in &asm_data.inputs {
            if let Some(match_idx) = input.matching_output {
                if match_idx < num_outputs && input.is_hidden_readwrite_input() {
                    hidden.push(match_idx);
                    continue;
                }
                // An explicit tied input (`"0"`) names its output's operand:
                // the same register, holding the input's value -- which the
                // linearizer put in the output's own pseudo. When that output
                // was given a scratch, the value has to be loaded into it.
                if match_idx < num_outputs && slots[match_idx].reg().is_some() {
                    let mut slot = slots[match_idx].clone();
                    slot.name = input.name.clone();
                    if let Some(&(reg, _, size)) = gp_output_moves
                        .iter()
                        .find(|(r, _, _)| Some(*r) == slots[match_idx].reg())
                    {
                        let pseudo = asm_data.outputs[match_idx].pseudo;
                        addr_regs
                            .setups
                            .push((reg, OperandSetup::Value { pseudo, size }));
                    }
                    slots.push(slot);
                    continue;
                }
            }
            // Handle matching constraints - use the matched output's location
            let loc = if let Some(match_idx) = input.matching_output {
                if match_idx < num_outputs {
                    self.get_location(asm_data.outputs[match_idx].pseudo)
                } else {
                    self.get_location(input.pseudo)
                }
            } else {
                self.get_location(input.pseudo)
            };
            let class = &input.class;
            let requires_mem = input.is_memory();
            let op_size = input.size;
            let op_name = input.name.clone();
            let mk = |value: AsmOperandValue<Reg>| crate::arch::AsmOperandSlot {
                value,
                size: op_size,
                name: op_name.clone(),
            };
            match loc {
                // A symbolic constant (`"S"`): the global's address, written
                // into the template. The operand names the global's `Sym`,
                // with any offset on it -- see
                // `ir::asm_operand::resolve_immediates`.
                Loc::Global(name) if class.is_immediate_only() => {
                    let sym = if name.starts_with('.') {
                        Symbol::local(name)
                    } else {
                        Symbol::global(name)
                    };
                    let sym = sym.format_for_target(&self.base.target);
                    slots.push(mk(AsmOperandValue::Symbol(
                        crate::arch::codegen::asm_symbol_constant(&sym, input.offset),
                    )));
                }
                Loc::Reg(r) if requires_mem => {
                    // See output-side note: memory-class input with
                    // its address in a register renders as `[xN]`.
                    slots.push(mk(AsmOperandValue::Mem(format!(
                        "[{}]",
                        asm_reg_name_64(r)
                    ))));
                }
                Loc::Reg(r) => {
                    slots.push(mk(AsmOperandValue::Reg(r)));
                }
                // A constant under a register-only constraint still goes in a
                // register: the template may use it where no immediate
                // encodes (`add x0, #100, #100`). So does one no immediate
                // letter's range holds, when a register is offered:
                // `"rI"(5000)`. A class that offers no register at all takes
                // the constant as written, if it fits.
                Loc::Imm(v)
                    if class.reg.is_some() && !class.imm.int.admits(canonical_int(v, op_size)) =>
                {
                    let Some(reg) = addr_regs.take(&mut gp_scratch, true) else {
                        crate::diag::error(
                            insn.pos.unwrap_or_default(),
                            "too many register operands in one asm statement; \
                             c17 has no register left to give one",
                        );
                        slots.push(mk(AsmOperandValue::Reg(Reg::X9)));
                        continue;
                    };
                    addr_regs.setups.push((reg, OperandSetup::Imm(v as i64)));
                    slots.push(mk(AsmOperandValue::Reg(reg)));
                }
                Loc::Imm(v)
                    if !class.imm.int.is_empty()
                        && !class.imm.int.admits(canonical_int(v, op_size)) =>
                {
                    crate::diag::error(
                        insn.pos.unwrap_or_default(),
                        &impossible_constant(slots.len(), v, &input.constraint),
                    );
                    slots.push(mk(AsmOperandValue::Int(0)));
                }
                Loc::Imm(v) => {
                    // Immediate value
                    slots.push(mk(AsmOperandValue::Int(canonical_int(v, op_size))));
                }
                // A floating constant has no address, so a memory-class
                // constraint cannot be satisfied. gcc says the same and stops.
                Loc::FImm(..) if requires_mem => {
                    crate::diag::error(
                        insn.pos.unwrap_or_default(),
                        &format!("memory input {} is not directly addressable", slots.len()),
                    );
                    slots.push(mk(AsmOperandValue::Mem("[sp]".to_string())));
                }
                // A register-class constraint wants the value in a register,
                // and a constant is never allocated one. Materialize it into
                // a scratch: nothing else is live there across the asm.
                Loc::FImm(v, imm_size) if class.reg.is_some() => {
                    let bits = v.to_bits_at_width(imm_size);
                    let Some(scratch) = gp_scratch.pop() else {
                        crate::diag::error(
                            insn.pos.unwrap_or_default(),
                            "too many register constraints in one asm statement; \
                             c17 has three general scratch registers to give",
                        );
                        slots.push(mk(AsmOperandValue::Reg(Reg::X9)));
                        continue;
                    };
                    self.emit_mov_imm(scratch, bits, 64);
                    if requires_vector(class) {
                        // Three scratch V registers are reserved, not one.
                        let Some(vreg) = v_scratch.pop() else {
                            crate::diag::error(
                                insn.pos.unwrap_or_default(),
                                "too many vector register constraints in one asm \
                                 statement; c17 has three scratch registers to give",
                            );
                            slots
                                .push(mk(AsmOperandValue::RegName(VReg::V16.name_d().to_string())));
                            continue;
                        };
                        let fp_size = match imm_size {
                            16 => FpSize::Half,
                            32 => FpSize::Single,
                            _ => FpSize::Double,
                        };
                        self.push_lir(Aarch64Inst::FmovFromGp {
                            size: fp_size,
                            src: scratch,
                            dst: vreg,
                        });
                        // A vector operand is named by its spelling, at the
                        // constant's width; `%s`/`%d`-style modifiers rename
                        // it at theirs.
                        slots.push(mk(AsmOperandValue::RegName(
                            Self::vreg_name(vreg, imm_size).to_string(),
                        )));
                    } else {
                        // A *register* slot, not a pre-rendered name: the
                        // template decides the width it wants, and `%w1`
                        // against a hard-coded `x9` assembled as
                        // `mov w0, x9`.
                        slots.push(mk(AsmOperandValue::Reg(scratch)));
                    }
                }
                // An FP *value* under a general-register constraint. Nothing
                // put it in a general register, and rendering the vector
                // register's name gave `mov x0, d0` -- the assembler reads
                // `d0` as an undefined symbol. Pre-existing, and reachable
                // from any FP variable passed as `"r"`, not just a constant:
                // `-0.0` arrives here rather than as an `FImm` because it is
                // computed as `fneg` of zero.
                // Already in a vector register, and that is what was asked
                // for: name it directly at the operand's width.
                Loc::VReg(v) if requires_vector(class) => {
                    slots.push(mk(AsmOperandValue::RegName(
                        Self::vreg_name(v, op_size).to_string(),
                    )));
                }
                // A vector-class input that is not in a vector register. The
                // output loop's note applies: without this the operand named
                // whatever the allocator gave the pseudo.
                _ if requires_vector(class) => {
                    let Some(vreg) = v_scratch.pop() else {
                        crate::diag::error(
                            insn.pos.unwrap_or_default(),
                            "too many vector register constraints in one asm \
                             statement; c17 has three scratch registers to give",
                        );
                        slots.push(mk(AsmOperandValue::RegName(VReg::V16.name_d().to_string())));
                        continue;
                    };
                    slots.push(mk(AsmOperandValue::RegName(
                        Self::vreg_name(vreg, op_size).to_string(),
                    )));
                    vec_input_moves.push((vreg, loc.clone(), op_size));
                }
                Loc::VReg(v) if !requires_vector(class) => {
                    let Some(scratch) = gp_scratch.pop() else {
                        crate::diag::error(
                            insn.pos.unwrap_or_default(),
                            "too many register constraints in one asm statement; \
                             c17 has three general scratch registers to give",
                        );
                        slots.push(mk(AsmOperandValue::Reg(Reg::X9)));
                        continue;
                    };
                    let fp_size = match op_size {
                        16 => FpSize::Half,
                        32 => FpSize::Single,
                        _ => FpSize::Double,
                    };
                    self.push_lir(Aarch64Inst::FmovToGp {
                        size: fp_size,
                        src: v,
                        dst: scratch,
                    });
                    slots.push(mk(AsmOperandValue::Reg(scratch)));
                }
                // A register input the allocator gave no register: loaded into
                // a scratch before the template.
                Loc::Stack(_) | Loc::IncomingArg(_) | Loc::Global(_)
                    if !requires_mem && class.reg.is_some() =>
                {
                    let setup = OperandSetup::Value {
                        pseudo: input.pseudo,
                        size: op_size,
                    };
                    let x15_ok = !self.setup_borrows_legalize_reg(&setup);
                    let Some(reg) = addr_regs.take(&mut gp_scratch, x15_ok) else {
                        crate::diag::error(
                            insn.pos.unwrap_or_default(),
                            "too many register operands in one asm statement; \
                             c17 has no register left to give one",
                        );
                        slots.push(mk(AsmOperandValue::Reg(Reg::X9)));
                        continue;
                    };
                    addr_regs.setups.push((reg, setup));
                    slots.push(mk(AsmOperandValue::Reg(reg)));
                }
                _ if requires_mem => {
                    // A `+m` input shares its output's pseudo, and so its
                    // address register.
                    let (pseudo, offset) = match input.matching_output {
                        Some(i) if i < num_outputs => {
                            (asm_data.outputs[i].pseudo, asm_data.outputs[i].offset)
                        }
                        _ => (input.pseudo, input.offset),
                    };
                    let mem_str = self.memory_operand(
                        pseudo,
                        offset,
                        class.mem,
                        &loc,
                        op_size,
                        &mut addr_regs,
                        &mut gp_scratch,
                        insn,
                    );
                    slots.push(mk(mem_str));
                }
                _ => {
                    // Memory or other location
                    slots.push(mk(self.loc_to_asm_operand(&loc, op_size)));
                }
            }
        }

        for idx in hidden {
            let slot = slots[idx].clone();
            slots.push(slot);
        }

        // Convert goto_labels from (BasicBlockId, String) to (label_string, label_name)
        let goto_labels_formatted: Vec<(String, String)> = asm_data
            .goto_labels
            .iter()
            .map(|(bb_id, name)| {
                // Through `Label` rather than a second spelling of the same
                // format, so the quoting cannot be missed here.
                let label_str =
                    crate::arch::lir::Label::block(&self.base.current_fn, bb_id.0).name();
                (label_str, name.clone())
            })
            .collect();

        // Load vector operands into their scratch before the template runs.
        for (vreg, loc, size) in &vec_input_moves {
            self.emit_vec_load_from_loc(*vreg, loc, *size, insn.pos);
        }

        // Scratch operands, last: nothing may run between these and the
        // template, since the legalizer writes X15 expanding anything.
        let mut setups = std::mem::take(&mut addr_regs.setups);
        setups.sort_by_key(|(reg, _)| *reg == LEGALIZE_REG);
        for (reg, setup) in setups {
            self.emit_operand_setup(reg, setup);
        }

        let asm_output = match crate::arch::substitute_asm_operands(
            self,
            &asm_data.template,
            &slots,
            &goto_labels_formatted,
        ) {
            Ok(text) => text,
            Err(msg) => {
                crate::diag::error(insn.pos.unwrap_or_default(), &msg);
                return;
            }
        };

        // Emit the inline assembly as raw text
        // Split by newlines and emit each line
        for line in asm_output.lines() {
            let trimmed = line.trim();
            if !trimmed.is_empty() {
                self.push_lir(Aarch64Inst::Directive(Directive::Raw(trimmed.to_string())));
            }
        }

        // Register outputs held in a scratch go back where they live. Only on
        // the fall-through: a jump to an `asm goto` label skips these, so
        // such a statement must not need any.
        if !gp_output_moves.is_empty() && !asm_data.goto_labels.is_empty() {
            crate::diag::error(
                insn.pos.unwrap_or_default(),
                "an asm goto output needs a register of its own, and this statement \
                 has more register operands than c17 can give them",
            );
        }
        for (reg, loc, size) in &gp_output_moves {
            self.emit_move_to_loc(*reg, loc, *size);
        }

        // Copy vector outputs out of their scratch into where the operand
        // actually lives.
        for (vreg, loc, size) in &vec_output_moves {
            self.emit_vec_store_to_loc(*vreg, loc, *size, insn.pos);
        }

        // Handle clobbers - for now just emit comments for documentation
        // Our simple codegen doesn't do sophisticated register allocation across asm
        for clobber in &asm_data.clobbers {
            match clobber.as_str() {
                "memory" => {
                    // Memory clobber - acts as compiler memory barrier
                    // Our codegen doesn't reorder loads/stores, so this is mostly informational
                }
                "cc" => {
                    // Condition codes clobbered - informational for our simple codegen
                }
                _ => {
                    // Register clobber - could save/restore if needed
                    // For now, trust that the register allocator has handled this
                }
            }
        }
    }

    /// The text of a memory-class operand whose address is not already in a
    /// register.
    ///
    /// The operand's pseudo is the lvalue's *address*. When the allocator
    /// spilled it, the slot holds that address, so rendering the slot as
    /// `[x29, #N]` handed the template the spilled pointer rather than the
    /// object -- a silent wrong answer. It is loaded into a register instead.
    /// When the pseudo is a stack object itself, its slot is the operand, and
    /// is rendered in place if the template's access can encode the offset:
    /// the operand's own size decides the scaled range, and an operand of no
    /// natural access size gets only the unscaled one -- and a `Q` operand,
    /// which encodes a base register alone, no offset at all. Otherwise the
    /// object's address goes in a register.
    #[allow(clippy::too_many_arguments)]
    fn memory_operand(
        &mut self,
        pseudo: PseudoId,
        object_offset: i64,
        mem: Option<AsmMemClass>,
        loc: &Loc,
        size_bits: u32,
        regs: &mut OperandRegs,
        gp_scratch: &mut Vec<Reg>,
        insn: &Instruction,
    ) -> AsmOperandValue<Reg> {
        let Ok(object_offset) = i32::try_from(object_offset) else {
            crate::diag::error(
                insn.pos.unwrap_or_default(),
                "an asm memory operand lies past the displacement an instruction can hold",
            );
            return AsmOperandValue::Mem("[sp]".to_string());
        };
        // Two members of one object share its `Sym` and differ in offset, so
        // an address register is reused only for the same pseudo *and* offset.
        let key = (pseudo, object_offset);
        if let Some(&(_, reg)) = regs.given.iter().find(|(k, _)| *k == key) {
            return AsmOperandValue::Mem(format!("[{}]", asm_reg_name_64(reg)));
        }
        let setup = match loc {
            Loc::Stack(_) | Loc::IncomingArg(_) if self.pseudos.is_sym(pseudo) => {
                let (base, offset) = self.loc_addr_parts(loc).unwrap();
                let offset = offset + object_offset;
                let bytes = i64::from(size_bits / 8);
                let fits = if mem == Some(AsmMemClass::BaseOnly) {
                    offset == 0
                } else if matches!(bytes, 1 | 2 | 4 | 8 | 16) {
                    single_offset_fits(offset.into(), bytes)
                } else {
                    (-256..=255).contains(&offset)
                };
                if fits {
                    return AsmOperandValue::Mem(if offset == 0 {
                        format!("[{}]", asm_reg_name_64(base))
                    } else {
                        format!("[{}, #{}]", asm_reg_name_64(base), offset)
                    });
                }
                OperandSetup::Object { base, offset }
            }
            Loc::Stack(_) | Loc::IncomingArg(_) => {
                OperandSetup::Spilled(self.loc_mem(loc).unwrap())
            }
            Loc::Global(name) => OperandSetup::Global {
                name: name.clone(),
                offset: object_offset.into(),
            },
            other => return self.loc_to_asm_operand(other, size_bits),
        };
        let x15_ok = !self.setup_borrows_legalize_reg(&setup);
        let Some(reg) = regs.take(gp_scratch, x15_ok) else {
            crate::diag::error(
                insn.pos.unwrap_or_default(),
                "too many memory operands in one asm statement need their address \
                 in a register; c17 has six scratch registers to give, fewer if \
                 the statement uses or clobbers them",
            );
            return AsmOperandValue::Mem("[sp]".to_string());
        };
        regs.given.push((key, reg));
        regs.setups.push((reg, setup));
        AsmOperandValue::Mem(format!("[{}]", asm_reg_name_64(reg)))
    }

    /// Whether putting `setup` in a register would also need X15 as a
    /// temporary: the Initial Exec sequence for a thread-local's address,
    /// whether the operand is that address or the thread-local's value. See
    /// [`OperandRegs`].
    fn setup_borrows_legalize_reg(&self, setup: &OperandSetup) -> bool {
        match setup {
            OperandSetup::Global { name, .. } => self.is_ie_thread_local(name),
            OperandSetup::Value { pseudo, .. } => self.names_ie_thread_local(*pseudo),
            _ => false,
        }
    }

    /// Whether `pseudo` lives in an Initial Exec thread-local, whose address
    /// takes X15 as a temporary to compute.
    fn names_ie_thread_local(&self, pseudo: PseudoId) -> bool {
        matches!(self.get_location(pseudo), Loc::Global(name) if self.is_ie_thread_local(&name))
    }

    fn is_ie_thread_local(&self, name: &str) -> bool {
        self.is_elf_tls(name) && self.use_tls_ie(name)
    }

    /// Put an operand's address or value in `reg`.
    fn emit_operand_setup(&mut self, reg: Reg, setup: OperandSetup) {
        match setup {
            OperandSetup::Object { base, offset } => self.push_lir(Aarch64Inst::Add {
                size: OperandSize::B64,
                src1: base,
                src2: GpOperand::Imm(offset.into()),
                dst: reg,
            }),
            OperandSetup::Spilled(addr) => self.push_lir(Aarch64Inst::Ldr {
                size: OperandSize::B64,
                addr,
                dst: reg,
            }),
            OperandSetup::Global { name, offset } => {
                self.emit_load_addr(&name, reg);
                if offset != 0 {
                    self.push_lir(Aarch64Inst::Add {
                        size: OperandSize::B64,
                        src1: reg,
                        src2: GpOperand::Imm(offset),
                        dst: reg,
                    });
                }
            }
            OperandSetup::Value { pseudo, size } => self.emit_move(pseudo, reg, size.max(32)),
            OperandSetup::Imm(v) => self.emit_mov_imm(reg, v, 64),
        }
    }

    /// Move an operand's value into a scratch vector register.
    fn emit_vec_load_from_loc(
        &mut self,
        dst: VReg,
        loc: &Loc,
        size: u32,
        pos: Option<crate::diag::Position>,
    ) {
        let fp_size = match size {
            16 => FpSize::Half,
            32 => FpSize::Single,
            _ => FpSize::Double,
        };
        match loc {
            Loc::VReg(src) => {
                if *src != dst {
                    self.push_lir(Aarch64Inst::FmovReg {
                        size: fp_size,
                        src: *src,
                        dst,
                    });
                }
            }
            Loc::Reg(src) => self.push_lir(Aarch64Inst::FmovFromGp {
                size: fp_size,
                src: *src,
                dst,
            }),
            Loc::Stack(off) => {
                let addr = self.stack_mem(*off);
                self.push_lir(Aarch64Inst::LdrFp {
                    size: fp_size,
                    addr,
                    dst,
                });
            }
            _ => crate::diag::error(
                pos.unwrap_or_default(),
                "a vector asm operand cannot be read from this location",
            ),
        }
    }

    /// Move a scratch vector register's value back to where the operand lives.
    fn emit_vec_store_to_loc(
        &mut self,
        src: VReg,
        loc: &Loc,
        size: u32,
        pos: Option<crate::diag::Position>,
    ) {
        let fp_size = match size {
            16 => FpSize::Half,
            32 => FpSize::Single,
            _ => FpSize::Double,
        };
        match loc {
            Loc::VReg(dst) => {
                if *dst != src {
                    self.push_lir(Aarch64Inst::FmovReg {
                        size: fp_size,
                        src,
                        dst: *dst,
                    });
                }
            }
            Loc::Reg(dst) => self.push_lir(Aarch64Inst::FmovToGp {
                size: fp_size,
                src,
                dst: *dst,
            }),
            Loc::Stack(off) => {
                let addr = self.stack_mem(*off);
                self.push_lir(Aarch64Inst::StrFp {
                    size: fp_size,
                    src,
                    addr,
                });
            }
            _ => crate::diag::error(
                pos.unwrap_or_default(),
                "a vector asm output cannot be written back to this location",
            ),
        }
    }

    /// The vector register spelling an operand of `size_bits` wants.
    ///
    /// AArch64 names the same register `b`/`h`/`s`/`d`/`q` by the width being
    /// operated on, and an instruction that wants `d0` rejects `v0`.
    fn vreg_name(v: VReg, size_bits: u32) -> &'static str {
        match size_bits {
            0..=8 => v.name_b(),
            16 => v.name_h(),
            32 => v.name_s(),
            64 => v.name_d(),
            _ => v.name_q(),
        }
    }

    /// A location as the template substitutes it, for an operand of
    /// `size_bits`.
    fn loc_to_asm_operand(&self, loc: &Loc, size_bits: u32) -> AsmOperandValue<Reg> {
        match loc {
            Loc::Reg(r) => AsmOperandValue::Reg(*r),
            loc @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                // AArch64 addresses a local from whichever base the frame uses
                let (base, actual) = self.loc_addr_parts(loc).unwrap();
                AsmOperandValue::Mem(format!("[{}, #{}]", asm_reg_name_64(base), actual))
            }
            Loc::Imm(v) => AsmOperandValue::Int(canonical_int(*v, size_bits)),
            Loc::VReg(vreg) => AsmOperandValue::RegName(vreg.name_d().to_string()),
            // An immediate-class constraint takes the constant itself. The
            // register and memory classes never reach here; `emit_inline_asm`
            // materializes or diagnoses them first.
            Loc::FImm(v, fp_size) => AsmOperandValue::Float(*v, *fp_size),
            Loc::Global(name) => AsmOperandValue::Mem(name.clone()),
        }
    }
}
