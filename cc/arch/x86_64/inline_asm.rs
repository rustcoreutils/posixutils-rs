//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 inline assembly: operand substitution, constraint classification
// and the moves that place operands where a constraint demands
//

use crate::arch::lir::{Directive, FpSize};
use crate::arch::x86_64::codegen::X86_64CodeGen;
use crate::arch::x86_64::lir::X86Inst;
use crate::arch::x86_64::regalloc::{Loc, Reg, XmmReg};
use crate::ir::{AsmData, Instruction, PseudoId};
use crate::target::Os;

/// Everything the two operand-building passes accumulate before any code is
/// emitted, bundled because both passes touch nearly all of it: as separate
/// locals they were fourteen of them, and threading those through a helper
/// meant a fourteen-parameter signature.
struct AsmOperandBuild {
    slots: Vec<crate::arch::AsmOperandSlot<Reg>>,
    /// Outputs to move out of a specific register once the template has run:
    /// (output index, specific register, actual location, size in bits).
    output_moves: Vec<(usize, Reg, Loc, u32)>,
    /// Inputs to move into a specific register before the template runs:
    /// (specific register, actual location, size in bits).
    input_moves: Vec<(Reg, Loc, u32)>,
    /// Allocated registers that collided with a reserved one, and the temp
    /// standing in for them: (original, temp, location to restore, size).
    remap_setup: Vec<(Reg, Reg, Loc, u32)>,
    remap_restore: Vec<(Reg, Reg, Loc, u32)>,
    /// Pseudos already given a temp register, so a `"+r"` operand whose input
    /// and output share a pseudo reuses one rather than taking two.
    pseudo_to_temp: std::collections::HashMap<PseudoId, Reg>,
    used_regs: std::collections::HashSet<Reg>,
    /// The XMM registers this statement may spend on SSE-class operands,
    /// most-preferred last so `pop` hands them out in order.  Shared by both
    /// passes so an in-and-out pair cannot hand the same register to both.
    sse_scratch: Vec<XmmReg>,
    /// SSE outputs to copy back once the template has run:
    /// (scratch register, destination, operand size in bits).
    sse_output_moves: Vec<(XmmReg, Loc, u32)>,
    /// SSE read-write (`"+x"`) operands whose current value must reach the
    /// scratch before the template runs: (scratch, source pseudo, size).
    sse_input_moves: Vec<(XmmReg, PseudoId, u32)>,
    /// Pseudos already given an SSE scratch, so a tied `"+x"` input reuses its
    /// output's register rather than taking a second one.
    pseudo_to_xmm: std::collections::HashMap<PseudoId, XmmReg>,
    /// x87 operands live on the FP stack rather than in a register, so they
    /// are not given one: each is pushed with `fldt` before the template and
    /// the result popped back with `fstpt` after, and the template names them
    /// `%st` and `%st(1)`.
    ///
    /// `t` is st(0) and `u` is st(1), so `u` has to be pushed first for `t` to
    /// end up on top.  Collected in operand order and reversed at emit time.
    x87_pushes: Vec<(PseudoId, u32)>,
    /// Set only when there is an output to write back to, so a template that
    /// consumes its operand (`fistpl` on a `"t"` input) leaves nothing to
    /// store.  A pure `"=t"` output is not pushed -- the template supplies the
    /// value, as `fldz` does.
    x87_store: Option<(PseudoId, u32)>,
    x87_slots: usize,
}

impl AsmOperandBuild {
    fn new(asm_data: &AsmData, reserved_regs: &std::collections::HashSet<Reg>) -> Self {
        let operand_count = asm_data.outputs.len() + asm_data.inputs.len();
        Self {
            slots: Vec::with_capacity(operand_count),
            output_moves: Vec::with_capacity(asm_data.outputs.len()),
            input_moves: Vec::with_capacity(asm_data.inputs.len()),
            remap_setup: Vec::with_capacity(operand_count),
            remap_restore: Vec::with_capacity(operand_count),
            pseudo_to_temp: std::collections::HashMap::new(),
            used_regs: reserved_regs.clone(),
            // Xmm15 is the primary scratch and Xmm14 the secondary, the same
            // pair `float.rs` uses for its own scratch needs.
            sse_scratch: vec![XmmReg::Xmm14, XmmReg::Xmm15],
            sse_output_moves: Vec::new(),
            sse_input_moves: Vec::new(),
            pseudo_to_xmm: std::collections::HashMap::new(),
            x87_pushes: Vec::new(),
            x87_store: None,
            x87_slots: 0,
        }
    }
}

/// A temp register that is neither reserved by a constraint nor already spent.
/// R10 and R11 come first: caller-saved, and rarely used for arguments.
fn find_temp_reg(
    reserved: &std::collections::HashSet<Reg>,
    used: &std::collections::HashSet<Reg>,
) -> Reg {
    for r in [Reg::R10, Reg::R11, Reg::R8, Reg::R9, Reg::Rsi, Reg::Rdi] {
        if !reserved.contains(&r) && !used.contains(&r) {
            return r;
        }
    }
    Reg::R10 // Fallback
}

impl X86_64CodeGen {
    /// Emit inline assembly
    pub(super) fn emit_inline_asm(&mut self, insn: &Instruction) {
        let asm_data = match &insn.asm_data {
            Some(data) => data.as_ref(),
            None => return,
        };

        let reserved_regs = Self::collect_reserved_regs(asm_data);
        let mut build = AsmOperandBuild::new(asm_data, &reserved_regs);
        self.build_output_slots(insn, asm_data, &reserved_regs, &mut build);
        self.build_input_slots(insn, asm_data, &reserved_regs, &mut build);
        self.emit_asm_prologue_moves(&mut build);
        self.emit_asm_template(asm_data, &build);
        self.emit_asm_epilogue_moves(insn, asm_data, &build);
    }

    /// Collect the registers named by specific constraints (a, b, c, d, S, D).
    /// Nothing else may be allocated to them for the duration of the statement.
    fn collect_reserved_regs(asm_data: &AsmData) -> std::collections::HashSet<Reg> {
        let mut reserved_regs: std::collections::HashSet<Reg> = std::collections::HashSet::new();
        for output in &asm_data.outputs {
            if let Some(r) = Self::constraint_to_specific_reg(&output.constraint) {
                reserved_regs.insert(r);
            }
        }
        for input in &asm_data.inputs {
            if let Some(r) = Self::constraint_to_specific_reg(&input.constraint) {
                reserved_regs.insert(r);
            }
        }
        reserved_regs
    }

    /// Build the substitution slot for each output operand, recording the
    /// moves that have to run after the template.
    fn build_output_slots(
        &mut self,
        insn: &Instruction,
        asm_data: &AsmData,
        reserved_regs: &std::collections::HashSet<Reg>,
        build: &mut AsmOperandBuild,
    ) {
        let AsmOperandBuild {
            slots,
            output_moves,
            remap_restore,
            pseudo_to_temp,
            used_regs,
            sse_scratch,
            sse_output_moves,
            x87_pushes,
            x87_store,
            x87_slots,
            pseudo_to_xmm,
            ..
        } = build;
        // Process output operands (they go first: %0, %1, etc.)
        for (idx, output) in asm_data.outputs.iter().enumerate() {
            let loc = self.get_location(output.pseudo);
            let op_size = output.size;
            let op_name = output.name.clone();
            // Helper to build the slot with the shared per-operand
            // context already filled in. Each branch supplies the
            // (reg, mem) pair appropriate for its outcome.
            let mk = |reg: Option<Reg>, mem: Option<String>| crate::arch::AsmOperandSlot {
                reg,
                mem,
                size: op_size,
                name: op_name.clone(),
            };

            // Check for specific register constraint
            if let Some(specific_reg) = Self::constraint_to_specific_reg(&output.constraint) {
                // Output goes to specific register, then we'll move to actual loc after asm
                slots.push(mk(Some(specific_reg), None));
                // Only need to move if actual loc is different from specific reg
                if loc != Loc::Reg(specific_reg) {
                    output_moves.push((idx, specific_reg, loc, op_size));
                }
            } else {
                let requires_reg = Self::constraint_requires_register(&output.constraint);
                let requires_mem = Self::constraint_requires_memory(&output.constraint);
                // No specific register - use allocated location
                match loc {
                    Loc::Reg(r) if requires_mem => {
                        // Memory-class output (e.g. `"=m"(*p)`/`"+m"(*p)`):
                        // the pseudo holds the ADDRESS of the lvalue
                        // (set up by the linearizer's `is_memory`
                        // branch). Render as indirect `(%rN)` so the
                        // asm modifies the memory directly. Without
                        // this guard the template substitutes `%eax`
                        // and `addl $1, %0` becomes `addl $1, %eax`,
                        // incrementing the address bits instead of
                        // the value at that address.
                        let mem_str = format!("(%{})", self.reg_name_64(r));
                        slots.push(mk(None, Some(mem_str)));
                    }
                    // An x87-class output. Written back off the FP stack once
                    // the template has run; a read-write `"+t"` is also pushed
                    // before it, while a pure `"=t"` takes its value from the
                    // template.
                    _ if Self::constraint_requires_x87(&output.constraint) => {
                        let name = Self::x87_slot_name(*x87_slots);
                        *x87_slots += 1;
                        // The result is written back through the operand's
                        // own stack slot. A pseudo the allocator gave no slot
                        // -- which is what a bare `"=t"` local gets, since
                        // nothing else in the function forces one -- is
                        // refused rather than guessed at: addressing it
                        // through an uninitialised register is how this
                        // segfaulted while being developed.
                        if x87_store.is_some() {
                            // `x87_store` holds one operand. A second output
                            // would overwrite it and the first result would be
                            // dropped on the floor, with the stack depth no
                            // longer matching what the template left.
                            crate::diag::error(
                                insn.pos.unwrap_or_default(),
                                "only one x87 asm output is supported in one asm \
                                 statement",
                            );
                        } else if matches!(loc, Loc::Stack(_)) {
                            if output.constraint.contains('+') {
                                x87_pushes.push((output.pseudo, op_size));
                            }
                            *x87_store = Some((output.pseudo, op_size));
                        } else {
                            crate::diag::error(
                                insn.pos.unwrap_or_default(),
                                "an x87 asm output must be an object with storage; \
                                 c17 cannot give a write-only x87 operand a home",
                            );
                        }
                        slots.push(mk(None, Some(name)));
                    }
                    _ if Self::constraint_requires_sse(&output.constraint) => {
                        match sse_scratch.pop() {
                            Some(xmm) => {
                                slots.push(mk(None, Some(xmm.name().to_string())));
                                pseudo_to_xmm.insert(output.pseudo, xmm);
                                // Copied back once the template has run. A
                                // `Loc::Xmm` destination would already be the
                                // right place, but the pseudo is only ever
                                // given one by accident today, so the move is
                                // unconditional and `emit_fp_move_from_xmm`
                                // makes a same-register copy a no-op.
                                sse_output_moves.push((xmm, loc.clone(), op_size));
                            }
                            None => {
                                crate::diag::error(
                                    insn.pos.unwrap_or_default(),
                                    "too many SSE register constraints in one asm \
                                     statement; c17 has two scratch registers to \
                                     give",
                                );
                                slots.push(mk(None, Some(XmmReg::Xmm15.name().to_string())));
                            }
                        }
                    }
                    Loc::Reg(r) => {
                        // Check if allocated reg conflicts with reserved
                        if reserved_regs.contains(&r) {
                            // Use a temp register instead
                            let temp = find_temp_reg(reserved_regs, used_regs);
                            used_regs.insert(temp);
                            slots.push(mk(Some(temp), None));
                            // For outputs, move from temp to actual loc after asm
                            remap_restore.push((temp, r, loc.clone(), op_size));
                            // Track this pseudo -> temp mapping for +r inputs
                            pseudo_to_temp.insert(output.pseudo, temp);
                        } else {
                            slots.push(mk(Some(r), None));
                            used_regs.insert(r);
                        }
                    }
                    Loc::Imm(_) if requires_reg => {
                        // Constant-propagated value used as asm output.
                        // Allocate temp register; after asm, the register holds the
                        // modified value — update the location map directly.
                        let temp = find_temp_reg(reserved_regs, used_regs);
                        used_regs.insert(temp);
                        slots.push(mk(Some(temp), None));
                        // Don't add to output_moves (can't store to Imm).
                        // Instead, update the pseudo's location to the temp reg after asm.
                        self.locations.set(output.pseudo, Loc::Reg(temp));
                        pseudo_to_temp.insert(output.pseudo, temp);
                    }
                    _ if requires_reg => {
                        // Constraint requires register but value is on stack/memory.
                        // Allocate a temp register; move from temp to actual loc after asm.
                        let temp = find_temp_reg(reserved_regs, used_regs);
                        used_regs.insert(temp);
                        slots.push(mk(Some(temp), None));
                        output_moves.push((idx, temp, loc.clone(), op_size));
                        pseudo_to_temp.insert(output.pseudo, temp);
                    }
                    _ => {
                        // Memory or other location - emit as memory operand
                        let mem_str = self.loc_to_asm_string(&loc);
                        slots.push(mk(None, Some(mem_str)));
                    }
                }
            }
        }
    }

    /// Build the substitution slot for each input operand, recording the moves
    /// that have to run before the template.
    fn build_input_slots(
        &mut self,
        insn: &Instruction,
        asm_data: &AsmData,
        reserved_regs: &std::collections::HashSet<Reg>,
        build: &mut AsmOperandBuild,
    ) {
        let AsmOperandBuild {
            slots,
            input_moves,
            remap_setup,
            pseudo_to_temp,
            used_regs,
            sse_scratch,
            sse_input_moves,
            x87_pushes,
            x87_slots,
            pseudo_to_xmm,
            ..
        } = build;
        let num_outputs = asm_data.outputs.len();
        // Process input operands
        for input in &asm_data.inputs {
            let op_size = input.size;

            // Handle matching constraints - use the matched output's location/register
            let (loc, constraint_for_reg) = if let Some(match_idx) = input.matching_output {
                if match_idx < num_outputs {
                    // Use the same register/location as the matched output
                    (
                        self.get_location(asm_data.outputs[match_idx].pseudo),
                        &asm_data.outputs[match_idx].constraint,
                    )
                } else {
                    (self.get_location(input.pseudo), &input.constraint)
                }
            } else {
                (self.get_location(input.pseudo), &input.constraint)
            };

            // Matching inputs from '+' constraints share the output's operand number
            // (GCC: "+r" counts as two operands but uses one %N number).
            // We DON'T push a new operand slot — the output's slot is reused.
            // But we DO need to load the initial value into the output's register.
            if let Some(match_idx) = input.matching_output {
                if match_idx < num_outputs {
                    if let Some(reg) = slots[match_idx].reg {
                        // Load initial value into the register before asm
                        input_moves.push((reg, loc, op_size));
                    } else if let Some(&xmm) =
                        pseudo_to_xmm.get(&asm_data.outputs[match_idx].pseudo)
                    {
                        // An SSE `"+x"` operand. Its slot carries text rather
                        // than a register, so the branch above finds nothing
                        // and the initial value never reached the scratch:
                        // `addsd %xmm15, %xmm15` ran on whatever was there.
                        sse_input_moves.push((xmm, asm_data.outputs[match_idx].pseudo, op_size));
                    }
                    continue; // Skip — don't add a new operand slot
                }
            }

            let op_name = input.name.clone();
            let mk = |reg: Option<Reg>, mem: Option<String>| crate::arch::AsmOperandSlot {
                reg,
                mem,
                size: op_size,
                name: op_name.clone(),
            };

            // Check for specific register constraint
            if let Some(specific_reg) = Self::constraint_to_specific_reg(constraint_for_reg) {
                // Input must go to specific register
                slots.push(mk(Some(specific_reg), None));
                // Only need to move if actual loc is different from specific reg
                if loc != Loc::Reg(specific_reg) {
                    input_moves.push((specific_reg, loc, op_size));
                }
            } else {
                // Check if this input shares a pseudo with an output that was remapped
                // This happens with +r constraints where input and output share the same pseudo
                if let Some(&temp) = pseudo_to_temp.get(&input.pseudo) {
                    // Reuse the same temp register as the output
                    slots.push(mk(Some(temp), None));
                    // Add setup move to load value into temp
                    remap_setup.push((temp, temp, loc.clone(), op_size));
                } else {
                    let requires_reg = Self::constraint_requires_register(constraint_for_reg);
                    let requires_mem = Self::constraint_requires_memory(constraint_for_reg);
                    // No specific register - use allocated location
                    match loc {
                        // An x87-class input: pushed onto the FP stack before the
                        // template, which then names it `%st`/`%st(1)`. Without
                        // this arm an x87 input fell through to the general path
                        // and the template ran on whatever happened to be on the
                        // stack -- `__asm__("fmulp" : "+t"(a) : "u"(b))` answered
                        // -nan.
                        _ if Self::constraint_requires_x87(constraint_for_reg) => {
                            let name = Self::x87_slot_name(*x87_slots);
                            *x87_slots += 1;
                            if Self::x87_addressable(&loc) {
                                x87_pushes.push((input.pseudo, input.size));
                            } else {
                                crate::diag::error(
                                    insn.pos.unwrap_or_default(),
                                    "an x87 asm operand must live somewhere addressable; \
                                     c17 cannot spill one here",
                                );
                            }
                            slots.push(mk(None, Some(name)));
                        }
                        Loc::FImm(..) if requires_mem => {
                            crate::diag::error(
                                insn.pos.unwrap_or_default(),
                                &format!(
                                    "memory input {} is not directly addressable",
                                    slots.len()
                                ),
                            );
                            slots.push(mk(None, Some("(%rip)".to_string())));
                        }
                        // An SSE-class constraint wants the value in an XMM
                        // register, and a constant is never allocated one.
                        // Materialize it into the reserved scratch: nothing
                        // else is live there across the asm.
                        Loc::FImm(v, imm_size)
                            if Self::constraint_requires_sse(constraint_for_reg) =>
                        {
                            // Only the scratch registers are free across the
                            // asm body, and there are two. Say so rather than
                            // hand the same one to two operands and emit wrong
                            // code; naming a variable instead always works.
                            match sse_scratch.pop() {
                                Some(xmm) => {
                                    self.emit_fp_imm_to_xmm(v, xmm, imm_size);
                                    slots.push(mk(None, Some(xmm.name().to_string())));
                                }
                                None => {
                                    crate::diag::error(
                                        insn.pos.unwrap_or_default(),
                                        "too many SSE register constraints in one asm \
                                         statement; c17 has two scratch registers to \
                                         give",
                                    );
                                    slots.push(mk(None, Some(XmmReg::Xmm15.name().to_string())));
                                }
                            }
                        }
                        Loc::Imm(_) if requires_mem => {
                            // Memory constraint with constant address (may be dead code
                            // from unoptimized switch on constant ORDER in atomic macros).
                            // Load address into temp reg and emit as indirect memory ref.
                            let temp = find_temp_reg(reserved_regs, used_regs);
                            used_regs.insert(temp);
                            input_moves.push((temp, loc.clone(), op_size));
                            let mem_str = format!("(%{})", self.reg_name_64(temp));
                            slots.push(mk(None, Some(mem_str)));
                        }
                        Loc::Reg(r) if requires_mem => {
                            // Memory constraint with value in register — emit as indirect
                            let mem_str = format!("(%{})", self.reg_name_64(r));
                            slots.push(mk(None, Some(mem_str)));
                        }
                        Loc::Reg(r) => {
                            // Check if allocated reg conflicts with reserved
                            if reserved_regs.contains(&r) {
                                // Use a temp register instead
                                let temp = find_temp_reg(reserved_regs, used_regs);
                                used_regs.insert(temp);
                                slots.push(mk(Some(temp), None));
                                // For inputs, move from actual loc to temp before asm
                                remap_setup.push((r, temp, loc.clone(), op_size));
                            } else {
                                slots.push(mk(Some(r), None));
                                used_regs.insert(r);
                            }
                        }
                        Loc::Imm(v) => {
                            // Immediate value
                            slots.push(mk(None, Some(format!("${}", v as i64))));
                        }
                        _ if requires_reg => {
                            // Constraint requires register but value is on stack/memory.
                            // Allocate a temp register and load value before asm.
                            let temp = find_temp_reg(reserved_regs, used_regs);
                            used_regs.insert(temp);
                            slots.push(mk(Some(temp), None));
                            input_moves.push((temp, loc.clone(), op_size));
                        }
                        _ => {
                            // Memory or other location
                            let mem_str = self.loc_to_asm_string(&loc);
                            slots.push(mk(None, Some(mem_str)));
                        }
                    }
                }
            }
        }
    }

    /// Everything that must be in place before the template runs.
    fn emit_asm_prologue_moves(&mut self, build: &mut AsmOperandBuild) {
        let AsmOperandBuild {
            input_moves,
            remap_setup,
            sse_input_moves,
            x87_pushes,
            ..
        } = build;
        let x87_pushes = std::mem::take(x87_pushes);
        // Emit remap setup moves (for inputs that conflicted with reserved regs)
        for (_orig, temp, actual_loc, size) in remap_setup.iter() {
            self.emit_raw_mov_from_loc(actual_loc, *temp, *size);
        }

        // Emit moves from actual locations to specific registers (for inputs)
        // Load `"+x"` operands into their scratch before the template runs.
        for (xmm, pseudo, size) in sse_input_moves.iter() {
            let fp_size = FpSize::from_bits(*size, &self.base.target);
            self.emit_fp_move(*pseudo, *xmm, fp_size);
        }

        for (specific_reg, actual_loc, size) in input_moves.iter() {
            self.emit_raw_mov_from_loc(actual_loc, *specific_reg, *size);
        }

        // Push the x87 operands. Reversed, so that the first declared ends on
        // top of the stack where `%st` names it and the second at `%st(1)`.
        for (pseudo, size) in x87_pushes.into_iter().rev() {
            let addr = self.get_x87_mem_addr(pseudo).format(&self.base.target);
            let mnemonic = match size {
                32 => "flds",
                64 => "fldl",
                _ => "fldt",
            };
            self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                "{mnemonic} {addr}"
            ))));
        }
    }

    /// Substitute the operands into the template and emit it as raw text.
    fn emit_asm_template(&mut self, asm_data: &AsmData, build: &AsmOperandBuild) {
        let slots = &build.slots;
        // Convert goto_labels from (BasicBlockId, String) to (label_string, label_name)
        let goto_labels_formatted: Vec<(String, String)> = asm_data
            .goto_labels
            .iter()
            .map(|(bb_id, name)| {
                // Through `Label` rather than a second spelling of the same
                // format, so the quoting cannot be missed here.
                let label_str = crate::arch::lir::Label::new(&self.base.current_fn, bb_id.0).name();
                (label_str, name.clone())
            })
            .collect();

        // Substitute %0, %1, %[name], %l0, %l[name], etc. in the template with actual operands
        let asm_output =
            self.substitute_asm_operands(&asm_data.template, slots, &goto_labels_formatted);

        // Emit the inline assembly as raw text
        // Split by newlines and emit each line
        for line in asm_output.lines() {
            let trimmed = line.trim();
            if !trimmed.is_empty() {
                self.push_lir(X86Inst::Directive(Directive::Raw(trimmed.to_string())));
            }
        }
    }

    /// Everything that must run once the template has finished: results copied
    /// out of the registers the constraints forced them into.
    fn emit_asm_epilogue_moves(
        &mut self,
        insn: &Instruction,
        asm_data: &AsmData,
        build: &AsmOperandBuild,
    ) {
        let AsmOperandBuild {
            output_moves,
            remap_restore,
            sse_output_moves,
            x87_store,
            ..
        } = build;
        // Pop the x87 result back into the operand's own storage. Only an
        // output has somewhere to go; a template that consumed its input --
        // `fistpl` on a `"t"` operand -- leaves nothing here.
        if let Some((pseudo, size)) = x87_store {
            let addr = self.get_x87_mem_addr(*pseudo).format(&self.base.target);
            let mnemonic = match size {
                32 => "fstps",
                64 => "fstpl",
                _ => "fstpt",
            };
            self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                "{mnemonic} {addr}"
            ))));
        }

        // Copy SSE outputs out of the scratch register into where the
        // operand actually lives. `emit_raw_mov_to_loc` below cannot do this
        // -- its source is a general register.
        for (xmm, actual_loc, size) in sse_output_moves.iter() {
            // `emit_fp_move_from_xmm` silently does nothing for a destination
            // it does not handle, which would lose the value. Say so instead.
            if matches!(actual_loc, Loc::Global(_) | Loc::IncomingArg(_)) {
                crate::diag::error(
                    insn.pos.unwrap_or_default(),
                    "an SSE asm output cannot be written back to this location",
                );
                continue;
            }
            let fp_size = FpSize::from_bits(*size, &self.base.target);
            self.emit_fp_move_from_xmm(*xmm, actual_loc, fp_size);
        }

        // Emit moves from specific registers to actual locations (for outputs)
        for (_idx, specific_reg, actual_loc, size) in output_moves.iter() {
            self.emit_raw_mov_to_loc(*specific_reg, actual_loc, *size);
        }

        // Emit remap restore moves (for outputs that conflicted with reserved regs)
        for (temp, _orig, actual_loc, size) in remap_restore.iter() {
            self.emit_raw_mov_to_loc(*temp, actual_loc, *size);
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

    /// Get the mov suffix and register size modifier for a given bit size
    fn asm_mov_info(size_bits: u32) -> (&'static str, char) {
        match size_bits {
            0..=8 => ("movb", 'b'),
            9..=16 => ("movw", 'w'),
            17..=32 => ("movl", 'k'),
            _ => ("movq", 'q'),
        }
    }

    /// Emit a raw mov instruction from a location to a register (for asm input setup)
    fn emit_raw_mov_from_loc(&mut self, loc: &Loc, dest_reg: Reg, size_bits: u32) {
        let (mov, sz) = Self::asm_mov_info(size_bits);
        let dest_name = if sz == 'q' {
            self.reg_name_64(dest_reg)
        } else {
            self.sized_reg_name(dest_reg, sz)
        };
        match loc {
            Loc::Reg(src_reg) => {
                let src_name = if sz == 'q' {
                    self.reg_name_64(*src_reg)
                } else {
                    self.sized_reg_name(*src_reg, sz)
                };
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "{} %{}, %{}",
                    mov, src_name, dest_name
                ))));
            }
            Loc::Stack(offset) => {
                let mem = self.stack_mem(*offset);
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "{} {}, %{}",
                    mov,
                    mem.format(&self.base.target),
                    dest_name
                ))));
            }
            Loc::Imm(v) => {
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "{} ${}, %{}",
                    mov, *v as i64, dest_name
                ))));
            }
            // A general register holding a floating constant holds its bits,
            // which is what gcc materializes for `"r"(1.0)`. A `double`'s
            // pattern does not fit a 32-bit immediate, so the load is always
            // 64-bit -- `movl $0x3ff0000000000000` would be truncated.
            Loc::FImm(v, fp_size) => {
                let bits = v.to_bits_at_width(*fp_size);
                let (wide_mov, wide_name) = ("movabsq", self.reg_name_64(dest_reg));
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "{} ${}, %{}",
                    wide_mov, bits, wide_name
                ))));
            }
            Loc::Global(name) => {
                // Check TLS before GOT - TLS symbols need special access pattern
                if self.tls_symbols.contains(name) && self.base.target.os == Os::Linux {
                    self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                        "{} %fs:{}@TPOFF, %{}",
                        mov,
                        self.format_symbol_name(name),
                        dest_name
                    ))));
                } else if self.needs_got_access(name) {
                    // GOT indirection always uses 64-bit address load
                    self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                        "movq {}@GOTPCREL(%rip), %r11",
                        self.format_symbol_name(name)
                    ))));
                    self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                        "{} (%r11), %{}",
                        mov, dest_name
                    ))));
                } else {
                    self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                        "{} {}(%rip), %{}",
                        mov,
                        self.format_symbol_name(name),
                        dest_name
                    ))));
                }
            }
            _ => {
                let loc_str = self.loc_to_asm_string(loc);
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "{} {}, %{}",
                    mov, loc_str, dest_name
                ))));
            }
        }
    }

    /// Emit a raw mov instruction from a register to a location (for asm output store)
    fn emit_raw_mov_to_loc(&mut self, src_reg: Reg, loc: &Loc, size_bits: u32) {
        let (mov, sz) = Self::asm_mov_info(size_bits);
        let src_name = if sz == 'q' {
            self.reg_name_64(src_reg)
        } else {
            self.sized_reg_name(src_reg, sz)
        };
        match loc {
            Loc::Reg(dest_reg) => {
                let dest_name = if sz == 'q' {
                    self.reg_name_64(*dest_reg)
                } else {
                    self.sized_reg_name(*dest_reg, sz)
                };
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "{} %{}, %{}",
                    mov, src_name, dest_name
                ))));
            }
            Loc::Stack(offset) => {
                let mem = self.stack_mem(*offset);
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "{} %{}, {}",
                    mov,
                    src_name,
                    mem.format(&self.base.target)
                ))));
            }
            Loc::Global(name) => {
                if self.tls_symbols.contains(name) && self.base.target.os == Os::Linux {
                    self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                        "{} %{}, %fs:{}@TPOFF",
                        mov,
                        src_name,
                        self.format_symbol_name(name)
                    ))));
                } else if self.needs_got_access(name) {
                    // GOT indirection always uses 64-bit address load
                    self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                        "movq {}@GOTPCREL(%rip), %r11",
                        self.format_symbol_name(name)
                    ))));
                    self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                        "{} %{}, (%r11)",
                        mov, src_name
                    ))));
                } else {
                    self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                        "{} %{}, {}(%rip)",
                        mov,
                        src_name,
                        self.format_symbol_name(name)
                    ))));
                }
            }
            Loc::Imm(_) => {
                // Can't store to an immediate — dead code. Skip.
            }
            _ => {
                let loc_str = self.loc_to_asm_string(loc);
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "{} %{}, {}",
                    mov, src_name, loc_str
                ))));
            }
        }
    }

    /// Convert a location to an asm operand string
    fn loc_to_asm_string(&self, loc: &Loc) -> String {
        match loc {
            Loc::Reg(r) => format!("%{}", self.reg_name_64(*r)),
            Loc::Stack(offset) => self.stack_mem(*offset).format(&self.base.target),
            Loc::IncomingArg(offset) => {
                format!("{}(%rbp)", offset)
            }
            Loc::Imm(v) => format!("${}", *v as i64),
            Loc::Xmm(xmm) => xmm.name().to_string(),
            // An immediate-class constraint takes the constant's bit pattern,
            // which is what gcc substitutes: `"i"(1.0)` gives
            // `$0x3ff0000000000000`. The register and memory classes never
            // reach here; `emit_inline_asm` materializes or diagnoses them
            // first.
            Loc::FImm(v, fp_size) => format!("${}", v.to_bits_at_width(*fp_size)),
            Loc::Global(name) => {
                format!("{}(%rip)", self.format_symbol_name(name))
            }
        }
    }

    /// Format a symbol name with platform-specific prefix.
    ///
    /// Decorates like [`Symbol::format_for_target`] but decides "local" from
    /// the name's leading `.` rather than from a flag, because the callers
    /// here have a bare `&str`. The quoting rule is shared, so the two cannot
    /// disagree about *that* even while they still differ about decoration.
    pub(super) fn format_symbol_name(&self, name: &str) -> String {
        // An asm label is the final name; see `lir::VERBATIM_MARKER`.
        if let Some(verbatim) = crate::arch::lir::strip_verbatim(name) {
            return crate::arch::lir::quote_symbol_if_needed(verbatim);
        }
        let decorated = if self.base.target.os == Os::MacOS && !name.starts_with('.') {
            format!("_{}", name)
        } else {
            name.to_string()
        };
        crate::arch::lir::quote_symbol_if_needed(&decorated)
    }

    /// Get the 64-bit register name
    fn reg_name_64(&self, reg: Reg) -> &'static str {
        match reg {
            Reg::Rax => "rax",
            Reg::Rbx => "rbx",
            Reg::Rcx => "rcx",
            Reg::Rdx => "rdx",
            Reg::Rsi => "rsi",
            Reg::Rdi => "rdi",
            Reg::Rbp => "rbp",
            Reg::Rsp => "rsp",
            Reg::R8 => "r8",
            Reg::R9 => "r9",
            Reg::R10 => "r10",
            Reg::R11 => "r11",
            Reg::R12 => "r12",
            Reg::R13 => "r13",
            Reg::R14 => "r14",
            Reg::R15 => "r15",
        }
    }

    /// Extract the specific register required by an x86 asm constraint.
    /// Returns Some(Reg) if the constraint requires a specific register,
    /// None if any register is acceptable (e.g., "r").
    fn constraint_to_specific_reg(constraint: &str) -> Option<Reg> {
        // Scan constraint for specific register indicators
        // Skip modifiers like =, +, &, %
        for c in constraint.chars() {
            match c {
                'a' => return Some(Reg::Rax),
                'b' => return Some(Reg::Rbx),
                'c' => return Some(Reg::Rcx),
                'd' => return Some(Reg::Rdx),
                'S' => return Some(Reg::Rsi),
                'D' => return Some(Reg::Rdi),
                _ => {}
            }
        }
        None
    }

    /// Whether the constraint asks for an SSE register specifically.
    ///
    /// These are separate from `constraint_requires_register`, which is about
    /// the general registers: an operand can be spilled to the stack and still
    /// satisfy `"x"`, but it cannot be handed over as a general register.
    fn constraint_requires_sse(constraint: &str) -> bool {
        // `f`, `t` and `u` are the **x87 stack** classes on x86, not SSE:
        // `f` any x87 register, `t` st(0), `u` st(1). Counting them here sent
        // a long double through an XMM scratch and emitted `movt ..., %xmm15`,
        // which is not an instruction -- musl's
        // `long double sqrtl(long double x){ __asm__("fsqrt" : "+t"(x)); }`
        // failed to assemble. See `constraint_requires_x87`.
        constraint.chars().any(|c| matches!(c, 'x' | 'v' | 'Y'))
    }

    /// Whether `get_x87_mem_addr` can address this location soundly.
    ///
    /// It has no arm for `Loc::Xmm` and falls back to `[rbp+0]` -- the saved
    /// frame pointer -- so an x87 constraint on a value sitting in an XMM
    /// register, which is where a `double` lives, silently read garbage.
    /// Refusing is not gcc's answer, which spills it, but it is the honest one
    /// until c17 can spill here.
    fn x87_addressable(loc: &Loc) -> bool {
        matches!(
            loc,
            Loc::Stack(_) | Loc::IncomingArg(_) | Loc::Global(_) | Loc::Reg(_) | Loc::FImm(..)
        )
    }

    /// How the template names the `n`th x87 operand.
    ///
    /// `t` is the top of the stack and `u` the one below it, and operands are
    /// pushed so that the first declared ends on top.
    fn x87_slot_name(n: usize) -> String {
        if n == 0 {
            "%st".to_string()
        } else {
            format!("%st({n})")
        }
    }

    fn constraint_requires_x87(constraint: &str) -> bool {
        constraint.chars().any(|c| matches!(c, 'f' | 't' | 'u'))
    }

    fn constraint_requires_register(constraint: &str) -> bool {
        let mut has_reg_class = false;
        let mut has_mem_class = false;
        for c in constraint.chars() {
            match c {
                'r' | 'a' | 'b' | 'c' | 'd' | 'S' | 'D' | 'q' | 'R' | 'l' => has_reg_class = true,
                'm' | 'o' | 'V' | 'Q' | 'g' | 'X' => has_mem_class = true,
                // 'i' / 'n' / 'I' / 'J' / 'K' / 'L' / 'M' / 'N' / 'O'
                // are immediate-class — substitute literal when the
                // value is const-folded. They don't make memory
                // acceptable, so they don't disable requires_register.
                _ => {}
            }
        }
        has_reg_class && !has_mem_class
    }

    /// Check if an inline asm constraint requires the operand to be
    /// in memory — i.e., the codegen must produce a memory operand
    /// reference rather than a register reference.
    ///
    /// Multi-alternative semantics: a constraint that lists any
    /// non-memory class (`r`, `a`..`d`, `S`, `D`, `i`, `n`, `g`) does
    /// NOT require memory, because the operand can use the register
    /// or immediate form directly. Only constraints that are memory-
    /// class-only force a memory operand.
    fn constraint_requires_memory(constraint: &str) -> bool {
        let mut has_mem_class = false;
        let mut has_non_mem_class = false;
        for c in constraint.chars() {
            match c {
                'm' | 'o' | 'V' | 'Q' => has_mem_class = true,
                'r' | 'a' | 'b' | 'c' | 'd' | 'S' | 'D' | 'q' | 'R' | 'l' | 'i' | 'n' | 'g'
                | 'X' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' => has_non_mem_class = true,
                _ => {}
            }
        }
        has_mem_class && !has_non_mem_class
    }

    /// Substitute %0, %1, %[name], %l0, %l[name], etc. with actual operand strings
    /// goto_labels: (label_string, label_name) - label_string is the fully formatted label
    fn substitute_asm_operands(
        &self,
        template: &str,
        slots: &[crate::arch::AsmOperandSlot<Reg>],
        goto_labels: &[(String, String)],
    ) -> String {
        crate::arch::substitute_asm_operands(self, template, slots, goto_labels)
    }

    /// Get a sized register name based on modifier
    pub(super) fn sized_reg_name(&self, reg: Reg, size_mod: char) -> &'static str {
        match (reg, size_mod) {
            // 8-bit (b)
            (Reg::Rax, 'b') => "al",
            (Reg::Rbx, 'b') => "bl",
            (Reg::Rcx, 'b') => "cl",
            (Reg::Rdx, 'b') => "dl",
            (Reg::Rsi, 'b') => "sil",
            (Reg::Rdi, 'b') => "dil",
            (Reg::R8, 'b') => "r8b",
            (Reg::R9, 'b') => "r9b",
            (Reg::R10, 'b') => "r10b",
            (Reg::R11, 'b') => "r11b",
            (Reg::R12, 'b') => "r12b",
            (Reg::R13, 'b') => "r13b",
            (Reg::R14, 'b') => "r14b",
            (Reg::R15, 'b') => "r15b",
            // 16-bit (w)
            (Reg::Rax, 'w') => "ax",
            (Reg::Rbx, 'w') => "bx",
            (Reg::Rcx, 'w') => "cx",
            (Reg::Rdx, 'w') => "dx",
            (Reg::Rsi, 'w') => "si",
            (Reg::Rdi, 'w') => "di",
            (Reg::R8, 'w') => "r8w",
            (Reg::R9, 'w') => "r9w",
            (Reg::R10, 'w') => "r10w",
            (Reg::R11, 'w') => "r11w",
            (Reg::R12, 'w') => "r12w",
            (Reg::R13, 'w') => "r13w",
            (Reg::R14, 'w') => "r14w",
            (Reg::R15, 'w') => "r15w",
            // 32-bit (k or l)
            (Reg::Rax, 'k') => "eax",
            (Reg::Rbx, 'k') => "ebx",
            (Reg::Rcx, 'k') => "ecx",
            (Reg::Rdx, 'k') => "edx",
            (Reg::Rsi, 'k') => "esi",
            (Reg::Rdi, 'k') => "edi",
            (Reg::R8, 'k') => "r8d",
            (Reg::R9, 'k') => "r9d",
            (Reg::R10, 'k') => "r10d",
            (Reg::R11, 'k') => "r11d",
            (Reg::R12, 'k') => "r12d",
            (Reg::R13, 'k') => "r13d",
            (Reg::R14, 'k') => "r14d",
            (Reg::R15, 'k') => "r15d",
            // 64-bit (q) - default
            _ => self.reg_name_64(reg),
        }
    }
}
