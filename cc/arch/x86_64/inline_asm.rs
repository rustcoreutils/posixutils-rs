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

use crate::arch::asm_constraints::{AsmOperandClass, AsmRegClass, X87Slot};
use crate::arch::lir::{Directive, FpSize};
use crate::arch::x86_64::codegen::X86_64CodeGen;
use crate::arch::x86_64::lir::X86Inst;
use crate::arch::x86_64::regalloc::{asm_pinned_regs, Loc, Reg, XmmReg};
use crate::ir::{AsmConstraint, AsmData, Instruction, PseudoId};
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
    /// are not given one: each is pushed before the template and the result
    /// popped back after, and the template names them `%st` and `%st(1)`.
    ///
    /// `t` is st(0) and `u` is st(1), so the deepest is pushed first for `t`
    /// to end up on top. Collected in operand order -- where a tied input
    /// comes after the inputs it sits above -- and ordered at emit time.
    x87_pushes: Vec<X87Operand>,
    /// Set only when there is an output to write back to, so a template that
    /// consumes its operand (`fistpl` on a `"t"` input) leaves nothing to
    /// store. A pure `"=t"` output is not pushed -- the template supplies the
    /// value, as `fldz` does; a tied input (`"+t"`, or a `"0"` naming it) is.
    /// Carries the output's index, which a tied input names.
    x87_store: Option<(usize, X87Operand)>,
    x87_slots: usize,
}

/// One operand on the x87 register stack.
#[derive(Clone, Copy)]
struct X87Operand {
    /// Depth on the stack while the template runs: 0 is `%st`.
    depth: usize,
    pseudo: PseudoId,
    /// Operand width in bits: 32 and 64 are `float` and `double`, anything
    /// wider an x87 `long double`.
    size: u32,
}

/// The x87 stack slot an operand's class asks for: `f` any of it, `t` its
/// top and `u` the register below. On x86 these are not SSE classes.
///
/// The register allocator asks the same question, so that an x87 operand
/// gets a floating-point home -- and a `long double` one a stack slot, the
/// only place an 80-bit value can live -- rather than a general register.
fn x87_slot(class: &AsmOperandClass) -> Option<X87Slot> {
    match class.reg {
        Some(AsmRegClass::X87(slot)) => Some(slot),
        _ => None,
    }
}

/// The class a template substitution follows: a tied input's is the output's
/// it names.
fn effective_class<'a>(asm: &'a AsmData, c: &'a AsmConstraint) -> &'a AsmOperandClass {
    match c.matching_output {
        Some(i) if i < asm.outputs.len() => &asm.outputs[i].class,
        _ => &c.class,
    }
}

/// The operands of an `asm` that live on the x87 stack: not in memory, and
/// with an x87 constraint -- a tied input's being the output it names.
pub(super) fn x87_operands(asm: &AsmData) -> impl Iterator<Item = &AsmConstraint> {
    asm.outputs
        .iter()
        .chain(&asm.inputs)
        .filter(|c| !c.is_memory() && x87_slot(effective_class(asm, c)).is_some())
}

/// Whether the class asks for a general register and offers no memory: a
/// value that lives in memory must be loaded into one.
fn requires_gp_register(class: &AsmOperandClass) -> bool {
    matches!(
        class.reg,
        Some(AsmRegClass::General | AsmRegClass::Pinned(_) | AsmRegClass::HighByte)
    ) && class.mem.is_none()
}

/// Whether the class asks for an SSE register specifically.
///
/// Separate from `requires_gp_register`, which is about the general
/// registers: an operand can be spilled to the stack and still satisfy
/// `"x"`, but it cannot be handed over as a general register.
fn requires_sse(class: &AsmOperandClass) -> bool {
    class.reg == Some(AsmRegClass::Vector)
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
            // A register the statement clobbers is no temp: the template
            // would destroy what it holds.
            used_regs: reserved_regs
                .iter()
                .copied()
                .chain(
                    asm_data
                        .clobbers
                        .iter()
                        .filter_map(|c| crate::arch::x86_64::regalloc::parse_gp_clobber_name(c)),
                )
                .collect(),
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

/// A temp register for an operand the allocator gave no register of its own,
/// neither named by the statement nor already spent.
///
/// Only R10 and R11: they are never allocated, so nothing lives in them. The
/// list used to go on to R8, R9, RSI and RDI, which are, so a third temp
/// landed on another operand or a value live across the asm -- an input read
/// twice, or a result overwritten. Running out is an error, not a guess.
fn find_temp_reg(
    reserved: &std::collections::HashSet<Reg>,
    used: &std::collections::HashSet<Reg>,
    pos: Option<crate::diag::Position>,
) -> Reg {
    for r in [Reg::R10, Reg::R11] {
        if !reserved.contains(&r) && !used.contains(&r) {
            return r;
        }
    }
    crate::diag::error(
        pos.unwrap_or_default(),
        "too many register operands in one asm statement; c17 has no register \
         left to give one",
    );
    Reg::R10
}

impl X86_64CodeGen {
    /// Emit inline assembly
    pub(super) fn emit_inline_asm(&mut self, insn: &Instruction) {
        let asm_data = match &insn.extra().asm_data {
            Some(data) => data.as_ref(),
            None => return,
        };

        let pins = asm_pinned_regs(asm_data);
        Self::check_high_byte_pins(insn, asm_data, &pins);
        let reserved_regs: std::collections::HashSet<Reg> =
            pins.iter().flatten().copied().collect();
        let mut build = AsmOperandBuild::new(asm_data, &reserved_regs);
        self.build_output_slots(insn, asm_data, &pins, &reserved_regs, &mut build);
        self.build_input_slots(insn, asm_data, &pins, &reserved_regs, &mut build);
        self.emit_asm_prologue_moves(&mut build);
        self.emit_asm_template(asm_data, &build);
        self.emit_asm_epilogue_moves(insn, asm_data, &build);
    }

    /// A `Q` operand left without a register: the statement pins or
    /// clobbers all four with a high byte. gcc reports the same.
    fn check_high_byte_pins(insn: &Instruction, asm_data: &AsmData, pins: &[Option<Reg>]) {
        let operands = asm_data.outputs.iter().chain(&asm_data.inputs);
        if operands
            .zip(pins)
            .any(|(c, pin)| c.class.reg == Some(AsmRegClass::HighByte) && pin.is_none())
        {
            crate::diag::error(
                insn.pos.unwrap_or_default(),
                "no register with a high byte is left for a `Q` asm operand",
            );
        }
    }

    /// Build the substitution slot for each output operand, recording the
    /// moves that have to run after the template.
    fn build_output_slots(
        &mut self,
        insn: &Instruction,
        asm_data: &AsmData,
        pins: &[Option<Reg>],
        reserved_regs: &std::collections::HashSet<Reg>,
        build: &mut AsmOperandBuild,
    ) {
        let AsmOperandBuild {
            slots,
            output_moves,
            input_moves,
            remap_restore,
            pseudo_to_temp,
            used_regs,
            sse_scratch,
            sse_output_moves,
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

            // An operand pinned to one register
            if let Some(specific_reg) = pins[idx] {
                // Output goes to specific register, then we'll move to actual loc after asm
                slots.push(mk(Some(specific_reg), None));
                // Only need to move if actual loc is different from specific reg
                if loc != Loc::Reg(specific_reg) {
                    output_moves.push((idx, specific_reg, loc, op_size));
                }
            } else {
                let requires_reg = requires_gp_register(&output.class);
                let requires_mem = output.is_memory();
                // An x87-class output. Written back off the FP stack once
                // the template has run; a read-write `"+t"` is also pushed
                // before it, by its tied input, while a pure `"=t"` takes
                // its value from the template.
                if let Some(slot) = x87_slot(&output.class) {
                    let depth = Self::x87_depth(slot, x87_slots);
                    let operand = X87Operand {
                        depth,
                        pseudo: output.pseudo,
                        size: op_size,
                    };
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
                    } else if Self::x87_output_home(&loc, op_size) {
                        *x87_store = Some((idx, operand));
                    } else {
                        crate::diag::error(
                            insn.pos.unwrap_or_default(),
                            "an x87 asm output cannot be written back to this \
                             location",
                        );
                    }
                    slots.push(mk(None, Some(Self::x87_slot_name(depth))));
                    continue;
                }
                // No specific register - use allocated location
                match loc {
                    // Memory-class output (`"=m"(x)`/`"+m"(*p)`). Without
                    // this arm the template substitutes `%eax` for an address
                    // held in a register, and `addl $1, %0` increments the
                    // address bits instead of the value at that address.
                    _ if requires_mem => {
                        let mem_str = self.asm_memory_slot(
                            output.pseudo,
                            output.offset,
                            &loc,
                            reserved_regs,
                            used_regs,
                            input_moves,
                            insn.pos,
                        );
                        slots.push(mk(None, Some(mem_str)));
                    }
                    _ if requires_sse(&output.class) => {
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
                            let temp = find_temp_reg(reserved_regs, used_regs, insn.pos);
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
                        let temp = find_temp_reg(reserved_regs, used_regs, insn.pos);
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
                        let temp = find_temp_reg(reserved_regs, used_regs, insn.pos);
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
        pins: &[Option<Reg>],
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
            x87_store,
            x87_slots,
            pseudo_to_xmm,
            ..
        } = build;
        let num_outputs = asm_data.outputs.len();
        // The hidden inputs of `"+"` outputs, numbered after every explicit
        // input -- see `AsmConstraint::is_hidden_readwrite_input`.
        let mut hidden: Vec<usize> = Vec::new();
        // Process input operands
        for (input_idx, input) in asm_data.inputs.iter().enumerate() {
            let op_size = input.size;

            // Handle matching constraints - use the matched output's location/register
            let loc = match input.matching_output {
                Some(match_idx) if match_idx < num_outputs => {
                    self.get_location(asm_data.outputs[match_idx].pseudo)
                }
                _ => self.get_location(input.pseudo),
            };
            let class = effective_class(asm_data, input);

            // A tied input names the output's register, so its value is
            // loaded there before the template. An explicit `"0"` is an
            // operand of its own, `%N` in input order; the hidden input of a
            // `"+"` output is numbered after all of them. Skipping every tied
            // input numbered the ones after an explicit `"0"` one short, so
            // `addq %2, %0` with `"0"(a), "r"(b)` named nothing.
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
                    } else if let Some((_, out)) = x87_store.filter(|(i, _)| *i == match_idx) {
                        // An x87 output's initial value, pushed to the
                        // output's own depth. Nothing pushed it before, so
                        // `"=t"(r) : "0"(x)` ran the template on whatever the
                        // FP stack held.
                        x87_pushes.push(X87Operand {
                            pseudo: input.pseudo,
                            ..out
                        });
                    }
                    if input.is_hidden_readwrite_input() {
                        hidden.push(match_idx);
                    } else {
                        let mut slot = slots[match_idx].clone();
                        slot.name = input.name.clone();
                        slots.push(slot);
                    }
                    continue;
                }
            }

            let op_name = input.name.clone();
            let mk = |reg: Option<Reg>, mem: Option<String>| crate::arch::AsmOperandSlot {
                reg,
                mem,
                size: op_size,
                name: op_name.clone(),
            };

            // An operand pinned to one register
            if let Some(specific_reg) = pins[num_outputs + input_idx] {
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
                    let requires_reg = requires_gp_register(class);
                    let requires_mem = class.is_memory_only();
                    // An x87-class input: pushed onto the FP stack before the
                    // template, which then names it `%st`/`%st(1)`. Without
                    // this arm an x87 input fell through to the general path
                    // and the template ran on whatever happened to be on the
                    // stack -- `__asm__("fmulp" : "+t"(a) : "u"(b))` answered
                    // -nan.
                    if let Some(slot) = x87_slot(class) {
                        let depth = Self::x87_depth(slot, x87_slots);
                        let name = Self::x87_slot_name(depth);
                        if Self::x87_input_home(&loc, input.size) {
                            x87_pushes.push(X87Operand {
                                depth,
                                pseudo: input.pseudo,
                                size: input.size,
                            });
                        } else {
                            crate::diag::error(
                                insn.pos.unwrap_or_default(),
                                "a long double x87 asm operand must live somewhere \
                                 addressable; c17 cannot spill one here",
                            );
                        }
                        slots.push(mk(None, Some(name)));
                        continue;
                    }
                    // No specific register - use allocated location
                    match loc {
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
                        Loc::FImm(v, imm_size) if requires_sse(class) => {
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
                            let temp = find_temp_reg(reserved_regs, used_regs, insn.pos);
                            used_regs.insert(temp);
                            input_moves.push((temp, loc.clone(), op_size));
                            let mem_str = format!("(%{})", self.reg_name_64(temp));
                            slots.push(mk(None, Some(mem_str)));
                        }
                        _ if requires_mem => {
                            let mem_str = self.asm_memory_slot(
                                input.pseudo,
                                input.offset,
                                &loc,
                                reserved_regs,
                                used_regs,
                                input_moves,
                                insn.pos,
                            );
                            slots.push(mk(None, Some(mem_str)));
                        }
                        Loc::Reg(r) => {
                            // Check if allocated reg conflicts with reserved
                            if reserved_regs.contains(&r) {
                                // Use a temp register instead
                                let temp = find_temp_reg(reserved_regs, used_regs, insn.pos);
                                used_regs.insert(temp);
                                slots.push(mk(Some(temp), None));
                                // For inputs, move from actual loc to temp before asm
                                remap_setup.push((r, temp, loc.clone(), op_size));
                            } else {
                                slots.push(mk(Some(r), None));
                                used_regs.insert(r);
                            }
                        }
                        // A constant under a register-only constraint still goes
                        // in a register: the template may use it where no
                        // immediate is allowed (`leaq 8($100), %rax`).
                        Loc::Imm(_) if requires_reg && !class.imm => {
                            let temp = find_temp_reg(reserved_regs, used_regs, insn.pos);
                            used_regs.insert(temp);
                            slots.push(mk(Some(temp), None));
                            input_moves.push((temp, loc.clone(), op_size));
                        }
                        Loc::Imm(v) => {
                            // Immediate value
                            slots.push(mk(None, Some(format!("${}", v as i64))));
                        }
                        _ if requires_reg => {
                            // Constraint requires register but value is on stack/memory.
                            // Allocate a temp register and load value before asm.
                            let temp = find_temp_reg(reserved_regs, used_regs, insn.pos);
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
        for idx in hidden {
            let slot = slots[idx].clone();
            slots.push(slot);
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
        let mut x87_pushes = std::mem::take(x87_pushes);
        // Push the x87 operands, deepest first so that `t` ends on top of the
        // stack where `%st` names it and `u` at `%st(1)`. Before anything
        // else: a float or double is staged through the reserved XMM scratch,
        // which the SSE operands below are about to be given, and may borrow
        // a scratch general register the remapped operands are about to take.
        x87_pushes.sort_by_key(|op| std::cmp::Reverse(op.depth));
        for op in x87_pushes {
            self.emit_x87_asm_push(op.pseudo, op.size);
        }

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
                let label_str =
                    crate::arch::lir::Label::block(&self.base.current_fn, bb_id.0).name();
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

        // Pop the x87 result back into the operand's home. Only an output has
        // somewhere to go; a template that consumed its input -- `fistpl` on a
        // `"t"` operand -- leaves nothing here. After the SSE outputs, whose
        // scratch a float or double result is staged through.
        if let Some((_, op)) = x87_store {
            self.emit_x87_asm_pop(op.pseudo, op.size);
        }

        // These run only on the fall-through: a jump to an `asm goto` label
        // skips them, and the output would reach the label unwritten. Such a
        // statement has to have every output in its own register.
        let moved = output_moves
            .iter()
            .any(|(_, r, loc, _)| *loc != Loc::Reg(*r))
            || !remap_restore.is_empty()
            || !sse_output_moves.is_empty()
            || x87_store.is_some();
        if moved && !asm_data.goto_labels.is_empty() {
            crate::diag::error(
                insn.pos.unwrap_or_default(),
                "an asm goto output needs a register of its own, and this statement \
                 has more register operands than c17 can give them",
            );
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
                if self.is_tls_symbol(name) {
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
                if self.is_tls_symbol(name) {
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

    /// The text of a memory-class operand.
    ///
    /// The linearizer hands over one of two things. A named object at a
    /// constant offset arrives as its own `Sym`, and is addressed where it
    /// lives -- frame slot, incoming-argument slot or symbol -- with no
    /// register spent on it. Anything else is an address *value*: in a
    /// register it is used as the base; spilled, it is loaded into a scratch
    /// register first. Substituting the spill slot itself, as this used to,
    /// handed the template the saved pointer rather than the object -- a
    /// statement with more operands than registers read and wrote the wrong
    /// memory.
    #[allow(clippy::too_many_arguments)]
    fn asm_memory_slot(
        &mut self,
        pseudo: PseudoId,
        offset: i64,
        loc: &Loc,
        reserved: &std::collections::HashSet<Reg>,
        used: &mut std::collections::HashSet<Reg>,
        input_moves: &mut Vec<(Reg, Loc, u32)>,
        pos: Option<crate::diag::Position>,
    ) -> String {
        let target = self.base.target.clone();
        if self.pseudos.is_sym(pseudo) {
            let Ok(offset) = i32::try_from(offset) else {
                crate::diag::error(
                    pos.unwrap_or_default(),
                    "an asm memory operand lies past the displacement an instruction can hold",
                );
                return "0".to_string();
            };
            return match loc {
                Loc::Stack(slot) => self.stack_field(*slot, offset).format(&target),
                Loc::IncomingArg(off) => format!("{}(%rbp)", off + offset),
                Loc::Global(name) => {
                    if self.is_tls_symbol(name) || self.needs_got_access(name) {
                        let temp = find_temp_reg(reserved, used, pos);
                        used.insert(temp);
                        let name = name.clone();
                        self.global_mem(&name, offset, temp).format(&target)
                    } else if offset == 0 {
                        format!("{}(%rip)", self.format_symbol_name(name))
                    } else {
                        format!("{}{:+}(%rip)", self.format_symbol_name(name), offset)
                    }
                }
                other => self.loc_to_asm_string(other),
            };
        }
        match loc {
            Loc::Reg(r) => format!("(%{})", self.reg_name_64(*r)),
            Loc::Stack(_) | Loc::IncomingArg(_) | Loc::Global(_) => {
                let temp = find_temp_reg(reserved, used, pos);
                used.insert(temp);
                input_moves.push((temp, loc.clone(), 64));
                format!("(%{})", self.reg_name_64(temp))
            }
            other => self.loc_to_asm_string(other),
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
    pub(super) fn reg_name_64(&self, reg: Reg) -> &'static str {
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

    /// Whether an x87 input at `loc` can be pushed; see `emit_x87_asm_push`.
    ///
    /// A `float` or `double` is staged through the x87 scratch from wherever
    /// it is. A `long double` is loaded from memory by `get_x87_mem_addr`,
    /// which has no arm for `Loc::Xmm` and falls back to `[rbp+0]` -- the
    /// saved frame pointer -- so anything it cannot address is refused.
    fn x87_input_home(loc: &Loc, size: u32) -> bool {
        size <= 64
            || matches!(
                loc,
                Loc::Stack(_) | Loc::IncomingArg(_) | Loc::Global(_) | Loc::Reg(_) | Loc::FImm(..)
            )
    }

    /// Whether an x87 output at `loc` can be popped into; see
    /// `emit_x87_asm_pop`.
    ///
    /// A `long double` needs memory of its own, which the allocator gives
    /// every x87 operand of that type. A `float` or `double` is staged through
    /// the x87 scratch into a register or its slot -- where the allocator puts
    /// one, and at -O1 and above that is usually an XMM register.
    fn x87_output_home(loc: &Loc, size: u32) -> bool {
        if size > 64 {
            matches!(loc, Loc::Stack(_) | Loc::IncomingArg(_))
        } else {
            matches!(loc, Loc::Stack(_) | Loc::Xmm(_) | Loc::Reg(_))
        }
    }

    /// The stack depth an x87 operand occupies while the template runs,
    /// taking the next operand number from `count`.
    ///
    /// `t` is the top of the stack and `u` the one below it, whatever order
    /// the operands are written in; `f` takes its operand number.
    fn x87_depth(slot: X87Slot, count: &mut usize) -> usize {
        let n = *count;
        *count += 1;
        match slot {
            X87Slot::Top => 0,
            X87Slot::Second => 1,
            X87Slot::Any => n,
        }
    }

    /// How the template names the x87 operand at stack depth `n`.
    fn x87_slot_name(n: usize) -> String {
        if n == 0 {
            "%st".to_string()
        } else {
            format!("%st({n})")
        }
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
