//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 Register Allocator
// Linear scan register allocation for x86-64
//
// ============================================================================
// REGISTER ALLOCATION POLICY (LLVM-style constraint-aware allocation)
// ============================================================================
//
// Reserved registers (NEVER allocated to pseudos):
//   R10, R11 - Codegen scratch registers
//
// Codegen MUST use only scratch registers (R10, R11) for temporaries.
// Using allocatable registers (Rax, Rbx, etc.) risks clobbering live values
// that were assigned by the register allocator.
//
// Constrained registers (handled via LLVM-style constraint system):
//   Rax:Rdx - Clobbered by division (idiv/div uses edx:eax dividend, writes
//             quotient to eax and remainder to edx)
//   Rcx     - Shift counts (shl, shr, sar) for variable shifts
//
// The allocator uses RegConstraints to track which instructions clobber
// specific registers, and ConstraintPoint to identify positions where
// constraints apply. When allocating, pseudos that are live across a
// constraint point (but not involved in that instruction) are excluded
// from being allocated to the clobbered registers.
//
// Calling convention (System V AMD64 ABI):
//   Rdi, Rsi, Rdx, Rcx, R8, R9 - Integer arguments
//   Xmm0-Xmm7                   - FP arguments
//   Rax, Xmm0                   - Return values
//   Rbx, Rbp, R12-R15          - Callee-saved
// ============================================================================

use crate::arch::regalloc::{
    compute_live_intervals, find_call_positions, identify_addr_taken_syms, identify_fp_pseudos,
    interval_crosses_call, ConstraintPoint, FreeSlot, LiveInterval, LivenessResult,
};
use crate::ir::{Function, Instruction, Opcode, PseudoId, PseudoKind};
use crate::types::TypeTable;
use std::collections::{BTreeMap, HashMap, HashSet};

// ============================================================================
// x86-64 Register Definitions
// ============================================================================

/// x86-64 physical registers
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Reg {
    // 64-bit general purpose registers
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Rbp,
    Rsp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Reg {
    /// Get AT&T syntax name for 64-bit register
    pub fn name64(&self) -> &'static str {
        match self {
            Reg::Rax => "%rax",
            Reg::Rbx => "%rbx",
            Reg::Rcx => "%rcx",
            Reg::Rdx => "%rdx",
            Reg::Rsi => "%rsi",
            Reg::Rdi => "%rdi",
            Reg::Rbp => "%rbp",
            Reg::Rsp => "%rsp",
            Reg::R8 => "%r8",
            Reg::R9 => "%r9",
            Reg::R10 => "%r10",
            Reg::R11 => "%r11",
            Reg::R12 => "%r12",
            Reg::R13 => "%r13",
            Reg::R14 => "%r14",
            Reg::R15 => "%r15",
        }
    }

    /// Get AT&T syntax name for 32-bit register
    pub fn name32(&self) -> &'static str {
        match self {
            Reg::Rax => "%eax",
            Reg::Rbx => "%ebx",
            Reg::Rcx => "%ecx",
            Reg::Rdx => "%edx",
            Reg::Rsi => "%esi",
            Reg::Rdi => "%edi",
            Reg::Rbp => "%ebp",
            Reg::Rsp => "%esp",
            Reg::R8 => "%r8d",
            Reg::R9 => "%r9d",
            Reg::R10 => "%r10d",
            Reg::R11 => "%r11d",
            Reg::R12 => "%r12d",
            Reg::R13 => "%r13d",
            Reg::R14 => "%r14d",
            Reg::R15 => "%r15d",
        }
    }

    /// Get AT&T syntax name for 16-bit register
    pub fn name16(&self) -> &'static str {
        match self {
            Reg::Rax => "%ax",
            Reg::Rbx => "%bx",
            Reg::Rcx => "%cx",
            Reg::Rdx => "%dx",
            Reg::Rsi => "%si",
            Reg::Rdi => "%di",
            Reg::Rbp => "%bp",
            Reg::Rsp => "%sp",
            Reg::R8 => "%r8w",
            Reg::R9 => "%r9w",
            Reg::R10 => "%r10w",
            Reg::R11 => "%r11w",
            Reg::R12 => "%r12w",
            Reg::R13 => "%r13w",
            Reg::R14 => "%r14w",
            Reg::R15 => "%r15w",
        }
    }

    /// Get AT&T syntax name for 8-bit register (low byte)
    pub fn name8(&self) -> &'static str {
        match self {
            Reg::Rax => "%al",
            Reg::Rbx => "%bl",
            Reg::Rcx => "%cl",
            Reg::Rdx => "%dl",
            Reg::Rsi => "%sil",
            Reg::Rdi => "%dil",
            Reg::Rbp => "%bpl",
            Reg::Rsp => "%spl",
            Reg::R8 => "%r8b",
            Reg::R9 => "%r9b",
            Reg::R10 => "%r10b",
            Reg::R11 => "%r11b",
            Reg::R12 => "%r12b",
            Reg::R13 => "%r13b",
            Reg::R14 => "%r14b",
            Reg::R15 => "%r15b",
        }
    }

    /// Get register name for a given bit size
    pub fn name_for_size(&self, bits: u32) -> &'static str {
        match bits {
            8 => self.name8(),
            16 => self.name16(),
            32 => self.name32(),
            _ => self.name64(),
        }
    }

    /// Is this a callee-saved register?
    pub fn is_callee_saved(&self) -> bool {
        matches!(
            self,
            Reg::Rbx | Reg::Rbp | Reg::R12 | Reg::R13 | Reg::R14 | Reg::R15
        )
    }

    /// Argument registers in order (System V AMD64 ABI)
    pub fn arg_regs() -> &'static [Reg] {
        &[Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9]
    }

    /// All allocatable registers (excluding RSP, RBP, R10, R11)
    /// R10 and R11 are reserved as scratch registers for codegen operations.
    /// Codegen MUST use only R10/R11 for temporaries to avoid clobbering
    /// live values assigned by the register allocator.
    ///
    /// Note: Rax, Rdx, and Rcx are allocatable but have constraints:
    /// - Rax/Rdx: Clobbered by division instructions
    /// - Rcx: Required for variable shift counts
    ///
    /// These constraints are handled by the LLVM-style constraint system
    /// in the register allocator, NOT by codegen save/restore.
    pub fn allocatable() -> &'static [Reg] {
        &[
            Reg::Rax,
            Reg::Rbx,
            Reg::Rcx,
            Reg::Rdx,
            Reg::Rsi,
            Reg::Rdi,
            Reg::R8,
            Reg::R9,
            // R10 and R11 are reserved as scratch for codegen
            Reg::R12,
            Reg::R13,
            Reg::R14,
            Reg::R15,
        ]
    }

    /// Stack pointer register
    pub fn sp() -> Reg {
        Reg::Rsp
    }

    /// Base/frame pointer register
    pub fn bp() -> Reg {
        Reg::Rbp
    }
}

// ============================================================================
// Register Constraints (LLVM-style constraint-aware allocation)
// ============================================================================

/// Register constraints for an instruction.
/// Used by the register allocator to avoid assigning pseudos to registers
/// that will be clobbered by constrained instructions like division.
#[derive(Debug, Clone)]
pub struct RegConstraints {
    /// Registers that are clobbered (written) by this instruction
    pub clobbers: &'static [Reg],
}

impl RegConstraints {
    /// No constraints - most instructions have no implicit register requirements
    pub const NONE: RegConstraints = RegConstraints { clobbers: &[] };
}

/// Get register constraints for an opcode.
/// Division clobbers Rax (quotient) and Rdx (remainder).
/// Shifts require the count in Cl (Rcx) for variable shifts.
/// VaArg clobbers Rax (used as scratch) and Rcx (used for sign-extended offset).
pub fn opcode_constraints(op: Opcode) -> RegConstraints {
    match op {
        Opcode::DivS | Opcode::DivU | Opcode::ModS | Opcode::ModU => RegConstraints {
            clobbers: &[Reg::Rax, Reg::Rdx],
        },
        // Int128 mul uses `mulq` which clobbers RAX:RDX. Regular mul uses IMul2
        // which doesn't depend on these, so marking them as clobbers is safe for both.
        Opcode::Mul => RegConstraints {
            clobbers: &[Reg::Rax, Reg::Rdx],
        },
        Opcode::Shl | Opcode::Lsr | Opcode::Asr => RegConstraints {
            clobbers: &[Reg::Rcx],
        },
        Opcode::VaArg => RegConstraints {
            clobbers: &[Reg::Rax, Reg::Rcx],
        },
        _ => RegConstraints::NONE,
    }
}

/// True when the IR opcode's codegen lowering uses `R10` and/or `R11`
/// as a temp register. The constraint declaration prevents the
/// chordal allocator from placing a *cross-instruction* live pseudo
/// in R10/R11 — operands of the instruction itself remain exempt
/// via `involved_pseudos`, so the codegen helper can continue using
/// the scratch register freely during its lowering.
///
/// The list is conservative: any helper whose body reads or writes
/// R10/R11 anywhere (even on a single conditional branch) is
/// included. False positives are harmless — they just mean the
/// allocator avoids R10/R11 for one more class of pseudo. False
/// negatives would silently miscompile.
///
/// The audit was done by grepping `Reg::R10` / `Reg::R11` across the
/// x86_64 backend and mapping each occurrence back to the
/// dispatching `Opcode`. Helpers whose only R10/R11 use is the
/// libc-call argument-spill helper (already covered by
/// `is_call_like_x86_64`) are not listed — their cross-call clobber
/// already forbids R10/R11 alongside every other caller-saved
/// register.
fn opcode_clobbers_r10_r11(op: Opcode) -> bool {
    // **Infrastructure only — see the documentation at
    // `R10_R11_FREEING_DEFERRED` below for why R10/R11 are still
    // in the reserved-scratch list.** This predicate is the
    // future-extension point: when the codegen refactor that
    // moves scratch declarations off hardcoded R10/R11 lands,
    // adding R10/R11 to `Reg::allocatable()` will start producing
    // ConstraintPoint forbiddings without changing
    // `get_constraint_info`.
    //
    // Conservative coverage: every opcode whose codegen helper
    // touches a non-trivial code path is included. Excluded
    // (truly clean) opcodes:
    //
    // - `Br` → single `jmp`.
    // - `Nop` → no emission.
    // - `Phi` / `PhiSource` → lowered out by `cc/ir/lower.rs`
    //   before codegen runs.
    // - `Unreachable` → `ud2`.
    // - `Fence` → single `mfence`/`sfence`/`lfence`.
    // - `VaEnd` → no-op on x86_64 SysV.
    //
    // **Entry is dirty** — the function prologue's `rep stosq`
    // path saves rdi/rcx into R10/R11. Any pseudo whose live
    // range starts at the entry position needs the prologue-
    // clobber declaration.
    !matches!(
        op,
        Opcode::Br
            | Opcode::Nop
            | Opcode::Phi
            | Opcode::PhiSource
            | Opcode::Unreachable
            | Opcode::Fence
            | Opcode::VaEnd
    )
}

// ============================================================================
// C4 status — R10/R11 freeing deferred.
// ============================================================================
//
// The C4 milestone intended to add R10/R11 to `Reg::allocatable()`
// so the chordal allocator could place pseudos in them when their
// live range doesn't cross an opcode whose codegen helper uses them
// as scratch. The constraint declaration `opcode_clobbers_r10_r11`
// above is the infrastructure that *would* make the allocator
// respect those clobbers.
//
// In practice the per-IR-opcode constraint model is **not
// sufficient**. Inter-instruction codegen paths use R10/R11 in
// ways no IR-instruction-level constraint can express:
//
// 1. `emit_prologue` saves rdi/rcx into R10/R11 around the
//    `rep stosq` that zeroes the local frame.
// 2. `emit_variadic_save_area` shuttles arg regs through R10/R11.
// 3. `spill_args_across_calls` codegen restores spilled args
//    via R10/R11 in the prologue.
// 4. Several FP and struct lowerings use R10/R11 between LIR
//    pushes that span more than one IR instruction's worth of
//    LIR output.
//
// Empirical proof: adding R10/R11 to `Reg::allocatable()` even
// with `opcode_clobbers_r10_r11` returning `true` for everything
// but the 7 truly-trivial opcodes still segfaults CPython's
// `_bootstrap_python` at deepfreeze generation. The crash site
// (`ucs1lib_default_find`) is in code whose codegen exercises one
// of the inter-instruction paths.
//
// Genuine freeing requires either:
// - **Codegen refactor**: every `emit_*` helper takes an
//   allocator-supplied scratch register. Prologue/epilogue/
//   variadic-save also route through the allocator. Estimated
//   4000–8000 LOC change.
// - **Pre-IR scratch-declaration table**: every helper publishes
//   its scratch needs into a side table the allocator consults
//   before liveness analysis; ConstraintPoints get added for
//   prologue/epilogue positions. Estimated 1500–2500 LOC.
//
// Both are multi-session milestones in their own right. C4 ships
// the constraint-declaration infrastructure that enables either
// approach without lock-in.

/// Opcodes whose x86_64 codegen lowering invokes an external function
/// (libc or otherwise) and therefore clobbers caller-saved registers.
/// Used by `find_call_positions` to drive the chordal allocator's
/// cross-call caller-saved forbidding.
///
/// Beyond the obvious `Call` / `Longjmp` / `Setjmp`:
/// - `Fabs32` / `Fabs64` → `fabsf` / `fabs` libc call (features.rs:1359+)
/// - `Signbit32` / `Signbit64` → `__signbitf` / target-specific
///   signbit-double libc call (features.rs:1407+)
/// - `Memset` / `Memcpy` / `Memmove` → libc memset/memcpy/memmove
///   (features.rs:1214+)
///
/// Before this list existed, the chordal allocator (M6+M7) had a
/// latent bug: live FP pseudos in the same block as a `Signbit64`
/// would be allocated to XMM regs that the emitted libc call
/// clobbered. Linear scan never used those XMM regs (pop'd from
/// end of palette) so the bug was invisible until M6.
pub fn is_call_like_x86_64(op: Opcode) -> bool {
    matches!(
        op,
        Opcode::Call
            | Opcode::Longjmp
            | Opcode::Setjmp
            | Opcode::Fabs32
            | Opcode::Fabs64
            | Opcode::Signbit32
            | Opcode::Signbit64
            | Opcode::Memset
            | Opcode::Memcpy
            | Opcode::Memmove
    )
}

/// Map a single-letter GCC operand-constraint Fixed-register letter
/// to the corresponding x86_64 GP register.
///
/// The C2 vocabulary covers `a` (rax), `b` (rbx), `c` (rcx), `d`
/// (rdx), `S` (rsi), `D` (rdi). Rarer letters (`q`, `Q`, `R`, `l`,
/// `t`, ...) are out of scope until after C5 lands.
pub fn parse_x86_64_fixed_letter(letter: char) -> Option<Reg> {
    Some(match letter {
        'a' => Reg::Rax,
        'b' => Reg::Rbx,
        'c' => Reg::Rcx,
        'd' => Reg::Rdx,
        'S' => Reg::Rsi,
        'D' => Reg::Rdi,
        _ => return None,
    })
}

/// Map a clobber-list register name (lowercase, GCC-style) to the
/// corresponding `Reg`. Accepts the 64-bit canonical name (`rax`,
/// `r10`, ...), the 32/16/8-bit alias (`eax`, `ax`, `al`, `r10d`,
/// `r10w`, `r10b`), and the GCC-style "%rax" with leading `%`.
/// Returns `None` for names the GP table doesn't know about
/// (XMM registers, `memory`, `cc`, x87 stack, ...).
fn parse_gp_clobber_name(raw: &str) -> Option<Reg> {
    let s = raw.trim_start_matches('%').to_ascii_lowercase();
    Some(match s.as_str() {
        "rax" | "eax" | "ax" | "al" | "ah" => Reg::Rax,
        "rbx" | "ebx" | "bx" | "bl" | "bh" => Reg::Rbx,
        "rcx" | "ecx" | "cx" | "cl" | "ch" => Reg::Rcx,
        "rdx" | "edx" | "dx" | "dl" | "dh" => Reg::Rdx,
        "rsi" | "esi" | "si" | "sil" => Reg::Rsi,
        "rdi" | "edi" | "di" | "dil" => Reg::Rdi,
        "rbp" | "ebp" | "bp" | "bpl" => Reg::Rbp,
        "rsp" | "esp" | "sp" | "spl" => Reg::Rsp,
        "r8" | "r8d" | "r8w" | "r8b" => Reg::R8,
        "r9" | "r9d" | "r9w" | "r9b" => Reg::R9,
        "r10" | "r10d" | "r10w" | "r10b" => Reg::R10,
        "r11" | "r11d" | "r11w" | "r11b" => Reg::R11,
        "r12" | "r12d" | "r12w" | "r12b" => Reg::R12,
        "r13" | "r13d" | "r13w" | "r13b" => Reg::R13,
        "r14" | "r14d" | "r14w" | "r14b" => Reg::R14,
        "r15" | "r15d" | "r15w" | "r15b" => Reg::R15,
        _ => return None,
    })
}

/// Get constraint info for an instruction (used by shared compute_live_intervals).
/// Returns (clobbered_registers, involved_pseudos) if constraints apply, None otherwise.
///
/// Sources:
/// - **Opcode-level hardware constraints** (`opcode_constraints`):
///   division clobbers Rax/Rdx; shifts clobber Rcx; vaarg clobbers
///   Rax/Rcx; mul (int128 path) clobbers Rax/Rdx.
/// - **Inline-asm explicit clobbers** (`Opcode::Asm`): the parser
///   collected the asm's clobber list into `AsmData.clobbers`. Named
///   GP registers in that list become hard clobbers at the asm
///   position. Special tokens (`"memory"`, `"cc"`) are not GP
///   register clobbers and are filtered out here — `"memory"` gets
///   full barrier semantics in C3 via `InstrConstraints.memory_barrier`;
///   `"cc"` is a no-op for our allocator since we don't track
///   condition-code liveness.
///
/// `involved_pseudos` includes the instruction's target + sources so
/// they may legally occupy a clobbered register if needed (the
/// operand exemption — see `pseudos_interfere` documentation). VaArg
/// is the exception: its sources must NOT alias the clobbered regs.
/// Build the per-operand `InstrConstraints` view of an inline-asm
/// instruction. Walks `AsmData.outputs` + `AsmData.inputs` + the
/// clobber list and produces a single structured value. Returns
/// `None` if the instruction has no `AsmData` (shouldn't happen for
/// `Opcode::Asm` in well-formed IR).
///
/// Constraint-string parse errors are reported by `pcc`'s front end
/// at IR-construction time (the parser already accepts these
/// strings). Here we treat a parse error as "ignore this operand" —
/// the existing inline-asm codegen continues to use the raw constraint
/// string for letters this C2 vocabulary doesn't cover yet.
pub fn build_asm_instr_constraints_x86_64(
    insn: &Instruction,
) -> Option<crate::arch::asm_constraints::InstrConstraints<Reg>> {
    use crate::arch::asm_constraints::{
        parse_constraint, InstrConstraints, OperandKind, OperandSpec,
    };

    let asm_data = insn.asm_data.as_ref()?;
    let mut operands = Vec::new();
    for ac in asm_data.outputs.iter().chain(asm_data.inputs.iter()) {
        if let Ok((kind, constraint)) = parse_constraint(&ac.constraint, parse_x86_64_fixed_letter)
        {
            // Default-kind from the parser is correct for inputs and
            // matches GCC for outputs (`=` and `+` modifiers come
            // through the string). Explicit override only needed if
            // the parser's heuristic mismatches an output we know is
            // a Def — leave to C3 if it shows up.
            let _ = OperandKind::Use; // silence unused-variant warning
            operands.push(OperandSpec {
                pseudo: ac.pseudo,
                kind,
                constraint,
            });
        }
    }
    let mut clobbers: Vec<Reg> = asm_data
        .clobbers
        .iter()
        .filter_map(|name| parse_gp_clobber_name(name))
        .collect();
    clobbers.sort();
    clobbers.dedup();
    let memory_barrier = asm_data.clobbers.iter().any(|c| c == "memory");
    Some(InstrConstraints {
        operands,
        clobbers,
        memory_barrier,
    })
}

/// Lower a richer `InstrConstraints` down to the
/// `(clobbers, involved_pseudos)` shape the current `ConstraintPoint`
/// mechanism understands. The C2 commit keeps `InstrConstraints` as
/// a pass-through; the chordal allocator still drives entirely off
/// the lowered `ConstraintPoint`. C3 will start consuming
/// `InstrConstraints.operands` and `memory_barrier` directly.
pub fn lower_instr_constraints_to_constraint_point(
    ic: &crate::arch::asm_constraints::InstrConstraints<Reg>,
    insn: &Instruction,
) -> (Vec<Reg>, Vec<PseudoId>) {
    use crate::arch::asm_constraints::{OperandConstraint, OperandKind};

    let mut clobbers = ic.clobbers.clone();
    for op in &ic.operands {
        // The C2 lowering only uses Fixed (-> implicit clobber) and
        // EarlyClobber kind (-> implicit clobber). The other variants
        // are recognised here for completeness and for C3, which
        // consumes them directly off `InstrConstraints` instead of
        // going through `ConstraintPoint`.
        match &op.constraint {
            OperandConstraint::Fixed(r) => clobbers.push(*r),
            OperandConstraint::Match(_idx) => { /* C3: coalescing edge */ }
            OperandConstraint::Any | OperandConstraint::Mem | OperandConstraint::Imm => {}
        }
        // Early-clobber outputs are written before all inputs are
        // read, so the allocator must keep them disjoint from every
        // input. C3 implements this as per-operand forbidden edges;
        // C2 cannot express it through ConstraintPoint.
        if matches!(op.kind, OperandKind::EarlyClobber) { /* C3: extra interference */ }
    }
    // `memory_barrier` is parsed and stored on `InstrConstraints`
    // but its strict semantics (force a barrier on every memory-
    // promoted pseudo) require allocator awareness that arrives in
    // C3. C1 already treats inline-asm memory clobber as a clobber
    // list entry the allocator otherwise ignores (`parse_gp_clobber_name`
    // returns None for the string `"memory"`).
    let _ = ic.memory_barrier;
    clobbers.sort();
    clobbers.dedup();

    let mut involved = Vec::new();
    if let Some(t) = insn.target {
        involved.push(t);
    }
    involved.extend(insn.src.iter().copied());
    for op in &ic.operands {
        if !involved.contains(&op.pseudo) {
            involved.push(op.pseudo);
        }
    }

    (clobbers, involved)
}

/// Walk a function's inline-asm instructions and collect
/// `(operand_pseudo, fixed_reg)` pairs from every `Fixed`-constrained
/// operand. Used by the chordal allocator to pre-color those
/// operands so they land in the constraint-required register
/// directly instead of being placed elsewhere and moved into the
/// fixed register by the inline-asm codegen.
///
/// The codegen's "move-into-fixed-register-around-asm" path stays
/// in place as a fallback — if pre-coloring conflicts with an
/// earlier ABI pin and the allocator can't honor it, the codegen
/// still emits the corrective move. C3 just lets the allocator
/// honor the constraint *cleanly* in the common case.
pub fn collect_asm_fixed_precolors_x86_64(func: &Function) -> BTreeMap<PseudoId, Reg> {
    let mut out = BTreeMap::new();
    for block in &func.blocks {
        for insn in &block.insns {
            if insn.op != Opcode::Asm {
                continue;
            }
            let Some(ic) = build_asm_instr_constraints_x86_64(insn) else {
                continue;
            };
            for op in &ic.operands {
                if let crate::arch::asm_constraints::OperandConstraint::Fixed(r) = &op.constraint {
                    // First Fixed seen wins. Duplicate pins on the
                    // same pseudo across multiple asm blocks would be
                    // a source bug; ignore the second pin.
                    out.entry(op.pseudo).or_insert(*r);
                }
            }
        }
    }
    out
}

pub fn get_constraint_info(insn: &Instruction) -> Option<(Vec<Reg>, Vec<PseudoId>)> {
    // Inline asm: route through the C2 per-operand constraint
    // vocabulary and lower the result back to ConstraintPoint so the
    // chordal allocator behaves exactly as it did under C1. C3
    // switches the allocator to consume `InstrConstraints` directly.
    if insn.op == Opcode::Asm {
        let ic = build_asm_instr_constraints_x86_64(insn)?;
        let (mut clobbers, involved) = lower_instr_constraints_to_constraint_point(&ic, insn);
        if opcode_clobbers_r10_r11(insn.op) {
            clobbers.push(Reg::R10);
            clobbers.push(Reg::R11);
            clobbers.sort();
            clobbers.dedup();
        }
        if clobbers.is_empty() {
            return None;
        }
        return Some((clobbers, involved));
    }

    // Opcode-level hardware constraints, plus the C4 R10/R11 scratch
    // clobbers for any opcode whose codegen helper uses them.
    let constraints = opcode_constraints(insn.op);
    let needs_r10_r11 = opcode_clobbers_r10_r11(insn.op);
    if constraints.clobbers.is_empty() && !needs_r10_r11 {
        return None;
    }

    let mut clobbers: Vec<Reg> = constraints.clobbers.to_vec();
    if needs_r10_r11 {
        clobbers.push(Reg::R10);
        clobbers.push(Reg::R11);
    }
    clobbers.sort();
    clobbers.dedup();

    let mut involved = Vec::new();
    if let Some(t) = insn.target {
        involved.push(t);
    }
    // For VaArg, sources should NOT be in clobbered registers,
    // so we don't add them to involved_pseudos
    if insn.op != Opcode::VaArg {
        involved.extend(insn.src.iter().copied());
    }

    Some((clobbers, involved))
}

// ============================================================================
// XMM Register Definitions (SSE/FP)
// ============================================================================

/// x86-64 XMM registers for floating-point operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum XmmReg {
    Xmm0,
    Xmm1,
    Xmm2,
    Xmm3,
    Xmm4,
    Xmm5,
    Xmm6,
    Xmm7,
    Xmm8,
    Xmm9,
    Xmm10,
    Xmm11,
    Xmm12,
    Xmm13,
    Xmm14,
    Xmm15,
}

impl XmmReg {
    /// Get AT&T syntax name for XMM register
    pub fn name(&self) -> &'static str {
        match self {
            XmmReg::Xmm0 => "%xmm0",
            XmmReg::Xmm1 => "%xmm1",
            XmmReg::Xmm2 => "%xmm2",
            XmmReg::Xmm3 => "%xmm3",
            XmmReg::Xmm4 => "%xmm4",
            XmmReg::Xmm5 => "%xmm5",
            XmmReg::Xmm6 => "%xmm6",
            XmmReg::Xmm7 => "%xmm7",
            XmmReg::Xmm8 => "%xmm8",
            XmmReg::Xmm9 => "%xmm9",
            XmmReg::Xmm10 => "%xmm10",
            XmmReg::Xmm11 => "%xmm11",
            XmmReg::Xmm12 => "%xmm12",
            XmmReg::Xmm13 => "%xmm13",
            XmmReg::Xmm14 => "%xmm14",
            XmmReg::Xmm15 => "%xmm15",
        }
    }

    /// Floating-point argument registers (System V AMD64 ABI)
    pub fn arg_regs() -> &'static [XmmReg] {
        &[
            XmmReg::Xmm0,
            XmmReg::Xmm1,
            XmmReg::Xmm2,
            XmmReg::Xmm3,
            XmmReg::Xmm4,
            XmmReg::Xmm5,
            XmmReg::Xmm6,
            XmmReg::Xmm7,
        ]
    }

    /// All allocatable XMM registers
    /// All XMM registers (XMM0-XMM15) are caller-saved on x86-64 SysV ABI.
    /// Values in XMM registers are NOT preserved across function calls.
    /// XMM14 and XMM15 are reserved as scratch registers for codegen operations
    pub fn allocatable() -> &'static [XmmReg] {
        &[
            XmmReg::Xmm0,
            XmmReg::Xmm1,
            XmmReg::Xmm2,
            XmmReg::Xmm3,
            XmmReg::Xmm4,
            XmmReg::Xmm5,
            XmmReg::Xmm6,
            XmmReg::Xmm7,
            XmmReg::Xmm8,
            XmmReg::Xmm9,
            XmmReg::Xmm10,
            XmmReg::Xmm11,
            XmmReg::Xmm12,
            XmmReg::Xmm13,
            // XMM14 and XMM15 are reserved for scratch use in codegen
        ]
    }
}

// ============================================================================
// Operand - Location of a value (register or memory)
// ============================================================================

/// Location of a value
#[derive(Debug, Clone, PartialEq)]
pub enum Loc {
    /// In a general-purpose register
    Reg(Reg),
    /// In an XMM register (floating-point)
    Xmm(XmmReg),
    /// On the stack at [rbp - offset]
    Stack(i32),
    /// Incoming stack argument at [rbp + offset] (positive offset, above return address)
    /// Used for function parameters 7+ that are passed on the stack by the caller
    IncomingArg(i32),
    /// Immediate integer constant
    Imm(i128),
    /// Immediate float constant (value, size in bits)
    FImm(f64, u32),
    /// Global symbol
    Global(String),
}

/// Information about an argument spilled from a caller-saved register to stack
#[derive(Debug, Clone)]
pub struct SpilledArg {
    /// The pseudo that was spilled
    pub pseudo: PseudoId,
    /// The register the argument originally arrived in
    pub from_reg: Reg,
    /// The stack offset where it was spilled to
    pub to_stack_offset: i32,
}

/// XMM argument that was spilled from an XMM register to the stack.
/// All XMM registers are caller-saved on x86-64 SysV ABI, so FP arguments
/// must be spilled if their live interval extends past the function entry.
pub struct SpilledXmmArg {
    /// The pseudo that was spilled
    pub pseudo: PseudoId,
    /// The XMM register the argument originally arrived in
    pub from_xmm: XmmReg,
    /// The stack offset where it was spilled to
    pub to_stack_offset: i32,
}

// ============================================================================
// Register Allocator (Linear Scan)
// ============================================================================

/// Simple linear scan register allocator for x86-64
pub struct RegAlloc {
    /// Mapping from pseudo to location
    locations: HashMap<PseudoId, Loc>,
    /// Free general-purpose registers
    free_regs: Vec<Reg>,
    /// Free XMM registers (for floating-point)
    free_xmm_regs: Vec<XmmReg>,
    /// Active integer register intervals (sorted by end point)
    active: Vec<(LiveInterval, Reg)>,
    /// Active XMM register intervals (sorted by end point)
    active_xmm: Vec<(LiveInterval, XmmReg)>,
    /// Next stack slot offset
    stack_offset: i32,
    /// Callee-saved registers that were used
    used_callee_saved: Vec<Reg>,
    /// Track which pseudos need FP registers (based on type)
    fp_pseudos: HashSet<PseudoId>,
    /// Track which pseudos are long double (use x87, need 16-byte stack slots)
    ld_pseudos: HashSet<PseudoId>,
    /// Track which pseudos are 128-bit integers (need 16-byte stack slots, never registers)
    int128_pseudos: HashSet<PseudoId>,
    /// Arguments spilled from caller-saved registers to stack
    spilled_args: Vec<SpilledArg>,
    /// XMM arguments spilled from XMM registers to stack
    spilled_xmm_args: Vec<SpilledXmmArg>,
    /// Active stack slot intervals (interval, offset, size) for reuse tracking
    active_stack: Vec<crate::arch::regalloc::ActiveSlot>,
    /// Free stack slots keyed by size, available for reuse
    free_stack_slots: BTreeMap<i32, Vec<FreeSlot>>,
    /// Sym pseudos whose address is taken (cannot participate in slot reuse)
    addr_taken_syms: HashSet<PseudoId>,
    /// Per-block live-in sets for interference-based stack coloring
    live_in: Vec<HashSet<PseudoId>>,
    /// Per-block live-out sets for interference-based stack coloring
    live_out: Vec<HashSet<PseudoId>>,
    /// Maximum alignment requirement of any local variable (for dynamic stack alignment)
    max_local_align: i32,
}

impl RegAlloc {
    pub fn new() -> Self {
        Self {
            locations: HashMap::new(),
            free_regs: Reg::allocatable().to_vec(),
            free_xmm_regs: XmmReg::allocatable().to_vec(),
            active: Vec::new(),
            active_xmm: Vec::new(),
            stack_offset: 0,
            used_callee_saved: Vec::new(),
            fp_pseudos: HashSet::new(),
            ld_pseudos: HashSet::new(),
            int128_pseudos: HashSet::new(),
            spilled_args: Vec::new(),
            spilled_xmm_args: Vec::new(),
            active_stack: Vec::new(),
            free_stack_slots: BTreeMap::new(),
            addr_taken_syms: HashSet::new(),
            live_in: Vec::new(),
            live_out: Vec::new(),
            max_local_align: 8,
        }
    }

    /// Perform register allocation for a function
    pub fn allocate(
        &mut self,
        func: &Function,
        types: &TypeTable,
    ) -> crate::arch::regalloc::LocationMap<Loc> {
        self.reset_state();
        // Use shared identify_fp_pseudos with type-checker closure
        self.fp_pseudos = identify_fp_pseudos(func, |typ| types.is_float(typ));
        // Identify long double pseudos (use x87 not XMM)
        self.identify_ld_pseudos(func, types);
        // Identify 128-bit integer pseudos (always spill to 16-byte stack slots)
        self.identify_int128_pseudos(func, types);
        self.addr_taken_syms = identify_addr_taken_syms(func);
        self.allocate_arguments(func, types);

        let result = self.compute_live_intervals(func);
        self.live_in = result.live_in;
        self.live_out = result.live_out;
        let intervals = result.intervals;
        let constraint_points = result.constraint_points;
        let call_positions = find_call_positions(func, is_call_like_x86_64);

        self.spill_args_across_calls(func, &intervals, &call_positions);
        self.spill_args_across_constraints(func, &intervals, &constraint_points);
        self.allocate_alloca_to_stack(func);
        self.run_chordal_color(func, types, intervals, &call_positions, &constraint_points);

        crate::arch::regalloc::LocationMap::from(self.locations.clone())
    }

    /// Reset allocator state for a new function
    fn reset_state(&mut self) {
        self.locations.clear();
        self.free_regs = Reg::allocatable().to_vec();
        self.free_xmm_regs = XmmReg::allocatable().to_vec();
        self.active.clear();
        self.active_xmm.clear();
        self.stack_offset = 0;
        self.used_callee_saved.clear();
        self.fp_pseudos.clear();
        self.ld_pseudos.clear();
        self.int128_pseudos.clear();
        self.spilled_args.clear();
        self.spilled_xmm_args.clear();
        self.active_stack.clear();
        self.free_stack_slots.clear();
        self.addr_taken_syms.clear();
        self.live_in.clear();
        self.live_out.clear();
        self.max_local_align = 8;
    }

    /// Identify pseudos that are long double (80-bit extended precision).
    /// These use x87 FPU instead of XMM and need 16-byte stack slots.
    fn identify_ld_pseudos(&mut self, func: &Function, types: &TypeTable) {
        for block in &func.blocks {
            for insn in &block.insns {
                // Check if this instruction operates on long double
                let is_longdouble = insn
                    .typ
                    .is_some_and(|t| types.kind(t) == crate::types::TypeKind::LongDouble);

                if is_longdouble {
                    // Mark target as long double
                    if let Some(target) = insn.target {
                        self.ld_pseudos.insert(target);
                    }
                    // Mark sources as long double for Load/Store/Copy
                    for &src in &insn.src {
                        self.ld_pseudos.insert(src);
                    }
                }
            }
        }
    }

    /// Identify pseudos that are 128-bit integers (__int128).
    /// These need 16-byte stack slots and must never be allocated to GP registers.
    fn identify_int128_pseudos(&mut self, func: &Function, types: &TypeTable) {
        // First pass: identify targets of 128-bit instructions and all sources
        // of 128-bit binary/unary ops.
        for block in &func.blocks {
            for insn in &block.insns {
                // Only match Int128 type, not 16-byte structs or long doubles
                let is_int128 = insn
                    .typ
                    .is_some_and(|t| types.kind(t) == crate::types::TypeKind::Int128);

                if is_int128 {
                    // Comparison results are always small integers, not 128-bit.
                    let is_comparison = matches!(
                        insn.op,
                        Opcode::SetEq
                            | Opcode::SetNe
                            | Opcode::SetLt
                            | Opcode::SetLe
                            | Opcode::SetGt
                            | Opcode::SetGe
                            | Opcode::SetB
                            | Opcode::SetBe
                            | Opcode::SetA
                            | Opcode::SetAe
                    );

                    // Lo64/Hi64: target is 64-bit (not int128), source is int128
                    // Pair64: target is int128, sources are 64-bit (not int128)
                    // AddC/AdcC/SubC/SbcC/UMulHi: 64-bit ops, not int128
                    match insn.op {
                        Opcode::Lo64 | Opcode::Hi64 => {
                            // Source is int128, target is 64-bit
                            for &src in &insn.src {
                                self.int128_pseudos.insert(src);
                            }
                        }
                        Opcode::Pair64 => {
                            // Target is int128, sources are 64-bit
                            if let Some(target) = insn.target {
                                self.int128_pseudos.insert(target);
                            }
                        }
                        _ => {
                            // For Load: target is int128, but src[0] is the address (64-bit pointer).
                            // For Store: src[0] is address (64-bit), src[1] is the int128 value.
                            // For comparisons: target is a small integer result.
                            if !is_comparison && !matches!(insn.op, Opcode::Load) {
                                if let Some(target) = insn.target {
                                    self.int128_pseudos.insert(target);
                                }
                            }
                            if matches!(insn.op, Opcode::Load) {
                                if let Some(target) = insn.target {
                                    self.int128_pseudos.insert(target);
                                }
                            } else if matches!(insn.op, Opcode::Store) {
                                if let Some(&val) = insn.src.get(1) {
                                    self.int128_pseudos.insert(val);
                                }
                            } else {
                                for &src in &insn.src {
                                    self.int128_pseudos.insert(src);
                                }
                            }
                        }
                    }
                }
            }
        }
        // Second pass: propagate through Copy instructions only.
        // Load src is an address (not int128), so don't propagate through Load.
        let mut changed = true;
        while changed {
            changed = false;
            for block in &func.blocks {
                for insn in &block.insns {
                    if insn.op == Opcode::Copy {
                        if let (Some(target), Some(&src)) = (insn.target, insn.src.first()) {
                            if self.int128_pseudos.contains(&src)
                                && !self.int128_pseudos.contains(&target)
                            {
                                self.int128_pseudos.insert(target);
                                changed = true;
                            }
                            if self.int128_pseudos.contains(&target)
                                && !self.int128_pseudos.contains(&src)
                            {
                                self.int128_pseudos.insert(src);
                                changed = true;
                            }
                        }
                    }
                }
            }
        }
    }

    /// Pre-allocate argument registers per System V AMD64 ABI
    ///
    /// System V AMD64 ABI passes arguments as follows:
    /// - First 6 integer/pointer args in RDI, RSI, RDX, RCX, R8, R9
    /// - First 8 FP args in XMM0-XMM7
    /// - Remaining args go on the stack in parameter order (not separated by type)
    fn allocate_arguments(&mut self, func: &Function, types: &TypeTable) {
        use crate::arch::regalloc::AbiLowering;

        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = XmmReg::arg_regs();
        let mut int_arg_idx = 0;
        let mut fp_arg_idx = 0;
        // Stack offset for overflow args - must be shared across all types
        // because System V AMD64 ABI places stack args in parameter order
        // 16 = saved rbp (8) + return address (8)
        let mut stack_arg_offset = 16i32;

        // M1: use the shared AbiLowering helper for sret detection and
        // O(1) Arg(n) → pseudo lookup. The classification dispatch below
        // retains the original inline type-kind checks to guarantee
        // byte-identical codegen with the pre-M1 implementation.
        let lowering = AbiLowering::new(func, types);
        let arg_idx_offset = lowering.arg_idx_offset;

        // Allocate RDI for hidden return pointer if present
        if let Some(sret_id) = lowering.sret_pseudo {
            self.locations.insert(sret_id, Loc::Reg(int_arg_regs[0]));
            self.free_regs.retain(|&r| r != int_arg_regs[0]);
            int_arg_idx += 1;
        }

        for (i, (_name, typ)) in func.params.iter().enumerate() {
            let arg_n = (i as u32) + arg_idx_offset;
            let Some(pseudo_id) = lowering.arg_pseudos.get(arg_n as usize).copied().flatten()
            else {
                continue;
            };
            let is_longdouble = types.kind(*typ) == crate::types::TypeKind::LongDouble;
            let is_fp = types.is_float(*typ);
            let is_complex = types.is_complex(*typ);
            let type_size = types.size_bits(*typ);
            let is_two_sse_struct = !is_complex
                && (types.kind(*typ) == crate::types::TypeKind::Struct
                    || types.kind(*typ) == crate::types::TypeKind::Union)
                && type_size > 64
                && type_size <= 128
                && {
                    use crate::abi::{Abi, SysVAmd64Abi};
                    matches!(
                        SysVAmd64Abi.classify_param(*typ, types),
                        crate::abi::ArgClass::Direct { ref classes, .. }
                            if classes.len() == 2
                                && classes.iter().all(|c| *c == crate::abi::RegClass::Sse)
                    )
                };

            // Long double uses x87 FPU and is passed on the stack per System V AMD64 ABI
            if is_longdouble {
                // Long double takes 16 bytes on stack (80-bit padded to 128-bit)
                self.locations
                    .insert(pseudo_id, Loc::IncomingArg(stack_arg_offset));
                self.fp_pseudos.insert(pseudo_id);
                stack_arg_offset += 16;
            } else if is_two_sse_struct {
                // 2-SSE struct: uses two XMM regs. Don't assign to register —
                // the codegen stores both XMM values to the local's stack slot.
                // Just consume the FP arg indices without assigning a location;
                // the pseudo will get a stack slot from normal allocation.
                fp_arg_idx += 2;
            } else if is_complex {
                // Complex: uses two consecutive XMM registers
                if fp_arg_idx + 1 < fp_arg_regs.len() {
                    self.locations
                        .insert(pseudo_id, Loc::Xmm(fp_arg_regs[fp_arg_idx]));
                    self.free_xmm_regs.retain(|&r| {
                        r != fp_arg_regs[fp_arg_idx] && r != fp_arg_regs[fp_arg_idx + 1]
                    });
                    self.fp_pseudos.insert(pseudo_id);
                } else {
                    self.locations
                        .insert(pseudo_id, Loc::IncomingArg(stack_arg_offset));
                    stack_arg_offset += 16;
                }
                fp_arg_idx += 2;
            } else if is_fp {
                if fp_arg_idx < fp_arg_regs.len() {
                    self.locations
                        .insert(pseudo_id, Loc::Xmm(fp_arg_regs[fp_arg_idx]));
                    self.free_xmm_regs.retain(|&r| r != fp_arg_regs[fp_arg_idx]);
                    self.fp_pseudos.insert(pseudo_id);
                } else {
                    // Stack args are placed in parameter order per System V AMD64 ABI
                    self.locations
                        .insert(pseudo_id, Loc::IncomingArg(stack_arg_offset));
                    stack_arg_offset += 8;
                }
                fp_arg_idx += 1;
            } else if types.kind(*typ) == crate::types::TypeKind::Int128 {
                // __int128: uses two GP registers when available.
                // Always allocate a local stack slot — for register params,
                // store_args_to_stack stores register values; for stack params,
                // store_args_to_stack copies from the incoming arg area.
                self.stack_offset = (self.stack_offset + 15) & !15;
                self.stack_offset += 16;
                self.locations
                    .insert(pseudo_id, Loc::Stack(self.stack_offset));
                self.int128_pseudos.insert(pseudo_id);
                if int_arg_idx + 1 < int_arg_regs.len() {
                    self.free_regs.retain(|&r| {
                        r != int_arg_regs[int_arg_idx] && r != int_arg_regs[int_arg_idx + 1]
                    });
                } else {
                    stack_arg_offset += 16;
                }
                int_arg_idx += 2;
            } else {
                let type_size = types.size_bits(*typ);
                let is_large_struct = (types.kind(*typ) == crate::types::TypeKind::Struct
                    || types.kind(*typ) == crate::types::TypeKind::Union)
                    && type_size > 128;
                if is_large_struct {
                    // Large struct (> 16 bytes): always passed on stack per
                    // SysV AMD64 ABI. Advance by full struct size.
                    self.locations
                        .insert(pseudo_id, Loc::IncomingArg(stack_arg_offset));
                    stack_arg_offset += (type_size / 8) as i32;
                    // Don't increment int_arg_idx — no GP register consumed
                } else if int_arg_idx < int_arg_regs.len() {
                    self.locations
                        .insert(pseudo_id, Loc::Reg(int_arg_regs[int_arg_idx]));
                    self.free_regs.retain(|&r| r != int_arg_regs[int_arg_idx]);
                    int_arg_idx += 1;
                } else {
                    // Stack args are placed in parameter order per System V AMD64 ABI
                    self.locations
                        .insert(pseudo_id, Loc::IncomingArg(stack_arg_offset));
                    stack_arg_offset += 8;
                    int_arg_idx += 1;
                }
            }
        }
    }

    /// Spill arguments in caller-saved registers if their interval crosses a call
    fn spill_args_across_calls(
        &mut self,
        _func: &Function,
        intervals: &[LiveInterval],
        call_positions: &[usize],
    ) {
        let int_arg_regs_set: &[Reg] = Reg::arg_regs();
        let spilled_args = &mut self.spilled_args;
        let free_regs = &mut self.free_regs;
        crate::arch::regalloc::spill_gp_args_across_calls(
            intervals,
            call_positions,
            &mut self.locations,
            &mut self.stack_offset,
            |reg| int_arg_regs_set.contains(&reg),
            |loc| {
                if let Loc::Reg(reg) = loc {
                    Some(*reg)
                } else {
                    None
                }
            },
            Loc::Stack,
            |pseudo, from_reg, to_stack_offset| {
                spilled_args.push(SpilledArg {
                    pseudo,
                    from_reg,
                    to_stack_offset,
                });
            },
            |reg| free_regs.push(reg),
        );

        // Always spill XMM function parameter arguments to stack.
        // All XMM registers are caller-saved on x86-64 SysV ABI, and any float
        // computation within the function may reuse the same XMM register,
        // clobbering the parameter value.
        let xmm_arg_regs = XmmReg::arg_regs();
        for interval in intervals {
            if let Some(Loc::Xmm(xmm)) = self.locations.get(&interval.pseudo) {
                if xmm_arg_regs.contains(xmm) && interval.start == 0 {
                    // This is a function parameter in an XMM register — always spill
                    let from_xmm = *xmm;
                    self.stack_offset += 8;
                    let to_stack_offset = self.stack_offset;

                    self.spilled_xmm_args.push(SpilledXmmArg {
                        pseudo: interval.pseudo,
                        from_xmm,
                        to_stack_offset,
                    });

                    self.locations
                        .insert(interval.pseudo, Loc::Stack(to_stack_offset));
                    self.free_xmm_regs.push(from_xmm);
                }
            }
        }
    }

    /// Get arguments that were spilled from caller-saved registers
    pub fn spilled_args(&self) -> &[SpilledArg] {
        &self.spilled_args
    }

    /// Get XMM arguments that were spilled from XMM registers
    pub fn spilled_xmm_args(&self) -> &[SpilledXmmArg] {
        &self.spilled_xmm_args
    }

    /// Get the set of pseudos identified as 128-bit integers
    pub fn int128_pseudos(&self) -> &HashSet<PseudoId> {
        &self.int128_pseudos
    }

    /// Spill arguments in registers that would be clobbered by constraint points (e.g., shifts)
    ///
    /// For example, if the 4th parameter is in Rcx and the function contains variable shifts,
    /// Rcx will be clobbered when the shift count is loaded. We must spill such arguments
    /// to the stack before they get clobbered.
    fn spill_args_across_constraints(
        &mut self,
        _func: &Function,
        intervals: &[LiveInterval],
        constraint_points: &[ConstraintPoint<Reg>],
    ) {
        // For each argument in a register, check if its interval is live across
        // any constraint point that clobbers that register
        let int_arg_regs_set = Reg::arg_regs();
        for interval in intervals {
            if let Some(Loc::Reg(reg)) = self.locations.get(&interval.pseudo) {
                if int_arg_regs_set.contains(reg) {
                    // Check if this register is clobbered by any constraint point
                    // while the interval is live (and the pseudo is not involved)
                    let needs_spill = constraint_points.iter().any(|cp| {
                        interval.start <= cp.position
                            && cp.position <= interval.end
                            && !cp.involved_pseudos.contains(&interval.pseudo)
                            && cp.clobbers.contains(reg)
                    });

                    if needs_spill {
                        let from_reg = *reg;
                        self.stack_offset += 8;
                        let to_stack_offset = self.stack_offset;

                        // Record the spill for codegen to emit stores in prologue
                        self.spilled_args.push(SpilledArg {
                            pseudo: interval.pseudo,
                            from_reg,
                            to_stack_offset,
                        });

                        self.locations
                            .insert(interval.pseudo, Loc::Stack(to_stack_offset));
                        self.free_regs.push(from_reg);
                    }
                }
            }
        }
    }

    /// Force alloca results to stack to avoid clobbering issues
    fn allocate_alloca_to_stack(&mut self, func: &Function) {
        crate::arch::regalloc::assign_alloca_slots(
            func,
            &mut self.stack_offset,
            &mut self.locations,
            Loc::Stack,
        );
    }

    /// Try to reuse a freed stack slot of the given size and alignment.
    /// Uses interference check to ensure the candidate doesn't overlap with the slot's owner.
    fn try_reuse_stack_slot(
        &mut self,
        size: i32,
        alignment: i32,
        candidate_interval: &LiveInterval,
    ) -> Option<(i32, Vec<LiveInterval>)> {
        super::super::regalloc::try_reuse_stack_slot(
            &mut self.free_stack_slots,
            size,
            alignment,
            candidate_interval,
        )
    }

    /// Allocate a stack slot, optionally reusing a freed slot.
    /// Only short-lived spills (no register available, not crossing calls/loops)
    /// should set `reusable=true`. Call-crossing and in-loop spills have
    /// unreliable interval estimates in complex control flow (e.g., computed gotos).
    fn alloc_stack_slot(
        &mut self,
        interval: &LiveInterval,
        size: i32,
        alignment: i32,
        reusable: bool,
    ) {
        // Track maximum alignment for dynamic stack alignment
        if alignment > self.max_local_align {
            self.max_local_align = alignment;
        }
        if reusable {
            if let Some((reused, past)) = self.try_reuse_stack_slot(size, alignment, interval) {
                self.locations.insert(interval.pseudo, Loc::Stack(reused));
                self.active_stack.push(crate::arch::regalloc::ActiveSlot {
                    current: interval.clone(),
                    past,
                    offset: reused,
                    size,
                });
                return;
            }
        }
        if alignment > 8 {
            self.stack_offset = (self.stack_offset + alignment - 1) & !(alignment - 1);
        }
        self.stack_offset += size;
        let offset = self.stack_offset;
        self.locations.insert(interval.pseudo, Loc::Stack(offset));
        if reusable {
            self.active_stack.push(crate::arch::regalloc::ActiveSlot {
                current: interval.clone(),
                past: Vec::new(),
                offset,
                size,
            });
        }
    }

    /// Look up an interval by pseudo id (linear scan over the small
    /// intervals vector; vec is sorted by start position, not pseudo,
    /// so a linear find is fine).
    fn interval_by_pseudo(intervals: &[LiveInterval], p: PseudoId) -> Option<&LiveInterval> {
        intervals.iter().find(|i| i.pseudo == p)
    }

    /// M6 chordal coloring with stub M7 (spill-on-fail).
    ///
    /// Three phases:
    ///   1. Pre-pass: route non-register-allocated pseudos to their
    ///      proper Locs (constants → Imm/FImm, Sym → stack slot,
    ///      int128 → 16-byte stack, long-double → x87 stack, FP
    ///      crossing call/block boundary → spill XMM caller-saved).
    ///      The remaining pseudos go into per-bank candidate sets.
    ///   2. Color: per-bank chordal coloring. ABI-pinned args become
    ///      pre-colored vertices; constraint clobbers (idiv RAX/RDX,
    ///      shift Rcx, varargs RAX) and cross-call → caller-saved
    ///      become per-vertex forbidden colors. In-loop pseudos get a
    ///      callee-first preferred palette (soft, not forbidden).
    ///   3. Commit: write Loc::Reg / Loc::Xmm for colored vertices,
    ///      allocate stack slots for spilled vertices, track
    ///      `used_callee_saved` for the prologue.
    #[allow(clippy::too_many_arguments)]
    fn run_chordal_color(
        &mut self,
        func: &Function,
        types: &TypeTable,
        intervals: Vec<LiveInterval>,
        call_positions: &[usize],
        constraint_points: &[ConstraintPoint<Reg>],
    ) {
        // -------- Phase 1: pre-pass --------
        let mut gp_candidates: std::collections::BTreeSet<PseudoId> =
            std::collections::BTreeSet::new();
        let mut xmm_candidates: std::collections::BTreeSet<PseudoId> =
            std::collections::BTreeSet::new();
        for interval in &intervals {
            // Intervals come pre-sorted by start position from
            // compute_live_intervals, so this monotonic expiration
            // recovers the linear-scan-era slot-reuse behavior the
            // chordal sweep would otherwise lose.
            crate::arch::regalloc::expire_stack_intervals(
                &mut self.active_stack,
                &mut self.free_stack_slots,
                interval.start,
            );
            if self.locations.contains_key(&interval.pseudo) {
                // Already assigned by allocate_arguments / spill_args
                // / alloca passes; arg pseudos become pre-colored
                // graph vertices in Phase 2.
                continue;
            }
            if let Some(pseudo) = func.get_pseudo(interval.pseudo) {
                match &pseudo.kind {
                    PseudoKind::Val(v) => {
                        self.locations.insert(interval.pseudo, Loc::Imm(*v));
                        continue;
                    }
                    PseudoKind::FVal(v) => {
                        let size = func
                            .blocks
                            .iter()
                            .flat_map(|b| &b.insns)
                            .find(|insn| {
                                insn.op == Opcode::SetVal && insn.target == Some(interval.pseudo)
                            })
                            .map(|insn| insn.size)
                            .unwrap_or(64);
                        self.locations.insert(interval.pseudo, Loc::FImm(*v, size));
                        self.fp_pseudos.insert(interval.pseudo);
                        continue;
                    }
                    PseudoKind::Sym(name) => {
                        if let Some(local_var) = func.locals.get(name) {
                            let size = (types.size_bits(local_var.typ) / 8) as i32;
                            let size = size.max(8);
                            let natural_align = types.alignment(local_var.typ) as i32;
                            let alignment = if let Some(explicit) = local_var.explicit_align {
                                explicit as i32
                            } else {
                                natural_align.max(8)
                            };
                            let aligned_size = (size + alignment - 1) & !(alignment - 1);
                            // Sym slot reuse disabled. The IR-level
                            // interval of a Sym pseudo only captures
                            // its direct Store/Load/SymAddr uses,
                            // not the lifetime of register pseudos
                            // that derive their values from the
                            // slot. Linear scan allocated slot
                            // offsets monotonically and so happened
                            // not to reuse Sym slots in conflicting
                            // ways; chordal coloring exposes the
                            // gap. CPython `_warnings.o::init_filters`
                            // and `flowgraph.o::_PyCfgBuilder_Addop`
                            // both miscompiled on the Sym-slot reuse
                            // pattern even with the
                            // [[interval-overlap fix]] (the Sym's
                            // IR interval ends before the derived
                            // register pseudo's lifetime does, so
                            // interval-overlap reports no conflict).
                            //
                            // Future fix: extend the Sym's interval
                            // to cover all derived register pseudos'
                            // lifetimes. Until then, Sym slots are
                            // permanent. The `addr_taken_syms`
                            // computation stays in place — it remains
                            // the correct gating predicate when slot
                            // reuse is re-enabled.
                            let _ = self.addr_taken_syms.contains(&interval.pseudo);
                            let reusable = false;
                            self.alloc_stack_slot(interval, aligned_size, alignment, reusable);
                            if types.is_float(local_var.typ) {
                                self.fp_pseudos.insert(interval.pseudo);
                            }
                        } else {
                            self.locations
                                .insert(interval.pseudo, Loc::Global(name.clone()));
                        }
                        continue;
                    }
                    _ => {}
                }
            }
            if self.int128_pseudos.contains(&interval.pseudo) {
                self.alloc_stack_slot(interval, 16, 16, true);
                continue;
            }
            let needs_fp = self.fp_pseudos.contains(&interval.pseudo);
            if needs_fp {
                let is_longdouble = self.ld_pseudos.contains(&interval.pseudo);
                let crosses_call = interval_crosses_call(interval, call_positions);
                let crosses_block = self.live_out.iter().any(|lo| lo.contains(&interval.pseudo));
                if is_longdouble {
                    self.alloc_stack_slot(interval, 16, 16, false);
                    continue;
                }
                if crosses_call || crosses_block {
                    self.alloc_stack_slot(interval, 8, 8, true);
                    continue;
                }
                xmm_candidates.insert(interval.pseudo);
            } else {
                gp_candidates.insert(interval.pseudo);
            }
        }

        // -------- Phase 2: per-bank chordal coloring --------
        self.color_gp_bank(
            func,
            &intervals,
            call_positions,
            constraint_points,
            &gp_candidates,
        );
        self.color_xmm_bank(func, &intervals, &xmm_candidates);
    }

    fn color_gp_bank(
        &mut self,
        func: &Function,
        intervals: &[LiveInterval],
        call_positions: &[usize],
        constraint_points: &[ConstraintPoint<Reg>],
        gp_candidates: &std::collections::BTreeSet<PseudoId>,
    ) {
        use crate::arch::regalloc::{build_interference_graph, greedy_color, mcs_ordering};
        if gp_candidates.is_empty() {
            return;
        }

        // Pre-colored vertices: any pseudo already mapped to a GP reg
        // (ABI-pinned args from allocate_arguments) plus inline-asm
        // operands with `Fixed(R)` constraints (e.g. `"a"(x)` pins x
        // to RAX). Add them to the graph so live conflicts are
        // respected and the operand lands directly in the
        // constraint-required register — no codegen move needed.
        let mut pre_colored: BTreeMap<PseudoId, Reg> = BTreeMap::new();
        let mut all_vertices: std::collections::BTreeSet<PseudoId> = gp_candidates.clone();
        for (&pid, loc) in self.locations.iter() {
            if let Loc::Reg(r) = loc {
                pre_colored.insert(pid, *r);
                all_vertices.insert(pid);
            }
        }
        for (pid, reg) in collect_asm_fixed_precolors_x86_64(func) {
            // Only pre-color if the pseudo is a GP candidate. The
            // C2 lowering already routes Fixed-operand registers
            // into the ConstraintPoint clobber set, so even if
            // pre-coloring is skipped here the operand remains
            // exempt via `involved_pseudos`.
            if !gp_candidates.contains(&pid) {
                continue;
            }
            pre_colored.entry(pid).or_insert(reg);
            all_vertices.insert(pid);
            // Phase 3's commit loop skips pre-colored vertices on
            // the assumption that their locations are already in
            // `self.locations` (true for ABI-pinned args, which
            // `allocate_arguments` inserts before chordal runs).
            // Inline-asm Fixed pre-colors arrive here without going
            // through `allocate_arguments`, so insert directly. If
            // missed, `get_location` defaults to `Loc::Imm(0)` and
            // every Store/Load involving the operand silently
            // writes/reads zero.
            self.locations.insert(pid, Loc::Reg(reg));
        }

        // GP coloring needs def-vs-src edges: some codegen lowerings
        // (notably x86_64 `cmov` for ternary `(cond)?a:b`) materialize
        // the target into a register and then read source values,
        // which would clobber a source sharing the target's register.
        let graph = build_interference_graph(&all_vertices, func, &self.live_out, true);

        // Forbidden colors. Three sources:
        //   (a) constraint clobbers (idiv RAX/RDX, etc.) — for pseudos
        //       live across the constraint that are NOT operands.
        //   (b) cross-call → all caller-saved registers (HARD
        //       constraint, NOT a preference: the call clobbers them
        //       and the value would be lost).
        //   (c) NB: in-loop is NOT a forbidden constraint, only a
        //       preference (soft) — see preferred_palette below.
        let caller_saved: Vec<Reg> = Reg::allocatable()
            .iter()
            .copied()
            .filter(|r| !r.is_callee_saved())
            .collect();
        let mut forbidden: BTreeMap<PseudoId, std::collections::BTreeSet<Reg>> = BTreeMap::new();
        for cp in constraint_points {
            for interval in intervals {
                if !gp_candidates.contains(&interval.pseudo) {
                    continue;
                }
                if interval.start > cp.position || cp.position > interval.end {
                    continue;
                }
                if cp.involved_pseudos.contains(&interval.pseudo) {
                    continue;
                }
                let entry = forbidden.entry(interval.pseudo).or_default();
                for &c in &cp.clobbers {
                    entry.insert(c);
                }
            }
        }
        let mut in_loop_set: std::collections::BTreeSet<PseudoId> =
            std::collections::BTreeSet::new();
        for interval in intervals {
            if !gp_candidates.contains(&interval.pseudo) {
                continue;
            }
            if interval_crosses_call(interval, call_positions) {
                let entry = forbidden.entry(interval.pseudo).or_default();
                for &r in &caller_saved {
                    entry.insert(r);
                }
            } else if interval.in_loop {
                in_loop_set.insert(interval.pseudo);
            }
        }

        // Preferred palettes: in-loop → callee-saved first, else
        // caller-saved first (to avoid touching callee-saved slots in
        // the prologue/epilogue when not needed).
        let caller_first: Vec<Reg> = {
            let mut v = caller_saved.clone();
            for &r in Reg::allocatable() {
                if r.is_callee_saved() {
                    v.push(r);
                }
            }
            v
        };
        let callee_first: Vec<Reg> = {
            let mut v: Vec<Reg> = Reg::allocatable()
                .iter()
                .copied()
                .filter(|r| r.is_callee_saved())
                .collect();
            for &r in &caller_saved {
                v.push(r);
            }
            v
        };
        let caller_first_c = caller_first.clone();
        let callee_first_c = callee_first.clone();

        let order = mcs_ordering(&graph);
        let result = greedy_color(
            &graph,
            &order,
            Reg::allocatable(),
            &pre_colored,
            &forbidden,
            |v| {
                if in_loop_set.contains(&v) {
                    Some(callee_first_c.clone())
                } else {
                    Some(caller_first_c.clone())
                }
            },
        );

        // -------- M7 Belady eviction --------
        // For each pseudo the greedy pass couldn't place, look at its
        // colored neighbors. If a neighbor's next-use is STRICTLY
        // further than the failing pseudo's, hand the neighbor's
        // register to the failing pseudo and spill the neighbor —
        // Belady's "evict the value used furthest in the future" rule
        // applied at color-failure time. Eviction only succeeds when
        // the neighbor's color is also legal for the failing pseudo
        // (not in its forbidden set, not held by any OTHER neighbor).
        let uses = crate::arch::regalloc::compute_use_positions(func);
        let mut colors = result.colors;
        let mut final_spilled: std::collections::BTreeSet<PseudoId> =
            std::collections::BTreeSet::new();
        for spilled in result.spilled {
            if colors.contains_key(&spilled) {
                continue;
            }
            let interval = match Self::interval_by_pseudo(intervals, spilled) {
                Some(i) => i,
                None => {
                    final_spilled.insert(spilled);
                    continue;
                }
            };
            let empty = std::collections::BTreeSet::new();
            let forbid = forbidden.get(&spilled).unwrap_or(&empty);
            let spilled_next =
                crate::arch::regalloc::next_use_distance(&uses, spilled, interval.start);
            // Snapshot the neighbor list before scanning; we mutate
            // `colors` inside the inner search so the iteration set
            // needs to be stable.
            let neighbors: Vec<PseudoId> = graph.neighbors(spilled).collect();
            let mut best_evict: Option<(PseudoId, Reg, usize)> = None;
            for &n in &neighbors {
                if pre_colored.contains_key(&n) {
                    // Don't evict ABI-pinned args.
                    continue;
                }
                let Some(&color) = colors.get(&n) else {
                    continue;
                };
                if forbid.contains(&color) {
                    continue;
                }
                // Color is legal for `spilled` only if no OTHER
                // neighbor already holds it.
                let conflict = neighbors
                    .iter()
                    .any(|&m| m != n && colors.get(&m).copied() == Some(color));
                if conflict {
                    continue;
                }
                let nd = crate::arch::regalloc::next_use_distance(&uses, n, interval.start);
                if nd > spilled_next && best_evict.is_none_or(|(_, _, d)| nd > d) {
                    best_evict = Some((n, color, nd));
                }
            }
            if let Some((evicted, color, _)) = best_evict {
                colors.remove(&evicted);
                colors.insert(spilled, color);
                final_spilled.insert(evicted);
            } else {
                final_spilled.insert(spilled);
            }
        }

        // -------- Phase 3: commit --------
        for (&pid, &reg) in &colors {
            if pre_colored.contains_key(&pid) {
                continue;
            }
            self.locations.insert(pid, Loc::Reg(reg));
            if reg.is_callee_saved() && !self.used_callee_saved.contains(&reg) {
                self.used_callee_saved.push(reg);
            }
        }
        // Process spill commits in interval.start order with
        // monotonic expiration. Earlier (M6+M7 v1) drained everything
        // with `usize::MAX` then relied on `pseudos_interfere` to
        // gate reuse — but that check uses block-level live_in/out
        // sets which miss within-block interference. A spilled
        // register pseudo whose lifetime sits entirely inside one
        // block has empty live_in/out projections and was happily
        // assigned to a slot owned by a Sym pseudo still alive in
        // the same block (root cause of the CPython
        // `PyThread_acquire_lock_timed` miscompile — slot reused for
        // `_PyTime_Add` result while `thelock` was still live).
        //
        // The monotonic sweep mirrors linear scan's invariant: a
        // slot is only freed once the owning interval has ended,
        // and a new interval's start ≥ the freed slot's owner's
        // end, so they cannot interfere within a block.
        let mut ordered_spilled: Vec<(usize, PseudoId)> = final_spilled
            .iter()
            .filter_map(|&p| Self::interval_by_pseudo(intervals, p).map(|i| (i.start, p)))
            .collect();
        ordered_spilled.sort_by_key(|&(start, _)| start);
        for (start, spilled) in ordered_spilled {
            if self.locations.contains_key(&spilled) {
                continue;
            }
            crate::arch::regalloc::expire_stack_intervals(
                &mut self.active_stack,
                &mut self.free_stack_slots,
                start,
            );
            if let Some(interval) = Self::interval_by_pseudo(intervals, spilled) {
                self.alloc_stack_slot(interval, 8, 8, true);
            }
        }
    }

    fn color_xmm_bank(
        &mut self,
        func: &Function,
        intervals: &[LiveInterval],
        xmm_candidates: &std::collections::BTreeSet<PseudoId>,
    ) {
        use crate::arch::regalloc::{build_interference_graph, greedy_color, mcs_ordering};
        if xmm_candidates.is_empty() {
            return;
        }
        let mut pre_colored: BTreeMap<PseudoId, XmmReg> = BTreeMap::new();
        let mut all_vertices: std::collections::BTreeSet<PseudoId> = xmm_candidates.clone();
        for (&pid, loc) in self.locations.iter() {
            if let Loc::Xmm(r) = loc {
                pre_colored.insert(pid, *r);
                all_vertices.insert(pid);
            }
        }
        // XMM coloring does NOT need def-vs-src edges: SSE FP ops are
        // three-operand at the IR level (target, src1, src2) and the
        // codegen lowers them to either `movsd src1, dst; opsd src2,
        // dst` (which is safe regardless of register sharing) or to
        // AVX/SSE3-style three-operand forms. Adding def-vs-src edges
        // here over-constrains coloring and corrupts FP-heavy code
        // paths (e.g. _Py_dg_strtod's correction loop) in non-obvious
        // ways.
        let graph = build_interference_graph(&all_vertices, func, &self.live_out, false);
        let forbidden: BTreeMap<PseudoId, std::collections::BTreeSet<XmmReg>> = BTreeMap::new();
        let order = mcs_ordering(&graph);
        let result = greedy_color(
            &graph,
            &order,
            XmmReg::allocatable(),
            &pre_colored,
            &forbidden,
            |_| None::<Vec<XmmReg>>,
        );
        for (&pid, &reg) in &result.colors {
            if pre_colored.contains_key(&pid) {
                continue;
            }
            self.locations.insert(pid, Loc::Xmm(reg));
        }
        // Same monotonic-by-start spill commit as `color_gp_bank` —
        // see the comment there for why the earlier `usize::MAX`
        // drain was unsafe.
        let mut ordered_spilled: Vec<(usize, PseudoId)> = result
            .spilled
            .iter()
            .filter_map(|&p| Self::interval_by_pseudo(intervals, p).map(|i| (i.start, p)))
            .collect();
        ordered_spilled.sort_by_key(|&(start, _)| start);
        for (start, spilled) in ordered_spilled {
            if self.locations.contains_key(&spilled) {
                continue;
            }
            crate::arch::regalloc::expire_stack_intervals(
                &mut self.active_stack,
                &mut self.free_stack_slots,
                start,
            );
            if let Some(interval) = Self::interval_by_pseudo(intervals, spilled) {
                self.alloc_stack_slot(interval, 8, 8, true);
            }
        }
    }

    /// Compute live intervals, constraint points, and per-block liveness sets.
    fn compute_live_intervals(&self, func: &Function) -> LivenessResult<Reg> {
        compute_live_intervals(func, get_constraint_info)
    }

    /// Get stack size needed (aligned to max local alignment, minimum 16)
    pub fn stack_size(&self) -> i32 {
        let align = self.max_local_align.max(16);
        (self.stack_offset + align - 1) & !(align - 1)
    }

    /// Get the maximum alignment requirement of any local variable
    pub fn max_local_align(&self) -> i32 {
        self.max_local_align
    }

    /// Get callee-saved registers that need to be preserved
    pub fn callee_saved_used(&self) -> &[Reg] {
        &self.used_callee_saved
    }
}

impl Default for RegAlloc {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_gp_clobber_name_64bit_canonical() {
        assert_eq!(parse_gp_clobber_name("rax"), Some(Reg::Rax));
        assert_eq!(parse_gp_clobber_name("rdx"), Some(Reg::Rdx));
        assert_eq!(parse_gp_clobber_name("r10"), Some(Reg::R10));
        assert_eq!(parse_gp_clobber_name("r15"), Some(Reg::R15));
    }

    #[test]
    fn parse_gp_clobber_name_size_aliases() {
        // 32-bit, 16-bit, 8-bit aliases all resolve to the underlying Reg.
        assert_eq!(parse_gp_clobber_name("eax"), Some(Reg::Rax));
        assert_eq!(parse_gp_clobber_name("ax"), Some(Reg::Rax));
        assert_eq!(parse_gp_clobber_name("al"), Some(Reg::Rax));
        assert_eq!(parse_gp_clobber_name("ah"), Some(Reg::Rax));
        assert_eq!(parse_gp_clobber_name("r10d"), Some(Reg::R10));
        assert_eq!(parse_gp_clobber_name("r10w"), Some(Reg::R10));
        assert_eq!(parse_gp_clobber_name("r10b"), Some(Reg::R10));
    }

    #[test]
    fn parse_gp_clobber_name_leading_percent() {
        // GCC-style %rax is accepted.
        assert_eq!(parse_gp_clobber_name("%rax"), Some(Reg::Rax));
        assert_eq!(parse_gp_clobber_name("%r10d"), Some(Reg::R10));
    }

    #[test]
    fn parse_gp_clobber_name_case_insensitive() {
        assert_eq!(parse_gp_clobber_name("RAX"), Some(Reg::Rax));
        assert_eq!(parse_gp_clobber_name("R10D"), Some(Reg::R10));
    }

    #[test]
    fn parse_gp_clobber_name_unknown() {
        // Special tokens and non-GP names return None — the asm
        // clobber walker filters them out silently. "memory" / "cc"
        // get other treatment (C3 memory barrier; cc is a no-op).
        assert_eq!(parse_gp_clobber_name("memory"), None);
        assert_eq!(parse_gp_clobber_name("cc"), None);
        assert_eq!(parse_gp_clobber_name("xmm0"), None);
        assert_eq!(parse_gp_clobber_name("st0"), None);
        assert_eq!(parse_gp_clobber_name(""), None);
        assert_eq!(parse_gp_clobber_name("not_a_reg"), None);
    }

    #[test]
    fn is_call_like_x86_64_covers_libc_emitters() {
        // Anchor the libc-call-emitting opcodes so a future codegen
        // change that adds a new builtin → libc lowering doesn't
        // silently drop out of the chordal allocator's caller-saved
        // forbidding.
        assert!(is_call_like_x86_64(Opcode::Call));
        assert!(is_call_like_x86_64(Opcode::Longjmp));
        assert!(is_call_like_x86_64(Opcode::Setjmp));
        assert!(is_call_like_x86_64(Opcode::Fabs32));
        assert!(is_call_like_x86_64(Opcode::Fabs64));
        assert!(is_call_like_x86_64(Opcode::Signbit32));
        assert!(is_call_like_x86_64(Opcode::Signbit64));
        assert!(is_call_like_x86_64(Opcode::Memset));
        assert!(is_call_like_x86_64(Opcode::Memcpy));
        assert!(is_call_like_x86_64(Opcode::Memmove));
        // Non-call-like opcodes stay off.
        assert!(!is_call_like_x86_64(Opcode::Add));
        assert!(!is_call_like_x86_64(Opcode::Asm));
    }

    fn make_asm_insn(clobbers: &[&str]) -> Instruction {
        use crate::ir::AsmData;
        let mut insn = Instruction::new(Opcode::Asm);
        insn.asm_data = Some(Box::new(AsmData {
            template: String::new(),
            outputs: Vec::new(),
            inputs: Vec::new(),
            clobbers: clobbers.iter().map(|s| s.to_string()).collect(),
            goto_labels: Vec::new(),
        }));
        insn
    }

    #[test]
    fn build_asm_instr_constraints_x86_64_propagates_memory_barrier() {
        // C6a contract: a `"memory"` clobber on an `Opcode::Asm` must
        // flip `InstrConstraints.memory_barrier`. This is the load-
        // bearing flag that future memory-reordering passes (GVN,
        // LICM, machine scheduler) will consult before crossing.
        let with_mem = make_asm_insn(&["rax", "memory", "cc"]);
        let ic = build_asm_instr_constraints_x86_64(&with_mem).expect("has asm_data");
        assert!(ic.memory_barrier, "\"memory\" clobber must set the flag");

        // Conversely, non-memory clobbers leave the flag clear.
        let no_mem = make_asm_insn(&["rax", "cc"]);
        let ic = build_asm_instr_constraints_x86_64(&no_mem).expect("has asm_data");
        assert!(
            !ic.memory_barrier,
            "asm without \"memory\" clobber must not be a barrier"
        );

        // Empty clobber list: also not a barrier.
        let bare = make_asm_insn(&[]);
        let ic = build_asm_instr_constraints_x86_64(&bare).expect("has asm_data");
        assert!(!ic.memory_barrier);
    }
}
