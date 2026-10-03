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
//
// An `ms_abi` function's arguments all live in its incoming area instead,
// and it saves Rsi, Rdi and Xmm6-Xmm15 as well (see win64.rs). Every call is
// taken to clobber the System V set, which covers both conventions.
// ============================================================================

use super::x87::{is_x87_float_to_int, uses_x87_scratch};
use crate::arch::asm_constraints::{AsmOperandClass, AsmRegClass, PinnedGp};
use crate::arch::lir::FpSize;
use crate::arch::regalloc::{
    compute_live_intervals, find_call_positions, identify_fp_pseudos, interval_crosses_call,
    ConstraintPoint, FreeSlot, LiveInterval, LivenessResult,
};
use crate::float::FloatVal;
use crate::ir::{AsmConstraint, AsmData, Function, Instruction, Opcode, PseudoId, PseudoKind};
use crate::types::TypeTable;
use std::collections::{BTreeMap, HashMap, HashSet};

// x86-64 Register Definitions

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

    pub fn name_for_size(&self, bits: u32) -> &'static str {
        match bits {
            8 => self.name8(),
            16 => self.name16(),
            32 => self.name32(),
            _ => self.name64(),
        }
    }

    /// This register's DWARF number, for debug location expressions.
    ///
    /// The System V AMD64 psABI fixes the mapping (figure 3.36) and it is not
    /// the encoding order: `%rbx` is 3 and `%rcx` is 2, where the instruction
    /// encoding has them the other way round.
    pub fn dwarf_number(&self) -> u16 {
        match self {
            Reg::Rax => 0,
            Reg::Rdx => 1,
            Reg::Rcx => 2,
            Reg::Rbx => 3,
            Reg::Rsi => 4,
            Reg::Rdi => 5,
            Reg::Rbp => 6,
            Reg::Rsp => 7,
            Reg::R8 => 8,
            Reg::R9 => 9,
            Reg::R10 => 10,
            Reg::R11 => 11,
            Reg::R12 => 12,
            Reg::R13 => 13,
            Reg::R14 => 14,
            Reg::R15 => 15,
        }
    }

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

/// Spend `n` argument registers out of a file of `file_len`.
///
/// These counters hold the number of registers *consumed* so far, and System
/// V 3.2.3 step 5 gives an argument that does not fit no registers at all --
/// so the count can never exceed the file. Adding unconditionally on the
/// memory path kept the answer right for a `used < file_len` test and wrong
/// for a `used + needed <= file_len` one when `needed` is zero: a
/// two-general-eightbyte aggregate arriving after nine stacked `double`s
/// asked whether the SSE file had room for none of it, and was told no.
pub(super) fn spend_arg_regs(used: &mut usize, n: usize, file_len: usize) {
    *used = (*used + n).min(file_len);
}

// Register Constraints (LLVM-style constraint-aware allocation)

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
        //
        // `UMulHi` is the *other* half of that same `mulq`, and it was missing
        // here. The 128-bit product expansion reads `a_lo` twice -- once for
        // `umulhi(a_lo, b_lo)` and again for the cross term `a_lo * b_hi` --
        // so an `a_lo` parked in RAX was destroyed by the multiply and the
        // cross term computed from the low product instead. The fault only
        // showed when `b_hi` was non-zero, because otherwise that term is
        // multiplied by zero.
        Opcode::Mul | Opcode::UMulHi => RegConstraints {
            clobbers: &[Reg::Rax, Reg::Rdx],
        },
        // The TLS descriptor sequence hard-uses %rax: the `@TLSCALL`
        // relocation names it, and the resolver returns through it. Nothing
        // else is clobbered -- the resolver preserves every other register,
        // which is why this is a plain clobber here and deliberately *not* an
        // entry in `is_call_like_x86_64`. Declaring it call-like would spill
        // every live floating-point value to the stack and spill argument
        // registers, for a sequence that needs neither.
        Opcode::TlsAddr => RegConstraints {
            clobbers: &[Reg::Rax],
        },
        Opcode::Shl | Opcode::Lsr | Opcode::Asr => RegConstraints {
            clobbers: &[Reg::Rcx],
        },
        Opcode::VaArg => RegConstraints {
            clobbers: &[Reg::Rax, Reg::Rcx],
        },
        // The atomic emitters use RAX/RCX as fixed scratch (and R8/R9 for the
        // CAS success flag and expected-object address), all of which are
        // allocatable. Undeclared, any
        // pseudo the allocator parked there whose live range crossed an atomic
        // operation was silently corrupted -- six live ints bracketing one
        // __c11_atomic_fetch_add summed to 22 instead of 31.
        Opcode::AtomicLoad
        | Opcode::AtomicStore
        | Opcode::AtomicSwap
        | Opcode::AtomicCas
        | Opcode::AtomicFetchAdd
        | Opcode::AtomicFetchSub
        | Opcode::AtomicFetchAnd
        | Opcode::AtomicFetchOr
        | Opcode::AtomicFetchXor => RegConstraints {
            clobbers: &[Reg::Rax, Reg::Rcx, Reg::R8, Reg::R9],
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
    // **Infrastructure only — see the note below on inter-instruction
    // scratch use for why R10/R11 are still in the reserved-scratch
    // list.** This predicate is the extension point: adding R10/R11 to
    // `Reg::allocatable()` starts producing ConstraintPoint forbiddings
    // without changing `get_constraint_info`.
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
    // - `Fence` → `mfence` or nothing.
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

// The per-IR-opcode constraint model is **not sufficient** on its own:
// R10/R11 stay reserved because the prologue's `rep stosq`, the
// variadic save area, the spilled-arg restore, and several
// multi-instruction FP and struct lowerings all use them across
// IR-instruction boundaries.

/// Opcodes whose x86_64 codegen lowering invokes an external function
/// (libc or otherwise) and therefore clobbers caller-saved registers.
/// Used by `find_call_positions` to drive the chordal allocator's
/// cross-call caller-saved forbidding.
///
/// Beyond the obvious `Call` / `Longjmp` / `Setjmp`:
/// - `Memset` / `Memcpy` / `Memmove` → libc memset/memcpy/memmove
///   (features.rs)
pub fn is_call_like_x86_64(op: Opcode) -> bool {
    matches!(
        op,
        Opcode::Call
            | Opcode::Longjmp
            | Opcode::Setjmp
            | Opcode::Memset
            | Opcode::Memcpy
            | Opcode::Memmove
    )
}

/// The register each operand of an inline-asm statement is pinned to --
/// outputs, then inputs, in order -- or `None` for one the allocator places.
///
/// A letter that names a register (`a` ... `D`) pins its operand there. `Q`
/// asks for any register with an addressable high byte, and gets the first of
/// %rax, %rbx, %rcx, %rdx the statement neither pins nor clobbers: the
/// allocator has no such class, so the choice is made here, once, and both
/// the allocator and the template substitution read it. A tied input takes
/// its output's register.
pub fn asm_pinned_regs(asm: &AsmData) -> Vec<Option<Reg>> {
    let named = |c: &AsmOperandClass| match c.reg {
        Some(AsmRegClass::Pinned(p)) => Some(match p {
            PinnedGp::Rax => Reg::Rax,
            PinnedGp::Rbx => Reg::Rbx,
            PinnedGp::Rcx => Reg::Rcx,
            PinnedGp::Rdx => Reg::Rdx,
            PinnedGp::Rsi => Reg::Rsi,
            PinnedGp::Rdi => Reg::Rdi,
        }),
        _ => None,
    };
    let operands: Vec<&AsmConstraint> = asm.outputs.iter().chain(&asm.inputs).collect();
    let mut taken: Vec<Reg> = operands.iter().filter_map(|c| named(&c.class)).collect();
    taken.extend(asm.clobbers.iter().filter_map(|c| parse_gp_clobber_name(c)));
    let mut pins: Vec<Option<Reg>> = Vec::with_capacity(operands.len());
    for (idx, c) in operands.iter().enumerate() {
        let tied = c
            .matching_output
            .filter(|&i| idx >= asm.outputs.len() && i < asm.outputs.len());
        let pin = if let Some(i) = tied {
            pins[i]
        } else if c.class.reg == Some(AsmRegClass::HighByte) {
            let free = [Reg::Rax, Reg::Rbx, Reg::Rcx, Reg::Rdx]
                .into_iter()
                .find(|r| !taken.contains(r));
            taken.extend(free);
            free
        } else {
            named(&c.class)
        };
        pins.push(pin);
    }
    pins
}

/// Map a clobber-list register name (lowercase, GCC-style) to the
/// corresponding `Reg`. Accepts the 64-bit canonical name (`rax`,
/// `r10`, ...), the 32/16/8-bit alias (`eax`, `ax`, `al`, `r10d`,
/// `r10w`, `r10b`), and the GCC-style "%rax" with leading `%`.
/// Returns `None` for names the GP table doesn't know about
/// (XMM registers, `memory`, `cc`, x87 stack, ...).
pub(super) fn parse_gp_clobber_name(raw: &str) -> Option<Reg> {
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

/// The allocator's view of an inline-asm instruction: its pinned operands
/// and its clobbers. `None` if the instruction has no `AsmData`.
pub fn build_asm_instr_constraints_x86_64(
    insn: &Instruction,
) -> Option<crate::arch::asm_constraints::InstrConstraints<Reg>> {
    let asm = insn.extra().asm_data.as_ref()?;
    Some(crate::arch::asm_constraints::InstrConstraints::of_asm(
        asm,
        &asm_pinned_regs(asm),
        parse_gp_clobber_name,
    ))
}

/// Walk a function's inline-asm instructions and collect
/// `(operand_pseudo, fixed_reg)` pairs from every pinned operand
/// (see `asm_pinned_regs`). Used by the chordal allocator to pre-color those
/// operands so they land in the constraint-required register
/// directly instead of being placed elsewhere and moved into the
/// fixed register by the inline-asm codegen.
///
/// The codegen's "move-into-fixed-register-around-asm" path stays
/// in place as a fallback — if pre-coloring conflicts with an
/// earlier ABI pin and the allocator can't honor it, the codegen
/// still emits the corrective move.
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
            for &(pseudo, r) in &ic.pinned {
                // First pin seen wins. Duplicate pins on the same pseudo
                // across multiple asm blocks would be a source bug; ignore
                // the second pin.
                out.entry(pseudo).or_insert(r);
            }
        }
    }
    out
}

/// `tls` is how this target obtains a thread-local's address, which decides
/// what a `TlsAddr` clobbers: the ELF descriptor call hard-uses only %rax,
/// while the Mach-O TLV getter is also passed its descriptor in %rdi.
pub fn get_constraint_info(
    insn: &Instruction,
    tls: crate::target::TlsAccess,
) -> Option<(Vec<Reg>, Vec<PseudoId>)> {
    // Inline asm: route through the per-operand constraint vocabulary
    // and lower the result back to ConstraintPoint for the chordal
    // allocator.
    if insn.op == Opcode::Asm {
        let ic = build_asm_instr_constraints_x86_64(insn)?;
        let (mut clobbers, involved) = ic.to_constraint_point();
        // The R10/R11 scratch clobbers apply to the inline-asm path
        // too: `emit_inline_asm` in `cc/arch/x86_64/codegen.rs` uses
        // R10/R11 to shuffle operands into pinned registers,
        // remap allocated regs that collide with the reserved-scratch
        // set (`find_temp_reg` falls back to R10 / R11), and host
        // input/output spill helpers around the asm body.
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

    // Opcode-level hardware constraints, plus the R10/R11 scratch
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
    // The Mach-O TLV getter preserves every general register but %rax, which
    // returns the address, and %rdi, which carries the descriptor in --
    // LLVM's `CSR_64_TLS_Darwin` keeps the callee-saved set plus %rcx, %rdx,
    // %rsi and %r8-%r11. It keeps no XMM register; see
    // `RegAlloc::fp_call_positions`.
    if insn.op == Opcode::TlsAddr && tls == crate::target::TlsAccess::MachOTlv {
        clobbers.push(Reg::Rdi);
    }
    clobbers.sort();
    clobbers.dedup();

    let mut involved = Vec::new();
    if let Some(t) = insn.target {
        involved.push(t);
    }
    // An instruction's own sources are normally exempt from its clobber set,
    // because the codegen helper reads them before touching its scratch
    // registers. That is false for VaArg and for the atomic operations: both
    // write their scratch registers while the source values are still needed,
    // so a source parked in one of them is destroyed.
    if insn.op != Opcode::VaArg && !insn.op.is_atomic() {
        involved.extend(insn.src.iter().copied());
    }

    Some((clobbers, involved))
}

// XMM Register Definitions (SSE/FP)

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

    /// The DWARF register number: XMM0-XMM15 are 17-32 on x86-64.
    pub fn dwarf_number(&self) -> u16 {
        17 + *self as u16
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

// Operand - Location of a value (register or memory)

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
    FImm(FloatVal, u32),
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
    /// The move width. A `__float128` argument is a whole XMM; storing it as
    /// a `double` dropped its top half.
    pub size: FpSize,
}

// Register Allocator (Linear Scan)

/// Simple linear scan register allocator for x86-64
pub struct RegAlloc {
    /// Mapping from pseudo to location
    locations: HashMap<PseudoId, Loc>,
    /// How the target obtains a thread-local's address, which decides what a
    /// `TlsAddr` clobbers. Set with [`RegAlloc::with_tls_access`].
    tls_access: crate::target::TlsAccess,
    /// How many GP argument registers the **named** parameters consumed, capped
    /// at the register file size. `va_start` needs this to seed `gp_offset`.
    named_gp_regs: usize,
    /// The same for SSE argument registers, seeding `fp_offset`.
    named_fp_regs: usize,
    /// The `%rbp` displacement just past the last **named** stacked parameter,
    /// i.e. where the variadic arguments begin. `va_start` stores this as
    /// `overflow_arg_area`.
    ///
    /// It is taken from `allocate_arguments` rather than recomputed, because
    /// only that loop applies the full System V AMD64 psABI section 3.2.3
    /// dispatch -- X87 and MEMORY classes occupy real bytes here while
    /// consuming no register, and step 5 charges nothing for an argument that
    /// did not fit. A separate tally of "how many registers overflowed" can
    /// express neither, nor the alignment padding `IncomingOff::take` inserts.
    named_incoming_end: i32,
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
    /// Reserved when the function converts a long double to an integer.
    x87_scratch: Option<X87Scratch>,
    x87_control_words: Option<X87ControlWords>,
    /// A source position for this function, for the frame diagnostics
    /// `grow_frame` and [`crate::abi::slot_bytes`] emit. `alloc_stack_slot` and
    /// `IncomingOff::take` have no `&Function` to recover one from.
    func_pos: crate::diag::Position,
    /// Callee-saved registers that were used
    used_callee_saved: Vec<Reg>,
    /// The XMM registers an `ms_abi` function must save and restore.
    win64_xmm_saves: Vec<XmmReg>,
    /// Track which pseudos need FP registers (based on type)
    fp_pseudos: HashSet<PseudoId>,
    /// Track which pseudos are long double (use x87, need 16-byte stack slots)
    ld_pseudos: HashSet<PseudoId>,
    /// Pseudos holding an IEEE binary128 value (`__float128`). They occupy a
    /// whole XMM register and a 16-byte stack slot, unlike every other SSE
    /// value here.
    quad_pseudos: HashSet<PseudoId>,
    /// Track which pseudos are 128-bit integers (need 16-byte stack slots, never registers)
    int128_pseudos: HashSet<PseudoId>,
    /// Where a stack-passed `__int128` parameter arrives, by pseudo.
    ///
    /// Such a parameter is given a *local* sixteen-byte slot as its location,
    /// because that is where the prologue copies it to; the caller's offset has
    /// nowhere else to live, and the prologue must not recompute it.
    int128_incoming: BTreeMap<PseudoId, i32>,
    /// Arguments spilled from caller-saved registers to stack
    spilled_args: Vec<SpilledArg>,
    /// XMM arguments spilled from XMM registers to stack
    spilled_xmm_args: Vec<SpilledXmmArg>,
    /// Active stack slot intervals (interval, offset, size) for reuse tracking
    active_stack: Vec<crate::arch::regalloc::ActiveSlot>,
    /// Free stack slots keyed by size, available for reuse
    free_stack_slots: BTreeMap<i32, Vec<FreeSlot>>,
    /// Per-block live-in sets for interference-based stack coloring
    live_in: Vec<HashSet<PseudoId>>,
    /// Per-block live-out sets for interference-based stack coloring
    live_out: Vec<HashSet<PseudoId>>,
    /// Maximum alignment requirement of any local variable (for dynamic stack alignment)
    max_local_align: i32,
    /// How this function's locals are addressed.
    frame_base: FrameBase,
}

/// How a function's locals are addressed.
///
/// The aarch64 allocator carries the same decision under the same name, and
/// for the same reason: `%rbp`/`x29` is only guaranteed 16-byte aligned, so a
/// local wanting more has to be reached from a base the prologue rounds up.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FrameBase {
    /// The frame pointer. Every local is satisfied by the stack's own
    /// alignment, so no register is spent.
    Rbp,
    /// `reg` holds the locals area's `align`-rounded start, and every local is
    /// addressed from there. `reg` must not be allocatable.
    ///
    /// A register rather than `%rsp` itself, which is what this used to be.
    /// `%rsp` is the right *value* -- the prologue's `andq` puts it exactly
    /// here -- but it does not stay that value: `alloca` subtracts from it, and
    /// every local in the function then moved out from under its own address.
    /// A function needs no `alloca` of its own to be hit, because the inliner
    /// splices callees that have one into callers that do not.
    Aligned { reg: Reg, align: i32 },
}

impl FrameBase {
    /// Registers this function's inline asm claims for itself.
    ///
    /// Both spellings count: an explicit clobber, and a constraint letter that
    /// pins an operand to a fixed register. Withholding the frame base from
    /// `allocatable_regs` does not cover either -- asm pins bypass allocation
    /// entirely, and a clobber only reaches the constraint points, which spill
    /// *pseudos*. The frame base is not a pseudo.
    fn asm_claimed_regs(func: &Function) -> std::collections::BTreeSet<Reg> {
        let mut claimed = std::collections::BTreeSet::new();
        for block in &func.blocks {
            for insn in &block.insns {
                if insn.op != Opcode::Asm {
                    continue;
                }
                let Some(ic) = build_asm_instr_constraints_x86_64(insn) else {
                    continue;
                };
                claimed.extend(ic.clobbers.iter().copied());
                claimed.extend(ic.pinned.iter().map(|&(_, r)| r));
            }
        }
        claimed
    }

    /// Decide from the function's declared locals.
    ///
    /// The alignment expression must stay in step with the one the `Sym` arm
    /// applies when it lays a slot out, or the frame would be padded for one
    /// alignment and addressed for another.
    ///
    /// The base must be callee-saved -- it has to survive the calls the
    /// function makes -- and it must be one the function's own inline asm has
    /// not claimed. The prologue writes it once and every local is addressed
    /// from it for the rest of the body, so an `asm` that clobbers it or pins
    /// an operand to it invalidates every local at a stroke: hard-coding `%rbx`
    /// made `asm("..." ::: "rbx")` beside an over-aligned array segfault on
    /// code gcc accepts.
    fn of(func: &Function, types: &TypeTable) -> FrameBase {
        let local_align = func
            .locals
            .values()
            .map(|local| {
                local
                    .explicit_align
                    .map(|a| a as i32)
                    .unwrap_or_else(|| (types.alignment(local.typ) as i32).max(8))
            })
            .max()
            .unwrap_or(8);
        // An outgoing argument more aligned than the call boundary needs the
        // *area* it sits in to start on that alignment, and the area starts at
        // `%rsp` less the reserved bytes. Only a realigned frame makes `%rsp`
        // a known multiple of anything above 16, so a call passing such an
        // argument realigns this function for the same reason an over-aligned
        // local does. `classify_call_args` rounds the reservation to match.
        let call_align = func
            .blocks
            .iter()
            .flat_map(|b| b.insns.iter())
            .filter(|insn| matches!(insn.op, Opcode::Call))
            .flat_map(|insn| insn.extra().arg_types.iter())
            .map(|t| types.alignment(*t) as i32)
            .max()
            .unwrap_or(8);
        let align = local_align.max(call_align);
        if align <= 16 {
            return FrameBase::Rbp;
        }

        let claimed = Self::asm_claimed_regs(func);
        // %rbp is the frame pointer and %rsp the stack pointer; the rest of the
        // callee-saved set is fair game, in the order the allocator would reach
        // for them last.
        const CANDIDATES: [Reg; 5] = [Reg::Rbx, Reg::R12, Reg::R13, Reg::R14, Reg::R15];
        match CANDIDATES.iter().find(|r| !claimed.contains(r)) {
            Some(&reg) => FrameBase::Aligned { reg, align },
            None => {
                // Every candidate is spoken for. Refusing is the only honest
                // answer: there is no register left to hold the aligned base,
                // and silently reusing one would corrupt every local.
                let pos = func
                    .blocks
                    .iter()
                    .flat_map(|b| b.insns.iter())
                    .find(|i| i.op == Opcode::Asm)
                    .and_then(|i| i.pos)
                    .unwrap_or_default();
                crate::diag::error(
                    pos,
                    "inline asm claims every callee-saved register, leaving none \
                     to address this function's over-aligned locals",
                );
                FrameBase::Rbp
            }
        }
    }

    /// The register held back from allocation, if any.
    pub fn reg(self) -> Option<Reg> {
        match self {
            FrameBase::Rbp => None,
            FrameBase::Aligned { reg, .. } => Some(reg),
        }
    }
}

/// Whether a constraint point may leave `interval`'s pseudo in a register
/// it clobbers: only an operand of the instruction, and only at the ends of
/// its live range (see [`ConstraintPoint::operand_survives`]). The one rule
/// for x86-64, asked both when coloring and when deciding whether an
/// ABI-pinned argument must leave its register.
fn exempt_from_clobber(cp: &ConstraintPoint<Reg>, interval: &LiveInterval) -> bool {
    cp.operand_survives(interval.pseudo, interval.start, interval.end)
}

/// The frame slot `x87.rs` stages a value through on its way into or out of
/// the FPU -- `fild` and `fld` have no register form, so an immediate or a
/// general register goes through memory.
///
/// Reserved for a function with an instruction that
/// [`super::x87::uses_x87_scratch`], and only for those; every other frame
/// starts its locals at zero. See [`X86_64CodeGen::x87_scratch_addr`], the
/// one way to address it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct X87Scratch {
    slot: i32,
}

impl X87Scratch {
    /// Two eight-byte halves: `emit_x87_float_to_u64` uses both at once.
    pub(super) const BYTES: i32 = 16;

    pub(super) fn slot(self) -> i32 {
        self.slot
    }
}

/// The frame slot holding the two x87 control words a long double to integer
/// conversion switches between.
///
/// `fistp` rounds as the control word says, and C truncates, so every such
/// conversion saves the control word it finds, stores a copy with rounding
/// control set to truncate, loads that around the `fistp`, and loads the saved
/// one back -- `fisttp`, which truncates unaided, is SSE3 and so not baseline.
/// The two words live in one slot the allocator reserves for any function
/// containing such a conversion, and only for those.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct X87ControlWords {
    slot: i32,
}

impl X87ControlWords {
    /// Bytes reserved: the two 16-bit words, rounded up to the eight every
    /// other slot is laid out in, so the slots after this one stay aligned.
    const BYTES: i32 = 8;

    /// The stack slot the two words are fields of.
    pub(super) fn slot(self) -> i32 {
        self.slot
    }

    /// Byte offset of the control word found on entry, restored afterwards.
    pub(super) const SAVED: i32 = 0;

    /// Byte offset of the truncating copy loaded around the `fistp`.
    pub(super) const TRUNCATING: i32 = 2;
}

/// Where the next stack-passed argument starts, measured from `%rbp` in the
/// callee's frame.
///
/// SysV AMD64 §3.2.3 places each stacked argument at an address rounded up to
/// `max(8, alignof(type))`, so an argument's own alignment decides where it
/// begins, not just the running total. A distinct type keeps every arm going
/// through `IncomingOff::take`, which applies the rounding; aarch64's
/// `IncomingOff::take` is the same shape.
#[derive(Clone, Copy)]
struct IncomingOff(i32);

impl IncomingOff {
    /// The first stacked argument sits above the saved `%rbp` and the return
    /// address.
    const FIRST: IncomingOff = IncomingOff(16);

    /// Reserve `bytes` for an argument of alignment `align`, returning where it
    /// starts and leaving `next` past its end.
    ///
    /// The rounding is applied to the offset **within the argument area**, not
    /// to the `%rbp` displacement. They are not the same thing: the
    /// displacement already carries the saved `%rbp` and the return address, so
    /// rounding it directly charged an over-aligned argument for that 16 bytes
    /// and started it a whole alignment unit too high. A 32-byte-aligned struct
    /// arriving first went to `32(%rbp)` while the caller -- which measures
    /// from the outgoing area's own base, correctly -- had written it at
    /// `16(%rbp)`, so the callee read the second half of the struct and ran off
    /// its end.
    ///
    /// It went unnoticed because it is invisible below 32-byte alignment: 16 is
    /// already a multiple of 8 and of 16, so only an argument wanting more than
    /// the area's own alignment can tell the two bases apart.
    ///
    /// Summed in `i64` and saturated: two stacked arguments each inside the
    /// frame ceiling can still overflow their total. The caller checks the
    /// end once, with [`crate::arch::regalloc::check_incoming_area`].
    fn take(next: &mut IncomingOff, bytes: i32, align: i32) -> i32 {
        let align = i64::from(align.max(8));
        let base = i64::from(IncomingOff::FIRST.0);
        let at = base + ((i64::from(next.0) - base + align - 1) & !(align - 1));
        let end = at + ((i64::from(bytes) + 7) & !7);
        next.0 = i32::try_from(end).unwrap_or(i32::MAX);
        i32::try_from(at).unwrap_or(i32::MAX)
    }
}

impl RegAlloc {
    /// The allocator for a target whose thread-locals are reached by
    /// `access`; see [`crate::target::Target::tls_access`].
    pub fn with_tls_access(mut self, access: crate::target::TlsAccess) -> Self {
        self.tls_access = access;
        self
    }

    pub fn new() -> Self {
        Self {
            locations: HashMap::new(),
            named_gp_regs: 0,
            named_fp_regs: 0,
            named_incoming_end: IncomingOff::FIRST.0,
            free_regs: Reg::allocatable().to_vec(),
            free_xmm_regs: XmmReg::allocatable().to_vec(),
            active: Vec::new(),
            active_xmm: Vec::new(),
            stack_offset: 0,
            x87_scratch: None,
            x87_control_words: None,
            func_pos: crate::diag::Position::default(),
            used_callee_saved: Vec::new(),
            win64_xmm_saves: Vec::new(),
            fp_pseudos: HashSet::new(),
            ld_pseudos: HashSet::new(),
            quad_pseudos: HashSet::new(),
            int128_pseudos: HashSet::new(),
            int128_incoming: BTreeMap::new(),
            spilled_args: Vec::new(),
            spilled_xmm_args: Vec::new(),
            active_stack: Vec::new(),
            free_stack_slots: BTreeMap::new(),
            live_in: Vec::new(),
            live_out: Vec::new(),
            max_local_align: 8,
            frame_base: FrameBase::Rbp,
            tls_access: crate::target::TlsAccess::ElfStatic,
        }
    }

    /// Perform register allocation for a function
    pub fn allocate(
        &mut self,
        func: &Function,
        types: &TypeTable,
    ) -> crate::arch::regalloc::LocationMap<Loc> {
        self.reset_state();
        self.func_pos = crate::arch::func_pos(func);
        // Before anything is coloured: the frame base claims a register, and
        // every palette below has to be built without it.
        self.frame_base = FrameBase::of(func, types);
        if let Some(base) = self.frame_base.reg() {
            self.free_regs.retain(|r| *r != base);
            // The prologue writes it, so the function must save it.
            self.used_callee_saved.push(base);
        }
        let win64 = func.conv == crate::abi::CallingConv::Win64;
        if win64 {
            self.used_callee_saved
                .extend_from_slice(&super::win64::CALLEE_SAVED_GP);
        }
        // Use shared identify_fp_pseudos with type-checker closure
        self.fp_pseudos = identify_fp_pseudos(func, |typ| types.is_float(typ));
        // Identify long double pseudos (use x87 not XMM)
        self.identify_ld_pseudos(func, types);
        self.reserve_x87_frame(func, types);
        self.identify_x87_asm_operands(func);
        self.identify_sse_asm_operands(func);
        self.identify_quad_pseudos(func, types);
        // Identify 128-bit integer pseudos (always spill to 16-byte stack slots)
        self.identify_int128_pseudos(func, types);
        if win64 {
            self.allocate_win64_arguments(func);
        } else {
            self.allocate_arguments(func, types);
        }

        let result = self.compute_live_intervals(func);
        self.live_in = result.live_in;
        self.live_out = result.live_out;
        let intervals = result.intervals;
        let constraint_points = result.constraint_points;
        let call_positions = find_call_positions(func, is_call_like_x86_64);
        let fp_call_positions = self.fp_call_positions(func, &call_positions);

        self.spill_args_across_calls(func, types, &intervals, &call_positions);
        self.spill_gp_args(&intervals, |interval, reg| {
            crate::arch::regalloc::clobbered_while_live(
                interval,
                reg,
                &constraint_points,
                exempt_from_clobber,
            )
        });
        self.allocate_alloca_to_stack(func);
        self.place_locals(func, types, &intervals);
        self.run_chordal_color(
            func,
            intervals,
            &call_positions,
            &fp_call_positions,
            &constraint_points,
        );
        if win64 {
            self.win64_xmm_saves = self.win64_xmm_to_save(func);
        }

        crate::arch::regalloc::LocationMap::from(self.locations.clone())
    }

    /// Give every argument of an `ms_abi` function its position's slot in
    /// the incoming area -- the shadow slot for the first four, which the
    /// prologue homes them to (`win64::homed_positions`). Each then lives in
    /// memory for the whole function, like any stacked argument: nothing has
    /// to track which register it arrived in, and nothing the body does can
    /// overwrite it there.
    fn allocate_win64_arguments(&mut self, func: &Function) {
        for p in &func.pseudos {
            if let PseudoKind::Arg(n) = p.kind {
                let at = super::win64::incoming_offset(n as usize);
                self.locations.insert(p.id, Loc::IncomingArg(at));
            }
        }
        let positions = func.params.len() + usize::from(func.sret_arg().is_some());
        self.named_incoming_end = super::win64::incoming_offset(positions);
        crate::arch::regalloc::check_incoming_area(self.named_incoming_end, self.func_pos);
    }

    /// The XMM registers an `ms_abi` function has to save: every one Win64
    /// makes callee-saved that the function may write.
    ///
    /// That is all ten once it calls anything -- the callee may be System V,
    /// which preserves none of them, and a `memcpy` is a call too -- or runs
    /// inline asm. Otherwise it is the ones the allocator handed out, and the
    /// two scratch registers, which the back end writes without asking it.
    fn win64_xmm_to_save(&self, func: &Function) -> Vec<XmmReg> {
        let calls = func.blocks.iter().flat_map(|b| &b.insns).any(|insn| {
            is_call_like_x86_64(insn.op) || matches!(insn.op, Opcode::Asm | Opcode::TlsAddr)
        });
        let allocated: HashSet<XmmReg> = self
            .locations
            .values()
            .filter_map(|loc| match loc {
                Loc::Xmm(x) => Some(*x),
                _ => None,
            })
            .collect();
        super::win64::CALLEE_SAVED_XMM
            .iter()
            .copied()
            .filter(|x| {
                calls || matches!(x, XmmReg::Xmm14 | XmmReg::Xmm15) || allocated.contains(x)
            })
            .collect()
    }

    /// The XMM registers an `ms_abi` function saves; empty for any other.
    pub fn win64_xmm_saves(&self) -> &[XmmReg] {
        &self.win64_xmm_saves
    }

    /// The positions that destroy every XMM register: the calls, and on
    /// Darwin also each `TlsAddr`.
    ///
    /// The Mach-O TLV getter preserves most general registers, which
    /// `get_constraint_info` models as a narrow clobber, but no XMM register
    /// (LLVM's `CSR_64_TLS_Darwin` lists none). A floating-point value live
    /// across it has to be where one is live across a call: on the stack.
    /// The ELF descriptor resolver preserves them all, so there it adds
    /// nothing.
    fn fp_call_positions(&self, func: &Function, call_positions: &[usize]) -> Vec<usize> {
        if self.tls_access != crate::target::TlsAccess::MachOTlv {
            return call_positions.to_vec();
        }
        find_call_positions(func, |op| is_call_like_x86_64(op) || op == Opcode::TlsAddr)
    }

    /// Reset allocator state for a new function
    fn reset_state(&mut self) {
        self.locations.clear();
        self.free_regs = Reg::allocatable().to_vec();
        self.free_xmm_regs = XmmReg::allocatable().to_vec();
        self.active.clear();
        self.active_xmm.clear();
        self.stack_offset = 0;
        self.x87_scratch = None;
        self.x87_control_words = None;
        self.used_callee_saved.clear();
        self.win64_xmm_saves.clear();
        self.fp_pseudos.clear();
        self.ld_pseudos.clear();
        self.quad_pseudos.clear();
        self.int128_pseudos.clear();
        self.int128_incoming.clear();
        self.spilled_args.clear();
        self.spilled_xmm_args.clear();
        self.active_stack.clear();
        self.free_stack_slots.clear();
        self.live_in.clear();
        self.live_out.clear();
        self.max_local_align = 8;
    }

    /// Reserve the [`X87Scratch`] if an instruction of `func` stages a value
    /// through it, and the [`X87ControlWords`] if one converts a long double
    /// to an integer.
    fn reserve_x87_frame(&mut self, func: &Function, types: &TypeTable) {
        let insns = || func.blocks.iter().flat_map(|block| &block.insns);
        let frame_align = self.frame_align();
        let mut reserve = |bytes: i32| {
            crate::arch::regalloc::grow_frame(
                &mut self.stack_offset,
                bytes,
                bytes,
                frame_align,
                self.func_pos,
            )
        };
        if insns().any(|insn| uses_x87_scratch(insn, types)) {
            let slot = reserve(X87Scratch::BYTES);
            self.x87_scratch = Some(X87Scratch { slot });
        }
        if insns().any(|insn| is_x87_float_to_int(insn, types)) {
            let slot = reserve(X87ControlWords::BYTES);
            self.x87_control_words = Some(X87ControlWords { slot });
        }
    }

    /// The x87 scratch slot, if this function stages a value through it.
    pub(super) fn x87_scratch(&self) -> Option<X87Scratch> {
        self.x87_scratch
    }

    /// The control-word slot, if this function converts a long double to an
    /// integer.
    pub(super) fn x87_control_words(&self) -> Option<X87ControlWords> {
        self.x87_control_words
    }

    /// Identify pseudos that are long double (80-bit extended precision).
    /// These use x87 FPU instead of XMM and need 16-byte stack slots.
    fn identify_ld_pseudos(&mut self, func: &Function, types: &TypeTable) {
        for (p, t) in crate::arch::regalloc::arg_pseudo_types(func) {
            if types.kind(t) == crate::types::TypeKind::LongDouble {
                self.ld_pseudos.insert(p);
            }
        }
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
                } else if insn.op.is_float_comparison()
                    && insn
                        .operand_type()
                        .is_some_and(|t| types.kind(t) == crate::types::TypeKind::LongDouble)
                {
                    // Comparing two `long double`s: the operands are x87
                    // values, the result an `int`.
                    self.ld_pseudos.extend(insn.src.iter().copied());
                }
            }
        }
    }

    /// Give every x87-class inline-asm operand a floating-point home.
    ///
    /// Nothing else types an asm output: a bare `"=t"(r)` pseudo is defined
    /// only by the asm, so it looked like an integer and got a general
    /// register -- and a `long double` one then had no memory for `fstpt` to
    /// store into. A tied input is classed by the output it names.
    fn identify_x87_asm_operands(&mut self, func: &Function) {
        for insn in func.blocks.iter().flat_map(|b| &b.insns) {
            let Some(asm) = insn
                .extra()
                .asm_data
                .as_ref()
                .filter(|_| insn.op == Opcode::Asm)
            else {
                continue;
            };
            for c in super::inline_asm::x87_operands(asm) {
                self.fp_pseudos.insert(c.pseudo);
                if c.size > 64 {
                    self.ld_pseudos.insert(c.pseudo);
                }
            }
        }
    }

    /// Class the pseudos of SSE asm operands (`"x"`) as floating: defined
    /// only by the asm, they looked like integers and got a general
    /// register, which held eight bytes of a sixteen-byte vector. A sixteen
    /// byte one needs the slot and the moves a `__float128` gets.
    fn identify_sse_asm_operands(&mut self, func: &Function) {
        for insn in func.blocks.iter().flat_map(|b| &b.insns) {
            let Some(asm) = insn
                .extra()
                .asm_data
                .as_ref()
                .filter(|_| insn.op == Opcode::Asm)
            else {
                continue;
            };
            for c in super::inline_asm::sse_operands(asm) {
                self.fp_pseudos.insert(c.pseudo);
                if c.size > 64 {
                    self.quad_pseudos.insert(c.pseudo);
                }
            }
        }
    }

    /// Identify pseudos holding a `__float128`.
    ///
    /// Keyed on the type, not the width: an x87 `long double` is also 128 bits
    /// wide on this target, and giving it a binary128 slot or a 16-byte move
    /// would be wrong in both directions.
    fn identify_quad_pseudos(&mut self, func: &Function, types: &TypeTable) {
        for (p, t) in crate::arch::regalloc::arg_pseudo_types(func) {
            if types.kind(t) == crate::types::TypeKind::Float128 {
                self.quad_pseudos.insert(p);
            }
        }
        for block in &func.blocks {
            for insn in &block.insns {
                let is_quad = insn
                    .typ
                    .is_some_and(|t| types.kind(t) == crate::types::TypeKind::Float128);
                if is_quad {
                    if let Some(target) = insn.target {
                        self.quad_pseudos.insert(target);
                    }
                    for &src in &insn.src {
                        self.quad_pseudos.insert(src);
                    }
                } else if insn.op.is_float_comparison()
                    && insn
                        .operand_type()
                        .is_some_and(|t| types.kind(t) == crate::types::TypeKind::Float128)
                {
                    self.quad_pseudos.extend(insn.src.iter().copied());
                }
            }
        }
    }

    /// Identify pseudos that are 128-bit integers (__int128).
    /// These need 16-byte stack slots and must never be allocated to GP registers.
    fn identify_int128_pseudos(&mut self, func: &Function, types: &TypeTable) {
        for (p, t) in crate::arch::regalloc::arg_pseudo_types(func) {
            if types.is_plain_int128(t) {
                self.int128_pseudos.insert(p);
            }
        }
        // First pass: identify targets of 128-bit instructions and all sources
        // of 128-bit binary/unary ops.
        for block in &func.blocks {
            for insn in &block.insns {
                // Only match Int128 type, not 16-byte structs or long doubles
                let is_int128 = insn.typ.is_some_and(|t| types.is_plain_int128(t));

                if is_int128 {
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
                            // A comparison is never here: its result is an `int`,
                            // and `mapping` has split its 128-bit operands.
                            if !matches!(insn.op, Opcode::Load) {
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
        let mut stack_arg_offset = IncomingOff::FIRST;

        // The shared AbiLowering helper does sret detection and O(1)
        // Arg(n) → pseudo lookup; the classification dispatch below keeps
        // its own inline type-kind checks.
        let lowering = AbiLowering::new(func);
        let arg_idx_offset = lowering.arg_idx_offset;

        // Allocate RDI for hidden return pointer if present
        if let Some(sret_id) = lowering.sret_pseudo {
            self.locations.insert(sret_id, Loc::Reg(int_arg_regs[0]));
            self.free_regs.retain(|&r| r != int_arg_regs[0]);
            spend_arg_regs(&mut int_arg_idx, 1, int_arg_regs.len());
        }

        for (i, (_name, typ)) in func.params.iter().enumerate() {
            let arg_n = (i as u32) + arg_idx_offset;
            let Some(pseudo_id) = lowering.arg_pseudos.get(arg_n as usize).copied().flatten()
            else {
                // A declared parameter with no `Arg` pseudo would be skipped
                // by the whole ABI dispatch below, so it would consume neither
                // a register nor its bytes of the incoming area -- and since
                // `va_start` now reads its three answers off the end of this
                // loop, every later parameter and every variadic argument
                // would shift. The linearizer creates one per parameter, so
                // this is a guard against that changing, not a live case.
                debug_assert!(
                    false,
                    "parameter {i} of {} has no Arg pseudo; the incoming-argument \
                     layout this loop records would be short by its size",
                    func.name
                );
                continue;
            };
            // `kind()` answers the *base* kind for a complex type, so without
            // the guard `long double _Complex` satisfies this and takes the
            // sixteen-byte branch below rather than the COMPLEX_X87 branch
            // further down that is meant for its thirty-two. Both sibling
            // sites, in `call.rs` and `codegen.rs`, exclude complex too.
            // A zero-sized parameter occupies nothing, so it must not take a
            // register here either -- the call site's layout already skips
            // it, and charging one made every later parameter read from the
            // wrong register.
            if crate::abi::param_is_ignored(*typ, types) {
                continue;
            }
            let is_longdouble = types.kind(*typ) == crate::types::TypeKind::LongDouble
                && !types.is_complex_float(*typ);
            let is_fp = types.is_float(*typ);
            let is_complex = types.is_complex_float(*typ);
            let sse_struct = crate::abi::sse_struct_regs(*typ, types);

            // Long double uses x87 FPU and is passed on the stack per System V AMD64 ABI
            if is_longdouble {
                // Long double takes 16 bytes on stack (80-bit padded to 128-bit)
                self.locations.insert(
                    pseudo_id,
                    Loc::IncomingArg(IncomingOff::take(&mut stack_arg_offset, 16, 16)),
                );
                self.fp_pseudos.insert(pseudo_id);
            } else if sse_struct.is_some() && types.size_bits(*typ) <= 64 {
                // One eightbyte, arriving in an XMM register as a value: the
                // parameter pseudo *is* that value, and the linearizer's
                // small-struct path stores it straight into the local. Larger
                // ones travel by address and are handled below.
                if fp_arg_idx < fp_arg_regs.len() {
                    self.locations
                        .insert(pseudo_id, Loc::Xmm(fp_arg_regs[fp_arg_idx]));
                    self.free_xmm_regs.retain(|&r| r != fp_arg_regs[fp_arg_idx]);
                    self.fp_pseudos.insert(pseudo_id);
                    spend_arg_regs(&mut fp_arg_idx, 1, fp_arg_regs.len());
                } else {
                    self.locations.insert(
                        pseudo_id,
                        Loc::IncomingArg(IncomingOff::take(
                            &mut stack_arg_offset,
                            8,
                            types.alignment(*typ) as i32,
                        )),
                    );
                    self.fp_pseudos.insert(pseudo_id);
                }
            } else if let Some(sse_regs) = sse_struct {
                // All-SSE struct: uses one XMM per class. Don't assign to a
                // register — the codegen stores the XMM values to the local's
                // stack slot. Just consume the FP arg indices without assigning
                // a location; the pseudo gets a stack slot from normal
                // allocation. Two doubles take two registers; a lone binary128
                // takes one, for all sixteen bytes.
                //
                // ...but only when there are that many registers left. System
                // V 3.2.3 step 5 sends an argument that does not fit to memory
                // *whole*, consuming none of the registers it did not fit in.
                // Advancing unconditionally left the pseudo with no location
                // at all, so the callee read an untouched local: `struct
                // {double,double}` after eight doubles came back as zero,
                // while c17's own *caller* passed it correctly. The two
                // neighbouring arms below already do this; this one did not.
                if fp_arg_idx + sse_regs <= fp_arg_regs.len() {
                    spend_arg_regs(&mut fp_arg_idx, sse_regs, fp_arg_regs.len());
                } else {
                    self.locations.insert(
                        pseudo_id,
                        Loc::IncomingArg(IncomingOff::take(
                            &mut stack_arg_offset,
                            crate::abi::slot_bytes(
                                types.size_bytes(*typ),
                                self.func_pos,
                                "a stacked parameter",
                            ),
                            types.alignment(*typ) as i32,
                        )),
                    );
                }
            } else if let Some(classes) = crate::abi::struct_param_classes(*typ, types) {
                // Two eightbytes in two registers -- both general, or one of
                // each. Like the all-SSE case above, no location is assigned
                // when they arrive in registers: the prologue writes them into
                // the parameter's local and the pseudo takes an ordinary slot.
                // The class order says which register file each came from.
                let gp_needed = classes
                    .iter()
                    .filter(|c| **c != crate::abi::RegClass::Sse)
                    .count();
                let sse_needed = classes.len() - gp_needed;
                if int_arg_idx + gp_needed > int_arg_regs.len()
                    || fp_arg_idx + sse_needed > fp_arg_regs.len()
                {
                    // System V 3.2.3 step 5: an argument that does not fit goes
                    // to memory *whole*, and consumes none of the registers it
                    // did not fit in.
                    self.locations.insert(
                        pseudo_id,
                        Loc::IncomingArg(IncomingOff::take(
                            &mut stack_arg_offset,
                            crate::abi::slot_bytes(
                                types.size_bytes(*typ),
                                self.func_pos,
                                "a stacked parameter",
                            ),
                            types.alignment(*typ) as i32,
                        )),
                    );
                } else {
                    spend_arg_regs(&mut int_arg_idx, gp_needed, int_arg_regs.len());
                    spend_arg_regs(&mut fp_arg_idx, sse_needed, fp_arg_regs.len());
                }
            } else if is_complex {
                // How many XMM registers this complex type actually occupies:
                // one for `float _Complex` (both halves packed into a single
                // eightbyte), two for `double _Complex`, none for
                // `long double _Complex`, which is COMPLEX_X87 and arrives on
                // the stack. Consuming two unconditionally pushed every later
                // floating-point parameter one register along, so a `float`
                // after a `float _Complex` was read from the wrong register.
                let sse_regs = crate::arch::lir::complex_sse_regs(types, *typ);
                if sse_regs == 0 {
                    self.locations.insert(
                        pseudo_id,
                        Loc::IncomingArg(IncomingOff::take(
                            &mut stack_arg_offset,
                            crate::abi::slot_bytes(
                                types.size_bytes(*typ),
                                self.func_pos,
                                "a stacked parameter",
                            ),
                            types.alignment(*typ) as i32,
                        )),
                    );
                } else if fp_arg_idx + sse_regs <= fp_arg_regs.len() {
                    self.locations
                        .insert(pseudo_id, Loc::Xmm(fp_arg_regs[fp_arg_idx]));
                    let used = &fp_arg_regs[fp_arg_idx..fp_arg_idx + sse_regs];
                    self.free_xmm_regs.retain(|r| !used.contains(r));
                    self.fp_pseudos.insert(pseudo_id);
                    spend_arg_regs(&mut fp_arg_idx, sse_regs, fp_arg_regs.len());
                } else {
                    // Not enough XMM registers left for every eightbyte, so
                    // §3.2.3 step 5 puts the *whole* argument in memory — and
                    // it consumes no registers at all, leaving them for the
                    // arguments that follow. Advancing `fp_arg_idx` here moved
                    // every later floating-point parameter one slot along.
                    self.locations.insert(
                        pseudo_id,
                        Loc::IncomingArg(IncomingOff::take(
                            &mut stack_arg_offset,
                            crate::abi::slot_bytes(
                                types.size_bytes(*typ),
                                self.func_pos,
                                "a stacked parameter",
                            ),
                            types.alignment(*typ) as i32,
                        )),
                    );
                }
            } else if is_fp {
                if fp_arg_idx < fp_arg_regs.len() {
                    self.locations
                        .insert(pseudo_id, Loc::Xmm(fp_arg_regs[fp_arg_idx]));
                    self.free_xmm_regs.retain(|&r| r != fp_arg_regs[fp_arg_idx]);
                    self.fp_pseudos.insert(pseudo_id);
                } else {
                    // Stack args are placed in parameter order per System V AMD64 ABI
                    // A binary128 occupies two eightbytes; advancing by one
                    // put the next argument on top of its upper half.
                    self.locations.insert(
                        pseudo_id,
                        Loc::IncomingArg(IncomingOff::take(
                            &mut stack_arg_offset,
                            crate::abi::slot_bytes(
                                types.size_bytes(*typ),
                                self.func_pos,
                                "a stacked parameter",
                            ),
                            types.alignment(*typ) as i32,
                        )),
                    );
                }
                spend_arg_regs(&mut fp_arg_idx, 1, fp_arg_regs.len());
            } else if types.is_plain_int128(*typ) {
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
                    spend_arg_regs(&mut int_arg_idx, 2, int_arg_regs.len());
                } else {
                    let at = IncomingOff::take(&mut stack_arg_offset, 16, 16);
                    self.int128_incoming.insert(pseudo_id, at);
                    // 3.2.3 step 5: an argument that does not fit goes to memory
                    // *whole* and consumes no registers, so the ones it did not
                    // fit in stay available to later arguments. Advancing here
                    // pushed the next argument onto the stack, where the caller
                    // -- which gets this right -- had not put it.
                    //
                    // Note this is the opposite of AAPCS64 stage C.11, where an
                    // overflowing argument does exhaust the file.
                }
            } else {
                let is_large_struct = crate::abi::param_is_memory_class(*typ, types);
                if is_large_struct {
                    // MEMORY class: passed on the stack by value. Advance by
                    // the full struct size.
                    self.locations.insert(
                        pseudo_id,
                        Loc::IncomingArg(IncomingOff::take(
                            &mut stack_arg_offset,
                            crate::abi::slot_bytes(
                                types.size_bytes(*typ),
                                self.func_pos,
                                "a stacked parameter",
                            ),
                            types.alignment(*typ) as i32,
                        )),
                    );
                    // Don't increment int_arg_idx — no GP register consumed
                } else if int_arg_idx < int_arg_regs.len() {
                    self.locations
                        .insert(pseudo_id, Loc::Reg(int_arg_regs[int_arg_idx]));
                    self.free_regs.retain(|&r| r != int_arg_regs[int_arg_idx]);
                    spend_arg_regs(&mut int_arg_idx, 1, int_arg_regs.len());
                } else {
                    // Stack args are placed in parameter order per System V AMD64 ABI
                    self.locations.insert(
                        pseudo_id,
                        Loc::IncomingArg(IncomingOff::take(
                            &mut stack_arg_offset,
                            8,
                            types.alignment(*typ) as i32,
                        )),
                    );
                    spend_arg_regs(&mut int_arg_idx, 1, int_arg_regs.len());
                }
            }
        }

        // What `va_start` needs, recorded by the one loop that knows the ABI.
        //
        // The counters are capped rather than used raw: several branches above
        // advance them even when the argument was stacked, and once a bank is
        // exhausted it stays exhausted, so the cap is the number actually
        // consumed. `gp_offset`/`fp_offset` index *into* the save area, so a
        // value past its end would make every register-path test fail.
        self.named_gp_regs = int_arg_idx.min(int_arg_regs.len());
        self.named_fp_regs = fp_arg_idx.min(fp_arg_regs.len());
        self.named_incoming_end = stack_arg_offset.0;
        crate::arch::regalloc::check_incoming_area(stack_arg_offset.0, self.func_pos);
    }

    /// Spill arguments in caller-saved registers if their interval crosses a call
    fn spill_args_across_calls(
        &mut self,
        func: &Function,
        types: &TypeTable,
        intervals: &[LiveInterval],
        call_positions: &[usize],
    ) {
        self.spill_gp_args(intervals, |interval, _| {
            interval_crosses_call(interval, call_positions)
        });

        // Always spill XMM function parameter arguments to stack.
        // All XMM registers are caller-saved on x86-64 SysV ABI, and any float
        // computation within the function may reuse the same XMM register,
        // clobbering the parameter value.
        //
        // A *complex* parameter is excluded. It arrives in one or two XMM
        // registers depending on its base type, and the prologue in
        // `store_args_to_stack` already knows how to place both halves into
        // the parameter's own local. Spilling it here would save a single
        // register into an unrelated 8-byte slot -- losing the imaginary half
        // of a `double _Complex` -- and, because the pseudo would then be
        // marked already-spilled, suppress that correct handling entirely.
        let lowering = crate::arch::regalloc::AbiLowering::new(func);
        let complex_arg_pseudos: HashSet<PseudoId> = func
            .pseudos
            .iter()
            .filter_map(|p| match p.kind {
                PseudoKind::Arg(idx) => lowering
                    .param_type(func, idx)
                    .filter(|typ| types.is_complex_float(*typ))
                    .map(|_| p.id),
                _ => None,
            })
            .collect();

        let xmm_arg_regs = XmmReg::arg_regs();
        for interval in intervals {
            if complex_arg_pseudos.contains(&interval.pseudo) {
                continue;
            }
            if let Some(Loc::Xmm(xmm)) = self.locations.get(&interval.pseudo) {
                if xmm_arg_regs.contains(xmm) && interval.start == 0 {
                    // This is a function parameter in an XMM register — always spill
                    let from_xmm = *xmm;
                    let is_quad = self.quad_pseudos.contains(&interval.pseudo);
                    self.stack_offset += if is_quad { 16 } else { 8 };
                    let to_stack_offset = self.stack_offset;

                    self.spilled_xmm_args.push(SpilledXmmArg {
                        pseudo: interval.pseudo,
                        from_xmm,
                        to_stack_offset,
                        size: if is_quad {
                            FpSize::Quad
                        } else {
                            FpSize::Double
                        },
                    });

                    self.locations
                        .insert(interval.pseudo, Loc::Stack(to_stack_offset));
                    self.free_xmm_regs.push(from_xmm);
                }
            }
        }
    }

    /// Where a stack-passed `__int128` parameter arrives, if it did.
    pub fn int128_incoming(&self, pseudo: PseudoId) -> Option<i32> {
        self.int128_incoming.get(&pseudo).copied()
    }

    /// GP argument registers consumed by the named parameters.
    pub fn named_gp_regs(&self) -> usize {
        self.named_gp_regs
    }

    /// SSE argument registers consumed by the named parameters.
    pub fn named_fp_regs(&self) -> usize {
        self.named_fp_regs
    }

    /// The `%rbp` displacement where the variadic arguments begin.
    pub fn named_incoming_end(&self) -> i32 {
        self.named_incoming_end
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

    /// Spill a GP argument out of its ABI register when `overwritten` says
    /// something destroys that register while the argument is live: a call,
    /// or a constraint point such as a variable shift loading its count into
    /// RCX while the fourth parameter still lives there. See
    /// [`crate::arch::regalloc::spill_gp_args_across`].
    fn spill_gp_args(
        &mut self,
        intervals: &[LiveInterval],
        overwritten: impl Fn(&LiveInterval, Reg) -> bool,
    ) {
        let int_arg_regs_set: &[Reg] = Reg::arg_regs();
        let spilled_args = &mut self.spilled_args;
        let free_regs = &mut self.free_regs;
        crate::arch::regalloc::spill_gp_args_across(
            intervals,
            overwritten,
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
    /// A fresh frame slot of `size` bytes at `alignment`, shared with nothing.
    fn new_frame_slot(&mut self, size: i32, alignment: i32) -> i32 {
        if alignment > self.max_local_align {
            self.max_local_align = alignment;
        }
        let frame_align = self.frame_align();
        crate::arch::regalloc::grow_frame(
            &mut self.stack_offset,
            size,
            alignment,
            frame_align,
            self.func_pos,
        )
    }

    /// Give every local its frame slot; see `arch::regalloc::place_locals`.
    fn place_locals(&mut self, func: &Function, types: &TypeTable, intervals: &[LiveInterval]) {
        let pos = self.func_pos;
        let placed = crate::arch::regalloc::place_locals(func, types, pos, intervals, |b, a| {
            self.new_frame_slot(b, a)
        });
        for (local, offset) in placed {
            self.locations.insert(local, Loc::Stack(offset));
            if func.local_of(local).is_some_and(|l| types.is_float(l.typ)) {
                self.fp_pseudos.insert(local);
            }
        }
    }

    fn alloc_stack_slot(
        &mut self,
        interval: &LiveInterval,
        size: i32,
        alignment: i32,
        reusable: bool,
    ) {
        // A reused slot was counted toward the frame's alignment when it was
        // made; a new one is counted by `new_frame_slot`.
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
        let offset = self.new_frame_slot(size, alignment);
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

    /// Chordal coloring, with spill-on-fail for uncolorable vertices.
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
    fn run_chordal_color(
        &mut self,
        func: &Function,
        intervals: Vec<LiveInterval>,
        call_positions: &[usize],
        fp_call_positions: &[usize],
        constraint_points: &[ConstraintPoint<Reg>],
    ) {
        // -------- Phase 1: pre-pass --------
        let crosses_blocks = crate::arch::regalloc::live_out_anywhere(&self.live_out);
        let setval_sizes = crate::arch::regalloc::setval_sizes(func);
        let mut gp_candidates: std::collections::BTreeSet<PseudoId> =
            std::collections::BTreeSet::new();
        let mut xmm_candidates: std::collections::BTreeSet<PseudoId> =
            std::collections::BTreeSet::new();
        for interval in &intervals {
            // Intervals come pre-sorted by start position from
            // compute_live_intervals, so this monotonic expiration
            // keeps slot reuse available to the chordal sweep.
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
                        // A 128-bit constant needs sixteen bytes of memory
                        // like any other `__int128`: every consumer reaches it
                        // through `int128_lo_mem_loc`/`int128_hi_mem_loc`,
                        // which take an address and panic on anything else --
                        // so `f((__int128)1)` aborted the compiler.
                        //
                        // Asked of the constant's own defining `SetVal`, not
                        // of `int128_pseudos`: that set also holds the narrow
                        // constants that merely *feed* a 128-bit instruction,
                        // and those are still ordinary immediates that get
                        // widened at the use site.
                        let defined_128 = setval_sizes.get(&interval.pseudo) == Some(&128);
                        if defined_128 {
                            self.alloc_stack_slot(interval, 16, 16, true);
                            continue;
                        }
                        self.locations.insert(interval.pseudo, Loc::Imm(*v));
                        continue;
                    }
                    PseudoKind::FVal(v) => {
                        let size = setval_sizes.get(&interval.pseudo).copied().unwrap_or(64);
                        self.locations.insert(interval.pseudo, Loc::FImm(*v, size));
                        self.fp_pseudos.insert(interval.pseudo);
                        continue;
                    }
                    PseudoKind::Sym(name) => {
                        // By identity, not by name: a global reached through a
                        // block-scope `extern` carries the same bare name as a
                        // parameter, and answering by name gave the global the
                        // parameter's slot instead of `Loc::Global`.
                        // A local has its slot already: `place_locals`.
                        debug_assert!(func.local_of(interval.pseudo).is_none());
                        self.locations
                            .insert(interval.pseudo, Loc::Global(name.clone()));
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
                let is_quad = self.quad_pseudos.contains(&interval.pseudo);
                let crosses_call = interval_crosses_call(interval, fp_call_positions);
                let crosses_block = crosses_blocks.contains(&interval.pseudo);
                if is_longdouble {
                    self.alloc_stack_slot(interval, 16, 16, false);
                    continue;
                }
                if is_quad && (crosses_call || crosses_block) {
                    // A `__float128` is 16 bytes. An 8-byte slot let a
                    // 16-byte store run over its neighbour and read back
                    // garbage in the low half.
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
        let by_pseudo = crate::arch::regalloc::intervals_by_pseudo(intervals);
        use crate::arch::regalloc::{build_interference_graph, greedy_color, mcs_ordering};
        if gp_candidates.is_empty() {
            return;
        }

        // Pre-colored vertices: any pseudo already mapped to a GP reg
        // (ABI-pinned args from allocate_arguments) plus inline-asm
        // operands pinned to one register (e.g. `"a"(x)` pins x
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
            // lowering already routes pinned-operand registers
            // into the ConstraintPoint clobber set, so even if
            // pre-coloring is skipped here the operand remains
            // exempt via `involved_pseudos`.
            if !gp_candidates.contains(&pid) {
                continue;
            }
            // If the pseudo is already pre-colored (ABI-pinned, or an
            // earlier asm operand pinned it), `or_insert` keeps the
            // existing register. We must mirror exactly that choice
            // into `self.locations` — committing the asm-requested
            // register when the allocator is going to honor the
            // earlier pin would split codegen's view (`self.locations`)
            // from coloring's view (`pre_colored`), causing
            // Store/Load to address a register that doesn't hold the
            // value.
            //
            // Phase 3's commit loop skips pre-colored vertices on the
            // assumption that their locations are already in
            // `self.locations` (true for ABI-pinned args, which
            // `allocate_arguments` inserts before chordal runs).
            // Inline-asm pin pre-colors arrive here without going
            // through `allocate_arguments`, so we insert directly. If
            // missed, `get_location` defaults to `Loc::Imm(0)` and
            // every Store/Load involving the operand silently
            // writes/reads zero.
            let committed = *pre_colored.entry(pid).or_insert(reg);
            all_vertices.insert(pid);
            self.locations.insert(pid, Loc::Reg(committed));
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
        let allocatable = self.allocatable_regs();
        let caller_saved: Vec<Reg> = allocatable
            .iter()
            .copied()
            .filter(|r| !r.is_callee_saved())
            .collect();
        let mut forbidden = crate::arch::regalloc::constraint_clobbers(
            constraint_points,
            intervals,
            gp_candidates,
            exempt_from_clobber,
        );
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
            for &r in &allocatable {
                if r.is_callee_saved() {
                    v.push(r);
                }
            }
            v
        };
        let callee_first: Vec<Reg> = {
            let mut v: Vec<Reg> = allocatable
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

        // Asm register operands are colored first: gcc guarantees each
        // one a register, so it is the other values that spill.
        let asm_ops = crate::arch::regalloc::asm_register_operands(func);
        let order = crate::arch::regalloc::asm_operands_first(mcs_ordering(&graph), &asm_ops);
        let result = greedy_color(
            &graph,
            &order,
            &allocatable,
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

        // -------- Belady eviction --------
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
            let interval = match by_pseudo.get(&spilled).copied() {
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
                // Don't evict ABI-pinned args, or an asm register operand:
                // the template needs it in a register.
                if pre_colored.contains_key(&n) || asm_ops.contains(&n) {
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

        // For each `Opcode::Copy { target: t, src: [s] }` in the
        // function, try to migrate `t`'s location to `s`'s location
        // (so the Copy becomes identity and M9a elides it). Skip if:
        // - `t` or `s` isn't a GP candidate
        // - `t` is pre-colored (ABI-pinned or asm-pinned; moving it
        //   would violate the constraint)
        // - `t` and `s` interfere (a Copy whose targets interfere
        //   means the IR is asking for `t = s` while both must hold
        //   distinct values elsewhere — moving t to s.Loc would
        //   corrupt one of them)
        // - any neighbor of `t` is already at `s`'s register (the
        //   move would create a conflict)
        // - `t`'s forbidden set contains `s`'s register (the
        //   constraint system reserved that register against `t`)
        let candidates = crate::arch::regalloc::find_copy_coalesce_candidates(func);
        for (t, s) in candidates {
            if !gp_candidates.contains(&t) || !gp_candidates.contains(&s) {
                continue;
            }
            if pre_colored.contains_key(&t) {
                continue;
            }
            let t_loc = self.locations.get(&t).cloned();
            let s_loc = self.locations.get(&s).cloned();
            let (Some(Loc::Reg(t_reg)), Some(Loc::Reg(s_reg))) = (t_loc, s_loc) else {
                continue;
            };
            if t_reg == s_reg {
                continue;
            }
            // Interference check.
            if graph.neighbors(t).any(|n| n == s) {
                continue;
            }
            // Forbidden check.
            let empty = std::collections::BTreeSet::new();
            let t_forbid = forbidden.get(&t).unwrap_or(&empty);
            if t_forbid.contains(&s_reg) {
                continue;
            }
            // Neighbor-occupancy check: no neighbor of t may already
            // hold s_reg.
            let conflict = graph.neighbors(t).any(|n| {
                self.locations
                    .get(&n)
                    .map(|l| matches!(l, Loc::Reg(r) if *r == s_reg))
                    .unwrap_or(false)
            });
            if conflict {
                continue;
            }
            self.locations.insert(t, Loc::Reg(s_reg));
            // Track callee-saved usage if we just claimed one.
            if s_reg.is_callee_saved() && !self.used_callee_saved.contains(&s_reg) {
                self.used_callee_saved.push(s_reg);
            }
        }
        // The monotonic sweep mirrors linear scan's invariant: a
        // slot is only freed once the owning interval has ended,
        // and a new interval's start ≥ the freed slot's owner's
        // end, so they cannot interfere within a block.
        let mut ordered_spilled: Vec<(usize, PseudoId)> = final_spilled
            .iter()
            .filter_map(|&p| by_pseudo.get(&p).copied().map(|i| (i.start, p)))
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
            if let Some(interval) = by_pseudo.get(&spilled).copied() {
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
        let by_pseudo = crate::arch::regalloc::intervals_by_pseudo(intervals);
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
            .filter_map(|&p| by_pseudo.get(&p).copied().map(|i| (i.start, p)))
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
            if let Some(interval) = by_pseudo.get(&spilled).copied() {
                let bytes = self.fp_slot_bytes(spilled);
                self.alloc_stack_slot(interval, bytes, bytes, true);
            }
        }
    }

    /// The bytes, and alignment, of a stack slot holding the XMM value
    /// `pseudo`: sixteen for a whole-register (`Quad`) value, eight for any
    /// other. A spilled `__float128` once got eight, so a neighbouring
    /// spill's store ran over it.
    fn fp_slot_bytes(&self, pseudo: PseudoId) -> i32 {
        if self.quad_pseudos.contains(&pseudo) {
            16
        } else {
            8
        }
    }

    /// Compute live intervals, constraint points, and per-block liveness sets.
    fn compute_live_intervals(&self, func: &Function) -> LivenessResult<Reg> {
        let tls = self.tls_access;
        compute_live_intervals(func, |insn| get_constraint_info(insn, tls))
    }

    /// Get stack size needed (aligned to max local alignment, minimum 16)
    /// The alignment the locals area is rounded to at the end, as far as it
    /// is known: the frame base's, when over-aligned, and every slot's so far.
    fn frame_align(&self) -> i32 {
        let base = match self.frame_base {
            FrameBase::Aligned { align, .. } => align,
            FrameBase::Rbp => 16,
        };
        base.max(self.max_local_align).max(16)
    }

    pub fn stack_size(&self) -> i32 {
        let align = self.max_local_align.max(16);
        (self.stack_offset + align - 1) & !(align - 1)
    }

    /// Get the maximum alignment requirement of any local variable
    pub fn max_local_align(&self) -> i32 {
        self.max_local_align
    }

    /// How this function's locals are addressed.
    pub fn frame_base(&self) -> FrameBase {
        self.frame_base
    }

    /// The registers this function may colour with: the machine's allocatable
    /// set, less whatever the frame has claimed for its base.
    ///
    /// Every consumer must come through here rather than reading
    /// `Reg::allocatable()` -- the colourer builds its own preference orders,
    /// and withholding the base from `free_regs` alone left it free to hand
    /// out the very register the prologue overwrites.
    fn allocatable_regs(&self) -> Vec<Reg> {
        let claimed = self.frame_base.reg();
        Reg::allocatable()
            .iter()
            .copied()
            .filter(|r| Some(*r) != claimed)
            .collect()
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

    /// Stacked arguments are laid out from the argument area's base, not from
    /// the frame displacement.
    ///
    /// The two differ by the saved `%rbp` and the return address, and rounding the displacement
    /// directly charged an over-aligned argument for those 16 bytes: an
    /// argument wanting 32-byte alignment and arriving first went to offset 32
    /// instead of 16, one whole alignment unit past where the caller -- which
    /// measures from its outgoing area's own base -- had written it.
    ///
    /// Only alignment past 16 can tell the two bases apart, since 16 is
    /// already a multiple of 8 and of 16. The 8 and 16 cases are asserted so
    /// that stays true.
    #[test]
    fn incoming_args_are_laid_out_from_the_area_base() {
        let first = IncomingOff::FIRST.0;

        // Alignment up to the base's own: unchanged, and packed at 8.
        let mut next = IncomingOff::FIRST;
        assert_eq!(IncomingOff::take(&mut next, 8, 8), first);
        assert_eq!(IncomingOff::take(&mut next, 8, 8), first + 8);
        assert_eq!(IncomingOff::take(&mut next, 16, 16), first + 16);

        // A 16-aligned argument landing on an odd 8-byte slot still rounds.
        let mut next = IncomingOff::FIRST;
        assert_eq!(IncomingOff::take(&mut next, 8, 8), first);
        assert_eq!(IncomingOff::take(&mut next, 16, 16), first + 16);

        // Past the base's alignment is where the bases diverge. Arriving
        // first, an over-aligned argument must start *at* the base.
        for align in [32, 64] {
            let mut next = IncomingOff::FIRST;
            assert_eq!(
                IncomingOff::take(&mut next, align, align),
                first,
                "an {align}-aligned argument arriving first starts at the area base"
            );
        }

        // And after an 8-byte argument it rounds to the next multiple of its
        // alignment measured from the base, not from the displacement.
        for align in [32, 64] {
            let mut next = IncomingOff::FIRST;
            assert_eq!(IncomingOff::take(&mut next, 8, 8), first);
            assert_eq!(
                IncomingOff::take(&mut next, align, align),
                first + align,
                "an {align}-aligned argument rounds within the argument area"
            );
        }
    }

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
        // get other treatment (memory barrier; cc is a no-op).
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
        assert!(is_call_like_x86_64(Opcode::Memset));
        assert!(is_call_like_x86_64(Opcode::Memcpy));
        assert!(is_call_like_x86_64(Opcode::Memmove));
        // Non-call-like opcodes stay off. The sign operations are computed
        // in place, so a value may stay in a caller-saved register across
        // one.
        assert!(!is_call_like_x86_64(Opcode::Fabs));
        assert!(!is_call_like_x86_64(Opcode::CopySign));
        assert!(!is_call_like_x86_64(Opcode::Sqrt));
        assert!(!is_call_like_x86_64(Opcode::RoundToIntegral(
            crate::float::IntegralRounding::Rint
        )));
        assert!(!is_call_like_x86_64(Opcode::Signbit));
        assert!(!is_call_like_x86_64(Opcode::Add));
        assert!(!is_call_like_x86_64(Opcode::Asm));
    }

    fn make_asm_insn(clobbers: &[&str], operands: &[(&str, PseudoId)]) -> Instruction {
        use crate::arch::asm_constraints::AsmAccess;
        use crate::ir::{AsmConstraint, AsmData};
        let (outputs, inputs): (Vec<_>, Vec<_>) = operands
            .iter()
            .map(|&(c, pseudo)| AsmConstraint::new(pseudo, c, crate::target::Arch::X86_64, 64))
            .partition(|c| c.class.access != AsmAccess::Read);
        let mut insn = Instruction::new(Opcode::Asm);
        insn.extra_mut().asm_data = Some(Box::new(AsmData {
            template: String::new(),
            outputs,
            inputs,
            clobbers: clobbers.iter().map(|s| s.to_string()).collect(),
            goto_labels: Vec::new(),
        }));
        insn
    }

    /// What a `TlsAddr` clobbers follows the model: the ELF descriptor call
    /// hard-uses %rax alone; the Mach-O TLV getter also takes its descriptor
    /// in %rdi. Every other general register survives either.
    #[test]
    fn tls_addr_clobbers_follow_the_model() {
        use crate::target::TlsAccess;
        let insn = Instruction::tls_addr(PseudoId(1), PseudoId(2), crate::types::TypeId::INVALID);
        let (elf, _) = get_constraint_info(&insn, TlsAccess::ElfDescriptor).unwrap();
        let (macho, _) = get_constraint_info(&insn, TlsAccess::MachOTlv).unwrap();
        assert!(elf.contains(&Reg::Rax) && !elf.contains(&Reg::Rdi));
        assert!(macho.contains(&Reg::Rax) && macho.contains(&Reg::Rdi));
        for kept in [Reg::Rcx, Reg::Rdx, Reg::Rsi, Reg::R8, Reg::R9, Reg::Rbx] {
            assert!(
                !macho.contains(&kept),
                "{kept:?} is preserved by the getter"
            );
        }
    }

    /// No operand of an asm statement may live in a register it clobbers:
    /// the template may write that register before reading its operands.
    /// So an asm's declared clobbers exempt none of its operands, register
    /// or memory -- unlike an instruction such as `idivq`, which reads its
    /// operands before touching the registers it claims.
    #[test]
    fn asm_operands_are_not_exempt_from_the_statements_clobbers() {
        let insn = make_asm_insn(
            &["rax", "memory"],
            &[("=r", PseudoId(1)), ("r", PseudoId(2)), ("m", PseudoId(5))],
        );
        let (clobbers, exempt) = get_constraint_info(&insn, crate::target::TlsAccess::ElfStatic)
            .expect("a register is claimed");
        assert!(clobbers.contains(&Reg::Rax));
        assert!(exempt.is_empty(), "{exempt:?}");

        // A pinned operand is the one exemption: it is precolored to its own
        // register and must not be forbidden it. Another operand still may
        // not take that register.
        let mut pinned = make_asm_insn(&[], &[("=a", PseudoId(3)), ("r", PseudoId(4))]);
        let (clobbers, exempt) = get_constraint_info(&pinned, crate::target::TlsAccess::ElfStatic)
            .expect("rax is claimed");
        assert!(clobbers.contains(&Reg::Rax));
        assert_eq!(exempt, vec![PseudoId(3)]);
        pinned
            .extra_mut()
            .asm_data
            .as_mut()
            .unwrap()
            .clobbers
            .push("rcx".into());
        let (clobbers, _) =
            get_constraint_info(&pinned, crate::target::TlsAccess::ElfStatic).unwrap();
        assert!(clobbers.contains(&Reg::Rcx));
    }

    /// An x87 asm operand is floating point, and a `long double` one is a
    /// long double, whatever else defines it -- so a bare `"=t"` output gets a
    /// floating-point home, never a general register. A tied input takes its
    /// output's class; a general-register operand is left alone.
    #[test]
    fn x87_asm_operands_get_a_floating_point_home() {
        use crate::ir::{BasicBlock, BasicBlockId, Function};
        let mut asm = make_asm_insn(
            &[],
            &[
                ("=t", PseudoId(1)),
                ("=r", PseudoId(2)),
                ("u", PseudoId(3)),
                ("0", PseudoId(4)),
            ],
        );
        let data = asm.extra_mut().asm_data.as_mut().unwrap();
        data.inputs[1].matching_output = Some(0);
        let mut ld = make_asm_insn(&[], &[("=f", PseudoId(5))]);
        ld.extra_mut().asm_data.as_mut().unwrap().outputs[0].size = 128;

        let types = crate::types::TypeTable::new(&crate::target::Target::host());
        let mut func = Function::new("f", types.void_id);
        let mut block = BasicBlock::new(BasicBlockId(0));
        block.insns = vec![asm, ld];
        func.blocks.push(block);

        let mut ra = RegAlloc::new();
        ra.identify_x87_asm_operands(&func);
        for p in [1, 3, 4, 5] {
            assert!(ra.fp_pseudos.contains(&PseudoId(p)), "%{p} is x87");
        }
        assert!(!ra.fp_pseudos.contains(&PseudoId(2)));
        assert!(ra.ld_pseudos.contains(&PseudoId(5)));
        assert!(!ra.ld_pseudos.contains(&PseudoId(1)));
    }

    /// A `Q` operand is pinned to the first of %rax..%rdx that the statement
    /// neither pins nor clobbers, each to a different one; a tied input takes
    /// its output's register, and the allocator sees every pin.
    #[test]
    fn asm_q_operand_is_pinned_to_a_free_high_byte_register() {
        let mut insn = make_asm_insn(
            &["rbx"],
            &[
                ("=Q", PseudoId(1)),
                ("a", PseudoId(2)),
                ("Q", PseudoId(3)),
                ("0", PseudoId(1)),
                ("r", PseudoId(4)),
            ],
        );
        let data = insn.extra_mut().asm_data.as_mut().unwrap();
        data.inputs[2].matching_output = Some(0);
        assert_eq!(
            asm_pinned_regs(data),
            vec![
                Some(Reg::Rcx),
                Some(Reg::Rax),
                Some(Reg::Rdx),
                Some(Reg::Rcx),
                None
            ]
        );
        let (clobbers, exempt) =
            get_constraint_info(&insn, crate::target::TlsAccess::ElfStatic).unwrap();
        // R10 and R11 are the inline-asm codegen's own scratch.
        assert_eq!(
            clobbers,
            vec![Reg::Rax, Reg::Rbx, Reg::Rcx, Reg::Rdx, Reg::R10, Reg::R11]
        );
        assert_eq!(exempt, vec![PseudoId(1), PseudoId(2), PseudoId(3)]);

        // With all four taken there is none to give.
        let full = make_asm_insn(
            &["rax", "rbx", "rcx"],
            &[("=d", PseudoId(1)), ("Q", PseudoId(2))],
        );
        let data = full.extra().asm_data.as_ref().unwrap();
        assert_eq!(asm_pinned_regs(data), vec![Some(Reg::Rdx), None]);
    }
}

#[cfg(test)]
mod arg_location_tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Function, Pseudo};
    use crate::target::Target;
    use crate::types::{TypeId, TypeTable};

    /// A function whose parameters have the given types, each with its
    /// `Arg(i)` pseudo.
    fn func_with_params(types: &TypeTable, params: &[TypeId]) -> Function {
        let mut func = Function::new("f", types.int_id);
        let mut block = BasicBlock::new(BasicBlockId(0));
        for (i, typ) in params.iter().enumerate() {
            func.add_param(format!("p{i}"), *typ);
            func.pseudos.push(Pseudo::arg(PseudoId(i as u32), i as u32));
        }
        block.insns.push(Instruction::new(Opcode::Ret));
        func.add_block(block);
        func
    }

    /// `named_incoming_end` is where the variadic arguments begin, and it has
    /// to account for named parameters that occupy the incoming area while
    /// consuming no register.
    ///
    /// A `long double` is X87 class and an aggregate over sixteen bytes is
    /// MEMORY class: System V AMD64 psABI 3.2.3 places both in memory and
    /// charges them no register at all. The tally this replaced derived the
    /// area's end from register *overflow* alone -- `max(gp - 6, 0) +
    /// max(fp - 8, 0)` eight-byte slots -- so it charged nothing for either,
    /// and `va_start` pointed `overflow_arg_area` back inside the named
    /// arguments. The expectations below are gcc's, read off its own output
    /// for the same signatures.
    #[test]
    fn named_incoming_end_covers_memory_class_params() {
        let types = TypeTable::new(&Target::host());
        let first = IncomingOff::FIRST.0;
        let int = types.int_id;
        let dbl = types.double_id;
        let ld = types.longdouble_id;

        // Six ints fill the GP file, the seventh stacks (8 bytes), then a
        // `long double` rounds to 16 and takes 16. End: 8 + 8 + 16 = 32.
        let func = func_with_params(&types, &[int, int, int, int, int, int, int, ld]);
        let mut ra = RegAlloc::new();
        ra.allocate(&func, &types);
        assert_eq!(ra.named_incoming_end(), first + 32);
        assert_eq!(ra.named_gp_regs(), 6, "the GP file is full, not over-full");
        assert_eq!(ra.named_fp_regs(), 0, "a long double takes no SSE register");

        // Seven doubles stay in XMM0-6; only the `long double` is stacked.
        let func = func_with_params(&types, &[dbl, dbl, dbl, dbl, dbl, dbl, dbl, ld]);
        let mut ra = RegAlloc::new();
        ra.allocate(&func, &types);
        assert_eq!(ra.named_incoming_end(), first + 16);
        assert_eq!(ra.named_gp_regs(), 0);
        assert_eq!(ra.named_fp_regs(), 7);

        // Nothing named at all: the area is empty and begins at its base.
        let func = func_with_params(&types, &[]);
        let mut ra = RegAlloc::new();
        ra.allocate(&func, &types);
        assert_eq!(ra.named_incoming_end(), first);
    }

    /// A function of `n` `long` parameters, each with its `Arg(i)` pseudo.
    fn func_with_int_params(types: &TypeTable, n: u32) -> Function {
        func_with_params(types, &vec![types.long_id; n as usize])
    }

    /// Where an `Arg` pseudo lives must be where the ABI actually puts it.
    #[test]
    fn arg_pseudos_land_where_the_abi_puts_them() {
        let types = TypeTable::new(&Target::host());
        // Six GP argument registers, then the incoming stack area.
        let func = func_with_int_params(&types, 9);

        let mut ra = RegAlloc::new();
        let locs = ra.allocate(&func, &types);

        let gp = Reg::arg_regs();
        for i in 0..6u32 {
            assert_eq!(
                locs.get(PseudoId(i)),
                Some(Loc::Reg(gp[i as usize])),
                "Arg({i}) should arrive in {:?}",
                gp[i as usize]
            );
        }

        // Past the sixth, arguments arrive in the caller's frame: 16 bytes up
        // from the frame pointer (saved rbp + return address), then every 8.
        for (i, want) in [(6u32, 16i32), (7, 24), (8, 32)] {
            assert_eq!(
                locs.get(PseudoId(i)),
                Some(Loc::IncomingArg(want)),
                "Arg({i}) should be an incoming stack argument at +{want}"
            );
        }

        // The shape Bug 3 described must not appear: an argument the ABI
        // passes in the caller's frame must use `IncomingArg`, never
        // `Loc::Stack`. The two are not distinguishable by sign on this
        // target -- `stack_mem` reads a `Loc::Stack` as a positive *slot
        // index* and emits `-(slot + callee_saved_offset)`, so a positive
        // value there is the callee's own frame -- which is precisely why the
        // separate variant, rather than a sign convention, is what fixed it.
        for i in 6..9u32 {
            assert!(
                matches!(locs.get(PseudoId(i)), Some(Loc::IncomingArg(_))),
                "Arg({i}) arrives in the caller's frame and must say so, got {:?}",
                locs.get(PseudoId(i))
            );
        }
    }

    /// The same invariant with floating-point parameters interleaved among
    /// integer ones, so the two argument cursors advance independently and a
    /// mistake in either shows up as a misplaced later argument.
    ///
    /// (Bug 3's report also named a variadic caller. `Function` carries no
    /// variadic flag -- that lives in the type -- so this covers the mixed
    /// cursors only.)
    #[test]
    fn arg_pseudos_land_correctly_when_mixed() {
        let types = TypeTable::new(&Target::host());

        let mut func = Function::new("g", types.int_id);
        let mut block = BasicBlock::new(BasicBlockId(0));
        // int, double, int, double, ... past both register files.
        let order = [
            types.long_id,
            types.double_id,
            types.long_id,
            types.double_id,
            types.long_id,
            types.long_id,
            types.long_id,
            types.long_id,
            types.long_id,
        ];
        for (i, typ) in order.iter().enumerate() {
            func.add_param(format!("p{i}"), *typ);
            func.pseudos.push(Pseudo::arg(PseudoId(i as u32), i as u32));
        }
        block.insns.push(Instruction::new(Opcode::Ret));
        func.add_block(block);

        let mut ra = RegAlloc::new();
        let locs = ra.allocate(&func, &types);

        for (i, typ) in order.iter().enumerate() {
            let id = PseudoId(i as u32);
            let loc = locs.get(id).unwrap_or_else(|| panic!("Arg({i}) unplaced"));
            // Every argument must be somewhere the ABI can name. A callee
            // stack slot is legitimate here: a parameter that arrived in an
            // XMM register is spilled to one, because every XMM is
            // caller-saved and any float computation would clobber it.
            assert!(
                matches!(
                    loc,
                    Loc::Reg(_) | Loc::Xmm(_) | Loc::IncomingArg(_) | Loc::Stack(_)
                ),
                "Arg({i}) of type {:?} got an impossible location {loc:?}",
                types.kind(*typ)
            );
        }

        // The seventh integer argument and beyond overflow to the caller's
        // frame even though XMM registers remain: the two cursors are
        // independent, and conflating them is what put a later argument at
        // the wrong offset.
        let stacked = (0..order.len())
            .filter(|i| matches!(locs.get(PseudoId(*i as u32)), Some(Loc::IncomingArg(_))))
            .count();
        assert!(
            stacked > 0,
            "with nine parameters at least one must overflow to the caller's frame"
        );
    }
}
