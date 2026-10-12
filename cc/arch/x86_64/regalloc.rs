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

use super::simd::{writes_spare_xmm, SPARE_XMM};
use super::x87::{is_x87_float_to_int, uses_x87_scratch};
use crate::arch::asm_constraints::{operand_pins, AsmOperandClass, AsmRegClass, PinnedGp};
use crate::arch::lir::FpSize;
use crate::arch::regalloc::{
    asm_clobbered, compute_live_intervals, find_call_positions, find_insn_positions,
    identify_fp_pseudos, ConstraintPoint, Crossing, LiveInterval, LivenessResult, SlotShape,
};
use crate::float::FloatVal;
use crate::ir::{AsmData, Function, Instruction, Opcode, PseudoId, PseudoKind};
use crate::types::TypeTable;
use crate::units::{Bits, ByteSize};
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
            | Opcode::Trap
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
/// A letter that names a register (`a` ... `D`) pins its operand there, as
/// does a register variable (`register long x asm("r10")`). `Q`
/// asks for any register with an addressable high byte, and gets the first of
/// %rax, %rbx, %rcx, %rdx the statement neither pins nor clobbers: the
/// allocator has no such class, so the choice is made here, once, and both
/// the allocator and the template substitution read it. A tied input takes
/// its output's register.
pub fn asm_pinned_regs(asm: &AsmData) -> Vec<Option<Reg>> {
    let named = |c: &AsmOperandClass| match c.reg {
        Some(AsmRegClass::Pinned(PinnedGp::X86_64(r))) => Some(r),
        _ => None,
    };
    let mut taken: Vec<Reg> = asm
        .outputs
        .iter()
        .chain(&asm.inputs)
        .filter_map(|c| named(&c.class))
        .collect();
    taken.extend(asm.clobbers.iter().filter_map(|c| parse_gp_clobber_name(c)));
    operand_pins(asm, |c| {
        if c.class.reg == Some(AsmRegClass::HighByte) {
            let free = [Reg::Rax, Reg::Rbx, Reg::Rcx, Reg::Rdx]
                .into_iter()
                .find(|r| !taken.contains(r));
            taken.extend(free);
            free
        } else {
            named(&c.class)
        }
    })
}

/// Every general register an inline asm of `func` may change: those it
/// clobbers, and those its operands are pinned to -- an input in `%rbx` is
/// loaded there by the function itself. A callee-saved one among them is the
/// function's to preserve, as any it allocates is, though no pseudo lives
/// in it.
fn asm_written_regs(func: &Function) -> impl Iterator<Item = Reg> + '_ {
    func.blocks
        .iter()
        .flat_map(|block| &block.insns)
        .filter(|insn| insn.op == Opcode::Asm)
        .filter_map(|insn| insn.extra().asm_data.as_deref())
        .flat_map(|asm| {
            let pinned = asm_pinned_regs(asm).into_iter().flatten();
            let clobbered = asm.clobbers.iter().filter_map(|c| parse_gp_clobber_name(c));
            pinned.chain(clobbered).collect::<Vec<_>>()
        })
}

/// The number of the vector register a clobber-list name means: `xmm3`,
/// `ymm3` and `zmm3` all write register 3, optionally with gcc's leading
/// `%`. `None` for any other name.
fn parse_xmm_clobber_name(raw: &str) -> Option<u8> {
    let s = raw.trim_start_matches('%').to_ascii_lowercase();
    let n = ["xmm", "ymm", "zmm"]
        .iter()
        .find_map(|p| s.strip_prefix(p))?;
    n.parse().ok().filter(|&n: &u8| n < 32)
}

/// Map a clobber-list register name (lowercase, GCC-style) to the
/// corresponding `Reg`. Accepts the 64-bit canonical name (`rax`,
/// `r10`, ...), the 32/16/8-bit alias (`eax`, `ax`, `al`, `r10d`,
/// `r10w`, `r10b`), and the GCC-style "%rax" with leading `%`.
/// Returns `None` for names the GP table doesn't know about
/// (XMM registers, `memory`, `cc`, x87 stack, ...).
pub(crate) fn parse_gp_clobber_name(raw: &str) -> Option<Reg> {
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

    if insn.returns_twice() {
        return Some(crate::arch::regalloc::returns_twice_constraint(
            insn,
            Reg::allocatable(),
        ));
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
    /// XMM14 and XMM15 are reserved as scratch registers for codegen operations.
    /// XMM13 is allocatable, but kept free where a SIMD sequence needs a third
    /// temporary (`simd::SPARE_XMM`).
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
    Stack(i64),
    /// Incoming stack argument at [rbp + offset] (positive offset, above return address)
    /// Used for function parameters 7+ that are passed on the stack by the caller
    IncomingArg(i64),
    /// Immediate integer constant
    Imm(i128),
    /// Immediate float constant (value, size in bits)
    FImm(FloatVal, Bits),
    /// Global symbol
    Global(String),
}

/// A register as the allocation verifier compares them: one per bank.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum RegUnit {
    Gp(Reg),
    Xmm(XmmReg),
}

/// Every general register.
const EVERY_GP: [Reg; 16] = [
    Reg::Rax,
    Reg::Rbx,
    Reg::Rcx,
    Reg::Rdx,
    Reg::Rsi,
    Reg::Rdi,
    Reg::Rbp,
    Reg::Rsp,
    Reg::R8,
    Reg::R9,
    Reg::R10,
    Reg::R11,
    Reg::R12,
    Reg::R13,
    Reg::R14,
    Reg::R15,
];

/// Information about an argument spilled from a caller-saved register to stack
#[derive(Debug, Clone)]
pub struct SpilledArg {
    /// The pseudo that was spilled
    pub pseudo: PseudoId,
    /// The register the argument originally arrived in
    pub from_reg: Reg,
    /// The stack offset where it was spilled to
    pub to_stack_offset: i64,
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
    pub to_stack_offset: i64,
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
    /// Whether the function gets a stack-protector canary; set with
    /// [`RegAlloc::with_stack_guard`].
    guard_requested: bool,
    /// The canary's slot: the frame's first, so above every other local.
    guard_slot: Option<i64>,
    /// What `%rsp` is a multiple of at a call; see
    /// [`RegAlloc::with_incoming_stack_align`].
    incoming_stack_align: i32,
    /// Registers the unit's file-scope register variables are: never
    /// allocated, saved or restored in any of its functions. Set with
    /// `with_global_registers`.
    global_registers: Vec<Reg>,
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
    named_incoming_end: i64,
    /// Next stack slot offset
    stack_offset: i64,
    /// Reserved when the function converts a long double to an integer.
    x87_scratch: Option<X87Scratch>,
    x87_control_words: Option<X87ControlWords>,
    /// A source position for this function, for the frame diagnostics
    /// `grow_frame` and [`crate::abi::slot_bytes`] emit. `new_frame_slot` and
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
    int128_incoming: BTreeMap<PseudoId, i64>,
    /// Arguments spilled from caller-saved registers to stack
    spilled_args: Vec<SpilledArg>,
    /// XMM arguments spilled from XMM registers to stack
    spilled_xmm_args: Vec<SpilledXmmArg>,
    /// The pseudos that live in a frame slot, with the slot each needs:
    /// spills, arguments moved out of their registers, 128-bit values, alloca
    /// pointers. Their offsets are decided together, once both banks are
    /// colored ([`crate::arch::regalloc::color_stack_slots`]); a spilled
    /// argument's record is completed then.
    stack_homed: BTreeMap<PseudoId, SlotShape>,
    /// Per-block live-in sets for interference-based stack coloring
    live_in: Vec<HashSet<PseudoId>>,
    /// Per-block live-out sets for interference-based stack coloring
    live_out: Vec<HashSet<PseudoId>>,
    /// Maximum alignment requirement of any local variable (for dynamic stack alignment)
    max_local_align: i32,
    /// How this function's locals are addressed.
    frame_base: FrameBase,
    /// The register held back for displacements past 32 bits, if the frame
    /// may have any; see [`far_scratch_of`].
    far_scratch: Option<Reg>,
    /// The size of every frame slot handed to a pseudo; see
    /// [`crate::arch::regalloc::SlotBytes`].
    slot_bytes: crate::arch::regalloc::SlotBytes,
    /// An argument arriving in more than one register -- a `double _Complex`
    /// in two XMM registers -- by how many. Its location names the first.
    arg_span: HashMap<PseudoId, usize>,
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
    fn of(
        func: &Function,
        types: &TypeTable,
        global_registers: &[Reg],
        incoming_align: i32,
    ) -> FrameBase {
        let local_align = func
            .locals
            .values()
            .map(|local| {
                local
                    .explicit_align
                    .map(|a| a as i32)
                    .unwrap_or_else(|| (crate::abi::slot_align(types.align(local.typ))).max(8))
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
            .map(|t| crate::abi::slot_align(types.align(*t)))
            .max()
            .unwrap_or(8);
        // `%rbp` is as aligned as the stack was at the call, 16 bytes or,
        // under `-mpreferred-stack-boundary=3`, 8.
        let align = local_align.max(call_align);
        if align <= incoming_align {
            return FrameBase::Rbp;
        }

        let mut claimed = Self::asm_claimed_regs(func);
        claimed.extend(global_registers.iter().copied());
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

/// The register a function whose frame may reach past `i32` holds back as
/// its *far scratch*: `super::legalize` loads each displacement no 32-bit
/// field holds into it with `movabsq` and addresses through it.
///
/// Decided before allocation, from `crate::arch::regalloc::frame_reach`, an
/// upper bound on every frame displacement, so allocation runs once; a
/// function under the bound -- every frame under two gigabytes -- gives
/// nothing up and comes out as it did. Callee-saved, so the prologue saves
/// it like any register the function writes; and one the function's inline
/// asm has not claimed, nor the frame base, nor a global register variable,
/// for the reasons [`FrameBase::of`] gives. Taken from the far end of the
/// callee-saved set, where the frame base is taken from the near one.
fn far_scratch_of(
    func: &Function,
    types: &TypeTable,
    global_registers: &[Reg],
    frame_base: FrameBase,
) -> Option<Reg> {
    if crate::arch::regalloc::frame_reach(func, types) <= i32::MAX as u64 {
        return None;
    }
    let mut claimed = FrameBase::asm_claimed_regs(func);
    claimed.extend(global_registers.iter().copied());
    claimed.extend(frame_base.reg());
    const CANDIDATES: [Reg; 5] = [Reg::R15, Reg::R14, Reg::R13, Reg::R12, Reg::Rbx];
    let reg = CANDIDATES.into_iter().find(|r| !claimed.contains(r));
    if reg.is_none() {
        crate::diag::error(
            crate::arch::func_pos(func),
            "inline asm claims every callee-saved register, leaving none \
             to address this function's frame past two gigabytes",
        );
    }
    reg
}

/// Whether a constraint point may leave `p` in a register it clobbers --
/// `across` says whether `p` is live across it: only an operand of the
/// instruction, and only one not live across it (see
/// [`ConstraintPoint::operand_survives`]). The one rule for x86-64, asked
/// both when coloring and when deciding whether an ABI-pinned argument must
/// leave its register.
fn exempt_from_clobber(cp: &ConstraintPoint<Reg>, p: PseudoId, across: bool) -> bool {
    cp.operand_survives(p, across)
}

/// The arguments of `insn` that a System V call reads from the very
/// register it passes them in, each with that register: none, unless every
/// argument is an integer or pointer scalar in a register.
///
/// In general an argument may not be in a caller-saved register at its
/// call, and every argument counts as crossing it
/// ([`Crossing::call_crossers`]): `push_stack_args` stages stacked arguments
/// through `%rax` and copies aggregates with `rep movsq`, and
/// `setup_register_args` writes the argument registers in order, while
/// `save_clobbered_arg_regs` can rescue only two sources an earlier write
/// would overwrite. A call with nothing on the stack and only scalar integer
/// arguments writes each argument register once, from its own argument; one
/// already there is read in place, by no move at all, and nothing before it
/// writes that register. So such an argument may live there -- in that
/// register only -- and the value passed from one call's result to the next
/// needs no callee-saved register. An argument read twice, or also the
/// call's target, is left out.
fn args_in_place(insn: &Instruction, types: &TypeTable) -> Vec<(PseudoId, Reg)> {
    let system_v = insn
        .extra()
        .abi_info
        .as_ref()
        .is_some_and(|ai| ai.conv != crate::abi::CallingConv::Win64);
    let arg_types = &insn.extra().arg_types;
    let scalar = |t: &crate::types::TypeId| {
        (types.is_integer(*t) || types.kind(*t) == crate::types::TypeKind::Pointer)
            && !types.is_complex(*t)
            && types.byte_size(*t) <= ByteSize::new(8)
    };
    if insn.op != Opcode::Call
        || !system_v
        || insn.src.len() > Reg::arg_regs().len()
        || arg_types.len() != insn.src.len()
        || !arg_types.iter().all(scalar)
    {
        return Vec::new();
    }
    let read = |p: PseudoId| {
        insn.src.iter().filter(|&&s| s == p).count()
            + usize::from(insn.extra().indirect_target == Some(p))
    };
    insn.src
        .iter()
        .zip(Reg::arg_regs())
        .filter(|&(&p, _)| read(p) == 1)
        .map(|(&p, &r)| (p, r))
        .collect()
}

/// What crosses each call and each constraint point that matters, by exact
/// liveness ([`crate::arch::regalloc::crossings`]): the one answer every
/// clobber question of this allocator reads.
struct Clobbering {
    crossings: HashMap<usize, Crossing>,
    /// Each call's position, with the arguments it reads in place.
    calls: Vec<(usize, Vec<(PseudoId, Reg)>)>,
    /// The positions that destroy every XMM register
    /// ([`RegAlloc::fp_call_positions`]).
    fp_calls: Vec<usize>,
    /// The instructions that may write [`SPARE_XMM`].
    spare: Vec<usize>,
    /// The constraint points that clobber a register a value may be in:
    /// the others clobber only the scratch R10/R11.
    points: Vec<ConstraintPoint<Reg>>,
}

impl Clobbering {
    fn of(
        func: &Function,
        types: &TypeTable,
        live_out: &[HashSet<PseudoId>],
        calls: &[usize],
        fp_calls: Vec<usize>,
        constraint_points: &[ConstraintPoint<Reg>],
        allocatable: &[Reg],
    ) -> Self {
        let spare = find_call_positions(func, writes_spare_xmm);
        let points: Vec<ConstraintPoint<Reg>> = constraint_points
            .iter()
            .filter(|cp| {
                cp.clobbers
                    .iter()
                    .any(|r| allocatable.contains(r) || Reg::arg_regs().contains(r))
            })
            .cloned()
            .collect();
        let mut positions: std::collections::BTreeSet<usize> = calls.iter().copied().collect();
        positions.extend(&fp_calls);
        positions.extend(&spare);
        positions.extend(points.iter().map(|cp| cp.position));
        let insns: Vec<&Instruction> = func.blocks.iter().flat_map(|b| &b.insns).collect();
        Self {
            crossings: crate::arch::regalloc::crossings(func, live_out, &positions),
            calls: calls
                .iter()
                .map(|&pos| (pos, args_in_place(insns[pos], types)))
                .collect(),
            fp_calls,
            spare,
            points,
        }
    }

    /// Every pseudo some call touches, in the general bank's sense.
    fn gp_call_touched(&self) -> HashSet<PseudoId> {
        self.calls
            .iter()
            .flat_map(|(pos, _)| self.crossings[pos].call_crossers())
            .collect()
    }

    /// Every pseudo that crosses an instruction destroying the XMM
    /// registers, an argument and an asm's output included
    /// ([`Crossing::bank_crossers`]): none of them may be in one.
    fn fp_call_crossers(&self) -> HashSet<PseudoId> {
        self.fp_calls
            .iter()
            .flat_map(|pos| self.crossings[pos].bank_crossers())
            .collect()
    }

    /// Keeps [`SPARE_XMM`] free wherever an instruction may write it: no
    /// candidate the instruction touches -- an operand included, since the
    /// sequence writes it while its operands are still wanted -- may take it.
    fn spare_xmm_forbidden(
        &self,
        xmm_candidates: &std::collections::BTreeSet<PseudoId>,
    ) -> BTreeMap<PseudoId, std::collections::BTreeSet<XmmReg>> {
        self.spare
            .iter()
            .flat_map(|pos| self.crossings[pos].touching())
            .filter(|(p, _)| xmm_candidates.contains(p))
            .map(|(p, _)| (p, std::collections::BTreeSet::from([SPARE_XMM])))
            .collect()
    }
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
    slot: i64,
}

impl X87Scratch {
    /// Two eight-byte halves: `emit_x87_float_to_u64` uses both at once.
    pub(super) const BYTES: i64 = 16;

    pub(super) fn slot(self) -> i64 {
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
    slot: i64,
}

impl X87ControlWords {
    /// Bytes reserved: the two 16-bit words, rounded up to the eight every
    /// other slot is laid out in, so the slots after this one stay aligned.
    const BYTES: i64 = 8;

    /// The stack slot the two words are fields of.
    pub(super) fn slot(self) -> i64 {
        self.slot
    }

    /// Byte offset of the control word found on entry, restored afterwards.
    pub(super) const SAVED: i64 = 0;

    /// Byte offset of the truncating copy loaded around the `fistp`.
    pub(super) const TRUNCATING: i64 = 2;
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
struct IncomingOff(i64);

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
    fn take(next: &mut IncomingOff, bytes: i64, align: i32) -> i64 {
        let align = i64::from(align.max(8));
        let base = IncomingOff::FIRST.0;
        let at = base.saturating_add((next.0 - base + align - 1) & !(align - 1));
        next.0 = at.saturating_add((bytes + 7) & !7);
        at
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
            stack_homed: BTreeMap::new(),
            live_in: Vec::new(),
            live_out: Vec::new(),
            max_local_align: 8,
            frame_base: FrameBase::Rbp,
            far_scratch: None,
            slot_bytes: HashMap::new(),
            arg_span: HashMap::new(),
            tls_access: crate::target::TlsAccess::ElfStatic,
            guard_requested: false,
            guard_slot: None,
            global_registers: Vec::new(),
            incoming_stack_align: 16,
        }
    }

    /// The allocator for a unit whose file-scope register variables are
    /// `regs` (`register long r asm("r14")`): each is the variable for the
    /// whole unit, so no function may allocate it, use it for its frame
    /// base, or save and restore it -- that would undo a write to the
    /// variable on return.
    pub fn with_global_registers(mut self, regs: &[Reg]) -> Self {
        self.global_registers = regs.to_vec();
        self
    }

    /// An allocator for code that may count on `%rsp` being a multiple of
    /// `bytes` at a call: 16, or 8 under `-mpreferred-stack-boundary=3`,
    /// where a local or argument aligned to 16 needs the frame realigned.
    pub fn with_incoming_stack_align(mut self, bytes: i32) -> Self {
        self.incoming_stack_align = bytes;
        self
    }

    /// An allocator that reserves a stack-protector canary slot when
    /// `guarded`; see `arch::stack_protect`.
    pub fn with_stack_guard(mut self, guarded: bool) -> Self {
        self.guard_requested = guarded;
        self
    }

    /// The canary's slot, if [`RegAlloc::with_stack_guard`] asked for one.
    pub fn stack_guard_slot(&self) -> Option<i64> {
        self.guard_slot
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
        self.frame_base = FrameBase::of(
            func,
            types,
            &self.global_registers,
            self.incoming_stack_align,
        );
        if let Some(base) = self.frame_base.reg() {
            // The prologue writes it, so the function must save it.
            self.used_callee_saved.push(base);
        }
        self.far_scratch = far_scratch_of(func, types, &self.global_registers, self.frame_base);
        if let Some(scratch) = self.far_scratch {
            // So does the legalizer, wherever it needs it.
            self.used_callee_saved.push(scratch);
        }
        let allocatable = self.allocatable_regs();
        // Before any other slot: the first is the highest, nearest the saved
        // registers, which is where the canary has to be -- and the arrays
        // right under it.
        self.guard_slot = self.guard_requested.then(|| self.new_frame_slot(8, 8));
        let (arrays, rest) = crate::arch::regalloc::LocalSet::of(self.guard_requested);
        if let Some(arrays) = arrays {
            let early = self.compute_live_intervals(func);
            self.place_locals(func, types, &early.intervals, arrays);
        }
        let win64 = func.conv == crate::abi::CallingConv::Win64;
        if win64 {
            // Less a global register, which is the variable's, never saved.
            let saved = super::win64::CALLEE_SAVED_GP
                .into_iter()
                .filter(|r| !self.global_registers.contains(r));
            self.used_callee_saved.extend(saved);
        }
        if func.receives_nonlocal_goto() {
            for &reg in &allocatable {
                if reg.is_callee_saved() && !self.used_callee_saved.contains(&reg) {
                    self.used_callee_saved.push(reg);
                }
            }
        }
        // The frame pointer is the prologue's own, saved already, and a
        // global register is the variable's, never saved.
        for reg in asm_written_regs(func).filter(|r| allocatable.contains(r)) {
            if reg.is_callee_saved() && !self.used_callee_saved.contains(&reg) {
                self.used_callee_saved.push(reg);
            }
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
        let clobbering = Clobbering::of(
            func,
            types,
            &self.live_out,
            &call_positions,
            self.fp_call_positions(func),
            &constraint_points,
            &self.allocatable_regs(),
        );

        self.spill_args_across_calls(func, types, &intervals, &clobbering);
        self.allocate_alloca_to_stack(func);
        self.place_locals(func, types, &intervals, rest);
        self.run_chordal_color(func, intervals, &clobbering);
        self.place_stack_slots(func);
        if win64 {
            self.win64_xmm_saves = self.win64_xmm_to_save(func);
        }
        if cfg!(debug_assertions) {
            self.verify(func, &constraint_points, &call_positions);
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
    /// inline asm. Otherwise it is the ones the allocator handed out, the
    /// two scratch registers, which the back end writes without asking it,
    /// and [`SPARE_XMM`] where an instruction may write it.
    fn win64_xmm_to_save(&self, func: &Function) -> Vec<XmmReg> {
        let insns = || func.blocks.iter().flat_map(|b| &b.insns);
        let calls = insns().any(|insn| {
            is_call_like_x86_64(insn.op) || matches!(insn.op, Opcode::Asm | Opcode::TlsAddr)
        });
        let spare = insns().any(|insn| writes_spare_xmm(insn.op));
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
                calls
                    || matches!(x, XmmReg::Xmm14 | XmmReg::Xmm15)
                    || (spare && *x == SPARE_XMM)
                    || allocated.contains(x)
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
    ///
    /// An inline asm that clobbers an XMM register is one too: the clobber
    /// list is all the allocator knows of what the template writes.
    fn fp_call_positions(&self, func: &Function) -> Vec<usize> {
        let tlv = self.tls_access == crate::target::TlsAccess::MachOTlv;
        find_insn_positions(func, |insn| {
            is_call_like_x86_64(insn.op)
                || (tlv && insn.op == Opcode::TlsAddr)
                || !asm_clobbered(insn, parse_xmm_clobber_name).is_empty()
        })
    }

    /// Reset allocator state for a new function
    fn reset_state(&mut self) {
        self.locations.clear();
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
        self.stack_homed.clear();
        self.live_in.clear();
        self.live_out.clear();
        self.max_local_align = 8;
        self.slot_bytes.clear();
        self.arg_span.clear();
    }

    /// Reserve the [`X87Scratch`] if an instruction of `func` stages a value
    /// through it, and the [`X87ControlWords`] if one converts a long double
    /// to an integer.
    fn reserve_x87_frame(&mut self, func: &Function, types: &TypeTable) {
        let insns = || func.blocks.iter().flat_map(|block| &block.insns);
        let frame_align = self.frame_align();
        let mut reserve = |bytes: i64| {
            crate::arch::regalloc::grow_frame(
                &mut self.stack_offset,
                bytes,
                bytes as i32,
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
                if c.size > Bits::new(64) {
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
                if c.size > Bits::new(64) {
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
                // A 16-byte vector operation's operands and result are its
                // carrier's whole XMM register, as a binary128's are.
                let is_quad = insn
                    .typ
                    .is_some_and(|t| types.kind(t) == crate::types::TypeKind::Float128)
                    || (matches!(insn.op, Opcode::Simd(_)) && insn.size > Bits::new(64));
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
            } else if sse_struct.is_some() && types.byte_size(*typ) <= ByteSize::new(8) {
                // One eightbyte, arriving in an XMM register as a value: the
                // parameter pseudo *is* that value, and the linearizer's
                // small-struct path stores it straight into the local. Larger
                // ones travel by address and are handled below.
                if fp_arg_idx < fp_arg_regs.len() {
                    self.locations
                        .insert(pseudo_id, Loc::Xmm(fp_arg_regs[fp_arg_idx]));
                    self.fp_pseudos.insert(pseudo_id);
                    spend_arg_regs(&mut fp_arg_idx, 1, fp_arg_regs.len());
                } else {
                    self.locations.insert(
                        pseudo_id,
                        Loc::IncomingArg(IncomingOff::take(
                            &mut stack_arg_offset,
                            8,
                            crate::abi::slot_align(types.align(*typ)),
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
                                types.byte_size(*typ),
                                self.func_pos,
                                "a stacked parameter",
                            ),
                            crate::abi::slot_align(types.align(*typ)),
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
                                types.byte_size(*typ),
                                self.func_pos,
                                "a stacked parameter",
                            ),
                            crate::abi::slot_align(types.align(*typ)),
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
                                types.byte_size(*typ),
                                self.func_pos,
                                "a stacked parameter",
                            ),
                            crate::abi::slot_align(types.align(*typ)),
                        )),
                    );
                } else if fp_arg_idx + sse_regs <= fp_arg_regs.len() {
                    self.locations
                        .insert(pseudo_id, Loc::Xmm(fp_arg_regs[fp_arg_idx]));
                    self.arg_span.insert(pseudo_id, sse_regs);
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
                                types.byte_size(*typ),
                                self.func_pos,
                                "a stacked parameter",
                            ),
                            crate::abi::slot_align(types.align(*typ)),
                        )),
                    );
                }
            } else if is_fp {
                if fp_arg_idx < fp_arg_regs.len() {
                    self.locations
                        .insert(pseudo_id, Loc::Xmm(fp_arg_regs[fp_arg_idx]));
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
                                types.byte_size(*typ),
                                self.func_pos,
                                "a stacked parameter",
                            ),
                            crate::abi::slot_align(types.align(*typ)),
                        )),
                    );
                }
                spend_arg_regs(&mut fp_arg_idx, 1, fp_arg_regs.len());
            } else if types.is_plain_int128(*typ) {
                // __int128: uses two GP registers when available.
                // Always lives in a frame slot — for register params,
                // store_args_to_stack stores register values; for stack params,
                // store_args_to_stack copies from the incoming arg area.
                self.home_on_stack(pseudo_id, SlotShape::new(16, 16));
                self.int128_pseudos.insert(pseudo_id);
                if int_arg_idx + 1 < int_arg_regs.len() {
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
                                types.byte_size(*typ),
                                self.func_pos,
                                "a stacked parameter",
                            ),
                            crate::abi::slot_align(types.align(*typ)),
                        )),
                    );
                    // Don't increment int_arg_idx — no GP register consumed
                } else if int_arg_idx < int_arg_regs.len() {
                    self.locations
                        .insert(pseudo_id, Loc::Reg(int_arg_regs[int_arg_idx]));
                    spend_arg_regs(&mut int_arg_idx, 1, int_arg_regs.len());
                } else {
                    // Stack args are placed in parameter order per System V AMD64 ABI
                    self.locations.insert(
                        pseudo_id,
                        Loc::IncomingArg(IncomingOff::take(
                            &mut stack_arg_offset,
                            8,
                            crate::abi::slot_align(types.align(*typ)),
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

    /// Move an argument out of the register it arrived in when that register
    /// is overwritten while the argument is live: a general register by the
    /// rule every value is colored by ([`RegAlloc::gp_forbidden`]) -- a call
    /// it crosses, or a constraint point that clobbers it, such as a variable
    /// shift loading its count into RCX while the fourth parameter still
    /// lives there -- and an XMM register where it crosses a call.
    fn spill_args_across_calls(
        &mut self,
        func: &Function,
        types: &TypeTable,
        intervals: &[LiveInterval],
        clobbering: &Clobbering,
    ) {
        let args: std::collections::BTreeSet<PseudoId> = self
            .locations
            .iter()
            .filter(|(_, loc)| matches!(loc, Loc::Reg(r) if Reg::arg_regs().contains(r)))
            .map(|(&p, _)| p)
            .collect();
        let forbidden = self.gp_forbidden(clobbering, &args);
        self.spill_gp_args(intervals, |p, reg| {
            forbidden.get(&p).is_some_and(|regs| regs.contains(&reg))
        });

        // An XMM argument crossing an instruction that destroys the XMM
        // registers -- a call: System V preserves none -- is stored to a
        // slot at entry. One that crosses none stays where it arrived, a
        // precolored vertex the other values are colored around.
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
        let crossers = clobbering.fp_call_crossers();
        for interval in intervals {
            if complex_arg_pseudos.contains(&interval.pseudo)
                || !crossers.contains(&interval.pseudo)
            {
                continue;
            }
            if let Some(Loc::Xmm(xmm)) = self.locations.get(&interval.pseudo) {
                if xmm_arg_regs.contains(xmm) && interval.start == 0 {
                    let from_xmm = *xmm;
                    let is_quad = self.quad_pseudos.contains(&interval.pseudo);
                    let bytes = if is_quad { 16 } else { 8 };
                    // The slot's offset is filled in once it is placed.
                    self.spilled_xmm_args.push(SpilledXmmArg {
                        pseudo: interval.pseudo,
                        from_xmm,
                        to_stack_offset: 0,
                        size: if is_quad {
                            FpSize::Quad
                        } else {
                            FpSize::Double
                        },
                    });
                    self.home_on_stack(interval.pseudo, SlotShape::new(bytes, bytes));
                }
            }
        }
    }

    /// Where a stack-passed `__int128` parameter arrives, if it did.
    pub fn int128_incoming(&self, pseudo: PseudoId) -> Option<i64> {
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
    pub fn named_incoming_end(&self) -> i64 {
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
    /// something destroys that register while the argument is live. See
    /// [`crate::arch::regalloc::gp_args_overwritten`].
    fn spill_gp_args(
        &mut self,
        intervals: &[LiveInterval],
        overwritten: impl Fn(PseudoId, Reg) -> bool,
    ) {
        let spills = crate::arch::regalloc::gp_args_overwritten(
            intervals,
            |p| match self.locations.get(&p) {
                Some(Loc::Reg(r)) if Reg::arg_regs().contains(r) => Some(*r),
                _ => None,
            },
            overwritten,
        );
        for (pseudo, from_reg) in spills {
            // The slot's offset is filled in once it is placed.
            self.spilled_args.push(SpilledArg {
                pseudo,
                from_reg,
                to_stack_offset: 0,
            });
            self.home_on_stack(pseudo, SlotShape::new(8, 8));
        }
    }

    /// Alloca results live in the frame.
    fn allocate_alloca_to_stack(&mut self, func: &Function) {
        for p in crate::arch::regalloc::alloca_results(func) {
            self.home_on_stack(p, SlotShape::new(8, 8));
        }
    }

    /// A fresh frame slot of `size` bytes at `alignment`, shared with nothing.
    fn new_frame_slot(&mut self, size: i64, alignment: i32) -> i64 {
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
    fn place_locals(
        &mut self,
        func: &Function,
        types: &TypeTable,
        intervals: &[LiveInterval],
        set: crate::arch::regalloc::LocalSet,
    ) {
        let pos = self.func_pos;
        let placed =
            crate::arch::regalloc::place_locals(func, types, pos, intervals, set, |b, a| {
                self.new_frame_slot(b, a)
            });
        for (local, offset, bytes) in placed {
            self.locations.insert(local, Loc::Stack(offset));
            self.slot_bytes.insert(local, bytes);
            if func.local_of(local).is_some_and(|l| types.is_float(l.typ)) {
                self.fp_pseudos.insert(local);
            }
        }
    }

    /// `pseudo` lives in a frame slot of `shape`, wherever it was before. The
    /// slot is placed by [`RegAlloc::place_stack_slots`].
    fn home_on_stack(&mut self, pseudo: PseudoId, shape: SlotShape) {
        self.locations.remove(&pseudo);
        self.stack_homed.insert(pseudo, shape);
    }

    /// Has `pseudo` a home already: a location, or a frame slot to come?
    fn is_placed(&self, pseudo: PseudoId) -> bool {
        self.locations.contains_key(&pseudo) || self.stack_homed.contains_key(&pseudo)
    }

    /// Lay out the frame slots of every stack-homed pseudo, sharing between
    /// those never live together
    /// ([`crate::arch::regalloc::color_stack_slots`]), and complete the
    /// spilled arguments' records with where they went. The most aligned
    /// slots first, so that rounding pads the frame once.
    fn place_stack_slots(&mut self, func: &Function) {
        let homed = std::mem::take(&mut self.stack_homed);
        let coloring = crate::arch::regalloc::color_stack_slots(func, &self.live_out, &homed);
        let mut order: Vec<usize> = (0..coloring.slots.len()).collect();
        order.sort_by_key(|&n| (std::cmp::Reverse(coloring.slots[n].align), n));
        let mut offset = vec![0; coloring.slots.len()];
        for n in order {
            let shape = coloring.slots[n];
            offset[n] = self.new_frame_slot(shape.bytes.into(), shape.align);
        }
        for (&p, &n) in &coloring.slot_of {
            self.locations.insert(p, Loc::Stack(offset[n]));
            self.slot_bytes.insert(p, homed[&p].bytes.into());
        }
        let slot = |p: PseudoId| offset[coloring.slot_of[&p]];
        for arg in &mut self.spilled_args {
            arg.to_stack_offset = slot(arg.pseudo);
        }
        for arg in &mut self.spilled_xmm_args {
            arg.to_stack_offset = slot(arg.pseudo);
        }
    }

    /// Chordal coloring, with spill-on-fail for uncolorable vertices.
    ///
    /// Three phases:
    ///   1. Pre-pass: route non-register-allocated pseudos to their
    ///      proper Locs (constants → Imm/FImm, Sym → stack slot,
    ///      int128 → 16-byte stack, long-double → x87 stack, FP
    ///      crossing a call → stack, every XMM being caller-saved).
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
        clobbering: &Clobbering,
    ) {
        // -------- Phase 1: pre-pass --------
        let crosses_calls = clobbering.fp_call_crossers();
        let setval_sizes = crate::arch::regalloc::setval_sizes(func);
        let mut gp_candidates: std::collections::BTreeSet<PseudoId> =
            std::collections::BTreeSet::new();
        let mut xmm_candidates: std::collections::BTreeSet<PseudoId> =
            std::collections::BTreeSet::new();
        for interval in &intervals {
            if self.is_placed(interval.pseudo) {
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
                        let defined_128 =
                            setval_sizes.get(&interval.pseudo) == Some(&Bits::new(128));
                        if defined_128 {
                            self.home_on_stack(interval.pseudo, SlotShape::new(16, 16));
                            continue;
                        }
                        self.locations.insert(interval.pseudo, Loc::Imm(*v));
                        continue;
                    }
                    PseudoKind::FVal(v) => {
                        let size = setval_sizes
                            .get(&interval.pseudo)
                            .copied()
                            .unwrap_or(Bits::new(64));
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
                self.home_on_stack(interval.pseudo, SlotShape::new(16, 16));
                continue;
            }
            let needs_fp = self.fp_pseudos.contains(&interval.pseudo);
            if needs_fp {
                if self.ld_pseudos.contains(&interval.pseudo) {
                    self.home_on_stack(interval.pseudo, SlotShape::new(16, 16));
                    continue;
                }
                // System V preserves no XMM register across a call, so a
                // value crossing one lives in memory -- sixteen bytes for a
                // `__float128`, whose 16-byte store ran over an 8-byte slot's
                // neighbour. Every other value is colored, whatever blocks it
                // spans.
                if crosses_calls.contains(&interval.pseudo) {
                    let bytes = self.fp_slot_bytes(interval.pseudo);
                    self.home_on_stack(interval.pseudo, SlotShape::new(bytes, bytes));
                    continue;
                }
                xmm_candidates.insert(interval.pseudo);
            } else {
                gp_candidates.insert(interval.pseudo);
            }
        }

        // -------- Phase 2: per-bank chordal coloring --------
        self.color_gp_bank(func, &intervals, clobbering, &gp_candidates);
        self.color_xmm_bank(func, clobbering, &xmm_candidates);
    }

    /// The general registers each of `candidates` may not take: every
    /// register a constraint point it touches clobbers, unless the point
    /// excuses it ([`exempt_from_clobber`]), and every caller-saved register
    /// where it crosses a call ([`Crossing::call_crossers`]) -- less, for an
    /// argument of the call, the register the call takes it in, where the
    /// call reads it in place ([`args_in_place`]).
    fn gp_forbidden(
        &self,
        clobbering: &Clobbering,
        candidates: &std::collections::BTreeSet<PseudoId>,
    ) -> BTreeMap<PseudoId, std::collections::BTreeSet<Reg>> {
        let mut forbidden = crate::arch::regalloc::constraint_clobbers(
            &clobbering.points,
            &clobbering.crossings,
            candidates,
            exempt_from_clobber,
        );
        // The argument registers are all caller-saved, allocatable or not.
        let mut caller_saved: Vec<Reg> = self
            .allocatable_regs()
            .into_iter()
            .filter(|r| !r.is_callee_saved())
            .collect();
        for &r in Reg::arg_regs() {
            if !caller_saved.contains(&r) {
                caller_saved.push(r);
            }
        }
        for (pos, in_place) in &clobbering.calls {
            let crossing = &clobbering.crossings[pos];
            for p in crossing.call_crossers() {
                if !candidates.contains(&p) {
                    continue;
                }
                let kept = in_place
                    .iter()
                    .find(|&&(arg, _)| arg == p && !crossing.across.contains(&p))
                    .map(|&(_, r)| r);
                forbidden
                    .entry(p)
                    .or_default()
                    .extend(caller_saved.iter().filter(|&&r| Some(r) != kept));
            }
        }
        forbidden
    }

    fn color_gp_bank(
        &mut self,
        func: &Function,
        intervals: &[LiveInterval],
        clobbering: &Clobbering,
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
        // Each asm's outputs, by position: an asm writing its own pinned
        // output -- a `"+a"` operand, live on both sides -- is the value
        // arriving in that register, not something overwriting it.
        // An `asm goto` output, though, has to be in its register: the jump
        // skips any move after the template, so it is precolored as before.
        let mut asm_outputs_at: HashMap<usize, Vec<PseudoId>> = HashMap::new();
        let mut goto_outputs: HashSet<PseudoId> = HashSet::new();
        for (pos, insn) in func.blocks.iter().flat_map(|b| &b.insns).enumerate() {
            if let Some(asm) = insn.extra().asm_data.as_deref() {
                let outputs = asm.outputs.iter().map(|o| o.pseudo);
                if !asm.goto_labels.is_empty() {
                    goto_outputs.extend(outputs.clone());
                }
                asm_outputs_at.insert(pos, outputs.collect());
            }
        }
        let writes_it = |cp: &ConstraintPoint<Reg>, p: PseudoId, across: bool| {
            exempt_from_clobber(cp, p, across)
                || asm_outputs_at
                    .get(&cp.position)
                    .is_some_and(|outs| outs.contains(&p))
        };
        let allocatable = self.allocatable_regs();
        let asm_pins = collect_asm_fixed_precolors_x86_64(func);
        let pinned: std::collections::BTreeSet<PseudoId> = asm_pins.keys().copied().collect();
        let pins_clobbered = crate::arch::regalloc::constraint_clobbers(
            &clobbering.points,
            &clobbering.crossings,
            &pinned,
            writes_it,
        );
        for (pid, reg) in asm_pins {
            // Only pre-color if the pseudo is a GP candidate. The
            // lowering already routes pinned-operand registers
            // into the ConstraintPoint clobber set, so even if
            // pre-coloring is skipped here the operand remains
            // exempt via `involved_pseudos`.
            if !gp_candidates.contains(&pid) {
                continue;
            }
            // Nor into a register no pseudo may have: a register variable
            // can pin its operand to the codegen scratch %r10/%r11, to %rsp
            // or to a global register. The asm codegen moves the operand
            // there itself.
            if !allocatable.contains(&reg) {
                continue;
            }
            // Nor if something else writes that register while the pseudo
            // is live -- the next `mulq` of three in a row, whose `"=a"` and
            // `"=d"` outputs are pinned to the same two registers. Precolored,
            // every product sat in %rax/%rdx and each asm overwrote the last
            // one's; uncolored, the pseudo is placed like any other value,
            // kept out of the register by that constraint point, and the asm
            // codegen moves it in and out.
            let overwritten = pins_clobbered
                .get(&pid)
                .is_some_and(|regs| regs.contains(&reg));
            // Nor if another value already colored that register is live
            // with it: an argument still in %rsi when the operand pinned to
            // %rsi is copied in. `swap(x, y)` into `"S"(x), "D"(y)` wrote x
            // over y before y was read.
            let shares_register = by_pseudo.get(&pid).is_some_and(|interval| {
                pre_colored.iter().any(|(&other, &r)| {
                    r == reg
                        && other != pid
                        && by_pseudo
                            .get(&other)
                            .is_some_and(|o| interval.start < o.end && o.start < interval.end)
                })
            });
            if (overwritten || shares_register)
                && !pre_colored.contains_key(&pid)
                && !goto_outputs.contains(&pid)
            {
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

        // Forbidden colors (`gp_forbidden`): constraint clobbers (idiv
        // RAX/RDX, etc.) for the pseudos a constraint touches that are not
        // its operands, and every caller-saved register for one that crosses
        // a call -- a HARD constraint, NOT a preference: the call clobbers
        // them and the value would be lost. In-loop is NOT a forbidden
        // constraint, only a preference (soft) -- see preferred_palette
        // below -- for a value no call touches.
        let allocatable = self.allocatable_regs();
        let caller_saved: Vec<Reg> = allocatable
            .iter()
            .copied()
            .filter(|r| !r.is_callee_saved())
            .collect();
        let forbidden = self.gp_forbidden(clobbering, gp_candidates);
        let touched = clobbering.gp_call_touched();
        let in_loop_set: std::collections::BTreeSet<PseudoId> = intervals
            .iter()
            .filter(|i| i.in_loop && gp_candidates.contains(&i.pseudo))
            .filter(|i| !touched.contains(&i.pseudo))
            .map(|i| i.pseudo)
            .collect();

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
        for spilled in final_spilled {
            if !self.is_placed(spilled) {
                self.home_on_stack(spilled, SlotShape::new(8, 8));
            }
        }
    }

    fn color_xmm_bank(
        &mut self,
        func: &Function,
        clobbering: &Clobbering,
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
        let forbidden = clobbering.spare_xmm_forbidden(xmm_candidates);
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
        for spilled in result.spilled {
            if !self.is_placed(spilled) {
                let bytes = self.fp_slot_bytes(spilled);
                self.home_on_stack(spilled, SlotShape::new(bytes, bytes));
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

    /// Check the finished assignment against exact liveness; see
    /// [`crate::arch::regalloc::verify_assignment`]. A debug build only.
    ///
    /// What this backend tells the verifier: where each location is; what
    /// each instruction overwrites -- a call every caller-saved register and
    /// every XMM register, a constraint point its clobbers, an asm the XMM
    /// registers it names, a SIMD sequence [`SPARE_XMM`], the Mach-O TLV
    /// getter every XMM register; which frame slots the frame reserves; and
    /// that a general register or a frame slot may not hold a def and a
    /// source of the same instruction -- the two-address lowerings write the
    /// target before they read the second source -- while an XMM register
    /// may, and an identity `Copy` is no instruction at all.
    fn verify(
        &self,
        func: &Function,
        constraint_points: &[ConstraintPoint<Reg>],
        call_positions: &[usize],
    ) {
        use crate::arch::regalloc::{Assignment, Cell, Clobber, Spared};
        let cells = |p: PseudoId, loc: &Loc| -> Vec<Cell<RegUnit>> {
            match loc {
                Loc::Reg(r) => vec![Cell::Reg(RegUnit::Gp(*r))],
                Loc::Xmm(x) => {
                    let span = self.arg_span.get(&p).copied().unwrap_or(1);
                    let regs = XmmReg::arg_regs();
                    match regs.iter().position(|r| r == x) {
                        Some(first) if span > 1 => regs[first..first + span]
                            .iter()
                            .map(|&r| Cell::Reg(RegUnit::Xmm(r)))
                            .collect(),
                        _ => vec![Cell::Reg(RegUnit::Xmm(*x))],
                    }
                }
                Loc::Stack(n) => {
                    let bytes = self.slot_bytes.get(&p).copied().unwrap_or_else(|| {
                        panic!(
                            "internal compiler error: {p} of `{}` has frame slot {n} \
                             but no recorded size",
                            func.name
                        )
                    });
                    let lo = -(*n);
                    vec![Cell::Frame { lo, hi: lo + bytes }]
                }
                Loc::IncomingArg(o) => vec![Cell::Incoming(*o)],
                Loc::Imm(_) | Loc::FImm(..) | Loc::Global(_) => Vec::new(),
            }
        };

        let gp = |regs: &[Reg]| regs.iter().map(|&r| RegUnit::Gp(r)).collect::<Vec<_>>();
        let every_xmm = || XmmReg::allocatable().iter().map(|&x| RegUnit::Xmm(x));
        let mut clobbers: Vec<Clobber<RegUnit>> = constraint_points
            .iter()
            .map(|cp| Clobber {
                position: cp.position,
                regs: gp(&cp.clobbers),
                spared: Spared::Ends(cp.involved_pseudos.clone()),
                why: "the instruction",
            })
            .collect();
        let caller_saved: Vec<RegUnit> = Reg::allocatable()
            .iter()
            .filter(|r| !r.is_callee_saved())
            .map(|&r| RegUnit::Gp(r))
            .chain(every_xmm())
            .collect();
        clobbers.extend(call_positions.iter().map(|&position| Clobber {
            position,
            regs: caller_saved.clone(),
            spared: Spared::Operands,
            why: "a call",
        }));
        let tlv = self.tls_access == crate::target::TlsAccess::MachOTlv;
        for (position, insn) in func.blocks.iter().flat_map(|b| &b.insns).enumerate() {
            if tlv && insn.op == Opcode::TlsAddr {
                clobbers.push(Clobber {
                    position,
                    regs: every_xmm().collect(),
                    spared: Spared::Operands,
                    why: "the TLV getter",
                });
            }
            let named: Vec<RegUnit> = asm_clobbered(insn, parse_xmm_clobber_name)
                .into_iter()
                .filter_map(|n| XmmReg::allocatable().get(usize::from(n)))
                .map(|&x| RegUnit::Xmm(x))
                .collect();
            if !named.is_empty() {
                clobbers.push(Clobber {
                    position,
                    regs: named,
                    spared: Spared::Nothing,
                    why: "the asm's clobber list",
                });
            }
            if writes_spare_xmm(insn.op) {
                clobbers.push(Clobber {
                    position,
                    regs: vec![RegUnit::Xmm(SPARE_XMM)],
                    spared: Spared::Nothing,
                    why: "the SIMD sequence",
                });
            }
        }

        let frame = |slot: i64, bytes: i64| {
            let lo = -slot;
            Cell::Frame { lo, hi: lo + bytes }
        };
        let mut reserved = Vec::new();
        if let Some(slot) = self.guard_slot {
            reserved.push(("the canary's slot", frame(slot, 8)));
        }
        if let Some(s) = self.x87_scratch {
            reserved.push(("the x87 scratch slot", frame(s.slot(), X87Scratch::BYTES)));
        }
        if let Some(s) = self.x87_control_words {
            reserved.push((
                "the x87 control words' slot",
                frame(s.slot(), X87ControlWords::BYTES),
            ));
        }
        // The stack and frame pointers, the scratch pair, the frame base
        // and the global register variables: every general register this
        // function may not colour with, and the two scratch XMM registers.
        let allocatable = self.allocatable_regs();
        let held_back = EVERY_GP
            .iter()
            .filter(|r| !allocatable.contains(r))
            .map(|&r| RegUnit::Gp(r))
            .chain([RegUnit::Xmm(XmmReg::Xmm14), RegUnit::Xmm(XmmReg::Xmm15)]);
        reserved.extend(held_back.map(|u| ("a register held back from allocation", Cell::Reg(u))));
        let def_may_share_src = |insn: &Instruction, cell: &Cell<RegUnit>| {
            insn.op == Opcode::Copy || matches!(cell, Cell::Reg(RegUnit::Xmm(_)))
        };
        crate::arch::regalloc::verify_assignment(&Assignment {
            func,
            live_out: &self.live_out,
            locations: &self.locations,
            cells: &cells,
            clobbers,
            reserved,
            def_may_share_src: &def_may_share_src,
        });
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

    pub fn stack_size(&self) -> i64 {
        let align = i64::from(self.max_local_align.max(16));
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
    /// set, less whatever the frame has claimed for its base and the unit's
    /// global registers.
    ///
    /// Every consumer must come through here rather than reading
    /// `Reg::allocatable()` -- the colourer builds its own preference orders,
    /// and withholding the base from `free_regs` alone left it free to hand
    /// out the very register the prologue overwrites.
    fn allocatable_regs(&self) -> Vec<Reg> {
        let claimed = [self.frame_base.reg(), self.far_scratch];
        Reg::allocatable()
            .iter()
            .copied()
            .filter(|r| !claimed.contains(&Some(*r)) && !self.global_registers.contains(r))
            .collect()
    }

    /// The register this function reaches far displacements through, if its
    /// frame may have any; see [`far_scratch_of`].
    pub fn far_scratch(&self) -> Option<Reg> {
        self.far_scratch
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
                IncomingOff::take(&mut next, align, align as i32),
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
                IncomingOff::take(&mut next, align, align as i32),
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
    fn parse_xmm_clobber_name_every_width() {
        for name in ["xmm3", "ymm3", "zmm3", "%xmm3", "XMM3"] {
            assert_eq!(parse_xmm_clobber_name(name), Some(3), "{name}");
        }
        for name in ["xmm32", "rax", "st", "mm0", "memory"] {
            assert_eq!(parse_xmm_clobber_name(name), None, "{name}");
        }
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
            .map(|&(c, pseudo)| {
                AsmConstraint::new(pseudo, c, crate::target::Arch::X86_64, Bits::new(64))
            })
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
        ld.extra_mut().asm_data.as_mut().unwrap().outputs[0].size = Bits::new(128);

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

    /// A register output interferes with the address of a memory output of
    /// the same statement, even when that address dies there: the template
    /// writes through it. It may still share with a plain input that dies.
    #[test]
    fn asm_register_output_interferes_with_a_memory_outputs_address() {
        use crate::arch::regalloc::build_interference_graph;
        use crate::ir::{BasicBlock, BasicBlockId, Function};
        let asm = make_asm_insn(
            &[],
            &[("=r", PseudoId(1)), ("=m", PseudoId(2)), ("r", PseudoId(3))],
        );
        let types = crate::types::TypeTable::new(&crate::target::Target::host());
        let mut func = Function::new("f", types.void_id);
        let mut block = BasicBlock::new(BasicBlockId(0));
        block.insns = vec![asm];
        func.blocks.push(block);
        let candidates = [1, 2, 3].map(PseudoId).into_iter().collect();
        let live_out = vec![HashSet::new()];
        let graph = build_interference_graph(&candidates, &func, &live_out, false);
        let neighbors: Vec<PseudoId> = graph.neighbors(PseudoId(1)).collect();
        assert!(neighbors.contains(&PseudoId(2)), "{neighbors:?}");
        assert!(!neighbors.contains(&PseudoId(3)), "{neighbors:?}");
    }

    /// Only a frame that may reach past `i32` holds back a far scratch: the
    /// last callee-saved register the frame base, a global register variable
    /// and inline asm leave it. Every other frame gives nothing up.
    #[test]
    fn far_scratch_only_for_a_frame_past_two_gigabytes() {
        use crate::ir::{BasicBlock, BasicBlockId, Function};
        use crate::types::Type;
        let mut types = crate::types::TypeTable::new(&crate::target::Target::host());
        let big = types.intern(Type::array(types.char_id, 3_000_000_000));
        let small = types.intern(Type::array(types.char_id, 4096));
        let func = |typ, clobbers: &[&str]| {
            let mut func = Function::new("f", types.void_id);
            func.add_local("a", PseudoId(1), typ, None, None);
            let mut block = BasicBlock::new(BasicBlockId(0));
            if !clobbers.is_empty() {
                block.insns.push(make_asm_insn(clobbers, &[]));
            }
            func.blocks.push(block);
            func
        };
        let near = func(small, &[]);
        assert_eq!(far_scratch_of(&near, &types, &[], FrameBase::Rbp), None);
        let far = func(big, &[]);
        assert_eq!(
            far_scratch_of(&far, &types, &[], FrameBase::Rbp),
            Some(Reg::R15)
        );
        let base = FrameBase::Aligned {
            reg: Reg::R13,
            align: 32,
        };
        let claimed = func(big, &["r15"]);
        assert_eq!(
            far_scratch_of(&claimed, &types, &[Reg::R14], base),
            Some(Reg::R12)
        );
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
        for (i, want) in [(6u32, 16i64), (7, 24), (8, 32)] {
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
