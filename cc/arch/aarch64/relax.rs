//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 branch relaxation: a conditional branch whose target is out of its
// encodable range becomes the inverse condition over an unconditional `b`.
//

use super::lir::Aarch64Inst;
use crate::arch::lir::{Directive, EmitAsm};
use crate::target::Target;
use std::collections::HashMap;

/// `b.cond` and `cbnz` encode a signed 19-bit word offset: +-1 MiB.
const COND_BRANCH_MIN: i64 = -(1 << 20);
const COND_BRANCH_MAX: i64 = (1 << 20) - 4;

/// Rewrite, in place, every `BCond`/`Cbnz` in one function's LIR whose target
/// label is out of range into its `BCondFar`/`CbnzFar` form.
///
/// Offsets come from printing each instruction and measuring the text
/// ([`CodeSize`]), so they cannot drift from what the printer emits. Relaxing
/// only lengthens code, so the pass repeats until nothing more moves out of
/// range. Rewriting in place, never inserting, keeps every index into the
/// LIR buffer valid -- `-fverbose-asm` comments are keyed by index.
///
/// A function whose text cannot be sized -- inline assembly that switches
/// section or defines a macro -- has every conditional branch relaxed: an
/// unknown distance has to be treated as out of range. An unconditional `b`
/// reaches +-128 MiB and is not relaxed.
pub(super) fn relax_branches(insts: &mut [Aarch64Inst], target: &Target) {
    let Some(mut sizes) = measure(insts, target) else {
        for inst in insts.iter_mut() {
            relax_one(inst);
        }
        return;
    };
    loop {
        let mut offsets = Vec::with_capacity(insts.len());
        let mut labels: HashMap<String, i64> = HashMap::new();
        let mut at = 0i64;
        for (inst, size) in insts.iter().zip(&sizes) {
            offsets.push(at);
            if let Aarch64Inst::Directive(Directive::BlockLabel(label)) = inst {
                labels.insert(label.name(), at);
            }
            at += i64::from(*size);
        }
        let mut changed = false;
        for (i, inst) in insts.iter_mut().enumerate() {
            let label = match inst {
                Aarch64Inst::BCond { target, .. } | Aarch64Inst::Cbnz { target, .. } => target,
                _ => continue,
            };
            // A label this function does not define is out of reach of any
            // measurement, so it is out of range.
            let in_range = labels.get(&label.name()).is_some_and(|&to| {
                (COND_BRANCH_MIN..=COND_BRANCH_MAX).contains(&(to - offsets[i]))
            });
            if !in_range {
                relax_one(inst);
                sizes[i] += 4;
                changed = true;
            }
        }
        if !changed {
            return;
        }
    }
}

/// The far form of a conditional branch; anything else is left alone.
fn relax_one(inst: &mut Aarch64Inst) {
    let far = match inst {
        Aarch64Inst::BCond { cond, target } => Aarch64Inst::BCondFar {
            cond: *cond,
            target: target.clone(),
        },
        Aarch64Inst::Cbnz { size, src, target } => Aarch64Inst::CbnzFar {
            size: *size,
            src: *src,
            target: target.clone(),
        },
        _ => return,
    };
    *inst = far;
}

/// Each instruction's size in bytes, or `None` if some text cannot be sized.
fn measure(insts: &[Aarch64Inst], target: &Target) -> Option<Vec<u32>> {
    let mut size = CodeSize::default();
    let mut text = String::new();
    let mut sizes = Vec::with_capacity(insts.len());
    for inst in insts {
        text.clear();
        inst.emit(target, &mut text);
        sizes.push(size.of(&text)?);
    }
    // An unterminated `.rept` repeats code nothing here has counted.
    size.rept.is_empty().then_some(sizes)
}

/// Bytes of machine code in assembler text, counted the way the assembler
/// lays it out: four per instruction, a data directive's own size, the
/// worst-case padding of an alignment. An overestimate is safe -- it can only
/// relax a branch that did not need it -- so padding is counted at its
/// maximum and a `;`-separated line counts every statement.
///
/// State carries across calls because inline assembly arrives one line per
/// LIR entry, so a `.rept` and its `.endr` are different instructions.
#[derive(Default)]
struct CodeSize {
    /// Enclosing `.rept` counts, innermost last, with the bytes counted inside.
    rept: Vec<(u64, u64)>,
}

impl CodeSize {
    fn of(&mut self, text: &str) -> Option<u32> {
        let mut outer = 0u64;
        for line in text.lines() {
            let line = line.split("//").next().unwrap_or("");
            for stmt in line.split(';') {
                let bytes = match self.statement(stmt)? {
                    Stmt::Bytes(n) => n,
                    Stmt::ReptStart(n) => {
                        self.rept.push((n, 0));
                        continue;
                    }
                    Stmt::ReptEnd => {
                        let (count, inner) = self.rept.pop()?;
                        count.checked_mul(inner)?
                    }
                };
                match self.rept.last_mut() {
                    Some((_, inner)) => *inner = inner.checked_add(bytes)?,
                    None => outer = outer.checked_add(bytes)?,
                }
            }
        }
        u32::try_from(outer).ok()
    }

    fn statement(&self, stmt: &str) -> Option<Stmt> {
        let stmt = strip_labels(stmt.trim());
        if stmt.is_empty() {
            return Some(Stmt::Bytes(0));
        }
        if !stmt.starts_with('.') {
            return Some(Stmt::Bytes(4));
        }
        let (name, args) = match stmt.find(char::is_whitespace) {
            Some(i) => (&stmt[..i], stmt[i..].trim()),
            None => (stmt, ""),
        };
        let items = || args.split(',').filter(|a| !a.trim().is_empty()).count() as u64;
        let bytes = match name {
            ".byte" => items(),
            ".hword" | ".short" | ".2byte" | ".value" => 2 * items(),
            ".word" | ".long" | ".int" | ".4byte" | ".inst" | ".float" | ".single" => 4 * items(),
            ".quad" | ".xword" | ".dword" | ".8byte" | ".double" => 8 * items(),
            ".space" | ".skip" | ".zero" => number(args.split(',').next()?)?,
            ".fill" => {
                let mut a = args.split(',');
                let repeat = number(a.next()?)?;
                let size = a.next().map_or(Some(1), number)?;
                repeat.checked_mul(size)?
            }
            ".ascii" | ".asciz" | ".string" => args.len() as u64 + 1,
            // `.align` is a power of two on aarch64, as `.p2align` is.
            ".p2align" | ".align" => (1u64 << number(args.split(',').next()?)?.min(30)) - 1,
            ".balign" => number(args.split(',').next()?)?,
            ".rept" => return Some(Stmt::ReptStart(number(args)?)),
            ".endr" => return Some(Stmt::ReptEnd),
            // Code or data after these lands somewhere else, or is generated
            // by a macro this does not expand: the distance is unknown.
            ".section" | ".pushsection" | ".popsection" | ".previous" | ".subsection" | ".text"
            | ".data" | ".bss" | ".macro" | ".irp" | ".irpc" | ".incbin" | ".org" => return None,
            // Everything else emits nothing into the section: symbol and
            // debug bookkeeping (`.loc`, `.cfi_*`, `.type`, `.size`, `.globl`).
            _ => 0,
        };
        Some(Stmt::Bytes(bytes))
    }
}

enum Stmt {
    Bytes(u64),
    ReptStart(u64),
    ReptEnd,
}

/// `stmt` without any leading `label:` definitions.
fn strip_labels(mut stmt: &str) -> &str {
    loop {
        let Some(colon) = stmt.find(':') else {
            return stmt;
        };
        let head = &stmt[..colon];
        let is_label = !head.is_empty()
            && head
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '.' | '$' | '"'));
        if !is_label {
            return stmt;
        }
        stmt = stmt[colon + 1..].trim_start();
    }
}

/// A non-negative integer literal: decimal, `0x` hex, or `0b` binary.
fn number(text: &str) -> Option<u64> {
    let text = text.trim();
    if let Some(hex) = text.strip_prefix("0x").or_else(|| text.strip_prefix("0X")) {
        u64::from_str_radix(hex, 16).ok()
    } else if let Some(bin) = text.strip_prefix("0b").or_else(|| text.strip_prefix("0B")) {
        u64::from_str_radix(bin, 2).ok()
    } else {
        text.parse().ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arch::aarch64::regalloc::Reg;
    use crate::arch::lir::{CondCode, Label, OperandSize};
    use crate::target::{Arch, Os};

    fn linux() -> Target {
        Target::new(Arch::Aarch64, Os::Linux)
    }

    fn label(id: u32) -> Label {
        Label::block("f", id)
    }

    fn at(id: u32) -> Aarch64Inst {
        Aarch64Inst::Directive(Directive::BlockLabel(label(id)))
    }

    fn bcond(id: u32) -> Aarch64Inst {
        Aarch64Inst::BCond {
            cond: CondCode::Slt,
            target: label(id),
        }
    }

    /// `n` bytes of straight-line code, as raw text.
    fn pad(n: u64) -> Aarch64Inst {
        Aarch64Inst::Directive(Directive::Raw(format!(".skip {n}")))
    }

    fn sizes(text: &str) -> Option<u32> {
        CodeSize::default().of(text)
    }

    #[test]
    fn code_size_counts_what_the_assembler_lays_out() {
        assert_eq!(sizes("    add x0, x0, #1\n"), Some(4));
        assert_eq!(sizes("    b.ge .+8\n    b .L1\n"), Some(8));
        assert_eq!(sizes(".Lf_3:\n"), Some(0));
        assert_eq!(sizes("1: nop"), Some(4));
        assert_eq!(sizes("    .loc 1 2 3\n    .cfi_def_cfa w29, 16\n"), Some(0));
        assert_eq!(sizes("nop; nop // two"), Some(8));
        assert_eq!(sizes(".skip 0x100"), Some(256));
        assert_eq!(sizes(".word 1, 2, 3"), Some(12));
        assert_eq!(sizes(".fill 3, 8"), Some(24));
        assert_eq!(sizes(".p2align 4"), Some(15));
        assert_eq!(sizes(".section .data"), None);
    }

    #[test]
    fn code_size_multiplies_a_rept_across_lines() {
        let mut c = CodeSize::default();
        assert_eq!(c.of(".rept 10"), Some(0));
        assert_eq!(c.of("nop"), Some(0));
        assert_eq!(c.of("nop"), Some(0));
        assert_eq!(c.of(".endr"), Some(80));
    }

    /// Every instruction's measured size is four bytes per line the printer
    /// writes, for the forms relaxation produces and the ones it reads.
    #[test]
    fn measured_size_matches_the_printed_lines() {
        let target = linux();
        for inst in [
            bcond(1),
            Aarch64Inst::BCondFar {
                cond: CondCode::Eq,
                target: label(1),
            },
            Aarch64Inst::CbnzFar {
                size: OperandSize::B64,
                src: Reg::X0,
                target: label(1),
            },
            at(1),
        ] {
            let mut text = String::new();
            inst.emit(&target, &mut text);
            let lines = text
                .lines()
                .filter(|l| l.starts_with("    ") && !l.trim_start().starts_with('.'))
                .count() as u32;
            assert_eq!(measure(&[inst], &target), Some(vec![4 * lines]));
        }
    }

    #[test]
    fn in_range_branches_are_untouched() {
        let mut insts = vec![bcond(1), pad(1 << 19), at(1), pad(1 << 19), bcond(1)];
        relax_branches(&mut insts, &linux());
        assert!(matches!(insts[0], Aarch64Inst::BCond { .. }));
        assert!(matches!(insts[4], Aarch64Inst::BCond { .. }));
    }

    #[test]
    fn far_branches_both_ways_are_relaxed() {
        // Backwards, -1 MiB itself still encodes; one word further does not.
        let mut insts = vec![bcond(1), pad(1 << 20), at(1), pad((1 << 20) + 4), bcond(1)];
        relax_branches(&mut insts, &linux());
        assert!(matches!(insts[0], Aarch64Inst::BCondFar { .. }));
        assert!(matches!(insts[4], Aarch64Inst::BCondFar { .. }));
        let mut edge = vec![at(1), pad(1 << 20), bcond(1)];
        relax_branches(&mut edge, &linux());
        assert!(matches!(edge[2], Aarch64Inst::BCond { .. }));
    }

    /// The exact boundary: +1 MiB - 4 encodes, +1 MiB does not.
    #[test]
    fn the_range_limit_is_exact() {
        let mut fits = vec![bcond(1), pad((1 << 20) - 8), at(1)];
        relax_branches(&mut fits, &linux());
        assert!(matches!(fits[0], Aarch64Inst::BCond { .. }));
        let mut over = vec![bcond(1), pad((1 << 20) - 4), at(1)];
        relax_branches(&mut over, &linux());
        assert!(matches!(over[0], Aarch64Inst::BCondFar { .. }));
    }

    /// Relaxing one branch lengthens the code another spans: the pass runs to
    /// a fixed point.
    #[test]
    fn relaxation_cascades() {
        // The first branch reaches label 2 exactly; the second is out of
        // range regardless, and relaxing it pushes label 2 one word too far.
        let mut insts = vec![
            bcond(2),
            bcond(3),
            pad((1 << 20) - 12),
            at(2),
            pad(1 << 20),
            at(3),
        ];
        relax_branches(&mut insts, &linux());
        assert!(matches!(insts[1], Aarch64Inst::BCondFar { .. }));
        assert!(matches!(insts[0], Aarch64Inst::BCondFar { .. }));
    }

    #[test]
    fn unsizable_text_relaxes_every_conditional_branch() {
        let mut insts = vec![
            bcond(1),
            Aarch64Inst::Directive(Directive::Raw(".pushsection .data".into())),
            at(1),
        ];
        relax_branches(&mut insts, &linux());
        assert!(matches!(insts[0], Aarch64Inst::BCondFar { .. }));
    }
}
