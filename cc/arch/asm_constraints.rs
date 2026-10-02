//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GCC-style inline-asm constraint letters, classified once per operand.
//
// A constraint string is read in exactly one place, `AsmOperandClass::parse`,
// for the target being compiled, and the result is stored on the operand
// (`AsmConstraint::class`). The linearizer, liveness, the register allocator
// and both backends read that classification; none of them reads the letters.
// Each used to keep its own letter set, and they disagreed: aarch64 `w` was a
// register class to the backend and an unknown letter to liveness, so `"+wm"`
// was a register to one and memory to the other; x86 `Q` is a register class
// and was read as memory; the allocator's parser rejected `"=&d"`.
//
// The letters mean different things on different targets -- `Q` is a
// register class on x86-64 and base-register memory on aarch64, `S` a
// register on one and a symbolic constant on the other -- which is why the
// classifier takes the target.

use crate::ir::{AsmData, PseudoId};
use crate::target::Arch;

/// How the template accesses an operand: `=` writes it, `+` reads and
/// writes it, and an operand with neither is only read.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsmAccess {
    Read,
    Write,
    ReadWrite,
}

/// The x87 stack position an operand takes: `f` any, `t` the top, `u` the
/// register below it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum X87Slot {
    Any,
    Top,
    Second,
}

/// A general register one x86-64 letter names: `a`, `b`, `c`, `d`, `S`, `D`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PinnedGp {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
}

/// The register class an operand may be given.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsmRegClass {
    /// Any general register: `r`, and x86-64 `q`, `R`, `l`.
    General,
    /// The one general register an x86-64 letter names.
    Pinned(PinnedGp),
    /// x86-64 `Q`: a general register with an addressable high byte -- one
    /// of `a`, `b`, `c`, `d`, which the backend chooses per statement.
    HighByte,
    /// A floating-point/SIMD register: x86-64 `x`, `v`, `Y`; aarch64 `w`,
    /// `x`, `y`.
    Vector,
    /// The x86-64 x87 register stack.
    X87(X87Slot),
}

impl AsmRegClass {
    /// Which class wins when a constraint lists several: a register the
    /// letter names outright, then the narrower classes, then any general
    /// register.
    fn rank(self) -> u8 {
        match self {
            AsmRegClass::Pinned(_) => 6,
            AsmRegClass::HighByte => 5,
            AsmRegClass::X87(X87Slot::Top) => 4,
            AsmRegClass::X87(X87Slot::Second) => 3,
            AsmRegClass::X87(X87Slot::Any) => 2,
            AsmRegClass::Vector => 1,
            AsmRegClass::General => 0,
        }
    }
}

/// The memory an operand may be.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsmMemClass {
    /// Any addressing mode: `m`, `o`, `V`.
    Any,
    /// aarch64 `Q`: a base register alone, with no offset -- what the
    /// exclusive loads and stores encode.
    BaseOnly,
}

/// What one operand's constraint string allows, read once for the target.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AsmOperandClass {
    pub access: AsmAccess,
    /// `&`: an output written before the template has read every input, so
    /// it may share a register with none of them.
    pub early_clobber: bool,
    /// The register class the operand may take, if any.
    pub reg: Option<AsmRegClass>,
    /// The memory it may be, if any.
    pub mem: Option<AsmMemClass>,
    /// Whether a constant may be substituted as an immediate.
    pub imm: bool,
    /// A matching constraint: the output operand number it names, which may
    /// have more than one digit.
    pub tied: Option<usize>,
}

/// What one letter contributes to a class.
enum Letter {
    Reg(AsmRegClass),
    Mem(AsmMemClass),
    Imm,
    /// `g` and `X`: a general register, memory or an immediate.
    Any,
}

impl AsmOperandClass {
    /// Classify `constraint` for `arch`.
    ///
    /// Alternatives listed in one string (`"rm"`, `"ri"`, `"g"`) accumulate:
    /// the operand may be any of them. Letters this compiler does not model
    /// (and modifiers such as `%`, `,`, `*`, `?`, `!`) contribute nothing.
    pub fn parse(constraint: &str, arch: Arch) -> Self {
        let mut class = AsmOperandClass {
            access: AsmAccess::Read,
            early_clobber: false,
            reg: None,
            mem: None,
            imm: false,
            tied: None,
        };
        let mut chars = constraint.chars().peekable();
        while let Some(c) = chars.next() {
            match c {
                '=' if class.access == AsmAccess::Read => class.access = AsmAccess::Write,
                '+' => class.access = AsmAccess::ReadWrite,
                '&' => class.early_clobber = true,
                '0'..='9' => {
                    let mut n = c.to_digit(10).unwrap() as usize;
                    while let Some(d) = chars.peek().and_then(|d| d.to_digit(10)) {
                        n = n * 10 + d as usize;
                        chars.next();
                    }
                    class.tied = Some(n);
                }
                _ => match letter(c, arch) {
                    Some(Letter::Reg(r)) => class.add_reg(r),
                    Some(Letter::Mem(m)) => class.add_mem(m),
                    Some(Letter::Imm) => class.imm = true,
                    Some(Letter::Any) => {
                        class.add_reg(AsmRegClass::General);
                        class.add_mem(AsmMemClass::Any);
                        class.imm = true;
                    }
                    None => {}
                },
            }
        }
        class
    }

    fn add_reg(&mut self, r: AsmRegClass) {
        if self.reg.is_none_or(|have| r.rank() > have.rank()) {
            self.reg = Some(r);
        }
    }

    /// Any addressing mode is a wider alternative than base-register only.
    fn add_mem(&mut self, m: AsmMemClass) {
        if self.mem != Some(AsmMemClass::Any) {
            self.mem = Some(m);
        }
    }

    /// True when the operand can only be memory: no register and no
    /// immediate alternative is offered.
    pub fn is_memory_only(&self) -> bool {
        self.mem.is_some() && self.reg.is_none() && !self.imm
    }
}

/// What `c` means on `arch`, or `None` for a letter c17 does not model.
fn letter(c: char, arch: Arch) -> Option<Letter> {
    use AsmRegClass::*;
    Some(match (arch, c) {
        (_, 'r') => Letter::Reg(General),
        (_, 'm' | 'o' | 'V') => Letter::Mem(AsmMemClass::Any),
        (_, 'i' | 'n' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'Z') => Letter::Imm,
        (_, 'g' | 'X') => Letter::Any,

        (Arch::X86_64, 'q' | 'R' | 'l') => Letter::Reg(General),
        (Arch::X86_64, 'a') => Letter::Reg(Pinned(PinnedGp::Rax)),
        (Arch::X86_64, 'b') => Letter::Reg(Pinned(PinnedGp::Rbx)),
        (Arch::X86_64, 'c') => Letter::Reg(Pinned(PinnedGp::Rcx)),
        (Arch::X86_64, 'd') => Letter::Reg(Pinned(PinnedGp::Rdx)),
        (Arch::X86_64, 'S') => Letter::Reg(Pinned(PinnedGp::Rsi)),
        (Arch::X86_64, 'D') => Letter::Reg(Pinned(PinnedGp::Rdi)),
        (Arch::X86_64, 'Q') => Letter::Reg(HighByte),
        (Arch::X86_64, 'x' | 'v' | 'Y') => Letter::Reg(Vector),
        (Arch::X86_64, 'f') => Letter::Reg(X87(X87Slot::Any)),
        (Arch::X86_64, 't') => Letter::Reg(X87(X87Slot::Top)),
        (Arch::X86_64, 'u') => Letter::Reg(X87(X87Slot::Second)),
        (Arch::X86_64, 'e' | 's') => Letter::Imm,

        (Arch::Aarch64, 'w' | 'x' | 'y') => Letter::Reg(Vector),
        (Arch::Aarch64, 'Q') => Letter::Mem(AsmMemClass::BaseOnly),
        (Arch::Aarch64, 'S' | 'Y') => Letter::Imm,

        _ => return None,
    })
}

/// The register-allocator view of one inline-asm statement: the operands
/// pinned to one register, and the registers the statement clobbers.
#[derive(Debug, Clone)]
pub struct InstrConstraints<R> {
    /// Each operand the statement pins, with its register.
    pub pinned: Vec<(PseudoId, R)>,
    /// Hard clobbers in addition to the pinned registers. A `"memory"`
    /// clobber is not one of these: it orders memory rather than claiming a
    /// register, and is answered by `Instruction::is_memory_barrier`.
    pub clobbers: Vec<R>,
}

impl<R: Copy + Ord> InstrConstraints<R> {
    /// The constraints of one inline-asm statement, given the register each
    /// operand is pinned to (outputs, then inputs, in order) and the target's
    /// clobber names. One implementation for both backends.
    pub fn of_asm(
        asm_data: &AsmData,
        pins: &[Option<R>],
        clobber_name: impl Fn(&str) -> Option<R>,
    ) -> Self {
        let pinned = asm_data
            .outputs
            .iter()
            .chain(asm_data.inputs.iter())
            .zip(pins)
            .filter_map(|(ac, pin)| pin.map(|r| (ac.pseudo, r)))
            .collect();
        let mut clobbers: Vec<R> = asm_data
            .clobbers
            .iter()
            .filter_map(|name| clobber_name(name))
            .collect();
        clobbers.sort();
        clobbers.dedup();
        Self { pinned, clobbers }
    }

    /// The statement as the allocator's `ConstraintPoint`: the registers it
    /// claims, and the operands exempt from that claim.
    ///
    /// It claims its declared clobbers and every register an operand is
    /// pinned to. gcc's rule is that no operand may live in a clobbered
    /// register -- the template may write it before reading them -- nor in a
    /// register another operand is pinned to. So only the pinned operands
    /// themselves are exempt: each is precolored to its own register and must
    /// not be forbidden it. Exempting every operand, as both backends did,
    /// let an input or a memory operand's address land in a declared
    /// clobber, and a template that wrote it first destroyed the operand.
    pub fn to_constraint_point(&self) -> (Vec<R>, Vec<PseudoId>) {
        let mut clobbers = self.clobbers.clone();
        let mut pinned = Vec::new();
        for &(pseudo, r) in &self.pinned {
            clobbers.push(r);
            if !pinned.contains(&pseudo) {
                pinned.push(pseudo);
            }
        }
        clobbers.sort();
        clobbers.dedup();
        (clobbers, pinned)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use AsmRegClass::*;

    fn x86(s: &str) -> AsmOperandClass {
        AsmOperandClass::parse(s, Arch::X86_64)
    }

    fn a64(s: &str) -> AsmOperandClass {
        AsmOperandClass::parse(s, Arch::Aarch64)
    }

    /// `(reg, mem, imm)` for a constraint body.
    type Allows = (Option<AsmRegClass>, Option<AsmMemClass>, bool);

    const MEM: Option<AsmMemClass> = Some(AsmMemClass::Any);

    fn allows(c: AsmOperandClass) -> Allows {
        (c.reg, c.mem, c.imm)
    }

    /// The letters both targets share mean the same on each.
    #[test]
    fn common_letters() {
        let table: &[(&str, Allows)] = &[
            ("r", (Some(General), None, false)),
            ("m", (None, MEM, false)),
            ("o", (None, MEM, false)),
            ("V", (None, MEM, false)),
            ("i", (None, None, true)),
            ("n", (None, None, true)),
            ("I", (None, None, true)),
            ("rm", (Some(General), MEM, false)),
            ("ri", (Some(General), None, true)),
            ("rmi", (Some(General), MEM, true)),
            ("g", (Some(General), MEM, true)),
            ("X", (Some(General), MEM, true)),
        ];
        for &(s, want) in table {
            assert_eq!(allows(x86(s)), want, "x86-64 {s:?}");
            assert_eq!(allows(a64(s)), want, "aarch64 {s:?}");
        }
    }

    #[test]
    fn x86_64_letters() {
        let table: &[(&str, Allows)] = &[
            ("a", (Some(Pinned(PinnedGp::Rax)), None, false)),
            ("b", (Some(Pinned(PinnedGp::Rbx)), None, false)),
            ("c", (Some(Pinned(PinnedGp::Rcx)), None, false)),
            ("d", (Some(Pinned(PinnedGp::Rdx)), None, false)),
            ("S", (Some(Pinned(PinnedGp::Rsi)), None, false)),
            ("D", (Some(Pinned(PinnedGp::Rdi)), None, false)),
            // A register class, not memory as on aarch64.
            ("Q", (Some(HighByte), None, false)),
            ("q", (Some(General), None, false)),
            ("R", (Some(General), None, false)),
            ("l", (Some(General), None, false)),
            ("x", (Some(Vector), None, false)),
            ("v", (Some(Vector), None, false)),
            ("xm", (Some(Vector), MEM, false)),
            ("f", (Some(X87(X87Slot::Any)), None, false)),
            ("t", (Some(X87(X87Slot::Top)), None, false)),
            ("u", (Some(X87(X87Slot::Second)), None, false)),
            ("e", (None, None, true)),
            ("Z", (None, None, true)),
            ("s", (None, None, true)),
            // A named register outranks a class listed beside it.
            ("ra", (Some(Pinned(PinnedGp::Rax)), None, false)),
            // aarch64's register letter is no x86-64 class.
            ("w", (None, None, false)),
        ];
        for &(s, want) in table {
            assert_eq!(allows(x86(s)), want, "{s:?}");
        }
    }

    #[test]
    fn aarch64_letters() {
        let table: &[(&str, Allows)] = &[
            ("w", (Some(Vector), None, false)),
            ("x", (Some(Vector), None, false)),
            ("y", (Some(Vector), None, false)),
            // A register class with a memory alternative: not memory-only.
            ("wm", (Some(Vector), MEM, false)),
            // Memory, by a base register alone.
            ("Q", (None, Some(AsmMemClass::BaseOnly), false)),
            ("Qm", (None, MEM, false)),
            ("S", (None, None, true)),
            ("Y", (None, None, true)),
            ("Z", (None, None, true)),
            ("K", (None, None, true)),
            // x86-64's pinned letters name nothing here.
            ("a", (None, None, false)),
            ("D", (None, None, false)),
        ];
        for &(s, want) in table {
            assert_eq!(allows(a64(s)), want, "{s:?}");
        }
    }

    #[test]
    fn modifiers() {
        let table: &[(&str, AsmAccess, bool)] = &[
            ("r", AsmAccess::Read, false),
            ("=r", AsmAccess::Write, false),
            ("+r", AsmAccess::ReadWrite, false),
            ("=&r", AsmAccess::Write, true),
            ("&=r", AsmAccess::Write, true),
            ("+&r", AsmAccess::ReadWrite, true),
            ("=&d", AsmAccess::Write, true),
            ("=m", AsmAccess::Write, false),
            ("0", AsmAccess::Read, false),
        ];
        for &(s, access, early) in table {
            let c = x86(s);
            assert_eq!((c.access, c.early_clobber), (access, early), "{s:?}");
        }
        // The modifiers do not disturb the class.
        assert_eq!(x86("=&d").reg, Some(Pinned(PinnedGp::Rdx)));
        assert_eq!(a64("+wm").reg, Some(Vector));
    }

    /// A matching constraint is an operand number, and may have two digits.
    #[test]
    fn matching_digits() {
        assert_eq!(x86("0").tied, Some(0));
        assert_eq!(x86("9").tied, Some(9));
        assert_eq!(x86("10").tied, Some(10));
        assert_eq!(a64("12").tied, Some(12));
        assert_eq!(x86("r").tied, None);
        let tied = x86("1");
        assert_eq!((tied.reg, tied.mem, tied.imm), (None, None, false));
    }

    #[test]
    fn memory_only() {
        for s in ["m", "=m", "+m", "o", "V", "mo"] {
            assert!(x86(s).is_memory_only(), "x86-64 {s:?}");
            assert!(a64(s).is_memory_only(), "aarch64 {s:?}");
        }
        assert!(a64("Q").is_memory_only());
        for s in ["rm", "g", "X", "mi", "r", "i", "0"] {
            assert!(!x86(s).is_memory_only(), "x86-64 {s:?}");
            assert!(!a64(s).is_memory_only(), "aarch64 {s:?}");
        }
        for s in ["Q", "xm", "tm"] {
            assert!(!x86(s).is_memory_only(), "x86-64 {s:?}");
        }
        assert!(!a64("wm").is_memory_only());
    }

    fn operand(pseudo: u32, constraint: &str) -> crate::ir::AsmConstraint {
        crate::ir::AsmConstraint::new(PseudoId(pseudo), constraint, Arch::X86_64, 64)
    }

    #[test]
    fn constraint_point_claims_pins_and_clobbers() {
        let asm = AsmData {
            template: String::new(),
            outputs: vec![operand(1, "=a")],
            inputs: vec![operand(2, "r"), operand(3, "D")],
            clobbers: vec!["b".into(), "z".into()],
            goto_labels: Vec::new(),
        };
        let ic = InstrConstraints::of_asm(&asm, &[Some(10u8), None, Some(5u8)], |n| {
            (n == "b").then_some(7u8)
        });
        assert_eq!(ic.pinned, vec![(PseudoId(1), 10), (PseudoId(3), 5)]);
        let (claimed, exempt) = ic.to_constraint_point();
        assert_eq!(claimed, vec![5, 7, 10]);
        assert_eq!(exempt, vec![PseudoId(1), PseudoId(3)]);
    }
}
