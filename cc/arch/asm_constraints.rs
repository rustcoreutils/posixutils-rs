//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GCC-style inline-asm constraint vocabulary and parser.
//
// This module sits alongside `cc/arch/regalloc.rs::ConstraintPoint`.
// The existing ConstraintPoint mechanism (a per-instruction set of
// hard-clobbered physical registers plus an "involved pseudos"
// exemption set) is enough to express opcode-level hardware clobbers
// like x86_64 idiv → {RAX, RDX} or shifts → {RCX}, plus the
// inline-asm clobber list and the caller-saved set across libc-call-
// emitting opcodes (sourced in commit `0f58a71c`).
//
// It is *not* enough to express the *per-operand* shape of GCC's
// inline-asm constraint strings:
//
//   * `"a"(x)`  — operand x must be in RAX (Fixed)
//   * `"=r"(y)` — operand y is a write-only output (Def)
//   * `"+r"(z)` — operand z is read+modified (UseDef)
//   * `"0"(w)`  — operand w must share a slot with output #0 (Match)
//   * `"&=r"(t)` — operand t is an early-clobber output (allocator
//                  must keep it disjoint from every input)
//   * `"m"(p)`  — operand p must be a memory operand
//
// `InstrConstraints<R>` collects per-operand `OperandConstraint`s,
// the hard clobber set, and a `memory_barrier` flag. C2 introduces
// the vocabulary and parser; C3 wires the allocator to consume
// `OperandConstraint::{Fixed, Match, Mem, EarlyClobber}` and the
// memory barrier. Until then, `InstrConstraints` lowers to the
// existing `ConstraintPoint` so the allocator's behaviour is
// unchanged.

use crate::ir::PseudoId;

/// What the allocator should do with this operand.
#[derive(Debug, Clone)]
pub enum OperandConstraint<R> {
    /// `"r"` (GP) / `"w"` (V/XMM) — allocator chooses any register
    /// in the appropriate bank.
    Any,
    /// `"a"` / `"b"` / `"c"` / `"d"` / `"S"` / `"D"` on x86_64 —
    /// must be this exact physical register. Per-arch parsers
    /// supply the letter→register mapping.
    Fixed(R),
    /// `"0"` / `"1"` / ... — the operand at this index in the
    /// inline-asm operand list must share its physical location
    /// with this operand.
    Match(usize),
    /// `"m"` / `"Q"` — must be a memory operand. The allocator
    /// places the value in a stack slot (or any base-reg+offset
    /// addressing mode) and substitutes the memory operand into
    /// the asm template.
    Mem,
    /// `"i"` / `"n"` — must be an immediate. No register is
    /// allocated; the value is substituted as a literal into the
    /// asm template.
    Imm,
}

/// Per-operand semantics — whether the operand is read, written, or
/// both, and the GCC early-clobber distinction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandKind {
    /// Read-only input (`"r"`, `"m"`, `"i"`).
    Use,
    /// Write-only output (`"=r"`). The new value isn't visible
    /// until after the asm completes; the allocator may share the
    /// physical location with any input that's already been read.
    Def,
    /// Read+write (`"+r"`). The same physical location is read,
    /// modified by the asm, and written back.
    UseDef,
    /// Like `Def`, but the asm may write the output before all
    /// inputs are read (`"&=r"`). The allocator must keep the
    /// output's physical location disjoint from every input.
    EarlyClobber,
}

/// Per-instruction operand+clobber constraints. Lowers to
/// `ConstraintPoint` for the C2 commit; C3 consumes it directly.
#[derive(Debug, Clone)]
pub struct InstrConstraints<R> {
    /// Per-operand constraint, in the order the operands appear
    /// in the inline-asm operand list (outputs first, then inputs).
    pub operands: Vec<OperandSpec<R>>,
    /// Hard clobbers in addition to whatever the operands imply.
    pub clobbers: Vec<R>,
    /// True iff the inline asm declared a `"memory"` clobber.
    /// C3 enforces the strict semantics: every memory-promoted
    /// value's live range is treated as crossing the barrier, so
    /// stores are flushed before the asm and reloads issued after.
    pub memory_barrier: bool,
}

/// One operand's constraint description.
#[derive(Debug, Clone)]
pub struct OperandSpec<R> {
    pub pseudo: PseudoId,
    pub kind: OperandKind,
    pub constraint: OperandConstraint<R>,
}

/// Parse a GCC constraint string into a `(kind, constraint)` pair.
///
/// Supported syntax (C2 scope — see plan):
///
/// ```text
///   modifiers ::= '&'? ('=' | '+')?
///   constraint ::= modifiers letter
///   letter ::= 'r' | 'w' | 'm' | 'Q' | 'i' | 'n'
///            | '0'..'9'
///            | <any letter that `letter_map` accepts as Fixed(R)>
/// ```
///
/// Multi-alternative constraints (`"rm"`, `"ri"`, `"g"`) and rare
/// per-arch letters (`q`, `l`, `R`, `t`, `I`, `J`, ...) are out of
/// scope for C2 and produce an error. They become a separate
/// follow-up after C5 lands.
///
/// `letter_map` is the per-arch resolver for Fixed-register letters
/// (e.g., x86_64 maps `'a' → Rax`, `'D' → Rdi`; aarch64 currently
/// has no Fixed letters in scope and supplies `|_| None`).
pub fn parse_constraint<R: Copy>(
    s: &str,
    letter_map: impl Fn(char) -> Option<R>,
) -> Result<(OperandKind, OperandConstraint<R>), ConstraintParseError> {
    // Strip leading `&` (early-clobber modifier).
    let (early, after_amp) = match s.strip_prefix('&') {
        Some(rest) => (true, rest),
        None => (false, s),
    };
    // Strip `=` (Def) or `+` (UseDef). No modifier → Use.
    let (kind, body) = if let Some(rest) = after_amp.strip_prefix('=') {
        (
            if early {
                OperandKind::EarlyClobber
            } else {
                OperandKind::Def
            },
            rest,
        )
    } else if let Some(rest) = after_amp.strip_prefix('+') {
        // `&+r` would mean early-clobber UseDef which is rare and
        // semantically equivalent to EarlyClobber for our purposes.
        if early {
            (OperandKind::EarlyClobber, rest)
        } else {
            (OperandKind::UseDef, rest)
        }
    } else {
        if early {
            return Err(ConstraintParseError::EarlyClobberOnInput(s.to_string()));
        }
        (OperandKind::Use, after_amp)
    };

    let mut chars = body.chars();
    let Some(letter) = chars.next() else {
        return Err(ConstraintParseError::Empty(s.to_string()));
    };
    if chars.next().is_some() {
        return Err(ConstraintParseError::MultiAlternative(s.to_string()));
    }

    let con = match letter {
        'r' | 'w' => OperandConstraint::Any,
        'm' | 'Q' => OperandConstraint::Mem,
        'i' | 'n' => OperandConstraint::Imm,
        '0'..='9' => OperandConstraint::Match((letter as u8 - b'0') as usize),
        _ => match letter_map(letter) {
            Some(r) => OperandConstraint::Fixed(r),
            None => return Err(ConstraintParseError::UnknownLetter(letter, s.to_string())),
        },
    };
    Ok((kind, con))
}

/// Errors from `parse_constraint`. Carried as a diagnostic into the
/// front end's existing inline-asm error path in C3+.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintParseError {
    /// `""` — no letter after the modifiers.
    Empty(String),
    /// `"&"` followed by no `=` or `+` — early-clobber on a non-
    /// output makes no sense.
    EarlyClobberOnInput(String),
    /// `"rm"`, `"ri"`, `"g"` — out of scope for C2.
    MultiAlternative(String),
    /// Letter not recognised by this arch.
    UnknownLetter(char, String),
}

impl std::fmt::Display for ConstraintParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstraintParseError::Empty(s) => {
                write!(f, "empty inline-asm constraint string: {s:?}")
            }
            ConstraintParseError::EarlyClobberOnInput(s) => {
                write!(
                    f,
                    "early-clobber `&` modifier requires `=` or `+` output marker: {s:?}"
                )
            }
            ConstraintParseError::MultiAlternative(s) => {
                write!(
                    f,
                    "multi-alternative inline-asm constraint not supported: {s:?}"
                )
            }
            ConstraintParseError::UnknownLetter(c, s) => {
                write!(f, "unknown inline-asm constraint letter {c:?} in {s:?}")
            }
        }
    }
}

impl std::error::Error for ConstraintParseError {}

#[cfg(test)]
mod tests {
    use super::*;

    // A dummy register type for arch-agnostic parser tests.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum Fake {
        A,
        B,
    }

    fn fake_map(c: char) -> Option<Fake> {
        match c {
            'a' => Some(Fake::A),
            'b' => Some(Fake::B),
            _ => None,
        }
    }

    #[test]
    fn parse_use_any() {
        let (k, c) = parse_constraint::<Fake>("r", fake_map).unwrap();
        assert_eq!(k, OperandKind::Use);
        assert!(matches!(c, OperandConstraint::Any));
    }

    #[test]
    fn parse_def_any() {
        let (k, c) = parse_constraint::<Fake>("=r", fake_map).unwrap();
        assert_eq!(k, OperandKind::Def);
        assert!(matches!(c, OperandConstraint::Any));
    }

    #[test]
    fn parse_usedef_any() {
        let (k, c) = parse_constraint::<Fake>("+r", fake_map).unwrap();
        assert_eq!(k, OperandKind::UseDef);
        assert!(matches!(c, OperandConstraint::Any));
    }

    #[test]
    fn parse_early_clobber() {
        let (k, c) = parse_constraint::<Fake>("&=r", fake_map).unwrap();
        assert_eq!(k, OperandKind::EarlyClobber);
        assert!(matches!(c, OperandConstraint::Any));
        // `&+r` also maps to EarlyClobber (read+write that may
        // overwrite an input before all inputs are read).
        let (k, _) = parse_constraint::<Fake>("&+r", fake_map).unwrap();
        assert_eq!(k, OperandKind::EarlyClobber);
    }

    #[test]
    fn parse_fixed_register() {
        let (k, c) = parse_constraint::<Fake>("=a", fake_map).unwrap();
        assert_eq!(k, OperandKind::Def);
        assert!(matches!(c, OperandConstraint::Fixed(Fake::A)));
        let (k, c) = parse_constraint::<Fake>("b", fake_map).unwrap();
        assert_eq!(k, OperandKind::Use);
        assert!(matches!(c, OperandConstraint::Fixed(Fake::B)));
    }

    #[test]
    fn parse_match_operand() {
        let (k, c) = parse_constraint::<Fake>("0", fake_map).unwrap();
        assert_eq!(k, OperandKind::Use);
        assert!(matches!(c, OperandConstraint::Match(0)));
        let (k, c) = parse_constraint::<Fake>("3", fake_map).unwrap();
        assert_eq!(k, OperandKind::Use);
        assert!(matches!(c, OperandConstraint::Match(3)));
    }

    #[test]
    fn parse_memory_operand() {
        let (k, c) = parse_constraint::<Fake>("m", fake_map).unwrap();
        assert_eq!(k, OperandKind::Use);
        assert!(matches!(c, OperandConstraint::Mem));
        let (k, c) = parse_constraint::<Fake>("=m", fake_map).unwrap();
        assert_eq!(k, OperandKind::Def);
        assert!(matches!(c, OperandConstraint::Mem));
    }

    #[test]
    fn parse_immediate() {
        let (_, c) = parse_constraint::<Fake>("i", fake_map).unwrap();
        assert!(matches!(c, OperandConstraint::Imm));
        let (_, c) = parse_constraint::<Fake>("n", fake_map).unwrap();
        assert!(matches!(c, OperandConstraint::Imm));
    }

    #[test]
    fn reject_empty() {
        assert!(matches!(
            parse_constraint::<Fake>("", fake_map),
            Err(ConstraintParseError::Empty(_))
        ));
        // `&` alone has no `=` or `+`.
        assert!(matches!(
            parse_constraint::<Fake>("&", fake_map),
            Err(ConstraintParseError::EarlyClobberOnInput(_))
        ));
    }

    #[test]
    fn reject_multi_alternative() {
        // GCC's `"rm"` (register OR memory) — out of scope.
        assert!(matches!(
            parse_constraint::<Fake>("rm", fake_map),
            Err(ConstraintParseError::MultiAlternative(_))
        ));
        assert!(matches!(
            parse_constraint::<Fake>("=rm", fake_map),
            Err(ConstraintParseError::MultiAlternative(_))
        ));
    }

    #[test]
    fn reject_unknown_letter() {
        assert!(matches!(
            parse_constraint::<Fake>("z", fake_map),
            Err(ConstraintParseError::UnknownLetter('z', _))
        ));
    }
}
