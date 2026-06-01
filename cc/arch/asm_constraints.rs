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
    /// `"rm"`, `"ri"`, `"rmi"`, `"g"` — the operand satisfies any
    /// one of these alternatives. The list is non-empty,
    /// deduplicated, and flattened (no nested `Alternatives`).
    /// The allocator picks the cheapest fit at lowering time based
    /// on what the operand's actual location can satisfy. C9 scope:
    /// alternatives are restricted to `Any` / `Mem` / `Imm` only —
    /// `Fixed` and `Match` cannot appear inside `Alternatives`
    /// (mixing pin-to-physreg or pin-to-other-operand with "or
    /// memory" makes no sense and GCC rejects it too).
    Alternatives(Vec<OperandConstraint<R>>),
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
/// Supported syntax (C9 scope):
///
/// ```text
///   modifiers ::= '&'? ('=' | '+')?
///   constraint ::= modifiers body
///   body ::= class_letter+ | single_letter
///   class_letter ::= 'r' | 'w' | 'm' | 'Q' | 'i' | 'n' | 'g'
///   single_letter ::= class_letter | '0'..'9' | <Fixed letter>
/// ```
///
/// Multi-character bodies build an `OperandConstraint::Alternatives`
/// (e.g. `"rm"` → register OR memory; `"g"` is sugar for `"rmi"`).
/// `Fixed` register letters and `Match` digits cannot appear inside
/// a multi-character body — GCC's `"ra"` / `"r0"` are nonsensical
/// (you can't be "either register-class or pinned-to-RAX").
///
/// Rare per-arch letters (`q`, `l`, `R`, `t`, `I`, `J`, ...) remain
/// out of scope (C10).
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

    let body_chars: Vec<char> = body.chars().collect();
    if body_chars.is_empty() {
        return Err(ConstraintParseError::Empty(s.to_string()));
    }

    // Single-character body — fast path. Handles Fixed letters and
    // Match digits, which are illegal inside multi-char bodies.
    if body_chars.len() == 1 {
        return Ok((kind, parse_single_letter(body_chars[0], &letter_map, s)?));
    }

    // Multi-character body: every letter must be a class letter
    // (Any / Mem / Imm, or `g` sugar). Build the alternatives in
    // appearance order, dedup, and flatten `g` (which itself is
    // `Alternatives([Any, Mem, Imm])`).
    let mut alts: Vec<OperandConstraint<R>> = Vec::with_capacity(body_chars.len());
    for &c in &body_chars {
        let sub = parse_single_letter(c, &letter_map, s)?;
        match sub {
            OperandConstraint::Any | OperandConstraint::Mem | OperandConstraint::Imm => {
                push_dedup(&mut alts, sub);
            }
            OperandConstraint::Alternatives(inner) => {
                // `g` sugar — flatten in place.
                for a in inner {
                    push_dedup(&mut alts, a);
                }
            }
            OperandConstraint::Fixed(_) | OperandConstraint::Match(_) => {
                return Err(ConstraintParseError::AlternativeWithFixed(c, s.to_string()));
            }
        }
    }

    // Single-alternative collapse — e.g. `"rr"` → `Any`. Spec-legal:
    // duplicates have no semantic meaning.
    let con = if alts.len() == 1 {
        alts.into_iter().next().unwrap()
    } else {
        OperandConstraint::Alternatives(alts)
    };
    Ok((kind, con))
}

/// Parse a single constraint letter into its `OperandConstraint`.
/// Shared by the single-char fast path and the multi-char alternatives
/// loop.
fn parse_single_letter<R: Copy>(
    letter: char,
    letter_map: &impl Fn(char) -> Option<R>,
    full: &str,
) -> Result<OperandConstraint<R>, ConstraintParseError> {
    Ok(match letter {
        'r' | 'w' => OperandConstraint::Any,
        'm' | 'Q' => OperandConstraint::Mem,
        'i' | 'n' => OperandConstraint::Imm,
        // GCC `g` is shorthand for "any general-purpose operand":
        // register, memory, or immediate. Same as `"rmi"`.
        'g' => OperandConstraint::Alternatives(vec![
            OperandConstraint::Any,
            OperandConstraint::Mem,
            OperandConstraint::Imm,
        ]),
        '0'..='9' => OperandConstraint::Match((letter as u8 - b'0') as usize),
        _ => match letter_map(letter) {
            Some(r) => OperandConstraint::Fixed(r),
            None => {
                return Err(ConstraintParseError::UnknownLetter(
                    letter,
                    full.to_string(),
                ))
            }
        },
    })
}

/// Insert `con` into `alts` if no kind-equal element is already there.
/// Alternatives within `Alternatives` are restricted to `Any`/`Mem`/
/// `Imm` by parse_constraint's check, so a tag-only comparison is
/// sufficient.
fn push_dedup<R: Copy>(alts: &mut Vec<OperandConstraint<R>>, con: OperandConstraint<R>) {
    let same = |a: &OperandConstraint<R>, b: &OperandConstraint<R>| {
        std::mem::discriminant(a) == std::mem::discriminant(b)
    };
    if !alts.iter().any(|a| same(a, &con)) {
        alts.push(con);
    }
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
    /// A multi-character constraint body mixed a class letter
    /// (`r`/`m`/`i`/`g`) with a Fixed-register letter or a Match
    /// digit (e.g. `"ra"`, `"r0"`). GCC rejects these; we do too.
    /// Carried char is the offending letter inside the body.
    AlternativeWithFixed(char, String),
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
            ConstraintParseError::AlternativeWithFixed(c, s) => {
                write!(
                    f,
                    "inline-asm constraint {s:?} cannot mix class letters (r/m/i/g) \
                     with the Fixed-register / Match-operand letter {c:?}"
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
    fn reject_unknown_letter() {
        assert!(matches!(
            parse_constraint::<Fake>("z", fake_map),
            Err(ConstraintParseError::UnknownLetter('z', _))
        ));
    }

    // ========================================================================
    // C9 — Multi-alternative constraints
    // ========================================================================

    fn alternative_kinds<R: Copy>(c: &OperandConstraint<R>) -> Vec<&'static str> {
        match c {
            OperandConstraint::Alternatives(alts) => alts
                .iter()
                .map(|a| match a {
                    OperandConstraint::Any => "any",
                    OperandConstraint::Mem => "mem",
                    OperandConstraint::Imm => "imm",
                    OperandConstraint::Fixed(_) => "fixed",
                    OperandConstraint::Match(_) => "match",
                    OperandConstraint::Alternatives(_) => "nested",
                })
                .collect(),
            _ => vec![],
        }
    }

    #[test]
    fn parse_multi_alt_rm() {
        // `"rm"` — register or memory. The most common kernel/UAPI
        // multi-alt constraint.
        let (k, c) = parse_constraint::<Fake>("rm", fake_map).unwrap();
        assert_eq!(k, OperandKind::Use);
        assert_eq!(alternative_kinds(&c), vec!["any", "mem"]);

        let (k, c) = parse_constraint::<Fake>("=rm", fake_map).unwrap();
        assert_eq!(k, OperandKind::Def);
        assert_eq!(alternative_kinds(&c), vec!["any", "mem"]);

        let (k, c) = parse_constraint::<Fake>("+rm", fake_map).unwrap();
        assert_eq!(k, OperandKind::UseDef);
        assert_eq!(alternative_kinds(&c), vec!["any", "mem"]);
    }

    #[test]
    fn parse_multi_alt_ri() {
        let (k, c) = parse_constraint::<Fake>("ri", fake_map).unwrap();
        assert_eq!(k, OperandKind::Use);
        assert_eq!(alternative_kinds(&c), vec!["any", "imm"]);
    }

    #[test]
    fn parse_multi_alt_rmi() {
        let (k, c) = parse_constraint::<Fake>("rmi", fake_map).unwrap();
        assert_eq!(k, OperandKind::Use);
        assert_eq!(alternative_kinds(&c), vec!["any", "mem", "imm"]);
    }

    #[test]
    fn parse_g_is_sugar_for_rmi() {
        // GCC `g` ≡ "register, memory, or immediate" — same as `rmi`.
        let (k, c) = parse_constraint::<Fake>("g", fake_map).unwrap();
        assert_eq!(k, OperandKind::Use);
        assert_eq!(alternative_kinds(&c), vec!["any", "mem", "imm"]);
    }

    #[test]
    fn parse_multi_alt_g_flattens() {
        // `g` inside a multi-char body flattens; the resulting
        // Alternatives carries Any/Mem/Imm exactly once each.
        let (_, c) = parse_constraint::<Fake>("rg", fake_map).unwrap();
        assert_eq!(alternative_kinds(&c), vec!["any", "mem", "imm"]);
    }

    #[test]
    fn parse_multi_alt_dedup() {
        // `"rr"` collapses to a single `Any` — the duplicate has no
        // semantic value, just dedupes.
        let (_, c) = parse_constraint::<Fake>("rr", fake_map).unwrap();
        assert!(matches!(c, OperandConstraint::Any));

        // `"rmm"` collapses to `[Any, Mem]`.
        let (_, c) = parse_constraint::<Fake>("rmm", fake_map).unwrap();
        assert_eq!(alternative_kinds(&c), vec!["any", "mem"]);
    }

    #[test]
    fn reject_alternative_with_fixed_letter() {
        // Mixing a class letter with a Fixed register letter is
        // nonsensical — GCC rejects, we reject.
        assert!(matches!(
            parse_constraint::<Fake>("ra", fake_map),
            Err(ConstraintParseError::AlternativeWithFixed('a', _))
        ));
        // Same for `=` outputs.
        assert!(matches!(
            parse_constraint::<Fake>("=mb", fake_map),
            Err(ConstraintParseError::AlternativeWithFixed('b', _))
        ));
    }

    #[test]
    fn reject_alternative_with_match_digit() {
        // Match digits also can't appear inside multi-alt bodies.
        assert!(matches!(
            parse_constraint::<Fake>("r0", fake_map),
            Err(ConstraintParseError::AlternativeWithFixed('0', _))
        ));
    }

    #[test]
    fn single_char_fixed_still_works() {
        // C2/C3 behavior preserved: single-char Fixed letters still
        // produce `OperandConstraint::Fixed(_)`, not Alternatives.
        let (_, c) = parse_constraint::<Fake>("=a", fake_map).unwrap();
        assert!(matches!(c, OperandConstraint::Fixed(Fake::A)));
    }

    #[test]
    fn single_char_match_still_works() {
        let (_, c) = parse_constraint::<Fake>("0", fake_map).unwrap();
        assert!(matches!(c, OperandConstraint::Match(0)));
    }
}
