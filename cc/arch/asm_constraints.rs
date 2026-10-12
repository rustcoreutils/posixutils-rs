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

use crate::arch::aarch64::regalloc::Reg as A64Reg;
use crate::arch::x86_64::regalloc::Reg as X86Reg;
use crate::float::FloatVal;
use crate::ir::{AsmConstraint, AsmData, PseudoId};
use crate::target::Arch;
use crate::units::Bits;

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

/// The one general register an operand is pinned to: by an x86-64 letter
/// (`a`, `b`, `c`, `d`, `S`, `D`), or on either target by a GNU register
/// variable (`register long x asm("r10")`) used as a register operand.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PinnedGp {
    X86_64(X86Reg),
    Aarch64(A64Reg),
}

impl PinnedGp {
    /// The general register `name` spells on `arch` -- as a clobber list
    /// spells it, so `rax`, `eax`, `r10` and `%rdx` all name one on x86-64,
    /// `x0`, `w0` and `sp` on aarch64. `None` for anything else, which a
    /// register variable reports as an invalid register name. The zero
    /// register holds nothing, so it is none.
    pub fn named(name: &str, arch: Arch) -> Option<Self> {
        match arch {
            Arch::X86_64 => {
                crate::arch::x86_64::regalloc::parse_gp_clobber_name(name).map(Self::X86_64)
            }
            Arch::Aarch64 => crate::arch::aarch64::regalloc::parse_gp_clobber_name(name)
                .filter(|&r| r != A64Reg::Xzr)
                .map(Self::Aarch64),
        }
    }

    /// The x86-64 register this is, if it is one.
    pub fn x86_64(self) -> Option<X86Reg> {
        match self {
            Self::X86_64(r) => Some(r),
            Self::Aarch64(_) => None,
        }
    }

    /// The aarch64 register this is, if it is one.
    pub fn aarch64(self) -> Option<A64Reg> {
        match self {
            Self::Aarch64(r) => Some(r),
            Self::X86_64(_) => None,
        }
    }

    /// What a file-scope register variable here would be. Every function
    /// of the unit leaves the register alone -- never allocates, saves or
    /// restores it -- and only the transfers the calling convention makes
    /// write it, as in gcc.
    pub fn global_hold(self) -> GlobalHold {
        use A64Reg as A;
        use X86Reg as X;
        match self {
            Self::X86_64(X::Rsp | X::Rbx | X::R12 | X::R13 | X::R14 | X::R15)
            | Self::Aarch64(
                A::SP
                | A::X19
                | A::X20
                | A::X21
                | A::X22
                | A::X23
                | A::X24
                | A::X25
                | A::X26
                | A::X27
                | A::X28,
            ) => GlobalHold::Reserved,
            // An argument register, or one only the allocator hands out.
            Self::X86_64(X::Rsi | X::Rdi)
            | Self::Aarch64(
                A::X0
                | A::X1
                | A::X2
                | A::X3
                | A::X4
                | A::X5
                | A::X6
                | A::X7
                | A::X12
                | A::X13
                | A::X14,
            ) => GlobalHold::CallClobbered,
            Self::X86_64(X::Rbp) | Self::Aarch64(A::X29) => GlobalHold::FramePointer,
            _ => GlobalHold::Unavailable,
        }
    }

    /// The stack pointer: `%rsp`, `sp`.
    pub fn is_stack_pointer(self) -> bool {
        matches!(self, Self::X86_64(X86Reg::Rsp) | Self::Aarch64(A64Reg::SP))
    }

    /// The frame pointer, which every c17 function keeps: `%rbp`, `x29`.
    pub fn is_frame_pointer(self) -> bool {
        matches!(self, Self::X86_64(X86Reg::Rbp) | Self::Aarch64(A64Reg::X29))
    }
}

/// What a file-scope register variable can be in its register
/// ([`PinnedGp::global_hold`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GlobalHold {
    /// The stack pointer, which is never allocated, or a callee-saved
    /// register, which a function in another unit preserves.
    Reserved,
    /// A register a call may clobber: a function in another unit is free to
    /// change it, and an argument or return value in it overwrites the
    /// variable. gcc takes it with a warning, and so does c17.
    CallClobbered,
    /// The frame pointer, which every c17 function keeps: gcc refuses a
    /// function needing one ("frame pointer required, but reserved"), and
    /// every c17 function does.
    FramePointer,
    /// A register c17's generated code uses for its own purposes -- a
    /// scratch register, or one an instruction such as a division or a
    /// shift writes -- which gcc takes, saving it around such an
    /// instruction, and c17 cannot.
    Unavailable,
}

/// The register class an operand may be given.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsmRegClass {
    /// Any general register: `r`, and x86-64 `q`, `R`, `l`.
    General,
    /// The one general register an operand is pinned to; see [`PinnedGp`].
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

/// The constants an operand may be written into the template as.
///
/// gcc distinguishes three kinds, and so must the classification: `n` takes
/// an integer and nothing else, `s` a link-time symbolic address and never an
/// integer, `i` either -- and a floating constant too. Which
/// letters take a symbolic address also differs by target: under the
/// position-independent code both targets build by default, gcc substitutes
/// `$sym` for x86-64 `i`, while on aarch64 only `S` names a symbol.
///
/// Most integer letters take only a range, which is what an instruction
/// field encodes: x86-64 `I` a shift count, aarch64 `K` a logical
/// immediate. A constant outside every range offered is "impossible".
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct AsmImm {
    /// The integer constants allowed.
    pub int: IntRanges,
    /// The floating constants allowed: x86-64 substitutes the bit pattern,
    /// aarch64 the value.
    pub float: FloatImm,
    /// The address of a global object or function, plus a constant offset.
    pub symbol: bool,
}

/// The integers one immediate letter takes on its target. Each is checked
/// against the constant sign-extended from the operand's width, as gcc
/// checks its canonical constant: `"N"((unsigned char)200)` is -56, and out
/// of range.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntRange {
    /// Any integer: `i`, `n`, `g`, `X`.
    Any,
    /// x86-64 `I`: a 32-bit shift count, 0..=31.
    X86Shift32,
    /// x86-64 `J`: a 64-bit shift count, 0..=63.
    X86Shift64,
    /// x86-64 `K`: a signed 8-bit constant.
    X86Signed8,
    /// x86-64 `L`: `0xff`, `0xffff` or `0xffffffff`, the masks a `movz`
    /// replaces an `and` with.
    X86ZeroExtendMask,
    /// x86-64 `M`: an `lea` shift, 0..=3.
    X86LeaShift,
    /// x86-64 `N`: an `in`/`out` port, 0..=255.
    X86Port,
    /// x86-64 `O`: a 128-bit shift count, 0..=127.
    X86Shift128,
    /// x86-64 `e`: a signed 32-bit constant.
    X86Signed32,
    /// x86-64 `Z`: an unsigned 32-bit constant.
    X86Unsigned32,
    /// aarch64 `I`: an `add` immediate, 12 bits optionally shifted by 12.
    A64Add,
    /// aarch64 `J`: the negation of an `add` immediate, for `sub`.
    A64Sub,
    /// aarch64 `K`: a 32-bit logical immediate.
    A64Logical32,
    /// aarch64 `L`: a 64-bit logical immediate.
    A64Logical64,
    /// aarch64 `M`: a 32-bit value one `mov` loads.
    A64Mov32,
    /// aarch64 `N`: a 64-bit value one `mov` loads.
    A64Mov64,
    /// aarch64 `Z`: zero.
    Zero,
}

impl IntRange {
    const ALL: [IntRange; 17] = [
        IntRange::Any,
        IntRange::X86Shift32,
        IntRange::X86Shift64,
        IntRange::X86Signed8,
        IntRange::X86ZeroExtendMask,
        IntRange::X86LeaShift,
        IntRange::X86Port,
        IntRange::X86Shift128,
        IntRange::X86Signed32,
        IntRange::X86Unsigned32,
        IntRange::A64Add,
        IntRange::A64Sub,
        IntRange::A64Logical32,
        IntRange::A64Logical64,
        IntRange::A64Mov32,
        IntRange::A64Mov64,
        IntRange::Zero,
    ];

    /// Whether `v`, already sign-extended from the operand's width, is in
    /// the range.
    pub fn admits(self, v: i64) -> bool {
        match self {
            IntRange::Any => true,
            IntRange::X86Shift32 => (0..=31).contains(&v),
            IntRange::X86Shift64 => (0..=63).contains(&v),
            IntRange::X86Signed8 => i8::try_from(v).is_ok(),
            IntRange::X86ZeroExtendMask => matches!(v, 0xff | 0xffff | 0xffff_ffff),
            IntRange::X86LeaShift => (0..=3).contains(&v),
            IntRange::X86Port => (0..=255).contains(&v),
            IntRange::X86Shift128 => (0..=127).contains(&v),
            IntRange::X86Signed32 => i32::try_from(v).is_ok(),
            IntRange::X86Unsigned32 => u32::try_from(v).is_ok(),
            IntRange::A64Add => a64_add_imm(v),
            IntRange::A64Sub => v.checked_neg().is_some_and(a64_add_imm),
            IntRange::A64Logical32 => a64_logical_imm(replicate32(v)),
            IntRange::A64Logical64 => a64_logical_imm(v as u64),
            IntRange::A64Mov32 => a64_mov_imm(v as u64, true),
            IntRange::A64Mov64 => a64_mov_imm(v as u64, false),
            IntRange::Zero => v == 0,
        }
    }
}

/// A set of [`IntRange`]s: the integer letters one constraint lists.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct IntRanges(u32);

impl IntRanges {
    const ANY: Self = Self::of(IntRange::Any);

    const fn of(r: IntRange) -> Self {
        Self(1 << r as u32)
    }

    /// Whether no integer is allowed.
    pub fn is_empty(self) -> bool {
        self.0 == 0
    }

    /// Whether some range offered holds `v`, sign-extended from the
    /// operand's width (see [`canonical_int`]).
    pub fn admits(self, v: i64) -> bool {
        IntRange::ALL
            .iter()
            .any(|&r| self.0 & Self::of(r).0 != 0 && r.admits(v))
    }

    fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }
}

/// The floating constants a constraint takes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum FloatImm {
    #[default]
    None,
    /// aarch64 `Y`: positive zero, the one value `fcmp` and `fmov` name
    /// without a register.
    Zero,
    /// Any floating constant.
    Any,
}

impl FloatImm {
    /// Whether `v` is allowed.
    pub fn admits(self, v: FloatVal) -> bool {
        match self {
            FloatImm::None => false,
            FloatImm::Zero => v.is_positive_zero(),
            FloatImm::Any => true,
        }
    }
}

/// An integer constant as gcc substitutes and checks it: sign-extended from
/// the operand's width. `"i"(0xffffffffu)` is `$-1`, not `$4294967295`.
pub fn canonical_int(v: i128, width: Bits) -> i64 {
    let bits = width.get();
    match bits {
        1..=63 => ((v as i64) << (64 - bits)) >> (64 - bits),
        _ => v as i64,
    }
}

/// An aarch64 `add`/`sub` immediate: 12 bits, optionally shifted left by 12.
fn a64_add_imm(v: i64) -> bool {
    v & 0xfff == v || v & 0xfff000 == v
}

/// The low 32 bits of `v` in both halves: a 32-bit logical immediate is the
/// 64-bit one whose pattern repeats at least every 32 bits.
fn replicate32(v: i64) -> u64 {
    let low = v as u64 & 0xffff_ffff;
    low | low << 32
}

/// An aarch64 64-bit logical (bitmask) immediate: a pattern of 2, 4, ..., 64
/// bits repeated across the register, each a rotated run of ones, never all
/// zeros or all ones.
fn a64_logical_imm(v: u64) -> bool {
    if v == 0 || v == u64::MAX {
        return false;
    }
    // The smallest period the value repeats with.
    let mut size = 64;
    while size > 2 {
        let half = size / 2;
        let mask = (1u64 << half) - 1;
        if (v & mask) != ((v >> half) & mask) {
            break;
        }
        size = half;
    }
    let mask = if size == 64 {
        u64::MAX
    } else {
        (1u64 << size) - 1
    };
    let elem = v & mask;
    // A rotated run of ones within `size` bits has exactly one 0->1 and one
    // 1->0 transition going round the element.
    let rotated = ((elem >> 1) | (elem << (size - 1))) & mask;
    (elem ^ rotated).count_ones() == 2
}

/// A value one aarch64 `mov` loads: one 16-bit chunk (`movz`), its inverse
/// (`movn`), or a logical immediate (`orr` from the zero register). A 32-bit
/// value is read in its low 32 bits, where a 32-bit logical immediate
/// repeats; a 64-bit one is taken whole.
fn a64_mov_imm(v: u64, w: bool) -> bool {
    let mask = if w { 0xffff_ffff } else { u64::MAX };
    let movz = |x: u64| (0..64).step_by(16).any(|s| x & !(0xffffu64 << s) == 0);
    movz(v & mask) || movz(!v & mask) || a64_logical_imm(if w { replicate32(v as i64) } else { v })
}

impl AsmImm {
    const INT: Self = Self::ints(IntRange::Any);
    const NUMBER: Self = Self {
        int: IntRanges::ANY,
        float: FloatImm::Any,
        symbol: false,
    };
    const SYMBOL: Self = Self {
        int: IntRanges(0),
        float: FloatImm::None,
        symbol: true,
    };
    const FLOAT_ZERO: Self = Self {
        int: IntRanges(0),
        float: FloatImm::Zero,
        symbol: false,
    };
    const ANY: Self = Self {
        int: IntRanges::ANY,
        float: FloatImm::Any,
        symbol: true,
    };

    /// The integers of one range, and nothing else.
    const fn ints(r: IntRange) -> Self {
        Self {
            int: IntRanges::of(r),
            float: FloatImm::None,
            symbol: false,
        }
    }

    /// Whether any constant is allowed.
    pub fn any(self) -> bool {
        !self.int.is_empty() || self.float != FloatImm::None || self.symbol
    }

    fn union(self, other: Self) -> Self {
        Self {
            int: self.int.union(other.int),
            float: self.float.max(other.float),
            symbol: self.symbol || other.symbol,
        }
    }

    /// What the operand must be, for a diagnostic.
    pub fn describe(self) -> &'static str {
        let int = !self.int.is_empty();
        let float = self.float != FloatImm::None;
        match (int || float, self.symbol) {
            (true, true) => "a constant",
            (false, true) => "a symbolic address constant",
            _ if !float => "an integer constant",
            _ if !int => "a floating constant",
            _ => "a numeric constant",
        }
    }
}

/// A constraint string c17 does not classify, and so rejects rather than
/// read as some other class.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintError {
    /// A letter, or a multi-letter sequence, that c17 does not model on the
    /// target.
    Unsupported(String),
    /// A flag output naming no condition the target has: `"=@ccfoo"`, or
    /// x86-64's `"=@cceq"`.
    UnknownFlag,
    /// A flag output with alternatives: `"=@ccz,r"`.
    FlagAlternatives,
    /// `@` anywhere but at the head of a write-only output: an input, a
    /// `"+"` output, or after a register or memory letter.
    FlagPunctuation,
    /// An early-clobber flag output on aarch64, `"=&@cceq"`, which gcc finds
    /// no way to satisfy there (x86-64 takes it).
    EarlyClobberFlag,
}

impl ConstraintError {
    /// The diagnostic for `constraint`.
    pub fn message(&self, constraint: &str) -> String {
        match self {
            ConstraintError::Unsupported(what) => {
                format!("unsupported constraint '{what}' in asm operand \"{constraint}\"")
            }
            ConstraintError::UnknownFlag => {
                format!("unknown 'asm' flag output '{constraint}'")
            }
            ConstraintError::FlagAlternatives => {
                "alternatives not allowed in 'asm' flag output".to_string()
            }
            ConstraintError::FlagPunctuation => "invalid punctuation '@' in constraint".to_string(),
            ConstraintError::EarlyClobberFlag => "impossible constraint in 'asm'".to_string(),
        }
    }
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
    /// The constants that may be substituted as an immediate.
    pub imm: AsmImm,
    /// A matching constraint: the output operand number it names, which may
    /// have more than one digit.
    pub tied: Option<usize>,
    /// A flag output, `"=@cc<cond>"`: the operand is no register, memory or
    /// constant, but the condition read off the flags the template leaves,
    /// 1 when it holds and 0 when not.
    pub flag: Option<FlagCondition>,
}

/// The condition a flag output reads (`"=@ccnz"`, `"=@cceq"`): one gcc names
/// for the target, kept as the suffix of the conditional set that reads it
/// -- `setnz`, `cset w0, eq` -- since gcc's names are the assemblers' own
/// condition suffixes on both targets.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FlagCondition(&'static str);

impl FlagCondition {
    /// gcc's x86-64 names: the `set<cc>` suffixes, without `pe` and `po`.
    const X86_64: [&'static str; 28] = [
        "a", "ae", "b", "be", "c", "e", "z", "g", "ge", "l", "le", "na", "nae", "nb", "nbe", "nc",
        "ne", "ng", "nge", "nl", "nle", "no", "np", "ns", "nz", "o", "p", "s",
    ];
    /// gcc's aarch64 names: the condition codes but `al` and `nv`, which
    /// test nothing.
    const AARCH64: [&'static str; 16] = [
        "eq", "ne", "cs", "hs", "cc", "lo", "mi", "pl", "vs", "vc", "hi", "ls", "ge", "lt", "gt",
        "le",
    ];

    /// The condition `name` is on `arch`, if gcc names one so.
    pub fn named(name: &str, arch: Arch) -> Option<Self> {
        let names: &[&'static str] = match arch {
            Arch::X86_64 => &Self::X86_64,
            Arch::Aarch64 => &Self::AARCH64,
        };
        names.iter().find(|&&n| n == name).map(|&n| Self(n))
    }

    /// The condition as its target's conditional set spells it.
    pub fn suffix(self) -> &'static str {
        self.0
    }
}

/// What one letter contributes to a class.
enum Letter {
    Reg(AsmRegClass),
    Mem(AsmMemClass),
    Imm(AsmImm),
    /// `g` and `X`: a general register, memory or an immediate.
    Any,
    /// A modifier that leaves the class as it is: `%` (commutative), `,`
    /// (between alternatives), and the register-preference and
    /// auto-increment markers `*`, `?`, `!`, `^`, `$`, `<`, `>`.
    Modifier,
}

impl AsmOperandClass {
    /// Classify `constraint` for `arch`.
    ///
    /// Alternatives listed in one string (`"rm"`, `"ri"`, `"g"`) accumulate:
    /// the operand may be any of them. A letter or sequence c17 does not
    /// model is an error naming it: read as nothing, it left the operand to
    /// whatever class the rest of the string gave it, and `"Yz"` became an
    /// arbitrary vector register. A flag output, `"=@cc<cond>"`, is the
    /// whole rest of the string after its `@`.
    pub fn parse(constraint: &str, arch: Arch) -> Result<Self, ConstraintError> {
        let mut class = AsmOperandClass {
            access: AsmAccess::Read,
            early_clobber: false,
            reg: None,
            mem: None,
            imm: AsmImm::default(),
            tied: None,
            flag: None,
        };
        let mut chars = constraint.chars().peekable();
        while let Some(c) = chars.next() {
            match c {
                '=' if class.access == AsmAccess::Read => class.access = AsmAccess::Write,
                '+' => class.access = AsmAccess::ReadWrite,
                '&' => class.early_clobber = true,
                '@' => return class.flag_output(&chars.collect::<String>(), arch),
                // `#`: the rest of the alternative only steers register
                // preference; it is not part of the constraint.
                '#' => while chars.next_if(|&d| d != ',').is_some() {},
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
                    Some(Letter::Imm(k)) => class.imm = class.imm.union(k),
                    Some(Letter::Any) => {
                        class.add_reg(AsmRegClass::General);
                        class.add_mem(AsmMemClass::Any);
                        class.imm = AsmImm::ANY;
                    }
                    Some(Letter::Modifier) => {}
                    None => {
                        let mut what = c.to_string();
                        for _ in 0..sequence_tail(c, arch) {
                            what.extend(chars.next());
                        }
                        return Err(ConstraintError::Unsupported(what));
                    }
                },
            }
        }
        Ok(class)
    }

    /// An operand in `reg` and nowhere else, accessed as `access`: how a
    /// file-scope register variable is read and written.
    pub fn in_register(reg: PinnedGp, access: AsmAccess) -> Self {
        AsmOperandClass {
            access,
            early_clobber: false,
            reg: Some(AsmRegClass::Pinned(reg)),
            mem: None,
            imm: AsmImm::default(),
            tied: None,
            flag: None,
        }
    }

    /// This class, narrowed to `reg` when it offers any general register:
    /// what a register variable as the operand of `"r"`, `"+r"`, `"g"` or
    /// `"rm"` asks for, since gcc then places the operand in the variable's
    /// register. A class that names its own register (`"a"`), or offers
    /// none, is left alone, as is a tied input, which takes its output's.
    pub fn pinned_to(self, reg: PinnedGp) -> Self {
        if self.reg != Some(AsmRegClass::General) || self.tied.is_some() {
            return self;
        }
        AsmOperandClass {
            early_clobber: self.early_clobber,
            ..Self::in_register(reg, self.access)
        }
    }

    /// This class, a write-only output so far, as the flag output `rest`
    /// names: `cc` and a condition of the target's, all of what follows the
    /// `@`, with no alternatives.
    fn flag_output(mut self, rest: &str, arch: Arch) -> Result<Self, ConstraintError> {
        let bare = self.reg.is_none() && self.mem.is_none() && !self.imm.any();
        if self.access != AsmAccess::Write || !bare || self.tied.is_some() {
            return Err(ConstraintError::FlagPunctuation);
        }
        if rest.contains(',') {
            return Err(ConstraintError::FlagAlternatives);
        }
        let cond = rest
            .strip_prefix("cc")
            .and_then(|name| FlagCondition::named(name, arch))
            .ok_or(ConstraintError::UnknownFlag)?;
        if self.early_clobber && arch == Arch::Aarch64 {
            return Err(ConstraintError::EarlyClobberFlag);
        }
        self.flag = Some(cond);
        Ok(self)
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
        self.mem.is_some() && self.reg.is_none() && !self.imm.any()
    }

    /// True when the operand can only be a constant written into the
    /// template: no register, memory or matching alternative is offered.
    pub fn is_immediate_only(&self) -> bool {
        self.imm.any() && self.reg.is_none() && self.mem.is_none() && self.tied.is_none()
    }
}

/// What `c` means on `arch`, or `None` for a letter c17 does not model.
fn letter(c: char, arch: Arch) -> Option<Letter> {
    use AsmRegClass::*;
    Some(match (arch, c) {
        (_, '%' | ',' | '*' | '?' | '!' | '^' | '$' | '<' | '>') => Letter::Modifier,
        (_, 'r') => Letter::Reg(General),
        (_, 'm' | 'o' | 'V') => Letter::Mem(AsmMemClass::Any),
        (_, 'n') => Letter::Imm(AsmImm::INT),
        (_, 'g' | 'X') => Letter::Any,

        (Arch::X86_64, 'i') => Letter::Imm(AsmImm::ANY),
        (Arch::X86_64, 'I') => Letter::Imm(AsmImm::ints(IntRange::X86Shift32)),
        (Arch::X86_64, 'J') => Letter::Imm(AsmImm::ints(IntRange::X86Shift64)),
        (Arch::X86_64, 'K') => Letter::Imm(AsmImm::ints(IntRange::X86Signed8)),
        (Arch::X86_64, 'L') => Letter::Imm(AsmImm::ints(IntRange::X86ZeroExtendMask)),
        (Arch::X86_64, 'M') => Letter::Imm(AsmImm::ints(IntRange::X86LeaShift)),
        (Arch::X86_64, 'N') => Letter::Imm(AsmImm::ints(IntRange::X86Port)),
        (Arch::X86_64, 'O') => Letter::Imm(AsmImm::ints(IntRange::X86Shift128)),
        (Arch::X86_64, 'e') => Letter::Imm(AsmImm::ints(IntRange::X86Signed32)),
        (Arch::X86_64, 'Z') => Letter::Imm(AsmImm::ints(IntRange::X86Unsigned32)),
        (Arch::X86_64, 's') => Letter::Imm(AsmImm::SYMBOL),
        (Arch::X86_64, 'q' | 'R' | 'l') => Letter::Reg(General),
        (Arch::X86_64, 'a') => Letter::Reg(Pinned(PinnedGp::X86_64(X86Reg::Rax))),
        (Arch::X86_64, 'b') => Letter::Reg(Pinned(PinnedGp::X86_64(X86Reg::Rbx))),
        (Arch::X86_64, 'c') => Letter::Reg(Pinned(PinnedGp::X86_64(X86Reg::Rcx))),
        (Arch::X86_64, 'd') => Letter::Reg(Pinned(PinnedGp::X86_64(X86Reg::Rdx))),
        (Arch::X86_64, 'S') => Letter::Reg(Pinned(PinnedGp::X86_64(X86Reg::Rsi))),
        (Arch::X86_64, 'D') => Letter::Reg(Pinned(PinnedGp::X86_64(X86Reg::Rdi))),
        (Arch::X86_64, 'Q') => Letter::Reg(HighByte),
        (Arch::X86_64, 'x' | 'v') => Letter::Reg(Vector),
        (Arch::X86_64, 'f') => Letter::Reg(X87(X87Slot::Any)),
        (Arch::X86_64, 't') => Letter::Reg(X87(X87Slot::Top)),
        (Arch::X86_64, 'u') => Letter::Reg(X87(X87Slot::Second)),

        (Arch::Aarch64, 'i') => Letter::Imm(AsmImm::NUMBER),
        (Arch::Aarch64, 'S') => Letter::Imm(AsmImm::SYMBOL),
        (Arch::Aarch64, 'Y') => Letter::Imm(AsmImm::FLOAT_ZERO),
        (Arch::Aarch64, 'I') => Letter::Imm(AsmImm::ints(IntRange::A64Add)),
        (Arch::Aarch64, 'J') => Letter::Imm(AsmImm::ints(IntRange::A64Sub)),
        (Arch::Aarch64, 'K') => Letter::Imm(AsmImm::ints(IntRange::A64Logical32)),
        (Arch::Aarch64, 'L') => Letter::Imm(AsmImm::ints(IntRange::A64Logical64)),
        (Arch::Aarch64, 'M') => Letter::Imm(AsmImm::ints(IntRange::A64Mov32)),
        (Arch::Aarch64, 'N') => Letter::Imm(AsmImm::ints(IntRange::A64Mov64)),
        (Arch::Aarch64, 'Z') => Letter::Imm(AsmImm::ints(IntRange::Zero)),
        (Arch::Aarch64, 'w' | 'x' | 'y') => Letter::Reg(Vector),
        (Arch::Aarch64, 'Q') => Letter::Mem(AsmMemClass::BaseOnly),

        _ => return None,
    })
}

/// How many characters after `c` belong to the same constraint, for the
/// letters that open a multi-letter one on `arch` (x86-64 `Yz`, `Bm`, `Wz`,
/// `Tv`; aarch64 `Ump`, `Dz`). Only the diagnostic reads it: none of these
/// is modelled, and naming `Y` alone for `"Yz"` would misdescribe it.
fn sequence_tail(c: char, arch: Arch) -> usize {
    match (arch, c) {
        (Arch::X86_64, 'Y' | 'B' | 'W' | 'T') => 1,
        (Arch::Aarch64, 'U') => 2,
        (Arch::Aarch64, 'D') => 1,
        _ => 0,
    }
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

/// The register each operand of `asm` is pinned to -- outputs, then inputs,
/// in order -- given what `pin` answers for an operand in its own right. A
/// tied input names its output's operand, so it takes the output's register
/// whatever its own class says; `pin` is not asked about it.
pub fn operand_pins<R: Copy>(
    asm: &AsmData,
    mut pin: impl FnMut(&AsmConstraint) -> Option<R>,
) -> Vec<Option<R>> {
    let mut pins: Vec<Option<R>> = Vec::with_capacity(asm.outputs.len() + asm.inputs.len());
    for (idx, c) in asm.outputs.iter().chain(&asm.inputs).enumerate() {
        let tied = c
            .matching_output
            .filter(|&i| idx >= asm.outputs.len() && i < asm.outputs.len());
        let p = match tied {
            Some(i) => pins[i],
            None => pin(c),
        };
        pins.push(p);
    }
    pins
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
    use crate::units::Bits;
    use AsmRegClass::*;

    fn x86(s: &str) -> AsmOperandClass {
        AsmOperandClass::parse(s, Arch::X86_64).unwrap()
    }

    fn a64(s: &str) -> AsmOperandClass {
        AsmOperandClass::parse(s, Arch::Aarch64).unwrap()
    }

    /// The class of an operand an x86-64 letter pins to `r`.
    fn pin(r: X86Reg) -> Option<AsmRegClass> {
        Some(Pinned(PinnedGp::X86_64(r)))
    }

    /// `(reg, mem, imm)` for a constraint body.
    type Allows = (Option<AsmRegClass>, Option<AsmMemClass>, bool);

    const MEM: Option<AsmMemClass> = Some(AsmMemClass::Any);

    fn allows(c: AsmOperandClass) -> Allows {
        (c.reg, c.mem, c.imm.any())
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
            ("a", (pin(X86Reg::Rax), None, false)),
            ("b", (pin(X86Reg::Rbx), None, false)),
            ("c", (pin(X86Reg::Rcx), None, false)),
            ("d", (pin(X86Reg::Rdx), None, false)),
            ("S", (pin(X86Reg::Rsi), None, false)),
            ("D", (pin(X86Reg::Rdi), None, false)),
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
            ("O", (None, None, true)),
            // A named register outranks a class listed beside it.
            ("ra", (pin(X86Reg::Rax), None, false)),
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
        assert_eq!(x86("=&d").reg, pin(X86Reg::Rdx));
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
        assert_eq!((tied.reg, tied.mem, tied.imm.any()), (None, None, false));
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

    /// Which constants each immediate letter takes, per target.
    #[test]
    fn immediate_kinds() {
        const INT: AsmImm = AsmImm::INT;
        const SYM: AsmImm = AsmImm::SYMBOL;
        let table: &[(Arch, &str, AsmImm)] = &[
            // x86-64 `i` substitutes `$sym`; aarch64's takes numbers only.
            (Arch::X86_64, "i", AsmImm::ANY),
            (Arch::Aarch64, "i", AsmImm::NUMBER),
            (Arch::X86_64, "n", INT),
            (Arch::Aarch64, "n", INT),
            (Arch::X86_64, "s", SYM),
            (Arch::Aarch64, "S", SYM),
            (Arch::X86_64, "e", AsmImm::ints(IntRange::X86Signed32)),
            (Arch::X86_64, "I", AsmImm::ints(IntRange::X86Shift32)),
            (Arch::Aarch64, "I", AsmImm::ints(IntRange::A64Add)),
            (Arch::Aarch64, "Z", AsmImm::ints(IntRange::Zero)),
            (Arch::Aarch64, "Y", AsmImm::FLOAT_ZERO),
            (
                Arch::X86_64,
                "IK",
                AsmImm {
                    int: IntRanges::of(IntRange::X86Shift32)
                        .union(IntRanges::of(IntRange::X86Signed8)),
                    ..AsmImm::default()
                },
            ),
            (
                Arch::X86_64,
                "ns",
                AsmImm {
                    symbol: true,
                    ..INT
                },
            ),
            (Arch::X86_64, "g", AsmImm::ANY),
            (Arch::X86_64, "r", AsmImm::default()),
        ];
        for &(arch, s, want) in table {
            let got = AsmOperandClass::parse(s, arch).unwrap().imm;
            assert_eq!(got, want, "{arch:?} {s:?}");
        }
    }

    /// Each range letter's bounds, as gcc 13 accepts and rejects them: the
    /// constant sign-extended from the operand's width is what is checked.
    #[test]
    fn immediate_ranges() {
        let admits =
            |arch, s: &str, v: i64| AsmOperandClass::parse(s, arch).unwrap().imm.int.admits(v);
        let x86: &[(&str, &[i64], &[i64])] = &[
            ("I", &[0, 31], &[-1, 32]),
            ("J", &[0, 63], &[-1, 64]),
            ("K", &[-128, 127], &[-129, 128]),
            (
                "L",
                &[0xff, 0xffff, 0xffff_ffff],
                &[0xfe, -1, 0x1_0000_0000],
            ),
            ("M", &[0, 3], &[-1, 4]),
            ("N", &[0, 255], &[-56, 256]),
            ("O", &[0, 127], &[-1, 128]),
            (
                "e",
                &[i32::MIN as i64, i32::MAX as i64],
                &[1 << 31, -(1 << 31) - 1],
            ),
            ("Z", &[0, 0xffff_ffff], &[-1, 1 << 32]),
            ("IK", &[31, -128, 127], &[128]),
            ("n", &[i64::MIN, i64::MAX], &[]),
        ];
        for &(s, yes, no) in x86 {
            for &v in yes {
                assert!(admits(Arch::X86_64, s, v), "x86-64 {s:?} {v:#x}");
            }
            for &v in no {
                assert!(!admits(Arch::X86_64, s, v), "x86-64 {s:?} {v:#x}");
            }
        }
        let a64: &[(&str, &[i64], &[i64])] = &[
            ("I", &[0, 4095, 0x1000, 0xfff000], &[-1, 4097, 0x1001000]),
            ("J", &[0, -1, -4095, -0x1000], &[1, -4097]),
            (
                "K",
                &[0xff, 0xff00ff00u32 as i32 as i64, 0x5555_5555, -256],
                &[0, -1, 0x1234],
            ),
            (
                "L",
                &[0xff, 0xff00_ff00_ff00_ff00u64 as i64, 0x0ff0, 1 << 63],
                &[0, -1, 0x1234, 0xff00_ff00],
            ),
            (
                "M",
                &[0, 0xffff, 0x1234_0000, -1, 0xffff_1234u32 as i32 as i64],
                &[0x1234_5678],
            ),
            (
                "N",
                &[
                    0x1234_0000_0000_0000,
                    -2,
                    0x0ff0,
                    0xffff_ffff_0000_ffffu64 as i64,
                ],
                &[0xff00_ff00, 0xffff_1234, 0x1234_5678],
            ),
            ("Z", &[0], &[1, -1]),
        ];
        for &(s, yes, no) in a64 {
            for &v in yes {
                assert!(admits(Arch::Aarch64, s, v), "aarch64 {s:?} {v:#x}");
            }
            for &v in no {
                assert!(!admits(Arch::Aarch64, s, v), "aarch64 {s:?} {v:#x}");
            }
        }
    }

    #[test]
    fn canonical_int_sign_extends() {
        assert_eq!(canonical_int(0xffff_ffff, Bits::new(32)), -1);
        assert_eq!(canonical_int(200, Bits::new(8)), -56);
        assert_eq!(canonical_int(200, Bits::new(16)), 200);
        assert_eq!(canonical_int(0xffff_ffff, Bits::new(64)), 0xffff_ffff);
        assert_eq!(canonical_int(-1, Bits::new(64)), -1);
    }

    #[test]
    fn immediate_only() {
        for s in ["i", "n", "s", "I", "e"] {
            assert!(x86(s).is_immediate_only(), "{s:?}");
        }
        for s in ["ri", "mi", "g", "X", "0", "r", "m", "i0"] {
            assert!(!x86(s).is_immediate_only(), "{s:?}");
        }
        assert!(a64("S").is_immediate_only());
        assert!(!a64("rS").is_immediate_only());
    }

    /// `#` hides the rest of its alternative from the constraint.
    #[test]
    fn hash_ends_the_alternative() {
        assert!(x86("i#*X").is_immediate_only());
        assert_eq!(x86("r#m").mem, None);
        assert_eq!(x86("r#m,m").mem, Some(AsmMemClass::Any));
    }

    /// Every letter or sequence c17 does not model is an error naming it,
    /// never a class it was silently read as.
    #[test]
    fn unsupported_constraints_are_errors() {
        use ConstraintError::*;
        let err = |s: &str, arch| AsmOperandClass::parse(s, arch).unwrap_err();
        let x86_table: &[(&str, &str)] = &[
            ("Yz", "Yz"),
            ("Y", "Y"),
            ("=Yk", "Yk"),
            ("Bm", "Bm"),
            ("Wz", "Wz"),
            ("Tv", "Tv"),
            ("p", "p"),
            ("A", "A"),
            ("y", "y"),
            ("k", "k"),
            ("C", "C"),
            ("G", "G"),
            ("E", "E"),
            ("F", "F"),
            ("r ", " "),
            ("-r", "-"),
            // aarch64's register letter is no x86-64 class.
            ("w", "w"),
        ];
        for &(s, what) in x86_table {
            assert_eq!(
                err(s, Arch::X86_64),
                Unsupported(what.into()),
                "x86-64 {s:?}"
            );
        }
        let a64_table: &[(&str, &str)] = &[
            ("Ump", "Ump"),
            ("=Utf", "Utf"),
            ("Dz", "Dz"),
            ("k", "k"),
            ("s", "s"),
            ("O", "O"),
            // x86-64's pinned letters name nothing here.
            ("a", "a"),
            ("D", "D"),
        ];
        for &(s, what) in a64_table {
            assert_eq!(
                err(s, Arch::Aarch64),
                Unsupported(what.into()),
                "aarch64 {s:?}"
            );
        }
        assert!(err("Yz", Arch::X86_64)
            .message("Yz")
            .contains("'Yz' in asm operand \"Yz\""));
    }

    /// A flag output is its condition, by every name gcc gives the target,
    /// and nothing else; anything else after `@` is refused as gcc refuses it.
    #[test]
    fn flag_outputs() {
        let cond = |s: &str, arch| AsmOperandClass::parse(s, arch).map(|c| c.flag);
        let suffix = |s: &str, arch| cond(s, arch).unwrap().unwrap().suffix();
        for name in FlagCondition::X86_64 {
            assert_eq!(suffix(&format!("=@cc{name}"), Arch::X86_64), name);
        }
        for name in FlagCondition::AARCH64 {
            assert_eq!(suffix(&format!("=@cc{name}"), Arch::Aarch64), name);
        }
        let c = x86("=@ccz");
        assert_eq!(
            (c.access, c.reg, c.mem, c.imm.any()),
            (AsmAccess::Write, None, None, false)
        );
        assert_eq!(suffix("=&@ccz", Arch::X86_64), "z");
        let table: &[(&str, Arch, ConstraintError)] = &[
            ("=@ccpe", Arch::X86_64, ConstraintError::UnknownFlag),
            ("=@ccpo", Arch::X86_64, ConstraintError::UnknownFlag),
            ("=@cceq", Arch::X86_64, ConstraintError::UnknownFlag),
            ("=@ccz", Arch::Aarch64, ConstraintError::UnknownFlag),
            ("=@ccal", Arch::Aarch64, ConstraintError::UnknownFlag),
            ("=@cc", Arch::X86_64, ConstraintError::UnknownFlag),
            ("=@z", Arch::X86_64, ConstraintError::UnknownFlag),
            ("=@ccz,r", Arch::X86_64, ConstraintError::FlagAlternatives),
            ("@ccz", Arch::X86_64, ConstraintError::FlagPunctuation),
            ("+@ccz", Arch::X86_64, ConstraintError::FlagPunctuation),
            ("=r@ccz", Arch::X86_64, ConstraintError::FlagPunctuation),
            ("=&@cceq", Arch::Aarch64, ConstraintError::EarlyClobberFlag),
        ];
        for (s, arch, want) in table {
            assert_eq!(cond(s, *arch).as_ref(), Err(want), "{arch:?} {s:?}");
        }
        assert_eq!(
            ConstraintError::UnknownFlag.message("=@ccfoo"),
            "unknown 'asm' flag output '=@ccfoo'"
        );
    }

    fn operand(pseudo: u32, constraint: &str) -> crate::ir::AsmConstraint {
        crate::ir::AsmConstraint::new(PseudoId(pseudo), constraint, Arch::X86_64, Bits::new(64))
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
