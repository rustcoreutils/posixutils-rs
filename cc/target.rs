//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Target configuration for c17
//
// Handles architecture, OS, and ABI-specific settings needed for
// preprocessing and code generation.
//

use crate::float::{ComplexDivision, ComplexRoutineFormat, Contraction};
use std::fmt;

/// The value of `__STDC_VERSION__`. c17 compiles one language: C17, the
/// ISO/IEC 9899:2018 revision POSIX.2024 binds the `c17` utility to.
pub const STDC_VERSION: &str = "201710L";

/// What a `-std=` argument asks for.
///
/// c17 implements a single language — C17 plus the GNU extensions it has always
/// provided — so this classifies the request rather than selecting a dialect.
/// Language *versions* and *extension sets* are not switchable: there is one
/// mode, and `-std=` exists only because build systems pass it unconditionally.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StdRequest {
    /// A C17 spelling (`c17`, `c18`, `gnu17`, `gnu18`, `iso9899:2017/2018`) —
    /// what we compile anyway, so it passes without comment.
    C17,
    /// An older revision (`c89`, `c99`, `c11`, the `gnu*` and `iso9899:`
    /// equivalents). Accepted and compiled as C17; the driver says so.
    Older,
}

/// Classify the argument of `-std=`, e.g. `c17`, `gnu11`, `iso9899:1999`.
///
/// Returns `None` for an unrecognized spelling, which the driver reports as an
/// error. A typo must not pass silently — accepting and discarding a `-std=`
/// is how `__STDC_VERSION__` once came to disagree with the binary's own name.
pub fn classify_std(spec: &str) -> Option<StdRequest> {
    // The revision names are keyed to their prefix, as in gcc: `c` and `gnu`
    // take the short forms, `iso9899:` the years. Accepting any number after
    // any prefix would let `c1990` or `iso9899:99` through -- spellings no
    // compiler defines, and far likelier a typo than a request.
    if let Some(year) = spec.strip_prefix("iso9899:") {
        return match year {
            "2017" | "2018" => Some(StdRequest::C17),
            "1990" | "199409" | "199x" | "1999" | "2011" => Some(StdRequest::Older),
            _ => None,
        };
    }

    let rev = spec
        .strip_prefix("gnu")
        .or_else(|| spec.strip_prefix('c'))?;
    match rev {
        "17" | "18" => Some(StdRequest::C17),
        "89" | "90" | "9x" | "99" | "1x" | "11" => Some(StdRequest::Older),
        _ => None,
    }
}

/// Target CPU architecture
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arch {
    X86_64,
    Aarch64,
}

impl fmt::Display for Arch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Arch::X86_64 => write!(f, "x86_64"),
            Arch::Aarch64 => write!(f, "aarch64"),
        }
    }
}

/// The position independence code is generated with.
///
/// One value drives both code generation and the `__PIC__`/`__PIE__` macros
/// that describe it, so a header that tests the macro sees the code it gets.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct PositionIndependence {
    /// Position-independent code: `-fPIC`, `-shared`, or a PIE.
    pub pic: bool,
    /// Code for a position-independent executable.
    pub pie: bool,
}

/// The x86-64 SIMD extensions code may assume beyond the SSE2 baseline,
/// in order: each implies the ones before it.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum X86Simd {
    #[default]
    Sse2,
    Sse3,
    Ssse3,
    Sse41,
    Sse42,
}

/// The x86-64 instruction-set extensions a compilation may assume, from
/// `-msse3` .. `-msse4.2`, `-mpopcnt`, their `-mno-` forms and `-march=`.
/// They are statements about the target, which the feature macros
/// (`__SSE4_1__` and the rest) report as gcc's do, and which pick the packed
/// instructions vector operations use (`arch::simd`). c17's intrinsic
/// headers provide every function whatever the level, since they are
/// written in portable C. AVX and later are not modelled:
/// `-march=x86-64-v3` claims only what v2 does.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct X86Isa {
    pub simd: X86Simd,
    pub popcnt: bool,
}

/// A set of the x86-64 extensions [`X86Isa`] models, as gcc's option
/// handling sees them: one bit each.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct IsaSet(u8);

impl IsaSet {
    const SSE3: Self = Self(1);
    const SSSE3: Self = Self(2);
    const SSE41: Self = Self(4);
    const SSE42: Self = Self(8);
    const POPCNT: Self = Self(16);

    /// SSE3 .. SSE4.2 up to `top`: an extension and every one it needs.
    const fn and_below(top: Self) -> Self {
        Self(top.0 | (top.0 - 1) & 15)
    }

    /// `bottom` and every SIMD extension that needs it.
    const fn and_above(bottom: Self) -> Self {
        Self(!(bottom.0 - 1) & 15)
    }

    pub(crate) const fn with(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    const fn without(self, other: Self) -> Self {
        Self(self.0 & !other.0)
    }

    const fn has(self, other: Self) -> bool {
        self.0 & other.0 == other.0
    }

    /// What `-march=cpu` enables, of these: gcc 13's `-dM -E` for each
    /// 64-bit name in its roster. A name not listed here -- `x86-64`,
    /// `k8`, `opteron`, `athlon64`, `athlon-fx` -- is the SSE2 baseline.
    pub(crate) fn of_arch(cpu: &str) -> Self {
        let sse42 = Self::and_below(Self::SSE42).with(Self::POPCNT);
        match cpu {
            "native" => Self::host(),
            "nocona" | "k8-sse3" | "opteron-sse3" | "athlon64-sse3" | "eden-x2" => Self::SSE3,
            "core2" | "bonnell" | "atom" | "nano" | "nano-1000" | "nano-2000" => {
                Self::and_below(Self::SSSE3)
            }
            "nano-3000" | "nano-x2" | "eden-x4" | "nano-x4" => Self::and_below(Self::SSE41),
            "amdfam10" | "barcelona" => Self::SSE3.with(Self::POPCNT),
            "btver1" => Self::and_below(Self::SSSE3).with(Self::POPCNT),
            "nehalem" | "corei7" | "westmere" | "sandybridge" | "corei7-avx" | "ivybridge"
            | "core-avx-i" | "haswell" | "core-avx2" | "broadwell" | "skylake"
            | "skylake-avx512" | "cannonlake" | "icelake-client" | "rocketlake"
            | "icelake-server" | "cascadelake" | "tigerlake" | "cooperlake" | "sapphirerapids"
            | "emeraldrapids" | "alderlake" | "raptorlake" | "meteorlake" | "graniterapids"
            | "graniterapids-d" | "silvermont" | "slm" | "goldmont" | "goldmont-plus"
            | "tremont" | "gracemont" | "sierraforest" | "grandridge" | "knl" | "knm"
            | "x86-64-v2" | "x86-64-v3" | "x86-64-v4" | "lujiazui" | "bdver1" | "bdver2"
            | "bdver3" | "bdver4" | "znver1" | "znver2" | "znver3" | "znver4" | "btver2" => sse42,
            _ => Self::default(),
        }
    }

    /// What `-march=native` finds on this machine, when it is an x86-64.
    fn host() -> Self {
        #[cfg(target_arch = "x86_64")]
        {
            let mut set = Self::default();
            for (on, ext) in [
                (std::arch::is_x86_feature_detected!("sse3"), Self::SSE3),
                (std::arch::is_x86_feature_detected!("ssse3"), Self::SSSE3),
                (std::arch::is_x86_feature_detected!("sse4.1"), Self::SSE41),
                (std::arch::is_x86_feature_detected!("sse4.2"), Self::SSE42),
                (std::arch::is_x86_feature_detected!("popcnt"), Self::POPCNT),
            ] {
                if on {
                    set = set.with(ext);
                }
            }
            set
        }
        #[cfg(not(target_arch = "x86_64"))]
        {
            Self::default()
        }
    }
}

/// What one ISA `-m` option does: turn on an extension and those it needs,
/// or turn off one and those that need it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum IsaEdit {
    Enable(IsaSet),
    Disable(IsaSet),
}

/// The edit an ISA `-m` option makes, or `None` for any other flag.
pub(crate) fn isa_edit(flag: &str) -> Option<IsaEdit> {
    use IsaEdit::{Disable, Enable};
    Some(match flag {
        "-msse3" => Enable(IsaSet::and_below(IsaSet::SSE3)),
        "-mssse3" => Enable(IsaSet::and_below(IsaSet::SSSE3)),
        "-msse4.1" => Enable(IsaSet::and_below(IsaSet::SSE41)),
        "-msse4.2" | "-msse4" => Enable(IsaSet::and_below(IsaSet::SSE42)),
        "-mpopcnt" => Enable(IsaSet::POPCNT),
        "-mno-sse3" => Disable(IsaSet::and_above(IsaSet::SSE3)),
        "-mno-ssse3" => Disable(IsaSet::and_above(IsaSet::SSSE3)),
        // gcc's -mno-sse4 is -mno-sse4.1, not the inverse of -msse4.
        "-mno-sse4.1" | "-mno-sse4" => Disable(IsaSet::and_above(IsaSet::SSE41)),
        "-mno-sse4.2" => Disable(IsaSet::SSE42),
        "-mno-popcnt" => Disable(IsaSet::POPCNT),
        _ => return None,
    })
}

/// The option an ISA flag sets, whichever its sense: gcc's driver keeps
/// only the last of `-mX` and `-mno-X`. `-msse4` and `-mno-sse4` are two
/// options, each its own.
fn isa_option(flag: &str) -> &str {
    match flag {
        "-msse4" | "-mno-sse4" => flag,
        _ => flag.strip_prefix("-mno-").unwrap_or(&flag[2..]),
    }
}

/// Whether `flag` is one of the ISA options [`X86Isa`] models: `-msse3` ..
/// `-msse4.2`, `-msse4`, `-mpopcnt`, and their `-mno-` forms.
pub fn is_isa_flag(flag: &str) -> bool {
    isa_edit(flag).is_some()
}

impl X86Isa {
    /// The extensions `flags` -- the `-m` options, in order -- ask for, as
    /// gcc decides them: the last `-march=` gives a base, the ISA options
    /// edit it in order wherever they stand, and an extension one of them
    /// named keeps the state it gave, whatever the CPU has. SSE4.2 brings
    /// POPCNT unless an option named POPCNT.
    pub fn from_flags(flags: &[String]) -> Self {
        let mut arch = IsaSet::default();
        let mut set = IsaSet::default();
        let mut named = IsaSet::default();
        for (i, flag) in flags.iter().enumerate() {
            if let Some(cpu) = flag.strip_prefix("-march=") {
                arch = IsaSet::of_arch(cpu);
                continue;
            }
            let Some(edit) = isa_edit(flag) else {
                continue;
            };
            let option = isa_option(flag);
            let overridden = flags[i + 1..]
                .iter()
                .any(|later| is_isa_flag(later) && isa_option(later) == option);
            if overridden {
                continue;
            }
            let exts = match edit {
                IsaEdit::Enable(exts) => {
                    set = set.with(exts);
                    exts
                }
                IsaEdit::Disable(exts) => {
                    set = set.without(exts);
                    exts
                }
            };
            named = named.with(exts);
        }
        set = set.with(arch.without(named));
        if set.has(IsaSet::SSE42) && !named.has(IsaSet::POPCNT) {
            set = set.with(IsaSet::POPCNT);
        }
        Self::from_set(set)
    }

    /// This ISA as the set of extensions it holds.
    fn to_set(self) -> IsaSet {
        let simd = match self.simd {
            X86Simd::Sse2 => IsaSet::default(),
            X86Simd::Sse3 => IsaSet::SSE3,
            X86Simd::Ssse3 => IsaSet::and_below(IsaSet::SSSE3),
            X86Simd::Sse41 => IsaSet::and_below(IsaSet::SSE41),
            X86Simd::Sse42 => IsaSet::and_below(IsaSet::SSE42),
        };
        if self.popcnt {
            simd.with(IsaSet::POPCNT)
        } else {
            simd
        }
    }

    /// The ISA a function with `__attribute__((target(...)))` is compiled
    /// for, when the translation unit's is `self`: an `arch=` adds what
    /// that CPU has, and the feature edits apply on top in order, as the
    /// `-m` options do. Enabling SSE4.2 brings POPCNT unless the request
    /// named POPCNT, as `-msse4.2` does.
    pub fn with_request(self, request: &IsaRequest) -> Self {
        let mut set = self.to_set();
        if let Some(arch) = request.arch {
            set = set.with(arch);
        }
        let mut named = IsaSet::default();
        let mut enabled = IsaSet::default();
        for edit in &request.edits {
            match *edit {
                IsaEdit::Enable(exts) => {
                    set = set.with(exts);
                    enabled = enabled.with(exts);
                    named = named.with(exts);
                }
                IsaEdit::Disable(exts) => {
                    set = set.without(exts);
                    named = named.with(exts);
                }
            }
        }
        let raised_sse42 =
            enabled.has(IsaSet::SSE42) || request.arch.is_some_and(|a| a.has(IsaSet::SSE42));
        if raised_sse42 && set.has(IsaSet::SSE42) && !named.has(IsaSet::POPCNT) {
            set = set.with(IsaSet::POPCNT);
        }
        Self::from_set(set)
    }

    /// Whether code compiled for `other` may run as part of code compiled
    /// for `self`: every extension `other` uses, `self` has. gcc's rule for
    /// inlining one `target` function into another.
    pub fn includes(self, other: Self) -> bool {
        self.simd >= other.simd && (self.popcnt || !other.popcnt)
    }

    /// The level `set` reaches: SSE3 .. SSE4.2, each needing the one before.
    fn from_set(set: IsaSet) -> Self {
        let simd = [
            (IsaSet::SSE42, X86Simd::Sse42),
            (IsaSet::SSE41, X86Simd::Sse41),
            (IsaSet::SSSE3, X86Simd::Ssse3),
            (IsaSet::SSE3, X86Simd::Sse3),
        ]
        .into_iter()
        .find(|&(ext, _)| set.has(IsaSet::and_below(ext)))
        .map_or(X86Simd::Sse2, |(_, level)| level);
        Self {
            simd,
            popcnt: set.has(IsaSet::POPCNT),
        }
    }

    /// The feature macros this level defines, beyond the baseline's.
    pub fn macros(self) -> Vec<&'static str> {
        let mut m = Vec::new();
        for (level, name) in [
            (X86Simd::Sse3, "__SSE3__"),
            (X86Simd::Ssse3, "__SSSE3__"),
            (X86Simd::Sse41, "__SSE4_1__"),
            (X86Simd::Sse42, "__SSE4_2__"),
        ] {
            if self.simd >= level {
                m.push(name);
            }
        }
        if self.popcnt {
            m.push("__POPCNT__");
        }
        m
    }
}

/// What `__attribute__((target("...")))` asks of one function's x86-64
/// ISA, relative to the translation unit's: the extensions an `arch=` CPU
/// has, and the feature edits (`sse4.1`, `no-sse4.2`, `popcnt`) in order.
/// Read from the attribute by `crate::target_attr`; applied by
/// [`X86Isa::with_request`].
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct IsaRequest {
    pub(crate) arch: Option<IsaSet>,
    pub(crate) edits: Vec<IsaEdit>,
}

impl IsaRequest {
    /// `other` asked after this: its CPU's extensions join any this names,
    /// and its edits follow these.
    pub fn extend(&mut self, other: IsaRequest) {
        if let Some(set) = other.arch {
            self.arch = Some(self.arch.map_or(set, |a| a.with(set)));
        }
        self.edits.extend(other.edits);
    }
}

/// Target operating system
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Os {
    Linux,
    MacOS,
    FreeBSD,
}

impl fmt::Display for Os {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Os::Linux => write!(f, "linux"),
            Os::MacOS => write!(f, "macos"),
            Os::FreeBSD => write!(f, "freebsd"),
        }
    }
}

/// Whether plain `char` is a signed or an unsigned type on a target.
///
/// C17 6.2.5p15 leaves the choice to the implementation, and the platform ABIs
/// make it -- per architecture *and* operating system, not per architecture:
///
/// - x86-64 (System V psABI, and Darwin, which follows it): signed.
/// - AAPCS64, which Linux and FreeBSD follow: unsigned.
/// - Apple arm64 departs from AAPCS64 here ("Writing ARM64 code for Apple
///   platforms": "The char type is signed"), so Darwin is signed on both
///   architectures.
///
/// Decided in one place -- [`CharSignedness::of`] -- and carried on
/// [`Target`], because the type system, the predefined macros and the
/// preprocessor's `#if` arithmetic must all give the same answer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CharSignedness {
    Signed,
    Unsigned,
}

impl CharSignedness {
    /// The platform ABI's choice for plain `char`.
    pub fn of(arch: Arch, os: Os) -> Self {
        match (arch, os) {
            (Arch::X86_64, _) => CharSignedness::Signed,
            (Arch::Aarch64, Os::MacOS) => CharSignedness::Signed,
            (Arch::Aarch64, Os::Linux | Os::FreeBSD) => CharSignedness::Unsigned,
        }
    }

    /// The value a `char` object holding the byte `b` has, converted to an
    /// integer: C17 6.4.4.4p10 gives an unprefixed one-character constant
    /// exactly this value, in the compiler and in `#if` alike.
    pub fn byte_value(self, b: u8) -> i64 {
        match self {
            CharSignedness::Signed => b as i8 as i64,
            CharSignedness::Unsigned => b as i64,
        }
    }
}

/// A standard integer type, as a platform ABI names the type behind one of
/// the library's integer typedefs (`int64_t`, `size_t`, `wchar_t`, ...).
///
/// The typedefs are the platform's choice and differ between targets of the
/// same width -- `int64_t` is `long` on glibc and `long long` on Darwin -- so
/// [`Target`] decides each one, in one place, and everything that describes
/// the type is derived from it: the `__*_TYPE__` spelling, its limits, its
/// constant suffix and its `printf` length modifier -- and, for `wchar_t`,
/// `char16_t` and `char32_t`, the type the parser gives a prefixed literal.
/// Hand-typing those per macro is how `__INT64_MAX__` came to say `LL` for a
/// `long`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntType {
    SChar,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LongLong,
    ULongLong,
}

impl IntType {
    pub fn is_signed(self) -> bool {
        matches!(
            self,
            IntType::SChar | IntType::Short | IntType::Int | IntType::Long | IntType::LongLong
        )
    }

    /// The unsigned type of the same rank (C17 6.2.5p6).
    pub fn to_unsigned(self) -> IntType {
        match self {
            IntType::SChar | IntType::UChar => IntType::UChar,
            IntType::Short | IntType::UShort => IntType::UShort,
            IntType::Int | IntType::UInt => IntType::UInt,
            IntType::Long | IntType::ULong => IntType::ULong,
            IntType::LongLong | IntType::ULongLong => IntType::ULongLong,
        }
    }

    /// The signed type of the same rank (C17 6.2.5p6).
    pub fn to_signed(self) -> IntType {
        match self {
            IntType::SChar | IntType::UChar => IntType::SChar,
            IntType::Short | IntType::UShort => IntType::Short,
            IntType::Int | IntType::UInt => IntType::Int,
            IntType::Long | IntType::ULong => IntType::Long,
            IntType::LongLong | IntType::ULongLong => IntType::LongLong,
        }
    }

    /// The type's name as gcc spells it in a `__*_TYPE__` macro.
    pub fn spelling(self) -> &'static str {
        match self {
            IntType::SChar => "signed char",
            IntType::UChar => "unsigned char",
            IntType::Short => "short int",
            IntType::UShort => "short unsigned int",
            IntType::Int => "int",
            IntType::UInt => "unsigned int",
            IntType::Long => "long int",
            IntType::ULong => "long unsigned int",
            IntType::LongLong => "long long int",
            IntType::ULongLong => "long long unsigned int",
        }
    }

    /// The suffix that gives an integer constant this type once promoted --
    /// what `INT64_C(c)` pastes on. A type narrower than `int` has none: no
    /// constant has such a type, and C17 7.20.4p2 wants the promoted one.
    pub fn constant_suffix(self) -> &'static str {
        match self {
            IntType::SChar | IntType::UChar | IntType::Short | IntType::UShort | IntType::Int => "",
            IntType::UInt => "U",
            IntType::Long => "L",
            IntType::ULong => "UL",
            IntType::LongLong => "LL",
            IntType::ULongLong => "ULL",
        }
    }

    /// The `printf` length modifier for the type (C17 7.21.6.1p7).
    pub fn printf_length(self) -> &'static str {
        match self {
            IntType::SChar | IntType::UChar => "hh",
            IntType::Short | IntType::UShort => "h",
            IntType::Int | IntType::UInt => "",
            IntType::Long | IntType::ULong => "l",
            IntType::LongLong | IntType::ULongLong => "ll",
        }
    }
}

/// Whether an atomic object `bytes` wide is always lock-free: exactly the
/// machine integer widths, on every target here. There is no 16-byte atomic,
/// and c17 does not link libatomic's lock-based fallbacks.
///
/// One rule for the three places that answer it: the `__GCC_ATOMIC_*_LOCK_FREE`
/// predefines (and so `<stdatomic.h>`'s `ATOMIC_*_LOCK_FREE`),
/// `__atomic_always_lock_free` / `__atomic_is_lock_free`, and the linearizer's
/// choice between an instruction and a rejection.
pub fn atomic_is_lock_free(bytes: u64) -> bool {
    matches!(bytes, 1 | 2 | 4 | 8)
}

/// What `__atomic_test_and_set` stores into the flag, and so what
/// `__GCC_ATOMIC_TEST_AND_SET_TRUEVAL` says.
pub const ATOMIC_TEST_AND_SET_TRUEVAL: i64 = 1;

/// How a thread-local's address is obtained on a target.
///
/// Decided in one place -- [`Target::tls_access`] -- because two consumers
/// must agree on it: `ir::tls::expand_dynamic_tls`, which makes a call-based
/// computation visible to the register allocator as an explicit `TlsAddr`,
/// and the backends, which emit the sequence for that `TlsAddr`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TlsAccess {
    /// ELF Local Exec / Initial Exec: the offset from the thread pointer is a
    /// link-time or load-time constant, so a backend folds the access into
    /// the load or store itself. No call, nothing for the allocator to see.
    ElfStatic,
    /// ELF TLS descriptor, the dynamic model: the address comes from a call
    /// through a resolver, needed by code that may live in a `dlopen`ed
    /// shared object.
    ElfDescriptor,
    /// Mach-O thread-local variable descriptors. Every access, in every kind
    /// of image, calls the getter the descriptor names; there is no static
    /// model on Darwin.
    MachOTlv,
}

impl TlsAccess {
    /// Whether computing the address is a call, which the IR has to expose
    /// as an explicit `TlsAddr` so the allocator sees what it clobbers.
    pub fn is_call(self) -> bool {
        matches!(self, TlsAccess::ElfDescriptor | TlsAccess::MachOTlv)
    }
}

/// The order in which the bytes of a multi-byte scalar lie in memory: what
/// gcc's `scalar_storage_order` attribute and pragma name.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ByteOrder {
    /// Most significant byte first.
    BigEndian,
    /// Least significant byte first.
    LittleEndian,
}

/// Target configuration
#[derive(Debug, Clone)]
pub struct Target {
    /// CPU architecture
    pub arch: Arch,
    /// Operating system
    pub os: Os,
    /// Pointer size in bits
    pub pointer_width: u32,
    /// Size of long in bits
    pub long_width: u32,
    /// Plain `char`'s signedness, the platform ABI's choice
    pub plain_char: CharSignedness,
    /// Maximum size (in bits) for aggregate types (struct/union) that can be
    /// passed or returned by value in registers. Aggregates larger than this
    /// require indirect passing (pointer) or sret (struct return pointer).
    pub max_aggregate_register_bits: u32,
    /// The x86-64 extensions code may use beyond SSE2, from the `-m` flags:
    /// which packed instructions `arch::simd` lists. The baseline for any
    /// other target, where it means nothing.
    pub x86_isa: X86Isa,
}

impl Target {
    /// Create target for the host system
    pub fn host() -> Self {
        let arch = Self::detect_arch();
        let os = Self::detect_os();

        Self::new(arch, os)
    }

    /// Create target for a specific arch/os combination
    pub fn new(arch: Arch, os: Os) -> Self {
        let pointer_width = 64;

        // LP64 model for Unix-like systems (long and pointer are 64-bit)
        let long_width = match os {
            Os::Linux | Os::MacOS | Os::FreeBSD => 64,
        };

        // Maximum aggregate size that can be returned in registers.
        // Both x86-64 SysV ABI and AAPCS64 support returning 16-byte structs
        // in two registers (rax+rdx or x0+x1). Structs larger than 16 bytes
        // use sret (hidden pointer parameter).
        let max_aggregate_register_bits = 128;

        Self {
            arch,
            os,
            pointer_width,
            long_width,
            plain_char: CharSignedness::of(arch, os),
            max_aggregate_register_bits,
            x86_isa: X86Isa::default(),
        }
    }

    /// The type behind `int64_t`: `long` or `long long`?
    ///
    /// Every target here is LP64, so `long` is 64 bits and either spelling is
    /// wide enough — but they are *distinct types*, and our `<stdint.h>` has to
    /// name the same one the host's headers do or a translation unit including
    /// both is rejected. Linux and the BSDs use `long`; Darwin uses
    /// `long long`, and picking by width alone made every macOS build that
    /// reached a system header fail on `int64_t`.
    fn int64_type(&self) -> IntType {
        match self.os {
            Os::MacOS => IntType::LongLong,
            Os::Linux | Os::FreeBSD => IntType::Long,
        }
    }

    /// Whether a multi-byte integer lies in this target's memory lowest
    /// byte first. Every target here is little-endian; the host running the
    /// compiler need not be, so what is stored is laid out by this, never
    /// by the host's own order.
    pub fn little_endian(&self) -> bool {
        match self.arch {
            Arch::X86_64 | Arch::Aarch64 => true,
        }
    }

    /// The order this target stores the bytes of a scalar in.
    pub fn byte_order(&self) -> ByteOrder {
        if self.little_endian() {
            ByteOrder::LittleEndian
        } else {
            ByteOrder::BigEndian
        }
    }

    /// The prefix that makes an assembler symbol private: a label the
    /// assembler resolves itself and never writes to the symbol table.
    ///
    /// ELF assemblers treat `.L` that way. Mach-O's treats only `L` (and
    /// `l`), so on Mach-O a `.L` name is an ordinary symbol, and an ordinary
    /// symbol in `__text` starts a new atom: a CFI advance across a block
    /// label stops being a constant, and Apple's assembler rejects it
    /// ("invalid CFI advance_loc expression").
    pub fn private_label_prefix(&self) -> &'static str {
        match self.os {
            Os::MacOS => "L",
            Os::Linux | Os::FreeBSD => ".L",
        }
    }

    /// The width in bits of an integer type on this target.
    pub fn int_width(&self, t: IntType) -> u32 {
        match t {
            IntType::SChar | IntType::UChar => 8,
            IntType::Short | IntType::UShort => 16,
            IntType::Int | IntType::UInt => 32,
            IntType::Long | IntType::ULong => self.long_width,
            IntType::LongLong | IntType::ULongLong => 64,
        }
    }

    /// The largest value of an integer type on this target.
    pub fn int_max(&self, t: IntType) -> u64 {
        let bits = self.int_width(t) - u32::from(t.is_signed());
        u64::MAX >> (64 - bits)
    }

    /// `intN_t`, for N of 8, 16, 32 and 64 (C17 7.20.1.1).
    pub fn exact_int_type(&self, bits: u32) -> IntType {
        match bits {
            8 => IntType::SChar,
            16 => IntType::Short,
            32 => IntType::Int,
            64 => self.int64_type(),
            _ => panic!("no int{bits}_t"),
        }
    }

    /// `int_leastN_t` (C17 7.20.1.2): the exact-width type on every target,
    /// since every target has all four.
    pub fn least_int_type(&self, bits: u32) -> IntType {
        self.exact_int_type(bits)
    }

    /// `int_fastN_t` (C17 7.20.1.3), which each C library chooses for itself
    /// and our `<stdint.h>` has to choose the same way:
    ///
    /// - glibc: `signed char` for 8, and `long` -- the word -- for 16, 32
    ///   and 64 on a 64-bit target (`__WORDSIZE == 64` in its `<stdint.h>`).
    /// - FreeBSD: `int` for 8, 16 and 32 (`__int_fast*_t` in
    ///   `<machine/_types.h>`), `int64_t` for 64.
    /// - Darwin: the exact-width type at every width.
    ///
    /// Answering the exact-width type everywhere made `int_fast16_t` a
    /// 2-byte `short` in c17 and an 8-byte `long` in gcc on Linux -- a
    /// different size for the same type in any structure or prototype the
    /// two share.
    pub fn fast_int_type(&self, bits: u32) -> IntType {
        match (self.os, bits) {
            (Os::Linux, 16 | 32) => IntType::Long,
            (Os::FreeBSD, 8 | 16) => IntType::Int,
            _ => self.exact_int_type(bits),
        }
    }

    /// `intptr_t` (C17 7.20.1.4): `long` on every LP64 target, Darwin
    /// included.
    pub fn intptr_type(&self) -> IntType {
        IntType::Long
    }

    /// `ptrdiff_t` (C17 7.19), the type of a pointer difference.
    pub fn ptrdiff_type(&self) -> IntType {
        IntType::Long
    }

    /// `intmax_t` (C17 7.20.1.5). `long` everywhere -- on Darwin too, where
    /// `int64_t` is `long long` but `intmax_t` is not.
    pub fn intmax_type(&self) -> IntType {
        IntType::Long
    }

    /// `size_t` (C17 7.19).
    pub fn size_type(&self) -> IntType {
        IntType::ULong
    }

    /// `wchar_t` (C17 7.19), the type of a wide character constant and the
    /// element type of a wide string literal (6.4.4.4p10, 6.4.5p6).
    ///
    /// AAPCS64 makes it `unsigned int` (its "Arm C and C++ language
    /// mappings"), and Linux and FreeBSD follow it; Apple arm64 departs and
    /// makes it `int`, as x86-64 is everywhere. The type system, the
    /// predefines and `#if` all read this answer, so `L'\xffffffff' > 0`,
    /// `(wchar_t)-1 > 0` and `WCHAR_MIN == 0` agree with gcc on aarch64
    /// Linux.
    pub fn wchar_type(&self) -> IntType {
        match (self.arch, self.os) {
            (Arch::Aarch64, Os::Linux | Os::FreeBSD) => IntType::UInt,
            (Arch::Aarch64, Os::MacOS) | (Arch::X86_64, _) => IntType::Int,
        }
    }

    /// `wint_t` (C17 7.29.1), which has to hold every `wchar_t` value *plus*
    /// `WEOF`, and the platforms solve that differently. glibc makes it
    /// `unsigned int`, so `WEOF` -- `(wint_t)-1` -- is `0xffffffff`, a value
    /// no `wchar_t` reaches. Darwin and FreeBSD make it `int`, following
    /// their `__ct_rune_t`, and spend the negative half of the range
    /// instead.
    ///
    /// It has to be the platform's choice rather than ours: the C library's
    /// own headers typedef `wint_t` from their own definition, and
    /// `__mbstate_t` holds one.
    pub fn wint_type(&self) -> IntType {
        match self.os {
            Os::MacOS | Os::FreeBSD => IntType::Int,
            Os::Linux => IntType::UInt,
        }
    }

    /// `char16_t` (C17 7.28): `uint_least16_t`.
    pub fn char16_type(&self) -> IntType {
        self.least_int_type(16).to_unsigned()
    }

    /// `char32_t` (C17 7.28): `uint_least32_t`.
    pub fn char32_type(&self) -> IntType {
        self.least_int_type(32).to_unsigned()
    }

    /// `sig_atomic_t` (C17 7.14).
    pub fn sig_atomic_type(&self) -> IntType {
        IntType::Int
    }

    /// Detect host architecture at runtime
    fn detect_arch() -> Arch {
        #[cfg(target_arch = "x86_64")]
        {
            Arch::X86_64
        }
        #[cfg(target_arch = "aarch64")]
        {
            Arch::Aarch64
        }
        #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
        {
            // Default to x86_64 for unknown architectures
            Arch::X86_64
        }
    }

    /// Detect host OS at runtime
    fn detect_os() -> Os {
        #[cfg(target_os = "linux")]
        {
            Os::Linux
        }
        #[cfg(target_os = "macos")]
        {
            Os::MacOS
        }
        #[cfg(target_os = "freebsd")]
        {
            Os::FreeBSD
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "freebsd")))]
        {
            // Default to Linux for unknown OS
            Os::Linux
        }
    }
}

impl Default for Target {
    fn default() -> Self {
        Self::host()
    }
}

impl Target {
    /// How this target obtains a thread-local's address. `shared_mode` is
    /// set for `-shared` and `-fPIC`: code that may be `dlopen`ed.
    ///
    /// ELF (Linux, FreeBSD) folds Local and Initial Exec into the access and
    /// needs a descriptor call only for shared code, and only Linux takes the
    /// descriptor model here: FreeBSD's shared code uses Initial Exec, never
    /// Local Exec (see `CodeGenBase::use_tls_ie`). Mach-O always calls the TLV
    /// getter.
    pub fn tls_access(&self, shared_mode: bool) -> TlsAccess {
        match self.os {
            Os::MacOS => TlsAccess::MachOTlv,
            Os::Linux if shared_mode => TlsAccess::ElfDescriptor,
            Os::Linux | Os::FreeBSD => TlsAccess::ElfStatic,
        }
    }

    /// Which `__div?c3` this target ships, in `routine`'s format.
    ///
    /// Linux ships libgcc's, whose Smith's-method steps fuse on aarch64 at
    /// the two formats that have an `fmadd`; every other target here ships
    /// compiler-rt's, which scales the divisor by a power of two taken from
    /// `logb` of its larger half instead. The two differ in the last place
    /// for operands neither has to scale, so a constant folded as one
    /// divides disagrees with the other's run-time answer -- which is what
    /// `(0.1 + 0.7i) / (0.3 + 0.9i)` did on Darwin, folded as libgcc divides
    /// and computed by the `__divdc3` beside it.
    ///
    /// Apple is measured; FreeBSD is taken to be compiler-rt's because it
    /// builds with clang and ships compiler-rt, and is not tested here.
    pub fn complex_division(&self, routine: ComplexRoutineFormat) -> ComplexDivision {
        // Both libraries write each sum's two products inside the expression
        // that adds them, so a target with an `fmadd` contracts one of them.
        // aarch64 has one at the two formats it computes in hardware;
        // binary128 is software and fuses nothing, and x86-64 has no `fma` at
        // the baseline.
        let contraction = match (self.arch, routine) {
            (Arch::Aarch64, ComplexRoutineFormat::Binary32 | ComplexRoutineFormat::Binary64) => {
                Contraction::Fused
            }
            _ => Contraction::Separate,
        };
        if self.os != Os::Linux {
            return ComplexDivision::CompilerRt(contraction);
        }
        ComplexDivision::Libgcc(contraction)
    }
}

impl Target {
    /// Parse a target triple (e.g., "aarch64-apple-darwin", "x86_64-unknown-linux-gnu")
    pub fn from_triple(triple: &str) -> Option<Self> {
        let parts: Vec<&str> = triple.split('-').collect();
        if parts.is_empty() {
            return None;
        }

        let arch = match parts[0] {
            "x86_64" => Arch::X86_64,
            "aarch64" | "arm64" => Arch::Aarch64,
            _ => return None,
        };

        // Detect OS from triple (second or third part typically)
        let os = if triple.contains("linux") {
            Os::Linux
        } else if triple.contains("darwin") || triple.contains("macos") || triple.contains("apple")
        {
            Os::MacOS
        } else if triple.contains("freebsd") {
            Os::FreeBSD
        } else if parts.len() == 1 {
            // A bare architecture means the usual system for it.
            Os::Linux
        } else {
            // An operating system c17 has no support for -- Windows, another
            // BSD, bare metal. Taking it for Linux defined `__linux__` and
            // `__ELF__` for a target that is neither.
            return None;
        };

        Some(Self::new(arch, os))
    }

    /// The triple gcc's `-dumpmachine` prints for this target.
    ///
    /// Debian's spelling on Linux, which is also the multiarch directory
    /// name; Apple's `arm64` on macOS, where the system compiler is clang. No
    /// OS release is appended: c17 does not target one release over another.
    /// Each round-trips through [`Target::from_triple`].
    pub fn gcc_triple(&self) -> &'static str {
        match (self.arch, self.os) {
            (Arch::X86_64, Os::Linux) => "x86_64-linux-gnu",
            (Arch::Aarch64, Os::Linux) => "aarch64-linux-gnu",
            (Arch::X86_64, Os::MacOS) => "x86_64-apple-darwin",
            (Arch::Aarch64, Os::MacOS) => "arm64-apple-darwin",
            (Arch::X86_64, Os::FreeBSD) => "x86_64-unknown-freebsd",
            (Arch::Aarch64, Os::FreeBSD) => "aarch64-unknown-freebsd",
        }
    }

    /// The multiarch tuple `-print-multiarch` prints: Debian's library
    /// directory name, which only Linux has.
    pub fn multiarch(&self) -> Option<&'static str> {
        (self.os == Os::Linux).then(|| self.gcc_triple())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every gcc triple names its own target again, and multiarch is the
    /// triple on Linux and nothing elsewhere.
    #[test]
    fn test_gcc_triple_round_trips() {
        for arch in [Arch::X86_64, Arch::Aarch64] {
            for os in [Os::Linux, Os::MacOS, Os::FreeBSD] {
                let t = Target::new(arch, os);
                let back = Target::from_triple(t.gcc_triple()).expect("parses");
                assert_eq!((back.arch, back.os), (arch, os), "{}", t.gcc_triple());
                assert_eq!(t.multiarch().is_some(), os == Os::Linux);
            }
        }
        assert_eq!(
            Target::new(Arch::Aarch64, Os::Linux).multiarch(),
            Some("aarch64-linux-gnu")
        );
    }

    /// Each level implies the ones below it, enabling options accumulate, and
    /// gcc's -msse4.2 brings POPCNT with it.
    #[test]
    fn test_x86_isa_from_flags() {
        let isa = |flags: &[&str]| {
            let owned: Vec<String> = flags.iter().map(|f| f.to_string()).collect();
            X86Isa::from_flags(&owned)
        };
        assert_eq!(isa(&[]), X86Isa::default());
        assert_eq!(isa(&["-msse4.1", "-msse3"]).simd, X86Simd::Sse41);
        assert!(!isa(&["-msse4.1"]).popcnt);
        assert_eq!(
            isa(&["-msse4.2"]),
            X86Isa {
                simd: X86Simd::Sse42,
                popcnt: true
            }
        );
        assert_eq!(isa(&["-march=x86-64-v2"]), isa(&["-msse4.2"]));
        assert_eq!(isa(&["-msse3"]).macros(), vec!["__SSE3__"]);
        assert_eq!(isa(&["-mpopcnt"]).macros(), vec!["__POPCNT__"]);
    }

    /// The last `-march=` sets the base and the `-m` options, wherever they
    /// stand, apply on top in order, as gcc 13's `-dM -E` reports.
    #[test]
    fn test_x86_isa_flag_order_matches_gcc() {
        const ALL: &str = "SSE3 SSSE3 SSE4_1 SSE4_2 POPCNT";
        let cases: &[(&str, &str)] = &[
            ("-march=x86-64-v2 -march=x86-64", ""),
            ("-march=x86-64 -march=x86-64-v2", ALL),
            ("-march=haswell -march=x86-64", ""),
            ("-msse4.2 -march=x86-64", ALL),
            ("-march=x86-64-v2 -mno-sse4.2", "SSE3 SSSE3 SSE4_1 POPCNT"),
            ("-mno-sse4.2 -march=x86-64-v2", "SSE3 SSSE3 SSE4_1 POPCNT"),
            ("-mno-sse4.2 -march=haswell", "SSE3 SSSE3 SSE4_1 POPCNT"),
            ("-march=x86-64-v2 -mno-sse4.1", "SSE3 SSSE3 POPCNT"),
            ("-march=x86-64-v2 -mno-sse4", "SSE3 SSSE3 POPCNT"),
            ("-march=x86-64-v2 -mno-ssse3", "SSE3 POPCNT"),
            ("-march=x86-64-v2 -mno-sse3", "POPCNT"),
            ("-march=x86-64-v2 -mno-popcnt", "SSE3 SSSE3 SSE4_1 SSE4_2"),
            ("-mssse3 -march=x86-64-v2 -mno-sse3", "POPCNT"),
            ("-msse4.2 -march=x86-64-v2 -mno-sse4.1", "SSE3 SSSE3 POPCNT"),
            (
                "-march=x86-64-v2 -msse4.2 -mno-sse4.2",
                "SSE3 SSSE3 SSE4_1 POPCNT",
            ),
            ("-msse4.2 -mno-sse4.1", "SSE3 SSSE3"),
            ("-mno-sse4.1 -msse4.2", ALL),
            ("-msse4.2 -mno-sse4.2", ""),
            ("-msse4.2 -msse3 -mno-sse4.2", "SSE3"),
            ("-msse4.2 -mno-sse3 -mssse3", "SSE3 SSSE3"),
            ("-msse4 -mno-sse4", "SSE3 SSSE3"),
            ("-msse4 -mno-sse4.2", "SSE3 SSSE3 SSE4_1"),
            ("-msse4.2 -mno-popcnt", "SSE3 SSSE3 SSE4_1 SSE4_2"),
            ("-mno-popcnt -msse4.2", "SSE3 SSSE3 SSE4_1 SSE4_2"),
            ("-mpopcnt -mno-sse4.2", "POPCNT"),
            ("-msse4.2 -mpopcnt -mno-sse4.2", "POPCNT"),
            ("-msse4.1 -mno-ssse3", "SSE3"),
            ("-mno-ssse3 -msse4.1", "SSE3 SSSE3 SSE4_1"),
            ("-march=k8", ""),
            ("-march=nocona", "SSE3"),
            ("-march=core2", "SSE3 SSSE3"),
            ("-march=barcelona", "SSE3 POPCNT"),
            ("-march=btver1", "SSE3 SSSE3 POPCNT"),
            ("-march=nano-x2", "SSE3 SSSE3 SSE4_1"),
            ("-march=znver4", ALL),
            ("-march=x86-64-v4", ALL),
        ];
        for (flags, gcc) in cases {
            let owned: Vec<String> = flags.split(' ').map(String::from).collect();
            let got: Vec<String> = X86Isa::from_flags(&owned)
                .macros()
                .iter()
                .map(|m| m.trim_matches('_').to_string())
                .collect();
            assert_eq!(got.join(" "), *gcc, "{flags}");
        }
    }

    /// A triple names an OS c17 supports, or it is refused; a bare
    /// architecture is the usual system for it.
    #[test]
    fn test_from_triple_refuses_unknown_systems() {
        assert_eq!(
            Target::from_triple("x86_64-unknown-linux-gnu").map(|t| t.os),
            Some(Os::Linux)
        );
        assert_eq!(
            Target::from_triple("aarch64-apple-darwin").map(|t| t.os),
            Some(Os::MacOS)
        );
        assert_eq!(
            Target::from_triple("x86_64-unknown-freebsd").map(|t| t.os),
            Some(Os::FreeBSD)
        );
        assert_eq!(
            Target::from_triple("aarch64").map(|t| t.os),
            Some(Os::Linux)
        );
        for triple in [
            "x86_64-pc-windows-msvc",
            "x86_64-w64-mingw32",
            "aarch64-unknown-none",
        ] {
            assert!(Target::from_triple(triple).is_none(), "{triple}");
        }
    }

    /// `wint_t` is the C library's: `unsigned int` for glibc, `int` for
    /// Darwin and FreeBSD.
    #[test]
    fn test_wint_type_per_os() {
        for arch in [Arch::X86_64, Arch::Aarch64] {
            assert_eq!(Target::new(arch, Os::Linux).wint_type(), IntType::UInt);
            assert_eq!(Target::new(arch, Os::MacOS).wint_type(), IntType::Int);
            assert_eq!(Target::new(arch, Os::FreeBSD).wint_type(), IntType::Int);
        }
    }

    /// Mach-O always calls the TLV getter; ELF calls only for shared code,
    /// and only Linux takes the descriptor model; FreeBSD is ELF too.
    #[test]
    fn test_tls_access_per_target() {
        for arch in [Arch::X86_64, Arch::Aarch64] {
            let mac = Target::new(arch, Os::MacOS);
            assert_eq!(mac.tls_access(false), TlsAccess::MachOTlv);
            assert_eq!(mac.tls_access(true), TlsAccess::MachOTlv);
            let linux = Target::new(arch, Os::Linux);
            assert_eq!(linux.tls_access(false), TlsAccess::ElfStatic);
            assert_eq!(linux.tls_access(true), TlsAccess::ElfDescriptor);
            let bsd = Target::new(arch, Os::FreeBSD);
            assert_eq!(bsd.tls_access(false), TlsAccess::ElfStatic);
        }
        assert!(TlsAccess::MachOTlv.is_call() && TlsAccess::ElfDescriptor.is_call());
        assert!(!TlsAccess::ElfStatic.is_call());
    }

    #[test]
    fn test_host_target() {
        let target = Target::host();
        // Basic sanity checks
        assert_eq!(target.pointer_width, 64);
    }

    #[test]
    fn test_x86_64_linux() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        assert_eq!(target.arch, Arch::X86_64);
        assert_eq!(target.os, Os::Linux);
        assert_eq!(target.pointer_width, 64);
        assert_eq!(target.long_width, 64); // LP64
    }

    /// Every accepted spelling is classified; the `c`/`gnu` prefix does not
    /// change the answer, because it selects nothing.
    #[test]
    fn test_classify_std_spellings() {
        for spec in [
            "c17",
            "c18",
            "gnu17",
            "gnu18",
            "iso9899:2017",
            "iso9899:2018",
        ] {
            assert_eq!(classify_std(spec), Some(StdRequest::C17), "{spec}");
        }

        for spec in [
            "c89",
            "c90",
            "c9x",
            "c99",
            "c1x",
            "c11",
            "gnu89",
            "gnu90",
            "gnu9x",
            "gnu99",
            "gnu1x",
            "gnu11",
            "iso9899:1990",
            "iso9899:199409",
            "iso9899:199x",
            "iso9899:1999",
            "iso9899:2011",
        ] {
            assert_eq!(classify_std(spec), Some(StdRequest::Older), "{spec}");
        }
    }

    /// A revision name belongs to its prefix. Mixing them is a typo, and gcc
    /// rejects each of these too.
    #[test]
    fn test_classify_std_does_not_mix_prefix_and_revision_forms() {
        for spec in [
            "c1990",
            "c199409",
            "c1999",
            "c2011",
            "c2017",
            "gnu1990",
            "gnu1999",
            "iso9899:89",
            "iso9899:90",
            "iso9899:99",
            "iso9899:11",
            "iso9899:17",
        ] {
            assert!(classify_std(spec).is_none(), "{spec} should be rejected");
        }
    }

    #[test]
    fn test_classify_std_rejects_unknown() {
        for spec in ["c42", "gnu42", "c++17", "", "iso9899:1234", "nonsense"] {
            assert!(classify_std(spec).is_none(), "{spec} should be rejected");
        }
    }

    #[test]
    fn test_aarch64_linux() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        assert_eq!(target.arch, Arch::Aarch64);
        assert_eq!(target.os, Os::Linux);
        assert_eq!(target.pointer_width, 64);
    }

    /// Plain `char` follows the platform ABI, which is a property of the
    /// architecture *and* the OS: Apple arm64 is signed where AAPCS64 is not.
    #[test]
    fn test_plain_char_signedness_per_target() {
        use CharSignedness::{Signed, Unsigned};
        for (arch, os, want) in [
            (Arch::X86_64, Os::Linux, Signed),
            (Arch::X86_64, Os::MacOS, Signed),
            (Arch::X86_64, Os::FreeBSD, Signed),
            (Arch::Aarch64, Os::Linux, Unsigned),
            (Arch::Aarch64, Os::FreeBSD, Unsigned),
            (Arch::Aarch64, Os::MacOS, Signed),
        ] {
            assert_eq!(Target::new(arch, os).plain_char, want, "{arch}-{os}");
        }
    }

    /// `wchar_t` is unsigned under AAPCS64, which Linux and FreeBSD follow,
    /// and signed on Apple arm64 and on x86-64.
    #[test]
    fn test_wchar_type_per_target() {
        for (arch, os, want) in [
            (Arch::X86_64, Os::Linux, IntType::Int),
            (Arch::X86_64, Os::MacOS, IntType::Int),
            (Arch::X86_64, Os::FreeBSD, IntType::Int),
            (Arch::Aarch64, Os::Linux, IntType::UInt),
            (Arch::Aarch64, Os::FreeBSD, IntType::UInt),
            (Arch::Aarch64, Os::MacOS, IntType::Int),
        ] {
            assert_eq!(Target::new(arch, os).wchar_type(), want, "{arch}-{os}");
        }
    }

    /// `int_fastN_t` is each C library's own choice, and differs at 16 and
    /// 32 bits between all three.
    #[test]
    fn test_fast_int_types_per_platform() {
        use IntType::*;
        for arch in [Arch::X86_64, Arch::Aarch64] {
            for (os, want) in [
                (Os::Linux, [SChar, Long, Long, Long]),
                (Os::FreeBSD, [Int, Int, Int, Long]),
                (Os::MacOS, [SChar, Short, Int, LongLong]),
            ] {
                let t = Target::new(arch, os);
                let got = [8, 16, 32, 64].map(|bits| t.fast_int_type(bits));
                assert_eq!(got, want, "{arch}-{os}");
                for (bits, ty) in [8, 16, 32, 64].into_iter().zip(got) {
                    assert!(t.int_width(ty) >= bits && ty.is_signed());
                }
            }
        }
    }

    #[test]
    fn test_char_byte_value() {
        assert_eq!(CharSignedness::Signed.byte_value(0x80), -128);
        assert_eq!(CharSignedness::Signed.byte_value(0xff), -1);
        assert_eq!(CharSignedness::Signed.byte_value(0x7f), 127);
        assert_eq!(CharSignedness::Unsigned.byte_value(0x80), 128);
        assert_eq!(CharSignedness::Unsigned.byte_value(0xff), 255);
        assert_eq!(CharSignedness::Unsigned.byte_value(0x7f), 127);
    }
}
