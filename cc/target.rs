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
}

#[cfg(test)]
mod tests {
    use super::*;

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
