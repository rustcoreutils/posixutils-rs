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
    /// C17, or C99 or C11 (`c99`, `c11`, `c17`, their `gnu*` and `iso9899:`
    /// equivalents and draft names): a program in any of them is a C17
    /// program, so it passes without comment.
    Compiled,
    /// C90 (`c89`, `c90`, `gnu89`, `gnu90`, `iso9899:1990`, `iso9899:199409`),
    /// whose implicit `int` and implicit declarations C17 refuses. Accepted
    /// and compiled as C17; the driver says so. A revision after C17 is not
    /// accepted at all: c17 cannot compile it, and a configure probe for it
    /// must be told no.
    Ignored,
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
            "199x" | "1999" | "2011" | "2017" | "2018" => Some(StdRequest::Compiled),
            "1990" | "199409" => Some(StdRequest::Ignored),
            _ => None,
        };
    }

    let rev = spec
        .strip_prefix("gnu")
        .or_else(|| spec.strip_prefix('c'))?;
    match rev {
        "9x" | "99" | "1x" | "11" | "17" | "18" => Some(StdRequest::Compiled),
        "89" | "90" => Some(StdRequest::Ignored),
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

/// Which spelling of a position-independence option asked for it: gcc's
/// `-fpic`/`-fpie` (1 in `__PIC__`) or `-fPIC`/`-fPIE` (2).
///
/// c17 generates the same code for both -- the unrestricted GOT access of the
/// upper-case spelling, which is valid wherever the lower-case one is -- so
/// the level is only ever what the macros report.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PicLevel {
    Small,
    Large,
}

impl PicLevel {
    /// The value of `__PIC__` and `__PIE__`.
    pub fn macro_value(self) -> &'static str {
        match self {
            PicLevel::Small => "1",
            PicLevel::Large => "2",
        }
    }
}

/// The position independence code is generated with: what gcc's `-fpic`
/// family selects.
///
/// `-fpic`, `-fPIC`, `-fpie`, `-fPIE`, `-fno-pic`, `-fno-PIC`, `-fno-pie` and
/// `-fno-PIE` are one option to gcc: the last one given wins, and every
/// `-fno-` spelling means position-dependent code, PIE included. The link
/// options (`-pie`, `-no-pie`, `-static-pie`, `-shared`) are not members.
///
/// One value drives both code generation and the `__PIC__`/`__PIE__` macros
/// that describe it, so a header that tests the macro sees the code it gets.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum PositionIndependence {
    /// Position-dependent code: `-fno-pic` and the other `-fno-` spellings.
    #[default]
    Absolute,
    /// Code that can go in a shared object: `-fpic`, `-fPIC`.
    Pic(PicLevel),
    /// Code for a position-independent executable: `-fpie`, `-fPIE`.
    Pie(PicLevel),
}

impl PositionIndependence {
    /// What one member of the `-fpic` family asks for, or `None` for an
    /// option outside it.
    pub fn from_flag(flag: &str) -> Option<Self> {
        Some(match flag {
            "-fpic" => Self::Pic(PicLevel::Small),
            "-fPIC" => Self::Pic(PicLevel::Large),
            "-fpie" => Self::Pie(PicLevel::Small),
            "-fPIE" => Self::Pie(PicLevel::Large),
            "-fno-pic" | "-fno-PIC" | "-fno-pie" | "-fno-PIE" => Self::Absolute,
            _ => return None,
        })
    }

    /// What a compilation gets when no member of the family is given: a
    /// large-model PIE on Linux, as Debian's gcc is configured
    /// (`--enable-default-pie`), and position-dependent code elsewhere --
    /// Mach-O code is position independent whatever it is told, see
    /// [`Self::macro_level`].
    pub fn target_default(target: &Target) -> Self {
        match target.os {
            Os::Linux => Self::Pie(PicLevel::Large),
            Os::MacOS | Os::FreeBSD => Self::Absolute,
        }
    }

    /// Whether the code is position independent.
    pub fn is_pic(self) -> bool {
        self != Self::Absolute
    }

    /// Whether the code is for a position-independent executable.
    pub fn is_pie(self) -> bool {
        matches!(self, Self::Pie(_))
    }

    /// Whether the code was asked to be able to live in a shared object
    /// (`-fpic`, `-fPIC`), which is what gcc calls `flag_shlib`. A PIE is
    /// position independent yet still resolves its own symbols, thread-locals
    /// included, at link time.
    pub fn is_shared_code(self) -> bool {
        matches!(self, Self::Pic(_))
    }

    /// The value `__PIC__` is defined to, or `None` to leave it undefined.
    /// Mach-O code is always position independent, and clang defines
    /// `__PIC__` as 2 there whatever was asked.
    pub fn macro_level(self, target: &Target) -> Option<PicLevel> {
        match self {
            Self::Pic(level) | Self::Pie(level) => Some(level),
            Self::Absolute if target.os == Os::MacOS => Some(PicLevel::Large),
            Self::Absolute => None,
        }
    }
}

/// How the object file lays out and links to its symbols: gcc's
/// `-ffunction-sections`, `-fdata-sections`, `-fcommon` and `-fno-plt`.
/// ELF only; a Mach-O object places every symbol in its own atom anyway,
/// and clang ignores these there.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ObjectLayout {
    /// `-ffunction-sections`: each function in `.text.NAME`.
    pub function_sections: bool,
    /// `-fdata-sections`: each object in `.data.NAME`, `.bss.NAME`,
    /// `.rodata.NAME` and the like.
    pub data_sections: bool,
    /// `-fcommon`: a tentative definition is a common symbol.
    pub common: bool,
    /// Not `-fno-plt`: a call to a function that may be defined elsewhere
    /// goes through a PLT stub, rather than loading its GOT entry.
    pub plt: bool,
}

impl Default for ObjectLayout {
    fn default() -> Self {
        Self {
            function_sections: false,
            data_sections: false,
            common: false,
            plt: true,
        }
    }
}

/// One `.p2align` gcc puts before code it aligns: to `1 << log` bytes,
/// skipping at most `max_skip` bytes of padding to get there.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AlignLevel {
    pub log: u32,
    pub max_skip: u32,
}

impl AlignLevel {
    /// `n` bytes, padding at most `m - 1` (`m` = `n` when not given), as
    /// gcc's `parse_N_M` reads them: `n` rounded up to a power of two.
    fn of(n: u32, m: Option<u32>) -> Self {
        let log = (2 * n.max(1) - 1).ilog2();
        let full = (1 << log) - 1;
        let max_skip = m.unwrap_or(n).saturating_sub(1).min(full);
        Self { log, max_skip }
    }

    /// Whether the padding is limited: gcc's `ASM_OUTPUT_MAX_SKIP_ALIGN`
    /// writes the limit only then, a limit of 0 meaning none.
    pub fn limited(self) -> bool {
        self.max_skip != 0 && self.max_skip < (1 << self.log) - 1
    }
}

/// gcc's `-falign-functions=N[:M[:N2[:M2]]]`: what precedes each function,
/// as gcc's `final` writes it -- the first alignment, and when that one is
/// limited, the second.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AlignSpec {
    first: AlignLevel,
    second: Option<AlignLevel>,
}

impl AlignSpec {
    /// gcc's largest alignment value.
    const MAX: u32 = 65536;

    /// `value` (`""` for the bare option) for `-f<option>=`, on `arch`;
    /// `Err` is gcc's message. A value of 0, and the bare option, is the
    /// target's default: 16 bytes on x86-64, and on aarch64 16 with at most
    /// 11 bytes of padding.
    pub fn parse(option: &str, value: &str, arch: Arch) -> Result<Self, String> {
        let invalid = || format!("invalid arguments for '-f{option}' option: '{value}'");
        let mut numbers = Vec::new();
        if !value.is_empty() {
            for part in value.split(':') {
                let n: u64 = part.parse().map_err(|_| invalid())?;
                if n > u64::from(Self::MAX) {
                    return Err(format!("'-f{option}' is not between 0 and {}", Self::MAX));
                }
                numbers.push(n as u32);
            }
            if numbers.len() > 4 {
                return Err(invalid());
            }
        }
        if numbers.first().is_none_or(|&n| n == 0) {
            numbers = match arch {
                Arch::X86_64 => vec![16],
                Arch::Aarch64 => vec![16, 12],
            };
        }
        let first = AlignLevel::of(numbers[0], numbers.get(1).copied());
        let second = match numbers.get(2) {
            Some(&n2) => Some(AlignLevel::of(n2, numbers.get(3).copied())),
            // x86's old secondary alignment, which gcc keeps when N2 is not
            // given: to 8 bytes, after an alignment above it.
            None if arch == Arch::X86_64 && first.log > 3 => Some(AlignLevel::of(8, None)),
            None => None,
        };
        Ok(Self { first, second })
    }

    /// No alignment: `-fno-align-functions`, or a value of 1.
    pub fn none() -> Self {
        Self {
            first: AlignLevel::of(1, None),
            second: None,
        }
    }

    /// The `.p2align`s, in order.
    pub fn levels(self) -> Vec<AlignLevel> {
        if self.first.log == 0 {
            return Vec::new();
        }
        let mut levels = vec![self.first];
        if self.first.limited() {
            levels.extend(self.second.filter(|l| l.log != 0));
        }
        levels
    }
}

/// `-fpatchable-function-entry=N,M`: `N` nops at each function's entry,
/// `M` of them before its label, recorded in `__patchable_function_entries`
/// for a tracer to patch.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PatchableEntry {
    pub total: u32,
    pub before: u32,
}

/// `-fzero-call-used-regs=`: which call-clobbered registers a function
/// zeroes before it returns, so that what it computed does not survive into
/// its caller for a gadget to use. gcc 13's choices; `leafy` is gcc 14's.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ZeroRegs {
    /// Any register at all; `skip` is none.
    pub on: bool,
    /// Every one (`all`), rather than only those the function uses
    /// (`used`).
    pub all: bool,
    /// General registers only (`-gpr`).
    pub gpr_only: bool,
    /// Argument registers only (`-arg`).
    pub arg_only: bool,
}

impl ZeroRegs {
    /// gcc 13's names, in its order.
    pub const NAMES: [&'static str; 9] = [
        "skip",
        "used-gpr-arg",
        "used-gpr",
        "used-arg",
        "used",
        "all-gpr-arg",
        "all-gpr",
        "all-arg",
        "all",
    ];

    /// The choice `name` names, if it is one of [`Self::NAMES`].
    pub fn from_name(name: &str) -> Option<Self> {
        if !Self::NAMES.contains(&name) {
            return None;
        }
        if name == "skip" {
            return Some(Self::default());
        }
        let (scope, rest) = name.split_once('-').unwrap_or((name, ""));
        Some(Self {
            on: true,
            all: scope == "all",
            gpr_only: rest.starts_with("gpr"),
            arg_only: rest.ends_with("arg"),
        })
    }
}

/// `-ftrivial-auto-var-init=`: what an automatic variable without an
/// initializer starts as.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum AutoVarInit {
    /// Whatever its storage held: C's own rule.
    #[default]
    Uninitialized,
    /// All bytes zero.
    Zero,
    /// gcc's repeated byte 0xFE, which makes an integer large, a pointer
    /// invalid and a floating value a NaN.
    Pattern,
}

/// How each function begins and ends: gcc's `-falign-functions=`,
/// `-fmin-function-alignment=`, `-fpatchable-function-entry=` and
/// `-fzero-call-used-regs=`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct FunctionShape {
    /// `-falign-functions`: for a function that asks no alignment of its
    /// own.
    pub align: Option<AlignSpec>,
    /// `-fmin-function-alignment=N` (gcc 14), in bytes: every function,
    /// whatever it asks.
    pub min_align: u32,
    /// `-fpatchable-function-entry=`.
    pub patchable: Option<PatchableEntry>,
    /// `-fzero-call-used-regs=`.
    pub zero_regs: ZeroRegs,
}

/// `-fstack-clash-protection`'s two `--param`s, as written: each the log2 of
/// a byte count. c17 probes no stack; these move where gcc would, and so
/// which functions c17 warns about.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct StackClash {
    /// `stack-clash-protection-guard-size`.
    pub guard_size: Option<u32>,
    /// `stack-clash-protection-probe-interval`.
    pub probe_interval: Option<u32>,
}

impl StackClash {
    /// The space aarch64 leaves for the caller's own use of the guard,
    /// gcc's `STACK_CLASH_CALLER_GUARD`.
    const AARCH64_CALLER_GUARD: i64 = 1024;

    /// The smallest static allocation gcc probes on `arch`. On x86-64 one of
    /// at least the guard size is probed every interval, so it is probed
    /// once it reaches both (gcc's defaults: 4 KiB each). aarch64 keeps the
    /// two equal (64 KiB by default) and probes once a frame passes the
    /// guard less what the caller may have used of it.
    pub fn probed_from(self, arch: Arch) -> i64 {
        match arch {
            Arch::X86_64 => {
                let guard = self.guard_size.unwrap_or(12);
                let interval = self.probe_interval.unwrap_or(12);
                1i64 << guard.max(interval)
            }
            Arch::Aarch64 => (1i64 << self.guard_size.unwrap_or(16)) - Self::AARCH64_CALLER_GUARD,
        }
    }

    /// What gcc's aarch64 back end refuses of the two, in its words: a
    /// guard of other than 4 KiB or 64 KiB, and an interval that is not the
    /// guard, which an unset interval follows. With or without the
    /// protection asked for, as in gcc.
    pub fn aarch64_errors(self) -> Vec<String> {
        let guard = self.guard_size.unwrap_or(16);
        let interval = self.probe_interval.unwrap_or(guard);
        let mut errors = Vec::new();
        if guard != 12 && guard != 16 {
            errors.push(format!(
                "only values 12 (4 KB) and 16 (64 KB) are supported for guard size.  \
                 Given value {guard} ({} KB) is out of range",
                (1u64 << guard) / 1024
            ));
        }
        if guard != interval {
            errors.push(format!(
                "stack clash guard size '{guard}' must be equal to probing interval '{interval}'"
            ));
        }
        errors
    }
}

/// `-fcf-protection`: which of Intel CET's two protections x86-64 code is
/// built to cooperate with. The default is neither, as Debian's gcc has it.
///
/// Branch protection puts `endbr64` wherever an indirect branch may land;
/// either protection is announced to the linker by the unit's
/// `.note.gnu.property`, and to the source by `__CET__`. gcc's
/// `__builtin_setjmp` buffer, which code built by another compiler may read,
/// follows it too. Only x86-64 ELF has CET: elsewhere a level that asks for a
/// protection is an error, as in gcc.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CfProtection {
    /// Indirect-branch tracking: an indirect jump lands on an `endbr64`.
    pub branch: bool,
    /// The shadow stack: return addresses are kept on a second stack.
    pub ret: bool,
    /// `check`: gcc's link-time optimizer checks that the objects agree.
    /// Nothing is built differently, but `__CET__` says so.
    pub check: bool,
}

impl CfProtection {
    /// The protection `-fcf-protection=<level>` selects, or `None` for a
    /// level gcc does not have.
    pub fn from_level(level: &str) -> Option<Self> {
        let (branch, ret, check) = match level {
            "none" => (false, false, false),
            "check" => (false, false, true),
            "branch" => (true, false, false),
            "return" => (false, true, false),
            "full" => (true, true, false),
            _ => return None,
        };
        Some(Self { branch, ret, check })
    }

    /// The level's name, as [`Self::from_level`] reads it.
    pub fn level(self) -> &'static str {
        match (self.branch, self.ret, self.check) {
            (true, true, _) => "full",
            (true, false, _) => "branch",
            (false, true, _) => "return",
            (false, false, true) => "check",
            (false, false, false) => "none",
        }
    }

    /// Whether code is built for either protection, which only x86-64 ELF
    /// can be.
    pub fn protects(self) -> bool {
        self.branch || self.ret
    }

    /// Whether `target` can be built for this level: gcc refuses one that
    /// asks for a protection where there is no CET -- aarch64, and Mach-O,
    /// which has no property note to carry it.
    pub fn supported_on(self, target: &Target) -> bool {
        !self.protects() || (target.arch == Arch::X86_64 && target.os != Os::MacOS)
    }

    /// gcc's `__CET__`: 1 for branch protection, 2 for return protection,
    /// their sum for both and 8 for `check`; not defined for `none`, nor
    /// for a target without CET.
    pub fn cet_macro(self, target: &Target) -> Option<u32> {
        if target.arch != Arch::X86_64 {
            return None;
        }
        let value = u32::from(self.branch) | u32::from(self.ret) << 1 | u32::from(self.check) << 3;
        (value != 0).then_some(value)
    }

    /// The `GNU_PROPERTY_X86_FEATURE_1_AND` bits the unit's
    /// `.note.gnu.property` carries: IBT (1) and SHSTK (2). `None`: no note.
    pub fn feature_bits(self) -> Option<u32> {
        self.protects()
            .then_some(u32::from(self.branch) | u32::from(self.ret) << 1)
    }
}

/// aarch64's `-mbranch-protection=`: whether indirect branches land only
/// on `bti` landing pads, and whether return addresses are signed, with
/// which key. The default is neither, as gcc's.
///
/// Like `-fcf-protection` on x86-64, announced to the linker and loader by
/// the unit's `.note.gnu.property` and to the source by predefined macros.
/// Only aarch64 ELF has it: Darwin has no property note, and x86-64 gcc does
/// not know the option.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct BranchProtection {
    /// `bti`: every indirect branch lands on a `bti` instruction.
    pub bti: bool,
    /// `pac-ret`: the return address is signed where it is saved and
    /// authenticated before the return.
    pub pac_ret: Option<PacRet>,
}

/// How `pac-ret` signs return addresses.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct PacRet {
    /// `+leaf`: sign in every function, not only one that saves the link
    /// register. Every c17 frame saves it, so the code is the same.
    pub leaf: bool,
    /// `+b-key`: sign with the B key instead of the A key.
    pub b_key: bool,
}

impl BranchProtection {
    /// The protection `-mbranch-protection=<value>` selects, read as gcc 13
    /// reads it: `+`-separated words, empty ones skipped; `none` and
    /// `standard` reset everything and must be the last word, `bti` adds
    /// landing pads, and `pac-ret` signs with the A key in the functions that
    /// save the link register, as modified by the `leaf` and `b-key` words
    /// directly after it. `Err` is gcc's message.
    pub fn parse(value: &str) -> Result<Self, String> {
        if value.is_empty() {
            return Err("missing argument to '-mbranch-protection='".to_string());
        }
        let words: Vec<&str> = value.split('+').filter(|w| !w.is_empty()).collect();
        if words.is_empty() {
            return Err("missing argument for '-mbranch-protection='".to_string());
        }
        let mut protection = Self::default();
        let mut i = 0;
        while i < words.len() {
            let (word, next) = (words[i], words.get(i + 1));
            match word {
                "none" | "standard" => {
                    if let Some(next) = next {
                        return Err(format!("unexpected '{next}' after '{word}'"));
                    }
                    protection = if word == "none" {
                        Self::default()
                    } else {
                        Self {
                            bti: true,
                            pac_ret: Some(PacRet::default()),
                        }
                    };
                }
                "bti" => protection.bti = true,
                "pac-ret" => {
                    let mut pac = PacRet::default();
                    while let Some(&modifier) = words.get(i + 1) {
                        match modifier {
                            "leaf" => pac.leaf = true,
                            "b-key" => pac.b_key = true,
                            _ => break,
                        }
                        i += 1;
                    }
                    protection.pac_ret = Some(pac);
                }
                _ => {
                    return Err(format!(
                        "invalid argument '{word}' for '-mbranch-protection='"
                    ))
                }
            }
            i += 1;
        }
        Ok(protection)
    }

    /// The protection the last `-mbranch-protection=` among the machine
    /// `flags` selects; `Err` is gcc's message for it.
    pub fn from_flags(flags: &[String]) -> Result<Self, String> {
        flags
            .iter()
            .rev()
            .find_map(|f| f.strip_prefix("-mbranch-protection="))
            .map_or(Ok(Self::default()), Self::parse)
    }

    /// Whether code is built for either protection.
    pub fn protects(self) -> bool {
        self.bti || self.pac_ret.is_some()
    }

    /// gcc's `__ARM_FEATURE_PAC_DEFAULT`: bit 0 for the A key, bit 1 for
    /// the B key, bit 2 for `leaf`; undefined without `pac-ret`.
    pub fn pac_macro(self) -> Option<u32> {
        self.pac_ret
            .map(|p| if p.b_key { 2 } else { 1 } | u32::from(p.leaf) << 2)
    }

    /// The `GNU_PROPERTY_AARCH64_FEATURE_1_AND` bits the unit's
    /// `.note.gnu.property` carries: BTI (1) and PAC (2). `None`: no note.
    pub fn feature_bits(self) -> Option<u32> {
        self.protects()
            .then_some(u32::from(self.bti) | u32::from(self.pac_ret.is_some()) << 1)
    }
}

/// `-pg` (or `-p`): a call to the profiler's counting routine at the entry
/// of every function, and the x86-64 options that reshape it.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Profiling {
    /// `-mfentry`: call `__fentry__` before the prologue instead of `mcount`
    /// after it.
    pub fentry: bool,
    /// `-mrecord-mcount`: record the address of each call in `__mcount_loc`.
    pub record: bool,
    /// `-mnop-mcount`: a five-byte nop where the call would be, for a tracer
    /// to patch.
    pub nop: bool,
}

impl Profiling {
    /// The `-m` options that shape the call, in both senses: x86-64's only.
    pub const MACHINE_FLAGS: [&'static str; 6] = [
        "-mfentry",
        "-mno-fentry",
        "-mrecord-mcount",
        "-mno-record-mcount",
        "-mnop-mcount",
        "-mno-nop-mcount",
    ];

    /// The shape the `-m` options in `flags` give the call, the last of each
    /// pair winning.
    pub fn from_flags(flags: &[String]) -> Self {
        let mut p = Self::default();
        for flag in flags {
            match flag.as_str() {
                "-mfentry" => p.fentry = true,
                "-mno-fentry" => p.fentry = false,
                "-mrecord-mcount" => p.record = true,
                "-mno-record-mcount" => p.record = false,
                "-mnop-mcount" => p.nop = true,
                "-mno-nop-mcount" => p.nop = false,
                _ => {}
            }
        }
        p
    }

    /// The routine the call names on `target`: x86-64's `mcount` or
    /// `__fentry__`, aarch64's `_mcount`.
    pub fn routine(self, target: &Target) -> &'static str {
        match target.arch {
            Arch::X86_64 if self.fentry => "__fentry__",
            Arch::X86_64 => "mcount",
            Arch::Aarch64 => "_mcount",
        }
    }
}

/// `-f[no-]asynchronous-unwind-tables`, `-f[no-]unwind-tables` and
/// `-f[no-]exceptions`, each the last one given (`None`: not given), which
/// together decide whether a unit has unwind tables; see [`Self::cfi`].
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct UnwindTables {
    pub asynchronous: Option<bool>,
    pub synchronous: Option<bool>,
    pub exceptions: Option<bool>,
}

/// Where a unit's call frame information goes.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum Cfi {
    /// `.eh_frame`, the unwind tables `backtrace()` and the C++ runtime read.
    #[default]
    EhFrame,
    /// `.debug_frame` only (`.cfi_sections .debug_frame`): no unwind tables,
    /// but `-g` keeps the rules for the debugger.
    DebugFrame,
    /// None at all.
    Omitted,
}

impl Cfi {
    /// Whether any CFI directives are emitted.
    pub fn emitted(self) -> bool {
        self != Cfi::Omitted
    }
}

impl UnwindTables {
    /// The request after the option `-f<name>`, or `None` for an option
    /// that is not one of the six.
    pub fn after(self, name: &str) -> Option<Self> {
        let (on, stem) = match name.strip_prefix("no-") {
            Some(stem) => (false, stem),
            None => (true, name),
        };
        let mut next = self;
        match stem {
            "asynchronous-unwind-tables" => next.asynchronous = Some(on),
            "unwind-tables" => next.synchronous = Some(on),
            "exceptions" => next.exceptions = Some(on),
            _ => return None,
        }
        Some(next)
    }

    /// The request the options named `names` make, in order, the last of
    /// each kind winning; a name that is not one of the six is passed over.
    pub fn from_names<S: AsRef<str>>(names: &[S]) -> Self {
        names.iter().fold(Self::default(), |u, name| {
            u.after(name.as_ref()).unwrap_or(u)
        })
    }

    /// `-fexceptions`: a frame that a `cleanup` variable is in scope in runs
    /// the variable's cleanup when it is unwound, and `__EXCEPTIONS` is
    /// defined. Off by default, as C's default is in gcc.
    pub fn exceptions(self) -> bool {
        self.exceptions.unwrap_or(false)
    }

    /// Where `target`'s CFI goes, `debug` being `-g`. As in gcc 13, a unit
    /// has unwind tables unless both kinds are off and `-fexceptions` does
    /// not need them: the asynchronous kind is on by default for both
    /// targets, and the synchronous kind for aarch64 alone, so x86-64 drops
    /// them for `-fno-asynchronous-unwind-tables` and aarch64 only with
    /// `-fno-unwind-tables` too. Without them, `-g` still gives the debugger
    /// its rules, in `.debug_frame`.
    ///
    /// Mach-O keeps its tables whatever the options say: Apple's linker
    /// builds the compact unwind table every Darwin unwinder reads from them,
    /// and c17 has no other way to give it one.
    pub fn cfi(self, target: &Target, debug: bool) -> Cfi {
        let tables = self.exceptions()
            || self.asynchronous.unwrap_or(true)
            || self.synchronous.unwrap_or(target.arch == Arch::Aarch64);
        if tables || target.os == Os::MacOS {
            Cfi::EhFrame
        } else if debug {
            Cfi::DebugFrame
        } else {
            Cfi::Omitted
        }
    }
}

/// The stack protector as the command line sets it: the level, and gcc's
/// `--param ssp-buffer-size`, the bytes from which plain `-fstack-protector`
/// counts a `char` array (`arch::stack_protect`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StackProtection {
    pub level: StackProtector,
    pub buffer_size: u64,
}

impl StackProtection {
    /// gcc's default `ssp-buffer-size`.
    pub const DEFAULT_BUFFER_SIZE: u64 = 8;
}

impl Default for StackProtection {
    fn default() -> Self {
        Self {
            level: StackProtector::Off,
            buffer_size: Self::DEFAULT_BUFFER_SIZE,
        }
    }
}

/// `-fstack-protector` and its levels: which functions get a canary between
/// their locals and their saved registers, checked before every return
/// (`arch::stack_protect`). The default is none, as Debian's gcc has it;
/// `-fno-stack-protector` returns to it, and the last of the family wins.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum StackProtector {
    #[default]
    Off,
    /// `-fstack-protector`: a `char` array of eight bytes or more, or
    /// `alloca`.
    Default,
    /// `-fstack-protector-strong`: any array, or any local whose address
    /// is taken.
    Strong,
    /// `-fstack-protector-all`: every function.
    All,
    /// `-fstack-protector-explicit`: only `__attribute__((stack_protect))`.
    Explicit,
}

impl StackProtector {
    /// The level the option `-f<name>` selects, or `None` for any other
    /// option.
    pub fn from_option(name: &str) -> Option<Self> {
        Some(match name {
            "no-stack-protector" => Self::Off,
            "stack-protector" => Self::Default,
            "stack-protector-strong" => Self::Strong,
            "stack-protector-all" => Self::All,
            "stack-protector-explicit" => Self::Explicit,
            _ => return None,
        })
    }

    /// The `-f` option spelling this level, as [`Self::from_option`] reads it.
    pub fn option(self) -> &'static str {
        match self {
            Self::Off => "no-stack-protector",
            Self::Default => "stack-protector",
            Self::Strong => "stack-protector-strong",
            Self::All => "stack-protector-all",
            Self::Explicit => "stack-protector-explicit",
        }
    }

    /// The macro gcc predefines for this level, with its value.
    pub fn predefined_macro(self) -> Option<(&'static str, &'static str)> {
        match self {
            Self::Off => None,
            Self::Default => Some(("__SSP__", "1")),
            Self::Strong => Some(("__SSP_STRONG__", "3")),
            Self::All => Some(("__SSP_ALL__", "2")),
            Self::Explicit => Some(("__SSP_EXPLICIT__", "4")),
        }
    }
}

/// One x86-64 instruction-set extension gcc 13 has an `-m` option for, and
/// that `__attribute__((target("...")))` names the same way.
struct IsaFeature {
    /// The option's name: `-m<name>`, `-mno-<name>`, `target("<name>")`.
    name: &'static str,
    /// The macro gcc defines to 1 while the extension is on, if it has one.
    macro_name: Option<&'static str>,
    /// The extensions it is built on: turning it on turns them on, and what
    /// they are built on, and turning one of them off turns it off (gcc's
    /// `OPTION_MASK_ISA_*_SET` and `_UNSET`).
    needs: &'static [&'static str],
    /// Extensions turning it on turns on too, which it survives the loss of.
    brings: &'static [&'static str],
    /// Turning it on turns on what it needs, but not what those need: gcc's
    /// AMX sets name AMX-TILE and not AMX-TILE's own XSAVE.
    shallow: bool,
}

const fn isa(
    name: &'static str,
    macro_name: &'static str,
    needs: &'static [&'static str],
) -> IsaFeature {
    IsaFeature {
        name,
        macro_name: Some(macro_name),
        needs,
        brings: &[],
        shallow: false,
    }
}

/// An AMX extension built on AMX-TILE: see [`IsaFeature::shallow`].
const fn amx(name: &'static str, macro_name: &'static str) -> IsaFeature {
    IsaFeature {
        name,
        macro_name: Some(macro_name),
        needs: &["amx-tile"],
        brings: &[],
        shallow: true,
    }
}

/// Part of the x86-64 baseline, whose macros the target defines.
const fn base(name: &'static str, needs: &'static [&'static str]) -> IsaFeature {
    IsaFeature {
        name,
        macro_name: None,
        needs,
        brings: &[],
        shallow: false,
    }
}

/// An extension gcc defines no macro for.
const fn unnamed(name: &'static str) -> IsaFeature {
    IsaFeature {
        name,
        macro_name: None,
        needs: &[],
        brings: &[],
        shallow: false,
    }
}

/// Every extension gcc 13's x86-64 options name. In the order their
/// macros are listed; SSE, SSE2 and the x87, the baseline, last, their
/// macros being defined apart (`token::preprocess::define_isa_macros`).
/// The needs are gcc 13's, read off its `-dM -E` for each `-mX` and for
/// `-mno-X` after every other.
#[rustfmt::skip]
const ISA_FEATURES: &[IsaFeature] = &[
    isa("mmx",          "__MMX__",          &[]),
    isa("fxsr",         "__FXSR__",         &[]),
    isa("sse3",         "__SSE3__",         &["sse2"]),
    isa("ssse3",        "__SSSE3__",        &["sse3"]),
    isa("sse4.1",       "__SSE4_1__",       &["ssse3"]),
    isa("sse4.2",       "__SSE4_2__",       &["sse4.1"]),
    isa("popcnt",       "__POPCNT__",       &[]),
    isa("crc32",        "__CRC32__",        &[]),
    isa("sse4a",        "__SSE4A__",        &["sse3"]),
    isa("xsave",        "__XSAVE__",        &[]),
    isa("xsavec",       "__XSAVEC__",       &["xsave"]),
    isa("xsaveopt",     "__XSAVEOPT__",     &["xsave"]),
    isa("xsaves",       "__XSAVES__",       &["xsave"]),
    isa("avx",          "__AVX__",          &["sse4.2", "xsave"]),
    isa("avx2",         "__AVX2__",         &["avx"]),
    isa("f16c",         "__F16C__",         &["avx"]),
    isa("fma",          "__FMA__",          &["avx"]),
    isa("fma4",         "__FMA4__",         &["sse4a", "avx"]),
    isa("xop",          "__XOP__",          &["fma4"]),
    isa("avx512f",      "__AVX512F__",      &["avx2"]),
    isa("avx512bw",     "__AVX512BW__",     &["avx512f"]),
    isa("avx512dq",     "__AVX512DQ__",     &["avx512f"]),
    IsaFeature {
        name: "avx512vp2intersect",
        macro_name: Some("__AVX512VP2INTERSECT__"),
        needs: &["avx512f"],
        brings: &["avx512dq"],
        shallow: false,
    },
    isa("avx512cd",     "__AVX512CD__",     &["avx512f"]),
    isa("avx512er",     "__AVX512ER__",     &["avx512f"]),
    isa("avx512pf",     "__AVX512PF__",     &["avx512f"]),
    isa("avx512vl",     "__AVX512VL__",     &["avx512f"]),
    isa("avx512ifma",   "__AVX512IFMA__",   &["avx512f"]),
    isa("avx512vbmi",   "__AVX512VBMI__",   &["avx512bw"]),
    isa("avx512vbmi2",  "__AVX512VBMI2__",  &["avx512f"]),
    isa("avx512vnni",   "__AVX512VNNI__",   &["avx512f"]),
    isa("avx512bitalg", "__AVX512BITALG__", &["avx512f"]),
    isa("avx512vpopcntdq", "__AVX512VPOPCNTDQ__", &["avx512f"]),
    isa("avx5124fmaps", "__AVX5124FMAPS__", &["avx512f"]),
    isa("avx5124vnniw", "__AVX5124VNNIW__", &["avx512f"]),
    isa("avx512bf16",   "__AVX512BF16__",   &["avx512bw"]),
    isa("avx512fp16",   "__AVX512FP16__",   &["avx512bw"]),
    isa("avxvnni",      "__AVXVNNI__",      &["avx2"]),
    isa("avxifma",      "__AVXIFMA__",      &["avx2"]),
    isa("avxneconvert", "__AVXNECONVERT__", &["avx2"]),
    isa("avxvnniint8",  "__AVXVNNIINT8__",  &["avx2"]),
    isa("lzcnt",        "__LZCNT__",        &[]),
    isa("abm",          "__ABM__",          &[]),
    isa("bmi",          "__BMI__",          &[]),
    isa("bmi2",         "__BMI2__",         &[]),
    isa("tbm",          "__TBM__",          &[]),
    isa("adx",          "__ADX__",          &[]),
    isa("aes",          "__AES__",          &[]),
    isa("pclmul",       "__PCLMUL__",       &[]),
    isa("vaes",         "__VAES__",         &[]),
    isa("vpclmulqdq",   "__VPCLMULQDQ__",   &[]),
    isa("gfni",         "__GFNI__",         &[]),
    isa("sha",          "__SHA__",          &[]),
    isa("rtm",          "__RTM__",          &[]),
    isa("shstk",        "__SHSTK__",        &[]),
    isa("rdrnd",        "__RDRND__",        &[]),
    isa("rdseed",       "__RDSEED__",       &[]),
    isa("rdpid",        "__RDPID__",        &[]),
    isa("fsgsbase",     "__FSGSBASE__",     &[]),
    isa("movbe",        "__MOVBE__",        &[]),
    isa("prfchw",       "__PRFCHW__",       &[]),
    isa("prefetchwt1",  "__PREFETCHWT1__",  &[]),
    isa("prefetchi",    "__PREFETCHI__",    &[]),
    isa("clflushopt",   "__CLFLUSHOPT__",   &[]),
    isa("clwb",         "__CLWB__",         &[]),
    isa("clzero",       "__CLZERO__",       &[]),
    isa("cldemote",     "__CLDEMOTE__",     &[]),
    isa("cmpccxadd",    "__CMPCCXADD__",    &[]),
    isa("enqcmd",       "__ENQCMD__",       &[]),
    isa("hreset",       "__HRESET__",       &[]),
    isa("kl",           "__KL__",           &[]),
    isa("widekl",       "__WIDEKL__",       &["kl"]),
    isa("lwp",          "__LWP__",          &[]),
    isa("movdir64b",    "__MOVDIR64B__",    &[]),
    isa("movdiri",      "__MOVDIRI__",      &[]),
    isa("mwaitx",       "__MWAITX__",       &[]),
    isa("pconfig",      "__PCONFIG__",      &[]),
    isa("pku",          "__PKU__",          &[]),
    isa("ptwrite",      "__PTWRITE__",      &[]),
    isa("raoint",       "__RAOINT__",       &[]),
    isa("sahf",         "__LAHF_SAHF__",    &[]),
    isa("serialize",    "__SERIALIZE__",    &[]),
    isa("sgx",          "__SGX__",          &[]),
    isa("tsxldtrk",     "__TSXLDTRK__",     &[]),
    isa("uintr",        "__UINTR__",        &[]),
    isa("waitpkg",      "__WAITPKG__",      &[]),
    isa("wbnoinvd",     "__WBNOINVD__",     &[]),
    isa("3dnow",        "__3dNOW__",        &["mmx"]),
    isa("3dnowa",       "__3dNOW_A__",      &["3dnow"]),
    isa("amx-tile",     "__AMX_TILE__",     &["xsave"]),
    amx("amx-int8",     "__AMX_INT8__"),
    amx("amx-bf16",     "__AMX_BF16__"),
    amx("amx-fp16",     "__AMX_FP16__"),
    amx("amx-complex",  "__AMX_COMPLEX__"),
    // gcc defines no macro for these two.
    unnamed("hle"),
    unnamed("mwait"),
    // gcc's `__GCC_HAVE_SYNC_COMPARE_AND_SWAP_16` says the `__sync`
    // builtins are lock-free at 16 bytes, and c17's are not: CMPXCHG16B is
    // an extension it never generates, so the option names nothing more.
    unnamed("cx16"),
    // The baseline, which `-mno-sse`, `-mno-80387` (`-msoft-float`) and
    // `-mgeneral-regs-only` take away.
    base("sse", &[]),
    base("sse2", &["sse"]),
    base("80387", &[]),
];

/// Extensions gcc turns on for another once every option has been read,
/// unless an option named them: POPCNT and CRC32 for SSE4.2, POPCNT and
/// LZCNT for ABM. `-mavx -mno-sse4.1` so has neither POPCNT nor CRC32.
const ISA_FOLLOWERS: &[(&str, &[&str])] = &[
    ("sse4.2", &["popcnt", "crc32"]),
    ("abm", &["popcnt", "lzcnt"]),
];

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
/// gcc's ISA options (`-msse4.1`, `-mavx2`, `-mno-mmx`, ...) and `-march=`.
/// They are statements about the target, which the feature macros
/// (`__SSE4_1__`, `__AVX2__` and the rest) report as gcc's do. Code is
/// chosen by them where c17 has a choice to make: the packed instructions
/// vector operations use (`arch::simd`), up to SSE4.2, and `fma` by FMA.
/// c17's intrinsic headers provide every function whatever the level,
/// since they are written in portable C. A `-march=` CPU claims no more
/// than SSE4.2 and POPCNT: c17 has no AVX intrinsics for code that tests
/// `__AVX__` to use.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct X86Isa {
    set: IsaSet,
}

impl Default for X86Isa {
    /// The x86-64 baseline: MMX and FXSR, beside SSE and SSE2.
    fn default() -> Self {
        Self {
            set: IsaSet::BASELINE,
        }
    }
}

/// A set of the x86-64 extensions in [`ISA_FEATURES`]: one bit each, in
/// table order.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct IsaSet(u128);

/// Whether two names are the same; `str` has no `const` comparison.
const fn same_name(a: &str, b: &str) -> bool {
    let (a, b) = (a.as_bytes(), b.as_bytes());
    if a.len() != b.len() {
        return false;
    }
    let mut i = 0;
    while i < a.len() {
        if a[i] != b[i] {
            return false;
        }
        i += 1;
    }
    true
}

impl IsaSet {
    const SSE3: Self = Self::named("sse3");
    const SSSE3: Self = Self::named("ssse3");
    const SSE41: Self = Self::named("sse4.1");
    const SSE42: Self = Self::named("sse4.2");
    const POPCNT: Self = Self::named("popcnt");
    const AVX: Self = Self::named("avx");
    const FMA: Self = Self::named("fma");
    const FMA4: Self = Self::named("fma4");
    const AVX512F: Self = Self::named("avx512f");
    const SSE: Self = Self::named("sse");
    const SSE2: Self = Self::named("sse2");
    const X87: Self = Self::named("80387");
    const MMX: Self = Self::named("mmx");
    const BASELINE: Self = Self(
        Self::named("mmx").0 | Self::named("fxsr").0 | Self::SSE.0 | Self::SSE2.0 | Self::X87.0,
    );

    /// The extension `name`, found at compile time.
    const fn named(name: &str) -> Self {
        let mut i = 0;
        while i < ISA_FEATURES.len() {
            if same_name(ISA_FEATURES[i].name, name) {
                return Self(1 << i);
            }
            i += 1;
        }
        panic!("not an ISA_FEATURES name")
    }

    /// The extension an option or `target` item names, if it is one.
    fn by_name(name: &str) -> Option<Self> {
        ISA_FEATURES
            .iter()
            .position(|f| f.name == name)
            .map(|i| Self(1 << i))
    }

    fn of_names(names: &[&str]) -> Self {
        names.iter().fold(Self::default(), |set, name| {
            set.with(Self::by_name(name).expect("an ISA_FEATURES name"))
        })
    }

    fn members(self) -> impl Iterator<Item = (usize, &'static IsaFeature)> {
        ISA_FEATURES
            .iter()
            .enumerate()
            .filter(move |(i, _)| self.0 & (1 << i) != 0)
    }

    /// These extensions and every one they are built on, following `needs`
    /// -- and `brings` too, with `bringing`.
    fn closed(self, bringing: bool) -> Self {
        let mut set = self;
        loop {
            let grown = set.members().fold(set, |acc, (_, f)| {
                let acc = acc.with(Self::of_names(f.needs));
                if bringing {
                    acc.with(Self::of_names(f.brings))
                } else {
                    acc
                }
            });
            if grown == set {
                return set;
            }
            set = grown;
        }
    }

    /// What turning these on turns on: them, and what they need and bring
    /// -- only what they need, for a [`IsaFeature::shallow`] one.
    fn enabling(self) -> Self {
        self.members().fold(self, |set, (i, f)| {
            if f.shallow {
                set.with(Self::of_names(f.needs))
            } else {
                set.with(Self(1 << i).closed(true))
            }
        })
    }

    /// What turning these off turns off: them, and every extension built
    /// on one of them.
    fn disabling(self) -> Self {
        (0..ISA_FEATURES.len())
            .map(|i| Self(1 << i))
            .filter(|one| one.closed(false).0 & self.0 != 0)
            .fold(Self::default(), Self::with)
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

    /// [`ISA_FOLLOWERS`]: what `self` brings at the end, of what `named`
    /// leaves alone, for each leader `raised` says to consider.
    fn with_followers(self, named: Self, raised: impl Fn(Self) -> bool) -> Self {
        ISA_FOLLOWERS
            .iter()
            .map(|(leader, followers)| (Self::of_names(&[leader]), Self::of_names(followers)))
            .filter(|&(leader, _)| self.has(leader) && raised(leader))
            .fold(self, |set, (_, followers)| {
                set.with(followers.without(named))
            })
    }

    /// What `-march=cpu` enables, of these: gcc 13's `-dM -E` for each
    /// 64-bit name in its roster, as far as SSE4.2 and POPCNT. A name not
    /// listed here -- `x86-64`, `k8`, `opteron`, `athlon64`, `athlon-fx` --
    /// is the baseline.
    pub(crate) fn of_arch(cpu: &str) -> Self {
        let sse3 = Self::BASELINE.with(Self::SSE3);
        let ssse3 = Self::BASELINE.with(Self::SSSE3.enabling());
        let sse41 = Self::BASELINE.with(Self::SSE41.enabling());
        let sse42 = Self::BASELINE
            .with(Self::SSE42.enabling())
            .with(Self::POPCNT);
        match cpu {
            "native" => Self::host(),
            "nocona" | "k8-sse3" | "opteron-sse3" | "athlon64-sse3" | "eden-x2" => sse3,
            "core2" | "bonnell" | "atom" | "nano" | "nano-1000" | "nano-2000" => ssse3,
            "nano-3000" | "nano-x2" | "eden-x4" | "nano-x4" => sse41,
            "amdfam10" | "barcelona" => sse3.with(Self::POPCNT),
            "btver1" => ssse3.with(Self::POPCNT),
            "nehalem" | "corei7" | "westmere" | "sandybridge" | "corei7-avx" | "ivybridge"
            | "core-avx-i" | "haswell" | "core-avx2" | "broadwell" | "skylake"
            | "skylake-avx512" | "cannonlake" | "icelake-client" | "rocketlake"
            | "icelake-server" | "cascadelake" | "tigerlake" | "cooperlake" | "sapphirerapids"
            | "emeraldrapids" | "alderlake" | "raptorlake" | "meteorlake" | "graniterapids"
            | "graniterapids-d" | "silvermont" | "slm" | "goldmont" | "goldmont-plus"
            | "tremont" | "gracemont" | "sierraforest" | "grandridge" | "knl" | "knm"
            | "x86-64-v2" | "x86-64-v3" | "x86-64-v4" | "lujiazui" | "bdver1" | "bdver2"
            | "bdver3" | "bdver4" | "znver1" | "znver2" | "znver3" | "znver4" | "btver2" => sse42,
            _ => Self::BASELINE,
        }
    }

    /// What `-march=native` finds on this machine, when it is an x86-64,
    /// as far as [`Self::of_arch`] goes.
    fn host() -> Self {
        #[cfg(target_arch = "x86_64")]
        {
            let mut set = Self::BASELINE;
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
            Self::BASELINE
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
    let item = flag.strip_prefix("-m")?;
    match item {
        // gcc's spellings of `-mno-80387` and `-m80387`.
        "soft-float" => return Some(IsaEdit::Disable(IsaSet::X87.disabling())),
        "hard-float" => return Some(IsaEdit::Enable(IsaSet::X87)),
        // No register but the general ones: no SSE, MMX or x87, nor
        // anything built on them.
        "general-regs-only" => {
            let regs = IsaSet::SSE.with(IsaSet::MMX).with(IsaSet::X87);
            return Some(IsaEdit::Disable(regs.disabling()));
        }
        "sse4" => return Some(IsaEdit::Enable(IsaSet::SSE42.enabling())),
        // gcc's -mno-sse4 is -mno-sse4.1, not the inverse of -msse4.
        "no-sse4" => return Some(IsaEdit::Disable(IsaSet::SSE41.disabling())),
        _ => {}
    }
    Some(match item.strip_prefix("no-") {
        Some(name) => IsaEdit::Disable(IsaSet::by_name(name)?.disabling()),
        None => IsaEdit::Enable(IsaSet::by_name(item)?.enabling()),
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

/// Whether `flag` is one of gcc's x86-64 ISA options: an extension of
/// [`ISA_FEATURES`] or `-msse4`, either sense.
pub fn is_isa_flag(flag: &str) -> bool {
    isa_edit(flag).is_some()
}

/// Whether `name` is an extension a `target` attribute may name.
pub(crate) fn is_isa_name(name: &str) -> bool {
    name == "sse4" || IsaSet::by_name(name).is_some()
}

impl X86Isa {
    /// The extensions `flags` -- the `-m` options, in order -- ask for, as
    /// gcc decides them: the last `-march=` gives a base, the ISA options
    /// edit it in order wherever they stand, and an extension one of them
    /// named keeps the state it gave, whatever the CPU has. Last, the
    /// [`ISA_FOLLOWERS`] of what is on, but for those an option named.
    pub fn from_flags(flags: &[String]) -> Self {
        let mut arch = IsaSet::BASELINE;
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
        Self {
            set: set.with_followers(named, |_| true),
        }
    }

    /// The ISA a function with `__attribute__((target(...)))` is compiled
    /// for, when the translation unit's is `self`: an `arch=` adds what
    /// that CPU has, and the feature edits apply on top in order, as the
    /// `-m` options do. A leader of [`ISA_FOLLOWERS`] the request turns on
    /// brings its followers unless the request named them, as the option
    /// does.
    pub fn with_request(self, request: &IsaRequest) -> Self {
        let mut set = self.set;
        let arch = request.arch();
        if let Some(arch) = arch {
            set = set.with(arch);
        }
        let mut named = IsaSet::default();
        let mut enabled = arch.unwrap_or_default();
        for edit in request.edits() {
            match edit {
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
        Self {
            set: set.with_followers(named, |leader| enabled.has(leader)),
        }
    }

    /// Whether code compiled for `other` may run as part of code compiled
    /// for `self`: every extension `other` uses, `self` has. gcc's rule for
    /// inlining one `target` function into another.
    pub fn includes(self, other: Self) -> bool {
        self.set.has(other.set)
    }

    /// Whether SSE2 is on: c17 computes `float` and `double`, and moves
    /// vectors, in SSE2's registers and nowhere else.
    pub fn has_sse2(self) -> bool {
        self.set.has(IsaSet::SSE2)
    }

    /// Whether SSE is on, whose `__SSE__` the target defines.
    pub fn has_sse(self) -> bool {
        self.set.has(IsaSet::SSE)
    }

    /// Whether the x87 is on: where c17 computes `long double`.
    pub fn has_x87(self) -> bool {
        self.set.has(IsaSet::X87)
    }

    /// The SSE level reached: SSE3 .. SSE4.2, each needing the one before.
    pub fn simd(self) -> X86Simd {
        [
            (IsaSet::SSE42, X86Simd::Sse42),
            (IsaSet::SSE41, X86Simd::Sse41),
            (IsaSet::SSSE3, X86Simd::Ssse3),
            (IsaSet::SSE3, X86Simd::Sse3),
        ]
        .into_iter()
        .find(|&(ext, _)| self.set.has(ext.enabling()))
        .map_or(X86Simd::Sse2, |(_, level)| level)
    }

    /// The fused multiply-add instruction this ISA has for a scalar, if
    /// any: FMA's three-operand `vfmadd213s*` (AVX512F has it too), or
    /// FMA4's four-operand `vfmadds*`.
    pub fn fused_multiply_add(self) -> Option<FusedMultiplyAdd> {
        if self.set.has(IsaSet::FMA) || self.set.has(IsaSet::AVX512F) {
            Some(FusedMultiplyAdd::Fma3)
        } else if self.set.has(IsaSet::FMA4) {
            Some(FusedMultiplyAdd::Fma4)
        } else {
            None
        }
    }

    /// gcc's `__BIGGEST_ALIGNMENT__`, and what `__attribute__((aligned))`
    /// with no argument asks for: the widest register's, 16 bytes for SSE,
    /// 32 with AVX's and 64 with AVX512F's.
    pub fn biggest_alignment(self) -> u32 {
        if self.set.has(IsaSet::AVX512F) {
            64
        } else if self.set.has(IsaSet::AVX) {
            32
        } else {
            16
        }
    }

    /// The feature macros this ISA defines to 1, beyond SSE's and SSE2's:
    /// one per extension, and `__FP_FAST_FMA` and its siblings where `fma`
    /// is an instruction ([`Self::fused_multiply_add`]).
    pub fn macros(self) -> Vec<&'static str> {
        let mut m: Vec<&'static str> = self
            .set
            .members()
            .filter_map(|(_, f)| f.macro_name)
            .collect();
        if self.fused_multiply_add().is_some() {
            m.extend([
                "__FP_FAST_FMA",
                "__FP_FAST_FMAF",
                "__FP_FAST_FMAF32",
                "__FP_FAST_FMAF64",
                "__FP_FAST_FMAF32x",
            ]);
        }
        m
    }
}

/// The scalar fused multiply-add an x86-64 ISA has.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FusedMultiplyAdd {
    /// FMA (and AVX512F): `vfmadd213sd b, a, acc`, whose destination is an
    /// operand.
    Fma3,
    /// AMD's FMA4: `vfmaddsd c, b, a, dst`, four operands.
    Fma4,
}

/// What `__attribute__((target("...")))` asks of one function's x86-64
/// ISA, relative to the translation unit's: the extensions an `arch=` CPU
/// has, and the feature edits (`sse4.1`, `no-sse4.2`, `popcnt`) in order.
/// Read from the attribute by `crate::target_attr`; applied by
/// [`X86Isa::with_request`]. One list, so that a function's attributes stay
/// a pointer's size here whatever an [`IsaSet`] holds.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct IsaRequest {
    items: Vec<IsaRequestItem>,
}

/// One item of an [`IsaRequest`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IsaRequestItem {
    /// `arch=`: what that CPU has.
    Arch(IsaSet),
    /// A feature turned on or off.
    Edit(IsaEdit),
}

impl IsaRequest {
    /// Ask for what `arch=` of a CPU with `set` asks.
    pub(crate) fn push_arch(&mut self, set: IsaSet) {
        self.items.push(IsaRequestItem::Arch(set));
    }

    /// Ask for a feature edit, after those already asked for.
    pub(crate) fn push_edit(&mut self, edit: IsaEdit) {
        self.items.push(IsaRequestItem::Edit(edit));
    }

    /// The extensions of every `arch=` CPU asked for, if there is one.
    fn arch(&self) -> Option<IsaSet> {
        self.items
            .iter()
            .filter_map(|item| match item {
                IsaRequestItem::Arch(set) => Some(*set),
                IsaRequestItem::Edit(_) => None,
            })
            .reduce(IsaSet::with)
    }

    /// The feature edits, in order.
    fn edits(&self) -> impl Iterator<Item = IsaEdit> + '_ {
        self.items.iter().filter_map(|item| match item {
            IsaRequestItem::Edit(edit) => Some(*edit),
            IsaRequestItem::Arch(_) => None,
        })
    }

    /// `other` asked after this: its CPU's extensions join any this names,
    /// and its edits follow these.
    pub fn extend(&mut self, other: IsaRequest) {
        self.items.extend(other.items);
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

/// gcc's four ELF thread-local models, from the one that assumes least to
/// the one that assumes most, which is the order `Ord` gives them.
///
/// c17 has no Local Dynamic sequence: where gcc would use it, c17 uses the
/// descriptor call of Global Dynamic, which is valid wherever Local Dynamic
/// is (gcc's own aarch64 code makes the same choice).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum TlsModel {
    #[default]
    GlobalDynamic,
    LocalDynamic,
    InitialExec,
    LocalExec,
}

impl TlsModel {
    /// The model `-ftls-model=<name>` names, in gcc's spelling.
    pub fn from_name(name: &str) -> Option<Self> {
        Some(match name {
            "global-dynamic" => Self::GlobalDynamic,
            "local-dynamic" => Self::LocalDynamic,
            "initial-exec" => Self::InitialExec,
            "local-exec" => Self::LocalExec,
            _ => return None,
        })
    }

    /// The names [`Self::from_name`] takes, in the order gcc lists them.
    pub const NAMES: &'static str = "global-dynamic initial-exec local-dynamic local-exec";
}

/// What a compilation's thread-local accesses may assume, from which every
/// access's model follows: see [`TlsPolicy::initial_exec`] and
/// [`Target::tls_access`].
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct TlsPolicy {
    /// The code may live in a shared object (`-shared`, `-fpic`, `-fPIC`),
    /// so not even a thread-local it defines has an offset known at link
    /// time.
    pub shared_code: bool,
    /// `-ftls-model=`: the least optimized model any access may use. An
    /// access takes this or the model the code can prove, whichever assumes
    /// more, which is how gcc combines them.
    pub floor: TlsModel,
}

impl TlsPolicy {
    /// Whether the code must reach its thread-locals through a call, under
    /// a dynamic model: shared code, unless `-ftls-model=` allows a static
    /// one.
    pub fn dynamic(self) -> bool {
        self.shared_code && self.floor < TlsModel::InitialExec
    }

    /// Whether a static access to a thread-local is Initial Exec rather than
    /// Local Exec. Local Exec fixes the offset from the thread pointer at
    /// link time, which only holds for the executable's own thread-locals:
    /// one defined elsewhere (`is_extern`), or any in shared code, needs the
    /// offset loaded from the GOT -- unless `-ftls-model=local-exec` says the
    /// link will fix it anyway.
    pub fn initial_exec(self, is_extern: bool) -> bool {
        self.floor < TlsModel::LocalExec && (is_extern || self.shared_code)
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
    /// aarch64's `-mbranch-protection=`; none for any other target.
    pub branch_protection: BranchProtection,
    /// x86-64's code model, mitigations and stack-guard options; the
    /// defaults for any other target, where they mean nothing.
    pub x86: crate::machine::X86Options,
    /// `-fshort-wchar`: `wchar_t` is `unsigned short` whatever the ABI
    /// says.
    pub short_wchar: bool,
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
            branch_protection: BranchProtection::default(),
            x86: Default::default(),
            short_wchar: false,
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
    /// Linux. `-fshort-wchar` makes it `unsigned short` everywhere, as gcc
    /// does.
    pub fn wchar_type(&self) -> IntType {
        if self.short_wchar {
            return IntType::UShort;
        }
        match (self.arch, self.os) {
            (Arch::Aarch64, Os::Linux | Os::FreeBSD) => IntType::UInt,
            (Arch::Aarch64, Os::MacOS) | (Arch::X86_64, _) => IntType::Int,
        }
    }

    /// gcc's `__BIGGEST_ALIGNMENT__`, the alignment a bare
    /// `__attribute__((aligned))` asks for: the widest vector register's,
    /// which on x86-64 depends on the ISA ([`X86Isa::biggest_alignment`])
    /// and on aarch64 is always 16.
    pub fn biggest_alignment(&self) -> u32 {
        match self.arch {
            Arch::X86_64 => self.x86_isa.biggest_alignment(),
            Arch::Aarch64 => 16,
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
    /// How this target obtains a thread-local's address under `policy`.
    ///
    /// ELF (Linux, FreeBSD) folds Local and Initial Exec into the access and
    /// needs a descriptor call only where the policy is dynamic, and only
    /// Linux takes the descriptor model here: FreeBSD's shared code uses
    /// Initial Exec, never Local Exec (see [`TlsPolicy::initial_exec`]).
    /// Mach-O always calls the TLV getter, whatever `-ftls-model=` says, as
    /// clang does.
    pub fn tls_access(&self, policy: TlsPolicy) -> TlsAccess {
        match self.os {
            Os::MacOS => TlsAccess::MachOTlv,
            Os::Linux if policy.dynamic() => TlsAccess::ElfDescriptor,
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
    /// gcc's -msse4.2 brings POPCNT and CRC32 with it.
    #[test]
    fn test_x86_isa_from_flags() {
        let isa = |flags: &[&str]| {
            let owned: Vec<String> = flags.iter().map(|f| f.to_string()).collect();
            X86Isa::from_flags(&owned)
        };
        let has = |flags: &[&str], name: &str| isa(flags).macros().contains(&name);
        assert_eq!(isa(&[]), X86Isa::default());
        assert_eq!(isa(&[]).macros(), vec!["__MMX__", "__FXSR__"]);
        assert_eq!(isa(&["-msse4.1", "-msse3"]).simd(), X86Simd::Sse41);
        assert!(!has(&["-msse4.1"], "__POPCNT__"));
        assert_eq!(isa(&["-msse4.2"]).simd(), X86Simd::Sse42);
        assert!(has(&["-msse4.2"], "__POPCNT__"));
        assert_eq!(isa(&["-march=x86-64-v2"]), isa(&["-msse4.2"]));
        assert_eq!(isa(&["-mavx2"]).simd(), X86Simd::Sse42);
        assert!(isa(&["-mavx2"]).includes(isa(&["-mavx"])));
        assert!(!isa(&["-mavx"]).includes(isa(&["-mavx2"])));
        assert!(isa(&[]).includes(isa(&["-mno-mmx"])));
        assert_eq!(
            isa(&["-mfma"]).fused_multiply_add(),
            Some(FusedMultiplyAdd::Fma3)
        );
        assert_eq!(
            isa(&["-mfma4"]).fused_multiply_add(),
            Some(FusedMultiplyAdd::Fma4)
        );
        assert_eq!(
            isa(&["-mavx512f"]).fused_multiply_add(),
            Some(FusedMultiplyAdd::Fma3)
        );
        assert_eq!(isa(&["-mfma", "-mno-avx"]).fused_multiply_add(), None);
        assert_eq!(isa(&["-mavx2"]).biggest_alignment(), 32);
        assert_eq!(isa(&["-mavx512f"]).biggest_alignment(), 64);
        assert_eq!(isa(&["-mavx512f", "-mno-avx512bw"]).biggest_alignment(), 64);
        assert!(ISA_FEATURES.len() <= 128, "IsaSet holds 128");
        for f in ISA_FEATURES {
            assert!(is_isa_flag(&format!("-m{}", f.name)), "{}", f.name);
            assert!(is_isa_flag(&format!("-mno-{}", f.name)), "{}", f.name);
        }
        for flag in ["-msse", "-msse2", "-mno-80387", "-mgeneral-regs-only"] {
            assert!(is_isa_flag(flag), "{flag}");
        }
        for flag in ["-mavx10.1", "-mno-red-zone", "-mavx2x"] {
            assert!(!is_isa_flag(flag), "{flag}");
        }
    }

    /// The last `-march=` sets the base and the `-m` options, wherever they
    /// stand, apply on top in order, as gcc 13's `-dM -E` reports: of the
    /// SSE levels, POPCNT and CRC32, which SSE4.2 brings only at the end
    /// and only if no option named it.
    #[test]
    fn test_x86_isa_flag_order_matches_gcc() {
        const ALL: &str = "SSE3 SSSE3 SSE4_1 SSE4_2 POPCNT CRC32";
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
            (
                "-march=x86-64-v2 -mno-popcnt",
                "SSE3 SSSE3 SSE4_1 SSE4_2 CRC32",
            ),
            (
                "-march=x86-64-v2 -mno-crc32",
                "SSE3 SSSE3 SSE4_1 SSE4_2 POPCNT",
            ),
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
            ("-msse4.2 -mno-popcnt", "SSE3 SSSE3 SSE4_1 SSE4_2 CRC32"),
            ("-mno-popcnt -msse4.2", "SSE3 SSSE3 SSE4_1 SSE4_2 CRC32"),
            ("-mpopcnt -mno-sse4.2", "POPCNT"),
            ("-msse4.2 -mpopcnt -mno-sse4.2", "POPCNT"),
            ("-msse4.1 -mno-ssse3", "SSE3"),
            ("-mno-ssse3 -msse4.1", "SSE3 SSSE3 SSE4_1"),
            ("-mcrc32", "CRC32"),
            ("-march=k8", ""),
            ("-march=nocona", "SSE3"),
            ("-march=core2", "SSE3 SSSE3"),
            ("-march=barcelona", "SSE3 POPCNT"),
            ("-march=btver1", "SSE3 SSSE3 POPCNT"),
            ("-march=nano-x2", "SSE3 SSSE3 SSE4_1"),
            ("-march=znver4", ALL),
            ("-march=x86-64-v4", ALL),
        ];
        let levels = ["SSE3", "SSSE3", "SSE4_1", "SSE4_2", "POPCNT", "CRC32"];
        for (flags, gcc) in cases {
            let owned: Vec<String> = flags.split(' ').map(String::from).collect();
            let got: Vec<&str> = X86Isa::from_flags(&owned)
                .macros()
                .iter()
                .map(|m| m.trim_matches('_'))
                .filter(|m| levels.contains(m))
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
        let exe = TlsPolicy::default();
        let shared = TlsPolicy {
            shared_code: true,
            ..TlsPolicy::default()
        };
        for arch in [Arch::X86_64, Arch::Aarch64] {
            let mac = Target::new(arch, Os::MacOS);
            assert_eq!(mac.tls_access(exe), TlsAccess::MachOTlv);
            assert_eq!(mac.tls_access(shared), TlsAccess::MachOTlv);
            let linux = Target::new(arch, Os::Linux);
            assert_eq!(linux.tls_access(exe), TlsAccess::ElfStatic);
            assert_eq!(linux.tls_access(shared), TlsAccess::ElfDescriptor);
            let bsd = Target::new(arch, Os::FreeBSD);
            assert_eq!(bsd.tls_access(exe), TlsAccess::ElfStatic);
            assert_eq!(bsd.tls_access(shared), TlsAccess::ElfStatic);
        }
        assert!(TlsAccess::MachOTlv.is_call() && TlsAccess::ElfDescriptor.is_call());
        assert!(!TlsAccess::ElfStatic.is_call());
    }

    /// `-ftls-model=` is the least optimized model an access may take: the
    /// access is that or what the code proves, whichever assumes more.
    #[test]
    fn test_tls_policy_floor() {
        use TlsModel::*;
        let policy = |shared_code, floor| TlsPolicy { shared_code, floor };
        // (shared code, floor) -> (dynamic, IE for a defined one, IE for an extern one)
        for (shared, floor, want) in [
            (false, GlobalDynamic, (false, false, true)),
            (false, LocalDynamic, (false, false, true)),
            (false, InitialExec, (false, false, true)),
            (false, LocalExec, (false, false, false)),
            (true, GlobalDynamic, (true, true, true)),
            (true, LocalDynamic, (true, true, true)),
            (true, InitialExec, (false, true, true)),
            (true, LocalExec, (false, false, false)),
        ] {
            let p = policy(shared, floor);
            assert_eq!(
                (p.dynamic(), p.initial_exec(false), p.initial_exec(true)),
                want,
                "{p:?}"
            );
        }
        for name in TlsModel::NAMES.split(' ') {
            assert!(TlsModel::from_name(name).is_some(), "{name}");
        }
        assert_eq!(TlsModel::from_name("local-exec"), Some(LocalExec));
        assert_eq!(TlsModel::from_name("bogus"), None);
        assert!(GlobalDynamic < LocalDynamic && InitialExec < LocalExec);
    }

    /// The `-fpic` family: each spelling, the per-target default, and what
    /// the macros report.
    #[test]
    fn test_position_independence_flags() {
        use PositionIndependence::*;
        let linux = Target::new(Arch::X86_64, Os::Linux);
        let mac = Target::new(Arch::Aarch64, Os::MacOS);
        assert_eq!(
            PositionIndependence::from_flag("-fpic"),
            Some(Pic(PicLevel::Small))
        );
        assert_eq!(
            PositionIndependence::from_flag("-fPIE"),
            Some(Pie(PicLevel::Large))
        );
        for flag in ["-fno-pic", "-fno-PIC", "-fno-pie", "-fno-PIE"] {
            assert_eq!(PositionIndependence::from_flag(flag), Some(Absolute));
        }
        assert_eq!(PositionIndependence::from_flag("-pie"), None);
        assert_eq!(
            PositionIndependence::target_default(&linux),
            Pie(PicLevel::Large)
        );
        assert_eq!(PositionIndependence::target_default(&mac), Absolute);
        assert!(Pie(PicLevel::Small).is_pic() && !Pie(PicLevel::Small).is_shared_code());
        assert!(Pic(PicLevel::Small).is_shared_code() && !Pic(PicLevel::Small).is_pie());
        assert_eq!(Absolute.macro_level(&linux), None);
        assert_eq!(Absolute.macro_level(&mac), Some(PicLevel::Large));
        assert_eq!(
            Pic(PicLevel::Small).macro_level(&linux),
            Some(PicLevel::Small)
        );
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
            "c9x",
            "c99",
            "c1x",
            "c11",
            "c17",
            "c18",
            "gnu9x",
            "gnu99",
            "gnu1x",
            "gnu11",
            "gnu17",
            "gnu18",
            "iso9899:199x",
            "iso9899:1999",
            "iso9899:2011",
            "iso9899:2017",
            "iso9899:2018",
        ] {
            assert_eq!(classify_std(spec), Some(StdRequest::Compiled), "{spec}");
        }

        for spec in [
            "c89",
            "c90",
            "gnu89",
            "gnu90",
            "iso9899:1990",
            "iso9899:199409",
        ] {
            assert_eq!(classify_std(spec), Some(StdRequest::Ignored), "{spec}");
        }

        for spec in [
            "c2x",
            "c23",
            "c2y",
            "gnu2x",
            "gnu23",
            "gnu2y",
            "iso9899:2024",
        ] {
            assert_eq!(classify_std(spec), None, "{spec}");
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
