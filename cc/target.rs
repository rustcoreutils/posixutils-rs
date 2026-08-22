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

impl Os {
    /// Returns the libc function name for signbit(double)
    /// macOS/Darwin uses __signbitd, Linux/glibc and FreeBSD use __signbit
    pub fn signbit_double_fn(&self) -> &'static str {
        match self {
            Os::MacOS => "__signbitd",
            Os::Linux | Os::FreeBSD => "__signbit",
        }
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
    /// char is signed by default
    pub char_signed: bool,
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

        // char signedness varies by platform
        let char_signed = match (arch, os) {
            // ARM defaults to unsigned char
            (Arch::Aarch64, _) => false,
            // x86_64 defaults to signed char
            (Arch::X86_64, _) => true,
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
            char_signed,
            max_aggregate_register_bits,
        }
    }

    /// Does this platform spell the exact-width 64-bit integers `long long`?
    ///
    /// Every target here is LP64, so `long` is 64 bits and either spelling is
    /// wide enough — but they are *distinct types*, and our `<stdint.h>` has to
    /// name the same one the host's headers do or a translation unit including
    /// both is rejected. Linux and the BSDs use `long`; Darwin uses
    /// `long long`, and picking by width alone made every macOS build that
    /// reached a system header fail on `int64_t`.
    pub fn int64_is_long_long(&self) -> bool {
        self.os == Os::MacOS
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
        } else {
            // Default based on arch
            Os::Linux
        };

        Some(Self::new(arch, os))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        assert!(target.char_signed); // x86 default
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
        assert!(!target.char_signed); // ARM default
    }
}
