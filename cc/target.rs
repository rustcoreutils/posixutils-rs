//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Target configuration for pcc
//
// Handles architecture, OS, and ABI-specific settings needed for
// preprocessing and code generation.
//

use std::fmt;

/// Target CPU architecture
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)] // Variants used via cfg-based detection and tests
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
#[allow(dead_code)] // Variants used via cfg-based detection and tests
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

        Self {
            arch,
            os,
            pointer_width,
            long_width,
            char_signed,
        }
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

    #[test]
    fn test_aarch64_linux() {
        let target = Target::new(Arch::Aarch64, Os::Linux);
        assert_eq!(target.arch, Arch::Aarch64);
        assert_eq!(target.os, Os::Linux);
        assert_eq!(target.pointer_width, 64);
        assert!(!target.char_signed); // ARM default
    }
}
