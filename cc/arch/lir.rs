//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Low-level Intermediate Representation (LIR) - Shared Types
//
// Common types used across architecture-specific LIR implementations.
// Each architecture (x86-64, AArch64) has its own LIR instruction enum
// that uses these shared operand and size types.
//

use crate::target::{Os, Target};
use std::fmt::{self, Write};

// ============================================================================
// Operand Size
// ============================================================================

/// Size specifier for operations (in bits)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OperandSize {
    /// 8-bit (byte)
    B8,
    /// 16-bit (word on x86, halfword on ARM)
    B16,
    /// 32-bit (dword on x86, word on ARM)
    B32,
    /// 64-bit (qword on x86, doubleword on ARM)
    B64,
}

impl OperandSize {
    /// Create from bit count
    pub fn from_bits(bits: u32) -> Self {
        match bits {
            0..=8 => OperandSize::B8,
            9..=16 => OperandSize::B16,
            17..=32 => OperandSize::B32,
            _ => OperandSize::B64,
        }
    }

    /// Get bit count
    pub fn bits(&self) -> u32 {
        match self {
            OperandSize::B8 => 8,
            OperandSize::B16 => 16,
            OperandSize::B32 => 32,
            OperandSize::B64 => 64,
        }
    }

    /// x86-64 AT&T syntax suffix (b, w, l, q)
    pub fn x86_suffix(&self) -> &'static str {
        match self {
            OperandSize::B8 => "b",
            OperandSize::B16 => "w",
            OperandSize::B32 => "l",
            OperandSize::B64 => "q",
        }
    }
}

impl fmt::Display for OperandSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.bits())
    }
}

// ============================================================================
// Floating-Point Size
// ============================================================================

/// Floating-point size specifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FpSize {
    /// 32-bit single precision (float)
    Single,
    /// 64-bit double precision (double)
    Double,
}

impl FpSize {
    /// Create from bit count
    pub fn from_bits(bits: u32) -> Self {
        if bits <= 32 {
            FpSize::Single
        } else {
            FpSize::Double
        }
    }

    /// x86-64 SSE instruction suffix (ss for single, sd for double)
    pub fn x86_suffix(&self) -> &'static str {
        match self {
            FpSize::Single => "ss",
            FpSize::Double => "sd",
        }
    }

    /// x86-64 packed suffix (ps for single, pd for double)
    pub fn x86_packed_suffix(&self) -> &'static str {
        match self {
            FpSize::Single => "ps",
            FpSize::Double => "pd",
        }
    }
}

impl fmt::Display for FpSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FpSize::Single => write!(f, "f32"),
            FpSize::Double => write!(f, "f64"),
        }
    }
}

// ============================================================================
// Labels and Symbols
// ============================================================================

/// Label for local jumps (basic block targets within a function)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label {
    /// Function name (for unique label generation)
    pub func_name: String,
    /// Basic block ID within the function
    pub block_id: u32,
}

impl Label {
    pub fn new(func_name: impl Into<String>, block_id: u32) -> Self {
        Self {
            func_name: func_name.into(),
            block_id,
        }
    }

    /// Format as assembly label name
    pub fn name(&self) -> String {
        format!(".L{}_{}", self.func_name, self.block_id)
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// Global symbol reference (function, global variable, string literal)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    /// Symbol name
    pub name: String,
    /// Is this a local symbol (starts with '.')
    pub is_local: bool,
}

impl Symbol {
    pub fn local(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            is_local: true,
        }
    }

    pub fn global(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            is_local: false,
        }
    }

    /// Format symbol name for the target OS
    /// macOS prepends underscore to external symbols
    pub fn format_for_target(&self, target: &Target) -> String {
        if self.is_local {
            // Local symbols don't get underscore prefix
            self.name.clone()
        } else {
            match target.os {
                Os::MacOS => format!("_{}", self.name),
                Os::Linux | Os::FreeBSD => self.name.clone(),
            }
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

// ============================================================================
// Symbol Type (for ELF .type directive)
// ============================================================================

/// Symbol type for ELF .type directive
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolType {
    /// @function
    Function,
    /// @object
    Object,
}

impl SymbolType {
    pub fn as_str(&self) -> &'static str {
        match self {
            SymbolType::Function => "@function",
            SymbolType::Object => "@object",
        }
    }
}

// ============================================================================
// EmitAsm Trait
// ============================================================================

/// Trait for LIR instructions that can be emitted to assembly text
pub trait EmitAsm {
    /// Emit assembly text for this instruction
    fn emit(&self, target: &Target, out: &mut String);
}

// ============================================================================
// Assembler Directives (Architecture-Independent)
// ============================================================================

/// Assembler directives that are common across architectures.
/// These control the assembler behavior, emit data, or provide metadata.
#[derive(Debug, Clone, PartialEq)]
pub enum Directive {
    // ========================================================================
    // Labels
    // ========================================================================
    /// Local label for basic blocks within a function
    BlockLabel(Label),

    /// Global/function label (emits "name:" or "_name:" on macOS)
    GlobalLabel(Symbol),

    // ========================================================================
    // Sections
    // ========================================================================
    /// Switch to text section (.text or .section __TEXT,__text)
    Text,

    /// Switch to data section (.data or .section __DATA,__data)
    Data,

    /// Switch to read-only data section (.section .rodata or __TEXT,__cstring)
    Rodata,

    // ========================================================================
    // Symbol Visibility and Attributes
    // ========================================================================
    /// .globl symbol - mark symbol as globally visible
    Global(Symbol),

    /// .type symbol, @function/@object (ELF only)
    Type { sym: Symbol, kind: SymbolType },

    /// .size symbol, size (ELF only)
    Size { sym: Symbol, size: u32 },

    /// .comm symbol, size, align - allocate common (BSS) storage
    Comm { sym: Symbol, size: u32, align: u32 },

    // ========================================================================
    // Alignment
    // ========================================================================
    /// .p2align power (macOS) or .align bytes (Linux)
    /// Stores power of 2, emits appropriately for platform
    Align(u32),

    // ========================================================================
    // Data Emission
    // ========================================================================
    /// .zero N - emit N zero bytes
    Zero(u32),

    /// .byte value - emit 8-bit value
    Byte(i64),

    /// .short/.value value - emit 16-bit value
    Short(i64),

    /// .long value - emit 32-bit value
    Long(i64),

    /// .quad value - emit 64-bit value
    Quad(i64),

    /// .asciz "string" - emit null-terminated string
    Asciz(String),

    // ========================================================================
    // CFI (Call Frame Information) Directives
    // ========================================================================
    /// .cfi_startproc - begin procedure CFI
    CfiStartProc,

    /// .cfi_endproc - end procedure CFI
    CfiEndProc,

    /// .cfi_def_cfa register, offset - set CFA to register + offset
    CfiDefCfa { reg: String, offset: i32 },

    /// .cfi_def_cfa_offset offset - set CFA offset from current register
    CfiDefCfaOffset(i32),

    /// .cfi_offset register, offset - register saved at CFA+offset
    CfiOffset { reg: String, offset: i32 },

    /// .cfi_def_cfa_register register - CFA is now based on this register
    CfiDefCfaRegister(String),

    // ========================================================================
    // Debug Information
    // ========================================================================
    /// .file index "path" - declare source file for debug info
    File { index: u32, path: String },

    /// .loc file line column - source location for debug info
    Loc { file: u32, line: u32, col: u32 },

    // ========================================================================
    // Misc
    // ========================================================================
    /// Assembly comment (prefixed with #)
    Comment(String),

    /// Blank line for readability
    Blank,

    /// Raw assembly line (escape hatch for special cases)
    Raw(String),
}

impl Directive {
    // Convenience constructors

    pub fn global_label(name: impl Into<String>) -> Self {
        Directive::GlobalLabel(Symbol::global(name))
    }

    pub fn local_label(name: impl Into<String>) -> Self {
        Directive::GlobalLabel(Symbol::local(name))
    }

    pub fn loc(file: u32, line: u32, col: u32) -> Self {
        Directive::Loc { file, line, col }
    }

    pub fn global(name: impl Into<String>) -> Self {
        Directive::Global(Symbol::global(name))
    }

    pub fn type_func(name: impl Into<String>) -> Self {
        Directive::Type {
            sym: Symbol::global(name),
            kind: SymbolType::Function,
        }
    }

    pub fn type_object(name: impl Into<String>) -> Self {
        Directive::Type {
            sym: Symbol::global(name),
            kind: SymbolType::Object,
        }
    }

    pub fn size(name: impl Into<String>, size: u32) -> Self {
        Directive::Size {
            sym: Symbol::global(name),
            size,
        }
    }

    pub fn comm(name: impl Into<String>, size: u32, align: u32) -> Self {
        Directive::Comm {
            sym: Symbol::global(name),
            size,
            align,
        }
    }

    pub fn file(index: u32, path: impl Into<String>) -> Self {
        Directive::File {
            index,
            path: path.into(),
        }
    }

    pub fn cfi_offset(reg: impl Into<String>, offset: i32) -> Self {
        Directive::CfiOffset {
            reg: reg.into(),
            offset,
        }
    }

    pub fn cfi_def_cfa_register(reg: impl Into<String>) -> Self {
        Directive::CfiDefCfaRegister(reg.into())
    }

    pub fn cfi_def_cfa(reg: impl Into<String>, offset: i32) -> Self {
        Directive::CfiDefCfa {
            reg: reg.into(),
            offset,
        }
    }
}

impl EmitAsm for Directive {
    fn emit(&self, target: &Target, out: &mut String) {
        match self {
            // Labels
            Directive::BlockLabel(lbl) => {
                let _ = writeln!(out, "{}:", lbl.name());
            }
            Directive::GlobalLabel(sym) => {
                let _ = writeln!(out, "{}:", sym.format_for_target(target));
            }

            // Sections
            Directive::Text => match target.os {
                Os::MacOS => {
                    let _ = writeln!(out, ".section __TEXT,__text");
                }
                Os::Linux | Os::FreeBSD => {
                    let _ = writeln!(out, ".text");
                }
            },
            Directive::Data => match target.os {
                Os::MacOS => {
                    let _ = writeln!(out, ".section __DATA,__data");
                }
                Os::Linux | Os::FreeBSD => {
                    let _ = writeln!(out, ".data");
                }
            },
            Directive::Rodata => match target.os {
                Os::MacOS => {
                    let _ = writeln!(out, ".section __TEXT,__cstring,cstring_literals");
                }
                Os::Linux | Os::FreeBSD => {
                    let _ = writeln!(out, ".section .rodata");
                }
            },

            // Symbol visibility
            Directive::Global(sym) => {
                let _ = writeln!(out, ".globl {}", sym.format_for_target(target));
            }
            Directive::Type { sym, kind } => {
                // ELF only - skip on macOS
                if !matches!(target.os, Os::MacOS) {
                    let _ = writeln!(out, ".type {}, {}", sym.name, kind.as_str());
                }
            }
            Directive::Size { sym, size } => {
                // ELF only - skip on macOS
                if !matches!(target.os, Os::MacOS) {
                    let _ = writeln!(out, ".size {}, {}", sym.name, size);
                }
            }
            Directive::Comm { sym, size, align } => {
                let _ = writeln!(
                    out,
                    ".comm {},{},{}",
                    sym.format_for_target(target),
                    size,
                    align
                );
            }

            // Alignment
            Directive::Align(power) => match target.os {
                Os::MacOS => {
                    let _ = writeln!(out, ".p2align {}", power);
                }
                Os::Linux | Os::FreeBSD => {
                    // Linux .align takes bytes, not power of 2
                    let _ = writeln!(out, ".align {}", 1u32 << power);
                }
            },

            // Data emission
            Directive::Zero(n) => {
                let _ = writeln!(out, "    .zero {}", n);
            }
            Directive::Byte(v) => {
                let _ = writeln!(out, "    .byte {}", v);
            }
            Directive::Short(v) => {
                let _ = writeln!(out, "    .short {}", v);
            }
            Directive::Long(v) => {
                let _ = writeln!(out, "    .long {}", v);
            }
            Directive::Quad(v) => {
                let _ = writeln!(out, "    .quad {}", v);
            }
            Directive::Asciz(s) => {
                let _ = writeln!(out, "    .asciz \"{}\"", s);
            }

            // CFI directives
            Directive::CfiStartProc => {
                let _ = writeln!(out, "    .cfi_startproc");
            }
            Directive::CfiEndProc => {
                let _ = writeln!(out, "    .cfi_endproc");
            }
            Directive::CfiDefCfa { reg, offset } => {
                let _ = writeln!(out, "    .cfi_def_cfa {}, {}", reg, offset);
            }
            Directive::CfiDefCfaOffset(offset) => {
                let _ = writeln!(out, "    .cfi_def_cfa_offset {}", offset);
            }
            Directive::CfiOffset { reg, offset } => {
                let _ = writeln!(out, "    .cfi_offset {}, {}", reg, offset);
            }
            Directive::CfiDefCfaRegister(reg) => {
                let _ = writeln!(out, "    .cfi_def_cfa_register {}", reg);
            }

            // Debug info
            Directive::File { index, path } => {
                let _ = writeln!(out, "    .file {} \"{}\"", index, path);
            }
            Directive::Loc { file, line, col } => {
                let _ = writeln!(out, "    .loc {} {} {}", file, line, col);
            }

            // Misc
            Directive::Comment(text) => {
                let _ = writeln!(out, "# {}", text);
            }
            Directive::Blank => {
                let _ = writeln!(out);
            }
            Directive::Raw(text) => {
                let _ = writeln!(out, "{}", text);
            }
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::target::Arch;

    #[test]
    fn test_operand_size() {
        assert_eq!(OperandSize::from_bits(1), OperandSize::B8);
        assert_eq!(OperandSize::from_bits(8), OperandSize::B8);
        assert_eq!(OperandSize::from_bits(16), OperandSize::B16);
        assert_eq!(OperandSize::from_bits(32), OperandSize::B32);
        assert_eq!(OperandSize::from_bits(64), OperandSize::B64);

        assert_eq!(OperandSize::B8.bits(), 8);

        assert_eq!(OperandSize::B8.x86_suffix(), "b");
        assert_eq!(OperandSize::B16.x86_suffix(), "w");
        assert_eq!(OperandSize::B32.x86_suffix(), "l");
        assert_eq!(OperandSize::B64.x86_suffix(), "q");
    }

    #[test]
    fn test_fp_size() {
        assert_eq!(FpSize::from_bits(32), FpSize::Single);
        assert_eq!(FpSize::from_bits(64), FpSize::Double);

        assert_eq!(FpSize::Single.x86_suffix(), "ss");
        assert_eq!(FpSize::Double.x86_suffix(), "sd");
    }

    #[test]
    fn test_label() {
        let label = Label::new("main", 5);
        assert_eq!(label.name(), ".Lmain_5");
    }

    #[test]
    fn test_symbol_formatting() {
        let sym = Symbol::global("printf");

        let linux = Target::new(Arch::X86_64, Os::Linux);
        let macos = Target::new(Arch::X86_64, Os::MacOS);

        assert_eq!(sym.format_for_target(&linux), "printf");
        assert_eq!(sym.format_for_target(&macos), "_printf");

        // Local symbols don't get underscore
        let local = Symbol::local(".LC0");
        assert_eq!(local.format_for_target(&macos), ".LC0");
    }

    #[test]
    fn test_directive_loc() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let dir = Directive::loc(1, 42, 5);
        let mut out = String::new();
        dir.emit(&target, &mut out);
        assert_eq!(out, "    .loc 1 42 5\n");
    }

    #[test]
    fn test_directive_global_label() {
        let linux = Target::new(Arch::X86_64, Os::Linux);
        let macos = Target::new(Arch::X86_64, Os::MacOS);
        let dir = Directive::global_label("my_func");

        let mut out = String::new();
        dir.emit(&linux, &mut out);
        assert_eq!(out, "my_func:\n");

        let mut out = String::new();
        dir.emit(&macos, &mut out);
        assert_eq!(out, "_my_func:\n");
    }

    #[test]
    fn test_directive_sections() {
        let linux = Target::new(Arch::X86_64, Os::Linux);
        let macos = Target::new(Arch::X86_64, Os::MacOS);

        // Text section
        let mut out = String::new();
        Directive::Text.emit(&linux, &mut out);
        assert_eq!(out, ".text\n");

        let mut out = String::new();
        Directive::Text.emit(&macos, &mut out);
        assert_eq!(out, ".section __TEXT,__text\n");

        // Data section
        let mut out = String::new();
        Directive::Data.emit(&linux, &mut out);
        assert_eq!(out, ".data\n");

        let mut out = String::new();
        Directive::Data.emit(&macos, &mut out);
        assert_eq!(out, ".section __DATA,__data\n");
    }

    #[test]
    fn test_directive_global() {
        let linux = Target::new(Arch::X86_64, Os::Linux);
        let macos = Target::new(Arch::X86_64, Os::MacOS);

        let mut out = String::new();
        Directive::global("main").emit(&linux, &mut out);
        assert_eq!(out, ".globl main\n");

        let mut out = String::new();
        Directive::global("main").emit(&macos, &mut out);
        assert_eq!(out, ".globl _main\n");
    }

    #[test]
    fn test_directive_type_elf_only() {
        let linux = Target::new(Arch::X86_64, Os::Linux);
        let macos = Target::new(Arch::X86_64, Os::MacOS);

        let mut out = String::new();
        Directive::type_func("main").emit(&linux, &mut out);
        assert_eq!(out, ".type main, @function\n");

        // Should be empty on macOS
        let mut out = String::new();
        Directive::type_func("main").emit(&macos, &mut out);
        assert_eq!(out, "");
    }

    #[test]
    fn test_directive_align() {
        let linux = Target::new(Arch::X86_64, Os::Linux);
        let macos = Target::new(Arch::X86_64, Os::MacOS);

        // Align to 16 bytes (power=4, 2^4=16)
        let mut out = String::new();
        Directive::Align(4).emit(&linux, &mut out);
        assert_eq!(out, ".align 16\n");

        let mut out = String::new();
        Directive::Align(4).emit(&macos, &mut out);
        assert_eq!(out, ".p2align 4\n");
    }

    #[test]
    fn test_directive_data_emission() {
        let target = Target::new(Arch::X86_64, Os::Linux);

        let mut out = String::new();
        Directive::Zero(16).emit(&target, &mut out);
        assert_eq!(out, "    .zero 16\n");

        let mut out = String::new();
        Directive::Byte(42).emit(&target, &mut out);
        assert_eq!(out, "    .byte 42\n");

        let mut out = String::new();
        Directive::Long(0x12345678).emit(&target, &mut out);
        assert_eq!(out, "    .long 305419896\n");
    }

    #[test]
    fn test_directive_cfi() {
        let target = Target::new(Arch::X86_64, Os::Linux);

        let mut out = String::new();
        Directive::CfiStartProc.emit(&target, &mut out);
        assert_eq!(out, "    .cfi_startproc\n");

        let mut out = String::new();
        Directive::CfiDefCfaOffset(16).emit(&target, &mut out);
        assert_eq!(out, "    .cfi_def_cfa_offset 16\n");

        let mut out = String::new();
        Directive::cfi_offset("%rbp", -16).emit(&target, &mut out);
        assert_eq!(out, "    .cfi_offset %rbp, -16\n");
    }

    #[test]
    fn test_directive_comm() {
        let linux = Target::new(Arch::X86_64, Os::Linux);
        let macos = Target::new(Arch::X86_64, Os::MacOS);

        let mut out = String::new();
        Directive::comm("my_var", 8, 8).emit(&linux, &mut out);
        assert_eq!(out, ".comm my_var,8,8\n");

        let mut out = String::new();
        Directive::comm("my_var", 8, 8).emit(&macos, &mut out);
        assert_eq!(out, ".comm _my_var,8,8\n");
    }
}
