//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// DWARF debug information generation for pcc C99 compiler
//

use super::codegen::CodeGenBase;
use super::lir::{Directive, EmitAsm, LirInst, Symbol};

// ============================================================================
// DWARF Constants (DWARF Version 2)
// ============================================================================

// DWARF Tags (DW_TAG_*)
pub const DW_TAG_COMPILE_UNIT: u64 = 0x11;

// DWARF Attributes (DW_AT_*)
pub const DW_AT_NAME: u64 = 0x03;
pub const DW_AT_STMT_LIST: u64 = 0x10;
pub const DW_AT_LOW_PC: u64 = 0x11;
pub const DW_AT_HIGH_PC: u64 = 0x12;
pub const DW_AT_LANGUAGE: u64 = 0x13;
pub const DW_AT_COMP_DIR: u64 = 0x1b;
pub const DW_AT_PRODUCER: u64 = 0x25;

// DWARF Forms (DW_FORM_*)
pub const DW_FORM_ADDR: u64 = 0x01;
pub const DW_FORM_DATA4: u64 = 0x06;
pub const DW_FORM_STRING: u64 = 0x08;

// DWARF Language Codes (DW_LANG_*)
pub const DW_LANG_C99: u64 = 0x0c;

// DWARF Children flag
pub const DW_CHILDREN_NO: u64 = 0x00;

// ============================================================================
// DWARF Generation Functions
// ============================================================================

/// Generate the abbreviation table for a minimal compile unit.
/// This defines the structure of DIEs (Debug Information Entries).
pub fn generate_abbrev_table<I: LirInst + EmitAsm>(base: &mut CodeGenBase<I>) {
    // Switch to .debug_abbrev section
    base.push_directive(Directive::DebugAbbrev);
    base.push_directive(Directive::local_label(".Ldebug_abbrev0"));

    // Abbreviation 1: DW_TAG_compile_unit (no children)
    base.push_directive(Directive::Uleb128(1)); // Abbreviation code
    base.push_directive(Directive::Uleb128(DW_TAG_COMPILE_UNIT));
    base.push_directive(Directive::Byte(DW_CHILDREN_NO as i64));

    // Attribute: DW_AT_producer (string)
    base.push_directive(Directive::Uleb128(DW_AT_PRODUCER));
    base.push_directive(Directive::Uleb128(DW_FORM_STRING));

    // Attribute: DW_AT_language (4-byte data)
    base.push_directive(Directive::Uleb128(DW_AT_LANGUAGE));
    base.push_directive(Directive::Uleb128(DW_FORM_DATA4));

    // Attribute: DW_AT_name (string)
    base.push_directive(Directive::Uleb128(DW_AT_NAME));
    base.push_directive(Directive::Uleb128(DW_FORM_STRING));

    // Attribute: DW_AT_comp_dir (string)
    base.push_directive(Directive::Uleb128(DW_AT_COMP_DIR));
    base.push_directive(Directive::Uleb128(DW_FORM_STRING));

    // Attribute: DW_AT_stmt_list (4-byte offset to .debug_line)
    base.push_directive(Directive::Uleb128(DW_AT_STMT_LIST));
    base.push_directive(Directive::Uleb128(DW_FORM_DATA4));

    // Attribute: DW_AT_low_pc (address)
    base.push_directive(Directive::Uleb128(DW_AT_LOW_PC));
    base.push_directive(Directive::Uleb128(DW_FORM_ADDR));

    // Attribute: DW_AT_high_pc (address)
    base.push_directive(Directive::Uleb128(DW_AT_HIGH_PC));
    base.push_directive(Directive::Uleb128(DW_FORM_ADDR));

    // End of attributes for this abbreviation
    base.push_directive(Directive::Byte(0));
    base.push_directive(Directive::Byte(0));

    // End of abbreviation table
    base.push_directive(Directive::Byte(0));
}

/// Generate the .debug_info section with compile unit DIE.
///
/// # Arguments
/// * `base` - CodeGenBase to push directives to
/// * `producer` - Compiler identification string (e.g., "pcc 0.7.0")
/// * `source_name` - Primary source filename
/// * `comp_dir` - Compilation directory
/// * `low_pc_label` - Label for start of code (e.g., ".Ltext0")
/// * `high_pc_label` - Label for end of code (e.g., ".Ltext_end")
pub fn generate_debug_info<I: LirInst + EmitAsm>(
    base: &mut CodeGenBase<I>,
    producer: &str,
    source_name: &str,
    comp_dir: &str,
    low_pc_label: &str,
    high_pc_label: &str,
) {
    // Switch to .debug_info section
    base.push_directive(Directive::DebugInfo);

    // Unit length (will be computed by assembler/linker)
    // Use label arithmetic: .Ldebug_info_end - .Ldebug_info_start
    base.push_directive(Directive::local_label(".Ldebug_info0"));
    base.push_directive(Directive::Raw(
        "    .long .Ldebug_info_end - .Ldebug_info_start".into(),
    ));
    base.push_directive(Directive::local_label(".Ldebug_info_start"));

    // DWARF version (2)
    base.push_directive(Directive::TwoBytes(2));

    // Abbrev offset (offset into .debug_abbrev, always 0 for single CU)
    base.push_directive(Directive::Long(0));

    // Address size (8 bytes for 64-bit)
    base.push_directive(Directive::Byte(8));

    // Compile unit DIE using abbreviation code 1
    base.push_directive(Directive::Uleb128(1));

    // DW_AT_producer (inline string)
    base.push_directive(Directive::Asciz(producer.to_string()));

    // DW_AT_language (C99 = 12)
    base.push_directive(Directive::Long(DW_LANG_C99 as i64));

    // DW_AT_name (source filename)
    base.push_directive(Directive::Asciz(source_name.to_string()));

    // DW_AT_comp_dir (compilation directory)
    base.push_directive(Directive::Asciz(comp_dir.to_string()));

    // DW_AT_stmt_list (offset into .debug_line, 0 to reference start)
    base.push_directive(Directive::Long(0));

    // DW_AT_low_pc (start of code)
    base.push_directive(Directive::QuadSym(Symbol::local(low_pc_label)));

    // DW_AT_high_pc (end of code)
    base.push_directive(Directive::QuadSym(Symbol::local(high_pc_label)));

    // End label for unit length computation
    base.push_directive(Directive::local_label(".Ldebug_info_end"));
}
