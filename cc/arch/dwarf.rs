//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// DWARF debug information generation for c17 C17 compiler
//

use super::codegen::CodeGenBase;
use super::lir::{Directive, EmitAsm, LirInst, Symbol};
use crate::types::{TypeId, TypeKind, TypeTable};
use std::collections::HashMap;

// DWARF Constants (DWARF Version 2)

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
pub const DW_CHILDREN_YES: u64 = 0x01;

// Tags for the DIEs that describe functions and the things inside them.
pub const DW_TAG_ARRAY_TYPE: u64 = 0x01;
pub const DW_TAG_FORMAL_PARAMETER: u64 = 0x05;
pub const DW_TAG_POINTER_TYPE: u64 = 0x0f;
pub const DW_TAG_STRUCTURE_TYPE: u64 = 0x13;
pub const DW_TAG_UNION_TYPE: u64 = 0x17;
pub const DW_TAG_BASE_TYPE: u64 = 0x24;
pub const DW_TAG_SUBPROGRAM: u64 = 0x2e;
pub const DW_TAG_VARIABLE: u64 = 0x34;
pub const DW_TAG_SUBRANGE_TYPE: u64 = 0x21;

pub const DW_AT_BYTE_SIZE: u64 = 0x0b;
pub const DW_AT_ENCODING: u64 = 0x3e;
pub const DW_AT_EXTERNAL: u64 = 0x3f;
pub const DW_AT_LOCATION: u64 = 0x02;
pub const DW_AT_UPPER_BOUND: u64 = 0x2f;
pub const DW_AT_DECL_FILE: u64 = 0x3a;
pub const DW_AT_DECL_LINE: u64 = 0x3b;
pub const DW_AT_TYPE: u64 = 0x49;

pub const DW_FORM_BLOCK1: u64 = 0x0a;
pub const DW_FORM_DATA1: u64 = 0x0b;
pub const DW_FORM_FLAG: u64 = 0x0c;
pub const DW_FORM_REF4: u64 = 0x13;

// Base-type encodings (DW_ATE_*)
pub const DW_ATE_BOOLEAN: u64 = 0x02;
pub const DW_ATE_FLOAT: u64 = 0x04;
pub const DW_ATE_SIGNED: u64 = 0x05;
pub const DW_ATE_SIGNED_CHAR: u64 = 0x06;
pub const DW_ATE_UNSIGNED: u64 = 0x07;
pub const DW_ATE_UNSIGNED_CHAR: u64 = 0x08;

/// `DW_OP_breg0` -- register N's value plus a signed offset.
pub const DW_OP_BREG0: i64 = 0x70;

// Abbreviation codes. One per DIE shape; `.debug_info` stores the code and the
// attribute values, and this table says which attributes follow.
const ABBREV_CU: u64 = 1;
const ABBREV_SUBPROGRAM_TYPED: u64 = 2;
const ABBREV_SUBPROGRAM_VOID: u64 = 3;
const ABBREV_PARAM_LOC: u64 = 4;
const ABBREV_PARAM_NOLOC: u64 = 5;
const ABBREV_VAR_LOC: u64 = 6;
const ABBREV_VAR_NOLOC: u64 = 7;
const ABBREV_BASE_TYPE: u64 = 8;
const ABBREV_POINTER: u64 = 9;
const ABBREV_POINTER_VOID: u64 = 10;
const ABBREV_ARRAY: u64 = 11;
const ABBREV_SUBRANGE: u64 = 12;
const ABBREV_STRUCT: u64 = 13;
const ABBREV_UNION: u64 = 14;

/// Where a variable lives, as the code generator computed it: a register and a
/// byte offset from it.
///
/// Taken from the same address computation the loads and stores use, rather
/// than re-derived from the frame layout here. That is what makes it right for
/// an over-aligned frame, whose locals are not addressed from the frame
/// pointer at all -- and it means a location can never disagree with the code.
#[derive(Debug, Clone, Copy)]
pub struct VarLocation {
    /// The DWARF register number of the base.
    pub reg: u16,
    /// Byte displacement from that register.
    pub offset: i64,
}

/// A parameter or local a debugger should be able to name.
#[derive(Debug, Clone)]
pub struct VarDie {
    pub name: String,
    pub typ: TypeId,
    pub decl_line: u32,
    pub is_param: bool,
    /// `None` when the variable has no stack home -- promoted to a register,
    /// or optimized away. Emitting a location then would be a lie, and gdb
    /// says "optimized out" on its own when there is none.
    pub loc: Option<VarLocation>,
}

/// A function a debugger should be able to enter.
#[derive(Debug, Clone)]
pub struct FnDie {
    pub name: String,
    pub external: bool,
    pub decl_line: u32,
    /// Label marking the first byte past the function, for `DW_AT_high_pc`.
    pub end_label: String,
    /// `None` for a `void` function.
    pub ret_typ: Option<TypeId>,
    pub vars: Vec<VarDie>,
}

// DWARF Generation Functions

/// Generate the abbreviation table for a minimal compile unit.
/// This defines the structure of DIEs (Debug Information Entries).
/// One abbreviation: its code, tag, whether it has children, and the
/// (attribute, form) pairs that follow in `.debug_info`.
fn abbrev<I: LirInst + EmitAsm>(
    base: &mut CodeGenBase<I>,
    code: u64,
    tag: u64,
    children: u64,
    attrs: &[(u64, u64)],
) {
    base.push_directive(Directive::Uleb128(code));
    base.push_directive(Directive::Uleb128(tag));
    base.push_directive(Directive::Byte(children as i64));
    for (at, form) in attrs {
        base.push_directive(Directive::Uleb128(*at));
        base.push_directive(Directive::Uleb128(*form));
    }
    // (0, 0) ends the attribute list.
    base.push_directive(Directive::Uleb128(0));
    base.push_directive(Directive::Uleb128(0));
}

/// Generate the abbreviation table: the shapes of DIE that `.debug_info` uses.
///
/// This was a single entry -- a compile unit with no children -- so a c17
/// binary described its files and nothing inside them. gdb fell back to the
/// ELF symbol table for function names, which is why backtraces named
/// functions but `info args`, `info locals` and `ptype` all came back empty.
///
/// Several shapes come in pairs because DWARF has no "absent" value: a
/// function with no return type and one with a return type are different
/// shapes, as are a variable that has a location and one that does not.
pub fn generate_abbrev_table<I: LirInst + EmitAsm>(base: &mut CodeGenBase<I>) {
    base.push_directive(Directive::DebugAbbrev);
    base.push_directive(Directive::local_label(".Ldebug_abbrev0"));

    abbrev(
        base,
        ABBREV_CU,
        DW_TAG_COMPILE_UNIT,
        DW_CHILDREN_YES,
        &[
            (DW_AT_PRODUCER, DW_FORM_STRING),
            (DW_AT_LANGUAGE, DW_FORM_DATA4),
            (DW_AT_NAME, DW_FORM_STRING),
            (DW_AT_COMP_DIR, DW_FORM_STRING),
            (DW_AT_STMT_LIST, DW_FORM_DATA4),
            (DW_AT_LOW_PC, DW_FORM_ADDR),
            (DW_AT_HIGH_PC, DW_FORM_ADDR),
        ],
    );

    let subprogram_attrs: &[(u64, u64)] = &[
        (DW_AT_EXTERNAL, DW_FORM_FLAG),
        (DW_AT_NAME, DW_FORM_STRING),
        (DW_AT_DECL_FILE, DW_FORM_DATA1),
        (DW_AT_DECL_LINE, DW_FORM_DATA4),
        (DW_AT_LOW_PC, DW_FORM_ADDR),
        (DW_AT_HIGH_PC, DW_FORM_ADDR),
    ];
    let mut typed: Vec<(u64, u64)> = subprogram_attrs.to_vec();
    typed.push((DW_AT_TYPE, DW_FORM_REF4));
    abbrev(
        base,
        ABBREV_SUBPROGRAM_TYPED,
        DW_TAG_SUBPROGRAM,
        DW_CHILDREN_YES,
        &typed,
    );
    abbrev(
        base,
        ABBREV_SUBPROGRAM_VOID,
        DW_TAG_SUBPROGRAM,
        DW_CHILDREN_YES,
        subprogram_attrs,
    );

    // A parameter and a local differ only by tag; both come with and without a
    // location.
    let var_attrs: &[(u64, u64)] = &[
        (DW_AT_NAME, DW_FORM_STRING),
        (DW_AT_DECL_FILE, DW_FORM_DATA1),
        (DW_AT_DECL_LINE, DW_FORM_DATA4),
        (DW_AT_TYPE, DW_FORM_REF4),
    ];
    let mut with_loc: Vec<(u64, u64)> = var_attrs.to_vec();
    with_loc.push((DW_AT_LOCATION, DW_FORM_BLOCK1));
    for (code_loc, code_noloc, tag) in [
        (
            ABBREV_PARAM_LOC,
            ABBREV_PARAM_NOLOC,
            DW_TAG_FORMAL_PARAMETER,
        ),
        (ABBREV_VAR_LOC, ABBREV_VAR_NOLOC, DW_TAG_VARIABLE),
    ] {
        abbrev(base, code_loc, tag, DW_CHILDREN_NO, &with_loc);
        abbrev(base, code_noloc, tag, DW_CHILDREN_NO, var_attrs);
    }

    abbrev(
        base,
        ABBREV_BASE_TYPE,
        DW_TAG_BASE_TYPE,
        DW_CHILDREN_NO,
        &[
            (DW_AT_NAME, DW_FORM_STRING),
            (DW_AT_BYTE_SIZE, DW_FORM_DATA1),
            (DW_AT_ENCODING, DW_FORM_DATA1),
        ],
    );
    abbrev(
        base,
        ABBREV_POINTER,
        DW_TAG_POINTER_TYPE,
        DW_CHILDREN_NO,
        &[(DW_AT_BYTE_SIZE, DW_FORM_DATA1), (DW_AT_TYPE, DW_FORM_REF4)],
    );
    // `void *` has no pointee DIE to name.
    abbrev(
        base,
        ABBREV_POINTER_VOID,
        DW_TAG_POINTER_TYPE,
        DW_CHILDREN_NO,
        &[(DW_AT_BYTE_SIZE, DW_FORM_DATA1)],
    );
    abbrev(
        base,
        ABBREV_ARRAY,
        DW_TAG_ARRAY_TYPE,
        DW_CHILDREN_YES,
        &[(DW_AT_TYPE, DW_FORM_REF4)],
    );
    abbrev(
        base,
        ABBREV_SUBRANGE,
        DW_TAG_SUBRANGE_TYPE,
        DW_CHILDREN_NO,
        &[(DW_AT_UPPER_BOUND, DW_FORM_DATA4)],
    );
    // Aggregates carry their size but no members: a member's *name* is a
    // `StringId`, and no string table reaches the backend. gdb lists a variable
    // of such a type and knows how big it is, and prints it as an aggregate
    // whose fields it cannot name.
    for (code, tag) in [
        (ABBREV_STRUCT, DW_TAG_STRUCTURE_TYPE),
        (ABBREV_UNION, DW_TAG_UNION_TYPE),
    ] {
        abbrev(
            base,
            code,
            tag,
            DW_CHILDREN_NO,
            &[(DW_AT_BYTE_SIZE, DW_FORM_DATA4)],
        );
    }

    // A zero code ends the table.
    base.push_directive(Directive::Uleb128(0));
}

/// The type DIEs a set of functions needs, and where each one will sit.
///
/// Types are reached transitively -- a pointer needs its pointee, an array its
/// element. Emission follows `order`, the sequence in which they were first
/// reached, so the table does not depend on hash iteration order.
struct TypeDies {
    /// Type -> index, which names its label `.Ldie_ty<N>`. Lookup only:
    /// emission walks `order`, so hash iteration never reaches the output.
    index: HashMap<TypeId, usize>,
    /// Emission order, so a DIE is written once and the table is stable.
    order: Vec<TypeId>,
}

impl TypeDies {
    fn collect(fns: &[FnDie], types: &TypeTable) -> Self {
        let mut this = TypeDies {
            index: HashMap::new(),
            order: Vec::new(),
        };
        for f in fns {
            if let Some(t) = f.ret_typ {
                this.add(t, types, 0);
            }
            for v in &f.vars {
                this.add(v.typ, types, 0);
            }
        }
        this
    }

    /// Record `id` and whatever it refers to.
    ///
    /// `depth` guards against a type that reaches itself -- `struct s { struct
    /// s *next; };` is ordinary C, and the pointer arm would otherwise recurse
    /// forever. Aggregates emit no members here, so the walk stops at them
    /// anyway; the cap is for the pointer and array chains.
    fn add(&mut self, id: TypeId, types: &TypeTable, depth: u32) {
        if depth > 16 || self.index.contains_key(&id) {
            return;
        }
        let kind = types.kind(id);
        // A type c17 has no DIE shape for is left out; the variable using it
        // then has no `DW_AT_type` and is skipped rather than mislabelled.
        if !describable(kind) {
            return;
        }
        self.index.insert(id, self.order.len());
        self.order.push(id);
        if matches!(kind, TypeKind::Pointer | TypeKind::Array) {
            if let Some(base) = types.base_type(id) {
                self.add(base, types, depth + 1);
            }
        }
    }

    fn label(&self, id: TypeId) -> Option<String> {
        self.index.get(&id).map(|n| format!(".Ldie_ty{}", n))
    }
}

/// Whether this kind of type has a DIE shape in the abbreviation table.
fn describable(kind: TypeKind) -> bool {
    matches!(
        kind,
        TypeKind::Bool
            | TypeKind::Char
            | TypeKind::Short
            | TypeKind::Int
            | TypeKind::Long
            | TypeKind::LongLong
            | TypeKind::Float
            | TypeKind::Double
            | TypeKind::LongDouble
            | TypeKind::Enum
            | TypeKind::Pointer
            | TypeKind::Array
            | TypeKind::Struct
            | TypeKind::Union
    )
}

/// The DWARF encoding for a scalar: how a debugger should interpret its bytes.
fn base_encoding(id: TypeId, types: &TypeTable) -> u64 {
    match types.kind(id) {
        TypeKind::Bool => DW_ATE_BOOLEAN,
        TypeKind::Float | TypeKind::Double | TypeKind::LongDouble => DW_ATE_FLOAT,
        TypeKind::Char => {
            if types.is_unsigned(id) {
                DW_ATE_UNSIGNED_CHAR
            } else {
                DW_ATE_SIGNED_CHAR
            }
        }
        _ => {
            if types.is_unsigned(id) {
                DW_ATE_UNSIGNED
            } else {
                DW_ATE_SIGNED
            }
        }
    }
}

/// A CU-relative reference to another DIE (`DW_FORM_ref4`).
fn type_ref<I: LirInst + EmitAsm>(base: &mut CodeGenBase<I>, label: &str) {
    base.push_directive(Directive::Raw(format!(
        "    .long {} - .Ldebug_info0",
        label
    )));
}

/// Emit one DIE per collected type.
fn emit_type_dies<I: LirInst + EmitAsm>(
    base: &mut CodeGenBase<I>,
    dies: &TypeDies,
    types: &TypeTable,
) {
    for (n, &id) in dies.order.iter().enumerate() {
        base.push_directive(Directive::local_label(format!(".Ldie_ty{}", n)));
        let size = types.size_bytes(id) as i64;
        match types.kind(id) {
            TypeKind::Pointer => match types.base_type(id).and_then(|b| dies.label(b)) {
                Some(pointee) => {
                    base.push_directive(Directive::Uleb128(ABBREV_POINTER));
                    base.push_directive(Directive::Byte(size));
                    type_ref(base, &pointee);
                }
                None => {
                    base.push_directive(Directive::Uleb128(ABBREV_POINTER_VOID));
                    base.push_directive(Directive::Byte(size));
                }
            },
            TypeKind::Array => {
                let elem = types.base_type(id).and_then(|b| dies.label(b));
                let Some(elem) = elem else {
                    // No element DIE: describe it as an opaque block of bytes
                    // rather than claiming an element type it does not have.
                    base.push_directive(Directive::Uleb128(ABBREV_STRUCT));
                    base.push_directive(Directive::Long(size));
                    continue;
                };
                base.push_directive(Directive::Uleb128(ABBREV_ARRAY));
                type_ref(base, &elem);
                // One subrange child giving the last valid index.
                let elem_size = types
                    .base_type(id)
                    .map(|b| types.size_bytes(b) as i64)
                    .unwrap_or(1)
                    .max(1);
                base.push_directive(Directive::Uleb128(ABBREV_SUBRANGE));
                base.push_directive(Directive::Long((size / elem_size - 1).max(0)));
                base.push_directive(Directive::Byte(0)); // end of children
            }
            TypeKind::Struct => {
                base.push_directive(Directive::Uleb128(ABBREV_STRUCT));
                base.push_directive(Directive::Long(size));
            }
            TypeKind::Union => {
                base.push_directive(Directive::Uleb128(ABBREV_UNION));
                base.push_directive(Directive::Long(size));
            }
            _ => {
                base.push_directive(Directive::Uleb128(ABBREV_BASE_TYPE));
                base.push_directive(Directive::Asciz(types.format_type(id, None)));
                base.push_directive(Directive::Byte(size));
                base.push_directive(Directive::Byte(base_encoding(id, types) as i64));
            }
        }
    }
}

/// Emit the DIEs for one function and the variables inside it.
fn emit_fn_die<I: LirInst + EmitAsm>(base: &mut CodeGenBase<I>, f: &FnDie, dies: &TypeDies) {
    let ret = f.ret_typ.and_then(|t| dies.label(t));
    base.push_directive(Directive::Uleb128(match ret {
        Some(_) => ABBREV_SUBPROGRAM_TYPED,
        None => ABBREV_SUBPROGRAM_VOID,
    }));
    base.push_directive(Directive::Byte(if f.external { 1 } else { 0 }));
    base.push_directive(Directive::Asciz(f.name.clone()));
    base.push_directive(Directive::Byte(1)); // DW_AT_decl_file
    base.push_directive(Directive::Long(f.decl_line as i64));
    base.push_directive(Directive::QuadSym(Symbol::global(&f.name)));
    base.push_directive(Directive::QuadSym(Symbol::local(&f.end_label)));
    if let Some(r) = ret {
        type_ref(base, &r);
    }

    for v in &f.vars {
        // Without a type DIE there is nothing for a debugger to interpret the
        // bytes as, so the variable is left out rather than described wrongly.
        let Some(ty) = dies.label(v.typ) else {
            continue;
        };
        let code = match (v.is_param, v.loc.is_some()) {
            (true, true) => ABBREV_PARAM_LOC,
            (true, false) => ABBREV_PARAM_NOLOC,
            (false, true) => ABBREV_VAR_LOC,
            (false, false) => ABBREV_VAR_NOLOC,
        };
        base.push_directive(Directive::Uleb128(code));
        base.push_directive(Directive::Asciz(debug_name(&v.name).to_string()));
        base.push_directive(Directive::Byte(1)); // DW_AT_decl_file
        base.push_directive(Directive::Long(v.decl_line as i64));
        type_ref(base, &ty);
        if let Some(loc) = v.loc {
            // A `DW_FORM_block1` expression: one byte of length, then
            // `DW_OP_breg<reg>` and a signed offset. Naming the base register
            // directly rather than going through `DW_AT_frame_base` keeps this
            // true for a frame whose locals are not addressed from the frame
            // pointer.
            //
            // The length is computed rather than measured between labels: a
            // label pair would have to be unique across every variable in the
            // translation unit, and one keyed by register and offset collides
            // the moment two functions put a variable in the same place.
            base.push_directive(Directive::Byte(1 + sleb128_len(loc.offset) as i64));
            base.push_directive(Directive::Byte(DW_OP_BREG0 + loc.reg as i64));
            base.push_directive(Directive::Sleb128(loc.offset));
        }
    }

    base.push_directive(Directive::Byte(0)); // end of this subprogram's children
}

/// The name a debugger should show, without the linearizer's uniquing suffix.
///
/// A local is keyed as `c.4` in `func.locals` -- the trailing number keeps two
/// `c`s in different scopes apart. C identifiers cannot contain a `.`, so
/// everything from the last one is the compiler's and not the programmer's.
pub fn debug_name(name: &str) -> &str {
    match name.rsplit_once('.') {
        Some((head, tail)) if !head.is_empty() && tail.bytes().all(|b| b.is_ascii_digit()) => head,
        _ => name,
    }
}

/// How many bytes `.sleb128 v` occupies.
///
/// Needed because a `DW_FORM_block1` location expression states its own length
/// up front, and the assembler will not compute it for us without a label pair
/// that would have to be unique across the whole unit.
fn sleb128_len(mut v: i64) -> usize {
    let mut n = 1;
    loop {
        let byte = (v & 0x7f) as u8;
        v >>= 7;
        // The encoding stops when the remaining bits are all copies of the
        // sign bit already carried by this byte.
        let done = (v == 0 && byte & 0x40 == 0) || (v == -1 && byte & 0x40 != 0);
        if done {
            return n;
        }
        n += 1;
    }
}

/// What the compile-unit DIE says about the translation unit as a whole.
pub struct UnitInfo<'a> {
    pub producer: &'a str,
    pub source_name: &'a str,
    pub comp_dir: &'a str,
    /// Labels bounding the unit's code; `None` for a file with no functions.
    pub low_pc_label: Option<&'a str>,
    pub high_pc_label: Option<&'a str>,
}

pub fn generate_debug_info<I: LirInst + EmitAsm>(
    base: &mut CodeGenBase<I>,
    unit: &UnitInfo<'_>,
    fns: &[FnDie],
    types: &TypeTable,
) {
    let UnitInfo {
        producer,
        source_name,
        comp_dir,
        low_pc_label,
        high_pc_label,
    } = *unit;
    let dies = TypeDies::collect(fns, types);
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

    // DW_AT_stmt_list: this unit's own line program.
    //
    // A relocatable reference, not a literal 0. The assembler builds one line
    // program per object from the `.loc` directives, and the linker packs them
    // into `.debug_line` one after another -- so only the first object's
    // program sits at offset 0. Every unit claiming 0 made every unit share the
    // first one's line table: in a 56-unit link of sparse, exactly one file
    // resolved to source lines and the other 55 reported none, which reads
    // like missing debug info rather than a misdirected offset.
    base.push_directive(Directive::LongSym(Symbol::local(".Ldebug_line0")));

    // DW_AT_low_pc (start of code)
    // Use 0 for data-only files with no code section
    if let Some(label) = low_pc_label {
        base.push_directive(Directive::QuadSym(Symbol::local(label)));
    } else {
        base.push_directive(Directive::Quad(0));
    }

    // DW_AT_high_pc (end of code)
    // Use 0 for data-only files with no code section
    if let Some(label) = high_pc_label {
        base.push_directive(Directive::QuadSym(Symbol::local(label)));
    } else {
        base.push_directive(Directive::Quad(0));
    }

    // The compile unit's children: every type the functions mention, then the
    // functions themselves. A zero byte closes the list.
    emit_type_dies(base, &dies, types);
    for f in fns {
        emit_fn_die(base, f, &dies);
    }
    base.push_directive(Directive::Byte(0));

    // End label for unit length computation
    base.push_directive(Directive::local_label(".Ldebug_info_end"));

    // The label `DW_AT_stmt_list` names. Entering the section is enough to
    // anchor it at the start of this object's contribution; the assembler
    // appends the line program it builds from the `.loc` directives after it.
    base.push_directive(Directive::DebugLine);
    base.push_directive(Directive::local_label(".Ldebug_line0"));
}
