//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linearizer for pcc C99 compiler
// Converts AST to SSA-form IR with basic blocks and typed pseudo-registers
//

use super::ssa::ssa_convert;
use super::{
    BasicBlock, BasicBlockId, CallAbiInfo, Function, Initializer, Instruction, MemoryOrder, Module,
    Opcode, Pseudo, PseudoId,
};
use crate::abi::{get_abi_for_conv, CallingConv};
use crate::diag::{error, get_all_stream_names, Position};
use crate::parse::ast::{
    BinaryOp, BlockItem, Expr, ExprKind, ExternalDecl, FunctionDef, InitElement, OffsetOfPath,
    TranslationUnit, UnaryOp,
};
use crate::strings::{StringId, StringTable};
use crate::symbol::{SymbolId, SymbolTable};
use crate::target::Target;
use crate::types::{MemberInfo, TypeId, TypeKind, TypeModifiers, TypeTable};
use std::collections::HashMap;

const DEFAULT_VAR_MAP_CAPACITY: usize = 64;
const DEFAULT_LABEL_MAP_CAPACITY: usize = 16;
const DEFAULT_LOOP_DEPTH_CAPACITY: usize = 4;
const DEFAULT_FILE_SCOPE_CAPACITY: usize = 16;

/// Information about a local variable
#[derive(Clone)]
pub(crate) struct LocalVarInfo {
    /// Symbol pseudo (address of the variable)
    pub(crate) sym: PseudoId,
    /// Type of the variable
    pub(crate) typ: TypeId,
    /// For VLAs: symbol holding the number of elements (for runtime sizeof)
    /// This is stored in a hidden local variable so it survives SSA.
    pub(crate) vla_size_sym: Option<PseudoId>,
    /// For VLAs: the element type (for sizeof computation)
    pub(crate) vla_elem_type: Option<TypeId>,
    /// For multi-dimensional VLAs: symbols storing each dimension's size
    /// For int arr[n][m], this contains [sym_for_n, sym_for_m]
    /// These are needed to compute runtime strides for outer dimension access.
    pub(crate) vla_dim_syms: Vec<PseudoId>,
    /// True if this local holds a pointer to the actual data (e.g., va_list parameters).
    /// When true, linearize_lvalue loads the pointer instead of taking the address.
    pub(crate) is_indirect: bool,
}

pub(crate) struct ResolvedDesignator {
    pub(crate) offset: usize,
    pub(crate) typ: TypeId,
    pub(crate) bit_offset: Option<u32>,
    pub(crate) bit_width: Option<u32>,
    pub(crate) storage_unit_size: Option<u32>,
}

pub(crate) struct RawFieldInit {
    pub(crate) offset: usize,
    pub(crate) field_size: usize,
    pub(crate) init: Initializer,
    pub(crate) bit_offset: Option<u32>,
    pub(crate) bit_width: Option<u32>,
    pub(crate) storage_unit_size: Option<u32>,
}

/// Result from member_index_for_designator indicating where positional
/// initialization should continue after a designated field.
pub(crate) enum MemberDesignatorResult {
    /// Field found directly at outer level; next positional index is the value.
    Direct(usize),
    /// Field found inside an anonymous struct/union at `outer_idx`.
    /// `levels` is the stack of nesting from outermost to innermost.
    Anonymous {
        outer_idx: usize,
        levels: Vec<AnonLevel>,
    },
}

/// One level of anonymous struct nesting for positional continuation.
pub(crate) struct AnonLevel {
    /// Type of this anonymous struct/union
    pub(crate) anon_type: TypeId,
    /// Byte offset of this anonymous struct within the top-level struct
    pub(crate) base_offset: usize,
    /// Next member index to consume within this anonymous struct
    pub(crate) inner_next_idx: usize,
}

/// Tracks continuation state when a designator targeted a field inside
/// an anonymous struct/union. Supports arbitrary nesting depth.
/// The next positional element should continue from the innermost level,
/// popping up through parent anonymous structs when each level is exhausted.
pub(crate) struct AnonContinuation {
    /// Index of the outermost anonymous struct in the top-level members array.
    pub(crate) outer_idx: usize,
    /// Stack of nesting levels from outermost to innermost.
    pub(crate) levels: Vec<AnonLevel>,
}

/// Grouped array init elements, keyed by array index (sorted).
/// Shared between static (ast_init_list_to_ir) and runtime (linearize_init_list_at_offset) paths.
pub(crate) struct ArrayInitGroups {
    pub(crate) element_lists: HashMap<i64, Vec<InitElement>>,
    pub(crate) indices: Vec<i64>,
}

/// A field visit from walking struct/union initializer elements.
/// Shared between static and runtime init paths.
pub(crate) struct StructFieldVisit {
    pub(crate) offset: usize,
    pub(crate) typ: TypeId,
    pub(crate) field_size: usize,
    pub(crate) kind: StructFieldVisitKind,
    pub(crate) bit_offset: Option<u32>,
    pub(crate) bit_width: Option<u32>,
    pub(crate) storage_unit_size: Option<u32>,
}

pub(crate) enum StructFieldVisitKind {
    /// A single expression to initialize this field
    Expr(Box<Expr>),
    /// Sub-elements from brace elision
    BraceElision(Vec<InitElement>),
}

/// Information about a static local variable
#[derive(Clone)]
pub(crate) struct StaticLocalInfo {
    /// Global symbol name (unique across translation unit)
    pub(crate) global_name: String,
    /// Type of the variable
    pub(crate) typ: TypeId,
}

// ============================================================================
// Linearizer
// ============================================================================

/// Linearizer context for converting AST to IR
pub struct Linearizer<'a> {
    /// The module being built
    pub(crate) module: Module,
    /// Current function being linearized
    pub(crate) current_func: Option<Function>,
    /// Current basic block being built
    pub(crate) current_bb: Option<BasicBlockId>,
    /// Next pseudo ID
    pub(crate) next_pseudo: u32,
    /// Next basic block ID
    pub(crate) next_bb: u32,
    /// Parameter -> pseudo mapping (parameters are already SSA values)
    pub(crate) var_map: HashMap<String, PseudoId>,
    /// Local variables (use Load/Store, converted to SSA later)
    /// Keyed by SymbolId for proper scope handling
    pub(crate) locals: HashMap<SymbolId, LocalVarInfo>,
    /// Label -> basic block mapping
    pub(crate) label_map: HashMap<String, BasicBlockId>,
    /// Break target stack (for loops)
    pub(crate) break_targets: Vec<BasicBlockId>,
    /// Continue target stack (for loops)
    pub(crate) continue_targets: Vec<BasicBlockId>,
    /// Whether to run SSA conversion after linearization
    pub(crate) run_ssa: bool,
    /// Symbol table for looking up enum constants, etc.
    pub(crate) symbols: &'a SymbolTable,
    /// Type table for type information
    pub(crate) types: &'a TypeTable,
    /// String table for converting StringId to String at IR boundary
    pub(crate) strings: &'a StringTable,
    /// Hidden struct return pointer (for functions returning large structs via sret)
    pub(crate) struct_return_ptr: Option<PseudoId>,
    /// Size of struct being returned (for functions returning large structs via sret)
    pub(crate) struct_return_size: u32,
    /// Type of struct being returned via two registers (9-16 bytes, per ABI)
    pub(crate) two_reg_return_type: Option<TypeId>,
    /// Current function name (for generating unique static local names)
    pub(crate) current_func_name: String,
    /// Counter for generating unique static local names
    pub(crate) static_local_counter: u32,
    /// Counter for generating unique compound literal names (for file-scope compound literals)
    pub(crate) compound_literal_counter: u32,
    /// Static local variables (local name -> static local info)
    /// This is persistent across function calls (not cleared per function)
    pub(crate) static_locals: HashMap<String, StaticLocalInfo>,
    /// Current source position for debug info
    pub(crate) current_pos: Option<Position>,
    /// Target configuration (architecture, ABI details)
    pub(crate) target: &'a Target,
    /// Whether current function is a non-static inline function
    /// (used for enforcing C99 inline semantic restrictions)
    pub(crate) current_func_is_non_static_inline: bool,
    /// Set of file-scope static variable names (for inline semantic checks)
    pub(crate) file_scope_statics: std::collections::HashSet<String>,
    /// Calling convention of the current function being linearized
    pub(crate) current_calling_conv: CallingConv,
    /// Scope stack for locals: each entry records (sym, previous_value) pairs
    /// for undoing inserts when a scope exits.
    pub(crate) local_scope_stack: Vec<Vec<(SymbolId, Option<LocalVarInfo>)>>,
}

impl<'a> Linearizer<'a> {
    /// Create a new linearizer
    pub fn new(
        symbols: &'a SymbolTable,
        types: &'a TypeTable,
        strings: &'a StringTable,
        target: &'a Target,
    ) -> Self {
        Self {
            module: Module::default(),
            current_func: None,
            current_bb: None,
            next_pseudo: 0,
            next_bb: 0,
            var_map: HashMap::with_capacity(DEFAULT_VAR_MAP_CAPACITY),
            locals: HashMap::with_capacity(DEFAULT_VAR_MAP_CAPACITY),
            label_map: HashMap::with_capacity(DEFAULT_LABEL_MAP_CAPACITY),
            break_targets: Vec::with_capacity(DEFAULT_LOOP_DEPTH_CAPACITY),
            continue_targets: Vec::with_capacity(DEFAULT_LOOP_DEPTH_CAPACITY),
            run_ssa: true, // Enable SSA conversion by default
            symbols,
            types,
            strings,
            struct_return_ptr: None,
            struct_return_size: 0,
            two_reg_return_type: None,
            current_func_name: String::new(),
            static_local_counter: 0,
            compound_literal_counter: 0,
            static_locals: HashMap::with_capacity(DEFAULT_LABEL_MAP_CAPACITY),
            current_pos: None,
            target,
            current_func_is_non_static_inline: false,
            file_scope_statics: std::collections::HashSet::with_capacity(
                DEFAULT_FILE_SCOPE_CAPACITY,
            ),
            current_calling_conv: CallingConv::default(),
            local_scope_stack: Vec::new(),
        }
    }

    /// Create a linearizer with SSA conversion disabled (for testing)
    #[cfg(test)]
    pub fn new_no_ssa(
        symbols: &'a SymbolTable,
        types: &'a TypeTable,
        strings: &'a StringTable,
        target: &'a Target,
    ) -> Self {
        Self {
            run_ssa: false,
            ..Self::new(symbols, types, strings, target)
        }
    }

    /// Push a new local scope. Subsequent `insert_local` calls will record
    /// the previous value so `pop_scope` can restore it.
    pub(crate) fn push_scope(&mut self) {
        self.local_scope_stack.push(Vec::new());
    }

    /// Pop the current local scope, restoring all locals to their pre-scope values.
    pub(crate) fn pop_scope(&mut self) {
        if let Some(entries) = self.local_scope_stack.pop() {
            for (sym, prev) in entries.into_iter().rev() {
                match prev {
                    Some(info) => {
                        self.locals.insert(sym, info);
                    }
                    None => {
                        self.locals.remove(&sym);
                    }
                }
            }
        }
    }

    /// Insert a local variable, recording the previous value for scope restoration.
    pub(crate) fn insert_local(&mut self, sym: SymbolId, info: LocalVarInfo) {
        let prev = self.locals.insert(sym, info);
        if let Some(scope) = self.local_scope_stack.last_mut() {
            scope.push((sym, prev));
        }
    }

    /// Convert a StringId to a &str using the string table
    #[inline]
    pub(crate) fn str(&self, id: StringId) -> &str {
        self.strings.get(id)
    }

    /// Get the name of a symbol as a String
    #[inline]
    pub(crate) fn symbol_name(&self, id: SymbolId) -> String {
        self.str(self.symbols.get(id).name).to_string()
    }

    /// Linearize a translation unit
    pub fn linearize(&mut self, tu: &TranslationUnit) -> Module {
        for item in &tu.items {
            match item {
                ExternalDecl::FunctionDef(func) => {
                    self.linearize_function(func);
                }
                ExternalDecl::Declaration(decl) => {
                    self.linearize_global_decl(decl);
                }
            }
        }
        std::mem::take(&mut self.module)
    }

    /// Allocate a new pseudo ID
    pub(crate) fn alloc_pseudo(&mut self) -> PseudoId {
        let id = PseudoId(self.next_pseudo);
        self.next_pseudo += 1;
        id
    }

    /// Allocate a new pseudo and register it in the current function.
    pub(crate) fn alloc_reg_pseudo(&mut self) -> PseudoId {
        let id = self.alloc_pseudo();
        let pseudo = Pseudo::reg(id, id.0);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(pseudo);
        }
        id
    }

    /// Map a bitfield storage unit byte-size to the corresponding unsigned type.
    pub(crate) fn bitfield_storage_type(&self, storage_size: u32) -> TypeId {
        match storage_size {
            1 => self.types.uchar_id,
            2 => self.types.ushort_id,
            4 => self.types.uint_id,
            8 => self.types.ulong_id,
            _ => self.types.uint_id,
        }
    }

    /// Allocate a new basic block ID
    pub(crate) fn alloc_bb(&mut self) -> BasicBlockId {
        let id = BasicBlockId(self.next_bb);
        self.next_bb += 1;
        id
    }

    /// Get or create a basic block
    pub(crate) fn get_or_create_bb(&mut self, id: BasicBlockId) -> &mut BasicBlock {
        let func = self.current_func.as_mut().unwrap();
        if func.get_block(id).is_none() {
            func.add_block(BasicBlock::new(id));
        }
        func.get_block_mut(id).unwrap()
    }

    /// Emit a PhiSource instruction in a predecessor block.
    /// Returns the PhiSource target pseudo.
    pub(crate) fn emit_phi_source(
        &mut self,
        pred_bb: BasicBlockId,
        value: PseudoId,
        phi_target: PseudoId,
        phi_bb: BasicBlockId,
        typ: TypeId,
        size: u32,
    ) -> PseudoId {
        let phisrc_pseudo = self.alloc_pseudo();
        let pseudo = Pseudo::phi(phisrc_pseudo, phisrc_pseudo.0);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(pseudo);
        }

        let mut phisrc = Instruction::phi_source(phisrc_pseudo, value, typ, size);
        phisrc.phi_list = vec![(phi_bb, phi_target)];
        if let Some(pos) = self.current_pos {
            phisrc.pos = Some(pos);
        }

        if let Some(func) = &mut self.current_func {
            if let Some(bb) = func.get_block_mut(pred_bb) {
                bb.insert_before_terminator(phisrc);
            }
        }

        phisrc_pseudo
    }

    /// Add an instruction to the current basic block
    pub(crate) fn emit(&mut self, insn: Instruction) {
        if let Some(bb_id) = self.current_bb {
            // Attach current source position for debug info
            let insn = if let Some(pos) = self.current_pos {
                insn.with_pos(pos)
            } else {
                insn
            };
            let bb = self.get_or_create_bb(bb_id);
            bb.add_insn(insn);
        }
    }

    /// Apply C99 integer promotions (6.3.1.1)
    /// Types smaller than int are promoted to int (or unsigned int if int can't hold all values)
    pub(crate) fn integer_promote(&self, typ_id: TypeId) -> TypeId {
        // Integer promotions apply to _Bool, char, short (and their unsigned variants)
        // They are promoted to int if int can represent all values, otherwise unsigned int
        match self.types.kind(typ_id) {
            TypeKind::Bool | TypeKind::Char | TypeKind::Short => {
                // int (32-bit signed) can represent all values of:
                // - _Bool (0-1)
                // - char/signed char (-128 to 127)
                // - unsigned char (0 to 255)
                // - short/signed short (-32768 to 32767)
                // - unsigned short (0 to 65535)
                // So always promote to int
                self.types.int_id
            }
            _ => typ_id,
        }
    }

    /// Compute the common type for usual arithmetic conversions (C99 6.3.1.8)
    /// Returns the wider type that both operands should be converted to
    pub(crate) fn common_type(&self, left: TypeId, right: TypeId) -> TypeId {
        // C99 6.3.1.8 usual arithmetic conversions:
        // 1. If either is long double, convert to long double
        // 2. Else if either is double, convert to double
        // 3. Else if either is float, convert to float
        // 4. Otherwise apply integer promotions, then:
        //    a. If both have same type after promotion, done
        //    b. If both signed or both unsigned, convert narrower to wider
        //    c. If unsigned has rank >= signed, convert signed to unsigned
        //    d. If signed can represent all unsigned values, convert to signed
        //    e. Otherwise convert both to unsigned version of signed type

        // Check for floating point types
        let left_float = self.types.is_float(left);
        let right_float = self.types.is_float(right);

        let left_kind = self.types.kind(left);
        let right_kind = self.types.kind(right);

        if left_float || right_float {
            // At least one operand is floating point
            // Use the wider floating point type
            if left_kind == TypeKind::LongDouble || right_kind == TypeKind::LongDouble {
                return self.types.longdouble_id;
            }
            if left_kind == TypeKind::Double || right_kind == TypeKind::Double {
                return self.types.double_id;
            }
            if left_kind == TypeKind::Float || right_kind == TypeKind::Float {
                return self.types.float_id;
            }
            // Both are Float16 (C23: _Float16 stays as _Float16)
            if left_kind == TypeKind::Float16 || right_kind == TypeKind::Float16 {
                return self.types.float16_id;
            }
            // Fallback to float for any remaining float cases
            return self.types.float_id;
        }

        // Apply integer promotions first (C99 6.3.1.1)
        let left_promoted = self.integer_promote(left);
        let right_promoted = self.integer_promote(right);

        let left_size = self.types.size_bits(left_promoted);
        let right_size = self.types.size_bits(right_promoted);
        let left_unsigned = self.types.is_unsigned(left_promoted);
        let right_unsigned = self.types.is_unsigned(right_promoted);
        let left_kind = self.types.kind(left_promoted);
        let right_kind = self.types.kind(right_promoted);

        // If both have same type after promotion, use that type
        if left_kind == right_kind && left_unsigned == right_unsigned && left_size == right_size {
            return left_promoted;
        }

        // If both signed or both unsigned, convert narrower to wider
        if left_unsigned == right_unsigned {
            return if left_size >= right_size {
                left_promoted
            } else {
                right_promoted
            };
        }

        // Mixed signedness case
        let (signed_id, unsigned_id) = if left_unsigned {
            (right_promoted, left_promoted)
        } else {
            (left_promoted, right_promoted)
        };

        let signed_size = self.types.size_bits(signed_id);
        let unsigned_size = self.types.size_bits(unsigned_id);

        // If unsigned has rank >= signed, convert to unsigned
        if unsigned_size >= signed_size {
            return unsigned_id;
        }

        // If signed type can represent all values of unsigned type, use signed
        // (This is true when signed_size > unsigned_size on our platforms)
        if signed_size > unsigned_size {
            return signed_id;
        }

        // Otherwise convert both to unsigned version of signed type
        // (This case shouldn't happen on LP64 since we already handled size comparisons)
        self.types.unsigned_version(signed_id)
    }

    /// Emit a type conversion if needed
    /// Returns the (possibly converted) pseudo ID
    pub(crate) fn emit_convert(
        &mut self,
        val: PseudoId,
        from_typ: TypeId,
        to_typ: TypeId,
    ) -> PseudoId {
        let from_size = self.types.size_bits(from_typ);
        let to_size = self.types.size_bits(to_typ);
        let from_float = self.types.is_float(from_typ);
        let to_float = self.types.is_float(to_typ);
        let from_kind = self.types.kind(from_typ);
        let to_kind = self.types.kind(to_typ);

        // Same type and size - no conversion needed
        if from_kind == to_kind && from_size == to_size {
            return val;
        }

        // Array to pointer conversion (decay) - no actual conversion needed
        // The array value is already the address of the first element (64-bit)
        if from_kind == TypeKind::Array && to_kind == TypeKind::Pointer {
            return val;
        }

        // Function to pointer conversion (decay) - no actual conversion needed
        // Function name decays to function pointer (64-bit address)
        if from_kind == TypeKind::Function && to_kind == TypeKind::Pointer {
            return val;
        }

        // Pointer to pointer conversion - no actual conversion needed
        // All pointers are the same size (64-bit)
        if from_kind == TypeKind::Pointer && to_kind == TypeKind::Pointer {
            return val;
        }

        // Special handling for _Bool conversion (C99 6.3.1.2)
        // When any scalar value is converted to _Bool:
        // - Result is 0 if the value compares equal to 0
        // - Result is 1 otherwise
        if to_kind == TypeKind::Bool && from_kind != TypeKind::Bool {
            let result = self.alloc_reg_pseudo();

            // Create a zero constant for comparison
            let zero = self.emit_const(0, from_typ);

            // Compare val != 0
            let opcode = if from_float {
                Opcode::FCmpONe
            } else {
                Opcode::SetNe
            };

            let mut insn = Instruction::binop(opcode, result, val, zero, to_typ, to_size);
            insn.src_size = from_size;
            insn.src_typ = Some(from_typ);
            self.emit(insn);

            return result;
        }

        // Handle floating point conversions
        if from_float || to_float {
            let result = self.alloc_reg_pseudo();

            let opcode = if from_float && to_float {
                // Float to float (e.g., float to double or double to float)
                Opcode::FCvtF
            } else if from_float {
                // Float to integer
                if self.types.is_unsigned(to_typ) {
                    Opcode::FCvtU
                } else {
                    Opcode::FCvtS
                }
            } else {
                // Integer to float
                if self.types.is_unsigned(from_typ) {
                    Opcode::UCvtF
                } else {
                    Opcode::SCvtF
                }
            };

            let mut insn = Instruction::unop(opcode, result, val, to_typ, to_size);
            insn.src_size = from_size;
            insn.src_typ = Some(from_typ);
            self.emit(insn);
            return result;
        }

        // Integer to integer conversion
        if from_size == to_size {
            // Same size integers (e.g., signed to unsigned) - no actual conversion needed
            return val;
        }

        let result = self.alloc_reg_pseudo();

        if to_size > from_size {
            // Extending - use sign or zero extension based on source type
            let opcode = if self.types.is_unsigned(from_typ) {
                Opcode::Zext
            } else {
                Opcode::Sext
            };
            let mut insn = Instruction::unop(opcode, result, val, to_typ, to_size);
            insn.src_size = from_size;
            insn.src_typ = Some(from_typ);
            self.emit(insn);
        } else {
            // Truncating
            let mut insn = Instruction::unop(Opcode::Trunc, result, val, to_typ, to_size);
            insn.src_size = from_size;
            insn.src_typ = Some(from_typ);
            self.emit(insn);
        }

        result
    }

    /// Check if current basic block is terminated
    pub(crate) fn is_terminated(&self) -> bool {
        if let Some(bb_id) = self.current_bb {
            if let Some(func) = &self.current_func {
                if let Some(bb) = func.get_block(bb_id) {
                    return bb.is_terminated();
                }
            }
        }
        false
    }

    /// Link current block to merge block if not terminated.
    /// Used after linearizing then/else branches to connect to merge block.
    pub(crate) fn link_to_merge_if_needed(&mut self, merge_bb: BasicBlockId) {
        if self.is_terminated() {
            return;
        }
        if let Some(current) = self.current_bb {
            // If the block isn't terminated, it needs a branch to the merge block.
            // Any unreachable blocks will be cleaned up by dead code elimination.
            self.emit(Instruction::br(merge_bb));
            self.link_bb(current, merge_bb);
        }
    }

    /// Link two basic blocks (parent -> child)
    pub(crate) fn link_bb(&mut self, from: BasicBlockId, to: BasicBlockId) {
        let func = self.current_func.as_mut().unwrap();

        // Add child to parent
        if let Some(from_bb) = func.get_block_mut(from) {
            from_bb.add_child(to);
        }

        // Add parent to child - need to get it separately
        // First ensure it exists
        if func.get_block(to).is_none() {
            func.add_block(BasicBlock::new(to));
        }
        if let Some(to_bb) = func.get_block_mut(to) {
            to_bb.add_parent(from);
        }
    }

    /// Switch to a new basic block
    pub(crate) fn switch_bb(&mut self, id: BasicBlockId) {
        self.current_bb = Some(id);
        self.get_or_create_bb(id);
    }

    // ========================================================================
    // Function linearization
    // ========================================================================

    pub(crate) fn linearize_function(&mut self, func: &FunctionDef) {
        // Set current position for debug info (function definition location)
        self.current_pos = Some(func.pos);

        // Reset per-function state
        self.next_pseudo = 0;
        self.next_bb = 0;
        self.var_map.clear();
        self.locals.clear();
        self.local_scope_stack.clear();
        self.push_scope(); // function-level scope
        self.label_map.clear();
        self.break_targets.clear();
        self.continue_targets.clear();
        self.struct_return_ptr = None;
        self.struct_return_size = 0;
        self.two_reg_return_type = None;
        self.current_func_name = self.str(func.name).to_string();
        // Remove from extern_symbols since we're defining this function
        self.module.extern_symbols.remove(&self.current_func_name);
        // Note: static_locals is NOT cleared - it persists across functions

        // Create function - use storage class from FunctionDef
        let modifiers = self.types.modifiers(func.return_type);
        let is_static = func.is_static;
        let is_inline = func.is_inline;
        let _is_extern = modifiers.contains(TypeModifiers::EXTERN);
        let is_noreturn = modifiers.contains(TypeModifiers::NORETURN);

        // Track non-static inline functions for semantic restriction checks
        // C99 6.7.4: non-static inline functions have restrictions on
        // static variables they can access
        self.current_func_is_non_static_inline = is_inline && !is_static;

        // Store calling convention from function attributes (e.g., __attribute__((sysv_abi)))
        self.current_calling_conv = func.calling_conv;

        let mut ir_func = Function::new(self.str(func.name), func.return_type);
        // For linkage:
        // - static inline: internal linkage (same as static)
        // - inline (without extern): inline definition only, internal linkage
        // - extern inline: per C99, provides external definition, but since we
        //   treat inline functions as internal linkage candidates for inlining,
        //   avoid duplicate symbol errors when same inline function is defined
        //   in multiple translation units
        ir_func.is_static = is_static || is_inline;
        ir_func.is_noreturn = is_noreturn;
        ir_func.is_inline = is_inline;

        let ret_kind = self.types.kind(func.return_type);
        // Check if function returns a large struct
        // Large structs are returned via a hidden first parameter (sret)
        // that points to caller-allocated space
        let returns_large_struct = (ret_kind == TypeKind::Struct || ret_kind == TypeKind::Union)
            && self.types.size_bits(func.return_type) > self.target.max_aggregate_register_bits;

        // Argument index offset: if returning large struct, first arg is hidden return pointer
        let arg_offset: u32 = if returns_large_struct { 1 } else { 0 };

        // Add hidden return pointer parameter if needed
        if returns_large_struct {
            let sret_id = self.alloc_pseudo();
            let sret_pseudo = Pseudo::arg(sret_id, 0).with_name("__sret");
            ir_func.add_pseudo(sret_pseudo);
            self.struct_return_ptr = Some(sret_id);
            self.struct_return_size = self.types.size_bits(func.return_type);
        }

        // Check if function returns a medium struct (9-16 bytes) via two registers
        // This is the ABI-compliant way to return structs that fit in two GP registers
        let struct_size_bits = self.types.size_bits(func.return_type);
        let returns_two_reg_struct = (ret_kind == TypeKind::Struct || ret_kind == TypeKind::Union)
            && struct_size_bits > 64
            && struct_size_bits <= 128
            && !returns_large_struct; // Only if not using sret
        if returns_two_reg_struct {
            self.two_reg_return_type = Some(func.return_type);
        }

        // Add parameters
        // For struct/union parameters, we need to copy them to local storage
        // so member access works properly
        // Tuple: (name_string, symbol_id_option, type, pseudo_id)
        let mut struct_params: Vec<(String, Option<SymbolId>, TypeId, PseudoId)> =
            Vec::with_capacity(func.params.len());
        // Complex parameters also need local storage for real/imag access
        // Tuple: (name, symbol_id, type, arg_pseudo, arg_index_with_offset)
        let mut complex_params: Vec<(String, Option<SymbolId>, TypeId, PseudoId, u32)> =
            Vec::with_capacity(func.params.len());
        // Scalar parameters need local storage for SSA-correct reassignment handling
        let mut scalar_params: Vec<(String, Option<SymbolId>, TypeId, PseudoId)> =
            Vec::with_capacity(func.params.len());
        // va_list parameters need special handling (pointer storage)
        let mut valist_params: Vec<(String, Option<SymbolId>, TypeId, PseudoId)> =
            Vec::with_capacity(func.params.len());

        for (i, param) in func.params.iter().enumerate() {
            let name = param
                .symbol
                .map(|id| self.symbol_name(id))
                .unwrap_or_else(|| format!("arg{}", i));
            ir_func.add_param(&name, param.typ);

            // Create argument pseudo (offset by 1 if there's a hidden return pointer)
            let pseudo_id = self.alloc_pseudo();
            let pseudo = Pseudo::arg(pseudo_id, i as u32 + arg_offset).with_name(&name);
            ir_func.add_pseudo(pseudo);

            // For struct/union types, we'll copy to a local later
            // so member access works properly
            let param_kind = self.types.kind(param.typ);
            if param_kind == TypeKind::VaList {
                // va_list parameters are special: due to array-to-pointer decay at call site,
                // the actual value passed is a pointer to the va_list struct, not the struct itself.
                // We'll handle this after function setup.
                valist_params.push((name, param.symbol, param.typ, pseudo_id));
            } else if param_kind == TypeKind::Struct || param_kind == TypeKind::Union {
                // Medium structs (9-16 bytes) with all-SSE classification are
                // passed like complex types (in two XMM registers). Route them
                // through complex_params so the codegen handles the register split.
                let size = self.types.size_bits(param.typ);
                let is_two_fp_regs = size > 64 && size <= 128 && {
                    let abi = get_abi_for_conv(self.current_calling_conv, self.target);
                    let class = abi.classify_param(param.typ, self.types);
                    matches!(
                        class,
                        crate::abi::ArgClass::Direct { ref classes, .. }
                            if classes.len() == 2
                                && classes.iter().all(|c| *c == crate::abi::RegClass::Sse)
                    ) || matches!(class, crate::abi::ArgClass::Hfa { count: 2, .. })
                };
                if is_two_fp_regs {
                    complex_params.push((
                        name,
                        param.symbol,
                        param.typ,
                        pseudo_id,
                        i as u32 + arg_offset,
                    ));
                } else {
                    struct_params.push((name, param.symbol, param.typ, pseudo_id));
                }
            } else if self.types.is_complex(param.typ) {
                // Complex parameters: copy to local storage so real/imag access works
                // Unlike structs, complex types are passed in FP registers per ABI,
                // so we create local storage and the codegen handles the register split
                complex_params.push((
                    name,
                    param.symbol,
                    param.typ,
                    pseudo_id,
                    i as u32 + arg_offset,
                ));
            } else {
                // Store all scalar parameters to locals so SSA conversion can properly
                // handle reassignment with phi nodes. If the parameter is never modified,
                // SSA will optimize away the redundant load/store.
                scalar_params.push((name, param.symbol, param.typ, pseudo_id));
            }
        }

        self.current_func = Some(ir_func);

        // Create entry block
        let entry_bb = self.alloc_bb();
        self.switch_bb(entry_bb);

        // Entry instruction
        self.emit(Instruction::new(Opcode::Entry));

        // Handle va_list parameters: store the pointer value (not the struct)
        for (name, symbol_id_opt, typ, arg_pseudo) in valist_params {
            // va_list params are passed as pointers due to array decay at call site.
            // Store the pointer value (8 bytes) to a local.
            let ptr_type = self.types.pointer_to(typ);
            let local_sym = self.alloc_pseudo();
            let sym = Pseudo::sym(local_sym, name.clone());
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(sym);
                func.add_local(&name, local_sym, ptr_type, false, false, None, None);
            }
            let ptr_size = self.types.size_bits(ptr_type);
            self.emit(Instruction::store(
                arg_pseudo, local_sym, 0, ptr_type, ptr_size,
            ));
            if let Some(symbol_id) = symbol_id_opt {
                self.insert_local(
                    symbol_id,
                    LocalVarInfo {
                        sym: local_sym,
                        typ, // Keep original va_list type for type checking
                        vla_size_sym: None,
                        vla_elem_type: None,
                        vla_dim_syms: vec![],
                        is_indirect: true, // va_list param: local holds a pointer
                    },
                );
            }
        }

        // Copy struct parameters to local storage so member access works
        for (name, symbol_id_opt, typ, arg_pseudo) in struct_params {
            // Create a symbol pseudo for this local variable (its address)
            let local_sym = self.alloc_pseudo();
            let sym = Pseudo::sym(local_sym, name.clone());
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(sym);
                let mods = self.types.modifiers(typ);
                let is_volatile = mods.contains(TypeModifiers::VOLATILE);
                let is_atomic = mods.contains(TypeModifiers::ATOMIC);
                func.add_local(&name, local_sym, typ, is_volatile, is_atomic, None, None);
            }

            let typ_size = self.types.size_bits(typ);
            let is_aarch64 = self.target.arch == crate::target::Arch::Aarch64;
            if typ_size > 128 && !is_aarch64 {
                // x86-64: Large struct (> 16 bytes) passed by value on the stack.
                // arg_pseudo is an IncomingArg pointing to the struct data on the stack.
                // Use SymAddr to get the base address, then copy each 8-byte chunk.
                let ptr_type = self.types.pointer_to(typ);
                let addr_pseudo = self.alloc_reg_pseudo();
                self.emit(Instruction::sym_addr(addr_pseudo, arg_pseudo, ptr_type));

                let struct_size = typ_size / 8;
                let mut offset = 0i64;
                while offset < struct_size as i64 {
                    let temp = self.alloc_reg_pseudo();
                    self.emit(Instruction::load(
                        temp,
                        addr_pseudo,
                        offset,
                        self.types.long_id,
                        64,
                    ));
                    self.emit(Instruction::store(
                        temp,
                        local_sym,
                        offset,
                        self.types.long_id,
                        64,
                    ));
                    offset += 8;
                }
            } else if typ_size > 64 {
                // Medium struct (9-16 bytes): arg_pseudo is a pointer (current behavior).
                // Copy each 8-byte chunk through pointer dereference.
                let struct_size = typ_size / 8;
                let mut offset = 0i64;
                while offset < struct_size as i64 {
                    let temp = self.alloc_reg_pseudo();
                    self.emit(Instruction::load(
                        temp,
                        arg_pseudo,
                        offset,
                        self.types.long_id,
                        64,
                    ));
                    self.emit(Instruction::store(
                        temp,
                        local_sym,
                        offset,
                        self.types.long_id,
                        64,
                    ));
                    offset += 8;
                }
            } else {
                // Small struct: arg_pseudo contains the value directly
                self.emit(Instruction::store(arg_pseudo, local_sym, 0, typ, typ_size));
            }

            // Register as a local variable (only if named parameter)
            if let Some(symbol_id) = symbol_id_opt {
                self.insert_local(
                    symbol_id,
                    LocalVarInfo {
                        sym: local_sym,
                        typ,
                        vla_size_sym: None,
                        vla_elem_type: None,
                        vla_dim_syms: vec![],
                        is_indirect: false,
                    },
                );
            }
        }

        // Setup local storage for complex parameters
        // Complex types are passed in FP registers per ABI - the prologue codegen
        // handles storing from XMM registers to local storage
        for (name, symbol_id_opt, typ, _arg_pseudo, arg_idx) in complex_params {
            // Create a symbol pseudo for this local variable (its address)
            let local_sym = self.alloc_pseudo();
            let sym = Pseudo::sym(local_sym, name.clone());
            let typ_size_bytes = (self.types.size_bits(typ) / 8) as usize;
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(sym);
                let mods = self.types.modifiers(typ);
                let is_volatile = mods.contains(TypeModifiers::VOLATILE);
                let is_atomic = mods.contains(TypeModifiers::ATOMIC);
                func.add_local(&name, local_sym, typ, is_volatile, is_atomic, None, None);
                // Record for inliner: the backend prologue fills this local from
                // registers; the inliner must generate an explicit copy instead.
                func.implicit_param_copies.push(super::ImplicitParamCopy {
                    arg_index: arg_idx,
                    local_sym,
                    size_bytes: typ_size_bytes,
                    qword_type: self.types.long_id,
                });
            }

            // Don't emit a store here - the prologue codegen handles storing
            // from XMM0+XMM1/XMM2+XMM3/etc to local storage

            // Register as a local variable for name lookup (only if named parameter)
            if let Some(symbol_id) = symbol_id_opt {
                self.insert_local(
                    symbol_id,
                    LocalVarInfo {
                        sym: local_sym,
                        typ,
                        vla_size_sym: None,
                        vla_elem_type: None,
                        vla_dim_syms: vec![],
                        is_indirect: false,
                    },
                );
            }
        }

        // Store scalar parameters to local storage for SSA-correct reassignment handling
        // This ensures that if a parameter is reassigned inside a branch, phi nodes
        // are properly inserted at merge points.
        for (name, symbol_id_opt, typ, arg_pseudo) in scalar_params {
            // Create a symbol pseudo for this local variable (its address)
            let local_sym = self.alloc_pseudo();
            let sym = Pseudo::sym(local_sym, name.clone());
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(sym);
                let mods = self.types.modifiers(typ);
                let is_volatile = mods.contains(TypeModifiers::VOLATILE);
                let is_atomic = mods.contains(TypeModifiers::ATOMIC);
                func.add_local(&name, local_sym, typ, is_volatile, is_atomic, None, None);
            }

            // Store the incoming argument value to the local
            let typ_size = self.types.size_bits(typ);
            self.emit(Instruction::store(arg_pseudo, local_sym, 0, typ, typ_size));

            // Register as a local variable for name lookup (only if named parameter)
            if let Some(symbol_id) = symbol_id_opt {
                self.insert_local(
                    symbol_id,
                    LocalVarInfo {
                        sym: local_sym,
                        typ,
                        vla_size_sym: None,
                        vla_elem_type: None,
                        vla_dim_syms: vec![],
                        is_indirect: false,
                    },
                );
            }
        }

        // Linearize body
        self.linearize_stmt(&func.body);

        // Ensure function ends with a return
        if !self.is_terminated() {
            if ret_kind == TypeKind::Void {
                self.emit(Instruction::ret(None));
            } else {
                // Return 0 as default, widened to actual return type
                let ret_type = func.return_type;
                let ret_size = self.types.size_bits(ret_type).max(32);
                let zero = self.emit_const(0, ret_type);
                self.emit(Instruction::ret_typed(Some(zero), ret_type, ret_size));
            }
        }

        // Run SSA conversion if enabled
        if self.run_ssa {
            if let Some(ref mut ir_func) = self.current_func {
                ssa_convert(ir_func, self.types);
                // Note: ssa_convert sets ir_func.next_pseudo to account for phi nodes
            }
        } else {
            // Only set next_pseudo if SSA was NOT run (SSA sets its own)
            if let Some(ref mut ir_func) = self.current_func {
                ir_func.next_pseudo = self.next_pseudo;
            }
        }

        // Pop function-level scope
        self.pop_scope();

        // Add function to module
        if let Some(ir_func) = self.current_func.take() {
            self.module.add_function(ir_func);
        }
    }

    // ========================================================================
    // Statement linearization
    // ========================================================================

    /// Emit large struct return via hidden pointer (sret)
    pub(crate) fn emit_sret_return(&mut self, e: &Expr, sret_ptr: PseudoId, struct_size: u32) {
        let src_addr = self.linearize_lvalue(e);
        let struct_bytes = struct_size as i64 / 8;
        let mut byte_offset = 0i64;

        while byte_offset < struct_bytes {
            let temp = self.alloc_reg_pseudo();
            self.emit(Instruction::load(
                temp,
                src_addr,
                byte_offset,
                self.types.long_id,
                64,
            ));
            self.emit(Instruction::store(
                temp,
                sret_ptr,
                byte_offset,
                self.types.long_id,
                64,
            ));
            byte_offset += 8;
        }

        self.emit(Instruction::ret_typed(
            Some(sret_ptr),
            self.types.void_ptr_id,
            64,
        ));
    }

    /// Emit two-register struct return (9-16 bytes)
    pub(crate) fn emit_two_reg_return(&mut self, e: &Expr, ret_type: TypeId) {
        let src_addr = self.linearize_lvalue(e);
        let struct_size = self.types.size_bits(ret_type);

        // Load first 8 bytes
        let low_temp = self.alloc_reg_pseudo();
        self.emit(Instruction::load(
            low_temp,
            src_addr,
            0,
            self.types.long_id,
            64,
        ));

        // Load second portion (remaining bytes, up to 8)
        let high_temp = self.alloc_reg_pseudo();
        let high_size = std::cmp::min(64, struct_size - 64);
        self.emit(Instruction::load(
            high_temp,
            src_addr,
            8,
            self.types.long_id,
            high_size,
        ));

        // Emit return with both values and ABI info for two-register return
        let mut ret_insn = Instruction::ret_typed(Some(low_temp), ret_type, struct_size);
        ret_insn.src.push(high_temp);
        let abi = get_abi_for_conv(self.current_calling_conv, self.target);
        let ret_class = abi.classify_return(ret_type, self.types);
        ret_insn.abi_info = Some(Box::new(CallAbiInfo::new(vec![], ret_class)));
        self.emit(ret_insn);
    }

    // ========================================================================
    // Expression linearization
    // ========================================================================

    /// Get the type of an expression.
    /// PANICS if expression has no type - type evaluation pass must run first.
    /// The IR requires fully typed input from the type evaluation pass.
    pub(crate) fn expr_type(&self, expr: &Expr) -> TypeId {
        expr.typ.expect(
            "BUG: expression has no type. Type evaluation pass must run before linearization.",
        )
    }

    /// Check if an expression is "pure" (side-effect-free).
    /// Pure expressions can be speculatively evaluated, enabling cmov/csel codegen.
    ///
    /// An expression is pure if it contains NO:
    /// - Function calls
    /// - Volatile accesses
    /// - Pre/post increment/decrement (++, --)
    /// - Assignments (=, +=, -=, etc.)
    /// - Statement expressions (GNU extension with potential side effects)
    pub(crate) fn is_pure_expr(&self, expr: &Expr) -> bool {
        match &expr.kind {
            // Literals are always pure
            ExprKind::IntLit(_)
            | ExprKind::Int128Lit(_)
            | ExprKind::FloatLit(_)
            | ExprKind::CharLit(_)
            | ExprKind::StringLit(_)
            | ExprKind::WideStringLit(_) => true,

            // Identifiers are pure unless volatile
            ExprKind::Ident(_) => {
                if let Some(typ) = expr.typ {
                    !self.types.modifiers(typ).contains(TypeModifiers::VOLATILE)
                } else {
                    true
                }
            }

            // __func__ is a pure string-like value
            ExprKind::FuncName => true,

            // Binary ops are pure if both operands are pure AND the
            // operator can't trap. Division and modulo cause SIGFPE
            // on division by zero, so they're never pure.
            ExprKind::Binary {
                op, left, right, ..
            } => {
                !matches!(op, BinaryOp::Div | BinaryOp::Mod)
                    && self.is_pure_expr(left)
                    && self.is_pure_expr(right)
            }

            // Unary ops are pure if operand is pure, except for pre-inc/dec and dereference.
            // Dereference (*ptr) can cause UB/crash if the pointer is NULL or invalid,
            // so we must not eagerly evaluate it in conditional expressions.
            ExprKind::Unary { op, operand, .. } => match op {
                UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::Deref => false,
                _ => self.is_pure_expr(operand),
            },

            // Post-increment/decrement have side effects
            ExprKind::PostInc(_) | ExprKind::PostDec(_) => false,

            // Ternary is pure if all parts are pure
            ExprKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                self.is_pure_expr(cond)
                    && self.is_pure_expr(then_expr)
                    && self.is_pure_expr(else_expr)
            }

            // Function calls are never pure (may have side effects)
            ExprKind::Call { .. } => false,

            // Member access through struct value (.) is pure if the base is pure.
            ExprKind::Member { expr, .. } => self.is_pure_expr(expr),

            // Arrow access (ptr->member) can cause UB/crash if ptr is NULL,
            // so we must not eagerly evaluate it in conditional expressions.
            ExprKind::Arrow { .. } => false,

            // Array indexing can cause UB/crash if the pointer is invalid,
            // so we must not eagerly evaluate it in conditional expressions.
            ExprKind::Index { .. } => false,

            // Casts are pure if the operand is pure
            ExprKind::Cast { expr, .. } => self.is_pure_expr(expr),

            // Assignments have side effects
            ExprKind::Assign { .. } => false,

            // Sizeof and _Alignof are always pure (compile-time constant)
            ExprKind::SizeofType(_)
            | ExprKind::SizeofExpr(_)
            | ExprKind::AlignofType(_)
            | ExprKind::AlignofExpr(_) => true,

            // Comma expressions: pure if all sub-expressions are pure
            ExprKind::Comma(exprs) => exprs.iter().all(|e| self.is_pure_expr(e)),

            // Compound literals may have side effects in initializers
            ExprKind::CompoundLiteral { .. } => false,

            // Init lists may have side effects
            ExprKind::InitList { .. } => false,

            // Statement expressions have side effects
            ExprKind::StmtExpr { .. } => false,

            // Variadic builtins have side effects
            ExprKind::VaStart { .. }
            | ExprKind::VaArg { .. }
            | ExprKind::VaEnd { .. }
            | ExprKind::VaCopy { .. } => false,

            // Offsetof is always pure (compile-time constant)
            ExprKind::OffsetOf { .. } => true,

            // Builtins: bswap, ctz, clz, popcount are pure
            ExprKind::Bswap16 { arg }
            | ExprKind::Bswap32 { arg }
            | ExprKind::Bswap64 { arg }
            | ExprKind::Ctz { arg }
            | ExprKind::Ctzl { arg }
            | ExprKind::Ctzll { arg }
            | ExprKind::Clz { arg }
            | ExprKind::Clzl { arg }
            | ExprKind::Clzll { arg }
            | ExprKind::Popcount { arg }
            | ExprKind::Popcountl { arg }
            | ExprKind::Popcountll { arg }
            | ExprKind::Fabs { arg }
            | ExprKind::Fabsf { arg }
            | ExprKind::Fabsl { arg }
            | ExprKind::Signbit { arg }
            | ExprKind::Signbitf { arg }
            | ExprKind::Signbitl { arg } => self.is_pure_expr(arg),

            // Alloca allocates memory - not pure
            ExprKind::Alloca { .. } => false,

            // Memory builtins modify memory - not pure
            ExprKind::Memset { .. } | ExprKind::Memcpy { .. } | ExprKind::Memmove { .. } => false,

            // Unreachable is pure (no side effects, just UB hint)
            ExprKind::Unreachable => true,

            // Frame/return address builtins are pure (just read registers)
            ExprKind::FrameAddress { .. } | ExprKind::ReturnAddress { .. } => true,

            // Setjmp/longjmp have control flow side effects
            ExprKind::Setjmp { .. } | ExprKind::Longjmp { .. } => false,

            // Atomic operations have side effects (memory ordering)
            ExprKind::C11AtomicInit { .. }
            | ExprKind::C11AtomicLoad { .. }
            | ExprKind::C11AtomicStore { .. }
            | ExprKind::C11AtomicExchange { .. }
            | ExprKind::C11AtomicCompareExchangeStrong { .. }
            | ExprKind::C11AtomicCompareExchangeWeak { .. }
            | ExprKind::C11AtomicFetchAdd { .. }
            | ExprKind::C11AtomicFetchSub { .. }
            | ExprKind::C11AtomicFetchAnd { .. }
            | ExprKind::C11AtomicFetchOr { .. }
            | ExprKind::C11AtomicFetchXor { .. }
            | ExprKind::C11AtomicThreadFence { .. }
            | ExprKind::C11AtomicSignalFence { .. }
            | ExprKind::BuiltinComplex { .. } => false,
        }
    }

    /// Resolve an incomplete struct/union type to its complete definition.
    ///
    /// When a struct is forward-declared (e.g., `struct foo;`) and later
    /// defined, the forward declaration creates an incomplete TypeId.
    /// Pointers to the forward-declared type still reference this incomplete
    /// TypeId even after the struct is fully defined with a new TypeId.
    ///
    /// This method looks up the complete definition in the symbol table
    /// using the struct's tag name, returning the complete TypeId if found.
    pub(crate) fn resolve_struct_type(&self, type_id: TypeId) -> TypeId {
        let typ = self.types.get(type_id);

        // Only try to resolve struct/union types
        if typ.kind != TypeKind::Struct && typ.kind != TypeKind::Union {
            return type_id;
        }

        // Check if this is an incomplete type with a tag
        if let Some(ref composite) = typ.composite {
            if composite.is_complete {
                // Already complete, no resolution needed
                return type_id;
            }
            if let Some(tag) = composite.tag {
                // Look up the tag in the symbol table to find the complete type
                if let Some(symbol) = self.symbols.lookup_tag(tag) {
                    // Return the complete type from the symbol table
                    return symbol.typ;
                }
            }
        }

        // Couldn't resolve, return original
        type_id
    }

    /// Linearize an expression as an lvalue (get its address)
    pub(crate) fn linearize_lvalue(&mut self, expr: &Expr) -> PseudoId {
        match &expr.kind {
            ExprKind::Ident(symbol_id) => {
                let name_str = self.symbol_name(*symbol_id);
                // For local variables, emit SymAddr to get the stack address
                if let Some(local) = self.locals.get(symbol_id).cloned() {
                    // Check if this is a static local (sentinel value)
                    if local.sym.0 == u32::MAX {
                        // Static local - look up the global name
                        let key = format!("{}.{}", self.current_func_name, name_str);
                        if let Some(static_info) = self.static_locals.get(&key).cloned() {
                            let sym_id = self.alloc_pseudo();
                            let pseudo = Pseudo::sym(sym_id, static_info.global_name);
                            if let Some(func) = &mut self.current_func {
                                func.add_pseudo(pseudo);
                            }
                            let result = self.alloc_pseudo();
                            self.emit(Instruction::sym_addr(
                                result,
                                sym_id,
                                self.types.pointer_to(static_info.typ),
                            ));
                            return result;
                        } else {
                            unreachable!("static local sentinel without static_locals entry");
                        }
                    }
                    // Check if this local holds a pointer to the actual data
                    // (e.g., va_list parameters due to array-to-pointer decay at call site).
                    // If so, load the pointer instead of taking the address.
                    if local.is_indirect {
                        // The local stores a pointer; load and return it
                        let ptr_type = self.types.pointer_to(local.typ);
                        let result = self.alloc_pseudo();
                        let size = self.types.size_bits(ptr_type);
                        self.emit(Instruction::load(result, local.sym, 0, ptr_type, size));
                        return result;
                    }

                    let result = self.alloc_pseudo();
                    self.emit(Instruction::sym_addr(
                        result,
                        local.sym,
                        self.types.pointer_to(local.typ),
                    ));
                    result
                } else if let Some(&param_pseudo) = self.var_map.get(&name_str) {
                    // Parameter whose address is taken
                    let param_type = self.expr_type(expr);
                    let type_kind = self.types.kind(param_type);

                    // va_list parameters are special: the parameter value IS already a pointer
                    // to the va_list structure (due to array-to-pointer decay at call site).
                    // Return the pointer value directly instead of spilling.
                    if type_kind == TypeKind::VaList {
                        return param_pseudo;
                    }

                    // For other parameters, spill to local storage.
                    // Parameters are pass-by-value in the IR (Arg pseudos), but if
                    // their address is taken, we need to copy to a stack slot first.
                    let size = self.types.size_bits(param_type);

                    // Create a local variable to hold the parameter value
                    let local_sym = self.alloc_pseudo();
                    let local_pseudo = Pseudo::sym(local_sym, format!("{}_spill", name_str));
                    if let Some(func) = &mut self.current_func {
                        func.add_pseudo(local_pseudo);
                        func.locals.insert(
                            format!("{}_spill", name_str),
                            super::LocalVar {
                                sym: local_sym,
                                typ: param_type,
                                is_volatile: false,
                                is_atomic: false,
                                decl_block: self.current_bb,
                                explicit_align: None, // parameter spill storage
                            },
                        );
                    }

                    // Store the parameter value to the local
                    self.emit(Instruction::store(
                        param_pseudo,
                        local_sym,
                        0,
                        param_type,
                        size,
                    ));

                    // Update locals map so future accesses use the spilled location
                    self.insert_local(
                        *symbol_id,
                        LocalVarInfo {
                            sym: local_sym,
                            typ: param_type,
                            vla_size_sym: None,
                            vla_elem_type: None,
                            vla_dim_syms: vec![],
                            is_indirect: false,
                        },
                    );

                    // Also update var_map to point to the local for future value accesses
                    // (so reads go through load instead of using the original Arg pseudo)
                    // Note: We leave var_map unchanged here because reads should use
                    // the stored value via load from the local.

                    // Return address of the local
                    let result = self.alloc_pseudo();
                    self.emit(Instruction::sym_addr(
                        result,
                        local_sym,
                        self.types.pointer_to(param_type),
                    ));
                    result
                } else {
                    // Global variable - emit SymAddr to get its address
                    let sym_id = self.alloc_pseudo();
                    let pseudo = Pseudo::sym(sym_id, name_str.clone());
                    if let Some(func) = &mut self.current_func {
                        func.add_pseudo(pseudo);
                    }
                    let result = self.alloc_pseudo();
                    let typ = self.expr_type(expr);
                    self.emit(Instruction::sym_addr(
                        result,
                        sym_id,
                        self.types.pointer_to(typ),
                    ));
                    result
                }
            }
            ExprKind::Unary {
                op: UnaryOp::Deref,
                operand,
            } => {
                // *ptr as lvalue = ptr itself
                self.linearize_expr(operand)
            }
            ExprKind::Member {
                expr: inner,
                member,
            } => {
                // s.m as lvalue = &s + offset(m)
                let base = self.linearize_lvalue(inner);
                let base_struct_type = self.expr_type(inner);
                // Resolve if the struct type is incomplete (forward-declared)
                let struct_type = self.resolve_struct_type(base_struct_type);
                let member_info =
                    self.types
                        .find_member(struct_type, *member)
                        .unwrap_or_else(|| MemberInfo {
                            offset: 0,
                            typ: self.expr_type(expr),
                            bit_offset: None,
                            bit_width: None,
                            storage_unit_size: None,
                        });

                if member_info.offset == 0 {
                    base
                } else {
                    let offset_val =
                        self.emit_const(member_info.offset as i128, self.types.long_id);
                    let result = self.alloc_pseudo();
                    self.emit(Instruction::binop(
                        Opcode::Add,
                        result,
                        base,
                        offset_val,
                        self.types.long_id,
                        64,
                    ));
                    result
                }
            }
            ExprKind::Arrow {
                expr: inner,
                member,
            } => {
                // ptr->m as lvalue = ptr + offset(m)
                let ptr = self.linearize_expr(inner);
                let ptr_type = self.expr_type(inner);
                let base_struct_type = self
                    .types
                    .base_type(ptr_type)
                    .unwrap_or_else(|| self.expr_type(expr));
                // Resolve if the struct type is incomplete (forward-declared)
                let struct_type = self.resolve_struct_type(base_struct_type);
                let member_info =
                    self.types
                        .find_member(struct_type, *member)
                        .unwrap_or_else(|| MemberInfo {
                            offset: 0,
                            typ: self.expr_type(expr),
                            bit_offset: None,
                            bit_width: None,
                            storage_unit_size: None,
                        });

                if member_info.offset == 0 {
                    ptr
                } else {
                    let offset_val =
                        self.emit_const(member_info.offset as i128, self.types.long_id);
                    let result = self.alloc_pseudo();
                    self.emit(Instruction::binop(
                        Opcode::Add,
                        result,
                        ptr,
                        offset_val,
                        self.types.long_id,
                        64,
                    ));
                    result
                }
            }
            ExprKind::Index { array, index } => {
                // arr[idx] as lvalue = arr + idx * sizeof(elem)
                // Handle commutative form: 0[arr] is equivalent to arr[0]
                let array_type = self.expr_type(array);
                let index_type = self.expr_type(index);

                let array_kind = self.types.kind(array_type);
                let (ptr_expr, idx_expr, idx_type) =
                    if array_kind == TypeKind::Pointer || array_kind == TypeKind::Array {
                        (array, index, index_type)
                    } else {
                        // Swap: index is actually the pointer/array
                        (index, array, array_type)
                    };

                let arr = self.linearize_expr(ptr_expr);
                let idx = self.linearize_expr(idx_expr);
                let elem_type = self.expr_type(expr);
                let elem_size = self.types.size_bits(elem_type) / 8;
                let elem_size_val = self.emit_const(elem_size as i128, self.types.long_id);

                // Sign-extend index to 64-bit for proper pointer arithmetic (negative indices)
                let idx_extended = self.emit_convert(idx, idx_type, self.types.long_id);

                let offset = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::Mul,
                    offset,
                    idx_extended,
                    elem_size_val,
                    self.types.long_id,
                    64,
                ));

                let addr = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::Add,
                    addr,
                    arr,
                    offset,
                    self.types.long_id,
                    64,
                ));
                addr
            }
            ExprKind::CompoundLiteral { typ, elements } => {
                // Compound literal as lvalue: create it and return its address
                // This is used for &(struct S){...} and large struct assignment like *p = (struct S){...}
                let sym_id = self.alloc_pseudo();
                let unique_name = format!(".compound_literal.{}", sym_id.0);
                let sym = Pseudo::sym(sym_id, unique_name.clone());
                if let Some(func) = &mut self.current_func {
                    func.add_pseudo(sym);
                    func.add_local(
                        &unique_name,
                        sym_id,
                        *typ,
                        false,
                        false,
                        self.current_bb,
                        None,
                    );
                }

                // For compound literals with partial initialization, C99 6.7.8p21 requires
                // zero-initialization of all subobjects not explicitly initialized.
                // Zero the entire compound literal first, then initialize specific members.
                let type_kind = self.types.kind(*typ);
                if type_kind == TypeKind::Struct
                    || type_kind == TypeKind::Union
                    || type_kind == TypeKind::Array
                {
                    self.emit_aggregate_zero(sym_id, *typ);
                }

                self.linearize_init_list(sym_id, *typ, elements);

                // Return address of the compound literal
                let result = self.alloc_reg_pseudo();
                let ptr_type = self.types.pointer_to(*typ);
                self.emit(Instruction::sym_addr(result, sym_id, ptr_type));
                result
            }
            _ => {
                // Fallback: just evaluate the expression (shouldn't happen for valid lvalues)
                self.linearize_expr(expr)
            }
        }
    }

    /// Linearize a type cast expression
    pub(crate) fn linearize_cast(&mut self, inner_expr: &Expr, cast_type: TypeId) -> PseudoId {
        let src = self.linearize_expr(inner_expr);
        let src_type = self.expr_type(inner_expr);

        // Emit conversion if needed
        let src_is_float = self.types.is_float(src_type);
        let dst_is_float = self.types.is_float(cast_type);

        if src_is_float && !dst_is_float {
            // Float to integer conversion
            let result = self.alloc_reg_pseudo();
            // FCvtS for signed int, FCvtU for unsigned
            let opcode = if self.types.is_unsigned(cast_type) {
                Opcode::FCvtU
            } else {
                Opcode::FCvtS
            };
            let dst_size = self.types.size_bits(cast_type);
            let mut insn = Instruction::new(opcode)
                .with_target(result)
                .with_src(src)
                .with_type_and_size(cast_type, dst_size);
            insn.src_size = self.types.size_bits(src_type);
            insn.src_typ = Some(src_type);
            self.emit(insn);
            result
        } else if !src_is_float && dst_is_float {
            // Integer to float conversion
            let result = self.alloc_reg_pseudo();
            // SCvtF for signed int, UCvtF for unsigned
            let opcode = if self.types.is_unsigned(src_type) {
                Opcode::UCvtF
            } else {
                Opcode::SCvtF
            };
            let dst_size = self.types.size_bits(cast_type);
            let mut insn = Instruction::new(opcode)
                .with_target(result)
                .with_src(src)
                .with_type_and_size(cast_type, dst_size);
            insn.src_size = self.types.size_bits(src_type);
            insn.src_typ = Some(src_type);
            self.emit(insn);
            result
        } else if src_is_float && dst_is_float {
            // Float to float conversion (e.g., float to double)
            let src_size = self.types.size_bits(src_type);
            let dst_size = self.types.size_bits(cast_type);
            if src_size != dst_size {
                let result = self.alloc_reg_pseudo();
                let mut insn = Instruction::new(Opcode::FCvtF)
                    .with_target(result)
                    .with_src(src)
                    .with_type_and_size(cast_type, dst_size);
                insn.src_size = src_size;
                insn.src_typ = Some(src_type);
                self.emit(insn);
                result
            } else {
                src // Same size, no conversion needed
            }
        } else {
            // Integer to integer conversion
            // Use emit_convert for proper type conversions including _Bool
            self.emit_convert(src, src_type, cast_type)
        }
    }

    /// Linearize a struct member access expression (e.g., s.member)
    pub(crate) fn linearize_member(
        &mut self,
        expr: &Expr,
        inner_expr: &Expr,
        member: StringId,
    ) -> PseudoId {
        let base = self.linearize_lvalue(inner_expr);
        let base_struct_type = self.expr_type(inner_expr);
        let struct_type = self.resolve_struct_type(base_struct_type);
        self.emit_member_access(base, struct_type, member, self.expr_type(expr))
    }

    /// Linearize a pointer member access expression (e.g., p->member)
    pub(crate) fn linearize_arrow(
        &mut self,
        expr: &Expr,
        inner_expr: &Expr,
        member: StringId,
    ) -> PseudoId {
        let ptr = self.linearize_expr(inner_expr);
        let ptr_type = self.expr_type(inner_expr);
        let base_struct_type = self
            .types
            .base_type(ptr_type)
            .unwrap_or_else(|| self.expr_type(expr));
        let struct_type = self.resolve_struct_type(base_struct_type);
        self.emit_member_access(ptr, struct_type, member, self.expr_type(expr))
    }

    /// Shared logic for member access (both `.` and `->`).
    /// `base` is the address of the struct (for `.`) or the pointer value (for `->`).
    pub(crate) fn emit_member_access(
        &mut self,
        base: PseudoId,
        struct_type: TypeId,
        member: StringId,
        fallback_type: TypeId,
    ) -> PseudoId {
        let member_info = self
            .types
            .find_member(struct_type, member)
            .unwrap_or(MemberInfo {
                offset: 0,
                typ: fallback_type,
                bit_offset: None,
                bit_width: None,
                storage_unit_size: None,
            });

        // If member type is an array, return the address (arrays decay to pointers)
        if self.types.kind(member_info.typ) == TypeKind::Array {
            if member_info.offset == 0 {
                base
            } else {
                let result = self.alloc_pseudo();
                let offset_val = self.emit_const(member_info.offset as i128, self.types.long_id);
                self.emit(Instruction::binop(
                    Opcode::Add,
                    result,
                    base,
                    offset_val,
                    self.types.long_id,
                    64,
                ));
                result
            }
        } else if let (Some(bit_offset), Some(bit_width), Some(storage_size)) = (
            member_info.bit_offset,
            member_info.bit_width,
            member_info.storage_unit_size,
        ) {
            // Bitfield read
            self.emit_bitfield_load(
                base,
                member_info.offset,
                bit_offset,
                bit_width,
                storage_size,
                member_info.typ,
            )
        } else {
            let size = self.types.size_bits(member_info.typ);
            let member_kind = self.types.kind(member_info.typ);

            // Large structs (size > 64) can't be loaded into registers - return address
            if (member_kind == TypeKind::Struct || member_kind == TypeKind::Union) && size > 64 {
                if member_info.offset == 0 {
                    base
                } else {
                    let result = self.alloc_pseudo();
                    let offset_val =
                        self.emit_const(member_info.offset as i128, self.types.long_id);
                    self.emit(Instruction::binop(
                        Opcode::Add,
                        result,
                        base,
                        offset_val,
                        self.types.long_id,
                        64,
                    ));
                    result
                }
            } else {
                let result = self.alloc_pseudo();
                self.emit(Instruction::load(
                    result,
                    base,
                    member_info.offset as i64,
                    member_info.typ,
                    size,
                ));
                result
            }
        }
    }

    /// Linearize an array index expression (e.g., arr[i])
    pub(crate) fn linearize_index(&mut self, expr: &Expr, array: &Expr, index: &Expr) -> PseudoId {
        // In C, a[b] is defined as *(a + b), so either operand can be the pointer
        // Handle commutative form: 0[arr] is equivalent to arr[0]
        let array_type = self.expr_type(array);
        let index_type = self.expr_type(index);

        let array_kind = self.types.kind(array_type);
        let (ptr_expr, idx_expr, idx_type) =
            if array_kind == TypeKind::Pointer || array_kind == TypeKind::Array {
                (array, index, index_type)
            } else {
                // Swap: index is actually the pointer/array
                (index, array, array_type)
            };

        let arr = self.linearize_expr(ptr_expr);
        let idx = self.linearize_expr(idx_expr);

        // Get element type from the expression type
        let elem_type = self.expr_type(expr);
        let ptr_typ = self.types.long_id;

        // Check if we're indexing a VLA's outer dimension
        // This requires runtime stride computation.
        // For int arr[n][m], accessing arr[i] needs stride = m * sizeof(int)
        let elem_size_val = if let ExprKind::Ident(symbol_id) = &ptr_expr.kind {
            if let Some(info) = self.locals.get(symbol_id).cloned() {
                // Check if this is a multi-dimensional VLA AND elem_type is an array
                // (meaning we're accessing an outer dimension, not the innermost)
                if info.vla_dim_syms.len() > 1 && self.types.kind(elem_type) == TypeKind::Array {
                    // Compute runtime stride:
                    // stride = (product of inner dimensions) * sizeof(innermost element)
                    // For int arr[n][m] accessing arr[i]: stride = m * sizeof(int)
                    // For int arr[n][m][k] accessing arr[i]: stride = m * k * sizeof(int)

                    // Get innermost element type and its size
                    let mut innermost = elem_type;
                    while self.types.kind(innermost) == TypeKind::Array {
                        innermost = self.types.base_type(innermost).unwrap_or(self.types.int_id);
                    }
                    let innermost_size = self.types.size_bytes(innermost) as i64;

                    // Load all dimension sizes except the first (outermost we're indexing)
                    // and multiply them together
                    let mut stride: Option<PseudoId> = None;
                    for dim_sym in info.vla_dim_syms.iter().skip(1) {
                        // Load the dimension size
                        let dim_val = self.alloc_pseudo();
                        let load_insn =
                            Instruction::load(dim_val, *dim_sym, 0, self.types.ulong_id, 64);
                        self.emit(load_insn);

                        stride = Some(match stride {
                            None => dim_val,
                            Some(prev) => {
                                let result = self.alloc_pseudo();
                                self.emit(Instruction::binop(
                                    Opcode::Mul,
                                    result,
                                    prev,
                                    dim_val,
                                    ptr_typ,
                                    64,
                                ));
                                result
                            }
                        });
                    }

                    // Multiply by sizeof(innermost element)
                    let innermost_size_val =
                        self.emit_const(innermost_size as i128, self.types.long_id);
                    match stride {
                        Some(s) => {
                            let result = self.alloc_pseudo();
                            self.emit(Instruction::binop(
                                Opcode::Mul,
                                result,
                                s,
                                innermost_size_val,
                                ptr_typ,
                                64,
                            ));
                            result
                        }
                        None => innermost_size_val,
                    }
                } else {
                    // Not a multi-dimensional VLA or accessing innermost dimension
                    // Use compile-time size
                    let elem_size = self.types.size_bits(elem_type) / 8;
                    self.emit_const(elem_size as i128, self.types.long_id)
                }
            } else {
                // Variable not found in locals (global or something else)
                let elem_size = self.types.size_bits(elem_type) / 8;
                self.emit_const(elem_size as i128, self.types.long_id)
            }
        } else {
            // Not indexing an identifier directly (e.g., arr[i][j] where arr[i] is an Index)
            // Use compile-time size
            let elem_size = self.types.size_bits(elem_type) / 8;
            self.emit_const(elem_size as i128, self.types.long_id)
        };

        // Sign-extend index to 64-bit for proper pointer arithmetic (negative indices)
        let idx_extended = self.emit_convert(idx, idx_type, self.types.long_id);

        let offset = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Mul,
            offset,
            idx_extended,
            elem_size_val,
            ptr_typ,
            64,
        ));

        let addr = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Add,
            addr,
            arr,
            offset,
            ptr_typ,
            64,
        ));

        // If element type is an array, just return the address (arrays decay to pointers)
        let elem_kind = self.types.kind(elem_type);
        if elem_kind == TypeKind::Array {
            addr
        } else {
            let size = self.types.size_bits(elem_type);
            // Large structs/unions (> 64 bits) can't be loaded into registers - return address
            // Assignment will handle the actual copy via emit_assign's large struct handling
            if (elem_kind == TypeKind::Struct || elem_kind == TypeKind::Union) && size > 64 {
                addr
            } else {
                let result = self.alloc_pseudo();
                self.emit(Instruction::load(result, addr, 0, elem_type, size));
                result
            }
        }
    }

    /// Linearize a function call expression
    pub(crate) fn linearize_call(
        &mut self,
        expr: &Expr,
        func_expr: &Expr,
        args: &[Expr],
    ) -> PseudoId {
        // Determine if this is a direct or indirect call.
        // We need to check the TYPE of the function expression:
        // - If it's TypeKind::Function, it's a direct call to a function
        // - If it's TypeKind::Pointer to Function, it's an indirect call through function pointer
        let is_function_pointer = func_expr.typ.is_some_and(|t| {
            let typ = self.types.get(t);
            typ.kind == TypeKind::Pointer
        });

        let (func_name, indirect_target) = match &func_expr.kind {
            ExprKind::Ident(symbol_id) if !is_function_pointer => {
                // Direct call to named function (not a function pointer variable)
                (self.symbol_name(*symbol_id), None)
            }
            ExprKind::Unary {
                op: UnaryOp::Deref,
                operand,
            } => {
                // Explicit dereference form: (*fp)(args) or (*fpp)(args)
                // Check the type of the operand to determine behavior:
                // - If operand is function pointer (*fn): call through operand value
                // - If operand is pointer-to-function-pointer (**fn): dereference first
                let operand_type = self.expr_type(operand);
                let operand_kind = self.types.kind(operand_type);
                if operand_kind == TypeKind::Pointer {
                    // Check what it points to
                    let base_type = self.types.base_type(operand_type);
                    let base_kind = base_type.map(|t| self.types.kind(t));
                    if base_kind == Some(TypeKind::Function) {
                        // Operand is function pointer - use its value directly
                        let func_addr = self.linearize_expr(operand);
                        ("<indirect>".to_string(), Some(func_addr))
                    } else {
                        // Operand is pointer-to-pointer - dereference to get function pointer
                        let func_addr = self.linearize_expr(func_expr);
                        ("<indirect>".to_string(), Some(func_addr))
                    }
                } else {
                    // Unknown case - try to linearize the full expression
                    let func_addr = self.linearize_expr(func_expr);
                    ("<indirect>".to_string(), Some(func_addr))
                }
            }
            _ => {
                // Indirect call through function pointer variable: fp(args)
                // This includes identifiers that are function pointer variables
                let func_addr = self.linearize_expr(func_expr);
                ("<indirect>".to_string(), Some(func_addr))
            }
        };

        let typ = self.expr_type(expr); // Use evaluated type (function return type)

        // Check if this is a variadic function call and if it's noreturn
        // If the function expression has a type, check its variadic and noreturn flags
        let (variadic_arg_start, is_noreturn_call) = if let Some(func_type) = func_expr.typ {
            let ft = self.types.get(func_type);
            let variadic = if ft.variadic {
                // Variadic args start after the fixed parameters
                ft.params.as_ref().map(|p| p.len())
            } else {
                None
            };
            (variadic, ft.noreturn)
        } else {
            (None, false) // No type info, assume non-variadic and returns
        };

        // Check if function returns a large struct or complex type
        // Large structs: allocate space and pass address as hidden first argument
        // Complex types: allocate local storage for result (needs stack for 16-byte value)
        // Two-register structs (9-16 bytes): allocate local storage, codegen stores two regs
        let typ_kind = self.types.kind(typ);
        let struct_size_bits = self.types.size_bits(typ);
        let returns_large_struct = (typ_kind == TypeKind::Struct || typ_kind == TypeKind::Union)
            && struct_size_bits > self.target.max_aggregate_register_bits;
        let returns_two_reg_struct = (typ_kind == TypeKind::Struct || typ_kind == TypeKind::Union)
            && struct_size_bits > 64
            && struct_size_bits <= 128
            && !returns_large_struct;
        let returns_complex = self.types.is_complex(typ);

        let (result_sym, mut arg_vals, mut arg_types_vec) = if returns_large_struct {
            // Allocate local storage for the return value
            let sret_sym = self.alloc_pseudo();
            let sret_pseudo = Pseudo::sym(sret_sym, format!("__sret_{}", sret_sym.0));
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(sret_pseudo);
                // Internal sret storage is never volatile or atomic
                func.add_local(
                    format!("__sret_{}", sret_sym.0),
                    sret_sym,
                    typ,
                    false, // not volatile
                    false, // not atomic
                    self.current_bb,
                    None, // no explicit alignment
                );
            }

            // Get address of the allocated space
            let sret_addr = self.alloc_reg_pseudo();
            self.emit(Instruction::sym_addr(
                sret_addr,
                sret_sym,
                self.types.pointer_to(typ),
            ));

            // Hidden return pointer is the first argument (pointer type)
            (sret_sym, vec![sret_addr], vec![self.types.pointer_to(typ)])
        } else if returns_two_reg_struct {
            // Two-register struct returns: allocate local storage for the result
            // Codegen will store RAX+RDX (x86-64) or X0+X1 (AArch64) to this location
            let local_sym = self.alloc_pseudo();
            let unique_name = format!("__2reg_{}", local_sym.0);
            let local_pseudo = Pseudo::sym(local_sym, unique_name.clone());
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(local_pseudo);
                func.add_local(
                    &unique_name,
                    local_sym,
                    typ,
                    false,
                    false,
                    self.current_bb,
                    None,
                );
            }
            (local_sym, Vec::new(), Vec::new())
        } else if returns_complex {
            // Complex returns: allocate local storage for the result
            // Complex values are 16 bytes and need stack storage
            let local_sym = self.alloc_pseudo();
            let unique_name = format!("__cret_{}", local_sym.0);
            let local_pseudo = Pseudo::sym(local_sym, unique_name.clone());
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(local_pseudo);
                func.add_local(
                    &unique_name,
                    local_sym,
                    typ,
                    false,
                    false,
                    self.current_bb,
                    None,
                );
            }
            (local_sym, Vec::new(), Vec::new())
        } else if (typ_kind == TypeKind::Struct || typ_kind == TypeKind::Union)
            && struct_size_bits > 0
            && struct_size_bits <= 64
        {
            // Small struct/union returns (<=64 bits, single register):
            // Allocate local storage so the result has a stable address.
            // The codegen stores RAX (or XMM0) to this location.
            // Without this, the result pseudo holds a raw value which
            // emit_assign's block_copy would incorrectly dereference as a pointer.
            let local_sym = self.alloc_pseudo();
            let unique_name = format!("__sret1_{}", local_sym.0);
            let local_pseudo = Pseudo::sym(local_sym, unique_name.clone());
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(local_pseudo);
                func.add_local(
                    &unique_name,
                    local_sym,
                    typ,
                    false,
                    false,
                    self.current_bb,
                    None,
                );
            }
            (local_sym, Vec::new(), Vec::new())
        } else {
            let result = self.alloc_pseudo();
            (result, Vec::new(), Vec::new())
        };

        // Get formal parameter types for implicit widening conversions.
        // When a narrow int (e.g., int) is passed to a wider parameter (e.g., long),
        // C requires implicit promotion. This is transparent for non-inlined calls
        // (the ABI handles it), but inlining exposes the mismatch since the argument
        // pseudo is used directly without conversion.
        let formal_param_types: Option<Vec<TypeId>> = func_expr.typ.and_then(|ft_id| {
            let ft = self.types.get(ft_id);
            ft.params.clone()
        });

        // Linearize regular arguments
        // For large structs, pass by reference (address) instead of by value
        // Note: We pass structs > 64 bits by reference. While the ABI allows
        // two-register passing for 9-16 byte structs, we don't implement that yet.
        // For complex types, pass address so codegen can load real/imag into XMM registers
        // For arrays (including VLAs), decay to pointer
        for (arg_idx, a) in args.iter().enumerate() {
            let mut arg_type = self.expr_type(a);
            let arg_kind = self.types.kind(arg_type);
            let arg_val = if (arg_kind == TypeKind::Struct || arg_kind == TypeKind::Union)
                && self.types.size_bits(arg_type) > 64
            {
                let size_bits = self.types.size_bits(arg_type);
                if size_bits > 128 {
                    // Large struct (> 16 bytes): keep struct type so ABI classifies as
                    // Indirect/MEMORY. The pseudo is still the struct's address;
                    // codegen will copy bytes to the stack.
                    arg_types_vec.push(arg_type);
                } else {
                    // Medium struct (9-16 bytes): check ABI classification
                    let abi = get_abi_for_conv(self.current_calling_conv, self.target);
                    let class = abi.classify_param(arg_type, self.types);
                    let is_two_fp_regs =
                        matches!(
                            class,
                            crate::abi::ArgClass::Direct { ref classes, .. }
                                if classes.len() == 2
                                    && classes.iter().all(|c| *c == crate::abi::RegClass::Sse)
                        ) || matches!(class, crate::abi::ArgClass::Hfa { count: 2, .. });
                    if is_two_fp_regs {
                        // All-SSE struct: keep struct type for 2-XMM passing
                        arg_types_vec.push(arg_type);
                    } else {
                        // Integer or mixed struct: pass as pointer (existing behavior)
                        arg_types_vec.push(self.types.pointer_to(arg_type));
                    }
                }
                // For lvalue expressions (identifiers, member access), linearize_lvalue
                // returns a symaddr (pointer). For rvalue expressions (call results),
                // linearize_lvalue falls through to linearize_expr which returns the
                // data pseudo, not its address. In that case we must emit a symaddr
                // to get the address for pointer-based struct passing.
                let is_lvalue = matches!(
                    a.kind,
                    ExprKind::Ident(_)
                        | ExprKind::Member { .. }
                        | ExprKind::Arrow { .. }
                        | ExprKind::Index { .. }
                        | ExprKind::Unary {
                            op: crate::parse::ast::UnaryOp::Deref,
                            ..
                        }
                        | ExprKind::CompoundLiteral { .. }
                );
                if is_lvalue {
                    self.linearize_lvalue(a)
                } else {
                    // Rvalue (e.g., function call returning struct): evaluate,
                    // then take address of the result local
                    let val = self.linearize_expr(a);
                    let result = self.alloc_reg_pseudo();
                    let ptr_type = self.types.pointer_to(arg_type);
                    self.emit(Instruction::sym_addr(result, val, ptr_type));
                    result
                }
            } else if self.types.is_complex(arg_type) {
                // Complex types: pass address, codegen loads real/imag into XMM registers
                // Type stays as complex (not pointer) so codegen knows it's complex
                arg_types_vec.push(arg_type);
                self.linearize_lvalue(a)
            } else if arg_kind == TypeKind::Array {
                // Array decay to pointer (C99 6.3.2.1)
                // This applies to both fixed-size arrays and VLAs
                let elem_type = self.types.base_type(arg_type).unwrap_or(self.types.int_id);
                arg_types_vec.push(self.types.pointer_to(elem_type));
                self.linearize_expr(a)
            } else if arg_kind == TypeKind::VaList {
                // va_list decay to pointer (C99 7.15.1)
                // va_list is defined as __va_list_tag[1] (an array), so it decays to
                // a pointer when passed to a function taking va_list parameter
                arg_types_vec.push(self.types.pointer_to(arg_type));
                self.linearize_lvalue(a)
            } else if arg_kind == TypeKind::Function {
                // Function decay to pointer (C99 6.3.2.1)
                // Function names passed as arguments decay to function pointers
                arg_types_vec.push(self.types.pointer_to(arg_type));
                self.linearize_expr(a)
            } else {
                let mut val = self.linearize_expr(a);

                // Implicit argument conversion when actual type differs from
                // formal parameter type. Covers:
                // - Integer widening: int→long (sign/zero extend)
                // - FP widening/narrowing: float↔double↔long double
                // - Int→FP: uint32_t→double (e.g., log10(uint32_t_val))
                // - FP→Int: rare but legal
                if let Some(ref params) = formal_param_types {
                    if arg_idx < params.len() {
                        let param_type = params[arg_idx];
                        let arg_size = self.types.size_bits(arg_type);
                        let param_size = self.types.size_bits(param_type);
                        let arg_is_int = self.types.is_integer(arg_type);
                        let param_is_int = self.types.is_integer(param_type);
                        let arg_is_fp = self.types.is_float(arg_type);
                        let param_is_fp = self.types.is_float(param_type);

                        let needs_convert =
                            // Integer widening (e.g., int→long)
                            (arg_is_int && param_is_int && arg_size < param_size
                                && arg_size <= 32 && param_size <= 64)
                            // FP size mismatch (float→double, long double→double, etc.)
                            || (arg_is_fp && param_is_fp && arg_size != param_size)
                            // Integer to FP (uint32_t→double, int→float, etc.)
                            || (arg_is_int && param_is_fp)
                            // FP to integer (rare but legal)
                            || (arg_is_fp && param_is_int);

                        if needs_convert {
                            val = self.emit_convert(val, arg_type, param_type);
                            arg_type = param_type;
                        }
                    }
                }

                // C99 6.5.2.2p7: Default argument promotions for variadic args.
                // float/_Float16 are promoted to double; integer promotions
                // also apply (already handled above for integers).
                if let Some(va_start) = variadic_arg_start {
                    if arg_idx >= va_start
                        && matches!(
                            self.types.kind(arg_type),
                            TypeKind::Float | TypeKind::Float16
                        )
                    {
                        val = self.emit_convert(val, arg_type, self.types.double_id);
                        arg_type = self.types.double_id;
                    }
                }

                arg_types_vec.push(arg_type);
                val
            };
            arg_vals.push(arg_val);
        }

        // Compute ABI classification for parameters and return value.
        // This provides rich metadata for the backend to generate correct calling code.
        // Use the current function's calling convention (which may be overridden via attributes)
        let abi = get_abi_for_conv(self.current_calling_conv, self.target);
        let param_classes: Vec<_> = arg_types_vec
            .iter()
            .map(|&t| abi.classify_param(t, self.types))
            .collect();
        let ret_class = abi.classify_return(typ, self.types);
        let call_abi_info = Box::new(CallAbiInfo::new(param_classes, ret_class));

        if returns_large_struct {
            // For large struct returns, the return value is the address
            // stored in result_sym (which is a local symbol containing the struct)
            let result = self.alloc_reg_pseudo();
            let ptr_typ = self.types.pointer_to(typ);
            let mut call_insn = if let Some(func_addr) = indirect_target {
                // Indirect call through function pointer
                Instruction::call_indirect(
                    Some(result),
                    func_addr,
                    arg_vals,
                    arg_types_vec,
                    ptr_typ,
                    64, // pointers are 64-bit
                )
            } else {
                // Direct call
                Instruction::call(
                    Some(result),
                    &func_name,
                    arg_vals,
                    arg_types_vec,
                    ptr_typ,
                    64, // pointers are 64-bit
                )
            };
            call_insn.variadic_arg_start = variadic_arg_start;
            call_insn.is_noreturn_call = is_noreturn_call;
            call_insn.abi_info = Some(call_abi_info);
            self.emit(call_insn);
            // Return the symbol (address) where struct is stored
            result_sym
        } else {
            let ret_size = self.types.size_bits(typ);
            let mut call_insn = if let Some(func_addr) = indirect_target {
                // Indirect call through function pointer
                Instruction::call_indirect(
                    Some(result_sym),
                    func_addr,
                    arg_vals,
                    arg_types_vec,
                    typ,
                    ret_size,
                )
            } else {
                // Direct call
                Instruction::call(
                    Some(result_sym),
                    &func_name,
                    arg_vals,
                    arg_types_vec,
                    typ,
                    ret_size,
                )
            };
            call_insn.variadic_arg_start = variadic_arg_start;
            call_insn.is_noreturn_call = is_noreturn_call;
            call_insn.abi_info = Some(call_abi_info);
            self.emit(call_insn);
            result_sym
        }
    }

    /// Linearize a post-increment or post-decrement expression
    pub(crate) fn linearize_postop(&mut self, operand: &Expr, is_inc: bool) -> PseudoId {
        let val = self.linearize_expr(operand);
        let typ = self.expr_type(operand);
        let is_float = self.types.is_float(typ);
        let is_ptr = self.types.kind(typ) == TypeKind::Pointer;

        // For locals, we need to save the old value before updating
        // because the pseudo will be reloaded from stack which gets overwritten
        let is_local = if let ExprKind::Ident(symbol_id) = &operand.kind {
            self.locals.contains_key(symbol_id)
        } else {
            false
        };

        let old_val = if is_local {
            // Copy the old value to a temp
            let temp = self.alloc_reg_pseudo();
            self.emit(
                Instruction::new(Opcode::Copy)
                    .with_target(temp)
                    .with_src(val)
                    .with_type(typ)
                    .with_size(self.types.size_bits(typ)),
            );
            temp
        } else {
            val
        };

        // For pointers, increment/decrement by element size; for others, by 1
        let delta = if is_ptr {
            let elem_type = self.types.base_type(typ).unwrap_or(self.types.char_id);
            let elem_size = self.types.size_bits(elem_type) / 8;
            self.emit_const(elem_size as i128, self.types.long_id)
        } else if is_float {
            self.emit_fconst(1.0, typ)
        } else {
            self.emit_const(1, typ)
        };
        let result = self.alloc_reg_pseudo();
        let opcode = if is_float {
            if is_inc {
                Opcode::FAdd
            } else {
                Opcode::FSub
            }
        } else if is_inc {
            Opcode::Add
        } else {
            Opcode::Sub
        };
        let arith_type = if is_ptr { self.types.long_id } else { typ };
        let arith_size = self.types.size_bits(arith_type);
        self.emit(Instruction::binop(
            opcode, result, val, delta, arith_type, arith_size,
        ));

        // For _Bool, normalize the result (any non-zero -> 1)
        let final_result = if self.types.kind(typ) == TypeKind::Bool {
            self.emit_convert(result, self.types.int_id, typ)
        } else {
            result
        };

        // Store to local, update parameter mapping, or store through pointer
        let store_size = self.types.size_bits(typ);
        match &operand.kind {
            ExprKind::Ident(symbol_id) => {
                let name_str = self.symbol_name(*symbol_id);
                if let Some(local) = self.locals.get(symbol_id).cloned() {
                    // Check if this is a static local (sentinel value)
                    if local.sym.0 == u32::MAX {
                        self.emit_static_local_store(&name_str, final_result, typ, store_size);
                    } else {
                        // Regular local variable
                        self.emit(Instruction::store(
                            final_result,
                            local.sym,
                            0,
                            typ,
                            store_size,
                        ));
                    }
                } else if self.var_map.contains_key(&name_str) {
                    self.var_map.insert(name_str.clone(), final_result);
                } else {
                    // Global variable - emit store
                    let sym_id = self.alloc_pseudo();
                    let pseudo = Pseudo::sym(sym_id, name_str.clone());
                    if let Some(func) = &mut self.current_func {
                        func.add_pseudo(pseudo);
                    }
                    self.emit(Instruction::store(final_result, sym_id, 0, typ, store_size));
                }
            }
            ExprKind::Unary {
                op: UnaryOp::Deref,
                operand: ptr_expr,
            } => {
                // (*p)++ or (*p)-- - store back through the pointer
                let addr = self.linearize_expr(ptr_expr);
                self.emit(Instruction::store(final_result, addr, 0, typ, store_size));
            }
            ExprKind::Member { expr, member } => {
                // Struct member: get address and store with offset
                let base = self.linearize_lvalue(expr);
                let base_struct_type = self.expr_type(expr);
                let struct_type = self.resolve_struct_type(base_struct_type);
                if let Some(member_info) = self.types.find_member(struct_type, *member) {
                    self.emit(Instruction::store(
                        final_result,
                        base,
                        member_info.offset as i64,
                        typ,
                        store_size,
                    ));
                }
            }
            ExprKind::Arrow { expr, member } => {
                // Pointer member: pointer value is the base address
                let ptr = self.linearize_expr(expr);
                let ptr_type = self.expr_type(expr);
                let base_struct_type = self.types.base_type(ptr_type).unwrap_or(typ);
                let struct_type = self.resolve_struct_type(base_struct_type);
                if let Some(member_info) = self.types.find_member(struct_type, *member) {
                    self.emit(Instruction::store(
                        final_result,
                        ptr,
                        member_info.offset as i64,
                        typ,
                        store_size,
                    ));
                }
            }
            ExprKind::Index { array, index } => {
                // Array subscript: compute address and store
                let array_type = self.expr_type(array);
                let index_type = self.expr_type(index);
                let array_kind = self.types.kind(array_type);
                let (ptr_expr, idx_expr, idx_type) =
                    if array_kind == TypeKind::Pointer || array_kind == TypeKind::Array {
                        (array.as_ref(), index.as_ref(), index_type)
                    } else {
                        (index.as_ref(), array.as_ref(), array_type)
                    };
                let arr = self.linearize_expr(ptr_expr);
                let idx = self.linearize_expr(idx_expr);
                let elem_size = store_size / 8;
                let elem_size_val = self.emit_const(elem_size as i128, self.types.long_id);
                let idx_extended = self.emit_convert(idx, idx_type, self.types.long_id);
                let offset = self.alloc_pseudo();
                let ptr_typ = self.types.long_id;
                self.emit(Instruction::binop(
                    Opcode::Mul,
                    offset,
                    idx_extended,
                    elem_size_val,
                    ptr_typ,
                    64,
                ));
                let addr = self.alloc_reg_pseudo();
                self.emit(Instruction::binop(
                    Opcode::Add,
                    addr,
                    arr,
                    offset,
                    ptr_typ,
                    64,
                ));
                self.emit(Instruction::store(final_result, addr, 0, typ, store_size));
            }
            _ => {}
        }

        old_val // Return old value
    }

    /// Linearize a binary expression (arithmetic, comparison, logical operators)
    pub(crate) fn linearize_binary(
        &mut self,
        expr: &Expr,
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
    ) -> PseudoId {
        // Handle short-circuit operators before linearizing both operands
        // C99 requires that && and || only evaluate the RHS if needed
        if op == BinaryOp::LogAnd {
            return self.emit_logical_and(left, right);
        }
        if op == BinaryOp::LogOr {
            return self.emit_logical_or(left, right);
        }

        let left_typ = self.expr_type(left);
        let right_typ = self.expr_type(right);
        let result_typ = self.expr_type(expr);

        // Check for pointer arithmetic: ptr +/- int or int + ptr
        let left_kind = self.types.kind(left_typ);
        let right_kind = self.types.kind(right_typ);
        let left_is_ptr_or_arr = left_kind == TypeKind::Pointer || left_kind == TypeKind::Array;
        let right_is_ptr_or_arr = right_kind == TypeKind::Pointer || right_kind == TypeKind::Array;
        let is_ptr_arith = (op == BinaryOp::Add || op == BinaryOp::Sub)
            && ((left_is_ptr_or_arr && self.types.is_integer(right_typ))
                || (self.types.is_integer(left_typ) && right_is_ptr_or_arr));

        // Check for pointer difference: ptr - ptr
        let is_ptr_diff = op == BinaryOp::Sub && left_is_ptr_or_arr && right_is_ptr_or_arr;

        if is_ptr_diff {
            // Pointer difference: (ptr1 - ptr2) / element_size
            let left_val = self.linearize_expr(left);
            let right_val = self.linearize_expr(right);

            // Compute byte difference
            let byte_diff = self.alloc_pseudo();
            self.emit(Instruction::binop(
                Opcode::Sub,
                byte_diff,
                left_val,
                right_val,
                self.types.long_id,
                64,
            ));

            // Get element size from the pointer type
            let elem_type = self.types.base_type(left_typ).unwrap_or(self.types.char_id);
            let elem_size = self.types.size_bits(elem_type) / 8;

            // Divide by element size
            let scale = self.emit_const(elem_size as i128, self.types.long_id);
            let result = self.alloc_pseudo();
            self.emit(Instruction::binop(
                Opcode::DivS,
                result,
                byte_diff,
                scale,
                self.types.long_id,
                64,
            ));
            result
        } else if is_ptr_arith {
            // Pointer arithmetic: scale integer operand by element size
            let (ptr_val, ptr_typ, int_val) = if left_is_ptr_or_arr {
                let ptr = self.linearize_expr(left);
                let int = self.linearize_expr(right);
                (ptr, left_typ, int)
            } else {
                // int + ptr case
                let int = self.linearize_expr(left);
                let ptr = self.linearize_expr(right);
                (ptr, right_typ, int)
            };

            // Get element size
            let elem_type = self.types.base_type(ptr_typ).unwrap_or(self.types.char_id);
            let elem_size = self.types.size_bits(elem_type) / 8;

            // Scale the integer by element size
            let scale = self.emit_const(elem_size as i128, self.types.long_id);
            let scaled_offset = self.alloc_pseudo();
            // Extend int_val to 64-bit for proper address arithmetic
            let actual_int_type = if left_is_ptr_or_arr {
                right_typ
            } else {
                left_typ
            };
            let int_val_extended = self.emit_convert(int_val, actual_int_type, self.types.long_id);
            self.emit(Instruction::binop(
                Opcode::Mul,
                scaled_offset,
                int_val_extended,
                scale,
                self.types.long_id,
                64,
            ));

            // Add (or subtract) to pointer
            let result = self.alloc_pseudo();
            let opcode = if op == BinaryOp::Sub {
                Opcode::Sub
            } else {
                Opcode::Add
            };
            self.emit(Instruction::binop(
                opcode,
                result,
                ptr_val,
                scaled_offset,
                self.types.long_id,
                64,
            ));
            result
        } else if self.types.is_complex(result_typ) {
            // Complex arithmetic: expand to real/imaginary operations
            // For complex types, we need addresses to load real/imag parts
            // If an operand is not complex (e.g., real scalar), promote it
            let left_addr = if self.types.is_complex(left_typ) {
                self.linearize_lvalue(left)
            } else {
                self.promote_real_to_complex(left, result_typ)
            };
            let right_addr = if self.types.is_complex(right_typ) {
                self.linearize_lvalue(right)
            } else {
                self.promote_real_to_complex(right, result_typ)
            };
            self.emit_complex_binary(op, left_addr, right_addr, result_typ)
        } else {
            // For comparisons, compute common type for both operands
            // (usual arithmetic conversions)
            let operand_typ = if op.is_comparison() {
                self.common_type(left_typ, right_typ)
            } else {
                result_typ
            };

            // Linearize operands
            let left_val = self.linearize_expr(left);
            let right_val = self.linearize_expr(right);

            // Emit type conversions if needed
            let left_val = self.emit_convert(left_val, left_typ, operand_typ);
            let right_val = self.emit_convert(right_val, right_typ, operand_typ);

            self.emit_binary(op, left_val, right_val, result_typ, operand_typ)
        }
    }

    /// Linearize a unary expression (prefix operators, address-of, dereference, etc.)
    pub(crate) fn linearize_unary(&mut self, expr: &Expr, op: UnaryOp, operand: &Expr) -> PseudoId {
        // Handle AddrOf specially - we need the lvalue address, not the value
        if op == UnaryOp::AddrOf {
            return self.linearize_lvalue(operand);
        }

        // Handle PreInc/PreDec specially - they need store-back
        if op == UnaryOp::PreInc || op == UnaryOp::PreDec {
            // For deref operands like *s++, compute the lvalue address once
            // to avoid re-evaluating side effects (PostInc etc.) when storing back.
            let deref_addr = if let ExprKind::Unary {
                op: UnaryOp::Deref, ..
            } = &operand.kind
            {
                let addr = self.linearize_lvalue(operand);
                Some(addr)
            } else {
                None
            };
            // If we pre-computed the deref address, load from it.
            // Otherwise, evaluate normally.
            let val = if let Some(addr) = deref_addr {
                let deref_typ = self.expr_type(operand);
                let deref_size = self.types.size_bits(deref_typ);
                let loaded = self.alloc_reg_pseudo();
                self.emit(Instruction::load(loaded, addr, 0, deref_typ, deref_size));
                loaded
            } else {
                self.linearize_expr(operand)
            };
            let typ = self.expr_type(operand);
            let is_float = self.types.is_float(typ);
            let is_ptr = self.types.kind(typ) == TypeKind::Pointer;

            // Compute new value - for pointers, scale by element size
            let increment = if is_ptr {
                let elem_type = self.types.base_type(typ).unwrap_or(self.types.char_id);
                let elem_size = self.types.size_bits(elem_type) / 8;
                self.emit_const(elem_size as i128, self.types.long_id)
            } else if is_float {
                self.emit_fconst(1.0, typ)
            } else {
                self.emit_const(1, typ)
            };
            let result = self.alloc_reg_pseudo();
            let opcode = if is_float {
                if op == UnaryOp::PreInc {
                    Opcode::FAdd
                } else {
                    Opcode::FSub
                }
            } else if op == UnaryOp::PreInc {
                Opcode::Add
            } else {
                Opcode::Sub
            };
            let size = self.types.size_bits(typ);
            self.emit(Instruction::binop(
                opcode, result, val, increment, typ, size,
            ));

            // For _Bool, normalize the result (any non-zero -> 1)
            let final_result = if self.types.kind(typ) == TypeKind::Bool {
                self.emit_convert(result, self.types.int_id, typ)
            } else {
                result
            };

            // Store back to the lvalue
            let store_size = self.types.size_bits(typ);
            match &operand.kind {
                ExprKind::Ident(symbol_id) => {
                    let name_str = self.symbol_name(*symbol_id);
                    if let Some(local) = self.locals.get(symbol_id).cloned() {
                        // Check if this is a static local (sentinel value)
                        if local.sym.0 == u32::MAX {
                            self.emit_static_local_store(&name_str, final_result, typ, store_size);
                        } else {
                            // Regular local variable
                            self.emit(Instruction::store(
                                final_result,
                                local.sym,
                                0,
                                typ,
                                store_size,
                            ));
                        }
                    } else if self.var_map.contains_key(&name_str) {
                        self.var_map.insert(name_str.clone(), final_result);
                    } else {
                        // Global variable - emit store
                        let sym_id = self.alloc_pseudo();
                        let pseudo = Pseudo::sym(sym_id, name_str.clone());
                        if let Some(func) = &mut self.current_func {
                            func.add_pseudo(pseudo);
                        }
                        self.emit(Instruction::store(final_result, sym_id, 0, typ, store_size));
                    }
                }
                ExprKind::Member { expr, member } => {
                    // Struct member: get address and store with offset
                    let base = self.linearize_lvalue(expr);
                    let base_struct_type = self.expr_type(expr);
                    let struct_type = self.resolve_struct_type(base_struct_type);
                    if let Some(member_info) = self.types.find_member(struct_type, *member) {
                        self.emit(Instruction::store(
                            final_result,
                            base,
                            member_info.offset as i64,
                            typ,
                            store_size,
                        ));
                    }
                }
                ExprKind::Arrow { expr, member } => {
                    // Pointer member: pointer value is the base address
                    let ptr = self.linearize_expr(expr);
                    let ptr_type = self.expr_type(expr);
                    let base_struct_type = self.types.base_type(ptr_type).unwrap_or(typ);
                    let struct_type = self.resolve_struct_type(base_struct_type);
                    if let Some(member_info) = self.types.find_member(struct_type, *member) {
                        self.emit(Instruction::store(
                            final_result,
                            ptr,
                            member_info.offset as i64,
                            typ,
                            store_size,
                        ));
                    }
                }
                ExprKind::Unary {
                    op: UnaryOp::Deref, ..
                } => {
                    // Dereference: store to the pointer address.
                    // Use the pre-computed address to avoid re-evaluating
                    // side effects (e.g., s++ in ++*s++).
                    let ptr = deref_addr.expect("deref_addr should be set for Deref operand");
                    self.emit(Instruction::store(final_result, ptr, 0, typ, store_size));
                }
                ExprKind::Index { array, index } => {
                    // Array subscript: compute address and store
                    let array_type = self.expr_type(array);
                    let index_type = self.expr_type(index);
                    let array_kind = self.types.kind(array_type);
                    let (ptr_expr, idx_expr, idx_type) =
                        if array_kind == TypeKind::Pointer || array_kind == TypeKind::Array {
                            (array.as_ref(), index.as_ref(), index_type)
                        } else {
                            (index.as_ref(), array.as_ref(), array_type)
                        };
                    let arr = self.linearize_expr(ptr_expr);
                    let idx = self.linearize_expr(idx_expr);
                    let elem_size = store_size / 8;
                    let elem_size_val = self.emit_const(elem_size as i128, self.types.long_id);
                    let idx_extended = self.emit_convert(idx, idx_type, self.types.long_id);
                    let offset = self.alloc_pseudo();
                    let ptr_typ = self.types.long_id;
                    self.emit(Instruction::binop(
                        Opcode::Mul,
                        offset,
                        idx_extended,
                        elem_size_val,
                        ptr_typ,
                        64,
                    ));
                    let addr = self.alloc_reg_pseudo();
                    self.emit(Instruction::binop(
                        Opcode::Add,
                        addr,
                        arr,
                        offset,
                        ptr_typ,
                        64,
                    ));
                    self.emit(Instruction::store(final_result, addr, 0, typ, store_size));
                }
                _ => {
                    // Fallback: shouldn't happen for valid lvalues
                }
            }

            return final_result;
        }

        let src = self.linearize_expr(operand);
        let result_typ = self.expr_type(expr);
        let operand_typ = self.expr_type(operand);
        // For logical NOT, use operand type for comparison size
        let typ = if op == UnaryOp::Not {
            operand_typ
        } else {
            result_typ
        };
        self.emit_unary(op, src, typ)
    }

    /// Linearize an identifier expression (variable reference)
    /// Handle __func__, __FUNCTION__, __PRETTY_FUNCTION__ builtins
    pub(crate) fn linearize_func_name(&mut self) -> PseudoId {
        // C99 6.4.2.2: __func__ behaves as if declared:
        // static const char __func__[] = "function-name";
        // GCC extensions: __FUNCTION__ and __PRETTY_FUNCTION__ behave the same way

        // Add function name as a string literal to the module
        let label = self.module.add_string(self.current_func_name.clone());

        // Create symbol pseudo for the string label
        let sym_id = self.alloc_pseudo();
        let sym_pseudo = Pseudo::sym(sym_id, label);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(sym_pseudo);
        }

        // Create result pseudo for the address
        let result = self.alloc_reg_pseudo();

        // Type: const char* (pointer to char)
        let char_type = self.types.char_id;
        let ptr_type = self.types.pointer_to(char_type);
        self.emit(Instruction::sym_addr(result, sym_id, ptr_type));
        result
    }

    pub(crate) fn linearize_ident(&mut self, expr: &Expr, symbol_id: SymbolId) -> PseudoId {
        let sym = self.symbols.get(symbol_id);
        let name_str = self.str(sym.name).to_string();

        // First check if it's an enum constant
        if sym.is_enum_constant() {
            if let Some(value) = sym.enum_value {
                return self.emit_const(value as i128, self.types.int_id);
            }
        }

        // Check if it's a local variable
        if let Some(local) = self.locals.get(&symbol_id).cloned() {
            // Check if this is a static local (sentinel value)
            if local.sym.0 == u32::MAX {
                // Static local - look up the global name and treat as global
                let key = format!("{}.{}", self.current_func_name, &name_str);
                if let Some(static_info) = self.static_locals.get(&key).cloned() {
                    let sym_id = self.alloc_pseudo();
                    let pseudo = Pseudo::sym(sym_id, static_info.global_name);
                    if let Some(func) = &mut self.current_func {
                        func.add_pseudo(pseudo);
                    }
                    let typ = static_info.typ;
                    let type_kind = self.types.kind(typ);
                    let size = self.types.size_bits(typ);
                    // Arrays decay to pointers - get address, not value
                    if type_kind == TypeKind::Array {
                        let result = self.alloc_pseudo();
                        let elem_type = self.types.base_type(typ).unwrap_or(self.types.int_id);
                        let ptr_type = self.types.pointer_to(elem_type);
                        self.emit(Instruction::sym_addr(result, sym_id, ptr_type));
                        return result;
                    } else if type_kind == TypeKind::VaList {
                        // va_list is defined as __va_list_tag[1] (an array type), so it decays to
                        // a pointer when used in expressions (C99 6.3.2.1, 7.15.1)
                        let result = self.alloc_pseudo();
                        let ptr_type = self.types.pointer_to(typ);
                        self.emit(Instruction::sym_addr(result, sym_id, ptr_type));
                        return result;
                    } else if (type_kind == TypeKind::Struct || type_kind == TypeKind::Union)
                        && size > 64
                    {
                        // Large structs can't be loaded into registers - return address
                        let result = self.alloc_pseudo();
                        let ptr_type = self.types.pointer_to(typ);
                        self.emit(Instruction::sym_addr(result, sym_id, ptr_type));
                        return result;
                    } else {
                        let result = self.alloc_pseudo();
                        self.emit(Instruction::load(result, sym_id, 0, typ, size));
                        return result;
                    }
                } else {
                    unreachable!("static local sentinel without static_locals entry");
                }
            }
            let result = self.alloc_reg_pseudo();
            let type_kind = self.types.kind(local.typ);
            let size = self.types.size_bits(local.typ);
            // Arrays decay to pointers - get address, not value
            if type_kind == TypeKind::Array {
                let elem_type = self.types.base_type(local.typ).unwrap_or(self.types.int_id);
                let ptr_type = self.types.pointer_to(elem_type);
                self.emit(Instruction::sym_addr(result, local.sym, ptr_type));
            } else if type_kind == TypeKind::VaList {
                // va_list is defined as __va_list_tag[1] (an array type), so it decays to
                // a pointer when used in expressions (C99 6.3.2.1, 7.15.1)
                if local.is_indirect {
                    // va_list parameter: local holds a pointer to the va_list struct
                    // Load the pointer value (array decay already happened at call site)
                    let ptr_type = self.types.pointer_to(local.typ);
                    let ptr_size = self.types.size_bits(ptr_type);
                    self.emit(Instruction::load(result, local.sym, 0, ptr_type, ptr_size));
                } else {
                    // Regular va_list local: take address (normal array decay)
                    let ptr_type = self.types.pointer_to(local.typ);
                    self.emit(Instruction::sym_addr(result, local.sym, ptr_type));
                }
            } else if (type_kind == TypeKind::Struct || type_kind == TypeKind::Union) && size > 64 {
                // Large structs can't be loaded into registers - return address
                let ptr_type = self.types.pointer_to(local.typ);
                self.emit(Instruction::sym_addr(result, local.sym, ptr_type));
            } else {
                self.emit(Instruction::load(result, local.sym, 0, local.typ, size));
            }
            result
        }
        // Check if it's a parameter (already SSA value)
        else if let Some(&pseudo) = self.var_map.get(&name_str) {
            pseudo
        }
        // Global variable - create symbol reference and load
        else {
            // C99 6.7.4p3: A non-static inline function cannot refer to
            // a file-scope static variable
            if self.current_func_is_non_static_inline && self.file_scope_statics.contains(&name_str)
            {
                if let Some(pos) = self.current_pos {
                    error(
                        pos,
                        &format!(
                            "non-static inline function '{}' cannot reference file-scope static variable '{}'",
                            self.current_func_name, name_str
                        ),
                    );
                }
            }

            let sym_id = self.alloc_pseudo();
            let pseudo = Pseudo::sym(sym_id, name_str.clone());
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(pseudo);
            }
            let typ = self.expr_type(expr);
            let type_kind = self.types.kind(typ);
            let size = self.types.size_bits(typ);
            // Arrays decay to pointers - get address, not value
            if type_kind == TypeKind::Array {
                let result = self.alloc_pseudo();
                let elem_type = self.types.base_type(typ).unwrap_or(self.types.int_id);
                let ptr_type = self.types.pointer_to(elem_type);
                self.emit(Instruction::sym_addr(result, sym_id, ptr_type));
                result
            }
            // Functions decay to function pointers, va_list decays to pointer (C99 6.3.2.1, 7.15.1),
            // and large structs can't be loaded into registers - for all cases, return the address
            else if type_kind == TypeKind::Function
                || type_kind == TypeKind::VaList
                || ((type_kind == TypeKind::Struct || type_kind == TypeKind::Union) && size > 64)
            {
                let result = self.alloc_pseudo();
                let ptr_type = self.types.pointer_to(typ);
                self.emit(Instruction::sym_addr(result, sym_id, ptr_type));
                result
            } else {
                let result = self.alloc_pseudo();
                self.emit(Instruction::load(result, sym_id, 0, typ, size));
                result
            }
        }
    }

    /// Emit a symbol address for a string/wide-string label.
    pub(crate) fn emit_string_sym(&mut self, expr: &Expr, label: String) -> PseudoId {
        let sym_id = self.alloc_pseudo();
        let sym_pseudo = Pseudo::sym(sym_id, label);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(sym_pseudo);
        }
        let result = self.alloc_reg_pseudo();
        let typ = self.expr_type(expr);
        self.emit(Instruction::sym_addr(result, sym_id, typ));
        result
    }

    pub(crate) fn linearize_ternary(
        &mut self,
        expr: &Expr,
        cond: &Expr,
        then_expr: &Expr,
        else_expr: &Expr,
    ) -> PseudoId {
        let result_typ = self.expr_type(expr);
        let size = if self.types.kind(result_typ) == TypeKind::Function {
            64
        } else {
            self.types.size_bits(result_typ)
        };

        if self.is_pure_expr(then_expr) && self.is_pure_expr(else_expr) && size <= 64 {
            // Pure: use Select instruction (enables cmov/csel)
            let cond_val = self.linearize_expr(cond);
            let cond_typ = self.expr_type(cond);
            let cond_bool = self.emit_compare_zero(cond_val, cond_typ);
            let mut then_val = self.linearize_expr(then_expr);
            let mut else_val = self.linearize_expr(else_expr);

            let then_typ = self.expr_type(then_expr);
            let else_typ = self.expr_type(else_expr);
            let then_size = self.types.size_bits(then_typ);
            let else_size = self.types.size_bits(else_typ);
            if then_size < size && then_size <= 32 && self.types.is_integer(then_typ) {
                then_val = self.emit_convert(then_val, then_typ, result_typ);
            }
            if else_size < size && else_size <= 32 && self.types.is_integer(else_typ) {
                else_val = self.emit_convert(else_val, else_typ, result_typ);
            }

            let result = self.alloc_pseudo();
            self.emit(Instruction::select(
                result, cond_bool, then_val, else_val, result_typ, size,
            ));
            result
        } else {
            // Impure: use control flow + phi for proper short-circuit evaluation
            let then_bb = self.alloc_bb();
            let else_bb = self.alloc_bb();
            let merge_bb = self.alloc_bb();

            let cond_val = self.linearize_expr(cond);
            let cond_typ = self.expr_type(cond);
            let cond_bool = self.emit_compare_zero(cond_val, cond_typ);
            let cond_end_bb = self.current_bb.unwrap();

            self.emit(Instruction::cbr(cond_bool, then_bb, else_bb));
            self.link_bb(cond_end_bb, then_bb);
            self.link_bb(cond_end_bb, else_bb);

            self.switch_bb(then_bb);
            let mut then_val = self.linearize_expr(then_expr);
            let then_typ = self.expr_type(then_expr);
            let then_size = self.types.size_bits(then_typ);
            if then_size < size && then_size <= 32 && self.types.is_integer(then_typ) {
                then_val = self.emit_convert(then_val, then_typ, result_typ);
            }
            let then_end_bb = self.current_bb.unwrap();
            self.emit(Instruction::br(merge_bb));
            self.link_bb(then_end_bb, merge_bb);

            self.switch_bb(else_bb);
            let mut else_val = self.linearize_expr(else_expr);
            let else_typ = self.expr_type(else_expr);
            let else_size = self.types.size_bits(else_typ);
            if else_size < size && else_size <= 32 && self.types.is_integer(else_typ) {
                else_val = self.emit_convert(else_val, else_typ, result_typ);
            }
            let else_end_bb = self.current_bb.unwrap();
            self.emit(Instruction::br(merge_bb));
            self.link_bb(else_end_bb, merge_bb);

            self.switch_bb(merge_bb);
            let result = self.alloc_pseudo();
            let phi_pseudo = Pseudo::phi(result, result.0);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(phi_pseudo);
            }
            let mut phi_insn = Instruction::phi(result, result_typ, size);
            let phisrc1 =
                self.emit_phi_source(then_end_bb, then_val, result, merge_bb, result_typ, size);
            phi_insn.phi_list.push((then_end_bb, phisrc1));
            let phisrc2 =
                self.emit_phi_source(else_end_bb, else_val, result, merge_bb, result_typ, size);
            phi_insn.phi_list.push((else_end_bb, phisrc2));
            self.emit(phi_insn);

            result
        }
    }

    pub(crate) fn linearize_compound_literal(&mut self, expr: &Expr) -> PseudoId {
        match &expr.kind {
            ExprKind::CompoundLiteral { typ, elements } => {
                // Compound literals have automatic storage at block scope
                // Create an anonymous local variable, similar to how local variables work

                // Create a symbol pseudo for the compound literal (its address)
                let sym_id = self.alloc_pseudo();
                let unique_name = format!(".compound_literal.{}", sym_id.0);
                let sym = Pseudo::sym(sym_id, unique_name.clone());
                if let Some(func) = &mut self.current_func {
                    func.add_pseudo(sym);
                    // Register as local for proper stack allocation
                    func.add_local(
                        &unique_name,
                        sym_id,
                        *typ,
                        false,
                        false,
                        self.current_bb,
                        None,
                    );
                }

                // For compound literals with partial initialization, C99 6.7.8p21 requires
                // zero-initialization of all subobjects not explicitly initialized.
                // Zero the entire compound literal first, then initialize specific members.
                let type_kind = self.types.kind(*typ);
                if type_kind == TypeKind::Struct
                    || type_kind == TypeKind::Union
                    || type_kind == TypeKind::Array
                {
                    self.emit_aggregate_zero(sym_id, *typ);
                }

                // Initialize using existing init list machinery
                self.linearize_init_list(sym_id, *typ, elements);

                // For arrays: return pointer (array-to-pointer decay)
                // For structs/scalars: load and return the value
                let result = self.alloc_reg_pseudo();

                let type_kind = self.types.kind(*typ);
                let size = self.types.size_bits(*typ);
                if type_kind == TypeKind::Array {
                    // Array compound literal - decay to pointer to first element
                    let elem_type = self.types.base_type(*typ).unwrap_or(self.types.int_id);
                    let ptr_type = self.types.pointer_to(elem_type);
                    self.emit(Instruction::sym_addr(result, sym_id, ptr_type));
                } else if (type_kind == TypeKind::Struct || type_kind == TypeKind::Union)
                    && size > 64
                {
                    // Large struct/union compound literal - return address
                    // Large structs can't be "loaded" into registers; assignment handles copying
                    let ptr_type = self.types.pointer_to(*typ);
                    self.emit(Instruction::sym_addr(result, sym_id, ptr_type));
                } else {
                    // Scalar or small struct compound literal - load the value
                    self.emit(Instruction::load(result, sym_id, 0, *typ, size));
                }
                result
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn linearize_va_op(&mut self, expr: &Expr) -> PseudoId {
        match &expr.kind {
            // ================================================================
            // Variadic function support (va_* builtins)
            // ================================================================
            ExprKind::VaStart { ap, last_param } => {
                // va_start(ap, last_param)
                // Get address of ap (it's an lvalue)
                let ap_addr = self.linearize_lvalue(ap);
                let result = self.alloc_pseudo();

                // Create instruction with last_param stored in func_name field
                let insn = Instruction::new(Opcode::VaStart)
                    .with_target(result)
                    .with_src(ap_addr)
                    .with_func(self.str(*last_param).to_string())
                    .with_type(self.types.void_id)
                    .with_size(0);
                self.emit(insn);
                result
            }

            ExprKind::VaArg { ap, arg_type } => {
                // va_arg(ap, type)
                // Get address of ap (it's an lvalue)
                let ap_addr = self.linearize_lvalue(ap);
                let result = self.alloc_pseudo();

                let arg_size = self.types.size_bits(*arg_type);
                let insn = Instruction::new(Opcode::VaArg)
                    .with_target(result)
                    .with_src(ap_addr)
                    .with_type(*arg_type)
                    .with_size(arg_size);
                self.emit(insn);
                result
            }

            ExprKind::VaEnd { ap } => {
                // va_end(ap) - usually a no-op
                let ap_addr = self.linearize_lvalue(ap);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::VaEnd)
                    .with_target(result)
                    .with_src(ap_addr)
                    .with_type(self.types.void_id)
                    .with_size(0);
                self.emit(insn);
                result
            }

            ExprKind::VaCopy { dest, src } => {
                // va_copy(dest, src)
                let dest_addr = self.linearize_lvalue(dest);
                let src_addr = self.linearize_lvalue(src);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::VaCopy)
                    .with_target(result)
                    .with_src(dest_addr)
                    .with_src(src_addr)
                    .with_type(self.types.void_id)
                    .with_size(0);
                self.emit(insn);
                result
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn linearize_builtin(&mut self, expr: &Expr) -> PseudoId {
        match &expr.kind {
            // ================================================================
            // Byte-swapping builtins
            // ================================================================
            ExprKind::Bswap16 { arg } => {
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Bswap16)
                    .with_target(result)
                    .with_src(arg_val)
                    .with_size(16)
                    .with_type(self.types.ushort_id);
                self.emit(insn);
                result
            }

            ExprKind::Bswap32 { arg } => {
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Bswap32)
                    .with_target(result)
                    .with_src(arg_val)
                    .with_size(32)
                    .with_type(self.types.uint_id);
                self.emit(insn);
                result
            }

            ExprKind::Bswap64 { arg } => {
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Bswap64)
                    .with_target(result)
                    .with_src(arg_val)
                    .with_size(64)
                    .with_type(self.types.ulonglong_id);
                self.emit(insn);
                result
            }

            // ================================================================
            // Count trailing zeros builtins
            // ================================================================
            ExprKind::Ctz { arg } => {
                // __builtin_ctz - counts trailing zeros in unsigned int (32-bit)
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Ctz32)
                    .with_target(result)
                    .with_src(arg_val)
                    .with_size(32)
                    .with_type(self.types.int_id);
                self.emit(insn);
                result
            }

            ExprKind::Ctzl { arg } | ExprKind::Ctzll { arg } => {
                // __builtin_ctzl/ctzll - counts trailing zeros in 64-bit value
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Ctz64)
                    .with_target(result)
                    .with_src(arg_val)
                    .with_size(64)
                    .with_type(self.types.int_id);
                self.emit(insn);
                result
            }

            // ================================================================
            // Count leading zeros builtins
            // ================================================================
            ExprKind::Clz { arg } => {
                // __builtin_clz - counts leading zeros in unsigned int (32-bit)
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Clz32)
                    .with_target(result)
                    .with_src(arg_val)
                    .with_size(32)
                    .with_type(self.types.int_id);
                self.emit(insn);
                result
            }

            ExprKind::Clzl { arg } | ExprKind::Clzll { arg } => {
                // __builtin_clzl/clzll - counts leading zeros in 64-bit value
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Clz64)
                    .with_target(result)
                    .with_src(arg_val)
                    .with_size(64)
                    .with_type(self.types.int_id);
                self.emit(insn);
                result
            }

            // ================================================================
            // Population count builtins
            // ================================================================
            ExprKind::Popcount { arg } => {
                // __builtin_popcount - counts set bits in unsigned int (32-bit)
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Popcount32)
                    .with_target(result)
                    .with_src(arg_val)
                    .with_size(32)
                    .with_type(self.types.int_id);
                self.emit(insn);
                result
            }

            ExprKind::Popcountl { arg } | ExprKind::Popcountll { arg } => {
                // __builtin_popcountl/popcountll - counts set bits in 64-bit value
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Popcount64)
                    .with_target(result)
                    .with_src(arg_val)
                    .with_size(64)
                    .with_type(self.types.int_id);
                self.emit(insn);
                result
            }

            ExprKind::Alloca { size } => {
                let size_val = self.linearize_expr(size);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Alloca)
                    .with_target(result)
                    .with_src(size_val)
                    .with_type_and_size(self.types.void_ptr_id, 64);
                self.emit(insn);
                result
            }

            ExprKind::Memset { dest, c, n } => {
                let dest_val = self.linearize_expr(dest);
                let c_val = self.linearize_expr(c);
                let n_val = self.linearize_expr(n);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Memset)
                    .with_target(result)
                    .with_src3(dest_val, c_val, n_val)
                    .with_type_and_size(self.types.void_ptr_id, 64);
                self.emit(insn);
                result
            }

            ExprKind::Memcpy { dest, src, n } => {
                let dest_val = self.linearize_expr(dest);
                let src_val = self.linearize_expr(src);
                let n_val = self.linearize_expr(n);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Memcpy)
                    .with_target(result)
                    .with_src3(dest_val, src_val, n_val)
                    .with_type_and_size(self.types.void_ptr_id, 64);
                self.emit(insn);
                result
            }

            ExprKind::Memmove { dest, src, n } => {
                let dest_val = self.linearize_expr(dest);
                let src_val = self.linearize_expr(src);
                let n_val = self.linearize_expr(n);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Memmove)
                    .with_target(result)
                    .with_src3(dest_val, src_val, n_val)
                    .with_type_and_size(self.types.void_ptr_id, 64);
                self.emit(insn);
                result
            }

            ExprKind::Fabs { arg } => {
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Fabs64)
                    .with_target(result)
                    .with_src(arg_val)
                    .with_size(64)
                    .with_type(self.types.double_id);
                self.emit(insn);
                result
            }

            ExprKind::Fabsf { arg } => {
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Fabs32)
                    .with_target(result)
                    .with_src(arg_val)
                    .with_size(32)
                    .with_type(self.types.float_id);
                self.emit(insn);
                result
            }

            ExprKind::Fabsl { arg } => {
                // Long double fabs - treat as 64-bit for now
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Fabs64)
                    .with_target(result)
                    .with_src(arg_val)
                    .with_size(128) // long double is 128-bit on our targets
                    .with_type(self.types.longdouble_id);
                self.emit(insn);
                result
            }

            ExprKind::Signbit { arg } => {
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Signbit64)
                    .with_target(result)
                    .with_src(arg_val)
                    .with_size(64)
                    .with_type(self.types.int_id);
                self.emit(insn);
                result
            }

            ExprKind::Signbitf { arg } => {
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Signbit32)
                    .with_target(result)
                    .with_src(arg_val)
                    .with_size(32)
                    .with_type(self.types.int_id);
                self.emit(insn);
                result
            }

            ExprKind::Signbitl { arg } => {
                // Long double signbit - use 64-bit version
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Signbit64)
                    .with_target(result)
                    .with_src(arg_val)
                    .with_size(128) // long double is 128-bit on our targets
                    .with_type(self.types.int_id);
                self.emit(insn);
                result
            }

            ExprKind::BuiltinComplex { real, imag } => {
                // __builtin_complex(real, imag) - construct complex value
                let complex_typ = self.expr_type(expr);
                let base_typ = self.types.complex_base(complex_typ);
                let base_bits = self.types.size_bits(base_typ);
                let base_bytes = (base_bits / 8) as i64;

                let real_val = self.linearize_expr(real);
                let imag_val = self.linearize_expr(imag);

                // Allocate local to hold the complex value
                let result = self.alloc_local_temp(complex_typ);

                // Store real and imag parts
                self.emit(Instruction::store(real_val, result, 0, base_typ, base_bits));
                self.emit(Instruction::store(
                    imag_val, result, base_bytes, base_typ, base_bits,
                ));

                result
            }

            ExprKind::Unreachable => {
                // __builtin_unreachable() - marks code path as never reached
                // Emits an instruction that will trap if actually executed
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Unreachable)
                    .with_target(result)
                    .with_type(self.types.void_id);
                self.emit(insn);
                result
            }

            ExprKind::FrameAddress { level } => {
                // __builtin_frame_address(level) - returns frame pointer at given level
                let level_val = self.linearize_expr(level);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::FrameAddress)
                    .with_target(result)
                    .with_src(level_val)
                    .with_type_and_size(self.types.void_ptr_id, 64);
                self.emit(insn);
                result
            }

            ExprKind::ReturnAddress { level } => {
                // __builtin_return_address(level) - returns return address at given level
                let level_val = self.linearize_expr(level);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::ReturnAddress)
                    .with_target(result)
                    .with_src(level_val)
                    .with_type_and_size(self.types.void_ptr_id, 64);
                self.emit(insn);
                result
            }

            ExprKind::Setjmp { env } => {
                // setjmp(env) - saves execution context, returns int
                let env_val = self.linearize_expr(env);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Setjmp)
                    .with_target(result)
                    .with_src(env_val)
                    .with_type_and_size(self.types.int_id, 32);
                self.emit(insn);
                result
            }

            ExprKind::Longjmp { env, val } => {
                // longjmp(env, val) - restores execution context (never returns)
                let env_val = self.linearize_expr(env);
                let val_val = self.linearize_expr(val);
                let result = self.alloc_pseudo();

                let mut insn = Instruction::new(Opcode::Longjmp);
                insn.target = Some(result);
                insn.src = vec![env_val, val_val];
                insn.typ = Some(self.types.void_id);
                self.emit(insn);
                result
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn linearize_c11_atomic(&mut self, expr: &Expr) -> PseudoId {
        match &expr.kind {
            // ================================================================
            // Atomic builtins (Clang __c11_atomic_* for C11 stdatomic.h)
            // ================================================================
            ExprKind::C11AtomicInit { ptr, val } => {
                // atomic_init is a non-atomic store (no memory ordering)
                let ptr_val = self.linearize_expr(ptr);
                let value = self.linearize_expr(val);
                let ptr_type = self.expr_type(ptr);
                let elem_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                let size = self.types.size_bits(elem_type);
                let result = self.alloc_pseudo();

                // Use AtomicStore with Relaxed ordering for init
                let insn = Instruction::new(Opcode::AtomicStore)
                    .with_target(result)
                    .with_src(ptr_val)
                    .with_src(value)
                    .with_src(self.emit_const(0, self.types.int_id)) // Relaxed = 0
                    .with_type_and_size(elem_type, size)
                    .with_memory_order(MemoryOrder::Relaxed);
                self.emit(insn);
                result
            }

            ExprKind::C11AtomicLoad { ptr, order } => {
                let ptr_val = self.linearize_expr(ptr);
                let order_val = self.linearize_expr(order);
                let memory_order = self.eval_memory_order(order);
                let ptr_type = self.expr_type(ptr);
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                let size = self.types.size_bits(result_type);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::AtomicLoad)
                    .with_target(result)
                    .with_src(ptr_val)
                    .with_src(order_val)
                    .with_type_and_size(result_type, size)
                    .with_memory_order(memory_order);
                self.emit(insn);
                result
            }

            ExprKind::C11AtomicStore { ptr, val, order } => {
                let ptr_val = self.linearize_expr(ptr);
                let value = self.linearize_expr(val);
                let order_val = self.linearize_expr(order);
                let memory_order = self.eval_memory_order(order);
                let ptr_type = self.expr_type(ptr);
                let elem_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                let size = self.types.size_bits(elem_type);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::AtomicStore)
                    .with_target(result)
                    .with_src(ptr_val)
                    .with_src(value)
                    .with_src(order_val)
                    .with_type_and_size(elem_type, size)
                    .with_memory_order(memory_order);
                self.emit(insn);
                result
            }

            ExprKind::C11AtomicExchange { ptr, val, order } => {
                let ptr_val = self.linearize_expr(ptr);
                let value = self.linearize_expr(val);
                let order_val = self.linearize_expr(order);
                let memory_order = self.eval_memory_order(order);
                let ptr_type = self.expr_type(ptr);
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                let size = self.types.size_bits(result_type);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::AtomicSwap)
                    .with_target(result)
                    .with_src(ptr_val)
                    .with_src(value)
                    .with_src(order_val)
                    .with_type_and_size(result_type, size)
                    .with_memory_order(memory_order);
                self.emit(insn);
                result
            }

            ExprKind::C11AtomicCompareExchangeStrong {
                ptr,
                expected,
                desired,
                succ_order,
            }
            | ExprKind::C11AtomicCompareExchangeWeak {
                ptr,
                expected,
                desired,
                succ_order,
            } => {
                // Both strong and weak are implemented the same (as strong)
                let ptr_val = self.linearize_expr(ptr);
                let expected_ptr = self.linearize_expr(expected);
                let desired_val = self.linearize_expr(desired);
                let order_val = self.linearize_expr(succ_order);
                let memory_order = self.eval_memory_order(succ_order);
                let ptr_type = self.expr_type(ptr);
                let elem_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                let elem_size = self.types.size_bits(elem_type);
                let result = self.alloc_pseudo();

                // For CAS, typ is bool (result), but size is the element size for codegen
                let insn = Instruction::new(Opcode::AtomicCas)
                    .with_target(result)
                    .with_src(ptr_val)
                    .with_src(expected_ptr)
                    .with_src(desired_val)
                    .with_src(order_val)
                    .with_type(self.types.bool_id)
                    .with_size(elem_size)
                    .with_memory_order(memory_order);
                self.emit(insn);
                result
            }

            ExprKind::C11AtomicFetchAdd { ptr, val, order } => {
                let ptr_val = self.linearize_expr(ptr);
                let value = self.linearize_expr(val);
                let order_val = self.linearize_expr(order);
                let memory_order = self.eval_memory_order(order);
                let ptr_type = self.expr_type(ptr);
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                let size = self.types.size_bits(result_type);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::AtomicFetchAdd)
                    .with_target(result)
                    .with_src(ptr_val)
                    .with_src(value)
                    .with_src(order_val)
                    .with_type_and_size(result_type, size)
                    .with_memory_order(memory_order);
                self.emit(insn);
                result
            }

            ExprKind::C11AtomicFetchSub { ptr, val, order } => {
                let ptr_val = self.linearize_expr(ptr);
                let value = self.linearize_expr(val);
                let order_val = self.linearize_expr(order);
                let memory_order = self.eval_memory_order(order);
                let ptr_type = self.expr_type(ptr);
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                let size = self.types.size_bits(result_type);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::AtomicFetchSub)
                    .with_target(result)
                    .with_src(ptr_val)
                    .with_src(value)
                    .with_src(order_val)
                    .with_type_and_size(result_type, size)
                    .with_memory_order(memory_order);
                self.emit(insn);
                result
            }

            ExprKind::C11AtomicFetchAnd { ptr, val, order } => {
                let ptr_val = self.linearize_expr(ptr);
                let value = self.linearize_expr(val);
                let order_val = self.linearize_expr(order);
                let memory_order = self.eval_memory_order(order);
                let ptr_type = self.expr_type(ptr);
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                let size = self.types.size_bits(result_type);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::AtomicFetchAnd)
                    .with_target(result)
                    .with_src(ptr_val)
                    .with_src(value)
                    .with_src(order_val)
                    .with_type_and_size(result_type, size)
                    .with_memory_order(memory_order);
                self.emit(insn);
                result
            }

            ExprKind::C11AtomicFetchOr { ptr, val, order } => {
                let ptr_val = self.linearize_expr(ptr);
                let value = self.linearize_expr(val);
                let order_val = self.linearize_expr(order);
                let memory_order = self.eval_memory_order(order);
                let ptr_type = self.expr_type(ptr);
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                let size = self.types.size_bits(result_type);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::AtomicFetchOr)
                    .with_target(result)
                    .with_src(ptr_val)
                    .with_src(value)
                    .with_src(order_val)
                    .with_type_and_size(result_type, size)
                    .with_memory_order(memory_order);
                self.emit(insn);
                result
            }

            ExprKind::C11AtomicFetchXor { ptr, val, order } => {
                let ptr_val = self.linearize_expr(ptr);
                let value = self.linearize_expr(val);
                let order_val = self.linearize_expr(order);
                let memory_order = self.eval_memory_order(order);
                let ptr_type = self.expr_type(ptr);
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                let size = self.types.size_bits(result_type);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::AtomicFetchXor)
                    .with_target(result)
                    .with_src(ptr_val)
                    .with_src(value)
                    .with_src(order_val)
                    .with_type_and_size(result_type, size)
                    .with_memory_order(memory_order);
                self.emit(insn);
                result
            }

            ExprKind::C11AtomicThreadFence { order } => {
                let order_val = self.linearize_expr(order);
                let memory_order = self.eval_memory_order(order);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Fence)
                    .with_target(result)
                    .with_src(order_val)
                    .with_type(self.types.void_id)
                    .with_memory_order(memory_order);
                self.emit(insn);
                result
            }

            ExprKind::C11AtomicSignalFence { order } => {
                // Signal fence is a compiler barrier only (no memory fence instruction)
                // For now, treat it the same as thread fence
                let order_val = self.linearize_expr(order);
                let memory_order = self.eval_memory_order(order);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Fence)
                    .with_target(result)
                    .with_src(order_val)
                    .with_type(self.types.void_id)
                    .with_memory_order(memory_order);
                self.emit(insn);
                result
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn linearize_expr(&mut self, expr: &Expr) -> PseudoId {
        // Set current position for debug info
        self.current_pos = Some(expr.pos);

        match &expr.kind {
            ExprKind::IntLit(val) => {
                let typ = self.expr_type(expr);
                self.emit_const(*val as i128, typ)
            }

            ExprKind::Int128Lit(val) => {
                let typ = self.expr_type(expr);
                self.emit_const(*val, typ)
            }

            ExprKind::FloatLit(val) => {
                let typ = self.expr_type(expr);
                self.emit_fconst(*val, typ)
            }

            ExprKind::CharLit(c) => {
                let typ = self.expr_type(expr);
                self.emit_const(*c as u8 as i8 as i128, typ)
            }

            ExprKind::StringLit(s) => {
                let label = self.module.add_string(s.clone());
                self.emit_string_sym(expr, label)
            }

            ExprKind::WideStringLit(s) => {
                let label = self.module.add_wide_string(s.clone());
                self.emit_string_sym(expr, label)
            }

            ExprKind::Ident(symbol_id) => self.linearize_ident(expr, *symbol_id),

            ExprKind::FuncName => self.linearize_func_name(),

            ExprKind::Unary { op, operand } => self.linearize_unary(expr, *op, operand),

            ExprKind::Binary { op, left, right } => self.linearize_binary(expr, *op, left, right),

            ExprKind::Assign { op, target, value } => self.emit_assign(*op, target, value),

            ExprKind::PostInc(operand) => self.linearize_postop(operand, true),

            ExprKind::PostDec(operand) => self.linearize_postop(operand, false),

            ExprKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => self.linearize_ternary(expr, cond, then_expr, else_expr),

            ExprKind::Call { func, args } => self.linearize_call(expr, func, args),

            ExprKind::Member {
                expr: inner_expr,
                member,
            } => self.linearize_member(expr, inner_expr, *member),

            ExprKind::Arrow {
                expr: inner_expr,
                member,
            } => self.linearize_arrow(expr, inner_expr, *member),

            ExprKind::Index { array, index } => self.linearize_index(expr, array, index),

            ExprKind::Cast {
                cast_type,
                expr: inner_expr,
            } => self.linearize_cast(inner_expr, *cast_type),

            ExprKind::SizeofType(typ) => {
                let size = self.types.size_bits(*typ) / 8;
                // sizeof returns size_t, which is unsigned long in our implementation
                let result_typ = self.types.ulong_id;
                self.emit_const(size as i128, result_typ)
            }

            ExprKind::SizeofExpr(inner_expr) => {
                // Check if this is a VLA variable - need runtime sizeof
                if let ExprKind::Ident(symbol_id) = &inner_expr.kind {
                    if let Some(info) = self.locals.get(symbol_id).cloned() {
                        if let (Some(size_sym), Some(elem_type)) =
                            (info.vla_size_sym, info.vla_elem_type)
                        {
                            // VLA: compute sizeof at runtime as num_elements * sizeof(element)
                            let result_typ = self.types.ulong_id;
                            let elem_size = self.types.size_bytes(elem_type) as i64;

                            // Load the stored number of elements
                            let num_elements = self.alloc_pseudo();
                            let load_insn =
                                Instruction::load(num_elements, size_sym, 0, result_typ, 64);
                            self.emit(load_insn);

                            // Multiply by element size
                            let elem_size_const = self.emit_const(elem_size as i128, result_typ);
                            let result = self.alloc_pseudo();
                            let mul_insn = Instruction::new(Opcode::Mul)
                                .with_target(result)
                                .with_src(num_elements)
                                .with_src(elem_size_const)
                                .with_size(64)
                                .with_type(result_typ);
                            self.emit(mul_insn);
                            return result;
                        }
                    }
                }

                // Non-VLA: compute size at compile time
                let inner_typ = self.expr_type(inner_expr);
                let size = self.types.size_bits(inner_typ) / 8;
                // sizeof returns size_t, which is unsigned long in our implementation
                let result_typ = self.types.ulong_id;
                self.emit_const(size as i128, result_typ)
            }

            ExprKind::AlignofType(typ) => {
                let align = self.types.alignment(*typ);
                // _Alignof returns size_t
                let result_typ = self.types.ulong_id;
                self.emit_const(align as i128, result_typ)
            }

            ExprKind::AlignofExpr(inner_expr) => {
                let inner_typ = self.expr_type(inner_expr);
                let align = self.types.alignment(inner_typ);
                // _Alignof returns size_t
                let result_typ = self.types.ulong_id;
                self.emit_const(align as i128, result_typ)
            }

            ExprKind::Comma(exprs) => {
                let mut result = self.emit_const(0, self.types.int_id);
                for e in exprs {
                    result = self.linearize_expr(e);
                }
                result
            }

            ExprKind::InitList { .. } => {
                // InitList is handled specially in linearize_local_decl and linearize_global_decl
                // It shouldn't be reached here during normal expression evaluation
                panic!("InitList should be handled in declaration context, not as standalone expression")
            }

            ExprKind::CompoundLiteral { .. } => self.linearize_compound_literal(expr),

            ExprKind::VaStart { .. }
            | ExprKind::VaArg { .. }
            | ExprKind::VaEnd { .. }
            | ExprKind::VaCopy { .. } => self.linearize_va_op(expr),

            ExprKind::Bswap16 { .. }
            | ExprKind::Bswap32 { .. }
            | ExprKind::Bswap64 { .. }
            | ExprKind::Ctz { .. }
            | ExprKind::Ctzl { .. }
            | ExprKind::Ctzll { .. }
            | ExprKind::Clz { .. }
            | ExprKind::Clzl { .. }
            | ExprKind::Clzll { .. }
            | ExprKind::Popcount { .. }
            | ExprKind::Popcountl { .. }
            | ExprKind::Popcountll { .. }
            | ExprKind::Alloca { .. }
            | ExprKind::Memset { .. }
            | ExprKind::Memcpy { .. }
            | ExprKind::Memmove { .. }
            | ExprKind::Fabs { .. }
            | ExprKind::Fabsf { .. }
            | ExprKind::Fabsl { .. }
            | ExprKind::Signbit { .. }
            | ExprKind::Signbitf { .. }
            | ExprKind::Signbitl { .. }
            | ExprKind::Unreachable
            | ExprKind::FrameAddress { .. }
            | ExprKind::ReturnAddress { .. }
            | ExprKind::Setjmp { .. }
            | ExprKind::Longjmp { .. } => self.linearize_builtin(expr),

            ExprKind::OffsetOf { type_id, path } => {
                // __builtin_offsetof(type, member-designator)
                // Compute the byte offset of the member within the struct
                let mut offset: u64 = 0;
                let mut current_type = *type_id;

                for element in path {
                    match element {
                        OffsetOfPath::Field(field_id) => {
                            // Look up the field in the current struct type
                            let struct_type = self.resolve_struct_type(current_type);
                            let member_info = self
                                .types
                                .find_member(struct_type, *field_id)
                                .expect("offsetof: field not found in struct type");
                            offset += member_info.offset as u64;
                            current_type = member_info.typ;
                        }
                        OffsetOfPath::Index(index) => {
                            // Array indexing: offset += index * sizeof(element)
                            let elem_type = self
                                .types
                                .base_type(current_type)
                                .expect("offsetof: array index on non-array type");
                            let elem_size = self.types.size_bytes(elem_type);
                            offset += (*index as u64) * (elem_size as u64);
                            current_type = elem_type;
                        }
                    }
                }

                // Return the offset as a constant
                self.emit_const(offset as i128, self.types.ulong_id)
            }

            ExprKind::C11AtomicInit { .. }
            | ExprKind::C11AtomicLoad { .. }
            | ExprKind::C11AtomicStore { .. }
            | ExprKind::C11AtomicExchange { .. }
            | ExprKind::C11AtomicCompareExchangeStrong { .. }
            | ExprKind::C11AtomicCompareExchangeWeak { .. }
            | ExprKind::C11AtomicFetchAdd { .. }
            | ExprKind::C11AtomicFetchSub { .. }
            | ExprKind::C11AtomicFetchAnd { .. }
            | ExprKind::C11AtomicFetchOr { .. }
            | ExprKind::C11AtomicFetchXor { .. }
            | ExprKind::C11AtomicThreadFence { .. }
            | ExprKind::C11AtomicSignalFence { .. } => self.linearize_c11_atomic(expr),

            ExprKind::StmtExpr { stmts, result } => {
                // GNU statement expression: ({ stmt; stmt; expr; })
                // Linearize all the statements first
                for item in stmts {
                    match item {
                        BlockItem::Declaration(decl) => self.linearize_local_decl(decl),
                        BlockItem::Statement(s) => self.linearize_stmt(s),
                    }
                }
                // The result is the value of the final expression
                self.linearize_expr(result)
            }

            ExprKind::BuiltinComplex { real, imag } => {
                // __builtin_complex(real, imag) - construct complex value
                let complex_typ = self.expr_type(expr);
                let base_typ = self.types.complex_base(complex_typ);
                let base_bits = self.types.size_bits(base_typ);
                let base_bytes = (base_bits / 8) as i64;

                let real_val = self.linearize_expr(real);
                let imag_val = self.linearize_expr(imag);

                // Allocate local to hold the complex value, return its address
                let result = self.alloc_local_temp(complex_typ);
                self.emit(Instruction::store(real_val, result, 0, base_typ, base_bits));
                self.emit(Instruction::store(
                    imag_val, result, base_bytes, base_typ, base_bits,
                ));
                result
            }
        }
    }

    /// Evaluate a memory order expression to a MemoryOrder enum value.
    /// If the expression is not a constant or out of range, defaults to SeqCst.
    pub(crate) fn eval_memory_order(&self, expr: &Expr) -> MemoryOrder {
        // Try to evaluate as a constant integer
        if let ExprKind::IntLit(val) = &expr.kind {
            match *val {
                0 => MemoryOrder::Relaxed,
                1 => MemoryOrder::Consume,
                2 => MemoryOrder::Acquire,
                3 => MemoryOrder::Release,
                4 => MemoryOrder::AcqRel,
                5 => MemoryOrder::SeqCst,
                _ => MemoryOrder::SeqCst, // Invalid, use strongest ordering
            }
        } else {
            // Non-constant order expression - use SeqCst for safety
            MemoryOrder::SeqCst
        }
    }
}

// ============================================================================
// Public API
// ============================================================================

/// Linearize an AST to IR
pub fn linearize(
    tu: &TranslationUnit,
    symbols: &SymbolTable,
    types: &TypeTable,
    strings: &StringTable,
    target: &Target,
    debug: bool,
) -> Module {
    let mut linearizer = Linearizer::new(symbols, types, strings, target);
    let mut module = linearizer.linearize(tu);
    module.debug = debug;
    // Get all source files from the stream registry (includes all #included files)
    // The stream IDs in Position map directly to indices in this vector
    // Filter out synthetic file names (like "<paste>", "<built-in>") which start with '<'
    module.source_files = get_all_stream_names()
        .into_iter()
        .map(|name| {
            if name.starts_with('<') {
                // Replace synthetic names with empty string - still need placeholder
                // to keep stream ID indices aligned with .file directive numbers
                String::new()
            } else {
                name
            }
        })
        .collect();
    module
}

// Additional tests in separate file to keep this file manageable
#[cfg(test)]
#[path = "test_linearize.rs"]
mod test_linearize;
