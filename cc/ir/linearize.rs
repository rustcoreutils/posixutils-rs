//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linearizer for pcc C99 compiler
// Converts AST to SSA-style IR following sparse's design
//

use super::ssa::ssa_convert;
use super::{
    BasicBlock, BasicBlockId, Function, Initializer, Instruction, Module, Opcode, Pseudo, PseudoId,
};
use crate::diag::{error, Position};
use crate::parse::ast::{
    AssignOp, BinaryOp, BlockItem, Declaration, Designator, Expr, ExprKind, ExternalDecl, ForInit,
    FunctionDef, InitElement, Stmt, TranslationUnit, UnaryOp,
};
use crate::strings::{StringId, StringTable};
use crate::symbol::SymbolTable;
use crate::target::Target;
use crate::types::{MemberInfo, TypeId, TypeKind, TypeModifiers, TypeTable};
use std::collections::HashMap;

/// Information about a local variable
#[derive(Clone)]
struct LocalVarInfo {
    /// Symbol pseudo (address of the variable)
    sym: PseudoId,
    /// Type of the variable
    typ: TypeId,
    /// For VLAs: symbol holding the number of elements (for runtime sizeof)
    /// This is stored in a hidden local variable so it survives SSA.
    vla_size_sym: Option<PseudoId>,
    /// For VLAs: the element type (for sizeof computation)
    vla_elem_type: Option<TypeId>,
    /// For multi-dimensional VLAs: symbols storing each dimension's size
    /// For int arr[n][m], this contains [sym_for_n, sym_for_m]
    /// These are needed to compute runtime strides for outer dimension access.
    vla_dim_syms: Vec<PseudoId>,
}

/// Information about a static local variable
#[derive(Clone)]
struct StaticLocalInfo {
    /// Global symbol name (unique across translation unit)
    global_name: String,
    /// Type of the variable
    typ: TypeId,
}

// ============================================================================
// Linearizer
// ============================================================================

/// Linearizer context for converting AST to IR
pub struct Linearizer<'a> {
    /// The module being built
    module: Module,
    /// Current function being linearized
    current_func: Option<Function>,
    /// Current basic block being built
    current_bb: Option<BasicBlockId>,
    /// Next pseudo ID
    next_pseudo: u32,
    /// Next basic block ID
    next_bb: u32,
    /// Parameter -> pseudo mapping (parameters are already SSA values)
    var_map: HashMap<String, PseudoId>,
    /// Local variables (use Load/Store, converted to SSA later)
    locals: HashMap<String, LocalVarInfo>,
    /// Label -> basic block mapping
    label_map: HashMap<String, BasicBlockId>,
    /// Break target stack (for loops)
    break_targets: Vec<BasicBlockId>,
    /// Continue target stack (for loops)
    continue_targets: Vec<BasicBlockId>,
    /// Whether to run SSA conversion after linearization
    run_ssa: bool,
    /// Symbol table for looking up enum constants, etc.
    symbols: &'a SymbolTable,
    /// Type table for type information
    types: &'a TypeTable,
    /// String table for converting StringId to String at IR boundary
    strings: &'a StringTable,
    /// Hidden struct return pointer (for functions returning large structs via sret)
    struct_return_ptr: Option<PseudoId>,
    /// Size of struct being returned (for functions returning large structs via sret)
    struct_return_size: u32,
    /// Type of struct being returned via two registers (9-16 bytes, per ABI)
    two_reg_return_type: Option<TypeId>,
    /// Current function name (for generating unique static local names)
    current_func_name: String,
    /// Counter for generating unique static local names
    static_local_counter: u32,
    /// Counter for generating unique compound literal names (for file-scope compound literals)
    compound_literal_counter: u32,
    /// Static local variables (local name -> static local info)
    /// This is persistent across function calls (not cleared per function)
    static_locals: HashMap<String, StaticLocalInfo>,
    /// Current source position for debug info
    current_pos: Option<Position>,
    /// Target configuration (architecture, ABI details)
    target: &'a Target,
    /// Whether current function is a non-static inline function
    /// (used for enforcing C99 inline semantic restrictions)
    current_func_is_non_static_inline: bool,
    /// Set of file-scope static variable names (for inline semantic checks)
    file_scope_statics: std::collections::HashSet<String>,
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
            module: Module::new(),
            current_func: None,
            current_bb: None,
            next_pseudo: 0,
            next_bb: 0,
            var_map: HashMap::new(),
            locals: HashMap::new(),
            label_map: HashMap::new(),
            break_targets: Vec::new(),
            continue_targets: Vec::new(),
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
            static_locals: HashMap::new(),
            current_pos: None,
            target,
            current_func_is_non_static_inline: false,
            file_scope_statics: std::collections::HashSet::new(),
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

    /// Convert a StringId to a &str using the string table
    #[inline]
    fn str(&self, id: StringId) -> &str {
        self.strings.get(id)
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
    fn alloc_pseudo(&mut self) -> PseudoId {
        let id = PseudoId(self.next_pseudo);
        self.next_pseudo += 1;
        id
    }

    /// Allocate a new basic block ID
    fn alloc_bb(&mut self) -> BasicBlockId {
        let id = BasicBlockId(self.next_bb);
        self.next_bb += 1;
        id
    }

    /// Get or create a basic block
    fn get_or_create_bb(&mut self, id: BasicBlockId) -> &mut BasicBlock {
        let func = self.current_func.as_mut().unwrap();
        if func.get_block(id).is_none() {
            func.add_block(BasicBlock::new(id));
        }
        func.get_block_mut(id).unwrap()
    }

    /// Add an instruction to the current basic block
    fn emit(&mut self, insn: Instruction) {
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
    fn integer_promote(&self, typ_id: TypeId) -> TypeId {
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
    fn common_type(&self, left: TypeId, right: TypeId) -> TypeId {
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
            // Both are float or one is float and one is integer
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
    fn emit_convert(&mut self, val: PseudoId, from_typ: TypeId, to_typ: TypeId) -> PseudoId {
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
            let result = self.alloc_pseudo();
            let pseudo = Pseudo::reg(result, result.0);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(pseudo);
            }

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
            self.emit(insn);

            return result;
        }

        // Handle floating point conversions
        if from_float || to_float {
            let result = self.alloc_pseudo();
            let pseudo = Pseudo::reg(result, result.0);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(pseudo);
            }

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
            self.emit(insn);
            return result;
        }

        // Integer to integer conversion
        if from_size == to_size {
            // Same size integers (e.g., signed to unsigned) - no actual conversion needed
            return val;
        }

        let result = self.alloc_pseudo();
        let pseudo = Pseudo::reg(result, result.0);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(pseudo);
        }

        if to_size > from_size {
            // Extending - use sign or zero extension based on source type
            let opcode = if self.types.is_unsigned(from_typ) {
                Opcode::Zext
            } else {
                Opcode::Sext
            };
            let mut insn = Instruction::unop(opcode, result, val, to_typ, to_size);
            insn.src_size = from_size;
            self.emit(insn);
        } else {
            // Truncating
            let mut insn = Instruction::unop(Opcode::Trunc, result, val, to_typ, to_size);
            insn.src_size = from_size;
            self.emit(insn);
        }

        result
    }

    /// Check if current basic block is terminated
    fn is_terminated(&self) -> bool {
        if let Some(bb_id) = self.current_bb {
            if let Some(func) = &self.current_func {
                if let Some(bb) = func.get_block(bb_id) {
                    return bb.is_terminated();
                }
            }
        }
        false
    }

    /// Link two basic blocks (parent -> child)
    fn link_bb(&mut self, from: BasicBlockId, to: BasicBlockId) {
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
    fn switch_bb(&mut self, id: BasicBlockId) {
        self.current_bb = Some(id);
        self.get_or_create_bb(id);
    }

    // ========================================================================
    // Global declarations
    // ========================================================================

    fn linearize_global_decl(&mut self, decl: &Declaration) {
        for declarator in &decl.declarators {
            let modifiers = self.types.modifiers(declarator.typ);

            // Skip typedef declarations - they don't define storage
            if modifiers.contains(TypeModifiers::TYPEDEF) {
                continue;
            }

            // Skip function declarations (they're just forward declarations for external functions)
            if self.types.kind(declarator.typ) == TypeKind::Function {
                continue;
            }

            // Skip extern declarations - they don't define storage
            if modifiers.contains(TypeModifiers::EXTERN) {
                continue;
            }

            let init = declarator.init.as_ref().map_or(Initializer::None, |e| {
                self.ast_init_to_ir(e, declarator.typ)
            });

            let name = self.str(declarator.name).to_string();

            // Track file-scope static variables for inline semantic checks
            if modifiers.contains(TypeModifiers::STATIC) {
                self.file_scope_statics.insert(name.clone());
            }

            self.module.add_global(&name, declarator.typ, init);
        }
    }

    /// Convert an AST initializer expression to an IR Initializer
    ///
    /// This handles:
    /// - Scalar initializers (int, float, char literals)
    /// - String literals (for char arrays or char pointers)
    /// - Array initializers with designated and positional elements
    /// - Struct initializers with designated and positional fields
    /// - Address-of expressions (&symbol)
    /// - Nested initializers
    /// - Compound literals (C99 6.5.2.5)
    fn ast_init_to_ir(&mut self, expr: &Expr, typ: TypeId) -> Initializer {
        match &expr.kind {
            ExprKind::IntLit(v) => Initializer::Int(*v),
            ExprKind::FloatLit(v) => Initializer::Float(*v),
            ExprKind::CharLit(c) => Initializer::Int(*c as i64),

            // String literal - for arrays, store as String; for pointers, create label reference
            ExprKind::StringLit(s) => {
                let type_kind = self.types.kind(typ);
                if type_kind == TypeKind::Array {
                    // char array - embed the string directly
                    Initializer::String(s.clone())
                } else {
                    // Pointer - would need to create a string constant and reference it
                    // For now, just use SymAddr with a label we'll create
                    let label = format!(".LC{}", self.module.strings.len());
                    Initializer::SymAddr(label)
                }
            }

            // Negative literal (fast path for simple cases)
            ExprKind::Unary {
                op: UnaryOp::Neg,
                operand,
            } => match &operand.kind {
                ExprKind::IntLit(v) => Initializer::Int(-*v),
                ExprKind::FloatLit(v) => Initializer::Float(-*v),
                // For more complex expressions like -(1+2), try constant evaluation
                _ => {
                    if let Some(val) = self.eval_const_expr(expr) {
                        Initializer::Int(val)
                    } else {
                        Initializer::None
                    }
                }
            },

            // Address-of expression
            ExprKind::Unary {
                op: UnaryOp::AddrOf,
                operand,
            } => {
                if let ExprKind::Ident { name } = &operand.kind {
                    Initializer::SymAddr(self.str(*name).to_string())
                } else {
                    Initializer::None
                }
            }

            // Cast expression - evaluate the inner expression
            ExprKind::Cast { expr: inner, .. } => self.ast_init_to_ir(inner, typ),

            // Initializer list for arrays/structs
            ExprKind::InitList { elements } => self.ast_init_list_to_ir(elements, typ),

            // Compound literal in initializer context (C99 6.5.2.5)
            ExprKind::CompoundLiteral {
                typ: cl_type,
                elements,
            } => {
                // Check if compound literal type matches target type
                if *cl_type == typ {
                    // Direct value - treat like InitList
                    self.ast_init_list_to_ir(elements, typ)
                } else if self.types.kind(typ) == TypeKind::Pointer {
                    // Pointer initialization - create anonymous static global
                    // and return its address
                    let anon_name = format!(".CL{}", self.compound_literal_counter);
                    self.compound_literal_counter += 1;

                    // Create the anonymous global
                    let init = self.ast_init_list_to_ir(elements, *cl_type);
                    self.module.add_global(&anon_name, *cl_type, init);

                    // Return address of the anonymous global
                    Initializer::SymAddr(anon_name)
                } else {
                    // Type mismatch - try to use the initializer list directly
                    self.ast_init_list_to_ir(elements, typ)
                }
            }

            // Identifier - for constant addresses (function pointers, array decay, etc.)
            // or enum constants
            ExprKind::Ident { name } => {
                let type_kind = self.types.kind(typ);
                // For pointer types, this is likely a function address or array decay
                if type_kind == TypeKind::Pointer {
                    Initializer::SymAddr(self.str(*name).to_string())
                } else {
                    // Check if it's an enum constant
                    if let Some(val) = self.symbols.get_enum_value(*name) {
                        Initializer::Int(val)
                    } else {
                        Initializer::None
                    }
                }
            }

            // Binary/unary expressions and other constant expressions
            // Try to evaluate as integer constant expression
            _ => {
                if let Some(val) = self.eval_const_expr(expr) {
                    Initializer::Int(val)
                } else {
                    Initializer::None
                }
            }
        }
    }

    /// Convert an AST initializer list to an IR Initializer
    fn ast_init_list_to_ir(&mut self, elements: &[InitElement], typ: TypeId) -> Initializer {
        let type_kind = self.types.kind(typ);
        let total_size = (self.types.size_bits(typ) / 8) as usize;

        match type_kind {
            TypeKind::Array => {
                let elem_type = self.types.base_type(typ).unwrap_or(self.types.int_id);
                let elem_size = (self.types.size_bits(elem_type) / 8) as usize;

                let mut init_elements = Vec::new();
                let mut current_idx: i64 = 0;

                for element in elements {
                    // Calculate the actual index considering designators
                    let actual_idx = if element.designators.is_empty() {
                        let idx = current_idx;
                        current_idx += 1;
                        idx
                    } else {
                        // Use the first designator (should be Index for arrays)
                        let idx = match &element.designators[0] {
                            Designator::Index(i) => *i,
                            Designator::Field(_) => current_idx, // Fallback
                        };
                        current_idx = idx + 1;
                        idx
                    };

                    let offset = (actual_idx as usize) * elem_size;
                    let elem_init = self.ast_init_to_ir(&element.value, elem_type);
                    init_elements.push((offset, elem_init));
                }

                // Sort elements by offset to ensure proper emission order
                // (designated initializers can be in any order)
                init_elements.sort_by_key(|(offset, _)| *offset);

                Initializer::Array {
                    elem_size,
                    total_size,
                    elements: init_elements,
                }
            }

            TypeKind::Struct | TypeKind::Union => {
                if let Some(composite) = self.types.get(typ).composite.as_ref() {
                    let members = &composite.members;
                    let mut init_fields = Vec::new();
                    let mut current_field_idx = 0;

                    for element in elements {
                        // Find the field (by designator or position)
                        let member = if let Some(Designator::Field(name)) =
                            element.designators.first()
                        {
                            // Designated initializer: .field = value
                            let found = members.iter().enumerate().find(|(_, m)| &m.name == name);
                            if let Some((idx, m)) = found {
                                current_field_idx = idx + 1;
                                Some(m)
                            } else {
                                None
                            }
                        } else if current_field_idx < members.len() {
                            // Positional initializer
                            let m = &members[current_field_idx];
                            current_field_idx += 1;
                            Some(m)
                        } else {
                            None
                        };

                        if let Some(member) = member {
                            let offset = member.offset;
                            let field_size = (self.types.size_bits(member.typ) / 8) as usize;
                            let field_init = self.ast_init_to_ir(&element.value, member.typ);
                            init_fields.push((offset, field_size, field_init));
                        }
                    }

                    // Sort fields by offset to ensure proper emission order
                    // (designated initializers can be in any order)
                    init_fields.sort_by_key(|(offset, _, _)| *offset);

                    Initializer::Struct {
                        total_size,
                        fields: init_fields,
                    }
                } else {
                    Initializer::None
                }
            }

            _ => {
                // For scalar types, just use the first element
                if let Some(element) = elements.first() {
                    self.ast_init_to_ir(&element.value, typ)
                } else {
                    Initializer::None
                }
            }
        }
    }

    // ========================================================================
    // Function linearization
    // ========================================================================

    fn linearize_function(&mut self, func: &FunctionDef) {
        // Set current position for debug info (function definition location)
        self.current_pos = Some(func.pos);

        // Reset per-function state
        self.next_pseudo = 0;
        self.next_bb = 0;
        self.var_map.clear();
        self.locals.clear();
        self.label_map.clear();
        self.break_targets.clear();
        self.continue_targets.clear();
        self.struct_return_ptr = None;
        self.struct_return_size = 0;
        self.two_reg_return_type = None;
        self.current_func_name = self.str(func.name).to_string();
        // Note: static_locals is NOT cleared - it persists across functions

        // Create function
        let modifiers = self.types.modifiers(func.return_type);
        let is_static = modifiers.contains(TypeModifiers::STATIC);
        let is_inline = modifiers.contains(TypeModifiers::INLINE);
        let _is_extern = modifiers.contains(TypeModifiers::EXTERN);
        let is_noreturn = modifiers.contains(TypeModifiers::NORETURN);

        // Track non-static inline functions for semantic restriction checks
        // C99 6.7.4: non-static inline functions have restrictions on
        // static variables they can access
        self.current_func_is_non_static_inline = is_inline && !is_static;

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
        let mut struct_params: Vec<(String, TypeId, PseudoId)> = Vec::new();
        // Complex parameters also need local storage for real/imag access
        let mut complex_params: Vec<(String, TypeId, PseudoId)> = Vec::new();

        for (i, param) in func.params.iter().enumerate() {
            let name = param
                .name
                .map(|id| self.str(id).to_string())
                .unwrap_or_else(|| format!("arg{}", i));
            ir_func.add_param(&name, param.typ);

            // Create argument pseudo (offset by 1 if there's a hidden return pointer)
            let pseudo_id = self.alloc_pseudo();
            let pseudo = Pseudo::arg(pseudo_id, i as u32 + arg_offset).with_name(&name);
            ir_func.add_pseudo(pseudo);

            // For struct/union types, we'll copy to a local later
            // so member access works properly
            let param_kind = self.types.kind(param.typ);
            if param_kind == TypeKind::Struct || param_kind == TypeKind::Union {
                struct_params.push((name, param.typ, pseudo_id));
            } else if self.types.is_complex(param.typ) {
                // Complex parameters: copy to local storage so real/imag access works
                // Unlike structs, complex types are passed in FP registers per ABI,
                // so we create local storage and the codegen handles the register split
                complex_params.push((name, param.typ, pseudo_id));
            } else {
                self.var_map.insert(name, pseudo_id);
            }
        }

        self.current_func = Some(ir_func);

        // Create entry block
        let entry_bb = self.alloc_bb();
        self.switch_bb(entry_bb);

        // Entry instruction
        self.emit(Instruction::new(Opcode::Entry));

        // Copy struct parameters to local storage so member access works
        for (name, typ, arg_pseudo) in struct_params {
            // Create a symbol pseudo for this local variable (its address)
            let local_sym = self.alloc_pseudo();
            let sym = Pseudo::sym(local_sym, name.clone());
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(sym);
                let is_volatile = self.types.modifiers(typ).contains(TypeModifiers::VOLATILE);
                func.add_local(&name, local_sym, typ, is_volatile, None);
            }

            let typ_size = self.types.size_bits(typ);
            // For large structs (> 64 bits), arg_pseudo is a pointer to the struct
            // We need to copy the data from that pointer to local storage
            // Note: We receive struct parameters > 64 bits as pointers, even though
            // the ABI allows two-register passing for 9-16 byte structs.
            if typ_size > 64 {
                // arg_pseudo is a pointer - copy each 8-byte chunk
                let struct_size = typ_size / 8;
                let mut offset = 0i64;
                while offset < struct_size as i64 {
                    // Load 8 bytes from arg_pseudo + offset
                    let temp = self.alloc_pseudo();
                    let temp_pseudo = Pseudo::reg(temp, temp.0);
                    if let Some(func) = &mut self.current_func {
                        func.add_pseudo(temp_pseudo);
                    }
                    self.emit(Instruction::load(
                        temp,
                        arg_pseudo,
                        offset,
                        self.types.long_id,
                        64,
                    ));
                    // Store to local_sym + offset
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

            // Register as a local variable
            self.locals.insert(
                name,
                LocalVarInfo {
                    sym: local_sym,
                    typ,
                    vla_size_sym: None,
                    vla_elem_type: None,
                    vla_dim_syms: vec![],
                },
            );
        }

        // Setup local storage for complex parameters
        // Complex types are passed in FP registers per ABI - the prologue codegen
        // handles storing from XMM registers to local storage
        for (name, typ, _arg_pseudo) in complex_params {
            // Create a symbol pseudo for this local variable (its address)
            let local_sym = self.alloc_pseudo();
            let sym = Pseudo::sym(local_sym, name.clone());
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(sym);
                let is_volatile = self.types.modifiers(typ).contains(TypeModifiers::VOLATILE);
                func.add_local(&name, local_sym, typ, is_volatile, None);
            }

            // Don't emit a store here - the prologue codegen handles storing
            // from XMM0+XMM1/XMM2+XMM3/etc to local storage

            // Register as a local variable for name lookup
            self.locals.insert(
                name,
                LocalVarInfo {
                    sym: local_sym,
                    typ,
                    vla_size_sym: None,
                    vla_elem_type: None,
                    vla_dim_syms: vec![],
                },
            );
        }

        // Linearize body
        self.linearize_stmt(&func.body);

        // Ensure function ends with a return
        if !self.is_terminated() {
            if ret_kind == TypeKind::Void {
                self.emit(Instruction::ret(None));
            } else {
                // Return 0 as default
                let zero = self.emit_const(0, self.types.int_id);
                self.emit(Instruction::ret(Some(zero)));
            }
        }

        // Run SSA conversion if enabled
        if self.run_ssa {
            if let Some(ref mut ir_func) = self.current_func {
                ssa_convert(ir_func, self.types);
            }
        }

        // Add function to module
        if let Some(ir_func) = self.current_func.take() {
            self.module.add_function(ir_func);
        }
    }

    // ========================================================================
    // Statement linearization
    // ========================================================================

    /// Emit large struct return via hidden pointer (sret)
    fn emit_sret_return(&mut self, e: &Expr, sret_ptr: PseudoId, struct_size: u32) {
        let src_addr = self.linearize_lvalue(e);
        let struct_bytes = struct_size as i64 / 8;
        let mut byte_offset = 0i64;

        while byte_offset < struct_bytes {
            let temp = self.alloc_pseudo();
            let temp_pseudo = Pseudo::reg(temp, temp.0);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(temp_pseudo);
            }
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
    fn emit_two_reg_return(&mut self, e: &Expr, ret_type: TypeId) {
        let src_addr = self.linearize_lvalue(e);
        let struct_size = self.types.size_bits(ret_type);

        // Load first 8 bytes
        let low_temp = self.alloc_pseudo();
        let low_pseudo = Pseudo::reg(low_temp, low_temp.0);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(low_pseudo);
        }
        self.emit(Instruction::load(
            low_temp,
            src_addr,
            0,
            self.types.long_id,
            64,
        ));

        // Load second portion (remaining bytes, up to 8)
        let high_temp = self.alloc_pseudo();
        let high_pseudo = Pseudo::reg(high_temp, high_temp.0);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(high_pseudo);
        }
        let high_size = std::cmp::min(64, struct_size - 64);
        self.emit(Instruction::load(
            high_temp,
            src_addr,
            8,
            self.types.long_id,
            high_size,
        ));

        // Emit return with both values and two_reg_return flag
        let mut ret_insn = Instruction::ret_typed(Some(low_temp), ret_type, struct_size);
        ret_insn.src.push(high_temp);
        ret_insn.is_two_reg_return = true;
        self.emit(ret_insn);
    }

    fn linearize_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Empty => {}

            Stmt::Expr(expr) => {
                self.linearize_expr(expr);
            }

            Stmt::Block(items) => {
                for item in items {
                    match item {
                        BlockItem::Declaration(decl) => self.linearize_local_decl(decl),
                        BlockItem::Statement(s) => self.linearize_stmt(s),
                    }
                }
            }

            Stmt::If {
                cond,
                then_stmt,
                else_stmt,
            } => {
                self.linearize_if(cond, then_stmt, else_stmt.as_deref());
            }

            Stmt::While { cond, body } => {
                self.linearize_while(cond, body);
            }

            Stmt::DoWhile { body, cond } => {
                self.linearize_do_while(body, cond);
            }

            Stmt::For {
                init,
                cond,
                post,
                body,
            } => {
                self.linearize_for(init.as_ref(), cond.as_ref(), post.as_ref(), body);
            }

            Stmt::Return(expr) => {
                if let Some(e) = expr {
                    let typ = self.expr_type(e);

                    if let Some(sret_ptr) = self.struct_return_ptr {
                        self.emit_sret_return(e, sret_ptr, self.struct_return_size);
                    } else if let Some(ret_type) = self.two_reg_return_type {
                        self.emit_two_reg_return(e, ret_type);
                    } else if self.types.is_complex(typ) {
                        let addr = self.linearize_lvalue(e);
                        let typ_size = self.types.size_bits(typ);
                        self.emit(Instruction::ret_typed(Some(addr), typ, typ_size));
                    } else {
                        let val = self.linearize_expr(e);
                        let typ_size = self.types.size_bits(typ);
                        self.emit(Instruction::ret_typed(Some(val), typ, typ_size));
                    }
                } else {
                    self.emit(Instruction::ret(None));
                }
            }

            Stmt::Break => {
                if let Some(&target) = self.break_targets.last() {
                    if let Some(current) = self.current_bb {
                        self.emit(Instruction::br(target));
                        self.link_bb(current, target);
                    }
                }
            }

            Stmt::Continue => {
                if let Some(&target) = self.continue_targets.last() {
                    if let Some(current) = self.current_bb {
                        self.emit(Instruction::br(target));
                        self.link_bb(current, target);
                    }
                }
            }

            Stmt::Goto(label) => {
                let label_str = self.str(*label).to_string();
                let target = self.get_or_create_label(&label_str);
                if let Some(current) = self.current_bb {
                    self.emit(Instruction::br(target));
                    self.link_bb(current, target);
                }

                // Set current_bb to None - any subsequent code until a label is dead
                // emit() will safely skip when current_bb is None
                self.current_bb = None;
            }

            Stmt::Label { name, stmt } => {
                let name_str = self.str(*name).to_string();
                let label_bb = self.get_or_create_label(&name_str);

                // If current block is not terminated, branch to label
                if !self.is_terminated() {
                    if let Some(current) = self.current_bb {
                        self.emit(Instruction::br(label_bb));
                        self.link_bb(current, label_bb);
                    }
                }

                self.switch_bb(label_bb);
                self.linearize_stmt(stmt);
            }

            Stmt::Switch { expr, body } => {
                self.linearize_switch(expr, body);
            }

            Stmt::Case(_) | Stmt::Default => {
                // Case/Default labels are handled by linearize_switch
                // If we encounter them outside a switch, ignore them
            }
        }
    }

    fn linearize_local_decl(&mut self, decl: &Declaration) {
        for declarator in &decl.declarators {
            let typ = declarator.typ;

            // Check if this is a static local variable
            if self.types.modifiers(typ).contains(TypeModifiers::STATIC) {
                // Static local: create a global with unique name
                self.linearize_static_local(declarator);
                continue;
            }

            // Check if this is a VLA (Variable Length Array)
            if !declarator.vla_sizes.is_empty() {
                // C99 6.7.8: VLAs cannot have initializers
                if declarator.init.is_some() {
                    if let Some(pos) = self.current_pos {
                        error(pos, "variable length arrays cannot have initializers");
                    }
                }
                self.linearize_vla_decl(declarator);
                continue;
            }

            // Create a symbol pseudo for this local variable (its address)
            // Use unique name (name#id) to distinguish shadowed variables for SSA
            let sym_id = self.alloc_pseudo();
            let unique_name = format!("{}#{}", declarator.name, sym_id.0);
            let sym = Pseudo::sym(sym_id, unique_name.clone());
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(sym);
                // Register with function's local variable tracking for SSA
                // Pass the current basic block as the declaration block for scope-aware phi placement
                let is_volatile = self.types.modifiers(typ).contains(TypeModifiers::VOLATILE);
                func.add_local(&unique_name, sym_id, typ, is_volatile, self.current_bb);
            }

            // Track in linearizer's locals map
            let name_str = self.str(declarator.name).to_string();
            self.locals.insert(
                name_str,
                LocalVarInfo {
                    sym: sym_id,
                    typ,
                    vla_size_sym: None,
                    vla_elem_type: None,
                    vla_dim_syms: vec![],
                },
            );

            // If there's an initializer, emit Store(s)
            if let Some(init) = &declarator.init {
                if let ExprKind::InitList { elements } = &init.kind {
                    // Handle initializer list for arrays and structs
                    self.linearize_init_list(sym_id, typ, elements);
                } else if self.types.is_complex(typ) {
                    // Complex type initialization - linearize_expr returns an address
                    // to a temp containing the complex value. Copy from temp to local.
                    let value_addr = self.linearize_expr(init);
                    let base_typ = self.types.complex_base(typ);
                    let base_bits = self.types.size_bits(base_typ);
                    let base_bytes = (base_bits / 8) as i64;

                    // Load real and imag parts from source temp
                    let val_real = self.alloc_pseudo();
                    let val_imag = self.alloc_pseudo();
                    self.emit(Instruction::load(
                        val_real, value_addr, 0, base_typ, base_bits,
                    ));
                    self.emit(Instruction::load(
                        val_imag, value_addr, base_bytes, base_typ, base_bits,
                    ));

                    // Store to local variable
                    self.emit(Instruction::store(val_real, sym_id, 0, base_typ, base_bits));
                    self.emit(Instruction::store(
                        val_imag, sym_id, base_bytes, base_typ, base_bits,
                    ));
                } else {
                    // Simple scalar initializer
                    let val = self.linearize_expr(init);
                    // Convert the value to the target type (important for _Bool normalization)
                    let init_type = self.expr_type(init);
                    let converted = self.emit_convert(val, init_type, typ);
                    let size = self.types.size_bits(typ);
                    self.emit(Instruction::store(converted, sym_id, 0, typ, size));
                }
            }
        }
    }

    /// Linearize a VLA (Variable Length Array) declaration
    ///
    /// VLAs are allocated on the stack at runtime using Alloca.
    /// The size is computed as: product of all dimension sizes * sizeof(element_type)
    ///
    /// Unlike regular arrays where the symbol is the address of stack memory,
    /// for VLAs we store the Alloca result (pointer) and then access it through
    /// a pointer load. We create a pointer variable to hold the VLA address.
    ///
    /// We also create hidden local variables to store the dimension sizes
    /// so that sizeof(vla) can be computed at runtime.
    fn linearize_vla_decl(&mut self, declarator: &crate::parse::ast::InitDeclarator) {
        let typ = declarator.typ;

        // Get the element type by stripping only VLA dimensions from the array type.
        // For int arr[n][4], vla_sizes has 1 element, so we strip 1 dimension to get int[4].
        // For int arr[n][m], vla_sizes has 2 elements, so we strip 2 dimensions to get int.
        let num_vla_dims = declarator.vla_sizes.len();
        let mut elem_type = typ;
        for _ in 0..num_vla_dims {
            if self.types.kind(elem_type) == TypeKind::Array {
                elem_type = self.types.base_type(elem_type).unwrap_or(self.types.int_id);
            }
        }
        let elem_size = self.types.size_bytes(elem_type) as i64;

        let ulong_type = self.types.ulong_id;

        // Evaluate all VLA size expressions, store each in a hidden local,
        // and compute total element count.
        // For int arr[n][m], we store n and m separately (for stride computation)
        // and compute total_count = n * m.
        let mut vla_dim_syms: Vec<PseudoId> = Vec::new();
        let mut total_count: Option<PseudoId> = None;

        for (dim_idx, vla_size_expr) in declarator.vla_sizes.iter().enumerate() {
            let dim_size = self.linearize_expr(vla_size_expr);

            // Create a hidden local to store this dimension's size
            // This is needed for runtime stride computation in multi-dimensional VLAs
            let dim_sym_id = self.alloc_pseudo();
            let dim_var_name = format!("__vla_dim{}_{}#{}", dim_idx, declarator.name, dim_sym_id.0);
            let dim_sym = Pseudo::sym(dim_sym_id, dim_var_name.clone());

            if let Some(func) = &mut self.current_func {
                func.add_pseudo(dim_sym);
                func.add_local(
                    &dim_var_name,
                    dim_sym_id,
                    ulong_type,
                    false,
                    self.current_bb,
                );
            }

            // Store the dimension size
            let store_dim_insn = Instruction::store(dim_size, dim_sym_id, 0, ulong_type, 64);
            self.emit(store_dim_insn);
            vla_dim_syms.push(dim_sym_id);

            // Update running total count
            total_count = Some(match total_count {
                None => dim_size,
                Some(prev) => {
                    // Multiply: prev * dim_size
                    let result = self.alloc_pseudo();
                    let mul_insn = Instruction::new(Opcode::Mul)
                        .with_target(result)
                        .with_src(prev)
                        .with_src(dim_size)
                        .with_size(64);
                    self.emit(mul_insn);
                    result
                }
            });
        }
        let num_elements = total_count.expect("VLA must have at least one size expression");

        // Create a hidden local variable to store the total number of elements
        // This is needed for sizeof(vla) to work at runtime
        let size_sym_id = self.alloc_pseudo();
        let size_var_name = format!("__vla_size_{}#{}", declarator.name, size_sym_id.0);
        let size_sym = Pseudo::sym(size_sym_id, size_var_name.clone());

        if let Some(func) = &mut self.current_func {
            func.add_pseudo(size_sym);
            func.add_local(
                &size_var_name,
                size_sym_id,
                ulong_type,
                false,
                self.current_bb,
            );
        }

        // Store num_elements into the hidden size variable
        let store_size_insn = Instruction::store(num_elements, size_sym_id, 0, ulong_type, 64);
        self.emit(store_size_insn);

        // Compute total size in bytes: num_elements * sizeof(element)
        let elem_size_const = self.emit_const(elem_size, self.types.long_id);
        let total_size = self.alloc_pseudo();
        let mul_insn = Instruction::new(Opcode::Mul)
            .with_target(total_size)
            .with_src(num_elements)
            .with_src(elem_size_const)
            .with_size(64);
        self.emit(mul_insn);

        // Emit Alloca instruction to allocate stack space
        let alloca_result = self.alloc_pseudo();
        let alloca_insn = Instruction::new(Opcode::Alloca)
            .with_target(alloca_result)
            .with_src(total_size)
            .with_type_and_size(self.types.void_ptr_id, 64);
        self.emit(alloca_insn);

        // Create a symbol pseudo for the VLA pointer variable
        // This symbol stores the pointer to the VLA memory (like a pointer variable)
        let sym_id = self.alloc_pseudo();
        let unique_name = format!("{}#{}", declarator.name, sym_id.0);
        let sym = Pseudo::sym(sym_id, unique_name.clone());

        // Create a pointer type for the VLA (pointer to element type)
        let ptr_type = self.types.pointer_to(elem_type);

        if let Some(func) = &mut self.current_func {
            func.add_pseudo(sym);
            // Register as a pointer variable, not as the array type
            let is_volatile = self.types.modifiers(typ).contains(TypeModifiers::VOLATILE);
            func.add_local(&unique_name, sym_id, ptr_type, is_volatile, self.current_bb);
        }

        // Store the Alloca result (pointer) into the VLA symbol
        let store_insn = Instruction::store(alloca_result, sym_id, 0, ptr_type, 64);
        self.emit(store_insn);

        // Track in linearizer's locals map with pointer type and VLA size info
        // This makes arr[i] behave like ptr[i] - load ptr, then offset
        let name_str = self.str(declarator.name).to_string();
        self.locals.insert(
            name_str,
            LocalVarInfo {
                sym: sym_id,
                typ: ptr_type,
                vla_size_sym: Some(size_sym_id),
                vla_elem_type: Some(elem_type),
                vla_dim_syms,
            },
        );
    }

    /// Linearize a static local variable declaration
    ///
    /// Static locals have static storage duration but no linkage.
    /// They are implemented as globals with unique names like `funcname.varname.N`.
    /// Initialization happens once at program start (compile-time).
    fn linearize_static_local(&mut self, declarator: &crate::parse::ast::InitDeclarator) {
        let name_str = self.str(declarator.name).to_string();

        // C99 6.7.4p3: A non-static inline function cannot define a non-const
        // function-local static variable
        if self.current_func_is_non_static_inline {
            let is_const = self
                .types
                .modifiers(declarator.typ)
                .contains(TypeModifiers::CONST);
            if !is_const {
                if let Some(pos) = self.current_pos {
                    error(
                        pos,
                        &format!(
                            "non-static inline function '{}' cannot define non-const static variable '{}'",
                            self.current_func_name, name_str
                        ),
                    );
                }
            }
        }

        // Generate unique global name: funcname.varname.counter
        let global_name = format!(
            "{}.{}.{}",
            self.current_func_name, name_str, self.static_local_counter
        );
        self.static_local_counter += 1;

        // Track mapping from local name to global name for this function's scope
        // Use a key that includes function name to handle same-named statics in different functions
        let key = format!("{}.{}", self.current_func_name, name_str);
        self.static_locals.insert(
            key,
            StaticLocalInfo {
                global_name: global_name.clone(),
                typ: declarator.typ,
            },
        );

        // Also insert with just the local name for the current function scope
        // This is used during expression linearization
        self.locals.insert(
            name_str,
            LocalVarInfo {
                // Use a sentinel value - we'll handle static locals specially
                sym: PseudoId(u32::MAX),
                typ: declarator.typ,
                vla_size_sym: None,
                vla_elem_type: None,
                vla_dim_syms: vec![],
            },
        );

        // Determine initializer (static locals are initialized at compile time)
        let init = declarator.init.as_ref().map_or(Initializer::None, |e| {
            self.ast_init_to_ir(e, declarator.typ)
        });

        // Add as a global (type already has STATIC modifier which codegen uses)
        self.module.add_global(&global_name, declarator.typ, init);
    }

    /// Linearize an initializer list for arrays or structs
    fn linearize_init_list(&mut self, base_sym: PseudoId, typ: TypeId, elements: &[InitElement]) {
        self.linearize_init_list_at_offset(base_sym, 0, typ, elements);
    }

    /// Linearize an initializer list at a given base offset
    fn linearize_init_list_at_offset(
        &mut self,
        base_sym: PseudoId,
        base_offset: i64,
        typ: TypeId,
        elements: &[InitElement],
    ) {
        match self.types.kind(typ) {
            TypeKind::Array => {
                let elem_type = self.types.base_type(typ).unwrap_or(self.types.int_id);
                let elem_size = self.types.size_bits(elem_type) / 8;

                for (idx, element) in elements.iter().enumerate() {
                    // Calculate the actual index considering designators
                    let actual_idx = if element.designators.is_empty() {
                        idx as i64
                    } else {
                        // Use the first designator (should be Index for arrays)
                        match &element.designators[0] {
                            Designator::Index(i) => *i,
                            Designator::Field(_) => idx as i64, // Fall back for mismatched designator
                        }
                    };

                    let offset = base_offset + actual_idx * elem_size as i64;

                    // Handle nested initializer lists or scalar values
                    if let ExprKind::InitList {
                        elements: nested_elems,
                    } = &element.value.kind
                    {
                        // Nested array/struct initialization - recurse with accumulated offset
                        self.linearize_init_list_at_offset(
                            base_sym,
                            offset,
                            elem_type,
                            nested_elems,
                        );
                    } else {
                        // Scalar value
                        let val = self.linearize_expr(&element.value);
                        let val_type = self.expr_type(&element.value);
                        let converted = self.emit_convert(val, val_type, elem_type);
                        let elem_size = self.types.size_bits(elem_type);
                        self.emit(Instruction::store(
                            converted, base_sym, offset, elem_type, elem_size,
                        ));
                    }
                }
            }
            TypeKind::Struct | TypeKind::Union => {
                // Get struct fields from the type's composite data
                if let Some(composite) = self.types.get(typ).composite.as_ref() {
                    // Clone members to avoid borrow issues
                    let members: Vec<_> = composite.members.clone();

                    for (idx, element) in elements.iter().enumerate() {
                        // Find the field (by designator or position)
                        let member =
                            if let Some(Designator::Field(name)) = element.designators.first() {
                                // Designated initializer: .field = value
                                members.iter().find(|m| &m.name == name)
                            } else if idx < members.len() {
                                // Positional initializer
                                Some(&members[idx])
                            } else {
                                None // Too many initializers
                            };

                        let Some(member) = member else {
                            continue;
                        };

                        let offset = base_offset + member.offset as i64;
                        let field_type = member.typ;

                        // Handle nested initializer lists or scalar values
                        if let ExprKind::InitList {
                            elements: nested_elems,
                        } = &element.value.kind
                        {
                            // Nested struct/array initialization - recurse with accumulated offset
                            self.linearize_init_list_at_offset(
                                base_sym,
                                offset,
                                field_type,
                                nested_elems,
                            );
                        } else {
                            // Scalar value
                            let val = self.linearize_expr(&element.value);
                            let val_type = self.expr_type(&element.value);
                            let converted = self.emit_convert(val, val_type, field_type);
                            let field_size = self.types.size_bits(field_type);
                            self.emit(Instruction::store(
                                converted, base_sym, offset, field_type, field_size,
                            ));
                        }
                    }
                }
            }
            _ => {
                // For other types, just use the first element if present
                if let Some(element) = elements.first() {
                    let val = self.linearize_expr(&element.value);
                    let val_type = self.expr_type(&element.value);
                    let converted = self.emit_convert(val, val_type, typ);
                    let typ_size = self.types.size_bits(typ);
                    self.emit(Instruction::store(
                        converted,
                        base_sym,
                        base_offset,
                        typ,
                        typ_size,
                    ));
                }
            }
        }
    }

    fn linearize_if(&mut self, cond: &Expr, then_stmt: &Stmt, else_stmt: Option<&Stmt>) {
        let cond_val = self.linearize_expr(cond);

        let then_bb = self.alloc_bb();
        let else_bb = self.alloc_bb();
        let merge_bb = self.alloc_bb();

        // Conditional branch
        if let Some(current) = self.current_bb {
            if else_stmt.is_some() {
                self.emit(Instruction::cbr(cond_val, then_bb, else_bb));
            } else {
                self.emit(Instruction::cbr(cond_val, then_bb, merge_bb));
            }
            self.link_bb(current, then_bb);
            if else_stmt.is_some() {
                self.link_bb(current, else_bb);
            } else {
                self.link_bb(current, merge_bb);
            }
        }

        // Then block
        self.switch_bb(then_bb);
        self.linearize_stmt(then_stmt);
        if !self.is_terminated() {
            self.emit(Instruction::br(merge_bb));
            self.link_bb(then_bb, merge_bb);
        }

        // Else block
        if let Some(else_s) = else_stmt {
            self.switch_bb(else_bb);
            self.linearize_stmt(else_s);
            if !self.is_terminated() {
                self.emit(Instruction::br(merge_bb));
                self.link_bb(else_bb, merge_bb);
            }
        }

        // Merge block
        self.switch_bb(merge_bb);
    }

    fn linearize_while(&mut self, cond: &Expr, body: &Stmt) {
        let cond_bb = self.alloc_bb();
        let body_bb = self.alloc_bb();
        let exit_bb = self.alloc_bb();

        // Jump to condition
        if let Some(current) = self.current_bb {
            if !self.is_terminated() {
                self.emit(Instruction::br(cond_bb));
                self.link_bb(current, cond_bb);
            }
        }

        // Condition block
        self.switch_bb(cond_bb);
        let cond_val = self.linearize_expr(cond);
        self.emit(Instruction::cbr(cond_val, body_bb, exit_bb));
        self.link_bb(cond_bb, body_bb);
        self.link_bb(cond_bb, exit_bb);

        // Body block
        self.break_targets.push(exit_bb);
        self.continue_targets.push(cond_bb);

        self.switch_bb(body_bb);
        self.linearize_stmt(body);
        if !self.is_terminated() {
            // After linearizing body, current_bb may be different from body_bb
            // (e.g., if body contains nested loops). Link the CURRENT block to cond_bb.
            if let Some(current) = self.current_bb {
                self.emit(Instruction::br(cond_bb));
                self.link_bb(current, cond_bb);
            }
        }

        self.break_targets.pop();
        self.continue_targets.pop();

        // Exit block
        self.switch_bb(exit_bb);
    }

    fn linearize_do_while(&mut self, body: &Stmt, cond: &Expr) {
        let body_bb = self.alloc_bb();
        let cond_bb = self.alloc_bb();
        let exit_bb = self.alloc_bb();

        // Jump to body
        if let Some(current) = self.current_bb {
            if !self.is_terminated() {
                self.emit(Instruction::br(body_bb));
                self.link_bb(current, body_bb);
            }
        }

        // Body block
        self.break_targets.push(exit_bb);
        self.continue_targets.push(cond_bb);

        self.switch_bb(body_bb);
        self.linearize_stmt(body);
        if !self.is_terminated() {
            // After linearizing body, current_bb may be different from body_bb
            if let Some(current) = self.current_bb {
                self.emit(Instruction::br(cond_bb));
                self.link_bb(current, cond_bb);
            }
        }

        self.break_targets.pop();
        self.continue_targets.pop();

        // Condition block
        self.switch_bb(cond_bb);
        let cond_val = self.linearize_expr(cond);
        self.emit(Instruction::cbr(cond_val, body_bb, exit_bb));
        self.link_bb(cond_bb, body_bb);
        self.link_bb(cond_bb, exit_bb);

        // Exit block
        self.switch_bb(exit_bb);
    }

    fn linearize_for(
        &mut self,
        init: Option<&ForInit>,
        cond: Option<&Expr>,
        post: Option<&Expr>,
        body: &Stmt,
    ) {
        // Init
        if let Some(init) = init {
            match init {
                ForInit::Declaration(decl) => self.linearize_local_decl(decl),
                ForInit::Expression(expr) => {
                    self.linearize_expr(expr);
                }
            }
        }

        let cond_bb = self.alloc_bb();
        let body_bb = self.alloc_bb();
        let post_bb = self.alloc_bb();
        let exit_bb = self.alloc_bb();

        // Jump to condition
        if let Some(current) = self.current_bb {
            if !self.is_terminated() {
                self.emit(Instruction::br(cond_bb));
                self.link_bb(current, cond_bb);
            }
        }

        // Condition block
        self.switch_bb(cond_bb);
        if let Some(cond_expr) = cond {
            let cond_val = self.linearize_expr(cond_expr);
            self.emit(Instruction::cbr(cond_val, body_bb, exit_bb));
        } else {
            // No condition = always true
            self.emit(Instruction::br(body_bb));
        }
        self.link_bb(cond_bb, body_bb);
        self.link_bb(cond_bb, exit_bb);

        // Body block
        self.break_targets.push(exit_bb);
        self.continue_targets.push(post_bb);

        self.switch_bb(body_bb);
        self.linearize_stmt(body);
        if !self.is_terminated() {
            // After linearizing body, current_bb may be different from body_bb
            if let Some(current) = self.current_bb {
                self.emit(Instruction::br(post_bb));
                self.link_bb(current, post_bb);
            }
        }

        self.break_targets.pop();
        self.continue_targets.pop();

        // Post block
        self.switch_bb(post_bb);
        if let Some(post_expr) = post {
            self.linearize_expr(post_expr);
        }
        self.emit(Instruction::br(cond_bb));
        self.link_bb(post_bb, cond_bb);

        // Exit block
        self.switch_bb(exit_bb);
    }

    fn linearize_switch(&mut self, expr: &Expr, body: &Stmt) {
        // Linearize the switch expression
        let switch_val = self.linearize_expr(expr);
        let expr_type = self.expr_type(expr);
        let size = self.types.size_bits(expr_type);

        let exit_bb = self.alloc_bb();

        // Push exit block for break handling
        self.break_targets.push(exit_bb);

        // Collect case labels and create basic blocks for each
        let (case_values, has_default) = self.collect_switch_cases(body);
        let case_bbs: Vec<BasicBlockId> = case_values.iter().map(|_| self.alloc_bb()).collect();
        let default_bb = if has_default {
            Some(self.alloc_bb())
        } else {
            None
        };

        // Build switch instruction with case -> block mapping
        let switch_cases: Vec<(i64, BasicBlockId)> = case_values
            .iter()
            .zip(case_bbs.iter())
            .map(|(val, bb)| (*val, *bb))
            .collect();

        // Default goes to default_bb if present, otherwise exit_bb
        let default_target = default_bb.unwrap_or(exit_bb);

        // Emit switch instruction
        self.emit(Instruction::switch_insn(
            switch_val,
            switch_cases.clone(),
            Some(default_target),
            size,
        ));

        // Link CFG edges from current block to all case/default/exit blocks
        if let Some(current) = self.current_bb {
            for &(_, bb) in &switch_cases {
                self.link_bb(current, bb);
            }
            self.link_bb(current, default_target);
            if default_bb.is_none() {
                self.link_bb(current, exit_bb);
            }
        }

        // Linearize body with case block switching
        self.linearize_switch_body(body, &case_values, &case_bbs, default_bb);

        // If not terminated after body, jump to exit
        if !self.is_terminated() {
            if let Some(current) = self.current_bb {
                self.emit(Instruction::br(exit_bb));
                self.link_bb(current, exit_bb);
            }
        }

        self.break_targets.pop();

        // Exit block
        self.switch_bb(exit_bb);
    }

    /// Collect case values from switch body (must be a Block)
    /// Returns (case_values, has_default)
    fn collect_switch_cases(&self, body: &Stmt) -> (Vec<i64>, bool) {
        let mut case_values = Vec::new();
        let mut has_default = false;

        if let Stmt::Block(items) = body {
            for item in items {
                if let BlockItem::Statement(stmt) = item {
                    self.collect_cases_from_stmt(stmt, &mut case_values, &mut has_default);
                }
            }
        }

        (case_values, has_default)
    }

    fn collect_cases_from_stmt(
        &self,
        stmt: &Stmt,
        case_values: &mut Vec<i64>,
        has_default: &mut bool,
    ) {
        match stmt {
            Stmt::Case(expr) => {
                // Extract constant value from case expression
                if let Some(val) = self.eval_const_expr(expr) {
                    case_values.push(val);
                }
            }
            Stmt::Default => {
                *has_default = true;
            }
            // Recursively check labeled statements
            Stmt::Label { stmt, .. } => {
                self.collect_cases_from_stmt(stmt, case_values, has_default);
            }
            _ => {}
        }
    }

    /// Evaluate a constant expression (for case labels, static initializers)
    ///
    /// C99 6.6 defines integer constant expressions. This function evaluates
    /// expressions that can be computed at compile time.
    fn eval_const_expr(&self, expr: &Expr) -> Option<i64> {
        match &expr.kind {
            ExprKind::IntLit(val) => Some(*val),
            ExprKind::CharLit(c) => Some(*c as i64),

            ExprKind::Ident { name, .. } => {
                // Check if it's an enum constant
                self.symbols.get_enum_value(*name)
            }

            ExprKind::Unary { op, operand } => {
                let val = self.eval_const_expr(operand)?;
                match op {
                    UnaryOp::Neg => Some(-val),
                    UnaryOp::Not => Some(if val == 0 { 1 } else { 0 }),
                    UnaryOp::BitNot => Some(!val),
                    _ => None,
                }
            }

            ExprKind::Binary { op, left, right } => {
                let l = self.eval_const_expr(left)?;
                let r = self.eval_const_expr(right)?;
                match op {
                    BinaryOp::Add => Some(l.wrapping_add(r)),
                    BinaryOp::Sub => Some(l.wrapping_sub(r)),
                    BinaryOp::Mul => Some(l.wrapping_mul(r)),
                    BinaryOp::Div => {
                        if r != 0 {
                            Some(l / r)
                        } else {
                            None
                        }
                    }
                    BinaryOp::Mod => {
                        if r != 0 {
                            Some(l % r)
                        } else {
                            None
                        }
                    }
                    BinaryOp::BitAnd => Some(l & r),
                    BinaryOp::BitOr => Some(l | r),
                    BinaryOp::BitXor => Some(l ^ r),
                    BinaryOp::Shl | BinaryOp::Shr => {
                        // Mask shift amount to match C semantics and target hardware behavior.
                        // After C integer promotion: 8/16/32-bit types use 32-bit shifts (mask 31),
                        // 64-bit types use 64-bit shifts (mask 63).
                        let size_bits = left.typ.map(|t| self.types.size_bits(t)).unwrap_or(32);
                        let mask = if size_bits > 32 { 63 } else { 31 };
                        let shift_amt = (r & mask) as u32;
                        match op {
                            BinaryOp::Shl => Some(l << shift_amt),
                            BinaryOp::Shr => Some(l >> shift_amt),
                            _ => unreachable!(),
                        }
                    }
                    BinaryOp::Lt => Some(if l < r { 1 } else { 0 }),
                    BinaryOp::Le => Some(if l <= r { 1 } else { 0 }),
                    BinaryOp::Gt => Some(if l > r { 1 } else { 0 }),
                    BinaryOp::Ge => Some(if l >= r { 1 } else { 0 }),
                    BinaryOp::Eq => Some(if l == r { 1 } else { 0 }),
                    BinaryOp::Ne => Some(if l != r { 1 } else { 0 }),
                    BinaryOp::LogAnd => Some(if l != 0 && r != 0 { 1 } else { 0 }),
                    BinaryOp::LogOr => Some(if l != 0 || r != 0 { 1 } else { 0 }),
                }
            }

            ExprKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_val = self.eval_const_expr(cond)?;
                if cond_val != 0 {
                    self.eval_const_expr(then_expr)
                } else {
                    self.eval_const_expr(else_expr)
                }
            }

            // sizeof(type) - constant for complete types
            ExprKind::SizeofType(type_id) => {
                let size_bits = self.types.size_bits(*type_id);
                Some((size_bits / 8) as i64)
            }

            // sizeof(expr) - constant if expr type is complete
            ExprKind::SizeofExpr(inner_expr) => {
                if let Some(typ) = inner_expr.typ {
                    let size_bits = self.types.size_bits(typ);
                    Some((size_bits / 8) as i64)
                } else {
                    None
                }
            }

            // Cast to integer type - evaluate inner expression
            ExprKind::Cast { expr: inner, .. } => self.eval_const_expr(inner),

            _ => None,
        }
    }

    /// Linearize switch body, switching basic blocks at case/default labels
    fn linearize_switch_body(
        &mut self,
        body: &Stmt,
        case_values: &[i64],
        case_bbs: &[BasicBlockId],
        default_bb: Option<BasicBlockId>,
    ) {
        let mut case_idx = 0;

        if let Stmt::Block(items) = body {
            for item in items {
                match item {
                    BlockItem::Declaration(decl) => self.linearize_local_decl(decl),
                    BlockItem::Statement(stmt) => {
                        self.linearize_switch_stmt(
                            stmt,
                            case_values,
                            case_bbs,
                            default_bb,
                            &mut case_idx,
                        );
                    }
                }
            }
        }
    }

    fn linearize_switch_stmt(
        &mut self,
        stmt: &Stmt,
        case_values: &[i64],
        case_bbs: &[BasicBlockId],
        default_bb: Option<BasicBlockId>,
        case_idx: &mut usize,
    ) {
        match stmt {
            Stmt::Case(expr) => {
                // Find the matching case block
                if let Some(val) = self.eval_const_expr(expr) {
                    if let Some(idx) = case_values.iter().position(|v| *v == val) {
                        let case_bb = case_bbs[idx];

                        // Fall through from previous case if not terminated
                        if !self.is_terminated() {
                            if let Some(current) = self.current_bb {
                                self.emit(Instruction::br(case_bb));
                                self.link_bb(current, case_bb);
                            }
                        }

                        self.switch_bb(case_bb);
                        *case_idx = idx + 1;
                    }
                }
            }
            Stmt::Default => {
                if let Some(def_bb) = default_bb {
                    // Fall through from previous case if not terminated
                    if !self.is_terminated() {
                        if let Some(current) = self.current_bb {
                            self.emit(Instruction::br(def_bb));
                            self.link_bb(current, def_bb);
                        }
                    }

                    self.switch_bb(def_bb);
                }
            }
            _ => {
                // Regular statement - linearize it
                self.linearize_stmt(stmt);
            }
        }
    }

    fn get_or_create_label(&mut self, name: &str) -> BasicBlockId {
        if let Some(&bb) = self.label_map.get(name) {
            bb
        } else {
            let bb = self.alloc_bb();
            self.label_map.insert(name.to_string(), bb);
            // Set label name on the block
            let block = self.get_or_create_bb(bb);
            block.label = Some(name.to_string());
            bb
        }
    }

    // ========================================================================
    // Expression linearization
    // ========================================================================

    /// Get the type of an expression.
    /// PANICS if expression has no type - type evaluation pass must run first.
    /// Following sparse's design: the IR should ALWAYS receive fully typed input.
    fn expr_type(&self, expr: &Expr) -> TypeId {
        expr.typ.expect(
            "BUG: expression has no type. Type evaluation pass must run before linearization.",
        )
    }

    /// Linearize an expression as an lvalue (get its address)
    fn linearize_lvalue(&mut self, expr: &Expr) -> PseudoId {
        match &expr.kind {
            ExprKind::Ident { name, .. } => {
                let name_str = self.str(*name).to_string();
                // For local variables, emit SymAddr to get the stack address
                if let Some(local) = self.locals.get(&name_str).cloned() {
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
                        }
                    }
                    let result = self.alloc_pseudo();
                    self.emit(Instruction::sym_addr(
                        result,
                        local.sym,
                        self.types.pointer_to(local.typ),
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
                let struct_type = self.expr_type(inner);
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
                    let offset_val = self.emit_const(member_info.offset as i64, self.types.long_id);
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
                let struct_type = self
                    .types
                    .base_type(ptr_type)
                    .unwrap_or_else(|| self.expr_type(expr));
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
                    let offset_val = self.emit_const(member_info.offset as i64, self.types.long_id);
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
                let elem_size_val = self.emit_const(elem_size as i64, self.types.long_id);

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
                // This is used for &(struct S){...}
                let sym_id = self.alloc_pseudo();
                let unique_name = format!(".compound_literal#{}", sym_id.0);
                let sym = Pseudo::sym(sym_id, unique_name.clone());
                if let Some(func) = &mut self.current_func {
                    func.add_pseudo(sym);
                    func.add_local(&unique_name, sym_id, *typ, false, self.current_bb);
                }
                self.linearize_init_list(sym_id, *typ, elements);

                // Return address of the compound literal
                let result = self.alloc_pseudo();
                let pseudo = Pseudo::reg(result, result.0);
                if let Some(func) = &mut self.current_func {
                    func.add_pseudo(pseudo);
                }
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
    fn linearize_cast(&mut self, inner_expr: &Expr, cast_type: TypeId) -> PseudoId {
        let src = self.linearize_expr(inner_expr);
        let src_type = self.expr_type(inner_expr);

        // Emit conversion if needed
        let src_is_float = self.types.is_float(src_type);
        let dst_is_float = self.types.is_float(cast_type);

        if src_is_float && !dst_is_float {
            // Float to integer conversion
            let result = self.alloc_pseudo();
            let pseudo = Pseudo::reg(result, result.0);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(pseudo);
            }
            // FCvtS for signed int, FCvtU for unsigned
            let opcode = if self.types.is_unsigned(cast_type) {
                Opcode::FCvtU
            } else {
                Opcode::FCvtS
            };
            let mut insn = Instruction::new(opcode)
                .with_target(result)
                .with_src(src)
                .with_type(cast_type);
            insn.src_size = self.types.size_bits(src_type);
            self.emit(insn);
            result
        } else if !src_is_float && dst_is_float {
            // Integer to float conversion
            let result = self.alloc_pseudo();
            let pseudo = Pseudo::reg(result, result.0);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(pseudo);
            }
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
            self.emit(insn);
            result
        } else if src_is_float && dst_is_float {
            // Float to float conversion (e.g., float to double)
            let src_size = self.types.size_bits(src_type);
            let dst_size = self.types.size_bits(cast_type);
            if src_size != dst_size {
                let result = self.alloc_pseudo();
                let pseudo = Pseudo::reg(result, result.0);
                if let Some(func) = &mut self.current_func {
                    func.add_pseudo(pseudo);
                }
                let mut insn = Instruction::new(Opcode::FCvtF)
                    .with_target(result)
                    .with_src(src)
                    .with_type(cast_type);
                insn.src_size = src_size;
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
    fn linearize_member(&mut self, expr: &Expr, inner_expr: &Expr, member: StringId) -> PseudoId {
        // Get address of the struct base
        let base = self.linearize_lvalue(inner_expr);
        let struct_type = self.expr_type(inner_expr);

        // Look up member offset and type
        let member_info = self
            .types
            .find_member(struct_type, member)
            .unwrap_or_else(|| MemberInfo {
                offset: 0,
                typ: self.expr_type(expr),
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
                let offset_val = self.emit_const(member_info.offset as i64, self.types.long_id);
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
            let result = self.alloc_pseudo();
            let size = self.types.size_bits(member_info.typ);
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

    /// Linearize a pointer member access expression (e.g., p->member)
    fn linearize_arrow(&mut self, expr: &Expr, inner_expr: &Expr, member: StringId) -> PseudoId {
        // Pointer already contains the struct address
        let ptr = self.linearize_expr(inner_expr);
        let ptr_type = self.expr_type(inner_expr);

        // Dereference pointer to get struct type
        let struct_type = self
            .types
            .base_type(ptr_type)
            .unwrap_or_else(|| self.expr_type(expr));

        // Look up member offset and type
        let member_info = self
            .types
            .find_member(struct_type, member)
            .unwrap_or_else(|| MemberInfo {
                offset: 0,
                typ: self.expr_type(expr),
                bit_offset: None,
                bit_width: None,
                storage_unit_size: None,
            });

        // If member type is an array, return the address (arrays decay to pointers)
        if self.types.kind(member_info.typ) == TypeKind::Array {
            if member_info.offset == 0 {
                ptr
            } else {
                let result = self.alloc_pseudo();
                let offset_val = self.emit_const(member_info.offset as i64, self.types.long_id);
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
        } else if let (Some(bit_offset), Some(bit_width), Some(storage_size)) = (
            member_info.bit_offset,
            member_info.bit_width,
            member_info.storage_unit_size,
        ) {
            // Bitfield read
            self.emit_bitfield_load(
                ptr,
                member_info.offset,
                bit_offset,
                bit_width,
                storage_size,
                member_info.typ,
            )
        } else {
            let result = self.alloc_pseudo();
            let size = self.types.size_bits(member_info.typ);
            self.emit(Instruction::load(
                result,
                ptr,
                member_info.offset as i64,
                member_info.typ,
                size,
            ));
            result
        }
    }

    /// Linearize an array index expression (e.g., arr[i])
    fn linearize_index(&mut self, expr: &Expr, array: &Expr, index: &Expr) -> PseudoId {
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
        let elem_size_val = if let ExprKind::Ident { name } = &ptr_expr.kind {
            let name_str = self.str(*name).to_string();
            if let Some(info) = self.locals.get(&name_str).cloned() {
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
                    let innermost_size_val = self.emit_const(innermost_size, self.types.long_id);
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
                    self.emit_const(elem_size as i64, self.types.long_id)
                }
            } else {
                // Variable not found in locals (global or something else)
                let elem_size = self.types.size_bits(elem_type) / 8;
                self.emit_const(elem_size as i64, self.types.long_id)
            }
        } else {
            // Not indexing an identifier directly (e.g., arr[i][j] where arr[i] is an Index)
            // Use compile-time size
            let elem_size = self.types.size_bits(elem_type) / 8;
            self.emit_const(elem_size as i64, self.types.long_id)
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
        if self.types.kind(elem_type) == TypeKind::Array {
            addr
        } else {
            let result = self.alloc_pseudo();
            let size = self.types.size_bits(elem_type);
            self.emit(Instruction::load(result, addr, 0, elem_type, size));
            result
        }
    }

    /// Linearize a function call expression
    fn linearize_call(&mut self, expr: &Expr, func_expr: &Expr, args: &[Expr]) -> PseudoId {
        // Get function name
        let func_name = match &func_expr.kind {
            ExprKind::Ident { name, .. } => self.str(*name).to_string(),
            _ => "<indirect>".to_string(),
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
                // Internal sret storage is never volatile
                func.add_local(
                    format!("__sret_{}", sret_sym.0),
                    sret_sym,
                    typ,
                    false,
                    self.current_bb,
                );
            }

            // Get address of the allocated space
            let sret_addr = self.alloc_pseudo();
            let addr_pseudo = Pseudo::reg(sret_addr, sret_addr.0);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(addr_pseudo);
            }
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
                func.add_local(&unique_name, local_sym, typ, false, self.current_bb);
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
                func.add_local(&unique_name, local_sym, typ, false, self.current_bb);
            }
            (local_sym, Vec::new(), Vec::new())
        } else {
            let result = self.alloc_pseudo();
            (result, Vec::new(), Vec::new())
        };

        // Linearize regular arguments
        // For large structs, pass by reference (address) instead of by value
        // Note: We pass structs > 64 bits by reference. While the ABI allows
        // two-register passing for 9-16 byte structs, we don't implement that yet.
        // For complex types, pass address so codegen can load real/imag into XMM registers
        // For arrays (including VLAs), decay to pointer
        for a in args.iter() {
            let arg_type = self.expr_type(a);
            let arg_kind = self.types.kind(arg_type);
            let arg_val = if (arg_kind == TypeKind::Struct || arg_kind == TypeKind::Union)
                && self.types.size_bits(arg_type) > 64
            {
                // Large struct: pass address instead of value
                // The argument type becomes a pointer
                arg_types_vec.push(self.types.pointer_to(arg_type));
                self.linearize_lvalue(a)
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
            } else {
                arg_types_vec.push(arg_type);
                self.linearize_expr(a)
            };
            arg_vals.push(arg_val);
        }

        if returns_large_struct {
            // For large struct returns, the return value is the address
            // stored in result_sym (which is a local symbol containing the struct)
            let result = self.alloc_pseudo();
            let result_pseudo = Pseudo::reg(result, result.0);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(result_pseudo);
            }
            let ptr_typ = self.types.pointer_to(typ);
            let mut call_insn = Instruction::call(
                Some(result),
                &func_name,
                arg_vals,
                arg_types_vec,
                ptr_typ,
                64, // pointers are 64-bit
            );
            call_insn.variadic_arg_start = variadic_arg_start;
            call_insn.is_sret_call = true;
            call_insn.is_noreturn_call = is_noreturn_call;
            self.emit(call_insn);
            // Return the symbol (address) where struct is stored
            result_sym
        } else {
            let ret_size = self.types.size_bits(typ);
            let mut call_insn = Instruction::call(
                Some(result_sym),
                &func_name,
                arg_vals,
                arg_types_vec,
                typ,
                ret_size,
            );
            call_insn.variadic_arg_start = variadic_arg_start;
            call_insn.is_noreturn_call = is_noreturn_call;
            call_insn.is_two_reg_return = returns_two_reg_struct;
            self.emit(call_insn);
            result_sym
        }
    }

    /// Linearize a post-increment or post-decrement expression
    fn linearize_postop(&mut self, operand: &Expr, is_inc: bool) -> PseudoId {
        let val = self.linearize_expr(operand);
        let typ = self.expr_type(operand);
        let is_float = self.types.is_float(typ);
        let is_ptr = self.types.kind(typ) == TypeKind::Pointer;

        // For locals, we need to save the old value before updating
        // because the pseudo will be reloaded from stack which gets overwritten
        let is_local = if let ExprKind::Ident { name, .. } = &operand.kind {
            self.locals.contains_key(self.str(*name))
        } else {
            false
        };

        let old_val = if is_local {
            // Copy the old value to a temp
            let temp = self.alloc_pseudo();
            let pseudo = Pseudo::reg(temp, temp.0);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(pseudo);
            }
            self.emit(
                Instruction::new(Opcode::Copy)
                    .with_target(temp)
                    .with_src(val)
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
            self.emit_const(elem_size as i64, self.types.long_id)
        } else if is_float {
            self.emit_fconst(1.0, typ)
        } else {
            self.emit_const(1, typ)
        };
        let result = self.alloc_pseudo();
        let pseudo = Pseudo::reg(result, result.0);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(pseudo);
        }
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
            ExprKind::Ident { name, .. } => {
                let name_str = self.str(*name).to_string();
                if let Some(local) = self.locals.get(&name_str).cloned() {
                    self.emit(Instruction::store(
                        final_result,
                        local.sym,
                        0,
                        typ,
                        store_size,
                    ));
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
            _ => {}
        }

        old_val // Return old value
    }

    /// Linearize a binary expression (arithmetic, comparison, logical operators)
    fn linearize_binary(
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
            let scale = self.emit_const(elem_size as i64, self.types.long_id);
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
            let scale = self.emit_const(elem_size as i64, self.types.long_id);
            let scaled_offset = self.alloc_pseudo();
            // Extend int_val to 64-bit for proper address arithmetic
            let int_val_extended =
                self.emit_convert(int_val, self.types.int_id, self.types.long_id);
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
            let left_addr = self.linearize_lvalue(left);
            let right_addr = self.linearize_lvalue(right);
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
    fn linearize_unary(&mut self, expr: &Expr, op: UnaryOp, operand: &Expr) -> PseudoId {
        // Handle AddrOf specially - we need the lvalue address, not the value
        if op == UnaryOp::AddrOf {
            return self.linearize_lvalue(operand);
        }

        // Handle PreInc/PreDec specially - they need store-back
        if op == UnaryOp::PreInc || op == UnaryOp::PreDec {
            let val = self.linearize_expr(operand);
            let typ = self.expr_type(operand);
            let is_float = self.types.is_float(typ);
            let is_ptr = self.types.kind(typ) == TypeKind::Pointer;

            // Compute new value - for pointers, scale by element size
            let increment = if is_ptr {
                let elem_type = self.types.base_type(typ).unwrap_or(self.types.char_id);
                let elem_size = self.types.size_bits(elem_type) / 8;
                self.emit_const(elem_size as i64, self.types.long_id)
            } else if is_float {
                self.emit_fconst(1.0, typ)
            } else {
                self.emit_const(1, typ)
            };
            let result = self.alloc_pseudo();
            let pseudo = Pseudo::reg(result, result.0);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(pseudo);
            }
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

            // Store back to the variable
            if let ExprKind::Ident { name, .. } = &operand.kind {
                let name_str = self.str(*name).to_string();
                if let Some(local) = self.locals.get(&name_str).cloned() {
                    let store_size = self.types.size_bits(typ);
                    self.emit(Instruction::store(
                        final_result,
                        local.sym,
                        0,
                        typ,
                        store_size,
                    ));
                } else if self.var_map.contains_key(&name_str) {
                    self.var_map.insert(name_str.clone(), final_result);
                } else {
                    // Global variable - emit store
                    let sym_id = self.alloc_pseudo();
                    let pseudo = Pseudo::sym(sym_id, name_str.clone());
                    if let Some(func) = &mut self.current_func {
                        func.add_pseudo(pseudo);
                    }
                    let store_size = self.types.size_bits(typ);
                    self.emit(Instruction::store(final_result, sym_id, 0, typ, store_size));
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
    fn linearize_ident(&mut self, expr: &Expr, name: StringId) -> PseudoId {
        let name_str = self.str(name).to_string();

        // Handle C99 __func__ predefined identifier (6.4.2.2)
        // __func__ behaves as if declared: static const char __func__[] = "function-name";
        if name_str == "__func__" {
            // Add function name as a string literal to the module
            let label = self.module.add_string(self.current_func_name.clone());

            // Create symbol pseudo for the string label
            let sym_id = self.alloc_pseudo();
            let sym_pseudo = Pseudo::sym(sym_id, label);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(sym_pseudo);
            }

            // Create result pseudo for the address
            let result = self.alloc_pseudo();
            let pseudo = Pseudo::reg(result, result.0);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(pseudo);
            }

            // Type: const char* (pointer to char)
            let char_type = self.types.char_id;
            let ptr_type = self.types.pointer_to(char_type);
            self.emit(Instruction::sym_addr(result, sym_id, ptr_type));
            return result;
        }

        // First check if it's an enum constant
        if let Some(value) = self.symbols.get_enum_value(name) {
            self.emit_const(value, self.types.int_id)
        }
        // Check if it's a local variable
        else if let Some(local) = self.locals.get(&name_str).cloned() {
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
                    // Arrays decay to pointers - get address, not value
                    if self.types.kind(typ) == TypeKind::Array {
                        let result = self.alloc_pseudo();
                        let elem_type = self.types.base_type(typ).unwrap_or(self.types.int_id);
                        let ptr_type = self.types.pointer_to(elem_type);
                        self.emit(Instruction::sym_addr(result, sym_id, ptr_type));
                        return result;
                    } else {
                        let result = self.alloc_pseudo();
                        let size = self.types.size_bits(typ);
                        self.emit(Instruction::load(result, sym_id, 0, typ, size));
                        return result;
                    }
                }
            }
            let result = self.alloc_pseudo();
            let pseudo = Pseudo::reg(result, result.0);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(pseudo);
            }
            // Arrays decay to pointers - get address, not value
            if self.types.kind(local.typ) == TypeKind::Array {
                let elem_type = self.types.base_type(local.typ).unwrap_or(self.types.int_id);
                let ptr_type = self.types.pointer_to(elem_type);
                self.emit(Instruction::sym_addr(result, local.sym, ptr_type));
            } else {
                let size = self.types.size_bits(local.typ);
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
            // Arrays decay to pointers - get address, not value
            if self.types.kind(typ) == TypeKind::Array {
                let result = self.alloc_pseudo();
                let elem_type = self.types.base_type(typ).unwrap_or(self.types.int_id);
                let ptr_type = self.types.pointer_to(elem_type);
                self.emit(Instruction::sym_addr(result, sym_id, ptr_type));
                result
            } else {
                let result = self.alloc_pseudo();
                let size = self.types.size_bits(typ);
                self.emit(Instruction::load(result, sym_id, 0, typ, size));
                result
            }
        }
    }

    fn linearize_expr(&mut self, expr: &Expr) -> PseudoId {
        // Set current position for debug info
        self.current_pos = Some(expr.pos);

        match &expr.kind {
            ExprKind::IntLit(val) => {
                let typ = self.expr_type(expr);
                self.emit_const(*val, typ)
            }

            ExprKind::FloatLit(val) => {
                let typ = self.expr_type(expr);
                self.emit_fconst(*val, typ)
            }

            ExprKind::CharLit(c) => {
                let typ = self.expr_type(expr);
                self.emit_const(*c as i64, typ)
            }

            ExprKind::StringLit(s) => {
                // Add string to module and get its label
                let label = self.module.add_string(s.clone());

                // Create a symbol pseudo for the string label
                let sym_id = self.alloc_pseudo();
                let sym_pseudo = Pseudo::sym(sym_id, label.clone());
                if let Some(func) = &mut self.current_func {
                    func.add_pseudo(sym_pseudo);
                }

                // Create result pseudo for the address
                let result = self.alloc_pseudo();
                let typ = self.expr_type(expr);
                let pseudo = Pseudo::reg(result, result.0);
                if let Some(func) = &mut self.current_func {
                    func.add_pseudo(pseudo);
                }

                // Emit SymAddr to load the address of the string
                self.emit(Instruction::sym_addr(result, sym_id, typ));
                result
            }

            ExprKind::Ident { name, .. } => self.linearize_ident(expr, *name),

            ExprKind::Unary { op, operand } => self.linearize_unary(expr, *op, operand),

            ExprKind::Binary { op, left, right } => self.linearize_binary(expr, *op, left, right),

            ExprKind::Assign { op, target, value } => self.emit_assign(*op, target, value),

            ExprKind::PostInc(operand) => self.linearize_postop(operand, true),

            ExprKind::PostDec(operand) => self.linearize_postop(operand, false),

            ExprKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_val = self.linearize_expr(cond);
                let then_val = self.linearize_expr(then_expr);
                let else_val = self.linearize_expr(else_expr);

                let result = self.alloc_pseudo();
                let typ = self.expr_type(expr); // Use evaluated type
                let size = self.types.size_bits(typ);

                self.emit(Instruction::select(
                    result, cond_val, then_val, else_val, typ, size,
                ));
                result
            }

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
                self.emit_const(size as i64, result_typ)
            }

            ExprKind::SizeofExpr(inner_expr) => {
                // Check if this is a VLA variable - need runtime sizeof
                if let ExprKind::Ident { name } = &inner_expr.kind {
                    let name_str = self.str(*name).to_string();
                    if let Some(info) = self.locals.get(&name_str).cloned() {
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
                            let elem_size_const = self.emit_const(elem_size, result_typ);
                            let result = self.alloc_pseudo();
                            let mul_insn = Instruction::new(Opcode::Mul)
                                .with_target(result)
                                .with_src(num_elements)
                                .with_src(elem_size_const)
                                .with_size(64);
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
                self.emit_const(size as i64, result_typ)
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

            ExprKind::CompoundLiteral { typ, elements } => {
                // Compound literals have automatic storage at block scope
                // Create an anonymous local variable, similar to how local variables work

                // Create a symbol pseudo for the compound literal (its address)
                let sym_id = self.alloc_pseudo();
                let unique_name = format!(".compound_literal#{}", sym_id.0);
                let sym = Pseudo::sym(sym_id, unique_name.clone());
                if let Some(func) = &mut self.current_func {
                    func.add_pseudo(sym);
                    // Register as local for proper stack allocation
                    func.add_local(&unique_name, sym_id, *typ, false, self.current_bb);
                }

                // Initialize using existing init list machinery
                self.linearize_init_list(sym_id, *typ, elements);

                // For arrays: return pointer (array-to-pointer decay)
                // For structs/scalars: load and return the value
                let result = self.alloc_pseudo();
                let pseudo = Pseudo::reg(result, result.0);
                if let Some(func) = &mut self.current_func {
                    func.add_pseudo(pseudo);
                }

                if self.types.kind(*typ) == TypeKind::Array {
                    // Array compound literal - decay to pointer to first element
                    let elem_type = self.types.base_type(*typ).unwrap_or(self.types.int_id);
                    let ptr_type = self.types.pointer_to(elem_type);
                    self.emit(Instruction::sym_addr(result, sym_id, ptr_type));
                } else {
                    // Struct/scalar compound literal - load the value
                    let size = self.types.size_bits(*typ);
                    self.emit(Instruction::load(result, sym_id, 0, *typ, size));
                }
                result
            }

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
                    .with_type(self.types.void_id);
                self.emit(insn);
                result
            }

            ExprKind::VaArg { ap, arg_type } => {
                // va_arg(ap, type)
                // Get address of ap (it's an lvalue)
                let ap_addr = self.linearize_lvalue(ap);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::VaArg)
                    .with_target(result)
                    .with_src(ap_addr)
                    .with_type(*arg_type);
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
                    .with_type(self.types.void_id);
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
                    .with_type(self.types.void_id);
                self.emit(insn);
                result
            }

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
        }
    }

    fn emit_const(&mut self, val: i64, typ: TypeId) -> PseudoId {
        let id = self.alloc_pseudo();
        let pseudo = Pseudo::val(id, val);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(pseudo);
        }

        // Emit setval instruction
        let insn = Instruction::new(Opcode::SetVal)
            .with_target(id)
            .with_type_and_size(typ, self.types.size_bits(typ));
        self.emit(insn);

        id
    }

    fn emit_fconst(&mut self, val: f64, typ: TypeId) -> PseudoId {
        let id = self.alloc_pseudo();
        let pseudo = Pseudo::fval(id, val);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(pseudo);
        }

        // Emit setval instruction
        let insn = Instruction::new(Opcode::SetVal)
            .with_target(id)
            .with_type_and_size(typ, self.types.size_bits(typ));
        self.emit(insn);

        id
    }

    /// Emit code to load a bitfield value
    /// Returns the loaded value as a PseudoId
    fn emit_bitfield_load(
        &mut self,
        base: PseudoId,
        byte_offset: usize,
        bit_offset: u32,
        bit_width: u32,
        storage_size: u32,
        typ: TypeId,
    ) -> PseudoId {
        // Determine storage type based on storage unit size
        let storage_type = match storage_size {
            1 => self.types.uchar_id,
            2 => self.types.ushort_id,
            4 => self.types.uint_id,
            8 => self.types.ulong_id,
            _ => self.types.uint_id,
        };
        let storage_bits = storage_size * 8;

        // 1. Load the entire storage unit
        let storage_val = self.alloc_pseudo();
        self.emit(Instruction::load(
            storage_val,
            base,
            byte_offset as i64,
            storage_type,
            storage_bits,
        ));

        // 2. Shift right by bit_offset (using logical shift for unsigned extraction)
        let shifted = if bit_offset > 0 {
            let shift_amount = self.emit_const(bit_offset as i64, self.types.int_id);
            let shifted = self.alloc_pseudo();
            self.emit(Instruction::binop(
                Opcode::Lsr,
                shifted,
                storage_val,
                shift_amount,
                storage_type,
                storage_bits,
            ));
            shifted
        } else {
            storage_val
        };

        // 3. Mask to bit_width bits
        let mask = (1u64 << bit_width) - 1;
        let mask_val = self.emit_const(mask as i64, storage_type);
        let masked = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::And,
            masked,
            shifted,
            mask_val,
            storage_type,
            storage_bits,
        ));

        // 4. Sign extend if this is a signed bitfield
        if !self.types.is_unsigned(typ) && bit_width < storage_bits {
            self.emit_sign_extend_bitfield(masked, bit_width, storage_bits)
        } else {
            masked
        }
    }

    /// Sign-extend a bitfield value from bit_width to target_bits
    fn emit_sign_extend_bitfield(
        &mut self,
        value: PseudoId,
        bit_width: u32,
        target_bits: u32,
    ) -> PseudoId {
        // Sign extend by shifting left then arithmetic shifting right
        let shift_amount = target_bits - bit_width;
        let typ = self.types.int_id;

        let shift_val = self.emit_const(shift_amount as i64, typ);
        let shifted_left = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Shl,
            shifted_left,
            value,
            shift_val,
            typ,
            32,
        ));

        let result = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Asr,
            result,
            shifted_left,
            shift_val,
            typ,
            32,
        ));
        result
    }

    /// Emit code to store a value into a bitfield
    fn emit_bitfield_store(
        &mut self,
        base: PseudoId,
        byte_offset: usize,
        bit_offset: u32,
        bit_width: u32,
        storage_size: u32,
        new_value: PseudoId,
    ) {
        // Determine storage type based on storage unit size
        let storage_type = match storage_size {
            1 => self.types.uchar_id,
            2 => self.types.ushort_id,
            4 => self.types.uint_id,
            8 => self.types.ulong_id,
            _ => self.types.uint_id,
        };
        let storage_bits = storage_size * 8;

        // 1. Load current storage unit value
        let old_val = self.alloc_pseudo();
        self.emit(Instruction::load(
            old_val,
            base,
            byte_offset as i64,
            storage_type,
            storage_bits,
        ));

        // 2. Create mask for the bitfield bits: ~(((1 << width) - 1) << offset)
        let field_mask = ((1u64 << bit_width) - 1) << bit_offset;
        let clear_mask = !field_mask;
        let clear_mask_val = self.emit_const(clear_mask as i64, storage_type);

        // 3. Clear the bitfield bits in old value
        let cleared = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::And,
            cleared,
            old_val,
            clear_mask_val,
            storage_type,
            storage_bits,
        ));

        // 4. Mask new value to bit_width and shift to position
        let value_mask = (1u64 << bit_width) - 1;
        let value_mask_val = self.emit_const(value_mask as i64, storage_type);
        let masked_new = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::And,
            masked_new,
            new_value,
            value_mask_val,
            storage_type,
            storage_bits,
        ));

        let positioned = if bit_offset > 0 {
            let shift_val = self.emit_const(bit_offset as i64, self.types.int_id);
            let positioned = self.alloc_pseudo();
            self.emit(Instruction::binop(
                Opcode::Shl,
                positioned,
                masked_new,
                shift_val,
                storage_type,
                storage_bits,
            ));
            positioned
        } else {
            masked_new
        };

        // 5. OR cleared value with positioned new value
        let combined = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Or,
            combined,
            cleared,
            positioned,
            storage_type,
            storage_bits,
        ));

        // 6. Store back
        self.emit(Instruction::store(
            combined,
            base,
            byte_offset as i64,
            storage_type,
            storage_bits,
        ));
    }

    fn emit_unary(&mut self, op: UnaryOp, src: PseudoId, typ: TypeId) -> PseudoId {
        let result = self.alloc_pseudo();
        let is_float = self.types.is_float(typ);
        let size = self.types.size_bits(typ);

        let opcode = match op {
            UnaryOp::Neg => {
                if is_float {
                    Opcode::FNeg
                } else {
                    Opcode::Neg
                }
            }
            UnaryOp::Not => {
                // Logical not: compare with 0
                if is_float {
                    let zero = self.emit_fconst(0.0, typ);
                    self.emit(Instruction::binop(
                        Opcode::FCmpOEq,
                        result,
                        src,
                        zero,
                        typ,
                        size,
                    ));
                } else {
                    let zero = self.emit_const(0, typ);
                    self.emit(Instruction::binop(
                        Opcode::SetEq,
                        result,
                        src,
                        zero,
                        typ,
                        size,
                    ));
                }
                return result;
            }
            UnaryOp::BitNot => Opcode::Not,
            UnaryOp::AddrOf => {
                return src;
            }
            UnaryOp::Deref => {
                // Dereferencing a pointer-to-array gives an array, which is just an address
                // (arrays decay to their first element's address)
                if self.types.kind(typ) == TypeKind::Array {
                    return src;
                }
                self.emit(Instruction::load(result, src, 0, typ, size));
                return result;
            }
            UnaryOp::PreInc => {
                let is_ptr = self.types.kind(typ) == TypeKind::Pointer;
                let increment = if is_ptr {
                    let elem_type = self.types.base_type(typ).unwrap_or(self.types.char_id);
                    let elem_size = self.types.size_bits(elem_type) / 8;
                    self.emit_const(elem_size as i64, self.types.long_id)
                } else if is_float {
                    self.emit_fconst(1.0, typ)
                } else {
                    self.emit_const(1, typ)
                };
                let opcode = if is_float { Opcode::FAdd } else { Opcode::Add };
                self.emit(Instruction::binop(
                    opcode, result, src, increment, typ, size,
                ));
                return result;
            }
            UnaryOp::PreDec => {
                let is_ptr = self.types.kind(typ) == TypeKind::Pointer;
                let decrement = if is_ptr {
                    let elem_type = self.types.base_type(typ).unwrap_or(self.types.char_id);
                    let elem_size = self.types.size_bits(elem_type) / 8;
                    self.emit_const(elem_size as i64, self.types.long_id)
                } else if is_float {
                    self.emit_fconst(1.0, typ)
                } else {
                    self.emit_const(1, typ)
                };
                let opcode = if is_float { Opcode::FSub } else { Opcode::Sub };
                self.emit(Instruction::binop(
                    opcode, result, src, decrement, typ, size,
                ));
                return result;
            }
        };

        self.emit(Instruction::unop(opcode, result, src, typ, size));
        result
    }

    fn emit_binary(
        &mut self,
        op: BinaryOp,
        left: PseudoId,
        right: PseudoId,
        result_typ: TypeId,
        operand_typ: TypeId,
    ) -> PseudoId {
        let result = self.alloc_pseudo();

        let is_float = self.types.is_float(operand_typ);
        let is_unsigned = self.types.is_unsigned(operand_typ);

        let opcode = match op {
            BinaryOp::Add => {
                if is_float {
                    Opcode::FAdd
                } else {
                    Opcode::Add
                }
            }
            BinaryOp::Sub => {
                if is_float {
                    Opcode::FSub
                } else {
                    Opcode::Sub
                }
            }
            BinaryOp::Mul => {
                if is_float {
                    Opcode::FMul
                } else {
                    Opcode::Mul
                }
            }
            BinaryOp::Div => {
                if is_float {
                    Opcode::FDiv
                } else if is_unsigned {
                    Opcode::DivU
                } else {
                    Opcode::DivS
                }
            }
            BinaryOp::Mod => {
                // Modulo is not supported for floats in hardware - use fmod() library call
                // For now, use integer modulo (semantic analysis should catch float % float)
                if is_unsigned {
                    Opcode::ModU
                } else {
                    Opcode::ModS
                }
            }
            BinaryOp::Lt => {
                if is_float {
                    Opcode::FCmpOLt
                } else if is_unsigned {
                    Opcode::SetB
                } else {
                    Opcode::SetLt
                }
            }
            BinaryOp::Gt => {
                if is_float {
                    Opcode::FCmpOGt
                } else if is_unsigned {
                    Opcode::SetA
                } else {
                    Opcode::SetGt
                }
            }
            BinaryOp::Le => {
                if is_float {
                    Opcode::FCmpOLe
                } else if is_unsigned {
                    Opcode::SetBe
                } else {
                    Opcode::SetLe
                }
            }
            BinaryOp::Ge => {
                if is_float {
                    Opcode::FCmpOGe
                } else if is_unsigned {
                    Opcode::SetAe
                } else {
                    Opcode::SetGe
                }
            }
            BinaryOp::Eq => {
                if is_float {
                    Opcode::FCmpOEq
                } else {
                    Opcode::SetEq
                }
            }
            BinaryOp::Ne => {
                if is_float {
                    Opcode::FCmpONe
                } else {
                    Opcode::SetNe
                }
            }
            // LogAnd and LogOr are handled earlier in linearize_expr via
            // emit_logical_and/emit_logical_or for proper short-circuit evaluation
            BinaryOp::LogAnd | BinaryOp::LogOr => {
                unreachable!("LogAnd/LogOr should be handled in ExprKind::Binary")
            }
            BinaryOp::BitAnd => Opcode::And,
            BinaryOp::BitOr => Opcode::Or,
            BinaryOp::BitXor => Opcode::Xor,
            BinaryOp::Shl => Opcode::Shl,
            BinaryOp::Shr => {
                // Logical shift for unsigned, arithmetic for signed
                if is_unsigned {
                    Opcode::Lsr
                } else {
                    Opcode::Asr
                }
            }
        };

        // For comparison operations, use operand_typ to ensure correct size
        // (comparisons produce int result but must operate at operand size)
        let insn_typ = match opcode {
            Opcode::SetEq
            | Opcode::SetNe
            | Opcode::SetLt
            | Opcode::SetLe
            | Opcode::SetGt
            | Opcode::SetGe
            | Opcode::SetB
            | Opcode::SetBe
            | Opcode::SetA
            | Opcode::SetAe
            | Opcode::FCmpOEq
            | Opcode::FCmpONe
            | Opcode::FCmpOLt
            | Opcode::FCmpOLe
            | Opcode::FCmpOGt
            | Opcode::FCmpOGe => operand_typ,
            _ => result_typ,
        };
        let insn_size = self.types.size_bits(insn_typ);
        self.emit(Instruction::binop(
            opcode, result, left, right, insn_typ, insn_size,
        ));
        result
    }

    /// Emit complex arithmetic operation
    /// Complex values are stored as two adjacent float/double values (real, imag)
    /// This function expands complex ops to operations on the component parts
    fn emit_complex_binary(
        &mut self,
        op: BinaryOp,
        left_addr: PseudoId,
        right_addr: PseudoId,
        complex_typ: TypeId,
    ) -> PseudoId {
        // Get the base float type (float, double, or long double)
        let base_typ = self.types.complex_base(complex_typ);
        let base_size = self.types.size_bits(base_typ);
        let base_bytes = (base_size / 8) as i64;

        // Allocate result temporary (stack space for the complex result)
        let result_addr = self.alloc_local_temp(complex_typ);

        // Load real and imaginary parts of left operand
        let left_real = self.alloc_pseudo();
        self.emit(Instruction::load(
            left_real, left_addr, 0, base_typ, base_size,
        ));

        let left_imag = self.alloc_pseudo();
        self.emit(Instruction::load(
            left_imag, left_addr, base_bytes, base_typ, base_size,
        ));

        // Load real and imaginary parts of right operand
        let right_real = self.alloc_pseudo();
        self.emit(Instruction::load(
            right_real, right_addr, 0, base_typ, base_size,
        ));

        let right_imag = self.alloc_pseudo();
        self.emit(Instruction::load(
            right_imag, right_addr, base_bytes, base_typ, base_size,
        ));

        // Perform the operation on the components
        let (result_real, result_imag) = match op {
            BinaryOp::Add => {
                // (a + bi) + (c + di) = (a+c) + (b+d)i
                let real = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FAdd,
                    real,
                    left_real,
                    right_real,
                    base_typ,
                    base_size,
                ));
                let imag = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FAdd,
                    imag,
                    left_imag,
                    right_imag,
                    base_typ,
                    base_size,
                ));
                (real, imag)
            }
            BinaryOp::Sub => {
                // (a + bi) - (c + di) = (a-c) + (b-d)i
                let real = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FSub,
                    real,
                    left_real,
                    right_real,
                    base_typ,
                    base_size,
                ));
                let imag = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FSub,
                    imag,
                    left_imag,
                    right_imag,
                    base_typ,
                    base_size,
                ));
                (real, imag)
            }
            BinaryOp::Mul => {
                // (a + bi) * (c + di) = (ac - bd) + (ad + bc)i
                let ac = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FMul,
                    ac,
                    left_real,
                    right_real,
                    base_typ,
                    base_size,
                ));
                let bd = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FMul,
                    bd,
                    left_imag,
                    right_imag,
                    base_typ,
                    base_size,
                ));
                let ad = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FMul,
                    ad,
                    left_real,
                    right_imag,
                    base_typ,
                    base_size,
                ));
                let bc = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FMul,
                    bc,
                    left_imag,
                    right_real,
                    base_typ,
                    base_size,
                ));
                let real = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FSub,
                    real,
                    ac,
                    bd,
                    base_typ,
                    base_size,
                ));
                let imag = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FAdd,
                    imag,
                    ad,
                    bc,
                    base_typ,
                    base_size,
                ));
                (real, imag)
            }
            BinaryOp::Div => {
                // (a + bi) / (c + di) = ((ac + bd) + (bc - ad)i) / (c^2 + d^2)
                // Naive formula without overflow handling
                let cc = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FMul,
                    cc,
                    right_real,
                    right_real,
                    base_typ,
                    base_size,
                ));
                let dd = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FMul,
                    dd,
                    right_imag,
                    right_imag,
                    base_typ,
                    base_size,
                ));
                let denom = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FAdd,
                    denom,
                    cc,
                    dd,
                    base_typ,
                    base_size,
                ));

                let ac = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FMul,
                    ac,
                    left_real,
                    right_real,
                    base_typ,
                    base_size,
                ));
                let bd = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FMul,
                    bd,
                    left_imag,
                    right_imag,
                    base_typ,
                    base_size,
                ));
                let bc = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FMul,
                    bc,
                    left_imag,
                    right_real,
                    base_typ,
                    base_size,
                ));
                let ad = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FMul,
                    ad,
                    left_real,
                    right_imag,
                    base_typ,
                    base_size,
                ));

                let real_num = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FAdd,
                    real_num,
                    ac,
                    bd,
                    base_typ,
                    base_size,
                ));
                let imag_num = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FSub,
                    imag_num,
                    bc,
                    ad,
                    base_typ,
                    base_size,
                ));

                let real = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FDiv,
                    real,
                    real_num,
                    denom,
                    base_typ,
                    base_size,
                ));
                let imag = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::FDiv,
                    imag,
                    imag_num,
                    denom,
                    base_typ,
                    base_size,
                ));
                (real, imag)
            }
            _ => {
                // Other operations not supported for complex types
                error(
                    Position::default(),
                    &format!("unsupported operation {:?} on complex types", op),
                );
                return result_addr;
            }
        };

        // Store result components
        self.emit(Instruction::store(
            result_real,
            result_addr,
            0,
            base_typ,
            base_size,
        ));
        self.emit(Instruction::store(
            result_imag,
            result_addr,
            base_bytes,
            base_typ,
            base_size,
        ));

        result_addr
    }

    /// Allocate a local temporary variable for a complex result
    fn alloc_local_temp(&mut self, typ: TypeId) -> PseudoId {
        let size = self.types.size_bytes(typ);
        let size_const = self.emit_const(size as i64, self.types.ulong_id);
        let addr = self.alloc_pseudo();
        let alloca_insn = Instruction::new(Opcode::Alloca)
            .with_target(addr)
            .with_src(size_const)
            .with_type_and_size(self.types.void_ptr_id, 64);
        self.emit(alloca_insn);
        addr
    }

    fn emit_compare_zero(&mut self, val: PseudoId, operand_typ: TypeId) -> PseudoId {
        let result = self.alloc_pseudo();
        let zero = self.emit_const(0, operand_typ);
        let size = self.types.size_bits(operand_typ);
        self.emit(Instruction::binop(
            Opcode::SetNe,
            result,
            val,
            zero,
            operand_typ,
            size,
        ));
        result
    }

    /// Emit short-circuit logical AND: a && b
    /// If a is false, skip evaluation of b and return 0.
    /// Otherwise, evaluate b and return (b != 0).
    fn emit_logical_and(&mut self, left: &Expr, right: &Expr) -> PseudoId {
        let result_typ = self.types.int_id;

        // Create basic blocks
        let eval_b_bb = self.alloc_bb();
        let merge_bb = self.alloc_bb();

        // Evaluate LHS
        let left_typ = self.expr_type(left);
        let left_val = self.linearize_expr(left);
        let left_bool = self.emit_compare_zero(left_val, left_typ);

        // Emit the short-circuit value (0) BEFORE the branch, while still in LHS block
        // This value will be used if we short-circuit (LHS is false)
        let zero = self.emit_const(0, result_typ);

        // Get the block where LHS evaluation ended (may differ from initial block
        // if LHS contains nested control flow)
        let lhs_end_bb = self.current_bb.unwrap();

        // Branch: if LHS is false, go to merge (result = 0); else evaluate RHS
        self.emit(Instruction::cbr(left_bool, eval_b_bb, merge_bb));
        self.link_bb(lhs_end_bb, eval_b_bb);
        self.link_bb(lhs_end_bb, merge_bb);

        // eval_b_bb: Evaluate RHS
        self.switch_bb(eval_b_bb);
        let right_typ = self.expr_type(right);
        let right_val = self.linearize_expr(right);
        let right_bool = self.emit_compare_zero(right_val, right_typ);

        // Get the actual block where RHS evaluation ended (may differ from eval_b_bb
        // if RHS contains nested control flow like another &&/||)
        let rhs_end_bb = self.current_bb.unwrap();

        // Branch to merge
        self.emit(Instruction::br(merge_bb));
        self.link_bb(rhs_end_bb, merge_bb);

        // merge_bb: Create phi node to merge results
        self.switch_bb(merge_bb);

        // Result is 0 if we came from lhs_end_bb (LHS was false),
        // or right_bool if we came from rhs_end_bb (LHS was true)
        let result = self.alloc_pseudo();
        let mut phi_insn = Instruction::phi(result, result_typ, 32);
        phi_insn.phi_list.push((lhs_end_bb, zero));
        phi_insn.phi_list.push((rhs_end_bb, right_bool));
        self.emit(phi_insn);

        result
    }

    /// Emit short-circuit logical OR: a || b
    /// If a is true, skip evaluation of b and return 1.
    /// Otherwise, evaluate b and return (b != 0).
    fn emit_logical_or(&mut self, left: &Expr, right: &Expr) -> PseudoId {
        let result_typ = self.types.int_id;

        // Create basic blocks
        let eval_b_bb = self.alloc_bb();
        let merge_bb = self.alloc_bb();

        // Evaluate LHS
        let left_typ = self.expr_type(left);
        let left_val = self.linearize_expr(left);
        let left_bool = self.emit_compare_zero(left_val, left_typ);

        // Emit the short-circuit value (1) BEFORE the branch, while still in LHS block
        // This value will be used if we short-circuit (LHS is true)
        let one = self.emit_const(1, result_typ);

        // Get the block where LHS evaluation ended (may differ from initial block
        // if LHS contains nested control flow)
        let lhs_end_bb = self.current_bb.unwrap();

        // Branch: if LHS is true, go to merge (result = 1); else evaluate RHS
        self.emit(Instruction::cbr(left_bool, merge_bb, eval_b_bb));
        self.link_bb(lhs_end_bb, merge_bb);
        self.link_bb(lhs_end_bb, eval_b_bb);

        // eval_b_bb: Evaluate RHS
        self.switch_bb(eval_b_bb);
        let right_typ = self.expr_type(right);
        let right_val = self.linearize_expr(right);
        let right_bool = self.emit_compare_zero(right_val, right_typ);

        // Get the actual block where RHS evaluation ended (may differ from eval_b_bb
        // if RHS contains nested control flow like another &&/||)
        let rhs_end_bb = self.current_bb.unwrap();

        // Branch to merge
        self.emit(Instruction::br(merge_bb));
        self.link_bb(rhs_end_bb, merge_bb);

        // merge_bb: Create phi node to merge results
        self.switch_bb(merge_bb);

        // Result is 1 if we came from lhs_end_bb (LHS was true),
        // or right_bool if we came from rhs_end_bb (LHS was false)
        let result = self.alloc_pseudo();
        let mut phi_insn = Instruction::phi(result, result_typ, 32);
        phi_insn.phi_list.push((lhs_end_bb, one));
        phi_insn.phi_list.push((rhs_end_bb, right_bool));
        self.emit(phi_insn);

        result
    }

    fn emit_assign(&mut self, op: AssignOp, target: &Expr, value: &Expr) -> PseudoId {
        let target_typ = self.expr_type(target);
        let value_typ = self.expr_type(value);

        // For complex type assignment, handle specially - copy real and imag parts
        if self.types.is_complex(target_typ) && op == AssignOp::Assign {
            let target_addr = self.linearize_lvalue(target);
            let value_addr = self.linearize_lvalue(value);
            let base_typ = self.types.complex_base(target_typ);
            let base_size = self.types.size_bits(base_typ);
            let base_bytes = (base_size / 8) as i64;

            // Load and store real part
            let real = self.alloc_pseudo();
            self.emit(Instruction::load(real, value_addr, 0, base_typ, base_size));
            self.emit(Instruction::store(
                real,
                target_addr,
                0,
                base_typ,
                base_size,
            ));

            // Load and store imaginary part
            let imag = self.alloc_pseudo();
            self.emit(Instruction::load(
                imag, value_addr, base_bytes, base_typ, base_size,
            ));
            self.emit(Instruction::store(
                imag,
                target_addr,
                base_bytes,
                base_typ,
                base_size,
            ));

            return real; // Return real part as the result value
        }

        let rhs = self.linearize_expr(value);

        // Check for pointer compound assignment (p += n or p -= n)
        let is_ptr_arith = self.types.kind(target_typ) == TypeKind::Pointer
            && self.types.is_integer(value_typ)
            && (op == AssignOp::AddAssign || op == AssignOp::SubAssign);

        // Convert RHS to target type if needed (but not for pointer arithmetic)
        let rhs = if is_ptr_arith {
            // For pointer arithmetic, scale the integer by element size
            let elem_type = self
                .types
                .base_type(target_typ)
                .unwrap_or(self.types.char_id);
            let elem_size = self.types.size_bits(elem_type) / 8;
            let scale = self.emit_const(elem_size as i64, self.types.long_id);

            // Extend the integer to 64-bit for proper arithmetic
            let rhs_extended = self.emit_convert(rhs, value_typ, self.types.long_id);

            let scaled = self.alloc_pseudo();
            let pseudo = Pseudo::reg(scaled, scaled.0);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(pseudo);
            }
            self.emit(Instruction::binop(
                Opcode::Mul,
                scaled,
                rhs_extended,
                scale,
                self.types.long_id,
                64,
            ));
            scaled
        } else {
            self.emit_convert(rhs, value_typ, target_typ)
        };

        let final_val = match op {
            AssignOp::Assign => rhs,
            _ => {
                // Compound assignment - get current value and apply operation
                let lhs = self.linearize_expr(target);
                let result = self.alloc_pseudo();
                let pseudo = Pseudo::reg(result, result.0);
                if let Some(func) = &mut self.current_func {
                    func.add_pseudo(pseudo);
                }

                let is_float = self.types.is_float(target_typ);
                let is_unsigned = self.types.is_unsigned(target_typ);
                let opcode = match op {
                    AssignOp::AddAssign => {
                        if is_float {
                            Opcode::FAdd
                        } else {
                            Opcode::Add
                        }
                    }
                    AssignOp::SubAssign => {
                        if is_float {
                            Opcode::FSub
                        } else {
                            Opcode::Sub
                        }
                    }
                    AssignOp::MulAssign => {
                        if is_float {
                            Opcode::FMul
                        } else {
                            Opcode::Mul
                        }
                    }
                    AssignOp::DivAssign => {
                        if is_float {
                            Opcode::FDiv
                        } else if is_unsigned {
                            Opcode::DivU
                        } else {
                            Opcode::DivS
                        }
                    }
                    AssignOp::ModAssign => {
                        // Modulo not supported for floats
                        if is_unsigned {
                            Opcode::ModU
                        } else {
                            Opcode::ModS
                        }
                    }
                    AssignOp::AndAssign => Opcode::And,
                    AssignOp::OrAssign => Opcode::Or,
                    AssignOp::XorAssign => Opcode::Xor,
                    AssignOp::ShlAssign => Opcode::Shl,
                    AssignOp::ShrAssign => {
                        if is_unsigned {
                            Opcode::Lsr
                        } else {
                            Opcode::Asr
                        }
                    }
                    AssignOp::Assign => unreachable!(),
                };

                // For pointer arithmetic, use Long type for the operation
                let arith_type = if is_ptr_arith {
                    self.types.long_id
                } else {
                    target_typ
                };
                let arith_size = self.types.size_bits(arith_type);
                self.emit(Instruction::binop(
                    opcode, result, lhs, rhs, arith_type, arith_size,
                ));
                result
            }
        };

        // Store based on target expression type
        let target_size = self.types.size_bits(target_typ);
        match &target.kind {
            ExprKind::Ident { name, .. } => {
                let name_str = self.str(*name).to_string();
                if let Some(local) = self.locals.get(&name_str).cloned() {
                    // Check if this is a static local (sentinel value)
                    if local.sym.0 == u32::MAX {
                        // Static local - look up the global name and emit store to global
                        let key = format!("{}.{}", self.current_func_name, &name_str);
                        if let Some(static_info) = self.static_locals.get(&key).cloned() {
                            let sym_id = self.alloc_pseudo();
                            let pseudo = Pseudo::sym(sym_id, static_info.global_name);
                            if let Some(func) = &mut self.current_func {
                                func.add_pseudo(pseudo);
                            }
                            self.emit(Instruction::store(
                                final_val,
                                sym_id,
                                0,
                                target_typ,
                                target_size,
                            ));
                        }
                    } else {
                        // Regular local variable: emit Store
                        self.emit(Instruction::store(
                            final_val,
                            local.sym,
                            0,
                            target_typ,
                            target_size,
                        ));
                    }
                } else if self.var_map.contains_key(&name_str) {
                    // Parameter: this is not SSA-correct but parameters
                    // shouldn't be reassigned. If they are, we'd need to
                    // demote them to locals. For now, just update the mapping.
                    self.var_map.insert(name_str.clone(), final_val);
                } else {
                    // Global variable - emit store
                    let sym_id = self.alloc_pseudo();
                    let pseudo = Pseudo::sym(sym_id, name_str);
                    if let Some(func) = &mut self.current_func {
                        func.add_pseudo(pseudo);
                    }
                    self.emit(Instruction::store(
                        final_val,
                        sym_id,
                        0,
                        target_typ,
                        target_size,
                    ));
                }
            }
            ExprKind::Member { expr, member } => {
                // Struct member: get address and store with offset
                let base = self.linearize_lvalue(expr);
                let struct_type = self.expr_type(expr);
                let member_info =
                    self.types
                        .find_member(struct_type, *member)
                        .unwrap_or(MemberInfo {
                            offset: 0,
                            typ: target_typ,
                            bit_offset: None,
                            bit_width: None,
                            storage_unit_size: None,
                        });
                if let (Some(bit_offset), Some(bit_width), Some(storage_size)) = (
                    member_info.bit_offset,
                    member_info.bit_width,
                    member_info.storage_unit_size,
                ) {
                    // Bitfield store
                    self.emit_bitfield_store(
                        base,
                        member_info.offset,
                        bit_offset,
                        bit_width,
                        storage_size,
                        final_val,
                    );
                } else {
                    let member_size = self.types.size_bits(member_info.typ);
                    self.emit(Instruction::store(
                        final_val,
                        base,
                        member_info.offset as i64,
                        member_info.typ,
                        member_size,
                    ));
                }
            }
            ExprKind::Arrow { expr, member } => {
                // Pointer member: pointer value is the base address
                let ptr = self.linearize_expr(expr);
                let ptr_type = self.expr_type(expr);
                let struct_type = self.types.base_type(ptr_type).unwrap_or(target_typ);
                let member_info =
                    self.types
                        .find_member(struct_type, *member)
                        .unwrap_or(MemberInfo {
                            offset: 0,
                            typ: target_typ,
                            bit_offset: None,
                            bit_width: None,
                            storage_unit_size: None,
                        });
                if let (Some(bit_offset), Some(bit_width), Some(storage_size)) = (
                    member_info.bit_offset,
                    member_info.bit_width,
                    member_info.storage_unit_size,
                ) {
                    // Bitfield store
                    self.emit_bitfield_store(
                        ptr,
                        member_info.offset,
                        bit_offset,
                        bit_width,
                        storage_size,
                        final_val,
                    );
                } else {
                    let member_size = self.types.size_bits(member_info.typ);
                    self.emit(Instruction::store(
                        final_val,
                        ptr,
                        member_info.offset as i64,
                        member_info.typ,
                        member_size,
                    ));
                }
            }
            ExprKind::Unary {
                op: UnaryOp::Deref,
                operand,
            } => {
                // Dereference: store to the pointer address
                let ptr = self.linearize_expr(operand);
                self.emit(Instruction::store(
                    final_val,
                    ptr,
                    0,
                    target_typ,
                    target_size,
                ));
            }
            ExprKind::Index { array, index } => {
                // Array subscript: compute address and store
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
                let elem_size = target_size / 8;
                let elem_size_val = self.emit_const(elem_size as i64, self.types.long_id);

                // Sign-extend index to 64-bit for proper pointer arithmetic (negative indices)
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

                let addr = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::Add,
                    addr,
                    arr,
                    offset,
                    ptr_typ,
                    64,
                ));

                self.emit(Instruction::store(
                    final_val,
                    addr,
                    0,
                    target_typ,
                    target_size,
                ));
            }
            _ => {
                // Other lvalues - should not happen for valid C code
            }
        }

        final_val
    }
}

// ============================================================================
// Public API
// ============================================================================

/// Linearize an AST to IR (convenience wrapper for tests)
#[cfg(test)]
pub fn linearize(
    tu: &TranslationUnit,
    symbols: &SymbolTable,
    types: &TypeTable,
    strings: &StringTable,
    target: &Target,
) -> Module {
    linearize_with_debug(tu, symbols, types, strings, target, false, None)
}

/// Linearize an AST to IR with debug info support
pub fn linearize_with_debug(
    tu: &TranslationUnit,
    symbols: &SymbolTable,
    types: &TypeTable,
    strings: &StringTable,
    target: &Target,
    debug: bool,
    source_file: Option<&str>,
) -> Module {
    let mut linearizer = Linearizer::new(symbols, types, strings, target);
    let mut module = linearizer.linearize(tu);
    module.debug = debug;
    if let Some(path) = source_file {
        module.source_files.push(path.to_string());
    }
    module
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::ast::{ExternalDecl, FunctionDef, Parameter};
    use crate::strings::StringTable;

    /// Create a default position for test code
    fn test_pos() -> Position {
        Position {
            stream: 0,
            line: 1,
            col: 1,
            newline: false,
            whitespace: false,
            noexpand: false,
        }
    }

    fn test_linearize(tu: &TranslationUnit, types: &TypeTable, strings: &StringTable) -> Module {
        let symbols = SymbolTable::new();
        let target = Target::host();
        linearize(tu, &symbols, types, strings, &target)
    }

    fn make_simple_func(name: StringId, body: Stmt, types: &TypeTable) -> FunctionDef {
        FunctionDef {
            return_type: types.int_id,
            name,
            params: vec![],
            body,
            pos: test_pos(),
        }
    }

    #[test]
    fn test_linearize_empty_function() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let test_id = strings.intern("test");
        let func = make_simple_func(test_id, Stmt::Block(vec![]), &types);
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu, &types, &strings);
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].name, "test");
        assert!(!module.functions[0].blocks.is_empty());
    }

    #[test]
    fn test_linearize_return() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let test_id = strings.intern("test");
        let func = make_simple_func(test_id, Stmt::Return(Some(Expr::int(42, &types))), &types);
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu, &types, &strings);
        let ir = format!("{}", module);
        assert!(ir.contains("ret"));
    }

    #[test]
    fn test_linearize_if() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let test_id = strings.intern("test");
        let func = make_simple_func(
            test_id,
            Stmt::If {
                cond: Expr::int(1, &types),
                then_stmt: Box::new(Stmt::Return(Some(Expr::int(1, &types)))),
                else_stmt: Some(Box::new(Stmt::Return(Some(Expr::int(0, &types))))),
            },
            &types,
        );
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu, &types, &strings);
        let ir = format!("{}", module);
        assert!(ir.contains("cbr")); // Conditional branch
    }

    #[test]
    fn test_linearize_while() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let test_id = strings.intern("test");
        let func = make_simple_func(
            test_id,
            Stmt::While {
                cond: Expr::int(1, &types),
                body: Box::new(Stmt::Break),
            },
            &types,
        );
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu, &types, &strings);
        assert!(module.functions[0].blocks.len() >= 3); // cond, body, exit
    }

    #[test]
    fn test_linearize_for() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let test_id = strings.intern("test");
        let i_id = strings.intern("i");
        // for (int i = 0; i < 10; i++) { }
        let int_type = types.int_id;
        let i_var = Expr::var_typed(i_id, int_type);
        let func = make_simple_func(
            test_id,
            Stmt::For {
                init: Some(ForInit::Declaration(Declaration {
                    declarators: vec![crate::parse::ast::InitDeclarator {
                        name: i_id,
                        typ: int_type,
                        init: Some(Expr::int(0, &types)),
                        vla_sizes: vec![],
                    }],
                })),
                cond: Some(Expr::binary(
                    BinaryOp::Lt,
                    i_var.clone(),
                    Expr::int(10, &types),
                    &types,
                )),
                post: Some(Expr::typed(
                    ExprKind::PostInc(Box::new(i_var)),
                    int_type,
                    test_pos(),
                )),
                body: Box::new(Stmt::Empty),
            },
            &types,
        );
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu, &types, &strings);
        assert!(module.functions[0].blocks.len() >= 4); // entry, cond, body, post, exit
    }

    #[test]
    fn test_linearize_binary_expr() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let test_id = strings.intern("test");
        // return 1 + 2 * 3;
        let func = make_simple_func(
            test_id,
            Stmt::Return(Some(Expr::binary(
                BinaryOp::Add,
                Expr::int(1, &types),
                Expr::binary(
                    BinaryOp::Mul,
                    Expr::int(2, &types),
                    Expr::int(3, &types),
                    &types,
                ),
                &types,
            ))),
            &types,
        );
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu, &types, &strings);
        let ir = format!("{}", module);
        assert!(ir.contains("mul"));
        assert!(ir.contains("add"));
    }

    #[test]
    fn test_linearize_function_with_params() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let add_id = strings.intern("add");
        let a_id = strings.intern("a");
        let b_id = strings.intern("b");
        let int_type = types.int_id;
        let func = FunctionDef {
            return_type: int_type,
            name: add_id,
            params: vec![
                Parameter {
                    name: Some(a_id),
                    typ: int_type,
                },
                Parameter {
                    name: Some(b_id),
                    typ: int_type,
                },
            ],
            body: Stmt::Return(Some(Expr::binary(
                BinaryOp::Add,
                Expr::var_typed(a_id, int_type),
                Expr::var_typed(b_id, int_type),
                &types,
            ))),
            pos: test_pos(),
        };
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu, &types, &strings);
        let ir = format!("{}", module);
        assert!(ir.contains("add"));
        assert!(ir.contains("%a"));
        assert!(ir.contains("%b"));
    }

    #[test]
    fn test_linearize_call() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let test_id = strings.intern("test");
        let foo_id = strings.intern("foo");
        let func = make_simple_func(
            test_id,
            Stmt::Return(Some(Expr::call(
                Expr::var(foo_id),
                vec![Expr::int(1, &types), Expr::int(2, &types)],
                &types,
            ))),
            &types,
        );
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu, &types, &strings);
        let ir = format!("{}", module);
        assert!(ir.contains("call"));
        assert!(ir.contains("foo"));
    }

    #[test]
    fn test_linearize_comparison() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let test_id = strings.intern("test");
        let func = make_simple_func(
            test_id,
            Stmt::Return(Some(Expr::binary(
                BinaryOp::Lt,
                Expr::int(1, &types),
                Expr::int(2, &types),
                &types,
            ))),
            &types,
        );
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu, &types, &strings);
        let ir = format!("{}", module);
        assert!(ir.contains("setlt"));
    }

    #[test]
    fn test_linearize_unsigned_comparison() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let test_id = strings.intern("test");

        // Create unsigned comparison: (unsigned)1 < (unsigned)2
        let uint_type = types.uint_id;
        let mut left = Expr::int(1, &types);
        left.typ = Some(uint_type);
        let mut right = Expr::int(2, &types);
        right.typ = Some(uint_type);
        let mut cmp = Expr::binary(BinaryOp::Lt, left, right, &types);
        cmp.typ = Some(types.int_id);

        let func = make_simple_func(test_id, Stmt::Return(Some(cmp)), &types);
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu, &types, &strings);
        let ir = format!("{}", module);
        // Should use unsigned comparison opcode (setb = set if below)
        assert!(
            ir.contains("setb"),
            "Expected 'setb' for unsigned comparison, got:\n{}",
            ir
        );
    }

    #[test]
    fn test_display_module() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let main_id = strings.intern("main");
        let func = make_simple_func(main_id, Stmt::Return(Some(Expr::int(0, &types))), &types);
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu, &types, &strings);
        let ir = format!("{}", module);

        // Should have proper structure
        assert!(ir.contains("define"));
        assert!(ir.contains("main"));
        assert!(ir.contains(".L0:")); // Entry block label
        assert!(ir.contains("ret"));
    }

    #[test]
    fn test_type_propagation_expr_type() {
        let strings = StringTable::new();
        let types = TypeTable::new();

        // Create an expression with a type annotation
        let mut expr = Expr::int(42, &types);
        // Simulate type evaluation having set the type
        expr.typ = Some(types.int_id);

        // Create linearizer and test that expr_type reads from the expression
        let symbols = SymbolTable::new();
        let target = Target::host();
        let linearizer = Linearizer::new(&symbols, &types, &strings, &target);
        let typ = linearizer.expr_type(&expr);
        assert_eq!(types.kind(typ), TypeKind::Int);

        // Test with unsigned type
        let mut unsigned_expr = Expr::int(42, &types);
        unsigned_expr.typ = Some(types.uint_id);
        let typ = linearizer.expr_type(&unsigned_expr);
        assert!(types.is_unsigned(typ));
    }

    #[test]
    fn test_type_propagation_double_literal() {
        let strings = StringTable::new();
        let types = TypeTable::new();

        // Create a double literal
        let mut expr = Expr::new(ExprKind::FloatLit(3.14), test_pos());
        expr.typ = Some(types.double_id);

        let symbols = SymbolTable::new();
        let target = Target::host();
        let linearizer = Linearizer::new(&symbols, &types, &strings, &target);
        let typ = linearizer.expr_type(&expr);
        assert_eq!(types.kind(typ), TypeKind::Double);
    }

    // ========================================================================
    // SSA Conversion Tests
    // ========================================================================

    /// Helper to linearize without SSA conversion (for comparing before/after)
    fn linearize_no_ssa(tu: &TranslationUnit, types: &TypeTable, strings: &StringTable) -> Module {
        let symbols = SymbolTable::new();
        let target = Target::host();
        let mut linearizer = Linearizer::new_no_ssa(&symbols, types, strings, &target);
        linearizer.linearize(tu)
    }

    #[test]
    fn test_local_var_emits_load_store() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let test_id = strings.intern("test");
        let x_id = strings.intern("x");
        // int test() { int x = 1; return x; }
        let int_type = types.int_id;
        let func = FunctionDef {
            return_type: int_type,
            name: test_id,
            params: vec![],
            body: Stmt::Block(vec![
                BlockItem::Declaration(Declaration {
                    declarators: vec![crate::parse::ast::InitDeclarator {
                        name: x_id,
                        typ: int_type,
                        init: Some(Expr::int(1, &types)),
                        vla_sizes: vec![],
                    }],
                }),
                BlockItem::Statement(Stmt::Return(Some(Expr::var_typed(x_id, int_type)))),
            ]),
            pos: test_pos(),
        };
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        // Without SSA, should have store and load
        let module = linearize_no_ssa(&tu, &types, &strings);
        let ir = format!("{}", module);
        assert!(
            ir.contains("store"),
            "Should have store instruction before SSA: {}",
            ir
        );
        assert!(
            ir.contains("load"),
            "Should have load instruction before SSA: {}",
            ir
        );
    }

    #[test]
    fn test_ssa_converts_local_to_phi() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let test_id = strings.intern("test");
        let cond_id = strings.intern("cond");
        let x_id = strings.intern("x");
        // int test(int cond) {
        //     int x = 1;
        //     if (cond) x = 2;
        //     return x;
        // }
        let int_type = types.int_id;

        let func = FunctionDef {
            return_type: int_type,
            name: test_id,
            params: vec![Parameter {
                name: Some(cond_id),
                typ: int_type,
            }],
            body: Stmt::Block(vec![
                // int x = 1;
                BlockItem::Declaration(Declaration {
                    declarators: vec![crate::parse::ast::InitDeclarator {
                        name: x_id,
                        typ: int_type,
                        init: Some(Expr::int(1, &types)),
                        vla_sizes: vec![],
                    }],
                }),
                // if (cond) x = 2;
                BlockItem::Statement(Stmt::If {
                    cond: Expr::var_typed(cond_id, int_type),
                    then_stmt: Box::new(Stmt::Expr(Expr::assign(
                        Expr::var_typed(x_id, int_type),
                        Expr::int(2, &types),
                        &types,
                    ))),
                    else_stmt: None,
                }),
                // return x;
                BlockItem::Statement(Stmt::Return(Some(Expr::var_typed(x_id, int_type)))),
            ]),
            pos: test_pos(),
        };
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        // With SSA, should have phi node at merge point
        let module = test_linearize(&tu, &types, &strings);
        let ir = format!("{}", module);

        // Should have a phi instruction
        assert!(
            ir.contains("phi"),
            "SSA should insert phi node at merge point: {}",
            ir
        );
    }

    #[test]
    fn test_ssa_loop_variable() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let test_id = strings.intern("test");
        let i_id = strings.intern("i");
        // int test() {
        //     int i = 0;
        //     while (i < 10) { i = i + 1; }
        //     return i;
        // }
        let int_type = types.int_id;

        let i_var = || Expr::var_typed(i_id, int_type);

        let func = FunctionDef {
            return_type: int_type,
            name: test_id,
            params: vec![],
            body: Stmt::Block(vec![
                // int i = 0;
                BlockItem::Declaration(Declaration {
                    declarators: vec![crate::parse::ast::InitDeclarator {
                        name: i_id,
                        typ: int_type,
                        init: Some(Expr::int(0, &types)),
                        vla_sizes: vec![],
                    }],
                }),
                // while (i < 10) { i = i + 1; }
                BlockItem::Statement(Stmt::While {
                    cond: Expr::binary(BinaryOp::Lt, i_var(), Expr::int(10, &types), &types),
                    body: Box::new(Stmt::Expr(Expr::assign(
                        i_var(),
                        Expr::binary(BinaryOp::Add, i_var(), Expr::int(1, &types), &types),
                        &types,
                    ))),
                }),
                // return i;
                BlockItem::Statement(Stmt::Return(Some(i_var()))),
            ]),
            pos: test_pos(),
        };
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        // With SSA, should have phi node at loop header
        let module = test_linearize(&tu, &types, &strings);
        let ir = format!("{}", module);

        // Loop should have a phi at the condition block
        assert!(ir.contains("phi"), "Loop should have phi node: {}", ir);
    }

    #[test]
    fn test_short_circuit_and() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let test_id = strings.intern("test");
        let a_id = strings.intern("a");
        let b_id = strings.intern("b");
        // int test(int a, int b) {
        //     return a && b;
        // }
        // Short-circuit: if a is false, don't evaluate b
        let int_type = types.int_id;

        let func = FunctionDef {
            return_type: int_type,
            name: test_id,
            params: vec![
                Parameter {
                    name: Some(a_id),
                    typ: int_type,
                },
                Parameter {
                    name: Some(b_id),
                    typ: int_type,
                },
            ],
            body: Stmt::Return(Some(Expr::binary(
                BinaryOp::LogAnd,
                Expr::var_typed(a_id, int_type),
                Expr::var_typed(b_id, int_type),
                &types,
            ))),
            pos: test_pos(),
        };
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu, &types, &strings);
        let ir = format!("{}", module);

        // Short-circuit AND should have:
        // 1. A conditional branch (cbr) to skip evaluation of b if a is false
        // 2. A phi node to merge the result
        assert!(
            ir.contains("cbr"),
            "Short-circuit AND should have conditional branch: {}",
            ir
        );
        assert!(
            ir.contains("phi"),
            "Short-circuit AND should have phi node: {}",
            ir
        );
    }

    #[test]
    fn test_short_circuit_or() {
        let mut strings = StringTable::new();
        let types = TypeTable::new();
        let test_id = strings.intern("test");
        let a_id = strings.intern("a");
        let b_id = strings.intern("b");
        // int test(int a, int b) {
        //     return a || b;
        // }
        // Short-circuit: if a is true, don't evaluate b
        let int_type = types.int_id;

        let func = FunctionDef {
            return_type: int_type,
            name: test_id,
            params: vec![
                Parameter {
                    name: Some(a_id),
                    typ: int_type,
                },
                Parameter {
                    name: Some(b_id),
                    typ: int_type,
                },
            ],
            body: Stmt::Return(Some(Expr::binary(
                BinaryOp::LogOr,
                Expr::var_typed(a_id, int_type),
                Expr::var_typed(b_id, int_type),
                &types,
            ))),
            pos: test_pos(),
        };
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu, &types, &strings);
        let ir = format!("{}", module);

        // Short-circuit OR should have:
        // 1. A conditional branch (cbr) to skip evaluation of b if a is true
        // 2. A phi node to merge the result
        assert!(
            ir.contains("cbr"),
            "Short-circuit OR should have conditional branch: {}",
            ir
        );
        assert!(
            ir.contains("phi"),
            "Short-circuit OR should have phi node: {}",
            ir
        );
    }
}
