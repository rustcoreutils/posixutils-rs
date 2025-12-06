//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linearizer for pcc C99 compiler
// Converts AST to SSA-style IR following sparse's design
//

use crate::diag::Position;
use crate::ir::{
    BasicBlock, BasicBlockId, Function, Initializer, Instruction, Module, Opcode, Pseudo, PseudoId,
};
use crate::parse::ast::{
    AssignOp, BinaryOp, BlockItem, Declaration, Designator, Expr, ExprKind, ExternalDecl, ForInit,
    FunctionDef, InitElement, Stmt, TranslationUnit, UnaryOp,
};
use crate::ssa::ssa_convert;
use crate::symbol::SymbolTable;
use crate::types::{MemberInfo, Type, TypeKind, TypeModifiers};
use std::collections::HashMap;

/// Maximum size (in bits) for aggregate types (struct/union) to be passed or
/// returned by value in registers. Aggregates larger than this require
/// indirect passing (pointer) or sret (struct return pointer).
pub const MAX_REGISTER_AGGREGATE_BITS: u32 = 64;

/// Information about a local variable
#[derive(Clone)]
struct LocalVarInfo {
    /// Symbol pseudo (address of the variable)
    sym: PseudoId,
    /// Type of the variable
    typ: Type,
}

/// Information about a static local variable
#[derive(Clone)]
struct StaticLocalInfo {
    /// Global symbol name (unique across translation unit)
    global_name: String,
    /// Type of the variable
    typ: Type,
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
    /// Hidden struct return pointer (for functions returning large structs)
    struct_return_ptr: Option<PseudoId>,
    /// Size of struct being returned (for functions returning large structs)
    struct_return_size: u32,
    /// Current function name (for generating unique static local names)
    current_func_name: String,
    /// Counter for generating unique static local names
    static_local_counter: u32,
    /// Static local variables (local name -> static local info)
    /// This is persistent across function calls (not cleared per function)
    static_locals: HashMap<String, StaticLocalInfo>,
    /// Current source position for debug info
    current_pos: Option<Position>,
}

impl<'a> Linearizer<'a> {
    /// Create a new linearizer
    pub fn new(symbols: &'a SymbolTable) -> Self {
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
            struct_return_ptr: None,
            struct_return_size: 0,
            current_func_name: String::new(),
            static_local_counter: 0,
            static_locals: HashMap::new(),
            current_pos: None,
        }
    }

    /// Create a linearizer with SSA conversion disabled (for testing)
    #[cfg(test)]
    pub fn new_no_ssa(symbols: &'a SymbolTable) -> Self {
        Self {
            run_ssa: false,
            ..Self::new(symbols)
        }
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
    fn integer_promote(typ: &Type) -> Type {
        // Integer promotions apply to _Bool, char, short (and their unsigned variants)
        // They are promoted to int if int can represent all values, otherwise unsigned int
        match typ.kind {
            TypeKind::Bool | TypeKind::Char | TypeKind::Short => {
                // int (32-bit signed) can represent all values of:
                // - _Bool (0-1)
                // - char/signed char (-128 to 127)
                // - unsigned char (0 to 255)
                // - short/signed short (-32768 to 32767)
                // - unsigned short (0 to 65535)
                // So always promote to int
                Type::basic(TypeKind::Int)
            }
            _ => typ.clone(),
        }
    }

    /// Compute the common type for usual arithmetic conversions (C99 6.3.1.8)
    /// Returns the wider type that both operands should be converted to
    fn common_type(left: &Type, right: &Type) -> Type {
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
        let left_float = left.is_float();
        let right_float = right.is_float();

        if left_float || right_float {
            // At least one operand is floating point
            // Use the wider floating point type
            if left.kind == TypeKind::LongDouble || right.kind == TypeKind::LongDouble {
                return Type::basic(TypeKind::LongDouble);
            }
            if left.kind == TypeKind::Double || right.kind == TypeKind::Double {
                return Type::basic(TypeKind::Double);
            }
            // Both are float or one is float and one is integer
            return Type::basic(TypeKind::Float);
        }

        // Apply integer promotions first (C99 6.3.1.1)
        let left_promoted = Self::integer_promote(left);
        let right_promoted = Self::integer_promote(right);

        let left_size = left_promoted.size_bits();
        let right_size = right_promoted.size_bits();
        let left_unsigned = left_promoted.is_unsigned();
        let right_unsigned = right_promoted.is_unsigned();

        // If both have same type after promotion, use that type
        if left_promoted.kind == right_promoted.kind
            && left_unsigned == right_unsigned
            && left_size == right_size
        {
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
        let (signed_typ, unsigned_typ) = if left_unsigned {
            (&right_promoted, &left_promoted)
        } else {
            (&left_promoted, &right_promoted)
        };

        let signed_size = signed_typ.size_bits();
        let unsigned_size = unsigned_typ.size_bits();

        // If unsigned has rank >= signed, convert to unsigned
        if unsigned_size >= signed_size {
            return unsigned_typ.clone();
        }

        // If signed type can represent all values of unsigned type, use signed
        // (This is true when signed_size > unsigned_size on our platforms)
        if signed_size > unsigned_size {
            return signed_typ.clone();
        }

        // Otherwise convert both to unsigned version of signed type
        // (This case shouldn't happen on LP64 since we already handled size comparisons)
        Type::with_modifiers(signed_typ.kind, TypeModifiers::UNSIGNED)
    }

    /// Emit a type conversion if needed
    /// Returns the (possibly converted) pseudo ID
    fn emit_convert(&mut self, val: PseudoId, from_typ: &Type, to_typ: &Type) -> PseudoId {
        let from_size = from_typ.size_bits();
        let to_size = to_typ.size_bits();
        let from_float = from_typ.is_float();
        let to_float = to_typ.is_float();

        // Same type and size - no conversion needed
        if from_typ.kind == to_typ.kind && from_size == to_size {
            return val;
        }

        // Array to pointer conversion (decay) - no actual conversion needed
        // The array value is already the address of the first element (64-bit)
        if from_typ.kind == TypeKind::Array && to_typ.kind == TypeKind::Pointer {
            return val;
        }

        // Pointer to pointer conversion - no actual conversion needed
        // All pointers are the same size (64-bit)
        if from_typ.kind == TypeKind::Pointer && to_typ.kind == TypeKind::Pointer {
            return val;
        }

        // Special handling for _Bool conversion (C99 6.3.1.2)
        // When any scalar value is converted to _Bool:
        // - Result is 0 if the value compares equal to 0
        // - Result is 1 otherwise
        if to_typ.kind == TypeKind::Bool && from_typ.kind != TypeKind::Bool {
            let result = self.alloc_pseudo();
            let pseudo = Pseudo::reg(result, result.0);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(pseudo);
            }

            // Create a zero constant for comparison
            let zero = self.emit_const(0, from_typ.clone());

            // Compare val != 0
            let opcode = if from_float {
                Opcode::FCmpONe
            } else {
                Opcode::SetNe
            };

            let mut insn = Instruction::binop(opcode, result, val, zero, to_typ.clone());
            insn.size = to_size;
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
                if to_typ.is_unsigned() {
                    Opcode::FCvtU
                } else {
                    Opcode::FCvtS
                }
            } else {
                // Integer to float
                if from_typ.is_unsigned() {
                    Opcode::UCvtF
                } else {
                    Opcode::SCvtF
                }
            };

            let mut insn = Instruction::unop(opcode, result, val, to_typ.clone());
            insn.size = to_size;
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
            let opcode = if from_typ.is_unsigned() {
                Opcode::Zext
            } else {
                Opcode::Sext
            };
            let mut insn = Instruction::unop(opcode, result, val, to_typ.clone());
            insn.size = to_size;
            insn.src_size = from_size;
            self.emit(insn);
        } else {
            // Truncating
            let mut insn = Instruction::unop(Opcode::Trunc, result, val, to_typ.clone());
            insn.size = to_size;
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
            // Skip function declarations (they're just forward declarations for external functions)
            if declarator.typ.kind == TypeKind::Function {
                continue;
            }

            // Skip extern declarations - they don't define storage
            if declarator.typ.modifiers.contains(TypeModifiers::EXTERN) {
                continue;
            }

            let init = declarator
                .init
                .as_ref()
                .map_or(Initializer::None, |e| match &e.kind {
                    ExprKind::IntLit(v) => Initializer::Int(*v),
                    ExprKind::FloatLit(v) => Initializer::Float(*v),
                    _ => Initializer::None,
                });

            self.module
                .add_global(&declarator.name, declarator.typ.clone(), init);
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
        self.current_func_name = func.name.clone();
        // Note: static_locals is NOT cleared - it persists across functions

        // Create function
        let is_static = func.return_type.modifiers.contains(TypeModifiers::STATIC);
        let mut ir_func = Function::new(&func.name, func.return_type.clone());
        ir_func.is_static = is_static;

        // Check if function returns a large struct
        // Large structs are returned via a hidden first parameter (sret)
        // that points to caller-allocated space
        let returns_large_struct = (func.return_type.kind == TypeKind::Struct
            || func.return_type.kind == TypeKind::Union)
            && func.return_type.size_bits() > MAX_REGISTER_AGGREGATE_BITS;

        // Argument index offset: if returning large struct, first arg is hidden return pointer
        let arg_offset: u32 = if returns_large_struct { 1 } else { 0 };

        // Add hidden return pointer parameter if needed
        if returns_large_struct {
            let sret_id = self.alloc_pseudo();
            let sret_pseudo = Pseudo::arg(sret_id, 0).with_name("__sret");
            ir_func.add_pseudo(sret_pseudo);
            self.struct_return_ptr = Some(sret_id);
            self.struct_return_size = func.return_type.size_bits();
        }

        // Add parameters
        // For struct/union parameters, we need to copy them to local storage
        // so member access works properly
        let mut struct_params: Vec<(String, Type, PseudoId)> = Vec::new();

        for (i, param) in func.params.iter().enumerate() {
            let name = param.name.clone().unwrap_or_else(|| format!("arg{}", i));
            ir_func.add_param(&name, param.typ.clone());

            // Create argument pseudo (offset by 1 if there's a hidden return pointer)
            let pseudo_id = self.alloc_pseudo();
            let pseudo = Pseudo::arg(pseudo_id, i as u32 + arg_offset).with_name(&name);
            ir_func.add_pseudo(pseudo);

            // For struct/union types, we'll copy to a local later
            if param.typ.kind == TypeKind::Struct || param.typ.kind == TypeKind::Union {
                struct_params.push((name, param.typ.clone(), pseudo_id));
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
                let is_volatile = typ.modifiers.contains(TypeModifiers::VOLATILE);
                func.add_local(&name, local_sym, typ.clone(), is_volatile, None);
            }

            // For large structs, arg_pseudo is a pointer to the struct
            // We need to copy the data from that pointer to local storage
            if typ.size_bits() > MAX_REGISTER_AGGREGATE_BITS {
                // arg_pseudo is a pointer - copy each 8-byte chunk
                let struct_size = typ.size_bits() / 8;
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
                        Type::basic(TypeKind::Long),
                    ));
                    // Store to local_sym + offset
                    self.emit(Instruction::store(
                        temp,
                        local_sym,
                        offset,
                        Type::basic(TypeKind::Long),
                    ));
                    offset += 8;
                }
            } else {
                // Small struct: arg_pseudo contains the value directly
                self.emit(Instruction::store(arg_pseudo, local_sym, 0, typ.clone()));
            }

            // Register as a local variable
            self.locals.insert(
                name,
                LocalVarInfo {
                    sym: local_sym,
                    typ,
                },
            );
        }

        // Linearize body
        self.linearize_stmt(&func.body);

        // Ensure function ends with a return
        if !self.is_terminated() {
            if func.return_type.kind == TypeKind::Void {
                self.emit(Instruction::ret(None));
            } else {
                // Return 0 as default
                let zero = self.emit_const(0, Type::basic(TypeKind::Int));
                self.emit(Instruction::ret(Some(zero)));
            }
        }

        // Run SSA conversion if enabled
        if self.run_ssa {
            if let Some(ref mut ir_func) = self.current_func {
                ssa_convert(ir_func);
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

                    // Check if this is a large struct return with hidden pointer
                    if let Some(sret_ptr) = self.struct_return_ptr {
                        let struct_size = self.struct_return_size;
                        // Get address of the struct being returned
                        let src_addr = self.linearize_lvalue(e);

                        // Copy the struct to the hidden return pointer
                        // struct_size is in bits, so /8 gives bytes
                        let struct_bytes = struct_size as i64 / 8;
                        let mut byte_offset = 0i64;
                        while byte_offset < struct_bytes {
                            // Load 8 bytes from source
                            let temp = self.alloc_pseudo();
                            let temp_pseudo = Pseudo::reg(temp, temp.0);
                            if let Some(func) = &mut self.current_func {
                                func.add_pseudo(temp_pseudo);
                            }
                            self.emit(Instruction::load(
                                temp,
                                src_addr,
                                byte_offset,
                                Type::basic(TypeKind::Long),
                            ));
                            // Store to destination
                            self.emit(Instruction::store(
                                temp,
                                sret_ptr,
                                byte_offset,
                                Type::basic(TypeKind::Long),
                            ));
                            byte_offset += 8;
                        }

                        // Return the hidden pointer (ABI requirement)
                        self.emit(Instruction::ret_typed(Some(sret_ptr), Type::pointer(typ)));
                    } else {
                        let val = self.linearize_expr(e);
                        self.emit(Instruction::ret_typed(Some(val), typ));
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
                let target = self.get_or_create_label(label);
                self.emit(Instruction::br(target));
            }

            Stmt::Label { name, stmt } => {
                let label_bb = self.get_or_create_label(name);

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
            let typ = declarator.typ.clone();

            // Check if this is a static local variable
            if typ.modifiers.contains(TypeModifiers::STATIC) {
                // Static local: create a global with unique name
                self.linearize_static_local(declarator);
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
                let is_volatile = typ.modifiers.contains(TypeModifiers::VOLATILE);
                func.add_local(
                    &unique_name,
                    sym_id,
                    typ.clone(),
                    is_volatile,
                    self.current_bb,
                );
            }

            // Track in linearizer's locals map
            self.locals.insert(
                declarator.name.clone(),
                LocalVarInfo {
                    sym: sym_id,
                    typ: typ.clone(),
                },
            );

            // If there's an initializer, emit Store(s)
            if let Some(init) = &declarator.init {
                if let ExprKind::InitList { elements } = &init.kind {
                    // Handle initializer list for arrays and structs
                    self.linearize_init_list(sym_id, &typ, elements);
                } else {
                    // Simple scalar initializer
                    let val = self.linearize_expr(init);
                    // Convert the value to the target type (important for _Bool normalization)
                    let init_type = self.expr_type(init);
                    let converted = self.emit_convert(val, &init_type, &typ);
                    self.emit(Instruction::store(converted, sym_id, 0, typ));
                }
            }
        }
    }

    /// Linearize a static local variable declaration
    ///
    /// Static locals have static storage duration but no linkage.
    /// They are implemented as globals with unique names like `funcname.varname.N`.
    /// Initialization happens once at program start (compile-time).
    fn linearize_static_local(&mut self, declarator: &crate::parse::ast::InitDeclarator) {
        // Generate unique global name: funcname.varname.counter
        let global_name = format!(
            "{}.{}.{}",
            self.current_func_name, declarator.name, self.static_local_counter
        );
        self.static_local_counter += 1;

        // Track mapping from local name to global name for this function's scope
        // Use a key that includes function name to handle same-named statics in different functions
        let key = format!("{}.{}", self.current_func_name, declarator.name);
        self.static_locals.insert(
            key,
            StaticLocalInfo {
                global_name: global_name.clone(),
                typ: declarator.typ.clone(),
            },
        );

        // Also insert with just the local name for the current function scope
        // This is used during expression linearization
        self.locals.insert(
            declarator.name.clone(),
            LocalVarInfo {
                // Use a sentinel value - we'll handle static locals specially
                sym: PseudoId(u32::MAX),
                typ: declarator.typ.clone(),
            },
        );

        // Determine initializer (static locals are initialized at compile time)
        let init = declarator
            .init
            .as_ref()
            .map_or(Initializer::None, |e| match &e.kind {
                ExprKind::IntLit(v) => Initializer::Int(*v),
                ExprKind::FloatLit(v) => Initializer::Float(*v),
                ExprKind::CharLit(c) => Initializer::Int(*c as i64),
                ExprKind::Unary {
                    op: UnaryOp::Neg,
                    operand,
                } => {
                    // Handle negative literals
                    match &operand.kind {
                        ExprKind::IntLit(v) => Initializer::Int(-*v),
                        ExprKind::FloatLit(v) => Initializer::Float(-*v),
                        _ => Initializer::None,
                    }
                }
                _ => Initializer::None,
            });

        // Add as a global (type already has STATIC modifier which codegen uses)
        self.module
            .add_global(&global_name, declarator.typ.clone(), init);
    }

    /// Linearize an initializer list for arrays or structs
    fn linearize_init_list(&mut self, base_sym: PseudoId, typ: &Type, elements: &[InitElement]) {
        self.linearize_init_list_at_offset(base_sym, 0, typ, elements);
    }

    /// Linearize an initializer list at a given base offset
    fn linearize_init_list_at_offset(
        &mut self,
        base_sym: PseudoId,
        base_offset: i64,
        typ: &Type,
        elements: &[InitElement],
    ) {
        match typ.kind {
            TypeKind::Array => {
                let elem_type = typ
                    .base
                    .as_ref()
                    .map(|b| (**b).clone())
                    .unwrap_or_else(|| Type::basic(TypeKind::Int));
                let elem_size = elem_type.size_bits() / 8;

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
                            &elem_type,
                            nested_elems,
                        );
                    } else {
                        // Scalar value
                        let val = self.linearize_expr(&element.value);
                        let val_type = self.expr_type(&element.value);
                        let converted = self.emit_convert(val, &val_type, &elem_type);
                        self.emit(Instruction::store(
                            converted,
                            base_sym,
                            offset,
                            elem_type.clone(),
                        ));
                    }
                }
            }
            TypeKind::Struct | TypeKind::Union => {
                // Get struct fields from the type's composite data
                if let Some(composite) = &typ.composite {
                    let members = &composite.members;

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
                        let field_type = member.typ.clone();

                        // Handle nested initializer lists or scalar values
                        if let ExprKind::InitList {
                            elements: nested_elems,
                        } = &element.value.kind
                        {
                            // Nested struct/array initialization - recurse with accumulated offset
                            self.linearize_init_list_at_offset(
                                base_sym,
                                offset,
                                &field_type,
                                nested_elems,
                            );
                        } else {
                            // Scalar value
                            let val = self.linearize_expr(&element.value);
                            let val_type = self.expr_type(&element.value);
                            let converted = self.emit_convert(val, &val_type, &field_type);
                            self.emit(Instruction::store(converted, base_sym, offset, field_type));
                        }
                    }
                }
            }
            _ => {
                // For other types, just use the first element if present
                if let Some(element) = elements.first() {
                    let val = self.linearize_expr(&element.value);
                    let val_type = self.expr_type(&element.value);
                    let converted = self.emit_convert(val, &val_type, typ);
                    self.emit(Instruction::store(
                        converted,
                        base_sym,
                        base_offset,
                        typ.clone(),
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
        let size = expr_type.size_bits();

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

    /// Evaluate a constant expression (for case labels)
    fn eval_const_expr(&self, expr: &Expr) -> Option<i64> {
        match &expr.kind {
            ExprKind::IntLit(val) => Some(*val),
            ExprKind::CharLit(c) => Some(*c as i64),
            ExprKind::Ident { name, .. } => {
                // Check if it's an enum constant
                self.symbols.get_enum_value(name)
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
                    BinaryOp::Add => Some(l + r),
                    BinaryOp::Sub => Some(l - r),
                    BinaryOp::Mul => Some(l * r),
                    BinaryOp::Div => Some(l / r),
                    BinaryOp::Mod => Some(l % r),
                    BinaryOp::BitAnd => Some(l & r),
                    BinaryOp::BitOr => Some(l | r),
                    BinaryOp::BitXor => Some(l ^ r),
                    BinaryOp::Shl => Some(l << r),
                    BinaryOp::Shr => Some(l >> r),
                    _ => None,
                }
            }
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
    fn expr_type(&self, expr: &Expr) -> Type {
        expr.typ.clone().expect(
            "BUG: expression has no type. Type evaluation pass must run before linearization.",
        )
    }

    /// Linearize an expression as an lvalue (get its address)
    fn linearize_lvalue(&mut self, expr: &Expr) -> PseudoId {
        match &expr.kind {
            ExprKind::Ident { name, .. } => {
                // For local variables, emit SymAddr to get the stack address
                if let Some(local) = self.locals.get(name).cloned() {
                    // Check if this is a static local (sentinel value)
                    if local.sym.0 == u32::MAX {
                        // Static local - look up the global name
                        let key = format!("{}.{}", self.current_func_name, name);
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
                                Type::pointer(static_info.typ),
                            ));
                            return result;
                        }
                    }
                    let result = self.alloc_pseudo();
                    self.emit(Instruction::sym_addr(
                        result,
                        local.sym,
                        Type::pointer(local.typ),
                    ));
                    result
                } else {
                    // Global variable - emit SymAddr to get its address
                    let sym_id = self.alloc_pseudo();
                    let pseudo = Pseudo::sym(sym_id, name.clone());
                    if let Some(func) = &mut self.current_func {
                        func.add_pseudo(pseudo);
                    }
                    let result = self.alloc_pseudo();
                    let typ = self.expr_type(expr);
                    self.emit(Instruction::sym_addr(result, sym_id, Type::pointer(typ)));
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
                let member_info = struct_type
                    .find_member(member)
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
                        self.emit_const(member_info.offset as i64, Type::basic(TypeKind::Long));
                    let result = self.alloc_pseudo();
                    self.emit(Instruction::binop(
                        Opcode::Add,
                        result,
                        base,
                        offset_val,
                        Type::basic(TypeKind::Long),
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
                let struct_type = ptr_type
                    .base
                    .as_ref()
                    .map(|b| b.as_ref().clone())
                    .unwrap_or_else(|| self.expr_type(expr));
                let member_info = struct_type
                    .find_member(member)
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
                        self.emit_const(member_info.offset as i64, Type::basic(TypeKind::Long));
                    let result = self.alloc_pseudo();
                    self.emit(Instruction::binop(
                        Opcode::Add,
                        result,
                        ptr,
                        offset_val,
                        Type::basic(TypeKind::Long),
                    ));
                    result
                }
            }
            ExprKind::Index { array, index } => {
                // arr[idx] as lvalue = arr + idx * sizeof(elem)
                // Handle commutative form: 0[arr] is equivalent to arr[0]
                let array_type = self.expr_type(array);
                let index_type = self.expr_type(index);

                let (ptr_expr, idx_expr, idx_type) =
                    if array_type.kind == TypeKind::Pointer || array_type.kind == TypeKind::Array {
                        (array, index, index_type)
                    } else {
                        // Swap: index is actually the pointer/array
                        (index, array, array_type)
                    };

                let arr = self.linearize_expr(ptr_expr);
                let idx = self.linearize_expr(idx_expr);
                let elem_type = self.expr_type(expr);
                let elem_size = elem_type.size_bits() / 8;
                let elem_size_val = self.emit_const(elem_size as i64, Type::basic(TypeKind::Long));

                // Sign-extend index to 64-bit for proper pointer arithmetic (negative indices)
                let idx_extended = self.emit_convert(idx, &idx_type, &Type::basic(TypeKind::Long));

                let offset = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::Mul,
                    offset,
                    idx_extended,
                    elem_size_val,
                    Type::basic(TypeKind::Long),
                ));

                let addr = self.alloc_pseudo();
                self.emit(Instruction::binop(
                    Opcode::Add,
                    addr,
                    arr,
                    offset,
                    Type::basic(TypeKind::Long),
                ));
                addr
            }
            _ => {
                // Fallback: just evaluate the expression (shouldn't happen for valid lvalues)
                self.linearize_expr(expr)
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

            ExprKind::Ident { name, .. } => {
                // First check if it's an enum constant
                if let Some(value) = self.symbols.get_enum_value(name) {
                    self.emit_const(value, Type::basic(TypeKind::Int))
                }
                // Check if it's a local variable
                else if let Some(local) = self.locals.get(name).cloned() {
                    // Check if this is a static local (sentinel value)
                    if local.sym.0 == u32::MAX {
                        // Static local - look up the global name and treat as global
                        let key = format!("{}.{}", self.current_func_name, name);
                        if let Some(static_info) = self.static_locals.get(&key).cloned() {
                            let sym_id = self.alloc_pseudo();
                            let pseudo = Pseudo::sym(sym_id, static_info.global_name);
                            if let Some(func) = &mut self.current_func {
                                func.add_pseudo(pseudo);
                            }
                            let typ = static_info.typ;
                            // Arrays decay to pointers - get address, not value
                            if typ.kind == TypeKind::Array {
                                let result = self.alloc_pseudo();
                                self.emit(Instruction::sym_addr(
                                    result,
                                    sym_id,
                                    Type::pointer(
                                        typ.base
                                            .as_ref()
                                            .map(|b| (**b).clone())
                                            .unwrap_or_else(|| Type::basic(TypeKind::Int)),
                                    ),
                                ));
                                return result;
                            } else {
                                let result = self.alloc_pseudo();
                                self.emit(Instruction::load(result, sym_id, 0, typ));
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
                    if local.typ.kind == TypeKind::Array {
                        self.emit(Instruction::sym_addr(
                            result,
                            local.sym,
                            Type::pointer(
                                local
                                    .typ
                                    .base
                                    .as_ref()
                                    .map(|b| (**b).clone())
                                    .unwrap_or_else(|| Type::basic(TypeKind::Int)),
                            ),
                        ));
                    } else {
                        self.emit(Instruction::load(result, local.sym, 0, local.typ));
                    }
                    result
                }
                // Check if it's a parameter (already SSA value)
                else if let Some(&pseudo) = self.var_map.get(name) {
                    pseudo
                }
                // Global variable - create symbol reference and load
                else {
                    let sym_id = self.alloc_pseudo();
                    let pseudo = Pseudo::sym(sym_id, name.clone());
                    if let Some(func) = &mut self.current_func {
                        func.add_pseudo(pseudo);
                    }
                    let typ = self.expr_type(expr);
                    // Arrays decay to pointers - get address, not value
                    if typ.kind == TypeKind::Array {
                        let result = self.alloc_pseudo();
                        self.emit(Instruction::sym_addr(
                            result,
                            sym_id,
                            Type::pointer(
                                typ.base
                                    .as_ref()
                                    .map(|b| (**b).clone())
                                    .unwrap_or_else(|| Type::basic(TypeKind::Int)),
                            ),
                        ));
                        result
                    } else {
                        let result = self.alloc_pseudo();
                        self.emit(Instruction::load(result, sym_id, 0, typ));
                        result
                    }
                }
            }

            ExprKind::Unary { op, operand } => {
                // Handle AddrOf specially - we need the lvalue address, not the value
                if *op == UnaryOp::AddrOf {
                    return self.linearize_lvalue(operand);
                }

                // Handle PreInc/PreDec specially - they need store-back
                if *op == UnaryOp::PreInc || *op == UnaryOp::PreDec {
                    let val = self.linearize_expr(operand);
                    let typ = self.expr_type(operand);
                    let is_float = typ.is_float();
                    let is_ptr = typ.kind == TypeKind::Pointer;

                    // Compute new value - for pointers, scale by element size
                    let increment = if is_ptr {
                        let elem_type = typ
                            .base
                            .as_ref()
                            .map(|b| (**b).clone())
                            .unwrap_or_else(|| Type::basic(TypeKind::Char));
                        let elem_size = elem_type.size_bits() / 8;
                        self.emit_const(elem_size as i64, Type::basic(TypeKind::Long))
                    } else if is_float {
                        self.emit_fconst(1.0, typ.clone())
                    } else {
                        self.emit_const(1, typ.clone())
                    };
                    let result = self.alloc_pseudo();
                    let pseudo = Pseudo::reg(result, result.0);
                    if let Some(func) = &mut self.current_func {
                        func.add_pseudo(pseudo);
                    }
                    let opcode = if is_float {
                        if *op == UnaryOp::PreInc {
                            Opcode::FAdd
                        } else {
                            Opcode::FSub
                        }
                    } else if *op == UnaryOp::PreInc {
                        Opcode::Add
                    } else {
                        Opcode::Sub
                    };
                    self.emit(Instruction::binop(
                        opcode,
                        result,
                        val,
                        increment,
                        typ.clone(),
                    ));

                    // For _Bool, normalize the result (any non-zero -> 1)
                    let final_result = if typ.kind == TypeKind::Bool {
                        self.emit_convert(result, &Type::basic(TypeKind::Int), &typ)
                    } else {
                        result
                    };

                    // Store back to the variable
                    if let ExprKind::Ident { name, .. } = &operand.kind {
                        if let Some(local) = self.locals.get(name).cloned() {
                            self.emit(Instruction::store(final_result, local.sym, 0, typ));
                        } else if self.var_map.contains_key(name) {
                            self.var_map.insert(name.clone(), final_result);
                        } else {
                            // Global variable - emit store
                            let sym_id = self.alloc_pseudo();
                            let pseudo = Pseudo::sym(sym_id, name.clone());
                            if let Some(func) = &mut self.current_func {
                                func.add_pseudo(pseudo);
                            }
                            self.emit(Instruction::store(final_result, sym_id, 0, typ));
                        }
                    }

                    return final_result;
                }

                let src = self.linearize_expr(operand);
                let result_typ = self.expr_type(expr);
                let operand_typ = self.expr_type(operand);
                // For logical NOT, use operand type for comparison size
                let typ = if *op == UnaryOp::Not {
                    operand_typ
                } else {
                    result_typ
                };
                self.emit_unary(*op, src, typ)
            }

            ExprKind::Binary { op, left, right } => {
                // Handle short-circuit operators before linearizing both operands
                // C99 requires that && and || only evaluate the RHS if needed
                if *op == BinaryOp::LogAnd {
                    return self.emit_logical_and(left, right);
                }
                if *op == BinaryOp::LogOr {
                    return self.emit_logical_or(left, right);
                }

                let left_typ = self.expr_type(left);
                let right_typ = self.expr_type(right);
                let result_typ = self.expr_type(expr);

                // Check for pointer arithmetic: ptr +/- int or int + ptr
                let left_is_ptr_or_arr =
                    left_typ.kind == TypeKind::Pointer || left_typ.kind == TypeKind::Array;
                let right_is_ptr_or_arr =
                    right_typ.kind == TypeKind::Pointer || right_typ.kind == TypeKind::Array;
                let is_ptr_arith = (*op == BinaryOp::Add || *op == BinaryOp::Sub)
                    && ((left_is_ptr_or_arr && right_typ.is_integer())
                        || (left_typ.is_integer() && right_is_ptr_or_arr));

                // Check for pointer difference: ptr - ptr
                let is_ptr_diff = *op == BinaryOp::Sub && left_is_ptr_or_arr && right_is_ptr_or_arr;

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
                        Type::basic(TypeKind::Long),
                    ));

                    // Get element size from the pointer type
                    let elem_type = left_typ
                        .base
                        .as_ref()
                        .map(|b| (**b).clone())
                        .unwrap_or_else(|| Type::basic(TypeKind::Char));
                    let elem_size = elem_type.size_bits() / 8;

                    // Divide by element size
                    let scale = self.emit_const(elem_size as i64, Type::basic(TypeKind::Long));
                    let result = self.alloc_pseudo();
                    self.emit(Instruction::binop(
                        Opcode::DivS,
                        result,
                        byte_diff,
                        scale,
                        Type::basic(TypeKind::Long),
                    ));
                    result
                } else if is_ptr_arith {
                    // Pointer arithmetic: scale integer operand by element size
                    let (ptr_val, ptr_typ, int_val) = if left_is_ptr_or_arr {
                        let ptr = self.linearize_expr(left);
                        let int = self.linearize_expr(right);
                        (ptr, left_typ.clone(), int)
                    } else {
                        // int + ptr case
                        let int = self.linearize_expr(left);
                        let ptr = self.linearize_expr(right);
                        (ptr, right_typ.clone(), int)
                    };

                    // Get element size
                    let elem_type = ptr_typ
                        .base
                        .as_ref()
                        .map(|b| (**b).clone())
                        .unwrap_or_else(|| Type::basic(TypeKind::Char));
                    let elem_size = elem_type.size_bits() / 8;

                    // Scale the integer by element size
                    let scale = self.emit_const(elem_size as i64, Type::basic(TypeKind::Long));
                    let scaled_offset = self.alloc_pseudo();
                    // Extend int_val to 64-bit for proper address arithmetic
                    let int_val_extended = self.emit_convert(
                        int_val,
                        &Type::basic(TypeKind::Int),
                        &Type::basic(TypeKind::Long),
                    );
                    self.emit(Instruction::binop(
                        Opcode::Mul,
                        scaled_offset,
                        int_val_extended,
                        scale,
                        Type::basic(TypeKind::Long),
                    ));

                    // Add (or subtract) to pointer
                    let result = self.alloc_pseudo();
                    let opcode = if *op == BinaryOp::Sub {
                        Opcode::Sub
                    } else {
                        Opcode::Add
                    };
                    self.emit(Instruction::binop(
                        opcode,
                        result,
                        ptr_val,
                        scaled_offset,
                        Type::basic(TypeKind::Long),
                    ));
                    result
                } else {
                    // For comparisons, compute common type for both operands
                    // (usual arithmetic conversions)
                    let operand_typ = if op.is_comparison() {
                        Self::common_type(&left_typ, &right_typ)
                    } else {
                        result_typ.clone()
                    };

                    // Linearize operands
                    let left_val = self.linearize_expr(left);
                    let right_val = self.linearize_expr(right);

                    // Emit type conversions if needed
                    let left_val = self.emit_convert(left_val, &left_typ, &operand_typ);
                    let right_val = self.emit_convert(right_val, &right_typ, &operand_typ);

                    self.emit_binary(*op, left_val, right_val, result_typ, operand_typ)
                }
            }

            ExprKind::Assign { op, target, value } => self.emit_assign(*op, target, value),

            ExprKind::PostInc(operand) => {
                let val = self.linearize_expr(operand);
                let typ = self.expr_type(operand);
                let is_float = typ.is_float();
                let is_ptr = typ.kind == TypeKind::Pointer;

                // For locals, we need to save the old value before updating
                // because the pseudo will be reloaded from stack which gets overwritten
                let is_local = if let ExprKind::Ident { name, .. } = &operand.kind {
                    self.locals.contains_key(name)
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
                            .with_size(typ.size_bits()),
                    );
                    temp
                } else {
                    val
                };

                // For pointers, increment by element size; for others, increment by 1
                let increment = if is_ptr {
                    let elem_type = typ
                        .base
                        .as_ref()
                        .map(|b| (**b).clone())
                        .unwrap_or_else(|| Type::basic(TypeKind::Char));
                    let elem_size = elem_type.size_bits() / 8;
                    self.emit_const(elem_size as i64, Type::basic(TypeKind::Long))
                } else if is_float {
                    self.emit_fconst(1.0, typ.clone())
                } else {
                    self.emit_const(1, typ.clone())
                };
                let result = self.alloc_pseudo();
                let pseudo = Pseudo::reg(result, result.0);
                if let Some(func) = &mut self.current_func {
                    func.add_pseudo(pseudo);
                }
                let opcode = if is_float { Opcode::FAdd } else { Opcode::Add };
                let arith_type = if is_ptr {
                    Type::basic(TypeKind::Long)
                } else {
                    typ.clone()
                };
                self.emit(Instruction::binop(
                    opcode, result, val, increment, arith_type,
                ));

                // For _Bool, normalize the result (any non-zero -> 1)
                let final_result = if typ.kind == TypeKind::Bool {
                    self.emit_convert(result, &Type::basic(TypeKind::Int), &typ)
                } else {
                    result
                };

                // Store to local, update parameter mapping, or store through pointer
                match &operand.kind {
                    ExprKind::Ident { name, .. } => {
                        if let Some(local) = self.locals.get(name).cloned() {
                            self.emit(Instruction::store(final_result, local.sym, 0, typ));
                        } else if self.var_map.contains_key(name) {
                            self.var_map.insert(name.clone(), final_result);
                        } else {
                            // Global variable - emit store
                            let sym_id = self.alloc_pseudo();
                            let pseudo = Pseudo::sym(sym_id, name.clone());
                            if let Some(func) = &mut self.current_func {
                                func.add_pseudo(pseudo);
                            }
                            self.emit(Instruction::store(final_result, sym_id, 0, typ));
                        }
                    }
                    ExprKind::Unary {
                        op: UnaryOp::Deref,
                        operand: ptr_expr,
                    } => {
                        // (*p)++ - store back through the pointer
                        let addr = self.linearize_expr(ptr_expr);
                        self.emit(Instruction::store(final_result, addr, 0, typ));
                    }
                    _ => {}
                }

                old_val // Return old value
            }

            ExprKind::PostDec(operand) => {
                let val = self.linearize_expr(operand);
                let typ = self.expr_type(operand);
                let is_float = typ.is_float();
                let is_ptr = typ.kind == TypeKind::Pointer;

                // For locals, we need to save the old value before updating
                // because the pseudo will be reloaded from stack which gets overwritten
                let is_local = if let ExprKind::Ident { name, .. } = &operand.kind {
                    self.locals.contains_key(name)
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
                            .with_size(typ.size_bits()),
                    );
                    temp
                } else {
                    val
                };

                // For pointers, decrement by element size; for others, decrement by 1
                let decrement = if is_ptr {
                    let elem_type = typ
                        .base
                        .as_ref()
                        .map(|b| (**b).clone())
                        .unwrap_or_else(|| Type::basic(TypeKind::Char));
                    let elem_size = elem_type.size_bits() / 8;
                    self.emit_const(elem_size as i64, Type::basic(TypeKind::Long))
                } else if is_float {
                    self.emit_fconst(1.0, typ.clone())
                } else {
                    self.emit_const(1, typ.clone())
                };
                let result = self.alloc_pseudo();
                let pseudo = Pseudo::reg(result, result.0);
                if let Some(func) = &mut self.current_func {
                    func.add_pseudo(pseudo);
                }
                let opcode = if is_float { Opcode::FSub } else { Opcode::Sub };
                let arith_type = if is_ptr {
                    Type::basic(TypeKind::Long)
                } else {
                    typ.clone()
                };
                self.emit(Instruction::binop(
                    opcode, result, val, decrement, arith_type,
                ));

                // For _Bool, normalize the result (any non-zero -> 1)
                let final_result = if typ.kind == TypeKind::Bool {
                    self.emit_convert(result, &Type::basic(TypeKind::Int), &typ)
                } else {
                    result
                };

                // Store to local, update parameter mapping, or store through pointer
                match &operand.kind {
                    ExprKind::Ident { name, .. } => {
                        if let Some(local) = self.locals.get(name).cloned() {
                            self.emit(Instruction::store(final_result, local.sym, 0, typ));
                        } else if self.var_map.contains_key(name) {
                            self.var_map.insert(name.clone(), final_result);
                        } else {
                            // Global variable - emit store
                            let sym_id = self.alloc_pseudo();
                            let pseudo = Pseudo::sym(sym_id, name.clone());
                            if let Some(func) = &mut self.current_func {
                                func.add_pseudo(pseudo);
                            }
                            self.emit(Instruction::store(final_result, sym_id, 0, typ));
                        }
                    }
                    ExprKind::Unary {
                        op: UnaryOp::Deref,
                        operand: ptr_expr,
                    } => {
                        // (*p)-- - store back through the pointer
                        let addr = self.linearize_expr(ptr_expr);
                        self.emit(Instruction::store(final_result, addr, 0, typ));
                    }
                    _ => {}
                }

                old_val // Return old value
            }

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

                self.emit(Instruction::select(
                    result, cond_val, then_val, else_val, typ,
                ));
                result
            }

            ExprKind::Call { func, args } => {
                // Get function name
                let func_name = match &func.kind {
                    ExprKind::Ident { name, .. } => name.clone(),
                    _ => "<indirect>".to_string(),
                };

                let typ = self.expr_type(expr); // Use evaluated type (function return type)

                // Check if this is a variadic function call
                // If the function expression has a type, check its variadic flag
                let variadic_arg_start = if let Some(ref func_type) = func.typ {
                    if func_type.variadic {
                        // Variadic args start after the fixed parameters
                        func_type.params.as_ref().map(|p| p.len())
                    } else {
                        None
                    }
                } else {
                    None // No type info, assume non-variadic
                };

                // Check if function returns a large struct
                // If so, allocate space and pass address as hidden first argument
                let returns_large_struct = (typ.kind == TypeKind::Struct
                    || typ.kind == TypeKind::Union)
                    && typ.size_bits() > MAX_REGISTER_AGGREGATE_BITS;

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
                            typ.clone(),
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
                        Type::pointer(typ.clone()),
                    ));

                    // Hidden return pointer is the first argument (pointer type)
                    (sret_sym, vec![sret_addr], vec![Type::pointer(typ.clone())])
                } else {
                    let result = self.alloc_pseudo();
                    (result, Vec::new(), Vec::new())
                };

                // Linearize regular arguments
                // For large structs, pass by reference (address) instead of by value
                for a in args.iter() {
                    let arg_type = self.expr_type(a);
                    let arg_val = if (arg_type.kind == TypeKind::Struct
                        || arg_type.kind == TypeKind::Union)
                        && arg_type.size_bits() > MAX_REGISTER_AGGREGATE_BITS
                    {
                        // Large struct: pass address instead of value
                        // The argument type becomes a pointer
                        arg_types_vec.push(Type::pointer(arg_type));
                        self.linearize_lvalue(a)
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
                    let mut call_insn = Instruction::call(
                        Some(result),
                        &func_name,
                        arg_vals,
                        arg_types_vec,
                        Type::pointer(typ),
                    );
                    call_insn.variadic_arg_start = variadic_arg_start;
                    self.emit(call_insn);
                    // Return the symbol (address) where struct is stored
                    result_sym
                } else {
                    let mut call_insn = Instruction::call(
                        Some(result_sym),
                        &func_name,
                        arg_vals,
                        arg_types_vec,
                        typ,
                    );
                    call_insn.variadic_arg_start = variadic_arg_start;
                    self.emit(call_insn);
                    result_sym
                }
            }

            ExprKind::Member {
                expr: inner_expr,
                member,
            } => {
                // Get address of the struct base
                let base = self.linearize_lvalue(inner_expr);
                let struct_type = self.expr_type(inner_expr);

                // Look up member offset and type
                let member_info = struct_type
                    .find_member(member)
                    .unwrap_or_else(|| MemberInfo {
                        offset: 0,
                        typ: self.expr_type(expr),
                        bit_offset: None,
                        bit_width: None,
                        storage_unit_size: None,
                    });

                // If member type is an array, return the address (arrays decay to pointers)
                if member_info.typ.kind == TypeKind::Array {
                    if member_info.offset == 0 {
                        base
                    } else {
                        let result = self.alloc_pseudo();
                        let offset_val =
                            self.emit_const(member_info.offset as i64, Type::basic(TypeKind::Long));
                        self.emit(Instruction::binop(
                            Opcode::Add,
                            result,
                            base,
                            offset_val,
                            Type::basic(TypeKind::Long),
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
                        &member_info.typ,
                    )
                } else {
                    let result = self.alloc_pseudo();
                    self.emit(Instruction::load(
                        result,
                        base,
                        member_info.offset as i64,
                        member_info.typ,
                    ));
                    result
                }
            }

            ExprKind::Arrow {
                expr: inner_expr,
                member,
            } => {
                // Pointer already contains the struct address
                let ptr = self.linearize_expr(inner_expr);
                let ptr_type = self.expr_type(inner_expr);

                // Dereference pointer to get struct type
                let struct_type = ptr_type
                    .base
                    .as_ref()
                    .map(|b| b.as_ref().clone())
                    .unwrap_or_else(|| self.expr_type(expr));

                // Look up member offset and type
                let member_info = struct_type
                    .find_member(member)
                    .unwrap_or_else(|| MemberInfo {
                        offset: 0,
                        typ: self.expr_type(expr),
                        bit_offset: None,
                        bit_width: None,
                        storage_unit_size: None,
                    });

                // If member type is an array, return the address (arrays decay to pointers)
                if member_info.typ.kind == TypeKind::Array {
                    if member_info.offset == 0 {
                        ptr
                    } else {
                        let result = self.alloc_pseudo();
                        let offset_val =
                            self.emit_const(member_info.offset as i64, Type::basic(TypeKind::Long));
                        self.emit(Instruction::binop(
                            Opcode::Add,
                            result,
                            ptr,
                            offset_val,
                            Type::basic(TypeKind::Long),
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
                        &member_info.typ,
                    )
                } else {
                    let result = self.alloc_pseudo();
                    self.emit(Instruction::load(
                        result,
                        ptr,
                        member_info.offset as i64,
                        member_info.typ,
                    ));
                    result
                }
            }

            ExprKind::Index { array, index } => {
                // In C, a[b] is defined as *(a + b), so either operand can be the pointer
                // Handle commutative form: 0[arr] is equivalent to arr[0]
                let array_type = self.expr_type(array);
                let index_type = self.expr_type(index);

                let (ptr_expr, idx_expr, idx_type) =
                    if array_type.kind == TypeKind::Pointer || array_type.kind == TypeKind::Array {
                        (array, index, index_type)
                    } else {
                        // Swap: index is actually the pointer/array
                        (index, array, array_type)
                    };

                let arr = self.linearize_expr(ptr_expr);
                let idx = self.linearize_expr(idx_expr);

                // Get element type from the expression type
                let elem_type = self.expr_type(expr);
                let elem_size = elem_type.size_bits() / 8;
                let elem_size_val = self.emit_const(elem_size as i64, Type::basic(TypeKind::Long));

                // Sign-extend index to 64-bit for proper pointer arithmetic (negative indices)
                let idx_extended = self.emit_convert(idx, &idx_type, &Type::basic(TypeKind::Long));

                let offset = self.alloc_pseudo();
                let ptr_typ = Type::basic(TypeKind::Long);
                self.emit(Instruction::binop(
                    Opcode::Mul,
                    offset,
                    idx_extended,
                    elem_size_val,
                    ptr_typ.clone(),
                ));

                let addr = self.alloc_pseudo();
                self.emit(Instruction::binop(Opcode::Add, addr, arr, offset, ptr_typ));

                // If element type is an array, just return the address (arrays decay to pointers)
                if elem_type.kind == TypeKind::Array {
                    addr
                } else {
                    let result = self.alloc_pseudo();
                    self.emit(Instruction::load(result, addr, 0, elem_type));
                    result
                }
            }

            ExprKind::Cast {
                cast_type,
                expr: inner_expr,
            } => {
                let src = self.linearize_expr(inner_expr);
                let src_type = self.expr_type(inner_expr);

                // Emit conversion if needed
                let src_is_float = src_type.is_float();
                let dst_is_float = cast_type.is_float();

                if src_is_float && !dst_is_float {
                    // Float to integer conversion
                    let result = self.alloc_pseudo();
                    let pseudo = Pseudo::reg(result, result.0);
                    if let Some(func) = &mut self.current_func {
                        func.add_pseudo(pseudo);
                    }
                    // FCvtS for signed int, FCvtU for unsigned
                    let opcode = if cast_type.is_unsigned() {
                        Opcode::FCvtU
                    } else {
                        Opcode::FCvtS
                    };
                    let mut insn = Instruction::new(opcode)
                        .with_target(result)
                        .with_src(src)
                        .with_type(cast_type.clone());
                    insn.src_size = src_type.size_bits();
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
                    let opcode = if src_type.is_unsigned() {
                        Opcode::UCvtF
                    } else {
                        Opcode::SCvtF
                    };
                    let mut insn = Instruction::new(opcode)
                        .with_target(result)
                        .with_src(src)
                        .with_type(cast_type.clone());
                    insn.src_size = src_type.size_bits();
                    self.emit(insn);
                    result
                } else if src_is_float && dst_is_float {
                    // Float to float conversion (e.g., float to double)
                    if src_type.size_bits() != cast_type.size_bits() {
                        let result = self.alloc_pseudo();
                        let pseudo = Pseudo::reg(result, result.0);
                        if let Some(func) = &mut self.current_func {
                            func.add_pseudo(pseudo);
                        }
                        let mut insn = Instruction::new(Opcode::FCvtF)
                            .with_target(result)
                            .with_src(src)
                            .with_type(cast_type.clone());
                        insn.src_size = src_type.size_bits();
                        self.emit(insn);
                        result
                    } else {
                        src // Same size, no conversion needed
                    }
                } else {
                    // Integer to integer conversion
                    // Use emit_convert for proper type conversions including _Bool
                    self.emit_convert(src, &src_type, cast_type)
                }
            }

            ExprKind::SizeofType(typ) => {
                let size = typ.size_bits() / 8;
                // sizeof returns size_t, which is unsigned long in our implementation
                let result_typ = Type::with_modifiers(TypeKind::Long, TypeModifiers::UNSIGNED);
                self.emit_const(size as i64, result_typ)
            }

            ExprKind::SizeofExpr(inner_expr) => {
                // Get type from expression and compute size
                let inner_typ = self.expr_type(inner_expr);
                let size = inner_typ.size_bits() / 8;
                // sizeof returns size_t, which is unsigned long in our implementation
                let result_typ = Type::with_modifiers(TypeKind::Long, TypeModifiers::UNSIGNED);
                self.emit_const(size as i64, result_typ)
            }

            ExprKind::Comma(exprs) => {
                let mut result = self.emit_const(0, Type::basic(TypeKind::Int));
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
                    .with_func(last_param.clone())
                    .with_type(Type::basic(TypeKind::Void));
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
                    .with_type(arg_type.clone());
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
                    .with_type(Type::basic(TypeKind::Void));
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
                    .with_type(Type::basic(TypeKind::Void));
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
                    .with_type(Type::with_modifiers(
                        TypeKind::Short,
                        TypeModifiers::UNSIGNED,
                    ));
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
                    .with_type(Type::with_modifiers(TypeKind::Int, TypeModifiers::UNSIGNED));
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
                    .with_type(Type::with_modifiers(
                        TypeKind::LongLong,
                        TypeModifiers::UNSIGNED,
                    ));
                self.emit(insn);
                result
            }

            ExprKind::Alloca { size } => {
                let size_val = self.linearize_expr(size);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Alloca)
                    .with_target(result)
                    .with_src(size_val)
                    .with_size(64) // Returns pointer (64-bit)
                    .with_type(Type::pointer(Type::basic(TypeKind::Void)));
                self.emit(insn);
                result
            }
        }
    }

    fn emit_const(&mut self, val: i64, typ: Type) -> PseudoId {
        let id = self.alloc_pseudo();
        let pseudo = Pseudo::val(id, val);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(pseudo);
        }

        // Emit setval instruction
        let insn = Instruction::new(Opcode::SetVal)
            .with_target(id)
            .with_type(typ);
        self.emit(insn);

        id
    }

    fn emit_fconst(&mut self, val: f64, typ: Type) -> PseudoId {
        let id = self.alloc_pseudo();
        let pseudo = Pseudo::fval(id, val);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(pseudo);
        }

        // Emit setval instruction
        let insn = Instruction::new(Opcode::SetVal)
            .with_target(id)
            .with_type(typ);
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
        typ: &Type,
    ) -> PseudoId {
        // Determine storage type based on storage unit size
        let storage_type = match storage_size {
            1 => Type::with_modifiers(TypeKind::Char, TypeModifiers::UNSIGNED),
            2 => Type::with_modifiers(TypeKind::Short, TypeModifiers::UNSIGNED),
            4 => Type::with_modifiers(TypeKind::Int, TypeModifiers::UNSIGNED),
            8 => Type::with_modifiers(TypeKind::Long, TypeModifiers::UNSIGNED),
            _ => Type::with_modifiers(TypeKind::Int, TypeModifiers::UNSIGNED),
        };

        // 1. Load the entire storage unit
        let storage_val = self.alloc_pseudo();
        self.emit(Instruction::load(
            storage_val,
            base,
            byte_offset as i64,
            storage_type.clone(),
        ));

        // 2. Shift right by bit_offset (using logical shift for unsigned extraction)
        let shifted = if bit_offset > 0 {
            let shift_amount = self.emit_const(bit_offset as i64, Type::basic(TypeKind::Int));
            let shifted = self.alloc_pseudo();
            self.emit(Instruction::binop(
                Opcode::Lsr,
                shifted,
                storage_val,
                shift_amount,
                storage_type.clone(),
            ));
            shifted
        } else {
            storage_val
        };

        // 3. Mask to bit_width bits
        let mask = (1u64 << bit_width) - 1;
        let mask_val = self.emit_const(mask as i64, storage_type.clone());
        let masked = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::And,
            masked,
            shifted,
            mask_val,
            storage_type.clone(),
        ));

        // 4. Sign extend if this is a signed bitfield
        if !typ.is_unsigned() && bit_width < storage_size * 8 {
            self.emit_sign_extend_bitfield(masked, bit_width, storage_size * 8)
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
        let typ = Type::basic(TypeKind::Int);

        let shift_val = self.emit_const(shift_amount as i64, typ.clone());
        let shifted_left = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Shl,
            shifted_left,
            value,
            shift_val,
            typ.clone(),
        ));

        let result = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Asr,
            result,
            shifted_left,
            shift_val,
            typ,
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
            1 => Type::with_modifiers(TypeKind::Char, TypeModifiers::UNSIGNED),
            2 => Type::with_modifiers(TypeKind::Short, TypeModifiers::UNSIGNED),
            4 => Type::with_modifiers(TypeKind::Int, TypeModifiers::UNSIGNED),
            8 => Type::with_modifiers(TypeKind::Long, TypeModifiers::UNSIGNED),
            _ => Type::with_modifiers(TypeKind::Int, TypeModifiers::UNSIGNED),
        };

        // 1. Load current storage unit value
        let old_val = self.alloc_pseudo();
        self.emit(Instruction::load(
            old_val,
            base,
            byte_offset as i64,
            storage_type.clone(),
        ));

        // 2. Create mask for the bitfield bits: ~(((1 << width) - 1) << offset)
        let field_mask = ((1u64 << bit_width) - 1) << bit_offset;
        let clear_mask = !field_mask;
        let clear_mask_val = self.emit_const(clear_mask as i64, storage_type.clone());

        // 3. Clear the bitfield bits in old value
        let cleared = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::And,
            cleared,
            old_val,
            clear_mask_val,
            storage_type.clone(),
        ));

        // 4. Mask new value to bit_width and shift to position
        let value_mask = (1u64 << bit_width) - 1;
        let value_mask_val = self.emit_const(value_mask as i64, storage_type.clone());
        let masked_new = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::And,
            masked_new,
            new_value,
            value_mask_val,
            storage_type.clone(),
        ));

        let positioned = if bit_offset > 0 {
            let shift_val = self.emit_const(bit_offset as i64, Type::basic(TypeKind::Int));
            let positioned = self.alloc_pseudo();
            self.emit(Instruction::binop(
                Opcode::Shl,
                positioned,
                masked_new,
                shift_val,
                storage_type.clone(),
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
            storage_type.clone(),
        ));

        // 6. Store back
        self.emit(Instruction::store(
            combined,
            base,
            byte_offset as i64,
            storage_type,
        ));
    }

    fn emit_unary(&mut self, op: UnaryOp, src: PseudoId, typ: Type) -> PseudoId {
        let result = self.alloc_pseudo();
        let is_float = typ.is_float();

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
                    let zero = self.emit_fconst(0.0, typ.clone());
                    self.emit(Instruction::binop(Opcode::FCmpOEq, result, src, zero, typ));
                } else {
                    let zero = self.emit_const(0, typ.clone());
                    self.emit(Instruction::binop(Opcode::SetEq, result, src, zero, typ));
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
                if typ.kind == TypeKind::Array {
                    return src;
                }
                self.emit(Instruction::load(result, src, 0, typ));
                return result;
            }
            UnaryOp::PreInc => {
                let is_ptr = typ.kind == TypeKind::Pointer;
                let increment = if is_ptr {
                    let elem_type = typ
                        .base
                        .as_ref()
                        .map(|b| (**b).clone())
                        .unwrap_or_else(|| Type::basic(TypeKind::Char));
                    let elem_size = elem_type.size_bits() / 8;
                    self.emit_const(elem_size as i64, Type::basic(TypeKind::Long))
                } else if is_float {
                    self.emit_fconst(1.0, typ.clone())
                } else {
                    self.emit_const(1, typ.clone())
                };
                let opcode = if is_float { Opcode::FAdd } else { Opcode::Add };
                self.emit(Instruction::binop(opcode, result, src, increment, typ));
                return result;
            }
            UnaryOp::PreDec => {
                let is_ptr = typ.kind == TypeKind::Pointer;
                let decrement = if is_ptr {
                    let elem_type = typ
                        .base
                        .as_ref()
                        .map(|b| (**b).clone())
                        .unwrap_or_else(|| Type::basic(TypeKind::Char));
                    let elem_size = elem_type.size_bits() / 8;
                    self.emit_const(elem_size as i64, Type::basic(TypeKind::Long))
                } else if is_float {
                    self.emit_fconst(1.0, typ.clone())
                } else {
                    self.emit_const(1, typ.clone())
                };
                let opcode = if is_float { Opcode::FSub } else { Opcode::Sub };
                self.emit(Instruction::binop(opcode, result, src, decrement, typ));
                return result;
            }
        };

        self.emit(Instruction::unop(opcode, result, src, typ));
        result
    }

    fn emit_binary(
        &mut self,
        op: BinaryOp,
        left: PseudoId,
        right: PseudoId,
        result_typ: Type,
        operand_typ: Type,
    ) -> PseudoId {
        let result = self.alloc_pseudo();

        let is_float = operand_typ.is_float();

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
                } else if operand_typ.is_unsigned() {
                    Opcode::DivU
                } else {
                    Opcode::DivS
                }
            }
            BinaryOp::Mod => {
                // Modulo is not supported for floats in hardware - use fmod() library call
                // For now, use integer modulo (semantic analysis should catch float % float)
                if operand_typ.is_unsigned() {
                    Opcode::ModU
                } else {
                    Opcode::ModS
                }
            }
            BinaryOp::Lt => {
                if is_float {
                    Opcode::FCmpOLt
                } else if operand_typ.is_unsigned() {
                    Opcode::SetB
                } else {
                    Opcode::SetLt
                }
            }
            BinaryOp::Gt => {
                if is_float {
                    Opcode::FCmpOGt
                } else if operand_typ.is_unsigned() {
                    Opcode::SetA
                } else {
                    Opcode::SetGt
                }
            }
            BinaryOp::Le => {
                if is_float {
                    Opcode::FCmpOLe
                } else if operand_typ.is_unsigned() {
                    Opcode::SetBe
                } else {
                    Opcode::SetLe
                }
            }
            BinaryOp::Ge => {
                if is_float {
                    Opcode::FCmpOGe
                } else if operand_typ.is_unsigned() {
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
                if operand_typ.is_unsigned() {
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
        self.emit(Instruction::binop(opcode, result, left, right, insn_typ));
        result
    }

    fn emit_compare_zero(&mut self, val: PseudoId, operand_typ: &Type) -> PseudoId {
        let result = self.alloc_pseudo();
        let zero = self.emit_const(0, operand_typ.clone());
        self.emit(Instruction::binop(
            Opcode::SetNe,
            result,
            val,
            zero,
            operand_typ.clone(),
        ));
        result
    }

    /// Emit short-circuit logical AND: a && b
    /// If a is false, skip evaluation of b and return 0.
    /// Otherwise, evaluate b and return (b != 0).
    fn emit_logical_and(&mut self, left: &Expr, right: &Expr) -> PseudoId {
        let result_typ = Type::basic(TypeKind::Int);

        // Create basic blocks
        let eval_b_bb = self.alloc_bb();
        let merge_bb = self.alloc_bb();

        // Evaluate LHS
        let left_typ = self.expr_type(left);
        let left_val = self.linearize_expr(left);
        let left_bool = self.emit_compare_zero(left_val, &left_typ);

        // Emit the short-circuit value (0) BEFORE the branch, while still in LHS block
        // This value will be used if we short-circuit (LHS is false)
        let zero = self.emit_const(0, result_typ.clone());

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
        let right_bool = self.emit_compare_zero(right_val, &right_typ);

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
        let mut phi_insn = Instruction::phi(result, result_typ);
        phi_insn.phi_list.push((lhs_end_bb, zero));
        phi_insn.phi_list.push((rhs_end_bb, right_bool));
        self.emit(phi_insn);

        result
    }

    /// Emit short-circuit logical OR: a || b
    /// If a is true, skip evaluation of b and return 1.
    /// Otherwise, evaluate b and return (b != 0).
    fn emit_logical_or(&mut self, left: &Expr, right: &Expr) -> PseudoId {
        let result_typ = Type::basic(TypeKind::Int);

        // Create basic blocks
        let eval_b_bb = self.alloc_bb();
        let merge_bb = self.alloc_bb();

        // Evaluate LHS
        let left_typ = self.expr_type(left);
        let left_val = self.linearize_expr(left);
        let left_bool = self.emit_compare_zero(left_val, &left_typ);

        // Emit the short-circuit value (1) BEFORE the branch, while still in LHS block
        // This value will be used if we short-circuit (LHS is true)
        let one = self.emit_const(1, result_typ.clone());

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
        let right_bool = self.emit_compare_zero(right_val, &right_typ);

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
        let mut phi_insn = Instruction::phi(result, result_typ);
        phi_insn.phi_list.push((lhs_end_bb, one));
        phi_insn.phi_list.push((rhs_end_bb, right_bool));
        self.emit(phi_insn);

        result
    }

    fn emit_assign(&mut self, op: AssignOp, target: &Expr, value: &Expr) -> PseudoId {
        let rhs = self.linearize_expr(value);
        let target_typ = self.expr_type(target);
        let value_typ = self.expr_type(value);

        // Check for pointer compound assignment (p += n or p -= n)
        let is_ptr_arith = target_typ.kind == TypeKind::Pointer
            && value_typ.is_integer()
            && (op == AssignOp::AddAssign || op == AssignOp::SubAssign);

        // Convert RHS to target type if needed (but not for pointer arithmetic)
        let rhs = if is_ptr_arith {
            // For pointer arithmetic, scale the integer by element size
            let elem_type = target_typ
                .base
                .as_ref()
                .map(|b| (**b).clone())
                .unwrap_or_else(|| Type::basic(TypeKind::Char));
            let elem_size = elem_type.size_bits() / 8;
            let scale = self.emit_const(elem_size as i64, Type::basic(TypeKind::Long));

            // Extend the integer to 64-bit for proper arithmetic
            let rhs_extended = self.emit_convert(rhs, &value_typ, &Type::basic(TypeKind::Long));

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
                Type::basic(TypeKind::Long),
            ));
            scaled
        } else {
            self.emit_convert(rhs, &value_typ, &target_typ)
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

                let is_float = target_typ.is_float();
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
                        } else if target_typ.is_unsigned() {
                            Opcode::DivU
                        } else {
                            Opcode::DivS
                        }
                    }
                    AssignOp::ModAssign => {
                        // Modulo not supported for floats
                        if target_typ.is_unsigned() {
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
                        if target_typ.is_unsigned() {
                            Opcode::Lsr
                        } else {
                            Opcode::Asr
                        }
                    }
                    AssignOp::Assign => unreachable!(),
                };

                // For pointer arithmetic, use Long type for the operation
                let arith_type = if is_ptr_arith {
                    Type::basic(TypeKind::Long)
                } else {
                    target_typ.clone()
                };
                self.emit(Instruction::binop(opcode, result, lhs, rhs, arith_type));
                result
            }
        };

        // Store based on target expression type
        match &target.kind {
            ExprKind::Ident { name, .. } => {
                if let Some(local) = self.locals.get(name).cloned() {
                    // Check if this is a static local (sentinel value)
                    if local.sym.0 == u32::MAX {
                        // Static local - look up the global name and emit store to global
                        let key = format!("{}.{}", self.current_func_name, name);
                        if let Some(static_info) = self.static_locals.get(&key).cloned() {
                            let sym_id = self.alloc_pseudo();
                            let pseudo = Pseudo::sym(sym_id, static_info.global_name);
                            if let Some(func) = &mut self.current_func {
                                func.add_pseudo(pseudo);
                            }
                            self.emit(Instruction::store(final_val, sym_id, 0, target_typ.clone()));
                        }
                    } else {
                        // Regular local variable: emit Store
                        self.emit(Instruction::store(
                            final_val,
                            local.sym,
                            0,
                            target_typ.clone(),
                        ));
                    }
                } else if self.var_map.contains_key(name) {
                    // Parameter: this is not SSA-correct but parameters
                    // shouldn't be reassigned. If they are, we'd need to
                    // demote them to locals. For now, just update the mapping.
                    self.var_map.insert(name.clone(), final_val);
                } else {
                    // Global variable - emit store
                    let sym_id = self.alloc_pseudo();
                    let pseudo = Pseudo::sym(sym_id, name.clone());
                    if let Some(func) = &mut self.current_func {
                        func.add_pseudo(pseudo);
                    }
                    self.emit(Instruction::store(final_val, sym_id, 0, target_typ.clone()));
                }
            }
            ExprKind::Member { expr, member } => {
                // Struct member: get address and store with offset
                let base = self.linearize_lvalue(expr);
                let struct_type = self.expr_type(expr);
                let member_info = struct_type
                    .find_member(member)
                    .unwrap_or_else(|| MemberInfo {
                        offset: 0,
                        typ: target_typ.clone(),
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
                    self.emit(Instruction::store(
                        final_val,
                        base,
                        member_info.offset as i64,
                        member_info.typ,
                    ));
                }
            }
            ExprKind::Arrow { expr, member } => {
                // Pointer member: pointer value is the base address
                let ptr = self.linearize_expr(expr);
                let ptr_type = self.expr_type(expr);
                let struct_type = ptr_type
                    .base
                    .as_ref()
                    .map(|b| b.as_ref().clone())
                    .unwrap_or_else(|| target_typ.clone());
                let member_info = struct_type
                    .find_member(member)
                    .unwrap_or_else(|| MemberInfo {
                        offset: 0,
                        typ: target_typ.clone(),
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
                    self.emit(Instruction::store(
                        final_val,
                        ptr,
                        member_info.offset as i64,
                        member_info.typ,
                    ));
                }
            }
            ExprKind::Unary {
                op: UnaryOp::Deref,
                operand,
            } => {
                // Dereference: store to the pointer address
                let ptr = self.linearize_expr(operand);
                self.emit(Instruction::store(final_val, ptr, 0, target_typ.clone()));
            }
            ExprKind::Index { array, index } => {
                // Array subscript: compute address and store
                // Handle commutative form: 0[arr] is equivalent to arr[0]
                let array_type = self.expr_type(array);
                let index_type = self.expr_type(index);

                let (ptr_expr, idx_expr, idx_type) =
                    if array_type.kind == TypeKind::Pointer || array_type.kind == TypeKind::Array {
                        (array, index, index_type)
                    } else {
                        // Swap: index is actually the pointer/array
                        (index, array, array_type)
                    };

                let arr = self.linearize_expr(ptr_expr);
                let idx = self.linearize_expr(idx_expr);
                let elem_size = target_typ.size_bits() / 8;
                let elem_size_val = self.emit_const(elem_size as i64, Type::basic(TypeKind::Long));

                // Sign-extend index to 64-bit for proper pointer arithmetic (negative indices)
                let idx_extended = self.emit_convert(idx, &idx_type, &Type::basic(TypeKind::Long));

                let offset = self.alloc_pseudo();
                let ptr_typ = Type::basic(TypeKind::Long);
                self.emit(Instruction::binop(
                    Opcode::Mul,
                    offset,
                    idx_extended,
                    elem_size_val,
                    ptr_typ.clone(),
                ));

                let addr = self.alloc_pseudo();
                self.emit(Instruction::binop(Opcode::Add, addr, arr, offset, ptr_typ));

                self.emit(Instruction::store(final_val, addr, 0, target_typ.clone()));
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
pub fn linearize(tu: &TranslationUnit, symbols: &SymbolTable) -> Module {
    linearize_with_debug(tu, symbols, false, None)
}

/// Linearize an AST to IR with debug info support
pub fn linearize_with_debug(
    tu: &TranslationUnit,
    symbols: &SymbolTable,
    debug: bool,
    source_file: Option<&str>,
) -> Module {
    let mut linearizer = Linearizer::new(symbols);
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

    fn test_linearize(tu: &TranslationUnit) -> Module {
        let symbols = SymbolTable::new();
        linearize(tu, &symbols)
    }

    fn make_simple_func(name: &str, body: Stmt) -> FunctionDef {
        FunctionDef {
            return_type: Type::basic(TypeKind::Int),
            name: name.to_string(),
            params: vec![],
            body,
            pos: test_pos(),
        }
    }

    #[test]
    fn test_linearize_empty_function() {
        let func = make_simple_func("test", Stmt::Block(vec![]));
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu);
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].name, "test");
        assert!(!module.functions[0].blocks.is_empty());
    }

    #[test]
    fn test_linearize_return() {
        let func = make_simple_func("test", Stmt::Return(Some(Expr::int(42))));
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu);
        let ir = format!("{}", module);
        assert!(ir.contains("ret"));
    }

    #[test]
    fn test_linearize_if() {
        let func = make_simple_func(
            "test",
            Stmt::If {
                cond: Expr::int(1),
                then_stmt: Box::new(Stmt::Return(Some(Expr::int(1)))),
                else_stmt: Some(Box::new(Stmt::Return(Some(Expr::int(0))))),
            },
        );
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu);
        let ir = format!("{}", module);
        assert!(ir.contains("cbr")); // Conditional branch
    }

    #[test]
    fn test_linearize_while() {
        let func = make_simple_func(
            "test",
            Stmt::While {
                cond: Expr::int(1),
                body: Box::new(Stmt::Break),
            },
        );
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu);
        assert!(module.functions[0].blocks.len() >= 3); // cond, body, exit
    }

    #[test]
    fn test_linearize_for() {
        // for (int i = 0; i < 10; i++) { }
        let int_type = Type::basic(TypeKind::Int);
        let i_var = Expr::var_typed("i", int_type.clone());
        let func = make_simple_func(
            "test",
            Stmt::For {
                init: Some(ForInit::Declaration(Declaration {
                    declarators: vec![crate::parse::ast::InitDeclarator {
                        name: "i".to_string(),
                        typ: int_type.clone(),
                        init: Some(Expr::int(0)),
                    }],
                })),
                cond: Some(Expr::binary(BinaryOp::Lt, i_var.clone(), Expr::int(10))),
                post: Some(Expr::typed(
                    ExprKind::PostInc(Box::new(i_var)),
                    int_type,
                    test_pos(),
                )),
                body: Box::new(Stmt::Empty),
            },
        );
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu);
        assert!(module.functions[0].blocks.len() >= 4); // entry, cond, body, post, exit
    }

    #[test]
    fn test_linearize_binary_expr() {
        // return 1 + 2 * 3;
        let func = make_simple_func(
            "test",
            Stmt::Return(Some(Expr::binary(
                BinaryOp::Add,
                Expr::int(1),
                Expr::binary(BinaryOp::Mul, Expr::int(2), Expr::int(3)),
            ))),
        );
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu);
        let ir = format!("{}", module);
        assert!(ir.contains("mul"));
        assert!(ir.contains("add"));
    }

    #[test]
    fn test_linearize_function_with_params() {
        let int_type = Type::basic(TypeKind::Int);
        let func = FunctionDef {
            return_type: int_type.clone(),
            name: "add".to_string(),
            params: vec![
                Parameter {
                    name: Some("a".to_string()),
                    typ: int_type.clone(),
                },
                Parameter {
                    name: Some("b".to_string()),
                    typ: int_type.clone(),
                },
            ],
            body: Stmt::Return(Some(Expr::binary(
                BinaryOp::Add,
                Expr::var_typed("a", int_type.clone()),
                Expr::var_typed("b", int_type),
            ))),
            pos: test_pos(),
        };
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu);
        let ir = format!("{}", module);
        assert!(ir.contains("add"));
        assert!(ir.contains("%a"));
        assert!(ir.contains("%b"));
    }

    #[test]
    fn test_linearize_call() {
        let func = make_simple_func(
            "test",
            Stmt::Return(Some(Expr::call(
                Expr::var("foo"),
                vec![Expr::int(1), Expr::int(2)],
            ))),
        );
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu);
        let ir = format!("{}", module);
        assert!(ir.contains("call"));
        assert!(ir.contains("foo"));
    }

    #[test]
    fn test_linearize_comparison() {
        let func = make_simple_func(
            "test",
            Stmt::Return(Some(Expr::binary(BinaryOp::Lt, Expr::int(1), Expr::int(2)))),
        );
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu);
        let ir = format!("{}", module);
        assert!(ir.contains("setlt"));
    }

    #[test]
    fn test_linearize_unsigned_comparison() {
        use crate::types::TypeModifiers;

        // Create unsigned comparison: (unsigned)1 < (unsigned)2
        let uint_type = Type::with_modifiers(TypeKind::Int, TypeModifiers::UNSIGNED);
        let mut left = Expr::int(1);
        left.typ = Some(uint_type.clone());
        let mut right = Expr::int(2);
        right.typ = Some(uint_type);
        let mut cmp = Expr::binary(BinaryOp::Lt, left, right);
        cmp.typ = Some(Type::basic(TypeKind::Int));

        let func = make_simple_func("test", Stmt::Return(Some(cmp)));
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu);
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
        let func = make_simple_func("main", Stmt::Return(Some(Expr::int(0))));
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu);
        let ir = format!("{}", module);

        // Should have proper structure
        assert!(ir.contains("define"));
        assert!(ir.contains("main"));
        assert!(ir.contains(".L0:")); // Entry block label
        assert!(ir.contains("ret"));
    }

    #[test]
    fn test_type_propagation_expr_type() {
        use crate::types::TypeModifiers;

        // Create an expression with a type annotation
        let mut expr = Expr::int(42);
        // Simulate type evaluation having set the type
        expr.typ = Some(Type::basic(TypeKind::Int));

        // Create linearizer and test that expr_type reads from the expression
        let symbols = SymbolTable::new();
        let linearizer = Linearizer::new(&symbols);
        let typ = linearizer.expr_type(&expr);
        assert_eq!(typ.kind, TypeKind::Int);

        // Test with unsigned type
        let mut unsigned_expr = Expr::int(42);
        unsigned_expr.typ = Some(Type::with_modifiers(TypeKind::Int, TypeModifiers::UNSIGNED));
        let typ = linearizer.expr_type(&unsigned_expr);
        assert!(typ.is_unsigned());
    }

    #[test]
    fn test_type_propagation_double_literal() {
        // Create a double literal
        let mut expr = Expr::new(ExprKind::FloatLit(3.14), test_pos());
        expr.typ = Some(Type::basic(TypeKind::Double));

        let symbols = SymbolTable::new();
        let linearizer = Linearizer::new(&symbols);
        let typ = linearizer.expr_type(&expr);
        assert_eq!(typ.kind, TypeKind::Double);
    }

    // ========================================================================
    // SSA Conversion Tests
    // ========================================================================

    /// Helper to linearize without SSA conversion (for comparing before/after)
    fn linearize_no_ssa(tu: &TranslationUnit) -> Module {
        let symbols = SymbolTable::new();
        let mut linearizer = Linearizer::new_no_ssa(&symbols);
        linearizer.linearize(tu)
    }

    #[test]
    fn test_local_var_emits_load_store() {
        // int test() { int x = 1; return x; }
        let int_type = Type::basic(TypeKind::Int);
        let func = FunctionDef {
            return_type: int_type.clone(),
            name: "test".to_string(),
            params: vec![],
            body: Stmt::Block(vec![
                BlockItem::Declaration(Declaration {
                    declarators: vec![crate::parse::ast::InitDeclarator {
                        name: "x".to_string(),
                        typ: int_type.clone(),
                        init: Some(Expr::int(1)),
                    }],
                }),
                BlockItem::Statement(Stmt::Return(Some(Expr::var_typed("x", int_type)))),
            ]),
            pos: test_pos(),
        };
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        // Without SSA, should have store and load
        let module = linearize_no_ssa(&tu);
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
        // int test(int cond) {
        //     int x = 1;
        //     if (cond) x = 2;
        //     return x;
        // }
        let int_type = Type::basic(TypeKind::Int);

        let func = FunctionDef {
            return_type: int_type.clone(),
            name: "test".to_string(),
            params: vec![Parameter {
                name: Some("cond".to_string()),
                typ: int_type.clone(),
            }],
            body: Stmt::Block(vec![
                // int x = 1;
                BlockItem::Declaration(Declaration {
                    declarators: vec![crate::parse::ast::InitDeclarator {
                        name: "x".to_string(),
                        typ: int_type.clone(),
                        init: Some(Expr::int(1)),
                    }],
                }),
                // if (cond) x = 2;
                BlockItem::Statement(Stmt::If {
                    cond: Expr::var_typed("cond", int_type.clone()),
                    then_stmt: Box::new(Stmt::Expr(Expr::assign(
                        Expr::var_typed("x", int_type.clone()),
                        Expr::int(2),
                    ))),
                    else_stmt: None,
                }),
                // return x;
                BlockItem::Statement(Stmt::Return(Some(Expr::var_typed("x", int_type)))),
            ]),
            pos: test_pos(),
        };
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        // With SSA, should have phi node at merge point
        let module = test_linearize(&tu);
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
        // int test() {
        //     int i = 0;
        //     while (i < 10) { i = i + 1; }
        //     return i;
        // }
        let int_type = Type::basic(TypeKind::Int);

        let i_var = || Expr::var_typed("i", int_type.clone());

        let func = FunctionDef {
            return_type: int_type.clone(),
            name: "test".to_string(),
            params: vec![],
            body: Stmt::Block(vec![
                // int i = 0;
                BlockItem::Declaration(Declaration {
                    declarators: vec![crate::parse::ast::InitDeclarator {
                        name: "i".to_string(),
                        typ: int_type.clone(),
                        init: Some(Expr::int(0)),
                    }],
                }),
                // while (i < 10) { i = i + 1; }
                BlockItem::Statement(Stmt::While {
                    cond: Expr::binary(BinaryOp::Lt, i_var(), Expr::int(10)),
                    body: Box::new(Stmt::Expr(Expr::assign(
                        i_var(),
                        Expr::binary(BinaryOp::Add, i_var(), Expr::int(1)),
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
        let module = test_linearize(&tu);
        let ir = format!("{}", module);

        // Loop should have a phi at the condition block
        assert!(ir.contains("phi"), "Loop should have phi node: {}", ir);
    }

    #[test]
    fn test_short_circuit_and() {
        // int test(int a, int b) {
        //     return a && b;
        // }
        // Short-circuit: if a is false, don't evaluate b
        let int_type = Type::basic(TypeKind::Int);

        let func = FunctionDef {
            return_type: int_type.clone(),
            name: "test".to_string(),
            params: vec![
                Parameter {
                    name: Some("a".to_string()),
                    typ: int_type.clone(),
                },
                Parameter {
                    name: Some("b".to_string()),
                    typ: int_type.clone(),
                },
            ],
            body: Stmt::Return(Some(Expr::binary(
                BinaryOp::LogAnd,
                Expr::var_typed("a", int_type.clone()),
                Expr::var_typed("b", int_type),
            ))),
            pos: test_pos(),
        };
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu);
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
        // int test(int a, int b) {
        //     return a || b;
        // }
        // Short-circuit: if a is true, don't evaluate b
        let int_type = Type::basic(TypeKind::Int);

        let func = FunctionDef {
            return_type: int_type.clone(),
            name: "test".to_string(),
            params: vec![
                Parameter {
                    name: Some("a".to_string()),
                    typ: int_type.clone(),
                },
                Parameter {
                    name: Some("b".to_string()),
                    typ: int_type.clone(),
                },
            ],
            body: Stmt::Return(Some(Expr::binary(
                BinaryOp::LogOr,
                Expr::var_typed("a", int_type.clone()),
                Expr::var_typed("b", int_type),
            ))),
            pos: test_pos(),
        };
        let tu = TranslationUnit {
            items: vec![ExternalDecl::FunctionDef(func)],
        };

        let module = test_linearize(&tu);
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
