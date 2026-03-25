//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT

//! Statement linearization

use super::linearize::*;
use super::{
    AsmConstraint, AsmData, BasicBlockId, Initializer, Instruction, Opcode, Pseudo, PseudoId,
};
use crate::diag::error;
use crate::parse::ast::{
    AsmOperand, BinaryOp, BlockItem, Declaration, Expr, ExprKind, ForInit, InitElement,
    OffsetOfPath, Stmt, UnaryOp,
};
use crate::strings::StringId;
use crate::types::{TypeId, TypeKind, TypeModifiers};

impl<'a> super::linearize::Linearizer<'a> {
    pub(crate) fn linearize_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Empty => {}

            Stmt::Expr(expr) => {
                self.linearize_expr(expr);
            }

            Stmt::Block(items) => {
                self.push_scope();

                for item in items {
                    match item {
                        BlockItem::Declaration(decl) => self.linearize_local_decl(decl),
                        BlockItem::Statement(s) => self.linearize_stmt(s),
                    }
                }

                self.pop_scope();
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
                    let expr_typ = self.expr_type(e);
                    // Get the function's actual return type for proper conversion
                    let func_ret_type = self
                        .current_func
                        .as_ref()
                        .map(|f| f.return_type)
                        .unwrap_or(expr_typ);

                    if let Some(sret_ptr) = self.struct_return_ptr {
                        self.emit_sret_return(e, sret_ptr, self.struct_return_size);
                    } else if let Some(ret_type) = self.two_reg_return_type {
                        self.emit_two_reg_return(e, ret_type);
                    } else if self.types.is_complex(expr_typ) {
                        let addr = self.linearize_lvalue(e);
                        let typ_size = self.types.size_bits(func_ret_type);
                        self.emit(Instruction::ret_typed(Some(addr), func_ret_type, typ_size));
                    } else {
                        let val = self.linearize_expr(e);
                        // Convert expression value to function's return type if needed
                        let converted_val = if expr_typ != func_ret_type
                            && self.types.kind(func_ret_type) != TypeKind::Void
                        {
                            self.emit_convert(val, expr_typ, func_ret_type)
                        } else {
                            val
                        };
                        // Function types decay to pointers when returned
                        let typ_size = if self.types.kind(func_ret_type) == TypeKind::Function {
                            self.target.pointer_width
                        } else {
                            self.types.size_bits(func_ret_type)
                        };
                        self.emit(Instruction::ret_typed(
                            Some(converted_val),
                            func_ret_type,
                            typ_size,
                        ));
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

            Stmt::Asm {
                template,
                outputs,
                inputs,
                clobbers,
                goto_labels,
            } => {
                self.linearize_asm(template, outputs, inputs, clobbers, goto_labels);
            }
        }
    }

    pub(crate) fn linearize_local_decl(&mut self, decl: &Declaration) {
        for declarator in &decl.declarators {
            let typ = declarator.typ;

            // Check if this is a static local variable
            if declarator.storage_class.contains(TypeModifiers::STATIC) {
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
            // Use unique name (name.id) to distinguish shadowed variables
            let sym_id = self.alloc_pseudo();
            let name_str = self.symbol_name(declarator.symbol);
            let unique_name = format!("{}.{}", name_str, sym_id.0);
            let sym = Pseudo::sym(sym_id, unique_name.clone());
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(sym);
                // Register with function's local variable tracking for SSA
                // Pass the current basic block as the declaration block for scope-aware phi placement
                let mods = self.types.modifiers(typ);
                let is_volatile = mods.contains(TypeModifiers::VOLATILE);
                let is_atomic = mods.contains(TypeModifiers::ATOMIC);
                func.add_local(
                    &unique_name,
                    sym_id,
                    typ,
                    is_volatile,
                    is_atomic,
                    self.current_bb,
                    declarator.explicit_align,
                );
            }

            // Track in linearizer's locals map using SymbolId as key
            self.insert_local(
                declarator.symbol,
                LocalVarInfo {
                    sym: sym_id,
                    typ,
                    vla_size_sym: None,
                    vla_elem_type: None,
                    vla_dim_syms: vec![],
                    is_indirect: false,
                },
            );

            // If there's an initializer, emit Store(s)
            if let Some(init) = &declarator.init {
                if let ExprKind::InitList { elements } = &init.kind {
                    // Handle initializer list for arrays and structs
                    // C99 6.7.8p19: uninitialized members must be zero-initialized
                    // Zero the entire aggregate first, then apply explicit initializers
                    let type_kind = self.types.kind(typ);
                    if type_kind == TypeKind::Struct
                        || type_kind == TypeKind::Union
                        || type_kind == TypeKind::Array
                    {
                        self.emit_aggregate_zero(sym_id, typ);
                    }
                    self.linearize_init_list(sym_id, typ, elements);
                } else if let ExprKind::StringLit(s) = &init.kind {
                    // String literal initialization of char array
                    // Copy the string bytes to the local array
                    if self.types.kind(typ) == TypeKind::Array {
                        let elem_type = self.types.base_type(typ).unwrap_or(self.types.char_id);
                        let elem_size = self.types.size_bits(elem_type);

                        // Copy each byte from string literal to local array
                        for (i, byte) in s.bytes().enumerate() {
                            let byte_val = self.emit_const(byte as i128, elem_type);
                            self.emit(Instruction::store(
                                byte_val, sym_id, i as i64, elem_type, elem_size,
                            ));
                        }
                        // Store null terminator
                        let null_val = self.emit_const(0, elem_type);
                        self.emit(Instruction::store(
                            null_val,
                            sym_id,
                            s.chars().count() as i64,
                            elem_type,
                            elem_size,
                        ));
                    } else {
                        // Pointer initialized with string literal - store the address
                        let val = self.linearize_expr(init);
                        let init_type = self.expr_type(init);
                        let converted = self.emit_convert(val, init_type, typ);
                        let size = self.types.size_bits(typ);
                        self.emit(Instruction::store(converted, sym_id, 0, typ, size));
                    }
                } else if let ExprKind::WideStringLit(s) = &init.kind {
                    // Wide string literal initialization of wchar_t array
                    // Copy the wide string chars to the local array (4 bytes each)
                    if self.types.kind(typ) == TypeKind::Array {
                        let elem_type = self.types.base_type(typ).unwrap_or(self.types.int_id);
                        let elem_size = self.types.size_bits(elem_type);
                        let elem_bytes = (elem_size / 8) as i64;

                        // Copy each wchar_t from wide string literal to local array
                        for (i, ch) in s.chars().enumerate() {
                            let ch_val = self.emit_const(ch as i128, elem_type);
                            self.emit(Instruction::store(
                                ch_val,
                                sym_id,
                                (i as i64) * elem_bytes,
                                elem_type,
                                elem_size,
                            ));
                        }
                        // Store null terminator
                        let null_val = self.emit_const(0, elem_type);
                        self.emit(Instruction::store(
                            null_val,
                            sym_id,
                            (s.chars().count() as i64) * elem_bytes,
                            elem_type,
                            elem_size,
                        ));
                    } else {
                        // Pointer initialized with wide string literal - store the address
                        let val = self.linearize_expr(init);
                        let init_type = self.expr_type(init);
                        let converted = self.emit_convert(val, init_type, typ);
                        let size = self.types.size_bits(typ);
                        self.emit(Instruction::store(converted, sym_id, 0, typ, size));
                    }
                } else if self.types.is_complex(typ) {
                    let init_typ = self.expr_type(init);
                    let base_typ = self.types.complex_base(typ);
                    let base_bits = self.types.size_bits(base_typ);
                    let base_bytes = (base_bits / 8) as i64;

                    if self.types.is_complex(init_typ) {
                        // Complex-to-complex: linearize_expr returns an address
                        // to a temp containing the complex value. Copy from temp.
                        let value_addr = self.linearize_expr(init);
                        let val_real = self.alloc_pseudo();
                        let val_imag = self.alloc_pseudo();
                        self.emit(Instruction::load(
                            val_real, value_addr, 0, base_typ, base_bits,
                        ));
                        self.emit(Instruction::load(
                            val_imag, value_addr, base_bytes, base_typ, base_bits,
                        ));
                        self.emit(Instruction::store(val_real, sym_id, 0, base_typ, base_bits));
                        self.emit(Instruction::store(
                            val_imag, sym_id, base_bytes, base_typ, base_bits,
                        ));
                    } else {
                        // Real scalar to complex: set real = value, imag = 0.0
                        let val = self.linearize_expr(init);
                        let converted = self.emit_convert(val, init_typ, base_typ);
                        self.emit(Instruction::store(
                            converted, sym_id, 0, base_typ, base_bits,
                        ));
                        // Store 0.0 for imaginary part
                        let zero = self.emit_fconst(0.0, base_typ);
                        self.emit(Instruction::store(
                            zero, sym_id, base_bytes, base_typ, base_bits,
                        ));
                    }
                } else {
                    // Check for large struct/union initialization (> 64 bits)
                    // linearize_expr returns an address for large aggregates
                    let type_kind = self.types.kind(typ);
                    let type_size = self.types.size_bits(typ);
                    if (type_kind == TypeKind::Struct || type_kind == TypeKind::Union)
                        && type_size > 64
                    {
                        // Large struct/union init - source is an address, do block copy
                        let value_addr = self.linearize_expr(init);
                        let type_size_bytes = type_size / 8;

                        self.emit_block_copy(sym_id, value_addr, type_size_bytes as i64);
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
    pub(crate) fn linearize_vla_decl(&mut self, declarator: &crate::parse::ast::InitDeclarator) {
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
            let decl_name = self.symbol_name(declarator.symbol);
            let dim_var_name = format!("__vla_dim{}_{}.{}", dim_idx, decl_name, dim_sym_id.0);
            let dim_sym = Pseudo::sym(dim_sym_id, dim_var_name.clone());

            if let Some(func) = &mut self.current_func {
                func.add_pseudo(dim_sym);
                func.add_local(
                    &dim_var_name,
                    dim_sym_id,
                    ulong_type,
                    false, // not volatile
                    false, // not atomic
                    self.current_bb,
                    None, // no explicit alignment
                );
            }

            // Widen dimension size to 64-bit before storing
            let dim_expr_typ = vla_size_expr.typ.unwrap_or(self.types.int_id);
            let dim_size = self.emit_convert(dim_size, dim_expr_typ, ulong_type);
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
                        .with_size(64)
                        .with_type(self.types.ulong_id);
                    self.emit(mul_insn);
                    result
                }
            });
        }
        let num_elements = total_count.expect("VLA must have at least one size expression");

        // Create a hidden local variable to store the total number of elements
        // This is needed for sizeof(vla) to work at runtime
        let size_sym_id = self.alloc_pseudo();
        let vla_name = self.symbol_name(declarator.symbol);
        let size_var_name = format!("__vla_size_{}.{}", vla_name, size_sym_id.0);
        let size_sym = Pseudo::sym(size_sym_id, size_var_name.clone());

        if let Some(func) = &mut self.current_func {
            func.add_pseudo(size_sym);
            func.add_local(
                &size_var_name,
                size_sym_id,
                ulong_type,
                false, // not volatile
                false, // not atomic
                self.current_bb,
                None, // no explicit alignment
            );
        }

        // Store num_elements into the hidden size variable
        let store_size_insn = Instruction::store(num_elements, size_sym_id, 0, ulong_type, 64);
        self.emit(store_size_insn);

        // Compute total size in bytes: num_elements * sizeof(element)
        let elem_size_const = self.emit_const(elem_size as i128, self.types.long_id);
        let total_size = self.alloc_pseudo();
        let mul_insn = Instruction::new(Opcode::Mul)
            .with_target(total_size)
            .with_src(num_elements)
            .with_src(elem_size_const)
            .with_size(64)
            .with_type(self.types.ulong_id);
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
        let sym_name = self.symbol_name(declarator.symbol);
        let unique_name = format!("{}.{}", sym_name, sym_id.0);
        let sym = Pseudo::sym(sym_id, unique_name.clone());

        // Create a pointer type for the VLA (pointer to element type)
        let ptr_type = self.types.pointer_to(elem_type);

        if let Some(func) = &mut self.current_func {
            func.add_pseudo(sym);
            // Register as a pointer variable, not as the array type
            let mods = self.types.modifiers(typ);
            let is_volatile = mods.contains(TypeModifiers::VOLATILE);
            let is_atomic = mods.contains(TypeModifiers::ATOMIC);
            func.add_local(
                &unique_name,
                sym_id,
                ptr_type,
                is_volatile,
                is_atomic,
                self.current_bb,
                declarator.explicit_align, // VLA explicit alignment
            );
        }

        // Store the Alloca result (pointer) into the VLA symbol
        let store_insn = Instruction::store(alloca_result, sym_id, 0, ptr_type, 64);
        self.emit(store_insn);

        // Track in linearizer's locals map with pointer type and VLA size info
        // This makes arr[i] behave like ptr[i] - load ptr, then offset
        self.insert_local(
            declarator.symbol,
            LocalVarInfo {
                sym: sym_id,
                typ: ptr_type,
                vla_size_sym: Some(size_sym_id),
                vla_elem_type: Some(elem_type),
                vla_dim_syms,
                is_indirect: false,
            },
        );
    }

    /// Linearize a static local variable declaration
    ///
    /// Static locals have static storage duration but no linkage.
    /// They are implemented as globals with unique names like `funcname.varname.N`.
    /// Initialization happens once at program start (compile-time).
    pub(crate) fn linearize_static_local(
        &mut self,
        declarator: &crate::parse::ast::InitDeclarator,
    ) {
        let name_str = self.symbol_name(declarator.symbol);

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

        // Also insert with the SymbolId for the current function scope
        // This is used during expression linearization
        self.insert_local(
            declarator.symbol,
            LocalVarInfo {
                // Use a sentinel value - we'll handle static locals specially
                sym: PseudoId(u32::MAX),
                typ: declarator.typ,
                vla_size_sym: None,
                vla_elem_type: None,
                vla_dim_syms: vec![],
                is_indirect: false,
            },
        );

        // Determine initializer (static locals are initialized at compile time)
        let init = declarator.init.as_ref().map_or(Initializer::None, |e| {
            self.ast_init_to_ir(e, declarator.typ)
        });

        // Add as a global - static locals always have internal linkage
        // Check for thread-local storage
        let modifiers = self.types.modifiers(declarator.typ);
        if modifiers.contains(TypeModifiers::THREAD_LOCAL) {
            self.module.add_global_tls_aligned(
                &global_name,
                declarator.typ,
                init,
                declarator.explicit_align,
                true, // static locals always have internal linkage
            );
        } else {
            self.module.add_global_aligned(
                &global_name,
                declarator.typ,
                init,
                declarator.explicit_align,
                true, // static locals always have internal linkage
            );
        }
    }

    /// Linearize an initializer list for arrays or structs
    pub(crate) fn linearize_init_list(
        &mut self,
        base_sym: PseudoId,
        typ: TypeId,
        elements: &[InitElement],
    ) {
        self.linearize_init_list_at_offset(base_sym, 0, typ, elements);
    }

    /// Linearize an initializer list at a given base offset
    pub(crate) fn linearize_init_list_at_offset(
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
                let elem_is_aggregate = matches!(
                    self.types.kind(elem_type),
                    TypeKind::Array | TypeKind::Struct | TypeKind::Union
                );

                let groups = self.group_array_init_elements(elements, elem_type);
                for element_index in groups.indices {
                    let Some(list) = groups.element_lists.get(&element_index) else {
                        continue;
                    };
                    let offset = base_offset + element_index * elem_size as i64;
                    // When a string literal initializes a char array element
                    // (e.g., char arr[3][4] = {"Sun", "Mon", "Tue"}), handle
                    // it as a string copy rather than recursing into individual
                    // char stores. The recursion would treat the string as a
                    // pointer instead of inline data.
                    let is_string_for_char_array = elem_is_aggregate
                        && list.len() == 1
                        && matches!(
                            list[0].value.kind,
                            ExprKind::StringLit(_) | ExprKind::WideStringLit(_)
                        )
                        && self.types.kind(elem_type) == TypeKind::Array;
                    if is_string_for_char_array {
                        // Emit byte-by-byte stores for the string content
                        if let ExprKind::StringLit(s) = &list[0].value.kind {
                            let char_type = self
                                .types
                                .base_type(elem_type)
                                .unwrap_or(self.types.char_id);
                            let char_bits = self.types.size_bits(char_type);
                            for (i, ch) in s.chars().enumerate() {
                                let byte_val = self.emit_const(ch as u8 as i128, self.types.int_id);
                                self.emit(Instruction::store(
                                    byte_val,
                                    base_sym,
                                    offset + i as i64,
                                    char_type,
                                    char_bits,
                                ));
                            }
                            // Null terminator + zero fill
                            let arr_bytes = (self.types.size_bits(elem_type) / 8) as usize;
                            let str_len = s.chars().count();
                            for i in str_len..arr_bytes {
                                let zero = self.emit_const(0, self.types.int_id);
                                self.emit(Instruction::store(
                                    zero,
                                    base_sym,
                                    offset + i as i64,
                                    char_type,
                                    char_bits,
                                ));
                            }
                        }
                        continue;
                    }
                    if elem_is_aggregate {
                        self.linearize_init_list_at_offset(base_sym, offset, elem_type, list);
                        continue;
                    }
                    let Some(last) = list.last() else {
                        continue;
                    };
                    let val = self.linearize_expr(&last.value);
                    let val_type = self.expr_type(&last.value);
                    let converted = self.emit_convert(val, val_type, elem_type);
                    let elem_size = self.types.size_bits(elem_type);
                    self.emit(Instruction::store(
                        converted, base_sym, offset, elem_type, elem_size,
                    ));
                }
            }
            TypeKind::Struct | TypeKind::Union => {
                // If the initializer is a single expression of the same struct
                // type (e.g., `Py_complex in[1] = {a}` where `a` is Py_complex),
                // do a block copy instead of field-by-field initialization.
                if elements.len() == 1
                    && elements[0].designators.is_empty()
                    && elements[0].value.typ.is_some()
                {
                    let expr_type = self.expr_type(&elements[0].value);
                    let expr_kind = self.types.kind(expr_type);
                    if (expr_kind == TypeKind::Struct || expr_kind == TypeKind::Union)
                        && self.types.size_bits(expr_type) == self.types.size_bits(typ)
                    {
                        let src_addr = self.linearize_lvalue(&elements[0].value);
                        let target_size_bytes = self.types.size_bits(typ) / 8;
                        self.emit_block_copy_at_offset(
                            base_sym,
                            base_offset,
                            src_addr,
                            target_size_bytes as i64,
                        );
                        return;
                    }
                }

                let resolved_typ = self.resolve_struct_type(typ);
                if let Some(composite) = self.types.get(resolved_typ).composite.as_ref() {
                    let members: Vec<_> = composite.members.clone();
                    let is_union = self.types.kind(resolved_typ) == TypeKind::Union;

                    let visits =
                        self.walk_struct_init_fields(resolved_typ, &members, is_union, elements);

                    for visit in visits {
                        let offset = base_offset + visit.offset as i64;
                        let field_type = visit.typ;

                        match visit.kind {
                            StructFieldVisitKind::BraceElision(sub_elements) => {
                                self.linearize_init_list_at_offset(
                                    base_sym,
                                    offset,
                                    field_type,
                                    &sub_elements,
                                );
                            }
                            StructFieldVisitKind::Expr(expr) => {
                                if let (Some(bit_off), Some(bit_w), Some(storage_size)) =
                                    (visit.bit_offset, visit.bit_width, visit.storage_unit_size)
                                {
                                    let val = self.linearize_expr(&expr);
                                    let val_type = self.expr_type(&expr);
                                    let storage_type = self.bitfield_storage_type(storage_size);
                                    let converted = self.emit_convert(val, val_type, storage_type);
                                    self.emit_bitfield_store(
                                        base_sym,
                                        offset as usize,
                                        bit_off,
                                        bit_w,
                                        storage_size,
                                        converted,
                                    );
                                } else {
                                    self.linearize_struct_field_init(
                                        base_sym, offset, field_type, &expr,
                                    );
                                }
                            }
                        }
                    }
                }
            }
            _ => {
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

    pub(crate) fn linearize_struct_field_init(
        &mut self,
        base_sym: PseudoId,
        offset: i64,
        field_type: TypeId,
        value: &Expr,
    ) {
        if let ExprKind::InitList {
            elements: nested_elems,
        } = &value.kind
        {
            self.linearize_init_list_at_offset(base_sym, offset, field_type, nested_elems);
        } else if let ExprKind::StringLit(s) = &value.kind {
            if self.types.kind(field_type) == TypeKind::Array {
                let elem_type = self
                    .types
                    .base_type(field_type)
                    .unwrap_or(self.types.char_id);
                let elem_size = self.types.size_bits(elem_type);

                for (i, byte) in s.bytes().enumerate() {
                    let byte_val = self.emit_const(byte as i128, elem_type);
                    self.emit(Instruction::store(
                        byte_val,
                        base_sym,
                        offset + i as i64,
                        elem_type,
                        elem_size,
                    ));
                }
                let null_val = self.emit_const(0, elem_type);
                self.emit(Instruction::store(
                    null_val,
                    base_sym,
                    offset + s.chars().count() as i64,
                    elem_type,
                    elem_size,
                ));
            } else {
                let val = self.linearize_expr(value);
                let val_type = self.expr_type(value);
                let converted = self.emit_convert(val, val_type, field_type);
                let size = self.types.size_bits(field_type);
                self.emit(Instruction::store(
                    converted, base_sym, offset, field_type, size,
                ));
            }
        } else {
            let (actual_type, actual_size) = if self.types.kind(field_type) == TypeKind::Array {
                let elem_type = self.types.base_type(field_type).unwrap_or(field_type);
                (elem_type, self.types.size_bits(elem_type))
            } else {
                (field_type, self.types.size_bits(field_type))
            };
            let val = self.linearize_expr(value);
            let val_type = self.expr_type(value);
            let converted = self.emit_convert(val, val_type, actual_type);
            self.emit(Instruction::store(
                converted,
                base_sym,
                offset,
                actual_type,
                actual_size,
            ));
        }
    }

    pub(crate) fn linearize_if(&mut self, cond: &Expr, then_stmt: &Stmt, else_stmt: Option<&Stmt>) {
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
        self.link_to_merge_if_needed(merge_bb);

        // Else block
        if let Some(else_s) = else_stmt {
            self.switch_bb(else_bb);
            self.linearize_stmt(else_s);
            self.link_to_merge_if_needed(merge_bb);
        }

        // Merge block
        self.switch_bb(merge_bb);
    }

    pub(crate) fn linearize_while(&mut self, cond: &Expr, body: &Stmt) {
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
        // After linearizing condition, current_bb may be different from cond_bb
        // (e.g., if condition contains short-circuit operators like && or ||).
        // Link the CURRENT block to body_bb and exit_bb.
        if let Some(cond_end_bb) = self.current_bb {
            self.emit(Instruction::cbr(cond_val, body_bb, exit_bb));
            self.link_bb(cond_end_bb, body_bb);
            self.link_bb(cond_end_bb, exit_bb);
        }

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

    pub(crate) fn linearize_do_while(&mut self, body: &Stmt, cond: &Expr) {
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
        // After linearizing condition, current_bb may be different from cond_bb
        // (e.g., if condition contains short-circuit operators like && or ||).
        // Link the CURRENT block to body_bb and exit_bb.
        if let Some(cond_end_bb) = self.current_bb {
            self.emit(Instruction::cbr(cond_val, body_bb, exit_bb));
            self.link_bb(cond_end_bb, body_bb);
            self.link_bb(cond_end_bb, exit_bb);
        }

        // Exit block
        self.switch_bb(exit_bb);
    }

    pub(crate) fn linearize_for(
        &mut self,
        init: Option<&ForInit>,
        cond: Option<&Expr>,
        post: Option<&Expr>,
        body: &Stmt,
    ) {
        // C99 for-loop declarations (e.g., for (int i = 0; ...)) are scoped to the loop.
        self.push_scope();

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
            // After linearizing condition, current_bb may be different from cond_bb
            // (e.g., if condition contains short-circuit operators like && or ||).
            // Link the CURRENT block to body_bb and exit_bb.
            if let Some(cond_end_bb) = self.current_bb {
                self.emit(Instruction::cbr(cond_val, body_bb, exit_bb));
                self.link_bb(cond_end_bb, body_bb);
                self.link_bb(cond_end_bb, exit_bb);
            }
        } else {
            // No condition = always true
            self.emit(Instruction::br(body_bb));
            self.link_bb(cond_bb, body_bb);
            // No link to exit_bb since we always enter the body
        }

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

        // Restore locals to remove for-loop-scoped declarations
        self.pop_scope();
    }

    pub(crate) fn linearize_switch(&mut self, expr: &Expr, body: &Stmt) {
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
    pub(crate) fn collect_switch_cases(&self, body: &Stmt) -> (Vec<i64>, bool) {
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

    pub(crate) fn collect_cases_from_stmt(
        &self,
        stmt: &Stmt,
        case_values: &mut Vec<i64>,
        has_default: &mut bool,
    ) {
        match stmt {
            Stmt::Case(expr) => {
                // Extract constant value from case expression
                if let Some(val) = self.eval_const_expr(expr) {
                    case_values.push(val as i64); // switch cases truncated to i64
                }
            }
            Stmt::Default => {
                *has_default = true;
            }
            // Recursively check labeled statements
            Stmt::Label { stmt, .. } => {
                self.collect_cases_from_stmt(stmt, case_values, has_default);
            }
            // Recurse into nested statements for Duff's device pattern
            // (case labels inside loops/blocks within a switch)
            Stmt::Block(items) => {
                for item in items {
                    if let BlockItem::Statement(s) = item {
                        self.collect_cases_from_stmt(s, case_values, has_default);
                    }
                }
            }
            Stmt::DoWhile { body, .. } | Stmt::While { body, .. } | Stmt::For { body, .. } => {
                self.collect_cases_from_stmt(body, case_values, has_default);
            }
            Stmt::If {
                then_stmt,
                else_stmt,
                ..
            } => {
                self.collect_cases_from_stmt(then_stmt, case_values, has_default);
                if let Some(e) = else_stmt {
                    self.collect_cases_from_stmt(e, case_values, has_default);
                }
            }
            // Stop at inner switch — its case labels belong to it
            Stmt::Switch { .. } => {}
            _ => {}
        }
    }

    /// Evaluate a constant expression (for case labels, static initializers)
    ///
    /// C99 6.6 defines integer constant expressions. This function evaluates
    /// expressions that can be computed at compile time.
    pub(crate) fn eval_const_expr(&self, expr: &Expr) -> Option<i128> {
        match &expr.kind {
            ExprKind::IntLit(val) => Some(*val as i128),
            ExprKind::Int128Lit(val) => Some(*val),
            ExprKind::CharLit(c) => Some(*c as u8 as i8 as i128),

            ExprKind::Ident(symbol_id) => {
                // Check if it's an enum constant
                let sym = self.symbols.get(*symbol_id);
                if sym.is_enum_constant() {
                    sym.enum_value.map(|v| v as i128)
                } else {
                    None
                }
            }

            ExprKind::Unary { op, operand } => {
                let val = self.eval_const_expr(operand)?;
                match op {
                    UnaryOp::Neg => Some(val.wrapping_neg()),
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
                        // 8/16/32-bit: mask 31, 64-bit: mask 63, 128-bit: mask 127.
                        let size_bits = left.typ.map(|t| self.types.size_bits(t)).unwrap_or(32);
                        let mask: i128 = if size_bits > 64 {
                            127
                        } else if size_bits > 32 {
                            63
                        } else {
                            31
                        };
                        let shift_amt = (r & mask) as u32;
                        match op {
                            BinaryOp::Shl => Some(l << shift_amt),
                            BinaryOp::Shr => Some(l >> shift_amt),
                            _ => unreachable!(),
                        }
                    }
                    BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                        // Use unsigned comparison when either operand is unsigned.
                        // C promotes both to the common unsigned type for comparison.
                        let left_unsigned = left.typ.is_some_and(|t| {
                            self.types.modifiers(t).contains(TypeModifiers::UNSIGNED)
                        });
                        let right_unsigned = right.typ.is_some_and(|t| {
                            self.types.modifiers(t).contains(TypeModifiers::UNSIGNED)
                        });
                        let use_unsigned = left_unsigned || right_unsigned;
                        if use_unsigned {
                            let lu = l as u128;
                            let ru = r as u128;
                            Some(match op {
                                BinaryOp::Lt => {
                                    if lu < ru {
                                        1
                                    } else {
                                        0
                                    }
                                }
                                BinaryOp::Le => {
                                    if lu <= ru {
                                        1
                                    } else {
                                        0
                                    }
                                }
                                BinaryOp::Gt => {
                                    if lu > ru {
                                        1
                                    } else {
                                        0
                                    }
                                }
                                BinaryOp::Ge => {
                                    if lu >= ru {
                                        1
                                    } else {
                                        0
                                    }
                                }
                                _ => unreachable!(),
                            })
                        } else {
                            Some(match op {
                                BinaryOp::Lt => {
                                    if l < r {
                                        1
                                    } else {
                                        0
                                    }
                                }
                                BinaryOp::Le => {
                                    if l <= r {
                                        1
                                    } else {
                                        0
                                    }
                                }
                                BinaryOp::Gt => {
                                    if l > r {
                                        1
                                    } else {
                                        0
                                    }
                                }
                                BinaryOp::Ge => {
                                    if l >= r {
                                        1
                                    } else {
                                        0
                                    }
                                }
                                _ => unreachable!(),
                            })
                        }
                    }
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
                Some((size_bits / 8) as i128)
            }

            // sizeof(expr) - constant if expr type is complete
            ExprKind::SizeofExpr(inner_expr) => {
                if let Some(typ) = inner_expr.typ {
                    let size_bits = self.types.size_bits(typ);
                    Some((size_bits / 8) as i128)
                } else {
                    None
                }
            }

            // _Alignof(type) - constant for complete types
            ExprKind::AlignofType(type_id) => {
                let align = self.types.alignment(*type_id);
                Some(align as i128)
            }

            // _Alignof(expr) - constant if expr type is complete
            ExprKind::AlignofExpr(inner_expr) => {
                if let Some(typ) = inner_expr.typ {
                    let align = self.types.alignment(typ);
                    Some(align as i128)
                } else {
                    None
                }
            }

            // Cast to integer type - evaluate inner expression
            ExprKind::Cast { expr: inner, .. } => self.eval_const_expr(inner),

            // __builtin_offsetof(type, member-designator) - compile-time constant
            ExprKind::OffsetOf { type_id, path } => {
                let mut offset: u64 = 0;
                let mut current_type = *type_id;

                for element in path {
                    match element {
                        OffsetOfPath::Field(field_id) => {
                            let struct_type = self.resolve_struct_type(current_type);
                            if let Some(member_info) =
                                self.types.find_member(struct_type, *field_id)
                            {
                                offset += member_info.offset as u64;
                                current_type = member_info.typ;
                            } else {
                                return None; // Field not found
                            }
                        }
                        OffsetOfPath::Index(index) => {
                            if let Some(elem_type) = self.types.base_type(current_type) {
                                let elem_size = self.types.size_bytes(elem_type);
                                offset += (*index as u64) * (elem_size as u64);
                                current_type = elem_type;
                            } else {
                                return None; // Not an array type
                            }
                        }
                    }
                }

                Some(offset as i128)
            }

            _ => None,
        }
    }

    /// Evaluate a constant floating-point expression at compile time.
    /// Handles float literals, negation, and binary arithmetic on floats.
    pub(crate) fn eval_const_float_expr(&self, expr: &Expr) -> Option<f64> {
        match &expr.kind {
            ExprKind::FloatLit(v) => Some(*v),
            ExprKind::IntLit(v) => Some(*v as f64),
            ExprKind::CharLit(c) => Some(*c as i64 as f64),

            ExprKind::Unary { op, operand } => {
                let val = self.eval_const_float_expr(operand)?;
                match op {
                    UnaryOp::Neg => Some(-val),
                    _ => None,
                }
            }

            ExprKind::Binary { op, left, right } => {
                let l = self.eval_const_float_expr(left)?;
                let r = self.eval_const_float_expr(right)?;
                match op {
                    BinaryOp::Add => Some(l + r),
                    BinaryOp::Sub => Some(l - r),
                    BinaryOp::Mul => Some(l * r),
                    BinaryOp::Div => Some(l / r),
                    _ => None,
                }
            }

            ExprKind::Cast { expr: inner, .. } => self.eval_const_float_expr(inner),

            _ => None,
        }
    }

    /// Evaluate a static address expression (for initializers like `&symbol.field`)
    ///
    /// Returns Some((symbol_name, offset)) if the expression is a valid static address,
    /// or None if it can't be computed at compile time.
    pub(crate) fn eval_static_address(&self, expr: &Expr) -> Option<(String, i64)> {
        match &expr.kind {
            // Simple identifier: &symbol
            ExprKind::Ident(symbol_id) => {
                let name_str = self.symbol_name(*symbol_id);
                // Check if this is a static local variable
                let key = format!("{}.{}", self.current_func_name, name_str);
                if let Some(static_info) = self.static_locals.get(&key) {
                    Some((static_info.global_name.clone(), 0))
                } else {
                    Some((name_str, 0))
                }
            }

            // Member access: expr.member
            ExprKind::Member { expr: base, member } => {
                // Recursively evaluate the base address
                let (name, base_offset) = self.eval_static_address(base)?;

                // Get the offset of the member in the struct
                let base_type = base.typ?;
                let struct_type = self.resolve_struct_type(base_type);
                let member_info = self.types.find_member(struct_type, *member)?;

                Some((name, base_offset + member_info.offset as i64))
            }

            // Arrow access: expr->member (pointer dereference + member access)
            // Can be a static address when the pointer is a static address-of expression
            // (e.g., (&static_struct.field)->subfield in CPython macros)
            ExprKind::Arrow { expr: base, member } => {
                let (name, base_offset) = self.eval_static_address(base)?;
                let ptr_type = base.typ?;
                let pointee_type = self.types.base_type(ptr_type)?;
                let struct_type = self.resolve_struct_type(pointee_type);
                let member_info = self.types.find_member(struct_type, *member)?;
                Some((name, base_offset + member_info.offset as i64))
            }

            // Array subscript: array[index]
            ExprKind::Index { array, index } => {
                // Recursively evaluate the base address
                let (name, base_offset) = self.eval_static_address(array)?;

                // Get the index as a constant
                let idx = self.eval_const_expr(index)?;

                // Get the element size
                let array_type = array.typ?;
                let elem_type = self.types.base_type(array_type)?;
                let elem_size = self.types.size_bytes(elem_type) as i64;

                Some((name, base_offset + idx as i64 * elem_size))
            }

            // Address-of: &expr → same as evaluating expr as static address
            ExprKind::Unary {
                op: UnaryOp::AddrOf,
                operand,
            } => self.eval_static_address(operand),

            // Cast - evaluate the inner expression
            ExprKind::Cast { expr: inner, .. } => self.eval_static_address(inner),

            // Binary add/sub with pointer operand: ptr + int or ptr - int
            ExprKind::Binary {
                op: op @ (BinaryOp::Add | BinaryOp::Sub),
                left,
                right,
            } => {
                // Determine which side is the pointer and which is the integer
                let (ptr_expr, int_expr, is_sub) = if left.typ.is_some_and(|t| {
                    self.types.kind(t) == TypeKind::Pointer || self.types.kind(t) == TypeKind::Array
                }) {
                    (left.as_ref(), right.as_ref(), *op == BinaryOp::Sub)
                } else if right.typ.is_some_and(|t| {
                    self.types.kind(t) == TypeKind::Pointer || self.types.kind(t) == TypeKind::Array
                }) && *op == BinaryOp::Add
                {
                    (right.as_ref(), left.as_ref(), false)
                } else {
                    return None;
                };

                let (name, base_offset) = self.eval_static_address(ptr_expr)?;
                let int_val = self.eval_const_expr(int_expr)?;

                // Scale by pointee size for pointer arithmetic
                let pointee_size = ptr_expr
                    .typ
                    .and_then(|t| self.types.base_type(t))
                    .map(|t| self.types.size_bytes(t) as i64)
                    .unwrap_or(1);
                let byte_offset = if is_sub {
                    base_offset - int_val as i64 * pointee_size
                } else {
                    base_offset + int_val as i64 * pointee_size
                };

                Some((name, byte_offset))
            }

            _ => None,
        }
    }

    /// Linearize switch body, switching basic blocks at case/default labels
    pub(crate) fn linearize_switch_body(
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

    pub(crate) fn linearize_switch_stmt(
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
                    if let Some(idx) = case_values.iter().position(|v| *v as i128 == val) {
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

            // Duff's device: case labels can appear inside loops/blocks
            // within a switch body. Propagate switch context through them.
            Stmt::DoWhile { body, cond } => {
                let body_bb = self.alloc_bb();
                let cond_bb = self.alloc_bb();
                let exit_bb = self.alloc_bb();

                if let Some(current) = self.current_bb {
                    if !self.is_terminated() {
                        self.emit(Instruction::br(body_bb));
                        self.link_bb(current, body_bb);
                    }
                }

                self.break_targets.push(exit_bb);
                self.continue_targets.push(cond_bb);

                self.switch_bb(body_bb);
                self.linearize_switch_stmt(body, case_values, case_bbs, default_bb, case_idx);
                if !self.is_terminated() {
                    if let Some(current) = self.current_bb {
                        self.emit(Instruction::br(cond_bb));
                        self.link_bb(current, cond_bb);
                    }
                }

                self.break_targets.pop();
                self.continue_targets.pop();

                self.switch_bb(cond_bb);
                let cond_val = self.linearize_expr(cond);
                if let Some(cond_end_bb) = self.current_bb {
                    self.emit(Instruction::cbr(cond_val, body_bb, exit_bb));
                    self.link_bb(cond_end_bb, body_bb);
                    self.link_bb(cond_end_bb, exit_bb);
                }

                self.switch_bb(exit_bb);
            }

            Stmt::While { cond, body } => {
                let cond_bb = self.alloc_bb();
                let body_bb = self.alloc_bb();
                let exit_bb = self.alloc_bb();

                if let Some(current) = self.current_bb {
                    if !self.is_terminated() {
                        self.emit(Instruction::br(cond_bb));
                        self.link_bb(current, cond_bb);
                    }
                }

                self.switch_bb(cond_bb);
                let cond_val = self.linearize_expr(cond);
                if let Some(cond_end_bb) = self.current_bb {
                    self.emit(Instruction::cbr(cond_val, body_bb, exit_bb));
                    self.link_bb(cond_end_bb, body_bb);
                    self.link_bb(cond_end_bb, exit_bb);
                }

                self.break_targets.push(exit_bb);
                self.continue_targets.push(cond_bb);

                self.switch_bb(body_bb);
                self.linearize_switch_stmt(body, case_values, case_bbs, default_bb, case_idx);
                if !self.is_terminated() {
                    if let Some(current) = self.current_bb {
                        self.emit(Instruction::br(cond_bb));
                        self.link_bb(current, cond_bb);
                    }
                }

                self.break_targets.pop();
                self.continue_targets.pop();

                self.switch_bb(exit_bb);
            }

            Stmt::For {
                init,
                cond,
                post,
                body,
            } => {
                self.push_scope();

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

                if let Some(current) = self.current_bb {
                    if !self.is_terminated() {
                        self.emit(Instruction::br(cond_bb));
                        self.link_bb(current, cond_bb);
                    }
                }

                self.switch_bb(cond_bb);
                if let Some(cond_expr) = cond {
                    let cond_val = self.linearize_expr(cond_expr);
                    if let Some(cond_end_bb) = self.current_bb {
                        self.emit(Instruction::cbr(cond_val, body_bb, exit_bb));
                        self.link_bb(cond_end_bb, body_bb);
                        self.link_bb(cond_end_bb, exit_bb);
                    }
                } else {
                    self.emit(Instruction::br(body_bb));
                    self.link_bb(cond_bb, body_bb);
                }

                self.break_targets.push(exit_bb);
                self.continue_targets.push(post_bb);

                self.switch_bb(body_bb);
                self.linearize_switch_stmt(body, case_values, case_bbs, default_bb, case_idx);
                if !self.is_terminated() {
                    if let Some(current) = self.current_bb {
                        self.emit(Instruction::br(post_bb));
                        self.link_bb(current, post_bb);
                    }
                }

                self.break_targets.pop();
                self.continue_targets.pop();

                self.switch_bb(post_bb);
                if let Some(post_expr) = post {
                    self.linearize_expr(post_expr);
                }
                self.emit(Instruction::br(cond_bb));
                self.link_bb(post_bb, cond_bb);

                self.switch_bb(exit_bb);
                self.pop_scope();
            }

            Stmt::Block(items) => {
                self.push_scope();
                for item in items {
                    match item {
                        BlockItem::Declaration(decl) => self.linearize_local_decl(decl),
                        BlockItem::Statement(s) => {
                            self.linearize_switch_stmt(
                                s,
                                case_values,
                                case_bbs,
                                default_bb,
                                case_idx,
                            );
                        }
                    }
                }
                self.pop_scope();
            }

            Stmt::If {
                cond,
                then_stmt,
                else_stmt,
            } => {
                let then_bb = self.alloc_bb();
                let merge_bb = self.alloc_bb();
                let else_bb = if else_stmt.is_some() {
                    self.alloc_bb()
                } else {
                    merge_bb
                };

                let cond_val = self.linearize_expr(cond);
                if let Some(current) = self.current_bb {
                    self.emit(Instruction::cbr(cond_val, then_bb, else_bb));
                    self.link_bb(current, then_bb);
                    self.link_bb(current, else_bb);
                }

                self.switch_bb(then_bb);
                self.linearize_switch_stmt(then_stmt, case_values, case_bbs, default_bb, case_idx);
                self.link_to_merge_if_needed(merge_bb);

                if let Some(else_s) = else_stmt {
                    self.switch_bb(else_bb);
                    self.linearize_switch_stmt(else_s, case_values, case_bbs, default_bb, case_idx);
                    self.link_to_merge_if_needed(merge_bb);
                }

                self.switch_bb(merge_bb);
            }

            Stmt::Label { name, stmt } => {
                let name_str = self.str(*name).to_string();
                let label_bb = self.get_or_create_label(&name_str);

                if !self.is_terminated() {
                    if let Some(current) = self.current_bb {
                        self.emit(Instruction::br(label_bb));
                        self.link_bb(current, label_bb);
                    }
                }

                self.switch_bb(label_bb);
                self.linearize_switch_stmt(stmt, case_values, case_bbs, default_bb, case_idx);
            }

            // Inner switch owns its own cases — delegate to normal linearizer
            Stmt::Switch { .. } => {
                self.linearize_stmt(stmt);
            }

            _ => {
                // Regular statement - linearize it
                self.linearize_stmt(stmt);
            }
        }
    }

    // ========================================================================
    // Inline assembly linearization
    // ========================================================================

    /// Linearize an inline assembly statement
    /// Check if an expression is a simple identifier that's a parameter (in var_map)
    /// Returns Some((name, pseudo)) if it is, None otherwise
    pub(crate) fn get_param_if_ident(&self, expr: &Expr) -> Option<(String, PseudoId)> {
        if let ExprKind::Ident(symbol_id) = &expr.kind {
            let name_str = self.symbol_name(*symbol_id);
            if let Some(&pseudo) = self.var_map.get(&name_str) {
                return Some((name_str, pseudo));
            }
        }
        None
    }

    pub(crate) fn linearize_asm(
        &mut self,
        template: &str,
        outputs: &[AsmOperand],
        inputs: &[AsmOperand],
        clobbers: &[String],
        goto_labels: &[StringId],
    ) {
        let mut ir_outputs = Vec::new();
        let mut ir_inputs = Vec::new();
        // Track which outputs are parameters (need var_map update instead of store)
        let mut param_outputs: Vec<Option<String>> = Vec::new();

        // Process output operands
        for op in outputs {
            // Create a pseudo for the output
            let pseudo = self.alloc_pseudo();

            // Parse constraint to get flags
            let (_is_memory, is_readwrite, _matching) = self.parse_asm_constraint(&op.constraint);

            // Get symbolic name if present
            let name = op.name.map(|n| self.str(n).to_string());

            // Check if this output is a parameter (SSA value, not memory location)
            let param_info = self.get_param_if_ident(&op.expr);

            let typ = self.expr_type(&op.expr);
            let size = self.types.size_bits(typ);

            // For read-write outputs ("+r"), load the initial value into the SAME pseudo
            // so that input and output use the same register
            if is_readwrite {
                if let Some((_, param_pseudo)) = &param_info {
                    // Parameter: copy value directly (no memory address)
                    self.emit(
                        Instruction::new(Opcode::Copy)
                            .with_target(pseudo)
                            .with_src(*param_pseudo)
                            .with_type(typ)
                            .with_size(size),
                    );
                } else {
                    // Local or global: load from memory address
                    let addr = self.linearize_lvalue(&op.expr);
                    self.emit(Instruction::load(pseudo, addr, 0, typ, size));
                }

                // Also add as input, using the SAME pseudo and marking as matching
                // the output. Per GCC convention, '+' creates one operand number
                // shared by both output and input.
                ir_inputs.push(AsmConstraint {
                    pseudo, // Same pseudo as output - ensures same register
                    name: name.clone(),
                    matching_output: Some(ir_outputs.len()), // matches the output about to be pushed
                    constraint: op.constraint.clone(),
                    size,
                });
            }

            ir_outputs.push(AsmConstraint {
                pseudo,
                name,
                matching_output: None,
                constraint: op.constraint.clone(),
                size,
            });

            // Track if this is a parameter output
            param_outputs.push(param_info.map(|(name, _)| name));
        }

        // Process input operands
        for op in inputs {
            let (is_memory, _, matching) = self.parse_asm_constraint(&op.constraint);

            // Get symbolic name if present
            let name = op.name.map(|n| self.str(n).to_string());

            let typ = self.expr_type(&op.expr);
            let size = self.types.size_bits(typ);

            // For matching constraints (like "0"), we need to load the input value
            // into the matched output's pseudo so they use the same register
            let pseudo = if let Some(match_idx) = matching {
                if match_idx < ir_outputs.len() {
                    // Use the matched output's pseudo
                    let out_pseudo = ir_outputs[match_idx].pseudo;
                    // Load the input value into the output's pseudo
                    let val = self.linearize_expr(&op.expr);
                    // Copy val to out_pseudo so they share the same register
                    self.emit(
                        Instruction::new(Opcode::Copy)
                            .with_target(out_pseudo)
                            .with_src(val)
                            .with_type(typ)
                            .with_size(size),
                    );
                    out_pseudo
                } else {
                    self.linearize_expr(&op.expr)
                }
            } else if is_memory {
                // For memory operands, get the address
                self.linearize_lvalue(&op.expr)
            } else {
                // For register operands, evaluate the expression
                self.linearize_expr(&op.expr)
            };

            ir_inputs.push(AsmConstraint {
                pseudo,
                name,
                matching_output: matching,
                constraint: op.constraint.clone(),
                size,
            });
        }

        // Process goto labels - map label names to BasicBlockIds
        let ir_goto_labels: Vec<(BasicBlockId, String)> = goto_labels
            .iter()
            .map(|label_id| {
                let label_name = self.str(*label_id).to_string();
                let bb = self.get_or_create_label(&label_name);
                (bb, label_name)
            })
            .collect();

        // Create the asm data
        let asm_data = AsmData {
            template: template.to_string(),
            outputs: ir_outputs.clone(),
            inputs: ir_inputs,
            clobbers: clobbers.to_vec(),
            goto_labels: ir_goto_labels.clone(),
        };

        // Emit the asm instruction
        self.emit(Instruction::asm(asm_data));

        // For asm goto: add edges to all possible label targets
        // The asm may jump to any of these labels, so control flow can go there
        if !ir_goto_labels.is_empty() {
            if let Some(current) = self.current_bb {
                // After the asm instruction, we need a basic block for fall-through
                // and edges to all goto targets
                let fall_through = self.alloc_bb();

                // Add edges to all goto label targets
                for (target_bb, _) in &ir_goto_labels {
                    self.link_bb(current, *target_bb);
                }

                // Add edge to fall-through (normal case when asm doesn't jump)
                self.link_bb(current, fall_through);

                // Emit an explicit branch to the fallthrough block
                // This is necessary because the asm goto acts as a conditional terminator
                // Without this, code would fall through to whatever block comes next in layout
                self.emit(Instruction::br(fall_through));

                // Switch to fall-through block for subsequent instructions
                self.current_bb = Some(fall_through);
            }
        }

        // Store outputs back to their destinations
        // store(value, addr, ...) - value first, then address
        for (i, op) in outputs.iter().enumerate() {
            let out_pseudo = ir_outputs[i].pseudo;

            // Check if this output is a parameter (update var_map instead of memory store)
            if let Some(param_name) = &param_outputs[i] {
                // Parameter: update var_map with the new SSA value
                self.var_map.insert(param_name.clone(), out_pseudo);
            } else {
                // Local or global: store to memory address
                let addr = self.linearize_lvalue(&op.expr);
                let typ = self.expr_type(&op.expr);
                let size = self.types.size_bits(typ);
                self.emit(Instruction::store(out_pseudo, addr, 0, typ, size));
            }
        }
    }

    /// Parse an asm constraint string to extract flags
    /// Returns (is_memory, is_readwrite, matching_output)
    /// Note: Early clobber (&) is parsed but not used since our simple register
    /// allocator doesn't share registers between inputs and outputs anyway
    pub(crate) fn parse_asm_constraint(&self, constraint: &str) -> (bool, bool, Option<usize>) {
        let mut is_memory = false;
        let mut is_readwrite = false;
        let mut matching = None;

        for c in constraint.chars() {
            match c {
                '+' => is_readwrite = true,
                '&' | '=' | '%' => {} // Early clobber, output-only, commutative
                'r' | 'a' | 'b' | 'c' | 'd' | 'S' | 'D' => {} // Register constraints
                'm' | 'o' | 'V' | 'Q' => is_memory = true,
                'i' | 'n' | 'g' | 'X' => {} // Immediate, general
                '0'..='9' => matching = Some((c as u8 - b'0') as usize),
                _ => {} // Ignore unknown constraints
            }
        }

        (is_memory, is_readwrite, matching)
    }

    pub(crate) fn get_or_create_label(&mut self, name: &str) -> BasicBlockId {
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
}
