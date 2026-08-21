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
use crate::constexpr::ConstScope;
use crate::diag::{error, Position};
use crate::float::FloatVal;
use crate::parse::ast::{
    AsmOperand, BinaryOp, BlockItem, Declaration, Expr, ExprKind, ForInit, InitElement, Stmt,
    UnaryOp,
};
use crate::strings::StringId;
use crate::types::TypeTable;
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
                // C99 6.8.6.4p1: a `return` with an expression may not appear
                // in a void function, and one without an expression may not
                // appear in a function that returns a value. Both used to
                // compile clean.
                let declared_ret = self.current_func.as_ref().map(|f| f.return_type);
                if let Some(rt) = declared_ret {
                    let returns_void = self.types.kind(rt) == TypeKind::Void;
                    match expr {
                        // 6.8.6.4p1 forbids returning a *value*. An expression
                        // of type `void` has none, so `return f();` where `f`
                        // returns void — the ordinary tail-call wrapper, which
                        // GCC and Clang both accept — is not a violation.
                        Some(e)
                            if returns_void
                                && self.types.kind(self.expr_type(e)) != TypeKind::Void =>
                        {
                            error(e.pos, "'return' with a value in a function returning void")
                        }
                        None if !returns_void => error(
                            // `Stmt` carries no position, so fall back to the
                            // last expression lowered in this function.
                            self.current_pos.unwrap_or_default(),
                            "'return' with no value in a function returning non-void",
                        ),
                        // 6.8.6.4p3 converts the value "as if by assignment",
                        // so the simple-assignment constraints of 6.5.16.1
                        // govern it and are asked in exactly the same words.
                        Some(e) if !returns_void => self.check_return_type(rt, e),
                        _ => {}
                    }
                }

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
                        let addr = self.complex_operand_addr(e);
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

            Stmt::Break(_) => {
                if let Some(&target) = self.break_targets.last() {
                    if let Some(current) = self.current_bb {
                        self.emit(Instruction::br(target));
                        self.link_bb(current, target);
                    }
                }
            }

            Stmt::Continue(_) => {
                if let Some(&target) = self.continue_targets.last() {
                    if let Some(current) = self.current_bb {
                        self.emit(Instruction::br(target));
                        self.link_bb(current, target);
                    }
                }
            }

            // GNU computed goto. The target address is not statically known,
            // so every label whose address was taken in this function is
            // linked as a successor -- the same conservative edge set `asm
            // goto` uses, and the reason DCE does not delete those blocks.
            Stmt::GotoIndirect { target, pos } => {
                // 6.5.3.2: the operand of a computed goto is an address.
                // Anything else -- `goto *3;` -- would branch to a number.
                let target_typ = self.expr_type(target);
                // GCC: "computed goto must be pointer type". An integer is
                // scalar, so testing scalarity accepted `goto *3;` -- and the
                // 64-bit store of a 32-bit value then branched through a
                // half-initialised address.
                if self.types.kind(target_typ) != crate::types::TypeKind::Pointer {
                    let named = self.types.format_type(target_typ, Some(self.strings));
                    crate::diag::error_args(
                        *pos,
                        "computed goto must be a pointer, not '{0}'",
                        &[&named],
                    );
                }
                let addr = self.linearize_expr(target);
                let (dispatch_bb, slot) = self.indirect_dispatch_block();
                // Hand the address over in the hidden local and branch to the
                // one dispatch block, which is the only place that fans out to
                // the labels. See `Linearizer::indirect_dispatch`.
                let void_ptr = self.types.void_ptr_id;
                self.emit(Instruction::store(addr, slot, 0, void_ptr, 64));
                self.emit(Instruction::br(dispatch_bb));
                if let Some(current) = self.current_bb {
                    self.link_bb(current, dispatch_bb);
                }
                self.current_bb = None;
            }

            Stmt::Goto { name: label, .. } => {
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

            Stmt::Label { name, stmt, .. } => {
                let name_str = self.str(*name).to_string();
                self.defined_labels.insert(name_str.clone());
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

            Stmt::Case(..) | Stmt::Default(_) => {
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

            // A typedef declares a name, not an object, so nothing is
            // allocated for it. It used to fall through and take a local slot
            // for the type name -- harmless while nothing read it, and not
            // harmless once a typedef can be variably modified, since the VLA
            // arm below tests only `vla_sizes` and would have allocated the
            // array itself.
            if declarator.storage_class.contains(TypeModifiers::TYPEDEF) {
                // C17 6.7.7p3: a variably modified typedef's size expressions
                // are evaluated "each time the declaration of the typedef name
                // is reached in the order of execution" -- here, once, however
                // many objects the name goes on to declare. Each is spilled to
                // a hidden local that `ExprKind::VmTypedefExtent` reads back.
                if !declarator.vla_sizes.is_empty() && self.types.kind(typ) == TypeKind::Array {
                    let name = self.symbol_name(declarator.symbol);
                    let (dims, _elem) = self.record_vm_extents(typ, &declarator.vla_sizes, &name);
                    // Keep only the extents that were *variable*.
                    //
                    // `record_vm_extents` reports one entry per array level,
                    // constant levels included, but the parser mints an
                    // `ExprKind::VmTypedefExtent(sym, k)` per *size
                    // expression* -- one per variable level, since a constant
                    // one has no expression to evaluate. So `k` counts
                    // variable extents and the vector counted all of them, and
                    // the two disagreed the moment a constant extent appeared:
                    // `typedef int T[2][n]; T a;` read `dims[0]`, found the
                    // constant 2, and gave `a` the extent 2 instead of `n` --
                    // `sizeof a` 16 against gcc's 80, with every write past
                    // `a[0][3]` landing outside the object.
                    //
                    // Filtering here rather than widening the index keeps the
                    // meaning the parser already gives it: the k-th variable
                    // extent of this typedef.
                    let variable: Vec<VmDim> = dims
                        .into_iter()
                        .filter(|d| matches!(d, VmDim::Sym(_)))
                        .collect();
                    self.vm_typedef_dims.insert(declarator.symbol, variable);
                }
                continue;
            }

            // Check if this is a static local variable
            if declarator.storage_class.contains(TypeModifiers::STATIC) {
                // Static local: create a global with unique name
                self.linearize_static_local(declarator);
                continue;
            }

            // Check if this is a VLA (Variable Length Array).
            //
            // Run-time extents alone do not make one: in `int (*p)[n]` they
            // belong to the *pointee*, and `p` is an ordinary pointer, sized
            // at compile time and initializable like any other. Only a
            // declarator that is itself the array is allocated here.
            if !declarator.vla_sizes.is_empty() && self.types.kind(typ) == TypeKind::Array {
                // C99 6.7.8: VLAs cannot have initializers.
                // Report against the declarator's own position: `current_pos`
                // is an Option and, when it is None, this constraint violation
                // used to be dropped without any diagnostic at all.
                if declarator.init.is_some() {
                    error(
                        declarator.pos,
                        "variable length arrays cannot have initializers",
                    );
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
                    vm_row_dims: vec![],
                    storage: crate::ir::linearize::Storage::InSlot,
                },
            );

            // A pointer to a variably-modified array: the extents are the
            // pointee's, and they are what one index step off the pointer
            // has to advance by. C99 6.7.5.2p5 evaluates them where the
            // declaration appears, which is here -- before any initializer.
            //
            // This is the same shape as a variably-modified *parameter*,
            // which is adjusted to exactly this pointer type; see
            // `linearize_function`.
            if !declarator.vla_sizes.is_empty() {
                if let Some(pointee) = self.types.base_type(typ) {
                    let name = self.symbol_name(declarator.symbol);
                    let (dims, elem_type) =
                        self.record_vm_extents(pointee, &declarator.vla_sizes, &name);
                    if let Some(info) = self.locals.get_mut(&declarator.symbol) {
                        info.vm_row_dims = dims;
                        info.vla_elem_type = Some(elem_type);
                    }
                }
            }

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
                } else if let Some(units) = Self::string_literal_units(&init.kind) {
                    // A string literal initializing an automatic object. An
                    // array gets the code units copied in; a pointer gets the
                    // literal's address.
                    //
                    // All four encodings share this path. `u"..."` and
                    // `U"..."` had no arm at all, so they fell through to the
                    // scalar case below and stored the literal's *address*
                    // into the array's first element.
                    if self.types.kind(typ) == TypeKind::Array {
                        self.store_string_units(sym_id, 0, typ, &init.kind, &units);
                    } else {
                        // Pointer initialized with a string literal — store the address
                        let val = self.linearize_expr(init);
                        let init_type = self.expr_type(init);
                        let converted = self.emit_convert(val, init_type, typ);
                        let size = self.types.size_bits(typ);
                        self.emit(Instruction::store(converted, sym_id, 0, typ, size));
                    }
                } else if self.types.is_complex(typ) {
                    self.store_complex_at(sym_id, 0, typ, init);
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
        let ulong_type = self.types.ulong_id;

        // Walk every array level, pairing the run-time size expressions with
        // the levels that actually need one -- a level with a constant extent
        // takes none, so `int a[n][4][m]` consumes `n` then `m`. The element
        // type is the innermost non-array type, which makes one uniform
        // product of extents serve both the total element count and any row
        // size computed later.
        let decl_name = self.symbol_name(declarator.symbol);
        let (dims, elem_type) = self.record_vm_extents(typ, &declarator.vla_sizes, &decl_name);

        let elem_size = self.types.size_bytes(elem_type) as i64;

        // Total element count is the product of every extent. An array
        // declarator always has at least one level, so the empty product this
        // falls back on is unreachable -- but a compiler that answers "one
        // element" beats one that panics.
        let num_elements = self
            .vm_extent_product(&dims)
            .unwrap_or_else(|| self.emit_const(1, ulong_type));

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
                // One index step consumes the outermost extent, so what a row
                // still spans is everything after it.
                vm_row_dims: dims[1..].to_vec(),
                // The slot holds the `alloca` result, not the elements.
                storage: crate::ir::linearize::Storage::Indirect(ptr_type),
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
        if self.current_func_is_inline_definition {
            let is_const = self
                .types
                .modifiers(declarator.typ)
                .contains(TypeModifiers::CONST);
            if !is_const {
                if let Some(pos) = self.current_pos {
                    error(
                        pos,
                        &format!(
                            "inline definition of '{}' cannot define non-const static variable '{}'",
                            self.current_func_name, name_str
                        ),
                    );
                }
            }
        }

        // Generate unique global name: funcname.varname.counter
        //
        // The enclosing function's name may be a verbatim asm label, whose
        // marker belongs at the *start* of a name and not buried inside a
        // derived one -- this static is a symbol of its own, decorated like
        // any other.
        let global_name = format!(
            "{}.{}.{}",
            crate::arch::lir::undecorated(&self.current_func_name),
            name_str,
            self.static_local_counter
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
                vm_row_dims: vec![],
                storage: crate::ir::linearize::Storage::InSlot,
            },
        );

        // Determine initializer (static locals are initialized at compile time)
        let init = declarator.init.as_ref().map_or(Initializer::None, |e| {
            self.ast_init_to_ir(e, declarator.typ)
        });

        // Add as a global - static locals always have internal linkage
        // Check for thread-local storage
        let modifiers = self.types.modifiers(declarator.typ);
        // Const at the object level for section selection (see linearize_init.rs)
        let is_const = super::linearize_init::is_const_object_type(self.types, declarator.typ);
        if modifiers.contains(TypeModifiers::THREAD_LOCAL) {
            self.module.add_global_tls_aligned(
                &global_name,
                declarator.typ,
                init,
                declarator.explicit_align,
                true, // static locals always have internal linkage
                is_const,
            );
        } else {
            self.module.add_global_aligned(
                &global_name,
                declarator.typ,
                init,
                declarator.explicit_align,
                true, // static locals always have internal linkage
                is_const,
            );
        }
    }

    /// Linearize an initializer list for arrays or structs
    /// Check a returned value against the function's declared return type
    /// (C17 6.8.6.4p3, whose constraints are 6.5.16.1's).
    fn check_return_type(&mut self, declared: TypeId, expr: &Expr) {
        let value = self.expr_type(expr);
        let null_constant =
            self.types.kind(value) != TypeKind::Pointer && self.eval_const_expr(expr) == Some(0);
        let Some(fault) = self.types.assignment_fault(declared, value, null_constant) else {
            return;
        };
        if fault == crate::types::AssignFault::FunctionPointerVoid {
            if crate::diag::warning_group_enabled(crate::types::FUNCTION_POINTER_CONV) {
                crate::diag::warning(
                    expr.pos,
                    &gettextrs::gettext(
                        "ISO C forbids return between function pointer and 'void *'",
                    ),
                );
            }
            return;
        }
        let (d_name, v_name) = (
            self.types.format_type(declared, Some(self.strings)),
            self.types.format_type(value, Some(self.strings)),
        );
        if fault.is_error() {
            crate::diag::error_args(
                expr.pos,
                "incompatible types returning '{0}' from a function returning '{1}'",
                &[&v_name, &d_name],
            );
        } else {
            crate::diag::warning_args(
                expr.pos,
                "returning '{0}' from a function with return type '{1}' {2}",
                &[&v_name, &d_name, fault.describe()],
            );
        }
    }

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

                // `char b[] = {"hi"}` initializes *this* array with the
                // string (C17 6.7.9p14). Read as an ordinary element list it
                // was one element, so nothing was copied in. The nested form,
                // `char names[3][4] = {"Sun", "Mon"}`, was already handled
                // below; only the outermost level was missing.
                if self.types.is_integer(elem_type) {
                    if let [only] = elements {
                        if only.designators.is_empty() {
                            if let Some(units) = Self::string_literal_units(&only.value.kind) {
                                self.store_string_units(
                                    base_sym,
                                    base_offset,
                                    typ,
                                    &only.value.kind,
                                    &units,
                                );
                                return;
                            }
                        }
                    }
                }

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
                            ExprKind::StringLit(_)
                                | ExprKind::WideStringLit(_)
                                | ExprKind::Utf16StringLit(_)
                                | ExprKind::Utf32StringLit(_)
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
                    if self.types.is_complex(elem_type) {
                        // A complex element is two halves, not the scalar the
                        // store below assumes. `elem_is_aggregate` is false for
                        // it, so it reaches here.
                        self.store_complex_at(base_sym, offset, elem_type, &last.value);
                        continue;
                    }
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
                                    (visit.bit_offset, visit.bit_width, visit.access_bytes)
                                {
                                    let val = self.linearize_expr(&expr);
                                    let val_type = self.expr_type(&expr);

                                    // Convert to the *member's* type first, not
                                    // straight to the storage unit's. C17 6.7.9p11
                                    // initializes a member by converting to its own
                                    // type, and for `_Bool` that conversion is the
                                    // one that normalizes to 0/1 (6.3.1.2). Going
                                    // directly to the unsigned storage type skipped
                                    // it, so `struct { _Bool f:1; } v = {2};` stored
                                    // 2, truncated it to one bit and read back 0.
                                    // A *second* conversion, to a type derived from
                                    // the access span, used to follow. It truncated
                                    // whenever the span was not 1, 2, 4 or 8 bytes,
                                    // because `bitfield_storage_type` answers
                                    // `unsigned int` for everything else -- so a
                                    // 64-bit field in a sixteen-byte window, or a
                                    // packed nine-byte span, kept 32 bits of its
                                    // initializer. `emit_bitfield_store` takes the
                                    // member-typed value, exactly as the assignment
                                    // path hands it over through `emit_member_store`.
                                    let member_val = self.emit_convert(val, val_type, field_type);
                                    self.emit_bitfield_store(
                                        base_sym,
                                        offset as usize,
                                        bit_off,
                                        bit_w,
                                        storage_size,
                                        member_val,
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
                    // `double _Complex z = {1.0};` lands here rather than on
                    // the complex arm of `linearize_local_decl`, because the
                    // braces make it an initializer list first.
                    if self.types.is_complex(typ) {
                        self.store_complex_at(base_sym, base_offset, typ, &element.value);
                        return;
                    }
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

    /// Store a complex value into `base_sym` at `offset`, as two halves.
    ///
    /// A complex value lives in memory and travels by *address*, so storing it
    /// the way a scalar member is stored writes the address instead of the
    /// value. That is what left every complex member of an automatic aggregate
    /// reading back as a stack address reinterpreted as a double, and what
    /// made a real initializer for one crash outright.
    ///
    /// The initializer's base precision need not match the object's -- and
    /// usually does not, because `I` is `__builtin_complex(0.0, 1.0)`, a
    /// *double* complex, so `float _Complex f = 2.0f + 3.0f*I;` is a
    /// conversion. Reading the source with the target's base type and stride
    /// would mismatch both the width and the step.
    pub(crate) fn store_complex_at(
        &mut self,
        base_sym: PseudoId,
        offset: i64,
        complex_typ: TypeId,
        init: &Expr,
    ) {
        let init_typ = self.expr_type(init);
        let base_typ = self.types.complex_base(complex_typ);
        let base_bits = self.types.size_bits(base_typ);
        let base_bytes = (base_bits / 8) as i64;

        if self.types.is_complex(init_typ) {
            let value_addr = self.complex_operand_addr(init);
            let src_base = self.types.complex_base(init_typ);
            let src_bits = self.types.size_bits(src_base);
            let src_bytes = (src_bits / 8) as i64;

            let val_real = self.alloc_pseudo();
            let val_imag = self.alloc_pseudo();
            self.emit(Instruction::load(
                val_real, value_addr, 0, src_base, src_bits,
            ));
            self.emit(Instruction::load(
                val_imag, value_addr, src_bytes, src_base, src_bits,
            ));
            let val_real = self.emit_convert(val_real, src_base, base_typ);
            let val_imag = self.emit_convert(val_imag, src_base, base_typ);
            self.emit(Instruction::store(
                val_real, base_sym, offset, base_typ, base_bits,
            ));
            self.emit(Instruction::store(
                val_imag,
                base_sym,
                offset + base_bytes,
                base_typ,
                base_bits,
            ));
        } else {
            // A real scalar names only the real half; C99 6.3.1.7 gives the
            // imaginary half a positive zero.
            let val = self.linearize_expr(init);
            let converted = self.emit_convert(val, init_typ, base_typ);
            self.emit(Instruction::store(
                converted, base_sym, offset, base_typ, base_bits,
            ));
            let zero = self.emit_fconst(FloatVal::ZERO, base_typ);
            self.emit(Instruction::store(
                zero,
                base_sym,
                offset + base_bytes,
                base_typ,
                base_bits,
            ));
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
        } else if matches!(
            &value.kind,
            ExprKind::StringLit(_)
                | ExprKind::WideStringLit(_)
                | ExprKind::Utf16StringLit(_)
                | ExprKind::Utf32StringLit(_)
        ) {
            if self.types.kind(field_type) == TypeKind::Array {
                // One `char` per C byte, so the units are the scalar values --
                // `bytes()` gives Rust's UTF-8 encoding of them, which for
                // anything at or above 0x80 is one byte too many *and*
                // disagrees with the null terminator, placed by counting
                // `chars`. `struct { char t[4]; int g; }` initialized with
                // "\xc2\x80" wrote c3 82 00 80 where gcc writes c2 80 00 00.
                //
                // Shared with the two other string-store paths rather than
                // counted a third way here.
                if let Some(units) = Self::string_literal_units(&value.kind) {
                    self.store_string_units(base_sym, offset, field_type, &value.kind, &units);
                }
            } else {
                let val = self.linearize_expr(value);
                let val_type = self.expr_type(value);
                let converted = self.emit_convert(val, val_type, field_type);
                let size = self.types.size_bits(field_type);
                self.emit(Instruction::store(
                    converted, base_sym, offset, field_type, size,
                ));
            }
        } else if self.types.is_complex(field_type) {
            self.store_complex_at(base_sym, offset, field_type, value);
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
        // C17 6.8.4.2p1: the controlling expression shall have integer type.
        // The type was fetched only to size the instruction, so `switch (d)`
        // on a `double` compiled -- and took the wrong branch, since the
        // comparison ran on the value's bit pattern.
        if !self.types.is_integer(expr_type) {
            let named = self.types.format_type(expr_type, Some(self.strings));
            crate::diag::error_args(
                expr.pos,
                "switch quantity is not an integer: '{0}'",
                &[&named],
            );
        }
        let size = self.types.size_bits(expr_type);

        let exit_bb = self.alloc_bb();

        // Push exit block for break handling
        self.break_targets.push(exit_bb);

        // Collect case labels and create basic blocks for each
        let switch_unsigned = self.types.is_unsigned(expr_type);
        let (case_values, has_default) = self.collect_switch_cases(body, switch_unsigned);
        let case_bbs: Vec<BasicBlockId> = case_values.iter().map(|_| self.alloc_bb()).collect();
        let default_bb = if has_default {
            Some(self.alloc_bb())
        } else {
            None
        };

        // Build switch instruction with case -> block mapping
        let switch_cases: Vec<(i64, i64, BasicBlockId)> = case_values
            .iter()
            .zip(case_bbs.iter())
            .map(|((lo, hi), bb)| (*lo, *hi, *bb))
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
            for &(_, _, bb) in &switch_cases {
                self.link_bb(current, bb);
            }
            self.link_bb(current, default_target);
            if default_bb.is_none() {
                self.link_bb(current, exit_bb);
            }
        }

        // Control leaves here for a case label, so a statement standing before
        // the first one is unreachable -- C17 6.8.4.2 gives it no edge from the
        // switch. It was being lowered into the block the switch itself
        // terminates and ran unconditionally: `switch (x) { n = 5; case 1: ; }`
        // set `n` whatever `x` was. `None` is the same "nothing to emit into"
        // state `goto` leaves behind, and the first `case` restores a live
        // block via `switch_bb`.
        self.current_bb = None;

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

    /// Collect case values from a switch body.
    /// Returns (case_values, has_default)
    ///
    /// C17 6.8.4 makes the body one statement, which need not be a compound
    /// one. `switch (x) while (n < 3) { case 1: n++; }` is legal, and matching
    /// only `Stmt::Block` here collected no cases from it -- so the switch was
    /// emitted with an empty table and every value took the default edge.
    /// A non-compound body is one statement, so it is walked as one.
    pub(crate) fn collect_switch_cases(
        &self,
        body: &Stmt,
        unsigned: bool,
    ) -> (Vec<(i64, i64)>, bool) {
        let mut case_values = Vec::new();
        let mut has_default = false;

        match body {
            Stmt::Block(items) => {
                for item in items {
                    if let BlockItem::Statement(stmt) = item {
                        self.collect_cases_from_stmt(
                            stmt,
                            &mut case_values,
                            &mut has_default,
                            unsigned,
                        );
                    }
                }
            }
            stmt => {
                self.collect_cases_from_stmt(stmt, &mut case_values, &mut has_default, unsigned)
            }
        }

        (case_values, has_default)
    }

    /// The block every computed `goto` in this function branches through,
    /// creating it on first use along with the hidden local that carries the
    /// target address to it.
    ///
    /// Its successors are the address-taken labels, and they are linked once
    /// the whole function has been walked -- the set is not complete until
    /// then, since `&&label` may appear after the `goto *` that reaches it.
    fn indirect_dispatch_block(&mut self) -> (BasicBlockId, PseudoId) {
        if let Some(existing) = self.indirect_dispatch {
            return existing;
        }
        let void_ptr = self.types.void_ptr_id;
        let slot = self.alloc_pseudo();
        let name = format!("__goto_target.{}", slot.0);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(crate::ir::Pseudo::sym(slot, name.clone()));
            func.add_local(&name, slot, void_ptr, false, false, None, None);
        }

        let dispatch_bb = self.alloc_bb();
        let saved = self.current_bb;
        self.switch_bb(dispatch_bb);
        let loaded = self.alloc_pseudo();
        self.emit(Instruction::load(loaded, slot, 0, void_ptr, 64));
        self.emit(Instruction::indirect_br(loaded));
        self.current_bb = saved;

        self.indirect_dispatch = Some((dispatch_bb, slot));
        (dispatch_bb, slot)
    }

    /// Link the dispatch block to every label whose address was taken.
    ///
    /// Called once the function body is walked, because `&&label` may appear
    /// after the `goto *` that can reach it.
    pub(crate) fn finish_indirect_dispatch(&mut self) {
        // `&&label` naming a label the function never defines. The block
        // `get_or_create_label` minted stays empty and unterminated, so
        // branching to it ran off the end of the function -- the program hung.
        // Checked here because a forward reference is legal.
        for (name, pos) in std::mem::take(&mut self.label_addr_refs) {
            if !self.defined_labels.contains(&name) {
                crate::diag::error_args(pos, "label '{0}' used but not defined", &[&name]);
            }
        }

        let Some((dispatch_bb, _)) = self.indirect_dispatch else {
            // The address was taken but this function never branches on it --
            // it was stored for someone else, or only compared. The blocks
            // still have to survive DCE, or the symbol the address refers to
            // is never emitted and the link fails on an undefined `.L` label.
            for bb in self.addr_taken_labels.clone() {
                self.get_or_create_bb(bb).addr_taken = true;
            }
            return;
        };
        for bb in self.addr_taken_labels.clone() {
            self.link_bb(dispatch_bb, bb);
        }
    }

    /// C17 6.8.6.1p1: a `goto` shall not jump from outside the scope of an
    /// identifier having a variably modified type to inside it; 6.8.4.2p... the
    /// same for a `switch` reaching a `case` inside such a scope.
    ///
    /// The rule exists because entering the scope without executing the
    /// declaration leaves the object's size never computed: the array is
    /// whatever the stack held. Jumping *out* of the scope, within it, or to a
    /// label that precedes the declaration are all fine, and all are exercised
    /// by the accept-side tests.
    ///
    /// Reported at the jump, where gcc points, and naming the declaration that
    /// could not be entered, which gcc does not -- the position says where to
    /// look and the name says what the problem is. This used to report at the
    /// declaration for want of anything better: the jump and label statements
    /// carried no position until #C106 gave them one.
    pub(crate) fn check_jumps_into_variably_modified_scopes(&self, body: &Stmt) {
        let mut w = VmScopeWalk {
            open: Vec::new(),
            declared: Vec::new(),
            labels: Vec::new(),
            gotos: Vec::new(),
            bad_case_ids: Vec::new(),
            loop_depth: 0,
            switch_depth: 0,
            stray_jumps: Vec::new(),
        };
        w.walk(body, None);

        // 6.8.1p3: a label name is unique within the function it appears in.
        // Two labels of one name were silently merged into one basic block, so
        // `L: i++; if (i<2) goto L; L: return i;` looped forever.
        for (i, (name, _, pos)) in w.labels.iter().enumerate() {
            if w.labels[..i].iter().any(|(earlier, _, _)| earlier == name) {
                let spelled = self.strings.get(*name).to_string();
                crate::diag::error_args(*pos, "duplicate label '{0}'", &[&spelled]);
            }
        }

        // A `goto` is illegal exactly when its label sits inside a scope the
        // `goto` itself is not already in.
        for (name, from, goto_pos) in &w.gotos {
            let Some((_, to, _)) = w.labels.iter().find(|(n, _, _)| n == name) else {
                // 6.8.6.1p1: the label has to exist. Minting a block for it
                // left that block unterminated, and control fell out of the
                // function through whatever followed in layout order -- so the
                // program built, linked, and segfaulted.
                let spelled = self.strings.get(*name).to_string();
                crate::diag::error_args(*goto_pos, "label '{0}' used but not defined", &[&spelled]);
                continue;
            };
            if let Some(id) = to.iter().find(|id| !from.contains(id)) {
                self.report_vm_jump(&w.declared[*id], "jump", *goto_pos);
            }
        }
        for (id, pos) in w.bad_cases() {
            self.report_vm_jump(&w.declared[id], "switch jump", pos);
        }

        for (pos, what) in &w.stray_jumps {
            let message = match *what {
                "break" => "break statement not within loop or switch",
                "continue" => "continue statement not within a loop",
                "case" => "case label not within a switch statement",
                _ => "'default' label not within a switch statement",
            };
            error(*pos, &gettextrs::gettext(message));
        }
    }

    /// Name the declaration a jump would have entered without executing.
    fn report_vm_jump(&self, decl: &VmDecl, what: &str, pos: Position) {
        let name = self.strings.get(self.symbols.get(decl.symbol).name);
        error(
            pos,
            &format!("{what} into the scope of '{name}', which has a variably modified type",),
        );
    }

    /// Report a case endpoint the constant folder could not reduce.
    ///
    /// Split out because a range has two of them and both need the same
    /// distinction: a non-constant label is the program's error, while a
    /// constant this partial evaluator cannot fold is ours.
    fn report_unfoldable_case(&self, expr: &Expr) {
        if self.expr_is_runtime(expr) {
            error(expr.pos, "case label is not an integer constant expression");
        } else {
            error(
                expr.pos,
                "case label is a constant expression this compiler cannot evaluate",
            );
        }
    }

    pub(crate) fn collect_cases_from_stmt(
        &self,
        stmt: &Stmt,
        case_values: &mut Vec<(i64, i64)>,
        has_default: &mut bool,
        unsigned: bool,
    ) {
        match stmt {
            Stmt::Case(expr, high) => {
                // Extract constant value from case expression
                if let Some(val) = self.eval_const_expr(expr) {
                    let val = val as i64; // switch cases truncated to i64

                    // A GNU range `case lo ... hi:`. An absent high endpoint
                    // is the ordinary label, held as the degenerate range
                    // `(v, v)` so that everything downstream has one shape.
                    let hi = match high {
                        None => Some(val),
                        Some(hi_expr) => match self.eval_const_expr(hi_expr) {
                            Some(h) => Some(h as i64),
                            None => {
                                self.report_unfoldable_case(hi_expr);
                                None
                            }
                        },
                    };
                    let Some(hi) = hi else { return };

                    // 6.8.4.2p3 forbids two equal case constants, and GCC
                    // extends that to overlapping ranges -- an overlap would
                    // otherwise make one arm silently unreachable, since the
                    // body walk resolves a label by finding the first match.
                    // Order by the switch type's own signedness. The endpoints
                    // are carried as `i64`, so an unsigned 64-bit bound above
                    // `i64::MAX` looks negative: `case 0ul ... ULONG_MAX:` read
                    // as an empty range and never matched.
                    let below = |a: i64, b: i64| {
                        if unsigned {
                            (a as u64) < (b as u64)
                        } else {
                            a < b
                        }
                    };
                    let at_most = |a: i64, b: i64| !below(b, a);
                    if below(hi, val) {
                        // GCC accepts an empty range, warns, and never matches
                        // it. Nothing is recorded, so nothing can overlap it.
                        crate::diag::warning(expr.pos, "empty range specified");
                        return;
                    }
                    if let Some((lo2, hi2)) = case_values
                        .iter()
                        .find(|(lo2, hi2)| at_most(val, *hi2) && at_most(*lo2, hi))
                        .copied()
                    {
                        let what = if val == hi && lo2 == hi2 {
                            format!("duplicate case value '{}' in switch", val)
                        } else {
                            format!(
                                "duplicate (or overlapping) case value: {}..{} overlaps {}..{}",
                                val, hi, lo2, hi2
                            )
                        };
                        error(expr.pos, &what);
                    }
                    case_values.push((val, hi));
                } else if self.expr_is_runtime(expr) {
                    // A non-constant label can never match; it used to be
                    // dropped without a word.
                    error(expr.pos, "case label is not an integer constant expression");
                } else {
                    // Constant in principle, but `eval_const_expr` is a partial
                    // evaluator and could not fold it. Saying the program is
                    // invalid would be a false claim about the source — this is
                    // our limit, not its error. Either way the label cannot be
                    // emitted, so it still has to be reported rather than
                    // silently dropped.
                    error(
                        expr.pos,
                        "case label is a constant expression this compiler cannot evaluate",
                    );
                }
            }
            Stmt::Default(_) => {
                // C99 6.8.4.2p3: at most one default label per switch.
                if *has_default {
                    error(
                        self.current_pos.unwrap_or_default(),
                        "multiple default labels in one switch",
                    );
                }
                *has_default = true;
            }
            // Recursively check labeled statements
            Stmt::Label { stmt, .. } => {
                self.collect_cases_from_stmt(stmt, case_values, has_default, unsigned);
            }
            // Recurse into nested statements for Duff's device pattern
            // (case labels inside loops/blocks within a switch)
            Stmt::Block(items) => {
                for item in items {
                    if let BlockItem::Statement(s) = item {
                        self.collect_cases_from_stmt(s, case_values, has_default, unsigned);
                    }
                }
            }
            Stmt::DoWhile { body, .. } | Stmt::While { body, .. } | Stmt::For { body, .. } => {
                self.collect_cases_from_stmt(body, case_values, has_default, unsigned);
            }
            Stmt::If {
                then_stmt,
                else_stmt,
                ..
            } => {
                self.collect_cases_from_stmt(then_stmt, case_values, has_default, unsigned);
                if let Some(e) = else_stmt {
                    self.collect_cases_from_stmt(e, case_values, has_default, unsigned);
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
    /// Copy a string literal's code units into an array object, followed by
    /// its null terminator.
    ///
    /// Shared by the two ways a string can initialize an array: written
    /// directly (`char b[] = "hi"`) or enclosed in braces
    /// (`char b[] = {"hi"}`, C17 6.7.9p14). They used to be separate code, and
    /// only the first one worked.
    pub(crate) fn store_string_units(
        &mut self,
        base_sym: PseudoId,
        base_offset: i64,
        arr_typ: TypeId,
        kind: &ExprKind,
        units: &[i128],
    ) {
        let default_elem = match kind {
            ExprKind::StringLit(_) => self.types.char_id,
            _ => self.types.int_id,
        };
        let elem_type = self.types.base_type(arr_typ).unwrap_or(default_elem);
        let elem_size = self.types.size_bits(elem_type);
        let elem_bytes = (elem_size / 8) as i64;

        // C17 6.7.9p14 lets an array be exactly as long as the string, in
        // which case the terminating null is dropped rather than written --
        // `char b[2] = "hi"` holds two characters and no terminator. Writing
        // it unconditionally put one byte past the end of the object.
        //
        // An array with no size yet takes it from this initializer, so it
        // always has room.
        let capacity = self
            .types
            .array_size(arr_typ)
            .filter(|&n| n > 0)
            .unwrap_or(units.len() + 1);

        for (i, unit) in units.iter().enumerate().take(capacity) {
            let val = self.emit_const(*unit, elem_type);
            self.emit(Instruction::store(
                val,
                base_sym,
                base_offset + (i as i64) * elem_bytes,
                elem_type,
                elem_size,
            ));
        }
        if units.len() < capacity {
            let null_val = self.emit_const(0, elem_type);
            self.emit(Instruction::store(
                null_val,
                base_sym,
                base_offset + (units.len() as i64) * elem_bytes,
                elem_type,
                elem_size,
            ));
        }
    }

    /// The code units a string literal contributes to an array initializer,
    /// or `None` if this is not a string literal.
    ///
    /// A narrow literal's parsed form holds one C byte per Rust `char`, so the
    /// units are the scalar values — iterating `bytes()` UTF-8-encodes anything
    /// at or above 0x80, which turned `char a[] = "\x80"` into the two bytes
    /// 0xC2 0x80 and left the array one byte short of what `sizeof` reported.
    fn string_literal_units(kind: &ExprKind) -> Option<Vec<i128>> {
        match kind {
            ExprKind::StringLit(s) => Some(s.chars().map(|c| (c as u32 as u8) as i128).collect()),
            ExprKind::WideStringLit(s) => Some(s.chars().map(|c| c as u32 as i128).collect()),
            ExprKind::Utf16StringLit(u) => Some(u.iter().map(|c| *c as i128).collect()),
            ExprKind::Utf32StringLit(u) => Some(u.iter().map(|c| *c as i128).collect()),
            _ => None,
        }
    }

    /// Does this expression *provably* depend on a runtime value?
    ///
    /// The complement of "constant" is not "unfoldable": `eval_const_expr` is a
    /// partial evaluator, so treating everything it declines as a constraint
    /// violation turned each of its gaps into a rejection of valid code. This
    /// answers the narrower question, and answers `false` when unsure — a
    /// conservative direction, because the caller's fallback blames the
    /// compiler rather than the program.
    fn expr_is_runtime(&self, expr: &Expr) -> bool {
        match &expr.kind {
            // Reading an object, calling a function, or writing anything.
            ExprKind::Ident(sym) => !self.symbols.get(*sym).is_enum_constant(),
            ExprKind::Call { .. }
            | ExprKind::Assign { .. }
            | ExprKind::PostInc(_)
            | ExprKind::PostDec(_)
            | ExprKind::Member { .. }
            | ExprKind::Arrow { .. }
            | ExprKind::Index { .. }
            | ExprKind::CompoundLiteral { .. }
            | ExprKind::FuncName => true,

            ExprKind::Unary { op, operand } => {
                matches!(op, UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::Deref)
                    || self.expr_is_runtime(operand)
            }
            ExprKind::Binary { left, right, .. } => {
                self.expr_is_runtime(left) || self.expr_is_runtime(right)
            }
            ExprKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                self.expr_is_runtime(cond)
                    || self.expr_is_runtime(then_expr)
                    || self.expr_is_runtime(else_expr)
            }
            ExprKind::CondElvis { cond, else_expr } => {
                self.expr_is_runtime(cond) || self.expr_is_runtime(else_expr)
            }
            ExprKind::Cast { expr: inner, .. } => self.expr_is_runtime(inner),

            // `sizeof` of a variable length array type is the exception: it
            // does evaluate its operand, and is genuinely not an integer
            // constant expression. Saying otherwise would send
            // `case sizeof(int[n]):` to the caller's fallback message, which
            // blames the compiler for a limitation rather than the program
            // for a constraint violation.
            ExprKind::SizeofType(typ, dims) => {
                crate::parse::ast::sizeof_type_is_runtime(self.types, *typ, dims)
            }

            // `sizeof` otherwise, and `_Alignof`, do not evaluate their
            // operand, and literals are constant. Anything else: not proven
            // either way.
            _ => false,
        }
    }

    /// The value of a `const`-qualified object whose own initializer already
    /// folded to a constant, if this identifier names one.
    ///
    /// C makes no such object a constant expression -- that is a C++ rule --
    /// but gcc folds it in a static initializer, silently and even under
    /// `-pedantic`, so `const int c = 5; int w = c;` compiles everywhere. The
    /// qualifier alone is not enough: `const int c;` and `extern const int c;`
    /// have no visible value and gcc rejects both, which returning `None` here
    /// leaves to the caller's diagnostic.
    ///
    /// The already-emitted global is the record consulted, so this answers only
    /// for an object defined earlier in the translation unit -- which is what
    /// "visible value" means.
    fn const_object_value(&self, symbol_id: crate::symbol::SymbolId) -> Option<i128> {
        let name = self.symbol_name(symbol_id);
        let key = format!("{}.{}", self.current_func_name, name);
        let global_name = match self.static_locals.get(&key) {
            Some(info) => info.global_name.as_str(),
            None => name.as_str(),
        };
        let global = self
            .module
            .globals
            .iter()
            .find(|g| g.name == global_name && g.is_const)?;
        match &global.init {
            crate::ir::Initializer::Int(v) => Some(*v),
            _ => None,
        }
    }

    /// [`Self::const_object_value`] for a floating `const` object.
    fn const_object_float_value(&self, symbol_id: crate::symbol::SymbolId) -> Option<FloatVal> {
        let name = self.symbol_name(symbol_id);
        let key = format!("{}.{}", self.current_func_name, name);
        let global_name = match self.static_locals.get(&key) {
            Some(info) => info.global_name.as_str(),
            None => name.as_str(),
        };
        let global = self
            .module
            .globals
            .iter()
            .find(|g| g.name == global_name && g.is_const)?;
        match &global.init {
            crate::ir::Initializer::Float(v) => Some(*v),
            crate::ir::Initializer::Int(v) => Some(FloatVal::from_i128(*v)),
            _ => None,
        }
    }

    /// Evaluate an integer constant expression under C's own rule: an
    /// enumeration constant is the only identifier that has a value here.
    pub(crate) fn eval_const_expr(&self, expr: &Expr) -> Option<i128> {
        self.eval_const_expr_scoped(ConstScope::Standard, expr)
    }

    /// `eval_const_expr`, plus the `const`-object folding gcc performs in a
    /// *static initializer* and nowhere else.
    ///
    /// `const int c = 5; int w = c + 1;` compiles everywhere and was rejected
    /// here. The folding must not reach `eval_const_expr`'s other callers: C
    /// makes a `const` object no kind of constant expression, so `int a[c];`
    /// is a VLA and `case c:` an error, in gcc as here.
    pub(crate) fn eval_const_init_expr(&self, expr: &Expr) -> Option<i128> {
        self.eval_const_expr_scoped(ConstScope::StaticInitializer, expr)
    }

    fn eval_const_expr_scoped(&self, scope: ConstScope, expr: &Expr) -> Option<i128> {
        crate::constexpr::eval(self, scope, expr)
    }

    /// Fold a constant floating expression for a static initializer.
    ///
    /// Every step is done in the expression's own format, exactly, and rounded
    /// once -- the same thing the target would do at run time. Folding through
    /// `f64` instead cost a `long double` eleven significand bits and a
    /// `__float128` sixty, so `static __float128 c = 1.0q/3.0q;` disagreed with
    /// the *same initializer written for a local*, which is computed at run
    /// time and was right.
    /// Fold a floating constant expression for a static initializer, with the
    /// `const`-object folding [`Self::eval_const_init_expr`] describes.
    ///
    /// There is no `Standard` entry point beside this one because no strict
    /// context reaches the floating evaluator: an array size, a `case` label
    /// and `_Static_assert` all take an *integer* constant expression, and a
    /// floating one is refused before it gets here. The scope is still a
    /// parameter rather than a constant so that adding such a caller is a
    /// one-line change and cannot silently inherit the lax rule.
    pub(crate) fn eval_const_float_init_expr(&self, expr: &Expr) -> Option<FloatVal> {
        self.eval_const_float_expr_scoped(ConstScope::StaticInitializer, expr)
    }

    fn eval_const_float_expr_scoped(&self, scope: ConstScope, expr: &Expr) -> Option<FloatVal> {
        match &expr.kind {
            ExprKind::FloatLit(v) => Some(*v),
            // Exact: a `u128` mantissa has no more bits than the significand,
            // where `f64` would have rounded anything past the 53rd.
            ExprKind::IntLit(v) => Some(FloatVal::from_i128(*v as i128)),
            ExprKind::CharLit(c) => Some(FloatVal::from_i128(*c as i128)),

            // A `const double` folds in a static initializer exactly as a
            // `const int` does; without this `const double d = 2.5; double x =
            // d * 2;` was rejected while the integer spelling compiled.
            ExprKind::Ident(symbol_id) if scope == ConstScope::StaticInitializer => {
                self.const_object_float_value(*symbol_id)
            }

            ExprKind::Unary { op, operand } => {
                let val = self.eval_const_float_expr_scoped(scope, operand)?;
                match op {
                    UnaryOp::Neg => Some(val.negated()),
                    _ => None,
                }
            }

            // The operation is done in the format of its own result type,
            // which is the type the usual arithmetic conversions already gave
            // this node -- not in whatever width the operands were written at.
            ExprKind::Binary { op, left, right } => {
                let l = self.eval_const_float_expr_scoped(scope, left)?;
                let r = self.eval_const_float_expr_scoped(scope, right)?;
                let fmt = expr.typ.and_then(|t| self.types.fp_format(t))?;
                Some(match op {
                    BinaryOp::Add => l.add(r, fmt),
                    BinaryOp::Sub => l.sub(r, fmt),
                    BinaryOp::Mul => l.mul(r, fmt),
                    BinaryOp::Div => l.div(r, fmt),
                    _ => return None,
                })
            }

            // A cast rounds to the target format. Discarding the cast type
            // let `(float)0.1q` keep every bit of its binary128 value in a
            // static initializer, where the same cast at run time rounds.
            ExprKind::Cast {
                expr: inner,
                cast_type,
            } => {
                let val = self.eval_const_float_expr_scoped(scope, inner)?;
                Some(match self.types.fp_format(*cast_type) {
                    Some(fmt) => val.round_to_format(fmt),
                    None => val,
                })
            }

            _ => None,
        }
    }

    /// Evaluate a static address expression (for initializers like `&symbol.field`)
    ///
    /// Reached only from the initializer path, so a subscript folds with
    /// [`Self::eval_const_init_expr`]: `const int c = 5; int *p = &a[c-3];`
    /// is an ordinary static initializer and gcc accepts it.
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
                let idx = self.eval_const_init_expr(index)?;

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
                let int_val = self.eval_const_init_expr(int_expr)?;

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
        case_values: &[(i64, i64)],
        case_bbs: &[BasicBlockId],
        default_bb: Option<BasicBlockId>,
    ) {
        let mut case_idx = 0;

        // A non-compound body is one statement; lowering it through the same
        // walker is what keeps the labels inside it reachable. See
        // `collect_switch_cases`, which has to agree about this.
        match body {
            Stmt::Block(items) => {
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
            stmt => {
                self.linearize_switch_stmt(stmt, case_values, case_bbs, default_bb, &mut case_idx)
            }
        }
    }

    pub(crate) fn linearize_switch_stmt(
        &mut self,
        stmt: &Stmt,
        case_values: &[(i64, i64)],
        case_bbs: &[BasicBlockId],
        default_bb: Option<BasicBlockId>,
        case_idx: &mut usize,
    ) {
        match stmt {
            Stmt::Case(expr, high) => {
                // Find the matching case block. A label is identified by its
                // whole range, so that `case 1 ... 3:` and a later `case 1:`
                // could not resolve to the same block -- the overlap check
                // rejects that pair anyway, but matching on the low endpoint
                // alone would have made the two indistinguishable here.
                if let Some(val) = self.eval_const_expr(expr) {
                    let lo = val as i64;
                    let hi = match high {
                        None => Some(lo),
                        Some(hi_expr) => self.eval_const_expr(hi_expr).map(|h| h as i64),
                    };
                    let Some(hi) = hi else { return };
                    if let Some(idx) = case_values.iter().position(|r| *r == (lo, hi)) {
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
            Stmt::Default(_) => {
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

            Stmt::Label { name, stmt, .. } => {
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
        // Track which outputs need no post-asm processing — true for
        // memory-class outputs (`=m`/`+m`/`=o`/`+o`/...), where the asm
        // wrote the new value directly through the memory operand and
        // no follow-up store is required.
        let mut skip_post_handling: Vec<bool> = Vec::new();

        // Pre-scan inputs to learn which outputs will be tied to a matching
        // input (e.g. `"0"(x)`).  When a `+r` output IS tied to a matching
        // input, the tied input later emits a Copy that overwrites the
        // output's pseudo with the input's value — which makes the read-
        // half load redundant *and* an SSA violation (the optimizer-stage
        // validator I1 rejects two definitions of the same pseudo).
        // Bare `+r` with no tied input must still load the lvalue's
        // current value (only producer of the initial register contents),
        // so the load is gated on whether any input matches this output.
        let tied_inputs: Vec<bool> = {
            let mut out_has_tied_input = vec![false; outputs.len()];
            for op in inputs {
                let (_, _, matching) = self.parse_asm_constraint(&op.constraint);
                if let Some(idx) = matching {
                    if idx < out_has_tied_input.len() {
                        out_has_tied_input[idx] = true;
                    }
                }
            }
            out_has_tied_input
        };

        // Process output operands
        for (output_idx, op) in outputs.iter().enumerate() {
            // Parse constraint to get flags
            let (is_memory, is_readwrite, _matching) = self.parse_asm_constraint(&op.constraint);

            // Get symbolic name if present
            let name = op.name.map(|n| self.str(n).to_string());

            let typ = self.expr_type(&op.expr);
            let size = self.types.size_bits(typ);

            // For memory-class outputs (`=m`/`+m`/...): the asm operand
            // is the ADDRESS of the lvalue, not a register holding a
            // value. The asm performs the read/write through that
            // address with its own `ldr`/`str` (or x86 equivalents), so
            // no initial Load or post-asm Store is needed at the IR
            // level. Just use the lvalue address as the asm operand
            // pseudo directly.
            if is_memory {
                let addr = self.linearize_lvalue(&op.expr);

                if is_readwrite {
                    // `+m` — also add as matching input so the same
                    // operand number works on both sides.
                    ir_inputs.push(AsmConstraint {
                        pseudo: addr,
                        name: name.clone(),
                        matching_output: Some(ir_outputs.len()),
                        constraint: op.constraint.clone(),
                        size,
                    });
                }

                ir_outputs.push(AsmConstraint {
                    pseudo: addr,
                    name,
                    matching_output: None,
                    constraint: op.constraint.clone(),
                    size,
                });

                param_outputs.push(None);
                skip_post_handling.push(true);
                continue;
            }

            // Non-memory output (register or value class): allocate a
            // fresh pseudo for the asm to write into.
            let pseudo = self.alloc_pseudo();

            // Check if this output is a parameter (SSA value, not memory location)
            let param_info = self.get_param_if_ident(&op.expr);

            // For read-write outputs ("+r"), load the initial value into the SAME pseudo
            // so that input and output use the same register — unless a
            // tied matching input (e.g. `"0"(x)`) will later supply its own
            // value via a Copy into the same pseudo, which would otherwise
            // produce a double-definition (and violates the SSA-style
            // validator invariant after optimization).
            if is_readwrite {
                if !tied_inputs[output_idx] {
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
            skip_post_handling.push(false);
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
            // Memory-class outputs (`=m`/`+m`/...) need no post-asm
            // handling — the asm wrote the new value directly through
            // the memory operand.
            if skip_post_handling[i] {
                continue;
            }

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

    /// Parse an asm constraint string to extract flags.
    /// Returns `(is_memory, is_readwrite, matching_output)`.
    ///
    /// `is_memory` is true iff the constraint *requires* memory — i.e.,
    /// every alternative listed is a memory class (`m`, `o`, `V`, `Q`).
    /// For multi-alt constraints like `"rm"` / `"rmi"` / `"g"` that also
    /// list a register or immediate class, the linearizer evaluates the
    /// operand as a value (`linearize_expr`) and lets the codegen
    /// substitute register vs memory syntax based on the operand's
    /// actual location. Taking the lvalue (address) for a multi-alt
    /// like `"rm"` would force every asm substitution to use the
    /// address bits as if they were the value — broken for the very
    /// common `"+rm"` increment pattern.
    ///
    /// Note: Early clobber (&) is parsed but not used since our simple
    /// register allocator doesn't share registers between inputs and
    /// outputs anyway.
    pub(crate) fn parse_asm_constraint(&self, constraint: &str) -> (bool, bool, Option<usize>) {
        let mut is_readwrite = false;
        let mut matching = None;

        for c in constraint.chars() {
            match c {
                '+' => is_readwrite = true,
                '0'..='9' => matching = Some((c as u8 - b'0') as usize),
                _ => {}
            }
        }

        // The memory decision lives on `AsmConstraint::is_memory` so that
        // liveness analysis, which cannot reach the linearizer, applies the
        // identical rule. Two copies of these letter sets is exactly how a
        // memory output came to look write-only to DCE.
        let probe = AsmConstraint {
            pseudo: PseudoId(0),
            name: None,
            matching_output: None,
            constraint: constraint.to_string(),
            size: 0,
        };

        (probe.is_memory(), is_readwrite, matching)
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

/// A declaration whose type is variably modified, and so whose scope may not
/// be entered by a jump (C17 6.8.6.1p1).
pub(crate) struct VmDecl {
    pub(crate) symbol: crate::symbol::SymbolId,
}

/// Walks a function body recording, for every label and every jump, which
/// variably modified scopes enclose it.
///
/// A scope opens at the declaration and runs to the end of its block, so the
/// walk is order-sensitive within a block: a label *before* the declaration is
/// outside the scope and may be jumped to, which is why the scopes are pushed
/// as the items are visited rather than collected up front.
struct VmScopeWalk {
    /// Ids of the scopes currently open, innermost last.
    open: Vec<usize>,
    /// Every variably modified declaration seen, indexed by scope id.
    declared: Vec<VmDecl>,
    /// Label name, the scopes enclosing it, and where it was written.
    labels: Vec<(StringId, Vec<usize>, Position)>,
    /// Each `goto`, the scopes enclosing it, and where it was written.
    gotos: Vec<(StringId, Vec<usize>, Position)>,
    /// Scope ids a `case`/`default` was found inside but its `switch` was not,
    /// each with the position of the label that reached it.
    bad_case_ids: Vec<(usize, Position)>,
    /// How many loops enclose the statement being visited. `continue` needs
    /// one; `break` accepts a `switch` as well.
    loop_depth: u32,
    /// How many `switch`es enclose the statement being visited.
    switch_depth: u32,
    /// A `break` or `continue` with nothing to jump out of, and a `case` or
    /// `default` with no `switch`, as (position, keyword).
    stray_jumps: Vec<(Position, &'static str)>,
}

impl VmScopeWalk {
    /// The offending scopes, each reported once however many `case` labels
    /// sit inside it.
    fn bad_cases(&self) -> Vec<(usize, Position)> {
        let mut out: Vec<(usize, Position)> = Vec::new();
        for (id, pos) in &self.bad_case_ids {
            if !out.iter().any(|(seen, _)| seen == id) {
                out.push((*id, *pos));
            }
        }
        out
    }

    /// `switch_scopes` is the scope list in force at the innermost enclosing
    /// `switch`, against which a `case` label is judged.
    fn walk(&mut self, stmt: &Stmt, switch_scopes: Option<&[usize]>) {
        match stmt {
            Stmt::Block(items) => {
                let depth = self.open.len();
                for item in items {
                    match item {
                        BlockItem::Declaration(decl) => self.open_scopes(decl),
                        BlockItem::Statement(s) => self.walk(s, switch_scopes),
                    }
                }
                // Leaving the block closes every scope it opened.
                self.open.truncate(depth);
            }

            Stmt::Label { name, stmt, pos } => {
                self.labels.push((*name, self.open.clone(), *pos));
                self.walk(stmt, switch_scopes);
            }
            Stmt::Goto { name, pos } => self.gotos.push((*name, self.open.clone(), *pos)),

            // 6.8.6.3p1 and 6.8.6.2p1: a `break` needs an enclosing loop or
            // switch and a `continue` an enclosing loop. Checked here rather
            // than in the `linearize_stmt` arms, which look like the obvious
            // place -- they fail exactly when the target stack is empty -- but
            // a switch body is lowered by `linearize_switch_stmt`, which
            // delegates back for nested constructs, so an arm there cannot
            // tell "outside every construct" from "reached by delegation".
            Stmt::Break(pos) => {
                if self.loop_depth == 0 && self.switch_depth == 0 {
                    self.stray_jumps.push((*pos, "break"));
                }
            }
            Stmt::Continue(pos) => {
                if self.loop_depth == 0 {
                    self.stray_jumps.push((*pos, "continue"));
                }
            }

            Stmt::Case(..) | Stmt::Default(_) => {
                // 6.8.1p2: a `case` or `default` belongs to a `switch`.
                if self.switch_depth == 0 {
                    let (pos, what) = match stmt {
                        Stmt::Case(expr, _) => (expr.pos, "case"),
                        Stmt::Default(pos) => (*pos, "default"),
                        _ => unreachable!(),
                    };
                    self.stray_jumps.push((pos, what));
                }

                // Reaching a `case` transfers control from the `switch`, so
                // any scope open here but not there would be entered without
                // its declaration running.
                if let Some(outer) = switch_scopes {
                    let label_pos = match stmt {
                        Stmt::Case(expr, _) => expr.pos,
                        Stmt::Default(pos) => *pos,
                        _ => unreachable!("only a case or default reaches this arm"),
                    };
                    for id in &self.open {
                        if !outer.contains(id) {
                            self.bad_case_ids.push((*id, label_pos));
                        }
                    }
                }
            }

            // A `switch` becomes the reference point for the labels inside it.
            // Its own scopes are captured before the body is walked.
            Stmt::Switch { body, .. } => {
                let outer = self.open.clone();
                self.switch_depth += 1;
                self.walk(body, Some(&outer));
                self.switch_depth -= 1;
            }

            Stmt::If {
                then_stmt,
                else_stmt,
                ..
            } => {
                self.walk(then_stmt, switch_scopes);
                if let Some(e) = else_stmt {
                    self.walk(e, switch_scopes);
                }
            }
            Stmt::While { body, .. } | Stmt::DoWhile { body, .. } => {
                self.loop_depth += 1;
                self.walk(body, switch_scopes);
                self.loop_depth -= 1;
            }
            Stmt::For { init, body, .. } => {
                // A declaration in the init clause scopes over the body.
                let depth = self.open.len();
                if let Some(ForInit::Declaration(decl)) = init {
                    self.open_scopes(decl);
                }
                self.loop_depth += 1;
                self.walk(body, switch_scopes);
                self.loop_depth -= 1;
                self.open.truncate(depth);
            }

            _ => {}
        }
    }

    fn open_scopes(&mut self, decl: &Declaration) {
        for d in &decl.declarators {
            // A declarator with size expressions is variably modified --
            // whether it is the array itself or a pointer to one, both of
            // which C17 6.7.6.2 calls variably modified and gcc refuses to
            // let a jump enter.
            if !d.vla_sizes.is_empty() {
                let id = self.declared.len();
                self.declared.push(VmDecl { symbol: d.symbol });
                self.open.push(id);
            }
        }
    }
}

/// The linearizer's half of the shared C17 6.6 walk.
///
/// [`crate::constexpr`] owns the walk; what differs from the parser's half is
/// the `const`-object folding gcc performs in a static initializer, and that a
/// floating subexpression folds in its own format rather than through `f64`.
impl crate::constexpr::ConstEnv for Linearizer<'_> {
    fn types(&self) -> &TypeTable {
        self.types
    }

    fn ident_value(&self, sym: crate::symbol::SymbolId, scope: ConstScope) -> Option<i128> {
        let symbol = self.symbols.get(sym);
        if symbol.is_enum_constant() {
            return symbol.enum_value;
        }
        match scope {
            ConstScope::Standard => None,
            ConstScope::StaticInitializer => self.const_object_value(sym),
        }
    }

    fn struct_of(&self, typ: TypeId) -> TypeId {
        self.resolve_struct_type(typ)
    }

    /// Folded at the expression's own precision and narrowed once, rather than
    /// computed in `f64`: the two arms that ask for this -- an ordering and a
    /// truncating cast -- are both decided correctly by the `f64` result, and
    /// the exactness matters underneath it.
    fn float_value(&self, scope: ConstScope, expr: &Expr) -> Option<f64> {
        self.eval_const_float_expr_scoped(scope, expr)
            .map(|v| v.to_f64())
    }
}
