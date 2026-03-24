//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT

//! Initializer and global declaration linearization

use super::linearize::*;
use super::Initializer;
use crate::diag::error;
use crate::parse::ast::{BinaryOp, Declaration, Designator, Expr, ExprKind, InitElement, UnaryOp};
use crate::strings::StringId;
use crate::types::{MemberInfo, TypeId, TypeKind, TypeModifiers};
use std::collections::HashMap;

impl<'a> super::linearize::Linearizer<'a> {
    // ========================================================================
    // Global declarations
    // ========================================================================

    pub(crate) fn linearize_global_decl(&mut self, decl: &Declaration) {
        for declarator in &decl.declarators {
            // Use storage_class from declarator (extern, static, _Thread_local, etc.)
            // These are NOT stored in the type system
            let storage_class = declarator.storage_class;
            let name = self.symbol_name(declarator.symbol);

            // Skip typedef declarations - they don't define storage
            if storage_class.contains(TypeModifiers::TYPEDEF) {
                continue;
            }

            // Function declarations without bodies are external functions
            // Track them in extern_symbols so codegen uses GOT access
            if self.types.kind(declarator.typ) == TypeKind::Function {
                // Check if not defined in this module (forward refs will be cleaned up later)
                if !self.module.functions.iter().any(|f| f.name == name) {
                    self.module.extern_symbols.insert(name);
                }
                continue;
            }

            // Skip extern declarations - they don't define storage
            // But track them so codegen can use GOT access on macOS
            // Only add to extern_symbols if not already defined (handles both cases:
            // extern int x; int x = 1;  - x is defined, not extern
            // int x = 1; extern int x;  - x is defined, not extern)
            if storage_class.contains(TypeModifiers::EXTERN) {
                // Check if this symbol is already defined in globals
                if !self.module.globals.iter().any(|g| g.name == name) {
                    self.module.extern_symbols.insert(name.clone());
                    // Track extern thread-local symbols separately for TLS access
                    if storage_class.contains(TypeModifiers::THREAD_LOCAL) {
                        self.module.extern_tls_symbols.insert(name);
                    }
                }
                continue;
            }

            let init = declarator.init.as_ref().map_or(Initializer::None, |e| {
                self.ast_init_to_ir(e, declarator.typ)
            });

            // Track file-scope static variables for inline semantic checks
            if storage_class.contains(TypeModifiers::STATIC) {
                self.file_scope_statics.insert(name.clone());
            }

            // If this symbol was previously declared extern, remove it from extern_symbols
            // (we now have the actual definition)
            self.module.extern_symbols.remove(&name);

            // Check for thread-local storage
            let is_static = storage_class.contains(TypeModifiers::STATIC);
            if storage_class.contains(TypeModifiers::THREAD_LOCAL) {
                self.module.add_global_tls_aligned(
                    &name,
                    declarator.typ,
                    init,
                    declarator.explicit_align,
                    is_static,
                );
            } else {
                self.module.add_global_aligned(
                    &name,
                    declarator.typ,
                    init,
                    declarator.explicit_align,
                    is_static,
                );
            }
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
    pub(crate) fn ast_init_to_ir(&mut self, expr: &Expr, typ: TypeId) -> Initializer {
        match &expr.kind {
            ExprKind::IntLit(v) => Initializer::Int(*v as i128),
            ExprKind::Int128Lit(v) => Initializer::Int(*v),
            ExprKind::FloatLit(v) => Initializer::Float(*v),
            ExprKind::CharLit(c) => Initializer::Int(*c as u8 as i8 as i128),

            // String literal - for arrays, store as String; for pointers, create label reference
            ExprKind::StringLit(s) => {
                let type_kind = self.types.kind(typ);
                if type_kind == TypeKind::Array {
                    // char array - embed the string directly
                    Initializer::String(s.clone())
                } else {
                    // Pointer - create a string constant and reference it
                    let label = format!(".LC{}", self.module.strings.len());
                    self.module.strings.push((label.clone(), s.clone()));
                    Initializer::SymAddr(label)
                }
            }

            // Wide string literal - for arrays, store as WideString; for pointers, create label reference
            ExprKind::WideStringLit(s) => {
                let type_kind = self.types.kind(typ);
                if type_kind == TypeKind::Array {
                    // wchar_t array - embed the wide string directly
                    Initializer::WideString(s.clone())
                } else {
                    // Pointer - create a wide string constant and reference it
                    // Use .LWC prefix to avoid collision with regular .LC string labels
                    let label = format!(".LWC{}", self.module.wide_strings.len());
                    self.module.wide_strings.push((label.clone(), s.clone()));
                    Initializer::SymAddr(label)
                }
            }

            // Negative literal (fast path for simple cases)
            ExprKind::Unary {
                op: UnaryOp::Neg,
                operand,
            } => match &operand.kind {
                ExprKind::IntLit(v) => Initializer::Int(-(*v as i128)),
                ExprKind::Int128Lit(v) => Initializer::Int(v.wrapping_neg()),
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
                // Try to compute the address as symbol + offset
                if let Some((name, offset)) = self.eval_static_address(operand) {
                    if offset == 0 {
                        Initializer::SymAddr(name)
                    } else {
                        Initializer::SymAddrOffset(name, offset)
                    }
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
                    // Type mismatch - use the compound literal's own type
                    self.ast_init_list_to_ir(elements, *cl_type)
                }
            }

            // Identifier - for constant addresses (function pointers, array decay, etc.)
            // or enum constants
            ExprKind::Ident(symbol_id) => {
                let type_kind = self.types.kind(typ);
                // For pointer types, this is likely a function address or array decay
                if type_kind == TypeKind::Pointer {
                    let name_str = self.symbol_name(*symbol_id);
                    // Check if this is a static local variable
                    // Static locals have mangled names like "func_name.var_name.N"
                    let key = format!("{}.{}", self.current_func_name, name_str);
                    if let Some(static_info) = self.static_locals.get(&key) {
                        Initializer::SymAddr(static_info.global_name.clone())
                    } else {
                        Initializer::SymAddr(name_str)
                    }
                } else {
                    // Check if it's an enum constant
                    let sym = self.symbols.get(*symbol_id);
                    if let Some(val) = sym.enum_value {
                        Initializer::Int(val as i128)
                    } else {
                        Initializer::None
                    }
                }
            }

            // Binary add/sub with pointer operand → SymAddrOffset
            ExprKind::Binary {
                op: op @ (BinaryOp::Add | BinaryOp::Sub),
                left,
                right,
            } => {
                // Try pointer/array + int or int + pointer/array → symbol address with offset
                let is_ptr_or_array =
                    |t: TypeId| matches!(self.types.kind(t), TypeKind::Pointer | TypeKind::Array);
                let (ptr_expr, int_expr, is_sub) = if left.typ.is_some_and(is_ptr_or_array) {
                    (left.as_ref(), right.as_ref(), *op == BinaryOp::Sub)
                } else if right.typ.is_some_and(is_ptr_or_array) && *op == BinaryOp::Add {
                    (right.as_ref(), left.as_ref(), false)
                } else {
                    // Neither operand is pointer — try as integer constant
                    if let Some(val) = self.eval_const_expr(expr) {
                        return Initializer::Int(val);
                    }
                    error(
                        self.current_pos.unwrap_or_default(),
                        &format!(
                            "unsupported expression in global initializer: {:?}",
                            expr.kind
                        ),
                    );
                    return Initializer::None;
                };

                // Evaluate the pointer side as a static address
                if let Some((name, base_off)) = self.eval_static_address(ptr_expr) {
                    // Evaluate the integer side as a constant
                    if let Some(int_val) = self.eval_const_expr(int_expr) {
                        // Get the pointee size for pointer arithmetic scaling
                        let pointee_size = ptr_expr
                            .typ
                            .and_then(|t| self.types.base_type(t))
                            .map(|t| self.types.size_bytes(t) as i64)
                            .unwrap_or(1);
                        let byte_offset = if is_sub {
                            base_off - int_val as i64 * pointee_size
                        } else {
                            base_off + int_val as i64 * pointee_size
                        };
                        if byte_offset == 0 {
                            Initializer::SymAddr(name)
                        } else {
                            Initializer::SymAddrOffset(name, byte_offset)
                        }
                    } else if let Some(val) = self.eval_const_expr(expr) {
                        Initializer::Int(val)
                    } else {
                        error(
                            self.current_pos.unwrap_or_default(),
                            "non-constant offset in pointer arithmetic initializer",
                        );
                        Initializer::None
                    }
                } else if let Some(val) = self.eval_const_expr(expr) {
                    Initializer::Int(val)
                } else {
                    error(
                        self.current_pos.unwrap_or_default(),
                        "non-constant pointer expression in global initializer",
                    );
                    Initializer::None
                }
            }

            // Compile-time ternary: cond ? then_expr : else_expr
            // Used in CPython's _Py_LATIN1_CHR() macro for static initializers
            ExprKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                if let Some(cond_val) = self.eval_const_expr(cond) {
                    if cond_val != 0 {
                        return self.ast_init_to_ir(then_expr, typ);
                    } else {
                        return self.ast_init_to_ir(else_expr, typ);
                    }
                }
                // If condition isn't constant, fall through to error
                error(
                    self.current_pos.unwrap_or_default(),
                    &format!(
                        "non-constant condition in global initializer ternary: {:?}",
                        cond.kind
                    ),
                );
                Initializer::None
            }

            // Other constant expressions
            // Try to evaluate as integer or float constant expression
            _ => {
                if let Some(val) = self.eval_const_expr(expr) {
                    Initializer::Int(val)
                } else if let Some(val) = self.eval_const_float_expr(expr) {
                    Initializer::Float(val)
                } else if let Some((name, offset)) = self.eval_static_address(expr) {
                    // Try as a static address (e.g., &global.field->subfield chains)
                    if offset != 0 {
                        Initializer::SymAddrOffset(name, offset)
                    } else {
                        Initializer::SymAddr(name)
                    }
                } else {
                    // Hard error for non-empty expressions we can't evaluate
                    error(
                        self.current_pos.unwrap_or_default(),
                        &format!(
                            "unsupported expression in global initializer: {:?}",
                            expr.kind
                        ),
                    );
                    Initializer::None
                }
            }
        }
    }

    /// Count the number of scalar fields needed to fill an aggregate type
    /// (for brace elision per C99 6.7.8p17-20).
    pub(crate) fn count_scalar_fields(&self, typ: TypeId) -> usize {
        match self.types.kind(typ) {
            TypeKind::Array => {
                let elem_type = self.types.base_type(typ).unwrap_or(self.types.int_id);
                let count = self.types.get(typ).array_size.unwrap_or(0);
                count * self.count_scalar_fields(elem_type)
            }
            TypeKind::Struct => {
                if let Some(composite) = self.types.get(typ).composite.as_ref() {
                    composite
                        .members
                        .iter()
                        // Skip unnamed bitfield padding
                        .filter(|m| m.name != StringId::EMPTY || m.bit_width.is_none())
                        .map(|m| self.count_scalar_fields(m.typ))
                        .sum()
                } else {
                    1
                }
            }
            TypeKind::Union => {
                // Union only initializes first named member
                if let Some(composite) = self.types.get(typ).composite.as_ref() {
                    composite
                        .members
                        .iter()
                        .find(|m| m.name != StringId::EMPTY)
                        .map(|m| self.count_scalar_fields(m.typ))
                        .unwrap_or(1)
                } else {
                    1
                }
            }
            _ => 1,
        }
    }

    /// Check if brace elision applies: the element is a positional scalar targeting
    /// an aggregate member, and is NOT a string literal initializing a char array
    /// (C99 6.7.8p14: string literals are a special case for char arrays).
    pub(crate) fn is_brace_elision_candidate(
        &self,
        element: &InitElement,
        target_type: TypeId,
    ) -> bool {
        if !element.designators.is_empty() {
            return false;
        }
        let target_is_aggregate = matches!(
            self.types.kind(target_type),
            TypeKind::Array | TypeKind::Struct | TypeKind::Union
        );
        if !target_is_aggregate {
            return false;
        }
        // String/wide string literals can directly initialize char/wchar_t arrays
        // without brace elision (C99 6.7.8p14)
        if matches!(
            element.value.kind,
            ExprKind::InitList { .. } | ExprKind::StringLit(_) | ExprKind::WideStringLit(_)
        ) {
            return false;
        }
        true
    }

    /// Consume elements from `elements[elem_idx..]` via brace elision to fill
    /// an aggregate `target_type`. Returns the collected sub-elements.
    /// Advances `elem_idx` past the consumed elements.
    pub(crate) fn consume_brace_elision(
        &self,
        elements: &[InitElement],
        elem_idx: &mut usize,
        target_type: TypeId,
    ) -> Vec<InitElement> {
        let n = self.count_scalar_fields(target_type);
        let mut sub_elements = Vec::new();
        let mut consumed = 0;
        while consumed < n && *elem_idx < elements.len() {
            let e = &elements[*elem_idx];
            // Stop at designated elements (they apply to the current aggregate level)
            if consumed > 0 && !e.designators.is_empty() {
                break;
            }
            sub_elements.push(InitElement {
                designators: vec![],
                value: e.value.clone(),
            });
            *elem_idx += 1;
            consumed += 1;
        }
        sub_elements
    }

    /// Group array init elements by index, handling designators, brace elision,
    /// and nested InitList flattening. Shared between static and runtime paths.
    pub(crate) fn group_array_init_elements(
        &self,
        elements: &[InitElement],
        elem_type: TypeId,
    ) -> ArrayInitGroups {
        let mut element_lists: HashMap<i64, Vec<InitElement>> = HashMap::new();
        let mut element_indices: Vec<i64> = Vec::new();
        let mut current_idx: i64 = 0;
        let mut elem_idx = 0;

        while elem_idx < elements.len() {
            let element = &elements[elem_idx];
            let mut index = None;
            let mut index_pos = None;
            for (pos, designator) in element.designators.iter().enumerate() {
                if let Designator::Index(idx) = designator {
                    index = Some(*idx);
                    index_pos = Some(pos);
                    break;
                }
            }

            let element_index = if let Some(idx) = index {
                current_idx = idx + 1;
                idx
            } else {
                let idx = current_idx;
                current_idx += 1;
                idx
            };

            let remaining_designators = match index_pos {
                Some(pos) => element.designators[pos + 1..].to_vec(),
                None => element.designators.clone(),
            };

            // Brace elision (C99 6.7.8p20): positional scalar for aggregate element
            if remaining_designators.is_empty()
                && self.is_brace_elision_candidate(element, elem_type)
            {
                let sub_elements = self.consume_brace_elision(elements, &mut elem_idx, elem_type);
                let entry = element_lists.entry(element_index).or_insert_with(|| {
                    element_indices.push(element_index);
                    Vec::new()
                });
                entry.extend(sub_elements);
                continue;
            }

            let entry = element_lists.entry(element_index).or_insert_with(|| {
                element_indices.push(element_index);
                Vec::new()
            });

            if remaining_designators.is_empty() {
                if let ExprKind::InitList {
                    elements: nested_elements,
                } = &element.value.kind
                {
                    entry.extend(nested_elements.clone());
                    elem_idx += 1;
                    continue;
                }
            }

            entry.push(InitElement {
                designators: remaining_designators,
                value: element.value.clone(),
            });
            elem_idx += 1;
        }

        element_indices.sort();
        ArrayInitGroups {
            element_lists,
            indices: element_indices,
        }
    }

    /// Walk struct/union init elements and produce field visits.
    /// Handles positional iteration, anonymous struct continuation, designators,
    /// and brace elision. Shared between static and runtime init paths.
    pub(crate) fn walk_struct_init_fields(
        &self,
        resolved_typ: TypeId,
        members: &[crate::types::StructMember],
        is_union: bool,
        elements: &[InitElement],
    ) -> Vec<StructFieldVisit> {
        let mut visits = Vec::new();
        let mut current_field_idx = 0;
        let mut anon_cont: Option<AnonContinuation> = None;
        let mut elem_idx = 0;

        while elem_idx < elements.len() {
            let element = &elements[elem_idx];
            if element.designators.is_empty() {
                // Positional: check anonymous struct continuation, then next member
                let mut member = None;
                if anon_cont.is_some() {
                    member = self.get_anon_continuation_member(
                        &mut anon_cont,
                        members,
                        &mut current_field_idx,
                    );
                }
                if member.is_none() {
                    member = self.next_positional_member(members, is_union, &mut current_field_idx);
                }
                let Some(member) = member else {
                    elem_idx += 1;
                    continue;
                };
                let field_size = (self.types.size_bits(member.typ) / 8) as usize;

                // Brace elision: scalar for aggregate member
                let kind = if self.is_brace_elision_candidate(element, member.typ) {
                    let sub_elements =
                        self.consume_brace_elision(elements, &mut elem_idx, member.typ);
                    StructFieldVisitKind::BraceElision(sub_elements)
                } else {
                    elem_idx += 1;
                    StructFieldVisitKind::Expr(element.value.clone())
                };
                visits.push(StructFieldVisit {
                    offset: member.offset,
                    typ: member.typ,
                    field_size,
                    kind,
                    bit_offset: member.bit_offset,
                    bit_width: member.bit_width,
                    storage_unit_size: member.storage_unit_size,
                });
                continue;
            }

            // Designated path
            let resolved = self.resolve_designator_chain(resolved_typ, 0, &element.designators);
            let Some(ResolvedDesignator {
                offset,
                typ: field_type,
                bit_offset,
                bit_width,
                storage_unit_size,
            }) = resolved
            else {
                elem_idx += 1;
                continue;
            };
            if let Some(Designator::Field(name)) = element.designators.first() {
                if let Some(result) = self.member_index_for_designator(members, *name) {
                    match result {
                        MemberDesignatorResult::Direct(next_idx) => {
                            current_field_idx = next_idx;
                            anon_cont = None;
                        }
                        MemberDesignatorResult::Anonymous { outer_idx, levels } => {
                            current_field_idx = outer_idx;
                            anon_cont = Some(AnonContinuation { outer_idx, levels });
                        }
                    }
                }
            }
            let field_size = (self.types.size_bits(field_type) / 8) as usize;
            visits.push(StructFieldVisit {
                offset,
                typ: field_type,
                field_size,
                kind: StructFieldVisitKind::Expr(element.value.clone()),
                bit_offset,
                bit_width,
                storage_unit_size,
            });
            elem_idx += 1;
        }

        visits
    }

    /// Convert an AST initializer list to an IR Initializer
    pub(crate) fn ast_init_list_to_ir(
        &mut self,
        elements: &[InitElement],
        typ: TypeId,
    ) -> Initializer {
        let type_kind = self.types.kind(typ);
        let total_size = (self.types.size_bits(typ) / 8) as usize;

        match type_kind {
            TypeKind::Array => {
                let elem_type = self.types.base_type(typ).unwrap_or(self.types.int_id);
                let elem_size = (self.types.size_bits(elem_type) / 8) as usize;
                let elem_is_aggregate = matches!(
                    self.types.kind(elem_type),
                    TypeKind::Array | TypeKind::Struct | TypeKind::Union
                );

                let groups = self.group_array_init_elements(elements, elem_type);
                let mut init_elements = Vec::new();
                for element_index in groups.indices {
                    let Some(list) = groups.element_lists.get(&element_index) else {
                        continue;
                    };
                    let offset = (element_index as usize) * elem_size;
                    // When a string literal initializes a char/wchar_t array element
                    // (e.g., char names[3][4] = {"Sun", "Mon", "Tue"}), handle it
                    // directly with the ARRAY type. Otherwise ast_init_list_to_ir
                    // recurses and passes elem_type=char, causing the string to be
                    // stored as a pointer instead of inline char data.
                    let is_string_for_char_array = elem_is_aggregate
                        && list.len() == 1
                        && matches!(
                            list[0].value.kind,
                            ExprKind::StringLit(_) | ExprKind::WideStringLit(_)
                        )
                        && self.types.kind(elem_type) == TypeKind::Array;
                    let elem_init = if is_string_for_char_array {
                        self.ast_init_to_ir(&list[0].value, elem_type)
                    } else if elem_is_aggregate {
                        self.ast_init_list_to_ir(list, elem_type)
                    } else if let Some(last) = list.last() {
                        self.ast_init_to_ir(&last.value, elem_type)
                    } else {
                        Initializer::None
                    };
                    init_elements.push((offset, elem_init));
                }

                init_elements.sort_by_key(|(offset, _)| *offset);

                Initializer::Array {
                    elem_size,
                    total_size,
                    elements: init_elements,
                }
            }

            TypeKind::Struct | TypeKind::Union => {
                let resolved_typ = self.resolve_struct_type(typ);
                let resolved_size = (self.types.size_bits(resolved_typ) / 8) as usize;
                if let Some(composite) = self.types.get(resolved_typ).composite.as_ref() {
                    let members: Vec<_> = composite.members.clone();
                    let is_union = self.types.kind(resolved_typ) == TypeKind::Union;

                    let visits =
                        self.walk_struct_init_fields(resolved_typ, &members, is_union, elements);

                    // Convert field visits to RawFieldInit by evaluating expressions
                    let mut raw_fields: Vec<RawFieldInit> = Vec::new();
                    for visit in visits {
                        let field_init = match visit.kind {
                            StructFieldVisitKind::BraceElision(sub_elements) => {
                                self.ast_init_list_to_ir(&sub_elements, visit.typ)
                            }
                            StructFieldVisitKind::Expr(expr) => {
                                self.ast_init_to_ir(&expr, visit.typ)
                            }
                        };
                        raw_fields.push(RawFieldInit {
                            offset: visit.offset,
                            field_size: visit.field_size,
                            init: field_init,
                            bit_offset: visit.bit_offset,
                            bit_width: visit.bit_width,
                            storage_unit_size: visit.storage_unit_size,
                        });
                    }

                    // Sort fields by offset to ensure proper emission order
                    // (designated initializers can be in any order)
                    // For bitfields, also sort by bit_offset to keep them together
                    raw_fields.sort_by(|a, b| {
                        a.offset
                            .cmp(&b.offset)
                            .then_with(|| a.bit_offset.unwrap_or(0).cmp(&b.bit_offset.unwrap_or(0)))
                    });

                    // Remove duplicate initializations (later one wins, per C semantics)
                    let mut idx = 0;
                    while idx + 1 < raw_fields.len() {
                        let same_offset = raw_fields[idx].offset == raw_fields[idx + 1].offset;
                        let both_bitfields = raw_fields[idx].bit_offset.is_some()
                            && raw_fields[idx + 1].bit_offset.is_some();
                        let same_bitfield = both_bitfields
                            && raw_fields[idx].bit_offset == raw_fields[idx + 1].bit_offset;

                        if same_offset && (!both_bitfields || same_bitfield) {
                            raw_fields.remove(idx);
                        } else {
                            idx += 1;
                        }
                    }

                    // Pack bitfields that share the same storage unit
                    let mut init_fields: Vec<(usize, usize, Initializer)> = Vec::new();
                    let mut i = 0;
                    while i < raw_fields.len() {
                        let RawFieldInit {
                            offset,
                            field_size,
                            init,
                            bit_offset,
                            bit_width,
                            storage_unit_size,
                        } = &raw_fields[i];

                        if let (Some(bit_off), Some(bit_w), Some(storage_size)) =
                            (bit_offset, bit_width, storage_unit_size)
                        {
                            let mut packed_value: u64 = 0;
                            if let Initializer::Int(v) = init {
                                let mask = (1u64 << bit_w) - 1;
                                packed_value |= ((*v as u64) & mask) << bit_off;
                            }

                            let mut j = i + 1;
                            while j < raw_fields.len() {
                                let RawFieldInit {
                                    offset: next_off,
                                    init: next_init,
                                    bit_offset: next_bit_off,
                                    bit_width: next_bit_w,
                                    ..
                                } = &raw_fields[j];
                                if *next_off != *offset {
                                    break;
                                }
                                if let (Some(nb_off), Some(nb_w)) = (next_bit_off, next_bit_w) {
                                    if let Initializer::Int(v) = next_init {
                                        let mask = (1u64 << nb_w) - 1;
                                        packed_value |= ((*v as u64) & mask) << nb_off;
                                    }
                                }
                                j += 1;
                            }

                            init_fields.push((
                                *offset,
                                *storage_size as usize,
                                Initializer::Int(packed_value as i128),
                            ));
                            i = j;
                        } else {
                            init_fields.push((*offset, *field_size, init.clone()));
                            i += 1;
                        }
                    }

                    Initializer::Struct {
                        total_size: resolved_size,
                        fields: init_fields,
                    }
                } else {
                    Initializer::None
                }
            }

            _ => {
                if let Some(element) = elements.first() {
                    self.ast_init_to_ir(&element.value, typ)
                } else {
                    Initializer::None
                }
            }
        }
    }

    pub(crate) fn resolve_designator_chain(
        &self,
        base_type: TypeId,
        base_offset: usize,
        designators: &[Designator],
    ) -> Option<ResolvedDesignator> {
        let mut offset = base_offset;
        let mut typ = base_type;
        let mut bit_offset = None;
        let mut bit_width = None;
        let mut storage_unit_size = None;

        for (idx, designator) in designators.iter().enumerate() {
            match designator {
                Designator::Field(name) => {
                    let mut resolved = typ;
                    if self.types.kind(resolved) == TypeKind::Array {
                        resolved = self.types.base_type(resolved)?;
                    }
                    resolved = self.resolve_struct_type(resolved);
                    let member = self.types.find_member(resolved, *name)?;
                    offset += member.offset;
                    typ = member.typ;
                    if idx + 1 == designators.len() {
                        bit_offset = member.bit_offset;
                        bit_width = member.bit_width;
                        storage_unit_size = member.storage_unit_size;
                    } else {
                        bit_offset = None;
                        bit_width = None;
                        storage_unit_size = None;
                    }
                }
                Designator::Index(index) => {
                    if self.types.kind(typ) != TypeKind::Array {
                        return None;
                    }
                    let elem_type = self.types.base_type(typ)?;
                    let elem_size = self.types.size_bits(elem_type) / 8;
                    offset += (*index as usize) * (elem_size as usize);
                    typ = elem_type;
                    bit_offset = None;
                    bit_width = None;
                    storage_unit_size = None;
                }
            }
        }

        Some(ResolvedDesignator {
            offset,
            typ,
            bit_offset,
            bit_width,
            storage_unit_size,
        })
    }

    pub(crate) fn next_positional_member(
        &self,
        members: &[crate::types::StructMember],
        is_union: bool,
        current_field_idx: &mut usize,
    ) -> Option<MemberInfo> {
        if is_union {
            if *current_field_idx > 0 {
                return None;
            }
            let member = members.iter().find(|m| m.name != StringId::EMPTY)?;
            *current_field_idx = members.len();
            return Some(MemberInfo {
                offset: member.offset,
                typ: member.typ,
                bit_offset: member.bit_offset,
                bit_width: member.bit_width,
                storage_unit_size: member.storage_unit_size,
            });
        }

        while *current_field_idx < members.len() {
            let member = &members[*current_field_idx];
            if member.name == StringId::EMPTY && member.bit_width.is_some() {
                *current_field_idx += 1;
                continue;
            }
            if member.name != StringId::EMPTY || member.bit_width.is_none() {
                *current_field_idx += 1;
                return Some(MemberInfo {
                    offset: member.offset,
                    typ: member.typ,
                    bit_offset: member.bit_offset,
                    bit_width: member.bit_width,
                    storage_unit_size: member.storage_unit_size,
                });
            }
            *current_field_idx += 1;
        }

        None
    }

    /// Get the next positional member from an anonymous struct continuation.
    /// Walks the stack of anonymous struct levels from innermost to outermost.
    /// If the innermost level is exhausted, pops it and tries the next outer level.
    /// When all levels are exhausted, clears the continuation and returns None.
    pub(crate) fn get_anon_continuation_member(
        &self,
        cont: &mut Option<AnonContinuation>,
        _outer_members: &[crate::types::StructMember],
        current_field_idx: &mut usize,
    ) -> Option<MemberInfo> {
        let c = cont.as_mut()?;

        loop {
            let Some(level) = c.levels.last() else {
                // All levels exhausted
                *current_field_idx = c.outer_idx + 1;
                *cont = None;
                return None;
            };
            let anon_type_id = level.anon_type;
            let base_offset = level.base_offset;
            let mut idx = level.inner_next_idx;

            let anon_type = self.types.get(anon_type_id);
            let Some(composite) = anon_type.composite.as_ref() else {
                c.levels.pop();
                continue;
            };
            let members = composite.members.clone();

            // Scan members at this level
            let mut found_member = None;
            let mut descend_into = None;

            while idx < members.len() {
                let inner = &members[idx];
                // Skip unnamed bitfield padding
                if inner.name == StringId::EMPTY && inner.bit_width.is_some() {
                    idx += 1;
                    continue;
                }
                // Nested anonymous aggregate — descend into it
                if inner.name == StringId::EMPTY && inner.bit_width.is_none() {
                    let inner_type = self.types.get(inner.typ);
                    let is_nested_anon =
                        matches!(inner_type.kind, TypeKind::Struct | TypeKind::Union)
                            && inner_type
                                .composite
                                .as_ref()
                                .is_some_and(|comp| comp.tag.is_none());
                    if is_nested_anon {
                        descend_into = Some((inner.typ, base_offset + inner.offset, idx + 1));
                        break;
                    }
                }
                // Found a valid named member
                found_member = Some((idx + 1, inner.clone()));
                break;
            }

            if let Some((next_idx, inner)) = found_member {
                // Update the current level's index
                c.levels.last_mut().unwrap().inner_next_idx = next_idx;
                return Some(MemberInfo {
                    offset: base_offset + inner.offset,
                    typ: inner.typ,
                    bit_offset: inner.bit_offset,
                    bit_width: inner.bit_width,
                    storage_unit_size: inner.storage_unit_size,
                });
            }

            if let Some((nested_type, nested_offset, next_idx)) = descend_into {
                // Advance past the anon struct at this level, then descend
                c.levels.last_mut().unwrap().inner_next_idx = next_idx;
                c.levels.push(AnonLevel {
                    anon_type: nested_type,
                    base_offset: nested_offset,
                    inner_next_idx: 0,
                });
                continue;
            }

            // This level is exhausted — pop it
            c.levels.pop();
        }
    }

    pub(crate) fn member_index_for_designator(
        &self,
        members: &[crate::types::StructMember],
        name: StringId,
    ) -> Option<MemberDesignatorResult> {
        for (idx, member) in members.iter().enumerate() {
            if member.name == name {
                return Some(MemberDesignatorResult::Direct(idx + 1));
            }
            if member.name == StringId::EMPTY {
                let member_type = self.types.get(member.typ);
                let is_anon_aggregate =
                    matches!(member_type.kind, TypeKind::Struct | TypeKind::Union)
                        && member_type
                            .composite
                            .as_ref()
                            .is_some_and(|composite| composite.tag.is_none());
                if is_anon_aggregate {
                    // Recursively search for the field, building the nesting path
                    let mut path = Vec::new();
                    if self.find_anon_field_path(member.typ, member.offset, name, &mut path) {
                        return Some(MemberDesignatorResult::Anonymous {
                            outer_idx: idx,
                            levels: path,
                        });
                    }
                }
            }
        }

        None
    }

    /// Recursively search for `name` inside an anonymous aggregate, building
    /// the path of `AnonLevel`s needed for positional continuation.
    /// Returns true if the field was found.
    pub(crate) fn find_anon_field_path(
        &self,
        anon_type: TypeId,
        base_offset: usize,
        name: StringId,
        path: &mut Vec<AnonLevel>,
    ) -> bool {
        let typ = self.types.get(anon_type);
        let Some(composite) = typ.composite.as_ref() else {
            return false;
        };
        for (inner_idx, inner_member) in composite.members.iter().enumerate() {
            if inner_member.name == name {
                // Found it directly at this level
                path.push(AnonLevel {
                    anon_type,
                    base_offset,
                    inner_next_idx: inner_idx + 1,
                });
                return true;
            }
            // Check if this is a nested anonymous aggregate
            if inner_member.name == StringId::EMPTY {
                let inner_type = self.types.get(inner_member.typ);
                let is_nested_anon = matches!(inner_type.kind, TypeKind::Struct | TypeKind::Union)
                    && inner_type
                        .composite
                        .as_ref()
                        .is_some_and(|c| c.tag.is_none());
                if is_nested_anon {
                    // Push this level pointing PAST the nested anon struct.
                    // The inner level handles continuation within the nested anon;
                    // when it's exhausted, this level continues from the next member.
                    path.push(AnonLevel {
                        anon_type,
                        base_offset,
                        inner_next_idx: inner_idx + 1,
                    });
                    if self.find_anon_field_path(
                        inner_member.typ,
                        base_offset + inner_member.offset,
                        name,
                        path,
                    ) {
                        return true;
                    }
                    path.pop(); // not found in this branch
                }
            }
        }
        false
    }
}
