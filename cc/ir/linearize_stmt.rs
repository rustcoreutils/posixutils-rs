//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT

//! Statement linearization

use super::asm_operand::AddrWalk;
use super::linearize::*;
use super::linearize_cleanup::ScopeExit;
use super::linearize_emit::Controlling;
use super::{
    AsmConstraint, AsmData, BasicBlockId, GlobalStorage, Initializer, Instruction, Opcode, PseudoId,
};
use crate::arch::asm_constraints::{AsmAccess, AsmOperandClass};
use crate::constexpr::ConstScope;
use crate::diag::{error, Position};
use crate::float::FloatVal;
use crate::parse::ast::{
    AsmOperand, BinaryOp, BlockItem, Declaration, Expr, ExprKind, ForInit, InitElement, Label,
    LabelId, Stmt, UnaryOp,
};
use crate::symbol::SymbolId;
use crate::token::lexer::payload_bytes;
use crate::types::TypeTable;

use crate::types::{TypeId, TypeKind, TypeModifiers};

/// The `-Wno-<name>` group for a case label the switch's promoted controlling
/// type cannot hold, spelled as gcc spells the same diagnostic.
const CASE_RANGE_WARNING: &str = "switch-outside-range";

/// Whether [`Linearizer::store_string_units`] owes the destination's tail a
/// zero fill.
///
/// C17 6.7.9p21 zeroes every element a string initializer does not reach, and
/// exactly one of the two spellings already has that covered: the braced form
/// reaches the array through an initializer list, and each list is preceded by
/// a whole-object [`Linearizer::emit_aggregate_zero`]. Zeroing again there
/// would double the stores at `-O0`, where no `dse` runs to remove them.
#[derive(Clone, Copy)]
pub(crate) enum StringTail {
    /// The destination is already zero: the caller zeroed the whole aggregate
    /// before walking the initializer list.
    AlreadyZero,
    /// Nothing has written the destination yet, so the tail is this call's to
    /// fill.
    Zero,
}

/// Which construct a jump leaves, for `unwind_vla_marks`.
///
/// `break` leaves the innermost loop *or* switch; `continue` leaves the
/// innermost loop, which may be several switches out. The two ask different
/// nesting counters, and conflating them left a VLA unreclaimed on
/// `continue` from inside a `switch`.
#[derive(Clone, Copy)]
enum JumpKind {
    Break,
    Continue,
}

/// A `for` loop whose body is being lowered: what
/// [`Linearizer::open_for`] set up and [`Linearizer::close_for`] finishes.
#[must_use = "an opened `for` loop must be finished with close_for"]
struct OpenFor {
    /// The scope the init clause declares into, ended after `exit_bb`.
    scope: Scope,
    /// The block the back edge goes to.
    cond_bb: BasicBlockId,
    /// Where the body falls out to, and the `continue` target.
    post_bb: BasicBlockId,
    /// Where the loop ends, and the `break` target.
    exit_bb: BasicBlockId,
}

/// One initializer from a struct or union initializer list that has already
/// been stored into the object being initialized.
///
/// The automatic path emits a store per list entry and lets a later store land
/// on an earlier one, which is all C17 6.7.9p19 needs *while* the later store
/// covers every byte it supersedes. It does not when the later initializer
/// fills only part of the subobject it names, and it does not when the two
/// entries name different members of a union -- a union holds one member at a
/// time, so the member left behind does not show through the new one. Both
/// need the superseded bytes cleared, and deciding which bytes those are is
/// what this records.
struct WrittenInit {
    /// The bytes it wrote that no later entry has since cleared.
    live: std::ops::Range<usize>,
    /// The first byte of the subobject it initialized, and that subobject's
    /// type. Together they say whether a later entry names a member of this
    /// one -- in which case the rest of this one survives -- or reaches its
    /// bytes only by passing through a union.
    origin: usize,
    typ: TypeId,
    /// The member each union inside that subobject came to hold, which
    /// decides whether a later entry naming something inside one of them
    /// names the *same* member -- and so leaves the rest of it standing.
    held: UnionMembers,
    /// Set when the entry is a bit-field, to its bit offset. Two bit-fields
    /// sharing a carrier byte are different objects and neither supersedes
    /// the other, however their bytes overlap.
    bits: Option<u32>,
}

/// Grow `reset` to also cover `range`.
///
/// Every range joined here overlaps the range of the entry being stored, so
/// the union of them all is contiguous and one fill covers it.
fn widen_reset(reset: &mut Option<std::ops::Range<usize>>, range: std::ops::Range<usize>) {
    *reset = Some(match reset.take() {
        Some(cur) => cur.start.min(range.start)..cur.end.max(range.end),
        None => range,
    });
}

impl<'a> super::linearize::Linearizer<'a> {
    pub(crate) fn linearize_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Empty => {}

            Stmt::Expr(expr) => {
                self.linearize_expr(expr);
            }

            Stmt::Block(items) => {
                // Entering the scope also captures the stack pointer, and
                // leaving it puts the pointer back -- a VLA's storage lives
                // until control leaves the scope of its declaration (C17
                // 6.2.4p7). See `Scope`.
                let scope = self.push_scope();
                for item in items {
                    match item {
                        BlockItem::Declaration(decl) => self.linearize_local_decl(decl),
                        BlockItem::Statement(s) => self.linearize_stmt(s),
                    }
                }
                self.pop_scope(scope);
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
                // The parser has checked the value against the declared
                // return type (C17 6.8.6.4).
                if let Some(e) = expr {
                    let expr_typ = self.expr_type(e);
                    // Get the function's actual return type for proper conversion
                    let func_ret_type = self
                        .current_func
                        .as_ref()
                        .map(|f| f.return_type)
                        .unwrap_or(expr_typ);

                    if let (Some(vec), None) = (self.vector_return, self.struct_return_ptr) {
                        // A vector returned in a register, as its carrier's
                        // bits; one returned in memory takes the sret path.
                        let addr = self.vector_addr(e);
                        let conv = self.current_calling_conv;
                        let val = self.vector_return_value(addr, vec, func_ret_type, conv);
                        let size = self.types.size_bits(func_ret_type);
                        self.emit_return(Instruction::ret_typed(Some(val), func_ret_type, size));
                    } else if let Some(sret_ptr) = self.struct_return_ptr {
                        self.emit_sret_return(e, sret_ptr, func_ret_type);
                    } else if let Some(ret_type) = self.reg_aggregate_return_type {
                        self.emit_reg_aggregate_return(e, ret_type);
                    } else {
                        // Converted as if by assignment (C17 6.8.6.4p3).
                        let converted_val = if self.types.kind(func_ret_type) == TypeKind::Void {
                            self.linearize_expr(e)
                        } else {
                            self.linearize_converted(e, func_ret_type)
                        };
                        let converted_val = self.outlive_cleanups(converted_val, func_ret_type, 0);
                        // Function types decay to pointers when returned
                        let typ_size = if self.types.kind(func_ret_type) == TypeKind::Function {
                            self.target.pointer_width
                        } else {
                            self.types.size_bits(func_ret_type)
                        };
                        self.emit_return(Instruction::ret_typed(
                            Some(converted_val),
                            func_ret_type,
                            typ_size,
                        ));
                    }
                } else {
                    self.emit_return(Instruction::ret(None));
                }
                self.start_unreachable_block();
            }

            Stmt::Break(_) => {
                if let Some(&target) = self.break_targets.last() {
                    if self.current_bb.is_some() {
                        self.leave_scopes(ScopeExit::Break);
                        self.unwind_vla_marks(JumpKind::Break);
                        self.link_to_merge_if_needed(target);
                        self.start_unreachable_block();
                    }
                }
            }

            Stmt::Continue(_) => {
                if let Some(&target) = self.continue_targets.last() {
                    if self.current_bb.is_some() {
                        self.leave_scopes(ScopeExit::Continue);
                        self.unwind_vla_marks(JumpKind::Continue);
                        self.link_to_merge_if_needed(target);
                        self.start_unreachable_block();
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
                // A computed `goto` leaves scopes exactly as a named one
                // does, and left out here it left a loop body's VLA behind
                // every time round. Which scopes it leaves depends on which
                // label it reaches, which is not known until every label is
                // placed -- and then only as a set. See
                // `GotoTarget::AnyAddressTaken`.
                self.defer_vla_restore(GotoTarget::AnyAddressTaken);
                let (dispatch_bb, slot) = self.indirect_dispatch_block();
                // Hand the address over in the hidden local and branch to the
                // one dispatch block, which is the only place that fans out to
                // the labels. See `Linearizer::indirect_dispatch`.
                let void_ptr = self.types.void_ptr_id;
                self.emit(Instruction::store(addr, slot, 0, void_ptr, self.ptr_bits()));
                self.emit(Instruction::br(dispatch_bb));
                if let Some(current) = self.current_bb {
                    self.link_bb(current, dispatch_bb);
                }
                self.current_bb = None;
            }

            Stmt::Goto { label, pos } => {
                let target = self.refer_to_label(*label, *pos);
                if self.current_bb.is_some() {
                    self.leave_scopes(ScopeExit::Goto(*label));
                    self.release_vla_scopes_for_goto(target);
                    self.link_to_merge_if_needed(target);
                }

                // Set current_bb to None - any subsequent code until a label is dead
                // emit() will safely skip when current_bb is None
                self.current_bb = None;
            }

            Stmt::Labeled { labels, stmt } => {
                for label in labels {
                    match label {
                        Label::Case(expr, high) => self.enter_case_label(expr, high.as_ref()),
                        Label::Default(_) => self.enter_default_label(),
                        Label::Named { label, .. } => self.place_label(*label),
                    }
                }
                // Then the statement the labels prefix, which is where their
                // code actually is.
                self.linearize_stmt(stmt);
            }

            Stmt::Switch { expr, body } => {
                self.linearize_switch(expr, body);
            }

            Stmt::Asm {
                pos,
                template,
                outputs,
                inputs,
                clobbers,
                goto_labels,
            } => {
                // The statement's own position until an operand gives one.
                self.current_pos = Some(*pos);
                self.linearize_asm(template, outputs, inputs, clobbers, goto_labels);
            }
        }
    }

    pub(crate) fn linearize_local_decl(&mut self, decl: &Declaration) {
        for declarator in &decl.declarators {
            let typ = declarator.typ;

            // A typedef declares a name, not an object, so nothing is
            // allocated for it. Falling through would matter most for a
            // variably modified typedef: the VLA arm below tests only
            // `vla_sizes`, so it would allocate the array itself.
            if declarator.storage_class.contains(TypeModifiers::TYPEDEF) {
                // C17 6.7.7p3: a variably modified typedef's size expressions
                // are evaluated "each time the declaration of the typedef name
                // is reached in the order of execution" -- here, once, however
                // many objects the name goes on to declare. Each is spilled to
                // a hidden local that `ExprKind::VmTypedefExtent` reads back.
                //
                // A pointer to a VLA is sized by its pointee's extents, as an
                // object declared `int (*p)[n]` is below; asking only an
                // array typedef left `typedef int (*P)[n]; P p;` with no
                // extents, and `p + 1` a step of 0.
                let vm_type = match self.types.kind(typ) {
                    TypeKind::Array => Some(typ),
                    TypeKind::Pointer => self.types.base_type(typ),
                    _ => None,
                };
                if let Some(vm_type) = vm_type.filter(|_| !declarator.vla_sizes.is_empty()) {
                    let name = self.symbol_name(declarator.symbol);
                    let (dims, _elem) =
                        self.record_vm_extents(vm_type, &declarator.vla_sizes, &name);
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

            // C17 6.2.2p4: an identifier declared `extern` inside a function
            // refers to an object with external linkage; it declares no
            // storage of its own. A function declared in a block has external
            // linkage too (6.2.2p5), spelled `extern` or not.
            //
            // Falling through to the automatic-storage path below gave both a
            // stack slot and, worse, an entry in `self.locals`, which is what
            // every later reference consults -- so `extern int g;` read the
            // frame instead of `g`, and writes through it never reached the
            // object. Leaving the name out of `self.locals` is the whole fix:
            // `linearize_ident` and the store paths already treat an unknown
            // name as a global.
            //
            // The file-scope twin in `linearize_init.rs` has always done this.
            let is_block_scope_function = self.types.kind(typ) == TypeKind::Function;
            if declarator.storage_class.contains(TypeModifiers::EXTERN) || is_block_scope_function {
                // Mirror the file-scope path: record the reference so codegen
                // reaches the object the way an undefined symbol must be
                // reached -- through the GOT on macOS, and through the TLS
                // sequence for `extern _Thread_local`. A name this unit also
                // defines is not external, so it is left alone.
                let name = self.symbol_name(declarator.symbol).to_string();
                if !is_block_scope_function && !self.module.globals.iter().any(|g| g.name == name) {
                    self.module.extern_symbols.insert(name.clone());
                    self.module.extern_object_align.insert(
                        name.clone(),
                        declarator
                            .explicit_align
                            .unwrap_or(self.types.alignment(typ) as u32),
                    );
                    if declarator
                        .storage_class
                        .contains(TypeModifiers::THREAD_LOCAL)
                    {
                        self.module.extern_tls_symbols.insert(name);
                    }
                }
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
                // Report against the declarator's own position, which always
                // exists -- `current_pos` is an Option that can be None here.
                if declarator.init.is_some() {
                    error(
                        declarator.pos,
                        "variable length arrays cannot have initializers",
                    );
                }
                self.linearize_vla_decl(declarator);
                self.register_cleanup(declarator);
                continue;
            }

            // Create a symbol pseudo for this local variable (its address)
            // Use unique name (name.id) to distinguish shadowed variables
            let sym_id = self.alloc_pseudo();
            let name_str = self.symbol_name(declarator.symbol);
            let unique_name = format!("{}.{}", name_str, sym_id.0);
            // The current block is the declaration block, for scope-aware phi
            // placement.
            self.named_local(
                sym_id,
                unique_name,
                typ,
                self.current_bb,
                declarator.explicit_align,
            );

            // Track in linearizer's locals map using SymbolId as key
            self.insert_local(declarator.symbol, LocalVarInfo::frame(sym_id, typ));

            // A pointer to a variably-modified array: the extents are the
            // pointee's, and they are what one index step off the pointer
            // has to advance by. C99 6.7.5.2p5 evaluates them where the
            // declaration appears, which is here -- before any initializer.
            //
            // This is the same shape as a variably-modified *parameter*,
            // which is adjusted to exactly this pointer type; see
            // `linearize_function`.
            self.record_pointee_extents(declarator.symbol, declarator.typ, &declarator.vla_sizes);

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
                        // Nothing has zeroed this local -- the `InitList` arm
                        // above calls `emit_aggregate_zero` and this one never
                        // did -- so the elements past the literal are
                        // `store_string_units`' to fill.
                        self.store_string_units(
                            sym_id,
                            0,
                            typ,
                            &init.kind,
                            &units,
                            StringTail::Zero,
                        );
                    } else {
                        // Pointer initialized with a string literal — store the address
                        let val = self.linearize_expr(init);
                        let init_type = self.expr_type(init);
                        let converted = self.emit_convert(val, init_type, typ);
                        let size = self.types.size_bits(typ);
                        self.emit(Instruction::store(converted, sym_id, 0, typ, size));
                    }
                } else if self.types.is_complex(typ) || self.types.is_vector(typ) {
                    self.store_addressed_value_at(sym_id, 0, typ, init);
                } else {
                    // An aggregate that does not travel by value: the
                    // initializer yields its address, and it is block-copied
                    // -- by nothing at all when it is zero-sized.
                    let type_kind = self.types.kind(typ);
                    if matches!(type_kind, TypeKind::Struct | TypeKind::Union)
                        && !self.aggregate_travels_by_value(typ)
                    {
                        let value_addr = self.linearize_expr(init);
                        let type_size_bytes = self.types.size_bytes(typ);

                        let vol = self.block_volatility(typ, self.expr_type(init));
                        self.emit_block_copy(sym_id, value_addr, type_size_bytes as i64, vol);
                    } else {
                        // Simple scalar initializer, converted as if by
                        // assignment (C17 6.7.9p11).
                        let converted = self.linearize_converted(init, typ);
                        let size = self.types.size_bits(typ);
                        self.emit(Instruction::store(converted, sym_id, 0, typ, size));
                    }
                }
            }
            self.register_cleanup(declarator);
        }
    }

    /// Record the extents of the pointee of `symbol`, a pointer to a VLA of
    /// type `typ` sized by `vla_sizes`, on its local: what one index step off
    /// the pointer has to advance by. Nothing when there are no size
    /// expressions.
    pub(crate) fn record_pointee_extents(
        &mut self,
        symbol: SymbolId,
        typ: TypeId,
        vla_sizes: &[Expr],
    ) {
        if vla_sizes.is_empty() {
            return;
        }
        let Some(pointee) = self.types.base_type(typ) else {
            return;
        };
        let name = self.symbol_name(symbol);
        let (dims, elem_type) = self.record_vm_extents(pointee, vla_sizes, &name);
        if let Some(info) = self.locals.get_mut(&symbol) {
            info.vm_row_dims = dims;
            info.vla_elem_type = Some(elem_type);
        }
    }

    /// Evaluate the size expressions of a type-name's value
    /// ([`ExprKind::VmTypeName`]) and record its extents under `symbol`, as
    /// a declared pointer's are recorded under its name.
    ///
    /// The entry holds extents and nothing else -- no identifier names
    /// `symbol`, so no slot is ever read through it.
    pub(crate) fn record_type_name_extents(
        &mut self,
        symbol: SymbolId,
        typ: TypeId,
        dims: &[Expr],
    ) {
        self.insert_local(symbol, LocalVarInfo::new(LocalBinding::ExtentsOnly, typ));
        self.record_pointee_extents(symbol, typ, dims);
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
        self.named_local(
            size_sym_id,
            size_var_name,
            ulong_type,
            self.current_bb,
            None,
        );

        // Store num_elements into the hidden size variable
        let ulong_bits = self.types.size_bits(ulong_type);
        let store_size_insn =
            Instruction::store(num_elements, size_sym_id, 0, ulong_type, ulong_bits);
        self.emit(store_size_insn);

        // Compute total size in bytes: num_elements * sizeof(element)
        let elem_size_const = self.emit_const(elem_size as i128, self.types.long_id);
        let total_size = self.alloc_pseudo();
        let mul_insn = Instruction::new(Opcode::Mul)
            .with_target(total_size)
            .with_src(num_elements)
            .with_src(elem_size_const)
            .with_size(ulong_bits)
            .with_type(ulong_type);
        self.emit(mul_insn);

        // Capture the stack pointer before this array is allocated, so
        // whatever later leaves the array's scope can put it back. One mark
        // per declaration rather than per block: a label between two VLAs
        // must release only the one that follows it.
        self.push_vla_mark();

        // Emit Alloca instruction to allocate stack space
        let alloca_result = self.alloc_pseudo();
        let alloca_insn = Instruction::new(Opcode::Alloca)
            .with_target(alloca_result)
            .with_src(total_size)
            .with_type_and_size(self.types.void_ptr_id, self.ptr_bits());
        self.emit(alloca_insn);

        // Create a symbol pseudo for the VLA pointer variable
        // This symbol stores the pointer to the VLA memory (like a pointer variable)
        let sym_id = self.alloc_pseudo();
        let sym_name = self.symbol_name(declarator.symbol);
        let unique_name = format!("{}.{}", sym_name, sym_id.0);

        // Create a pointer type for the VLA (pointer to element type)
        let ptr_type = self.types.pointer_to(elem_type);

        // Register as a pointer variable, not as the array type
        self.named_local(
            sym_id,
            unique_name,
            ptr_type,
            self.current_bb,
            declarator.explicit_align,
        );

        // Store the Alloca result (pointer) into the VLA symbol
        let store_insn = Instruction::store(alloca_result, sym_id, 0, ptr_type, self.ptr_bits());
        self.emit(store_insn);

        // Track in linearizer's locals map with pointer type and VLA size info
        // This makes arr[i] behave like ptr[i] - load ptr, then offset
        self.insert_local(
            declarator.symbol,
            LocalVarInfo {
                vla_size_sym: Some(size_sym_id),
                vla_outer_extent: dims.first().copied(),
                vla_elem_type: Some(elem_type),
                // One index step consumes the outermost extent, so what a row
                // still spans is everything after it.
                vm_row_dims: dims[1..].to_vec(),
                ..LocalVarInfo::new(
                    LocalBinding::Frame {
                        sym: sym_id,
                        // The slot holds the `alloca` result, not the elements.
                        storage: Storage::Indirect(ptr_type),
                    },
                    ptr_type,
                )
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
        // A later `target_clones` version shares the object the first one
        // defined: one static, whichever version runs.
        let shared = self
            .clone_statics
            .as_ref()
            .and_then(|statics| statics.get(&declarator.symbol))
            .cloned();
        if let Some(global) = shared {
            self.insert_local(
                declarator.symbol,
                LocalVarInfo::new(LocalBinding::Static { global }, declarator.typ),
            );
            self.record_pointee_extents(declarator.symbol, declarator.typ, &declarator.vla_sizes);
            return;
        }
        let name_str = self.symbol_name(declarator.symbol);

        // C99 6.7.4p3: an inline definition shall not define a modifiable
        // object with static storage duration. gcc only warns, in its own
        // words -- an error under `-pedantic-errors`.
        if self.current_func_is_inline_definition {
            let is_const = self
                .types
                .modifiers(declarator.typ)
                .contains(TypeModifiers::CONST);
            if !is_const {
                if let Some(pos) = self.current_pos {
                    crate::diag::pedwarn_default_args(
                        pos,
                        "'{0}' is static but declared in inline function '{1}' which is not static",
                        &[name_str.as_str(), self.current_func_name.as_str()],
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
        if let Some(statics) = &mut self.clone_statics {
            statics.insert(declarator.symbol, global_name.clone());
        }

        // The name is bound in this scope like any local, so an inner
        // declaration shadows it and leaving the scope ends it.
        self.insert_local(
            declarator.symbol,
            LocalVarInfo::new(
                LocalBinding::Static {
                    global: global_name.clone(),
                },
                declarator.typ,
            ),
        );

        // A static pointer to a VLA -- the one variably modified type static
        // storage allows (C17 6.7.6.2p2) -- still steps by its pointee's
        // extents, evaluated each time the declaration is reached.
        self.record_pointee_extents(declarator.symbol, declarator.typ, &declarator.vla_sizes);

        // Determine initializer (static locals are initialized at compile time)
        let init = declarator.init.as_ref().map_or(Initializer::None, |e| {
            self.ast_init_to_ir(e, declarator.typ)
        });

        // Add as a global - static locals always have internal linkage
        // Const at the object level for section selection (see linearize_init.rs)
        let is_const = super::linearize_init::is_const_object_type(self.types, declarator.typ);
        // Thread-local storage is the declaration's storage class, not
        // anything about the type: a structure's type is its tag's and never
        // carries one, so asking the type made `static _Thread_local struct S
        // s;` one object shared by every thread.
        let storage = GlobalStorage {
            // Static locals always have internal linkage.
            is_static: true,
            is_const,
            is_thread_local: declarator
                .storage_class
                .contains(TypeModifiers::THREAD_LOCAL),
        };
        self.module.define_global(
            self.types,
            &global_name,
            declarator.typ,
            init,
            declarator.explicit_align,
            storage,
        );
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
        // A vector initialized by one vector value -- `v4si arr[2] = {a, b}`
        // reaches here once per element -- takes it whole, as a struct does
        // a struct value; read as a list, it was the first lane.
        if let [only] = elements {
            let whole = only.designators.is_empty()
                && self.types.is_vector(typ)
                && only.value.typ.is_some_and(|t| self.types.is_vector(t));
            if whole {
                self.store_addressed_value_at(base_sym, base_offset, typ, &only.value);
                return;
            }
        }
        match self.types.kind(typ) {
            TypeKind::Array => {
                // `qualified_with` already puts an array's qualifiers on its
                // element type (C17 6.7.3p10), so the element is the subobject.
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
                                // Every initializer list is preceded by a
                                // whole-object zero, so the tail is done.
                                self.store_string_units(
                                    base_sym,
                                    base_offset,
                                    typ,
                                    &only.value.kind,
                                    &units,
                                    StringTail::AlreadyZero,
                                );
                                return;
                            }
                        }
                    }
                }

                let elem_size = self.types.size_bytes(elem_type);
                let elem_is_aggregate = matches!(
                    self.types.kind(elem_type),
                    TypeKind::Array | TypeKind::Struct | TypeKind::Union
                );

                let groups = self.group_array_init_elements(elements, typ);
                for element_index in groups.indices {
                    let Some(list) = groups.element_lists.get(&element_index) else {
                        continue;
                    };
                    let offset = base_offset + element_index * elem_size as i64;
                    // A string literal initializing an array element --
                    // `char arr[3][4] = {"Sun", "Mon", "Tue"}` -- is inline
                    // data, not one element. Recursing into it would treat the
                    // literal as the pointer it decays to everywhere else.
                    //
                    // Shared with the other three string-store paths rather
                    // than counted a third way here. Written out, this loop
                    // recognized all four literal kinds and then handled only
                    // `StringLit`, dropping a wide element and leaving it
                    // zero; stepped the destination by raw *bytes* where a
                    // wide element is 2 or 4 bytes wide; and had no capacity
                    // clamp at all, so `char s[1][3] = {"hello"}` stored five
                    // bytes into a three-byte object -- two of them past the
                    // whole local, not merely into the next row.
                    let string_element = (list.len() == 1
                        && self.types.kind(elem_type) == TypeKind::Array)
                        .then(|| Self::string_literal_units(&list[0].value.kind))
                        .flatten();
                    if let Some(units) = string_element {
                        self.store_string_units(
                            base_sym,
                            offset,
                            elem_type,
                            &list[0].value.kind,
                            &units,
                            StringTail::AlreadyZero,
                        );
                        continue;
                    }
                    if elem_is_aggregate {
                        self.linearize_init_list_at_offset(base_sym, offset, elem_type, list);
                        continue;
                    }
                    let Some(last) = list.last() else {
                        continue;
                    };
                    if self.types.is_complex(elem_type) || self.types.is_vector(elem_type) {
                        // A complex element is two halves, and a vector one
                        // its lanes, not the scalar the store below assumes.
                        // `elem_is_aggregate` is false for them, so they
                        // reach here.
                        self.store_addressed_value_at(base_sym, offset, elem_type, &last.value);
                        continue;
                    }
                    let converted = self.linearize_converted(&last.value, elem_type);
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
                        && self.types.size_bytes(expr_type) == self.types.size_bytes(typ)
                    {
                        let src_addr = self.linearize_lvalue(&elements[0].value);
                        let target_size_bytes = self.types.size_bytes(typ);
                        let vol = self.block_volatility(typ, expr_type);
                        self.emit_block_copy_at_offset(
                            base_sym,
                            base_offset,
                            src_addr,
                            target_size_bytes as i64,
                            vol,
                        );
                        return;
                    }
                }

                // What every member inherits from the object.
                let object_quals = self.types.qualifiers(typ);
                if let Some(composite) = self.types.get(typ).composite.as_ref() {
                    let members: Vec<_> = composite.members.clone();
                    let is_union = self.types.kind(typ) == TypeKind::Union;

                    let mut visits =
                        self.walk_struct_init_fields(typ, &members, is_union, elements);
                    self.admit_fam_visits(&mut visits, &members, InitStorage::Automatic);

                    // C17 6.7.9p19 resolves two initializers for overlapping
                    // storage by subobject. Storing them in list order is
                    // enough only where the later store covers every byte it
                    // supersedes; where it does not, the bytes it leaves have
                    // to be cleared first. See [`WrittenInit`].
                    let mut written: Vec<WrittenInit> = Vec::new();

                    for visit in visits {
                        let held = self.held_union_members(visit.typ, &visit.kind, visit.offset);
                        if let Some(reset) = self.init_override_reset(&mut written, &visit, held) {
                            let volatile = self.types.contains_volatile(typ);
                            self.emit_block_zero(
                                base_sym,
                                base_offset + reset.start as i64,
                                (reset.end - reset.start) as i64,
                                volatile,
                            );
                        }

                        let offset = base_offset + visit.offset as i64;
                        let field_type = visit.typ;
                        // Placed in the object being initialized, not in the
                        // aggregate the visit walked.
                        let bitfield = visit.bitfield().map(|bf| crate::types::Bitfield {
                            offset: offset as usize,
                            ..bf
                        });
                        // The member as a subobject of this object: a store
                        // into a `volatile` object is a volatile access even
                        // where the member was not declared so.
                        let inherits_volatile =
                            (object_quals | visit.quals).contains(TypeModifiers::VOLATILE);
                        let outer_volatile_object = self.volatile_init_object;
                        if inherits_volatile {
                            self.volatile_init_object = Some(base_sym);
                        }

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
                                if let Some(bf) = bitfield {
                                    // C17 6.7.9p11 initializes a member by
                                    // converting to the *member's* type, not the
                                    // storage unit's: for `_Bool` that is the
                                    // conversion normalizing to 0/1 (6.3.1.2), so
                                    // `struct { _Bool f:1; } v = {2};` stores 1.
                                    let member_val = self.linearize_converted(&expr, field_type);
                                    self.emit_bitfield_store(base_sym, bf, member_val, field_type);
                                } else {
                                    self.linearize_struct_field_init(
                                        base_sym, offset, field_type, &expr,
                                    );
                                }
                            }
                        }
                        self.volatile_init_object = outer_volatile_object;
                    }
                }
            }
            _ => {
                if let Some(element) = elements.first() {
                    // `double _Complex z = {1.0};` lands here rather than on
                    // the complex arm of `linearize_local_decl`, because the
                    // braces make it an initializer list first.
                    if self.types.is_complex(typ) || self.types.is_vector(typ) {
                        self.store_addressed_value_at(base_sym, base_offset, typ, &element.value);
                        return;
                    }
                    let converted = self.linearize_converted(&element.value, typ);
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

    /// Record that `visit` is about to be stored, and answer which bytes of
    /// the object must be cleared first for C17 6.7.9p19 to hold.
    ///
    /// Nothing at all, for the usual case where the entry overlaps none of
    /// those already stored. Otherwise the entry's own bytes -- so that the
    /// part of the subobject it does not fill reads as zero rather than as
    /// the initializer it replaces -- together with the bytes of any earlier
    /// entry it invalidates: all of one it wholly contains, all of one it is
    /// not a subobject of, and, where it reaches an earlier entry's bytes only
    /// by naming a member of a union inside it, that union's bytes.
    ///
    /// A bit-field is stored by reading its carrier and writing it back, so
    /// its own bytes are not storage it owns and clearing them would blank the
    /// members sharing the carrier. It therefore never clears its own span --
    /// only what an earlier entry it supersedes requires, which is how a
    /// bit-field naming a second member of a union still resets it.
    fn init_override_reset(
        &self,
        written: &mut Vec<WrittenInit>,
        visit: &StructFieldVisit,
        held: UnionMembers,
    ) -> Option<std::ops::Range<usize>> {
        if visit.field_size == 0 || visit.bit_width == Some(0) {
            return None;
        }
        let bits = visit.bit_offset.filter(|_| visit.bit_width.is_some());
        let span = match (visit.bit_offset, visit.bit_width) {
            // Only the bytes the field's own bits reach; its access span is
            // wider and covers bytes other members own.
            (Some(bit_offset), Some(bit_width)) => {
                crate::types::own_bit_bytes(visit.offset, bit_offset, bit_width)
            }
            _ => visit.offset..visit.offset + visit.field_size,
        };
        let mut reset: Option<std::ops::Range<usize>> = None;

        for entry in written.iter() {
            if entry.live.start >= span.end || span.start >= entry.live.end {
                continue;
            }
            // Two bit-fields are different objects even when they share a
            // carrier byte, and the same one written twice needs no clearing:
            // the second store reads the carrier back and replaces its bits.
            if bits.is_some() && entry.bits.is_some() {
                continue;
            }
            if bits.is_none() {
                widen_reset(&mut reset, span.clone());

                // Wholly superseded: the entry's own bytes are all inside the
                // ones being cleared and rewritten.
                if span.start <= entry.live.start && entry.live.end <= span.end {
                    continue;
                }
            }

            let entry_end = entry.origin + self.types.size_bytes(entry.typ);
            let inner = span
                .start
                .checked_sub(entry.origin)
                .filter(|_| span.end <= entry_end);
            let unions = UnionFold::new(&entry.held, &visit.unions, entry.origin);
            let place = match inner {
                None => SubobjectPlace::NotASubobject,
                Some(inner) if bits.is_some() => {
                    self.classify_bitfield_carrier(entry.typ, inner, span.end - span.start, unions)
                }
                Some(inner) => self.classify_subobject(entry.typ, inner, visit.field_size, unions),
            };
            match place {
                // A member of the earlier entry's object: the rest of that
                // object is a different subobject and stands.
                SubobjectPlace::Member => {}
                SubobjectPlace::ThroughUnion { offset, size } => {
                    widen_reset(
                        &mut reset,
                        entry.origin + offset..entry.origin + offset + size,
                    );
                }
                // The carrier the earlier entry wrote is shared: the bits this
                // one names are replaced in place and its neighbours' stand.
                SubobjectPlace::BitfieldCarrier { .. } => {}
                SubobjectPlace::NotASubobject => {
                    widen_reset(&mut reset, entry.live.clone());
                }
            }
        }

        if let Some((from, to)) = reset.as_ref().map(|range| (range.start, range.end)) {
            // Whatever the fill covers is gone; the bytes an entry keeps on
            // either side of it are still its own.
            *written = written
                .drain(..)
                .flat_map(|entry| {
                    let (origin, typ, held, bits) =
                        (entry.origin, entry.typ, entry.held, entry.bits);
                    [
                        entry.live.start..entry.live.end.min(from),
                        entry.live.start.max(to)..entry.live.end,
                    ]
                    .into_iter()
                    .filter(|live| live.start < live.end)
                    .map(move |live| WrittenInit {
                        live,
                        origin,
                        typ,
                        held: held.clone(),
                        bits,
                    })
                })
                .collect();
        }

        written.push(WrittenInit {
            live: span,
            origin: visit.offset,
            typ: visit.typ,
            held,
            bits,
        });
        reset
    }

    /// Store a complex value into `base_sym` at `offset`, as two halves, or
    /// a GNU vector value, as its bytes.
    ///
    /// Both live in memory and travel by *address*, so storing one the way a
    /// scalar member is stored would write the address instead of the value.
    ///
    /// The initializer's base precision need not match the object's -- and
    /// usually does not, because `I` is `__builtin_complex(0.0, 1.0)`, a
    /// *double* complex, so `float _Complex f = 2.0f + 3.0f*I;` is a
    /// conversion, and so is a real initializer. Both are converted by
    /// [`Self::linearize_converted`] before the halves are copied.
    pub(crate) fn store_addressed_value_at(
        &mut self,
        base_sym: PseudoId,
        offset: i64,
        typ: TypeId,
        init: &Expr,
    ) {
        if self.types.is_vector(typ) {
            let value_addr = self.vector_addr(init);
            let bytes = self.types.size_bytes(typ) as i64;
            let vol = self.block_volatility(typ, self.expr_type(init));
            self.emit_block_copy_at_offset(base_sym, offset, value_addr, bytes, vol);
            return;
        }
        let value_addr = self.linearize_converted(init, typ);
        self.copy_complex(base_sym, offset, value_addr, typ);
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
                    // Reached only from an initializer list, which the caller
                    // zeroed whole before walking it.
                    self.store_string_units(
                        base_sym,
                        offset,
                        field_type,
                        &value.kind,
                        &units,
                        StringTail::AlreadyZero,
                    );
                }
            } else {
                let converted = self.linearize_converted(value, field_type);
                let size = self.types.size_bits(field_type);
                self.emit(Instruction::store(
                    converted, base_sym, offset, field_type, size,
                ));
            }
        } else if self.types.is_complex(field_type) || self.types.is_vector(field_type) {
            self.store_addressed_value_at(base_sym, offset, field_type, value);
        } else {
            let (actual_type, actual_size) = if self.types.kind(field_type) == TypeKind::Array {
                let elem_type = self.types.base_type(field_type).unwrap_or(field_type);
                (elem_type, self.types.size_bits(elem_type))
            } else {
                (field_type, self.types.size_bits(field_type))
            };
            let converted = self.linearize_converted(value, actual_type);
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
        let cond_val = self.controlling_value(cond);

        let then_bb = self.alloc_bb();
        let else_bb = self.alloc_bb();
        let merge_bb = self.alloc_bb();

        let false_bb = if else_stmt.is_some() {
            else_bb
        } else {
            merge_bb
        };
        self.branch_on(cond_val, then_bb, false_bb);

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
        // From the block the condition ended in, which short-circuit
        // operators can make a different one from cond_bb.
        self.branch_on_condition(cond, body_bb, exit_bb);

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
        // From the block the condition ended in, which short-circuit
        // operators can make a different one from cond_bb.
        self.branch_on_condition(cond, body_bb, exit_bb);

        // Exit block
        self.switch_bb(exit_bb);
    }

    /// Lower everything a `for` loop needs before its body: the init clause,
    /// the four blocks, the condition and the jump targets. Leaves the body
    /// block current, for the caller to lower the body into.
    ///
    /// The returned [`OpenFor`] goes back to [`Self::close_for`].
    fn open_for(&mut self, init: Option<&ForInit>, cond: Option<&Expr>) -> OpenFor {
        // C99 for-loop declarations (e.g., for (int i = 0; ...)) are scoped
        // to the loop -- and so is a VLA declared there, which the scope
        // releases at `exit_bb`. That is the right place for it: `break` and
        // `continue` both land inside this scope, so neither may release it.
        let scope = self.push_scope();

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
            // From the block the condition ended in, which short-circuit
            // operators can make a different one from cond_bb.
            self.branch_on_condition(cond_expr, body_bb, exit_bb);
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

        OpenFor {
            scope,
            cond_bb,
            post_bb,
            exit_bb,
        }
    }

    /// Close the loop [`Self::open_for`] opened, once its body is lowered:
    /// the back edge through the post-expression, then the exit block, then
    /// the loop's scope.
    fn close_for(&mut self, open: OpenFor, post: Option<&Expr>) {
        let OpenFor {
            scope,
            cond_bb,
            post_bb,
            exit_bb,
        } = open;

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
        // From the block the post-expression ended in, which `&&`, `||` and
        // `?:` can make a different one from post_bb. Linking the back edge
        // from post_bb itself recorded an edge out of a block that no longer
        // holds the branch, and left the merge block that does hold it with an
        // unrecorded successor -- the loop then never terminated.
        if let Some(current) = self.current_bb {
            self.emit(Instruction::br(cond_bb));
            self.link_bb(current, cond_bb);
        }

        // Exit block
        self.switch_bb(exit_bb);

        // Drop the for-loop-scoped declarations and release their storage.
        self.pop_scope(scope);
    }

    pub(crate) fn linearize_for(
        &mut self,
        init: Option<&ForInit>,
        cond: Option<&Expr>,
        post: Option<&Expr>,
        body: &Stmt,
    ) {
        let open = self.open_for(init, cond);
        self.linearize_stmt(body);
        self.close_for(open, post);
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
        // C17 6.8.4.2p5: the integer promotions are performed on the
        // controlling expression, and each case constant is converted to the
        // *promoted* type. Comparing at the operand's own narrow width made a
        // label collide with a value it does not equal: `switch ((signed
        // char) -1)` matched `case 255:`, because both are 0xFF in eight bits,
        // where the promoted comparison is -1 against 255.
        let cmp_type = self.types.integer_promote(expr_type);
        let switch_val = self.emit_convert(switch_val, expr_type, cmp_type);
        let size = self.types.size_bits(cmp_type);

        let exit_bb = self.alloc_bb();

        // Push exit block for break handling
        self.break_targets.push(exit_bb);

        // Collect case labels and create basic blocks for each. C17 6.8.4.2p5
        // converts every label to `cmp_type`, and `conv` is that conversion:
        // the collector applies it, and the body's lowering below reaches the
        // labels back through the same value, so the two cannot drift apart.
        let conv = CaseConv::of(self.types, cmp_type);
        let (case_values, has_default) = self.collect_switch_cases(body, conv);
        let case_bbs: Vec<BasicBlockId> = case_values
            .ranges()
            .iter()
            .map(|_| self.alloc_bb())
            .collect();
        let default_bb = if has_default {
            Some(self.alloc_bb())
        } else {
            None
        };

        // Default goes to default_bb if present, otherwise exit_bb
        let default_target = default_bb.unwrap_or(exit_bb);

        if let Some(selector) = self.eval_const_expr(expr) {
            // A constant selector takes one edge, as a constant condition does
            // (`branch_on`): the labels it does not select are reached only by
            // falling into them, and a block nothing reaches is not emitted.
            // Selector and labels are both converted to the promoted type, and
            // the range test runs in that type's signedness -- a plain signed
            // `i128` comparison answered differently from the runtime lowering
            // of the very same switch.
            let selector = conv.convert(selector);
            let target = case_values
                .ranges()
                .iter()
                .position(|&(lo, hi)| conv.contains(lo, hi, selector))
                .map_or(default_target, |idx| case_bbs[idx]);
            if let Some(current) = self.current_bb {
                self.emit(Instruction::br(target));
                self.link_bb(current, target);
            }
        } else if size > 64 {
            // The `Switch` instruction carries its labels as `i64` and both
            // backends compare in one general register, so a controlling
            // expression wider than that had its high half ignored:
            // `switch ((__int128)1 << 64)` matched `case 0:`. A wide switch
            // is lowered to explicit comparisons instead, which go through
            // the ordinary 128-bit compare path and are right at any width.
            self.emit_wide_switch(
                switch_val,
                cmp_type,
                conv,
                case_values.ranges(),
                &case_bbs,
                default_target,
            );
        } else {
            // Build switch instruction with case -> block mapping. The labels
            // have been converted to `cmp_type`, which is at most 64 bits
            // here, so the cast keeps every bit of each one.
            let switch_cases: Vec<(i64, i64, BasicBlockId)> = case_values
                .ranges()
                .iter()
                .zip(case_bbs.iter())
                .map(|((lo, hi), bb)| (*lo as i64, *hi as i64, *bb))
                .collect();

            // Emit switch instruction
            self.emit(Instruction::switch_insn(
                switch_val,
                switch_cases.clone(),
                Some(default_target),
                size,
            ));

            // Link CFG edges from current block to all case/default/exit blocks
            if let Some(current) = self.current_bb {
                self.link_bb_many(current, switch_cases.iter().map(|&(_, _, bb)| bb));
                self.link_bb(current, default_target);
                if default_bb.is_none() {
                    self.link_bb(current, exit_bb);
                }
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

        // The body is ordinary code; its `case` and `default` labels find
        // their blocks through the context pushed here, wherever they sit in
        // it. The index carries the collector's own conversion, so a label is
        // found under exactly the key the collector filed it under.
        self.switch_stack.push(SwitchCtx {
            index: CaseIndex::of(&case_values),
            case_bbs,
            default_bb,
        });
        self.linearize_stmt(body);
        self.switch_stack.pop();

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
    /// The body is walked as any statement is, compound or not.
    pub(crate) fn collect_switch_cases(&self, body: &Stmt, conv: CaseConv) -> (CaseSet, bool) {
        let mut case_values = CaseSet::new(conv);
        let mut has_default = false;
        self.collect_cases_from_stmt(body, &mut case_values, &mut has_default);
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
        self.named_local(
            slot,
            format!("__goto_target.{}", slot.0),
            void_ptr,
            None,
            None,
        );

        let dispatch_bb = self.alloc_bb();
        let saved = self.current_bb;
        self.switch_bb(dispatch_bb);
        let loaded = self.alloc_pseudo();
        self.emit(Instruction::load(
            loaded,
            slot,
            0,
            void_ptr,
            self.ptr_bits(),
        ));
        self.emit(Instruction::indirect_br(loaded));
        self.current_bb = saved;

        self.indirect_dispatch = Some((dispatch_bb, slot));
        (dispatch_bb, slot)
    }

    /// 6.8.6.1p1: every label a `goto`, `&&label` or `asm goto` names has to
    /// be defined in the function.
    ///
    /// The block `get_or_create_label` minted for a missing one stays empty
    /// and unterminated, so control fell out of the function through whatever
    /// followed in layout order -- the program built, linked, and segfaulted
    /// or hung. Checked once the body is walked, because a forward reference
    /// is legal.
    pub(crate) fn check_label_references(&mut self) {
        for (label, pos) in std::mem::take(&mut self.label_refs) {
            if self.defined_labels.contains(&label) {
                continue;
            }
            if self.written_labels.contains(&label) {
                self.place_unevaluated_label(label);
                continue;
            }
            let name = self.str(label.name).to_string();
            crate::diag::error_args(pos, "label '{0}' used but not defined", &[&name]);
        }
    }

    /// Give a label written inside an operand that is never evaluated -- the
    /// statement expression of `sizeof(({ L: x; }))` -- a block of its own,
    /// which nothing reaches.
    ///
    /// The label exists, so "used but not defined" is the wrong complaint: a
    /// `goto` to it has already been reported as a jump into a statement
    /// expression, which is gcc's one error, and gcc accepts `&&L`, whose
    /// address has to name some block.
    fn place_unevaluated_label(&mut self, label: LabelId) {
        let resume = self.current_bb;
        let label_bb = self.get_or_create_label(label);
        self.switch_bb(label_bb);
        self.emit(Instruction::new(Opcode::Unreachable).with_type(self.types.void_id));
        self.defined_labels.insert(label);
        self.current_bb = resume;
    }

    /// Link the dispatch block to every label whose address was taken.
    ///
    /// Called once the function body is walked, because `&&label` may appear
    /// after the `goto *` that can reach it.
    ///
    /// Every such block is also marked `addr_taken`, dispatch or not. Without
    /// a `goto *` the address was stored for someone else, or only compared,
    /// and no edge reaches the block at all: it still has to survive DCE, or
    /// the symbol the address refers to is never emitted and the link fails
    /// on an undefined `.L` label. With one, the edges can disappear with the
    /// dispatch while the address lives on; and the mark is what tells the
    /// inliner which symbols name blocks it has to rename.
    pub(crate) fn finish_indirect_dispatch(&mut self) {
        for bb in self.addr_taken_labels.clone() {
            self.get_or_create_bb(bb).addr_taken = true;
            if let Some((dispatch_bb, _)) = self.indirect_dispatch {
                self.link_bb(dispatch_bb, bb);
            }
        }
    }

    /// The jumps whose target a function body may not reach from where they
    /// are written.
    ///
    /// C17 6.8.6.1p1: a `goto` shall not jump from outside the scope of an
    /// identifier having a variably modified type to inside it, and 6.8.4.2p2
    /// the same for a `switch` reaching a `case` inside such a scope. Entering
    /// the scope without executing the declaration leaves the object's size
    /// never computed: the array is whatever the stack held.
    ///
    /// gcc holds a statement expression to the same rule, since a jump into
    /// one arrives in the middle of evaluating the expression around it.
    ///
    /// Jumping *out* of either, within one, or to a label that precedes the
    /// declaration are all fine, and all are exercised by the accept-side
    /// tests. Reported at the jump, where gcc points; a variably modified
    /// scope also names the declaration that could not be entered, which gcc
    /// does not -- the position says where to look and the name says what the
    /// problem is.
    ///
    /// Answers the name of every label the body writes, evaluated or not,
    /// and which cleanup scopes each label lies in.
    pub(crate) fn check_jumps_into_protected_scopes(&self, body: &Stmt) -> LabelScopes {
        let w = JumpScopeWalk::of(body);

        // 6.8.1p3: a label name is unique within the function it appears in.
        // Two labels of one name were silently merged into one basic block, so
        // `L: i++; if (i<2) goto L; L: return i;` looped forever.
        let (first_label, duplicates) = w.resolve_labels();
        for i in duplicates {
            let (label, _, pos) = &w.labels[i];
            let spelled = self.strings.get(label.name).to_string();
            crate::diag::error_args(*pos, "duplicate label '{0}'", &[&spelled]);
        }

        // A `goto` is illegal exactly when its label sits inside a scope the
        // `goto` itself is not already in.
        for jump in &w.gotos {
            // A missing label is reported by `check_label_references` once
            // the body is lowered.
            let Some(&i) = first_label.get(&jump.label) else {
                continue;
            };
            let (_, to, label_pos) = &w.labels[i];
            let pos = jump.pos.unwrap_or(*label_pos);
            for id in w.entered(&jump.from, to) {
                self.report_protected_jump(&w.scopes[id], false, pos);
            }
        }
        for (id, pos) in w.bad_cases() {
            self.report_protected_jump(&w.scopes[id], true, pos);
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

        let cleanups = first_label
            .iter()
            .map(|(&label, &i)| (label, w.cleanup_vars(&w.labels[i].1)))
            .collect();
        let written = w.labels.iter().map(|(label, _, _)| *label).collect();
        LabelScopes { written, cleanups }
    }

    /// Report a jump into `scope`, by a `switch` reaching a label inside it
    /// or by a `goto`.
    fn report_protected_jump(&self, scope: &JumpScope, by_switch: bool, pos: Position) {
        match scope {
            JumpScope::VariablyModified(symbol) => {
                let what = if by_switch { "switch jump" } else { "jump" };
                let name = self.strings.get(self.symbols.get(*symbol).name);
                error(
                    pos,
                    &format!(
                        "{what} into the scope of '{name}', which has a variably modified type",
                    ),
                );
            }
            // Entering one is legal: gcc accepts it in silence, and the
            // cleanup then runs on whatever the variable holds.
            JumpScope::Cleanup(_) => {}
            JumpScope::StmtExpr => {
                let message = if by_switch {
                    "switch jumps into statement expression"
                } else {
                    "jump into statement expression"
                };
                error(pos, &gettextrs::gettext(message));
            }
        }
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

    /// One case endpoint converted to the promoted controlling type, warning
    /// if the controlling type cannot hold the constant the label spells.
    ///
    /// C17 6.8.4.2p5 requires the conversion, and most of the time it changes
    /// nothing worth saying: `case -1:` in a `switch` on `unsigned` becomes
    /// 4294967295, which is exactly the value it is written to match, and gcc
    /// and clang are both silent there. What is worth a diagnostic is a label
    /// whose bits the conversion throws away, silently turning
    /// `case 4294967296LL:` into `case 0:`.
    ///
    /// The line between the two is whether converting back to the label's own
    /// type returns the constant: a merely reinterpreted value round-trips,
    /// while a truncated one does not. That is the test clang applies, and
    /// gcc's `-Wswitch-outside-range` draws the line in the same place, which
    /// is also the `-Wno-` name that silences this.
    fn convert_case_label(&self, expr: &Expr, val: i128, conv: CaseConv) -> i128 {
        let converted = conv.convert(val);
        if converted == val {
            return val;
        }
        // The label's own type, which the round trip goes back through. An
        // untyped or non-integer label is already an error elsewhere; treat it
        // as full width, which reduces the round trip to a plain comparison.
        let own = expr.typ.filter(|&t| self.types.is_integer(t)).map_or_else(
            || CaseConv::new(128, false),
            |t| CaseConv::of(self.types, t),
        );
        if own.convert(converted) != val && crate::diag::warning_group_enabled(CASE_RANGE_WARNING) {
            crate::diag::warning(
                expr.pos,
                &format!(
                    "overflow converting case value to switch condition type \
                     ({val} to {converted})"
                ),
            );
        }
        converted
    }

    /// Record one `case` label's value, or range, for its switch.
    fn collect_case_label(&self, expr: &Expr, high: Option<&Expr>, case_values: &mut CaseSet) {
        // Extract constant value from case expression
        let Some(raw_lo) = self.eval_const_expr(expr) else {
            self.report_unfoldable_case(expr);
            return;
        };
        // A GNU range `case lo ... hi:`. An absent high endpoint is
        // the ordinary label, held as the degenerate range `(v, v)` so
        // that everything downstream has one shape.
        let raw_hi = match high {
            None => Some(raw_lo),
            Some(hi_expr) => match self.eval_const_expr(hi_expr) {
                Some(h) => Some(h),
                None => {
                    self.report_unfoldable_case(hi_expr);
                    None
                }
            },
        };
        let Some(raw_hi) = raw_hi else { return };

        // C17 6.8.4.2p5: each case constant is converted to the
        // promoted type of the controlling expression. Evaluating the
        // label at full width and never converting it left c17's two
        // lowerings disagreeing about the same switch -- a runtime
        // selector kept the unconverted label in the `switch`
        // instruction, where the backend truncated it, while the
        // constant-selector path compared at 128 bits and did not
        // match at all. `case 4294967296LL:` in an `int` switch is
        // `case 0:`, and has to be that for both.
        let conv = case_values.conv();
        let lo = self.convert_case_label(expr, raw_lo, conv);
        let hi = match high {
            None => lo,
            Some(hi_expr) => self.convert_case_label(hi_expr, raw_hi, conv),
        };

        // 6.8.4.2p3 forbids two equal case constants, and GCC
        // extends that to overlapping ranges -- an overlap would
        // otherwise make one arm silently unreachable, since the
        // body walk resolves a label by finding the first match.
        // Both tests run on the converted values, since that is what
        // "equal" means once p5 has been applied: `case 0:` beside
        // `case 4294967296LL:` in an `int` switch is one value twice.
        //
        // Order by the switch type's own signedness. The endpoints are
        // carried as `i128`, and an unsigned 64-bit bound above
        // `i64::MAX` is still positive there -- but an unsigned
        // *128-bit* one is not, so the reinterpretation is still
        // needed: `case 0ul ... ULONG_MAX:` read as an empty range and
        // never matched.
        if conv.lt(hi, lo) {
            // GCC accepts an empty range, warns, and never matches
            // it. Nothing is recorded, so nothing can overlap it.
            crate::diag::warning(expr.pos, "empty range specified");
            return;
        }
        if let Some((lo2, hi2)) = case_values.overlap(lo, hi) {
            let what = if lo == hi && lo2 == hi2 {
                format!("duplicate case value '{}' in switch", lo)
            } else {
                format!(
                    "duplicate (or overlapping) case value: {}..{} overlaps {}..{}",
                    lo, hi, lo2, hi2
                )
            };
            error(expr.pos, &what);
        }
        case_values.insert(lo, hi);
    }

    fn collect_default_label(&self, has_default: &mut bool) {
        // C99 6.8.4.2p3: at most one default label per switch.
        if *has_default {
            error(
                self.current_pos.unwrap_or_default(),
                "multiple default labels in one switch",
            );
        }
        *has_default = true;
    }

    pub(crate) fn collect_cases_from_stmt(
        &self,
        stmt: &Stmt,
        case_values: &mut CaseSet,
        has_default: &mut bool,
    ) {
        match stmt {
            Stmt::Labeled { labels, stmt } => {
                for label in labels {
                    match label {
                        Label::Case(expr, high) => {
                            self.collect_case_label(expr, high.as_ref(), case_values)
                        }
                        Label::Default(_) => self.collect_default_label(has_default),
                        Label::Named { .. } => {}
                    }
                }
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
            // The rest hold statements only inside statement expressions,
            // which no switch jump may enter; their lowering hides the
            // enclosing switches as well.
            _ => {}
        }
    }

    /// Evaluate a constant expression (for case labels, static initializers)
    ///
    /// Copy a string literal's code units into an array object, followed by
    /// its null terminator.
    ///
    /// Shared by every way a string can initialize an array: written directly
    /// (`char b[] = "hi"`), enclosed in braces (`char b[] = {"hi"}`, C17
    /// 6.7.9p14), as a struct member (`struct { char t[4]; } s = {"hi"}`), or
    /// as an element of a nested array (`char n[2][4] = {"ab", "cd"}`).
    ///
    /// `tail` says whether the elements the literal does not reach are this
    /// call's to zero; see [`StringTail`].
    pub(crate) fn store_string_units(
        &mut self,
        base_sym: PseudoId,
        base_offset: i64,
        arr_typ: TypeId,
        kind: &ExprKind,
        units: &[i128],
        tail: StringTail,
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
        // The first element past everything the literal and its terminator
        // wrote. When the literal fills the array exactly, that is the whole
        // array; when the terminator was dropped, nothing is left either.
        let written = if units.len() < capacity {
            let null_val = self.emit_const(0, elem_type);
            self.emit(Instruction::store(
                null_val,
                base_sym,
                base_offset + (units.len() as i64) * elem_bytes,
                elem_type,
                elem_size,
            ));
            units.len() + 1
        } else {
            capacity
        };

        // C17 6.7.9p21: the members not initialized explicitly are
        // initialized as a static object would be, i.e. to zero. One
        // terminator is not the rest of the array -- `char b[8] = "hi"` wrote
        // three bytes and left five holding whatever the frame held, which on
        // first entry is zero because the backend zeroes the whole frame, and
        // on re-execution is the last iteration's data.
        //
        // Routed through the shared block fill, so the bound that keeps
        // `char b[1 << 20] = "x"` from becoming a million stores is the one
        // every other block operation uses.
        if matches!(tail, StringTail::Zero) {
            let start = base_offset + (written as i64) * elem_bytes;
            let bytes = (capacity - written) as i64 * elem_bytes;
            let volatile = self.types.contains_volatile(arr_typ);
            self.emit_block_zero(base_sym, start, bytes, volatile);
        }
    }

    /// The code units a string literal contributes to an array initializer,
    /// or `None` if this is not a string literal.
    ///
    /// A narrow literal's parsed form is a literal payload, one C byte per
    /// Rust `char`, so its units are [`payload_bytes`] — iterating `bytes()`
    /// UTF-8-encodes anything at or above 0x80, which turned
    /// `char a[] = "\x80"` into the two bytes 0xC2 0x80 and left the array one
    /// byte short of what `sizeof` reported.
    fn string_literal_units(kind: &ExprKind) -> Option<Vec<i128>> {
        match kind {
            ExprKind::StringLit(s) => Some(payload_bytes(s).map(i128::from).collect()),
            ExprKind::Utf16StringLit(u) => Some(u.iter().map(|c| *c as i128).collect()),
            ExprKind::WideStringLit(u) | ExprKind::Utf32StringLit(u) => {
                Some(u.iter().map(|c| *c as i128).collect())
            }
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
            // Its size expressions are evaluated at run time.
            ExprKind::VmTypeName { .. } => true,

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
        let global_name = self.global_name_of(symbol_id)?;
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

    /// The scalar initializer an element or member of a `const` object was
    /// given -- `a[1]`, `s.m`, `t.v[2].m` -- when the object's own
    /// initializer folded, as [`Self::const_object_value`] answers for the
    /// object named whole. gcc folds these in a static initializer too.
    ///
    /// Without it, `const int a[2] = {1, 2}; int w = a[1];` was initialized
    /// with the element's *address*: eight bytes of relocation over a
    /// four-byte `int`.
    fn const_subobject_init(&self, expr: &Expr) -> Option<&crate::ir::Initializer> {
        let (symbol_id, offset) = self.const_subobject_place(expr)?;
        let global_name = self.global_name_of(symbol_id)?;
        let global = self
            .module
            .globals
            .iter()
            .find(|g| g.name == global_name && g.is_const)?;
        let size = self.types.size_bytes(expr.typ?);
        let whole = self.types.size_bytes(global.typ);
        initializer_leaf(&global.init, offset, size, whole)
    }

    /// The object `expr` designates a subobject of, and the subobject's byte
    /// offset in it. Only a path of constant subscripts into arrays and of
    /// members that are not bit-fields qualifies.
    fn const_subobject_place(&self, expr: &Expr) -> Option<(crate::symbol::SymbolId, usize)> {
        match &expr.kind {
            ExprKind::Ident(symbol_id) => Some((*symbol_id, 0)),
            ExprKind::Index { array, index } if self.types.kind(array.typ?) == TypeKind::Array => {
                let (symbol_id, base) = self.const_subobject_place(array)?;
                let i = crate::constexpr::eval(self, ConstScope::StaticInitializer, index)?;
                let step = self.types.size_bytes(expr.typ?);
                let at = usize::try_from(i).ok()?.checked_mul(step)?;
                Some((symbol_id, base.checked_add(at)?))
            }
            ExprKind::Member {
                expr: inner,
                member,
            } => {
                let info = self.types.find_member(inner.typ?, *member)?;
                if info.bit_width.is_some() {
                    return None;
                }
                let (symbol_id, base) = self.const_subobject_place(inner)?;
                Some((symbol_id, base + info.offset))
            }
            _ => None,
        }
    }

    /// [`Self::const_object_value`] for a floating `const` object.
    fn const_object_float_value(&self, symbol_id: crate::symbol::SymbolId) -> Option<FloatVal> {
        let global_name = self.global_name_of(symbol_id)?;
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

    /// The byte offset `array[index]` adds to the address of `array`.
    ///
    /// Shared by the two static-address walks, which differ only in whether
    /// they may intern a literal, not in what a subscript means.
    pub(crate) fn index_byte_offset(&self, array: &Expr, index: &Expr) -> Option<i64> {
        let idx = self.eval_const_init_expr(index)?;
        let elem_type = self.types.base_type(array.typ?)?;
        Some(idx as i64 * self.types.size_bytes(elem_type) as i64)
    }

    /// Evaluate a static address expression (for initializers like `&symbol.field`)
    ///
    /// Reached only from the initializer path, so a subscript folds with
    /// [`Self::eval_const_init_expr`]: `const int c = 5; int *p = &a[c-3];`
    /// is an ordinary static initializer and gcc accepts it.
    ///
    /// Returns Some((symbol_name, offset)) if the expression is a valid static address,
    /// or None if it can't be computed at compile time.
    /// Is this expression an *address*, whatever its type says?
    ///
    /// A pointer is one, as are an array and a function, which decay to one.
    /// So is a cast of one: `(unsigned long)&x` has integer type and is still
    /// a relocation, which is how a kernel or a linker script's C half writes
    /// an address constant.
    ///
    /// An object *read* is not one, however freely its address could be
    /// taken. `int v = 5; int w = v + 1;` is not a constant expression, and
    /// asking `eval_static_address` alone would have answered that it was --
    /// that walk takes the address of any named object it is handed.
    pub(crate) fn is_address_valued(&self, expr: &Expr) -> bool {
        if expr
            .typ
            .is_some_and(|t| self.types.kind(self.types.decayed_value(t)) == TypeKind::Pointer)
        {
            return true;
        }
        match &expr.kind {
            ExprKind::Cast { expr: inner, .. } => self.is_address_valued(inner),
            ExprKind::Unary {
                op: UnaryOp::AddrOf,
                ..
            } => true,
            // An address plus or minus an integer is an address. An address
            // *minus an address* is an integer -- a byte or element count --
            // so the difference of two members of one object must not be read
            // as a relocation.
            ExprKind::Binary { op, left, right } => {
                let (l, r) = (self.is_address_valued(left), self.is_address_valued(right));
                match op {
                    BinaryOp::Add => l != r,
                    BinaryOp::Sub => l && !r,
                    _ => false,
                }
            }
            _ => false,
        }
    }

    pub(crate) fn eval_static_address(&mut self, expr: &Expr) -> Option<(String, i64)> {
        match &expr.kind {
            // A literal has a perfectly good static address, but only once it
            // has been interned and given a label -- which is why this walk
            // takes `&mut self`. Handling them anywhere but here means
            // handling the nodes *above* them twice; that split is what made
            // `"X" + 1` work while `&("X"[0])` was a diagnostic.
            ExprKind::StringLit(lit) => {
                let lit = lit.clone();
                Some((self.module.add_string(lit), 0))
            }
            ExprKind::Utf16StringLit(units) => {
                let units = units.clone();
                Some((self.module.add_utf16_string(units), 0))
            }
            ExprKind::WideStringLit(units) | ExprKind::Utf32StringLit(units) => {
                let units = units.clone();
                Some((self.module.add_utf32_string(units), 0))
            }

            // A compound literal at file scope has static storage duration
            // (C99 6.5.2.5p5), so it is an object with an address -- but it
            // only acquires one when it is given a name here.
            ExprKind::CompoundLiteral { typ, elements } => {
                let name = format!(".CL{}", self.compound_literal_counter);
                self.compound_literal_counter += 1;
                let typ = *typ;
                let elements = elements.clone();
                let init = self.new_static_object_init(&elements, typ);
                self.module.add_global(&name, typ, init);
                Some((name, 0))
            }

            // `&*x` is `x`: the pair cancels and no object is read. The
            // operand has to be something already *shaped* like an address,
            // which is what `static_address_operand` decides -- the `Ident`
            // arm below answers with the address *of* an object, because
            // every other caller has already seen an `&`, so recursing into
            // it here would fold `&*p` to the address of `p` rather than to
            // its value.
            ExprKind::Unary {
                op: UnaryOp::Deref,
                operand,
            } => {
                let operand = operand.clone();
                self.static_address_operand(&operand)
            }

            // Simple identifier: &symbol
            ExprKind::Ident(symbol_id) => Some((self.global_name_of(*symbol_id)?, 0)),

            // Member access: expr.member
            ExprKind::Member { expr: base, member } => {
                // Recursively evaluate the base address
                let (name, base_offset) = self.eval_static_address(base)?;
                let (at, _) = crate::constexpr::member_at(self, base.typ?, *member)?;
                Some((name, base_offset + at as i64))
            }

            // Arrow access: expr->member (pointer dereference + member access)
            // Can be a static address when the pointer is a static address-of expression
            // (e.g., (&static_struct.field)->subfield in CPython macros)
            ExprKind::Arrow { expr: base, member } => {
                let (name, base_offset) = self.eval_static_address(base)?;
                let pointee = self.types.base_type(base.typ?)?;
                let (at, _) = crate::constexpr::member_at(self, pointee, *member)?;
                Some((name, base_offset + at as i64))
            }

            // Array subscript: array[index]
            ExprKind::Index { array, index } => {
                let (name, base_offset) = self.eval_static_address(array)?;
                Some((name, base_offset + self.index_byte_offset(array, index)?))
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
                // Which side names a symbol, not which side has a pointer
                // type. A cast to an integer makes `(unsigned long)&_text - 1`
                // ordinary arithmetic to the type system and a relocation with
                // an addend to the linker, and asking the type alone answered
                // "not a constant expression" for every such initializer --
                // which is how a kernel or a linker script's C half is
                // written.
                let (ptr_expr, int_expr, is_sub) = if self.is_address_valued(left) {
                    (left.as_ref(), right.as_ref(), *op == BinaryOp::Sub)
                } else if self.is_address_valued(right) && *op == BinaryOp::Add {
                    (right.as_ref(), left.as_ref(), false)
                } else {
                    return None;
                };

                let (name, base_offset) = self.eval_static_address(ptr_expr)?;
                let int_val = self.eval_const_init_expr(int_expr)?;

                // Scale by pointee size for pointer arithmetic
                let pointee_size = ptr_expr
                    .typ
                    .and_then(|t| self.types.arithmetic_pointee(t))
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

    /// Lower a `switch` whose controlling expression is wider than a general
    /// register into explicit comparisons.
    ///
    /// The `Switch` instruction carries its labels as `i64` and both backends
    /// compare the value in one register, so a `__int128` controlling
    /// expression had its high half ignored -- `switch ((__int128)1 << 64)`
    /// matched `case 0:`. Comparing explicitly goes through the ordinary
    /// 128-bit compare path, which is right at any width, and keeps the case
    /// constants at full precision too.
    ///
    /// Leaves the cursor on a block that falls into `default_target`, and
    /// links every edge it creates.
    fn emit_wide_switch(
        &mut self,
        switch_val: PseudoId,
        cmp_type: TypeId,
        conv: CaseConv,
        case_values: &[(i128, i128)],
        case_bbs: &[BasicBlockId],
        default_target: BasicBlockId,
    ) {
        // `>=` and `<=` for a range, in the controlling type's own signedness
        // -- the same `conv` that converted the labels, so the comparison and
        // the constants it compares are describing one type.
        let (ge, le) = if conv.unsigned() {
            (Opcode::SetAe, Opcode::SetBe)
        } else {
            (Opcode::SetGe, Opcode::SetLe)
        };

        for (&(lo, hi), &case_bb) in case_values.iter().zip(case_bbs.iter()) {
            debug_assert_eq!(
                (conv.convert(lo), conv.convert(hi)),
                (lo, hi),
                "a case label reaches lowering already converted to the controlling type"
            );
            let next = self.alloc_bb();
            let cond = if lo == hi {
                let k = self.emit_const(lo, cmp_type);
                self.emit_compare(Opcode::SetEq, switch_val, k, cmp_type)
            } else {
                // A GNU `case lo ... hi:` range.
                let lo_k = self.emit_const(lo, cmp_type);
                let hi_k = self.emit_const(hi, cmp_type);
                let at_least = self.emit_compare(ge, switch_val, lo_k, cmp_type);
                let at_most = self.emit_compare(le, switch_val, hi_k, cmp_type);
                let int_typ = self.types.int_id;
                let int_bits = self.types.size_bits(int_typ);
                self.emit_int_binop(Opcode::And, at_least, at_most, int_typ, int_bits)
            };
            self.branch_on(Controlling::Value(cond), case_bb, next);
            self.switch_bb(next);
        }

        self.link_to_merge_if_needed(default_target);
    }

    /// Start the block of the `case` label `expr` (or `expr ... high`), in
    /// the innermost `switch` being lowered.
    ///
    /// The endpoints are the label's raw constants; `CaseIndex::lookup`
    /// converts them to the promoted controlling type with the very
    /// conversion the collector used, which is what keeps this lookup from
    /// missing and dropping the case body into the wrong block. With no
    /// `switch` open the label is stray, which
    /// `check_jumps_into_protected_scopes` reports; the statement it labels
    /// is still lowered, so the rest of the function is not lost behind it.
    fn enter_case_label(&mut self, expr: &Expr, high: Option<&Expr>) {
        let Some(lo) = self.eval_const_expr(expr) else {
            return;
        };
        let hi = match high {
            None => Some(lo),
            Some(hi_expr) => self.eval_const_expr(hi_expr),
        };
        let Some(ctx) = self.switch_stack.last() else {
            return;
        };
        let Some(idx) = hi.and_then(|hi| ctx.index.lookup(lo, hi)) else {
            return;
        };
        self.fall_into_case_block(ctx.case_bbs[idx]);
    }

    /// Start the block of a `default` label, in the innermost `switch` being
    /// lowered. As for `case`, a stray one has been reported already.
    fn enter_default_label(&mut self) {
        if let Some(bb) = self.switch_stack.last().and_then(|ctx| ctx.default_bb) {
            self.fall_into_case_block(bb);
        }
    }

    /// Make `bb`, a `case` or `default` label's block, current, falling
    /// through into it from the code before the label if that code does not
    /// already end in a jump.
    fn fall_into_case_block(&mut self, bb: BasicBlockId) {
        if !self.is_terminated() {
            if let Some(current) = self.current_bb {
                self.emit(Instruction::br(bb));
                self.link_bb(current, bb);
            }
        }
        self.switch_bb(bb);
    }

    // Inline assembly linearization

    /// What each operand's constraint allows on the target being compiled,
    /// outputs then inputs. Classified here once and stored on the operand,
    /// so liveness, the allocator and the backend all read the same answer.
    ///
    /// A constraint c17 cannot honour is reported here, at its operand --
    /// one it does not model, or one at odds with its side of the colon, with
    /// gcc's wording for the latter. Such an operand is then given a plain
    /// register class so the statement can still be linearized; the error
    /// stops the compilation.
    fn asm_operand_classes(
        &self,
        outputs: &[AsmOperand],
        inputs: &[AsmOperand],
    ) -> (Vec<AsmOperandClass>, Vec<AsmOperandClass>) {
        let classify = |op: &AsmOperand, is_output: bool| {
            let reason = match AsmOperandClass::parse(&op.constraint, self.target.arch) {
                Err(e) => Err(e.message(&op.constraint)),
                Ok(class) => asm_operand_misuse(&class, is_output, outputs.len())
                    .map_or(Ok(class), |why| Err(why.to_string())),
            };
            reason.unwrap_or_else(|msg| {
                error(op.expr.pos, &msg);
                let plain = if is_output { "=r" } else { "r" };
                AsmOperandClass::parse(plain, self.target.arch).expect("a register class")
            })
        };
        (
            outputs.iter().map(|op| classify(op, true)).collect(),
            inputs.iter().map(|op| classify(op, false)).collect(),
        )
    }

    /// An immediate-only operand that is a constant expression, as the
    /// constant: gcc's front end folds it at every level, so `"i"(~MASK)` or
    /// `"i"(-1.0)` at -O0 must not reach the backend as an instruction
    /// computing it. `None` for any other operand, which is linearized as
    /// usual and left for `ir::asm_operand::resolve_immediates` to judge.
    fn asm_constant_operand(
        &mut self,
        expr: &Expr,
        typ: TypeId,
        class: &AsmOperandClass,
    ) -> Option<PseudoId> {
        if !class.is_immediate_only() {
            return None;
        }
        // As `linearize_expr` would: the statement takes its position from
        // its operands, and a diagnostic about this one is reported there.
        self.current_pos = Some(expr.pos);
        if self.types.is_integer(typ) {
            let v = self.eval_const_expr(expr)?;
            return Some(self.emit_const(v, typ));
        }
        let fmt = self
            .types
            .fp_format(typ)
            .filter(|_| self.types.is_float(typ))?;
        let v = crate::constexpr::eval_as_float(self, ConstScope::Standard, expr, typ)?;
        Some(self.emit_fconst(v.round_to_format(fmt), typ))
    }

    /// Linearize an inline assembly statement
    pub(crate) fn linearize_asm(
        &mut self,
        template: &str,
        outputs: &[AsmOperand],
        inputs: &[AsmOperand],
        clobbers: &[String],
        goto_labels: &[LabelId],
    ) {
        let mut ir_outputs = Vec::new();
        let mut ir_inputs = Vec::new();
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
        let (output_classes, input_classes) = self.asm_operand_classes(outputs, inputs);
        let tied_inputs: Vec<bool> = {
            let mut out_has_tied_input = vec![false; outputs.len()];
            for class in &input_classes {
                if let Some(idx) = class.tied {
                    if idx < out_has_tied_input.len() {
                        out_has_tied_input[idx] = true;
                    }
                }
            }
            out_has_tied_input
        };

        // Where each non-parameter output operand lives, resolved **once**.
        // A `+r` operand is read before the asm and written after it, and both
        // sides used to call `linearize_lvalue` on the operand expression --
        // so `asm("" : "+r"(*bar()))` called `bar` twice. Same rule as any
        // other read-modify-write (see `RmwPlace`).
        let mut output_places: Vec<super::linearize_emit::RmwPlace> =
            Vec::with_capacity(outputs.len());

        // Process output operands
        for (output_idx, op) in outputs.iter().enumerate() {
            let class = output_classes[output_idx];
            let is_memory = class.is_memory_only();
            let is_readwrite = class.access == AsmAccess::ReadWrite;
            let place = self.resolve_rmw_place(&op.expr);

            // Get symbolic name if present
            let name = op.name.map(|n| self.str(n).to_string());

            let typ = self.asm_operand_type(&op.expr, is_memory);
            let size = self.types.size_bits(typ);

            // For memory-class outputs (`=m`/`+m`/...): the asm operand
            // is the ADDRESS of the lvalue, not a register holding a
            // value. The asm performs the read/write through that
            // address with its own `ldr`/`str` (or x86 equivalents), so
            // no initial Load or post-asm Store is needed at the IR
            // level. Just use the lvalue address as the asm operand
            // pseudo directly.
            if is_memory {
                // A memory operand is the lvalue's address. A bit-field has
                // no address at all and keeps the old path's behaviour.
                let addr = match Self::rmw_place_address(&place) {
                    Some(addr) => addr,
                    None => self.linearize_lvalue(&op.expr),
                };
                let (addr, offset) = self.asm_memory_operand(addr);
                output_places.push(place);

                if is_readwrite {
                    // `+m` — also add as matching input so the same
                    // operand number works on both sides.
                    ir_inputs.push(AsmConstraint {
                        pseudo: addr,
                        name: name.clone(),
                        matching_output: Some(ir_outputs.len()),
                        constraint: op.constraint.clone(),
                        class,
                        size,
                        offset,
                    });
                }

                ir_outputs.push(AsmConstraint {
                    pseudo: addr,
                    name,
                    matching_output: None,
                    constraint: op.constraint.clone(),
                    class,
                    size,
                    offset,
                });

                skip_post_handling.push(true);
                continue;
            }

            // Non-memory output (register or value class): allocate a
            // fresh pseudo for the asm to write into.
            let pseudo = self.alloc_pseudo();

            // For read-write outputs ("+r"), load the initial value into the SAME pseudo
            // so that input and output use the same register — unless a
            // tied matching input (e.g. `"0"(x)`) will later supply its own
            // value via a Copy into the same pseudo, which would otherwise
            // produce a double-definition (and violates the SSA-style
            // validator invariant after optimization).
            if is_readwrite {
                if !tied_inputs[output_idx] {
                    // Through the resolved place, so the operand expression
                    // runs once for the read and the write.
                    let val = self.load_rmw_place(&place, typ);
                    self.emit(
                        Instruction::new(Opcode::Copy)
                            .with_target(pseudo)
                            .with_src(val)
                            .with_type(typ)
                            .with_size(size),
                    );
                }

                // Also add as input, using the SAME pseudo and marking as matching
                // the output. Per GCC convention, '+' creates one operand number
                // shared by both output and input.
                ir_inputs.push(AsmConstraint {
                    pseudo, // Same pseudo as output - ensures same register
                    name: name.clone(),
                    matching_output: Some(ir_outputs.len()), // matches the output about to be pushed
                    constraint: op.constraint.clone(),
                    class,
                    size,
                    offset: 0,
                });
            }

            ir_outputs.push(AsmConstraint {
                pseudo,
                name,
                matching_output: None,
                constraint: op.constraint.clone(),
                class,
                size,
                offset: 0,
            });

            skip_post_handling.push(false);
            output_places.push(place);
        }

        // Process input operands
        for (op, &class) in inputs.iter().zip(&input_classes) {
            let is_memory = class.is_memory_only();
            let matching = class.tied;

            // Get symbolic name if present
            let name = op.name.map(|n| self.str(n).to_string());

            // A value operand is the expression's value, and an array or a
            // function there has decayed to a pointer (C17 6.3.2.1p3-4): its
            // width is the pointer's, not the array's. A memory operand is the
            // object itself and keeps the object's size.
            let declared = self.expr_type(&op.expr);
            let typ = self.asm_operand_type(&op.expr, is_memory);
            let typ = match self.types.kind(typ) {
                _ if typ != declared => typ,
                TypeKind::Array if !is_memory => {
                    let elem = self.types.base_type(typ).unwrap_or(typ);
                    self.types.pointer_to(elem)
                }
                TypeKind::Function if !is_memory => self.types.pointer_to(typ),
                _ => typ,
            };
            let size = self.types.size_bits(typ);

            // For matching constraints (like "0"), we need to load the input value
            // into the matched output's pseudo so they use the same register
            let mut memory_offset = 0;
            let pseudo = if let Some(match_idx) = matching {
                if match_idx < ir_outputs.len() {
                    // Use the matched output's pseudo
                    let out_pseudo = ir_outputs[match_idx].pseudo;
                    // Load the input value into the output's pseudo
                    let val = self.asm_value(&op.expr, typ);
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
                    self.asm_value(&op.expr, typ)
                }
            } else if is_memory {
                // For memory operands, get the address -- or the object
                // itself, when the address is a constant one.
                let addr = self.linearize_lvalue(&op.expr);
                let (pseudo, offset) = self.asm_memory_operand(addr);
                memory_offset = offset;
                pseudo
            } else if let Some(k) = self.asm_constant_operand(&op.expr, typ, &class) {
                k
            } else {
                // For register operands, evaluate the expression
                self.asm_value(&op.expr, typ)
            };

            ir_inputs.push(AsmConstraint {
                pseudo,
                name,
                matching_output: matching,
                constraint: op.constraint.clone(),
                class,
                size,
                offset: memory_offset,
            });
        }

        // Process goto labels - map label names to BasicBlockIds. The AST
        // keeps no position per label, so a missing one is reported at the
        // statement.
        let pos = self.current_pos.unwrap_or_default();
        let ir_goto_labels: Vec<(BasicBlockId, String)> = goto_labels
            .iter()
            .map(|&label| {
                let bb = self.refer_to_label(label, pos);
                // The template names the label as written: `%l[name]`.
                (bb, self.str(label.name).to_string())
            })
            .collect();

        // An `asm goto` with outputs leaves them valid on every path, the
        // label edges as well as the fall-through (gcc's documented rule, and
        // what the Linux kernel's user-access helpers rely on). The write-back
        // below ran only on the fall-through, so a jump reached its label
        // with every output unstored. Each label edge now gets a block of its
        // own that writes the outputs back and then jumps to the label.
        let writes_back = skip_post_handling.iter().any(|skip| !skip);
        // An `asm goto` has two kinds of exit and each must release the VLA
        // scopes it leaves. The fall-through is released by the enclosing
        // scope's own end, but a label edge branches straight past it -- so
        // the release goes in the edge block, which therefore has to exist
        // even when there is nothing to write back.
        let releases_vlas = !self.vla_marks.is_empty();
        let needs_edge_block = writes_back || releases_vlas;
        let label_edges: Vec<(BasicBlockId, BasicBlockId, String)> = ir_goto_labels
            .iter()
            .map(|(target, name)| {
                let edge = if needs_edge_block {
                    self.alloc_bb()
                } else {
                    *target
                };
                (edge, *target, name.clone())
            })
            .collect();

        // Create the asm data
        let asm_data = AsmData {
            template: template.to_string(),
            outputs: ir_outputs.clone(),
            inputs: ir_inputs,
            clobbers: clobbers.to_vec(),
            goto_labels: label_edges
                .iter()
                .map(|(edge, _, name)| (*edge, name.clone()))
                .collect(),
        };

        // Emit the asm instruction
        self.emit(Instruction::asm(asm_data));

        // For asm goto: add edges to all possible label targets
        // The asm may jump to any of these labels, so control flow can go there
        if !label_edges.is_empty() {
            if let Some(current) = self.current_bb {
                // After the asm instruction, we need a basic block for fall-through
                // and edges to all goto targets
                let fall_through = self.alloc_bb();

                // Add edges to all goto label targets
                for (edge, _, _) in &label_edges {
                    self.link_bb(current, *edge);
                }

                // Add edge to fall-through (normal case when asm doesn't jump)
                self.link_bb(current, fall_through);

                // Emit an explicit branch to the fallthrough block
                // This is necessary because the asm goto acts as a conditional terminator
                // Without this, code would fall through to whatever block comes next in layout
                self.emit(Instruction::br(fall_through));

                if needs_edge_block {
                    for (edge, target, _) in &label_edges {
                        self.switch_bb(*edge);
                        if writes_back {
                            self.emit_asm_output_writeback(
                                outputs,
                                &ir_outputs,
                                &skip_post_handling,
                                &output_places,
                            );
                        }
                        // The jump leaves this scope; the fall-through does
                        // not. Same rule as a plain `goto` to the label.
                        self.release_vla_scopes_for_goto(*target);
                        self.emit(Instruction::br(*target));
                        self.link_bb(*edge, *target);
                    }
                }

                // Switch to fall-through block for subsequent instructions
                self.current_bb = Some(fall_through);
            }
        }

        self.emit_asm_output_writeback(outputs, &ir_outputs, &skip_post_handling, &output_places);
    }

    /// Store an asm statement's register outputs back to their destinations.
    fn emit_asm_output_writeback(
        &mut self,
        outputs: &[AsmOperand],
        ir_outputs: &[AsmConstraint],
        skip_post_handling: &[bool],
        output_places: &[super::linearize_emit::RmwPlace],
    ) {
        // store(value, addr, ...) - value first, then address
        for (i, op) in outputs.iter().enumerate() {
            // Memory-class outputs (`=m`/`+m`/...) need no post-asm
            // handling — the asm wrote the new value directly through
            // the memory operand.
            if skip_post_handling[i] {
                continue;
            }

            let out_pseudo = ir_outputs[i].pseudo;

            let typ = self.asm_operand_type(&op.expr, false);
            // Back through the place the read came from, so the operand
            // expression is not evaluated a second time.
            self.store_rmw_place(&output_places[i], out_pseudo, typ);
        }
    }

    /// The type a register operand of expression `e` is handled at: its own,
    /// or for a GNU vector its carrier (`Abi::vector_carrier`) -- the value in
    /// the register, where the IR otherwise keeps a vector at an address. A
    /// sixteen-byte vector in an `"x"` operand was handed over as its address
    /// in a general register, and read back eight bytes wide.
    fn asm_operand_type(&self, e: &Expr, is_memory: bool) -> TypeId {
        let typ = self.expr_type(e);
        if is_memory || !self.types.is_vector(typ) {
            return typ;
        }
        self.vector_carrier(typ, crate::abi::CallingConv::C)
    }

    /// The value of the register operand `e`, at its operand type `typ`
    /// ([`Self::asm_operand_type`]).
    fn asm_value(&mut self, e: &Expr, typ: TypeId) -> PseudoId {
        if self.types.is_vector(self.expr_type(e)) && !self.types.is_vector(typ) {
            let addr = self.vector_addr(e);
            return self.vector_to_carrier(addr, typ);
        }
        self.linearize_expr(e)
    }

    /// A memory operand's address as the object it names and a constant byte
    /// offset into it, when it is one: a local, a parameter, a static or a
    /// global, reached through members and constant subscripts.
    ///
    /// The operand then names the object's `Sym`, so the backend addresses the
    /// object where it lives and no register is spent on it -- see
    /// [`AsmConstraint::offset`]. Anything else keeps its address pseudo: a
    /// pointer computed at run time, a VLA's storage, a function.
    ///
    /// The address arithmetic `linearize_lvalue` emitted is left alone. It
    /// may have readers besides the operand -- `"=m"(*(q = &arr[2]))` stores
    /// it into `q` before the asm -- and this cannot see every reader, so
    /// dropping it wrote an undefined register into `q`. Once the operand no
    /// longer names it, DCE removes whatever nothing else reads.
    fn asm_memory_operand(&self, addr: PseudoId) -> (PseudoId, i64) {
        let Some(bb_id) = self.current_bb else {
            return (addr, 0);
        };
        let func = self.current_func.as_ref().expect("inside a function");
        let Some(bb) = func.get_block(bb_id) else {
            return (addr, 0);
        };
        let defs: std::collections::HashMap<PseudoId, &Instruction> = bb
            .insns
            .iter()
            .filter_map(|insn| insn.target.map(|t| (t, insn)))
            .collect();
        let walk = AddrWalk { func, defs: &defs };
        // Storage an operand can be addressed in: a local, or a global that
        // is not a function.
        let data_object = |sym: PseudoId, symaddr: &Instruction| {
            func.local_of(sym).is_some()
                || symaddr
                    .typ
                    .and_then(|t| self.types.base_type(t))
                    .is_some_and(|t| self.types.kind(t) != TypeKind::Function)
        };
        walk.object(addr, &data_object).unwrap_or((addr, 0))
    }

    /// Capture the stack pointer ahead of a VLA's allocation.
    ///
    /// One mark per declaration, not per block: a label sitting between two
    /// VLAs must release only the one declared after it, and a block-wide
    /// mark cannot express that.
    ///
    /// The nesting depths are read *before* the construct being lowered
    /// pushes its own break or continue target, and that is deliberate. A
    /// VLA declared in a `for` init clause, or in the controlling expression
    /// of a `switch`, is allocated once, outside the loop or switch, and its
    /// scope encloses the exit the jump lands on -- so a `break` or
    /// `continue` inside must *not* release it. `continue` especially: the
    /// storage is still live on the next iteration. The construct's own
    /// scope, which ends after its exit block, is what releases it.
    fn push_vla_mark(&mut self) {
        if self.current_bb.is_none() {
            return;
        }
        let mark = self.alloc_reg_pseudo();
        self.emit(
            Instruction::new(Opcode::StackSave)
                .with_target(mark)
                .with_type_and_size(self.types.void_ptr_id, self.ptr_bits()),
        );
        self.vla_marks.push(super::linearize::VlaMark {
            mark,
            break_depth: self.break_targets.len(),
            continue_depth: self.continue_targets.len(),
        });
    }

    /// Give every forward `goto` the VLA restore its label turned out to need.
    ///
    /// Deferred because a label's depth is known only once it has been placed.
    /// A jump recorded at depth `G` to a label at depth `L` leaves the scopes
    /// `L..G`, and the stack as it stood on entry to the first of them is the
    /// mark at index `L` -- the same rule the backward case applies directly.
    /// `L == G` means the label is still inside every scope the jump is in,
    /// which C17 6.8.6.1p1 permits and which must emit nothing: restoring
    /// there would free a VLA still in scope at the label.
    ///
    /// The restore goes *before* the branch, so the insertions into one block
    /// are applied back to front and the earlier indices stay valid.
    pub(crate) fn resolve_forward_goto_vla_restores(&mut self) {
        if self.pending_goto_vla.is_empty() {
            return;
        }
        let mut pending = std::mem::take(&mut self.pending_goto_vla);
        pending.sort_by_key(|p| (p.bb.0, std::cmp::Reverse(p.at)));
        let void_ptr = self.types.void_ptr_id;
        let ptr_bits = self.ptr_bits();
        for p in pending {
            let Some(depth) = self.goto_target_vla_depth(&p.target) else {
                continue;
            };
            let Some(&mark) = p.marks.get(depth) else {
                continue;
            };
            let insn = Instruction::new(Opcode::StackRestore)
                .with_src(mark)
                .with_type_and_size(void_ptr, ptr_bits);
            if let Some(func) = self.current_func.as_mut() {
                if let Some(bb) = func.get_block_mut(p.bb) {
                    if p.at <= bb.insns.len() {
                        bb.insns.insert(p.at, insn);
                    }
                }
            }
        }
    }

    /// How many VLA scopes a jump is *inside* at the label it reaches, or
    /// `None` if that cannot be said -- an undefined label, diagnosed
    /// elsewhere, or a computed `goto` in a function that takes no label's
    /// address.
    fn goto_target_vla_depth(&self, target: &GotoTarget) -> Option<usize> {
        match target {
            GotoTarget::Label(bb) => self.label_vla_depth.get(bb).copied(),
            // See [`GotoTarget::AnyAddressTaken`]: the deepest candidate is
            // the only depth that releases nothing another candidate still
            // needs.
            GotoTarget::AnyAddressTaken => self
                .addr_taken_labels
                .iter()
                .filter_map(|bb| self.label_vla_depth.get(bb).copied())
                .max(),
        }
    }

    /// Release everything the scope allocated and forget its marks.
    ///
    /// Called only from [`Linearizer::pop_scope`], so that leaving a
    /// declaration scope and leaving a VLA scope are the same act.
    pub(crate) fn close_vla_scope(&mut self, scope: &Scope) {
        let entry = scope.vla_entry;
        if self.vla_marks.len() <= entry {
            return;
        }
        // The first mark the scope took is the stack as it stood on entry,
        // so one restore undoes all of them.
        let mark = self.vla_marks[entry].mark;
        if !self.is_terminated() && self.current_bb.is_some() {
            self.emit_stack_restore(mark);
        }
        self.vla_marks.truncate(entry);
    }

    /// Release every VLA scope a jump to the block `target` leaves.
    ///
    /// A backward jump -- the label is already linearized, so it has a
    /// recorded depth -- leaves the scope of every VLA declared after it, and
    /// that storage has to go back. Otherwise `lab: int x[n]; ... goto lab;`
    /// grows the stack every time round until the program dies.
    ///
    /// A *forward* jump cannot be decided here: its label has no depth
    /// recorded yet, and whether it stays inside the scope of the VLAs in
    /// force or leaves it is exactly what decides between no restore and one.
    /// It is recorded and resolved in
    /// [`Self::resolve_forward_goto_vla_restores`].
    ///
    /// Leaving it to "the scope's own exit does the restoring" was wrong: the
    /// branch *terminates* the block, so `close_vla_scope` emits nothing and
    /// then drops the marks, and the enclosing scope has no mark of its own
    /// to undo them with. A `goto` out of a loop body's inner block grew the
    /// stack every time round.
    ///
    /// Every jump that names a label goes through here: a `goto`, and each
    /// label edge of an `asm goto`.
    fn release_vla_scopes_for_goto(&mut self, target: BasicBlockId) {
        if let Some(&depth) = self.label_vla_depth.get(&target) {
            if let Some(m) = self.vla_marks.get(depth) {
                let mark = m.mark;
                self.emit_stack_restore(mark);
            }
        } else {
            self.defer_vla_restore(GotoTarget::Label(target));
        }
    }

    /// Record a restore whose depth is not yet known, to be placed by
    /// [`Self::resolve_forward_goto_vla_restores`] at the end of the
    /// function. The restore goes where the current block ends now, which is
    /// ahead of the branch the caller is about to emit.
    fn defer_vla_restore(&mut self, target: GotoTarget) {
        if self.vla_marks.is_empty() {
            return;
        }
        let Some(current) = self.current_bb else {
            return;
        };
        let at = self
            .current_func
            .as_ref()
            .and_then(|f| f.get_block(current))
            .map_or(0, |b| b.insns.len());
        let marks = self.vla_marks.iter().map(|m| m.mark).collect();
        self.pending_goto_vla
            .push(crate::ir::linearize::PendingGotoVla {
                target,
                bb: current,
                at,
                marks,
            });
    }

    /// Put the stack pointer back to what `mark` captured.
    fn emit_stack_restore(&mut self, mark: PseudoId) {
        self.emit(
            Instruction::new(Opcode::StackRestore)
                .with_src(mark)
                .with_type_and_size(self.types.void_ptr_id, self.ptr_bits()),
        );
    }

    /// Release every VLA allocated inside the loop or switch being left.
    ///
    /// A mark taken at a nesting depth at or beyond the current one was taken
    /// inside the construct being left, so restoring to the **outermost**
    /// such mark undoes everything it allocated in one move. Without this a
    /// `continue` past a VLA declaration skipped the block's own restore and
    /// the loop grew the stack anyway.
    ///
    /// The marks stay recorded: the block that owns each one still drops it
    /// when its own linearization ends.
    fn unwind_vla_marks(&mut self, leaving: JumpKind) {
        let found = match leaving {
            // A `break` leaves the innermost loop *or switch*, so it undoes
            // what was allocated inside that one.
            JumpKind::Break => {
                let depth = self.break_targets.len();
                self.vla_marks
                    .iter()
                    .find(|m| m.break_depth >= depth)
                    .map(|m| m.mark)
            }
            // A `continue` leaves the innermost *loop*, which may be several
            // switches out -- so it undoes everything allocated since the
            // loop began, not just since the switch did.
            JumpKind::Continue => {
                let depth = self.continue_targets.len();
                self.vla_marks
                    .iter()
                    .find(|m| m.continue_depth >= depth)
                    .map(|m| m.mark)
            }
        };
        if let Some(mark) = found {
            self.emit_stack_restore(mark);
        }
    }

    /// Define `label` here: fall into its block and continue there.
    ///
    /// Every named label is placed through this, which is what records it as
    /// defined -- `&&lbl` naming a label between the case labels of a
    /// `switch` included.
    fn place_label(&mut self, label: LabelId) {
        self.defined_labels.insert(label);
        let label_bb = self.get_or_create_label(label);

        // If current block is not terminated, branch to label
        if !self.is_terminated() {
            if let Some(current) = self.current_bb {
                self.emit(Instruction::br(label_bb));
                self.link_bb(current, label_bb);
            }
        }

        self.switch_bb(label_bb);
        // Remember how many VLA marks were in force here. A backward
        // jump to this label leaves the scope of every VLA declared
        // *after* it, and the first such declaration's mark is the
        // stack as it stood at the label -- so that mark is what the
        // jump restores.
        //
        // Recorded as an index rather than captured at the label,
        // because a label can be reachable only by the jump itself:
        // `if (0) { lab: ; }` never runs a capture placed there, and
        // restoring from it read an uninitialized register. The mark
        // this index names is always written first, since the
        // declaration that creates it lies between the label and the
        // jump.
        if self.func_has_vla {
            self.label_vla_depth.insert(label_bb, self.vla_marks.len());
        }
    }

    /// The block for `label`, named by a `goto`, `&&label` or `asm goto` at
    /// `pos`. Recorded so `check_label_references` can insist the label
    /// exists.
    fn refer_to_label(&mut self, label: LabelId, pos: Position) -> BasicBlockId {
        self.label_refs.push((label, pos));
        self.get_or_create_label(label)
    }

    /// The assembler symbol for `&&name` at `pos`, or `None` outside a
    /// function, which is diagnosed here.
    ///
    /// The label becomes a branch target for every computed `goto` in the
    /// function, and the CFG has to say so or DCE deletes the block.
    pub(crate) fn take_label_address(&mut self, label: LabelId, pos: Position) -> Option<String> {
        // Outside a function there is no block to name, and
        // `get_or_create_label` would unwrap a `None` current function -- an
        // ICE on `void *g = &&L;` at file scope.
        if self.current_func.is_none() {
            crate::diag::error_args(
                pos,
                "label '{0}' referenced outside of any function",
                &[self.str(label.name)],
            );
            return None;
        }
        let bb = self.refer_to_label(label, pos);
        self.addr_taken_labels.push(bb);
        if let Some(func) = &mut self.current_func {
            func.takes_label_addr = true;
        }
        Some(bb.label_symbol(&self.current_func_name))
    }

    pub(crate) fn get_or_create_label(&mut self, label: LabelId) -> BasicBlockId {
        if let Some(&bb) = self.label_map.get(&label) {
            bb
        } else {
            let bb = self.alloc_bb();
            self.label_map.insert(label, bb);
            // The block shows the label as written. Two local labels of one
            // spelling show alike, but they are two blocks.
            let name = self.str(label.name).to_string();
            let block = self.get_or_create_bb(bb);
            block.label = Some(name);
            bb
        }
    }
}

/// A scope that no jump may enter from outside it.
enum JumpScope {
    /// The scope of an identifier with a variably modified type, from its
    /// declarator to the end of its block (C17 6.8.6.1p1).
    VariablyModified(crate::symbol::SymbolId),
    /// A GNU statement expression. gcc forbids entering one by `goto`, by
    /// `asm goto` or by a `switch` reaching a `case` inside it: control would
    /// arrive in the middle of evaluating the expression around it. Leaving
    /// one is allowed, and so is a computed `goto`, which gcc documents as
    /// undefined rather than diagnosing.
    StmtExpr,
    /// The scope of a variable with `__attribute__((cleanup))`, from the end
    /// of its declarator to the end of its block. A jump may enter it; one
    /// that leaves it runs the cleanup.
    Cleanup(crate::symbol::SymbolId),
}

impl JumpScope {
    /// Whether a jump from outside may not enter this scope.
    fn forbids_entry(&self) -> bool {
        !matches!(self, JumpScope::Cleanup(_))
    }
}

/// What the jump-scope walk tells the lowering about a function's labels.
pub(crate) struct LabelScopes {
    /// Every label the body writes, evaluated or not.
    pub(crate) written: std::collections::HashSet<LabelId>,
    /// For each label, the variables with a cleanup in whose scope it lies,
    /// outermost first.
    pub(crate) cleanups: std::collections::HashMap<LabelId, Vec<crate::symbol::SymbolId>>,
}

/// A `goto`, or one label of an `asm goto`, as the walk found it.
struct JumpRecord {
    label: LabelId,
    /// The scopes enclosing the jump.
    from: Vec<usize>,
    /// Where the jump was written. An `asm goto` records none for its
    /// labels, and is reported at the label it names instead.
    pos: Option<Position>,
}

/// Walks a function body recording, for every label and every jump, which
/// [`JumpScope`]s enclose it.
///
/// A variably modified scope opens at the declarator and runs to the end of
/// its block, so the walk is order-sensitive within a block: a label *before*
/// the declaration is outside the scope and may be jumped to, which is why the
/// scopes are pushed as the items are visited rather than collected up front.
/// Expressions are walked too, because a statement expression inside one holds
/// statements -- and labels, and jumps -- of its own.
struct JumpScopeWalk {
    /// Ids of the scopes currently open, innermost last.
    open: Vec<usize>,
    /// Every scope seen, indexed by scope id.
    scopes: Vec<JumpScope>,
    /// Each label, the scopes enclosing it, and where it was written.
    labels: Vec<(LabelId, Vec<usize>, Position)>,
    /// Each `goto` and `asm goto` label.
    gotos: Vec<JumpRecord>,
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

impl JumpScopeWalk {
    /// The walk of one function body.
    fn of(body: &Stmt) -> Self {
        let mut w = JumpScopeWalk {
            open: Vec::new(),
            scopes: Vec::new(),
            labels: Vec::new(),
            gotos: Vec::new(),
            bad_case_ids: Vec::new(),
            loop_depth: 0,
            switch_depth: 0,
            stray_jumps: Vec::new(),
        };
        w.walk(body, None);
        w
    }

    /// The first definition of each label, as an index into `labels`, which
    /// is the one a `goto` reaches; and in order, every later definition of a
    /// label already seen. One lookup per label: comparing each label with the
    /// labels before it, and each `goto` with every label, was quadratic in
    /// the labels.
    fn resolve_labels(&self) -> (std::collections::HashMap<LabelId, usize>, Vec<usize>) {
        let mut first = std::collections::HashMap::with_capacity(self.labels.len());
        let mut duplicates = Vec::new();
        for (i, (label, _, _)) in self.labels.iter().enumerate() {
            if first.contains_key(label) {
                duplicates.push(i);
            } else {
                first.insert(*label, i);
            }
        }
        (first, duplicates)
    }

    /// The offending scopes. A variably modified scope is reported once
    /// however many `case` labels sit inside it; a statement expression once
    /// per label, as gcc does.
    fn bad_cases(&self) -> Vec<(usize, Position)> {
        let mut out: Vec<(usize, Position)> = Vec::new();
        for (id, pos) in &self.bad_case_ids {
            let once_per_scope = matches!(self.scopes[*id], JumpScope::VariablyModified(_));
            if !once_per_scope || !out.iter().any(|(seen, _)| seen == id) {
                out.push((*id, *pos));
            }
        }
        out
    }

    /// The scopes a jump from `from` into `to` would enter, keeping only the
    /// outermost of each kind: one jump earns at most one diagnostic of each.
    fn entered(&self, from: &[usize], to: &[usize]) -> Vec<usize> {
        let mut out: Vec<usize> = Vec::new();
        let entered = to
            .iter()
            .filter(|id| !from.contains(id) && self.scopes[**id].forbids_entry());
        for &id in entered {
            let same_kind = |other: &usize| {
                std::mem::discriminant(&self.scopes[*other])
                    == std::mem::discriminant(&self.scopes[id])
            };
            if !out.iter().any(same_kind) {
                out.push(id);
            }
        }
        // gcc names the variably modified scope first.
        out.sort_by_key(|id| matches!(self.scopes[*id], JumpScope::StmtExpr));
        out
    }

    /// A block's items, closing on the way out every scope they opened.
    fn walk_items(&mut self, items: &[BlockItem], switch_scopes: Option<&[usize]>) {
        let depth = self.open.len();
        for item in items {
            match item {
                BlockItem::Declaration(decl) => self.walk_decl(decl, switch_scopes),
                BlockItem::Statement(s) => self.walk(s, switch_scopes),
            }
        }
        self.open.truncate(depth);
    }

    /// `switch_scopes` is the scope list in force at the innermost enclosing
    /// `switch`, against which a `case` label is judged.
    fn walk(&mut self, stmt: &Stmt, switch_scopes: Option<&[usize]>) {
        match stmt {
            Stmt::Block(items) => self.walk_items(items, switch_scopes),

            Stmt::Labeled { labels, stmt } => {
                for label in labels {
                    match label {
                        Label::Named { label, pos } => {
                            self.labels.push((*label, self.open.clone(), *pos));
                        }
                        Label::Case(low, high) => {
                            self.walk_case(low.pos, "case", switch_scopes);
                            self.walk_expr(low, switch_scopes);
                            if let Some(high) = high {
                                self.walk_expr(high, switch_scopes);
                            }
                        }
                        Label::Default(pos) => self.walk_case(*pos, "default", switch_scopes),
                    }
                }
                self.walk(stmt, switch_scopes);
            }
            Stmt::Goto { label, pos } => self.gotos.push(JumpRecord {
                label: *label,
                from: self.open.clone(),
                pos: Some(*pos),
            }),

            // 6.8.6.3p1 and 6.8.6.2p1: a `break` needs an enclosing loop or
            // switch and a `continue` an enclosing loop. Checked here rather
            // than in the `linearize_stmt` arms, which look like the obvious
            // place -- they fail exactly when the target stack is empty -- but
            // the lowering pushes a loop's targets only around its body, while
            // this walk counts enclosing loops and switches by the rule itself
            // and reports these beside the stray `case` and `default` labels.
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

            // A `switch` becomes the reference point for the labels inside it.
            // Its own scopes are captured before the body is walked, and its
            // controlling expression is outside it.
            Stmt::Switch { expr, body } => {
                self.walk_expr(expr, switch_scopes);
                let outer = self.open.clone();
                self.switch_depth += 1;
                self.walk(body, Some(&outer));
                self.switch_depth -= 1;
            }

            Stmt::If {
                cond,
                then_stmt,
                else_stmt,
            } => {
                self.walk_expr(cond, switch_scopes);
                self.walk(then_stmt, switch_scopes);
                if let Some(e) = else_stmt {
                    self.walk(e, switch_scopes);
                }
            }
            // A loop's controlling expressions are not inside the loop: gcc
            // rejects a `break` in a statement expression there.
            Stmt::While { cond, body } | Stmt::DoWhile { body, cond } => {
                self.walk_expr(cond, switch_scopes);
                self.loop_depth += 1;
                self.walk(body, switch_scopes);
                self.loop_depth -= 1;
            }
            Stmt::For {
                init,
                cond,
                post,
                body,
            } => {
                // A declaration in the init clause scopes over the body.
                let depth = self.open.len();
                match init {
                    Some(ForInit::Declaration(decl)) => self.walk_decl(decl, switch_scopes),
                    Some(ForInit::Expression(e)) => self.walk_expr(e, switch_scopes),
                    None => {}
                }
                for e in cond.iter().chain(post) {
                    self.walk_expr(e, switch_scopes);
                }
                self.loop_depth += 1;
                self.walk(body, switch_scopes);
                self.loop_depth -= 1;
                self.open.truncate(depth);
            }

            Stmt::Expr(e) | Stmt::Return(Some(e)) | Stmt::GotoIndirect { target: e, .. } => {
                self.walk_expr(e, switch_scopes)
            }
            Stmt::Asm {
                outputs,
                inputs,
                goto_labels,
                ..
            } => {
                for operand in outputs.iter().chain(inputs) {
                    self.walk_expr(&operand.expr, switch_scopes);
                }
                // An `asm goto` may branch to each label it names, and gcc
                // holds each to the rule a `goto` is held to.
                for label in goto_labels {
                    self.gotos.push(JumpRecord {
                        label: *label,
                        from: self.open.clone(),
                        pos: None,
                    });
                }
            }

            Stmt::Empty | Stmt::Return(None) => {}
        }
    }

    /// A `case` or `default` label -- `what` says which -- at `label_pos`.
    fn walk_case(
        &mut self,
        label_pos: Position,
        what: &'static str,
        switch_scopes: Option<&[usize]>,
    ) {
        // 6.8.1p2: a `case` or `default` belongs to a `switch`.
        if self.switch_depth == 0 {
            self.stray_jumps.push((label_pos, what));
        }

        // Reaching a `case` transfers control from the `switch`, so any scope
        // open here but not there would be entered without its declaration
        // running, or in the middle of evaluating an expression.
        // Every variably modified scope is recorded, since each names a
        // different declaration; only the outermost statement expression is,
        // since they all say the same thing.
        if let Some(outer) = switch_scopes {
            let mut stmt_expr_seen = false;
            let entered = self
                .open
                .iter()
                .filter(|id| !outer.contains(id) && self.scopes[**id].forbids_entry());
            for &id in entered {
                if matches!(self.scopes[id], JumpScope::StmtExpr) {
                    if stmt_expr_seen {
                        continue;
                    }
                    stmt_expr_seen = true;
                }
                self.bad_case_ids.push((id, label_pos));
            }
        }
    }

    /// Walk the statement expressions inside `expr`, each a scope of its own.
    fn walk_expr(&mut self, expr: &Expr, switch_scopes: Option<&[usize]>) {
        match &expr.kind {
            ExprKind::StmtExpr { stmts, result } => {
                let depth = self.open.len();
                self.open.push(self.scopes.len());
                self.scopes.push(JumpScope::StmtExpr);
                self.walk_items(stmts, switch_scopes);
                self.walk_expr(result, switch_scopes);
                self.open.truncate(depth);
            }
            _ => {
                for operand in expr.operands() {
                    self.walk_expr(operand, switch_scopes);
                }
            }
        }
    }

    fn walk_decl(&mut self, decl: &Declaration, switch_scopes: Option<&[usize]>) {
        for d in &decl.declarators {
            for e in &d.vla_sizes {
                self.walk_expr(e, switch_scopes);
            }
            // A declarator with size expressions is variably modified --
            // whether it is the array itself or a pointer to one, both of
            // which C17 6.7.6.2 calls variably modified and gcc refuses to
            // let a jump enter. Its scope begins before its initializer.
            if !d.vla_sizes.is_empty() {
                self.open.push(self.scopes.len());
                self.scopes.push(JumpScope::VariablyModified(d.symbol));
            }
            if let Some(init) = &d.init {
                self.walk_expr(init, switch_scopes);
            }
            // A cleanup is in force once its variable is initialized.
            if d.cleanup.is_some() {
                self.open.push(self.scopes.len());
                self.scopes.push(JumpScope::Cleanup(d.symbol));
            }
        }
    }

    /// The variables with a cleanup among the scopes `open`, outermost first.
    fn cleanup_vars(&self, open: &[usize]) -> Vec<crate::symbol::SymbolId> {
        open.iter()
            .filter_map(|&id| match self.scopes[id] {
                JumpScope::Cleanup(var) => Some(var),
                _ => None,
            })
            .collect()
    }
}

/// The linearizer's half of the shared C17 6.6 walk.
///
/// [`crate::constexpr`] owns the walk; what differs from the parser's half is
/// the `const`-object folding gcc performs in a static initializer.
impl crate::constexpr::ConstEnv for Linearizer<'_> {
    /// A static initializer needs an answer; the linearizer's other folds --
    /// a constant `?:` condition, chiefly -- are optimizations, and one of
    /// those is exactly the shape `__builtin_constant_p` is written in.
    fn deferred_constant_p(&self, scope: ConstScope) -> Option<i128> {
        matches!(scope, ConstScope::StaticInitializer).then_some(0)
    }

    fn types(&self) -> &TypeTable {
        self.types
    }

    fn ident_value(&self, sym: crate::symbol::SymbolId, scope: ConstScope) -> Option<i128> {
        let symbol = self.symbols.get(sym);
        if symbol.is_enum_constant() {
            return symbol.enum_value;
        }
        match scope {
            ConstScope::Standard | ConstScope::ArrayBound => None,
            ConstScope::StaticInitializer => self.const_object_value(sym),
        }
    }

    fn subobject_value(&self, expr: &Expr, scope: ConstScope) -> Option<i128> {
        match (scope, self.const_subobject_init(expr)?) {
            (ConstScope::StaticInitializer, crate::ir::Initializer::Int(v)) => Some(*v),
            _ => None,
        }
    }

    fn float_subobject_value(&self, expr: &Expr, scope: ConstScope) -> Option<FloatVal> {
        match (scope, self.const_subobject_init(expr)?) {
            (ConstScope::StaticInitializer, crate::ir::Initializer::Float(v)) => Some(*v),
            (ConstScope::StaticInitializer, crate::ir::Initializer::Int(v)) => {
                Some(FloatVal::from_i128(*v))
            }
            _ => None,
        }
    }

    /// A `const` floating object folds only in a static initializer, as its
    /// integer counterpart in [`Self::ident_value`] does.
    fn float_ident_value(
        &self,
        sym: crate::symbol::SymbolId,
        scope: ConstScope,
    ) -> Option<FloatVal> {
        match scope {
            ConstScope::Standard | ConstScope::ArrayBound => None,
            ConstScope::StaticInitializer => self.const_object_float_value(sym),
        }
    }
}

/// Why an operand's class cannot stand on its side of the colon, in gcc's
/// words, or `None` when it can.
///
/// An output must be written (`=` or `+`) and be somewhere a value can be
/// written: a register or memory, never only a constant. An input is only
/// read, so `=`, `+` and `&` make no sense there, and a matching constraint
/// must name an output that exists.
fn asm_operand_misuse(
    class: &AsmOperandClass,
    is_output: bool,
    num_outputs: usize,
) -> Option<&'static str> {
    if is_output {
        if class.access == AsmAccess::Read {
            Some("output operand constraint lacks '='")
        } else if class.tied.is_some() {
            Some("matching constraint not valid in output operand")
        } else if class.reg.is_none() && class.mem.is_none() {
            Some("impossible constraint in 'asm'")
        } else {
            None
        }
    } else {
        match class.access {
            AsmAccess::Write => Some("input operand constraint contains '='"),
            AsmAccess::ReadWrite => Some("input operand constraint contains '+'"),
            AsmAccess::Read if class.early_clobber => Some("input operand constraint contains '&'"),
            AsmAccess::Read if class.tied.is_some_and(|n| n >= num_outputs) => {
                Some("matching constraint references invalid operand number")
            }
            AsmAccess::Read => None,
        }
    }
}

/// The promoted type of a switch's controlling expression: the width and the
/// signedness in which C17 6.8.4.2 says every case label lives.
///
/// p5 converts each case constant to that type and p3 forbids two of them
/// being equal *after* the conversion, so a label's converted value is the
/// only one the rest of the switch path may see. Two places have to agree
/// about it -- the collector that records a label's range, and the body walk
/// that looks that same range back up to find the block it was given. If they
/// disagreed the lookup would simply miss, leaving the case body emitted into
/// the wrong block with nothing diagnosed.
///
/// One value carries the whole conversion so they cannot disagree:
/// [`CaseSet`] owns the `CaseConv`, [`CaseSet::insert`] and
/// [`CaseSet::overlap`] convert what they are handed, [`CaseIndex::of`] copies
/// the conversion out of the set it indexes, and [`CaseIndex::lookup`] -- the
/// only way into the map -- converts too. Conversion is idempotent, so a
/// caller that has already converted for its own reasons stays in step.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct CaseConv {
    /// Width of the promoted controlling type, in bits.
    bits: u32,
    /// Whether that type is unsigned.
    unsigned: bool,
}

impl CaseConv {
    pub(crate) fn new(bits: u32, unsigned: bool) -> Self {
        Self { bits, unsigned }
    }

    /// The conversion a `switch` whose promoted controlling type is `typ`
    /// applies to its labels.
    pub(crate) fn of(types: &TypeTable, typ: TypeId) -> Self {
        Self::new(types.size_bits(typ), types.is_unsigned(typ))
    }

    pub(crate) fn unsigned(self) -> bool {
        self.unsigned
    }

    /// `v` converted to this type, per C17 6.3.1.3: its low `bits` bits, read
    /// back with the type's own signedness.
    ///
    /// The result is carried the way the whole switch path carries a value --
    /// an `i128` holding the type's bit pattern -- so a 128-bit unsigned label
    /// above `i128::MAX` stays negative here and is ordered by [`Self::lt`]
    /// rather than by Rust's signed `<`.
    pub(crate) fn convert(self, v: i128) -> i128 {
        if self.bits == 0 || self.bits >= 128 {
            return v;
        }
        let shift = 128 - self.bits;
        let truncated = ((v as u128) << shift) >> shift;
        if self.unsigned {
            truncated as i128
        } else {
            ((truncated << shift) as i128) >> shift
        }
    }

    /// `a < b` in this type's signedness.
    pub(crate) fn lt(self, a: i128, b: i128) -> bool {
        if self.unsigned {
            (a as u128) < (b as u128)
        } else {
            a < b
        }
    }

    /// Whether the converted range `lo..=hi` holds the converted value `v`.
    ///
    /// The constant-selector lowering picks its one edge with this, and has to
    /// ask in the switch's signedness: a plain `i128` test read `case -1:` in
    /// a `switch` on `unsigned` as a huge lower bound and never selected it.
    pub(crate) fn contains(self, lo: i128, hi: i128, v: i128) -> bool {
        !self.lt(v, lo) && !self.lt(hi, v)
    }
}

/// A `switch` whose body is being lowered: where each of its labels starts.
///
/// [`Linearizer::linearize_switch`] pushes one on `switch_stack` around its
/// body, and the ordinary `Stmt::Labeled` arm places a `case` or `default`
/// label against the innermost -- so a label inside a loop, `if` or block in
/// the body belongs to this switch, and one inside a nested switch to that.
pub(crate) struct SwitchCtx {
    /// The labels `collect_switch_cases` found, by range.
    pub(crate) index: CaseIndex,
    /// Each label's block, parallel to the collected ranges.
    pub(crate) case_bbs: Vec<BasicBlockId>,
    /// The `default` label's block, if there is one.
    pub(crate) default_bb: Option<BasicBlockId>,
}

/// Each case range's position among a switch's labels, keyed by the range as
/// the controlling type sees it.
pub(crate) struct CaseIndex {
    conv: CaseConv,
    by_range: std::collections::HashMap<(i128, i128), usize>,
}

impl CaseIndex {
    /// Index the labels `set` collected, carrying `set`'s own conversion so
    /// that a lookup converts exactly as the insert did.
    ///
    /// The first label of a repeated range wins, as a scan in source order
    /// would find it; a duplicate has already been reported.
    pub(crate) fn of(set: &CaseSet) -> Self {
        let mut by_range = std::collections::HashMap::new();
        for (idx, range) in set.ranges().iter().enumerate() {
            by_range.entry(*range).or_insert(idx);
        }
        Self {
            conv: set.conv(),
            by_range,
        }
    }

    /// The position of the label written `lo ... hi`, whose endpoints are the
    /// raw constants as the label spells them.
    ///
    /// A label is identified by its whole range, so that `case 1 ... 3:` and a
    /// later `case 1:` cannot resolve to the same block -- the overlap check
    /// rejects that pair anyway, but matching on the low endpoint alone would
    /// make the two indistinguishable here.
    pub(crate) fn lookup(&self, lo: i128, hi: i128) -> Option<usize> {
        let key = (self.conv.convert(lo), self.conv.convert(hi));
        self.by_range.get(&key).copied()
    }
}

/// A switch's case ranges, in source order, with an index that finds an
/// overlap in logarithmic time.
///
/// Checking each new label against every earlier one made a switch
/// quadratic in its case count: 70,000 labels took five seconds to compile
/// and gcc's `limits-caselabels` eleven.
pub(crate) struct CaseSet {
    /// The ranges `(lo, hi)`, converted to the controlling type, in the order
    /// the labels were written.
    ranges: Vec<(i128, i128)>,
    /// Each range by its low end, as an order-preserving key, to its high end.
    by_lo: std::collections::BTreeMap<i128, (i128, i128, i128)>,
    /// What every endpoint entering the set is converted by.
    conv: CaseConv,
}

impl CaseSet {
    fn new(conv: CaseConv) -> Self {
        Self {
            ranges: Vec::new(),
            by_lo: std::collections::BTreeMap::new(),
            conv,
        }
    }

    pub(crate) fn conv(&self) -> CaseConv {
        self.conv
    }

    /// The ranges, converted, in source order. Parallel to the case blocks.
    pub(crate) fn ranges(&self) -> &[(i128, i128)] {
        &self.ranges
    }

    /// `v` as a signed key ordered the way the switch's type orders it: an
    /// unsigned value has its top bit flipped, which maps unsigned order onto
    /// signed order.
    fn key(&self, v: i128) -> i128 {
        if self.conv.unsigned {
            v ^ i128::MIN
        } else {
            v
        }
    }

    /// An earlier range sharing a value with `lo..=hi`, if any, as converted.
    ///
    /// The ranges recorded are disjoint -- an overlap is an error -- so the
    /// only candidate is the one starting last at or before `hi`.
    fn overlap(&self, lo: i128, hi: i128) -> Option<(i128, i128)> {
        let (lo, hi) = (self.conv.convert(lo), self.conv.convert(hi));
        let (_, &(hi_key, lo2, hi2)) = self.by_lo.range(..=self.key(hi)).next_back()?;
        (hi_key >= self.key(lo)).then_some((lo2, hi2))
    }

    fn insert(&mut self, lo: i128, hi: i128) {
        let (lo, hi) = (self.conv.convert(lo), self.conv.convert(hi));
        self.ranges.push((lo, hi));
        let (lo_key, hi_key) = (self.key(lo), self.key(hi));
        self.by_lo.insert(lo_key, (hi_key, lo, hi));
    }
}

#[cfg(test)]
mod case_set_tests {
    use super::{CaseConv, CaseIndex, CaseSet};

    /// A 128-bit conversion is the identity, which is what the ranges below
    /// want: they are about ordering, not about width.
    fn wide(unsigned: bool) -> CaseConv {
        CaseConv::new(128, unsigned)
    }

    /// C17 6.8.4.2p5 converts a case constant to the promoted controlling
    /// type: the low bits, read back with that type's signedness.
    #[test]
    fn convert_takes_the_low_bits_with_the_types_signedness() {
        let int = CaseConv::new(32, false);
        let uint = CaseConv::new(32, true);

        // In range: unchanged either way.
        assert_eq!(int.convert(7), 7);
        assert_eq!(uint.convert(7), 7);

        // 2^32 is zero in 32 bits -- the label that silently became `case 0:`.
        assert_eq!(int.convert(4294967296), 0);
        assert_eq!(uint.convert(4294967296), 0);

        // -1 keeps its value as `int` and is the largest `unsigned int`.
        assert_eq!(int.convert(-1), -1);
        assert_eq!(uint.convert(-1), 4294967295);

        // The boundary of the signed range wraps the way C says.
        assert_eq!(int.convert(2147483648), -2147483648);
        assert_eq!(uint.convert(2147483648), 2147483648);

        // Narrower and wider types, and the 128-bit identity.
        assert_eq!(CaseConv::new(8, false).convert(255), -1);
        assert_eq!(CaseConv::new(8, true).convert(-1), 255);
        assert_eq!(CaseConv::new(64, true).convert(-1), u64::MAX as i128);
        assert_eq!(CaseConv::new(128, true).convert(-1), -1);
        assert_eq!(CaseConv::new(128, false).convert(i128::MIN), i128::MIN);
    }

    /// Converting is idempotent, which is what lets the collector convert for
    /// its own diagnostics and still hand the set and the index raw or
    /// converted endpoints interchangeably.
    #[test]
    fn convert_is_idempotent() {
        for conv in [
            CaseConv::new(8, false),
            CaseConv::new(16, true),
            CaseConv::new(32, false),
            CaseConv::new(64, true),
            CaseConv::new(128, true),
        ] {
            for v in [0, 1, -1, 255, 4294967296, i128::MIN, i128::MAX] {
                let once = conv.convert(v);
                assert_eq!(conv.convert(once), once, "{conv:?} {v}");
            }
        }
    }

    /// The constant-selector lowering asks in the switch's own signedness.
    #[test]
    fn contains_tests_the_range_in_the_switch_signedness() {
        let uint = CaseConv::new(32, true);
        let big = uint.convert(-1); // 4294967295
        assert!(uint.contains(big, big, big));
        assert!(!uint.contains(big, big, 0));
        assert!(uint.contains(0, big, 5));

        let int = CaseConv::new(32, false);
        assert!(int.contains(-1, -1, -1));
        assert!(int.contains(-5, 5, 0));
        assert!(!int.contains(-5, 5, 6));
        // A signed test would read the unsigned bound as below zero.
        assert!(!int.contains(0, 10, big));
    }

    /// The two-site invariant: what the collector inserts is exactly what the
    /// body walk finds, even though the walk looks the label up by the
    /// constant as written rather than as converted.
    #[test]
    fn the_index_finds_a_label_by_its_unconverted_constant() {
        let conv = CaseConv::new(32, false);
        let mut set = CaseSet::new(conv);
        set.insert(0, 0);
        set.insert(-1, -1);
        set.insert(70000, 70005);
        // Stored converted, and 2^32+3 is 3 in an `int` switch.
        set.insert(4294967299, 4294967299);
        assert_eq!(set.ranges(), [(0, 0), (-1, -1), (70000, 70005), (3, 3)]);

        let index = CaseIndex::of(&set);
        assert_eq!(index.lookup(0, 0), Some(0));
        assert_eq!(index.lookup(-1, -1), Some(1));
        assert_eq!(index.lookup(70000, 70005), Some(2));
        // Looked up as written, found as converted.
        assert_eq!(index.lookup(4294967299, 4294967299), Some(3));
        assert_eq!(index.lookup(3, 3), Some(3));
        assert_eq!(index.lookup(9, 9), None);

        // And a label the controlling type sees as negative.
        let uconv = CaseConv::new(32, true);
        let mut uset = CaseSet::new(uconv);
        uset.insert(-1, -1);
        assert_eq!(uset.ranges(), [(4294967295, 4294967295)]);
        let uindex = CaseIndex::of(&uset);
        assert_eq!(uindex.lookup(-1, -1), Some(0));
        assert_eq!(uindex.lookup(4294967295, 4294967295), Some(0));
    }

    /// Two labels that differ before the conversion collide after it, which is
    /// the duplicate C17 6.8.4.2p3 forbids.
    #[test]
    fn overlap_sees_the_converted_values() {
        let mut set = CaseSet::new(CaseConv::new(32, false));
        set.insert(0, 0);
        assert_eq!(set.overlap(4294967296, 4294967296), Some((0, 0)));
        assert_eq!(set.overlap(1, 1), None);
    }

    #[test]
    fn overlap_finds_the_range_sharing_a_value() {
        let mut set = CaseSet::new(wide(false));
        set.insert(-10, -5);
        set.insert(0, 0);
        set.insert(10, 20);
        assert_eq!(set.overlap(-7, -7), Some((-10, -5)));
        assert_eq!(set.overlap(-4, -1), None);
        assert_eq!(set.overlap(-1, 1), Some((0, 0)));
        assert_eq!(set.overlap(5, 9), None);
        assert_eq!(set.overlap(5, 10), Some((10, 20)));
        assert_eq!(set.overlap(20, 30), Some((10, 20)));
        assert_eq!(set.overlap(21, 30), None);
        assert_eq!(set.ranges, [(-10, -5), (0, 0), (10, 20)]);
    }

    /// Unsigned order: a value above `i64::MAX`, carried in an `i128` as a
    /// negative 128-bit pattern for `unsigned __int128`, still sorts above
    /// every small one.
    #[test]
    fn overlap_orders_by_the_switch_type_signedness() {
        let big = u128::MAX as i128; // -1 as i128, the largest unsigned value
        let mut set = CaseSet::new(wide(true));
        set.insert(1, 5);
        set.insert(big - 10, big);
        assert_eq!(set.overlap(big - 3, big - 3), Some((big - 10, big)));
        assert_eq!(set.overlap(6, 100), None);
        assert_eq!(set.overlap(0, 1), Some((1, 5)));
    }
}

#[cfg(test)]
mod jump_scope_tests {
    use super::{JumpScope, JumpScopeWalk};
    use crate::parse::ast::ExternalDecl;
    use crate::parse::parser::Parser;
    use crate::strings::StringTable;
    use crate::symbol::SymbolTable;
    use crate::target::Target;
    use crate::token::lexer::Tokenizer;
    use crate::types::TypeTable;

    /// The walk of the one function `src` defines.
    fn walk_of(src: &str) -> JumpScopeWalk {
        let mut strings = StringTable::new();
        let tokens = Tokenizer::new(src.as_bytes(), 0, &mut strings).tokenize();
        let mut symbols = SymbolTable::new();
        let mut types = TypeTable::new(&Target::host());
        let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
        let tu = parser.parse_translation_unit().expect("parses");
        let func = tu
            .items
            .iter()
            .find_map(|item| match item {
                ExternalDecl::FunctionDef(f) => Some(f),
                _ => None,
            })
            .expect("one function");
        JumpScopeWalk::of(&func.body)
    }

    /// Whether any `goto` in `src` enters a statement expression.
    fn goto_enters_stmt_expr(src: &str) -> bool {
        let w = walk_of(src);
        let (first, _) = w.resolve_labels();
        w.gotos.iter().any(|jump| {
            let (_, to, _) = &w.labels[first[&jump.label]];
            w.entered(&jump.from, to)
                .iter()
                .any(|id| matches!(w.scopes[*id], JumpScope::StmtExpr))
        })
    }

    #[test]
    fn a_goto_into_a_statement_expression_enters_it() {
        for src in [
            "int f(int x) { goto L; return ({ L: x; }); }",
            "int f(int x) { int a = ({ goto N; 1; }); int b = ({ N: 2; }); return a + b; }",
            "int f(int x) { return ({ goto L; ({ L: 1; }); }); }",
            "int f(int x) { goto L; return ({ { L: x; } 1; }); }",
            "int f(int x) { asm goto (\"\" :::: R); return ({ R: x; }); }",
        ] {
            assert!(goto_enters_stmt_expr(src), "{src}");
        }
    }

    #[test]
    fn a_goto_out_of_or_within_a_statement_expression_enters_nothing() {
        for src in [
            "int f(int x) { int y = ({ if (x) goto out; x; }); return y; out: return -1; }",
            "int f(int x) { return ({ int r = 0; goto M; r = 5; M: r; }); }",
            "int f(int x) { return ({ int r = ({ if (x) goto P; 1; }); P: r; }); }",
            "int f(int x) { L: x = ({ if (x) goto L; x; }); return x; }",
        ] {
            assert!(!goto_enters_stmt_expr(src), "{src}");
        }
    }

    #[test]
    fn a_case_inside_a_statement_expression_is_reached_by_its_switch() {
        let w = walk_of(
            "int f(int x) { switch (x) { case 0: x = ({ case 1: x; case 2: x; }); } return x; }",
        );
        let bad = w.bad_cases();
        // Once per label, as gcc reports it.
        assert_eq!(bad.len(), 2);
        assert!(bad
            .iter()
            .all(|(id, _)| matches!(w.scopes[*id], JumpScope::StmtExpr)));

        // A switch wholly inside one statement expression is fine.
        let w = walk_of(
            "int f(int x) { return ({ int r = 0; switch (x) { case 1: r = 1; break; default: r = 2; } r; }); }",
        );
        assert!(w.bad_cases().is_empty());
    }

    #[test]
    fn a_loop_condition_is_outside_its_loop() {
        let w = walk_of("int f(int x) { while (({ if (x) break; 1; })) x--; return x; }");
        assert_eq!(w.stray_jumps.len(), 1);
        let w = walk_of("int f(int x) { for (;;) { x = ({ if (x) break; x; }); } return x; }");
        assert!(w.stray_jumps.is_empty());
    }

    /// A `goto` reaches the first label of its name; every later one is a
    /// duplicate, each reported, a label in a statement expression included.
    #[test]
    fn labels_resolve_to_the_first_of_each_name() {
        let w = walk_of(
            "int f(int x) { A: x++; B: x++; A: x++; ({ B: x; }); A: goto B; C: return x; }",
        );
        let (first, duplicates) = w.resolve_labels();
        let mut firsts: Vec<usize> = first.values().copied().collect();
        firsts.sort_unstable();
        assert_eq!(firsts, [0, 1, 5]);
        assert_eq!(duplicates, [2, 3, 4]);
        assert_eq!(first[&w.gotos[0].label], 1);
    }
}

#[cfg(test)]
mod asm_operand_tests {
    use crate::ir::linearize::test_linearize::linearize_source;
    use crate::ir::Opcode;
    use crate::target::Target;

    /// A register operand of array or function type is the pointer it
    /// decays to, sixty-four bits wide; a memory operand keeps the object's
    /// size.
    #[test]
    fn an_array_value_operand_is_pointer_wide() {
        let module = linearize_source(
            "long a[4];\n\
             int g(void);\n\
             void f(void) {\n\
             __asm__ volatile(\"\" : : \"r\"(a), \"r\"(g), \"m\"(a));\n\
             }\n",
            &Target::host(),
        );
        let f = module.functions.iter().find(|f| f.name == "f").unwrap();
        let asm = f
            .blocks
            .iter()
            .flat_map(|b| &b.insns)
            .find(|i| i.op == Opcode::Asm)
            .unwrap();
        let sizes: Vec<u32> = asm
            .extra()
            .asm_data
            .as_ref()
            .unwrap()
            .inputs
            .iter()
            .map(|c| c.size)
            .collect();
        assert_eq!(sizes, [64, 64, 256]);
    }
}

/// The scalar initializer of `size` bytes at `offset` in `init`, which spans
/// `span` bytes, or `None` when none is there to read. A subobject the
/// initializer leaves out is zero, but has no entry to answer with.
fn initializer_leaf(
    init: &crate::ir::Initializer,
    offset: usize,
    size: usize,
    span: usize,
) -> Option<&crate::ir::Initializer> {
    use crate::ir::Initializer;
    match init {
        Initializer::Int(_) | Initializer::Float(_) => {
            (offset == 0 && size == span).then_some(init)
        }
        Initializer::Array {
            elem_size,
            elements,
            ..
        } => {
            let (start, element) = elements
                .iter()
                .find(|(start, _)| (*start..*start + *elem_size).contains(&offset))?;
            initializer_leaf(element, offset - start, size, *elem_size)
        }
        Initializer::Struct { fields, .. } => {
            let (start, field_size, field) = fields
                .iter()
                .find(|(start, len, _)| (*start..*start + *len).contains(&offset))?;
            initializer_leaf(field, offset - start, size, *field_size)
        }
        _ => None,
    }
}
